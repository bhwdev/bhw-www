;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/tests/clog
  (:use #:common-lisp)
  (:import-from #:alexandria)
  (:import-from #:cl-ppcre)
  (:import-from #:clog)
  (:import-from #:flexi-streams)
  (:import-from #:rove #:deftest #:ok #:ng #:testing)
  (:import-from #:usocket)
  (:local-nicknames (#:conn #:clog-connection))
  (:documentation
   "Tests for the fork of CLOG in clog/: its websockets (RFC 6455), checked
against a client side written here apart from the server's, and its server on
Hunchentoot, through a whole CLOG session over a socket."))

(in-package #:project-isidore/tests/clog)

;;; The client's side of the framing, apart from clog-websocket.lisp's.

(defun octets (&rest parts)
  "The octets of PARTS, each an octet, a vector of octets or a string (UTF-8)."
  (let ((out (flexi-streams:make-in-memory-output-stream)))
    (dolist (part parts (flexi-streams:get-output-stream-sequence out))
      (etypecase part
        ((unsigned-byte 8) (write-byte part out))
        (string (write-sequence (flexi-streams:string-to-octets part :external-format :utf-8) out))
        (vector (write-sequence part out))))))

(defun client-frame (opcode payload &key (fin t) (mask #(17 34 51 68)) (rsv 0))
  "A frame as a browser sends it: OPCODE, PAYLOAD (octets or a string),
masked with MASK unless that is NIL."
  (let* ((payload (octets payload))
         (length (length payload)))
    (octets (logior (if fin #x80 0) (ash rsv 4) opcode)
            (logior (if mask #x80 0) (cond ((< length 126) length) ((< length 65536) 126) (t 127)))
            (cond ((< length 126) #())
                  ((< length 65536) (vector (ldb (byte 8 8) length) (ldb (byte 8 0) length)))
                  (t (coerce (loop for shift from 56 downto 0 by 8
                                   collect (ldb (byte 8 shift) length))
                             'vector)))
            (or mask #())
            (if mask
                (map 'vector (lambda (octet i) (logxor octet (aref mask (mod i 4))))
                     payload (alexandria:iota length))
                payload))))

(defun read-server-frame (stream)
  "The opcode, payload and FIN of the next frame from the server on STREAM,
which must not be masked; NIL at end of file."
  (let ((first (read-byte stream nil nil)))
    (when first
      (let* ((second (read-byte stream))
             (length (ldb (byte 7 0) second)))
        (assert (not (logbitp 7 second)) () "a masked frame from the server")
        (setf length (case length
                       (126 (logior (ash (read-byte stream) 8) (read-byte stream)))
                       (127 (loop repeat 8 for value = (read-byte stream)
                                    then (logior (ash value 8) (read-byte stream))
                                  finally (return value)))
                       (t length)))
        (let ((payload (make-array length :element-type '(unsigned-byte 8))))
          (read-sequence payload stream)
          (values (ldb (byte 4 0) first) payload (logbitp 7 first)))))))

(defun in-memory-websocket (&rest frames)
  "A websocket whose browser sends FRAMES, and the stream that collects what
the server sends back."
  (let* ((in (flexi-streams:make-in-memory-input-stream (apply #'octets frames)))
         (out (flexi-streams:make-in-memory-output-stream)))
    (values (make-instance 'conn::websocket :stream (make-two-way-stream in out))
            out)))

(defun sent-frames (out)
  "The frames the server wrote to OUT, as (opcode payload) lists."
  (let ((in (flexi-streams:make-in-memory-input-stream
             (flexi-streams:get-output-stream-sequence out))))
    (loop for (opcode payload) = (multiple-value-list (read-server-frame in))
          while opcode collect (list opcode payload))))

(defun fails-with (code thunk)
  "True when THUNK signals a websocket error with status CODE."
  (handler-case (progn (funcall thunk) nil)
    (conn::websocket-error (e) (= code (conn::websocket-error-code e)))))

;;; Framing

;; RFC 6455, section 1.3.
(deftest websocket-accept-key
  (ok (string= "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
               (conn::websocket-accept "dGhlIHNhbXBsZSBub25jZQ==")))
  (ok (conn::websocket-key-p "dGhlIHNhbXBsZSBub25jZQ=="))
  (ng (conn::websocket-key-p "c2hvcnQ="))
  (ng (conn::websocket-key-p "not base64!"))
  (ok (conn::header-token-p "upgrade" "keep-alive, Upgrade"))
  (ng (conn::header-token-p "upgrade" "keep-alive")))

(deftest frames-both-ways
  (testing "a masked frame of each length encoding is read and unmasked"
    (dolist (length '(0 5 125 126 300 65535 65536 70000))
      (let* ((payload (make-array length :element-type '(unsigned-byte 8)
                                         :initial-contents (loop for i below length
                                                                 collect (mod (* 7 i) 256))))
             (in (flexi-streams:make-in-memory-input-stream (client-frame 2 payload))))
        (multiple-value-bind (opcode read fin)
            (conn::read-frame in (read-byte in) (* 1024 1024))
          (ok (and (= 2 opcode) fin (equalp payload read)) (princ-to-string length))))))
  (testing "the server's frames are unmasked, in the shortest encoding"
    (dolist (length '(0 5 125 126 300 65535 65536 70000))
      (let ((payload (make-array length :element-type '(unsigned-byte 8) :initial-element 65))
            (out (flexi-streams:make-in-memory-output-stream)))
        (conn::write-frame out 1 payload)
        (let ((frame (flexi-streams:get-output-stream-sequence out)))
          (ok (= (length frame) (+ length (cond ((< length 126) 2) ((< length 65536) 4) (t 10))))
              (princ-to-string length))
          (multiple-value-bind (opcode read)
              (read-server-frame (flexi-streams:make-in-memory-input-stream frame))
            (ok (and (= 1 opcode) (equalp payload read)) (princ-to-string length))))))))

(deftest frames-refused
  (flet ((read-it (frame &optional (max 1000))
           (lambda ()
             (let ((in (flexi-streams:make-in-memory-input-stream frame)))
               (conn::read-frame in (read-byte in) max)))))
    (ok (fails-with 1002 (read-it (client-frame 1 "hi" :mask nil))) "unmasked")
    (ok (fails-with 1002 (read-it (client-frame 1 "hi" :rsv 4))) "reserved bit")
    (ok (fails-with 1002 (read-it (client-frame 3 "hi"))) "unknown opcode")
    (ok (fails-with 1002 (read-it (client-frame 9 "hi" :fin nil))) "fragmented ping")
    (ok (fails-with 1002 (read-it (client-frame 9 (make-string 126 :initial-element #\a))))
        "long ping")
    (ok (fails-with 1009 (read-it (client-frame 1 (make-string 1001 :initial-element #\a))))
        "too long")))

;;; Messages

(deftest messages
  (testing "fragments make one message; a ping between them is answered"
    (multiple-value-bind (websocket out)
        (in-memory-websocket (client-frame 1 "héllo, " :fin nil)
                             (client-frame 9 "are you there")
                             (client-frame 0 "wör" :fin nil)
                             (client-frame 0 "ld"))
      (multiple-value-bind (kind text) (conn::read-message websocket)
        (ok (eq :text kind))
        (ok (equal "héllo, wörld" text)))
      (ok (equalp (list (list 10 (octets "are you there"))) (sent-frames out)))))
  (testing "a close frame is answered with its status, and ends the reading"
    (multiple-value-bind (websocket out)
        (in-memory-websocket (client-frame 8 (octets 3 233 "bye")))
      (ok (eq :close (conn::read-message websocket)))
      (ok (eq :closing (conn::websocket-state websocket)))
      (ok (equalp (list (list 8 (octets 3 233))) (sent-frames out)))))
  (testing "nothing more to read is a close"
    (ok (eq :close (conn::read-message (in-memory-websocket)))))
  (testing "text that is not UTF-8"
    (ok (fails-with 1007 (lambda ()
                           (conn::read-message
                            (in-memory-websocket (client-frame 1 (octets #xC3 #x28))))))))
  (testing "a continuation without a message, a message inside another"
    (ok (fails-with 1002 (lambda ()
                           (conn::read-message (in-memory-websocket (client-frame 0 "x"))))))
    (ok (fails-with 1002 (lambda ()
                           (conn::read-message
                            (in-memory-websocket (client-frame 1 "x" :fin nil)
                                                 (client-frame 1 "y")))))))
  (testing "a message over the limit, across its fragments"
    (let ((conn:*websocket-max-message-octets* 10))
      (ok (fails-with 1009 (lambda ()
                             (conn::read-message
                              (in-memory-websocket (client-frame 1 "123456" :fin nil)
                                                   (client-frame 0 "7890X")))))))))

(deftest sending
  (multiple-value-bind (websocket out) (in-memory-websocket)
    (ok (conn::websocket-send websocket "ça"))
    (conn::websocket-close websocket 1001 "away")
    (ng (conn::websocket-send websocket "too late"))
    (conn::websocket-close websocket 1000 "again")
    (ok (equalp (list (list 1 (octets "ça")) (list 8 (octets 3 233 "away")))
                (sent-frames out)))))

(deftest static-file-paths
  (let ((root (asdf:system-relative-pathname :project-isidore "")))
    (ok (conn::static-file root "/clog.asd"))
    (ok (conn::static-file root "/assets/global.css"))
    (dolist (path (list "/" "/assets" "/assets/" "/assets//global.css" "/../etc/passwd"
                        "/.gitignore" "/~/x" "//etc/passwd" "/SYS:SRC;X.LISP" "/a*"
                        "/clog.asd/" "/nothing-here"))
      (ng (conn::static-file root path) path)))
  (ok (equal "text/css; charset=utf-8" (conn:file-content-type #p"a.css")))
  (ok (equal "image/webp" (conn:file-content-type #p"a.webp")))
  (ok (equal "font/woff2" (conn:file-content-type #p"a.woff2")))
  (ok (equal "application/octet-stream" (conn:file-content-type #p"a.unknown-type"))))

(deftest script-strings
  ;; What a form posts to a boot page goes into a <script> as a string.
  (let ((s (conn::script-string "a';alert(1)//</script><script>x")))
    (ng (search "</" s))
    (ng (cl-ppcre:scan "(?<!\\\\)'" s))))

;;; A whole session, over a socket.  Port 8294, as 8293 is the application
;;; tests' and 8080 the production server's.

(defparameter *port* 8294)

(defun connect ()
  (usocket:socket-connect "127.0.0.1" *port* :element-type '(unsigned-byte 8)))

(defun send-request (stream &rest lines)
  (write-sequence (octets (format nil "~{~A~C~C~}~C~C"
                                  (loop for line in lines
                                        append (list line #\Return #\Linefeed))
                                  #\Return #\Linefeed))
                  stream)
  (finish-output stream))

(defun read-head (stream)
  "The status code and the header lines of the response on STREAM."
  (let ((lines (loop for line = (with-output-to-string (s)
                                  (loop for octet = (read-byte stream)
                                        until (= octet 10)
                                        unless (= octet 13) do (write-char (code-char octet) s)))
                     until (string= line "") collect line)))
    (values (parse-integer (first lines) :start 9 :end 12) (rest lines))))

(defun http-get (path)
  "The status and body of GET PATH, over a connection of its own."
  (let ((socket (connect)))
    (unwind-protect
         (let ((stream (usocket:socket-stream socket)))
           (send-request stream (format nil "GET ~A HTTP/1.0" path))
           (multiple-value-bind (status headers) (read-head stream)
             (declare (ignore headers))
             (values status
                     (flexi-streams:octets-to-string
                      (coerce (loop for octet = (read-byte stream nil) while octet collect octet)
                              '(vector (unsigned-byte 8)))
                      :external-format :utf-8))))
      (usocket:socket-close socket))))

(defun open-websocket (&key (key "dGhlIHNhbXBsZSBub25jZQ==") (version "13"))
  "A socket that has sent a websocket handshake for /clog, and the status and
header lines of the answer."
  (let ((socket (connect)))
    (send-request (usocket:socket-stream socket)
                  "GET /clog HTTP/1.1" "Host: localhost" "Upgrade: websocket"
                  "Connection: keep-alive, Upgrade" (format nil "Sec-WebSocket-Key: ~A" key)
                  (format nil "Sec-WebSocket-Version: ~A" version))
    (multiple-value-bind (status headers) (read-head (usocket:socket-stream socket))
      (values socket status headers))))

(defun next-text (stream)
  "The next text message from the server on STREAM."
  (loop (multiple-value-bind (opcode payload) (read-server-frame stream)
          (cond ((null opcode) (return nil))
                ((= 1 opcode)
                 (return (flexi-streams:octets-to-string payload :external-format :utf-8)))))))

(defun next-control-frame (stream)
  "The opcode and payload of the next frame from the server on STREAM that is
not a text message; NIL at end of file."
  (loop (multiple-value-bind (opcode payload) (read-server-frame stream)
          (unless (eql 1 opcode)
            (return (values opcode payload))))))

(defun send-text (stream text)
  (write-sequence (client-frame 1 text) stream)
  (finish-output stream))

(defun clog-session (stream)
  "Play the browser's part on STREAM, an open websocket, until the page is
built: answer each query the server sends with the page's path, \"/\".
Return the script that builds the page and every script the server sent."
  (loop with scripts = '()
        for script = (next-text stream)
        while script
        do (push script scripts)
           (multiple-value-bind (match groups)
               (cl-ppcre:scan-to-strings "^ws\\.send \\(\"(\\d+):\"" script)
             (if match
                 (send-text stream (format nil "~A:/" (aref groups 0)))
                 (when (search "hello from the fork" script)
                   (return (values script scripts)))))))

(deftest clog-server
  ;; The server's threads see the global values.  No wait for a reconnect
  ;; when the websocket closes.
  (let ((hide conn:*hide-connection-output*)
        (delay conn:*reconnect-delay*))
    (setf conn:*hide-connection-output* t
          conn:*reconnect-delay* nil)
    (let ((*standard-output* (make-broadcast-stream)))
      (clog:initialize (lambda (body) (clog:create-div body :content "hello from the fork"))
                       :host "127.0.0.1" :port *port* :static-root nil
                       :acceptor-initargs '(:read-timeout 1 :write-timeout 1)))
    (unwind-protect
         (clog-server-tests)
      (let ((*standard-output* (make-broadcast-stream)))
        (clog:shutdown))
      (setf conn:*hide-connection-output* hide
            conn:*reconnect-delay* delay))))

(defun clog-server-tests ()
       (progn
         (testing "boot page, boot.js and CLOG's own files"
           (multiple-value-bind (status body) (http-get "/")
             (ok (= 200 status))
             (ok (search "/js/boot.js" body))
             (ng (search "jquery" body)))
           (ok (= 200 (http-get "/js/boot.js")))
           (ok (= 404 (http-get "/js/jquery.min.js")))
           (ok (= 200 (http-get "/css/w3.css")))
           (multiple-value-bind (status body) (http-get "/nothing")
             (ok (= 404 status))
             (ng (search "Hunchentoot" body)))
           (ok (= 404 (http-get "/%2E%2E/clog.asd")))
           (ok (= 404 (http-get "//etc/passwd"))))
         (testing "handshakes refused"
           (ok (= 400 (http-get "/clog")))
           (multiple-value-bind (socket status headers) (open-websocket :version "8")
             (usocket:socket-close socket)
             (ok (= 426 status))
             (ok (member "Sec-WebSocket-Version: 13" headers :test #'string-equal)))
           (multiple-value-bind (socket status) (open-websocket :key "c2hvcnQ=")
             (usocket:socket-close socket)
             (ok (= 400 status))))
         (testing "a page built over the websocket"
           (multiple-value-bind (socket status headers) (open-websocket)
             (unwind-protect
                  (let ((stream (usocket:socket-stream socket)))
                    (ok (= 101 status))
                    (ok (member "Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=" headers
                                :test #'string-equal))
                    (ng (find-if (lambda (h) (cl-ppcre:scan "(?i)^(transfer-encoding|content-length|content-type):" h))
                                 headers))
                    (ok (cl-ppcre:scan "^clog\\['connection_id'\\]='[0-9a-f]{32}'$"
                                       (next-text stream)))
                    (multiple-value-bind (page scripts) (clog-session stream)
                      (ok page)
                      (ok (search "clog_make(" page))
                      ;; vanilla JavaScript: no jQuery in anything sent
                      (ng (find-if (lambda (script) (search "$(" script)) scripts)))
                    ;; Idle past the read timeout, between frames: still open.
                    (sleep 2.5)
                    (write-sequence (client-frame 9 "still there?") stream)
                    (finish-output stream)
                    (multiple-value-bind (opcode payload) (next-control-frame stream)
                      (ok (eql 10 opcode))
                      (ok (equalp (octets "still there?") payload)))
                    (write-sequence (client-frame 8 (octets 3 232)) stream)
                    (finish-output stream)
                    (multiple-value-bind (opcode payload) (next-control-frame stream)
                      (ok (eql 8 opcode))
                      (ok (equalp (octets 3 232) payload)))
                    (ok (null (next-control-frame stream)) "and then the socket is closed"))
               (usocket:socket-close socket))))))
