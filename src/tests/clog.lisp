;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:bhw/tests/clog
  (:use #:common-lisp)
  (:import-from #:alexandria)
  (:import-from #:bordeaux-threads)
  (:import-from #:cl-ppcre)
  (:import-from #:hunchentoot)
  (:import-from #:clog)
  (:import-from #:flexi-streams)
  (:import-from #:rove #:deftest #:ok #:ng #:testing)
  (:import-from #:usocket)
  (:local-nicknames (#:conn #:clog-connection))
  (:documentation
   "Tests for the fork of CLOG in clog/: its websockets (RFC 6455), checked
against a client side written here apart from the server's, and its server on
Hunchentoot, through a whole CLOG session over a socket."))

(in-package #:bhw/tests/clog)

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
  (let ((root (asdf:system-relative-pathname :bhw "")))
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

;;; Connections, without a socket: an in-memory websocket stands for the
;;; page's.

(defmacro with-connection ((var) &body body)
  "Run BODY with VAR a new connection, closed afterwards."
  `(let ((,var (conn::make-connection nil (list :path "/test"))))
     (unwind-protect (progn ,@body)
       (conn::close-connection ,var))))

(defun sent-texts (out)
  "The text messages the server wrote to OUT since this was last called."
  (loop for (opcode payload) in (sent-frames out)
        when (= opcode 1)
          collect (flexi-streams:octets-to-string payload :external-format :utf-8)))

(defun wait-until (predicate &optional (seconds 5))
  (loop repeat (* 100 seconds)
        until (funcall predicate)
        do (sleep 0.01))
  (funcall predicate))

(deftest messages-to-the-page
  ;; Each message is a JSON array of scripts.
  (ok (equal "[\"a\",\"b\\\"c\\\\d\"]" (conn::message-text (list "a" "b\"c\\d"))))
  (ok (equal "[\"\\u000A\\uD800\"]"
             (conn::message-text (list (coerce (list #\Newline (code-char #xD800)) 'string))))))

(deftest queries-are-the-connections-own
  (with-connection (a)
    (with-connection (b)
      (multiple-value-bind (websocket out) (in-memory-websocket)
        (setf (conn::connection-websocket a) websocket)
        (let* ((result :none)
               (thread (bordeaux-threads:make-thread
                        (lambda ()
                          (setf result (conn:query (conn::connection-id a) "1+1"
                                                   :default-answer "late" :timeout 5))))))
          (ok (wait-until (lambda () (plusp (hash-table-count (conn::connection-queries a))))))
          (let ((id (first (alexandria:hash-table-keys (conn::connection-queries a)))))
            (ok (search (format nil "clog_answer(~D," id) (first (sent-texts out))))
            ;; Another page answering it, or answering what nobody asked:
            ;; ignored, and nothing kept.
            (conn::receive b nil (format nil "~D:from b" id))
            (conn::receive b nil "123456789:nobody asked")
            (ok (zerop (hash-table-count (conn::connection-queries b))))
            (sleep 0.05)
            (ok (eq :none result))
            (conn::receive a websocket (format nil "~D:2" id))
            (bordeaux-threads:join-thread thread)
            (ok (equal "2" result))
            (ok (zerop (hash-table-count (conn::connection-queries a)))))))
      (testing "a script that fails in the browser gives the default at once"
        (let* ((result :none)
               (thread (bordeaux-threads:make-thread
                        (lambda ()
                          (setf result (conn:query (conn::connection-id b) "x("
                                                   :default-answer "failed" :timeout 5))))))
          (ok (wait-until (lambda () (plusp (hash-table-count (conn::connection-queries b))))))
          (let ((id (first (alexandria:hash-table-keys (conn::connection-queries b)))))
            (conn::receive b nil (format nil "~D!SyntaxError" id)))
          (bordeaux-threads:join-thread thread)
          (ok (equal "failed" result)))))))

(deftest a-connections-thread
  (with-connection (c)
    (multiple-value-bind (websocket out) (in-memory-websocket)
      (setf (conn::connection-websocket c) websocket)
      (let ((id (conn::connection-id c))
            (log '())
            (busy nil)
            (overlap nil)
            (threads '()))
        (dotimes (i 20)
          (let ((i i))
            (conn:post id (lambda ()
                            (when busy (setf overlap t))
                            (setf busy t)
                            (push conn::*connection* threads)
                            (sleep 0.001)
                            (push i log)
                            (conn:execute id (format nil "a~D" i))
                            (conn:execute id (format nil "b~D" i))
                            (setf busy nil)))))
        (ok (wait-until (lambda () (= 20 (length log)))))
        (sleep 0.05)
        (ok (equal (alexandria:iota 20) (reverse log)) "in the order posted")
        (ng overlap "one at a time")
        (ok (every (lambda (thread) (eq thread c)) threads) "in the connection's thread")
        (let ((texts (sent-texts out)))
          (ok (= 20 (length texts)))
          (ok (equal "[\"a0\",\"b0\"]" (first texts))
              "what a function sends goes as one message"))))))

(deftest replay-on-reconnecting
  (with-connection (c)
    (let ((id (conn::connection-id c)))
      ;; Sent while the page is away: kept.
      (conn:execute id "one")
      (conn:execute id "two")
      (conn:execute id "three")
      (conn::acknowledge c 1)
      (multiple-value-bind (websocket out) (in-memory-websocket)
        (ok (conn::attach c websocket 2))
        (ok (equal '("[\"three\"]") (sent-texts out)) "what the page had not run"))
      (ng (conn::attach c (in-memory-websocket) 4) "a page that ran more than was sent")
      (let ((conn:*replay-size* 30))
        (conn:execute id (make-string 20 :initial-element #\x))
        (conn:execute id (make-string 20 :initial-element #\y))
        (ng (conn::attach c (in-memory-websocket) 3) "messages dropped before it ran them")
        (ok (conn::attach c (in-memory-websocket) 4))))))

(deftest leaving-and-dropping
  (let ((conn:*reconnect-delay* 0.2))
    (with-connection (c)
      (let ((id (conn::connection-id c))
            (websocket (in-memory-websocket))
            (closed nil))
        (setf (conn::connection-websocket c) websocket)
        (conn:add-close-hook id (lambda () (setf closed conn::*connection*)))
        (testing "a socket that drops: the page may come back"
          (conn::websocket-ended c websocket nil)
          (ok (eq :detached (conn::connection-state c)))
          (ok (conn:validp id))
          (let ((again (in-memory-websocket)))
            (ok (conn::attach c again 0))
            (ok (eq :open (conn::connection-state c)))
            (sleep 0.3)
            (ok (conn:validp id) "and is not dropped once it has")
            (testing "a page that leaves is closed at once"
              (conn::websocket-ended c again 1001)
              (ng (conn:validp id))
              (ok (wait-until (lambda () closed)))
              (ok (eq c closed) "its close hooks run in its thread"))))))
    (with-connection (c)
      (let ((websocket (in-memory-websocket)))
        (setf (conn::connection-websocket c) websocket)
        (conn::websocket-ended c websocket nil)
        (ok (wait-until (lambda () (not (conn:validp (conn::connection-id c)))) 2)
            "one that does not come back in time is dropped")))))

(deftest timers
  (with-connection (c)
    (let* ((id (conn::connection-id c))
           (count 0)
           (threads '())
           (timer (conn:schedule id 0.05 (lambda ()
                                           (incf count)
                                           (push conn::*connection* threads))
                                 :repeat t)))
      (sleep 0.33)
      (conn:cancel-timer timer)
      ;; A run the timer queued before it was cancelled still runs.
      (sleep 0.1)
      (let ((seen count))
        (ok (<= 2 seen 9) (format nil "ran ~D times in a third of a second" seen))
        (sleep 0.15)
        (ok (= seen count) "cancelled"))
      (ok (every (lambda (thread) (eq thread c)) threads) "in the connection's thread")))
  (let ((ran nil))
    (with-connection (c)
      (conn:schedule (conn::connection-id c) 0.1 (lambda () (setf ran t))))
    (sleep 0.2)
    (ng ran "a closed connection's timers do not run")))

(deftest mirrors
  (with-connection (c)
    (let ((id (conn::connection-id c)))
      (conn:mirror id "CLOG5" "")
      (conn::receive c nil "M:CLOG5 hello, world")
      (conn::receive c nil "M:CLOG6 not mirrored")
      (ok (equal "hello, world" (conn:mirrored-value id "CLOG5")))
      (ng (nth-value 1 (conn:mirrored-value id "CLOG6")))
      (let ((field (make-instance 'clog:clog-form-element :connection-id id :html-id "CLOG5")))
        (ok (equal "hello, world" (clog:value field)) "VALUE reads it without asking")
        (setf (clog:value field) "set here")
        (ok (equal "set here" (conn:mirrored-value id "CLOG5")))))))

(deftest browser-gc
  ;; The ID comes from the browser: compared, not made a regex.
  (with-connection (c)
    (let* ((id (conn::connection-id c))
           (window (clog::make-clog-obj id "window"))
           (data (conn:get-connection-data id)))
      (setf (gethash "CLOG1:click" data) #'identity
            (gethash "CLOG12:click" data) #'identity)
      (clog::perform-gc window ".*")
      (clog::perform-gc window "(a+)+$")
      (ok (gethash "CLOG1:click" data))
      (clog::perform-gc window "CLOG1")
      (ng (gethash "CLOG1:click" data))
      (ok (gethash "CLOG12:click" data)))))

(deftest routes
  (let ((server (make-instance 'conn:clog-acceptor :port 0 :extended-routing t)))
    (conn:add-route "/" :root :server server)
    (conn:add-route "/plp" :plp :server server)
    (conn:add-route "/plp/console" :console :server server)
    (conn:add-route :default :default :server server)
    (ok (eq :plp (conn:find-route "/plp" :server server)))
    (ok (eq :console (conn:find-route "/plp/console/AB12" :server server))
        "the longest route a path is under")
    (ok (eq :plp (conn:find-route "/plp/screen/AB12" :server server)))
    (ok (null (conn:find-route "/other" :server server)) "/ is no other path's")
    (ok (eq :default (conn:find-route "/other/x" :server server :default t)))
    (ok (eq :root (conn:find-route "/" :server server)))))

(deftest websocket-limit
  ;; By default, from the process's limit on open files.
  (ok (typep (conn::open-files-limit) '(integer 1)))
  (let ((conn:*max-websockets* nil))
    (ok (= (min 4096 (max 64 (- (conn::open-files-limit) 512))) (conn::websocket-limit))))
  (let ((conn:*max-websockets* 7))
    (ok (= 7 (conn::websocket-limit)))
    (ok (= 7 (conn::acceptor-websocket-limit (make-instance 'conn:clog-acceptor :port 0))))))

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
  "The status, body and header lines of GET PATH, over a connection of its own."
  (let ((socket (connect)))
    (unwind-protect
         (let ((stream (usocket:socket-stream socket)))
           (send-request stream (format nil "GET ~A HTTP/1.0" path))
           (multiple-value-bind (status headers) (read-head stream)
             (values status
                     (flexi-streams:octets-to-string
                      (coerce (loop for octet = (read-byte stream nil) while octet collect octet)
                              '(vector (unsigned-byte 8)))
                      :external-format :utf-8)
                     headers)))
      (usocket:socket-close socket))))

(defun open-websocket (&key (key "dGhlIHNhbXBsZSBub25jZQ==") (version "13")
                            (query "p=/") origin)
  "A socket that has sent a websocket handshake for /clog?QUERY, and the
status and header lines of the answer."
  (let ((socket (connect)))
    (apply #'send-request (usocket:socket-stream socket)
           (format nil "GET /clog?~A HTTP/1.1" query) "Host: localhost:8294"
           "Upgrade: websocket" "Connection: keep-alive, Upgrade"
           (format nil "Sec-WebSocket-Key: ~A" key)
           (format nil "Sec-WebSocket-Version: ~A" version)
           (and origin (list (format nil "Origin: ~A" origin))))
    (multiple-value-bind (status headers) (read-head (usocket:socket-stream socket))
      (values socket status headers))))

(defun next-text (stream)
  "The next text message from the server on STREAM that is not a ping's
answer."
  (loop (multiple-value-bind (opcode payload) (read-server-frame stream)
          (cond ((null opcode) (return nil))
                ((and (= 1 opcode) (plusp (length payload)))
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

(defun connection-id-of (message)
  (let ((groups (nth-value 1 (cl-ppcre:scan-to-strings
                              "^\\[\"clog\\['connection_id'\\]='([0-9a-f]{32})'\"\\]$"
                              message))))
    (and groups (aref groups 0))))

(deftest clog-server
  ;; The server's threads see the global values.
  (let ((level conn:*log-level*)
        (delay conn:*reconnect-delay*)
        (per-address conn:*max-websockets-per-address*)
        ;; A server of this test's own, whatever else the image runs.
        (conn:*server* nil))
    (setf conn:*log-level* :error
          conn:*reconnect-delay* 30
          conn:*max-websockets-per-address* 6)
    (clog:initialize (lambda (body) (clog:create-div body :content "hello from the fork"))
                     :host "127.0.0.1" :port *port* :static-root nil
                     :acceptor-initargs
                     (list :read-timeout 1 :write-timeout 1
                           ;; Few threads, for the websockets to go past.
                           :taskmaster (make-instance
                                        'hunchentoot:one-thread-per-connection-taskmaster
                                        :max-thread-count 3 :max-accept-count 4)))
    (unwind-protect
         (clog-server-tests)
      (clog:shutdown)
      (setf conn:*log-level* level
            conn:*reconnect-delay* delay
            conn:*max-websockets-per-address* per-address))))

(defun clog-server-tests ()
  (testing "the boot page, boot.js and CLOG's own files"
    (multiple-value-bind (status body headers) (http-get "/")
      (ok (= 200 status))
      (ok (search (conn:boot-script-path) body))
      (ng (search "<script>" body) "no script in the page")
      (ok (member "Cache-Control: no-store" headers :test #'string-equal)))
    (ok (cl-ppcre:scan "^/js/boot\\.[0-9a-f]{16}\\.js$" (conn:boot-script-path)))
    (multiple-value-bind (status body headers) (http-get (conn:boot-script-path))
      (ok (= 200 status))
      (ok (search "clog_answer" body))
      (ok (member "Cache-Control: public, max-age=31536000, immutable" headers
                  :test #'string-equal)))
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
      (ok (= 400 status)))
    (multiple-value-bind (socket status) (open-websocket :origin "https://evil.example")
      (usocket:socket-close socket)
      (ok (= 403 status) "a page of another site")))
  (testing "a page built over the websocket, which reconnects"
    (let ((id nil))
      (multiple-value-bind (socket status headers) (open-websocket :origin "http://localhost:8294")
        (let ((stream (usocket:socket-stream socket)))
          (ok (= 101 status))
          (ok (member "Sec-WebSocket-Accept: s3pPLMBiTxaQ9kYGzzhZRbK+xOo=" headers
                      :test #'string-equal))
          (ng (find-if (lambda (h) (cl-ppcre:scan "(?i)^(transfer-encoding|content-length|content-type):" h))
                       headers))
          (setf id (connection-id-of (next-text stream)))
          (ok id)
          (let ((page (next-text stream)))
            (ok (search "hello from the fork" page) "the page, in one message")
            (ok (search "clog_make(" page)))
          ;; The socket drops, without a close frame.
          (usocket:socket-close socket)))
      (ok (wait-until (lambda () (eq :detached (conn::connection-state (conn::get-connection id))))))
      (multiple-value-bind (socket status) (open-websocket :query (format nil "r=~A&s=1" id))
        (let ((stream (usocket:socket-stream socket)))
          (ok (= 101 status))
          (ok (search "hello from the fork" (next-text stream)) "what the page missed")
          ;; Idle past the read timeout, between frames: still open.
          (sleep 2.5)
          (send-text stream "P:2")
          (ok (equal "" (multiple-value-bind (opcode payload) (read-server-frame stream)
                          (and (= opcode 1) (flexi-streams:octets-to-string payload))))
              "a ping is answered")
          (write-sequence (client-frame 9 "still there?") stream)
          (finish-output stream)
          (multiple-value-bind (opcode payload) (next-control-frame stream)
            (ok (eql 10 opcode))
            (ok (equalp (octets "still there?") payload)))
          ;; The page leaves: 1001.
          (write-sequence (client-frame 8 (octets 3 233)) stream)
          (finish-output stream)
          (multiple-value-bind (opcode payload) (next-control-frame stream)
            (ok (eql 8 opcode))
            (ok (equalp (octets 3 233) payload)))
          (ok (null (next-control-frame stream)) "and then the socket is closed")
          (usocket:socket-close socket)))
      (ok (wait-until (lambda () (not (conn:validp id)))) "a page that left is closed")
      (multiple-value-bind (socket status) (open-websocket :query (format nil "r=~A&s=2" id))
        (let ((stream (usocket:socket-stream socket)))
          (ok (= 101 status))
          (multiple-value-bind (opcode payload) (next-control-frame stream)
            (ok (eql 8 opcode))
            (ok (and payload (= 4000 (logior (ash (aref payload 0) 8) (aref payload 1))))
                "a page the server no longer knows is told to reload"))
          (usocket:socket-close socket)))))
  (testing "a page with the boot.js of before is told to reload, as it understands"
    (multiple-value-bind (socket status) (open-websocket :query "r=0123456789abcdef0123456789abcdef")
      (let ((stream (usocket:socket-stream socket)))
        (ok (= 101 status))
        (ok (equal "location.reload()" (next-text stream)))
        (usocket:socket-close socket))))
  (testing "websockets do not take the HTTP threads, but have limits"
    ;; Three threads for the HTTP requests; six websockets from one address.
    (ok (wait-until (lambda () (zerop (conn::acceptor-websocket-count conn:*server*)))))
    (let ((sockets (loop repeat 6 collect (open-websocket))))
      (unwind-protect
           (progn
             (ok (= 200 (http-get "/")) "HTTP is answered")
             (multiple-value-bind (socket status) (open-websocket)
               (usocket:socket-close socket)
               (ok (= 503 status) "one websocket too many from an address")))
        (mapc #'usocket:socket-close sockets)))))
