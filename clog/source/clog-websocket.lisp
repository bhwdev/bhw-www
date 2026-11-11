;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) David Botton                                                      ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-websocket.lisp                                                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The server side of RFC 6455 websockets, on Hunchentoot, in place of
;;; websocket-driver (whose server side runs on Clack's socket protocol).
;;; Only what CLOG's boot.js uses: no extensions or subprotocols; text
;;; messages are delivered, binary ones read and dropped.
;;;
;;; A websocket lives in the Hunchentoot thread whose request opened it:
;;; WEBSOCKET-UPGRADE answers the handshake with 101 and SERVE-WEBSOCKET reads
;;; frames until the browser closes, after which Hunchentoot closes the
;;; socket.  Any thread may send (WEBSOCKET-SEND); a lock orders the frames.

(in-package :clog-connection)

(defvar *websocket-idle-timeout* 120
  "Seconds a websocket may go without a frame from the browser before it is
closed.  boot.js pings every 10 seconds, but a browser may slow a background
tab's timers to one a minute.  Checked when a read times out, so in steps of
the acceptor's read timeout (Hunchentoot's default is 20 seconds); with no
read timeout the socket waits for the browser indefinitely.")

(defvar *websocket-max-message-octets* (* 16 1024 1024)
  "The longest message, in octets, a websocket accepts from the browser: a
longer one closes it with status 1009.  CLOG's are script results and event
data, usually far shorter.")

(defparameter +websocket-guid+ "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"
  "The GUID RFC 6455 has the server append to Sec-WebSocket-Key.")

(defvar *websocket-handshake* nil
  "True while WEBSOCKET-UPGRADE sends its 101 response, which must not be
chunked.  (Private)")

(define-condition websocket-error (error)
  ((code :initarg :code :reader websocket-error-code)
   (reason :initarg :reason :reader websocket-error-reason))
  (:report (lambda (condition stream)
             (format stream "Websocket error ~D: ~A"
                     (websocket-error-code condition)
                     (websocket-error-reason condition))))
  (:documentation "A violation of RFC 6455 by the browser, with the status
code to close the connection with.  (Private)"))

(defun websocket-fail (code reason)
  (error 'websocket-error :code code :reason reason))

(defclass websocket ()
  ((stream :initarg :stream :reader websocket-stream)
   (lock :initform (bordeaux-threads:make-lock "clog-websocket")
         :reader websocket-lock)
   (state :initform :open :accessor websocket-state
          :documentation ":OPEN, :CLOSING once this end has sent its close
frame, :CLOSED."))
  (:documentation "The server's end of a browser's websocket connection."))

(defmethod print-object ((websocket websocket) stream)
  (print-unreadable-object (websocket stream :type t :identity t)
    (princ (websocket-state websocket) stream)))

;;;;;;;;;;;;;;;
;; Handshake ;;
;;;;;;;;;;;;;;;

(defun websocket-accept (key)
  "The Sec-WebSocket-Accept value answering the Sec-WebSocket-Key KEY."
  (cl-base64:usb8-array-to-base64-string
   (ironclad:digest-sequence
    :sha1 (flex:string-to-octets (concatenate 'string key +websocket-guid+)
                                 :external-format :latin-1))))

(defun header-token-p (token value)
  "True when VALUE, a comma-separated header value or NIL, lists TOKEN,
ignoring case."
  (and value
       (member token (ppcre:split "\\s*,\\s*" (string-trim " " value))
               :test #'string-equal)
       t))

(defun websocket-key-p (key)
  "True when KEY is base64 for 16 octets, as RFC 6455 has Sec-WebSocket-Key."
  (and key
       (= 16 (length (or (ignore-errors (cl-base64:base64-string-to-usb8-array key))
                         #())))))

(defun websocket-upgrade (request)
  "Answer REQUEST, the opening handshake of a websocket, and return the
WEBSOCKET: 101 Switching Protocols, sent at once.  A request that is not a
version 13 handshake gets 400 (426 for another version) and NIL is returned."
  (let ((key (hunchentoot:header-in :sec-websocket-key request)))
    (cond ((not (and (eq :get (hunchentoot:request-method request))
                     (header-token-p "websocket" (hunchentoot:header-in :upgrade request))
                     (header-token-p "upgrade" (hunchentoot:header-in :connection request))
                     (websocket-key-p key)))
           (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+)
           nil)
          ((not (equal "13" (hunchentoot:header-in :sec-websocket-version request)))
           (setf (hunchentoot:return-code*) 426
                 (hunchentoot:header-out :sec-websocket-version) "13")
           nil)
          (t
           (setf (hunchentoot:return-code*) hunchentoot:+http-switching-protocols+
                 (hunchentoot:content-type*) nil
                 (hunchentoot:header-out :upgrade) "websocket"
                 (hunchentoot:header-out :connection) "Upgrade"
                 (hunchentoot:header-out :sec-websocket-accept) (websocket-accept key))
           ;; Without a Content-Length, Hunchentoot would chunk the response
           ;; and everything written after it; *WEBSOCKET-HANDSHAKE* stops
           ;; that (clog-connection-websockets.lisp).
           ;; With neither, it does not keep the connection alive, so it
           ;; closes the socket once SERVE-WEBSOCKET returns.
           (let ((stream (let ((*websocket-handshake* t))
                           (hunchentoot:send-headers))))
             (finish-output stream)
             (make-instance 'websocket :stream stream))))))

;;;;;;;;;;;;
;; Frames ;;
;;;;;;;;;;;;

(defun read-octets (stream count)
  "A fresh vector of the next COUNT octets of STREAM; END-OF-FILE before
then."
  (let* ((octets (make-array count :element-type '(unsigned-byte 8)))
         (end (read-sequence octets stream)))
    (when (< end count)
      (error 'end-of-file :stream stream))
    octets))

(defun read-unsigned (stream count)
  "The big-endian unsigned integer in the next COUNT octets of STREAM."
  (loop with value = 0
        for octet across (read-octets stream count)
        do (setf value (logior (ash value 8) octet))
        finally (return value)))

(defun read-frame (stream first-octet max-octets)
  "Read the frame from the browser whose first octet, FIRST-OCTET, was read
from STREAM, and return its opcode, its unmasked payload and whether it is
final.  WEBSOCKET-ERROR for a frame RFC 6455 does not allow a client to send
here, or with a payload longer than MAX-OCTETS."
  (let* ((second-octet (read-unsigned stream 1))
         (fin (logbitp 7 first-octet))
         (opcode (ldb (byte 4 0) first-octet))
         (length (ldb (byte 7 0) second-octet)))
    (unless (zerop (ldb (byte 3 4) first-octet))
      (websocket-fail 1002 "reserved bits set without an extension"))
    (unless (member opcode '(0 1 2 8 9 10))
      (websocket-fail 1002 "unknown opcode"))
    (unless (logbitp 7 second-octet)
      (websocket-fail 1002 "unmasked frame from the client"))
    (setf length (case length
                   (126 (read-unsigned stream 2))
                   (127 (read-unsigned stream 8))
                   (t length)))
    (when (>= opcode 8)
      (unless fin
        (websocket-fail 1002 "fragmented control frame"))
      (when (> length 125)
        (websocket-fail 1002 "control frame longer than 125 octets")))
    (when (> length max-octets)
      (websocket-fail 1009 "message too long"))
    (let ((mask (read-octets stream 4))
          (payload (read-octets stream length)))
      (declare (type (simple-array (unsigned-byte 8) (*)) mask payload))
      (dotimes (i (length payload))
        (setf (aref payload i) (logxor (aref payload i) (aref mask (logand i 3)))))
      (values opcode payload fin))))

(defun write-frame (stream opcode payload)
  "Write PAYLOAD, a vector of octets, to STREAM as one final, unmasked frame
of OPCODE, as a server sends them, and flush it."
  (let ((length (length payload)))
    (write-byte (logior #x80 opcode) stream)
    (flet ((write-unsigned (value count)
             (loop for shift from (* 8 (1- count)) downto 0 by 8
                   do (write-byte (ldb (byte 8 shift) value) stream))))
      (cond ((< length 126)
             (write-byte length stream))
            ((< length 65536)
             (write-byte 126 stream)
             (write-unsigned length 2))
            (t
             (write-byte 127 stream)
             (write-unsigned length 8))))
    (write-sequence payload stream)
    (finish-output stream)))

(defun close-payload (code reason)
  "The payload of a close frame with status CODE and REASON."
  (let ((reason (flex:string-to-octets reason :external-format :utf-8)))
    (concatenate '(vector (unsigned-byte 8))
                 (vector (ldb (byte 8 8) code) (ldb (byte 8 0) code))
                 (subseq reason 0 (min (length reason) 123)))))

(defun utf-8-text (octets)
  "OCTETS decoded as UTF-8; WEBSOCKET-ERROR 1007 if they are not UTF-8."
  (handler-case (let ((flex:*substitution-char* nil))
                  (flex:octets-to-string octets :external-format :utf-8))
    (error ()
      (websocket-fail 1007 "text that is not UTF-8"))))

;;;;;;;;;;;;;;;;;;;;
;; The connection ;;
;;;;;;;;;;;;;;;;;;;;

(defun websocket-send (websocket string)
  "Send STRING to the browser as a text message.  True if it was written;
NIL when the websocket is closing or closed or the write failed, which marks
it closed."
  (bordeaux-threads:with-lock-held ((websocket-lock websocket))
    (when (eq :open (websocket-state websocket))
      (handler-case
          (progn
            (write-frame (websocket-stream websocket) 1
                         (flex:string-to-octets string :external-format :utf-8))
            t)
        (error ()
          (setf (websocket-state websocket) :closed)
          nil)))))

(defun websocket-close (websocket &optional (code 1000) (reason ""))
  "Start closing WEBSOCKET: send the browser a close frame with status CODE
and REASON, unless one was sent already.  Its reading thread ends when the
browser answers, or when a read times out first."
  (bordeaux-threads:with-lock-held ((websocket-lock websocket))
    (when (eq :open (websocket-state websocket))
      (setf (websocket-state websocket) :closing)
      (ignore-errors
       (write-frame (websocket-stream websocket) 8 (close-payload code reason))))))

(defun read-first-octet (websocket)
  "The first octet of the next frame on WEBSOCKET, or NIL at the end of the
stream.  A read that times out here, between frames, is tried again until
the websocket has been idle for *WEBSOCKET-IDLE-TIMEOUT* seconds or this end
is closing, and then NIL is returned."
  (let ((stream (websocket-stream websocket))
        (since (get-universal-time)))
    (loop
      (handler-case (return (read-byte stream nil nil))
        (#+sbcl sb-sys:io-timeout #-sbcl usocket:timeout-error ()
          (when (or (not (eq :open (websocket-state websocket)))
                    (> (- (get-universal-time) since) *websocket-idle-timeout*))
            (return nil)))))))

(defun read-message (websocket)
  "Read WEBSOCKET's next message and return :TEXT and the string, :BINARY
and the octets, or :CLOSE once the browser has closed the connection (its
close frame answered) or gone.  Pings are answered and pongs ignored on the
way."
  (let ((stream (websocket-stream websocket))
        (kind nil)
        (parts '())
        (size 0))
    (loop
      (let ((first-octet (read-first-octet websocket)))
        (unless first-octet
          (return :close))
        (multiple-value-bind (opcode payload fin)
            (read-frame stream first-octet (- *websocket-max-message-octets* size))
          (case opcode
            (8
             (when (= 1 (length payload))
               (websocket-fail 1002 "close frame with a one-octet payload"))
             ;; Answer with the browser's status, if it is one a close
             ;; frame may carry.
             (let ((code (if (>= (length payload) 2)
                             (logior (ash (aref payload 0) 8) (aref payload 1))
                             1000)))
               (websocket-close websocket
                                (if (and (<= 1000 code 4999)
                                         (not (member code '(1004 1005 1006 1015))))
                                    code
                                    1002)
                                ""))
             (return :close))
            (9
             (bordeaux-threads:with-lock-held ((websocket-lock websocket))
               (when (eq :open (websocket-state websocket))
                 (write-frame stream 10 payload))))
            (10)
            (t
             (cond ((zerop opcode)
                    (unless kind
                      (websocket-fail 1002 "continuation frame outside a message")))
                   (kind
                    (websocket-fail 1002 "new message inside a fragmented one"))
                   (t
                    (setf kind (if (= opcode 1) :text :binary))))
             (push payload parts)
             (incf size (length payload))
             (when fin
               (let ((octets (apply #'concatenate '(vector (unsigned-byte 8))
                                    (nreverse parts))))
                 (return (if (eq kind :text)
                             (values :text (utf-8-text octets))
                             (values :binary octets))))))))))))

(defun serve-websocket (websocket on-open on-message on-close)
  "Serve WEBSOCKET, in the thread of the request that opened it, until it
closes: call ON-OPEN with it, ON-MESSAGE with it and each text message while
this end is open, and ON-CLOSE with it at the end, however it comes."
  (unwind-protect
       (handler-case
           (progn
             (funcall on-open websocket)
             (loop
               (multiple-value-bind (kind data) (read-message websocket)
                 (case kind
                   (:close (return))
                   (:text (when (eq :open (websocket-state websocket))
                            (funcall on-message websocket data)))))))
         (websocket-error (condition)
           (websocket-close websocket
                            (websocket-error-code condition)
                            (websocket-error-reason condition)))
         ;; The browser went away mid-frame, or the socket failed.
         (end-of-file ())
         (stream-error ()))
    (setf (websocket-state websocket) :closed)
    (funcall on-close websocket)))
