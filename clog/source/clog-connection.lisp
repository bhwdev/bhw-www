;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2025 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-connection.lisp                                                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The connection between Lisp and a page in a browser.
;;;
;;; clog <-> clog-connection (this file: connections, their threads, what they
;;;   send and are asked) <-> clog-connection-websockets.lisp (the web server)
;;;   <-> clog-websocket.lisp (RFC 6455) <-> Hunchentoot <-> boot.js
;;;
;;; A connection is a page, and outlives its websocket.  When the socket drops
;;; the page reconnects, saying how many messages it has run, and is sent
;;; again those it missed; a page the server no longer knows is reloaded.
;;;
;;; Everything Lisp does for a page -- its ON-NEW-WINDOW handler, its event
;;; handlers, what other threads POST to it, its timers -- runs one at a time,
;;; in order, in a thread of the connection's own (started when there is
;;; something to run, ended when there is not), and what each sends the
;;; browser goes out as one message when it returns.
;;;
;;; The protocol.  To the browser: each message is a JSON array of scripts,
;;; run in order; the empty message answers a ping and is not counted.  From
;;; the browser: "E:HTML-ID:EVENT DATA" an event, "M:HTML-ID VALUE" the value
;;; of a mirrored form element, "P:N" a ping saying it has run N messages,
;;; "ID:VALUE" or "ID!ERROR" the answer to query ID.

(defpackage #:clog-connection
  (:documentation "The Common List Omnificent GUI - Connection")
  (:use #:cl)
  (:export
   ;;; Settings
   #:*break-on-error*
   #:*disable-clog-debugging*
   #:*reconnect-delay*
   #:*query-time-out*
   #:*replay-size*
   #:*mailbox-limit*
   #:*log-level*
   #:*log-function*
   #:log-message
   ;;; The web server (clog-connection-websockets.lisp)
   #:clog-acceptor
   #:*server*
   #:initialize
   #:shutdown-clog
   #:random-port
   #:add-route
   #:find-route
   #:add-plugin-path
   #:serve-static-file
   #:file-content-type
   #:static-files-directory
   #:boot-script-path
   #:compiled-boot-html
   #:*websocket-idle-timeout*
   #:*websocket-max-message-octets*
   #:*max-websockets*
   #:*max-websockets-per-address*
   ;;; Connections
   #:validp
   #:get-connection-data
   #:connection-request
   #:post
   #:flush
   #:schedule
   #:cancel-timer
   #:add-close-hook
   #:mirror
   #:mirrored-value
   #:execute
   #:query
   #:cclose
   #:shutdown
   #:alert-box
   #:debug-mode
   ;;; Utilities
   #:escape-string
   #:generate-id
   #:random-hex-string
   #:make-hash-table*))

(in-package :clog-connection)

;; clog-websocket.lisp, which needs this package, defines them.
(declaim (ftype function websocket-send websocket-close))

;;;;;;;;;;;;;;
;; Settings ;;
;;;;;;;;;;;;;;

(defvar *break-on-error* nil
  "When true, an error in a page's handler enters the debugger, in the
connection's thread, and the page's other events wait for it.  NIL (the
default): the error is logged with its backtrace and the next event runs.
Set it at a REPL to debug; in an executable without a debugger, an error that
reaches the debugger ends the process.")

(defvar *disable-clog-debugging* nil
  "When true, the debug hooks of CLOG:*CLOG-DEBUG* and CLOG-GUI are not used.")

(defvar *reconnect-delay* 60
  "Seconds a connection whose websocket dropped is kept for its page to
reconnect; NIL drops it at once.  A page that closes its socket itself, as a
browser does on leaving the page, is dropped at once.")

(defvar *query-time-out* 3
  "Seconds QUERY waits for the browser's answer, by default.")

(defvar *replay-size* (* 1024 1024)
  "The most characters of messages a connection keeps, of those its page has
not acknowledged, to send them again when it reconnects.  Past it the oldest
go, and a page that had not run one of them is reloaded when it reconnects.")

(defvar *mailbox-limit* 1000
  "The most events and posted functions that may wait for a connection's
thread; more are dropped.")

;;;;;;;;;;;;;
;; Logging ;;
;;;;;;;;;;;;;

(defvar *log-level* :info
  "The least severe level LOG-MESSAGE logs: :DEBUG, :INFO, :WARNING or :ERROR.")

(defvar *log-function* 'write-log-line
  "The function LOG-MESSAGE calls with the level and the text of a line.")

(defparameter +log-levels+ '(:debug :info :warning :error))

(defun write-log-line (level text)
  "Write TEXT and its LEVEL as a line of *STANDARD-OUTPUT*."
  (format *standard-output* "~&CLOG ~(~A~): ~A~%" level text)
  (finish-output *standard-output*))

(defun log-message (level control &rest arguments)
  "Log (FORMAT NIL CONTROL ARGUMENTS) at LEVEL, if *LOG-LEVEL* lets it."
  (when (>= (or (position level +log-levels+) 3)
            (or (position *log-level* +log-levels+) 1))
    (funcall *log-function* level (apply #'format nil control arguments))))

(defun call-logging-errors (function control &rest arguments)
  "Call FUNCTION and return its values.  A serious condition it signals is
logged with a backtrace, as from (FORMAT NIL CONTROL ARGUMENTS), and NIL is
returned -- unless *BREAK-ON-ERROR*, which lets it reach the debugger."
  (block call
    (handler-bind ((serious-condition
                     (lambda (condition)
                       (unless *break-on-error*
                         (log-message :error "~? - ~A~%~A" control arguments
                                      (or (ignore-errors (princ-to-string condition))
                                          (type-of condition))
                                      (or (ignore-errors
                                           (with-output-to-string (out)
                                             (uiop:print-backtrace :stream out :count 40)))
                                          ""))
                         (return-from call nil)))))
      (funcall function))))

;;;;;;;;;;;;;;;
;; Utilities ;;
;;;;;;;;;;;;;;;

(defun make-hash-table* (&rest args)
  "Use native concurrent hash tables"
  ;; This covers sbcl ecl mazzano lw and ccl.
  ;; (lw and ccl default hash is synchronized)
  #+(or sbcl ecl mezzano)
  (apply #'make-hash-table :synchronized t args)
  #-(or sbcl ecl mezzano) (apply #'make-hash-table args))

(defvar *new-id* 0 "Last issued script ID")
(defvar *new-id-lock* (bordeaux-threads:make-lock "clog-new-id"))

(defun generate-id ()
  "Generate unique ids for use in scripts."
  (bordeaux-threads:with-lock-held (*new-id-lock*)
    (incf *new-id*)))

(defun random-hex-string ()
  "Generate cryptographic grade random ids for use in connections: 128 bits
from Ironclad's PRNG, which reads the operating system's.  (Upstream draws
from a shared cl-isaac context everywhere but Windows: no lock guards it from
concurrent connections, a saved image restores its state and so repeats its
IDs, and cl-isaac 1.0.8's RAND64 wedges at the end of each 256-word block.)"
  (ironclad:byte-array-to-hex-string (ironclad:random-data 16)))

(defun random-port (&key (host "0.0.0.0"))
  "Return a random open port on host"
  (let* ((l (usocket:socket-listen host 0))
         (p (usocket:get-local-port l)))
    (usocket:socket-close l)
    p))

(defun escape-string (str &key (no-nil nil) (html nil))
  "Escape STR for sending to browser script. If no-nil is t (default is nil)
if str is NIL returns empty string otherwise returns nil. If html is t the
quotes are changed to html entities and \n and \r are eliminated. Escape
string is used for wire readiness i.e. ability to be evaluated client side
and not for security purposes or html escapes."
  (if (and (not str) (not no-nil))
      nil
      (let ((res))
        (setf res (format nil "~@[~A~]" str))
        (setf res (ppcre:regex-replace-all "\\x5C" res "\\x5C")) ; \
        (cond (html
               (setf res (ppcre:regex-replace-all "\\x22" res "&#x22;")) ; "
               (setf res (ppcre:regex-replace-all "\\x27" res "&#x27;")) ; '
               (setf res (ppcre:regex-replace-all "\\x0A" res "&#x0A;")) ; \n
               (setf res (ppcre:regex-replace-all "\\x0D" res "&#x0D"))) ; \r
              (t
               (setf res (ppcre:regex-replace-all "\\x22" res "\\x22")) ; "
               (setf res (ppcre:regex-replace-all "\\x27" res "\\x27")) ; '
               (setf res (ppcre:regex-replace-all "\\x0A" res "\\x0A")) ; \n
               (setf res (ppcre:regex-replace-all "\\x0D" res "\\x0D")))) ; \r
        res)))

(defun write-json-string (string stream)
  "Write STRING to STREAM as a JSON string: quotes, backslashes, control
characters and surrogate code points escaped (a lone surrogate written as
UTF-8 would make the message invalid, and the browser would drop the
websocket)."
  (write-char #\" stream)
  (loop for char across string
        for code = (char-code char)
        do (cond ((char= char #\") (write-string "\\\"" stream))
                 ((char= char #\\) (write-string "\\\\" stream))
                 ((or (< code 32) (<= #xD800 code #xDFFF))
                  (format stream "\\u~4,'0X" code))
                 (t (write-char char stream))))
  (write-char #\" stream))

(defun message-text (scripts)
  "The text of the message that runs SCRIPTS: a JSON array of them."
  (with-output-to-string (out)
    (write-char #\[ out)
    (loop for (script . more) on scripts
          do (write-json-string script out)
             (when more (write-char #\, out)))
    (write-char #\] out)))

(defstruct (queue (:constructor make-queue ()))
  "A first-in first-out list."
  (head '())
  (tail '())
  (length 0))

(defun enqueue (item queue)
  (let ((cell (list item)))
    (if (queue-head queue)
        (setf (cdr (queue-tail queue)) cell)
        (setf (queue-head queue) cell))
    (setf (queue-tail queue) cell)
    (incf (queue-length queue))
    item))

(defun dequeue (queue)
  "The oldest item of QUEUE, taken from it; NIL when it is empty."
  (let ((cell (queue-head queue)))
    (when cell
      (setf (queue-head queue) (cdr cell))
      (unless (cdr cell)
        (setf (queue-tail queue) nil))
      (decf (queue-length queue))
      (car cell))))

(defun clear-queue (queue)
  (setf (queue-head queue) '()
        (queue-tail queue) '()
        (queue-length queue) 0))

(defun now ()
  "Seconds, from an arbitrary start."
  (/ (get-internal-real-time) (float internal-time-units-per-second 1d0)))

;;;;;;;;;;;;;;;;;
;; Connections ;;
;;;;;;;;;;;;;;;;;

(defvar *connections* (make-hash-table* :test #'equal)
  "The open connections by their IDs.")

(defvar *serial* 0
  "The serial number of the last connection made, which the log names it by:
its ID lets a page take it over, and is not logged.")

(defclass connection ()
  ((id :initarg :id :reader connection-id)
   (serial :initarg :serial :reader connection-serial)
   (server :initarg :server :reader connection-server)
   (request :initarg :request :accessor connection-request-plist
            :documentation "What the page told when it connected: see
CONNECTION-REQUEST.")
   (data :initform (make-hash-table* :test #'equal) :reader connection-data
         :documentation "CLOG's connection data: event handlers by
\"HTML-ID:EVENT\", and items the application keeps.")
   (queries :initform (make-hash-table* :test #'eql) :reader connection-queries
            :documentation "The queries waiting for the browser, by ID.")
   (mirrors :initform (make-hash-table* :test #'equal) :reader connection-mirrors
            :documentation "The values of mirrored form elements, by HTML ID.")
   ;; Under LOCK.
   (lock :initform (bordeaux-threads:make-lock "clog-connection")
         :reader connection-lock)
   (state :initform :open :accessor connection-state
          :documentation ":OPEN with a websocket, :DETACHED waiting for its page
to reconnect, :CLOSING running its close hooks, :CLOSED.")
   (mailbox :initform (make-queue) :reader connection-mailbox)
   (running :initform nil :accessor connection-running
            :documentation "True while the connection has a thread.")
   (overflowed :initform nil :accessor connection-overflowed
               :documentation "True once its mailbox has been full.")
   (timers :initform '() :accessor connection-timers)
   (close-hooks :initform '() :accessor connection-close-hooks)
   (detach-timer :initform nil :accessor connection-detach-timer)
   ;; Under SEND-LOCK, which is taken before LOCK when both are.
   (send-lock :initform (bordeaux-threads:make-lock "clog-connection-send")
              :reader connection-send-lock)
   (websocket :initform nil :accessor connection-websocket)
   (sent :initform 0 :accessor connection-sent
         :documentation "How many messages have been sent, or kept to send.")
   (replay :initform (make-queue) :reader connection-replay
           :documentation "(N . TEXT) of each message N the page has not
acknowledged, oldest first.")
   (replay-size :initform 0 :accessor connection-replay-size)
   (acknowledged :initform 0 :accessor connection-acknowledged)
   (lost :initform 0 :accessor connection-lost
         :documentation "The last message dropped from REPLAY before its page
acknowledged it.")))

(defmethod print-object ((connection connection) stream)
  (print-unreadable-object (connection stream :type t)
    (format stream "~D ~(~A~)" (connection-serial connection) (connection-state connection))))

(defun make-connection (server request)
  "A new connection, registered, for a page that told REQUEST."
  (let ((connection (make-instance 'connection
                                   :id (random-hex-string)
                                   :serial (bordeaux-threads:with-lock-held (*new-id-lock*)
                                             (incf *serial*))
                                   :server server
                                   :request request)))
    (setf (gethash "connection-id" (connection-data connection)) (connection-id connection)
          (gethash (connection-id connection) *connections*) connection)
    connection))

(defun get-connection (connection-id)
  "The open connection CONNECTION-ID names, or NIL."
  (and connection-id (gethash connection-id *connections*)))

(defun live-p (connection)
  (member (connection-state connection) '(:open :detached)))

(defun validp (connection-id)
  "True while CONNECTION-ID names a connection that has not closed: its page
is there, or may reconnect."
  (let ((connection (get-connection connection-id)))
    (and connection (live-p connection) t)))

(defun get-connection-data (connection-id)
  "The connection data, a hash table (test EQUAL), of CONNECTION-ID."
  (let ((connection (get-connection connection-id)))
    (and connection (connection-data connection))))

(defun connection-request (connection-id)
  "What CONNECTION-ID's page told when it connected, as a plist: :PATH and
:QUERY-STRING of its address; :HEADERS and :COOKIES, a-lists of the request
that opened its websocket (header names in lower case); :REMOTE-ADDRESS;
:TIMEZONE-OFFSET (minutes behind UTC, as JavaScript's getTimezoneOffset),
:VIEWPORT-WIDTH, :VIEWPORT-HEIGHT, :PIXEL-RATIO (a string) and :LANGUAGE of
the browser; and :POST-DATA or :MULTIPART-DATA, what a form posted to the
page."
  (let ((connection (get-connection connection-id)))
    (and connection (connection-request-plist connection))))

(defun (setf connection-request) (plist connection-id)
  (let ((connection (get-connection connection-id)))
    (when connection
      (setf (connection-request-plist connection) plist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The connection's thread (POST)  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *connection* nil
  "In a connection's thread, the connection.")

(defvar *batch* '()
  "In a connection's thread, the scripts the function running has sent,
newest first, to go out as one message when it returns.")

(defun enqueue-function (connection function &key force)
  "Have CONNECTION's thread run FUNCTION after what it has to run already,
starting the thread if it has none.  True if queued: not once the connection
is closed, nor past *MAILBOX-LIMIT* unless FORCE."
  (let ((start nil))
    (bordeaux-threads:with-lock-held ((connection-lock connection))
      (when (or (eq (connection-state connection) :closed)
                (and (not force)
                     (or (not (live-p connection))
                         (>= (queue-length (connection-mailbox connection)) *mailbox-limit*))))
        (when (and (live-p connection) (not (connection-overflowed connection)))
          (setf (connection-overflowed connection) t)
          (log-message :warning "Connection ~D: more than ~D events waiting; dropping them"
                       (connection-serial connection) *mailbox-limit*))
        (return-from enqueue-function nil))
      (enqueue function (connection-mailbox connection))
      (unless (connection-running connection)
        (setf (connection-running connection) t
              start t)))
    (when start
      (handler-case
          (bordeaux-threads:make-thread (lambda () (run-connection connection))
                                        :name (format nil "CLOG connection ~D"
                                                      (connection-serial connection)))
        (error (condition)
          (bordeaux-threads:with-lock-held ((connection-lock connection))
            (setf (connection-running connection) nil))
          (log-message :error "Connection ~D: no thread to run its events: ~A"
                       (connection-serial connection) condition))))
    t))

(defun run-connection (connection)
  "Run what CONNECTION's mailbox holds, in order, until it is empty."
  (let ((*connection* connection))
    (loop
      (let ((function (bordeaux-threads:with-lock-held ((connection-lock connection))
                        (or (dequeue (connection-mailbox connection))
                            (progn (setf (connection-running connection) nil)
                                   (return))))))
        (let ((*batch* '()))
          (unwind-protect
               (call-logging-errors function "Connection ~D, in a handler"
                                    (connection-serial connection))
            (flush-batch connection)))))))

(defun post (connection-id function)
  "Have FUNCTION called, with no arguments, in CONNECTION-ID's thread: after
the events and functions already waiting, and before those that come later,
never at the same time as another.  This is how another thread changes a
page.  True if queued; NIL if the connection is gone."
  (let ((connection (get-connection connection-id)))
    (and connection (enqueue-function connection function))))

;;;;;;;;;;;;;
;; Sending ;;
;;;;;;;;;;;;;

(defun send (connection scripts)
  "Send SCRIPTS as CONNECTION's next message to its page, or keep it for when
the page reconnects.  NIL once the connection is closing."
  (let ((text (message-text scripts)))
    (bordeaux-threads:with-lock-held ((connection-send-lock connection))
      (when (live-p connection)
        (let ((n (incf (connection-sent connection)))
              (replay (connection-replay connection)))
          (enqueue (cons n text) replay)
          (incf (connection-replay-size connection) (length text))
          (loop while (> (connection-replay-size connection) *replay-size*)
                do (let ((oldest (dequeue replay)))
                     (decf (connection-replay-size connection) (length (cdr oldest)))
                     (setf (connection-lost connection) (car oldest))))
          (let ((websocket (connection-websocket connection)))
            (when websocket
              (websocket-send websocket text)))
          t)))))

(defun flush-batch (connection)
  "Send what this connection thread has batched for CONNECTION."
  (when (and *batch* (eq connection *connection*))
    (let ((scripts (reverse *batch*)))
      (setf *batch* '())
      (send connection scripts))))

(defun flush (connection-id)
  "In CONNECTION-ID's thread, send now what it has sent so far, which would
otherwise go when the running function returns."
  (let ((connection (get-connection connection-id)))
    (when connection
      (flush-batch connection))))

(defun execute (connection-id script)
  "Run SCRIPT in CONNECTION-ID's page; its value is not returned.  In the
connection's thread it goes out with the rest of what the running function
sends, when it returns or makes a query."
  (let ((connection (get-connection connection-id)))
    (when connection
      (if (eq connection *connection*)
          (push script *batch*)
          (send connection (list script))))))

(defun acknowledge (connection n)
  "CONNECTION's page has run its first N messages: forget them."
  (bordeaux-threads:with-lock-held ((connection-send-lock connection))
    (when (<= (connection-acknowledged connection) n (connection-sent connection))
      (setf (connection-acknowledged connection) n)
      (let ((replay (connection-replay connection)))
        (loop for oldest = (car (queue-head replay))
              while (and oldest (<= (car oldest) n))
              do (dequeue replay)
                 (decf (connection-replay-size connection) (length (cdr oldest))))))))

;;;;;;;;;;;;
;; Timers ;;
;;;;;;;;;;;;

(defstruct (timer (:constructor make-timer (connection function due interval)))
  connection function due interval (cancelled nil))

(defvar *timers* '() "The timers waiting, soonest first.")
(defvar *timer-lock* (bordeaux-threads:make-lock "clog-timers"))
(defvar *timer-condition* (bordeaux-threads:make-condition-variable))
(defvar *timer-thread* nil)

(defun add-timer (timer)
  (bordeaux-threads:with-lock-held (*timer-lock*)
    (setf *timers* (merge 'list (list timer) *timers* #'< :key #'timer-due))
    (unless (and *timer-thread* (bordeaux-threads:thread-alive-p *timer-thread*))
      (setf *timer-thread* (bordeaux-threads:make-thread #'run-timers :name "CLOG timers")))
    (bordeaux-threads:condition-notify *timer-condition*)))

(defun fire-timer (timer)
  "Run TIMER's function: in its connection's thread, or in a thread of its
own for the server's timers.  Holding *TIMER-LOCK*."
  (let ((connection (timer-connection timer)))
    (cond ((null connection)
           (bordeaux-threads:make-thread
            (lambda () (call-logging-errors (timer-function timer) "In a CLOG timer"))
            :name "CLOG timer"))
          ((not (enqueue-function connection (timer-function timer)))
           (setf (timer-cancelled timer) t)))
    (unless (timer-interval timer)
      (when connection
        (bordeaux-threads:with-lock-held ((connection-lock connection))
          (setf (connection-timers connection)
                (delete timer (connection-timers connection))))))))

(defun run-timers ()
  (bordeaux-threads:with-lock-held (*timer-lock*)
    (loop
      (let ((next (first *timers*)))
        (cond ((null next)
               (bordeaux-threads:condition-wait *timer-condition* *timer-lock*))
              ((> (timer-due next) (now))
               (bordeaux-threads:condition-wait *timer-condition* *timer-lock*
                                                :timeout (- (timer-due next) (now))))
              (t
               (pop *timers*)
               (unless (timer-cancelled next)
                 (ignore-errors (fire-timer next))
                 (when (and (timer-interval next) (not (timer-cancelled next)))
                   (setf (timer-due next) (+ (now) (timer-interval next))
                         *timers* (merge 'list (list next) *timers* #'<
                                         :key #'timer-due))))))))))

(defun schedule (connection-id seconds function &key repeat)
  "Have FUNCTION called in CONNECTION-ID's thread, as by POST, SECONDS from
now, and with REPEAT every SECONDS after that, until CANCEL-TIMER or the
connection closes.  Returns the timer, or NIL if the connection is gone.  A
NIL CONNECTION-ID is the server's own: FUNCTION runs in a thread of its own."
  (let* ((connection (and connection-id (get-connection connection-id)))
         (timer (make-timer connection function (+ (now) seconds) (and repeat seconds))))
    (when (and connection-id (null connection))
      (return-from schedule nil))
    (when connection
      (bordeaux-threads:with-lock-held ((connection-lock connection))
        (unless (live-p connection)
          (return-from schedule nil))
        (push timer (connection-timers connection))))
    (add-timer timer)
    timer))

(defun cancel-timer (timer)
  "Stop TIMER, from SCHEDULE, before it runs again."
  (when timer
    (setf (timer-cancelled timer) t))
  nil)

;;;;;;;;;;;;;
;; Queries ;;
;;;;;;;;;;;;;

(defstruct (pending-query (:constructor make-pending-query ()))
  (semaphore (bordeaux-threads:make-semaphore))
  (answer nil)
  (error nil))

(defun query (connection-id script &key (default-answer nil) (timeout *query-time-out*))
  "Run SCRIPT, an expression, in CONNECTION-ID's page and return its value as
a string.  DEFAULT-ANSWER (as a string, if not NIL) when the browser does not
answer within TIMEOUT seconds, the script fails there, or the connection is
gone."
  (let ((connection (get-connection connection-id))
        (default (and default-answer (format nil "~A" default-answer))))
    (unless (and connection (live-p connection))
      (return-from query default))
    (flush-batch connection)
    (let ((id (generate-id))
          (pending (make-pending-query))
          (queries (connection-queries connection)))
      (setf (gethash id queries) pending)
      (unwind-protect
           (cond ((not (send connection
                             (list (format nil "clog_answer(~D,\"~A\")" id (escape-string script)))))
                  default)
                 ((not (bordeaux-threads:wait-on-semaphore (pending-query-semaphore pending)
                                                           :timeout timeout))
                  (log-message :debug "Connection ~D: no answer to a query in ~A s"
                               (connection-serial connection) timeout)
                  default)
                 ((pending-query-error pending)
                  (log-message :warning "Connection ~D: a query failed in the browser: ~A"
                               (connection-serial connection) (pending-query-error pending))
                  default)
                 (t
                  (or (pending-query-answer pending) default)))
        (remhash id queries)))))

(defun answer-query (connection message)
  "The browser's answer, MESSAGE, to one of CONNECTION's own queries: any other
is ignored."
  (let* ((end (position-if-not #'digit-char-p message))
         (pending (and end (< 0 end 19)
                       (gethash (parse-integer message :end end)
                                (connection-queries connection)))))
    (when pending
      (if (char= #\! (char message end))
          (setf (pending-query-error pending) (subseq message (1+ end)))
          (setf (pending-query-answer pending) (subseq message (1+ end))))
      (bordeaux-threads:signal-semaphore (pending-query-semaphore pending)))))

;;;;;;;;;;;;;
;; Mirrors ;;
;;;;;;;;;;;;;

(defun mirror (connection-id html-id value)
  "Keep, from now on, the value of CONNECTION-ID's form element HTML-ID, which
is now VALUE, as the browser sends it.  (The browser's side is set up by
CLOG:MIRROR.)"
  (let ((connection (get-connection connection-id)))
    (when connection
      (setf (gethash html-id (connection-mirrors connection)) (or value "")))))

(defun mirrored-value (connection-id html-id)
  "The value of the mirrored form element HTML-ID of CONNECTION-ID, and T; NIL
and NIL when it is not mirrored."
  (let ((connection (get-connection connection-id)))
    (if connection
        (gethash html-id (connection-mirrors connection))
        (values nil nil))))

(defun (setf mirrored-value) (value connection-id html-id)
  (let ((connection (get-connection connection-id)))
    (when (and connection (nth-value 1 (gethash html-id (connection-mirrors connection))))
      (setf (gethash html-id (connection-mirrors connection)) value))
    value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; From the browser, and closing  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun receive-event (connection text)
  "Queue the handler of the event TEXT, \"HTML-ID:EVENT DATA\", if it has one."
  (let* ((space (position #\Space text))
         (hook (subseq text 0 space))
         (data (and space (subseq text (1+ space))))
         (table (connection-data connection)))
    (when (gethash hook table)
      (enqueue-function
       connection
       (lambda ()
         (let ((handler (gethash hook table))
               (debug-hook (gethash "clog-debug" table)))
           (when handler
             (if (and debug-hook (not *disable-clog-debugging*))
                 (funcall debug-hook handler data)
                 (funcall handler data)))))))))

(defun receive (connection websocket message)
  "Handle MESSAGE from the browser on WEBSOCKET, CONNECTION's.  Messages from
a websocket the connection has since replaced are ignored."
  (when (and (plusp (length message))
             (eq websocket (connection-websocket connection)))
    (let ((kind (char message 0)))
      (cond ((digit-char-p kind)
             (answer-query connection message))
            ((< (length message) 2))
            ((string= "E:" message :end2 2)
             (receive-event connection (subseq message 2)))
            ((string= "M:" message :end2 2)
             (let* ((space (or (position #\Space message) (length message)))
                    (html-id (subseq message 2 space))
                    (mirrors (connection-mirrors connection)))
               (when (nth-value 1 (gethash html-id mirrors))
                 (setf (gethash html-id mirrors)
                       (if (< space (length message)) (subseq message (1+ space)) "")))))
            ((string= "P:" message :end2 2)
             (let ((n (ignore-errors (parse-integer message :start 2))))
               (when n
                 (acknowledge connection n)))
             (websocket-send websocket ""))))))

(defun attach (connection websocket n)
  "Give CONNECTION the WEBSOCKET of its page, reconnected, which has run its
first N messages, and send it those it has not.  NIL if the connection is
closing, or no longer keeps them."
  (let ((old nil))
    (bordeaux-threads:with-lock-held ((connection-send-lock connection))
      (bordeaux-threads:with-lock-held ((connection-lock connection))
        (unless (and (live-p connection)
                     (<= (max (connection-lost connection) (connection-acknowledged connection))
                         n
                         (connection-sent connection)))
          (return-from attach nil))
        (setf (connection-state connection) :open)
        (cancel-timer (connection-detach-timer connection))
        (setf (connection-detach-timer connection) nil))
      (setf old (connection-websocket connection)
            (connection-websocket connection) websocket)
      (loop for (number . text) in (queue-head (connection-replay connection))
            when (> number n)
              do (websocket-send websocket text)))
    (when old
      (websocket-close old 1000 "replaced"))
    t))

(defun close-connection (connection &key (code 1000) (reason "") only-if-detached)
  "Close CONNECTION: close its websocket with CODE and REASON, end its timers
and queries, and run its close hooks in its thread.  With ONLY-IF-DETACHED,
only if it is waiting for its page to reconnect."
  (let ((websocket nil))
    (bordeaux-threads:with-lock-held ((connection-send-lock connection))
      (bordeaux-threads:with-lock-held ((connection-lock connection))
        (when (or (not (live-p connection))
                  (and only-if-detached (not (eq (connection-state connection) :detached))))
          (return-from close-connection nil))
        (setf (connection-state connection) :closing)
        (cancel-timer (connection-detach-timer connection))
        (mapc #'cancel-timer (connection-timers connection))
        (setf (connection-detach-timer connection) nil
              (connection-timers connection) '()))
      (setf websocket (connection-websocket connection)
            (connection-websocket connection) nil)
      (clear-queue (connection-replay connection))
      (setf (connection-replay-size connection) 0))
    (remhash (connection-id connection) *connections*)
    (when websocket
      (websocket-close websocket code reason))
    (loop for pending being the hash-values of (connection-queries connection)
          do (bordeaux-threads:signal-semaphore (pending-query-semaphore pending)))
    (enqueue-function connection
                      (lambda ()
                        (loop for hook = (bordeaux-threads:with-lock-held
                                             ((connection-lock connection))
                                           (pop (connection-close-hooks connection)))
                              while hook
                              do (call-logging-errors hook "Connection ~D, in a close hook"
                                                      (connection-serial connection)))
                        (bordeaux-threads:with-lock-held ((connection-lock connection))
                          (setf (connection-state connection) :closed))
                        (clrhash (connection-data connection))
                        (clrhash (connection-mirrors connection)))
                      :force t)
    (log-message :info "Connection ~D closed" (connection-serial connection))
    t))

(defun websocket-ended (connection websocket close-code)
  "WEBSOCKET, CONNECTION's, has closed, after a close frame with CLOSE-CODE
from the browser, or none.  A page that left (1000 or 1001) is closed; one
whose socket dropped is waited for, *RECONNECT-DELAY* seconds."
  (let ((outcome nil))
    (bordeaux-threads:with-lock-held ((connection-send-lock connection))
      (when (eq websocket (connection-websocket connection))
        (setf (connection-websocket connection) nil)
        (bordeaux-threads:with-lock-held ((connection-lock connection))
          (when (eq (connection-state connection) :open)
            (if (or (member close-code '(1000 1001)) (null *reconnect-delay*))
                (setf outcome :close)
                (setf (connection-state connection) :detached
                      outcome :wait))))))
    (case outcome
      (:close
       (close-connection connection))
      (:wait
       (log-message :debug "Connection ~D: its websocket dropped" (connection-serial connection))
       ;; Scheduled holding no lock of the connection's: the timer thread
       ;; takes *TIMER-LOCK*, then a connection's.
       (let ((timer (schedule nil *reconnect-delay*
                              (lambda () (close-connection connection :only-if-detached t)))))
         (bordeaux-threads:with-lock-held ((connection-lock connection))
           (if (eq (connection-state connection) :detached)
               (setf (connection-detach-timer connection) timer)
               (cancel-timer timer))))))))

(defun add-close-hook (connection-id function)
  "Have FUNCTION called, with no arguments, in CONNECTION-ID's thread once the
connection has closed: its page left, or did not reconnect in time.  The page
is gone by then; FUNCTION is for the Lisp side.  NIL if the connection is
gone already."
  (let ((connection (get-connection connection-id)))
    (when connection
      (bordeaux-threads:with-lock-held ((connection-lock connection))
        (when (live-p connection)
          (push function (connection-close-hooks connection))
          t)))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; The page's browser ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun cclose (connection-id)
  "Close CONNECTION-ID's websocket; its page reconnects."
  (let ((connection (get-connection connection-id)))
    (when connection
      (let ((websocket (bordeaux-threads:with-lock-held ((connection-send-lock connection))
                         (connection-websocket connection))))
        (when websocket
          (websocket-close websocket 4002 "reconnect"))))))

(defun shutdown (connection-id)
  "Close CONNECTION-ID for good: its page does not reconnect."
  (let ((connection (get-connection connection-id)))
    (when connection
      (close-connection connection :code 1000 :reason "closed"))))

(defun alert-box (connection-id message)
  "Create an alert box on CONNECTION-ID with MESSAGE"
  (execute connection-id (format nil "alert('~A');" (escape-string message))))

(defun debug-mode (connection-id)
  "Have CONNECTION-ID's page log in its console each script it runs."
  (execute connection-id "clog_debug = true"))
