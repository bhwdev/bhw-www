;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2025 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog-connection.lisp                                                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; clog-connection.lisp contains the clog-connection package that handles
;;; the low level connectivity between lisp and the browser.
;;;
;;; clog <-> clog-connection <->
;;;   [clog-connection-websockets.lisp] - the web server and connection layer
;;;   [clog-websocket.lisp] - RFC 6455 websockets <->
;;;   Hunchentoot <-> internet/localhost <-> browser
;;;
;;; Normally users of clog do not interact with the exports of clog-connection
;;; package. Shutdown and startup of clog are found in clog-system.lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exports - clog-connection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defpackage #:clog-connection
  (:documentation "The Common List Omnificent GUI - Connection")
  (:use #:cl)
  (:export
   ;;; CLOG Connection
   ;; CLOG system startup and shutdown
   #:*verbose-output*
   #:*hide-connection-output*
   #:*browser-gc-on-ping*
   #:*break-on-error*
   #:*disable-clog-debugging*
   #:*reconnect-delay*
   #:*static-root*
   #:*clog-port*
   #:*clog-running*
   #:initialize
   #:random-port
   #:shutdown-clog
   #:set-on-connect
   #:set-clog-path
   #:get-connection-data
   #:delete-connection-data
   #:add-plugin-path
   ;; CLOG system utilities
   #:escape-string
   #:generate-id
   #:random-hex-string
   #:make-hash-table*
   ;; CLOG connection interface
   #:execute
   #:query
   #:validp
   #:cclose
   #:shutdown
   #:compiled-boot-html
   ;; CLOG low level browser access
   #:put
   #:put-line
   #:new-line
   #:alert-box
   #:debug-mode
   #:set-html-on-close
   ;;; The web server (this fork's: Hunchentoot, without Clack and Lack)
   #:clog-acceptor
   #:serve-static-file
   #:file-content-type
   #:static-files-directory
   #:*websocket-idle-timeout*
   #:*websocket-max-message-octets*))

(in-package :clog-connection)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implemetation - clog-connection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-hash-table* (&rest args)
  "Use native concurrent hash tables"
  ;; This covers sbcl ecl mazzano lw and ccl.
  ;; (lw and ccl default hash is synchronized)
  #+(or sbcl ecl mezzano)
  (apply #'make-hash-table :synchronized t args)
  #-(or sbcl ecl mezzano) (apply #'make-hash-table args))

(defvar *verbose-output* nil "Verbose server output (default nil)")
(defvar *hide-connection-output* nil "When true, do not print connection ids (default nil)")
(defvar *browser-gc-on-ping* nil "Run a browser-gc on every ping (default nil)")
(defvar *break-on-error* t   "Allow invoking debugger (default t)")
(defvar *disable-clog-debugging* nil "When true turns off debug hooks (default nil)")
(defvar *reconnect-delay* 7 "Time to delay in seconds for possible reconnect (default 7)")
(defparameter *static-root* nil "Contains the static-root setting after initialization.")
(defparameter *clog-port* 8080 "Port this instance of clog was started on")
(defvar *plugin-paths* (make-hash-table* :test #'equalp) "Path regex -> static-root")
(defvar *clog-running* nil "CLOG initialized and running.")

(defvar *on-connect-handler* nil "New connection event handler.")

(defvar *connections*     (make-hash-table*) "Connections to IDs")
(defvar *connection-ids*  (make-hash-table* :test #'equal) "IDs to connections")
(defvar *connection-data* (make-hash-table* :test #'equal) "Connection based data")

(defvar *new-id* 0 "Last issued script ID")
(defvar *new-id-lock* (bordeaux-threads:make-lock "clog-new-id"))

(defvar *queries*        (make-hash-table*) "Query ID to Answers")
(defvar *queries-sems*   (make-hash-table*) "Query ID to semiphores")
(defvar *query-time-out* 3
  "Number of seconds to timeout waiting for a query by default")

(defvar *url-to-boot-file* (make-hash-table* :test 'equalp) "URL to boot-file")

;;;;;;;;;;;;;;;;;
;; generate-id ;;
;;;;;;;;;;;;;;;;;

(defun generate-id ()
  "Generate unique ids for use in scripts."
  (bordeaux-threads:with-lock-held (*new-id-lock*)
    (incf *new-id*)))

;;;;;;;;;;;;;;;;:;;;;;;
;; random-hex-string ;;
;;;;;;;;;;;;;;;;;:;;;;;

(defun random-hex-string ()
  "Generate cryptographic grade random ids for use in connections: 128 bits
from Ironclad's PRNG, which reads the operating system's.  (Upstream draws
from a shared cl-isaac context everywhere but Windows: no lock guards it from
concurrent connections, a saved image restores its state and so repeats its
IDs, and cl-isaac 1.0.8's RAND64 wedges at the end of each 256-word block.)"
  (ironclad:byte-array-to-hex-string (ironclad:random-data 16)))

;;;;;;;;;;;;;;;;;;;;
;; get-connection ;;
;;;;;;;;;;;;;;;;;;;;

(defun get-connection (connection-id)
  "Return the connection associated with CONNECITION-ID. (Private)"
  (gethash connection-id *connection-ids*))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; get-connection-data ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-connection-data (connection-id)
  "Return the connecton data associated with the CONNECTION-ID a
hash test: #'equal."
  (gethash connection-id *connection-data*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; delete-connection-data ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun delete-connection-data (connection-id)
  "Delete CONNECTION-ID's data. (private)"
  (remhash connection-id *connection-data*))

;;;;;;;;;;;;;;;;
;; prep-query ;;
;;;;;;;;;;;;;;;;

(defun prep-query (id default-answer)
  "Setup up a query to be received from a script identified by ID an returning
with DEFAULT-ANSWER in case of a time out. (Private)"
  (setf (gethash id *queries-sems*) (bordeaux-threads:make-semaphore))
  (setf (gethash id *queries*) default-answer))

;;;;;;;;;;;;;;;;;;;;;
;; wait-for-answer ;;
;;;;;;;;;;;;;;;;;;;;;

(defun wait-for-answer (id &key (timeout *query-time-out*))
  "Block after prep-query and sending the query script with ID and TIMEOUT with
the default answer. (Private)"
  (handler-case
      (progn
        (bordeaux-threads:wait-on-semaphore (gethash id *queries-sems*)
                                            :timeout timeout)
        (let ((answer (gethash id *queries*)))
          (remhash id *queries*)
          (remhash id *queries-sems*)
          answer))
    (t (c)
      (format t "Condition caught in wait-for-answer - ~A.~&" c)
      (values 0 c))))

;;;;;;;;;;;;;;;;;
;; random-port ;;
;;;;;;;;;;;;;;;;;

(defun random-port (&key (host "0.0.0.0"))
  "Return a random open port on host"
  (let* ((l (usocket:socket-listen host 0))
         (p (usocket:get-local-port l)))
    (usocket:socket-close l)
    p))

;;;;;;;;;;;;;;;;;;;
;; shutdown-clog ;;
;;;;;;;;;;;;;;;;;;;

(defun shutdown-clog ()
  "Shutdown CLOG."
  (setf *clog-running* nil)
  (shutdown-connection)
  (clrhash *connection-data*)
  (clrhash *connections*)
  (clrhash *connection-ids*)
  (clrhash *url-to-boot-file*))

;;;;;;;;;;;;;;;;;;;;
;; set-on-connect ;;
;;;;;;;;;;;;;;;;;;;;

(defun set-on-connect (on-connect-handler)
  "Change the ON-CONNECTION-HANDLER set during Initialize."
  (setf *on-connect-handler* on-connect-handler))

;;;;;;;;;;;;;;;;;;;
;; set-clog-path ;;
;;;;;;;;;;;;;;;;;;;

(defun set-clog-path (path boot-file)
  "Associate URL path to BOOT-FILE"
  (if boot-file
      (setf (gethash path *url-to-boot-file*)
            ;; Make clog-path into a relative path of
            ;; of site-root.
            (if (eql (char boot-file 0) #\/)
                (concatenate 'string "." boot-file)
                boot-file))
      (remhash path *url-to-boot-file*)))

;;;;;;;;;;;;;;;;;;;
;; escape-string ;;
;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;
;; validp ;;
;;;;;;;;;;;;

(defun validp (connection-id)
  "Check if CONNECTION-ID is valid."
  (if (get-connection connection-id)
      t
      nil))

;;;;;;;;;;;;;;
;; shutdown ;;
;;;;;;;;;;;;;;

(defun shutdown (connection-id)
  "Shutdown connection to CONNECTION-ID. The boot file may not try to
reistablish connectivity."
  (execute connection-id "Shutdown_ws(event.reason='user')"))

;;;;;;;;;
;; put ;;
;;;;;;;;;

(defun put (connection-id text)
  "Write TEXT to document object of CONNECTION-ID with out new line."
  (execute connection-id
           (format nil "document.write('~A');" (escape-string text))))

;;;;;;;;;;;;;;
;; put-line ;;
;;;;;;;;;;;;;;

(defun put-line (connection-id text)
  "Write TEXT to document object of CONNECTION-ID with new line and
HTML <br />."
  (execute connection-id
           (format nil "document.writeln('~A<br />');" (escape-string text))))

;;;;;;;;;;;;;;
;; new-line ;;
;;;;;;;;;;;;;;

(defun new-line (connection-id)
  "Write a new line raw to document object of CONNECTION-ID with a <br />."
  (execute connection-id (format nil "document.writeln('<br />');")))

;;;;;;;;;;;;;;;
;; alert-box ;;
;;;;;;;;;;;;;;;

(defun alert-box (connection-id message)
  "Create an alert box on CONNECTION-ID with MESSAGE"
  (execute connection-id (format nil "alert('~A');" (escape-string message))))

;;;;;;;;;;;;;;;;
;; debug-mode ;;
;;;;;;;;;;;;;;;;

(defun debug-mode (connection-id)
  "Turn on javascript debug mode in the boot.js file"
  (execute connection-id "clog_debug = true"))

;;;;;;;;;;;;;;;;;;;;;;;
;; set-html-on-close ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun set-html-on-close (connection-id html)
  "Set the client side variable clog['html_on_close'] to replace
the browser contents in case of connection loss."
  (execute connection-id (format nil "clog['html_on_close']='~A'"
                                 (escape-string html))))
