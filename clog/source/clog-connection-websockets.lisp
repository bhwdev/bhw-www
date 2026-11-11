;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) David Botton                                                      ;;;;
;;;;                                                                       ;;;;
;;;; clog-connection-websockets.lisp                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Specific implementation using websockets and long polling, served by
;;; Hunchentoot.  Upstream builds a Lack application, which Clack runs on
;;; Hunchentoot (or another server), with websocket-driver for the
;;; websockets; this fork has CLOG-ACCEPTOR, a Hunchentoot acceptor, and
;;; clog-websocket.lisp.  An application extends the server the way
;;; Hunchentoot is extended: with a subclass of CLOG-ACCEPTOR, passed to
;;; INITIALIZE as :ACCEPTOR-CLASS, and methods on it -- on
;;; HUNCHENTOOT:ACCEPTOR-DISPATCH-REQUEST to answer requests before CLOG
;;; does, on SERVE-STATIC-FILE to send files differently.

(in-package :clog-connection)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implemetation - clog-connection-websockets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *acceptor* nil "The running CLOG-ACCEPTOR")

(defvar *long-poll-first* nil
                          "Dynamic variable indicating to use html output instead of
   websocket for output at start if connection.")
(defvar *extended-long-poll* nil
                             "Dynamic variable indicating to extend long polling beyond
   extablishing websocket for output.")
(defvar *long-poll-url* nil
                        "Dynamic variable indicating the url path used.")

(defun static-files-directory ()
  "The directory of CLOG's own static files: boot.js and w3.css.  The server
sends a file from it when nothing else claims the path."
  (asdf:component-pathname (asdf:find-component :clog "static-files")))

(defparameter *compiled-boot-js*
  (uiop:read-file-string (merge-pathnames "js/boot.js" (static-files-directory))
                         :external-format :utf-8)
  "A compiled version of current version of boot.js (private)")

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handle-new-connection ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handle-new-connection (connection id)
  "Handle new incoming websocket CONNECTIONS with ID from boot page. (Private)"
  (handler-case
      (cond ((and id (gethash id *connection-data*))
              (unless *hide-connection-output*
                (format t "Reconnection id - ~A to ~A~%" id connection))
              (let ((old (gethash id *connection-ids*)))
                (when *verbose-output*
                  (format t "Transfer id - ~A => ~A" old connection))
                (setf (gethash old *connections*) nil)
                (setf (gethash connection *connections*) id)
                (setf (gethash id *connection-ids*) connection)
                (handler-case
                    (websocket-close old 1000
                                     "Aborting this old connection since receiving a reconnection request.")
                  (t (c)
                    (when *verbose-output*
                      (format t "Failed to close the old connection when establishing reconnection. ~
                                  This can be normal: The old connection could not work for the client, ~
                                so the client is requesting to reconnect.~%Condition - ~A.~&"
                              c))))))
            (id
              (unless *hide-connection-output*
                (format t "Reconnection id ~A not found. Closing the connection.~%" id))
              (websocket-close connection)) ; Don't send the reason for better security.
            (t
              (setf id (random-hex-string))
              (setf (gethash connection *connections*) id)
              (setf (gethash id *connection-ids*) connection)
              (setf (gethash id *connection-data*)
                    (make-hash-table* :test #'equal))
              (setf (gethash "connection-id" (get-connection-data id)) id)
              (unless *hide-connection-output*
                (format t "New connection id - ~A - ~A~%" id connection))
              (websocket-send connection
                              (format nil "clog['connection_id']='~A'" id))
              (bordeaux-threads:make-thread
                (lambda ()
                  (if *break-on-error*
                      (funcall *on-connect-handler* id)
                      (handler-case
                          (funcall *on-connect-handler* id)
                        (t (c)
                          (if *hide-connection-output*
                              (format t "Condition caught in connection handler - ~A.~&" c)
                              (format t "Condition caught connection ~A - ~A.~&" id c))
                          (values 0 c)))))
                :name (format nil "CLOG connection ~A"
                              id))))
    (t (c)
      (format t "Condition caught in handle-new-connection - ~A.~&" c)
      (values 0 c))))

;;;;;;;;;;;;;;;;;;;;
;; handle-message ;;
;;;;;;;;;;;;;;;;;;;;

(defvar *message-handlers* '()
  "List of message handling functions.
Each entry in the list should be a FUNCTION-DESIGNATOR that will receive
a splitted message and a connection id and should return something
other than NIL iff it handled the message.")

(defun handle-message (connection message)
  "Handle incoming websocket MESSAGE on CONNECTION. (Private)"
  (handler-case
      (let ((connection-id (gethash connection *connections*))
            (ml (ppcre:split ":" message :limit 2)))
        (dolist (message-handler *message-handlers*)
          (when (funcall message-handler ml connection-id)
            (return-from handle-message)))
        (cond ((null connection-id)
                ;; a zombie connection
                (when *verbose-output*
                  (format t "A zombie connection ~A. CLOG doesn't remember its connection-id. Closing it.~%"
                          connection))
                (websocket-close connection)) ; don't send the reason for better security
              ((equal (first ml) "0")
                ;; a ping
                (when *browser-gc-on-ping*
                  ;; run browser gc
                  (execute connection-id
                           "Object.entries(clog).forEach(function(c,i,a)
                             {if ((c[1] !== null) && (typeof c[1] === 'object') && (c[1].nodeType===1))
                             {if (c[1].isConnected===false) {clog_trigger(clog['body'],'gc',c[0])}}})"))
                (when *verbose-output*
                  (format t "Connection ~A    Ping~%" connection-id)
                  (execute connection-id "null")))
              ((equal (first ml) "E")
                ;; an event
                (let* ((em (ppcre:split " " (second ml) :limit 2))
                       (event-id (first em))
                       (data (second em)))
                  (when *verbose-output*
                    (format t "Connection ~A    Hook = ~A    Data = ~A~%"
                            connection-id event-id data))
                  (bordeaux-threads:make-thread
                    (lambda ()
                      (if *break-on-error*
                          (let* ((event-hash (get-connection-data connection-id))
                                 (event      (when event-hash
                                               (gethash event-id event-hash))))
                            (when event
                              (let* ((debug-hook (gethash "clog-debug" event-hash)))
                                (if (and debug-hook (not *disable-clog-debugging*))
                                    (funcall debug-hook event data)
                                    (funcall event data)))))
                          (handler-case
                              (let* ((event-hash (get-connection-data connection-id))
                                     (event      (when event-hash
                                                   (gethash event-id
                                                            event-hash))))
                                (when event
                                  (funcall event data)))
                            (t (c)
                              (format t "Condition caught in handle-message for event - ~A.~&" c)
                              (values 0 c)))))
                    :name (format nil "CLOG event handler ~A"
                                  event-id))))
              (t
                ;; a JavaScript execution result
                (let ((server-query-id (first ml))
                      (browser-returned-answer (second ml)))
                  (when *verbose-output*
                    (format t "Connection ~A    ~A = ~A    ~A = ~A~%"
                            connection-id
                            'server-query-id
                            server-query-id
                            'browser-returned-answer
                            browser-returned-answer))
                  (setf (gethash (parse-integer server-query-id) *queries*) browser-returned-answer)
                  (bordeaux-threads:signal-semaphore
                    (gethash (parse-integer server-query-id) *queries-sems*))))))
    (t (c)
      (format t "Condition caught in handle-message - ~A.~&" c)
      (values 0 c))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handle-close-connection ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handle-close-connection (connection)
  "Close websocket CONNECTION. (Private)"
  (when *verbose-output*
    (format t "Connection close request ~A.~%"
            connection))
  (when (and *clog-running* *reconnect-delay*)
    (when *verbose-output*
      (format t "Connection close request ~A delayed ~A for reconnects.~%"
              connection *reconnect-delay*))
    (sleep *reconnect-delay*))
  (handler-case
      (let ((id (gethash connection *connections*)))
        (when id
          (when *verbose-output*
            (format t "Connection id ~A has closed. ~A~%" id connection))
          (remhash id *connection-data*)
          (remhash id *connection-ids*)
          (remhash connection *connections*)))
    (t (c)
      (format t "Condition caught in handle-close-connection - ~A.~&" c)
      (values 0 c))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; The web server      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-acceptor (hunchentoot:acceptor)
  ((extended-routing :initarg :extended-routing :initform nil
                     :reader acceptor-extended-routing)
   (long-poll-first :initarg :long-poll-first :initform nil
                    :reader acceptor-long-poll-first)
   (boot-function :initarg :boot-function :initform nil
                  :reader acceptor-boot-function)
   (static-boot-html :initarg :static-boot-html :initform nil
                     :reader acceptor-static-boot-html)
   (static-boot-js :initarg :static-boot-js :initform nil
                   :reader acceptor-static-boot-js))
  (:default-initargs
   :access-log-destination nil
   :error-template-directory nil)
  (:documentation "The Hunchentoot acceptor CLOG serves on: its boot pages,
the websocket at /clog, boot.js, and static files (see INITIALIZE for the
options in its slots).  Subclass it to serve more."))

(defmethod hunchentoot:process-connection :around ((acceptor clog-acceptor) socket)
  (declare (ignore socket))
  ;; As Clack's Hunchentoot handler set them, but for this acceptor's
  ;; connections only: replies in UTF-8, and a URL or form that is not
  ;; UTF-8 decoded with replacement characters instead of an error.
  (let ((hunchentoot:*hunchentoot-default-external-format*
          (flex:make-external-format :utf-8 :eol-style :lf))
        (hunchentoot:*default-content-type* "text/html; charset=utf-8")
        (flex:*substitution-char* (code-char #xFFFD)))
    (call-next-method)))

(defmethod hunchentoot:acceptor-output-chunking-p :around ((acceptor clog-acceptor))
  (and (not *websocket-handshake*) (call-next-method)))

(defmethod hunchentoot:acceptor-status-message ((acceptor clog-acceptor) http-status-code
                                                &key &allow-other-keys)
  ;; The reason phrase alone for an error without a body of its own, where
  ;; Hunchentoot's page would name its version and the Lisp's.
  (when (>= http-status-code 400)
    (hunchentoot:reason-phrase http-status-code)))

(defparameter *extra-content-types*
  '(("js" . "text/javascript") ("mjs" . "text/javascript")
    ("json" . "application/json") ("webp" . "image/webp") ("avif" . "image/avif")
    ("woff" . "font/woff") ("woff2" . "font/woff2"))
  "Content types Hunchentoot's table lacks or gives an outdated value for.")

(defun file-content-type (pathname)
  "The Content-Type to send the file PATHNAME with, by its extension: text in
UTF-8, application/octet-stream when the extension is unknown."
  (let* ((type (pathname-type pathname))
         (content-type (or (and type (cdr (assoc type *extra-content-types* :test #'string-equal)))
                           (hunchentoot:mime-type pathname)
                           "application/octet-stream")))
    (if (and (ppcre:scan "^text/" content-type) (not (search "charset" content-type)))
        (format nil "~A; charset=utf-8" content-type)
        content-type)))

(defgeneric serve-static-file (acceptor pathname)
  (:documentation "Answer the current request with the file PATHNAME, a
truename.  The default sends it with HUNCHENTOOT:HANDLE-STATIC-FILE, which
sends Last-Modified and answers If-Modified-Since, as the type
FILE-CONTENT-TYPE names.")
  (:method ((acceptor clog-acceptor) pathname)
    (hunchentoot:handle-static-file pathname (file-content-type pathname))))

(defun static-file (root path)
  "The truename of the regular file under the directory ROOT that PATH, a
request's decoded path, names, or NIL.  Only a path of '/'-separated segments
of letters, digits and -_.~, none of them empty or beginning with a dot or a
tilde, names a file: no other directory, no hidden file, no wild or logical
pathname.  (Lack's file server, which upstream uses, merges the path into
ROOT as a namestring, which an absolute path, ~ or a host prefix escapes.)"
  (when (and root (ppcre:scan "^(?:/[A-Za-z0-9_-][A-Za-z0-9_.~-]*)+$" path))
    (let ((truename (probe-file (merge-pathnames (uiop:parse-unix-namestring (subseq path 1))
                                                 (uiop:ensure-directory-pathname root)))))
      ;; PROBE-FILE gives a directory as a directory pathname, without a name.
      (and truename (pathname-name truename) truename))))

(defun clog-path-boot-file (path extended-routing)
  "The boot file of the CLOG path PATH, or with EXTENDED-ROUTING of the CLOG
path PATH extends, other than /; NIL when PATH is not a CLOG path."
  (or (gethash path *url-to-boot-file*)
      (and extended-routing
           (block found
             (maphash (lambda (clog-path boot-file)
                        (when (and (stringp clog-path)
                                   (string/= clog-path "/")
                                   (eql 0 (search (concatenate 'string clog-path "/") path)))
                          (return-from found boot-file)))
                      *url-to-boot-file*)
             nil))))

(defun script-string (string)
  "STRING escaped for a single-quoted JavaScript string in an HTML <script>
element: ESCAPE-STRING's escapes, and < as \\x3C, so that it cannot close
the element."
  (ppcre:regex-replace-all "<" (escape-string string :no-nil t) "\\x3C"))

(defun boot-post-data (request)
  "What a form posted to the boot page in REQUEST, for the page's script, or
NIL: the raw body of an application/x-www-form-urlencoded form, which
FORM-POST-DATA decodes, or the ID of a multipart/form-data form's fields,
kept as connection data until DELETE-MULTIPART-DATA, which
FORM-MULTIPART-DATA reads: (name . value), and for an uploaded file
\(name stream file-name content-type), its contents in a stream in memory,
as Hunchentoot deletes the file when the request ends."
  (let ((content-type (or (hunchentoot:header-in :content-type request) "")))
    (cond ((search "multipart/form-data;" content-type)
           (let ((id (random-hex-string)))
             (setf (gethash id *connection-data*)
                   (mapcar (lambda (parameter)
                             (destructuring-bind (name . value) parameter
                               (if (consp value)
                                   (destructuring-bind (file file-name type) value
                                     (list name
                                           (flex:make-in-memory-input-stream
                                            (alexandria:read-file-into-byte-vector file))
                                           file-name type))
                                   (cons name value))))
                           (hunchentoot:post-parameters request)))
             id))
          ((ppcre:scan "^application/x-www-form-urlencoded\\s*(?:;|$)" content-type)
           (hunchentoot:raw-post-data :request request :force-text t)))))

(defun boot-page (acceptor path boot-file)
  "The HTML of the boot page for PATH: BOOT-FILE, a path in the static root,
if it is there, else what STATIC-BOOT-HTML says, through the BOOT-FUNCTION."
  (let* ((file (and *static-root* (probe-file (uiop:subpathname *static-root* boot-file))))
         (static-boot-html (acceptor-static-boot-html acceptor))
         (page (cond (file
                      (uiop:read-file-string file :external-format :utf-8))
                     ((eq static-boot-html t) "")
                     ((eq static-boot-html :error)
                      (error "Can not open boot file - ~A" boot-file))
                     (static-boot-html)
                     (t (compiled-boot-html nil nil)))))
    (if (acceptor-boot-function acceptor)
        (funcall (acceptor-boot-function acceptor) path page)
        page)))

(defun serve-boot-page (acceptor request path boot-file)
  "Answer REQUEST, for the CLOG path PATH, with its boot page."
  (let ((page (boot-page acceptor path boot-file))
        (post-data (boot-post-data request))
        (long-poll-first (acceptor-long-poll-first acceptor)))
    (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
    (flet ((write-post-data (stream)
             (when post-data
               (format stream "<script>clog['post-data']='~A'</script>"
                       (script-string post-data)))))
      (cond
        (long-poll-first
         (let ((id (random-hex-string)))
           (setf (gethash id *connection-data*) (make-hash-table* :test #'equal))
           (setf (gethash "connection-id" (get-connection-data id)) id)
           (unless *hide-connection-output*
             (format t "New html connection id - ~A~%" id))
           (let* ((stream               (flex:make-flexi-stream (hunchentoot:send-headers)
                                                                :external-format :utf-8))
                  (*long-poll-url*      path)
                  (*long-poll-first*    stream)
                  (*extended-long-poll* (if (eq long-poll-first t)
                                            :extend
                                            long-poll-first)))
             (write-string page stream)
             (format stream "<script>clog['connection_id']='~A';Open_ws();</script>" id)
             (write-post-data stream)
             (if *break-on-error*
                 (funcall *on-connect-handler* id)
                 (handler-case
                     (funcall *on-connect-handler* id)
                   (t (c)
                     (if *hide-connection-output*
                         (format t "Condition caught in connection handler - ~A.~&" c)
                         (format t "Condition caught connection ~A - ~A.~&" id c))
                     (values 0 c))))
             (when *long-poll-first*
               (setf *long-poll-first* nil)
               (handler-case
                   (finish-output stream)
                 (t (c)
                   (if *hide-connection-output*
                       (format t "Condition caught finish-output - ~A.~&" c)
                       (format t "Condition caught finish-output ~A - ~A.~&" id c))
                   (values 0 c))))
             (unless *hide-connection-output*
               (format t "HTML connection closed - ~A~%" id))
             nil)))
        (t
         (with-output-to-string (stream)
           (write-string page stream)
           (write-post-data stream)))))))

(defun serve-clog-websocket (request)
  "Answer REQUEST, for /clog, by opening the websocket the boot page asks
for and serving it until it closes."
  (let ((websocket (websocket-upgrade request)))
    (when websocket
      (serve-websocket
       websocket
       (lambda (websocket)
         (handler-case
             (handle-new-connection websocket (hunchentoot:get-parameter "r" request))
           (t (c)
             (format t "Condition caught in clog-server :open - ~A.~&" c)
             (values 0 c))))
       (lambda (websocket message)
         (handler-case
             (handle-message websocket message)
           (t (c)
             (format t "Condition caught in clog-server :message - ~A.~&" c)
             (values 0 c))))
       (lambda (websocket)
         (handler-case
             (handle-close-connection websocket)
           (t (c)
             (format t "Condition caught in clog-server :close - ~A.~&" c)
             (values 0 c))))))
    nil))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor clog-acceptor) request)
  "Answer REQUEST as CLOG does: a CLOG path with its boot page, /js/boot.js
with the compiled boot.js (unless STATIC-BOOT-JS), /clog with the websocket,
and any other path with a static file: from the directory of the first plugin
path whose regex matches it, if any, else from the static root or else CLOG's
own static files.  404 when there is no such file."
  (let* ((path (hunchentoot:script-name request))
         (boot-file (clog-path-boot-file path (acceptor-extended-routing acceptor))))
    (cond (boot-file
           (serve-boot-page acceptor request path boot-file))
          ((and (not (acceptor-static-boot-js acceptor))
                (string= path "/js/boot.js"))
           (setf (hunchentoot:content-type*) "text/javascript; charset=utf-8")
           *compiled-boot-js*)
          ((string= path "/clog")
           (serve-clog-websocket request))
          (t
           (let* ((plugin-root (plugin-path path))
                  (file (if plugin-root
                            (static-file plugin-root path)
                            (or (static-file *static-root* path)
                                (static-file (static-files-directory) path)))))
             (cond (file
                    (serve-static-file acceptor file))
                   (t
                    (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
                    nil)))))))

;;;;;;;;;;;;;;;;
;; initialize ;;
;;;;;;;;;;;;;;;;

(defun initialize (on-connect-handler
                   &key
                   (host                 "0.0.0.0")
                   (port                 8080)
                   (acceptor-class       'clog-acceptor)
                   (acceptor-initargs    nil)
                   (extended-routing     nil)
                   (long-poll-first      nil)
                   (boot-file            "/boot.html")
                   (boot-function        nil)
                   (static-boot-html     nil)
                   (static-boot-js       nil)
                   (static-root          #P"./static-files/"))
  "Initialize CLOG on a socket using HOST and PORT to serve BOOT-FILE as the
default route for '/' to establish web-socket connections and static files
located at STATIC-ROOT. The server is an instance of ACCEPTOR-CLASS, a
CLOG-ACCEPTOR, made with ACCEPTOR-INITARGS too, and returned; it is started
before this returns.
If LONG-POLLING-FIRST is t, the output is sent as HTML instead of
websocket commands until the end of the on-new-window-handler, if
LONG-POLLING-FIRST is a number will keep long polling till that number of
queries to browser.  LONG-POLLING-FIRST is used in webserver applications to
enable crawling of your website. If BOOT-FILE is nil no initial clog-path's will
be setup, use clog-path to add. The on-connect-handler needs to indentify the
path by querying the browser. See PATH-NAME (in CLOG-LOCATION). If
EXTENDED-ROUTING is t routes will match even if extend with additional / and
additional paths. If static-boot-js is nil then boot.js is served from the file
/js/boot.js instead of the compiled version. If static-boot-html is t if
boot.html is not present will use compiled version otherwise if set to nil (default)
if a boot file not found returns returns a blank page, if it is set to :error will
signal an error and if set to a string will display the string. boot-function if set is
called with the url and the contents of boot-file and its return value replaces
the contents sent to the brower.  STATIC-ROOT may be NIL, for no static root."
  (set-on-connect on-connect-handler)
  (when boot-file
    (set-clog-path "/" boot-file))
  (setf *static-root* static-root)
  (setf *clog-port* port)
  (setf *acceptor* (apply #'make-instance acceptor-class
                          :address host
                          :port port
                          :extended-routing extended-routing
                          :long-poll-first long-poll-first
                          :boot-function boot-function
                          :static-boot-html static-boot-html
                          :static-boot-js static-boot-js
                          acceptor-initargs))
  (hunchentoot:start *acceptor*)
  (format t "HTTP listening on    : ~A:~A~%" host port)
  (format t "HTML root            : ~A~%"    static-root)
  (format t "Long poll first      : ~A~%"    (if long-poll-first
                                                 "yes"
                                                 "no"))
  (format t "Boot function added  : ~A~%"    (if boot-function
                                                 "yes"
                                                 "no"))
  (format t "Boot html source use : ~A~%"    (if static-boot-html
                                                 "static file"
                                                 "compiled version, when no file"))
  (format t "Boot js source use   : ~A~%"    (if static-boot-js
                                                 "static file"
                                                 "compiled version"))
  (format t "Boot file for path / : ~A~%"    boot-file)
  *acceptor*)

(defun add-plugin-path (regex static-path)
  "When a path to the webserver matches REGEX resolve the static file using
STATIC-PATH"
  (setf (gethash regex *plugin-paths*) static-path))

(defun plugin-path (path)
  (block ret-static-path
      (maphash (lambda (k v)
                 (when (ppcre:scan k path)
                   (return-from ret-static-path v)))
               *plugin-paths*)
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; shutdown-connection ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun shutdown-connection ()
  "Stop the server and start closing every websocket: a browser that does not
answer is let go when its socket's read times out."
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil))
  (loop for websocket being the hash-keys of *connections*
        do (websocket-close websocket 1001 "Server shutting down")))

;;;;;;;;;;;;
;; cclose ;;
;;;;;;;;;;;;

(defun cclose (connection-id)
  "Close connection to CONNECTION-ID. The boot file may try to reestablish
 connectivity."
  (execute connection-id "ws.close()"))

;;;;;;;;;;;;;
;; execute ;;
;;;;;;;;;;;;;

(defun execute (connection-id message)
  "Execute SCRIPT on CONNECTION-ID, disregard return value."
  (if *long-poll-first*
      (write-sequence (format nil "<script>~A</script>~%" message)
                      *long-poll-first*)
      (let ((con (get-connection connection-id)))
        (when con
          (websocket-send con message)))))

;;;;;;;;;;;
;; query ;;
;;;;;;;;;;;

(defun query (connection-id script &key (default-answer nil))
  "Execute SCRIPT on CONNECTION-ID, return value. If times out answer
DEFAULT-ANSWER."
  ;; Provide delay if needed to establish websocket connection for
  ;; response.
  (when *long-poll-first*
    (finish-output *long-poll-first*)
    (loop
      for n from 1 to 10 do
      (let ((con (get-connection connection-id)))
        (when con
          (unless (or (eq *extended-long-poll* :extend)
                      (> (decf *extended-long-poll*) 0))
            (unless *hide-connection-output*
              (format t "Closing long-poll for ~A~%" connection-id))
            (setf *long-poll-first* nil))
          (return))
        (unless *hide-connection-output*
          (format t "Awaiting websocket connection for ~A~%" connection-id))
        (sleep .1))))
  (let ((uid (generate-id)))
    (prep-query uid (when default-answer (format nil "~A" default-answer)))
    (execute connection-id
             (format nil "ws.send (\"~A:\"+eval(\"~A\"));"
                     uid
                     (escape-string script)))
    (wait-for-answer uid)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; compiled-boot-html ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun compiled-boot-html (path content)
  "Returns a compiled version version of boot.html.  It loads boot.js alone,
where upstream's also loads jQuery from the jQuery CDN, and has no template
slots in its <noscript> elements, which upstream's has for a BOOT-FUNCTION to
fill."
  (declare (ignore path content))
  "<!doctype HTML>
<HTML>
   <HEAD>
      <meta http-equiv='Cache-Control' content='no-cache, no-store, must-revalidate' />
      <meta http-equiv='Pragma' content='no-cache' />
      <meta http-equiv='Expires' content='0' />
      <meta charset='utf-8'>
      <meta name='viewport' content='width=device-width, initial-scale=1'>
      <script src='/js/boot.js' type='text/javascript'></script>
   </HEAD>
<BODY>
</BODY>
<noscript>Your browser must support JavaScript and be HTML 5 compilant to see this site.</noscript>
</HTML>")
