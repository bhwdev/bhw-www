;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) David Botton                                                      ;;;;
;;;;                                                                       ;;;;
;;;; clog-connection-websockets.lisp                                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The web server: CLOG-ACCEPTOR, a Hunchentoot acceptor, in place of
;;; upstream's Lack application under Clack.  It holds what upstream keeps in
;;; global variables -- the routes, the static root, the plugin paths -- so
;;; that an image may run more than one.  An application extends it the way
;;; Hunchentoot is extended: with a subclass, passed to INITIALIZE as
;;; :ACCEPTOR-CLASS, and methods on it -- on
;;; HUNCHENTOOT:ACCEPTOR-DISPATCH-REQUEST to answer requests before CLOG
;;; does, on SERVE-STATIC-FILE to send files differently.
;;;
;;; A page's websocket lives in the Hunchentoot thread of the request that
;;; opened it, but out of the taskmaster's count of threads, which is for
;;; HTTP requests: pages left open would otherwise take every thread, and the
;;; server would answer nothing else.  The websockets have limits of their
;;; own, *MAX-WEBSOCKETS* (by default from the process's limit on open
;;; files) and *MAX-WEBSOCKETS-PER-ADDRESS*.

(in-package :clog-connection)

(defvar *server* nil
  "The CLOG-ACCEPTOR INITIALIZE started last: the server of the functions
that take one, by default.")

(defvar *max-websockets* nil
  "The most websockets a server keeps open, one for each page open in a
browser; NIL (the default) for what the process's limit on open files
allows when the server starts: that limit less 512 (for the HTTP requests,
250 at most with CLOG-ACCEPTOR's taskmaster, and the application's own
files), at most 4096.  Each websocket is a file descriptor and a thread, and
a thread costs about six memory mappings (Linux allows 65530 by default) and
0.1 MB.  With the usual limit of 1024 files, 512.")

(defun open-files-limit ()
  "How many files this process may have open (its soft RLIMIT_NOFILE), from
/proc/self/limits; 1024, the usual default, where that cannot be read."
  (or (ignore-errors
       (with-open-file (in "/proc/self/limits")
         (loop for line = (read-line in nil)
               while line
               when (uiop:string-prefix-p "Max open files" line)
                 return (or (parse-integer line :start 14 :junk-allowed t)
                            most-positive-fixnum))))
      1024))

(defun websocket-limit ()
  "*MAX-WEBSOCKETS*, or what NIL there stands for."
  (or *max-websockets*
      (min 4096 (max 64 (- (open-files-limit) 512)))))

(defvar *max-websockets-per-address* 64
  "The most websockets a server keeps open for one client address.")

(defun static-files-directory ()
  "The directory of CLOG's own static files: boot.js and w3.css.  The server
sends a file from it when nothing else claims the path."
  (asdf:component-pathname (asdf:find-component :clog "static-files")))

(defparameter *compiled-boot-js*
  (uiop:read-file-string (merge-pathnames "js/boot.js" (static-files-directory))
                         :external-format :utf-8)
  "boot.js, as it was when CLOG was loaded.")

(defparameter *boot-script-path*
  (format nil "/js/boot.~A.js"
          (subseq (ironclad:byte-array-to-hex-string
                   (ironclad:digest-sequence
                    :sha256 (flex:string-to-octets *compiled-boot-js* :external-format :utf-8)))
                  0 16))
  "The path of boot.js, named by its contents, so that a browser may keep it
for good.")

(defun boot-script-path ()
  "The path the boot page loads boot.js from."
  *boot-script-path*)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; The web server      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass clog-acceptor (hunchentoot:acceptor)
  ((routes :initform (make-hash-table* :test #'equal) :reader acceptor-routes
           :documentation "What a page's connection starts with, by its path:
an ON-NEW-WINDOW handler, for CLOG.  The path :DEFAULT is any other path no
file answers.")
   (extended-routing :initarg :extended-routing :initform nil
                     :accessor acceptor-extended-routing
                     :documentation "When true a route also serves the paths
under it: /a serves /a/b, unless /a/b has a route.")
   (static-root :initarg :static-root :initform nil :accessor acceptor-static-root
                :documentation "The directory of the files the server sends,
or NIL.")
   (plugin-paths :initform '() :accessor acceptor-plugin-paths
                 :documentation "(REGEX . DIRECTORY) of each plugin path, in
the order added.")
   (on-connect :initarg :on-connect :initform nil :accessor acceptor-on-connect
               :documentation "Called in a new connection's thread with its ID
and its route's value.")
   (websocket-lock :initform (bordeaux-threads:make-lock "clog-websockets")
                   :reader acceptor-websocket-lock)
   (websocket-limit :initarg :websocket-limit :initform (websocket-limit)
                    :reader acceptor-websocket-limit
                    :documentation "The most websockets it keeps open: see
*MAX-WEBSOCKETS*.")
   (websocket-count :initform 0 :accessor acceptor-websocket-count)
   (websocket-addresses :initform (make-hash-table :test #'equal)
                        :reader acceptor-websocket-addresses))
  (:default-initargs
   :access-log-destination nil
   :error-template-directory nil
   ;; Twice Hunchentoot's default (100 threads, and 20 more requests
   ;; waiting): the threads are for HTTP requests, which a browser's
   ;; keep-alive connection holds between them for up to the read timeout.
   :taskmaster (make-instance 'hunchentoot:one-thread-per-connection-taskmaster
                              :max-thread-count 200
                              :max-accept-count 250))
  (:documentation "The Hunchentoot acceptor CLOG serves on: the boot pages of
its routes, the websocket at /clog, boot.js, and static files (see
INITIALIZE for its options).  Subclass it to serve more."))

(defmethod hunchentoot:process-connection :around ((acceptor clog-acceptor) socket)
  (declare (ignore socket))
  ;; As Clack's Hunchentoot handler set them, but for this acceptor's
  ;; connections only: replies in UTF-8, and a URL or form that is not
  ;; UTF-8 decoded with replacement characters instead of an error.
  (let ((hunchentoot:*hunchentoot-default-external-format*
          (flex:make-external-format :utf-8 :eol-style :lf))
        (hunchentoot:*default-content-type* "text/html; charset=utf-8")
        (flex:*substitution-char* (code-char #xFFFD)))
    ;; Hunchentoot answers an error with a 500, but a stack or heap
    ;; exhausted is not an error: it would reach the thread's toplevel, and
    ;; in a program whose debugger is disabled end the process.  It ends the
    ;; connection instead.
    (handler-case (call-next-method)
      (storage-condition (condition)
        (log-message :error "A connection's thread ran out of room and was ended: ~A"
                     (or (ignore-errors (princ-to-string condition)) (type-of condition)))))))

(defmethod hunchentoot:acceptor-output-chunking-p :around ((acceptor clog-acceptor))
  (and (not *websocket-handshake*) (call-next-method)))

(defmethod hunchentoot:acceptor-status-message ((acceptor clog-acceptor) http-status-code
                                                &key &allow-other-keys)
  ;; The reason phrase alone for an error without a body of its own, where
  ;; Hunchentoot's page would name its version and the Lisp's.
  (when (>= http-status-code 400)
    (hunchentoot:reason-phrase http-status-code)))

(defun the-server (server)
  (or server (error "CLOG is not running: call INITIALIZE first.")))

;;;;;;;;;;;;
;; Routes ;;
;;;;;;;;;;;;

(defun add-route (path handler &key (server *server*))
  "Serve the boot page at PATH, a string starting with /, or :DEFAULT for any
path that has no route and names no file, and start each connection of its
pages with HANDLER (given to the server's ON-CONNECT).  A NIL HANDLER removes
the route."
  (let ((routes (acceptor-routes (the-server server))))
    (if handler
        (setf (gethash path routes) handler)
        (remhash path routes))
    handler))

(defun find-route (path &key (server *server*) default)
  "The handler of PATH: its own route's, else with extended routing that of
the longest route PATH is under, else with DEFAULT the :DEFAULT route's.  /
is no other path's route."
  (let* ((server (the-server server))
         (routes (acceptor-routes server)))
    (or (gethash path routes)
        (and (acceptor-extended-routing server)
             (loop for end = (position #\/ path :from-end t)
                     then (position #\/ path :end end :from-end t)
                   while (and end (plusp end))
                   thereis (gethash (subseq path 0 end) routes)))
        (and default (gethash :default routes)))))

(defun add-plugin-path (regex static-path &key (server *server*))
  "Send a path that matches REGEX from the directory STATIC-PATH, rather than
the static root.  The plugin paths are tried in the order they were added."
  (let ((server (the-server server)))
    (setf (acceptor-plugin-paths server)
          (append (remove regex (acceptor-plugin-paths server) :key #'car :test #'equal)
                  (list (cons regex static-path))))
    static-path))

(defun plugin-path (acceptor path)
  "The directory of the first plugin path that PATH matches, or NIL."
  (cdr (find-if (lambda (plugin) (ppcre:scan (car plugin) path))
                (acceptor-plugin-paths acceptor))))

;;;;;;;;;;;;;;;;;;
;; Static files ;;
;;;;;;;;;;;;;;;;;;

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

(defun path-segments-p (path &key trailing-slash)
  "True when PATH is '/'-separated segments of letters, digits and -_.~, none
of them empty or beginning with a dot or a tilde, and ends with a segment,
or with TRAILING-SLASH with a slash too.  Read a segment at a time: one
regex for the whole path repeats a group for each segment, and CL-PPCRE's
matcher recurses for each repetition, so tens of thousands of segments
exhaust the stack."
  (and (stringp path)
       (plusp (length path))
       (char= #\/ (char path 0))
       (loop for (segment . more) on (rest (uiop:split-string path :separator "/"))
             always (or (ppcre:scan "^[A-Za-z0-9_-][A-Za-z0-9_.~-]*\\z" segment)
                        (and trailing-slash (null more) (string= segment ""))))))

(defun static-file (root path)
  "The truename of the regular file under the directory ROOT that PATH, a
request's decoded path, names, or NIL.  Only a path of '/'-separated segments
of letters, digits and -_.~, none of them empty or beginning with a dot or a
tilde, names a file: no other directory, no hidden file, no wild or logical
pathname.  (Lack's file server, which upstream uses, merges the path into
ROOT as a namestring, which an absolute path, ~ or a host prefix escapes.)"
  (when (and root (path-segments-p path))
    (let ((truename (probe-file (merge-pathnames (uiop:parse-unix-namestring (subseq path 1))
                                                 (uiop:ensure-directory-pathname root)))))
      ;; PROBE-FILE gives a directory as a directory pathname, without a name.
      (and truename (pathname-name truename) truename))))

;;;;;;;;;;;;;;;;;;;;
;; The boot page  ;;
;;;;;;;;;;;;;;;;;;;;

(defun compiled-boot-html (&optional token)
  "The boot page: boot.js and nothing else, which connects and is sent the
page.  TOKEN, if given, names what a form posted to it."
  (format nil "<!doctype html>
<html>
<head>
<meta charset='utf-8'>
<meta name='viewport' content='width=device-width, initial-scale=1'>~@[
<meta name='clog-boot' content='~A'>~]
<script src='~A'></script>
</head>
<body>
<noscript>This page needs JavaScript: it is drawn, and kept up to date, by the server over a websocket.</noscript>
</body>
</html>
" token *boot-script-path*))

(defvar *boot-data* (make-hash-table :test #'equal)
  "What forms posted to boot pages, until their pages connect: (TIME SIZE .
PLIST) by token, SIZE as KEEP-BOOT-DATA has it.  Under *BOOT-DATA-LOCK*.")

(defvar *boot-data-lock* (bordeaux-threads:make-lock "clog-boot-data"))

(defvar *boot-data-size* 0
  "The sizes of the entries of *BOOT-DATA*, added up.")

(defparameter +boot-data-seconds+ 60
  "How long what a form posted to a boot page is kept for its page.")

(defparameter *boot-data-limit* (* 8 1024 1024)
  "The most that is kept of what forms posted, their texts in characters and
their files in octets.  Whoever posts a form decides how much is kept, and
for as long as +BOOT-DATA-SECONDS+ if no page connects to take it: 200
posts of a mebibyte each held some 800 MB of strings.")

(defun keep-boot-data (plist size)
  "Keep PLIST, of SIZE characters and octets, for the page about to be sent;
its token, or NIL if too much is kept already."
  (bordeaux-threads:with-lock-held (*boot-data-lock*)
    (flet ((room-p ()
             (and (< (hash-table-count *boot-data*) 10000)
                  (<= (+ *boot-data-size* size) *boot-data-limit*))))
      (let ((now (get-universal-time)))
        (unless (room-p)
          (loop for token being the hash-keys of *boot-data* using (hash-value entry)
                when (> (- now (first entry)) +boot-data-seconds+)
                  do (remhash token *boot-data*)
                     (decf *boot-data-size* (second entry))))
        (when (room-p)
          (let ((token (random-hex-string)))
            (setf (gethash token *boot-data*) (list* now size plist))
            (incf *boot-data-size* size)
            token))))))

(defun take-boot-data (token)
  "The plist kept for TOKEN, forgotten, if it is recent."
  (bordeaux-threads:with-lock-held (*boot-data-lock*)
    (let ((entry (gethash token *boot-data*)))
      (when entry
        (remhash token *boot-data*)
        (decf *boot-data-size* (second entry))
        (and (<= (- (get-universal-time) (first entry)) +boot-data-seconds+)
             (cddr entry))))))

(defun boot-post-data (request)
  "What a form posted to the boot page in REQUEST, as a plist, or NIL:
:POST-DATA, the raw body of an application/x-www-form-urlencoded form, or
:MULTIPART-DATA, a multipart/form-data form's fields: (name . value), and for
an uploaded file (name stream file-name content-type), its contents in a
stream in memory, as Hunchentoot deletes the file when the request ends.
The second value is its size, its texts' characters and its files' octets."
  (let ((content-type (or (hunchentoot:header-in :content-type request) ""))
        (size 0))
    (cond ((search "multipart/form-data;" content-type)
           (values
            (list :multipart-data
                  (mapcar (lambda (parameter)
                            (destructuring-bind (name . value) parameter
                              (incf size (length name))
                              (if (consp value)
                                  (destructuring-bind (file file-name type) value
                                    (let ((octets (alexandria:read-file-into-byte-vector file)))
                                      (incf size (length octets))
                                      (list name (flex:make-in-memory-input-stream octets)
                                            file-name type)))
                                  (progn (incf size (length value))
                                         (cons name value)))))
                          (hunchentoot:post-parameters request)))
            size))
          ((ppcre:scan "^application/x-www-form-urlencoded\\s*(?:;|$)" content-type)
           (let ((text (hunchentoot:raw-post-data :request request :force-text t)))
             (values (list :post-data text) (length text)))))))

(defun serve-boot-page (request)
  "Answer REQUEST with the boot page."
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8"
        (hunchentoot:header-out :cache-control) "no-store")
  (compiled-boot-html (multiple-value-bind (data size) (boot-post-data request)
                        (and data (keep-boot-data data size)))))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor clog-acceptor) request)
  "Answer REQUEST as CLOG does: /clog with the websocket, the path of boot.js
with it, a path with a route with its boot page, and any other path with a
static file: from the directory of the first plugin path whose regex matches
it, if any, else from the static root or else CLOG's own static files.
Failing that, the boot page of the :DEFAULT route, as a 404, or a bare 404."
  (let ((path (hunchentoot:script-name request)))
    (cond ((string= path "/clog")
           (serve-clog-websocket acceptor request))
          ((string= path *boot-script-path*)
           (setf (hunchentoot:content-type*) "text/javascript; charset=utf-8"
                 (hunchentoot:header-out :cache-control) "public, max-age=31536000, immutable")
           *compiled-boot-js*)
          ((find-route path :server acceptor)
           (serve-boot-page request))
          (t
           (let* ((plugin-root (plugin-path acceptor path))
                  (file (if plugin-root
                            (static-file plugin-root path)
                            (or (static-file (acceptor-static-root acceptor) path)
                                (static-file (static-files-directory) path)))))
             (cond (file
                    (serve-static-file acceptor file))
                   (t
                    (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
                    (when (gethash :default (acceptor-routes acceptor))
                      (serve-boot-page request)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The pages' websockets ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun client-address (request)
  "The address of REQUEST's client: the last address of X-Forwarded-For, which
the reverse proxy adds, when the request comes from this host, else the
peer's."
  (let ((peer (hunchentoot:remote-addr request))
        (forwarded (hunchentoot:header-in :x-forwarded-for request)))
    (if (and forwarded (member peer '("127.0.0.1" "::1") :test #'equal))
        (string-trim " " (car (last (ppcre:split "," forwarded))))
        peer)))

(defun same-origin-p (request)
  "True unless REQUEST has an Origin whose host and port are not those its
Host names: a page of another site may not open a websocket here."
  (let ((origin (hunchentoot:header-in :origin request))
        (host (hunchentoot:header-in :host request)))
    (or (null origin)
        (let ((authority (nth-value 1 (ppcre:scan-to-strings
                                       "^[A-Za-z][A-Za-z0-9+.-]*://([^/?#]+)$" origin))))
          (and authority host (string-equal (aref authority 0) host))))))

(defun reserve-websocket (acceptor address)
  "Count a websocket for ADDRESS, if the limits allow one more."
  (bordeaux-threads:with-lock-held ((acceptor-websocket-lock acceptor))
    (let ((addresses (acceptor-websocket-addresses acceptor)))
      (when (and (< (acceptor-websocket-count acceptor) (acceptor-websocket-limit acceptor))
                 (< (gethash address addresses 0) *max-websockets-per-address*))
        (incf (acceptor-websocket-count acceptor))
        (incf (gethash address addresses 0))
        t))))

(defun release-websocket (acceptor address)
  (bordeaux-threads:with-lock-held ((acceptor-websocket-lock acceptor))
    (let ((addresses (acceptor-websocket-addresses acceptor)))
      (decf (acceptor-websocket-count acceptor))
      (when (<= (decf (gethash address addresses 0)) 0)
        (remhash address addresses)))))

(defun call-with-thread-released (acceptor function)
  "Call FUNCTION with this request's thread out of ACCEPTOR's taskmaster's
count, so that it does not keep a thread from the HTTP requests while it
serves a websocket."
  (let ((taskmaster (hunchentoot::acceptor-taskmaster acceptor)))
    (if (typep taskmaster 'hunchentoot:one-thread-per-connection-taskmaster)
        (progn
          ;; Hunchentoot counts the thread against both the threads and the
          ;; requests accepted, and uncounts both when the request ends.
          (hunchentoot:decrement-taskmaster-thread-count taskmaster)
          (unwind-protect (funcall function)
            (hunchentoot::increment-taskmaster-accept-count taskmaster)
            (hunchentoot:increment-taskmaster-thread-count taskmaster)))
        (funcall function))))

(defun get-integer (request name)
  (let ((value (hunchentoot:get-parameter name request)))
    (and value (< (length value) 12)
         (ignore-errors (parse-integer value)))))

(defparameter *max-page-path* 2048
  "The longest path a page may say it has: its route is looked for under
each of the path's directories in turn (FIND-ROUTE), in time that grows as
the square of the path's length.")

(defun page-request (request)
  "What a new page tells of itself in REQUEST, the opening of its websocket:
the plist CONNECTION-REQUEST describes.  A path that is not one, or is
longer than *MAX-PAGE-PATH*, is /, and a time zone offset of a day or more
none."
  (let ((path (hunchentoot:get-parameter "p" request))
        (token (hunchentoot:get-parameter "b" request))
        (offset (get-integer request "tz")))
    (append (list :path (if (and path (< 0 (length path) (1+ *max-page-path*))
                                 (char= #\/ (char path 0)))
                            path
                            "/")
                  :query-string (or (hunchentoot:get-parameter "q" request) "")
                  :headers (loop for (name . value) in (hunchentoot:headers-in request)
                                 collect (cons (string-downcase name) value))
                  :cookies (hunchentoot:cookies-in request)
                  :remote-address (client-address request)
                  ;; Clocks run from 12 hours behind UTC to 14 ahead.  The
                  ;; page chooses what it sends: an offset of years would
                  ;; make every time decoded with it fail.
                  :timezone-offset (and offset (< -1440 offset 1440) offset)
                  :viewport-width (get-integer request "vw")
                  :viewport-height (get-integer request "vh")
                  :pixel-ratio (hunchentoot:get-parameter "dpr" request)
                  :language (hunchentoot:get-parameter "lang" request))
            (and token (take-boot-data token)))))

(defun open-page (acceptor request websocket)
  "The connection of the page whose websocket REQUEST opened: a new one, or
the one it reconnects to, which sends it what it missed.  A page whose
connection is gone, or cannot be brought up to date, is told to reload."
  (let ((id (hunchentoot:get-parameter "r" request)))
    (when (not (or (hunchentoot:get-parameter "p" request)
                   (hunchentoot:get-parameter "s" request)))
      ;; A boot.js from before this protocol, in a page left open across an
      ;; upgrade, which runs each message as a script and would reconnect
      ;; for good: have it reload.
      (websocket-send websocket "location.reload()")
      (websocket-close websocket 4000 "reload")
      (return-from open-page nil))
    (if (null id)
        (let* ((connection (make-connection acceptor (page-request request)))
               (path (getf (connection-request-plist connection) :path)))
          (bordeaux-threads:with-lock-held ((connection-send-lock connection))
            (setf (connection-websocket connection) websocket))
          (send connection (list (format nil "clog['connection_id']='~A'"
                                         (connection-id connection))))
          (log-message :info "Connection ~D opened: ~A, from ~A"
                       (connection-serial connection) path (client-address request))
          (enqueue-function connection
                            (let ((on-connect (acceptor-on-connect acceptor))
                                  (handler (find-route path :server acceptor :default t)))
                              (lambda ()
                                (when on-connect
                                  (funcall on-connect (connection-id connection) handler)))))
          connection)
        (let ((connection (get-connection id)))
          (cond ((and connection (attach connection websocket (or (get-integer request "s") 0)))
                 (log-message :info "Connection ~D reconnected" (connection-serial connection))
                 connection)
                (t
                 (when connection
                   (close-connection connection :code 4000 :reason "reload"))
                 (websocket-close websocket 4000 "reload")
                 nil))))))

(defun serve-clog-websocket (acceptor request)
  "Answer REQUEST, for /clog, by opening a page's websocket and serving it
until it closes: 403 to a page of another origin, 503 past the limits."
  (let ((address (client-address request)))
    (cond ((not (same-origin-p request))
           (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+)
           nil)
          ((not (reserve-websocket acceptor address))
           (log-message :warning "Refused a websocket from ~A: too many open" address)
           (setf (hunchentoot:return-code*) hunchentoot:+http-service-unavailable+)
           nil)
          (t
           (unwind-protect
                (let ((websocket (websocket-upgrade request))
                      (connection nil))
                  (when websocket
                    (call-with-thread-released
                     acceptor
                     (lambda ()
                       (serve-websocket
                        websocket
                        (lambda (websocket)
                          (setf connection
                                (call-logging-errors
                                 (lambda () (open-page acceptor request websocket))
                                 "Opening a page's connection"))
                          ;; Unless it was told to reload.
                          (unless connection
                            (websocket-close websocket 1011 "server error")))
                        (lambda (websocket message)
                          (when connection
                            (call-logging-errors
                             (lambda () (receive connection websocket message))
                             "Connection ~D, a message from the browser"
                             (connection-serial connection))))
                        (lambda (websocket)
                          (when connection
                            (call-logging-errors
                             (lambda ()
                               (websocket-ended connection websocket
                                                (websocket-close-code websocket)))
                             "Connection ~D, closing" (connection-serial connection)))))))))
             (release-websocket acceptor address))
           nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Starting and stopping    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun initialize (on-connect
                   &key
                   (host                 "0.0.0.0")
                   (port                 8080)
                   (acceptor-class       'clog-acceptor)
                   (acceptor-initargs    nil)
                   (extended-routing     nil)
                   (static-root          nil))
  "Start a server, an instance of ACCEPTOR-CLASS (a CLOG-ACCEPTOR) made with
ACCEPTOR-INITARGS too, on HOST and PORT, that sends the files of STATIC-ROOT
(a directory, or NIL), and calls ON-CONNECT in each new connection's thread
with its ID and its route's handler.  It becomes *SERVER*, and is returned.
EXTENDED-ROUTING: see FIND-ROUTE."
  (let ((server (apply #'make-instance acceptor-class
                       :address host
                       :port port
                       :extended-routing extended-routing
                       :static-root static-root
                       :on-connect on-connect
                       acceptor-initargs)))
    (hunchentoot:start server)
    (setf *server* server)
    (log-message :info "HTTP listening on ~A:~A, static root ~A, at most ~D websockets"
                 host port (or static-root "none") (acceptor-websocket-limit server))
    server))

(defun shutdown-clog (&key (server *server*))
  "Stop SERVER and close its connections: their pages reconnect, and reload
when a server is there again."
  (when server
    (hunchentoot:stop server)
    (when (eq server *server*)
      (setf *server* nil))
    (loop for connection in (alexandria:hash-table-values *connections*)
          when (eq server (connection-server connection))
            do (close-connection connection :code 1001 :reason "server shutting down"))))
