;;;; SPDX-FileCopyrightText: 2021  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:bhw/main
  (:use #:common-lisp)
  (:import-from #:alexandria)
  (:import-from #:bordeaux-threads)
  (:import-from #:cffi)
  (:import-from #:cl+ssl)
  (:import-from #:cl-ppcre)
  (:import-from #:clog #:initialize #:is-running-p #:shutdown)
  (:import-from #:dexador)
  (:import-from #:flexi-streams)
  (:import-from #:hunchentoot)
  (:import-from #:ironclad)
  (:import-from #:slynk)
  (:local-nicknames (#:conn #:clog-connection)
                    (#:layout #:bhw/site/layout)
                    (#:pages #:bhw/site/pages)
                    (#:bible-db #:bhw/bible/database)
                    (#:bible #:bhw/bible/text)
                    (#:reader #:bhw/bible/reader)
                    (#:bot-db #:bhw/slack-bot/database)
                    (#:bot #:bhw/slack-bot/bot)
                    (#:plp-db #:bhw/plp/database)
                    (#:plp-live #:bhw/plp/live)
                    (#:plp #:bhw/plp/console))
  (:export #:initialize-application
           #:terminate-application
           #:application-toplevel)
  (:documentation
   "bhw-www's one process: the personal site, the bible reader and the
Slack bot, served by one web server: SITE-ACCEPTOR, the Hunchentoot acceptor
of the fork of CLOG in clog/.

INITIALIZE-APPLICATION opens each part's database, starts the bot's
reminder scheduler and the web server, and has each part register its pages;
SITE-ACCEPTOR answers the bot's endpoints and the colour theme's /theme
before CLOG sees the request.  Loading the system
does none of this, so loading has no side effects and the build can save the
image straight after loading it.  APPLICATION-TOPLEVEL is the executable's
entry point."))

(in-package #:bhw/main)

(defvar *server* nil
  "Non-NIL when the CLOG web server is running.")

;;; Request guard

(defparameter *max-request-octets* (* 1024 1024)
  "The largest request body the server accepts.  Nothing it serves takes an
upload: Slack's requests are a few kilobytes, and the browser talks to CLOG
over its websocket.")

(defun safe-path-p (path)
  "True when PATH, a request's decoded path, is one the site may serve:
'/'-separated segments of letters, digits and -_.~, none of them empty or
beginning with a dot or a tilde.

Any other path is refused before CLOG or the bot sees it.  The fork of
CLOG's file server applies the same rule to the files it sends
\(CLOG-CONNECTION::STATIC-FILE); upstream CLOG hands such a path to Lack's
file server, which merges it into the static root as an SBCL namestring, so
that \"//etc/passwd\" (also \"/%2Fetc/passwd\") reads /etc/passwd, \"/~/x\"
the server's home directory, and \"/SYS:SRC;...\" a logical pathname.
Dot-segments are refused too, so a checkout's .git is not served."
  (and (stringp path)
       (cl-ppcre:scan "^(?:/[A-Za-z0-9_-][A-Za-z0-9_.~-]*)*/?$" path)
       t))

(defun guard-response (env)
  "The response refusing the request plist ENV, or NIL to let it through:
400 to a path that is not SAFE-PATH-P, 411 to a body without a
Content-Length, and 413 to one longer than *MAX-REQUEST-OCTETS*.

The body limits protect what sizes a buffer from Content-Length or reads
the body whole before anything is authenticated: Hunchentoot reads a body it
has not been asked for into an array of Content-Length octets before it
replies, as it does a form posted to one of CLOG's pages, so a single request
declaring a huge length would exhaust the heap and kill the process; and the
Slack endpoints read the body before checking its signature."
  (let ((length (getf env :content-length)))
    (cond ((not (safe-path-p (getf env :path-info)))
           '(400 (:content-type "text/plain") ("Bad Request")))
          ((layout:request-header env "transfer-encoding")
           '(411 (:content-type "text/plain") ("Length Required")))
          ((and length (not (<= 0 length *max-request-octets*)))
           '(413 (:content-type "text/plain") ("Payload Too Large"))))))

;;; Requests and responses as data
;;;
;;; The guard, the bot's endpoints and the theme toggle take a request as a
;;; plist and return a response as a list, so that they, and their tests,
;;; need no server: REQUEST-ENV and REPLY convert at the edge.

(defun request-env (request)
  "The Hunchentoot REQUEST as a plist, without its body: :REQUEST-METHOD (a
keyword), :PATH-INFO (the decoded path), :CONTENT-LENGTH (an integer, or NIL)
and :HEADERS (a hash table of lowercase names)."
  (list :request-method (hunchentoot:request-method request)
        :path-info (hunchentoot:script-name request)
        :content-length (let ((value (hunchentoot:header-in :content-length request)))
                          (and value (parse-integer value :junk-allowed t)))
        :headers (let ((headers (make-hash-table :test #'equal)))
                   (loop for (name . value) in (hunchentoot:headers-in request)
                         do (setf (gethash (string-downcase name) headers) value))
                   headers)))

(defun reply (response)
  "Make RESPONSE, a list (STATUS HEADERS BODY) with HEADERS a plist and BODY
a list of strings, the reply to the current request, and return the body for
Hunchentoot to send, with its Content-Length."
  (destructuring-bind (status headers body) response
    (setf (hunchentoot:return-code*) status)
    (loop for (name value) on headers by #'cddr
          do (case name
               (:content-type (setf (hunchentoot:content-type*) value))
               (:content-length)
               (t (setf (hunchentoot:header-out name) value))))
    (format nil "~{~A~}" body)))

(defun theme-toggle-p (env)
  "True when the request plist ENV is the navbar's theme toggle, a POST to
/theme (LAYOUT:THEME-TOGGLE-RESPONSE)."
  (and (eq :post (getf env :request-method))
       (equal "/theme" (getf env :path-info))))

;;; Static files

(defvar *file-etags* (make-hash-table :test #'equal)
  "FILE-ETAG's cache: entity tags by (truename write-date length).")

(defvar *file-etags-lock* (bordeaux-threads:make-lock "file-etags"))

(defun file-etag (pathname)
  "The strong entity tag of the file PATHNAME, quoted: the first 128 bits of
the SHA-256 of its contents, in hex.  Cached for as long as the file keeps
its write date and length, which in the store is for good."
  (let ((key (with-open-file (in pathname :element-type '(unsigned-byte 8))
               (list (namestring (truename in)) (file-write-date in) (file-length in)))))
    (or (bordeaux-threads:with-lock-held (*file-etags-lock*)
          (gethash key *file-etags*))
        (let ((etag (format nil "\"~A\""
                            (ironclad:byte-array-to-hex-string
                             (ironclad:digest-file :sha256 pathname) :end 16))))
          (bordeaux-threads:with-lock-held (*file-etags-lock*)
            (setf (gethash key *file-etags*) etag))))))

(defun etag-matches-p (if-none-match etag)
  "True when IF-NONE-MATCH, the value of an If-None-Match header, is * or
lists ETAG.  The comparison is the weak one RFC 9110 (section 13.1.2) has
If-None-Match use: a W/ in front of either tag is ignored."
  (flet ((opaque (tag)
           (if (and (> (length tag) 2) (string= "W/" tag :end2 2))
               (subseq tag 2)
               tag)))
    (and (or (string= "*" (string-trim " " if-none-match))
             (member (opaque etag)
                     (cl-ppcre:all-matches-as-strings "(?:W/)?\"[^\"]*\"" if-none-match)
                     :key #'opaque :test #'string=))
         t)))

(defun send-file (pathname)
  "Send the contents of the file PATHNAME as the reply's body, with its
Content-Length, 64 KiB at a time, and return NIL.  Not
HUNCHENTOOT:HANDLE-STATIC-FILE, which adds the file's write date as
Last-Modified and answers an If-Modified-Since that names it with 304."
  (with-open-file (in pathname :element-type '(unsigned-byte 8))
    (setf (hunchentoot:content-length*) (file-length in))
    (let ((out (hunchentoot:send-headers))
          (buffer (make-array 65536 :element-type '(unsigned-byte 8))))
      (loop for end = (read-sequence buffer in)
            while (plusp end)
            do (write-sequence buffer out :end end))
      (finish-output out)))
  nil)

;;; The web server

(defclass site-acceptor (conn:clog-acceptor)
  ()
  (:documentation "The web server: CLOG's, which serves the pages and files,
with the request guard, the bot's endpoints and the theme toggle answered
first, and files sent with entity tags."))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor site-acceptor) request)
  "Answer REQUEST with the guard's refusal, the bot's or the theme toggle's
response, or else as CLOG does.  A refusal neither reads the body nor keeps
the connection, whose unread body would be taken for the next request."
  (let* ((env (request-env request))
         (refusal (guard-response env)))
    (cond (refusal
           ;; Taken as a stream, the body is not read when the reply goes out.
           (hunchentoot:raw-post-data :request request :want-stream t)
           (setf (hunchentoot:aux-request-value 'close-connection request) t)
           (reply refusal))
          ((bot:slack-request-p env)
           (reply (bot:slack-response
                   (if (getf env :content-length)
                       (list* :raw-body (hunchentoot:raw-post-data :request request
                                                                   :want-stream t)
                              env)
                       env))))
          ((theme-toggle-p env)
           (reply (layout:theme-toggle-response env)))
          ;; The presenter's waiting images are under the data root, which
          ;; CLOG's file server does not reach.
          ((plp-live:image-file (getf env :path-info))
           (conn:serve-static-file acceptor (plp-live:image-file (getf env :path-info))))
          (t
           (call-next-method)))))

;; Hunchentoot asks this as it sends the reply, to decide whether to keep
;; the connection; a refused request has it answer no.
(defmethod hunchentoot:acceptor-persistent-connections-p :around ((acceptor site-acceptor))
  (and (not (and (boundp 'hunchentoot:*request*)
                 hunchentoot:*request*
                 (hunchentoot:aux-request-value 'close-connection)))
       (call-next-method)))

(defmethod conn:serve-static-file ((acceptor site-acceptor) pathname)
  "Send the file PATHNAME, which a browser is to revalidate every time it
uses its copy, by an entity tag of the file's contents, and with the
reader's colour theme put into it if it is HTML.

The files are in the Guix store, where every write date is 1970-01-01
00:00:01 UTC.  Hunchentoot's file server, as Lack's did, sends that as
Last-Modified and answers 304 to an If-Modified-Since naming it, so a browser
would keep its copy of a file through every deploy: with no Cache-Control, a
Last-Modified that old leaves a copy fresh for over five years (a tenth of
its age, RFC 9111 section 4.2.2), and asking again only gets the 304.

So this sends each file with an ETag (FILE-ETAG) and Cache-Control: no-cache
in place of Last-Modified, and ignores If-Modified-Since.  An If-None-Match
that names the ETag gets a 304 with the same Content-Type, lest Hunchentoot's
default text/html replace the cached copy's.

An HTML file -- the Org blog's pages -- goes out with the theme the
request's cookie names (LAYOUT:INJECT-THEME), as the pages made here do, so
its ETag names the theme too, and it varies with the Cookie header."
  (let* ((request hunchentoot:*request*)
         (theme (and (member (pathname-type pathname) '("html" "htm") :test #'equalp)
                     (layout:theme-preference (request-env request))))
         (etag (if theme
                   (format nil "\"~A-~A\"" (string-trim "\"" (file-etag pathname)) theme)
                   (file-etag pathname)))
         (if-none-match (hunchentoot:header-in :if-none-match request)))
    (setf (hunchentoot:content-type*) (if theme
                                           "text/html; charset=utf-8"
                                           (conn:file-content-type pathname))
          (hunchentoot:header-out :etag) etag
          (hunchentoot:header-out :cache-control) "no-cache")
    (when theme
      (setf (hunchentoot:header-out :vary) "Cookie"))
    (cond ((and if-none-match (etag-matches-p if-none-match etag))
           (setf (hunchentoot:return-code*) hunchentoot:+http-not-modified+)
           nil)
          (theme
           (flexi-streams:string-to-octets
            (layout:inject-theme (uiop:read-file-string pathname :external-format :utf-8)
                                 theme)
            :external-format :utf-8))
          (t
           (send-file pathname)))))

(defun initialize-application (&key (port 8080))
  "Open the databases, start the Slack bot's reminder scheduler and serve the
site, the bible reader and the bot's endpoints on PORT, on localhost.

No static root: the files the site serves are under /assets/ (and
/favicon.ico), and CLOG serves its own; the checkout's other files, its
source, are not served."
  (terminate-application)
  (bible-db:open-database)
  (bible:open-search-index)
  (bot-db:open-database)
  (bot:load-config)
  (bot:start-reminder-scheduler)
  (plp-db:open-database)
  (plp-db:load-config)
  (initialize 'pages:on-index-handler
              :host "127.0.0.1"
              :port port
              :extended-routing t
              :acceptor-class 'site-acceptor
              :static-root nil)
  (pages:register-routes)
  (reader:register-routes)
  (plp:register-routes)
  (conn:add-plugin-path "^/assets/"
                        (namestring (asdf:system-relative-pathname :bhw "./")))
  (conn:add-plugin-path "^/favicon\\.ico"
                        (namestring (asdf:system-relative-pathname :bhw "assets/")))
  (setf *server* t)
  (format t "~%
========================================
bhw-www
========================================
Copyright (c) 2021  Ben H. W. <foss@bhw.name>
bhw-www is free software, provided as is, and comes with ABSOLUTELY NO
WARRANTY. This program is licensed under the GNU AFFERO GENERAL PUBLIC LICENSE
Version 3, 19 November 2007 You are welcome to redistribute the program or any
parts hereof under certain conditions. Please visit
https://www.gnu.org/licenses/agpl-3.0.html for License details.
Homepage: https://www.bhw.name/assets/blog/bhw-www-doc.html
Source code repository: https://github.com/bhwdev/bhw-www
Navigate to http://localhost:~A to continue... ~%" port)
  t)

(defun terminate-application ()
  "Stop what INITIALIZE-APPLICATION started and close the databases.
Idempotent."
  (bot:stop-reminder-scheduler)
  (when (or *server* (is-running-p))
    (ignore-errors (shutdown))
    (setf *server* nil))
  (bible:close-search-index)
  (bible-db:close-database)
  (bot-db:close-database)
  (plp-db:close-database)
  t)

(defun reset-network-state ()
  "Drop the foreign state a saved image inherits from the process that built
it: cl+ssl's foreign pointers (its Lisp BIO method, the SSL_CTX, the C string
it hands to OpenSSL's error queue, which it allocates when it loads) and
dexador's pool of keep-alive connections.  In the restarted image those
addresses and sockets belong to nobody: OpenSSL faults on them (\"CORRUPTION
WARNING ... Memory fault\" with the PC in libcrypto's BIO_new_ex) and a
request to a pooled host fails.  Registered below as a UIOP image-restore
hook, which runs when the saved image starts."
  ;; forget the SSL_CTX and method so the first TLS use re-initialises cl+ssl,
  ;; which also makes a fresh Lisp BIO method
  (cl+ssl:reload)
  (when (boundp 'cl+ssl::*file-name*)
    (setf cl+ssl::*file-name* (cffi:foreign-string-alloc "cl+ssl/src/bio.lisp")))
  ;; a new, empty pool: closing the inherited connections would call OpenSSL
  ;; on the same stale pointers
  (setf dexador.connection-cache:*connection-pool*
        (dexador.connection-cache:make-connection-pool)))

(uiop:register-image-restore-hook 'reset-network-state nil)

(defun application-toplevel ()
  "The executable's entry point: serve on $PORT (default 8080) until SIGINT.
On the default port a Slynk server on localhost:4005 allows a remote REPL."
  ;; CLOG runs each page's handler, and each event's, in a thread of its own,
  ;; and with *BREAK-ON-ERROR* (its default) lets an error there reach the
  ;; debugger, which the executable has disabled: SBCL then exits, and one
  ;; failing page takes the whole server down.  Off, CLOG catches the error
  ;; and prints it.  A REPL that calls INITIALIZE-APPLICATION keeps the
  ;; debugger.
  (setf conn:*break-on-error* nil)
  (let ((port (let ((env (uiop:getenvp "PORT")))
                (if env (parse-integer env) 8080))))
    (when (= 8080 port)
      (slynk:create-server :port 4005 :dont-close t)
      ;; A Slynk contrib defines this, so it is not known when this compiles.
      (setf (symbol-value 'slynk:*use-dedicated-output-stream*) nil))
    (initialize-application :port port)
    (format t "~% Close this window or press Control+C to exit the program...~%")
    (handler-case (loop (sleep 1))
      (sb-sys:interactive-interrupt ()
        (format *error-output* "~%Aborting.~&~%")
        (slynk:stop-server 4005)
        (terminate-application)
        (format t "~%Server successfully stopped.~%")
        (uiop:quit)))))
