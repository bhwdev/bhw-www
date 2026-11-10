;;;; SPDX-FileCopyrightText: 2021  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/main
  (:use #:common-lisp)
  (:import-from #:alexandria)
  (:import-from #:bordeaux-threads)
  (:import-from #:cffi)
  (:import-from #:cl+ssl)
  (:import-from #:cl-ppcre)
  (:import-from #:clog #:initialize #:is-running-p #:shutdown)
  (:import-from #:dexador)
  (:import-from #:flexi-streams)
  (:import-from #:ironclad)
  (:import-from #:slynk)
  (:import-from #:usocket)
  (:local-nicknames (#:conn #:clog-connection)
                    (#:layout #:project-isidore/site/layout)
                    (#:pages #:project-isidore/site/pages)
                    (#:bible-db #:project-isidore/bible/database)
                    (#:bible #:project-isidore/bible/text)
                    (#:reader #:project-isidore/bible/reader)
                    (#:bot-db #:project-isidore/slack-bot/database)
                    (#:bot #:project-isidore/slack-bot/bot))
  (:export #:initialize-application
           #:terminate-application
           #:application-toplevel)
  (:documentation
   "Project Isidore's one process: the personal site, the bible reader and the
Slack bot, served by one CLOG (Hunchentoot) web server.

INITIALIZE-APPLICATION opens each part's database, starts the bot's
reminder scheduler and the web server, and has each part register its pages;
the bot's endpoints, and the colour theme's /theme, are Lack middleware in
front of CLOG.  Loading the system
does none of this, so loading has no side effects and the build can save the
image straight after loading it.  APPLICATION-TOPLEVEL is the executable's
entry point."))

(in-package #:project-isidore/main)

(defvar *server* nil
  "Non-NIL when the CLOG web server is running.")

;; Override CLOG-CONNECTION::RANDOM-HEX-STRING to draw from ironclad on
;; every platform, as upstream already does on Windows; remove it once
;; Guix's CLOG includes rabbibotton/clog#458.  Elsewhere upstream draws
;; from a shared cl-isaac context, which fails three ways.  From cl-isaac
;; 1.0.8 on, RAND64 (RAND32 off x86-64) underflows its typed RANDCNT slot
;; at the end of each 256-word block and signals a TYPE-ERROR before
;; reaching its reset branch, so the PRNG stays wedged: CLOG can no
;; longer mint connection IDs, and fresh browser sessions never receive
;; the bootstrap frame.  Lack patched cl-isaac's RAND32 and RAND64 until
;; fukamachi/lack@f3302ce, which dropped cl-isaac.  The context has no
;; lock, though connections arrive on separate threads.  And it is
;; seeded at load time, so a saved image such as bin/project-isidore
;; restores the same state on every start and issues the same connection
;; IDs -- the handles a reconnecting browser presents to resume its
;; session.
(defun conn::random-hex-string ()
  "Generate a cryptographic 128-bit random hex string for connection IDs."
  (ironclad:byte-array-to-hex-string (ironclad:random-data 16)))

;;; Request guard

(defparameter *max-request-octets* (* 1024 1024)
  "The largest request body the server accepts.  Nothing it serves takes an
upload: Slack's requests are a few kilobytes, and the browser talks to CLOG
over its websocket.")

(defun safe-path-p (path)
  "True when PATH, a request's decoded path, is one the site may serve:
'/'-separated segments of letters, digits and -_.~, none of them empty or
beginning with a dot or a tilde.

CLOG hands any path no page claims to Lack's file server, which drops the
leading slash and merges the rest into the static root as an SBCL namestring.
That rejects \"..\" but not the forms that make the result absolute or
point elsewhere: \"//etc/passwd\" (also \"/%2Fetc/passwd\") reads
/etc/passwd, \"/~/x\" the server's home directory, and \"/SYS:SRC;...\"
a logical pathname.  Dot-segments are refused too, so a checkout's .git
is not served."
  (and (stringp path)
       (cl-ppcre:scan "^(?:/[A-Za-z0-9_-][A-Za-z0-9_.~-]*)*/?$" path)
       t))

(defun make-request-guard ()
  "Return a Lack middleware, to run before every other, that answers 400 to
a path that is not SAFE-PATH-P, 411 to a body without a Content-Length, and
413 to one longer than *MAX-REQUEST-OCTETS*.

The body limits protect handlers that size a buffer from Content-Length or
read the body whole before anything is authenticated: CLOG makes a string
of Content-Length characters for any form POST to one of its pages, so a
single request declaring a huge length exhausts the heap and kills the
process, and the Slack endpoints read the body before checking its
signature."
  (lambda (app)
    (lambda (env)
      (let ((length (getf env :content-length))
            (headers (getf env :headers)))
        (cond ((not (safe-path-p (getf env :path-info)))
               '(400 (:content-type "text/plain") ("Bad Request")))
              ((and headers (gethash "transfer-encoding" headers))
               '(411 (:content-type "text/plain") ("Length Required")))
              ((and length (not (<= 0 length *max-request-octets*)))
               '(413 (:content-type "text/plain") ("Payload Too Large")))
              (t (funcall app env)))))))

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

(defun stream-file (status headers pathname)
  "A Lack response of STATUS and HEADERS whose body is the contents of the
file PATHNAME, sent 64 KiB at a time.  A pathname body would not do: Clack's
Hunchentoot handler gives it to HUNCHENTOOT:HANDLE-STATIC-FILE, which adds
the file's write date as Last-Modified and answers an If-Modified-Since that
names it with 304."
  (lambda (responder)
    (let ((writer (funcall responder (list status headers)))
          (buffer (make-array 65536 :element-type '(unsigned-byte 8))))
      (with-open-file (in pathname :element-type '(unsigned-byte 8))
        (loop for end = (read-sequence buffer in)
              while (plusp end)
              do (funcall writer buffer :end end)))
      (funcall writer nil :close t))))

(defun make-file-validator ()
  "Return a Lack middleware, to run after every other, that has a browser
revalidate each file the static server sends, every time it uses its copy,
by an entity tag of the file's contents, and that puts the reader's colour
theme into each HTML file.

The files are in the Guix store, where every write date is 1970-01-01
00:00:01 UTC.  Lack's file server and Hunchentoot each send that as
Last-Modified and answer 304 to an If-Modified-Since naming it, so a browser
kept its copy of a file through every deploy: with no Cache-Control, a
Last-Modified that old leaves a copy fresh for over five years (a tenth of
its age, RFC 9111 section 4.2.2), and asking again only got the 304.

So this middleware hides If-Modified-Since from the file server, and sends
each file the server answers with (a 200 whose body is a pathname) itself,
with an ETag (FILE-ETAG) and Cache-Control: no-cache in place of
Last-Modified.  An If-None-Match that names the ETag gets a 304 with the
same Content-Type, lest Hunchentoot's default text/html replace the cached
copy's.

An HTML file -- the Org blog's pages -- goes out with the theme the
request's cookie names (LAYOUT:INJECT-THEME), as the pages made here do, so
its ETag names the theme too, and it varies with the Cookie header."
  (lambda (app)
    (lambda (env)
      (let ((headers (getf env :headers)))
        (when headers
          (remhash "if-modified-since" headers))
        (let ((response (funcall app env)))
          (if (not (and (consp response)
                        (eql 200 (first response))
                        (pathnamep (third response))))
              response
              (let* ((pathname (third response))
                     (theme (and (member (pathname-type pathname) '("html" "htm")
                                         :test #'equalp)
                                 (layout:theme-preference env)))
                     (etag (if theme
                               (format nil "\"~A-~A\"" (string-trim "\"" (file-etag pathname)) theme)
                               (file-etag pathname)))
                     (validated (list* :etag etag :cache-control "no-cache"
                                       (append
                                        (and theme (list :vary "Cookie"))
                                        (alexandria:remove-from-plist
                                         (second response)
                                         :last-modified :etag :cache-control :vary))))
                     (if-none-match (and headers (gethash "if-none-match" headers))))
                (cond ((and if-none-match (etag-matches-p if-none-match etag))
                       (list 304 (alexandria:remove-from-plist validated :content-length) '()))
                      (theme
                       (let ((body (flexi-streams:string-to-octets
                                    (layout:inject-theme
                                     (uiop:read-file-string pathname :external-format :utf-8)
                                     theme)
                                    :external-format :utf-8)))
                         (list 200
                               (list* :content-length (length body)
                                      (alexandria:remove-from-plist validated :content-length))
                               body)))
                      (t
                       (stream-file 200 validated pathname))))))))))

(defun make-theme-toggle ()
  "Return a Lack middleware that answers the navbar's theme toggle, a POST
to /theme (LAYOUT:THEME-TOGGLE-RESPONSE)."
  (lambda (app)
    (lambda (env)
      (if (and (eq :post (getf env :request-method))
               (equal "/theme" (getf env :path-info)))
          (layout:theme-toggle-response env)
          (funcall app env)))))

(defun wait-for-port-ready (port &key (host "127.0.0.1") (timeout 5.0) (interval 0.05))
  "Block until the web server on HOST:PORT has fully started, or TIMEOUT
seconds elapse.  Returns T on success, NIL on timeout.

CLOG's INITIALIZE returns as soon as clack:clackup hands back a handler
struct, but the underlying hunchentoot acceptor finishes START in a separate
thread, in two steps: START-LISTENING binds the socket, so the port accepts
connections, and only then does EXECUTE-ACCEPTOR store the listener thread in
the taskmaster's ACCEPTOR-PROCESS slot.  If TERMINATE-APPLICATION runs between
those steps, clack's BT2:DESTROY-THREAD cleanup calls HUNCHENTOOT:STOP, whose
SHUTDOWN reads the still-unbound slot and signals UNBOUND-SLOT in that thread,
and an unhandled error in any thread of a non-interactive SBCL, such as the
test suite's, terminates the whole process.  So this waits for both signs:
the port accepting connections and a \"hunchentoot-listener-HOST:PORT\"
thread existing, plus one more INTERVAL for the slot assignment that
immediately follows the thread's creation."
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second)))
        (listener-name (format nil "hunchentoot-listener-~A:~A" host port)))
    (flet ((port-ready-p ()
             (handler-case
                 (let ((sock (usocket:socket-connect host port)))
                   (usocket:socket-close sock)
                   t)
               (error () nil)))
           (listener-thread-p ()
             (find listener-name (bordeaux-threads:all-threads)
                   :key #'bordeaux-threads:thread-name :test #'equal))
           (expired-p ()
             (> (get-internal-real-time) deadline)))
      (loop until (and (port-ready-p) (listener-thread-p))
            do (when (expired-p)
                 (return-from wait-for-port-ready nil))
               (sleep interval))
      (sleep interval)
      t)))

(defun initialize-application (&key (port 8080))
  "Open the databases, start the Slack bot's reminder scheduler and serve the
site, the bible reader and the bot's endpoints on PORT, on localhost."
  (terminate-application)
  (bible-db:open-database)
  (bible:open-search-index)
  (bot-db:open-database)
  (bot:load-config)
  (bot:start-reminder-scheduler)
  (initialize 'pages:on-index-handler
              :host "127.0.0.1"
              :port port
              :extended-routing t
              ;; The first is outermost: the guard sees every request first,
              ;; and the file validator, last, sees what CLOG answers.
              :lack-middleware-list (list (make-request-guard)
                                          (bot:make-slack-middleware)
                                          (make-theme-toggle)
                                          (make-file-validator))
              :static-root (namestring
                            (asdf:system-relative-pathname :project-isidore "./")))
  (pages:register-routes)
  (reader:register-routes)
  ;; CLOG's own JS files (boot.js, jquery)
  (conn:add-plugin-path "^/js/"
                        (merge-pathnames "static-files/"
                                         (asdf:system-source-directory :clog)))
  (conn:add-plugin-path "^/assets/"
                        (namestring (asdf:system-relative-pathname :project-isidore "./")))
  (conn:add-plugin-path "^/favicon\\.ico"
                        (namestring (asdf:system-relative-pathname :project-isidore "assets/")))
  (setf *server* t)
  (format t "~%
========================================
Project Isidore
========================================
Copyright (c) 2021  Ben H. W. <foss@bhw.name>
Project Isidore is free software, provided as is, and comes with ABSOLUTELY NO
WARRANTY. This program is licensed under the GNU AFFERO GENERAL PUBLIC LICENSE
Version 3, 19 November 2007 You are welcome to redistribute the program or any
parts hereof under certain conditions. Please visit
https://www.gnu.org/licenses/agpl-3.0.html for License details.
Homepage: https://www.bhw.name/assets/blog/project-isidore-doc.html
Source code repository: https://github.com/bhwdev/project-isidore
Navigate to http://localhost:~A to continue... ~%" port)
  (wait-for-port-ready port)
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
