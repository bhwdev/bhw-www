;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) David Botton                                                      ;;;;
;;;;                                                                       ;;;;
;;;; clog-system.lisp                                                      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - CLOG System
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *clog-debug* nil
  "Set a debug hook that is called for every event with (event data)
that must be (funcall event data).")

;;;;;;;;;;;;;;;;
;; initialize ;;
;;;;;;;;;;;;;;;;

(defun on-connect (connection-id on-new-window)
  "Start a new page, in its connection's thread: make its CLOG-BODY and call
ON-NEW-WINDOW, its route's handler, with it.  (Private)"
  (let ((body (make-clog-body connection-id)))
    (setf (connection-data-item body "clog-path") (request-path body)
          (connection-data-item body "clog-body") body
          (connection-data-item body "clog-debug") *clog-debug*)
    (set-on-event-with-data (window body) "gc"
                            (lambda (obj data)
                              (perform-gc obj data)))
    (cond ((null on-new-window)
           (create-div body :content "No route to on-new-window"))
          (*clog-debug*
           (funcall *clog-debug* on-new-window body))
          (t
           (funcall on-new-window body)))))

(defun initialize
    (on-new-window-handler
     &key
       (host             "0.0.0.0")
       (port             8080)
       (acceptor-class   'clog-connection:clog-acceptor)
       (acceptor-initargs nil)
       (extended-routing nil)
       (static-root      (clog-connection:static-files-directory)))
  "Start CLOG's web server on HOST and PORT (when PORT is NIL or 0, a free
one), and serve ON-NEW-WINDOW-HANDLER's page at /.  Returns the server.

The server is a Hunchentoot acceptor of ACCEPTOR-CLASS, a subclass of
CLOG-CONNECTION:CLOG-ACCEPTOR, made with ACCEPTOR-INITARGS as well (e.g.
:READ-TIMEOUT, :TASKMASTER, or for a class that also inherits from
HUNCHENTOOT:SSL-ACCEPTOR, its certificate and key); extend the server with
methods on that class.  It sends the files under STATIC-ROOT (by default
CLOG's own static files; NIL for none); CLOG's own (boot.js, w3.css) are
served whatever it is.  With EXTENDED-ROUTING a route also serves the paths
under it (see SET-ON-NEW-WINDOW).

If CLOG is running already, this sets ON-NEW-WINDOW-HANDLER for / (unless it
is NIL) and changes nothing else."
  (unless clog-connection:*server*
    (clog-connection:initialize #'on-connect
                                :host host
                                :port (if (member port '(nil 0))
                                          (clog-connection:random-port)
                                          port)
                                :acceptor-class acceptor-class
                                :acceptor-initargs acceptor-initargs
                                :extended-routing extended-routing
                                :static-root static-root))
  (when on-new-window-handler
    (set-on-new-window on-new-window-handler :path "/"))
  clog-connection:*server*)

;;;;;;;;;;;;;;;;;;;;;;;
;; set-on-new-window ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun set-on-new-window (on-new-window-handler
                          &key (path "/") (server clog-connection:*server*))
  "Serve a page at PATH, starting with /, made by ON-NEW-WINDOW-HANDLER: it is
called with the page's CLOG-BODY, in the page's own thread, where its events
are handled too, one at a time, after it returns.  It should return once it
has built the page and set its handlers; to do something later, use timers
\(SCHEDULE) or ADD-CLOSE-HOOK.

With :EXTENDED-ROUTING the page also serves the paths under PATH that have
no route of their own: the longest route a path is under serves it.  PATH
:DEFAULT serves any path that no route serves and no file answers, as a 404.
A NIL ON-NEW-WINDOW-HANDLER removes the route."
  (clog-connection:add-route path on-new-window-handler :server server))

;;;;;;;;;;;;;;;;;;
;; is-running-p ;;
;;;;;;;;;;;;;;;;;;

(defun is-running-p ()
  (and clog-connection:*server* t))

;;;;;;;;;;;;;;
;; shutdown ;;
;;;;;;;;;;;;;;

(defun shutdown ()
  "Shutdown CLOG."
  (clog-connection:shutdown-clog))

;;;;;;;;;;;;;;;;
;; debug-mode ;;
;;;;;;;;;;;;;;;;

(defun debug-mode (obj)
  "Turn on browser console debugging for OBJ's connection."
  (clog-connection:debug-mode (connection-id obj)))
