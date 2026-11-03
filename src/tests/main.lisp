;;;; SPDX-FileCopyrightText: 2021  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/tests/main
  (:use #:common-lisp)
  (:import-from #:cl-ppcre)
  (:import-from #:rove #:deftest #:ok #:ng #:testing)
  (:import-from #:usocket)
  (:import-from #:project-isidore/tests/test-utils #:with-temp-data-root)
  (:local-nicknames (#:main #:project-isidore/main)
                    (#:layout #:project-isidore/site/layout))
  (:documentation
   "Tests for the application as a whole."))

(in-package #:project-isidore/tests/main)

(defun http-status (port request-line &rest header-lines)
  "Send REQUEST-LINE and HEADER-LINES, verbatim, as an HTTP/1.0 request with
no body to localhost:PORT, and return the status code of the reply.  Raw, so
that a path reaches the server exactly as written."
  (let ((socket (usocket:socket-connect "127.0.0.1" port)))
    (unwind-protect
         (let ((stream (usocket:socket-stream socket))
               (crlf (coerce '(#\Return #\Linefeed) 'string)))
           (format stream "~A~A~{~A~A~}~A" request-line crlf
                   (loop for line in header-lines collect line collect crlf)
                   crlf)
           (finish-output stream)
           (parse-integer (read-line stream) :start 9 :end 12))
      (usocket:socket-close socket))))

(defun http-get (port path &rest header-lines)
  "GET PATH from localhost:PORT over HTTP/1.1 with HEADER-LINES and
Connection: close, and return the reply's status code, its headers as an
alist of lowercase names to values, and its body's length in octets: every
octet after the headers, so that a 304 which ends with an empty chunk shows
it."
  (let ((socket (usocket:socket-connect "127.0.0.1" port
                                        :element-type '(unsigned-byte 8))))
    (unwind-protect
         (let* ((stream (usocket:socket-stream socket))
                (crlf (coerce '(#\Return #\Linefeed) 'string))
                (request (format nil "GET ~A HTTP/1.1~A~{~A~A~}~A" path crlf
                                 (loop for line in (list* "Host: localhost"
                                                          "Connection: close"
                                                          header-lines)
                                       collect line collect crlf)
                                 crlf))
                (reply (make-array 0 :element-type '(unsigned-byte 8)
                                     :adjustable t :fill-pointer 0)))
           (write-sequence (map '(vector (unsigned-byte 8)) #'char-code request) stream)
           (finish-output stream)
           (loop for octet = (read-byte stream nil)
                 while octet do (vector-push-extend octet reply))
           (let* ((end (search #(13 10 13 10) reply))
                  (lines (cl-ppcre:split "\\r\\n" (map 'string #'code-char (subseq reply 0 end)))))
             (values (parse-integer (first lines) :start 9 :end 12)
                     (loop for line in (rest lines)
                           for colon = (position #\: line)
                           collect (cons (string-downcase (subseq line 0 colon))
                                         (string-trim " " (subseq line (1+ colon)))))
                     (- (length reply) end 4))))
      (usocket:socket-close socket))))

(deftest safe-paths
  (dolist (path '("/" "/about" "/assets/global.css" "/assets/blog/archive.html"
                  "/bible/genesis-1-1/genesis-1-5" "/bible/47~4~2" "/bible/1.2.10/"
                  "/bible-search" "/js/boot.js" "/clog" "/slack/events"))
    (ok (main::safe-path-p path) path))
  (dolist (path (list "//etc/passwd" "/assets//etc/passwd" "/~/.bashrc" "/~root/x"
                      "/assets/../src/main.lisp" "/.git/config" "/assets/.hidden"
                      "/SYS:SRC;CODE;TARGET-PATHNAME.LISP" "/a\\b" "/a b" "/a*"
                      (format nil "/caf~C" (code-char 233)) "relative" nil))
    (ng (main::safe-path-p path) path)))

;; What a page quotes from the URL cannot open a tag or leave an attribute.
(deftest html-escaping
  (ok (string= "&lt;img src=x onerror=&quot;a(&#39;1&#39;)&quot;&gt; &amp;amp;"
               (layout:escape-html "<img src=x onerror=\"a('1')\"> &amp;")))
  (ok (string= "/bible/genesis-1-1" (layout:escape-html "/bible/genesis-1-1"))))

(deftest request-guard
  (let ((handler (funcall (main::make-request-guard)
                          (lambda (env) (declare (ignore env)) '(200 () ("app"))))))
    (flet ((status (&rest env) (first (funcall handler env))))
      (ok (= 200 (status :path-info "/about")))
      (ok (= 200 (status :path-info "/slack/events" :content-length 1024)))
      (ok (= 400 (status :path-info "//etc/passwd")))
      (ok (= 413 (status :path-info "/about" :content-length (1+ main::*max-request-octets*))))
      (ok (= 413 (status :path-info "/about" :content-length -1)))
      (let ((headers (make-hash-table :test #'equal)))
        (setf (gethash "transfer-encoding" headers) "chunked")
        (ok (= 411 (status :path-info "/slack/events" :headers headers)))))))

;; If-None-Match is compared weakly, and may list several tags.
(deftest etag-matching
  (let ((etag "\"0123abcd\""))
    (ok (main::etag-matches-p "\"0123abcd\"" etag))
    (ok (main::etag-matches-p "W/\"0123abcd\"" etag))
    (ok (main::etag-matches-p "\"x\", W/\"0123abcd\"" etag))
    (ok (main::etag-matches-p " * " etag))
    (ng (main::etag-matches-p "\"0123abce\"" etag))
    (ng (main::etag-matches-p "0123abcd" etag))
    (ng (main::etag-matches-p "" etag))))

;; A file's tag follows its contents, not its name.
(deftest file-etags
  (uiop:with-temporary-file (:pathname a :stream out :direction :output)
    (write-string "body { color: red; }" out)
    (finish-output out)
    (uiop:with-temporary-file (:pathname b :stream out :direction :output)
      (write-string "body { color: red; }" out)
      (finish-output out)
      (let ((tag (main::file-etag a)))
        (ok (cl-ppcre:scan "^\"[0-9a-f]{32}\"$" tag))
        (ok (string= tag (main::file-etag b)))
        (with-open-file (out b :direction :output :if-exists :supersede)
          (write-string "body { color: blue; }" out))
        (ng (string= tag (main::file-etag b)))))))

;; The application starts and stops cleanly, under a data root of its own,
;; and serves nothing outside its static root.  Port 8293, as 8080 is the
;; production server's.
(deftest can-app-init-and-exit
    (with-temp-data-root ()
      (ok (main:initialize-application :port 8293))
      (unwind-protect
           (progn
             (ok (= 200 (http-status 8293 "GET /assets/global.css HTTP/1.0")))
             ;; Without the guard each of these is a file outside the static
             ;; root: an absolute path (this system's own .asd), the home
             ;; directory, and the checkout's .gitignore.
             (dolist (path (list (format nil "/~A" (namestring
                                                    (asdf:system-relative-pathname
                                                     :project-isidore "project-isidore.asd")))
                                 "/%2Fetc/passwd"
                                 "/~/.profile"
                                 "/.gitignore"))
               (ok (= 400 (http-status 8293 (format nil "GET ~A HTTP/1.0" path))) path))
             ;; CLOG would make a string of this many characters and exhaust
             ;; the heap, taking the process down with it.
             (ok (= 413 (http-status 8293 "POST /about HTTP/1.0"
                                     "Content-Type: application/x-www-form-urlencoded"
                                     "Content-Length: 1000000000000")))
             (ok (= 200 (http-status 8293 "GET /assets/global.css HTTP/1.0")))
             (testing "a file is revalidated by its ETag, never by its 1970 write date"
               (let ((length (with-open-file (in (asdf:system-relative-pathname
                                                  :project-isidore "assets/global.css")
                                                 :element-type '(unsigned-byte 8))
                               (file-length in))))
                 (multiple-value-bind (status headers body)
                     (http-get 8293 "/assets/global.css"
                               "If-Modified-Since: Thu, 01 Jan 1970 00:00:01 GMT")
                   (ok (= 200 status))
                   (ok (= length body))
                   (ok (equal "no-cache" (cdr (assoc "cache-control" headers :test #'string=))))
                   (ng (assoc "last-modified" headers :test #'string=))
                   (ok (cl-ppcre:scan "^text/css" (cdr (assoc "content-type" headers
                                                              :test #'string=))))
                   (let ((etag (cdr (assoc "etag" headers :test #'string=))))
                     (ok (stringp etag))
                     (dolist (tag (list etag (format nil "W/~A" etag)))
                       (multiple-value-bind (status headers body)
                           (http-get 8293 "/assets/global.css"
                                     (format nil "If-None-Match: ~A" tag)
                                     "If-Modified-Since: Thu, 01 Jan 1970 00:00:01 GMT")
                         (ok (= 304 status) tag)
                         (ok (= 0 body) tag)
                         (ok (equal etag (cdr (assoc "etag" headers :test #'string=))))
                         (ok (cl-ppcre:scan "^text/css" (cdr (assoc "content-type" headers
                                                                    :test #'string=))))
                         (ng (assoc "transfer-encoding" headers :test #'string=))))
                     (multiple-value-bind (status headers body)
                         (http-get 8293 "/assets/global.css" "If-None-Match: \"stale\"")
                       (declare (ignore headers))
                       (ok (= 200 status))
                       (ok (= length body))))))))
        (ok (main:terminate-application)))))
