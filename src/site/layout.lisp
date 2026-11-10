;;;; SPDX-FileCopyrightText: 2021  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/site/layout
  (:use #:common-lisp)
  (:import-from #:cl-ppcre)
  (:import-from #:flexi-streams)
  (:import-from #:quri)
  (:export #:escape-html
           #:page
           #:html-response
           #:theme-preference
           #:theme-toggle-response
           #:inject-theme)
  (:documentation
   "The frame every page of the site shares -- stylesheet, colour theme,
navbar and footer, around content a page fills in -- and the HTML responses
that carry it.  Pages are whole HTML documents made on the server: nothing
on them needs a script.

The colour theme is a cookie: \"light\" (parchment, the default), \"dark\"
or \"auto\" (as the system prefers).  The navbar's toggle posts to /theme,
which sets the next one and sends the reader back.  A page states the
choice as <html data-theme-pref>, which picks the toggle's glyph, and links
/assets/dark.css, the dark palette, always for dark and under the media
query prefers-color-scheme: dark for auto.  The static HTML files (the Org
blog) get the same through INJECT-THEME."))

(in-package #:project-isidore/site/layout)

(defun escape-html (string)
  "STRING with & < > \" ' written as character references, for text or a
quoted attribute value in HTML."
  (with-output-to-string (out)
    (loop for ch across string
          do (case ch
               (#\& (write-string "&amp;" out))
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\" (write-string "&quot;" out))
               (#\' (write-string "&#39;" out))
               (t (write-char ch out))))))

;;; Colour theme

(defparameter *themes* '("light" "dark" "auto")
  "The colour themes, in the order the toggle cycles through them; the first
is the default.")

(defparameter *theme-names*
  '(("light" . "parchment") ("dark" . "dark") ("auto" . "automatic (as the system)"))
  "What the toggle's label calls each theme.")

(defun theme-preference (env)
  "The colour theme the request ENV's theme cookie names, or the default."
  (let* ((headers (getf env :headers))
         (cookie (and headers (gethash "cookie" headers)))
         (value (and cookie
                     (nth-value 1 (cl-ppcre:scan-to-strings
                                   "(?:^|;)\\s*theme=([a-z]+)\\s*(?:;|$)" cookie)))))
    (or (and value (find (aref value 0) *themes* :test #'string=))
        (first *themes*))))

(defun next-theme (theme)
  "The theme after THEME in the toggle's cycle."
  (or (second (member theme *themes* :test #'string=))
      (first *themes*)))

(defun dark-stylesheet (theme)
  "The <link> to the dark palette that THEME needs, or the empty string."
  (cond ((string= theme "dark")
         "<link rel=\"stylesheet\" href=\"/assets/dark.css\">")
        ((string= theme "auto")
         "<link rel=\"stylesheet\" href=\"/assets/dark.css\" media=\"(prefers-color-scheme: dark)\">")
        (t "")))

(defun local-target (referer)
  "The path and query of the URL REFERER when it has a path on this site,
else \"/\": where /theme sends the reader back to."
  (or (ignore-errors
       (let* ((uri (quri:uri referer))
              (path (quri:uri-path uri)))
         (when (and path
                    (cl-ppcre:scan "^/(?!/)" path)
                    (not (find-if (lambda (ch) (or (char< ch #\!) (char= ch #\Rubout)))
                                  path)))
           (format nil "~A~@[?~A~]" path (quri:uri-query uri)))))
      "/"))

(defun theme-toggle-response (env)
  "The response to the toggle's POST /theme: set the theme after the one the
request names and send the reader back to the page they were on."
  (let ((headers (getf env :headers)))
    (list 303
          (list :location (local-target (and headers (gethash "referer" headers)))
                :set-cookie (format nil "theme=~A; Path=/; Max-Age=31536000; SameSite=Lax; HttpOnly"
                                    (next-theme (theme-preference env)))
                :cache-control "no-store"
                :content-length 0)
          '())))

(defun inject-theme (html theme)
  "HTML, a static page, with THEME stated on its <html> and the dark palette
linked at the end of its <head>, as PAGE does for the pages made here."
  ;; In the replacements \& is the text matched.
  (cl-ppcre:regex-replace
   "(?i)</head>"
   (cl-ppcre:regex-replace "(?i)<html(?=[\\s>])" html
                           (format nil "\\& data-theme-pref=\"~A\"" theme))
   (format nil "~A\\&" (dark-stylesheet theme))))

;;; The frame

(defun navbar (theme)
  "The site's navbar, with the theme toggle labelled for THEME.  The toggle
is a form, so that it works without a script."
  (let ((label (format nil "Theme: ~A. Switch to ~A."
                       (cdr (assoc theme *theme-names* :test #'string=))
                       (cdr (assoc (next-theme theme) *theme-names* :test #'string=)))))
    (format nil "<div class=\"header header-fixed\"><div class=\"navbar container\"><div class=\"logo\"><a href=\"/\">BHW</a></div><input type=\"checkbox\" id=\"navbar-toggle\"><label for=\"navbar-toggle\"><i></i></label><nav class=\"menu\"><ul><li><a href=\"/about\">About</a></li><li><a href=\"/work\">Work</a></li><li><a href=\"/assets/blog/archive.html\">Blog</a></li><li><a href=\"/contact\">Contact</a></li></ul></nav><form class=\"theme-form\" method=\"post\" action=\"/theme\"><button type=\"submit\" class=\"theme-toggle\" title=\"~A\" aria-label=\"~:*~A\"></button></form></div></div>"
            label)))

(defparameter *footer*
  "<footer><hr><div class=\"copyright-container\"><div class=\"copyright\">Copyright (c) 2021  Ben H. W.</div></div></footer>"
  "The site's footer.")

(defun page (env &key title head (chrome t) content)
  "A whole HTML document for the request ENV: TITLE, HEAD (more of <head>,
raw HTML) and CONTENT (raw HTML), inside the navbar, a div.main and the
footer unless CHROME is NIL, in the colour theme ENV asks for."
  (let ((theme (theme-preference env)))
    (with-output-to-string (out)
      (format out "<!doctype html>
<html lang=\"en\" data-theme-pref=\"~A\">
<head>
<meta charset=\"utf-8\">
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<title>~A</title>
<link rel=\"stylesheet\" href=\"/assets/global.css\">
~A~@[~A~]
</head>
<body>
"
              theme (escape-html title) (dark-stylesheet theme) head)
      (if chrome
          (format out "~A~%<div class=\"main\">~%~A~%</div>~%~A~%" (navbar theme) content *footer*)
          (format out "~A~%" content))
      (format out "</body>~%</html>~%"))))

(defun html-response (html &key (status 200))
  "A response of STATUS whose body is the document HTML.  It depends on the
theme cookie, so caches keep it per cookie and revalidate it."
  (let ((body (flexi-streams:string-to-octets html :external-format :utf-8)))
    (list status
          (list :content-type "text/html; charset=utf-8"
                :content-length (length body)
                :cache-control "no-cache"
                :vary "Cookie")
          body)))
