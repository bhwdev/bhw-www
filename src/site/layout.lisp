;;;; SPDX-FileCopyrightText: 2021  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/site/layout
  (:use #:common-lisp)
  (:import-from #:cl-ppcre)
  (:import-from #:clog
                #:attribute #:create-child #:create-div #:document-element
                #:head-element #:html-document #:property)
  (:import-from #:clog-web #:clog-web-initialize)
  (:import-from #:quri)
  (:export #:with-page-chrome
           #:escape-html
           #:theme-preference
           #:theme-toggle-response
           #:inject-theme)
  (:documentation
   "The frame every page of the site shares: stylesheet, colour theme, navbar
and footer, around content a page fills in.

The colour theme is a cookie: \"light\" (parchment, the default), \"dark\"
or \"auto\" (as the system prefers).  The navbar's toggle is a form that
posts to /theme, which sets the next one and sends the reader back
(THEME-TOGGLE-RESPONSE, served by PROJECT-ISIDORE/MAIN).  A page states the
choice as <html data-theme-pref>, which picks the toggle's glyph, and links
/assets/dark.css, the dark palette, always for dark and under the media
query prefers-color-scheme: dark for auto: a page CLOG builds reads the
cookie through CLOG (WITH-PAGE-CHROME), and a static HTML file (the Org
blog) gets the same from INJECT-THEME as it is sent.  None of it is a
script of the site's own."))

(in-package #:project-isidore/site/layout)

(defun escape-html (string)
  "STRING with & < > \" ' written as character references, for text or a
quoted attribute value in HTML.  CLOG's ESCAPE-FOR-HTML does only < and >."
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

(defun cookie-theme (cookie)
  "The colour theme COOKIE, the value of a Cookie header or of
document.cookie, names, or the default."
  (let ((value (and cookie
                    (nth-value 1 (cl-ppcre:scan-to-strings
                                  "(?:^|;)\\s*theme=([a-z]+)\\s*(?:;|$)" cookie)))))
    (or (and value (find (aref value 0) *themes* :test #'string=))
        (first *themes*))))

(defun theme-preference (env)
  "The colour theme the request ENV's theme cookie names, or the default."
  (let ((headers (getf env :headers)))
    (cookie-theme (and headers (gethash "cookie" headers)))))

(defun next-theme (theme)
  "The theme after THEME in the toggle's cycle."
  (or (second (member theme *themes* :test #'string=))
      (first *themes*)))

(defun dark-stylesheet (theme)
  "The <link> to the dark palette that THEME needs, or NIL."
  (cond ((string= theme "dark")
         "<link rel=\"stylesheet\" href=\"/assets/dark.css\">")
        ((string= theme "auto")
         "<link rel=\"stylesheet\" href=\"/assets/dark.css\" media=\"(prefers-color-scheme: dark)\">")))

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
request names and send the reader back to the page they were on.  Not
HttpOnly: WITH-PAGE-CHROME reads it as document.cookie."
  (let ((headers (getf env :headers)))
    (list 303
          (list :location (local-target (and headers (gethash "referer" headers)))
                :set-cookie (format nil "theme=~A; Path=/; Max-Age=31536000; SameSite=Lax"
                                    (next-theme (theme-preference env)))
                :cache-control "no-store"
                :content-length 0)
          '())))

(defun inject-theme (html theme)
  "HTML, a static page, with THEME stated on its <html> and the dark palette
linked at the end of its <head>, as WITH-PAGE-CHROME does for the pages
CLOG builds."
  ;; In the replacements \& is the text matched.
  (cl-ppcre:regex-replace
   "(?i)</head>"
   (cl-ppcre:regex-replace "(?i)<html(?=[\\s>])" html
                           (format nil "\\& data-theme-pref=\"~A\"" theme))
   (format nil "~@[~A~]\\&" (dark-stylesheet theme))))

;;; The frame

(defun create-navbar (parent theme)
  "Create the site navbar matching the blog archive style, with the theme
toggle labelled for THEME.  Styled via global.css.  The toggle is a form,
so that it needs no script."
  (let ((label (format nil "Theme: ~A. Switch to ~A."
                       (cdr (assoc theme *theme-names* :test #'string=))
                       (cdr (assoc (next-theme theme) *theme-names* :test #'string=)))))
    (create-child parent
                  (format nil "<div class='header header-fixed'><div class='navbar container'><div class='logo'><a href='/'>BHW</a></div><input type='checkbox' id='navbar-toggle'><label for='navbar-toggle'><i></i></label><nav class='menu'><ul><li><a href='/about'>About</a></li><li><a href='/work'>Work</a></li><li><a href='/assets/blog/archive.html'>Blog</a></li><li><a href='/contact'>Contact</a></li></ul></nav><form class='theme-form' method='post' action='/theme'><button type='submit' class='theme-toggle' title='~A' aria-label='~:*~A'></button></form></div></div>"
                          (escape-html label)))))

(defun create-footer (parent)
  "Create the site footer."
  (create-child parent
                "<footer><hr><div class='copyright-container'><div class='copyright'>Copyright (c) 2021  Ben H. W.</div></div></footer>"))

(defun with-page-chrome (body content-fn)
  "Set up a page with navbar, main content area, and footer, in the colour
theme the reader's cookie names.  CONTENT-FN is called with the main content
div as argument."
  (clog-web-initialize body)
  (let* ((document (html-document body))
         (theme (cookie-theme (property document "cookie")))
         (dark (dark-stylesheet theme)))
    (create-child (head-element document)
                  "<link rel='stylesheet' href='/assets/global.css' type='text/css'>")
    (when dark
      (create-child (head-element document) dark))
    (setf (attribute (document-element document) "data-theme-pref") theme)
    (create-navbar body theme)
    (let ((main (create-div body :class "main")))
      (funcall content-fn main))
    (create-footer body)))
