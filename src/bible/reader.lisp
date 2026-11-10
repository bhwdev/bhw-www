;;;; SPDX-FileCopyrightText: 2021  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/bible/reader
  (:use #:common-lisp)
  (:import-from #:alexandria #:extremum #:iota)
  (:import-from #:cl-ppcre)
  (:import-from #:quri)
  (:import-from #:project-isidore/bible/database #:*database* #:+bible-book-url-alist+)
  (:import-from #:project-isidore/site/layout #:escape-html #:page #:html-response)
  (:local-nicknames (#:bible #:project-isidore/bible/text))
  (:export #:handle-request)
  (:documentation
   "The bible reader's pages: /bible/BOOK-CHAPTER-VERSE[/BOOK-CHAPTER-VERSE]
shows a passage with its commentary in a table, /bible-search?query=QUERY
searches it.  They are made whole on the server: the book and chapter
pickers are <details>, the search a plain GET form, and the font menu
global.css's :has() rules, so none of them needs a script."))

(in-package #:project-isidore/bible/reader)

;;; Error Pages

(defparameter *valid-url-help*
  "<p>An example of a valid URL is <code>/bible/matthew-4-14/john-3-15</code> where the ending input is optional. Instead of <code>matthew</code> the number 47 may be used instead. This does not hold true for the chapter or verse which must be numeric. <a href=\"https://github.com/bhwdev/project-isidore/blob/master/src/bible/reader.lisp\">Consult the source repository on what constitutes a valid URL (N.B. the function PARSE-UID-SYM)</a> or <a href=\"/contact\">contact me for more information.</a></p>"
  "The error pages' explanation of the reader's URLs.")

(defun bad-request-page ()
  "The content of the 400 Bad Request page."
  (format nil "<h1>1883 Haydock Douay Rheims Bible</h1>
<h4>Error Code 400 - Bad Request</h4>
<p>Please validate the URL. Resource exists but invalid arguments passed.</p>
~A" *valid-url-help*))

(defun negative-range-page (invalid-range)
  "The content of the page for a reversed range.  INVALID-RANGE is the
beginning and ending references as the URL wrote them, so they are escaped."
  (let* ((bi (escape-html (first invalid-range)))
         (ei (escape-html (second invalid-range)))
         (corrected-link (format nil "/bible/~A/~A/" ei bi)))
    (format nil "<h1>1883 Haydock Douay Rheims Bible</h1>
<h4>Error Code 416 - Range Not Specifiable</h4>
<p>Please validate the URL. The beginning input <code>~A</code> subtracted from the ending input <code>~A</code> results in a negative interval range. Perhaps swapping the values will provide a valid URL: <a href=\"~A\">~A</a></p>
~A"
            bi ei corrected-link corrected-link *valid-url-help*)))

;;; Bible Route Handling

(defun parse-verse-arg (str)
  "Parse a verse argument as either a numeric UID or a symbolic reference.
Pure integers are returned directly; otherwise `parse-uid-sym' is used, on
the string: interning text from the URL would add a symbol to the package
for every distinct path requested, which is never freed."
  (let ((n (parse-integer str :junk-allowed t)))
    (if (and n (string= (write-to-string n) str))
        n
        (parse-uid-sym (string-upcase str)))))

(defun parse-uid-sym (uid-sym)
  "Human readable call to `view:bible-page'. UID-SYM must be of the following format,
BOOK-delimiter-CHAPTER-delimiter-VERSE, where delimiters are the SAME single
NON-alphanumeric unreserved character. CHAPTER and VERSE must be numeric,
whereas BOOK must be alphanumeric. Unreserved characters are defined as
uppercase and lowercase letters, decimal digits, hyphen, period, underscore,
and tilde. See RFC3986 2.3. Unreserved Characters
https://www.ietf.org/rfc/rfc3986.txt

Ex. /bible/genesis-2-10,/bible/47~4~2 etc.

> (parse-uid-sym 'genesis-2-10)
51
> (parse-uid-sym '1.2.10)
51
> (parse-uid-sym '1.2-10)
ARG-COUNT-ERROR
"
  ;; Split on each delimiter and keep the split with the most parts.
  (let ((list (extremum (mapcar (lambda (delimiter)
                                  (cl-ppcre:split delimiter (string uid-sym)))
                                '("-" "\\." "_" "~"))
                        #'> :key #'length)))
    (if (= 3 (length list)) ; Assume a sublist of 3 is properly parsed.
        (destructuring-bind (book chapter verse)
            list
          (bible:get-bible-uid
           (if (parse-integer book :junk-allowed t)
               ;; It has a number, but is it a prefix or by its lonesome?
               (if (cl-ppcre:scan "[0-9]{1}[a-zA-Z]" book)
                   (bible:bible-book-convert-dwim
                    (concatenate 'string
                                 (subseq book 0 1) ; IV Kings will not work.
                                 " "
                                 (cadr (cl-ppcre:split "[0-9]" book))))
                   (parse-integer book))
               (bible:bible-book-convert-dwim book))
           (parse-integer chapter)
           (parse-integer verse)))
        (error "Invalid verse reference: ~A" uid-sym))))

(defun resolve-bible-path (path)
  "Parse /bible/X and /bible/X/Y from PATH and return a uid-list, or a keyword
indicating an error condition:
  :bad-request - invalid arguments
  (:negative-range (begin-str end-str)) - reversed range"
  (handler-case
      (let* ((remainder (subseq path (length "/bible/")))
             (parts (cl-ppcre:split "/" remainder))
             (beginning-str (first parts))
             (ending-str (or (second parts) beginning-str)))
        (let ((beginning-uid (parse-verse-arg beginning-str))
              (ending-uid (parse-verse-arg ending-str)))
          (cond
            ((and (<= 0 beginning-uid)
                  (>= 7 beginning-uid))
             :bad-request)
            ((> beginning-uid ending-uid)
             (list :negative-range (list beginning-str ending-str)))
            ((or (not (bible:valid-bible-uid-p beginning-uid))
                 (not (bible:valid-bible-uid-p ending-uid)))
             :bad-request)
            (t (iota (1+ (- ending-uid beginning-uid))
                     :start beginning-uid)))))
    (error () :bad-request)))

;;; Bible Content Rendering

(defparameter *fonts* '("Times New Roman" "Arial" "Courier New" "Garamond" "Verdana")
  "The fonts the reader's menu offers besides the page's own.  global.css
has a :has() rule for each, which applies it to the .verses beside the menu.")

(defun font-menu ()
  "The font menu, which restyles the .verses in the same .reader."
  (format nil "<div class=\"font-dropdown-menu\"><label>Font: <select id=\"input-font\" class=\"input\"><option value=\"\" selected>Default</option>~{<option value=\"~A\">~:*~A</option>~}</select></label></div>"
          *fonts*))

(defun link-list (summary links)
  "A <details> whose SUMMARY opens the list of LINKS, (url . text) pairs."
  (format nil "<details class=\"toggle\"><summary class=\"button\">~A</summary><div style=\"overflow:auto;\">~{~A~}</div></details>"
          summary
          (loop for (url . text) in links
                collect (format nil "<div style=\"width:200px;float:left\"><a href=\"~A\">~A</a></div>"
                                (escape-html url) (escape-html text)))))

(defun search-form (&optional (query ""))
  "The search form, a GET of /bible-search, filled with QUERY."
  (format nil "<form class=\"bible-search\" action=\"/bible-search\" method=\"get\" style=\"text-align:center;\"><label>Search: <input type=\"text\" name=\"query\" size=\"50\" required value=\"~A\"></label> <input type=\"submit\" value=\"Submit\"></form>"
          (escape-html query)))

(defun verse-row (bible-uid &key score)
  "The table row of the verse BIBLE-UID: its relevance SCORE when given,
heading, text with its cross-references, and footnotes.  The text and
commentary are the database's own HTML."
  (let* ((data (bible:get-bible-verse-data bible-uid))
         (int-ch (getf data :internal-chapter))
         (xref-text (getf data :cross-references))
         (fn-text (getf data :footnotes)))
    (format nil "<tr style=\"line-height: 1.5em;\">~@[<td>~D</td>~]<td>~A</td><td>~A~@[<br><br>~A~]</td><td style=\"width:~D%\">~@[~A~]</td></tr>"
            (and score (floor score 0.01))
            (escape-html (getf data :heading))
            (getf data :text)
            (and xref-text
                 (bible:encode-cross-references-to-html-links xref-text bible-uid int-ch))
            (if score 50 55)
            (and fn-text
                 (bible:encode-cross-references-to-html-links fn-text bible-uid int-ch)))))

(defun passage-page (uid-list)
  "The content of the page of the verses UID-LIST."
  (format nil "<h1>1883 Haydock Douay Rheims Bible</h1>
<h4>Presents commentary in a tabular format for ease of reading. <a href=\"/assets/blog/tabular-douay-rheims.html\">Click to learn more.</a></h4>
~A
<br>
~A
<br>
~A
<div class=\"reader\">
~A
<div class=\"verses\"><table id=\"main-content\">~{~A~}</table></div>
</div>"
          (link-list "Select Book" +bible-book-url-alist+)
          (link-list "Select Chapter" (bible:make-bible-chapter-url-list uid-list))
          (search-form)
          (font-menu)
          (mapcar #'verse-row uid-list)))

(defun bible-response (env)
  "The response to a GET of /bible or /bible/...: the passage the path
names, the first chapter of Genesis for /bible itself."
  (let* ((path (getf env :path-info))
         (result (if (string= (string-right-trim "/" path) "/bible")
                     (iota 31 :start 8)
                     (resolve-bible-path path))))
    (flet ((respond (content &optional (status 200))
             (html-response (page env :title "1883 Haydock Douay Rheims Bible" :content content)
                            :status status)))
      (cond ((eq result :bad-request)
             (respond (bad-request-page) 400))
            ((and (listp result) (eq (car result) :negative-range))
             (respond (negative-range-page (cadr result)) 400))
            (t
             (respond (passage-page result)))))))

;;; Bible Search

(defparameter *query-syntax*
  "<h2>1. FIELDS</h2><p>Each sentence of the Bible has the following metadata stored in fields,</p><li>(b) The book.</li><li>(c) The chapter number.</li><li>(v) The verse number.</li><li>(t) The text itself.</li><li>(f) If applicable, the footnote (Haydock) commentary.</li><li>(x) If applicable, the listed cross-references.</li><li>(implicit) All of the above fields.</li><h2>2. OPERATOR</h2><p>The fields are combined with Boolean OPERATOR(s),</p><li>(+) AND,</li><li>(implicit) OR</li><li>(!) NOT</li><p>Lastly, a particular predefined set of operations,</p><li>(*) TRUNCATE</li><p>The asterisk serves as the truncation (or wildcard) operator. Unlike the other operators, it should be appended to the word to be affected. Words match if they begin with the word preceding the * operator.</p><h2>3. FORM</h2><p>A query takes the form of,</p><p>[OPERATOR FIELD : EXPRESSION]</p><p>where EXPRESSION may be affixed with an asterisk character. Mixing of explicit and implicit forms as well as multiple forms are allowed. E.g.</p><li>charity</li><li>charity cymbal</li><li>loved much</li><li>mercy +b:matthew</li><li>broad path destruction narrow</li><li>principalities !c:2</li><li>compare 'exact*' with 'exacteth'</li><li>no greater love lay down</li><li>romans 3 23</li><li>+b:john +c:3 +v:15</li>"
  "The search's help, from the syntax the query parser in
PROJECT-ISIDORE/BIBLE/TEXT takes.")

(defun query-parameter (env)
  "The query parameter of the request ENV's query string, or NIL."
  (let ((query-string (getf env :query-string)))
    (when query-string
      (ignore-errors
       (cdr (assoc "query" (quri:url-decode-params query-string :lenient t)
                   :test #'string=))))))

(defun search-response (env)
  "The response to a GET of /bible-search: the search form, and when the
query string has a query, the verses it finds."
  (let ((query (query-parameter env)))
    (html-response
     (page env
           :title (if query
                      (format nil "~A - Bible search" query)
                      "Bible search")
           :content (format nil "<h1>1883 Haydock Douay Rheims Bible</h1>
<details class=\"syntax\"><summary><h2>Query Syntax (click to toggle)</h2></summary><div>~A</div></details>
<p><a href=\"/bible/1-1-1/1-1-31\" style=\"text-align:center;display:block;\">Return to tabular view.</a></p>
<div class=\"reader\">
~A
~A
<div class=\"verses\"><table id=\"main-content\">~{~A~}</table></div>
</div>"
                            *query-syntax*
                            (font-menu)
                            (search-form (or query ""))
                            (when query
                              (loop for (bible-uid . score)
                                      in (bible:search-bible query '(:num-docs 100))
                                    collect (verse-row bible-uid :score score))))))))

(defun unavailable-response (env)
  "The 503 response for a reader page on a machine without the bible."
  (html-response (page env :title "1883 Haydock Douay Rheims Bible"
                           :content "<h1>1883 Haydock Douay Rheims Bible</h1>
<p>The bible is not available on this server at the moment.</p>")
                 :status 503))

(defun handle-request (env)
  "The response to the request ENV when it is a GET (or HEAD) of the
reader's pages: /bible, /bible/... or /bible-search.  Otherwise NIL."
  (let ((path (getf env :path-info)))
    (when (member (getf env :request-method) '(:get :head))
      (let ((handler (cond ((or (string= path "/bible")
                                (uiop:string-prefix-p "/bible/" path))
                            #'bible-response)
                           ((string= path "/bible-search")
                            #'search-response))))
        (when handler
          (if *database*
              (funcall handler env)
              (unavailable-response env)))))))
