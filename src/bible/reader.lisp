;;;; SPDX-FileCopyrightText: 2021  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:bhw/bible/reader
  (:use #:common-lisp)
  (:import-from #:alexandria #:extremum #:iota)
  (:import-from #:cl-ppcre)
  (:import-from #:clog
                #:create-a #:create-br #:create-button #:create-div #:create-form
                #:create-form-element #:create-label #:create-option #:create-p
                #:create-section #:create-select #:create-table #:create-table-column
                #:create-table-row #:cursor #:inner-html #:location #:path-name
                #:requiredp #:set-on-change #:set-on-click #:set-on-new-window
                #:set-on-submit #:style #:url #:url-rewrite #:value #:window)
  (:import-from #:quri)
  (:import-from #:bhw/bible/database #:+bible-book-url-alist+)
  (:import-from #:bhw/site/layout #:escape-html #:with-page-chrome)
  (:local-nicknames (#:bible #:bhw/bible/text))
  (:export #:register-routes)
  (:documentation
   "The bible reader's pages: /bible/BOOK-CHAPTER-VERSE[/BOOK-CHAPTER-VERSE]
shows a passage with its commentary in a table, /bible-search searches it."))

(in-package #:bhw/bible/reader)

;;; Error Pages

(defparameter *title* "1883 Haydock Douay Rheims Bible"
  "The heading of every page of the reader.")

(defparameter *valid-url-help*
  "An example of a valid URL is <code>/bible/matthew-4-14/john-3-15</code> where the ending input is optional. Instead of <code>matthew</code> the number 47 may be used instead. This does not hold true for the chapter or verse which must be numeric. <a href=\"https://github.com/bhwdev/bhw-www/blob/master/src/bible/reader.lisp\">Consult the source repository on what constitutes a valid URL (N.B. the function PARSE-UID-SYM)</a> or <a href=\"/contact\">contact me for more information.</a>"
  "What a passage's URL looks like, for the error pages.")

(defun render-error-page (obj error-heading message)
  "Render an error page into OBJ: ERROR-HEADING, the paragraph MESSAGE (HTML)
and *VALID-URL-HELP*."
  (create-section obj :h1 :content *title*)
  (create-section obj :h4 :content error-heading)
  (create-p obj :content message)
  (create-p obj :content *valid-url-help*))

(defun render-400-page (obj)
  "Render 400 Bad Request content into OBJ."
  (render-error-page obj "Error Code 400 - Bad Request"
                     "Please validate the URL. Resource exists but invalid arguments passed."))

(defun render-negative-range-page (obj invalid-range)
  "Render 416 Range Not Specifiable content into OBJ.  INVALID-RANGE is the
beginning and ending references as the URL wrote them, so they are escaped."
  (let* ((bi (escape-html (first invalid-range)))
         (ei (escape-html (second invalid-range)))
         (corrected-link (format nil "/bible/~A/~A/" ei bi)))
    (render-error-page
     obj "Error Code 416 - Range Not Specifiable"
     (format nil "Please validate the URL. The beginning input <code>~A</code> subtracted from the ending input <code>~A</code> results in a negative interval range. Perhaps swapping the values will provide a valid URL: <a href=\"~A\">~A</a>"
             bi ei corrected-link corrected-link))))

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

(defun make-toggle (button content)
  "Wire a click toggle: clicking BUTTON toggles visibility of CONTENT.
BUTTON and CONTENT are CLOG objects."
  (let ((visible nil))
    (set-on-click button
                  (lambda (obj)
                    (declare (ignore obj))
                    (setf visible (not visible))
                    (setf (style content "display")
                          (if visible "block" "none"))))))

(defun wire-font-selector (select target)
  "Wire a <select> CLOG element to change font-family of TARGET."
  (set-on-change select
                 (lambda (obj)
                   (setf (style target "font-family")
                         (value obj)))))

(defun create-font-dropdown (parent)
  "Create a font dropdown selector. Returns the select element."
  (let ((container (create-div parent :class "font-dropdown-menu")))
    (let ((sel (create-select container :html-id "input-font"
                                        :class "input")))
      (create-option sel :content "Times New Roman"
                         :value "Times New Roman"
                         :selected t)
      (dolist (font-name '("Arial" "Courier New" "Garamond" "Verdana"))
        (create-option sel :content font-name :value font-name))
      sel)))

(defun create-link-menu (parent label links)
  "Create in PARENT a button labelled LABEL that shows and hides LINKS, an
alist of URL to link text."
  (let ((button (create-button parent :content label))
        (menu (create-div parent :style "overflow:auto;display:none;")))
    (make-toggle button menu)
    (loop for (link . link-text) in links
          do (create-a (create-div menu :style "width:200px;float:left")
                       :content link-text :link link))))

(defun create-search-form (parent on-query)
  "Create in PARENT the search form, which calls ON-QUERY with the query when
it is submitted.  Returns the query's input element."
  (let* ((form (create-form (create-div parent :style "text-align:center;")))
         (query-input (progn
                        (create-label form :content "Search: ")
                        (create-form-element form :text :name "query" :size 50))))
    (setf (requiredp query-input) t)
    (create-form-element form :submit :value "Submit")
    (set-on-submit form
                   (lambda (f)
                     (declare (ignore f))
                     (funcall on-query (value query-input))))
    query-input))

(defun search-url (query)
  "The URL of the search page showing the results of QUERY."
  (format nil "/bible-search?query=~A" (quri:url-encode query)))

(defun create-verse-row (table bible-uid &key score (footnotes-width "55%"))
  "Add to TABLE the row of the verse BIBLE-UID: its heading, its text with
its cross-references, and its footnotes in a cell FOOTNOTES-WIDTH wide, after
a cell for SCORE, a search result's relevance, when it is given."
  (let* ((data (bible:get-bible-verse-data bible-uid))
         (row (create-table-row table :style "line-height: 1.5em;"))
         (score-cell (and score (create-table-column row)))
         (heading-cell (create-table-column row))
         (text-cell (create-table-column row))
         (footnotes-cell (create-table-column
                          row :style (format nil "width:~A" footnotes-width))))
    (flet ((linked (text)
             (and text
                  (bible:encode-cross-references-to-html-links
                   text bible-uid (getf data :internal-chapter)))))
      (when score-cell
        (setf (inner-html score-cell) (write-to-string (floor score 0.01))))
      (setf (inner-html heading-cell) (getf data :heading)
            (inner-html text-cell) (format nil "~A~@[<br><br>~A~]"
                                           (getf data :text)
                                           (linked (getf data :cross-references))))
      (let ((footnotes (linked (getf data :footnotes))))
        (when footnotes
          (setf (inner-html footnotes-cell) footnotes))))))

(defun render-bible-content (obj uid-list)
  "Render Bible verse table into OBJ for the given UID-LIST."
  (let ((table (create-table obj :html-id "main-content")))
    (dolist (bible-uid uid-list)
      (create-verse-row table bible-uid))))

(defun on-bible-handler (body)
  "Bible page handler. Parses path, builds book/chapter toggles, search form,
font selector, and verse table via CLOG."
  (let ((path (path-name (location body))))
    (with-page-chrome body
      (lambda (main)
        (let ((result (if (string= path "/bible")
                          (iota 31 :start 8)
                          (resolve-bible-path path))))
          (cond
            ((eq result :bad-request)
             (render-400-page main))
            ((and (listp result) (eq (car result) :negative-range))
             (render-negative-range-page main (cadr result)))
            (t
             (let ((uid-list result))
               (create-section main :h1 :content *title*)
               (create-section main :h4
                               :content "Presents commentary in a tabular format for ease of reading. <a href=\"/assets/blog/tabular-douay-rheims.html\">Click to learn more.</a>")
               (create-link-menu main "Select Book" +bible-book-url-alist+)
               (create-br main)
               (create-link-menu main "Select Chapter"
                                 (bible:make-bible-chapter-url-list uid-list))
               (create-br main)
               (create-search-form main
                                   (lambda (query)
                                     (setf (url (location body)) (search-url query))))
               ;; Font selector + verse table
               (let ((font-sel (create-font-dropdown main))
                     (table-area (create-div main)))
                 (wire-font-selector font-sel table-area)
                 (render-bible-content table-area uid-list))))))))))

;;; Bible Search

(defun extract-query-param (url param)
  "Extract the value of PARAM from URL query string. Returns NIL if not found."
  (let ((query-start (position #\? url)))
    (when query-start
      (let* ((query-string (subseq url (1+ query-start)))
             (decoded (quri:url-decode query-string))
             (parts (cl-ppcre:split (format nil "~A=" param) decoded :limit 2)))
        (when (= 2 (length parts))
          ;; Strip any trailing &param=value pairs
          (let ((val (second parts)))
            (let ((amp (position #\& val)))
              (if amp (subseq val 0 amp) val))))))))

(defparameter *query-syntax-help*
  "<h2>1. FIELDS</h2><p>Each sentence of the Bible has the following metadata stored in fields,</p><li>(b) The book.</li><li>(c) The chapter number.</li><li>(v) The verse number.</li><li>(t) The text itself.</li><li>(f) If applicable, the footnote (Haydock) commentary.</li><li>(x) If applicable, the listed cross-references.</li><li>(implicit) All of the above fields.</li><h2>2. OPERATOR</h2><p>The fields are combined with Boolean OPERATOR(s),</p><li>(+) AND,</li><li>(implicit) OR</li><li>(!) NOT</li><p>Lastly, a particular predefined set of operations,</p><li>(*) TRUNCATE</li><p>The asterisk serves as the truncation (or wildcard) operator. Unlike the other operators, it should be appended to the word to be affected. Words match if they begin with the word preceding the * operator.</p><h2>3. FORM</h2><p>A query takes the form of,</p><p>[OPERATOR FIELD : EXPRESSION]</p><p>where EXPRESSION may be affixed with an asterisk character. Mixing of explicit and implicit forms as well as multiple forms are allowed. E.g.</p><li>charity</li><li>charity cymbal</li><li>loved much</li><li>mercy +b:matthew</li><li>broad path destruction narrow</li><li>principalities !c:2</li><li>compare 'exact*' with 'exacteth'</li><li>no greater love lay down</li><li>romans 3 23</li><li>+b:john +c:3 +v:15</li>"
  "The search page's account of the query syntax, as HTML.")

(defun render-search-results (obj query)
  "Render Bible search results for QUERY into OBJ. Clears OBJ first."
  (setf (inner-html obj) "")
  (let ((table (create-table obj :html-id "main-content")))
    (loop for (bible-uid . score) in (bible:search-bible query '(:num-docs 100))
          do (create-verse-row table bible-uid :score score :footnotes-width "50%"))))

(defun on-bible-search-handler (body)
  "Bible search page handler. Builds syntax help, search form with CLOG-native
submission and url-rewrite for shareable URLs."
  (let ((url (url (location body))))
    (with-page-chrome body
      (lambda (main)
        (create-section main :h1 :content *title*)
        ;; Query syntax toggle
        (let ((syntax-heading (create-section main :h2
                                              :content "Query Syntax (click to toggle)"))
              (syntax-div (create-div main :style "display:none;")))
          (setf (style syntax-heading "text-align") "center")
          (setf (cursor syntax-heading) :pointer)
          (make-toggle syntax-heading syntax-div)
          (setf (inner-html syntax-div) *query-syntax-help*))
        ;; Return to tabular view link
        (create-p main :content "<a href=\"/bible/1-1-1/1-1-31\" style=\"text-align:center;display:block;\">Return to tabular view.</a>")
        (let* ((font-sel (create-font-dropdown main))
               (results-area nil)
               (query-input
                 (create-search-form
                  main
                  (lambda (query)
                    (url-rewrite (window body) (search-url query))
                    (render-search-results results-area query))))
               (existing-query (extract-query-param url "query")))
          ;; Created after the form, so that the results come below it.
          (setf results-area (create-div main))
          (wire-font-selector font-sel results-area)
          ;; Auto-search if URL had query param
          (when existing-query
            (setf (value query-input) existing-query)
            (render-search-results results-area existing-query)))))))


(defun register-routes ()
  "Serve the reader's pages.  /bible takes the rest of the path as the
passage, which needs CLOG's :EXTENDED-ROUTING."
  (set-on-new-window 'on-bible-handler :path "/bible")
  (set-on-new-window 'on-bible-search-handler :path "/bible-search"))
