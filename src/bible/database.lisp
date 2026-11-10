;;;; SPDX-FileCopyrightText: 2021  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/bible/database
  (:use #:common-lisp)
  (:import-from #:alexandria #:ensure-gethash)
  (:import-from #:cl-ppcre)
  (:local-nicknames (#:store #:project-isidore/store))
  (:export #:*database*
           #:*database-path*
           #:open-database
           #:close-database
           #:read-export
           #:write-database

           #:verse
           #:unique-id-of #:heading-of #:text-of #:cross-references-of #:footnotes-of
           #:bible-obj-with-id
           #:search-verses

           #:+bible-book-numbers-alist+
           #:+bible-chapter-url-alist+
           #:+bible-book-url-alist+
           #:+bible-heading-to-uid+
           #:+bible-uid-to-heading+)
  (:documentation
   "The bible's SQLite database: the 1883 Haydock Douay-Rheims text, one row
per verse, with a full-text index over it, read-only at runtime.

The database is the file *DATABASE-PATH*, the same path in development and
production; a machine without it (the guix.scm build sandbox, a fresh
checkout) simply has no bible, and OPEN-DATABASE returns NIL there.
WRITE-DATABASE makes one from the plain data READ-EXPORT reads."))

(in-package #:project-isidore/bible/database)

(defvar *database-path* #p"/var/local/project-isidore/bible.sqlite3"
  "The file of the bible's database.")

(defvar *database* nil
  "The open bible store, or NIL before OPEN-DATABASE or without a database.")

;;; Schema.  A verse's heading is its book, chapter and verse numbers; the
;;; eight records before Genesis have no book, and a chapter or verse 0 is
;;; the introduction to a book or a chapter.

(defparameter *search-fields* '("b" "c" "v" "t" "f" "x")
  "The fields a search can name: b, the book's names; c and v, the chapter
and verse numbers; t, the text; f, the footnotes; x, the cross-references.
Each is indexed, by the verse's uid, in an FTS5 table of its own, SEARCH_b
and so on, so that BM25 weighs a word against the length of the field it is
in, not of the whole verse with its commentary.  The tables are contentless:
the text is in VERSE already.")

(defparameter *schema*
  (append
   '("CREATE TABLE IF NOT EXISTS verse (
        uid INTEGER PRIMARY KEY,
        book INTEGER,
        chapter INTEGER NOT NULL,
        verse INTEGER NOT NULL,
        text TEXT NOT NULL,
        cross_references TEXT,
        footnotes TEXT)"
     "CREATE TABLE IF NOT EXISTS book_name (
        position INTEGER PRIMARY KEY,
        name TEXT NOT NULL,
        book INTEGER NOT NULL)"
     "CREATE TABLE IF NOT EXISTS chapter_link (
        position INTEGER PRIMARY KEY,
        url TEXT NOT NULL,
        label TEXT NOT NULL)"
     "CREATE TABLE IF NOT EXISTS book_link (
        position INTEGER PRIMARY KEY,
        url TEXT NOT NULL,
        label TEXT NOT NULL)")
   (mapcar (lambda (field)
             (format nil "CREATE VIRTUAL TABLE IF NOT EXISTS search_~A
                            USING fts5(body, content = '')"
                     field))
           *search-fields*))
  "The statements WRITE-DATABASE runs to make an empty database.")

;;; Lookup tables, filled from the database by OPEN-DATABASE.

(defparameter +bible-book-numbers-alist+ nil
  "Alist of book name to book number, aliases included.")

(defparameter +bible-chapter-url-alist+ nil
  "Alist of chapter URL to \"Book N\" link text, in bible order.")

(defparameter +bible-book-url-alist+ nil
  "Alist of book URL to book name link text, in bible order.")

(defparameter +bible-heading-to-uid+ (make-hash-table :test 'equalp)
  "Maps (book chapter verse) lists to bible unique IDs.")

(defparameter +bible-uid-to-heading+ (make-hash-table :test 'eql)
  "Maps bible unique IDs to heading alists.")

(defun make-heading (book chapter verse)
  "The heading alist of the verse BOOK CHAPTER:VERSE."
  (list (cons :book book) (cons :chapter chapter) (cons :verse verse)))

(defun load-tables ()
  "Fill the lookup tables from *DATABASE*."
  (let ((heading-to-uid (make-hash-table :test 'equalp :size 40000))
        (uid-to-heading (make-hash-table :test 'eql :size 40000)))
    (loop for (uid book chapter verse)
            in (store:query *database* "SELECT uid, book, chapter, verse FROM verse ORDER BY uid")
          do (setf (gethash (list book chapter verse) heading-to-uid) uid
                   (gethash uid uid-to-heading) (make-heading book chapter verse)))
    (flet ((alist (sql)
             (mapcar (lambda (row) (cons (first row) (second row)))
                     (store:query *database* sql))))
      (setf +bible-book-numbers-alist+ (alist "SELECT name, book FROM book_name ORDER BY position")
            +bible-chapter-url-alist+ (alist "SELECT url, label FROM chapter_link ORDER BY position")
            +bible-book-url-alist+ (alist "SELECT url, label FROM book_link ORDER BY position")
            +bible-heading-to-uid+ heading-to-uid
            +bible-uid-to-heading+ uid-to-heading))))

(defun open-database ()
  "Open the bible's database and fill the lookup tables from it.  Returns
the store, or NIL when there is no database at *DATABASE-PATH*."
  (close-database)
  (setf *database* (store:open-store *database-path* :read-only t))
  (when *database*
    (load-tables)
    *database*))

(defun close-database ()
  "Close the bible's database if it is open."
  (setf *database* (store:close-store *database*)))

;;; Verses

(defstruct (verse (:constructor make-verse
                      (unique-id book chapter verse-number text cross-references footnotes)))
  "One verse of the bible, as its row in the database."
  (unique-id nil :type fixnum :read-only t)
  (book nil :type (or fixnum null) :read-only t)
  (chapter nil :type fixnum :read-only t)
  (verse-number nil :type fixnum :read-only t)
  (text nil :type string :read-only t)
  (cross-references nil :type (or string null) :read-only t)
  (footnotes nil :type (or string null) :read-only t))

(defun unique-id-of (verse) "The unique id of VERSE." (verse-unique-id verse))
(defun text-of (verse) "The text of VERSE." (verse-text verse))
(defun cross-references-of (verse) "The cross-references of VERSE, or NIL." (verse-cross-references verse))
(defun footnotes-of (verse) "The footnotes of VERSE, or NIL." (verse-footnotes verse))

(defun heading-of (verse)
  "The heading of VERSE: ((:BOOK . book) (:CHAPTER . chapter) (:VERSE . verse))."
  (make-heading (verse-book verse) (verse-chapter verse) (verse-verse-number verse)))

(defun bible-obj-with-id (bible-uid)
  "Return the VERSE whose unique id is BIBLE-UID, or NIL."
  (let ((row (store:query-row *database*
                              "SELECT uid, book, chapter, verse, text, cross_references, footnotes
                               FROM verse WHERE uid = ?"
                              bible-uid)))
    (when row
      (apply #'make-verse row))))

(defun index-form (string)
  "STRING as the search tables hold it: each chapter:verse reference made
one word (3:23 as 3v23), as Montezuma's tokenizer kept it, so that a
verse's own chapter and verse outweigh the references to them in other
verses' commentary.  Queries go through it too."
  (cl-ppcre:regex-replace-all "(\\d):(\\d)" string "\\1v\\2"))

(defun search-verses (clauses limit)
  "The uids of at most LIMIT verses that CLAUSES find, best first, each
consed to its relevance, larger being better.

CLAUSES is a list of (OCCURRENCE FIELD MATCH): MATCH is an FTS5 query for a
single column, run against the table of FIELD, one of *SEARCH-FIELDS*, or
of every field when FIELD is NIL.  A verse is found when it matches every
:MUST clause, or any :SHOULD clause when there is no :MUST, and no :NOT
clause.  Its relevance is the sum of the BM25 scores of the :MUST and
:SHOULD clauses it matches, in every field it matches them in, times the
fraction of those clauses it matches (Lucene's coord factor, which
Montezuma applied): a verse that has every word of a query outranks one
that has some of them often."
  (let ((selects '()) (parameters '()) (must '()) (should '()) (not '()))
    (loop for (occurrence field match) in clauses
          for clause from 0
          do (ecase occurrence
               (:must (push clause must))
               (:should (push clause should))
               (:not (push clause not)))
             (dolist (field (if field (list field) *search-fields*))
               ;; FIELD goes into the SQL, so it must be one of ours.
               (assert (member field *search-fields* :test #'string=))
               (push (format nil "SELECT ~D, rowid, -bm25(search_~A)
                                  FROM search_~:*~A WHERE search_~:*~A MATCH ?"
                             clause field)
                     selects)
               (push (index-form match) parameters)))
    (when (or must should)
      (mapcar (lambda (row) (cons (first row) (second row)))
              (apply #'store:query *database*
                     ;; MATERIALIZED, or SQLite inlines a single SELECT and
                     ;; then refuses BM25 inside the aggregate.
                     (format nil "WITH hits (clause, uid, score) AS MATERIALIZED
                                    (~{~A~^ UNION ALL ~})
                                  SELECT uid,
                                    SUM(CASE WHEN clause IN (~{~D~^, ~}) THEN score ELSE 0 END)
                                    * COUNT(DISTINCT CASE WHEN clause IN (~:*~{~D~^, ~}) THEN clause END)
                                    / ~D.0 AS score
                                  FROM hits GROUP BY uid
                                  HAVING ~{MAX(clause = ~D) = 1 AND ~}~
                                         ~@[MAX(clause IN (~{~D~^, ~})) = 1 AND ~]~
                                         ~{MAX(clause = ~D) = 0 AND ~}1
                                  ORDER BY score DESC, uid LIMIT ?"
                             (reverse selects) (append must should)
                             (+ (length must) (length should))
                             must (and (null must) should) not)
                     (append (reverse parameters) (list limit)))))))

;;; Making the database

(defun read-export (pathname)
  "Read the plist of bible data at PATHNAME that WRITE-DATABASE takes:
:BOOK-NUMBERS, :CHAPTER-URLS and :BOOK-URLS, the lookup tables' alists, and
:VERSES, a list of (uid book chapter verse text cross-references footnotes)."
  (with-open-file (in pathname :external-format :utf-8)
    (with-standard-io-syntax
      (let ((*read-eval* nil)
            (*package* (find-package :keyword)))
        (read in)))))

(defun book-search-names (book book-numbers)
  "The words of every name BOOK-NUMBERS gives the book numbered BOOK, but
for ordinals (1, I, ...), as one string: what the search's b field holds."
  (let ((words '()))
    (loop for (name . number) in book-numbers
          when (eql number book)
            do (dolist (word (cl-ppcre:split "\\s+" name))
                 (unless (cl-ppcre:scan "^(?i:[0-9]+|i{1,3}|iv)$" word)
                   (pushnew word words :test #'string-equal))))
    (format nil "~{~A~^ ~}" (reverse words))))

(defun write-database (data pathname)
  "Make the bible's database at PATHNAME, which must not exist yet, from
DATA, a plist as READ-EXPORT returns.  Returns PATHNAME."
  (when (probe-file pathname)
    (error "~A exists already." pathname))
  (destructuring-bind (&key book-numbers chapter-urls book-urls verses) data
    (let ((db (store:open-store pathname :schema *schema*)))
      (unwind-protect
           (progn
             (store:with-transaction (db :context "write-database")
               (flet ((insert-alist (table columns alist)
                        (loop for (key . value) in alist
                              for position from 0
                              do (store:execute db (format nil "INSERT INTO ~A (position, ~A) VALUES (?, ?, ?)"
                                                           table columns)
                                                position key value))))
                 (insert-alist "book_name" "name, book" book-numbers)
                 (insert-alist "chapter_link" "url, label" chapter-urls)
                 (insert-alist "book_link" "url, label" book-urls))
               (let ((names (make-hash-table)))
                 (loop for (uid book chapter verse text cross-references footnotes) in verses
                       do (store:execute db "INSERT INTO verse VALUES (?, ?, ?, ?, ?, ?, ?)"
                                         uid book chapter verse text cross-references footnotes)
                          ;; A verse without a field is left out of its
                          ;; table, so as not to lower the field's average
                          ;; length, against which BM25 weighs.
                          (loop for field in *search-fields*
                                for body in (list (and book
                                                       (ensure-gethash
                                                        book names
                                                        (book-search-names book book-numbers)))
                                                  (princ-to-string chapter)
                                                  (princ-to-string verse)
                                                  (index-form text)
                                                  (and footnotes (index-form footnotes))
                                                  (and cross-references
                                                       (index-form cross-references)))
                                when (and body (string/= body ""))
                                  do (store:execute db (format nil "INSERT INTO search_~A (rowid, body)
                                                                    VALUES (?, ?)"
                                                               field)
                                                    uid body))))
               (dolist (field *search-fields*)
                 (store:execute db (format nil "INSERT INTO search_~A (search_~:*~A)
                                                VALUES ('optimize')"
                                           field))))
             ;; Leave one self-contained file, which the application opens
             ;; read-only.
             (store:query-value db "PRAGMA journal_mode = DELETE"))
        (store:close-store db))))
  pathname)
