;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:bhw/plp/song
  (:use #:common-lisp)
  (:import-from #:cl-ppcre)
  (:export #:song #:make-song #:copy-song
           #:song-id #:song-title #:song-alt-titles #:song-writers
           #:song-copyright #:song-ccli #:song-number #:song-page
           #:song-sections #:song-order #:song-tags #:song-reviewed
           #:section-kind #:section-number #:section-label #:section-slides
           #:section-key #:section-name
           #:parse-song #:print-song #:song-problems
           #:song-first-line #:slug
           #:*max-lines* #:split-lines
           #:song-deck #:slide-key #:slide-label #:slide-lines #:slide-part
           #:slide-of #:slide-group
           #:deck-jump)
  (:documentation
   "A song as data, with no store and no page.

A SONG holds its credits and its SECTIONS in the order written.  A section is
a plist: :KIND (:VERSE :REFRAIN :BRIDGE :PRE-CHORUS :TAG :ENDING :OTHER),
:NUMBER, :LABEL (what the source called it) and :SLIDES, a list of slides,
each a list of lines.  A section's key is what the operator presses and what
an Order: header names: 1 2 3 for verses, R B P T O X for the other kinds,
with the number after it from the second on (R2).

PARSE-SONG and PRINT-SONG read and write the text format, the one the chord
book's import stages its songs in and the one a custom song is pasted in:

  Title: Abide With Me
  Writers: Henry Francis Lyte
  Order: 1 R 2 R

  1.
  Abide with me, fast falls the eventide

  Refrain:
  ...

Headers end at the first blank line.  A label stands alone on its line; a
blank line inside a labelled section breaks the slide; text under no label
is verses, one per block.

SONG-DECK lays the song out as the slides to show, in its order, and
DECK-JUMP finds where a key press leads."))

(in-package #:bhw/plp/song)

(defstruct song
  "A song.  TAGS is a list of (category name note), category a keyword and
note a string or NIL; ORDER a list of section keys or NIL for as written."
  id title alt-titles writers copyright ccli number page
  sections order tags reviewed)

(defun section-kind (section) (getf section :kind))
(defun section-number (section) (getf section :number))
(defun section-label (section) (getf section :label))
(defun section-slides (section) (getf section :slides))

(defparameter *kinds*
  '((:refrain "R" "Refrain" "refrain" "chorus" "ref" "refr")
    (:bridge "B" "Bridge" "bridge")
    (:pre-chorus "P" "Pre-Chorus" "pre-chorus" "prechorus" "pre chorus")
    (:tag "T" "Tag" "tag" "build")
    (:ending "O" "Outro" "outro" "coda" "ending" "end")
    (:verse nil "Verse" "verse")
    (:wordless nil nil "intro" "instrumental" "instr" "instr." "interlude")
    (:other "X" nil))
  "Each kind of section: its key letter, its default label and the words a
label line may use for it.")

(defun kind-letter (kind) (second (assoc kind *kinds*)))

(defun label-kind (word)
  "The kind that the label word WORD names, or :OTHER."
  (or (first (find-if (lambda (entry)
                        (member word (cdddr entry) :test #'string-equal))
                      *kinds*))
      :other))

(defun section-key (section)
  "The key of SECTION: \"2\" for the second verse, \"R\", \"R2\", \"B\"."
  (let ((number (or (section-number section) 1)))
    (if (eq (section-kind section) :verse)
        (princ-to-string number)
        (format nil "~A~:[~;~D~]" (kind-letter (section-kind section))
                (> number 1) number))))

(defun section-name (section)
  "What to call SECTION on a preview: \"Verse 2\", \"Refrain\", \"Bridge 2\"."
  (let ((number (or (section-number section) 1)))
    (format nil "~A~:[~; ~D~]" (section-label section)
            (or (eq (section-kind section) :verse) (> number 1))
            number)))

;;; Reading

(defun trim (string)
  (string-trim '(#\Space #\Tab #\Return #\Newline) string))

(defun split-list (string separator)
  "The trimmed, non-empty parts of STRING between occurrences of SEPARATOR."
  (remove "" (mapcar #'trim (cl-ppcre:split separator string)) :test #'string=))

(defun parse-tag (text)
  "The tag (category name note) that TEXT, as \"mass=Communion\" or
\"liturgy=Christmas {Dec 25}\", writes, or NIL."
  (cl-ppcre:register-groups-bind (category name note)
      ("^\\s*([A-Za-z-]+)\\s*=\\s*(.*?)\\s*(?:\\{(.*)\\})?\\s*$" text)
    (when (plusp (length name))
      (list (intern (string-upcase category) :keyword) name note))))

(defun label-line (line)
  "When LINE is a section label, its kind, number (or NIL) and label; else
NIL.  \"2.\" is a verse; a known word may come bare, bracketed or with a
colon (\"Chorus\", \"[Bridge 2]\", \"Refrain:\"); any other word needs the
colon (\"Women:\")."
  (or (cl-ppcre:register-groups-bind (number) ("^(\\d{1,2})\\.$" line)
        (list :verse (parse-integer number) "Verse"))
      (cl-ppcre:register-groups-bind (word number colon)
          ("^\\[?([A-Za-z][A-Za-z'.-]*(?: [A-Za-z'.-]+)?)\\s*(\\d{1,2})?\\]?(:)?$" line)
        (let ((kind (label-kind word)))
          (when (or colon (not (eq kind :other)))
            (list kind
                  (and number (parse-integer number))
                  (cond ((eq kind :other) word)
                        ((member word '("build" "coda" "ending" "end")
                                 :test #'string-equal)
                         (string-capitalize word))
                        (t (third (assoc kind *kinds*))))))))))

(defun blocks (lines)
  "LINES split at its blank lines into non-empty lists of lines."
  (let ((blocks '()) (block '()))
    (dolist (line lines)
      (cond ((string= line "")
             (when block
               (push (nreverse block) blocks)
               (setf block '())))
            (t (push line block))))
    (when block
      (push (nreverse block) blocks))
    (nreverse blocks)))

(defun number-sections (sections)
  "SECTIONS with a number given to each that has none: the lowest its kind
has not used yet."
  (let ((used '()))
    (dolist (section sections)
      (when (section-number section)
        (push (cons (section-kind section) (section-number section)) used)))
    (mapcar (lambda (section)
              (if (section-number section)
                  section
                  (let ((number (loop for n from 1
                                      unless (member (cons (section-kind section) n)
                                                     used :test #'equal)
                                        return n)))
                    (push (cons (section-kind section) number) used)
                    (list* :number number
                           (loop for (k v) on section by #'cddr
                                 unless (eq k :number) append (list k v))))))
            sections)))

(defun parse-body (lines)
  "The sections that LINES, a song's text without its headers, write."
  (let ((sections '()) (label nil) (pending '()))
    (flet ((flush ()
             (let ((blocks (blocks (nreverse pending))))
               (setf pending '())
               (cond ((null blocks))
                     ((null label)
                      ;; Under no label: verses, one per block.
                      (dolist (block blocks)
                        (push (list :kind :verse :number nil :label "Verse"
                                    :slides (list block))
                              sections)))
                     ((eq (first label) :wordless))
                     (t
                      (push (list :kind (first label) :number (second label)
                                  :label (third label) :slides blocks)
                            sections))))))
      (dolist (line lines)
        (let ((found (label-line line)))
          (cond (found (flush) (setf label found))
                (t (push line pending)))))
      (flush))
    (number-sections (nreverse sections))))

(defun parse-song (text)
  "The SONG that TEXT writes in the text format.  Lenient: what is missing
is NIL, and SONG-PROBLEMS says what is wrong with the result."
  (let* ((lines (mapcar #'trim (cl-ppcre:split "\\r?\\n" text)))
         (song (make-song)))
    (loop while (and lines (string= (first lines) "")) do (pop lines))
    (loop for line = (first lines)
          while (and line (string/= line ""))
          do (multiple-value-bind (match groups)
                 (cl-ppcre:scan-to-strings "^([A-Za-z]+):\\s*(.*)$" line)
               (unless match (return))
               (let ((key (string-downcase (aref groups 0)))
                     (value (aref groups 1)))
                 (cond ((string= key "title") (setf (song-title song) value))
                       ((string= key "id") (setf (song-id song) value))
                       ((string= key "alt")
                        (setf (song-alt-titles song)
                              (append (song-alt-titles song) (split-list value ";"))))
                       ((member key '("writers" "writer" "author" "authors" "artist")
                                :test #'string=)
                        (setf (song-writers song) value))
                       ((string= key "copyright") (setf (song-copyright song) value))
                       ((string= key "ccli") (setf (song-ccli song) value))
                       ((string= key "number")
                        (setf (song-number song) (parse-integer value :junk-allowed t)))
                       ((string= key "page")
                        (setf (song-page song) (parse-integer value :junk-allowed t)))
                       ((string= key "order")
                        (setf (song-order song)
                              (mapcar #'string-upcase (split-list value "[\\s,]+"))))
                       ((string= key "tags")
                        (setf (song-tags song)
                              (append (song-tags song)
                                      (remove nil (mapcar #'parse-tag
                                                          (split-list value ";"))))))
                       ((string= key "reviewed")
                        (setf (song-reviewed song)
                              (and (member value '("yes" "y" "true") :test #'string-equal)
                                   t)))
                       ;; Not a header: the body starts here.
                       (t (return))))
               (pop lines)))
    (setf (song-sections song) (parse-body lines))
    (dolist (field '(song-title song-writers song-copyright song-ccli song-id))
      (when (equal (funcall field song) "")
        (funcall (fdefinition `(setf ,field)) nil song)))
    song))

(defun song-problems (song)
  "What keeps SONG from being shown, as a list of sentences; NIL when it is
fine."
  (let ((keys (mapcar #'section-key (song-sections song))))
    (append
     (unless (song-title song) (list "It has no Title: header."))
     (unless (song-sections song) (list "It has no words."))
     (loop for (key . rest) on keys
           when (member key rest :test #'string=)
             collect (format nil "Two sections are both ~A." key))
     (loop for key in (song-order song)
           unless (member key keys :test #'string=)
             collect (format nil "Order: names ~A, which the song does not have." key)))))

;;; Writing

(defun print-tag (tag)
  (destructuring-bind (category name &optional note) tag
    (format nil "~(~A~)=~A~@[ {~A}~]" category name note)))

(defun print-song (song)
  "SONG in the text format; PARSE-SONG reads it back."
  (with-output-to-string (out)
    (flet ((header (name value)
             (when value (format out "~A: ~A~%" name value))))
      (header "Title" (song-title song))
      (header "Id" (song-id song))
      (when (song-alt-titles song)
        (format out "Alt: ~{~A~^; ~}~%" (song-alt-titles song)))
      (header "Writers" (song-writers song))
      (header "Copyright" (song-copyright song))
      (header "CCLI" (song-ccli song))
      (header "Number" (song-number song))
      (header "Page" (song-page song))
      (when (song-order song)
        (format out "Order: ~{~A~^ ~}~%" (song-order song)))
      (when (song-tags song)
        (format out "Tags: ~{~A~^; ~}~%" (mapcar #'print-tag (song-tags song))))
      (when (song-reviewed song)
        (header "Reviewed" "yes")))
    (dolist (section (song-sections song))
      (terpri out)
      (if (eq (section-kind section) :verse)
          (format out "~D.~%" (section-number section))
          (format out "~A:~%" (section-name section)))
      (format out "~{~{~A~%~}~^~%~}" (section-slides section)))))

(defun song-first-line (song)
  "The first line of SONG's words, or NIL."
  (first (first (section-slides (first (song-sections song))))))

(defun slug (string)
  "STRING in lowercase with each run of other than letters and digits made
one hyphen: \"O Come, All Ye Faithful\" -> \"o-come-all-ye-faithful\"."
  (string-trim "-" (cl-ppcre:regex-replace-all "[^a-z0-9]+" (string-downcase string) "-")))

;;; Slides

(defparameter *max-lines* 6
  "The most lines on one slide: a longer block of a section is split.")

(defun split-lines (lines &optional (max-lines *max-lines*))
  "LINES as a list of slides of at most MAX-LINES lines, as even as can be."
  (let* ((count (length lines))
         (parts (max 1 (ceiling count max-lines)))
         (size (ceiling count parts)))
    (loop for rest = lines then (nthcdr size rest)
          while rest
          collect (subseq rest 0 (min size (length rest))))))

(defun slide-key (slide) (getf slide :key))
(defun slide-label (slide) (getf slide :label))
(defun slide-lines (slide) (getf slide :lines))
(defun slide-part (slide) (getf slide :part))
(defun slide-of (slide) (getf slide :of))

(defun slide-group (slide)
  "The key that jumps to SLIDE: its section's key without the number, or the
verse's number."
  (getf slide :group))

(defun section-deck (section index)
  "The slides of SECTION, the INDEXth of its song."
  (let* ((slides (loop for block in (section-slides section)
                       append (split-lines block)))
         (of (length slides)))
    (loop for lines in slides
          for part from 1
          collect (list :key (section-key section)
                        :group (if (eq (section-kind section) :verse)
                                   (section-key section)
                                   (kind-letter (section-kind section)))
                        :label (section-name section)
                        :section index
                        :rank (or (section-number section) 1)
                        :part part :of of :lines lines))))

(defun song-deck (song)
  "The slides of SONG in the order it is sung, as a list of plists (:KEY
:GROUP :LABEL :SECTION :PART :OF :LINES): its Order:, or else as written,
and then any section the order left out."
  (let* ((sections (song-sections song))
         (keys (mapcar #'section-key sections))
         (order (remove-if-not (lambda (key) (member key keys :test #'string=))
                               (or (song-order song) keys)))
         (order (append order (set-difference keys order :test #'string=))))
    (loop for key in order
          for index = (position key keys :test #'string=)
          append (section-deck (nth index sections) index))))

(defun deck-jump (deck index group)
  "The index in DECK that pressing GROUP (\"2\", \"R\", \"B\") leads to from
INDEX, or NIL when the song has no such section.

The first press shows the group's first slide, a press while one of its
slides is showing the next of them (the second half of a long refrain,
Refrain 2), round again from the last.  Of the places where that slide stands
in the deck, the next from INDEX on is taken, so that what follows it is
what follows it in the song."
  (let* ((group (if (string-equal group "C") "R" (string-upcase group)))
         (members (remove-duplicates
                   (remove-if-not (lambda (slide) (string= (slide-group slide) group))
                                  deck)
                   :test #'equal
                   :key (lambda (slide) (list (getf slide :section) (slide-part slide)))
                   :from-end t))
         (members (stable-sort (copy-list members) #'<
                               :key (lambda (slide) (getf slide :rank))))
         (current (and index (nth index deck))))
    (when members
      (let* ((here (position-if (lambda (slide)
                                  (and current
                                       (eql (getf slide :section) (getf current :section))
                                       (eql (slide-part slide) (slide-part current))))
                                members))
             (target (nth (if here (mod (1+ here) (length members)) 0) members))
             (same (lambda (slide)
                     (and (eql (getf slide :section) (getf target :section))
                          (eql (slide-part slide) (slide-part target))))))
        (or (position-if same deck :start (or index 0))
            (position-if same deck))))))
