;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:bhw/tests/acl2
  (:use #:common-lisp)
  (:import-from #:rove #:deftest #:ok #:testing #:signals)
  (:local-nicknames (#:acl2 #:bhw/acl2)
                    (#:song #:bhw/plp/song)
                    (#:chords #:bhw/plp/chords)
                    (#:vb #:bhw/slack-bot/van-booking))
  (:documentation
   "Tests of the books of src/acl2/, which SBCL runs and formal/ proves
things of.  That their function bodies are Common Lisp, so that ACL2 and SBCL
read the same code; and differential tests: each function of the application
that runs one of them, against the Lisp it replaced (the REFERENCE- functions
below, as they stood), on every input of a small domain and on random ones
from a fixed seed.  The proofs say what the books do; these say it is what the
application did."))

(in-package #:bhw/tests/acl2)

;;; The books are Common Lisp

(defparameter *books* '("slides" "deck" "pitch" "windows" "reminder"))

(defun book-forms (book)
  "The forms of the book BOOK of src/acl2/, read as SBCL reads them."
  (let ((*package* (find-package '#:bhw/acl2)))
    (with-open-file (in (asdf:system-relative-pathname
                         "bhw" (format nil "src/acl2/~A.lisp" book)))
      (loop for form = (read in nil in)
            until (eq form in)
            collect form))))

(defun let-variables (form)
  "The variables every LET and LET* in FORM binds."
  (when (consp form)
    (append (and (member (first form) '(let let*))
                 (mapcar (lambda (binding) (if (consp binding) (first binding) binding))
                         (second form)))
            (loop for rest = form then (cdr rest)
                  while (consp rest)
                  append (let-variables (car rest))))))

(defun form-symbols (form)
  "The symbols in FORM, quoted data left out."
  (cond ((symbolp form) (list form))
        ((atom form) '())
        ((eq (first form) 'quote) '())
        (t (loop for rest = form then (cdr rest)
                 while (consp rest)
                 append (form-symbols (car rest))))))

(defun stray-symbols (definition functions)
  "The symbols the body of DEFINITION, a DEFUN, names that are none of Common
Lisp's, FUNCTIONS (the books' own) or its variables."
  (destructuring-bind (name lambda-list &rest body) (rest definition)
    (declare (ignore name))
    (let* ((body (remove-if (lambda (form)
                              (or (stringp form)
                                  (and (consp form) (eq (first form) 'declare))))
                            body))
           (variables (append lambda-list (let-variables body))))
      (remove-duplicates
       (remove-if (lambda (symbol)
                    (or (eq (symbol-package symbol) (find-package '#:common-lisp))
                        (keywordp symbol)
                        (member symbol functions)
                        (member symbol variables)))
                  (form-symbols body))))))

(deftest books-are-common-lisp
  (let* ((forms (loop for book in *books* append (book-forms book)))
         (definitions (remove 'defun forms :key #'first :test-not #'eq))
         (functions (mapcar #'second definitions)))
    (testing "a book holds definitions, and what is ACL2's alone"
      (ok (every (lambda (form)
                   (member (first form) '(in-package defun
                                          acl2::local acl2::set-verify-guards-eagerness)))
                 forms)))
    (testing "every body names Common Lisp's symbols, the books' functions and its variables only"
      (dolist (definition definitions)
        (let ((stray (stray-symbols definition functions)))
          (ok (null stray)
              (format nil "~(~A~)~@[ names ~{~(~A~)~^, ~}~]" (second definition) stray)))))
    (testing "every function SBCL compiled is one of the books'"
      (ok (every #'fboundp functions)))
    (testing "the check finds a body that calls ACL2's own functions"
      (ok (equal (list (intern "ZP" '#:bhw/acl2))
                 (stray-symbols `(defun f (n) (if (,(intern "ZP" '#:bhw/acl2) n) 0 (f (- n 1))))
                                '(f)))))))

;;; Differential tests

(defmacro with-seed (&body body)
  "BODY with *RANDOM-STATE* from a fixed seed, so that a failure recurs."
  `(let ((*random-state* (sb-ext:seed-random-state 20261123)))
     ,@body))

(defun pick (list)
  (nth (random (length list)) list))

;;; Slides

(defun reference-split-lines (lines max-lines)
  (let* ((count (length lines))
         (parts (max 1 (ceiling count max-lines)))
         (size (ceiling count parts)))
    (loop for rest = lines then (nthcdr size rest)
          while rest
          collect (subseq rest 0 (min size (length rest))))))

(deftest split-lines-as-before
  (testing "every block of up to 60 lines, on slides of up to 1 to 15 lines"
    (ok (loop for count from 0 to 60
              for lines = (loop for i below count collect (format nil "line ~D" i))
              always (loop for max-lines from 1 to 15
                           always (equal (song:split-lines lines max-lines)
                                         (reference-split-lines lines max-lines))))))
  (testing "outside the guard, an error rather than a slide"
    (ok (signals (song:split-lines '("a" "b") 0)))
    (ok (signals (song:split-lines '("a" . "b") 6)))))

;;; The deck

(defun reference-deck-next (deck index slide)
  (or (position slide deck :test #'song:same-slide-p
                           :start (min (1+ (or index -1)) (length deck)))
      (position slide deck :test #'song:same-slide-p)))

(defun random-slide ()
  (list :key "1" :section (random 4) :part (1+ (random 2)) :lines '("x")))

(deftest deck-next-as-before
  (testing "5000 random decks of up to 12 slides, from every index"
    (with-seed
      (ok (loop repeat 5000
                for deck = (loop repeat (random 13) collect (random-slide))
                for slide = (if (and deck (zerop (random 2))) (pick deck) (random-slide))
                always (loop for index in (cons nil (loop for i from 0 to (1+ (length deck))
                                                          collect i))
                             always (eql (song:deck-next deck index slide)
                                         (reference-deck-next deck index slide)))))))
  (testing "outside the guard, an error"
    (ok (signals (song:deck-next (list (random-slide)) -1 (random-slide))))))

;;; Notes and chords

(defparameter *sharp-notes* #("C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"))
(defparameter *flat-notes* #("C" "Db" "D" "Eb" "E" "F" "Gb" "G" "Ab" "A" "Bb" "B"))

(defun reference-note-index (note)
  (mod (+ (position (char note 0) "C D EF G A B")
          (cond ((= (length note) 1) 0)
                ((char= (char note 1) #\#) 1)
                (t -1)))
       12))

(defun reference-transpose-chord (chord semitones &key flats)
  (if (or (zerop (mod semitones 12)) (cl-ppcre:scan "^\\(?N\\.?C" chord))
      chord
      (cl-ppcre:regex-replace-all
       "[A-G][#b]?" chord
       (lambda (note)
         (aref (if flats *flat-notes* *sharp-notes*)
               (mod (+ (reference-note-index note) semitones) 12)))
       :simple-calls t)))

(defun reference-flat-key-p (key)
  (let ((note (and key (cl-ppcre:scan-to-strings "^[A-G][#b]?" key))))
    (and note
         (member (mod (+ (reference-note-index note)
                         (if (cl-ppcre:scan "^[A-G][#b]?m(?!aj)" key) 3 0))
                      12)
                 '(5 10 3 8 1))
         t)))

(defparameter *note-names*
  (loop for letter across "CDEFGAB"
        append (loop for accidental in '("" "#" "b")
                     collect (format nil "~C~A" letter accidental)))
  "Every name of a note a chord is written with: 21.")

(deftest transposition-as-before
  (testing "every note, by -30 to 30 semitones, either spelling"
    (ok (loop for name in *note-names*
              always (loop for semitones from -30 to 30
                           always (loop for flats in '(nil t)
                                        always (equal (chords:transpose-name name semitones flats)
                                                      (aref (if flats *flat-notes* *sharp-notes*)
                                                            (mod (+ (reference-note-index name)
                                                                    semitones)
                                                                 12))))))))
  (testing "a name that is no note, as it is"
    (ok (every (lambda (name) (equal name (chords:transpose-name name 3 nil)))
               '("" "H" "c" "C##" "Cb#" "Bm" "x2"))))
  (testing "every chord of a note, a seventh and a bass note, by -13 to 13"
    (let ((chords (append '("N.C." "(N.C.)" "x2" "-" "")
                          (loop for name in *note-names*
                                append (list name (format nil "~Am7" name)
                                             (format nil "~Asus4" name)))
                          (loop for name in *note-names*
                                append (loop for bass in *note-names*
                                             collect (format nil "~A/~A" name bass))))))
      (ok (loop for chord in chords
                always (loop for semitones from -13 to 13
                             always (loop for flats in '(nil t)
                                          always (equal (song:transpose-chord chord semitones
                                                                              :flats flats)
                                                        (reference-transpose-chord
                                                         chord semitones :flats flats))))))))
  (testing "the key of every note, major, minor and seventh, and no key"
    (let ((keys (append '(nil "" "x" "N.C.")
                        (loop for name in *note-names*
                              append (loop for kind in '("" "m" "maj7" "m7" "7")
                                           collect (format nil "~A~A" name kind))))))
      (ok (every (lambda (key) (eq (song:flat-key-p key) (reference-flat-key-p key)))
                 keys)))))

;;; Trip windows

(defun reference-request-window (dep-time ret-time)
  (let ((start (vb::sheet-clock dep-time))
        (end (vb::sheet-clock ret-time)))
    (when (and start end (< start end))
      (values start end))))

(defun reference-windows-overlap-p (start-a end-a start-b end-b)
  (and (< start-a end-b) (< start-b end-a)))

(defun reference-trip-blocks-window-p (trip start end)
  (and (vb::booked-trip-p trip)
       (multiple-value-bind (trip-start trip-end)
           (reference-request-window (vb:trip-departure trip) (vb:trip-return-time trip))
         (or (null trip-start)
             (reference-windows-overlap-p start end trip-start trip-end)))))

(defparameter *clock-cells*
  '(nil "" "TBD" "8:00 am" "*8:30 am" "9:00 am" "9:30" "10 am" "12:30 pm" "1:00 pm"
    "midnight" "11:59 pm")
  "Cells of the sheet's time rows, as people write them, readable or not.")

(deftest windows-as-before
  (testing "every pair of cells as a window"
    (ok (loop for dep in *clock-cells*
              always (loop for ret in *clock-cells*
                           always (equal (multiple-value-list (vb::request-window dep ret))
                                         (multiple-value-list
                                          (reference-request-window dep ret)))))))
  (testing "every two windows of minutes 0 to 6"
    (ok (loop for sa from 0 to 6
              always (loop for ea from 0 to 6
                           always (loop for sb from 0 to 6
                                        always (loop for eb from 0 to 6
                                                     always (eq (acl2:windows-overlap-p sa ea sb eb)
                                                                (reference-windows-overlap-p
                                                                 sa ea sb eb))))))))
  (testing "every trip of those cells, blank or booked, against requests over the morning"
    (let ((minutes '(420 480 510 540 570 600 750)))
      (ok (loop for destination in '("" "Town")
                always (loop for dep in *clock-cells*
                             always (loop for ret in *clock-cells*
                                          for trip = (vb:make-trip :destination destination
                                                                   :departure dep
                                                                   :return-time ret)
                                          always (loop for start in minutes
                                                       always (loop for end in minutes
                                                                    always (or (>= start end)
                                                                               (eq (vb::trip-blocks-window-p
                                                                                    trip start end)
                                                                                   (reference-trip-blocks-window-p
                                                                                    trip start end))))))))))))

;;; Reminders

(defun reference-due-p (leaves now lead-minutes)
  (and leaves (<= 0 (- leaves now) (* 60 (1+ lead-minutes)))))

(deftest due-p-as-before
  (testing "every second from 2 min before NOW to 33 min after, for leads of 0, 5 and 30 min, and an unreadable time"
    (let ((now 3988972800))
      (ok (loop for lead in '(0 5 30)
                always (loop for leaves in (cons nil (loop for d from -120 to 1980
                                                           collect (+ now d)))
                             always (eq (acl2:due-p leaves now lead)
                                        (reference-due-p leaves now lead))))))))
