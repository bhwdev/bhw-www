;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

;;;; Notes, their pitch classes and their transposition, for the chords of
;;;; bhw/plp/song (through bhw/plp/chords, which reads and writes the names).
;;;; An ACL2 book and SBCL's source alike: see package.lisp.
;;;; formal/pitch.lisp proves what it does.
;;;;
;;;; A note is (LETTER . ACCIDENTAL): LETTER 0 to 6 for C D E F G A B, and
;;;; ACCIDENTAL the semitones it is raised by, -1 for a flat, 1 for a sharp.

(in-package "BHW/ACL2")

(set-verify-guards-eagerness 2)

(defun note-p (note)
  "True when NOTE is a note, (LETTER . ACCIDENTAL)."
  (declare (xargs :guard t))
  (and (consp note)
       (integerp (car note)) (<= 0 (car note)) (< (car note) 7)
       (integerp (cdr note))))

(defun mod12 (x)
  "X modulo 12, for an integer X (MOD), by adding or taking away 12."
  (declare (xargs :guard (integerp x)
                  :measure (if (and (integerp x) (< x 0)) (- 12 x) (nfix x))))
  (cond ((not (integerp x)) 0)
        ((< x 0) (mod12 (+ x 12)))
        ((< x 12) x)
        (t (mod12 (- x 12)))))

(local
 (defthm mod12-range
   (implies (integerp x)
            (and (<= 0 (mod12 x)) (< (mod12 x) 12)))
   :rule-classes :linear))

(defun natural-pitch (letter)
  "How many semitones the natural note LETTER (0 to 6, C to B) is above C."
  (declare (xargs :guard (and (natp letter) (< letter 7))))
  (cond ((eql letter 0) 0)
        ((eql letter 1) 2)
        ((eql letter 2) 4)
        ((eql letter 3) 5)
        ((eql letter 4) 7)
        ((eql letter 5) 9)
        (t 11)))

(defun pitch-class (note)
  "How many semitones NOTE is above C, 0 to 11."
  (declare (xargs :guard (note-p note)))
  (mod12 (+ (natural-pitch (car note)) (cdr note))))

(defun spell (pc flats)
  "The note of pitch class PC (0 to 11), a black key spelled with a flat when
FLATS and with a sharp when not."
  (declare (xargs :guard (and (natp pc) (< pc 12))))
  (cond ((eql pc 0) '(0 . 0))
        ((eql pc 1) (if flats '(1 . -1) '(0 . 1)))
        ((eql pc 2) '(1 . 0))
        ((eql pc 3) (if flats '(2 . -1) '(1 . 1)))
        ((eql pc 4) '(2 . 0))
        ((eql pc 5) '(3 . 0))
        ((eql pc 6) (if flats '(4 . -1) '(3 . 1)))
        ((eql pc 7) '(4 . 0))
        ((eql pc 8) (if flats '(5 . -1) '(4 . 1)))
        ((eql pc 9) '(5 . 0))
        ((eql pc 10) (if flats '(6 . -1) '(5 . 1)))
        (t '(6 . 0))))

(defun transpose-note (note semitones flats)
  "NOTE moved SEMITONES up (down when negative), spelled as SPELL does."
  (declare (xargs :guard (and (note-p note) (integerp semitones))))
  (spell (mod12 (+ (pitch-class note) semitones)) flats))
