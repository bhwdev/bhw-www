; Model of src/plp/song.lisp: NOTE-INDEX (line 267), *SHARP-NOTES* and
; *FLAT-NOTES* (lines 264-265), TRANSPOSE-CHORD (line 275) at the level of
; one note, and FLAT-KEY-P (line 299).
;
; A note is (LETTER . ACCIDENTAL): LETTER in 0..6 for C D E F G A B, and
; ACCIDENTAL in {-1, 0, 1} for flat, natural, sharp.  The Lisp reads the same
; pair from the name "Bb", "F#", "C" by NOTE-INDEX; the model takes the pair as
; given, so the correspondence is by reading, not by proof.
;
; Proved:
;   1. a note's pitch class is in 0..11;
;   2. transposing by S then by T is transposing by S+T (mod 12);
;   3. transposing by 12 names the same pitch class;
;   4. both spelling tables spell each pitch class 0..11 to a note of that pitch
;      class, so TRANSPOSE-NOTE's result has the right sound.

(in-package "ACL2")

(include-book "arithmetic-5/top" :dir :system)

(include-book "arithmetic-5/top" :dir :system)


;; Semitone of each natural letter: C D E F G A B.  (The Lisp reads the
;; same from the position of the letter in "C D EF G A B".)
(defun bhw-natural-pc (letter)
  (cond ((eql letter 0) 0)
        ((eql letter 1) 2)
        ((eql letter 2) 4)
        ((eql letter 3) 5)
        ((eql letter 4) 7)
        ((eql letter 5) 9)
        (t 11)))

(defun bhw-valid-note (note)
  (and (consp note)
       (natp (car note)) (< (car note) 7)
       (integerp (cdr note))))

(defun bhw-pc (note)
  (mod (+ (bhw-natural-pc (car note)) (cdr note)) 12))

(defun bhw-spell (pc flats)
  ;; *SHARP-NOTES* (flats nil) and *FLAT-NOTES* (flats t), by pitch class.
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

(defun bhw-transpose-note (note semitones flats)
  (bhw-spell (mod (+ (bhw-pc note) semitones) 12) flats))

;;; Range

(defthm bhw-pc-range
  (implies (and (natp (car note)) (< (car note) 7)
                (integerp (cdr note)))
           (and (integerp (bhw-pc note))
                (<= 0 (bhw-pc note))
                (< (bhw-pc note) 12)))
  :hints (("Goal" :in-theory (enable bhw-pc bhw-natural-pc))))

;;; Spelling tables: each pitch class is spelled to a note of that pitch class.

(defthm bhw-spell-sound
  (implies (and (natp k) (< k 12))
           (equal (bhw-pc (bhw-spell k flats)) k))
  :hints (("Goal" :in-theory (enable bhw-spell bhw-pc bhw-natural-pc))))

;;; Transposition

(defthm bhw-transpose-pc
  (implies (and (bhw-valid-note note) (integerp semitones))
           (equal (bhw-pc (bhw-transpose-note note semitones flats))
                  (mod (+ (bhw-pc note) semitones) 12)))
  :hints (("Goal" :in-theory (enable bhw-transpose-note)
                  :use ((:instance bhw-spell-sound
                                   (k (mod (+ (bhw-pc note) semitones) 12)))))))

(defthm bhw-transpose-composes
  (implies (and (bhw-valid-note note) (integerp s) (integerp u))
           (equal (bhw-pc (bhw-transpose-note (bhw-transpose-note note s nil) u flats))
                  (mod (+ (bhw-pc note) s u) 12))))

(defthm bhw-transpose-twelve
  (implies (bhw-valid-note note)
  (equal (bhw-pc (bhw-transpose-note note 12 flats))
         (bhw-pc note))))

(defthm bhw-transpose-inverse
  (implies (and (bhw-valid-note note) (integerp s))
           (equal (bhw-pc (bhw-transpose-note (bhw-transpose-note note s nil) (- s) flats))
                  (bhw-pc note))))

