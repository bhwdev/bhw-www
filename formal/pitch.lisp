; Model of src/plp/song.lisp: NOTE-INDEX (line 267), *SHARP-NOTES* and
; *FLAT-NOTES* (lines 264-265), TRANSPOSE-CHORD (line 275) at the level of
; one note.
;
; A note is (LETTER . ACCIDENTAL): LETTER in 0..6 for C D E F G A B, and
; ACCIDENTAL in {-1, 0, 1} for flat, natural, sharp.  The Lisp reads the same
; pair from the name "Bb", "F#", "C" by NOTE-INDEX; the model takes the pair as
; given, so the correspondence is by reading, not by proof.
;
; The Lisp reduces with MOD 12.  The model uses BHW-WRAP, which is MOD 12 on
; the domain it is applied to (-12..35), and transposition amounts in 0..11
; (the Lisp's MOD of any amount is one of these, by the same reduction).
; Only ACL2's built-in arithmetic is used.
;
; Proved:
;   1. a note's pitch class is in 0..11;
;   2. both spelling tables sound: each pitch class spells to a note of that
;      pitch class;
;   3. transposing a note by S (0..11) moves its pitch class by S mod 12;
;   4. transposing by S then T is transposing by S+T mod 12 (for S, T in 0..11);
;   5. transposing by S then by 12-S (for S in 0..11) returns the pitch class.

(in-package "ACL2")

(defun bhw-natural-pc (letter)
  (cond ((eql letter 0) 0)
        ((eql letter 1) 2)
        ((eql letter 2) 4)
        ((eql letter 3) 5)
        ((eql letter 4) 7)
        ((eql letter 5) 9)
        (t 11)))

(defun bhw-wrap (x)
  (cond ((< x 0) (+ x 12))
        ((< x 12) x)
        ((< x 24) (- x 12))
        (t (- x 24))))

(defun bhw-valid-note (note)
  (and (consp note)
       (natp (car note)) (< (car note) 7)
       (integerp (cdr note))
       (<= -1 (cdr note)) (<= (cdr note) 1)))

(defun bhw-pc (note)
  (bhw-wrap (+ (bhw-natural-pc (car note)) (cdr note))))

;; *SHARP-NOTES* (flats nil) and *FLAT-NOTES* (flats t), by pitch class.
(defun bhw-spell (pc flats)
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
  (bhw-spell (bhw-wrap (+ (bhw-pc note) semitones)) flats))

;;; Range

(defthm bhw-natural-pc-range
  (and (integerp (bhw-natural-pc k))
       (<= 0 (bhw-natural-pc k))
       (<= (bhw-natural-pc k) 11))
  :rule-classes nil
  :hints (("Goal" :in-theory (enable bhw-natural-pc))))

(defthm bhw-wrap-range
  (implies (and (integerp x) (<= 0 x) (<= x 35))
           (and (<= 0 (bhw-wrap x)) (< (bhw-wrap x) 12)))
  :hints (("Goal" :in-theory (enable bhw-wrap))))

(defthm bhw-wrap-small
  (implies (and (integerp x) (<= 0 x) (< x 12))
           (equal (bhw-wrap x) x))
  :hints (("Goal" :in-theory (enable bhw-wrap))))

(defthm bhw-pc-range
  (implies (bhw-valid-note note)
           (and (<= 0 (bhw-pc note)) (< (bhw-pc note) 12)))
  :hints (("Goal" :in-theory (enable bhw-pc)
                  :use ((:instance bhw-natural-pc-range (k (car note)))))))

;;; Spelling tables: each pitch class is spelled to a note of that pitch class.

(defthm bhw-spell-sound
  (implies (and (natp k) (< k 12))
           (equal (bhw-pc (bhw-spell k flats)) k))
  :hints (("Goal" :in-theory (enable bhw-spell bhw-pc bhw-natural-pc bhw-wrap))))

;;; Transposition

(defthm bhw-transpose-pc
  (implies (and (bhw-valid-note note) (natp semitones) (< semitones 12))
           (equal (bhw-pc (bhw-transpose-note note semitones flats))
                  (bhw-wrap (+ (bhw-pc note) semitones))))
  :hints (("Goal" :in-theory (enable bhw-transpose-note)
                  :use ((:instance bhw-spell-sound
                                   (k (bhw-wrap (+ (bhw-pc note) semitones))))
                        (:instance bhw-pc-range)))))

(defthm bhw-transpose-zero
  (implies (and (bhw-valid-note note))
           (equal (bhw-pc (bhw-transpose-note note 0 flats))
                  (bhw-pc note)))
  :hints (("Goal" :use (bhw-transpose-pc (:instance bhw-pc-range)))))

(defthm bhw-transpose-composes
  (implies (and (bhw-valid-note note)
                (natp s) (< s 12) (natp u) (< u 12))
           (equal (bhw-wrap (+ (bhw-pc (bhw-transpose-note note s nil)) u))
                  (bhw-wrap (+ (bhw-pc note) s u))))
  :hints (("Goal" :use (bhw-transpose-pc (:instance bhw-pc-range)
                        (:instance bhw-wrap-range
                                   (x (+ (bhw-pc note) s u)))))))

(defthm bhw-transpose-inverse
  (implies (and (bhw-valid-note note) (natp s) (< s 12))
           (equal (bhw-pc (bhw-transpose-note (bhw-transpose-note note s nil)
                                              (if (eql s 0) 0 (- 12 s))
                                              flats))
                  (bhw-pc note)))
  :hints (("Goal" :use (bhw-transpose-pc (:instance bhw-pc-range)))))
