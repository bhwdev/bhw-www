; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
; SPDX-License-Identifier: AGPL-3.0-or-later

; Proofs about the notes of src/acl2/pitch.lisp, which bhw/plp/chords runs
; for bhw/plp/song's TRANSPOSE-CHORD and FLAT-KEY-P.
;
; Proved, for every note (NOTE-P: any letter 0..6, any integer accidental)
; and every integer amount, up or down:
;   1. MOD12 is in 0..11, is the identity there, and is periodic: adding 12
;      changes nothing, and reducing before adding is reducing after;
;   2. a note's pitch class is in 0..11;
;   3. both spellings are sound: each pitch class spells to a note of that
;      pitch class, with a letter 0..6 and an accidental -1, 0 or 1;
;   4. transposing a note by S moves its pitch class by S, modulo 12;
;   5. transposing by S and then by U is transposing by S + U;
;   6. transposing by 0, by 12, or by S and back by -S keeps the pitch class.
; Built-in arithmetic only (no books).

(in-package "BHW/ACL2")

(include-book "../src/acl2/pitch")

;;; MOD12

(defthm mod12-range
  (implies (integerp x)
           (and (<= 0 (mod12 x)) (< (mod12 x) 12)))
  :rule-classes (:rewrite :linear))

(defthm natp-of-mod12
  (natp (mod12 x))
  :rule-classes :type-prescription)

(defthm mod12-small
  (implies (and (integerp x) (<= 0 x) (< x 12))
           (equal (mod12 x) x)))

(defthm mod12-plus-12
  (implies (integerp x)
           (equal (mod12 (+ 12 x)) (mod12 x)))
  :hints (("Goal" :expand ((mod12 (+ 12 x)) (mod12 x)))))

(defthm mod12-of-mod12-plus
  (implies (and (integerp x) (integerp y))
           (equal (mod12 (+ y (mod12 x)))
                  (mod12 (+ x y))))
  :hints (("Goal" :induct (mod12 x))
          ("Subgoal *1/3" :use ((:instance mod12-plus-12 (x (+ -12 x y)))))))

(defthm mod12-of-mod12
  (implies (integerp x)
           (equal (mod12 (mod12 x)) (mod12 x))))

;;; Notes

(defthm pitch-class-range
  (implies (note-p note)
           (and (natp (pitch-class note))
                (< (pitch-class note) 12)))
  :rule-classes (:rewrite (:linear :corollary
                           (implies (note-p note)
                                    (and (<= 0 (pitch-class note))
                                         (< (pitch-class note) 12))))))

(defthm spell-sound
  (implies (and (natp k) (< k 12))
           (and (note-p (spell k flats))
                (member (cdr (spell k flats)) '(-1 0 1))
                (equal (pitch-class (spell k flats)) k)))
  :hints (("Goal" :in-theory (enable spell pitch-class natural-pitch mod12))))

(in-theory (disable spell pitch-class))

;;; Transposition

(defthm note-p-of-transpose-note
  (implies (and (note-p note) (integerp s))
           (note-p (transpose-note note s flats))))

(defthm transpose-pitch-class
  (implies (and (note-p note) (integerp s))
           (equal (pitch-class (transpose-note note s flats))
                  (mod12 (+ (pitch-class note) s)))))

(in-theory (disable transpose-note))

(defthm transpose-composes
  (implies (and (note-p note) (integerp s) (integerp u))
           (equal (pitch-class (transpose-note (transpose-note note s f) u g))
                  (mod12 (+ (pitch-class note) s u))))
  :hints (("Goal" :use ((:instance mod12-of-mod12-plus
                                   (x (+ (pitch-class note) s))
                                   (y u))))))

(defthm transpose-zero
  (implies (note-p note)
           (equal (pitch-class (transpose-note note 0 flats))
                  (pitch-class note))))

(defthm transpose-octave
  (implies (note-p note)
           (equal (pitch-class (transpose-note note 12 flats))
                  (pitch-class note)))
  :hints (("Goal" :use ((:instance mod12-plus-12 (x (pitch-class note)))))))

(defthm transpose-inverse
  (implies (and (note-p note) (integerp s))
           (equal (pitch-class (transpose-note (transpose-note note s f) (- s) g))
                  (pitch-class note)))
  :hints (("Goal" :use ((:instance transpose-composes (u (- s)))))))
