;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:bhw/plp/chords
  (:use #:coalton #:coalton-prelude)
  (:local-nicknames (#:str #:coalton/string)
                    (#:acl2 #:bhw/acl2))
  (:export #:transpose-name
           #:flat-key-name-p)
  (:documentation
   "The notes chords are named with, in Coalton: a note's name, \"F#\" or
\"Bb\", read into a LETTER and an ACCIDENTAL, moved by the transposition of
bhw/acl2 (src/acl2/pitch.lisp, whose properties formal/pitch.lisp proves) and
written back.  The types are what make every note handed to TRANSPOSE-NOTE
and PITCH-CLASS one of their guard, NOTE-P: a letter is one of seven, an
accidental one of three, so the numbers they are passed as are 0 to 6 and -1
to 1 by construction; a string that names no note is NONE, which the type
checker makes every caller deal with; and a MATCH that misses a case is a
compiler warning, which fails the build.

Called from bhw/plp/song with Lisp's strings, integers and booleans, which
the functions' type declarations have SBCL check on the way in."))

(in-package #:bhw/plp/chords)

(coalton-toplevel

  ;; A variable is never named after a constructor: the reader folds case, so
  ;; one named NOTE would hide the constructor Note.

  ;;; Names

  (repr :enum)
  (define-type Letter
    "The letter of a note, C to B."
    LetterC LetterD LetterE LetterF LetterG LetterA LetterB)

  (repr :enum)
  (define-type Accidental
    "What a note's letter is raised by: a chord's notes have one flat or one
sharp at most."
    Flat Natural Sharp)

  (define-type Note
    "A note as it is named: its letter and its accidental."
    (Note Letter Accidental))

  (declare letter-name (Letter -> String))
  (define (letter-name letter)
    (match letter
      ((LetterC) "C") ((LetterD) "D") ((LetterE) "E") ((LetterF) "F")
      ((LetterG) "G") ((LetterA) "A") ((LetterB) "B")))

  (declare char-letter (Char -> Optional Letter))
  (define (char-letter ch)
    (match ch
      (#\C (Some LetterC)) (#\D (Some LetterD)) (#\E (Some LetterE))
      (#\F (Some LetterF)) (#\G (Some LetterG)) (#\A (Some LetterA))
      (#\B (Some LetterB)) (_ None)))

  (declare accidental-name (Accidental -> String))
  (define (accidental-name accidental)
    (match accidental
      ((Flat) "b") ((Natural) "") ((Sharp) "#")))

  (declare name-accidental (String -> Optional Accidental))
  (define (name-accidental name)
    (match name
      ("b" (Some Flat)) ("" (Some Natural)) ("#" (Some Sharp)) (_ None)))

  (declare parse-note (String -> Optional Note))
  (define (parse-note name)
    "The note NAME names, \"F#\" or \"Bb\", or NONE."
    (do (first <- (str:ref name 0))
        (letter <- (char-letter first))
        (accidental <- (name-accidental (str:substring name 1 (str:length name))))
        (pure (Note letter accidental))))

  (declare note-name (Note -> String))
  (define (note-name n)
    (match n
      ((Note letter accidental)
       (str:concat (letter-name letter) (accidental-name accidental)))))

  ;;; As bhw/acl2 numbers them: (LETTER . ACCIDENTAL)

  (declare letter-index (Letter -> Integer))
  (define (letter-index letter)
    (match letter
      ((LetterC) 0) ((LetterD) 1) ((LetterE) 2) ((LetterF) 3)
      ((LetterG) 4) ((LetterA) 5) ((LetterB) 6)))

  (declare index-letter (Integer -> Optional Letter))
  (define (index-letter index)
    (match index
      (0 (Some LetterC)) (1 (Some LetterD)) (2 (Some LetterE)) (3 (Some LetterF))
      (4 (Some LetterG)) (5 (Some LetterA)) (6 (Some LetterB)) (_ None)))

  (declare accidental-semitones (Accidental -> Integer))
  (define (accidental-semitones accidental)
    (match accidental
      ((Flat) -1) ((Natural) 0) ((Sharp) 1)))

  (declare semitones-accidental (Integer -> Optional Accidental))
  (define (semitones-accidental semitones)
    (match semitones
      (-1 (Some Flat)) (0 (Some Natural)) (1 (Some Sharp)) (_ None)))

  (declare pitch-class (Note -> Integer))
  (define (pitch-class n)
    "How many semitones the note N is above C, 0 to 11 (bhw/acl2's
PITCH-CLASS)."
    (match n
      ((Note letter accidental)
       (let ((l (letter-index letter))
             (a (accidental-semitones accidental)))
         (lisp (-> Integer) (l a)
           (acl2:pitch-class (cl:cons l a)))))))

  (declare transpose (Integer * Boolean * Note -> Note))
  (define (transpose semitones flats n)
    "The note N moved SEMITONES up (down when negative) and spelled with flats
when FLATS, with sharps when not (bhw/acl2's TRANSPOSE-NOTE)."
    (match n
      ((Note letter accidental)
       (let ((l (letter-index letter))
             (a (accidental-semitones accidental)))
         (match (lisp (-> (Tuple Integer Integer)) (l a semitones flats)
                  (cl:let ((moved (acl2:transpose-note (cl:cons l a) semitones flats)))
                    (Tuple (cl:car moved) (cl:cdr moved))))
           ((Tuple l2 a2)
            (match (Tuple (index-letter l2) (semitones-accidental a2))
              ((Tuple (Some letter2) (Some accidental2))
               (Note letter2 accidental2))
              ;; SPELL-SOUND (formal/pitch.lisp): never.
              (_ (error "TRANSPOSE-NOTE gave no note of a letter and one accidental")))))))))

  ;;; For bhw/plp/song

  (declare transpose-name (String * Integer * Boolean -> String))
  (define (transpose-name name semitones flats)
    "The name of the note NAME (\"F#\", \"Bb\") SEMITONES higher, spelled with
flats when FLATS and with sharps when not; NAME itself when it names no
note."
    (match (parse-note name)
      ((Some n) (note-name (transpose semitones flats n)))
      ((None) name)))

  (declare flat-key-name-p (String * Boolean -> Boolean))
  (define (flat-key-name-p tonic minor)
    "True when the key of the note TONIC (\"F\", \"Bb\"), minor when MINOR, is
written with flats: F, Bb, Eb, Ab and Db major, and the minor keys a third
below them.  False when TONIC names no note."
    (match (parse-note tonic)
      ((Some n)
       ;; A minor key is written as the major a third above.
       (match (pitch-class (if minor (transpose 3 False n) n))
         (5 True) (10 True) (3 True) (8 True) (1 True) (_ False)))
      ((None) False))))
