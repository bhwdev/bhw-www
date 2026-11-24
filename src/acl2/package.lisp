;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:bhw/acl2
  (:use #:common-lisp)
  (:export #:split-lines
           #:deck-next
           #:note-p #:mod12 #:pitch-class #:spell #:transpose-note
           #:request-window #:windows-overlap-p #:window-p
           #:trip-blocks-window-p
           #:due-p)
  (:documentation
   "Functions of the application written in the part of Common Lisp that ACL2
reasons about, so that what formal/ proves of them is proved of the code that
runs.  Each other file of this directory is an ACL2 book and SBCL's source
alike: ACL2 certifies it (formal/certify.sh), verifying the guard of each of
its functions, and SBCL compiles it as it stands.  A verified guard is ACL2's
promise that Common Lisp's evaluation of a call whose arguments satisfy the
guard returns what the logic does; the theorems of formal/ are about the
logic.

So that the two read the same code, the books' function bodies name Common
Lisp's symbols and their own functions only (src/tests/acl2.lisp checks
this): ACL2's NATP or LEN may stand in a guard, a measure or a lemma, which
SBCL ignores, never in a body.  This package uses COMMON-LISP alone; ACL2's
(src/acl2/package.lsp) has ACL2's symbols too.

The callers in the application are where an argument outside a guard would
come from: one whose kernel would quietly take such a value checks it (see
bhw/plp/song's SPLIT-LINES and DECK-NEXT), and bhw/plp/chords makes the notes
it passes TRANSPOSE-NOTE from types that are NOTE-P by construction.  Others
take what Common Lisp itself rejects outside the guard (< of NIL, CAR of a
number), an error rather than a wrong answer."))

(in-package #:bhw/acl2)

;; The guards and measures, which are ACL2's business.
(declaim (declaration xargs))

(defmacro local (&rest forms)
  "ACL2's LOCAL: what a book needs for its proofs only, lemmas that admit a
definition or verify a guard.  SBCL skips it."
  (declare (ignore forms))
  nil)

(defmacro set-verify-guards-eagerness (level)
  "ACL2's setting, which each book makes 2 so that every function in it has
its guard verified.  SBCL skips it."
  (declare (ignore level))
  nil)
