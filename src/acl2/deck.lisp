;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

;;;; Where a slide next stands in a song's deck (bhw/plp/song's DECK-NEXT).
;;;; An ACL2 book and SBCL's source alike: see package.lisp.
;;;; formal/deck.lisp proves what it does.

(in-package "BHW/ACL2")

(set-verify-guards-eagerness 2)

(defun index-from (x l k)
  "The least index from K on at which L holds X (EQUAL), or NIL."
  (declare (xargs :guard (and (true-listp l) (natp k))
                  :measure (len l)))
  (cond ((endp l) nil)
        ((and (or (not (integerp k)) (<= k 0))
              (equal (car l) x))
         0)
        (t (let ((r (index-from x (cdr l)
                                (if (and (integerp k) (< 0 k)) (- k 1) 0))))
             (and r (+ 1 r))))))

(defun deck-next (deck index x)
  "The index in DECK, a list of slides' identities, where X next stands after
INDEX (from the start when INDEX is NIL), round again from the start; NIL
when DECK does not hold X."
  (declare (xargs :guard (and (true-listp deck)
                              (or (null index) (natp index)))))
  (or (index-from x deck (if index (+ 1 index) 0))
      (index-from x deck 0)))
