;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

;;;; How a block of a song's lines is laid out on slides (bhw/plp/song's
;;;; SPLIT-LINES).  An ACL2 book and SBCL's source alike: see package.lisp.
;;;; formal/slides.lisp proves what it does.

(in-package "BHW/ACL2")

(set-verify-guards-eagerness 2)

(defun lines-take (n lines)
  "The first N of LINES, or all of them when there are fewer."
  (declare (xargs :guard (and (natp n) (true-listp lines))))
  (if (or (not (integerp n)) (<= n 0) (endp lines))
      nil
      (cons (car lines) (lines-take (- n 1) (cdr lines)))))

(defun lines-drop (n lines)
  "LINES without the first N of them."
  (declare (xargs :guard (and (natp n) (true-listp lines))))
  (if (or (not (integerp n)) (<= n 0) (endp lines))
      lines
      (lines-drop (- n 1) (cdr lines))))

(local
 (defthm lines-drop-shorter
   (implies (and (integerp n) (< 0 n) (consp lines))
            (< (len (lines-drop n lines)) (len lines)))))

(local
 (defthm true-listp-of-lines-drop
   (implies (true-listp lines)
            (true-listp (lines-drop n lines)))))

(defun split-by (lines size)
  "LINES in runs of SIZE, the last of what is left."
  (declare (xargs :guard (and (true-listp lines) (natp size))
                  :measure (len lines)))
  (if (or (endp lines) (not (integerp size)) (<= size 0))
      nil
      (cons (lines-take size lines)
            (split-by (lines-drop size lines) size))))

(defun cdiv (a b)
  "A divided by B and rounded up (CEILING), for a natural A and a positive B;
0 for a negative A."
  (declare (xargs :guard (and (integerp a) (posp b))
                  :measure (nfix a)))
  (if (and (integerp a) (integerp b) (< 0 a) (< 0 b))
      (+ 1 (cdiv (- a b) b))
      0))

(defun split-lines (lines max-lines)
  "LINES as a list of slides of at most MAX-LINES lines, as even as can be:
as few slides as hold them, each the size that spreads LINES over that many,
the last what is left."
  (declare (xargs :guard (and (true-listp lines) (posp max-lines))))
  (let* ((count (length lines))
         (parts (max 1 (cdiv count max-lines)))
         (size (cdiv count parts)))
    (split-by lines size)))
