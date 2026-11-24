; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
; SPDX-License-Identifier: AGPL-3.0-or-later

; Proofs about SPLIT-LINES of src/acl2/slides.lisp, which bhw/plp/song's
; SPLIT-LINES runs.
;
; Proved, for every true list LINES and every positive MAX-LINES:
;   1. the slides, appended, are the lines;
;   2. every slide has at most MAX-LINES lines;
;   3. every slide is non-empty.
; Built-in arithmetic only (no books).

(in-package "BHW/ACL2")

(include-book "../src/acl2/slides")

(defun append-all (ls)
  (if (endp ls)
      nil
    (append (car ls) (append-all (cdr ls)))))

(defun all-len-<= (ls n)
  (if (endp ls)
      t
    (and (<= (len (car ls)) n)
         (all-len-<= (cdr ls) n))))

(defun all-non-empty (ls)
  (if (endp ls)
      t
    (and (consp (car ls))
         (all-non-empty (cdr ls)))))

;;; Take, drop and split

(defthm lines-take-drop
  (equal (append (lines-take n l) (lines-drop n l))
         l))

(defthm lines-take-len
  (implies (natp n)
           (<= (len (lines-take n l)) n)))

(defthm lines-take-non-empty
  (implies (and (posp n) (consp l))
           (consp (lines-take n l)))
  :hints (("Goal" :expand ((lines-take n l)))))

(defthm true-listp-of-lines-drop
  (implies (true-listp l)
           (true-listp (lines-drop n l))))

(defthm split-by-append
  (implies (and (posp n) (true-listp l))
           (equal (append-all (split-by l n))
                  l)))

(defthm split-by-bounded
  (implies (posp n)
           (all-len-<= (split-by l n) n)))

(defthm split-by-non-empty
  (implies (posp n)
           (all-non-empty (split-by l n))))

;;; Ceiling division

(defthm cdiv-bounds-below
  (implies (and (natp a) (posp b))
           (<= a (* (cdiv a b) b)))
  :hints (("Goal" :induct (cdiv a b))))

(defun cdiv-ind (a m b)
  (declare (xargs :measure (nfix a)))
  (if (and (posp a) (posp m) (posp b))
      (cdiv-ind (- a b) (- m 1) b)
    nil))

(defthm cdiv-least
  (implies (and (natp m) (natp a) (posp b) (<= a (* m b)))
           (<= (cdiv a b) m))
  :hints (("Goal" :induct (cdiv-ind a m b))))

(defthm cdiv-posp
  (implies (and (posp a) (posp b))
           (posp (cdiv a b))))

;;; The slide size

(defun slide-parts (count max)
  (max 1 (cdiv count max)))

(defun slide-size (count max)
  (cdiv count (slide-parts count max)))

(defthm split-lines-is-split-by
  (equal (split-lines lines max)
         (split-by lines (slide-size (len lines) max))))

(in-theory (disable split-lines slide-parts slide-size))

(defthm slide-parts-posp
  (posp (slide-parts count max))
  :hints (("Goal" :in-theory (enable slide-parts))))

(defthm count-fits-parts
  (implies (and (natp count) (posp max))
           (<= count (* max (slide-parts count max))))
  :hints (("Goal" :in-theory (enable slide-parts)
                  :use ((:instance cdiv-bounds-below (a count) (b max))))))

(defthm size-bounded
  (implies (and (natp count) (posp max))
           (<= (slide-size count max) max))
  :hints (("Goal" :in-theory (enable slide-size)
                  :use (slide-parts-posp
                        count-fits-parts
                        (:instance cdiv-least
                                   (m max) (a count)
                                   (b (slide-parts count max)))))))

(defthm size-posp
  (implies (and (posp count) (posp max))
           (posp (slide-size count max)))
  :hints (("Goal" :in-theory (enable slide-size)
                  :use (slide-parts-posp
                        (:instance cdiv-posp
                                   (a count)
                                   (b (slide-parts count max)))))))

;;; The theorems about SPLIT-LINES

(defthm all-len-<=-monotone
  (implies (and (all-len-<= ls n) (<= n m))
           (all-len-<= ls m)))

(defthm split-lines-append
  (implies (and (posp max-lines) (true-listp lines))
           (equal (append-all (split-lines lines max-lines))
                  lines))
  :hints (("Goal" :cases ((consp lines))
                  :use ((:instance size-posp (count (len lines)) (max max-lines))))))

(defthm split-lines-bounded
  (implies (posp max-lines)
           (all-len-<= (split-lines lines max-lines) max-lines))
  :hints (("Goal" :cases ((consp lines))
                  :use ((:instance split-by-bounded
                                   (l lines)
                                   (n (slide-size (len lines) max-lines)))
                        (:instance size-posp (count (len lines)) (max max-lines))
                        (:instance size-bounded (count (len lines)) (max max-lines))
                        (:instance all-len-<=-monotone
                                   (ls (split-by lines
                                                 (slide-size (len lines) max-lines)))
                                   (n (slide-size (len lines) max-lines))
                                   (m max-lines))))))

(defthm split-lines-non-empty
  (implies (posp max-lines)
           (all-non-empty (split-lines lines max-lines)))
  :hints (("Goal" :cases ((consp lines))
                  :use ((:instance split-by-non-empty
                                   (l lines)
                                   (n (slide-size (len lines) max-lines)))
                        (:instance size-posp (count (len lines)) (max max-lines))))))
