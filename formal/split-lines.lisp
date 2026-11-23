; Model of src/plp/song.lisp: SPLIT-LINES (line 705) and its helper loop.
;
; The Lisp:
;   (let* ((count (length lines))
;          (parts (max 1 (ceiling count max-lines)))
;          (size (ceiling count parts)))
;     (loop for rest = lines then (nthcdr size rest)
;           while rest
;           collect (subseq rest 0 (min size (length rest)))))
;
; CEILING is modelled by BHW-CDIV, defined by repeated subtraction, for a
; natural count and a positive divisor (the only uses here).
;
; Proved, for every true list LINES and every positive MAX-LINES:
;   1. the slides, appended, are the lines;
;   2. every slide has at most MAX-LINES lines;
;   3. every slide is non-empty.
; Uses only ACL2's built-in arithmetic (no books).

(in-package "ACL2")

(defun bhw-take (n l)
  (if (or (zp n) (endp l))
      nil
    (cons (car l) (bhw-take (1- n) (cdr l)))))

(defun bhw-drop (n l)
  (if (or (zp n) (endp l))
      l
    (bhw-drop (1- n) (cdr l))))

(defthm len-of-bhw-drop
  (equal (len (bhw-drop n l))
         (nfix (- (len l) (nfix n)))))

(defthm bhw-drop-shorter
  (implies (and (posp n) (consp l))
           (< (len (bhw-drop n l)) (len l))))

(defun bhw-split-by (l n)
  (declare (xargs :measure (len l)))
  (if (or (endp l) (zp n))
      nil
    (cons (bhw-take n l)
          (bhw-split-by (bhw-drop n l) n))))

(defun bhw-cdiv (a b)
  (declare (xargs :measure (nfix a)))
  (if (and (posp a) (posp b))
      (+ 1 (bhw-cdiv (- a b) b))
    0))

(defun bhw-slide-parts (count max)
  (max 1 (bhw-cdiv count max)))

(defun bhw-slide-size (count max)
  (bhw-cdiv count (bhw-slide-parts count max)))

(defun bhw-split-lines (lines max)
  (bhw-split-by lines (bhw-slide-size (len lines) max)))

(defun bhw-append-all (ls)
  (if (endp ls)
      nil
    (append (car ls) (bhw-append-all (cdr ls)))))

(defun bhw-all-len-<= (ls n)
  (if (endp ls)
      t
    (and (<= (len (car ls)) n)
         (bhw-all-len-<= (cdr ls) n))))

(defun bhw-all-non-empty (ls)
  (if (endp ls)
      t
    (and (consp (car ls))
         (bhw-all-non-empty (cdr ls)))))

;;; Take, drop and split

(defthm bhw-take-drop
  (equal (append (bhw-take n l) (bhw-drop n l))
         l))

(defthm bhw-take-len
  (implies (natp n)
           (<= (len (bhw-take n l)) n)))

(defthm bhw-take-nonempty
  (implies (and (posp n) (consp l))
           (consp (bhw-take n l)))
  :hints (("Goal" :induct (bhw-take n l)
                  :in-theory (enable bhw-take))))

(defthm true-listp-of-bhw-drop
  (implies (true-listp l)
           (true-listp (bhw-drop n l))))

(defthm bhw-split-by-append
  (implies (and (posp n) (true-listp l))
           (equal (bhw-append-all (bhw-split-by l n))
                  l)))

(defthm bhw-split-by-bounded
  (implies (posp n)
           (bhw-all-len-<= (bhw-split-by l n) n)))

(defthm bhw-split-by-non-empty
  (implies (posp n)
           (bhw-all-non-empty (bhw-split-by l n))))

;;; Ceiling division

(defthm bhw-cdiv-bounds-below
  (implies (and (natp a) (posp b))
           (<= a (* (bhw-cdiv a b) b)))
  :hints (("Goal" :induct (bhw-cdiv a b))))

(defun bhw-cdiv-ind (a m b)
  (declare (xargs :measure (nfix a)))
  (if (and (posp a) (posp m) (posp b))
      (bhw-cdiv-ind (- a b) (- m 1) b)
    nil))

(defthm bhw-cdiv-least
  (implies (and (natp m) (natp a) (posp b) (<= a (* m b)))
           (<= (bhw-cdiv a b) m))
  :hints (("Goal" :induct (bhw-cdiv-ind a m b)
                  :in-theory (enable bhw-cdiv))))

(defthm bhw-cdiv-posp
  (implies (and (posp a) (posp b))
           (posp (bhw-cdiv a b)))
  :hints (("Goal" :in-theory (enable bhw-cdiv))))

(defthm bhw-cdiv-zero
  (equal (bhw-cdiv 0 b) 0)
  :hints (("Goal" :in-theory (enable bhw-cdiv))))

;;; The slide size

(defthm bhw-slide-parts-posp
  (posp (bhw-slide-parts count max))
  :hints (("Goal" :in-theory (enable bhw-slide-parts))))

(defthm bhw-count-fits-parts
  (implies (and (natp count) (posp max))
           (<= count (* max (bhw-slide-parts count max))))
  :hints (("Goal" :in-theory (enable bhw-slide-parts)
                  :use ((:instance bhw-cdiv-bounds-below (a count) (b max))))))

(defthm bhw-size-bounded
  (implies (and (natp count) (posp max))
           (<= (bhw-slide-size count max) max))
  :hints (("Goal" :in-theory (enable bhw-slide-size)
                  :use (bhw-slide-parts-posp bhw-count-fits-parts
                        (:instance bhw-cdiv-least
                                   (m max) (a count)
                                   (b (bhw-slide-parts count max)))))))

(defthm bhw-size-posp
  (implies (and (posp count) (posp max))
           (posp (bhw-slide-size count max)))
  :hints (("Goal" :in-theory (enable bhw-slide-size)
                  :use (bhw-slide-parts-posp
                        (:instance bhw-cdiv-posp
                                   (a count)
                                   (b (bhw-slide-parts count max)))))))

;;; The theorems about SPLIT-LINES

(defthm bhw-all-len-<=-monotone
  (implies (and (bhw-all-len-<= ls n) (<= n m))
           (bhw-all-len-<= ls m)))

(defthm bhw-len-posp
  (implies (consp l)
           (posp (len l))))

(defthm bhw-split-lines-append
  (implies (and (posp max) (true-listp lines))
           (equal (bhw-append-all (bhw-split-lines lines max))
                  lines))
  :hints (("Goal" :in-theory (enable bhw-split-lines)
                  :cases ((consp lines)))))

(defthm bhw-split-lines-bounded-consp
  (implies (and (posp max) (consp lines))
           (bhw-all-len-<= (bhw-split-lines lines max) max))
  :hints (("Goal" :in-theory (enable bhw-split-lines)
                  :use ((:instance bhw-split-by-bounded
                                   (l lines)
                                   (n (bhw-slide-size (len lines) max)))
                        (:instance bhw-size-posp (count (len lines)))
                        (:instance bhw-size-bounded (count (len lines)))
                        (:instance bhw-all-len-<=-monotone
                                   (ls (bhw-split-by lines
                                                     (bhw-slide-size (len lines) max)))
                                   (n (bhw-slide-size (len lines) max))
                                   (m max))))))

(defthm bhw-split-lines-bounded
  (implies (posp max)
           (bhw-all-len-<= (bhw-split-lines lines max) max))
  :hints (("Goal" :in-theory (enable bhw-split-lines bhw-split-by)
                  :cases ((consp lines))
                  :use (bhw-split-lines-bounded-consp))))

(defthm bhw-split-lines-non-empty
  (implies (and (posp max) (consp lines))
           (bhw-all-non-empty (bhw-split-lines lines max)))
  :hints (("Goal" :in-theory (enable bhw-split-lines)
                  :use ((:instance bhw-split-by-non-empty
                                   (l lines)
                                   (n (bhw-slide-size (len lines) max)))
                        (:instance bhw-size-posp (count (len lines)))
                        bhw-len-posp))))
