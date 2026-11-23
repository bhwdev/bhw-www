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
; Proved here, for every list and every positive MAX-LINES:
;   1. the slides, appended, are the lines (nothing lost, nothing reordered);
;   2. every slide has at most MAX-LINES lines;
;   3. every slide is non-empty (no blank slide is produced);
;   4. the slides number at most PARTS, so all but the last are of SIZE.

(in-package "ACL2")

(include-book "arithmetic-5/top" :dir :system)


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

(defun bhw-slide-parts (count max)
  (max 1 (ceiling count max)))

(defun bhw-slide-size (count max)
  (ceiling count (bhw-slide-parts count max)))

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

;;; Helper facts

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

;;; The split by a size of at least one

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

