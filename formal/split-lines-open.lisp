; OPEN: ceiling-based obligations for split-lines.lisp, NOT certified.
; Needs arithmetic lemmas about CEILING (floor-based) that arithmetic-5 does not
; supply directly; see notes in formal/README.org.
(in-package "ACL2")
(include-book "split-lines")

;;; The arithmetic of the slide size

(defthm bhw-slide-parts-posp
  (posp (bhw-slide-parts count max)))

(defthm bhw-size-posp-when-lines
  (implies (and (natp count) (posp count) (posp max))
           (posp (bhw-slide-size count max))))

(defthm bhw-size-zero-when-empty
  (equal (bhw-slide-size 0 max) 0))

(defthm bhw-ceiling-le
  (implies (and (integerp a) (<= 0 a) (posp b) (posp m) (<= a (* m b)))
           (<= (ceiling a b) m))
  :hints (("Goal" :in-theory (enable ceiling-to-floor)
                  :use ((:instance floor-bound-lemma)))))

(defthm bhw-count-fits-parts
  (implies (and (natp count) (posp max))
           (<= count (* max (bhw-slide-parts count max))))
  :hints (("Goal" :in-theory (enable bhw-slide-parts))))

(defthm bhw-size-bounded
  (implies (and (natp count) (posp max))
           (<= (bhw-slide-size count max) max))
  :hints (("Goal" :use ((:instance bhw-ceiling-le
                                   (a count)
                                   (b (bhw-slide-parts count max))
                                   (m max))
                        bhw-count-fits-parts)
                  :in-theory (enable bhw-slide-size))))

;;; The theorems about SPLIT-LINES

(defthm bhw-split-lines-append
  (implies (and (natp count) (posp max) (true-listp lines)
                (equal count (len lines)))
           (equal (bhw-append-all (bhw-split-lines lines max))
                  lines)))

(defthm bhw-split-lines-bounded
  (implies (posp max)
           (bhw-all-len-<= (bhw-split-lines lines max) max)))

(defthm bhw-split-lines-non-empty
  (implies (posp max)
           (bhw-all-non-empty (bhw-split-lines lines max))))
