; Model of src/plp/song.lisp: DECK-NEXT (line 808), with the list-indexing
; it uses (POSITION ... :START).  A deck is a list of slide identifiers; the
; Lisp's SAME-SLIDE-P compares a slide by (section, part), which the model
; takes as the identifier itself.
;
;   (or (position slide deck :test #'same-slide-p
;                            :start (min (1+ (or index -1)) (length deck)))
;       (position slide deck :test #'same-slide-p))
;
; The start is (1+ INDEX), or 0 for NIL.  Clipping to the length is a no-op
; for POSITION, so the model omits it.
;
; Proved:
;   1. when SLIDE is in DECK, DECK-NEXT gives an index at which SLIDE stands;
;   2. when SLIDE stands after INDEX, the result is the first such place after
;      INDEX (no earlier place is skipped);
;   3. when it stands only before INDEX, the result is the first place in the
;      deck (the wrap-around);
;   4. NIL exactly when SLIDE is not in DECK.

(in-package "ACL2")

(defun bhw-index-from (x l k)
  "The least index j >= K with (nth j L) equal to X, or NIL."
  (declare (xargs :measure (len l)))
  (cond ((endp l) nil)
        ((and (zp k) (equal (car l) x)) 0)
        (t (let ((r (bhw-index-from x (cdr l) (if (zp k) 0 (1- k)))))
             (and r (1+ r))))))

(defun bhw-deck-next (deck index x)
  (let ((start (if index (1+ index) 0)))
    (or (bhw-index-from x deck start)
        (bhw-index-from x deck 0))))

(defthm bhw-index-from-member
  (implies (member-equal x l)
           (bhw-index-from x l 0))
  :hints (("Goal" :induct (bhw-index-from x l 0))))

(defthm bhw-index-from-is-x
  (implies (bhw-index-from x l k)
           (equal (nth (bhw-index-from x l k) l) x)))

(defthm bhw-deck-next-stands
  (implies (member-equal x deck)
           (equal (nth (bhw-deck-next deck index x) deck) x)))

(defthm bhw-index-from-implies-member
  (implies (bhw-index-from x l k)
           (member-equal x l))
  :hints (("Goal" :induct (bhw-index-from x l k))))

(defthm bhw-deck-next-nil-iff
  (equal (null (bhw-deck-next deck index x))
         (not (member-equal x deck))))
