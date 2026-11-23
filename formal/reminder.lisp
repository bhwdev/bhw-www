; Model of src/slack-bot/bot.lisp: DUE-P (line 1137).
;
;   (defun due-p (leaves now lead-minutes)
;     (and leaves (<= 0 (- leaves now) (* 60 (1+ lead-minutes)))))
;
; LEAVES and NOW are integers (seconds); LEAVES is NIL when the leg's time is
; unreadable (modelled by NIL, never due).
;
; Proved (ACL2's built-in arithmetic only):
;   1. a due leg leaves no earlier than NOW and at most 60*(LEAD+1) s later;
;   2. an unreadable leg is never due;
;   3. liveness: with sweeps every 60 s from NOW0, when NOW0 <= LEAVES, the
;      sweep NOW0 + 60*k, k = BHW-GRID-K of LEAVES-NOW0, is due for the leg.
;      BHW-GRID-K is the index of the last sweep at or before LEAVES.
;
; Not modelled: the sent-reminders table (bot.lisp) and the scheduler's sleep.

(in-package "ACL2")

(defun bhw-due-p (leaves now lead-minutes)
  (and leaves
       (<= 0 (- leaves now))
       (<= (- leaves now) (* 60 (+ 1 lead-minutes)))))

(defthm bhw-due-bounds
  (implies (bhw-due-p leaves now lead)
           (and (<= now leaves)
                (<= leaves (+ now (* 60 (+ 1 lead)))))))

(defthm bhw-due-needs-readable-time
  (not (bhw-due-p nil now lead)))

(defun bhw-grid-k (d)
  (declare (xargs :measure (nfix d)))
  (if (or (not (natp d)) (< d 60))
      0
    (+ 1 (bhw-grid-k (- d 60)))))

(defthm bhw-grid-k-bounds
  (implies (natp d)
           (and (<= (* 60 (bhw-grid-k d)) d)
                (< d (+ 60 (* 60 (bhw-grid-k d))))))
  :hints (("Goal" :induct (bhw-grid-k d))))

(defthm bhw-diff-natp
  (implies (and (integerp leaves) (integerp now0) (<= now0 leaves))
           (natp (+ leaves (- now0)))))

(defthm bhw-grid-k-lower
  (implies (and (integerp leaves) (integerp now0) (<= now0 leaves))
           (<= (* 60 (bhw-grid-k (+ leaves (- now0))))
               (+ leaves (- now0))))
  :hints (("Goal" :use ((:instance bhw-grid-k-bounds
                                   (d (+ leaves (- now0))))
                        bhw-diff-natp)))
  :rule-classes :linear)

(defthm bhw-grid-k-upper
  (implies (and (integerp leaves) (integerp now0) (<= now0 leaves))
           (< (+ leaves (- now0))
              (+ 60 (* 60 (bhw-grid-k (+ leaves (- now0)))))))
  :hints (("Goal" :use ((:instance bhw-grid-k-bounds
                                   (d (+ leaves (- now0))))
                        bhw-diff-natp)))
  :rule-classes :linear)

(defthm bhw-due-liveness
  (implies (and (integerp leaves) (integerp now0) (<= now0 leaves)
                (natp lead))
           (bhw-due-p leaves
                      (+ now0 (* 60 (bhw-grid-k (- leaves now0))))
                      lead))
  :hints (("Goal" :in-theory (enable bhw-due-p))))
