; Model of src/slack-bot/bot.lisp: DUE-P (line 1137).
;
;   (defun due-p (leaves now lead-minutes)
;     (and leaves (<= 0 (- leaves now) (* 60 (1+ lead-minutes)))))
;
; LEAVES and NOW are integers (seconds, universal time); LEAVES is NIL when the
; leg's time is unreadable (modelled by NIL, never due).
;
; Proved:
;   1. a due leg leaves no earlier than NOW and at most 60*(LEAD+1) s later;
;   2. an unreadable leg is never due;
;   3. liveness: if the sweeps run every 60 s from a sweep NOW0 that is no later
;      than LEAVES, some sweep is due for that leg, at the sweep
;      NOW0 + 60*floor((LEAVES-NOW0)/60).  The leg is never skipped by the grid.
;
; Not modelled: the sent-reminders table (a leg may be sent once per day, see
; today-date-key / maybe-reset-sent-reminders) and the scheduler's sleep. Only
; the arithmetic of the window is.

(in-package "ACL2")

(include-book "arithmetic-5/top" :dir :system)


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

(defthm bhw-due-liveness
  (implies (and (integerp leaves) (integerp now0) (<= now0 leaves)
                (natp lead))
           (bhw-due-p leaves
                      (+ now0 (* 60 (floor (- leaves now0) 60)))
                      lead)))
