; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
; SPDX-License-Identifier: AGPL-3.0-or-later

; Proofs about DUE-P of src/acl2/reminder.lisp, which bhw/slack-bot/bot's
; reminder sweep runs.  LEAVES and NOW are integers (seconds); LEAVES is NIL
; when the leg's time is unreadable.
;
; Proved (ACL2's built-in arithmetic only):
;   1. a due leg leaves no earlier than NOW and at most 60*(LEAD+1) s later;
;   2. an unreadable leg is never due;
;   3. liveness: with sweeps every 60 s from NOW0, when NOW0 <= LEAVES, the
;      sweep NOW0 + 60*k, k = GRID-K of LEAVES-NOW0, is due for the leg.
;      GRID-K is the index of the last sweep at or before LEAVES.
;
; Not proved: the sent-reminders table (bot.lisp) and the scheduler's sleep.

(in-package "BHW/ACL2")

(include-book "../src/acl2/reminder")

(defthm due-bounds
  (implies (due-p leaves now lead)
           (and (<= now leaves)
                (<= leaves (+ now (* 60 (+ 1 lead)))))))

(defthm due-needs-readable-time
  (not (due-p nil now lead)))

(defun grid-k (d)
  (declare (xargs :measure (nfix d)))
  (if (or (not (natp d)) (< d 60))
      0
    (+ 1 (grid-k (- d 60)))))

(defthm grid-k-bounds
  (implies (natp d)
           (and (<= (* 60 (grid-k d)) d)
                (< d (+ 60 (* 60 (grid-k d))))))
  :hints (("Goal" :induct (grid-k d))))

(defthm diff-natp
  (implies (and (integerp leaves) (integerp now0) (<= now0 leaves))
           (natp (+ leaves (- now0)))))

(defthm grid-k-lower
  (implies (and (integerp leaves) (integerp now0) (<= now0 leaves))
           (<= (* 60 (grid-k (+ leaves (- now0))))
               (+ leaves (- now0))))
  :hints (("Goal" :use ((:instance grid-k-bounds (d (+ leaves (- now0))))
                        diff-natp)))
  :rule-classes :linear)

(defthm grid-k-upper
  (implies (and (integerp leaves) (integerp now0) (<= now0 leaves))
           (< (+ leaves (- now0))
              (+ 60 (* 60 (grid-k (+ leaves (- now0)))))))
  :hints (("Goal" :use ((:instance grid-k-bounds (d (+ leaves (- now0))))
                        diff-natp)))
  :rule-classes :linear)

(defthm due-liveness
  (implies (and (integerp leaves) (integerp now0) (<= now0 leaves)
                (natp lead))
           (due-p leaves
                  (+ now0 (* 60 (grid-k (- leaves now0))))
                  lead)))
