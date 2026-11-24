;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

;;;; When the reminder of a trip's leg is due (bhw/slack-bot/bot's DUE-P).  An
;;;; ACL2 book and SBCL's source alike: see package.lisp.
;;;; formal/reminder.lisp proves what it does.

(in-package "BHW/ACL2")

(set-verify-guards-eagerness 2)

(defun due-p (leaves now lead-minutes)
  "True when a leg leaving at LEAVES (a universal time, or NIL when its time
cannot be read) is due, at NOW, for a DM LEAD-MINUTES ahead of it: from the
lead time plus the 60 s between sweeps until it leaves, so the DM goes out
between the lead time and a minute more ahead of it, and a leg booked or a
sweep run later than that still sends it, late."
  (declare (xargs :guard (and (or (null leaves) (integerp leaves))
                              (integerp now)
                              (natp lead-minutes))))
  (and leaves
       (<= 0 (- leaves now))
       (<= (- leaves now) (* 60 (+ 1 lead-minutes)))))
