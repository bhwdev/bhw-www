;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

;;;; The windows of time trips take, and their clashes (bhw/slack-bot/
;;;; van-booking's REQUEST-WINDOW, WINDOWS-OVERLAP-P and TRIP-BLOCKS-WINDOW-P),
;;;; on minutes since midnight that van-booking has read off the sheet.  An
;;;; ACL2 book and SBCL's source alike: see package.lisp.  formal/windows.lisp
;;;; proves what they do.

(in-package "BHW/ACL2")

(set-verify-guards-eagerness 2)

(defun request-window (start end)
  "(START END), for a trip leaving at START and back at END, or NIL when
either is not a time (NIL: blank or unreadable) or the two do not run
forwards."
  (declare (xargs :guard t))
  (if (and (integerp start) (integerp end) (< start end))
      (list start end)
      nil))

(defun windows-overlap-p (start-a end-a start-b end-b)
  "True when [START-A, END-A) and [START-B, END-B) share a minute.  The
intervals are half-open on purpose: a van due back at 11:00 may be taken out
again at 11:00."
  (declare (xargs :guard (and (rationalp start-a) (rationalp end-a)
                              (rationalp start-b) (rationalp end-b))))
  (and (< start-a end-b) (< start-b end-a)))

(defun window-p (window)
  "True when WINDOW is what REQUEST-WINDOW gives for a trip: (START END), or
NIL for times that cannot be read as a window."
  (declare (xargs :guard t))
  (or (null window)
      (and (consp window) (rationalp (car window))
           (consp (cdr window)) (rationalp (car (cdr window))))))

(defun trip-blocks-window-p (booked window start end)
  "True when a trip, BOOKED or blank, whose times read as WINDOW stands in
the way of a trip running START to END on the same day.  A booked trip whose
own times cannot be read (WINDOW NIL) blocks unconditionally: nothing can
show it is out of the way, and writing over a van that may already be on the
road is the worse mistake."
  (declare (xargs :guard (and (window-p window) (rationalp start) (rationalp end))))
  (and booked
       (or (null window)
           (windows-overlap-p start end (first window) (second window)))))
