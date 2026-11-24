; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
; SPDX-License-Identifier: AGPL-3.0-or-later

; Proofs about src/acl2/windows.lisp, which bhw/slack-bot/van-booking's
; REQUEST-WINDOW, WINDOWS-OVERLAP-P and TRIP-BLOCKS-WINDOW-P run on the times
; SHEET-CLOCK reads off the sheet (minutes since midnight, NIL unreadable).
;
; Proved:
;   1. a window that REQUEST-WINDOW returns is a WINDOW-P, of integers that
;      run forwards;
;   2. overlap is symmetric;
;   3. two windows that touch at an endpoint do not overlap (half-open);
;   4. for non-empty windows, overlap holds iff one start lies inside the
;      other: the test is exactly "share a minute";
;   5. a booked trip whose times cannot be read blocks; a blank trip never
;      does; a booked, readable, disjoint one does not.

(in-package "BHW/ACL2")

(include-book "../src/acl2/windows")

(defthm request-window-forwards
  (implies (request-window start end)
           (and (window-p (request-window start end))
                (integerp start) (integerp end) (< start end)
                (equal (request-window start end) (list start end)))))

(defthm overlap-symmetric
  (equal (windows-overlap-p sa ea sb eb)
         (windows-overlap-p sb eb sa ea)))

(defthm touching-do-not-overlap
  (implies (and (rationalp sa) (rationalp ea) (rationalp eb))
           (not (windows-overlap-p sa ea ea eb))))

(defthm overlap-iff-start-inside
  (implies (and (rationalp sa) (rationalp ea) (rationalp sb) (rationalp eb)
                (< sa ea) (< sb eb))
           (iff (windows-overlap-p sa ea sb eb)
                (or (and (<= sb sa) (< sa eb))
                    (and (<= sa sb) (< sb ea))))))

(defthm unreadable-booked-trip-blocks
  (implies booked
           (trip-blocks-window-p booked nil start end)))

(defthm blank-trip-does-not-block
  (not (trip-blocks-window-p nil window start end)))

(defthm disjoint-trip-does-not-block
  (implies (and (consp window)
                (not (windows-overlap-p start end
                                        (first window)
                                        (second window))))
           (not (trip-blocks-window-p booked window start end))))
