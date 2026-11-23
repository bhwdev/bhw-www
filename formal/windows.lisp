; Model of src/slack-bot/van-booking.lisp: REQUEST-WINDOW (line 328),
; WINDOWS-OVERLAP-P (line 344) and TRIP-BLOCKS-WINDOW-P (line 350).
; Times are integers (minutes since midnight). SHEET-CLOCK's NIL is NIL here.
;
; Proved:
;   1. a window that REQUEST-WINDOW returns is non-empty and forwards;
;   2. overlap is symmetric;
;   3. two windows that touch at an endpoint do not overlap (half-open);
;   4. for non-empty windows, overlap holds iff one start lies inside the other:
;      the test is exactly "share a minute".
;   5. an unreadable booked trip blocks; a readable, disjoint one does not.

(in-package "ACL2")


(defun bhw-request-window (start end)
  (if (and (integerp start) (integerp end) (< start end))
      (list start end)
    nil))

(defun bhw-windows-overlap-p (start-a end-a start-b end-b)
  (and (< start-a end-b) (< start-b end-a)))

;; TRIP-WINDOW gives NIL when the trip's times cannot be read.
(defun bhw-trip-blocks-window-p (booked trip-window start end)
  (and booked
       (or (null trip-window)
           (bhw-windows-overlap-p start end
                                  (first trip-window) (second trip-window)))))

(defthm bhw-request-window-forwards
  (implies (bhw-request-window start end)
           (and (integerp start) (integerp end) (< start end))))

(defthm bhw-overlap-symmetric
  (equal (bhw-windows-overlap-p sa ea sb eb)
         (bhw-windows-overlap-p sb eb sa ea)))

(defthm bhw-touching-do-not-overlap
  (implies (and (rationalp sa) (rationalp ea) (rationalp eb))
           (not (bhw-windows-overlap-p sa ea ea eb))))

(defthm bhw-overlap-iff-start-inside
  (implies (and (rationalp sa) (rationalp ea) (rationalp sb) (rationalp eb)
                (< sa ea) (< sb eb))
           (iff (bhw-windows-overlap-p sa ea sb eb)
                (or (and (<= sb sa) (< sa eb))
                    (and (<= sa sb) (< sb ea))))))

(defthm bhw-unreadable-booked-trip-blocks
  (implies booked
           (bhw-trip-blocks-window-p booked nil start end)))

(defthm bhw-disjoint-trip-does-not-block
  (implies (and booked
                (consp trip-window)
                (not (bhw-windows-overlap-p start end
                                            (first trip-window)
                                            (second trip-window))))
           (not (bhw-trip-blocks-window-p booked trip-window start end))))
