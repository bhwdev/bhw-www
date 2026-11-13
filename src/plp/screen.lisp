;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:bhw/plp/screen
  (:use #:common-lisp)
  (:import-from #:cl-ppcre)
  (:import-from #:clog
                #:create-child #:create-div #:css-class-name #:head-element
                #:html-document #:inner-html #:js-query #:location #:path-name
                #:set-on-click #:set-on-key-down #:style #:title #:validp)
  (:import-from #:bhw/site/layout #:escape-html)
  (:local-nicknames (#:song #:bhw/plp/song)
                    (#:live #:bhw/plp/live))
  (:export #:path-code
           #:setup-page
           #:timezone-offset
           #:font-size
           #:slide-html
           #:footer-credit
           #:footer-clock
           #:image-url
           #:handle-key
           #:on-screen)
  (:documentation
   "The projector's page, /plp-screen/CODE: the slide of the room CODE and
nothing else, white on black, to be put full screen (F11) on the display the
congregation sees.  Lower left the time and what is left of the session,
lower right the song and its writers; the copyright line under a song's
first slide.

The page also takes the keys (BHW/PLP/LIVE:PRESS-KEY) and a tap on its left
or right side, so that where the display can only mirror the operator's, it
is driven without the console.

The size of the type is worked out here from the slide, the page running no
script that could measure it: FONT-SIZE.  The one thing asked of the browser
is its clock's offset from UTC (TIMEZONE-OFFSET), without which the server
cannot tell the time where the screen hangs."))

(in-package #:bhw/plp/screen)

(defun path-code (path prefix)
  "The room code in PATH, what follows PREFIX in capitals, or NIL."
  (when (and (stringp path) (uiop:string-prefix-p prefix path))
    (let ((code (string-upcase (string-right-trim "/" (subseq path (length prefix))))))
      (and (live:valid-code-p code) code))))

(defun setup-page (body page-title)
  "Give BODY's document the presenter's stylesheet and PAGE-TITLE, and keep
search engines from indexing it."
  (let ((document (html-document body)))
    (create-child (head-element document)
                  "<link rel='stylesheet' href='/assets/plp.css' type='text/css'>")
    (create-child (head-element document)
                  "<meta name='robots' content='noindex, nofollow'>")
    (create-child (head-element document)
                  "<meta name='viewport' content='width=device-width, initial-scale=1'>")
    (setf (title document) page-title)))

(defun timezone-offset (body)
  "How many minutes the clock of BODY's browser is behind UTC; 0 when it
does not say."
  (or (ignore-errors
       (parse-integer (js-query body "new Date().getTimezoneOffset()")
                      :junk-allowed t))
      0))

(defun font-size (lines)
  "The CSS font-size at which LINES fill the screen: the smaller of what
its longest line allows across and what the number of lines allows down.
A letter is taken to be six tenths as wide as it is tall (bold Verdana,
the widest of the stylesheet's faces, averages a little under that), and a
line to take a height and a quarter; the slide is left nine tenths of the
width and seven tenths of the height."
  (let ((characters (max 14 (reduce #'max lines :key #'length :initial-value 0)))
        (count (max 2 (length lines))))
    (format nil "min(~,2Fvw, ~,2Fvh)" (/ 150 characters) (/ 56 count))))

(defun slide-html (slide)
  "The lines of SLIDE as HTML, one div each."
  (format nil "~{<div class='plp-line'>~A</div>~}"
          (mapcar #'escape-html (song:slide-lines slide))))

(defun footer-credit (song)
  "What the lower right corner says of SONG: its title and its writers."
  (if song
      (format nil "~A~@[ · ~A~]" (song:song-title song) (song:song-writers song))
      ""))

(defun footer-clock (now end-time offset)
  "What the lower left corner says at NOW: the time, and what is left until
END-TIME if that is to come."
  (format nil "~A~@[ · ~A~]" (live:clock-text now offset)
          (live:countdown-text now end-time)))

(defun image-url (name)
  "The address of the waiting image NAME."
  (format nil "/plp-images/~A" name))

(defun handle-key (room event)
  "Do what the keyboard EVENT asks of ROOM; a key held with Ctrl, Alt or
Meta is the browser's.  Returns true when the key was one of the map's."
  (unless (or (getf event :ctrl-key) (getf event :alt-key) (getf event :meta-key))
    (live:press-key room (or (getf event :key) "") :shift (getf event :shift-key))))

(defun on-screen (body)
  "The projector's page of the room in the address."
  (let ((room (live:ensure-room (path-code (path-name (location body)) "/plp-screen/"))))
    (setup-page body "Lyrics")
    (if (null room)
        (create-div body :class "plp-notice"
                         :content "No such screen: open one from <a href='/plp'>/plp</a>.")
        (let* ((stage (create-div body :class "plp-stage"))
               (waiting (create-div stage :class "plp-waiting"))
               (waiting-text (create-div waiting :class "plp-waiting-text"))
               (image-credit (create-div waiting :class "plp-image-credit"))
               (lines (create-div stage :class "plp-lines"))
               (copyright (create-div stage :class "plp-copyright"))
               (left (create-div stage :class "plp-footer-left"))
               (right (create-div stage :class "plp-footer-right"))
               (back (create-div stage :class "plp-tap plp-tap-back"))
               (forward (create-div stage :class "plp-tap plp-tap-forward"))
               (offset (timezone-offset body))
               (clock nil))
          (labels ((draw-clock ()
                     (let ((text (footer-clock (get-universal-time)
                                               (live:room-end-time room) offset)))
                       (unless (equal text clock)
                         (setf clock text
                               (inner-html left) (escape-html text)))))
                   (draw (what)
                     (declare (ignore what))
                     (let* ((state (live:snapshot room))
                            (slide (getf state :slide))
                            (song (getf state :song))
                            (mode (getf state :mode))
                            (image (getf state :waiting-image)))
                       (setf (css-class-name stage)
                             (format nil "plp-stage~:[~; plp-black~]~:[~; plp-is-waiting~]~:[ plp-no-footer~;~]"
                                     (eq mode :black) (eq mode :waiting)
                                     (getf state :footer-p)))
                       (setf (inner-html lines) (if slide (slide-html slide) ""))
                       (when slide
                         (setf (style lines "font-size") (font-size (song:slide-lines slide))))
                       (setf (inner-html right) (escape-html (footer-credit song)))
                       (setf (inner-html copyright)
                             (if (and song (zerop (getf state :index)) (song:song-copyright song))
                                 (escape-html (song:song-copyright song))
                                 ""))
                       (when (eq mode :waiting)
                         (setf (style waiting "background-image")
                               (if image (format nil "url('~A')" (image-url image)) "none"))
                         (setf (inner-html waiting-text)
                               (escape-html (getf state :waiting-text)))
                         (setf (inner-html image-credit)
                               (escape-html
                                (or (cdr (assoc image (live:waiting-images) :test #'equal))
                                    ""))))
                       (draw-clock))))
            (draw :song)
            (let ((subscription (live:subscribe room #'draw)))
              (set-on-key-down body (lambda (object event)
                                      (declare (ignore object))
                                      (handle-key room event)))
              (set-on-click back (lambda (object)
                                   (declare (ignore object))
                                   (live:step-slide room -1)))
              (set-on-click forward (lambda (object)
                                      (declare (ignore object))
                                      (live:step-slide room 1)))
              ;; The clock: this thread is the page's own, and ends with it.
              (unwind-protect
                   (loop while (validp body)
                         do (draw-clock)
                            (sleep 5))
                (live:unsubscribe room subscription))))))))
