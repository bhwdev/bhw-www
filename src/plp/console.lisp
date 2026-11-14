;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:bhw/plp/console
  (:use #:common-lisp)
  (:import-from #:bordeaux-threads)
  (:import-from #:clog
                #:add-class #:create-a #:create-button #:create-div
                #:create-form-element #:create-option #:create-section
                #:create-select #:create-span #:create-text-area #:css-class-name
                #:focus #:inner-html #:location #:path-name #:place-holder
                #:remove-class #:scroll-into-view #:set-on-change #:set-on-click
                #:set-on-input #:set-on-key-down #:set-on-new-window #:style
                #:tab-index #:text-value #:url #:validp #:value)
  (:import-from #:bhw/site/layout #:escape-html)
  (:import-from #:bhw/plp/screen
                #:handle-key #:on-screen #:path-code #:setup-page #:slide-html
                #:timezone-offset)
  (:local-nicknames (#:song #:bhw/plp/song)
                    (#:db #:bhw/plp/database)
                    (#:live #:bhw/plp/live))
  (:export #:*terms*
           #:*card-columns*
           #:row-step
           #:card-html
           #:on-front
           #:on-console
           #:register-routes)
  (:documentation
   "The operator's pages.

/plp states the terms the lyrics are shown under and, on agreement, opens a
console on a new room.  /plp-console/CODE is the console of the room CODE:
on the left the setlist and the search, in the middle every slide of the
song as a card in the order it is sung, on the right what the screen shows
and what comes next, and the text of a song to add or correct.

The keys (BHW/PLP/LIVE:PRESS-KEY) act while the middle pane has the focus,
so that typing in a field types; / goes to the search, Escape comes back,
and every button gives the focus back to the pane.  The cards stand in rows
of *CARD-COLUMNS*, a number the server sets, so that it knows which card is
above and below the one showing: the up and down arrows go there.  In the
search they move the mark among the songs found, and Enter takes the marked
one.

New opens a form over the console for a song's credits and its words as
pasted from anywhere; under them stands what BHW/PLP/SONG:OUTLINE makes of
their structure, each part with the kind it is taken for, to be corrected.
Done writes the song into the text field and closes the form.

Anyone can present.  Saving a setlist or a song's text, and deleting one,
asks for the passphrase (BHW/PLP/DATABASE:AUTHORIZED-P)."))

(in-package #:bhw/plp/console)

(defparameter *terms*
  "<p>The words of these songs belong to their authors and publishers, who
are named with each song. They are shown here by permission, for the prayer
and worship of the Companions of the Cross and of those who pray with
them.</p>
<p>By going on you agree to use them for that only: to show them where
people are gathered to pray, under whatever licence your community needs for
it, and not to copy, publish or pass them on.</p>"
  "What /plp says before it opens a console, as HTML.")

(defparameter *key-help*
  "<kbd>Space</kbd> <kbd>→</kbd> next · <kbd>←</kbd> back · <kbd>↑</kbd> <kbd>↓</kbd> the card
above / below · <kbd>1</kbd>–<kbd>9</kbd> verse ·
<kbd>R</kbd> refrain · <kbd>B</kbd> bridge · <kbd>P</kbd> pre-chorus · <kbd>T</kbd> tag ·
<kbd>O</kbd> outro · <kbd>.</kbd> black · <kbd>W</kbd> waiting · <kbd>N</kbd> / <kbd>Shift+N</kbd>
next / previous song · <kbd>/</kbd> search (<kbd>↑</kbd> <kbd>↓</kbd> choose, <kbd>Enter</kbd> adds,
<kbd>Shift+Enter</kbd> shows, <kbd>Esc</kbd> leaves)"
  "The key map, under the console.")

(defparameter *new-song-text*
  "
1.
The first verse, a line of the screen to a line

Refrain:
The refrain
"
  "What the song text has under its headers when a new song is given no
words.")

(defparameter *card-columns* 3
  "How many cards stand in a row of the middle pane.")

(defun row-step (index count columns direction)
  "The index of the card above (DIRECTION -1) or below (1) the INDEXth of
COUNT cards in rows of COLUMNS: under a card that has none below it but a
row is left, the last card; INDEX itself where there is no row to go to."
  (let ((target (+ index (* direction columns))))
    (cond ((< target 0) index)
          ((< target count) target)
          ((< (floor index columns) (floor (1- count) columns)) (1- count))
          (t index))))

(defun on-front (body)
  "/plp: the terms, and a button that opens a console on a new room."
  (setup-page body "Praise lyrics")
  (let* ((front (create-div body :class "plp-front"))
         (button (progn
                   (create-section front :h1 :content "Praise lyrics")
                   (create-div front :content *terms*)
                   (create-button front :content "I agree: open the console"))))
    (set-on-click button
                  (lambda (object)
                    (declare (ignore object))
                    (setf (url (location body))
                          (format nil "/plp-console/~A" (live:new-code)))))))

(defun card-html (slide)
  "The inside of the card of SLIDE: its key and name, and its lines."
  (format nil "<div class='plp-card-label'><kbd>~A</kbd>~A~A</div>~A"
          (escape-html (song:slide-group slide))
          (escape-html (song:slide-label slide))
          (if (> (song:slide-of slide) 1)
              (format nil " (~D/~D)" (song:slide-part slide) (song:slide-of slide))
              "")
          (slide-html slide)))

(defun song-title (id)
  "The title of the song ID, or ID itself when the store has no such song."
  (or (getf (find id (db:catalog) :key (lambda (entry) (getf entry :id)) :test #'string=)
            :title)
      id))

(defun build-console (body room)
  "Build the console of ROOM in BODY, and keep it drawn until its page goes."
  (let* ((offset (timezone-offset body))
         (root (create-div body :class "plp-console"))
         ;; The bar.
         (bar (create-div root :class "plp-bar"))
         (screen-link (progn
                        (create-span bar :content "Praise lyrics")
                        (create-span bar :class "plp-code" :content (live:room-code room))
                        (create-a bar :link (format nil "/plp-screen/~A" (live:room-code room))
                                      :content "Open the screen" :target "_blank")))
         (black (create-button bar :content "Black"))
         (waiting (create-button bar :content "Waiting"))
         (footer (create-button bar :content "Footer"))
         (end-input (create-form-element bar :text :size 5))
         (end-button (create-button bar :content "Set end"))
         (waiting-input (create-form-element bar :text :size 24))
         (passphrase (progn
                       (create-div bar :class "plp-grow")
                       (create-form-element bar :password :size 14)))
         ;; The left column: the setlist and the search.
         (left (create-div root :class "plp-column"))
         (saved-fields (progn
                         (create-section left :h2 :content "Setlist")
                         (create-div left :class "plp-fields")))
         (saved (create-select saved-fields))
         (load-button (create-button saved-fields :content "Load"))
         (list-area (create-div left))
         (name-fields (create-div left :class "plp-fields"))
         (name-input (create-form-element name-fields :text))
         (save-list (create-button name-fields :content "Save"))
         (delete-list (create-button name-fields :content "Delete"))
         (search-fields (progn
                          (create-section left :h2 :content "Songs")
                          (create-div left :class "plp-fields")))
         (search (create-form-element search-fields :text))
         (tag-fields (create-div left :class "plp-fields"))
         (tag-select (create-select tag-fields))
         (results-area (create-div left))
         ;; The middle: the song's slides.
         (pane (create-div root :class "plp-column plp-pane"))
         (title (create-div pane :class "plp-song-title"))
         (writers (create-div pane :class "plp-song-writers"))
         (cards-area (create-div pane :class "plp-cards"))
         ;; The right column: the screen, and a song's text.
         (right (create-div root :class "plp-column"))
         (now (progn
                (create-section right :h2 :content "On the screen")
                (create-div right :class "plp-preview")))
         (next (progn
                 (create-section right :h2 :content "Next")
                 (create-div right :class "plp-preview plp-next")))
         (editor (progn
                   (create-section right :h2 :content "Song text")
                   (create-text-area right :rows 16)))
         (editor-buttons (create-div right :class "plp-fields"))
         (new-song (create-button editor-buttons :content "New"))
         (this-song (create-button editor-buttons :content "This song"))
         (check-song (create-button editor-buttons :content "Check"))
         (save-song (create-button editor-buttons :content "Save"))
         (delete-song (create-button editor-buttons :content "Delete"))
         (message (create-div right :class "plp-message"))
         ;; The form for a new song, over the console.
         (popup (create-div root :class "plp-popup"))
         (dialog (create-div popup :class "plp-dialog"))
         (new-title (progn
                      (create-section dialog :h2 :content "New song")
                      (create-form-element dialog :text)))
         (new-writers (create-form-element dialog :text))
         (new-copyright (create-form-element dialog :text))
         (new-words (create-text-area dialog :rows 12))
         (outline-area (progn
                         (create-section dialog :h2 :content "Structure")
                         (create-div dialog :class "plp-outline")))
         (popup-buttons (create-div dialog :class "plp-fields"))
         (done (create-button popup-buttons :content "Done"))
         (cancel (create-button popup-buttons :content "Cancel"))
         (parts '()) (places '())
         (cards '()) (live-card nil)
         (results '()) (result-rows '()) (selected 0)
         ;; The song text that Save last held back as likely in the store
         ;; already: Save on the same text again stores it.
         (doubted nil)
         ;; Events come each in its own thread: one draws a list at a time.
         (lock (bordeaux-threads:make-recursive-lock "plp-console")))
    (declare (ignore screen-link))
    (create-div root :class "plp-keys" :content *key-help*)
    (setf (tab-index pane) 0
          (place-holder end-input) "20:00"
          (place-holder waiting-input) "Line on the waiting slide"
          (place-holder passphrase) "Passphrase"
          (place-holder name-input) "Name of the setlist"
          (place-holder search) "Title, number, writer, words"
          (place-holder new-title) "Title"
          (place-holder new-writers) "Writers"
          (place-holder new-copyright) "Copyright: © year, publisher"
          (place-holder new-words)
          "Paste the words here, a blank line between the verses"
          (style cards-area "grid-template-columns")
          (format nil "repeat(~D, minmax(0, 1fr))" *card-columns*))
    (labels ((say (control &rest arguments)
               (setf (inner-html message)
                     (escape-html (apply #'format nil control arguments))))
             (authorized ()
               (or (db:authorized-p (value passphrase))
                   (progn (say "That needs the passphrase.") nil)))
             (on (button on-p)
               (if on-p (add-class button "plp-on") (remove-class button "plp-on")))
             (click (object function)
               ;; Every button gives the keys back to the pane.
               (set-on-click object (lambda (clicked)
                                      (declare (ignore clicked))
                                      (funcall function)
                                      (focus pane))))
             (draw-cards ()
               (let* ((state (live:snapshot room))
                      (song (getf state :song)))
                 (setf (inner-html title)
                       (escape-html (if song (song:song-title song) "No song yet"))
                       (inner-html writers)
                       (escape-html (or (and song (song:song-writers song))
                                        (if song "" "Search for one on the left.")))
                       (inner-html cards-area) ""
                       live-card nil
                       cards (loop for slide in (getf state :deck)
                                   for index from 0
                                   collect (let ((card (create-div cards-area
                                                                   :class "plp-card"
                                                                   :content (card-html slide)))
                                                 (index index))
                                             (click card (lambda () (live:go-to room index)))
                                             card)))))
             (draw-live ()
               (let* ((state (live:snapshot room))
                      (mode (getf state :mode))
                      (card (nth (getf state :index) cards)))
                 (unless (eq card live-card)
                   (when live-card (remove-class live-card "plp-live"))
                   (when card
                     (add-class card "plp-live")
                     (scroll-into-view card))
                   (setf live-card card))
                 (on black (eq mode :black))
                 (on waiting (eq mode :waiting))
                 (setf (inner-html now)
                       (cond ((eq mode :black) "(black)")
                             ((eq mode :waiting) "(the waiting slide)")
                             ((getf state :slide) (slide-html (getf state :slide)))
                             (t ""))
                       (inner-html next)
                       (if (getf state :next) (slide-html (getf state :next)) ""))))
             (draw-list ()
               (let ((state (live:snapshot room)))
                 (setf (inner-html list-area) "")
                 (loop for (id . note) in (getf state :items)
                       for place from 0
                       do (let* ((place place)
                                 (row (create-div list-area
                                                  :class (if (eql place (getf state :position))
                                                             "plp-row plp-current"
                                                             "plp-row")))
                                 (name (create-span row :class "plp-name"
                                                        :content (format nil "~D. ~A~@[ <small>~A</small>~]"
                                                                         (1+ place)
                                                                         (escape-html (song-title id))
                                                                         (and (plusp (length note))
                                                                              (escape-html note))))))
                            (click name (lambda () (live:show-item room place)))
                            (click (create-button row :content "↑")
                                   (lambda () (live:move-item room place -1)))
                            (click (create-button row :content "↓")
                                   (lambda () (live:move-item room place 1)))
                            (click (create-button row :content "✕")
                                   (lambda () (live:remove-item room place)))))))
             (draw-session ()
               (let ((state (live:snapshot room)))
                 (on footer (getf state :footer-p))
                 (on end-button (getf state :end-time))
                 (setf (inner-html end-button)
                       (if (getf state :end-time)
                           (format nil "Ends ~A"
                                   (live:clock-text (getf state :end-time) offset))
                           "Set end"))))
             (draw (what)
               (ecase what
                 (:song (draw-cards) (draw-live) (draw-list))
                 (:slide (draw-live))
                 (:list (draw-list))
                 (:session (draw-session))))
             (show (id)
               ;; A song from the search goes after the one showing.
               (live:show-item room (live:add-item room id :after-current t)))
             (select-result (index)
               ;; Mark the INDEXth song found, or the nearest there is.
               (bordeaux-threads:with-recursive-lock-held (lock)
                 (when result-rows
                   (let ((old (nth selected result-rows))
                         (index (max 0 (min index (1- (length result-rows))))))
                     (when old (remove-class old "plp-current"))
                     (setf selected index)
                     (add-class (nth index result-rows) "plp-current")
                     (scroll-into-view (nth index result-rows))))))
             (draw-results ()
               (bordeaux-threads:with-recursive-lock-held (lock)
                 (setf results (if (and (string= (string-trim " " (value search)) "")
                                        (string= (value tag-select) ""))
                                   '()
                                   (db:search-songs (value search)
                                                    :tag (and (string/= (value tag-select) "")
                                                              (value tag-select))))
                       (inner-html results-area) ""
                       selected 0
                       result-rows
                       (loop for entry in results
                             collect (let* ((id (getf entry :id))
                                            (row (create-div results-area :class "plp-row"))
                                            (name (create-span
                                                   row :class "plp-name"
                                                   :content (format nil "~@[~D. ~]~A~@[ <small>~A</small>~]"
                                                                    (getf entry :number)
                                                                    (escape-html (getf entry :title))
                                                                    (and (getf entry :line)
                                                                         (escape-html (getf entry :line)))))))
                                       (click name (lambda () (show id)))
                                       (click (create-button row :content "Show")
                                              (lambda () (show id)))
                                       (click (create-button row :content "+")
                                              (lambda () (live:add-item room id)))
                                       row)))
                 (select-result 0)))
             (draw-outline ()
               ;; What the new song's words are taken for, part by part.
               (bordeaux-threads:with-recursive-lock-held (lock)
                 (setf (inner-html outline-area) "")
                 (if (null parts)
                     (create-div outline-area
                                 :content "The parts of the song will be listed here.")
                     (let* ((song (song:outline-song parts places))
                            (sections (song:song-sections song))
                            (keys (mapcar #'song:section-key sections)))
                       (loop for section in sections
                             for index from 0
                             do (let* ((index index)
                                       (kind (song:section-kind section))
                                       (row (create-div outline-area :class "plp-row"))
                                       (select (create-select row)))
                                  (dolist (option (if (member kind song:*outline-kinds*)
                                                      song:*outline-kinds*
                                                      (cons kind song:*outline-kinds*)))
                                    (create-option select
                                                   :value (string-downcase option)
                                                   :selected (eq option kind)
                                                   :content (escape-html
                                                             (or (song:kind-label option)
                                                                 (song:section-label section)))))
                                  (create-span
                                   row :class "plp-name"
                                   :content (format nil "<kbd>~A</kbd> ~A~@[ <small>×~D</small>~]"
                                                    (escape-html (nth index keys))
                                                    (escape-html
                                                     (first (first (song:section-slides section))))
                                                    (let ((times (count index places)))
                                                      (and (> times 1) times))))
                                  (set-on-change
                                   select
                                   (lambda (object)
                                     (declare (ignore object))
                                     (let ((kind (find (value select) song:*outline-kinds*
                                                       :test #'string-equal)))
                                       (when kind
                                         (bordeaux-threads:with-recursive-lock-held (lock)
                                           (when (< index (length parts))
                                             (setf (nth index parts)
                                                   (song:relabel (nth index parts) kind)))
                                           (draw-outline))))))))
                       (create-div outline-area
                                   :class "plp-order"
                                   :content (format nil "Sung: ~{~A~^ ~}"
                                                    (mapcar #'escape-html
                                                            (or (song:song-order song) keys))))))))
             (close-popup ()
               (remove-class popup "plp-open"))
             (draw-saved ()
               (setf (inner-html saved) "")
               (create-option saved :content "Saved setlists…" :value "")
               (dolist (setlist (db:all-setlists))
                 (create-option saved :value (getf setlist :id)
                                      :content (escape-html
                                                (format nil "~A (~D)" (getf setlist :name)
                                                        (length (getf setlist :items))))))))
      ;; The bar.
      (click black (lambda () (live:set-mode room :black)))
      (click waiting (lambda () (live:set-mode room :waiting)))
      (click footer (lambda ()
                      (live:set-footer room (not (getf (live:snapshot room) :footer-p)))))
      (click end-button
             (lambda ()
               (let* ((text (string-trim " " (value end-input)))
                      (time (live:parse-clock text (get-universal-time) offset)))
                 (cond ((string= text "") (live:set-end-time room nil))
                       (time (live:set-end-time room time))
                       (t (say "The end is a time of day, as 20:00."))))))
      (set-on-change waiting-input
                     (lambda (object)
                       (declare (ignore object))
                       (live:set-waiting-text room (value waiting-input))))
      ;; The setlist.
      (click load-button
             (lambda ()
               (let ((setlist (db:find-setlist (value saved))))
                 (when setlist
                   (setf (value name-input) (getf setlist :name))
                   (live:set-items room (getf setlist :items))))))
      (click save-list
             (lambda ()
               (when (authorized)
                 (handler-case
                     (progn
                       (db:save-setlist (string-trim " " (value name-input))
                                        (getf (live:snapshot room) :items))
                       (draw-saved)
                       (say "Saved the setlist ~A." (value name-input)))
                   (error (condition) (say "~A" condition))))))
      (click delete-list
             (lambda ()
               (when (authorized)
                 (if (db:delete-setlist (song:slug (value name-input)))
                     (progn (draw-saved) (say "Deleted the setlist ~A." (value name-input)))
                     (say "There is no setlist of that name.")))))
      ;; The search.
      (create-option tag-select :content "Any tag" :value "")
      (dolist (tag (db:all-tags))
        (create-option tag-select
                       :value (format nil "~(~A~)=~A" (first tag) (second tag))
                       :content (escape-html
                                 (format nil "~(~A~): ~A~@[ (~A)~]"
                                         (first tag) (second tag) (third tag)))))
      (set-on-input search (lambda (object) (declare (ignore object)) (draw-results)))
      (set-on-change tag-select (lambda (object) (declare (ignore object)) (draw-results)))
      (set-on-key-down search
                       (lambda (object event)
                         (declare (ignore object))
                         (let ((key (getf event :key)))
                           (cond ((equal key "Escape") (focus pane))
                                 ((equal key "ArrowDown") (select-result (1+ selected)))
                                 ((equal key "ArrowUp") (select-result (1- selected)))
                                 ((and (equal key "Enter") (nth selected results))
                                  (let ((id (getf (nth selected results) :id)))
                                    (if (getf event :shift-key)
                                        (progn (show id) (focus pane))
                                        (live:add-item room id)))))))
                       :disable-default '("ArrowUp" "ArrowDown"))
      ;; The keys.  The browser would scroll the pane for those listed.
      (set-on-key-down pane
                       (lambda (object event)
                         (declare (ignore object))
                         (let ((key (getf event :key))
                               (state (live:snapshot room)))
                           (cond ((equal key "/") (focus search))
                                 ((and (member key '("ArrowUp" "ArrowDown") :test #'equal)
                                       (eq (getf state :mode) :lyrics)
                                       (not (or (getf event :ctrl-key) (getf event :alt-key)
                                                (getf event :meta-key))))
                                  (live:go-to room (row-step (getf state :index) (length cards)
                                                             *card-columns*
                                                             (if (equal key "ArrowUp") -1 1))))
                                 (t (handle-key room event)))))
                       :disable-default '("ArrowUp" "ArrowDown" "ArrowLeft" "ArrowRight"
                                          " " "PageUp" "PageDown" "/"))
      ;; The song's text.
      (set-on-click new-song
                    (lambda (object)
                      (declare (ignore object))
                      (draw-outline)
                      (add-class popup "plp-open")
                      (focus new-title)))
      (set-on-input new-words
                    (lambda (object)
                      (declare (ignore object))
                      (bordeaux-threads:with-recursive-lock-held (lock)
                        (multiple-value-setq (parts places)
                          (song:outline (text-value new-words)))
                        (draw-outline))))
      (set-on-key-down dialog
                       (lambda (object event)
                         (declare (ignore object))
                         (when (equal (getf event :key) "Escape")
                           (close-popup)
                           (focus pane))))
      (click cancel #'close-popup)
      (click done
             (lambda ()
               (bordeaux-threads:with-recursive-lock-held (lock)
                 (setf (text-value editor)
                       (if parts
                           (song:print-song
                            (song:outline-song parts places
                                               :title (value new-title)
                                               :writers (value new-writers)
                                               :copyright (value new-copyright)))
                           (format nil "Title: ~A~%Writers: ~A~%Copyright: ~A~%~A"
                                   (value new-title) (value new-writers)
                                   (value new-copyright) *new-song-text*))
                       (value new-title) ""
                       (value new-writers) ""
                       (value new-copyright) ""
                       (text-value new-words) ""
                       parts '()
                       places '()))
               (close-popup)
               (say "The new song is in the text field: Check it, then Save.")))
      (click this-song
             (lambda ()
               (let ((song (getf (live:snapshot room) :song)))
                 (if song
                     (setf (text-value editor) (song:print-song song))
                     (say "No song is showing.")))))
      (click check-song
             (lambda ()
               (let* ((parsed (song:parse-song (text-value editor)))
                      (problems (song:song-problems parsed)))
                 (if problems
                     (say "~{~A~^~%~}" problems)
                     (say "~D slides: ~{~A~^ ~}" (length (song:song-deck parsed))
                          (mapcar #'song:slide-key (song:song-deck parsed)))))))
      (click save-song
             (lambda ()
               (when (authorized)
                 (let* ((text (text-value editor))
                        (parsed (song:parse-song text))
                        (problems (song:song-problems parsed))
                        ;; A new song is looked for among those stored,
                        ;; unless this very text was held back just before.
                        (similar (and (not problems)
                                      (not (equal text doubted))
                                      (db:similar-songs parsed))))
                   (cond (problems
                          (say "~{~A~^~%~}" problems))
                         (similar
                          (setf doubted text)
                          (say "Not saved: there ~:[are songs~;is a song~] like it already.~%~:{~@[~D. ~]~A (~A)~%~}Save again to add it all the same."
                               (null (rest similar))
                               (mapcar (lambda (entry)
                                         (list (getf entry :number) (getf entry :title)
                                               (getf entry :id)))
                                       similar)))
                         (t
                          (let ((id (db:save-song parsed))
                                (showing (getf (live:snapshot room) :song)))
                            (setf doubted nil)
                            (say "Saved as ~A." id)
                            (when (and showing (equal (song:song-id showing) id))
                              (live:show-song room id))
                            (draw-results))))))))
      (click delete-song
             (lambda ()
               (when (authorized)
                 (let ((id (song:song-id (song:parse-song (text-value editor)))))
                   (cond ((null id) (say "The text names no song: it has no Id: header."))
                         ((db:delete-song id) (draw-results) (say "Deleted ~A." id))
                         (t (say "There is no song ~A." id)))))))
      (draw-saved)
      (draw :song)
      (draw :session)
      (focus pane)
      (let ((subscription (live:subscribe room #'draw)))
        (unwind-protect
             (loop while (validp body) do (sleep 5))
          (live:unsubscribe room subscription))))))

(defun on-console (body)
  "/plp-console/CODE: the console of the room in the address."
  (let ((room (live:ensure-room (path-code (path-name (location body)) "/plp-console/"))))
    (setup-page body "Lyrics console")
    (if room
        (build-console body room)
        (create-div body :class "plp-notice"
                         :content "No such console: start from <a href='/plp'>/plp</a>."))))

(defun register-routes ()
  "Serve the presenter's pages.  The console and the screen take the room's
code as the rest of the path, which needs CLOG's :EXTENDED-ROUTING; neither
is under /plp/, whose routes that would shadow."
  (set-on-new-window 'on-front :path "/plp")
  (set-on-new-window 'on-console :path "/plp-console")
  (set-on-new-window 'on-screen :path "/plp-screen"))
