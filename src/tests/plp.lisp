;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:bhw/tests/plp
  (:use #:common-lisp)
  (:import-from #:rove #:deftest #:ok #:ng #:testing)
  (:import-from #:bhw/tests/test-utils #:with-temp-data-root)
  (:local-nicknames (#:song #:bhw/plp/song)
                    (#:db #:bhw/plp/database)
                    (#:import #:bhw/plp/import)
                    (#:live #:bhw/plp/live)
                    (#:screen #:bhw/plp/screen)
                    (#:console #:bhw/plp/console))
  (:documentation
   "Tests for the praise lyrics presenter: the song format and its slides,
the store, the chord book's import, the live room and what the pages work
out.  The fixtures are laid out as the chord book is, with the words of a
hymn in the public domain (Abide With Me, Henry Francis Lyte, 1847) and an
invented refrain: none of the book's text is in this repository."))

(in-package #:bhw/tests/plp)

(defparameter *abide*
  "Title: Abide With Me
Alt: Eventide; Fast Falls The Eventide
Writers: Henry Francis Lyte
Copyright: Public domain
CCLI: 43192
Number: 2
Order: 1 R 2 R B
Tags: tempo=Slow; liturgy=Christmas {Dec 25}

1.
Abide with me, fast falls the eventide;
The darkness deepens; Lord, with me abide.

Refrain:
When other helpers fail and comforts flee,
Help of the helpless, O abide with me.
Line three
Line four
Line five
Line six
Line seven

2.
Swift to its close ebbs out life's little day;

Intro:

[Bridge]
I need Thy presence every passing hour.

Refrain 2:
Abide with me.
"
  "A song in the text format, with a refrain too long for one slide.")

(defun keys (deck)
  (mapcar (lambda (slide) (list (song:slide-key slide) (song:slide-part slide)))
          deck))

(deftest song-text-format
  (let ((abide (song:parse-song *abide*)))
    (testing "headers"
      (ok (equal "Abide With Me" (song:song-title abide)))
      (ok (equal '("Eventide" "Fast Falls The Eventide") (song:song-alt-titles abide)))
      (ok (equal "Henry Francis Lyte" (song:song-writers abide)))
      (ok (eql 2 (song:song-number abide)))
      (ok (equal '("1" "R" "2" "R" "B") (song:song-order abide)))
      (ok (equal '((:tempo "Slow" nil) (:liturgy "Christmas" "Dec 25"))
                 (song:song-tags abide))))
    (testing "sections: a wordless one is dropped, the kinds are numbered"
      (ok (equal '("1" "R" "2" "B" "R2")
                 (mapcar #'song:section-key (song:song-sections abide))))
      (ok (equal '("Verse 1" "Refrain" "Verse 2" "Bridge" "Refrain 2")
                 (mapcar #'song:section-name (song:song-sections abide)))))
    (testing "it prints as it reads"
      (ok (equalp abide (song:parse-song (song:print-song abide)))))
    (ok (null (song:song-problems abide)))
    (ok (equal "Abide with me, fast falls the eventide;" (song:song-first-line abide))))
  (testing "text under no label is verses, one to a block"
    (let ((pasted (song:parse-song (format nil "Title: Pasted~%~%one~%two~%~%three~%~%Chorus~%four~%"))))
      (ok (equal '("1" "2" "R") (mapcar #'song:section-key (song:song-sections pasted))))))
  (testing "a blank line under a label breaks the slide"
    (let ((broken (song:parse-song (format nil "Title: T~%~%Refrain:~%a~%~%b~%"))))
      (ok (equal '(("a") ("b"))
                 (song:section-slides (first (song:song-sections broken)))))))
  (testing "problems"
    (ok (song:song-problems (song:parse-song "1.
words")) "no title")
    (ok (song:song-problems (song:parse-song "Title: T")) "no words")
    (ok (song:song-problems (song:parse-song (format nil "Title: T~%Order: 1 B~%~%1.~%words~%")))
        "the order names a section the song lacks")
    (ok (song:song-problems (song:parse-song (format nil "Title: T~%~%1.~%a~%~%1.~%b~%")))
        "two sections with one key"))
  (ok (equal "o-come-all-ye-faithful" (song:slug "O Come, All Ye Faithful!"))))

(deftest slides-and-jumps
  (let ((deck (song:song-deck (song:parse-song *abide*))))
    (testing "the deck follows the order, a long section is split evenly, and
a section the order leaves out comes last"
      (ok (equal '(("1" 1) ("R" 1) ("R" 2) ("2" 1) ("R" 1) ("R" 2) ("B" 1) ("R2" 1))
                 (keys deck)))
      (ok (equal '(4 3) (mapcar #'length (song:split-lines '(1 2 3 4 5 6 7)))))
      (ok (equal '((1 2 3 4 5 6)) (song:split-lines '(1 2 3 4 5 6)))))
    (testing "a key leads to its section's next place in the song"
      (ok (eql 1 (song:deck-jump deck 0 "R")) "the refrain after verse 1")
      (ok (eql 2 (song:deck-jump deck 1 "R")) "again: its second slide")
      (ok (eql 7 (song:deck-jump deck 2 "R")) "again: Refrain 2")
      (ok (eql 1 (song:deck-jump deck 7 "R")) "and round")
      (ok (eql 4 (song:deck-jump deck 3 "c")) "C is R; from verse 2, the refrain after it")
      (ok (eql 3 (song:deck-jump deck 0 "2")))
      (ok (eql 6 (song:deck-jump deck 0 "B")))
      (ok (eql 0 (song:deck-jump deck nil "1")))
      (ok (null (song:deck-jump deck 0 "P")) "no pre-chorus"))))

(defparameter *page*
  "                                  2. Abide With Me
                                     Henry Francis Lyte - Luke 24:29
Capo 0 - 2                                                              (Recording: Capo 3)

Intro: G - D

        G        D       Em
1.   Abide with me, fast falls the eventide;
        C          G           D          Asus4 - A
     The darkness deepens; Lord, with me a - bide.                    (Ref.)

                    G            D
Refrain:      When other helpers fail and comforts flee,
                    C            G/B   D
              Help of the helpless, O abide with me.

        G        D       Em                                    E/G#    4x2400
2.   Swift to its close ebbs out life's little day;
        C          G           D
     Earth's joys grow dim, its glories pass a ----- way.            (Ref. x2)

© Public domain. (on Hymns Ancient and Modern).
CCLI Song #43192. CCLI License #0000."
  "A page as pdftotext -layout gives the chord book's.")

(defparameter *two-columns*
  "                                  3. Abide With Me
                                     Henry Francis Lyte

        G        D                                                 G        D
1.   Abide with me,                                           3.   I need Thy presence
        C          G                                               C          G
     fast falls the eventide.                                      every passing hour.

        G        D                                                 G        D
2.   Swift to its close                                       4.   I fear no foe,
        C          G                                               C          G
     ebbs out life's little day.                                   with Thee at hand to bless."
  "A page set in two columns.")

(deftest chord-lines
  (ok (import:chord-line-p "   G        D       Em"))
  (ok (import:chord-line-p "C   G/B  Asus4 - A7   (Bm7)  D2"))
  (ok (import:chord-line-p "G-D  /F#  Em-A  C*  N.C."))
  (ng (import:chord-line-p "A mighty fortress is our God"))
  (ng (import:chord-line-p "Am I a soldier of the cross"))
  (ng (import:chord-line-p " - "))
  (ng (import:chord-line-p "")))

(deftest a-page-of-the-book
  (let ((page (import:parse-song-page *page*)))
    (ok (eql 2 (getf page :number)))
    (ok (equal "Abide With Me" (getf page :title)))
    (ok (equal "Henry Francis Lyte - Luke 24:29" (getf page :writers)))
    (ok (equal "43192" (getf page :ccli)))
    (ok (uiop:string-prefix-p "© Public domain." (getf page :copyright)))
    (ng (getf page :columns-p))
    (ng (getf page :residue) "no chords among the words")
    (multiple-value-bind (song notes) (import::build-song (list page) :aliases '("Eventide"))
      (ok (equal "cc2021/2" (song:song-id song)))
      (ok (equal '("Eventide") (song:song-alt-titles song)))
      (ok (equal '("1" "R" "2") (mapcar #'song:section-key (song:song-sections song)))
          "the chord-only intro has no words and is dropped")
      (ok (equal '("1" "R" "2" "R") (song:song-order song))
          "the marks put the refrain after each verse, once")
      (ok (equal '("Abide with me, fast falls the eventide;"
                   "The darkness deepens; Lord, with me abide.")
                 (first (song:section-slides (first (song:song-sections song)))))
          "chords, the mark and the syllable break are gone")
      (ok (equal "Earth's joys grow dim, its glories pass away."
                 (second (first (song:section-slides (third (song:song-sections song)))))))
      (ok (null (song:song-problems song)))
      (ok (null notes)))))

(deftest two-columns-and-two-songs
  (testing "the right column is read after the left"
    (let ((page (import:parse-song-page *two-columns*)))
      (ok (eq t (getf page :columns-p)))
      (let ((song (import::build-song (list page))))
        (ok (equal '("1" "2" "3" "4") (mapcar #'song:section-key (song:song-sections song))))
        (ok (equal '("I need Thy presence" "every passing hour.")
                   (first (song:section-slides (third (song:song-sections song)))))))))
  (testing "words that run across where the cut would be leave the page whole"
    (multiple-value-bind (lines cut)
        (import:split-columns
         (list "1.  A line of the verse that is long enough to run across the cut, on and on"
               "    short                                           3.  another"
               "    short                                               more words"
               "    short                                               more words"
               "    short                                               more words"))
      (ok (eq :uncut cut))
      (ok (= 5 (length lines)))))
  (testing "a page with two songs is two pages"
    (let ((parts (import::split-page
                  (format nil "~30T58. First~%   1.  words~%~%© one~%~30T59. Second~%   2. more~%"))))
      (ok (= 2 (length parts)))
      (ok (eql 59 (getf (import:parse-song-page (second parts)) :number))))))

(defun word (text left top height)
  (list :page 1 :left left :top top :width (* 4.0 (length text)) :height height :text text))

(deftest the-indices
  (testing "words are read from pdftotext's table"
    (let ((words (import:parse-index-words
                  (format nil "level	page_num	par_num	block_num	line_num	word_num	left	top	width	height	conf	text~%~
                               1	4	0	0	0	0	0.0	0.0	612.0	792.0	-1	###PAGE###~%~
                               5	4	0	0	0	1	57.6	141.5	18.1	8.4	100	Abide~%"))))
      (ok (= 1 (length words)))
      (ok (equal "Abide" (getf (first words) :text)))
      (ok (eql 4 (getf (first words) :page)))))
  (testing "entries take the heading above them in their column"
    (multiple-value-bind (entries unresolved)
        (import:index-tags
         (list (word "5." 135 58 25) (word "L" 177 58 25) (word "ITURGICAL" 199 63 20)
               ;; First column: a heading in small capitals with its date.
               (word "C" 57 117 10) (word "HRISTMAS" 61 118 8) (word "(Dec" 122 119 7)
               (word "25)" 141 119 7)
               (word "Abide" 57 141 8) (word "With" 80 141 8) (word "Me......." 100 141 8)
               (word "2" 193 141 8)
               ;; A title on two lines, the leader run into the number.
               (word "Holy" 57 151 8) (word "Light" 75 151 8)
               (word "(Alma" 75 161 8) (word "Lux)......127" 97 161 8)
               (word "Taizé" 57 171 8) (word "Chant" 80 171 8)
               ;; Second column.
               (word "L" 235 117 10) (word "ENT" 239 118 8)
               (word "Abide" 235 141 8) (word "With" 258 141 8) (word "Me" 278 141 8)
               (word "..........." 290 141 8) (word "2" 370 141 8)))
      (ok (equal '((2 :liturgy "Christmas" "Dec 25")
                   (127 :liturgy "Christmas" "Dec 25")
                   (2 :liturgy "Lent" nil))
                 entries))
      (ok (equal '("Taizé Chant") unresolved)))))

(defmacro with-store (&body body)
  "Run BODY with the presenter's store open under a data root of its own."
  `(with-temp-data-root ()
     (let ((*standard-output* (make-broadcast-stream)))
       (unwind-protect
            (progn (db:open-database) ,@body)
         (db:close-database)))))

(deftest the-store
  (with-store
    (let* ((abide (song:parse-song *abide*))
           (id (db:save-song abide))
           (grace (db:save-song (song:parse-song (format nil "Title: Amazing Grace~%Writers: John Newton~%Tags: tempo=Slow~%~%1.~%Amazing grace, how sweet the sound~%")))))
      (testing "a song without an id is given one"
        (ok (equal "custom/abide-with-me" id))
        (ok (equal "custom/amazing-grace" grace))
        (ok (equal "custom/amazing-grace-1"
                   (db:save-song (song:parse-song (format nil "Title: Amazing Grace~%~%1.~%other~%"))))))
      (testing "it comes back as it went in"
        (let ((found (db:find-song id)))
          (setf (song:song-id abide) id)
          (ok (equalp (song:song-sections abide) (song:song-sections found)))
          (ok (equal (song:song-order abide) (song:song-order found)))
          (ok (equal '((:liturgy "Christmas" "Dec 25") (:tempo "Slow" nil))
                     (song:song-tags found))))
        (ok (null (db:find-song "custom/none"))))
      (testing "saving again replaces the song and its tags"
        (setf (song:song-tags abide) '((:tempo "Fast" nil))
              (song:song-writers abide) "H. F. Lyte")
        (db:save-song abide)
        (ok (equal '((:tempo "Fast" nil)) (song:song-tags (db:find-song id))))
        (ok (equal "H. F. Lyte" (song:song-writers (db:find-song id))))
        (ok (= 3 (length (db:catalog)))))
      (testing "search"
        (flet ((ids (&rest arguments)
                 (mapcar (lambda (entry) (getf entry :id)) (apply #'db:search-songs arguments))))
          (ok (equal (list id) (ids "abide")))
          (ok (equal (list id) (ids "eventide")) "another title")
          (ok (equal (list grace) (ids "newton sweet")) "writer and first line")
          (ok (equal (list id) (ids "2")) "the number")
          (ok (equal (list grace) (ids "" :tag "tempo=Slow")))
          (ok (null (ids "nothing like it")))))
      (ok (member '(:tempo "Slow" nil) (db:all-tags) :test #'equal))
      (testing "setlists"
        (ok (equal "friday-adoration"
                   (db:save-setlist "Friday Adoration" (list (cons id "") (cons grace "in G")))))
        (db:save-setlist "Friday Adoration" (list (cons grace "in G")))
        (ok (equal (list (cons grace "in G"))
                   (getf (db:find-setlist "friday-adoration") :items)))
        (ok (= 1 (length (db:all-setlists))))
        (ok (db:delete-setlist "friday-adoration"))
        (ng (db:delete-setlist "friday-adoration"))
        (ok (null (db:all-setlists))))
      (testing "the store keeps it"
        (db:close-database)
        (db:open-database)
        (ok (equal "H. F. Lyte" (song:song-writers (db:find-song id)))))
      (testing "deleting"
        (ok (db:delete-song grace))
        (ng (db:delete-song grace))
        (ok (null (db:find-song grace)))))))

(deftest the-passphrase
  (let ((db::*passphrase* "open sesame"))
    (ok (db:authorized-p "open sesame"))
    (ng (db:authorized-p "open sesamE"))
    (ng (db:authorized-p "open"))
    (ng (db:authorized-p nil)))
  (let ((db::*passphrase* nil))
    (ng (db:authorized-p "") "with none set, nothing is allowed")
    (ng (db:authorized-p nil))))

(deftest the-room
  (ok (live:valid-code-p "AB12CD"))
  (ng (live:valid-code-p "ab12cd"))
  (ng (live:valid-code-p "../etc"))
  (ok (live:valid-code-p (live:new-code)))
  (ng (live:ensure-room "no"))
  (with-store
    (let* ((id (db:save-song (song:parse-song *abide*)))
           (other (db:save-song (song:parse-song (format nil "Title: Other~%~%1.~%words~%"))))
           (room (live:ensure-room (live:new-code)))
           (heard '()))
      (flet ((showing ()
               (let ((slide (getf (live:snapshot room) :slide)))
                 (and slide (list (song:slide-key slide) (song:slide-part slide))))))
        (ok (eq room (live:ensure-room (live:room-code room))))
        (live:subscribe room (lambda (what) (push what heard)))
        (live:subscribe room (lambda (what) (declare (ignore what)) (error "gone")))
        (testing "a song goes up at its first slide, and the room's pages hear of it"
          (ng (live:show-song room "custom/none"))
          (ok (live:show-song room id))
          (ok (equal '("1" 1) (showing)))
          (ok (equal '(:song) heard))
          (ok (= 1 (length (live::room-subscribers room)))
              "a subscriber that fails is dropped"))
        (testing "the keys"
          (ok (live:press-key room " "))
          (ok (equal '("R" 1) (showing)))
          (live:press-key room "r")
          (ok (equal '("R" 2) (showing)))
          (live:press-key room "2")
          (ok (equal '("2" 1) (showing)))
          (live:press-key room "ArrowLeft")
          (ok (equal '("R" 2) (showing)))
          (live:press-key room "B")
          (ok (equal '("B" 1) (showing)))
          (live:press-key room "p")
          (ok (equal '("B" 1) (showing)) "no pre-chorus: nothing moves")
          (dotimes (i 5) (live:press-key room "PageDown"))
          (ok (equal '("R2" 1) (showing)) "the last slide holds")
          (ng (live:press-key room "x") "not a key of the map")
          (ng (live:press-key room "Shift")))
        (testing "black and waiting"
          (live:press-key room ".")
          (ok (eq :black (getf (live:snapshot room) :mode)))
          (live:press-key room " ")
          (ok (eq :lyrics (getf (live:snapshot room) :mode)))
          (ok (equal '("R2" 1) (showing)) "the first step forward only brings it back")
          (live:press-key room "w")
          (ok (eq :waiting (getf (live:snapshot room) :mode)))
          (live:press-key room "w")
          (ok (eq :lyrics (getf (live:snapshot room) :mode))))
        (testing "the setlist"
          (live:set-items room (list (cons id "") (cons other "")))
          (ok (eql 0 (getf (live:snapshot room) :position)))
          (ok (live:press-key room "n"))
          (ok (equal "Other" (song:song-title (getf (live:snapshot room) :song))))
          (ok (eql 1 (getf (live:snapshot room) :position)))
          (ng (live:step-song room 1) "the end of the list")
          (live:press-key room "N" :shift t)
          (ok (eql 0 (getf (live:snapshot room) :position)))
          (ok (eql 1 (live:add-item room other :after-current t)))
          (ok (= 3 (length (getf (live:snapshot room) :items))))
          (ok (live:move-item room 0 1))
          (ok (eql 1 (getf (live:snapshot room) :position)) "the place follows the song")
          (ok (live:remove-item room 0))
          (ok (eql 0 (getf (live:snapshot room) :position)))
          (ng (live:move-item room 0 -1)))))))

(deftest the-clock
  ;; 2026-11-20 00:42 UTC is 19:42 on the 19th in Ottawa, five hours behind.
  (let ((now (encode-universal-time 0 42 0 20 11 2026 0)))
    (ok (equal "19:42" (live:clock-text now 300)))
    (ok (equal "00:42" (live:clock-text now 0)))
    (ok (equal "18 min" (live:countdown-text now (+ now (* 18 60)))))
    (ok (equal "1 h 05 min" (live:countdown-text now (+ now (* 65 60)))))
    (ok (null (live:countdown-text now now)))
    (ok (null (live:countdown-text now nil)))
    (ok (eql (+ now (* 18 60)) (live:parse-clock "20:00" now 300)))
    (ok (eql (+ now (* 23 3600)) (live:parse-clock "18:42" now 300)) "tomorrow's")
    (ok (null (live:parse-clock "25:00" now 300)))
    (ok (null (live:parse-clock "soon" now 300)))
    (ok (equal "19:42 · 18 min" (screen:footer-clock now (+ now (* 18 60)) 300)))
    (ok (equal "19:42" (screen:footer-clock now nil 300)))))

(deftest the-pages
  (testing "the room's code in an address"
    (ok (equal "AB12CD" (screen:path-code "/plp-screen/ab12cd" "/plp-screen/")))
    (ok (equal "AB12CD" (screen:path-code "/plp-console/AB12CD/" "/plp-console/")))
    (ng (screen:path-code "/plp-screen/" "/plp-screen/"))
    (ng (screen:path-code "/plp-screen/a/b" "/plp-screen/"))
    (ng (screen:path-code "/bible/1-1-1" "/plp-screen/")))
  (testing "the type shrinks with the longest line and with the number of lines"
    (ok (equal "min(10.71vw, 28.00vh)" (screen:font-size '("Abide with me"))))
    (ok (equal "min(3.75vw, 14.00vh)"
               (screen:font-size (list (make-string 40 :initial-element #\a) "b" "c" "d")))))
  (testing "words are escaped"
    (let ((slide (first (song:song-deck
                         (song:parse-song (format nil "Title: T~%~%1.~%a <b> & 'c'~%"))))))
      (ok (equal "<div class='plp-line'>a &lt;b&gt; &amp; &#39;c&#39;</div>"
                 (screen:slide-html slide)))
      (ok (search "<kbd>1</kbd>Verse 1</div><div class='plp-line'>" (console:card-html slide)))))
  (ok (equal "Abide With Me · Henry Francis Lyte"
             (screen:footer-credit (song:parse-song *abide*))))
  (ok (equal "" (screen:footer-credit nil)))
  (testing "a waiting image is a picture in the images directory and nothing else"
    (with-temp-data-root ()
      (let ((file (uiop:subpathname (live:images-directory) "chapel.jpg")))
        (ensure-directories-exist file)
        (with-open-file (out file :direction :output) (write-string "jpeg" out))
        (with-open-file (out (uiop:subpathname (live:images-directory) "credits.txt")
                             :direction :output)
          (format out "chapel.jpg~CA. Photographer, CC BY-SA 4.0~%" #\Tab))
        (ok (equal '(("chapel.jpg" . "A. Photographer, CC BY-SA 4.0")) (live:waiting-images)))
        (ok (live:image-file "/plp-images/chapel.jpg"))
        (ng (live:image-file "/plp-images/credits.txt"))
        (ng (live:image-file "/plp-images/missing.jpg"))
        (ng (live:image-file "/plp-images/../store/heap.dat"))
        (ng (live:image-file "/assets/chapel.jpg"))))))
