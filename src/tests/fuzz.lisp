;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:bhw/tests/fuzz
  (:use #:common-lisp)
  (:import-from #:rove #:deftest #:ok #:testing)
  (:import-from #:bhw/tests/test-utils #:with-temp-data-root #:with-mocked-functions)
  (:import-from #:cl-ppcre)
  (:import-from #:flexi-streams)
  (:import-from #:ironclad)
  (:import-from #:com.inuoe.jzon)
  (:local-nicknames (#:main #:bhw/main)
                    (#:layout #:bhw/site/layout)
                    (#:conn #:clog-connection)
                    (#:bot #:bhw/slack-bot/bot)
                    (#:bn #:bhw/slack-bot/booking-nlp)
                    (#:vb #:bhw/slack-bot/van-booking)
                    (#:vb-tests #:bhw/tests/slack-bot/van-booking)
                    (#:song #:bhw/plp/song)
                    (#:store #:bhw/store)
                    (#:db #:bhw/plp/database)
                    (#:live #:bhw/plp/live)
                    (#:console #:bhw/plp/console))
  (:documentation
   "Properties of what reads outside input, checked on inputs made at random:
request paths and headers, Slack's requests, a page's websocket frames and
messages, booking requests and the sheet's cells, song texts, a room driven
by its keys and buttons, and the presenter's store.  Each test draws from a
fixed seed, so that a run is the same every time and a failure can be
replayed; the inputs mix random characters (controls, non-ASCII, lone
surrogates) with the tokens each reader looks for and mutations of inputs it
reads.  A call that signals, exhausts the stack or runs past a few seconds
fails, as does one whose result breaks the property; up to three failing
inputs are shown in the assertion.  The regressions found by fuzzing are
tested where they were fixed; this keeps looking."))

(in-package #:bhw/tests/fuzz)

;;; Inputs

(defvar *random* (sb-ext:seed-random-state 0)
  "The random state the generators draw from; WITH-SEED binds it.")

(defmacro with-seed ((seed) &body body)
  `(let ((*random* (sb-ext:seed-random-state ,seed)))
     ,@body))

(defun below (n) (if (plusp n) (random n *random*) 0))
(defun chance (p) (< (random 1.0 *random*) p))
(defun pick (sequence) (elt sequence (below (length sequence))))

(defparameter *odd-characters*
  (mapcar #'code-char
          '(#x00A0 #x00AD #x00DF #x00E9 #x0130 #x0131 #x0301 #x0663 #x200B #x202E
            #x2013 #x2014 #x2019 #x2028 #x3000 #xFB00 #xFEFF #xFF11 #xFFFD #xFFFE
            #x1D7CE #x1F600 #x10FFFF #xD800 #xDFFF))
  "Characters that readers trip on: spaces and digits of other scripts,
ligatures, joiners, a lone surrogate.")

(defun random-character (&key latin-1)
  (let ((character
          (case (below 20)
            ((0 1 2 3 4 5 6 7 8) (code-char (+ 32 (below 95))))
            ((9 10) (pick '(#\Space #\Tab #\Newline #\Return #\Nul #\Rubout)))
            ((11 12 13) (pick "[]{}()<>:/\\%&=;\"'#?*+!.,-_~|0123456789"))
            (14 (code-char (+ 128 (below 128))))
            ((15 16) (pick *odd-characters*))
            (17 (code-char (below 32)))
            (t (code-char (below #x110000))))))
    (if (and latin-1 (> (char-code character) 255))
        (code-char (below 256))
        character)))

(defun random-string (&key (length (case (below 10)
                                     ((0 1 2 3 4 5) (below 12))
                                     ((6 7 8) (below 120))
                                     (t (below 1000))))
                           latin-1)
  (let ((string (make-string length)))
    (dotimes (i length string)
      (setf (char string i) (random-character :latin-1 latin-1)))))

(defun token-string (tokens)
  "Up to a dozen of TOKENS, mostly between spaces, with some noise."
  (with-output-to-string (out)
    (dotimes (i (1+ (below 12)))
      (when (plusp i)
        (write-string (if (chance 0.85) " " (pick '("" "  " ", " "-" "/" ":" "." "
"))) out))
      (if (chance 0.15)
          (write-string (random-string :length (below 5)) out)
          (write-string (pick tokens) out)))))

(defun mutation (string tokens)
  "STRING with a few random cuts, insertions, repeats and case changes."
  (let ((string (coerce string '(simple-array character (*)))))
    (dotimes (i (1+ (below 5)) string)
      (let* ((n (length string))
             (a (below (1+ n)))
             (b (min n (+ a (below 8)))))
        (setf string
              (case (below 6)
                (0 (concatenate 'string (subseq string 0 a) (subseq string b)))
                (1 (concatenate 'string (subseq string 0 a) (random-string :length (1+ (below 4)))
                                (subseq string a)))
                (2 (concatenate 'string (subseq string 0 a) (pick tokens) (subseq string a)))
                (3 (concatenate 'string (subseq string 0 b)
                                (format nil "~v@{~A~:*~}" (below 20) (subseq string a b))
                                (subseq string b)))
                (4 (concatenate 'string (subseq string 0 a) (string-upcase (subseq string a b))
                                (subseq string b)))
                (t (subseq string 0 a))))))))

(defun some-string (&key tokens seeds latin-1)
  "A random string, a string of TOKENS, or a mutation of one of SEEDS."
  (let ((string (case (below (if seeds 4 3))
                  (0 (random-string :latin-1 latin-1))
                  (1 (if tokens (token-string tokens) (random-string :latin-1 latin-1)))
                  (2 (if tokens (mutation (token-string tokens) tokens) (random-string :latin-1 latin-1)))
                  (t (mutation (pick seeds) (or tokens '("")))))))
    (if latin-1
        (map 'string (lambda (c) (if (> (char-code c) 255) (code-char (below 256)) c)) string)
        string)))

(defun random-integer ()
  (case (below 6)
    ((0 1) (- (below 40) 20))
    (2 (pick '(0 1 -1 11 12 24 59 60 125 126 127 65535 65536 1440 86400)))
    (3 (pick (list most-positive-fixnum most-negative-fixnum (expt 2 64) (- (expt 10 20)))))
    (t (- (below 2000000) 1000000))))

(defun random-octets (&optional (length (below 300)))
  (let ((octets (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (i length octets)
      (setf (aref octets i) (below 256)))))

(defun without-surrogates (string)
  (remove-if (lambda (c) (<= #xD800 (char-code c) #xDFFF)) string))

(defun utf-8 (string)
  (flexi-streams:string-to-octets (without-surrogates string) :external-format :utf-8))

;;; Checking

(defun show (object)
  "OBJECT as a short line: its strings' controls and non-ASCII as \\u{..}."
  (let ((text (let ((*print-length* 12) (*print-level* 4) (*print-pretty* nil))
                (prin1-to-string object))))
    (with-output-to-string (out)
      (loop for c across (subseq text 0 (min 300 (length text)))
            do (if (<= 32 (char-code c) 126)
                   (write-char c out)
                   (format out "\\u{~X}" (char-code c))))
      (when (> (length text) 300) (write-string "..." out)))))

(defun failures (count generator property &key (seconds 5) allow)
  "Up to three of COUNT inputs from GENERATOR (a list of arguments each) on
which PROPERTY fails, as (inputs problem): it signals an error other than one
ALLOW accepts, exhausts the stack, takes more than SECONDS, or returns a
problem (true) rather than NIL."
  (let ((failures '()))
    (loop repeat count
          while (< (length failures) 3)
          do (let* ((inputs (funcall generator))
                    (problem (handler-case
                                 (sb-ext:with-timeout seconds (apply property inputs))
                               (sb-ext:timeout () "took too long")
                               (storage-condition (c) (string (type-of c)))
                               (error (c)
                                 (unless (and allow (funcall allow c))
                                   (format nil "~S: ~A" (type-of c)
                                           (or (ignore-errors (princ-to-string c)) "?")))))))
               (when problem
                 (push (list inputs problem) failures))))
    (nreverse failures)))

(defmacro holds (description count generator property &rest options)
  "Assert that PROPERTY holds of COUNT inputs from GENERATOR (FAILURES)."
  `(let ((failures (failures ,count ,generator ,property ,@options)))
     (ok (null failures)
         (format nil "~A~@[ -- fails on ~{~A~^; ~}~]" ,description
                 (mapcar #'show failures)))))

;;; Requests

(defparameter *path-tokens*
  '("/" "//" "." ".." "~" "%2F" "assets" "favicon.ico" "clog" "theme" "plp-images" "a.png"
    "slack" "events" "bible" "etc" "passwd" "SYS:SRC;" "\\" ".git" "-" "_" "a.b"))

(defun random-path ()
  (let ((path (some-string :tokens *path-tokens* :latin-1 t
                           :seeds '("/assets/x.css" "/plp-images/a.png" "/slack/events" "/bible/1-1-1" "/a/b/c"))))
    (if (chance 0.8) (concatenate 'string "/" (remove #\Space path)) path)))

(defun headers (&rest names-and-values)
  (let ((table (make-hash-table :test #'equal)))
    (loop for (name value) on names-and-values by #'cddr
          when value do (setf (gethash name table) value))
    table))

(deftest request-paths
  (with-seed (1)
    (let ((root (asdf:system-relative-pathname :bhw "")))
      (holds "a path the guard lets through has no empty, dot or non-ASCII segment"
             20000 (lambda () (list (random-path)))
             (lambda (path)
               (and (main::safe-path-p path)
                    (or (search "//" path) (search "/." path)
                        (find-if (lambda (c) (> (char-code c) 126)) path))
                    "let through")))
      (holds "CLOG sends no file outside its root"
             5000 (lambda () (list (random-path)))
             (lambda (path)
               (let ((file (conn::static-file root path)))
                 (and file
                      (not (uiop:subpathp file (truename root)))
                      "outside the root"))))
      (holds "a path is read a segment at a time, as the regex read it whole"
             20000 (lambda () (list (random-path)))
             (lambda (path)
               (unless (eq (conn:path-segments-p path :trailing-slash t)
                           (and (plusp (length path))
                                (cl-ppcre:scan "^(?:/[A-Za-z0-9_-][A-Za-z0-9_.~-]*)*/?\\z" path)
                                t))
                 "disagrees")))
      (holds "the guard refuses as it says, or lets through"
             20000 (lambda ()
                     (list (list :request-method (pick '(:get :post :head))
                                 :path-info (if (chance 0.05)
                                                (format nil "~v@{~A~:*~}" (below 30000) "/a")
                                                (random-path))
                                 :content-length (and (chance 0.6) (random-integer))
                                 :headers (headers "transfer-encoding"
                                                   (and (chance 0.1) (random-string :latin-1 t))))))
             (lambda (env)
               (let ((response (main::guard-response env))
                     (length (getf env :content-length)))
                 (cond ((and response (not (member (first response) '(400 411 413 414))))
                        "an odd status")
                       ((and (null response) length (> length main::*max-request-octets*))
                        "a body too long let through")
                       ((and (null response) (> (length (getf env :path-info)) main::*max-path-length*))
                        "a path too long let through"))))))))

(deftest the-theme-toggle
  (with-seed (2)
    (flet ((local-p (location)
             (and (stringp location)
                  (uiop:string-prefix-p "/" location)
                  (not (uiop:string-prefix-p "//" location))
                  (notany (lambda (c) (or (< (char-code c) 33) (= (char-code c) 127))) location))))
      (holds "the toggle sends the reader back to a path on this site, and sets a theme"
             20000 (lambda ()
                     (list (list :request-method :post :path-info "/theme"
                                 :headers (headers
                                           "cookie" (some-string :latin-1 t :tokens '("theme=" "dark" "light" "auto" ";" " "))
                                           "referer" (some-string :latin-1 t
                                                                  :tokens '("http:" "https:" "//" "/" "evil.com" "?" "#" "%2F" "@" ":")
                                                                  :seeds '("https://www.bhw.name/bible/1-1-1?x=1" "/a"))))))
             (lambda (env)
               (let ((response (layout:theme-toggle-response env)))
                 (cond ((not (local-p (getf (second response) :location))) "not a local path")
                       ((not (cl-ppcre:scan "^theme=(light|dark|auto);" (getf (second response) :set-cookie)))
                        "no theme"))))))
    (holds "HTML escaping leaves no markup"
           10000 (lambda () (list (some-string :tokens '("<" ">" "&" "\"" "'" "&amp;"))))
           (lambda (text)
             (and (find-if (lambda (c) (find c "<>\"'")) (layout:escape-html text))
                  "markup left")))))

;;; Slack

(deftest slack-requests
  (with-seed (3)
    (let ((bot::*slack-signing-secret* "fuzz"))
      (holds "an unsigned request is refused, whatever its body (400 or 403)"
             10000 (lambda ()
                     (let ((body (if (chance 0.5)
                                     (random-octets)
                                     (utf-8 (some-string :tokens '("token=x" "&" "=" "%" "%E2%82" "{" "}" "\"type\""))))))
                       (when (and (plusp (length body)) (chance 0.3))
                         (setf body (concatenate '(vector (unsigned-byte 8)) body (vector (+ #xC0 (below 64))))))
                       (list (list :request-method :post
                                   :path-info (pick '("/slack/events" "/slack/commands"))
                                   :content-length (if (chance 0.9) (length body) (random-integer))
                                   :raw-body (flexi-streams:make-in-memory-input-stream body)
                                   :headers (headers "x-slack-request-timestamp"
                                                     (princ-to-string (- (get-universal-time) 2208988800))
                                                     "x-slack-signature"
                                                     (and (chance 0.8) (random-string :latin-1 t)))))))
             (lambda (env)
               (let ((response (bot:slack-response env)))
                 (unless (member (first response) '(400 403))
                   (format nil "answered ~A" (first response)))))))
    (holds "a reply's chunks keep to the limit, unless one line is longer, and lose nothing"
           5000 (lambda () (list (some-string :tokens '("
" "

" "a line" "a longer line of a day")) (1+ (below 60))))
           (lambda (text limit)
             (let ((chunks (bot::message-chunks text :limit limit))
                   (lines (cl-ppcre:split "\\n" text)))
               (flet ((letters (s) (count-if-not (lambda (c) (member c '(#\Newline #\Space))) s)))
                 (cond ((some (lambda (chunk) (and (> (length chunk) limit)
                                                   (notany (lambda (line) (search chunk line)) lines)))
                              chunks)
                        "a chunk over the limit")
                       ((/= (letters text) (reduce #'+ chunks :key #'letters))
                        "text lost or doubled"))))))
    (holds "what the bot quotes cannot hold a control sequence"
           5000 (lambda () (list (some-string :tokens '("<!channel>" "<@U1>" "<" ">"))))
           (lambda (text)
             (and (find-if (lambda (c) (find c "<>")) (bot::escape-control-sequences text))
                  "a < or > left")))
    (holds "initials taken from a profile or an argument are two or three capitals"
           10000 (lambda () (list (some-string :tokens '("Dcn." "Matt" "Marín" "(he/him)" "BW" "dmm" "ß" "İ" "my" "initials" "are"))))
           (lambda (text)
             (let ((guesses (list (bot::derive-initials text) (bot::parse-initial-argument text))))
               (and (some (lambda (initials) (and initials (not (cl-ppcre:scan "^[A-Z]{2,3}$" initials))))
                          guesses)
                    (format nil "~S" guesses)))))
    (holds "an answer and a command line are read without error"
           10000 (lambda () (list (some-string :tokens '("y" "yes" "n" "no" "edit" "/book" "/view" ":" "!"))))
           (lambda (text)
             (bot::answer-word text)
             (bot::parse-slash-text text)
             nil))))

;;; Bookings

(defparameter *booking-tokens*
  '("book" "a" "van" "the" "to" "for" "and" "at" "from" "on" "by" "back" "leaving" "until" "-" "–"
    "in" "days" "weeks" "next" "this" "tomorrow" "today" "Monday" "thurs" "Friday" "weekend" "month"
    "May" "Sept" "1st" "22nd" "32nd" "4/22" "12/31" "13/45" "2027" "0" "7" "12" "25" "99999999999999999999"
    "8:30" "24:00" "99:99" "8am" "pm" "a.m." "noon" "midnight" "o'clock" "BW" "MM" "bw" "me" "my" "I"
    "DGH" "LGH" "Chevy" "personal" "dentist" "SHMS" "add" "instead" "<@U1>"))

(defparameter *bookings*
  '("a van for swimming next Tuesday for CG and BW at 7 pm to 9 pm"
    "Can BW and MM take the DGH to the dentist tomorrow, leaving at 8:30 and back around 11?"
    "add GG to the SHMS trip on Thursday" "8 to 10 pm" "Sept 19-22" "this week and next week"))

(defun booking-text () (some-string :tokens *booking-tokens* :seeds *bookings*))

(defun random-cell ()
  (if (chance 0.6)
      ""
      (some-string :tokens '("8:35 am" "*12:30 pm" "4:55" "9am" "TBD" "13:00 pm" "BW" "MM" "SHMS" "Dep." "Chevy")
                   :seeds '("BW" "SHMS" "10:30 am"))))

(defun random-sheet ()
  "A sheet of a few blocks, of the week of April 6, with random cells and
rows put in or taken out by hand."
  (let ((year (nth-value 5 (decode-universal-time (get-universal-time)))))
    (apply #'vb-tests::make-test-sheet
           (loop repeat (1+ (below 3))
                 collect (let* ((day (if (chance 0.8)
                                         (let ((label (bn:format-booking-day
                                                       (bn:parse-day (format nil "April ~D ~D" (+ 6 (below 7)) year)))))
                                           (format nil "~A           ~A" label label))
                                         (random-cell)))
                                (vehicles '(:dgh :lgh :chevy :personal))
                                (block (vb-tests::make-block
                                        day
                                        (loop for v in vehicles when (chance 0.5) append (list v (random-cell)))
                                        (loop for v in vehicles when (chance 0.5)
                                              append (list v (list (random-cell) (random-cell))))
                                        (loop for v in vehicles when (chance 0.5)
                                              append (list v (list (loop repeat (below 4) collect (random-cell))
                                                                   (loop repeat (below 4) collect (random-cell))))))))
                           (loop repeat (below 3)
                                 do (let ((i (below (length block))))
                                      (setf block (if (chance 0.5)
                                                      (append (subseq block 0 i)
                                                              (list (loop repeat 13 collect (random-cell)))
                                                              (nthcdr i block))
                                                      (append (subseq block 0 i) (nthcdr (1+ i) block))))))
                           block)))))

(deftest bookings
  (with-seed (4)
    (holds "a booking request is read without error"
           5000 (lambda () (list (booking-text)
                                 (loop repeat (below 3) collect (pick '("BW" "MM" "DMM" "" "ÉÉ")))))
           (lambda (text initials)
             (bn:parse-booking text :known-initials initials)
             (bn:revise-booking (bn:parse-booking (pick *bookings*)) text :known-initials initials)
             nil))
    (holds "a date range for /view is read, or not, without error"
           5000 (lambda () (list (booking-text)))
           (lambda (text) (vb::date-range-phrase-p text) nil))
    (holds "a time cell is minutes of a day, or none"
           5000 (lambda () (list (random-cell)))
           (lambda (cell)
             (let ((minutes (vb::sheet-clock cell)))
               (and minutes (not (and (integerp minutes) (<= 0 minutes 1439)))
                    (format nil "~S" minutes)))))
    (holds "a sheet is read without error"
           1000 #'(lambda () (list (random-sheet)))
           (lambda (rows) (vb::parse-schedule rows :sheet-name "Apr 6-12") nil))
    (holds "a booking against a sheet is proposed, or said to be incomplete"
           1500 (lambda ()
                  (list (if (chance 0.5)
                            (format nil "~A ~A ~A" (pick *bookings*) (pick '("April 9" "on Apr 10" "4/11" "")) (booking-text))
                            (booking-text))
                        (random-sheet)))
           (lambda (text sheet)
             (let ((vb::*weeks* '("Apr 6-12"))
                   (*standard-output* (make-broadcast-stream)))
               (with-mocked-functions ((vb::read-sheet (lambda (name) (declare (ignore name)) sheet))
                                       (vb::write-range (lambda (&rest arguments)
                                                          (declare (ignore arguments))
                                                          (error "A proposal wrote to the sheet."))))
                 (let ((proposal (vb::propose-natural text :registered-initials '("BW" "MM"))))
                   (when proposal
                     (vb::proposal-day-view proposal)
                     (vb::proposal-trip-view proposal))
                   nil))))
           :allow (lambda (c) (typep c 'bn:booking-incomplete)))
    (holds "/view answers any range without failing"
           1000 (lambda () (list (some-string :tokens *booking-tokens* :seeds '("Apr 6 to Apr 26" "April" "the rest of April"))
                                 (random-sheet)))
           (lambda (text sheet)
             (let ((vb::*weeks* '("Apr 6-12" "Apr 13-19"))
                   (*standard-output* (make-broadcast-stream)))
               (with-mocked-functions ((vb::read-sheet (lambda (name) (declare (ignore name)) sheet)))
                 (let ((reply (bot::cmd-view (headers "text" text))))
                   (and (search "View failed [" (format nil "~{~A~}" (if (listp reply) reply (list reply))))
                        "failed"))))))))

;;; A page's websocket

(defun frames ()
  "Octets of a few client frames, mostly well formed, some broken or cut."
  (let ((out (flexi-streams:make-in-memory-output-stream)))
    (loop repeat (1+ (below 5))
          do (if (chance 0.15)
                 (write-sequence (random-octets (below 20)) out)
                 (let* ((opcode (pick '(0 1 1 1 2 8 9 10 3 11)))
                        (payload (if (chance 0.7)
                                     (utf-8 (some-string :seeds '("E:12:click" "M:x hello" "P:3" "5:answer")))
                                     (random-octets)))
                        (length (length payload))
                        (mask (random-octets 4)))
                   (write-byte (logior (if (chance 0.8) #x80 0) (if (chance 0.05) #x40 0) opcode) out)
                   (let ((masked (if (chance 0.97) #x80 0)))
                     (cond ((and (< length 126) (chance 0.9)) (write-byte (logior masked length) out))
                           ((and (< length 65536) (chance 0.8))
                            (write-byte (logior masked 126) out)
                            (write-byte (ldb (byte 8 8) length) out)
                            (write-byte (ldb (byte 8 0) length) out))
                           (t (write-byte (logior masked 127) out)
                              (let ((declared (if (chance 0.9) length (below (expt 2 64)))))
                                (loop for shift from 56 downto 0 by 8
                                      do (write-byte (ldb (byte 8 shift) declared) out))))))
                   (write-sequence mask out)
                   (dotimes (i length)
                     (write-byte (logxor (aref payload i) (aref mask (logand i 3))) out)))))
    (let ((octets (flexi-streams:get-output-stream-sequence out)))
      (if (chance 0.1) (subseq octets 0 (below (1+ (length octets)))) octets))))

(defun in-memory-websocket (octets)
  (make-instance 'conn::websocket
                 :stream (make-two-way-stream (flexi-streams:make-in-memory-input-stream octets)
                                              (flexi-streams:make-in-memory-output-stream))))

(deftest a-pages-websocket
  (with-seed (5)
    (holds "frames are read as messages, or end with what SERVE-WEBSOCKET handles"
           10000 (lambda () (list (frames)))
           (lambda (octets)
             (let ((websocket (in-memory-websocket octets)))
               (handler-case
                   (loop repeat 50
                         until (eq :close (conn::read-message websocket)))
                 ((or conn::websocket-error end-of-file stream-error) ()))
               nil)))
    (let ((connection (conn::make-connection nil (list :path "/fuzz"))))
      (unwind-protect
           (let ((websocket (in-memory-websocket #())))
             (setf (conn::connection-websocket connection) websocket)
             (conn:mirror (conn::connection-id connection) "CLOG1" "")
             (holds "a message from the page is taken or ignored"
                    10000 (lambda () (list (some-string :tokens '("E:" "M:" "P:" "0" "1" "!" " " "CLOG1" ":click" "-1" "12345678901234567890")
                                                        :seeds '("E:CLOG1:click 3 4" "M:CLOG1 value" "P:17" "3:answer" "4!error"))))
                    (lambda (message)
                      (conn::receive connection websocket message)
                      nil)))
        (conn::close-connection connection)))
    (holds "a message to the page is a JSON array of its scripts"
           5000 (lambda () (list (loop repeat (below 4) collect (without-surrogates (some-string)))))
           (lambda (scripts)
             (let ((parsed (com.inuoe.jzon:parse (conn::message-text scripts))))
               (unless (equal scripts (coerce parsed 'list))
                 "not the scripts sent"))))
    (let ((conn::*boot-data* (make-hash-table :test #'equal))
          (conn::*boot-data-size* 0)
          (conn::*boot-data-limit* 10000))
      (holds "what forms posted is kept within its limit"
             3000 (lambda () (list (below 3000) (chance 0.3)))
             (lambda (size take-p)
               (let ((token (conn::keep-boot-data (list :post-data "x") size)))
                 (when (and token take-p)
                   (conn::take-boot-data token))
                 (and (> conn::*boot-data-size* conn::*boot-data-limit*)
                      "over the limit")))))))

;;; Songs

(defparameter *lyric-words*
  '("abide" "with" "me" "fast" "falls" "the" "eventide" "darkness" "deepens" "Lord" "help" "of"
    "helpless" "O" "swift" "to" "its" "close" "ebbs" "out" "life's" "little" "day" "earth's" "joys"
    "grow" "dim" "glories" "pass" "away" "change" "and" "decay" "in" "all" "around" "I" "see")
  "Words of a lyric line: none a chord, a label or a header.")

(defun lyric-line ()
  (with-output-to-string (out)
    (dotimes (i (1+ (below 7)))
      (when (plusp i) (write-char #\Space out))
      (when (chance 0.2)
        (format out "[~A]" (pick '("G" "D/F#" "Em" "C" "Bm7" "Asus4" "A7" "N.C." "Bb" "F#m"))))
      (write-string (pick *lyric-words*) out)
      (when (chance 0.1) (write-string (pick '("," ";" "." "!" "?")) out)))
    (when (chance 0.1)
      (write-string (pick '(" (x2)" " (Ref.)" " (repeat)" " (V1, V2, Ref.)")) out))))

(defun well-formed-song ()
  "A song text as a person writes one: headers, then sections under labels
or none, lines of words with chords in brackets or over them, and marks."
  (with-output-to-string (out)
    (format out "Title: ~A~%" (lyric-line))
    (when (chance 0.5) (format out "Writers: ~A~%" (pick '("Henry Francis Lyte" "W. H. Monk"))))
    (when (chance 0.3) (format out "Order: ~{~A~^ ~}~%" (loop repeat (1+ (below 6)) collect (pick '("1" "2" "R" "B" "T")))))
    (when (chance 0.3) (format out "Tags: ~{~A~^; ~}~%" (loop repeat (1+ (below 3)) collect (pick '("mass=Communion" "tempo=Slow" "liturgy=Christmas {Dec 25}" "key=G")))))
    (terpri out)
    (dotimes (i (1+ (below 6)))
      (terpri out)
      (when (chance 0.7)
        (format out "~A~%" (pick '("1." "2." "3." "Refrain:" "Chorus" "[Bridge]" "Tag:" "Pre-Chorus:"
                                    "Verse 4" "Ending:" "Refrain 2:" "Chorus (x2)" "Bridge:"))))
      (dotimes (j (1+ (below 8)))
        (when (chance 0.1)
          (format out "  ~{~A~^   ~}~%" (loop repeat (1+ (below 4)) collect (pick '("G" "D" "Em" "C" "A7")))))
        (format out "~A~%" (lyric-line))))))

(defun section-shape (song)
  "SONG's sections with each plist's keys in order, for comparing."
  (mapcar (lambda (section)
            (sort (loop for (key value) on section by #'cddr collect (cons key value))
                  #'string< :key (lambda (pair) (symbol-name (car pair)))))
          (song:song-sections song)))

(defparameter *song-tokens*
  '("Title:" "Order:" "Tags:" "1." "2." "Refrain:" "Chorus" "[Chorus]" "Bridge:" "Tag:" "Intro:" "(x2)"
    "(V1, V2, Ref.)" "(Repeat last two lines)" "[G]" "[D/F#]" "[" "]" "G" "D/F#" "Em" "G-D" "Amazing" "grace"
    "
" "

" "  " "mass=Communion" "{" "}" "=" ";" "(" ")"))

(deftest songs
  (with-seed (6)
    (holds "a song written out reads back as it was"
           3000 (lambda () (list (well-formed-song)))
           (lambda (text)
             (let* ((song (song:parse-song text))
                    (printed (song:print-song song))
                    (again (song:parse-song printed)))
               (cond ((not (equalp (list (song:song-title song) (song:song-order song) (song:song-tags song)
                                         (section-shape song))
                                   (list (song:song-title again) (song:song-order again) (song:song-tags again)
                                         (section-shape again))))
                      (format nil "reads back otherwise: ~A" (show printed)))
                     ((string/= printed (song:print-song again)) "is not written the same twice")))))
    (holds "any text is read as a song, outlined, and laid out in slides that a key finds"
           5000 (lambda () (list (some-string :tokens *song-tokens* :seeds (list (well-formed-song) (well-formed-song)))))
           (lambda (text)
             (let* ((song (song:parse-song text))
                    (deck (song:song-deck song)))
               (song:song-problems song)
               (multiple-value-bind (parts places) (song:outline text)
                 (song:outline-song parts places :title "T")
                 (cond ((notevery (lambda (place) (< -1 place (length parts))) places)
                        "a place outside the parts")
                       ((some (lambda (group)
                                (let ((index (song:deck-jump deck (and deck (below (length deck))) group)))
                                  (and index (not (< -1 index (length deck))))))
                              '("1" "2" "R" "C" "B" "P" "T" "O"))
                        "a jump outside the deck")
                       ((some (lambda (slide)
                                (let ((index (song:deck-next deck (below (length deck)) slide)))
                                  (not (and index (song:same-slide-p slide (nth index deck))))))
                              (song:deck-chart deck))
                        "a slide not found where it stands"))))))
    (holds "a tag reads as the regex read it"
           10000 (lambda () (list (some-string :tokens '("mass" "=" " " "{" "}" "Dec 25" "Communion" "-")
                                               :seeds '("liturgy=Christmas {Dec 25}" "mass=Communion"))))
           (lambda (text)
             (let ((reference (cl-ppcre:register-groups-bind (category name note)
                                  ("^\\s*([A-Za-z-]+)\\s*=\\s*(.*?)\\s*(?:\\{(.*)\\})?\\s*$" text)
                                (when (plusp (length name))
                                  (list (intern (string-upcase category) :keyword) name note)))))
               (unless (equal reference (song:parse-tag text))
                 (format nil "~S, not ~S" (song:parse-tag text) reference)))))
    (holds "a chord moved keeps its notes, and moved by an octave is itself"
           10000 (lambda () (list (some-string :tokens '("A" "B" "C" "D" "E" "F" "G" "#" "b" "m" "maj" "7" "sus" "/" "-" "N.C." "(")
                                               :seeds '("G" "D/F#" "C#m7b5" "Bb/D"))
                                  (- (below 50) 25)
                                  (chance 0.5)))
           (lambda (chord semitones flats)
             (let ((moved (song:transpose-chord chord semitones :flats flats)))
               (flet ((notes (c) (length (cl-ppcre:all-matches-as-strings "[A-G][#b]?" c))))
                 (cond ((and (not (cl-ppcre:scan "^\\(?N\\.?C" chord)) (/= (notes chord) (notes moved)))
                        "a note gained or lost")
                       ((string/= chord (song:transpose-chord chord 12 :flats flats))
                        "an octave changed it"))))))))

(deftest song-texts-at-the-consoles-limit
  ;; The console reads a field's text up to *MAX-SONG-TEXT* characters, and
  ;; then everything it does with it has to be quick: before, words of a few
  ;; thousand parts took minutes, and a line of marks a hundred characters
  ;; long hours.
  (let ((limit console::*max-song-text*))
    (flet ((filled (piece)
             ;; PIECE's strings, one after another, LIMIT characters of them.
             (console::bounded (with-output-to-string (out)
                                 (loop for i from 0
                                       for written = 0 then (+ written (length string))
                                       for string = (funcall piece i)
                                       while (< written limit)
                                       do (write-string string out)))
                               limit)))
      (dolist (text (list (filled (lambda (i) (format nil "line ~D a~%line ~D b~%~%" i i)))
                          (filled (lambda (i) (format nil "~A~%x~D~%~%" (nth (mod i 6) '("Verse" "Chorus" "Bridge" "Tag" "1." "Refrain:")) i)))
                          (filled (lambda (i) (declare (ignore i)) "(V2 "))
                          (filled (lambda (i) (declare (ignore i)) (format nil "a~%")))
                          (concatenate 'string "Tags: a=" (filled (lambda (i) (declare (ignore i)) "{")))
                          (concatenate 'string "Order: " (filled (lambda (i) (format nil "~D " (mod i 99)))))
                          (filled (lambda (i) (declare (ignore i)) (format nil "G D Em~%words here~%")))
                          (filled (lambda (i) (format nil "~D.~%a~%~%" (mod i 99))))))
        (ok (null (failures 1 (lambda () (list text))
                      (lambda (text)
                        (let ((song (song:parse-song text)))
                          (song:song-problems song)
                          (song:song-deck song)
                          (song:print-song song)
                          (multiple-value-bind (parts places) (song:outline text)
                            (song:outline-song parts places)))
                        nil)
                      :seconds 5))
            (format nil "quick on ~A" (show (subseq text 0 40))))))))

;;; A room, driven as its pages drive it

(defun room-problem (room)
  (let* ((state (live:snapshot room))
         (deck (getf state :deck)) (index (getf state :index))
         (items (getf state :items)) (position (getf state :position)))
    (cond ((not (and (integerp index) (or (null deck) (< -1 index (length deck)))))
           (format nil "index ~S of a deck of ~D" index (length deck)))
          ((and position (not (< -1 position (length items))))
           (format nil "position ~S of ~D items" position (length items)))
          ((not (<= 0 (getf state :transposition) 11)) "transposition")
          ((not (member (getf state :mode) '(:lyrics :black :waiting))) "mode"))))

(defun press (room ids)
  "Do to ROOM one thing that its console or screen does."
  (case (below 15)
    (0 (live:press-key room (if (chance 0.8)
                                (pick '(" " "ArrowRight" "ArrowLeft" "PageUp" "PageDown" "1" "2" "9"
                                        "r" "R" "c" "b" "p" "t" "o" "." "w" "W" "n" "N" "x" "" "Enter"))
                                (random-string :length (below 3)))
                       :shift (chance 0.3)))
    (1 (live:transpose room (pick '(-1 1 nil))))
    (2 (live:go-to room (- (below 40) 5)))
    (3 (live:step-slide room (pick '(-1 1))))
    (4 (live:jump room (pick '("1" "2" "R" "C" "B" "T" "x"))))
    (5 (live:set-items room (loop repeat (below 6) collect (cons (if (chance 0.9) (pick ids) "custom/none") ""))
                       :transpositions (loop repeat (below 3) collect (cons (pick ids) (pick (list (random-integer) 3 nil))))))
    (6 (live:add-item room (pick ids) :after-current (chance 0.5)))
    (7 (live:move-item room (- (below 8) 1) (pick '(-1 1))))
    (8 (live:remove-item room (- (below 8) 1)))
    (9 (live:show-item room (- (below 8) 1)))
    (10 (live:step-song room (pick '(-1 1))))
    (11 (live:set-mode room (pick '(:black :waiting))))
    (12 (live:show-song room (pick ids)))
    (13 (live:set-end-time room (live:parse-clock (pick '("20:00" "8:05" "x")) (get-universal-time) 0)))
    (t (live:set-waiting-text room (random-string)))))

(deftest a-room-and-the-store
  (with-seed (7)
    (with-temp-data-root ()
      (let ((*standard-output* (make-broadcast-stream))
            (*error-output* (make-broadcast-stream)))
        (db:open-database)
        (unwind-protect
             (let ((ids '()))
               (holds "a song of any text is stored and read back, as STORABLE has it"
                      150 (lambda () (list (format nil "Title: ~A~%~%~A" (below 100000)
                                                   (some-string :tokens *song-tokens* :seeds (list (well-formed-song))))))
                      (lambda (text)
                        (let* ((song (song:parse-song text))
                               (id (db:save-song song)))
                          (push id ids)
                          (unless (equalp (store:storable (song:song-sections song))
                                          (song:song-sections (db:find-song id)))
                            "read back otherwise"))))
               (db:close-database)
               (db:open-database)
               (ok (every #'db:find-song ids) "the store opens again with them all")
               (holds "a room keeps to its deck and its setlist, whatever is pressed"
                      300 (lambda () (list (1+ (below 40))))
                      (lambda (steps)
                        (let ((room (live:ensure-room (format nil "FUZZ~D" (below 1000000)))))
                          (loop repeat steps
                                do (press room ids)
                                thereis (room-problem room)))))
               (holds "a clock time is the next time the clock shows it"
                      5000 (lambda () (list (some-string :tokens '("20:00" "8:05" "8.05" "8h05" "24:00" "23:60" " " "٢٠:٠٠"))
                                            (get-universal-time) (- (below 1800) 900)))
                      (lambda (text now offset)
                        (let ((at (live:parse-clock text now offset)))
                          (and at (not (< now at (+ now 86401))) "not within the next day")))))
          (db:close-database))))))
