;;;; SPDX-FileCopyrightText: 2021  Ben H. W. <foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(asdf:defsystem "bhw"
  :name "bhw-www"
  :author " Ben H. W.<foss@bhw.name>"
  :maintainer " Ben H. W.<foss@bhw.name>"
  :description "Personal Web Application"
  :long-description
  "bhw-www is a personal web application: a website, a Douay-Rheims
Bible reader and a Slack bot that books vans, served by one process."
  :license  "GNU Affero General Public License 3.0 or later"
  :homepage "https://www.bhw.name/blog/bhw-www-doc.html"
  :bug-tracker "https://github.com/bhwdev/bhw-www/issues"
  :source-control (:git "https://github.com/bhwdev/bhw-www.git")
  :class :package-inferred-system
  :pathname "src"
  :depends-on ("bhw/main")
  :in-order-to ((asdf:test-op (asdf:test-op "bhw/tests"))))

(register-system-packages "clog" '(:clog-connection :clog-gui))
;; The engine and its replication transport, without the HTTP API that the
;; full "graph-db" system adds (guix.scm builds only these two).
(register-system-packages "graph-db/replication" '(:graph-db))

(asdf:defsystem "bhw/tests"
  :class :package-inferred-system
  :pathname "src/tests"
  :depends-on ("rove"
               "bhw/tests/clog"
               "bhw/tests/main"
               "bhw/tests/bible"
               "bhw/tests/slack-bot/nlp"
               "bhw/tests/slack-bot/booking-nlp"
               "bhw/tests/slack-bot/van-booking"
               "bhw/tests/slack-bot/bot")
  ;; Run the suite of each test package above by name: ROVE:RUN finds a
  ;; system's suites through the fasls it loaded, which it cannot map back to
  ;; their packages under Guix's output translations, and so runs none in the
  ;; guix.scm build.  And unlike ROVE:RUN, fail the operation when a test
  ;; fails, so that the build stops there.
  :perform (asdf:test-op (op c)
                         (let ((failed
                                 (loop :for name :in (asdf:system-depends-on c)
                                       :when (and (uiop:string-prefix-p "bhw/tests/" name)
                                                  (not (uiop:symbol-call
                                                        :rove :run-suite
                                                        (intern (string-upcase name) :keyword))))
                                         :collect name)))
                           (when failed
                             (error "Failed tests: ~{~A~^, ~}" failed)))))
