;;; SPDX-FileCopyrightText: 2026  Ben H. W. <foss@bhw.name>
;;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Guix package of Project Isidore.
;;; It needs a Guix whose sbcl-clog is 2.4 or later (Guix commit fb6e77f):
;;; src/main.lisp passes INITIALIZE a :LACK-MIDDLEWARE-LIST, which 1.2 lacks.
;;; This file evaluates to the package:
;;;
;;;   guix build -f guix.scm       compiles, tests, and saves bin/project-isidore
;;;   guix shell -D -f guix.scm    development environment: SBCL and the inputs
;;;
;;; Production builds it with the host's own Guix: the deploy hook that
;;; .github/workflows/deploy.yml runs installs it with `guix package -f'.
;;; `guix time-machine --commit=COMMIT --' before any of these uses that
;;; Guix; `ssh oci-a1-flex guix describe' names the host's.

(use-modules (guix packages)
             (guix gexp)
             (guix git-download)
             (guix utils)
             (guix build-system asdf)
             ((guix licenses) #:prefix license:)
             (gnu packages lisp-check)
             (gnu packages lisp-xyz))

(define vcs-file?
  ;; Only the files under version control, so that build products and
  ;; editor files in the checkout do not change the source hash.
  (or (git-predicate (current-source-directory))
      (const #t)))

(define-public project-isidore
  (package
    (name "project-isidore")
    (version "2.0.0")
    (source (local-file "." "project-isidore-checkout"
                        #:recursive? #t
                        #:select? vcs-file?))
    (build-system asdf-build-system/sbcl)
    (arguments
     (list
      #:asd-systems ''("project-isidore")
      #:phases
      #~(modify-phases %standard-phases
          ;; 'build compiled the system and 'check ran its tests, each in a
          ;; process of its own; this one loads the system alone and saves
          ;; the executable.  Loading it starts nothing and opens nothing:
          ;; INITIALIZE-APPLICATION does that when the program runs.  (The
          ;; name must be lower case: BUILD-PROGRAM names an ASDF system
          ;; after it.)
          (add-after 'create-asdf-configuration 'build-program
            (lambda* (#:key outputs #:allow-other-keys)
              (build-program
               (string-append #$output "/bin/project-isidore")
               outputs
               #:dependencies '("project-isidore")
               #:entry-program '((project-isidore/main:application-toplevel))))))))
    (native-inputs
     (list sbcl-rove))
    (inputs
     (list sbcl-alexandria
           sbcl-bordeaux-threads
           sbcl-cffi
           sbcl-cl+ssl
           sbcl-cl-ppcre
           sbcl-cl-sqlite
           sbcl-clog
           sbcl-dexador
           sbcl-flexi-streams
           sbcl-ironclad
           sbcl-jzon
           sbcl-local-time
           sbcl-quri
           sbcl-slynk
           sbcl-usocket))
    (home-page "https://www.bhw.name/blog/project-isidore-doc.html")
    (synopsis "Personal web application")
    (description
     "Project Isidore is a personal web application: a website, a Douay-Rheims
Bible reader with search, and a Slack bot that books vans, served by one
process.")
    (license license:agpl3+)))

project-isidore
