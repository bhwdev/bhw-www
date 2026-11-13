;;; SPDX-FileCopyrightText: 2026  Ben H. W. <foss@bhw.name>
;;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Guix package of bhw-www, and of the dependency Guix lacks
;;; (graph-db).  This file evaluates to the package:
;;;
;;;   guix build -f guix.scm       compiles, tests, and saves bin/bhw-www
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

(define-public sbcl-graph-db
  (let ((commit "7bc038f589ef6512486e30e99b6c3d4581fc8419") ; v4.1.0
        (revision "0"))
    (package
      (name "sbcl-graph-db")
      (version (git-version "4.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kraison/vivace-graph")
               (commit commit)))
         (file-name (git-file-name "cl-graph-db" version))
         (sha256
          (base32 "0r3325z0aw64dg4cbbxd0659k0956pyg3r3q7xaqkp5zaiihwksk"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; The engine and its replication transport only.  The full
       ;; "graph-db" system adds the HTTP API (rest.lisp: ningle on a
       ;; Clack server of its own), which bhw-www does not serve.
       (list #:asd-systems ''("graph-db/core" "graph-db/replication")
             ;; The test systems need FiveAM, Drakma and the full system.
             #:tests? #f))
      (inputs
       (list sbcl-alexandria
             sbcl-bordeaux-threads
             sbcl-cffi
             sbcl-cl-fad
             sbcl-cl-json
             sbcl-cl-ppcre
             sbcl-cl-store
             sbcl-ieee-floats
             sbcl-iterate
             sbcl-local-time
             sbcl-log4cl
             sbcl-md5
             sbcl-split-sequence
             sbcl-usocket
             sbcl-uuid))
      (home-page "https://github.com/kraison/vivace-graph")
      (synopsis "Graph database in Common Lisp (VivaceGraph)")
      (description
       "VivaceGraph is an ACID-compliant object graph database written in
Common Lisp, with user-defined indexes, map-reduce views, replication and a
Prolog-like query language.")
      (license license:expat))))

;; Guix's CFFI writes the build's GCC into toolchain/c-toolchain.lisp, for
;; groveling, which keeps all of GCC (about 260 MiB) in the closure of any
;; program that loads CFFI, though nothing in this one compiles C.  This
;; names the program instead, which CFFI then finds in PATH (`guix shell -D'
;; has it).  Grafted (see WITH-CFFI/NO-GCC), which a replacement allows
;; only if it has every file of the original: this one differs in one line.
(define sbcl-cffi/no-gcc
  (package
    (inherit sbcl-cffi)
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-cffi)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-before 'copy-source 'unpin-gcc
              (lambda _
                (substitute* "toolchain/c-toolchain.lisp"
                  (("\"/gnu/store/[^\"]*/bin/gcc\"") "\"gcc\""))))
            ;; SUBSTITUTE* is silent when its pattern matches nothing.
            (add-after 'create-asdf-configuration 'check-no-gcc
              (lambda _
                (let ((gcc (dirname (dirname (which "gcc")))))
                  (when (zero? (system* "grep" "-rqF" gcc #$output))
                    (error "the output still refers to" gcc)))))))))))

;; A package with its dependencies on SBCL-CFFI/NO-GCC, grafted.  The graft
;; keeps the derivations, and so the substitutes, of the store items built on
;; Guix's CFFI, and copies them with their references rewritten to SBCL-CFFI/
;; NO-GCC; a build with `--no-grafts' brings Guix's CFFI, and GCC, back.
(define with-cffi/no-gcc
  (let ((cffi (package
                (inherit sbcl-cffi)
                (replacement sbcl-cffi/no-gcc))))
    (package-mapping (lambda (package)
                       (if (eq? package sbcl-cffi) cffi package))
                     (lambda (package)
                       (eq? package sbcl-cffi))
                     #:deep? #t)))

(define-public bhw-www
  (with-cffi/no-gcc
   (package
     (name "bhw-www")
     (version "2.0.0")
     (source (local-file "." "bhw-www-checkout"
                         #:recursive? #t
                         #:select? vcs-file?))
     (build-system asdf-build-system/sbcl)
     (arguments
      (list
       #:asd-systems ''("bhw")
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
                (string-append #$output "/bin/bhw-www")
                outputs
                #:dependencies '("bhw")
                #:entry-program '((bhw/main:application-toplevel))))))))
     (native-inputs
      (list sbcl-rove))
     (inputs
      ;; With those of the fork of CLOG in clog/ (clog.asd): cl-base64,
      ;; closer-mop, hunchentoot, parse-float, trivial-gray-streams.
      (list sbcl-alexandria
            sbcl-bordeaux-threads
            sbcl-cffi
            sbcl-cl+ssl
            sbcl-cl-base64
            sbcl-cl-ppcre
            sbcl-closer-mop
            sbcl-dexador
            sbcl-flexi-streams
            sbcl-graph-db
            sbcl-hunchentoot
            sbcl-ironclad
            sbcl-jzon
            sbcl-local-time
            sbcl-montezuma
            sbcl-parse-float
            sbcl-quri
            sbcl-slynk
            sbcl-trivial-gray-streams
            sbcl-usocket))
     (home-page "https://www.bhw.name/blog/bhw-www-doc.html")
     (synopsis "Personal web application")
     (description
      "bhw-www is a personal web application: a website, a Douay-Rheims
Bible reader with search, and a Slack bot that books vans, served by one
process.")
     (license license:agpl3+))))

bhw-www
