;;; SPDX-FileCopyrightText: 2026  Ben H. W. <foss@bhw.name>
;;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; Guix package of bhw-www, and of the dependencies Guix lacks (graph-db,
;;; pure-tls, cl-cancel, precise-time).  This file evaluates to the package:
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
             (guix build-system gnu)
             ((guix licenses) #:prefix license:)
             (gnu packages lisp)
             (gnu packages lisp-check)
             (gnu packages lisp-xyz)
             (gnu packages base)
             (gnu packages perl))

(define vcs-file?
  ;; Only the files under version control, so that build products and
  ;; editor files in the checkout do not change the source hash.
  (or (git-predicate (current-source-directory))
      (const #t)))

(define-public sbcl-graph-db
  (package
    (name "sbcl-graph-db")
    (version "5.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kraison/vivace-graph")
             (commit (string-append "v" version))))
       (file-name (git-file-name "cl-graph-db" version))
       (sha256
        (base32 "0dhdnjl1ksdhn097kimp8jjhxhnglqrvr2yggm22hiys2n3ckmgi"))))
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
    (license license:expat)))

(define-public sbcl-precise-time
  (let ((commit "93ea6aea74aa2ea8860e8581e67997636320e841")
        (revision "0"))
    (package
      (name "sbcl-precise-time")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://codeberg.org/shinmera/precise-time")
               (commit commit)))
         (file-name (git-file-name "cl-precise-time" version))
         (sha256
          (base32 "0gj98hvmzvay27rbdjnz65izcs0dr0rq1g11aac04c0w12xp1rq5"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; The test system needs Parachute.
       (list #:tests? #f))
      (inputs
       (list sbcl-cffi
             sbcl-documentation-utils
             sbcl-trivial-features))
      (home-page "https://shinmera.com/docs/precise-time/")
      (synopsis "Precise time measurements for Common Lisp")
      (description
       "This library reads the system's monotonic and real-time clocks with
the precision they offer, beyond @code{get-internal-real-time}.")
      (license license:zlib))))

(define-public sbcl-cl-cancel
  ;; After 0.1.0: deadlines kept rational when given as a float, and the
  ;; stream monitor's thread reaped.
  (let ((commit "6e0db3c2fc5012661ef536f138164c1eb270dc52")
        (revision "0"))
    (package
      (name "sbcl-cl-cancel")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/atgreen/cl-cancel")
               (commit commit)))
         (file-name (git-file-name "cl-cancel" version))
         (sha256
          (base32 "0njbkijjac7qj150wcjdwl9924rqffhd9a88vyhkgg5vapwlvxd3"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       ;; The test system needs FiveAM, and its TEST-OP does not fail when
       ;; a test does.
       (list #:asd-systems ''("cl-cancel")
             #:tests? #f))
      (inputs
       (list sbcl-atomics
             sbcl-bordeaux-threads
             sbcl-precise-time))
      (home-page "https://github.com/atgreen/cl-cancel")
      (synopsis "Cancellation, deadlines and timeouts for Common Lisp")
      (description
       "cl-cancel propagates cancellation through a tree of contexts, with
deadlines and timeouts, and closes streams whose context is cancelled.")
      (license license:expat))))

;; The TLS of bhw-www's HTTP client (Drakma): see WITH-DEPENDENCY-VARIANTS.
;; It provides the system "cl+ssl" too, its compatibility layer, so that it
;; replaces cl+ssl (and OpenSSL) for the programs that load that.
(define-public sbcl-pure-tls
  (let ((commit "401d59f088478ee4ee0a79df9059e059aaa44859")
        (revision "0"))
    (package
      (name "sbcl-pure-tls")
      (version (git-version "1.15.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/atgreen/pure-tls")
               (commit commit)))
         (file-name (git-file-name "cl-pure-tls" version))
         (sha256
          (base32 "1x5pz12l14m307lj7ca0dcqh4wps2x7pq7q04vpgcvca11b3yfwi"))))
      (build-system asdf-build-system/sbcl)
      (arguments
       (list
        #:asd-systems ''("pure-tls" "pure-tls/cl+ssl-compat" "cl+ssl")
        ;; The test system needs iparse, which Guix lacks, and the network.
        #:tests? #f
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'add-cl+ssl-system
              (lambda _
                (call-with-output-file "cl+ssl.asd"
                  (lambda (port)
                    (display "(asdf:defsystem \"cl+ssl\"
  :description \"cl+ssl's API, from pure-tls\"
  :depends-on (\"pure-tls/cl+ssl-compat\"))
" port))))))))
      (inputs
       (list sbcl-alexandria
             sbcl-bordeaux-threads
             sbcl-cl-base64
             sbcl-cl-cancel
             sbcl-flexi-streams
             sbcl-idna
             sbcl-ironclad
             sbcl-trivial-features
             sbcl-trivial-gray-streams
             sbcl-usocket))
      (home-page "https://github.com/atgreen/pure-tls")
      (synopsis "TLS 1.3 in Common Lisp")
      (description
       "pure-tls implements TLS 1.3, client and server, in Common Lisp, on
Ironclad's cryptography: Lisp streams over any stream, with certificate
verification against the system's trust store, and a compatibility layer
with cl+ssl's API.")
      (license license:expat))))

;; Hunchentoot without its SSL acceptor, and so without cl+ssl: in
;; production Caddy terminates TLS.  Hunchentoot's system loads cl+ssl unless
;; :HUNCHENTOOT-NO-SSL is a feature when ASDF reads it, in every image that
;; loads it, so its .asd adds the feature.
(define sbcl-hunchentoot/no-ssl
  (package
    (inherit sbcl-hunchentoot)
    (arguments
     (substitute-keyword-arguments (package-arguments sbcl-hunchentoot)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'add-no-ssl-feature
              (lambda _
                (substitute* "hunchentoot.asd"
                  (("^\\(defsystem :hunchentoot\n" defsystem)
                   (string-append "(pushnew :hunchentoot-no-ssl *features*)\n\n"
                                  defsystem)))
                ;; SUBSTITUTE* is silent when its pattern matches nothing.
                (invoke "grep" "-q" "^(pushnew :hunchentoot-no-ssl"
                        "hunchentoot.asd")))))))
    (inputs
     (modify-inputs (package-inputs sbcl-hunchentoot)
       (delete "sbcl-cl+ssl")))))

;; Guix's CFFI writes the build's GCC into toolchain/c-toolchain.lisp, for
;; groveling, which keeps all of GCC (about 260 MiB) in the closure of any
;; program that loads CFFI, though nothing in this one compiles C.  This
;; names the program instead, which CFFI then finds in PATH (`guix shell -D'
;; has it).  Grafted (see WITH-DEPENDENCY-VARIANTS), which a replacement
;; allows only if it has every file of the original: this one differs in one
;; line.
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

;; A package with its dependencies on these variants, through the whole
;; graph:
;;
;; - SBCL-CFFI/NO-GCC, grafted.  The graft keeps the derivations, and so the
;;   substitutes, of the store items built on Guix's CFFI, and copies them
;;   with their references rewritten to SBCL-CFFI/NO-GCC; a build with
;;   `--no-grafts' brings Guix's CFFI, and GCC, back.
;;
;; - SBCL-PURE-TLS for cl+ssl, rebuilt (Drakma, and what depends on it).  It
;;   cannot be grafted, having none of cl+ssl's files, and it must replace
;;   cl+ssl everywhere: each package's ASDF configuration includes those of
;;   all its inputs, native ones too, so one left on cl+ssl would register
;;   the real cl+ssl beside pure-tls's.
;;
;; - SBCL-HUNCHENTOOT/NO-SSL for Hunchentoot, rebuilt.
;;
;; One mapping: a second, over the first's result, would meet copies of
;; Guix's packages, not the packages its EQ? tests name.
(define with-dependency-variants
  (let ((cffi (package
                (inherit sbcl-cffi)
                (replacement sbcl-cffi/no-gcc))))
    (package-mapping (lambda (package)
                       (cond ((eq? package sbcl-cffi) cffi)
                             ((eq? package sbcl-cl+ssl) sbcl-pure-tls)
                             ((eq? package sbcl-hunchentoot)
                              sbcl-hunchentoot/no-ssl)
                             (else package)))
                     (lambda (package)
                       (eq? package sbcl-cffi))
                     #:deep? #t)))

;;; ACL2, the theorem prover, from the commit that the formal/ books were
;;; certified against.  Not in Guix.  The books the formal/ books include
;;; (arithmetic-5) are certified here, in the build, since a store item is
;;; read-only.  Build: `guix build -f guix.scm' does not build it (only the
;;; last expression is the package); see formal/README.org.
(define-public acl2
  (package
    (name "acl2")
    (version "8.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/acl2/acl2")
             (commit "c8f2e17b43501363bb3e9c2a954b53f3f4d37d83")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19h0rap14n3a23mg576wjfrxsl7gjsz2cb6yawndsxif3hwa7fjx"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'build
            (lambda* (#:key inputs #:allow-other-keys)
              ;; The checkout has no git metadata for the startup banner.
              (setenv "ACL2_SNAPSHOT_INFO" "c8f2e17 (Guix checkout)")
              (invoke "make" "LISP=sbcl"
                      (string-append "SHELL=" (which "sh")))
              ;; The launcher saved_acl2 is a shell script, #!/bin/sh.
              (patch-shebang "saved_acl2")))
          (replace 'check
            (lambda* (#:key inputs #:allow-other-keys)
              ;; The build's HOME is not writable; ACL2 writes there.
              (setenv "HOME" (getcwd))
              ;; build/cert.pl certifies one book and its dependencies.
              (with-directory-excursion "books"
                ;; cert.pl needs the generated certdep files first.
                (invoke "make" "build/Makefile-features"
                        (string-append "ACL2=" (getcwd) "/../saved_acl2"))
                (invoke "perl" "build/cert.pl"
                        "--acl2" (string-append (getcwd) "/../saved_acl2")
                        "arithmetic-5/top.lisp"))))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (share (string-append out "/share/acl2")))
                (mkdir-p share)
                (copy-recursively "." share)
                ;; The launcher names the core by the build directory.
                (invoke "sed" "-i"
                        (string-append "s|" (getcwd) "|" share "|g")
                        (string-append share "/saved_acl2"))
                (mkdir-p (string-append out "/bin"))
                (symlink (string-append share "/saved_acl2")
                         (string-append out "/bin/acl2"))))))))
    (native-inputs (list gnu-make perl which))
    (inputs (list sbcl))
    (home-page "https://www.cs.utexas.edu/~moore/acl2/")
    (synopsis "ACL2 theorem prover")
    (description
     "ACL2 is a logic, programming language and theorem prover for
applicative Common Lisp, with a mechanised-proof checker and certified books.")
    (license license:bsd-3)))

(define-public bhw-formal
  (package
    (name "bhw-formal")
    (version "0.1")
    (source (local-file "formal" "bhw-formal-checkout"
                        #:recursive? #t
                        #:select? vcs-file?))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'build
            (lambda* (#:key inputs #:allow-other-keys)
              ;; The build's HOME is not writable; ACL2 writes there.
              (setenv "HOME" (getcwd))
              ;; The system books of ACL2 are recorded in its image as the
              ;; build directory, which is gone; name arithmetic-5 by its
              ;; store path instead.
              (let ((books (string-append (assoc-ref inputs "acl2")
                                          "/share/acl2/books")))
                (invoke "sed" "-i"
                        (string-append
                         "s|\"arithmetic-5/top\" :dir :system|\""
                         books "/arithmetic-5/top\"|")
                        "split-lines.lisp" "pitch.lisp" "reminder.lisp"))
              ;; ACL2 exits 0 after a failed certify-book, so read its output.
              (invoke "sh" "-c"
                      (string-append (assoc-ref inputs "acl2")
                                     "/bin/acl2 < certify.lsp > certify.out 2>&1"))
              (when (zero? (system* "grep" "-q" "FAILED\\|ACL2 Error"
                                    "certify.out"))
                (error "a book failed to certify; see certify.out"))))
          (replace 'check (lambda _ #t))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (mkdir-p out)
                (copy-recursively "." out)))))))
    (inputs (list acl2))
    (home-page "https://www.bhw.name/blog/bhw-www-doc.html")
    (synopsis "ACL2 models and proofs of bhw-www's pure functions")
    (description
     "Certified ACL2 books modelling pure functions of bhw-www.")
    (license license:agpl3+)))

(define-public bhw-www
  (with-dependency-variants
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
      ;; closer-mop, hunchentoot, trivial-gray-streams.  Drakma costs
      ;; nothing: Hunchentoot's tests need it, so its output refers to it.
      ;; Its TLS is pure-tls's (see WITH-DEPENDENCY-VARIANTS).
      (list sbcl-alexandria
            sbcl-bordeaux-threads
            sbcl-cl-base64
            sbcl-cl-ppcre
            sbcl-closer-mop
            sbcl-drakma
            sbcl-flexi-streams
            sbcl-graph-db
            sbcl-hunchentoot
            sbcl-ironclad
            sbcl-jzon
            sbcl-local-time
            sbcl-montezuma
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
