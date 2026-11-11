;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) 2020-2024 David Botton                                            ;;;;
;;;; License BSD 3 Clause                                                  ;;;;
;;;;                                                                       ;;;;
;;;; clog.asd                                                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Project Isidore's fork of CLOG, in clog/ (see clog/README.org).  This
;;; file is here, beside project-isidore.asd, so that ASDF finds it wherever
;;; it is told to look for that one.

(asdf:defsystem #:clog
  :description "CLOG - The Common Lisp Omnificent GUI: Project Isidore's fork,
its core and CLOG-GUI on Hunchentoot"
  :author "David Botton <david@botton.com>"
  :license  "BSD"
  :version "2.4"
  :pathname "clog/"
  :serial t
  :depends-on (#:alexandria #:bordeaux-threads #:cl-base64 #:cl-ppcre #:closer-mop
               #:flexi-streams #:hunchentoot #:ironclad #:parse-float #:quri
               #:trivial-gray-streams #:usocket)
  :components ((:module "static-files"
                :components ((:static-file "js/boot.js")))
               (:module "source"
                :components (;; Connectivity
                             (:file "clog-connection")
                             (:file "clog-websocket")
                             (:file "clog-connection-websockets")
                             ;; CLOG Framework
                             (:file "clog")
                             (:file "clog-system")
                             (:file "clog-utilities")
                             ;; Base System
                             (:file "clog-base")
                             (:file "clog-element")
                             ;; DOM Elements
                             (:file "clog-body")
                             (:file "clog-document")
                             (:file "clog-window")
                             (:file "clog-location")
                             (:file "clog-navigator")
                             (:file "clog-style")
                             ;; HTML Elements
                             (:file "clog-element-common")
                             (:file "clog-form")
                             (:file "clog-multimedia")
                             (:file "clog-canvas")
                             ;; CLOG Extensions
                             (:file "clog-panel")
                             (:file "clog-tree")
                             (:file "clog-presentations")
                             (:file "clog-data")
                             ;; Desktop Environment
                             (:file "clog-gui")))))
