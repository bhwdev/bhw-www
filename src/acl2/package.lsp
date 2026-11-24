; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
; SPDX-License-Identifier: AGPL-3.0-or-later

; The package of the books in this directory, for ACL2: run (LD) before
; certifying or including one of them.  SBCL's is package.lisp.  Its symbols
; are ACL2's and Common Lisp's, so a book can name ACL2's functions (NATP,
; LEN) in its guards, measures and lemmas; package.lisp's are Common Lisp's
; alone, so a definition that SBCL runs cannot call them.

(defpkg "BHW/ACL2"
  (union-eq *acl2-exports*
            *common-lisp-symbols-from-main-lisp-package*))
