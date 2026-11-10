;;;; SPDX-FileCopyrightText: 2021  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/store
  (:use #:common-lisp)
  (:import-from #:alexandria #:with-gensyms)
  (:import-from #:bordeaux-threads)
  (:import-from #:sqlite)
  (:export #:data-root
           #:store
           #:store-path
           #:open-store
           #:close-store
           #:query
           #:query-row
           #:query-value
           #:execute
           #:with-transaction)
  (:documentation
   "The SQLite plumbing that every Project Isidore database shares.

Each part of the application owns its own database file and schema:
PROJECT-ISIDORE/BIBLE/DATABASE the bible's, PROJECT-ISIDORE/SLACK-BOT/DATABASE
the bot's.  A STORE is one connection to one file and the lock that keeps
the threads serving requests from using it at once: cl-sqlite caches
prepared statements per connection, unsynchronised."))

(in-package #:project-isidore/store)

(defun data-root ()
  "Return the directory that holds Project Isidore's writable data:
$PROJECT_ISIDORE_DATA, or else $XDG_DATA_HOME/project-isidore/."
  (let ((env (uiop:getenvp "PROJECT_ISIDORE_DATA")))
    (if env
        (uiop:ensure-directory-pathname env)
        (uiop:xdg-data-home "project-isidore/"))))

(defstruct (store (:constructor make-store (path connection)))
  "An open SQLite database: its file, its connection and the lock around it."
  (path nil :type pathname :read-only t)
  (connection nil :read-only t)
  (lock (bordeaux-threads:make-recursive-lock "store") :read-only t))

(defmacro with-connection ((connection store) &body body)
  "Run BODY with CONNECTION bound to STORE's connection, holding its lock."
  (with-gensyms (s)
    `(let ((,s ,store))
       (bordeaux-threads:with-recursive-lock-held ((store-lock ,s))
         (let ((,connection (store-connection ,s)))
           ,@body)))))

(defun open-store (path &key read-only schema)
  "Open the SQLite database at the file PATH and return its STORE.

READ-ONLY opens a database that must already exist (NIL is returned when
there is none, where SQLite would create an empty one) and refuses writes
to it.  Otherwise the file and its directory are created as needed, the
database is put in WAL mode, so that a reader never waits on a writer, and
SCHEMA, a list of SQL statements that must be idempotent (CREATE TABLE IF
NOT EXISTS ...), is run in one transaction."
  (let ((path (pathname path)))
    (unless (and read-only (not (probe-file path)))
      (unless read-only
        (ensure-directories-exist path))
      (let ((connection (sqlite:connect (uiop:native-namestring path)
                                        :busy-timeout 5000)))
        (handler-bind ((error (lambda (condition)
                                (declare (ignore condition))
                                (sqlite:disconnect connection))))
          (cond (read-only
                 (sqlite:execute-non-query connection "PRAGMA query_only = ON"))
                (t
                 (sqlite:execute-single connection "PRAGMA journal_mode = WAL")
                 (sqlite:with-transaction connection
                   (dolist (statement schema)
                     (sqlite:execute-non-query connection statement))))))
        (make-store path connection)))))

(defun close-store (store)
  "Close STORE, a store from OPEN-STORE, unless it is NIL.  Returns NIL."
  (when store
    (with-connection (connection store)
      (sqlite:disconnect connection)))
  nil)

(defun query (store sql &rest parameters)
  "The rows SQL returns with PARAMETERS bound to its ?s, each a list of
column values."
  (with-connection (connection store)
    (apply #'sqlite:execute-to-list connection sql parameters)))

(defun query-row (store sql &rest parameters)
  "The first row SQL returns with PARAMETERS bound to its ?s, or NIL."
  (first (apply #'query store sql parameters)))

(defun query-value (store sql &rest parameters)
  "The first column of the first row SQL returns with PARAMETERS bound to
its ?s, or NIL."
  (with-connection (connection store)
    (apply #'sqlite:execute-single connection sql parameters)))

(defun execute (store sql &rest parameters)
  "Run SQL, a statement that returns no rows, with PARAMETERS bound to its ?s."
  (with-connection (connection store)
    (apply #'sqlite:execute-non-query connection sql parameters)))

(defmacro with-transaction ((store &key (context "transaction")) &body body)
  "Run BODY in one transaction on STORE, holding its lock throughout, and
log an error that escapes it, tagged with CONTEXT, before passing it on.
Transactions do not nest."
  (with-gensyms (connection condition)
    `(with-connection (,connection ,store)
       (handler-bind ((error (lambda (,condition)
                               (format *error-output* "database: ~A failed: ~A~%"
                                       ,context ,condition))))
         (sqlite:with-transaction ,connection
           ,@body)))))
