;;;; SPDX-FileCopyrightText: 2021  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:bhw/store
  (:use #:common-lisp)
  (:import-from #:alexandria #:with-gensyms)
  (:import-from #:graph-db)
  (:export #:data-root
           #:system-directory
           #:store-exists-p
           #:open-store
           #:close-store
           #:delete-views
           #:with-transaction)
  (:documentation
   "The graph-db plumbing that every bhw-www database shares.

Each part of the application owns its own store and schema:
BHW/BIBLE/DATABASE the bible's, BHW/SLACK-BOT/DATABASE
the bot's.  What they cannot avoid sharing is graph-db's system directory,
which holds the registry giving every vertex type one id across all the
stores a process opens; graph-db keeps exactly one per image.

A vertex is found by its key through a unique constraint: a :UNIQUE slot,
or GRAPH-DB:DEF-UNIQUE for a key of several slots, read with
GRAPH-DB:UNIQUE-LOOKUP.  (graph-db 5.1.0 cannot read a DEF-UNIQUE of one
slot.)  OPEN-STORE rebuilds the constraints' indexes; see there why."))

(in-package #:bhw/store)

(defun data-root ()
  "Return the directory that holds bhw-www's writable data:
$BHW_WWW_DATA, or else $XDG_DATA_HOME/bhw-www/."
  (let ((env (uiop:getenvp "BHW_WWW_DATA")))
    (if env
        (uiop:ensure-directory-pathname env)
        (uiop:xdg-data-home "bhw-www/"))))

(defun system-directory ()
  "Return graph-db's system directory, <data-root>/system/.

The type registry in it adopts the ids a store already uses when it first
opens that store, and refuses a store whose ids it has given to other types.
Copying a store from another machine therefore means copying this directory
with it."
  (uiop:subpathname (data-root) "system/"))

(defun store-exists-p (location)
  "True when the directory LOCATION holds a graph-db store."
  (and (probe-file (merge-pathnames "schema.dat" location)) t))

(defun open-store (name location &key (initial-size (* 64 1024 1024)))
  "Open the graph-db store NAME at the directory LOCATION, creating it there
when LOCATION holds none, and return it.

A .dirty file left by an unclean shutdown is removed first, so that
OPEN-GRAPH recovers the store by replaying its transaction log: graph-db's
own procedure for its STORE-NOT-CLOSED-CLEANLY-ERROR.

The indexes of the store's unique constraints are then rebuilt from its
vertices.  OPEN-GRAPH reopens those its unique-indexes.dat names and builds
no :UNIQUE slot it does not: one added since the store was written, or one
that a build without it dropped from that file when it opened the store (a
rollback).  Such a constraint would find none of the vertices already
stored and let their keys be taken again.  The rebuild reads every vertex
of a store with a constraint: some 3 s for the bible's 37,199 verses,
which is why the bible has none.

A new store starts with INITIAL-SIZE bytes for each of its heap and index
regions, rather than graph-db's 1000 MB, and grows on demand.  Stores are
opened without graph-db's buffer pool, a vertex pre-allocator for write-heavy
loads that costs ~130 MB and starts a thread that outlives CLOSE-GRAPH."
  (setf graph-db:*system-directory*
        (namestring (ensure-directories-exist (system-directory))))
  (let ((dirty-file (merge-pathnames ".dirty" location)))
    (cond ((store-exists-p location)
           (when (probe-file dirty-file)
             (delete-file dirty-file))
           (let ((graph (graph-db:open-graph name location :buffer-pool-p nil)))
             (graph-db:regenerate-unique-indexes graph)
             graph))
          (t
           (ensure-directories-exist location)
           (graph-db:make-graph name location
                                :buffer-pool-p nil
                                :heap-size initial-size
                                :index-size initial-size)))))

(defun close-store (graph)
  "Close GRAPH, a store from OPEN-STORE, unless it is NIL.  Returns NIL."
  (when graph
    (graph-db:close-graph graph :snapshot-p nil))
  nil)

(defun delete-views (graph views)
  "Delete from GRAPH those of VIEWS, a list of (type view), that it holds.

graph-db keeps a store's views in its views.dat: it reopens every one of
them with the store and updates them on each write, whether or not a
DEF-VIEW still declares them."
  (loop for (type view) in views
        when (and (graph-db:lookup-view-group type graph)
                  (graph-db:lookup-view graph type view))
          do (graph-db:delete-view graph type view)))

(defmacro with-transaction ((graph &key (context "transaction")) &body body)
  "Run BODY in a graph-db transaction on GRAPH, logging an error that escapes
it, tagged with CONTEXT, before passing it on."
  (with-gensyms (condition)
    `(let ((graph-db:*graph* ,graph))
       (handler-bind ((error (lambda (,condition)
                               (format *error-output* "database: ~A failed: ~A~%"
                                       ,context ,condition))))
         (graph-db:with-transaction ()
           ,@body)))))
