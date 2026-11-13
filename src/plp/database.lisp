;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:bhw/plp/database
  (:use #:common-lisp)
  (:import-from #:bordeaux-threads)
  (:import-from #:cl-ppcre)
  (:import-from #:graph-db)
  (:local-nicknames (#:store #:bhw/store)
                    (#:song #:bhw/plp/song))
  (:export #:*database*
           #:plp-directory
           #:store-path
           #:open-database
           #:close-database
           #:load-config
           #:authorized-p

           #:save-song
           #:find-song
           #:delete-song
           #:catalog
           #:search-songs
           #:all-tags

           #:save-setlist
           #:find-setlist
           #:all-setlists
           #:delete-setlist)
  (:documentation
   "The presenter's graph-db store, at <data-root>/plp/store/: the songs,
their tags and the setlists.

A SONG vertex holds what a BHW/PLP/SONG song does, bar its tags: a TAG vertex
is one heading of the chord book's indices (category and name: :TEMPO
\"Slow\", :MASS \"Communion\", :LITURGY \"Advent\"), and a TAGGED edge runs
from a song to each of its tags.  A SETLIST vertex holds a named, ordered
list of song ids, each with a note.

The package's name is part of the store: graph-db spells a stored symbol
with it.  Do not rename it.

Songs are few (the book has 316), so searching is a pass over the CATALOG,
an in-memory list of what a search looks at, rebuilt on every change.

Anyone may read; a change needs the passphrase that LOAD-CONFIG reads from
$BHW_PLP_PASSPHRASE (AUTHORIZED-P), and with none set nothing can be
changed."))

(in-package #:bhw/plp/database)

(defvar *database* nil
  "The open store, or NIL before OPEN-DATABASE.")

(defvar *catalog* '()
  "What SEARCH-SONGS looks through; see CATALOG.")

(defvar *write-lock* (bordeaux-threads:make-lock "plp-database")
  "Held by each change to the store and the catalog rebuilt after it.")

(defvar *passphrase* nil
  "The passphrase that allows changes, or NIL when there is none.")

(defun plp-directory ()
  "<data-root>/plp/, which holds the store, the import's staging files and
the waiting images."
  (uiop:subpathname (store:data-root) "plp/"))

(defun store-path ()
  "The directory of the store, <data-root>/plp/store/."
  (uiop:subpathname (plp-directory) "store/"))

(defun load-config ()
  "Read the passphrase from $BHW_PLP_PASSPHRASE; empty counts as unset.  At
run time, as BHW/SLACK-BOT/BOT:LOAD-CONFIG."
  (setf *passphrase* (uiop:getenvp "BHW_PLP_PASSPHRASE")))

(defun authorized-p (attempt)
  "True when ATTEMPT is the passphrase.  Compares every character, so that
the time taken does not tell how many were right."
  (and *passphrase*
       (stringp attempt)
       (= (length attempt) (length *passphrase*))
       (zerop (reduce #'logior
                      (map 'list (lambda (a b) (logxor (char-code a) (char-code b)))
                           attempt *passphrase*)
                      :initial-value 0))))

;;; Schema

(graph-db:def-vertex song ()
  ((song-id    :accessor song-id-of    :type string)
   (title      :accessor title-of)
   (alt-titles :accessor alt-titles-of)
   (writers    :accessor writers-of)
   (copyright  :accessor copyright-of)
   (ccli       :accessor ccli-of)
   (number     :accessor number-of)
   (page       :accessor page-of)
   (sections   :accessor sections-of)
   (order      :accessor order-of)
   (reviewed   :accessor reviewed-of))
  :plp)

(graph-db:def-vertex tag ()
  ((category :accessor category-of)
   (name     :accessor name-of)
   (note     :accessor note-of))
  :plp)

(graph-db:def-vertex setlist ()
  ((setlist-id :accessor setlist-id-of :type string)
   (name       :accessor name-of)
   (date       :accessor date-of)
   (items      :accessor items-of))
  :plp)

(graph-db:def-edge tagged () () :plp)

(graph-db:def-view song-by-id :lessp (song :plp)
  (:map
   (lambda (v)
     (let ((id (graph-db:node-slot-value v :song-id)))
       (when id
         (graph-db:yield id nil))))))

(graph-db:def-view tag-by-key :lessp (tag :plp)
  (:map
   (lambda (v)
     (let ((category (graph-db:node-slot-value v :category))
           (name (graph-db:node-slot-value v :name)))
       (when (and category name)
         (graph-db:yield (format nil "~(~A~)=~A" category name) nil))))))

(graph-db:def-view setlist-by-id :lessp (setlist :plp)
  (:map
   (lambda (v)
     (let ((id (graph-db:node-slot-value v :setlist-id)))
       (when id
         (graph-db:yield id nil))))))

;;; Songs

(defun song-vertex (id)
  (store:find-by-view *database* 'song 'song-by-id id))

(defun tag-key (tag)
  (format nil "~(~A~)=~A" (first tag) (second tag)))

(defun tags-of (vertex)
  "The tags (category name note) of the SONG vertex VERTEX, sorted."
  (sort (loop for edge in (graph-db:outgoing-edges vertex :graph *database*
                                                          :edge-type 'tagged)
              for tag = (graph-db:lookup-vertex (graph-db:to edge) :graph *database*)
              when tag
                collect (list (category-of tag) (name-of tag) (note-of tag)))
        #'string< :key #'tag-key))

(defun vertex-song (vertex)
  "The BHW/PLP/SONG song that the SONG vertex VERTEX stores."
  (song:make-song :id (song-id-of vertex)
                  :title (title-of vertex)
                  :alt-titles (alt-titles-of vertex)
                  :writers (writers-of vertex)
                  :copyright (copyright-of vertex)
                  :ccli (ccli-of vertex)
                  :number (number-of vertex)
                  :page (page-of vertex)
                  :sections (sections-of vertex)
                  :order (order-of vertex)
                  :reviewed (reviewed-of vertex)
                  :tags (tags-of vertex)))

(defun catalog-entry (vertex)
  "What a search needs of the SONG vertex VERTEX: a plist of its :ID, :TITLE,
:WRITERS, :NUMBER, :FIRST-LINE, :TAGS (as \"mass=Communion\") and :HAYSTACK,
the text a query is looked for in."
  (let* ((song (vertex-song vertex))
         (first-line (song:song-first-line song)))
    (list :id (song:song-id song)
          :title (song:song-title song)
          :writers (song:song-writers song)
          :number (song:song-number song)
          :first-line first-line
          :tags (mapcar #'tag-key (song:song-tags song))
          :haystack (string-downcase
                     (format nil "~@[~D ~]~A ~{~A ~}~@[~A ~]~@[~A~]"
                             (song:song-number song) (song:song-title song)
                             (song:song-alt-titles song) (song:song-writers song)
                             first-line)))))

(defun rebuild-catalog ()
  (setf *catalog*
        (sort (graph-db:map-vertices #'catalog-entry *database*
                                     :vertex-type 'song :collect-p t)
              #'string-lessp :key (lambda (entry) (getf entry :title)))))

(defun catalog ()
  "Every song as a CATALOG-ENTRY plist, by title."
  *catalog*)

(defun find-song (id)
  "The song whose id is ID, as a BHW/PLP/SONG song with its tags, or NIL."
  (let ((vertex (and *database* (stringp id) (song-vertex id))))
    (and vertex (vertex-song vertex))))

(defun ensure-tag (tag)
  "The TAG vertex of TAG, (category name note), made when there is none."
  (destructuring-bind (category name &optional note) tag
    (or (store:find-by-view *database* 'tag 'tag-by-key (tag-key tag))
        (make-tag :category category :name name :note note))))

(defun unused-id (title)
  "An id for a custom song called TITLE that no song has: custom/<slug>,
with a number after it if need be."
  (let ((base (format nil "custom/~A" (let ((slug (song:slug title)))
                                        (if (string= slug "") "song" slug)))))
    (loop for n from 0
          for id = (if (zerop n) base (format nil "~A-~D" base n))
          unless (song-vertex id) return id)))

(defun save-song (song)
  "Store SONG, in place of the song with its id if there is one, with its
tags; a song without an id is given one.  Returns the id."
  (bordeaux-threads:with-lock-held (*write-lock*)
    (let ((id (or (song:song-id song) (unused-id (or (song:song-title song) ""))))
          (slots (list :title (song:song-title song)
                       :alt-titles (song:song-alt-titles song)
                       :writers (song:song-writers song)
                       :copyright (song:song-copyright song)
                       :ccli (song:song-ccli song)
                       :number (song:song-number song)
                       :page (song:song-page song)
                       :sections (song:song-sections song)
                       :order (song:song-order song)
                       :reviewed (song:song-reviewed song))))
      (store:with-transaction (*database* :context "save-song")
        (let* ((existing (song-vertex id))
               (vertex (cond (existing
                              ;; A stored vertex is changed through a copy.
                              (let ((copy (graph-db:copy existing)))
                                (loop for (slot value) on slots by #'cddr
                                      do (setf (slot-value copy (find-symbol (symbol-name slot)
                                                                             :bhw/plp/database))
                                               value))
                                (graph-db:save copy)
                                copy))
                             (t (apply #'make-song :song-id id slots)))))
          (dolist (edge (graph-db:outgoing-edges vertex :graph *database*
                                                        :edge-type 'tagged))
            (graph-db:mark-deleted edge))
          (dolist (tag (remove-duplicates (song:song-tags song)
                                          :test #'string= :key #'tag-key))
            (make-tagged :from vertex :to (ensure-tag tag)))))
      (rebuild-catalog)
      id)))

(defun delete-song (id)
  "Delete the song whose id is ID.  Returns T when there was one."
  (bordeaux-threads:with-lock-held (*write-lock*)
    (prog1 (store:with-transaction (*database* :context "delete-song")
             (let ((vertex (song-vertex id)))
               (when vertex
                 (dolist (edge (graph-db:outgoing-edges vertex :graph *database*
                                                               :edge-type 'tagged))
                   (graph-db:mark-deleted edge))
                 (graph-db:mark-deleted vertex)
                 t)))
      (rebuild-catalog))))

(defun search-songs (query &key tag (limit 40))
  "The catalog entries, at most LIMIT, that have every word of QUERY in
their number, titles, writers or first line and, when TAG is given (as
\"mass=Communion\"), that tag.  A song whose number QUERY is comes first."
  (let* ((words (cl-ppcre:split "\\s+" (string-downcase (string-trim " " (or query "")))))
         (number (and (= (length words) 1) (parse-integer (first words) :junk-allowed t)))
         (found (remove-if-not
                 (lambda (entry)
                   (and (or (null tag) (member tag (getf entry :tags) :test #'string=))
                        (every (lambda (word) (search word (getf entry :haystack)))
                               words)))
                 *catalog*))
         (found (if number
                    (stable-sort (copy-list found)
                                 (lambda (a b) (and a (not b)))
                                 :key (lambda (entry) (eql (getf entry :number) number)))
                    found)))
    (subseq found 0 (min limit (length found)))))

(defun all-tags ()
  "Every tag as (category name note), by category and name."
  (if *database*
      (sort (graph-db:map-vertices
             (lambda (tag) (list (category-of tag) (name-of tag) (note-of tag)))
             *database* :vertex-type 'tag :collect-p t)
            #'string< :key #'tag-key)
      '()))

;;; Setlists

(defun setlist-vertex (id)
  (store:find-by-view *database* 'setlist 'setlist-by-id id))

(defun setlist-plist (vertex)
  (list :id (setlist-id-of vertex) :name (name-of vertex)
        :date (date-of vertex) :items (items-of vertex)))

(defun save-setlist (name items &key date)
  "Store the setlist NAME, in place of the one of that name if there is one:
ITEMS is a list of (song-id . note).  Returns its id, the name's slug."
  (bordeaux-threads:with-lock-held (*write-lock*)
    (let ((id (song:slug name)))
      (when (string= id "")
        (error "A setlist needs a name."))
      (store:with-transaction (*database* :context "save-setlist")
        (let ((existing (setlist-vertex id)))
          (if existing
              (let ((copy (graph-db:copy existing)))
                (setf (name-of copy) name
                      (date-of copy) date
                      (items-of copy) items)
                (graph-db:save copy))
              (make-setlist :setlist-id id :name name :date date :items items))))
      id)))

(defun find-setlist (id)
  "The setlist whose id is ID as a plist (:ID :NAME :DATE :ITEMS), or NIL."
  (let ((vertex (and *database* (stringp id) (setlist-vertex id))))
    (and vertex (setlist-plist vertex))))

(defun all-setlists ()
  "Every setlist as a plist, by name."
  (if *database*
      (sort (graph-db:map-vertices #'setlist-plist *database*
                                   :vertex-type 'setlist :collect-p t)
            #'string-lessp :key (lambda (setlist) (getf setlist :name)))
      '()))

(defun delete-setlist (id)
  "Delete the setlist whose id is ID.  Returns T when there was one."
  (bordeaux-threads:with-lock-held (*write-lock*)
    (store:with-transaction (*database* :context "delete-setlist")
      (let ((vertex (setlist-vertex id)))
        (when vertex
          (graph-db:mark-deleted vertex)
          t)))))

;;; Opening

(defun open-database ()
  "Open the store, creating it when it does not exist yet."
  (close-database)
  (setf *database* (store:open-store :plp (store-path)))
  (rebuild-catalog)
  *database*)

(defun close-database ()
  "Close the store if it is open."
  (setf *catalog* '()
        *database* (store:close-store *database*)))
