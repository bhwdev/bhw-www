;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG - The Common Lisp Omnificent GUI                                 ;;;;
;;;; (c) David Botton                                                      ;;;;
;;;;                                                                       ;;;;
;;;; clog-data.lisp                                                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:in-package :clog)

;;; Various functions for binding data between lisp objects and panels,
;;; particularly useful for database binding.  (Upstream also has simple SQL
;;; writers here, for CLOG-DBI; this fork has neither.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Implementation - data load and write from objects
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;
;; data-load-plist ;;
;;;;;;;;;;;;;;;;;;;;;

(defun data-load-plist (obj plist &key (row-id-name nil) (upcase-key t))
  "Load a PLIST in to OBJ where key of plist is the name of slot on
OBJ and the value is the data to load.  If slot contains a CLOG-ELEMENT
TEXT-VALUE is set, if not the slot is set to the value. If key is not
the name of a slot it is ignored.  The key is coverted to a string and
upper cased before attempting to match it to a slot if :UPCASE-KEY t
(default). If :ROW-ID-NAME is set returns that fields value."
  (let ((result))
    (loop for (key value) on plist by #'cddr while value
          do
             (when (consp key)
               (setf key (second key)))
             (let* ((slot-str  (format nil "~A" key))
                    (slot-name (if upcase-key
                                   (string-upcase slot-str)
                                   slot-str))
                    (slot-sym  (find slot-name (closer-mop:compute-slots (class-of obj))
                                     :key #'closer-mop:slot-definition-name
                                     :test #'string=)))
               (when (equalp row-id-name slot-name)
                 (setf result value))
               (when slot-sym
                 (setf slot-sym (closer-mop:slot-definition-name slot-sym))
                 (if (and (slot-boundp obj slot-sym)
                          (typep (slot-value obj slot-sym) 'clog:clog-element))
                     (setf (text-value (slot-value obj slot-sym)) value)
                     (setf (slot-value obj slot-sym) value)))))
    result))

;;;;;;;;;;;;;;;;;;;;;;
;;  data-write-list ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun data-write-list (obj slot-name-list &key (upcase-key t))
  "Returns a list, one value for each slot name in SLOT-NAME-LIST. If
a slot contains a CLOG-ELEMENT then TEXT-VALUE is used to retrieve the
value. Slot names may be symbols, keywords or text (and will be
upcased before looking up symbol if :UPCASE-KEY t). All slot-names
must be bound."
  (let ((result))
    (dolist (slot (reverse slot-name-list))
      (when (consp slot)
          (setf slot (second slot)))
      (when (keywordp slot)
        (setf slot (format nil "~A" slot)))
      (unless (symbolp slot)
        (when upcase-key
          (setf slot (string-upcase slot))))
      (setf slot (closer-mop:slot-definition-name
                  (find slot (closer-mop:compute-slots (class-of obj))
                        :key #'closer-mop:slot-definition-name
                        :test #'string=)))
      (if (and (slot-boundp obj slot)
               (typep (slot-value obj slot) 'clog:clog-element))
          (push (text-value (slot-value obj slot)) result)
          (push (slot-value obj slot) result)))
    result))

;;;;;;;;;;;;;;;;;;;;;;;
;;  data-write-plist ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun data-write-plist (obj slot-name-list &key (upcase-key t)
                                              (keys-as-keywords t))
  "Returns a plist, one member for each slot name in SLOT-NAME-LIST,
the key is the slot name. If a slot contains a CLOG-ELEMENT then
TEXT-VALUE is used to retrieve the value otherwise it is the
slot-value. Slot names may be symbols, keywords, or text (and will be
upcased before looking up symbol if :UPCASE-KEY t). If a slot-name is
a cons, the first will be used as the key in the plist and the second
will be the actual slot-name. All slot-names must be bound. If
slot-name does not exist it is left out of returned plist. If
:KEYS-AS-KEYWORDS t (default) then the keys will be symbols in the
keyword package."
  (let (result
        pname)
    (dolist (slot (reverse slot-name-list))
      (cond ((consp slot)
             (setf pname (first slot))
             (setf slot (second slot)))
            (t
             (setf pname slot)))
      (when (keywordp slot)
        (setf slot (format nil "~A" slot)))
      (unless (symbolp slot)
        (when upcase-key
          (setf pname (string-upcase pname))
          (setf slot (string-upcase slot))))
      (setf slot (find slot (closer-mop:compute-slots (class-of obj))
                        :key #'closer-mop:slot-definition-name
                        :test #'string=))
      (when slot
        (setf slot (closer-mop:slot-definition-name slot))
        (if (and (slot-boundp obj slot)
                 (typep (slot-value obj slot) 'clog:clog-element))
            (push (text-value (slot-value obj slot)) result)
            (push (slot-value obj slot) result))
        (if keys-as-keywords
            (push (intern (format nil "~A" pname) 'keyword) result)
            (push pname result))))
    result))

