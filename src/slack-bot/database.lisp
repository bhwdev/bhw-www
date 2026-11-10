;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:project-isidore/slack-bot/database
  (:use #:common-lisp)
  (:local-nicknames (#:store #:project-isidore/store))
  (:export #:*database*
           #:store-path
           #:open-database
           #:close-database

           #:find-slack-subscription
           #:all-slack-subscriptions
           #:add-slack-subscription
           #:remove-slack-subscription

           #:find-slack-identity
           #:all-slack-identities
           #:add-slack-identity
           #:remove-slack-identity

           #:user-id-of
           #:initial-of)
  (:documentation
   "The Slack bot's SQLite database, <data-root>/slack-bot.sqlite3: who has
subscribed to reminder DMs, and which initials each Slack account goes by."))

(in-package #:project-isidore/slack-bot/database)

(defvar *database* nil
  "The open bot store, or NIL before OPEN-DATABASE.")

(defun store-path ()
  "The file of the bot's database, <data-root>/slack-bot.sqlite3."
  (uiop:subpathname (store:data-root) "slack-bot.sqlite3"))

;;; Schema.  The identities are deliberately a table of their own: a
;;; subscription is the opt-in to reminder DMs, and /unsubscribe must not
;;; cost someone their identity.  Slack user ids are compared without case.

(defparameter *schema*
  '("CREATE TABLE IF NOT EXISTS slack_subscription (
       user_id TEXT PRIMARY KEY COLLATE NOCASE,
       initial TEXT NOT NULL)"
    "CREATE TABLE IF NOT EXISTS slack_identity (
       user_id TEXT PRIMARY KEY COLLATE NOCASE,
       initial TEXT NOT NULL)")
  "The statements OPEN-DATABASE runs, each idempotent.")

(defun open-database ()
  "Open the bot's database, creating it when it does not exist yet."
  (close-database)
  (setf *database* (store:open-store (store-path) :schema *schema*)))

(defun close-database ()
  "Close the bot's database if it is open."
  (setf *database* (store:close-store *database*)))

;;; Both registries work alike: one row per Slack user, which gives the
;;; initials they go by.

(defstruct (registration (:constructor make-registration (user-id initial)))
  "One row of a registry: a Slack user id and its initials."
  (user-id nil :type string :read-only t)
  (initial nil :type string :read-only t))

(defun user-id-of (registration)
  "The Slack user id of REGISTRATION."
  (registration-user-id registration))

(defun initial-of (registration)
  "The initials of REGISTRATION."
  (registration-initial registration))

(defun find-in (table user-id)
  "The registration in TABLE for USER-ID, or NIL."
  (let ((row (store:query-row *database*
                              (format nil "SELECT user_id, initial FROM ~A WHERE user_id = ?" table)
                              user-id)))
    (when row
      (apply #'make-registration row))))

(defun all-in (table)
  "An alist of (user-id . initial) for every row of TABLE, oldest first."
  (mapcar (lambda (row) (cons (first row) (second row)))
          (store:query *database*
                       (format nil "SELECT user_id, initial FROM ~A ORDER BY rowid" table))))

(defun upsert (table user-id initial context)
  "Set to INITIAL the initials of USER-ID's row in TABLE, adding the row
when there is none.  A row keeps the case its user id was first given in."
  (store:with-transaction (*database* :context context)
    (store:execute *database*
                   (format nil "INSERT INTO ~A (user_id, initial) VALUES (?, ?)
                                ON CONFLICT (user_id) DO UPDATE SET initial = excluded.initial"
                           table)
                   user-id initial)))

(defun delete-in (table user-id context)
  "Delete USER-ID's row in TABLE.  Returns T when there was one."
  (store:with-transaction (*database* :context context)
    (when (find-in table user-id)
      (store:execute *database* (format nil "DELETE FROM ~A WHERE user_id = ?" table) user-id)
      t)))

;;; Subscriptions

(defun find-slack-subscription (user-id)
  "The subscription of USER-ID, or NIL."
  (find-in "slack_subscription" user-id))

(defun all-slack-subscriptions ()
  "An alist of (user-id . initial) for every subscription."
  (all-in "slack_subscription"))

(defun add-slack-subscription (user-id initial)
  "Subscribe USER-ID as INITIAL, or change the initial of their subscription."
  (upsert "slack_subscription" user-id initial "add-slack-subscription"))

(defun remove-slack-subscription (user-id)
  "Delete the subscription of USER-ID.  Returns T if there was one."
  (delete-in "slack_subscription" user-id "remove-slack-subscription"))

;;; Identities

(defun find-slack-identity (user-id)
  "The registered identity of USER-ID, or NIL."
  (find-in "slack_identity" user-id))

(defun all-slack-identities ()
  "An alist of (user-id . initial) for every registered identity."
  (all-in "slack_identity"))

(defun add-slack-identity (user-id initial)
  "Record that USER-ID goes by INITIAL."
  (upsert "slack_identity" user-id initial "add-slack-identity"))

(defun remove-slack-identity (user-id)
  "Forget the initials of USER-ID.  Returns T if they were registered."
  (delete-in "slack_identity" user-id "remove-slack-identity"))
