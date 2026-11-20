;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:bhw/http
  (:use #:common-lisp)
  (:import-from #:drakma)
  (:import-from #:flexi-streams)
  (:import-from #:hunchentoot)
  (:export #:*deadline*
           #:http-request)
  (:documentation
   "The HTTP client that the bot's calls to Slack's API and to the van
schedule's Apps Script go through: Drakma, with the server's certificate
verified and a deadline on the whole exchange."))

(in-package #:bhw/http)

(defvar *deadline* 20
  "The seconds a request may take, from connecting to the end of the reply.
Drakma has no read timeout on SBCL, and a reply that never came would hold
the thread that asked (the reminder scheduler's, say) for ever.")

(defun http-request (url &key (method :get) parameters content content-type
                              headers (redirect t))
  "Send METHOD to URL; return the reply's body, decoded as UTF-8, its status
code and its headers (an a-list keyed by keywords, as Drakma's).

PARAMETERS, an a-list of names and values, are URL-encoded as UTF-8 into the
query of a GET, or into the body of a POST without CONTENT.  CONTENT, a
string, is the body, sent as UTF-8 with the Content-Type CONTENT-TYPE.
HEADERS is an a-list of further headers.  Redirects are followed, a POST's
with a GET, unless REDIRECT is NIL.

Signals an error when the server's certificate does not verify, when the
status is 400 or more, and when the reply has not ended within *DEADLINE*
seconds."
  (multiple-value-bind (body status reply-headers)
      (handler-case
          ;; pure-tls reads the socket's FD-STREAM, which waits for it
          ;; through WAIT-UNTIL-FD-USABLE, which honours the deadline;
          ;; DEADLINE-TIMEOUT is no ERROR.
          (sb-sys:with-deadline (:seconds *deadline*)
            (drakma:http-request url
                                 :method method
                                 :parameters parameters
                                 ;; as before: a space as %20, not +
                                 :url-encoder #'hunchentoot:url-encode
                                 :content content
                                 :content-type content-type
                                 :additional-headers headers
                                 :external-format-out :utf-8
                                 :force-binary t
                                 :redirect (and redirect 5)
                                 :verify :required
                                 :connection-timeout *deadline*))
        (sb-sys:deadline-timeout ()
          (error "~A ~A had no reply within ~D seconds" method url *deadline*)))
    (when (>= status 400)
      (error "~A ~A answered ~D" method url status))
    (values (if body
                (flexi-streams:octets-to-string body :external-format :utf-8)
                "")
            status
            reply-headers)))
