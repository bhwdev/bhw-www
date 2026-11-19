;;;; SPDX-FileCopyrightText: 2026  Ben H. W.<foss@bhw.name>
;;;; SPDX-License-Identifier: AGPL-3.0-or-later

(uiop:define-package #:bhw/tests/http
  (:use #:common-lisp)
  (:import-from #:bordeaux-threads)
  (:import-from #:cl-ppcre)
  (:import-from #:flexi-streams)
  (:import-from #:rove #:deftest #:ok #:signals #:testing)
  (:import-from #:usocket)
  (:import-from #:bhw/http #:*deadline* #:http-request)
  (:documentation
   "Tests for bhw/http: what a request sends and how its reply is read, against
a server on localhost, and the deadline."))

(in-package #:bhw/tests/http)

(defun response (status body &rest header-lines)
  "The octets of an HTTP/1.1 response with STATUS (\"200 OK\"), HEADER-LINES
and BODY, a string sent as UTF-8."
  (let ((body (flexi-streams:string-to-octets body :external-format :utf-8))
        (crlf (coerce '(#\Return #\Linefeed) 'string)))
    (concatenate '(vector (unsigned-byte 8))
                 (flexi-streams:string-to-octets
                  (format nil "HTTP/1.1 ~A~A~{~A~A~}Content-Length: ~D~A~A"
                          status crlf
                          (loop for line in header-lines collect line collect crlf)
                          (length body) crlf crlf)
                  :external-format :latin-1)
                 body)))

(defun read-request (stream)
  "The HTTP request on STREAM: its head, as a string, and the body its
Content-Length declares, decoded as UTF-8."
  (let ((head (make-array 0 :element-type '(unsigned-byte 8)
                            :adjustable t :fill-pointer 0)))
    (loop until (and (>= (length head) 4)
                     (equalp #(13 10 13 10) (subseq head (- (length head) 4))))
          do (vector-push-extend (read-byte stream) head))
    (let* ((head (flexi-streams:octets-to-string head :external-format :latin-1))
           (length (cl-ppcre:register-groups-bind ((#'parse-integer length))
                       ("(?i)\\r\\nContent-Length: *(\\d+)" head)
                     length))
           (body (make-array (or length 0) :element-type '(unsigned-byte 8))))
      (read-sequence body stream)
      (values head (flexi-streams:octets-to-string body :external-format :utf-8)))))

(defun call-with-server (reply function)
  "Call FUNCTION with the URL of a server on localhost that answers one
request with REPLY, the octets of a response.  Return FUNCTION's values as a
list, and the head and body of the request the server received."
  (let* ((listener (usocket:socket-listen "127.0.0.1" 0
                                          :reuse-address t
                                          :element-type '(unsigned-byte 8)))
         (head nil)
         (body nil)
         (server (bordeaux-threads:make-thread
                  (lambda ()
                    ;; an error here would stop in a debugger nobody reads
                    (ignore-errors
                     (let ((socket (usocket:socket-accept listener)))
                       (unwind-protect
                            (let ((stream (usocket:socket-stream socket)))
                              (setf (values head body) (read-request stream))
                              (write-sequence reply stream)
                              (finish-output stream))
                         (usocket:socket-close socket)))))
                  :name "bhw/tests/http server")))
    (unwind-protect
         (let ((values (multiple-value-list
                        (funcall function
                                 (format nil "http://127.0.0.1:~D/exec"
                                         (usocket:get-local-port listener))))))
           (bordeaux-threads:join-thread server)
           (values values head body))
      (usocket:socket-close listener))))

(deftest a-get
  (multiple-value-bind (values head)
      (call-with-server (response "200 OK" "{\"a\":\"Marín\"}"
                                  "Content-Type: application/json")
                        (lambda (url)
                          (http-request url :parameters '(("sheet" . "Apr 20-26")
                                                          ("who" . "Marín")))))
    (destructuring-bind (body status headers) values
      (testing "sends its parameters in the query, as UTF-8, a space as %20"
        (ok (eql 0 (search "GET /exec?sheet=Apr%2020-26&who=Mar%C3%ADn HTTP/1.1" head))))
      (testing "decodes the reply as UTF-8, whatever its type"
        (ok (string= "{\"a\":\"Marín\"}" body))
        (ok (eql 200 status))
        (ok (string= "application/json" (cdr (assoc :content-type headers))))))))

(deftest a-post
  (multiple-value-bind (values head body)
      (call-with-server (response "200 OK" "{\"ok\":true}")
                        (lambda (url)
                          (http-request url :method :post
                                            :content "{\"text\":\"Marín\"}"
                                            :content-type "application/json"
                                            :headers '(("Authorization" . "Bearer x")))))
    (ok (string= "{\"ok\":true}" (first values)))
    (ok (eql 0 (search "POST /exec HTTP/1.1" head)))
    (testing "sends its content as UTF-8, with its type and further headers"
      (ok (string= "{\"text\":\"Marín\"}" body))
      (ok (search (format nil "~C~CContent-Length: 17~C~C" #\Return #\Linefeed #\Return #\Linefeed)
                  head))
      (ok (search "Content-Type: application/json" head))
      (ok (search "Authorization: Bearer x" head)))))

(deftest a-reply-not-followed
  (testing "a redirect is the reply when REDIRECT is NIL"
    (destructuring-bind (body status headers)
        (call-with-server (response "302 Found" "" "Location: https://example.invalid/r")
                          (lambda (url)
                            (http-request url :method :post :content "{}"
                                              :content-type "application/json"
                                              :redirect nil)))
      (ok (string= "" body))
      (ok (eql 302 status))
      (ok (string= "https://example.invalid/r" (cdr (assoc :location headers))))))
  (testing "a status of 400 or more is an error"
    (call-with-server (response "404 Not Found" "no")
                      (lambda (url)
                        (ok (signals (http-request url) 'error))))))

(deftest the-deadline
  ;; A peer that accepts the connection and never answers: the TLS handshake
  ;; waits for a reply that does not come.
  (let ((listener (usocket:socket-listen "127.0.0.1" 0 :reuse-address t))
        (start (get-internal-real-time)))
    (unwind-protect
         (let ((*deadline* 1))
           (ok (signals (http-request (format nil "https://127.0.0.1:~D/"
                                              (usocket:get-local-port listener)))
                        'error))
           (ok (< (/ (- (get-internal-real-time) start) internal-time-units-per-second)
                  5)
               "it ends the request at its deadline"))
      (usocket:socket-close listener))))
