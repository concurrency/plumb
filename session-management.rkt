;; The MIT License (MIT)
;; 
;; Copyright (c) 2013 Matthew C. Jadud
;; 
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

#lang racket

(require db
         net/url)
(require ;"path-handling.rkt"
         "response-handling.rkt"
         "util.rkt"
         "debug.rkt"
         "seq.rkt"
         )

(provide (all-defined-out))

(define (make-session-dir config rs)
  (make-directory (session-dir config rs)))

;; Make sure this is a good session ID
;; This could be the gatekeeper
(define (session-dir config session-id)
  (build-path (send (config) get-config 'TEMPDIR) session-id))

(define (add-session-file config session-id filename code)
  (parameterize ([current-directory (session-dir config session-id)])
    (let ([fp (open-output-file filename #:exists 'replace)])
      (display code fp)
      (newline fp)
      (close-output-port fp))))

(define <c> (make-parameter false))
(define (qe sql . args)
  (apply query-exec (append (list (<c>) sql) args)))
(define (qr sql . args)
  (apply query-rows (append (list (<c>) sql) args)))

(define (sql:init-db)
  (format "CREATE TABLE IF NOT EXISTS sessions (sessionid TEXT, created INT);"))
(define (init-db config)
  (<c> (sqlite3-connect #:database (send (config) get-config 'SESSION-DB) #:mode 'create))
  (qe (sql:init-db))
  )

(define (sql:add-session)
  "INSERT INTO sessions (sessionid, created) VALUES ($1, $2);")
(define (add-session id)
  (qe (sql:add-session) id (current-seconds)))

(define (sql:session-exists?)
  "SELECT * FROM sessions WHERE sessionid = $1;")
(define (session-exists? id)
  (= 1 (length (qr (sql:session-exists?) id))))

(define (sql:remove-session)
  "DELETE FROM sessions WHERE sessionid = $1;")
(define (remove-session id)
  (qe (sql:remove-session) id))

(define (sql:all-sessions)
  "SELECT * FROM sessions;")
(define (all-sessions)
  (qr (sql:all-sessions)))

(define DAY (* 24 (* 60 60)))
(define MINUTE 60)
(define session-timeout (make-parameter DAY))

(define (cleanup-old-sessions config)
  (define now (current-seconds))
  (for ([s (all-sessions)])
    ;; Get rid of sessions more than 24 hours old
    ;;(printf "~a~n" (- now (vector-ref s 1)))
    (when (> (- now (vector-ref s 1)) (session-timeout))
      ;;(printf "DELETING ~a~n" (vector-ref s 0))
      (cleanup-session config (vector-ref s 0))
      )))

;; This should be an order-maintained list, not a hash
(define (cleanup-session config id)
  ;; Now, cleanup the specified session.
  (when (session-exists? id)
    ;; Remove it from the DB
    (remove-session id)
    ;; Make sure it is in the filesystem
    (when (directory-exists? (session-dir config id))
      ;; Remove all the files in the directory
      (parameterize ([current-directory (session-dir config id)])
        (for ([f (directory-list)])
          (delete-file f)))
      ;; Then the directory
      (delete-directory (session-dir config id)))
    )
  )