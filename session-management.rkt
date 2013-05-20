#lang racket
 
(require db)
(require "path-handling.rkt"
         "response-handling.rkt"
         "util.rkt")

(provide (all-defined-out))

(define (make-session-dir rs)
  (make-directory (session-dir rs)))

;; Make sure this is a good session ID
;; This could be the gatekeeper
(define (session-dir session-id)
  (build-path TEMPDIR session-id))

(define (add-session-file session-id filename code)
  (parameterize ([current-directory (session-dir session-id)])
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
  (format "CREATE TABLE IF NOT EXISTS sessions (uid INT AUTO_INCREMENT, sessionid TEXT, created INT);"))
(define (init-db)
  (<c> (sqlite3-connect #:database SESSION-DB #:mode 'create))
  (qe (sql:init-db)))

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

;; This should be an order-maintained list, not a hash
(define (cleanup-session id)
  (when (session-exists? id)
    ;; Remove it from the DB
    (remove-session id)
    ;; Make sure it is in the filesystem
    (when (directory-exists? (session-dir id))
      ;; Remove all the files in the directory
      (parameterize ([current-directory (session-dir id)])
        (for ([f (directory-list)])
          (delete-file f)))
      ;; Then the directory
      (delete-directory (session-dir id)))
    ))