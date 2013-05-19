#lang racket
 
(require db)
(require "path-handling.rkt"
         "response-handling.rkt"
         "util.rkt")

(provide 
 init-db
 add-session
 cleanup-session
 session-exists?
 )

(define <c> (make-parameter false))

(define (sql:init-db)
  (format "CREATE TABLE IF NOT EXISTS sessions (uid INT AUTO_INCREMENT, sessionid TEXT, created INT);"))
(define (init-db)
  (<c> (sqlite3-connect #:database SESSION-DB #:mode 'create))
  (query-exec (<c>) (sql:init-db)))

(define (sql:add-session)
  "INSERT INTO sessions (sessionid, created) VALUES ($1, $2);")
(define (add-session id)
  (query-exec (<c>) (sql:add-session) id (current-seconds)))

(define (sql:session-exists?)
  "SELECT * FROM sessions WHERE sessionid = $1;")
(define (session-exists? id)
  (= 1 (length (query-rows (<c>) (sql:session-exists?) id))))
          
;; This should be an order-maintained list, not a hash
(define (cleanup-session id names)
  '...)
