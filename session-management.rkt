#lang racket
 
(require db
         net/url)
(require "path-handling.rkt"
         "response-handling.rkt"
         "util.rkt"
         "debug.rkt")

(provide (all-defined-out))

(define (start-session HOST PORT session-id)
  
  (define response 
    (make-parameter (get-response 'ERROR)))
  
  (debug 'START-SESSION "DEFAULT ERROR: ~a" (response))
  
  (debug 'START-SESSION "SERVER URL: ~a" 
         (url->string
          (make-server-url (HOST) (PORT) "start-session")))
  
  (set/catch response error-response?
    (get-response 'ERROR-NO-CONNECTION)
    (get-pure-port (make-server-url (HOST) (PORT) "start-session")))
  
  (debug 'START-SESSION "PORT: ~a" (response))
  
  (set/catch response port?
    (get-response 'ERROR-PROCESS-RESPONSE)
    (process-response (response)))
  
  (debug 'START-SESSION "RESPONSE: ~a" (response))
  
  (set/catch response hash?
    (get-response 'ERROR-BAD-RESPONSE)
    (hash-ref (response) 'sessionid))
  
  (debug 'START-SESSION "SESSION ID: ~a" (response))
  
  (session-id (response))
  
  (response))

(define (make-session-dir rs)
  (make-directory (session-dir rs)))

;; Make sure this is a good session ID
;; This could be the gatekeeper
(define (session-dir session-id)
  (build-path (get-config 'TEMPDIR) session-id))

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
  (format "CREATE TABLE IF NOT EXISTS sessions (sessionid TEXT, created INT);"))
(define (init-db)
  (<c> (sqlite3-connect #:database (get-config 'SESSION-DB) #:mode 'create))
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

(define (cleanup-old-sessions)
  (define now (current-seconds))
  (for ([s (all-sessions)])
    ;; Get rid of sessions more than 24 hours old
    ;;(printf "~a~n" (- now (vector-ref s 1)))
    (when (> (- now (vector-ref s 1)) (session-timeout))
      ;;(printf "DELETING ~a~n" (vector-ref s 0))
      (cleanup-session (vector-ref s 0))
      )))

;; This should be an order-maintained list, not a hash
(define (cleanup-session id)
  ;; Now, cleanup the specified session.
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
    )
  )