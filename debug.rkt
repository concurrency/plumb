#lang racket

(provide (all-defined-out))

(define FLAGS (make-parameter '()))

(define (enable-debug! key)
  (FLAGS (cons key (FLAGS))))

(define (disable-debug! key)
  (FLAGS (filter (λ (v)
                   (not (equal? v key)))
                 (FLAGS))))

(define debug-channel false)
(define debug-thread false)

(define (set-debug-thread! t)
  (set! debug-thread t))

(define (set-debug-channel! c)
  (set! debug-channel c))

(define (set-textual-debug)
      (let ([c (make-channel)])
        (set-debug-channel! c)
        (when debug-thread
          (kill-thread debug-thread))
        (set-debug-thread! (thread (λ ()
                                     (let loop ()
                                       (printf "~a" (channel-get c))
                                       (loop)))))
        ))

(define-syntax-rule (debug key msg args ...)
  (when (or (member key (FLAGS))
            (member 'ALL (FLAGS)))
    (define new (make-hash))
    (define (filter-hash o)
      (if (hash? o)
          (begin
            (hash-for-each 
             o (λ (k v)
                 (if (equal? k 'hex)
                     (hash-set! new k (string-length (format "~a" v)))
                     (hash-set! new k v))))
            new)
          o))
    
    (channel-put 
     debug-channel       
     (format "[~a] ~a~n"
             key
             (apply format (cons msg (map filter-hash (list args ...))))))))