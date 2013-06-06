#lang racket

(provide (all-defined-out))

(define FLAGS (make-parameter '()))

(define (enable-debug! key)
  (FLAGS (cons key (FLAGS))))

(define (disable-debug! key)
  (FLAGS (filter (λ (v)
                   (not (equal? v key)))
                 (FLAGS))))

(define-syntax-rule (debug key msg args ...)
  (when (or (member key (FLAGS))
            (member 'ALL (FLAGS)))
    ;;FIXME : This will not work on Windows as written.
    ;; Need better log management in the app if I'm going
    ;; to handle user problems.
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
                         
    (case (system-type)
      [(macosx)
       (with-output-to-file
           #:exists 'append
         "/tmp/plumb.log"
         (thunk
          (printf "[~a] ~a~n"
                  key
                  (apply format (cons msg (map filter-hash (list args ...))))
                  )))]
      [else 
       (printf "[~a] ~a~n"
                  key
                  (format msg args ...))
       ])))