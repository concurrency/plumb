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
    ;; Remove 'hex from debug messages.
    ;; How ugly is this!
    (let ([cleaned-args
           (map (λ (a)
                  (if (and (hash? a)
                           (hash-ref a 'hex (λ () false)))
                      (let ([c (hash-copy a)])
                        (hash-remove! c 'hex)
                        c)
                      a))
                (list args ...))])
      (printf "[~a] ~a~n"
              key
              (apply format (cons msg cleaned-args))))))