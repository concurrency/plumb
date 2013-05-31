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
            (equal? key 'ALL))
    (printf "[~a] ~a~n"
            key
            (format msg args ...))))