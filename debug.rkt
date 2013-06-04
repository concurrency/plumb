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
    
    (case (system-type)
      [(macosx)
       (with-output-to-file
           #:exists 'append
         "/tmp/plumb.log"
         (thunk
          (printf "[~a] ~a~n"
                  key
                  (format msg args ...))))]
      [else 
       (printf "[~a] ~a~n"
                  key
                  (format msg args ...))
       ])))