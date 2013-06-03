#lang racket

(provide seq
         process%
         initial?
         pass
         NO-CHANGE)

(define NO-CHANGE 'NO-CHANGE)

(define (initial? o)
  (equal? o 'INITIAL))

(define (pass o) true)

(define (ok? obj)
  (λ (any) 
    (send obj status-ok?)))

(define process%
  (class object%
    (init-field [context "NO CONTEXT"]
                [update (λ (m) 
                          (format "Update from ~a" context))]
                )
    (field [status 'OK]
           [value 'INITIAL])
    
    (define/public (set x)
      (when (not (equal? NO-CHANGE x))
        (set! value x)))
    
    (define/public (get) value)
    
    (define/public (status-ok?)
      (equal? status 'OK))
    
    (define/public (check-condition pred?)
      (pred? value))
    
    (define/public (on-error flag)
      (set! status flag)
      (update flag)
      )
    
    (define/public (to-string)
      (format "[~a] ~a" status value))
    
    (define/public (get-context)
      context)
    (define/public (get-error)
      status)
    
    (define/public (message m)
      (update m))
    
    (super-new)))

(struct step (precondition? on-error next-steps))

(define-syntax-rule (thunk o)
  (λ () o))

(define-syntax handle-seq-case
  (syntax-rules ()
    [(_ obj pre? on-err next-steps)
     ;; Watch for defaults
     (cond
       ;; Only run code if we're OK, and 
       ;; the pre-condition is met
       [(and (send obj status-ok?)
             (send obj check-condition pre?))
        (let ([steps (map (λ (ns) (ns)) next-steps)])
          (send obj set (last steps)))
        ]
       ;; Only apply the error if we haven't done so before
       [(send obj status-ok?)
        (send obj on-error (on-err))
        (raise-user-error 
         (send obj get-context)
         (send obj to-string))
        ])
     ]))

(define-syntax seq
  (syntax-rules ()
    [(seq obj [(pre? on-err) next-steps ...] ...)
     (let ([results
            (map (λ (p oe ns)
                   (handle-seq-case obj p oe ns))
                 (list pre? ...)
                 (list (thunk on-err) ...)
                 (list (list (thunk next-steps) ...) ...))])
       (last results))
     ]))

#|
(define o (new process%))
(seq o
          [(initial? 'eight)
           (+ 3 4)]
          [((λ (n) (= n 8)) 'ten)
           (+ (send o get) 2)]
          [((λ (n) (= n 11)) 'last)
           (+ (send o get) 2)]
          )
            
(send o to-string)

|#