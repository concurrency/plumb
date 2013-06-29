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