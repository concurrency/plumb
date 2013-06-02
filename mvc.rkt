#lang racket

(require "session.rkt")

(provide (all-defined-out))

(define model%
  (class object%
    (field [views empty])
    
    (define/public (add-view v)
      (set! views (cons v views)))
    
    (define/public (update)
      (for ([v views])
        (send v update)))
    
    (super-new)))

(define updateable<%>
  (interface () update))

(define view%
  (class* object% (updateable<%>)
    (init [m false])
    
    (abstract update)
    
    (super-new)))

(define controller%
  (class object%
    (init [m false])
    
    (super-new)))