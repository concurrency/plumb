#lang racket

(require "session.rkt")

(provide (all-defined-out))

(define model%
  (class object%
    (field [views empty]
           [session false])
    
    (define (init-session host port)
      (set! session (new session%
                         [host host]
                         [port port]))
      (update))
                         
    (define (update)
      (for ([v views])
        (send v update)))
    
    (super-new)))

(define updateable<%>
  (interface () update))

(define view%
  (class* object% (updateable<%>)
    (init [m false])
    
    (define/public (update)
      '...)
    
    (super-new)))

(define controller%
  (class object%
    (init [m false])
    
    (super-new)))