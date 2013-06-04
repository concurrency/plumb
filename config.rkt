#lang racket

(provide config%)

(define config%
  (class object%
    
    (field [data (make-hash)])
    
    (define/public (get-data)
      data)
    
    (define/public (add-config key val)
      (hash-set! data key val))
    
    (define/public (get-config key)
      (hash-ref data key))
    
    (super-new)))