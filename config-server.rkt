#lang racket

(define config%
  (class object%
    
    (field [data (make-hash)])
    
    (super-new)))