#lang racket

(provide (all-defined-out))

;; for make-unique-name
(require file/sha1)

(define (->sym v)
  (string->symbol (format "~a" v)))

;; CONTRACT :: (list-of any) any -> (list-of any)
;; Intersperses the object 'o' throughout the list.
(define (list-intersperse ls o)
  (cond
    [(empty? ls) ls]
    [(empty? (rest ls)) ls]
    [else
     (cons (first ls)
           (cons o 
                 (list-intersperse (rest ls) o)))]))

;; CONTRACT :: any -> string
;; Converts any object to a string.
;; Potentially in an ugly way.
(define (->string o)
  (format "~a" o))

(define (symbol<? a b)
  (string<? (symbol->string a)
            (symbol->string b)))
 
(define (snoc ls o)
  (reverse (cons o (reverse ls))))

(define (make-unique-name content extension seed)
  (let ([sha (sha1 (open-input-bytes
                    (string->bytes/utf-8 
                     (format "~a~a" content seed))))])
    (format "~a.~a" sha extension)))

(define name-generator 
  (λ (content)
    (let ([seed (current-milliseconds)])
      (λ (extension)
        (make-unique-name content extension seed)))))
