#lang racket

(provide (all-defined-out))

;; for make-unique-name
(require file/sha1
         net/base64)

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

(define (b64-decode str)
  (format "~a" 
          (base64-decode (string->bytes/locale str))))

(define (symbol<? a b)
  (string<? (symbol->string a)
            (symbol->string b)))
 
(define (snoc ls o)
  (reverse (cons o (reverse ls))))

(define (name-generator filename)
  (let ([base (list-ref (regexp-match "(.*?)\\.occ" filename) 1)])
    (λ (ext)
      (format "~a.~a" base ext))))

(define CHARS '(a b c d e f g h i j k l m n o p q r s t u v w k y z
                  A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
                  1 2 3 4 5 6 7 8 9))
(define (random-string leng)
  (let ([sp (open-output-string)])
    (for ([i leng])
      (write (list-ref CHARS (random (length CHARS))) sp))
    (get-output-string sp)))

(define (occam-file? filename)
  (regexp-match "\\.occ$" filename))