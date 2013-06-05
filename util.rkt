#lang racket

(provide (all-defined-out))

;; for make-unique-name
(require file/sha1
         net/url
         net/base64)

(require "debug.rkt")

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

(define (read-url str)
  (read-all (get-pure-port 
             (string->url str)
             (list "User-Agent: PLT Racket/5.3.3 (Plumb)"))))

(define (strip str)
  (for ([pat '("^[ ]+" "[ ]+$" "\n" "\r")])
    (set! str (regexp-replace pat str "")))
  str)

(define (process-config str)
  (define h (make-hash))
  (debug 'PC "lines: ~a" (regexp-split "\n" str))
  
  (for ([line (regexp-split "\n" str)])
    (debug 'PC "line: ~a" line)
    (when (and (not (regexp-match "^#" line))
               (regexp-match "(.*?):(.*?)" line))
      (let ([halves (map strip (regexp-split ":" line))])
        (debug 'PC "halves: ~a" halves)
        (hash-set! h
                   (->sym (first halves))
                   (second halves)))))
  h)

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


(define (read-all port)
  (let ([content ""])
    (let ([ip port])
      (let loop ([line (read-line ip)])
        (unless (eof-object? line)
          (set! content (format "~a~a~n" content line))
          (loop (read-line ip))))
      (close-input-port ip)
      )
    content))

(define (read-all/bytes port)
  (define ls (make-parameter '()))
  (let ([ip port])
    (let loop ([b (read-byte ip)])
      (unless (eof-object? b)
        (ls (snoc (ls) b))
        (loop (read-byte ip))))
    (close-input-port ip))
  (list->bytes (ls)))
      

(define (extract-filename path)
  (define-values (base name dir?) (split-path path))
  (->string name))


(define (extract-filedir path)
  (define-values (base name dir?) (split-path path))
  (->string base))


(define (occam-file? filename)
  (regexp-match "\\.occ$" filename))

(define (hex-file? filename)
  (regexp-match "\\.hex$" filename))

(define (file-extension filename)
  (third (regexp-match "^(.*)\\.(.*?)$" (extract-filename filename))))

(define make-server-url 
  (λ args
    (string->url
     (format "http://~a:~a~a"
             (first args)
             (second args)
             (apply string-append
                    (map (λ (p) (format "/~a" p)) 
                         (rest (rest args))))))))

(define (filter-hash hash key)
  (let ([c (hash-copy hash)])
    (if (and (hash-has-key? c key)
             (string? (hash-ref c key)))
        (hash-set! c key (string-length (hash-ref c key)))
        (hash-set! c key 'KEYNOTFOUND))
    c))