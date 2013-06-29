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

(provide (all-defined-out))

;; for make-unique-name
(require file/sha1
         net/url
         net/base64
         )

(require "debug.rkt"
         "version.rkt")

(define LOG-KEY "MODICUM")

(define (safe-url-fetch reader url-string #:default [default-message ""])
  (let ([result ""])
    (with-handlers ([exn? (λ (e)
                            (debug 'SUF "Can't fetch ~a" url-string)
                            (debug 'SUF "exn: ~a~n" e)
                            (set! result default-message))])
      (set! result (reader (get-pure-port 
                            (cond
                              [(string? url-string) 
                               (string->url url-string)]
                              [else url-string]))))
      (debug 'SUF "result: ~a" result)
      result)))

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
  (~a (base64-decode 
       (string->bytes/locale
        (regexp-replace* #px"_" (~a str) "/")))))

(define (b64-encode str)
  (regexp-replace* #px"/"
                   (base64-encode (string->bytes/utf-8 str))
                   "_"))

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
  (debug 'PC "lines: ~a" str)
  
  (for ([line (cond [(string? str) (regexp-split "\n" str)]
                    [(list? str) str])])
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
  (regexp-match #px"\\.occ$" filename))

(define (hex-file? filename)
  (regexp-match #px"\\.hex$" filename))

(define (file-extension filename)
  (let ([m (regexp-match #px"^(.*)\\.(.*?)$" (extract-filename filename))])
    (cond
      [m
       (third m)]
      [else "EXTENSIONERROR"])))

(define make-server-url 
  (λ args
    (let* ([url-str 
            (format "http://~a:~a~a"
                    (first args)
                    (second args)
                    (apply string-append
                           (map (λ (p) (format "/~a" p)) 
                                (rest (rest args)))))]
           [the-url (string->url url-str)])
      (debug 'MAKE-SERVER-URL "url: ~a~n" url-str)
      the-url)))

(define (filter-hash hash key)
  (let ([c (hash-copy hash)])
    (if (and (hash-has-key? c key)
             (string? (hash-ref c key)))
        (hash-set! c key (string-length (hash-ref c key)))
        (hash-set! c key 'KEYNOTFOUND))
    c))
