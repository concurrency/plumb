#lang racket
(require racket/date)

(define (pad n)
  (if (< n 10)
      (format "0~a" n)
      n))

(define (today)
  (define d (current-date))
  (format "~a~a~a" (date-year d) (pad (date-month d)) (pad (date-day d))))
  
(let ([o (open-output-file "version.rkt" #:exists 'replace)])
  (fprintf o "#lang racket~n")
  (fprintf o "(provide (all-defined-out))~n")
  (fprintf o "(define VERSION \"~a\")~n" (today))
  (fprintf o "~n")
  (close-output-port o))