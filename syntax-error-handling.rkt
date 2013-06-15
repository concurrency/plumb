#lang racket

(provide (all-defined-out))

(require "debug.rkt"
         "github.rkt"
         "util.rkt"
         )

(define error-regexps (make-parameter false))

;; Error-occ21-error.occ(5)- foo is not declared
;; foo undeclared on "error.occ" line 5

(struct err-pat (name msg subs pattern parts) #:transparent)

(define BASE "Error-occ21-(.*?)\\.occ\\(([0-9]+)\\)- ")
(define BASE-PARTS '(filename line-number))

(define-syntax-rule (e-p name format-str subs str ids ...)
  (err-pat (quote name)
           format-str
           (quote subs)
           (string-append BASE str)
           (append BASE-PARTS (list (quote ids) ...))))
           
(error-regexps
 (list 
  
  (e-p undeclared 
       "You used '~a' on line ~a, but you did not define or declare it previously."
       (identifier line-number)
       "(.*?) is not declared"
       identifier)
  
  (err-pat 'default
           "Line ~a, ~a"
           '(line-number message)
           (string-append BASE "(.*?)$")
           (append BASE-PARTS '(message)))
  ))

(define (load-error-regexps)
  (define gh (new github%
                    [owner "jadudm"]
                    [repos "plumbing-syntax-errors"]))
  (define gh-read
    (read (open-input-string
           (send gh get-content "errors.rkt"))))

  (debug 'SYNTAX-ERROR-HANDLING "~a~n" gh-read)
  
  (let ([pats
         (map (λ (e)
                (err-pat (->sym (first e))
                         (second e)
                         (map ->sym (third e))
                         (string-append BASE (fourth e))
                         (append BASE-PARTS (list (->sym (fifth e))))
                         ))
              gh-read)])
    (debug 'SYNTAX-ERROR-HANDLING "~a" pats)
    (error-regexps pats)))

    
  
#|
#hasheq((message . Syntax error.) (code . ERROR-SYNTAX) (responsetype . ERROR) (errormessage . (1 error found in source
:

    :#INCLUDE "plumbing.module"
    :
    :PROC main ()
    :  PAR
   5:    foo ()
    :    blink (13, 100)
    :    blink (5, 333)
    ::
    :
Error-occ21-error.occ(5)- foo is not declared
foo undeclared on "error.occ" line 5
occbuild: Command failed: avr-occ21 -t2 -V -etc -w -y -znd -znec -udo -zncc -init -xin -mobiles -zrpe -zcxdiv -zcxrem -zep -DEF F.CPU=16000000 -DEF OCCBUILD.TVM error.occ

)))
|#

;; (struct err-pat (name msg subs pattern parts) #:transparent)
(define (index-of o ls)
  (cond
    [(empty? ls) 10000]
    [(equal? (first ls) o) 0]
    [else
     (add1 (index-of o (rest ls)))]))

(define (extract-part id match ep)
  (list-ref match
            (add1 (index-of id (err-pat-parts ep)))))

(define (build-error-message ls)
  (let ([h (first ls)]
        [ep (second ls)]
        [line (third ls)])
    (let ([format-string (string-append "[~a, line ~a] " (err-pat-msg ep))]
          [field-order (err-pat-subs ep)]
          [match (regexp-match (err-pat-pattern ep) line)])
      (apply format 
             `(,format-string
               ,@(append
                 (list (err-pat-name ep)
                       (extract-part 'line-number match ep))
                 (map (λ (id)
                        (extract-part id match ep))
                      (err-pat-subs ep))
                 )))
      )))
    
(define (process-error-message h)
  (define response (make-parameter false))
  (let ([msg (hash-ref h 'errormessage)])
    (for ([line (regexp-split "\n" (first msg))])
      (for ([ep (error-regexps)])
        (when (and (not (response))
                   (regexp-match (err-pat-pattern ep) line))
          (response (list h ep line))))))
  (build-error-message (response))
  )