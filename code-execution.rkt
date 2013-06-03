#lang racket

(provide system-call
         exe-in-tempdir
         exe-in-session
         )
(require "util.rkt")

;; CODE EXECUTION
;;;;;;;;;;;;;;;;;;

(define (exe-in-tempdir temp-dir cmd)
  (parameterize ([current-directory temp-dir])
    (system/exit-code cmd)))    

;; Server side... FIXME
(define (exe-in-session config id cmd)
  (parameterize ([current-directory (send config get-config 'SESSION-DIR)])
    (system/exit-code cmd)))

(define (system-call prog flags)
  (format "~a ~a"
          ;(build-bin-path prog)
          prog
          (render (parse flags))))


(struct cmd (app args) #:transparent)
(struct arg2 (flag value) #:transparent)
(struct arg1 (flag) #:transparent)
(struct set (param value) #:transparent)
(struct nospace (flag value) #:transparent)

(define (parse sexp)
  (match sexp
    [`(= ,rand1 ,rand2)
     (set rand1 rand2)]
    [`(nospace ,flag ,val)
     (nospace flag val)]
    [`(,command ,args ...)
     (cmd command (map parse args))]
    ;; FIXME: The list of length two is subsumed by
    ;; the previous rule... unnecessary?
    [`(,flag ,value)
     (arg2 flag value)]
    [flag/value
     (arg1 flag/value)]))


(define (render ast)
  (match ast
    [(struct cmd (command args))
     (format "~a ~a" 
             command
             (apply string-append
                    (list-intersperse (map render args) " ")))]
    [(struct set (param value))
     (format "~a=~a" param value)]
    [(struct arg2 (flag value))
     (format "~a ~a" flag value)]
    [(struct arg1 (flag/value))
     (format "~a" flag/value)]
    [(struct nospace (flag value))
     (format "~a~a" flag value)]
    ))