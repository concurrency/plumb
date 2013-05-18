#lang racket
(require "util.rkt"
         "path-handling.rkt")

(provide compile
         exe-in-session 
         compile-cmd
         plinker-cmd
         binhex-cmd)

;; RUNNING COMMANDS
;; We'll define commands as S-expressions. The language
;; looks like
;; (cmd -flag0 (-flag1 value1) (= -p1 value2))
;; which becomes
;; "cmd -flag0 -flag1 value1 -p1=value2"
;; Note we don't insert hyphens, but we make sure
;; spaces come out right.

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

(define (system-call prog flags)
  (format "~a ~a"
          ;(build-bin-path prog)
          prog
          (render (parse flags))))

(define (exe-in-session id cmd)
  (parameterize ([current-directory (session-dir id)])
    (system/exit-code cmd)))

(define (compile-cmd fname)    
  (system-call
  COMPILE
  `(-t2 -V -etc -w -y -znd -znec 
         -udo -zncc -init -xin -mobiles 
         -zrpe -zcxdiv -zcxrem -zep -b -tle 
         -DEF (= F.CPU 16000000) -DEF OCCBUILD.TVM
         ,fname)))

(define (plinker-cmd tce tbc)
  (system-call
   LINKER
   `(-s -o ,tbc
        ,(->string (occam-lib-path 'forall))
        ,tce)))

;;FIXME : Magic Number (bytecode location)
(define (binhex-cmd tbc hex)
  (system-call
   BINHEX
   `(0x4F00 ,tbc ,hex)))