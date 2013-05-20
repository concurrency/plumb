#lang racket
(require "util.rkt"
         "path-handling.rkt"
         "response-handling.rkt"
         "session-management.rkt")

(provide compile
         binhex
         output-exists?
         exe-in-session 
         compile-cmd
         compile
         plink
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

(define (compile id cmd)
  (parameterize ([current-directory (session-dir id)])
    (let-values ([(stdout stdin pid stderr control)
                  (apply values (process cmd))])
      (define result (make-parameter 'UNKNOWN))
      
      (let loop ([status (control 'status)])
        (case status
          [(running) (sleep 1) (loop (control 'status))]
          [(done-ok) 
           (result (get-response 'OK))]
          [(done-error)
           (let ([err-msg (read-all stdout)]
                 [details (read-all stderr)])
             (close-input-port stdout)
             (close-input-port stderr)
             (close-output-port stdin)
             (control 'kill)
             (result (get-response
                      'ERROR-SYNTAX
                      #:extra
                      `((errormessage
                         ,(format "~a:~n~a~n" err-msg details)))))
             )]))
      
      (result))))
    
(define (compile-cmd names)    
  (system-call
  COMPILE
  `(-t2 -V -etc -w -y -znd -znec 
         -udo -zncc -init -xin -mobiles 
         -zrpe -zcxdiv -zcxrem -zep -b -tle 
         -DEF (= F.CPU 16000000) -DEF OCCBUILD.TVM
         ,(hash-ref names 'occ))))

(define (output-exists? id names ext)
  (parameterize ([current-directory (session-dir id)])
    (file-exists? (hash-ref names ext))))

(define (plink session-id names)
  (define result 
    (make-parameter 
     (exe-in-session 
      session-id 
      (plinker-cmd (hash-ref names 'tce)
                   (hash-ref names 'tbc)))))

  (cond
    [(zero? (result)) (get-response 'OK)]
    [else (error)]))
  

(define (plinker-cmd tce tbc)
  (system-call
   LINKER
   `(-s -o ,tbc
        ,(->string (occam-lib-path 'forall))
        ,tce)))

(define (binhex session-id names)
  (define result
    (make-parameter
     (exe-in-session session-id (binhex-cmd names))))
  (cond
    [(zero? (result)) (get-response 'OK)]
    [else (error)]))

;;FIXME : Magic Number (bytecode location)
(define (binhex-cmd names)
  (system-call
   BINHEX
   `(0x4F00 ,(hash-ref names 'tbc) 
            ,(hash-ref names 'hex))))