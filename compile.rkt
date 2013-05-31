#lang racket
(require "util.rkt"
         "debug.rkt"
         "path-handling.rkt"
         "response-handling.rkt"
         "session-management.rkt"
         "code-execution.rkt")

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


     

(define (compile id cmd)
  (parameterize ([current-directory (session-dir id)])
    (debug 'COMPILE "Current directory: ~a~n" (current-directory))
    (debug 'COMPILE "****~n~a~n****~n" cmd)
           
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
    
(define (compile-cmd-x names)    
  (system-call
  (get-config 'COMPILE)
  `(-t2 -V -etc -w -y -znd -znec 
         -udo -zncc -init -xin -mobiles 
         -zrpe -zcxdiv -zcxrem -zep -b -tle 
         -DEF (= F.CPU 16000000) -DEF OCCBUILD.TVM
         ,(hash-ref names 'occ))))

;; NEED TO PARAMETERIZE
#|
 avr-occbuild --program fastblink.occ 
--search /home/jupiter/git/kroc/tvm/arduino/occam/include/ 
--search /home/jupiter/git/kroc/tvm/arduino/occam/include/arch/common/ 
--search /home/jupiter/git/kroc/tvm/arduino/occam/include/arch/m328p/ -D F.CPU=16000 
--search /home/jupiter/git/kroc/tvm/arduino/occam/include/platforms/arduino
|#

(define (compile-cmd names)
  (system-call
   (get-config 'OCCBUILD)
   `(--search ,(get-config 'INCLUDE)
              --search ,(build-path (get-config 'INCLUDE) "arch" "common")
              --search ,(build-path (get-config 'INCLUDE) "arch" "m328p")
              --search ,(build-path (get-config 'INCLUDE) "platforms" "arduino")
              -D F.CPU=16000
              ;; --program needs to come last
              --program ,(hash-ref names 'occ))))

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
   (get-config 'LINKER)
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
   (get-config 'BINHEX)
   `(0x4F00 ,(hash-ref names 'tbc) 
            ,(hash-ref names 'hex))))