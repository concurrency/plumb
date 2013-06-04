#lang racket
(require "util.rkt"
         "debug.rkt"
         ;"path-handling.rkt"
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


     

(define (compile config id cmd)
  (parameterize ([current-directory (session-dir config id)])
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
    
;; NEED TO PARAMETERIZE
#|
 avr-occbuild --program fastblink.occ 
--search /home/jupiter/git/kroc/tvm/arduino/occam/include/ 
--search /home/jupiter/git/kroc/tvm/arduino/occam/include/arch/common/ 
--search /home/jupiter/git/kroc/tvm/arduino/occam/include/arch/m328p/ -D F.CPU=16000 
--search /home/jupiter/git/kroc/tvm/arduino/occam/include/platforms/arduino
|#

(define (compile-cmd config names)
  (define board (send (config) get-config 'BOARD))
  (system-call
   (send (config) get-config 'OCCBUILD)
   `(--search ,(send (config) get-config 'INCLUDE)
              --search ,(build-path (send (config) get-config 'INCLUDE) "arch" "common")
              --search ,(build-path (send (config) get-config 'INCLUDE) "arch" (hash-ref board 'mcpu))
              --search ,(build-path (send (config) get-config 'INCLUDE) "platforms" (hash-ref board 'platform))
              -D ,(format "F.CPU=~a" (hash-ref board 'F_CPU))
              ;; --program needs to come last
              --program ,(hash-ref names 'occ))))

(define (output-exists? config id names ext)
  (parameterize ([current-directory (session-dir config id)])
    (file-exists? (hash-ref names ext))))

(define (plink config session-id names)
  (define result 
    (make-parameter 
     (exe-in-session 
      config
      session-id 
      (plinker-cmd config 
                   (hash-ref names 'tce)
                   (hash-ref names 'tbc)))))

  (cond
    [(zero? (result)) (get-response 'OK)]
    [else (error)]))
  

(define (plinker-cmd config tce tbc)
  (system-call
   (send (config) get-config 'LINKER)
   `(-s -o ,tbc
        ,(->string ((send (config) get-config 'occam-lib-path) 'forall))
        ,tce)))

(define (binhex config session-id names)
  (define result
    (make-parameter
     (exe-in-session config session-id (binhex-cmd config names))))
  (cond
    [(zero? (result)) (get-response 'OK)]
    [else (error)]))

;;FIXME : Magic Number (bytecode location)
(define (binhex-cmd config names)
  (system-call
   (send (config) get-config 'BINHEX)
   `(,(hash-ref (send (config) get-config 'BOARD) 'start-address) 
     ,(hash-ref names 'tbc) 
     ,(hash-ref names 'hex))))