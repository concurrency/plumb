#lang racket

(require (prefix-in srfi1: srfi/1)
        "code-execution.rkt"
        "util.rkt")

(provide list-arduinos
         avrdude-cmd)

(define (list-arduinos)
  (case (system-type)
    [(macosx)
     (filter (λ (str)
               (and (regexp-match "tty" str)
                    (regexp-match "usb" str)))
             (directory-list "/dev"))]
    [(unix)
     (filter (λ (str)
               (or (regexp-match "USB[0-9]+" str)
                   (regexp-match "ACM[0-9]+" str)))
             (directory-list "/dev"))]
    [(windows win) 
     (filter 
      string?
      (map (λ (n)
             (let ([path (format "\\\\.\\COM~a" n)])
               (with-handlers ([exn:fail?
                                (λ (e) 'Oops)])
                 (call-with-input-file path
                   (λ (p) path)))))
           (srfi1:iota 15)))
     ]
    ))

(define (install-firmware board-config)
  '...)

(define (avrdude-cmd config file board-config serial-port)
  (system-call
   (send config get-config 'AVRDUDE)
   `(-C ,(->string (send config get-config 'AVRDUDE.CONF))
        -V -F 
        (-p ,(hash-ref board-config 'mcpu))
        (-b ,(hash-ref board-config 'baud))
        (-c arduino)
        (-P ,serial-port)
        -D -U 
        ,(format "flash:w:~a" file))))

#|
(define (avrdude-firmware serial-port)
  (parameterize ([current-directory (get-config 'TEMPDIR)])
    (define fhex "firmware.hex")
    ;; Get the board config
    (define board (get-config 'BOARD))
    
    (debug 'UPLOAD "Writing firmware to temp file.")
    
    ;; Dump the firmware
    (when (file-exists? fhex)
      (debug 'UPLOAD "Removing old firmware.")
      (delete-file fhex))
    
    (with-output-to-file fhex
      (thunk 
       (begin
         (debug 'UPLOAD "Writing firmware to disk.")
         (printf "~a~n" (hash-ref board 'hex)))))
    
    (debug 'UPLOAD "Firmware written.")
    
    (debug 'UPLOAD "Attempting AVRDUDE.")
    
    (let ()
      (define result 
        (make-parameter 
         (exe-in-tempdir
          (avrdude-cmd fhex board serial-port))))
      (cond
        [(zero? (result)) 
         (debug 'UPLOAD "Upload successful.")
         (get-response 'OK)]
        [else (error)]))
    ))
  |#