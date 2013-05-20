#lang racket

(provide (all-defined-out))


(define (avrdude-cmd hex)
  (define board (get-config 'BOARD))
  
  (system-call
   'avrdude
   `(-C ,(->string (get-config 'AVRDUDE.CONF))
        -V -F 
        (-p ,(board 'mcpu))
        (-b ,(board 'baud))
        (-c arduino)
        (-P ,(build-port sp))
        -D -U 
        ,(format "flash:w:~a" (->string file)))))

;; For uploading user code, not firmware.
(define (avrdude)
  (define ARDUINO-PORT (get-data 'port))
  (define cmd (avrdude-cmd ARDUINO-PORT 
                           (format "~a.hex" temp-file-base)
                           #;(hex-file)
                           ))
  (report 'AVRDUDE cmd)
  (when ARDUINO-PORT
    (exe cmd)))