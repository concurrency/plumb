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

(require (prefix-in srfi1: srfi/1)
         "path-handling.rkt"
         "util.rkt"
         "code-execution.rkt"
         "response-handling.rkt"
         "debug.rkt"
         )
(provide (all-defined-out))


(define (build-port sp)
  (define PORT
    (case (system-type)
      [(macosx unix) (format "/dev/~a" sp)]
      [(windows) sp]))
  PORT)

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

(define (avrdude-cmd file board serial-port)
  (system-call
   (get-config 'AVRDUDE)
   `(-C ,(->string (get-config 'AVRDUDE.CONF))
        -V -F 
        (-p ,(hash-ref board 'mcpu))
        (-b ,(hash-ref board 'baud))
        (-c arduino)
        (-P ,serial-port)
        -D -U 
        ,(format "flash:w:~a" file))))

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

(define (avrdude-code serial-port code)
  (parameterize ([current-directory (get-config 'TEMPDIR)])
    (define chex "code.hex")
    
    ;; Get the board config
    (define board (get-config 'BOARD))
    
    (debug 'UPLOAD "Writing code to temp file.")
    
    ;; Dump the firmware
    (when (file-exists? chex)
      (delete-file chex))
    (with-output-to-file chex
      (thunk (printf "~a~n" code)))
    
    (debug 'UPLOAD "Code written.")
    
    (let ()
      (define result 
        (make-parameter 
         (exe-in-tempdir
          (avrdude-cmd chex board serial-port))))
      (cond
        [(zero? (result)) (get-response 'OK)]
        [else (error)]))
    ))


;; For uploading user code, not firmware.
#;(define (avrdude)
    (define ARDUINO-PORT (get-data 'port))
    (define cmd (avrdude-cmd ARDUINO-PORT 
                             (format "~a.hex" temp-file-base)
                             #;(hex-file)
                             ))
    
    (when ARDUINO-PORT
      (exe cmd)))