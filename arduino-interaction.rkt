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

(define (quote-path path)
  (case (system-type)
    ;; FIXME
    ;; Might be a problem on the Mac as well.
    [(macosx) (path->string path)]
    [(windows)
     (format "\"~a\"" (path->string path))]))

(define (avrdude-cmd config file board-config serial-port)
  (system-call
   (quote-path (send config get-config 'AVRDUDE))
   `(-C ,(quote-path (send config get-config 'AVRDUDE.CONF))
        -V -F 
        (-p ,(hash-ref board-config 'mcpu))
        (-b ,(hash-ref board-config 'baud))
        (-c arduino)
        (-P ,serial-port)
        -D -U 
        ,(format "flash:w:~a" (extract-filename file)))))
