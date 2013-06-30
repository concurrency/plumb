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

(provide server-config%)

(require "config.rkt"
         "util.rkt"
         "debug.rkt")

(define server-config%
  (class config%
    (super-new)
    
    (define (bp cmd)
      (build-path (send this get-config 'BINPATH) cmd))
    
    (send this add-config 'occam-lib-path
          (λ (lib)
            (build-path (send this get-config 'LIBPATH) (format "~a.lib" lib))))
    
    (send this add-config 'HOST-TYPE 'aws)
    
    (send this add-config 'ARDUINO (build-path (getenv "HOME") "local" "arduino"))
    (send this add-config 'GIT (build-path (getenv "HOME") "git"))
    (send this add-config 'KROC (build-path (getenv "HOME") "git" "kroc"))
    
    (send this add-config 'BINPATH (build-path (getenv "HOME") "local" "arduino" "bin"))
    ;; Which one?
    (send this add-config 'LIBPATH (build-path (send this get-config 'ARDUINO) "share" "tvm" "avr-vtlib"))
    ;; These are the libraries for AVR work (Plumbing) 
    (send this add-config 'INCLUDE (build-path  (send this get-config 'GIT) "plumbing" "src"))
    
    (send this add-config 'TEMPDIR (build-path "/tmp" "jupiter"))
    (send this add-config 'SESSION-DB (build-path (send this get-config 'TEMPDIR) "jupiter.sqlite"))
    
    ;; Server Configs
    (send this add-config 'CONFIG   (build-path (getenv "HOME") 
                                                "git" "plumb-live" 
                                                "server-config"))
    (send this add-config 'CONFIG-BOARDS (build-path (send this get-config 'CONFIG) "boards"))
    (send this add-config 'FIRMWARES (build-path (send this get-config 'CONFIG) "firmwares"))
    
    (send this add-config 'COMPILE  (bp "avr-occ21"))
    (send this add-config 'OCCBUILD (bp "avr-occbuild"))
    (send this add-config 'LINKER   (bp "avr-plinker.pl"))
    (send this add-config 'BINHEX   (bp "binary-to-ihex"))
    
    ;; Server Config
    (send this add-config 'PORT 9000)
    (send this add-config 'LISTEN-IP false)
    (send this add-config 'SERVER-LOG-DIR (build-path (getenv "HOME") "logs"))
    ))