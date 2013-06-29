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

(require "debug.rkt"
         "util.rkt")

(provide (all-defined-out))

(define per-host (make-parameter (make-hash)))

(define (init-config host)
  (hash-set! (per-host) host (make-hash)))

(define (add-host host conf)
  (hash-set! (per-host) host conf))

(define (add-config conf key val)
  (hash-set! conf key val))

(define (empty-config)
  (make-hash))

(define config (make-parameter (make-hash)))

(define (set-config key)
  (config (hash-ref (per-host) key)))

(define (get-config k)
  (hash-ref (config) k))


(define (load-config name)
  (debug 'CONFIG "Loading config: ~a~n" name)
  (case (->sym name)
    [(mac osx macosx) (load-macosx-config)
               (set-config 'macosx)]
    
    [(bereacs) (load-bereacs-config)
               (set-config 'bereacs)]
    
    [(aws amazon) (load-aws-config)
                  (set-config 'aws)]
    ))

(define (load-macosx-config)
  (config (empty-config))
  (add-config (config) 'HOST-TYPE 'macosx)
  
  (add-config (config) 'BINPATH (build-path (current-directory) "bin" "macosx"))
  (add-config (config) 'LIBPATH (build-path (current-directory) "occam" "lib"))
  (add-config (config) 'INCLUDE (build-path (current-directory) "occam" "include"))
  (add-config (config) 'TEMPDIR (build-path "/tmp" "jupiter"))
  (add-config (config) 'SESSION-DB (build-path (get-config 'TEMPDIR) "jupiter.sqlite"))
  
  ;; Server Configs
  (add-config (config) 'CONFIG   (build-path (current-directory) "server-config"))
  (add-config (config) 'CONFIG-BOARDS (build-path (get-config 'CONFIG) "boards"))
  (add-config (config) 'FIRMWARES (build-path (current-directory) "server-config" "firmwares"))
  
  (add-config (config) 'COMPILE  (bp "occ21"))
  (add-config (config) 'LINKER   (bp "plinker.pl"))
  (add-config (config) 'BINHEX   (bp "binary-to-ihex"))
  
  ;; Server Config
  (add-config (config) 'PORT 9000)
  (add-config (config) 'LISTEN-IP false)
  
  ;; Arduino Config (plumb.rkt only)
  ;;(add-config (config) 'SERVER-HOST "ec2-54-234-140-198.compute-1.amazonaws.com")
  (add-config (config) 'SERVER-PORT 9000)
  
  (add-config (config) 'SERIAL-PORT false)
  (add-config (config) 'BOARD false)
  ;; This will have to change for Windows. Actually, for the GUI app in general.
  (add-config (config) 'AVRDUDE.CONF (build-path (current-directory)
                                                 "client-config" 
                                                 (->string (get-config 'HOST-TYPE))
                                                 "conf"
                                                 "avrdude.conf"))
  (add-config (config) 'AVRDUDE (build-path (current-directory)
                                            "client-config"
                                            (->string (get-config 'HOST-TYPE))
                                            "bin"
                                            "avrdude"))
  
  
  (init-config 'macosx)
  (add-host 'macosx (config))
  
  (debug 'CONFIG "Mac Config: ~a~n" (config))
  )

(define (load-aws-config)
  (config (empty-config))
  (add-config (config) 'HOST-TYPE 'aws)
  
  (add-config (config) 'ARDUINO (build-path (getenv "HOME") "local" "arduino"))
  (add-config (config) 'KROC (build-path (getenv "HOME") "git" "kroc"))
  
  (add-config (config) 'BINPATH (build-path (getenv "HOME") "local" "arduino" "bin"))
  ;; Which one?
  (add-config (config) 'LIBPATH (build-path (get-config 'ARDUINO) "share" "tvm" "avr-vtlib"))
  ;; These are the libraries for AVR work (Plumbing) 
  (add-config (config) 'INCLUDE (build-path  (get-config 'KROC) "tvm" "arduino" "occam" "include"))
  
  (add-config (config) 'TEMPDIR (build-path "/tmp" "jupiter"))
  (add-config (config) 'SESSION-DB (build-path (get-config 'TEMPDIR) "jupiter.sqlite"))
  
  ;; Server Configs
  (add-config (config) 'CONFIG   (build-path (current-directory) "server-config"))
  (add-config (config) 'CONFIG-BOARDS (build-path (get-config 'CONFIG) "boards"))
  (add-config (config) 'FIRMWARES (build-path (current-directory) "server-config" "firmwares"))
  
  (add-config (config) 'COMPILE  (bp "avr-occ21"))
  (add-config (config) 'OCCBUILD (bp "avr-occbuild"))
  (add-config (config) 'LINKER   (bp "avr-plinker.pl"))
  (add-config (config) 'BINHEX   (bp "binary-to-ihex"))
  
  ;; Server Config
  (add-config (config) 'PORT 9000)
  (add-config (config) 'LISTEN-IP false)
  
  ;; Arduino Config (plumb.rkt only)
  (add-config (config) 'SERIAL-PORT false)
  (add-config (config) 'BOARD false)
  
  ;; This will have to change for Windows. Actually, for the GUI app in general.
  (add-config (config) 'AVRDUDE.CONF (build-path (current-directory)
                                                 "client-config" 
                                                 (->string (get-config 'HOST-TYPE))
                                                 "conf"
                                                 "avrdude.conf"))
  (add-config (config) 'AVRDUDE (build-path (current-directory)
                                            "client-config"
                                            (->string (get-config 'HOST-TYPE))
                                            "bin"
                                            "avrdude"))
  
  
  (init-config 'aws)
  (add-host 'aws (config))
  
  (debug 'CONFIG "AWS Config: ~a~n" (config))
  )


(define (load-bereacs-config)
  (config (empty-config))
  (add-config (config) 'BINPATH (build-path (getenv "HOME") "local" "kroc" "bin"))
  (add-config (config) 'LIBPATH (build-path (getenv "HOME") "local" "occam" "lib"))
  (add-config (config) 'INCLUDE (build-path (getenv "HOME") "local" "occam" "include"))
  (add-config (config) 'TEMPDIR (build-path "/tmp/jupiter"))
  (add-config (config) 'SESSION-DB (build-path (get-config 'TEMPDIR) "jupiter.sqlite"))
  
  (add-config (config) 'COMPILE  (bp "occ21"))
  (add-config (config) 'LINKER   (bp "plinker.pl"))
  (add-config (config) 'BINHEX   (bp "binary-to-ihex"))
  
  (init-config 'bereacs)
  (add-host 'bereacs (config))
  )


         
(define (bp cmd)
  (build-path (get-config 'BINPATH) cmd))



(define (occam-lib-path lib)
  (build-path (get-config 'LIBPATH) (format "~a.lib" lib)))

