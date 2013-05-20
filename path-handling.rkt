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



(define (load-mac-config)
  (config (empty-config))
  (add-config (config) 'HOST-TYPE 'mac)
  
  (add-config (config) 'BINPATH (build-path (current-directory) "bin" "macosx"))
  (add-config (config) 'LIBPATH (build-path (current-directory) "occam" "lib"))
  (add-config (config) 'INCLUDE (build-path (current-directory) "occam" "include"))
  (add-config (config) 'TEMPDIR (build-path "/tmp/jupiter"))
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
  
  
  (init-config 'mac)
  (add-host 'mac (config))
  
  (debug 'CONFIG "Mac Config: ~a~n" (config))
  )

(define (load-bereacs-config)
  (config (empty-config))
  (add-config (config) 'BINPATH (build-path (getenv 'HOME) "local" "kroc" "bin"))
  (add-config (config) 'LIBPATH (build-path (getenv 'HOME) "local" "occam" "lib"))
  (add-config (config) 'INCLUDE (build-path (getenv 'HOME) "local" "occam" "include"))
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

