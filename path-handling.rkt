#lang racket

(require "debug.rkt")

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
  (add-config (config) 'BINPATH (build-path (current-directory) "bin" "macosx"))
  (add-config (config) 'LIBPATH (build-path (current-directory) "occam" "lib"))
  (add-config (config) 'INCLUDE (build-path (current-directory) "occam" "include"))
  (add-config (config) 'TEMPDIR (build-path "/tmp/jupiter"))
  (add-config (config) 'SESSION-DB (build-path (get-config 'TEMPDIR) "jupiter.sqlite"))
  
  (add-config (config) 'COMPILE  (bp "occ21"))
  (add-config (config) 'LINKER   (bp "plinker.pl"))
  (add-config (config) 'BINHEX   (bp "binary-to-ihex"))
  
  (add-config (config) 'PORT 9000)
  (add-config (config) 'LISTEN-IP false)
  
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

