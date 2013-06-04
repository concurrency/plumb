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
    
    (define (occam-lib-path lib)
      (build-path (send this get-config 'LIBPATH) (format "~a.lib" lib)))
    
    (send this add-config 'HOST-TYPE 'aws)
    
    (send this add-config 'ARDUINO (build-path (getenv "HOME") "local" "arduino"))
    (send this add-config 'KROC (build-path (getenv "HOME") "git" "kroc"))
    
    (send this add-config 'BINPATH (build-path (getenv "HOME") "local" "arduino" "bin"))
    ;; Which one?
    (send this add-config 'LIBPATH (build-path (send this get-config 'ARDUINO) "share" "tvm" "avr-vtlib"))
    ;; These are the libraries for AVR work (Plumbing) 
    (send this add-config 'INCLUDE (build-path  (send this get-config 'KROC) "tvm" "arduino" "occam" "include"))
    
    (send this add-config 'TEMPDIR (build-path "/tmp" "jupiter"))
    (send this add-config 'SESSION-DB (build-path (send this get-config 'TEMPDIR) "jupiter.sqlite"))
    
    ;; Server Configs
    (send this add-config 'CONFIG   (build-path (current-directory) "server-config"))
    (send this add-config 'CONFIG-BOARDS (build-path (send this get-config 'CONFIG) "boards"))
    (send this add-config 'FIRMWARES (build-path (current-directory) "server-config" "firmwares"))
    
    (send this add-config 'COMPILE  (bp "avr-occ21"))
    (send this add-config 'OCCBUILD (bp "avr-occbuild"))
    (send this add-config 'LINKER   (bp "avr-plinker.pl"))
    (send this add-configadd-config 'BINHEX   (bp "binary-to-ihex"))
    
    ;; Server Config
    (send this add-config 'PORT 9000)
    (send this add-config 'LISTEN-IP false)
    
    ))