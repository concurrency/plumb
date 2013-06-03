#lang racket

(provide client-config%)

(require "util.rkt"
         "debug.rkt")

(define client-config%
  (class object%
    
    (field [data (make-hash)])
    
    (define/public (add-config key val)
      (hash-set! data key val))
    
    (define/public (get-config key)
      (hash-ref data key))
    
    (define (bp cmd)
      (build-path (get-config 'BINPATH) cmd))
 
    (define (occam-lib-path lib)
      (build-path (get-config 'LIBPATH) (format "~a.lib" lib)))
    
    (define (load-macosx-config)
      
      (add-config 'HOST-TYPE 'macosx)
      
      (add-config 'BINPATH (build-path (current-directory) "bin" "macosx"))
      (add-config 'LIBPATH (build-path (current-directory) "occam" "lib"))
      (add-config 'INCLUDE (build-path (current-directory) "occam" "include"))
      (add-config 'TEMPDIR (build-path "/tmp" "jupiter"))
      (add-config 'SESSION-DB (build-path (get-config 'TEMPDIR) "jupiter.sqlite"))
      
      ;; Server Configs
      (add-config 'CONFIG   (build-path (current-directory) "server-config"))
      (add-config 'CONFIG-BOARDS (build-path (get-config 'CONFIG) "boards"))
      (add-config 'FIRMWARES (build-path (current-directory) "server-config" "firmwares"))
      
      (add-config 'COMPILE  (bp "occ21"))
      (add-config 'LINKER   (bp "plinker.pl"))
      (add-config 'BINHEX   (bp "binary-to-ihex"))
      
      ;; Server Config
      (add-config 'PORT 9000)
      (add-config 'LISTEN-IP false)
      
      ;; Arduino Config (plumb.rkt only)
      ;;(add-config 'SERVER-HOST "ec2-54-234-140-198.compute-1.amazonaws.com")
      (add-config 'SERVER-PORT 9000)
      
      (add-config 'SERIAL-PORT false)
      (add-config 'BOARD false)
      ;; This will have to change for Windows. Actually, for the GUI app in general.
      (add-config 'AVRDUDE.CONF (build-path (current-directory)
                                            "client-config" 
                                            (->string (get-config 'HOST-TYPE))
                                            "conf"
                                            "avrdude.conf"))
      (add-config 'AVRDUDE (build-path (current-directory)
                                       "client-config"
                                       (->string (get-config 'HOST-TYPE))
                                       "bin"
                                       "avrdude"))
      
      (debug 'CONFIG "Mac Config: ~a~n" data)
      )
    
    ;; Load the correct config, based on our
    ;; platform.
    (case (->sym (system-type))
      [(mac osx macosx) (load-macosx-config)]
      ;; [(windows) (load-windows-config)]
      )
    
    (super-new)))