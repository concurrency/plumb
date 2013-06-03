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
    
    (define (load-macosx-client-config)
      
      (add-config 'HOST-TYPE 'macosx)
      
      (add-config 'BINPATH (build-path (current-directory) "bin" "macosx"))
      
      (add-config 'COMPILE  (bp "occ21"))
      (add-config 'LINKER   (bp "plinker.pl"))
      (add-config 'BINHEX   (bp "binary-to-ihex"))
      
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
    
    (define (load-windows-client-config)
      
      (add-config 'HOST-TYPE 'windows)
      
      (add-config 'BINPATH (build-path (current-directory) "bin" "windows"))
      
      ;; Server Configs
      (add-config 'CONFIG   (build-path (current-directory) "server-config"))
      (add-config 'CONFIG-BOARDS (build-path (get-config 'CONFIG) "boards"))
      (add-config 'FIRMWARES (build-path (current-directory) "server-config" "firmwares"))
      
      (add-config 'COMPILE  (bp "occ21"))
      (add-config 'LINKER   (bp "plinker.pl"))
      (add-config 'BINHEX   (bp "binary-to-ihex"))
      
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
                                       "avrdude.exe"))
      
      (debug 'CONFIG "Windows Config: ~a~n" data)
      )
    
    ;; Load the correct config, based on our
    ;; platform.
    (case (->sym (system-type))
      [(mac osx macosx) (load-macosx-client-config)]
      [(windows) (load-windows-client-config)]
      )
    
    (super-new)))