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
      
      (add-config 'HOST-TYPE (system-type))
      ;; This should give us the root of the Plumb.app
      (add-config 'APP-ROOT (find-system-path 'run-file))
      
      ;; FIXME: Are we running the GUI or command-line version?
      ;; For now, assume GUI.
      (add-config 'CONTENTS (build-path
                             (get-config 'APP-ROOT)
                             "Contents"))
      
      (add-config 'BINPATH (build-path 
                            (get-config 'CONTENTS)
                            "client-config" 
                            (->string (get-config 'HOST-TYPE))
                            "bin"))
      (add-config 'CONFPATH (build-path 
                             (get-config 'CONTENTS)
                             "client-config" 
                             (->string (get-config 'HOST-TYPE))
                             "conf"))
 
      ;; This will have to change for Windows. Actually, for the GUI app in general.
      (add-config 'AVRDUDE.CONF (build-path (get-config 'CONFPATH)
                                            "avrdude.conf"))
      (add-config 'AVRDUDE (build-path (get-config 'BINPATH)
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