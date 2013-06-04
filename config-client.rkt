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
      
      ;; When we are inside a .app bundle, set the contents path
      ;; one way. When we're running from the command line (which
      ;; is primarily a development activity), change things around.
      (cond
        ;; We're in an app bundle
        [(regexp-match "app" (->string (get-config 'APP-ROOT)))
         (add-config 'CONTENTS (build-path
                                (get-config 'APP-ROOT)
                                "Contents"))]
        [else
         (add-config 'APP-ROOT (current-directory))
         (add-config 'CONTENTS (get-config 'APP-ROOT))])
      
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
      
       (add-config 'HOST-TYPE (system-type))
      ;; This should give us the root of the Plumb.app
      (add-config 'APP-ROOT (find-system-path 'run-file))
      
      ;; When we are inside a .app bundle, set the contents path
      ;; one way. When we're running from the command line (which
      ;; is primarily a development activity), change things around.
      (cond
        ;; We're in an app bundle
        [(regexp-match "app" (->string (get-config 'APP-ROOT)))
         (add-config 'CONTENTS (build-path
                                (get-config 'APP-ROOT)
                                ))]
        [else
         (add-config 'APP-ROOT (current-directory))
         (add-config 'CONTENTS (get-config 'APP-ROOT))])

      
      (add-config 'BINPATH (build-path (get-config 'CONTENTS) 
                                       "client-config"
                                       "windows"
                                       "bin"))
      (add-config 'CONFPATH (build-path (get-config 'CONTENTS) 
                                       "client-config"
                                       "windows"
                                       "conf"))
      
      (add-config 'AVRDUDE.CONF (build-path (get-config 'CONFPATH)
                                            "avrdude.conf"))
      (add-config 'AVRDUDE (build-path (get-config 'BINPATH)
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