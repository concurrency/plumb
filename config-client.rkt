#lang racket

(provide client-config%)

(require "config.rkt"
         "util.rkt"
         "debug.rkt")

(define client-config%
  (class config%
    (super-new)
    
    (define (bp cmd)
      (build-path (send this get-config 'BINPATH) cmd))
    
    (define (occam-lib-path lib)
      (build-path (send this get-config 'LIBPATH) (format "~a.lib" lib)))
    
    
    (define (load-macosx-client-config)
      
      (send this add-config 'HOST-TYPE (system-type))
      ;; This should give us the root of the Plumb.app
      (send this add-config 'APP-ROOT (find-system-path 'run-file))
      
      ;; When we are inside a .app bundle, set the contents path
      ;; one way. When we're running from the command line (which
      ;; is primarily a development activity), change things around.
      (debug 'CONFIG "APP-ROOT is [~a]" (send this get-config 'APP-ROOT))
      (cond
        ;; FIXME: THis should not be in two places (mac and win setup)
        ;; We're in an app bundle
        [(and (regexp-match ".app" (->string (send this get-config 'APP-ROOT)))
              ;; but we're not running under DrRacket
              (not (regexp-match "DrRacket.app" (->string (send this get-config 'APP-ROOT))))
              )
         
         (send this add-config 'CONTENTS (build-path
                                          (extract-filedir (send this get-config 'APP-ROOT))
                                          'up
                                          ))]
        [else
         (send this add-config 'APP-ROOT (current-directory))
         (send this add-config 'CONTENTS (send this get-config 'APP-ROOT))])
      
      (send this add-config 'BINPATH (build-path 
                                      (send this get-config 'CONTENTS)
                                      "client-config" 
                                      (->string (send this get-config 'HOST-TYPE))
                                      "bin"))
      (send this add-config 'CONFPATH (build-path 
                                       (send this get-config 'CONTENTS)
                                       "client-config" 
                                       (->string (send this get-config 'HOST-TYPE))
                                       "conf"))
      
      ;; This will have to change for Windows. Actually, for the GUI app in general.
      (send this add-config 'AVRDUDE.CONF (build-path (send this get-config 'CONFPATH)
                                                      "avrdude.conf"))
      (send this add-config 'AVRDUDE (build-path (send this get-config 'BINPATH)
                                                 "avrdude"))
      
      (debug 'CONFIG "Mac Config: ~a~n" (send this get-data))
      )
    
    (define (load-windows-client-config)
      
      (send this add-config 'HOST-TYPE (system-type))
      ;; This should give us the root of the Plumb.app
      (send this add-config 'APP-ROOT (find-system-path 'run-file))
      
      ;; When we are inside a .app bundle, set the contents path
      ;; one way. When we're running from the command line (which
      ;; is primarily a development activity), change things around.
      (debug 'CONFIG "APP-ROOT is [~a]" (send this get-config 'APP-ROOT))
      (cond
        ;; We're in an app bundle
        [(and (regexp-match "Plumb.app" (->string (send this get-config 'APP-ROOT)))
              ;; but we're not running under DrRacket
              ;(not (regexp-match "DrRacket.app" (->string (send this get-config 'APP-ROOT))))
              )
         
         (send this add-config 'CONTENTS (build-path
                                          (send this get-config 'APP-ROOT)
                                          ))]
        [else
         (send this add-config 'APP-ROOT (current-directory))
         (send this add-config 'CONTENTS (send this get-config 'APP-ROOT))])
      
      
      (send this add-config 'BINPATH (build-path (send this get-config 'CONTENTS) 
                                                 "client-config"
                                                 "windows"
                                                 "bin"))
      (send this add-config 'CONFPATH (build-path (send this get-config 'CONTENTS) 
                                                  "client-config"
                                                  "windows"
                                                  "conf"))
      
      (send this add-config 'AVRDUDE.CONF (build-path (send this get-config 'CONFPATH)
                                                      "avrdude.conf"))
      (send this add-config 'AVRDUDE (build-path (send this get-config 'BINPATH)
                                                 "avrdude.exe"))
      
      (debug 'CONFIG "Windows Config: ~a~n" (send this get-data))
      )
    
    ;; Load the correct config, based on our
    ;; platform.
    (case (->sym (system-type))
      [(mac osx macosx) (load-macosx-client-config)]
      [(windows) (load-windows-client-config)]
      )
    
    ))