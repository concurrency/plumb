#lang racket

(require racket/gui 
         framework
         net/url
         browser/external)

(require "code-text.rkt"
         "tabbed-texts.rkt"
         "model-plumb.rkt"
         "menu-examples.rkt"
         "debug.rkt"
         "mvc.rkt"
         "util.rkt"
         "version.rkt"
         )

(define NUMBER-OF-ERROR-LINES 3)

(define ide%
  (class view%
    
    (field [f false]
           [menu false]
           [ribbon false]
           [err-msg-text false]
           [tabbed-texts false]
           [canvas false]
           [contents (make-vector 255 false)]
           [feedback false]
           [compile-driver false]
           [hardware false]
           [network false]
           [show-debug? false]
           )
    
    ;; ---------------------------------------------------------------------------
    ;; BUILD-RIBBON
    ;; ---------------------------------------------------------------------------
    (define (build-ribbon rent)
      (define RIBBON-HEIGHT 50)
      (set! ribbon  (new group-box-panel%
                         [parent rent]
                         [label ""]
                         [horiz-margin 5]
                         [stretchable-height false]
                         [min-height RIBBON-HEIGHT]
                         ))
      
      (define h1 (new horizontal-panel%
                      [parent ribbon]))
      
      (define (compile-check-callback kind)
        (λ (b e)
          (send hardware set-error-message "")
          (send err-msg-text erase)
          (update)
          (let ([current-file (send tabbed-texts get-filename)])
            (cond
              [current-file
               ;; Save file before compiling.
               
               (send tabbed-texts save-file)
               (debug 'IDE-COMPILE "COMPILE FILE: ~a" current-file)
               ;; Set the main file
               (send hardware set-main-file current-file)
               
               (debug 'IDE-COMPILE "Main file: ~a" (send hardware get-main-file))
               
               (case kind
                 [(compile)
                  (send hardware compile)]
                 [(check)
                  (send hardware check-syntax)])]
              
              [else
               (case kind
                 [(compile)
                  (send f set-status-text 
                        "File must be saved before it can be compiled.")]
                 [(check)
                  (send f set-status-text 
                        "File must be saved before it can be compiled.")])]
              ))))
      
      
      (define check-code (new button%
                              [parent h1]
                              [label "Check Code"]
                              [stretchable-height true]
                              [stretchable-width true]
                              [callback (compile-check-callback 'check)]))
      (define compile-code (new button%
                                [parent h1]
                                [label (format "Send code to ~a"
                                               (first
                                                (send hardware get-board-choices)))]
                                [stretchable-width true]
                                [callback (compile-check-callback 'compile)]))
      
      
      (define h2 (new horizontal-panel%
                      [parent ribbon]))
      
      
      (define board (new choice% 
                         [parent h2]
                         [label "Board"]
                         [stretchable-width true]
                         [choices (send hardware get-board-choices)]
                         [callback (λ (v e)
                                     (let ([selection-string
                                            (send board get-string 
                                                  (send board get-selection))])
                                       (send compile-code set-label 
                                             (format "Send code to ~a" selection-string))
                                       (send hardware set-board-type selection-string)
                                       (update)))]
                         ))
      
      (define serial-port (new choice%
                               [parent h2]
                               [label "Port"]
                               [stretchable-width true]
                               [choices 
                                (send hardware get-arduino-ports)]
                               [callback (λ (v e)
                                           (send hardware set-arduino-port 
                                                 (send serial-port get-string 
                                                       (send serial-port get-selection)))
                                           (update))]))
      
      (define refresh (new button%
                           [parent h2]
                           [label "Refresh Ports"]
                           [callback (λ (b e)
                                       (send serial-port clear)
                                       (send hardware enumerate-arduinos)
                                       (let ([arduinos
                                              (send hardware get-arduino-ports)])
                                         (map (λ (i)
                                                (send serial-port append i))
                                              arduinos)))]))
      
      ;; Need to update the model with
      ;; the current board, port
      (unless (zero? (send board get-number))
        (send hardware set-board-type 
              (send board get-string 
                    (send board get-selection))))
      
      (unless (zero? (send serial-port get-number))
        (send hardware set-arduino-port 
              (send serial-port get-string 
                    (send serial-port get-selection))))
      )
    
    
    (define/public (close-tab)
      (send tabbed-texts close-tab))
    
    ;; ---------------------------------------------------------------------------
    ;; BUILD-MENU
    ;; ---------------------------------------------------------------------------
    (define (build-menu)
      (define menu-bar (new menu-bar%
                            [parent f]))
      (define file (new menu%
                        [label "&File"]
                        [parent menu-bar]))
      (new menu-item%
           [label "&New Tab"]
           [parent file]
           [callback (λ (m e)
                       (send tabbed-texts new-document))])
      
      (new menu-item%
           [label "Open in New Tab"]
           [parent file]
           [callback 
            (λ (m e)
              (let ([f (get-file "Open File")])
                (when (and f (file-exists? f))
                  (send tabbed-texts open-file f)
                  )))])
      
      (new menu-item%
           [label "Save Tab"]
           [parent file]
           [callback (λ (m e)
                       (send tabbed-texts save))])
      
      (new menu-item%
           [label "Save Tab As"]
           [parent file]
           [callback 
            (λ (m e)
              (send tabbed-texts save-as))]
           )
      
      (new menu-item%
           [label "Close Tab"]
           [parent file]
           [callback 
            (λ (m e)
              (send tabbed-texts close-tab))]
           )
      
      ;; EXAMPLES MENU
      (define examples (new menu%
                            [label "&Examples"]
                            [parent menu-bar]))
      
      
      ;; Loads stuff from servers
      
      (define (replace-tags-in-code conf)
        (let ([code (send hardware get-static #:as 'text "plumbing-examples" (hash-ref conf 'path))]
              [result '()])
          
          ;; Append a standard header
          (when (and (hash-has-key? conf 'name)
                     (hash-has-key? conf 'tweet)
                     (hash-has-key? conf 'author)
                     (hash-has-key? conf 'email))
            (set! code 
                  (cons
                   "-- {{name}}\n-- {{tweet}}\n-- {{author}} ({{email}})\n\n"
                   code)))
          
          (for ([line code])
            (for ([key (hash-keys conf)])
              (set! line (regexp-replace* (format "{{~a}}" key)
                                          line
                                          (hash-ref conf key))))
            (set! result (snoc result (format "~a~n" line))))
          (apply string-append result)))
      
      (define example-submenus
        (new menu-examples%
             [model hardware]
             [main this]
             [menu examples]
             [callback (λ (s)
                         (λ (m e)
                           (send tabbed-texts new-document 
                                 #:content
                                 (replace-tags-in-code s))))]
             ))
      
      (define help (new menu%
                        [label "&Help"]
                        [parent menu-bar]))
      
      (new menu-item%
           [label "Show Debug Window"]
           [parent help]
           [callback 
            (λ (m e)
              (toggle-debug-window))])
      
      (new menu-item%
           [label (format "Version: ~a" VERSION)]
           [parent help]
           [callback (λ (m e) '...)])
      
      'DONEWITHMENU
      )
    
    (define debug-frame%
      (class frame%
        (define/augment (on-close)
           (set! show-debug? false))
        (super-new)))
    
    (define gui-debug-window
      (new debug-frame% 
           [width 600]
           [label "Debug Messages"]
          ))
    (define debug-msg-canvas (new editor-canvas%
                                  (parent gui-debug-window)
                                  (label "")
                                  (stretchable-width true)
                                  (stretchable-height true)
                                  (line-count 30)
                                  ))
    (define keymap (keymap:get-editor))
    (define debug-msg-text (new text%
                                (auto-wrap true)
                                ))
    (send debug-msg-text set-keymap keymap)
    (send debug-msg-canvas set-editor debug-msg-text)
    
    (define (toggle-debug-window)
      (set! show-debug? (not show-debug?))
      
      ;; Kill the old thread
      (when debug-thread
        (kill-thread debug-thread)
        (set-debug-thread! false))
      
      (if show-debug?
          ;; Start a new one piping to the editor component
          (set-gui-debug)
          (set-textual-debug))
    
      (send gui-debug-window show show-debug?)
      )
    
    
    (define (set-gui-debug)
      (let ([c (make-channel)])
        (set-debug-channel! c)
        (when debug-thread
          (kill-thread debug-thread))
        (set-debug-thread! (thread (λ ()
                                     (let loop ()
                                       (send debug-msg-text insert
                                             (channel-get c)
                                             (send debug-msg-text last-position))
                                       (loop)))))
        ))
    
    (define (set-textual-debug)
      (let ([c (make-channel)])
        (set-debug-channel! c)
        (when debug-thread
          (kill-thread debug-thread))
        (set-debug-thread! (thread (λ ()
                                     (let loop ()
                                       (printf "~a" (channel-get c))
                                       (loop)))))
        ))
    
    ;; ---------------------------------------------------------------------------
    ;; BUILD-IDE
    ;; ---------------------------------------------------------------------------
    (define (build-ide)
      
      (set! f (new frame%
                   [width 600]
                   [height 800]
                   [label "Plumb"]))
      (send f create-status-line)
      (set! tabbed-texts (new tabbed-texts% 
                              [parent f]))
      (build-ribbon f)
      
      (define err-msg-canvas (new editor-canvas%
                                  (parent f)
                                  (label "")
                                  (stretchable-width true)
                                  (stretchable-height false)
                                  (line-count NUMBER-OF-ERROR-LINES)
                                  ))
      (set! err-msg-text (new text% (auto-wrap true)))
      (send err-msg-canvas set-editor err-msg-text)
      
      (build-menu)
      
      
      'OK
      )
    
    
 
    (define (check-version)
      (let ([remote-version
             (read
              (get-pure-port
               (string->url
                "http://concurrency.cc/plumb/client-conf/version.txt")))])
        (debug 'CHECK-VERSION "[~a] LOCAL [~a] REMOTE" 
               VERSION
               remote-version)
        
        (define block? true)
        (define get? false)
        
        (when (> remote-version
                 (string->number VERSION))
          (debug 'CHECK-VERSION "Newer version exists!")
          (define vf (new frame%
                          [label "New Version!"]
                          [parent f]
                          ))
          (new message% 
               [label (format "You're running version ~a of Plumb." VERSION)]
               [parent vf])
          
          (new message%
               [label    (format "We recommend version ~a instead." remote-version)]
               [parent vf])
          
          (define h (new horizontal-panel%
                         [parent vf]
                         [stretchable-width true]))
          (define b (new button%
                         [label "Later"]
                         [parent h]
                         [stretchable-width true]
                         [callback (λ (b e)
                                     (send vf show false))]))
          (define b2 (new button%
                         [label "Get it now!"]
                         [parent h]
                         [stretchable-width true]
                         [callback (λ (b e)
                                     (send-url "http://concurrency.cc/downloads/")
                                     (send vf show false)                           
                                     )]))
          (send vf show true)
          ;; Use thread/yield here to busywait.
          )))
    
    (define/public (create)
      
      ;; Interaction
      ;; Need this to build the IDE
      (set! hardware (new plumb%))
      (send hardware load-config)
      (send hardware enumerate-arduinos)
      (send hardware compilation-server-config)
      (send hardware add-view this)
      (enable-debug! 'ALL)
      (set-textual-debug)
      (check-version)
      (build-ide)
      
      
      )
    
    (define/public (show bool)
      (send f show bool))
    
    ;; ---------------------------------------------------------------------------
    ;; UPDATE-MODEL
    ;; ---------------------------------------------------------------------------
    (define (update-model)
      'DoNothing 
      )
    
    (define/override (update)
      ;; On update, update the status text
      (send f set-status-text (send hardware get-message))
      ;; Clear the last status message
      (let ([err-msg (send hardware get-error-message)])
        (when (string? err-msg)
          (send err-msg-text insert err-msg)
          (send tabbed-texts highlight-line (send hardware get-error-line))
          )))
    
    (super-new)
    ))


(define ide (new ide%))
(send ide create)
(send ide show true)