#lang racket

(require racket/gui)

(require "code-text.rkt"
         "tabbed-texts.rkt"
         "model-plumb.rkt"
         "menu-examples.rkt"
         "debug.rkt"
         "mvc.rkt"
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
                                       (send hardware set-board-type selection-string)))]
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
                                                       (send serial-port get-selection))))]))
      
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
      
      'DONEWITHMENU
      )
    
    
    
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
    
    (define/public (create)
      
      ;; Interaction
      ;; Need this to build the IDE
      (set! hardware (new plumb%))
      (send hardware load-config)
      (send hardware enumerate-arduinos)
      (send hardware compilation-server-config)
      (send hardware add-view this)
      (enable-debug! 'ALL)
      (build-ide))
    
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