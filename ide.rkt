#lang racket

(require racket/gui)

(require "code-text.rkt"
         "model-plumb.rkt"
         "menu-examples.rkt"
         "debug.rkt"
         "mvc.rkt"
         )


(define ide%
  (class view%
    
    (field [f false]
           [menu false]
           [ribbon false]
           [tab-panel false]
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
          (let ([current-file 
                 (send (current-text) get-filename)])
            (cond
              [current-file
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
                                         (printf "A: ~a~n" arduinos)
                                         (map (λ (i)
                                                (send serial-port append i))
                                              arduinos)))]))
      
      ;; Need to update the model with
      ;; the current board, port
      (send hardware set-board-type 
            (send board get-string 
                  (send board get-selection)))
      (send hardware set-arduino-port 
            (send serial-port get-string 
                  (send serial-port get-selection)))
      )
    
    ;; ---------------------------------------------------------------------------
    ;; BUILD-TEXT
    ;; ---------------------------------------------------------------------------
    (define (build-text content)
      
      (define (min-build)
        (define n (send tab-panel get-number))
        (send tab-panel append (format "draft ~a" n))
        (vector-set! contents n (new code%))
        (send (last-text) setup-code)
        (send tab-panel set-selection n)
        (send canvas set-editor (last-text)))
      (cond
        [(not content) (min-build)]
        [(string? content)
         (min-build)
         (send (last-text) insert content)]
        [(path? content)
         (build-text (file->string content))]
        ))
    
    (define (last-text) (vector-ref contents 
                                    (sub1 (send tab-panel get-number))))
    
    (define (current-text)
      (vector-ref contents 
                  (send tab-panel get-selection)))
    
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
           [label "New"]
           [parent file]
           [callback (λ (m e)
                       (build-text false))])
      
      (new menu-item%
           [label "Open"]
           [parent file]
           [callback 
            (λ (m e)
              (let ([f (get-file "Open file")])
                (when (and f (file-exists? f))
                  (build-text f)
                  (send (last-text) set-filename f)
                  (send tab-panel set-item-label 
                        (send tab-panel get-selection) 
                        (let-values ([(base fname dir?)
                                      (split-path f)])
                          (format "~a" fname)))
                  )))])
      
      (new menu-item%
           [label "Save"]
           [parent file]
           [callback (λ (m e)
                       (cond
                         [(send (current-text) get-filename)
                          (send (current-text) save-yourself)]
                         [else
                          (let ([f (put-file "Save file as...")])
                            (when f
                              (send (current-text) set-filename f)
                              (send (current-text) save-yourself)
                              (send tab-panel set-item-label 
                                    (send tab-panel get-selection) 
                                    (let-values ([(base fname dir?)
                                                  (split-path f)])
                                      (format "~a" fname)))
                              ))
                          ]))])
      
      (new menu-item%
           [label "Save As"]
           [parent file]
           [callback 
            (λ (m e)
              (let ([f (put-file "Save file as...")])
                (when f
                  (send (current-text) set-filename f)
                  (send (current-text) save-yourself)
                  (send tab-panel set-item-label 
                        (send tab-panel get-selection) 
                        (let-values ([(base fname dir?)
                                      (split-path f)])
                          (format "~a" fname)))
                  )))]
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
                           (build-text (replace-tags-in-code s))
                           ))]
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
      (set! tab-panel (new tab-panel% 
                           [parent f]
                           [choices empty]
                           [callback 
                            (λ (t e)
                              (let ([next-editor
                                     (vector-ref contents 
                                                 (send t get-selection))])
                                (send canvas set-editor next-editor)))]
                           ))
      (set! canvas (new editor-canvas% [parent tab-panel]))
      (build-text false)
      (build-ribbon f)
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
      )
    
    (super-new)
    ))


(define ide (new ide%))
(send ide create)
(send ide show true)