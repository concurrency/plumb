#lang racket

(require racket/gui)

(require "code-text.rkt"
         "model-plumb.rkt"
         "menu-examples.rkt"
         "debug.rkt"
         "mvc.rkt"
         )

(define tabbed-texts%
  (class object%
    (init-field parent)
    (field [tab-panel false]
           [contents (make-hash)]
           [canvas false]
           [tab-count 0])
    
    (define/public (close-tab)
      (let ([ndx (send tab-panel get-selection)]
            [sure? true])
        
        (debug 'IDE "Content not saved? : ~a" 
               (send (hash-ref contents (->sym (send tab-panel get-item-label ndx)))
                         not-saved?))
        
        
        (when (send (hash-ref contents (->sym (send tab-panel get-item-label ndx)))
                         not-saved?)
          (let ([d
                 (new dialog%
                      [label "Are you sure?"]
                      [parent parent])])
            (new message% 
                 [label (format "~a is unsaved." 
                                (->sym (send tab-panel get-item-label ndx)))]
                 [parent d])
            (define h (new horizontal-panel% [parent d]))
            (new button%
                 [parent h]
                 [label "Save first..."]
                 [callback (λ (b e)
                             (save)
                             (send d show false))])
            (new button%
                 [parent h]
                 [label "Nah. Throw it away..."]
                 [callback (λ (b e)
                             (send d show false))])
            (send d show true)
            ))
        
        (debug 'TABBED-TEXTS "close-tab: ~a" ndx)
        ;; Invalidate the editor; we use labels as keys
        (hash-set! contents
                   (->sym
                    (send tab-panel get-item-label ndx))
                   false)
        ;; Delete a tab
        (send tab-panel delete ndx)
        ;; Now, get the next selection
        (let ([next (send tab-panel get-selection)])
          ;; Update to the visible editor
          (send canvas set-editor
                (hash-ref contents (->sym
                                    (send tab-panel get-item-label next))))
          )))
    
    (define (->sym o) (string->symbol o))
    ;; ---------------------------------------------------------------------------
    ;; BUILD-TEXT
    ;; ---------------------------------------------------------------------------
    (define (build-text content)
      (define (min-build)
        (define n (send tab-panel get-number))
        (set! tab-count (add1 tab-count))
        (send tab-panel append (format "unsaved &~a" tab-count))
        (hash-set! contents 
                   (->sym (send tab-panel get-item-label n))
                   (new code% [ide this]))
        (send (last-text) setup-code)
        (send tab-panel set-selection n)
        (debug 'IDE "~a~n" (last-text))
        (send canvas set-editor (last-text)))
      (cond
        [(not content) (min-build)]
        [(string? content)
         (min-build)
         (send (last-text) insert content)]
        [(path? content)
         (build-text (file->string content))]
        ))
    
    
    (define (last-text) 
      (hash-ref contents 
                (->sym
                 (send tab-panel get-item-label 
                       (sub1 (send tab-panel get-number))))))
    
    (define (current-text)
      (hash-ref contents (->sym (send tab-panel get-item-label (send tab-panel get-selection)))))
    
    (define/public (get-filename)
      (send (current-text) get-filename))
    
    (define/public (save-file)
      (send (current-text) save-file))
    
    (define/public (save)
      (cond
        [(send (current-text) get-filename)
         (send (current-text) save-file)]
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
         ]))
    
    (define/public (save-as)
      (let ([f (put-file "Save file as...")])
        (when f
          (send (current-text) set-filename f)
          (send (current-text) save-yourself)
          (send tab-panel set-item-label 
                (send tab-panel get-selection) 
                (let-values ([(base fname dir?)
                              (split-path f)])
                  (format "~a" fname)))
          )))
    
    (define/public (open-file f)
      (new-document)
      (send (last-text) set-filename f)
      (send tab-panel set-item-label 
            (send tab-panel get-selection) 
            (let-values ([(base fname dir?)
                          (split-path f)])
              (format "~a" fname))))
    
    (define/public (new-document #:content [content false])
      (build-text content))
    
    (set! tab-panel (new tab-panel% 
                         [parent parent]
                         [choices empty]
                         [stretchable-height true]
                         [callback 
                          (λ (t e)
                            (let ([next-editor (current-text)])
                              (send canvas set-editor next-editor)))]
                         ))
    
    (set! canvas (new editor-canvas% [parent tab-panel]))
    (new-document)
    (super-new)
    ))

(define ide%
  (class view%
    
    (field [f false]
           [menu false]
           [ribbon false]
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
           [label "New"]
           [parent file]
           [callback (λ (m e)
                       (send tabbed-texts new-document))])
      
      (new menu-item%
           [label "Open"]
           [parent file]
           [callback 
            (λ (m e)
              (let ([f (get-file "Open file")])
                (when (and f (file-exists? f))
                  (send tabbed-texts open-file f)
                  )))])
      
      (new menu-item%
           [label "Save"]
           [parent file]
           [callback (λ (m e)
                       (send tabbed-texts save))])
      
      (new menu-item%
           [label "Save As"]
           [parent file]
           [callback 
            (λ (m e)
              (send tabbed-texts save-as))]
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