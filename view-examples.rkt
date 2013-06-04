#lang racket

(provide win-examples%
         menu-examples%)

(require racket/gui
         browser/external
         browser
         net/url)

(require "mvc.rkt"
         "util.rkt")

(define win-examples%
  (class view%
    (init-field model code-title code-url)
    (field [content false]
           [temp-file false])
   
    (define f (new frame% 
                   [label code-title]
                   [width 500]
                   [height 400]))
                                  
    (define editor-canvas (new editor-canvas%
                               [parent f]
                               [label ""]
                               [stretchable-height true]))
    (define text (new text%))
    
    (define v1 (new vertical-panel%
                    [parent f]
                    [stretchable-height false]))
    
    (define hortz (new horizontal-panel% 
                       [parent v1]
                       [stretchable-height false]))
    
    (define serial-port (new choice%
                             [parent hortz]
                             [label "Port"]
                             [choices 
                              (send model get-arduino-ports)]))
    
    (define board (new choice% 
                       [parent hortz]
                       [label "Board"]
                       [choices (send model get-board-choices)]
                       [stretchable-width true]
                       ))
    
    (define h2 (new horizontal-panel%
                    [parent v1]
                    [stretchable-height false]))
    
    (define open-docs (new button%
                           [parent h2]
                           [label "Open Docs for This Example."]
                           [stretchable-width true]
                           [callback (λ (b e)
                                       (open-docs))]))
                            
    (define run (new button%
                     [parent h2]
                     [label "Run This Example"]
                     [stretchable-width true]
                     [enabled true]
                     [callback (λ (b e)
                                 (update-model)
                                 ;; Set the main file
                                 (send model set-main-file temp-file)
                                 ;; Compile
                                 (send model compile)
                                 ;; Remove evidence.
                                 (delete-file temp-file)
                                 (delete-directory (extract-filedir temp-file))
                                 )]
                     ))
    
    
    (define/public (load-example)
      (set! content 
            (read-all
             (get-pure-port 
              (string->url code-url))))
      
      ;; Load the text in
      (send text erase)
      (send text insert content)
      
      ;; Create temp file now.
      
      (let* ([d (send model 
                      create-temp-dir 
                      (format "example-~a" (random-string 32)))]
             [file (build-path
                    d
                    "example.occ")]) 
        (when (not (directory-exists? d))
          (make-directory d))
        (parameterize ([current-directory d])
          (set! temp-file file)
          (with-output-to-file temp-file
            #:exists 'replace
            (thunk
             (printf "~a" content)))))
      )
    
    (define/override (update)
      'FIXME)
    
    
    (define (update-model)
      (send model set-arduino-port 
            (send serial-port get-string 
                  (send serial-port get-selection)))
      (send model set-board-type 
            (send board get-string 
                  (send board get-selection)))
      )
    
    
    (send editor-canvas set-editor text)
    (send f show true)
    (load-example)
    (super-new)
    ))
    
(define menu-examples%
  (class view%
    (init-field model menu)
    
    (define/override (update)
      'FIXME)
    
    (define menus (send model get-menus))
    
    ;; Build the menus
    (for ([m menus])
      (define tmp (new menu%
                       [parent menu]
                       [label (first m)]
                       ))
      (for ([s (second m)])
        (new menu-item% 
             [parent tmp]
             [label (first s)]
             [callback (λ (m e)
                         (new win-examples% 
                              [model model]
                              [code-title (first s)]
                              [code-url (third s)])
                         )]
             [help-string (second s)])))
                       
    (super-new)
    ))