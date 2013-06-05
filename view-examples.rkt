#lang racket

(provide win-examples%
         menu-examples%)

(require racket/gui
         browser/external
         browser
         net/url)

(require "mvc.rkt"
         "util.rkt"
         "debug.rkt")

(define win-examples%
  (class view%
    (init-field model main code-title code-url)
    (field [temp-file false])
   
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
                                 (send b enable false)
                                 (create-temp-file (send text get-text))
                                 (update-model)
                                 (send main set-remote-host)
                                 ;; Set the main file
                                 (debug 'EXAMPLES "Main file: ~a" temp-file)
                                 (send model set-main-file temp-file)
                                 ;; Compile
                                 (send model compile)
                                 ;; Remove evidence.
                                 (delete-file temp-file)
                                 (delete-directory (extract-filedir temp-file))
                                 (send b enable true)
                                 )]
                     ))
    
    
    (define (create-temp-file content)
      (let* ([example  (format "example-~a" (random-string 32))]
             [d (send model create-temp-dir example)]
             [example-file (build-path d "example.occ")]) 
        (debug 'EXAMPLES "Full path: ~a" example-file)
        (when (not (directory-exists? d))
          (make-directory d))
        (debug 'EXAMPLES "TEMP FILE: ~a" example-file)
        (parameterize ([current-directory d])
          (set! temp-file example-file)
          (with-output-to-file temp-file
            #:exists 'replace
            (thunk
             (printf "~a" content))))))
    
    (define/public (load-example)
      (define content 
        (read-all
         (get-pure-port 
          (string->url code-url))))
      
      ;; Load the text in
      (send text erase)
      (send text insert content)
      
      ;; Apply styling
      (apply-formatting)
      )
    
    (define (apply-formatting)
      (define delta (new style-delta%))
      (send delta set-family 'modern)
      (send delta set-weight-on 'bold)
      (send delta set-size-add 4)
      (send text change-style delta 0 (send text last-position) #f)
      (apply-syntax-highlighting))
    
    ;; http://docs.racket-lang.org/draw/color-database___.html?q=color%25
    (define c 0)
    (define (apply-syntax-highlighting)
      (define txt (send text get-text))
      (define delta (new style-delta%))
      
      (debug 'SYNTAX-HIGHLIGHT "SH: ~a" c)
      (set! c (add1 c))
      
      ;; Hightlight Keywords
      (send delta set-delta-foreground "Dark Green")
      (for ([pat (list "PROC" "SEQ" "PAR" "IF" "WHILE" "SKIP" "STOP" "IS")])
        (for ([loc (regexp-match-positions* pat txt)])
          (send text change-style delta (car loc) (cdr loc) #f)))
      
      ;; Constants
      (send delta set-delta-foreground "DodgerBlue")
      (for ([pat (list "TRUE" "FALSE" "[0-9]+")])
        (for ([loc (regexp-match-positions* pat txt)])
          (send text change-style delta (car loc) (cdr loc) #f)))
      
      ;; Comments
      (send delta set-delta-foreground "Khaki")
      (for ([pat (list "--.*?\n")])
        (for ([loc (regexp-match-positions* pat txt)])
          (send text change-style delta (car loc) (cdr loc) #f)))
      
      ;; Types
      (send delta set-delta-foreground "DarkRed")
      (for ([pat (list "INT" "BYTE" "BOOL" "CHAN" "INITIAL" "LEVEL" "REAL32" "INT16" "INT32")])
        (for ([loc (regexp-match-positions* pat txt)])
          (send text change-style delta (car loc) (cdr loc) #f)))
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
    (init-field model main menu)
    
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
                              [main main]
                              [code-title (first s)]
                              [code-url (third s)])
                         )]
             [help-string (second s)])))
                       
    (super-new)
    ))