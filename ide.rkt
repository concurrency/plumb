#lang racket

(require racket/gui)

(require "code-text.rkt")


(define ide%
  (class object%
    
    (field [f false]
           [menu false]
           [ribbon false]
           [tab-panel false]
           [canvas false]
           [contents (make-vector 255 false)]
           [feedback false]
           [compile-driver false]
           )
    
    (define (build-ribbon rent)
      (define RIBBON-HEIGHT 50)
      (set! ribbon  (new group-box-panel%
                         [parent rent]
                         [label ""]
                         [horiz-margin 5]
                         [stretchable-height false]
                         [min-height RIBBON-HEIGHT]
                         ))
      (define compile (new button%
                           [parent ribbon]
                           [label "Compile"]
                           [stretchable-height true]
                           [callback (λ (b e)
                                       'clicked)]))
      'Done
      )
    
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
    
    (define (build-menu)
      (define menu-bar (new menu-bar%
                            [parent f]))
      (define file (new menu%
                        [label "&File"]
                        [parent menu-bar]))
      (new menu-item%
           [label "Open"]
           [parent file]
           [callback 
            (λ (m e)
              (let ([f (get-file "Open file")])
                (when (and f (file-exists? f))
                  (build-text f)
                  (send (last-text) set-file f)
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
                       (send (current-text) save-yourself))])
      
      (new menu-item%
           [label "Save As"]
           [parent file]
           [callback 
            (λ (m e)
              (let ([f (put-file "Save file")])
                (when (and f (not (file-exists? f)))
                  (printf "SAVING ~a~n" f))))]
           ))
    
    (define (build-ide)
      (set! f (new frame%
                   [width 600]
                   [height 800]
                   [label "Pie Plate"]))
      (send f create-status-line)
      (build-ribbon f)
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
      (build-menu)
      'OK
      )
    
    (define/public (create)
      (build-ide))
    
    (define/public (show bool)
      (send f show bool))
    
    (super-new)
    ))


(define ide (new ide%))
(send ide create)
(send ide show true)