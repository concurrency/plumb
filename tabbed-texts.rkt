#lang racket

(provide tabbed-texts%)

(require racket/gui)
(require "debug.rkt"
         "code-text.rkt")

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
        
        ;; Handle unsaved tabs
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
        
        (cond
          [(zero? (send tab-panel get-number))
           (build-text false)]
          [else
           ;; Now, get the next selection
           (let* ([next (send tab-panel get-selection)]
                  [next-tag (->sym
                             (send tab-panel get-item-label next))])
             (debug 'CLOSE-TAB "'next' is ~a" next)
             (debug 'CLOSE-TAB "'next-tag' is ~a" next-tag)
             ;; Update to the visible editor
             (send canvas set-editor (hash-ref contents next-tag))
             )])))
    
    (define (->sym o) (string->symbol o))
    ;; ---------------------------------------------------------------------------
    ;; BUILD-TEXT
    ;; ---------------------------------------------------------------------------
    (define (build-text content #:tab-id [tab-id false])
      (define (min-build)
        (unless tab-id
          (set! tab-id (format "unsaved ~a" tab-count)))
        (define n (send tab-panel get-number))
        
        (send tab-panel append tab-id)
        (hash-set! contents 
                   (->sym (send tab-panel get-item-label n))
                   (new code% [ide this]))
        (send (last-text) setup-code)
        (send tab-panel set-selection n)
        (debug 'IDE "~a~n" (last-text))
        (send canvas set-editor (last-text)))
      
      (set! tab-count (add1 tab-count))
      (cond
        [(not content)  (min-build)] 
        [(string? content)
         (min-build)
         (send (last-text) insert content)]
        [(path? content)
         (build-text (file->string content) #:tab-id (~a tab-id))]
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
         (let* ([f (put-file "Save file as...")]
               [filename (let-values ([(base fname dir?)
                                       (split-path f)])
                           (format "~a" fname))])
           (when f
             ;; Copy the existing editor to a 
             ;; new name in the contents hash
             (hash-set! contents
                        (->sym filename)
                        (current-text))
             ;; Invalidate the old
             (hash-set! contents
                        (->sym (send tab-panel get-item-label (send tab-panel get-selection)))
                        false)
             ;; Update the label, so everything works on the new
             (send tab-panel set-item-label 
                   (send tab-panel get-selection) 
                   filename)
             
             (send (current-text) set-filename f)
             (send (current-text) save-yourself)
             
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
      (let-values ([(base fname dir?)
                          (split-path f)])
        (debug 'TABBED-TEXT "open-file with ~a" f)
        (build-text f #:tab-id fname)
        (send (last-text) set-filename f)
        (debug 'TABBED-TEXT "open-file tab name: ~a" fname)
        ;(send tab-panel set-item-label (send tab-panel get-selection) fname)
        (hash-set! contents (->sym (format "~a" fname)) (last-text))
        (send (last-text) set-saved!)
        ))
      
    
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