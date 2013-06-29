;; The MIT License (MIT)
;; 
;; Copyright (c) 2013 Matthew C. Jadud
;; 
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

#lang racket

(provide tabbed-texts%)

(require racket/gui
         mrlib/path-dialog)
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
               (send (hash-ref contents (->sym (get-label ndx)))
                     not-saved?))
        
        ;; Handle unsaved tabs
        (when (send (hash-ref contents (->sym (get-label ndx)))
                    not-saved?)
          (let ([d
                 (new dialog%
                      [label "Are you sure?"]
                      [parent parent])])
            (new message% 
                 [label (format "~a is unsaved." 
                                (->sym (get-label ndx)))]
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
                    (get-label ndx))
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
                             (get-label next))])
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
        (define n (send tab-panel get-number))
        
        (unless tab-id
          (set! tab-id (format "unsaved ~a" tab-count)))
        
        (send tab-panel set-label tab-id)
        (send tab-panel append tab-id)
        
        (hash-set! contents 
                   (->sym (get-label n))
                   (new code%
                        [tab-panel this]
                        [ide parent]))
        (send (last-text) setup-code)
        (send tab-panel set-selection n)
        (debug 'IDE "~a~n" (last-text))
        (send canvas set-editor (last-text))
        )
      
      (set! tab-count (add1 tab-count))
      (cond
        [(not content)  (min-build)
                        (send (last-text) set-saved!)] 
        [(string? content)
         (min-build)
         (send (last-text) insert content)]
        [(path? content)
         (build-text (file->string content) #:tab-id (~a tab-id))
         (send (last-text) set-saved!)]
        ))
    
    (define (get-label n)
      (regexp-replace #px"^• " (send tab-panel get-item-label n) ""))
    
    (define/public (show-dirty)
      (let ([current-label 
             (send tab-panel
                   get-item-label 
                   (send tab-panel get-selection))])
        (unless (regexp-match #px"^•" current-label)
          (send tab-panel
                set-item-label
                (send tab-panel get-selection)
                (format "• ~a" (send tab-panel
                                     get-item-label 
                                     (send tab-panel get-selection)))
                ))))
    
    (define/public (show-clean)
      (send tab-panel 
            set-item-label
            (send tab-panel get-selection)
            (regexp-replace "^• "
                            (send tab-panel
                                  get-item-label 
                                  (send tab-panel get-selection))
                            "")))
    
    (define/public (highlight-line n)
      (send (current-text) highlight-line n))
    
    
    (define (last-text) 
      (hash-ref contents 
                (->sym
                 (get-label (sub1 (send tab-panel get-number))))))
    
    (define (current-text)
      (hash-ref contents (->sym (get-label (send tab-panel get-selection)))))
    
    (define/public (get-filename)
      (send (current-text) get-filename))
    
    (define/public (save-file)
      ;(send (current-text) save-file)
      (save))
    
    (define/public (save)
      (cond
        [(send (current-text) get-filename)
         (send (current-text) save-file)]
        [else
         (let* ([f (let ([result  "default.occ"])
                     (let loop ()
                       (set! result (put-file "Save file as..."))
                       (unless (and result
                                    (regexp-match #px".*\\.(occ|module|inc)$" result))
                         (define d (new dialog% 
                                        [label "Try again!"]
                                        ))
                         (new message% 
                              [parent d]
                              [label "Filenames must end in .occ!"])
                         (new button% 
                              [parent d]
                              [label "OK!"] 
                              [callback (λ (b e) 
                                          (send d show false))])
                         (send d show true)
                         (loop)))
                     result)]
                        
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
                        (->sym (get-label (send tab-panel get-selection)))
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
        (debug 'OPEN-FILE "Setting ~a as saved." fname)
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