#lang racket

(provide win-feedback%)

(require racket/gui)
(require "mvc.rkt")

;  ;;      ;; ;;  ;;;;;;; ;;     ;     ;;   ;;   
;   ;;     ;  ;;  ;;;;;;;  ;    ;;;    ;  ;;;;;  
;   ;;    ;;  ;;  ;;       ;;   ;;;   ;;  ;; ;   
;    ;    ;;  ;;  ;;       ;;   ;;;   ;;  ;;     
;    ;;   ;   ;;  ;;;;;;    ;  ;; ;;  ;   ;;;    
;    ;;  ;;   ;;  ;;;;;;    ;; ;; ;; ;;     ;;;  
;     ;; ;;   ;;  ;;        ;; ;; ;; ;;       ;; 
;     ;; ;    ;;  ;;        ;;;;   ;;;        ;; 
;      ;;;    ;;  ;;         ;;;   ;;;    ;   ;; 
;      ;;;    ;;  ;;         ;;;   ;;;    ;; ;;; 
;      ;;     ;;  ;;;;;;;    ;;     ;;     ;;;;  

(define win-feedback%
  (class view% 
    
    (init-field model)
    
    (define f (new frame% 
                   [label "Messages"]
                   [width 300]
                   [height 400]))
    
    (define editor-canvas (new editor-canvas%
                               (parent f)
                               (label "Editor Canvas")))
    (define text (new text%))
    
    (define/override (update)
      (when (send model get-compilation-result)
        (send f show true)
        (send text insert (send model get-compilation-result))))
    
    ; (send text insert (send model get-compilation-result))
    (send editor-canvas set-editor text)
    (super-new)
    ))
