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
