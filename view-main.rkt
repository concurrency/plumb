#lang racket

(provide win-main%)

(require racket/gui
         mrlib/path-dialog
         )
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

(define win-main%
  (class view% 
    (init-field model)
    
    (define f (new frame%
                   [label "Plumb GUI"]
                   [width 400]
                   [height 200]
                   ))
    
    (define hortz1 (new horizontal-panel%
                        [parent f]))
    
    (define host (new text-field% 
                      [parent hortz1]
                      [label "Server"]
                      [init-value "ec2-54-226-131-120.compute-1.amazonaws.com"]
                      [stretchable-width true]
                      ))
    
    (define port (new text-field% 
                      [parent hortz1]
                      [label ""]
                      [init-value "9000"]
                      [stretchable-width false]
                      ))
    
    (define serial-port (new choice%
                             [parent f]
                             [label "Arduino Port"]
                             [choices 
                              (send model get-arduino-ports)]))
    
    (define board (new choice% 
                       [parent f]
                       [label "Board Type"]
                       [choices (list "Arduino Duemilanove")]))
    
    (define hortz2 (new horizontal-panel%
                        [parent f]))
    
    (define choose-file (new button%
                             [parent hortz2]
                             [label "Choose Code"]
                             [stretchable-width true]
                             [callback (λ (b e)
                                         (let ([d (new path-dialog%
                                                       [label "occam code chooser"]
                                                       [message "Choose your main .occ file."]
                                                       [parent f]
                                                       [existing? true]
                                                       [filters (list (list "occam files" "*.occ"))]
                                                       [dir? false])])
                                           (send model set-main-file (send d run))
                                           ))]
                             ))
    
    (define check (new button%
                       [parent hortz2]
                       [label "Check"]
                       [stretchable-width true]
                       [enabled false]
                       [callback (λ (b e)
                                   (update-model)
                                   (set-remote-host)
                                   (send model check-syntax)
                                   )]))
    
    (define run (new button%
                     [parent hortz2]
                     [label "Run"]
                     [stretchable-width true]
                     [enabled false]
                     [callback (λ (b e)
                                 (set-remote-host)
                                 (send model do-compilation))]
                     ))
    
    (define messages (new message%
                          [parent f]
                          [stretchable-width true]
                          [auto-resize true]
                          (label "")))
    
    (define (update-model)
      (send model set-arduino-port 
            (send serial-port get-string 
                  (send serial-port get-selection)))
      (send model set-board-type 
            (send board get-string 
                  (send board get-selection))))
            
    (define (set-remote-host)
      (send model set-remote-host
            (send host get-value)
            (send port get-value)))
    
    (define/public (show bool)
      (send f show bool))
    
    (define/override (update)
      ;; When we have a file, we can check the syntax on it.
      (when (send model main-file-set?)
        (send check enable true))
      
      ;; When we have a file, and we have a serial port,
      ;; it is allowable to compile and upload something.
      (when (and (send model main-file-set?)
                 (not (zero? (length (send model get-arduino-ports)))))
        (send run enable true))
      
      ;; Do we have any messages to display?
      (when (send model get-message)
        (send messages set-label (send model get-message)))
      )
    
    
    
    (super-new)
    ))