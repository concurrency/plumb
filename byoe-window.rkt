#lang racket

(provide win-main%)

(require racket/gui
         mrlib/path-dialog
         )

(require "mvc.rkt"
         "debug.rkt"
         "version.rkt"
         )


;   ;;       ;;     ;;     ;;  ;;      ;; 
;   ;;;     ;;;     ;;     ;;  ;;;     ;; 
;   ;;;;   ;;;;    ;;;;    ;;  ;;;;    ;; 
;   ;; ;; ;; ;;    ;;;;    ;;  ;;;;;   ;; 
;   ;; ;; ;; ;;    ;  ;;   ;;  ;; ;;   ;; 
;   ;;  ;;;  ;;   ;;  ;;   ;;  ;;  ;;  ;; 
;   ;;   ;   ;;   ;;   ;   ;;  ;;   ;; ;; 
;   ;;       ;;  ;;;;;;;;  ;;  ;;   ;;;;; 
;   ;;       ;;  ;;;;;;;;  ;;  ;;    ;;;; 
;   ;;       ;;  ;;     ;; ;;  ;;     ;;; 
;   ;;       ;; ;;      ;; ;;  ;;      ;; 

(define win-main%
  (class view% 
    (init-field model)
    (define first-check-or-compile? true)
    
    (define f (new frame%
                   [label "Plumb @ concurrency.cc"]
                   [width 400]
                   [height 200]
                   ))
    #|
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
    |#
    
    (define top-half (new vertical-panel%
                          [parent f]
                          [stretchable-height false]))
    
    (define serial-port (new choice%
                             [parent top-half]
                             [label "Arduino Port"]
                             [choices 
                              (send model get-arduino-ports)]))
    
    (define board (new choice% 
                       [parent top-half]
                       [label "Board Type"]
                       [choices (send model get-board-choices)]))
    
    (define hortz2 (new horizontal-panel%
                        [parent top-half]))
    
    (define choose-file (new button%
                             [parent hortz2]
                             [label "Choose Code"]
                             [stretchable-width true]
                             [callback (λ (b e)
                                         (let* ([d (new path-dialog%
                                                       [label "occam code chooser"]
                                                       [message "Choose your main .occ file."]
                                                       [parent f]
                                                       [existing? true]
                                                       [directory (or (getenv "HOME")
                                                                      (getenv "USERPROFILE"))]
                                                       [filters (list (list "occam files" "*.occ"))]
                                                       [dir? false])]
                                                [f (send d run)])
                                           (when f
                                             (send model set-main-file f))
                                           ))]
                             ))
    
    (define check (new button%
                       [parent hortz2]
                       [label "Check"]
                       [stretchable-width true]
                       [enabled false]
                       [callback (λ (b e)
                                   (send b enable false)
                                   (send model set-error-message "")
                                   (update-model)
                                   (set-remote-host)
                                   
                                   ;; Set the main file
                                   (debug 'CHECK-SYNTAX "Main file: ~a" 
                                          (send model get-main-file))
                                   ;; Compile
                                
                                   (when first-check-or-compile?
                                     (set! first-check-or-compile? false)
                                     ;; This loads things from Bitbucket.
                                     (send model load-error-regexps))
                                   
                                   (send model check-syntax)
                                   (send b enable true)
                                   )]))
    
    (define run (new button%
                     [parent hortz2]
                     [label "Run"]
                     [stretchable-width true]
                     [enabled false]
                     [callback (λ (b e)
                                 
                                 (send b enable false)
                                 (send model set-error-message "")
                                 (update-model)
                                 (set-remote-host)
                                 ;; Set the main file
                                 (debug 'COMPILE "Main file: ~a" 
                                        (send model get-main-file))
                                 
                                 (when first-check-or-compile?
                                     (set! first-check-or-compile? false)
                                     ;; This loads things from Bitbucket.
                                     (send model load-error-regexps))
                                 
                                 ;; Compile
                                 (send model compile)
                                 (send b enable true)
                                 )]
                     ))
    
    (define messages (new message%
                          [parent f]
                          [stretchable-width true]
                          [auto-resize true]
                          (label "")))
    
    (define bottom-half (new vertical-panel%
                             [parent f]
                             [stretchable-height true]))
    
    (define err-msg-canvas (new editor-canvas%
                                (parent bottom-half)
                                (label "")
                                (stretchable-width true)
                                (line-count 5)
                                ))
    (define err-msg-text (new text% (auto-wrap true)))
    (send err-msg-canvas set-editor err-msg-text)
    
    
    
    ;   ;;       ;;  ;;;;;;;  ;;      ;;  ;;      ;;   ;;   
    ;   ;;;     ;;;  ;;;;;;;  ;;;     ;;  ;;      ;; ;;;;;  
    ;   ;;;;   ;;;;  ;;       ;;;;    ;;  ;;      ;; ;; ;   
    ;   ;; ;; ;; ;;  ;;       ;;;;;   ;;  ;;      ;; ;;     
    ;   ;; ;; ;; ;;  ;;;;;;   ;; ;;   ;;  ;;      ;; ;;;    
    ;   ;;  ;;;  ;;  ;;;;;;   ;;  ;;  ;;  ;;      ;;   ;;;  
    ;   ;;   ;   ;;  ;;       ;;   ;; ;;  ;;      ;;     ;; 
    ;   ;;       ;;  ;;       ;;   ;;;;;  ;;      ;;     ;; 
    ;   ;;       ;;  ;;       ;;    ;;;;   ;;    ;;  ;   ;; 
    ;   ;;       ;;  ;;       ;;     ;;;   ;;;;;;;;  ;; ;;; 
    ;   ;;       ;;  ;;;;;;;  ;;      ;;     ;;;;     ;;;;  
    
    (define/public (get-frame)
      f)
    
    ;; FIXME
    ;; No longer needed?
    (define/public (set-remote-host)
      'DoNothing
      #|
      (send model set-remote-host
            (send host get-value)
            (send port get-value))
      |#
      )
    
    (define (populate-menu-bar)
      
      (define help (new menu%
                        [label "&Help"]
                        [parent menu-bar]))
      
      (new menu-item%
           [label (format "Version: ~a" VERSION)]
           [parent help]
           [callback (λ (m e) '...)])
      
      ;; FIXME 
      ;; Probably handled at app startup now.
      ;; In case we need it
      ;; (set-remote-host)
      'JustDefine?
      )
    
    (define menu-bar (new menu-bar% [parent f]))
    (populate-menu-bar)
    
    ;;;;;;;
    
    (define (update-model)
      'DoNothing 
      
      ;; FIXME This can become a menu option.
      (when (not (zero? (length (send model get-arduino-ports))))
        (send model set-arduino-port 
              (send serial-port get-string 
                    (send serial-port get-selection))))
      (send model set-board-type 
            (send board get-string 
                  (send board get-selection)))
      
      )
    
    
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
      
      (send err-msg-text erase)
      (send err-msg-text insert (send model get-error-message))
      )
    
    
    (super-new)
    ))

