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

(provide code%)
(require racket/gui
         framework)
(require "debug.rkt")

(define FONT-SIZE 
  (case (system-type)
    [(macosx) 16]
    [(win windows) 12]
    [(unix) 12]
))

(define code%
  (class (text:line-numbers-mixin 
          (editor:standard-style-list-mixin
           (editor:basic-mixin text%)))
    (init-field tab-panel ide)
    (field [saved? false]
           [keymap false]
           ; [filename false]
           )
    
    (define/public (is-saved?)
      saved?)
    
    (define/public (not-saved?)
      (not saved?))
    
    (define/public (set-saved!)
      (send tab-panel show-clean)
      (set! saved? true))
    
    (define/public (set-dirty!)
      (send tab-panel show-dirty)
      (set! saved? false))
    
    (define/public (save-yourself)
      (with-output-to-file (send this get-filename)
        #:exists 'replace
        (λ ()
          (printf "~a" 
                  (send this get-text 0 'eof))))
      (set-saved!))
    
    (define/override (save-file)
      (save-yourself))
    
    (define (hex->triplet str)
      (define (conv ls)
        (string->number (second ls) 16))
      (map conv
           (list (regexp-match "#(..)...." str)
                 (regexp-match "#..(..).." str)
                 (regexp-match "#....(..)" str))))
    
    ;; These are bad copy-pastes from the WWW.
    ;; I'm going to force the cleanup with a regexp.
    (define blue (apply make-color (hex->triplet "#0000FF")))
    (define black (apply make-color (hex->triplet "#000000")))
    (define columbiablue (apply make-color (hex->triplet "#87AFC7")))
    (define magenta (apply make-color (hex->triplet "#FF00FF")))
    (define violet (apply make-color (hex->triplet "#6A5ACD")))
    (define cyan (apply make-color (hex->triplet "#008A8C")))
    (define green (apply make-color (hex->triplet "#2E8B57")))
    (define darkgreen (apply make-color (hex->triplet "#347235")))
    (define bordeaux (apply make-color (hex->triplet "#A52A2A")))
    (define red (apply make-color (hex->triplet "#FF0000")))
    (define yellow (apply make-color (hex->triplet "#FFFF00")))
    (define purple (apply make-color (hex->triplet "#A020F0")))
    (define purplejam (apply make-color (hex->triplet "#6A287E")))
    (define beer (apply make-color (hex->triplet "#FBB117")))
    
    (define (reapply-syntax start end)
      (define txt (send this get-text start end))
      (define delta (new style-delta%))
      (send delta set-delta 'change-size FONT-SIZE)
      (send this change-style delta start (- end start) #f)
      (apply-syntax-highlighting start end))
    
    (define (count-spaces-from posn)
      ;(printf "c[~a] = ~a~n" posn (send this get-character posn))
      (cond
        [ (equal? #\space (send this get-character posn))
          (add1 (count-spaces-from (add1 posn)))]
        [else 0]))
    
    (define (auto-indent? start end)
      (define indent false)
      (for ([p '("PAR" "SEQ" "IF" "ALT" "PRI ALT" "PROC" "WHILE")])
        (when (send this find-string p 'forward start end)
          (set! indent true)))
      indent)
    
    (define (insert-spaces posn)
      (when (equal? (send this get-character posn) #\newline)
        ;(printf "This newline posn: ~a~n" posn)
        ;(printf "Prev newline posn: ~a~n" (send this find-newline 'backward (sub1 posn)))
        ;(printf "Line: ~a~n" (send this position-line posn))
        ;(printf "Start line posn: ~a~n" (send this line-start-position (send this position-line posn)))
        ;(printf "Spaces: ~a~n" (count-spaces-from (send this line-start-position (send this position-line posn))))
        (let* ([line-start
                (send this line-start-position 
                      (send this position-line posn))]
               [line-end (send this find-newline 'forward line-start)]
               [front-spaces (count-spaces-from line-start)])
          (when (auto-indent? line-start line-end)
            (set! front-spaces (+ 2 front-spaces)))
          
          (when (not (zero? front-spaces))
            (send this insert front-spaces (make-string front-spaces #\space) (add1 posn)))
          
          )))
    
    
    ;; This doesn't work.
    (define/public (highlight-line n)
      (define loc 0)
      (for ([i (in-range (sub1 n))])
        (set! loc (+ loc (send this find-newline 'forward loc))))
      
      (let* ([line-start
              (send this line-start-position 
                    (send this position-line loc))]
             [line-end (send this find-newline 'forward line-start)])
        'NeedToGetMixinRightFIXME
        ;;(send this highlight-range line-start line-end yellow)
        ))
    
    ;; Handle copy-paste of bodies of text.
    (define/augment (after-insert start len)
      (set-dirty!)
      (define end (+ start len))
      (cond
        ;; When we have a colon, see if it is closing a proc, and 
        ;; take us to the start of the line. (There are some cases where
        ;; this won't be correct, but it is infrequent enough.)
        [(equal? (send this get-character start) #\:)
         (let* ([line-start
                 (send this line-start-position (send this position-line start))]
                [line-end (send this find-newline 'forward line-start)]
                [this-line (send this get-text line-start line-end)])
           (when (regexp-match  #px"^[[:space:]]+:[[:space:]]*$" this-line)
             (send this delete line-start line-end)
             (send this insert #\:)
             (send this insert #\newline)))]
        [else
         (let loop ([start start])
           ;; (printf "LOOP: ~a -> ~a~n" start end)
           (unless (> start end)
             (let* ([line-start
                     (send this line-start-position (send this position-line start))]
                    [line-end (send this find-newline 'forward line-start)])
               ;; (printf "S[~a] L[~a]~n" line-start line-end)
               (insert-spaces start)
               (reapply-syntax line-start line-end)
               (loop (add1 line-end)))))]))
    
    (define/augment (after-delete start len)
      (set-dirty!)
      
      (let* ([line-start
              (send this line-start-position 
                    (send this position-line start))]
             [line-end (send this find-newline 'forward line-start)])
        (reapply-syntax line-start line-end)
        ))
    
    (define/public (apply-syntax-highlighting start end)
      (define txt (send this get-text start end))
      ;; (printf "ASH: ~s~n" txt)
      (define delta (new style-delta%))
      (send delta set-delta 'change-size FONT-SIZE)
      (send delta set-family 'modern)
      (send delta set-delta-foreground "Black")
      (send this change-style delta start end #f)
      
      ;; Hightlight Keywords
      (send delta set-delta-foreground bordeaux)
      (for ([pat (list "PROC" "SEQ" "PAR" "IF" "WHILE" "SKIP" "STOP" "IS" "ALT" "PRI ALT")])
        (for ([loc (regexp-match-positions* pat txt)])
          (send this change-style delta (+ start (car loc)) (+ start (cdr loc)) #f)))
      
      ;; Constants
      (send delta set-delta-foreground purplejam)
      (for ([pat (list "TRUE" "FALSE" "[0-9]+")])
        (for ([loc (regexp-match-positions* pat txt)])
          (send this change-style delta (+ start (car loc)) (+ start (cdr loc)) #f)))
      
      ;; Comments
      (send delta set-delta-foreground columbiablue)
      (for ([pat (list "--.*$")])
        (for ([loc (regexp-match-positions* pat txt)])
          (send this change-style delta (+ start (car loc)) (+ start (cdr loc)) #f)))
      
      ;; Types
      (send delta set-delta-foreground green)
      (for ([pat (list "INT" "BYTE" "BOOL" "CHAN" "INITIAL" "LEVEL" "REAL32" "INT16" "INT32")])
        (for ([loc (regexp-match-positions* pat txt)])
          (send this change-style delta (+ start (car loc)) (+ start (cdr loc)) #f))))
    
    
    
    (define (setup-keymap)
      (define k (new keymap:aug-keymap%))
      
      (define-syntax-rule (mapfun key name)
        (case (system-type)
          [(macosx) 
           (send k map-function (format "d:~a" key) name)]
          [(win windows)
           (send k map-function (format "c:~a" key) name)]))
      
      (mapfun "w" "close-tab")
      (send k add-function "close-tab"
            (λ (o e)
              (debug 'KEYMAP "close-tab")
              (send tab-panel close-tab)))
      
      (mapfun "t" "new-tab")
      (mapfun "n" "new-tab")
      (send k add-function "new-tab"
            (λ (o e) (send tab-panel new-document)))
      
      (mapfun "s" "save")
      (send k add-function "save"
            (λ (o e) (send tab-panel save)))
      
      
      
      ;; Send this last
      (send k chain-to-keymap (keymap:get-global) false)
      (send k chain-to-keymap (keymap:get-editor) false)
      
      k)
    
    (define/public (setup-code)
      
      
      (define delta (new style-delta%))
      
      
      (send this set-keymap (setup-keymap))
      
      (send delta set-family 'modern)
      ;(send delta set-weight-on 'bold)
      ;(send delta set-size-add 4)
      (send delta set-delta 'change-size FONT-SIZE)
      (send this change-style delta 0 (send this last-position) #f)
      (send this apply-syntax-highlighting 0 (send this last-position))
      
      ;(send this set-keymap keymap)
      (set-dirty!)
      
      (send this show-line-numbers! true)
      
      (send this set-max-undo-history 'forever)
      )
    
    
    (super-new)
    
    ))
