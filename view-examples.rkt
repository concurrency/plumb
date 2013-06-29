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

(provide win-examples%
         menu-examples%)

(require racket/gui
         framework
         browser/external
         browser
         net/url)

(require "mvc.rkt"
         "seq.rkt"
         "util.rkt"
         "debug.rkt")

(define after%
  (class text%
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
    
    (define (reapply-syntax)
      (define txt (send this get-text))
      (define delta (new style-delta%))
      ;; Clear all color.
      (send delta set-delta-foreground "Black")
      (send this change-style delta 0 (string-length (send this get-text)) #f)
      (apply-syntax-highlighting))
    
    (define/augment (after-insert start len)
      (reapply-syntax))
    (define/augment (after-delete start len)
      (reapply-syntax))
    
    (define/public (apply-syntax-highlighting)
      (define txt (send this get-text))
      (define delta (new style-delta%))
      
      ;; Hightlight Keywords
      (send delta set-delta-foreground bordeaux)
      (for ([pat (list "PROC" "SEQ" "PAR" "IF" "WHILE" "SKIP" "STOP" "IS")])
        (for ([loc (regexp-match-positions* pat txt)])
          (send this change-style delta (car loc) (cdr loc) #f)))
      
      ;; Constants
      (send delta set-delta-foreground purplejam)
      (for ([pat (list "TRUE" "FALSE" "[0-9]+")])
        (for ([loc (regexp-match-positions* pat txt)])
          (send this change-style delta (car loc) (cdr loc) #f)))
      
      ;; Comments
      (send delta set-delta-foreground columbiablue)
      (for ([pat (list "--.*?\n")])
        (for ([loc (regexp-match-positions* pat txt)])
          (send this change-style delta (car loc) (cdr loc) #f)))
      
      ;; Types
      (send delta set-delta-foreground green)
      (for ([pat (list "INT" "BYTE" "BOOL" "CHAN" "INITIAL" "LEVEL" "REAL32" "INT16" "INT32")])
        (for ([loc (regexp-match-positions* pat txt)])
          (send this change-style delta (car loc) (cdr loc) #f))))
    (super-new)))


(define win-examples%
  (class view%
    (init-field model main conf)
    (field [temp-file false])
    
    (define f (new frame% 
                   [label (hash-ref conf 'tweet)]
                   [width 600]
                   [height 500]))
    
    (define editor-canvas (new editor-canvas%
                               [parent f]
                               [label ""]
                               [stretchable-height true]))
    
    ;; keymap% and text%
    (define keymap (keymap:get-editor))
    ; OR
    #|
    (define keymap (new keymap%))
    (add-text-keymap-functions keymap) 
    (send keymap map-function "c:x" "cut-clipboard")
    (send keymap map-function "c:c" "copy-clipboard")
    (send keymap map-function "c:v" "paste-clipboard")
    (send keymap map-function "middlebutton" "paste-x-selection")
    |# 
    
    (define text (new after%))
    (send text set-keymap keymap)
    
    
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
    
    (define (replace-tags-in-code conf)
      (let ([code (send model get-static #:as 'text (hash-ref conf 'path))]
            [result '()])
        
        ;; Append a standard header
        (when (and (hash-has-key? conf 'name)
                   (hash-has-key? conf 'tweet)
                   (hash-has-key? conf 'author)
                   (hash-has-key? conf 'email))
          (set! code 
                (string-append
                 "-- {{name}}\n-- {{tweet}}\n-- {{author}} ({{email}})\n\n"
                 code)))
        
        (for ([line (regexp-split "\n" code)])
          (for ([key (hash-keys conf)])
            (set! line (regexp-replace* (format "{{~a}}" key)
                                        line
                                        (hash-ref conf key))))
          (set! result (snoc result (format "~a~n" line))))
        (apply string-append result)))
    
    (define/public (load-example)
      (debug 'LOAD-EXAMPLE "Loading from conf:~n~a" conf)
      (define content (replace-tags-in-code conf))
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
      (send text apply-syntax-highlighting))
    
    
    
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

(require json)
 
(define menu-examples%
  (class view%
    (init-field model main menu)
    
    (define/override (update)
      'FIXME)
    
    ;; Move list of allowed categories to server?
    (define categories 
      (let ()
        (filter (λ (s)
                  (>= (string-length s) 1))
                (send model get-static #:as 'text "categories.conf"))))
    
    (define (allowed-category? o)
      (member o categories))
    
    ;; https://api.github.com/repos/concurrency/plumbing-examples/contents/repositories.conf
    ;; Use the API
    (define/public (get-menus)
      (define menu-hash (make-hash))
      
      (define (extend-category! conf)
        (when (and (hash? conf)
                   (hash-ref conf 'category))
          (let ([ls (hash-ref menu-hash (hash-ref conf 'category) (λ () empty))])
            (set! ls (snoc ls conf))
            (hash-set! menu-hash (hash-ref conf 'category) ls))))
      
      (define p (new process% [context 'GET-MENUS]))
      
      (debug (send p get-context) "Getting menus.")
      (seq p
        [(initial? 'ERROR-READ-WHOLE-URL)
         (debug (send p get-context) "Getting content from server." )
         (send model get-static #:as 'text "plumbing-examples" "paths.conf")]
        [(list? 'ERROR2)
         (debug (send p get-context) "REPOSES:~n~a" (send p get))
         (for ([path (send p get)])
           (debug (send p get-context) "Considering [~a]" path)
           (when (and (< 2 (string-length path))
                      (not (regexp-match "#" path)))
             (debug (send p get-context) "REPOS: ~a" path)
             (let* ([raw-conf (send model get-static #:as 'text (format 
                                                                 "~a/~a/~a"
                                                                 "plumbing-examples"
                                                                 path
                                                                 "info.conf"))])
               (debug (send p get-context) "raw-conf: ~a" raw-conf)
               (let ([conf (process-config raw-conf)])
                 (debug (send p get-context) "CONF: ~a" conf)
                 (extend-category! conf)))))]
        )
      menu-hash
      )
    
    (define menus (get-menus))
    
    ;; Build the menus
    (for ([c categories])
      (define tmp (new menu%
                       [parent menu]
                       [label c]
                       ))
      (for ([s (hash-ref menus c)])
        (new menu-item% 
             [parent tmp]
             [label (hash-ref s 'tweet)]
             [callback (λ (m e)
                         (new win-examples% 
                              [model model]
                              [main main]
                              [conf s]))]
             )))
    
    (super-new)
    ))