#lang racket

(provide win-examples%
         menu-examples%)

(require racket/gui
         browser/external
         browser
         net/url)

(require "mvc.rkt"
         "seq.rkt"
         "util.rkt"
         "debug.rkt")

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
    
    (define (replace-tags-in-code conf)
      (define gh (new github% 
                      [owner OWNER]
                      [repos "plumbing-examples"]))
      
      (let ([code (send gh get-content (hash-ref conf 'path))]
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

(require json)

;(define OWNER "concurrency")
(define OWNER "jadudm")
;(define PROCESSOR b64-decode)
(define PROCESSOR (λ (o) o))

(define github%
  (class object%
    (init-field owner repos)
    ; https://bitbucket.org/api/1.0/repositories/jadudm/plumbing-examples/src/master/REDME.md
    ;(define root "https://api.github.com/repos")
    (define root "https://bitbucket.org/api/1.0/repositories")
    (define CONTENT-KEY 'data)
    (define/public (get path)
      (define p (new process%))
      (seq p
        [(initial? 'ERROR-GH1)
         (format "~a/~a/~a/src/master/~a"
                                  root
                                  owner
                                  repos
                                  path)]
        [(string? 'ERROR-GH2)
         (debug 'GITHUB "URL [~a]" (send p get))
         (read-url (send p get))]
        [(string? 'ERROR-GH3)
         (debug 'GITHUB "Response [~a]" (send p get))
         (string->jsexpr (send p get))]
        [(hash? 'ERROR-GH4)
         NO-CHANGE])
      (send p get))
      
    
    (define/public (get-content path)
      (debug 'GITHUB "Fetching [~a]" path)
      (let ([h (get path)])
        (when (and (hash? h)
                   (hash-has-key? h CONTENT-KEY))
          (PROCESSOR (hash-ref h CONTENT-KEY)))))
    
    (super-new)
    ))
                                  
                          

(define menu-examples%
  (class view%
    (init-field model main menu)
    
    (define/override (update)
      'FIXME)
   
    
    ;; Move list of allowed categories to server?
    (define categories '("Testing" "Basics"))
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
        [(initial? 'ERROR-GET-ROOT)
         (debug (send p get-context) "Generating github% object")
         (new github% [owner OWNER] [repos "plumbing-examples"])]
        [(object? 'ERROR-READ-WHOLE-URL)
         (debug (send p get-context) "Getting content from ~a" (send p get))
         (send (send p get) get-content "paths.conf")]
        [(string? 'ERROR1)
         (debug (send p get-context) "result:~n~s" (send p get))
         (regexp-split "\n" (send p get))]
        [(list? 'ERROR2)
         (debug (send p get-context) "REPOSES:~n~a" (send p get))
         (for ([path (send p get)])
           (debug (send p get-context) "Considering [~a]" path)
           (when (and (< 2 (string-length path))
                      (not (regexp-match "#" path)))
             (debug (send p get-context) "REPOS: ~a" path)
             (let* ([gh (new github% [owner OWNER] [repos "plumbing-examples"])]
                    [raw-conf (send gh get-content (format "~a/~a" path "info.conf"))])
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