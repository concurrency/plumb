#lang racket

(provide menu-examples%
         replace-tags-in-code)

(require racket/gui
         framework
         browser/external
         browser
         net/url
         json)

(require "mvc.rkt"
         "seq.rkt"
         "util.rkt"
         "debug.rkt"
         "github.rkt")

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
    

(define menu-examples%
  (class view%
    (init-field model main menu callback)
    
    (define/override (update)
      'FIXME)
    
    ;; Move list of allowed categories to server?
    (define categories 
      (let ([gh (new github%
                     [owner "jadudm"]
                     [repos "plumbing-examples"])])
        (filter (λ (s)
                  (> (string-length s) 2))
                (regexp-split "\n" (send gh get-content "categories.conf")))))
    
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
             [callback (callback s)]
             )))
    
    (super-new)
    ))