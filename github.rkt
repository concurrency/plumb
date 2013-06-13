#lang racket

(provide (all-defined-out))
(require "seq.rkt"
         "debug.rkt"
         net/url
         "util.rkt"
         json)

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