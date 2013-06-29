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