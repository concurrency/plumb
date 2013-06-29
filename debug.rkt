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

(define FLAGS (make-parameter '()))

(define (enable-debug! key)
  (FLAGS (cons key (FLAGS))))

(define (disable-debug! key)
  (FLAGS (filter (λ (v)
                   (not (equal? v key)))
                 (FLAGS))))

(define debug-channel false)
(define debug-thread false)

(define (set-debug-thread! t)
  (set! debug-thread t))

(define (set-debug-channel! c)
  (set! debug-channel c))

(define (set-textual-debug)
      (let ([c (make-channel)])
        (set-debug-channel! c)
        (when debug-thread
          (kill-thread debug-thread))
        (set-debug-thread! (thread (λ ()
                                     (let loop ()
                                       (printf "~a" (channel-get c))
                                       (loop)))))
        ))

(define-syntax-rule (debug key msg args ...)
  (when (or (member key (FLAGS))
            (member 'ALL (FLAGS)))
    (define new (make-hash))
    (define (filter-hash o)
      (if (hash? o)
          (begin
            (hash-for-each 
             o (λ (k v)
                 (if (equal? k 'hex)
                     (hash-set! new k (string-length (format "~a" v)))
                     (hash-set! new k v))))
            new)
          o))
    
    (channel-put 
     debug-channel       
     (format "[~a] ~a~n"
             key
             (apply format (cons msg (map filter-hash (list args ...))))))))