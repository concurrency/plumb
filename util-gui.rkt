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
(require racket/gui
         browser/external)
(require "util.rkt"
         "debug.rkt"
         "version.rkt")


(define (check-version f)
      (let* ([remote-version
             (safe-url-fetch 
              read 
              "http://concurrency.cc/plumb/client-conf/version.txt"
              #:default "1000000")]
            [changelog (safe-url-fetch port->string 
                                       (format "http://concurrency.cc/plumb/client-conf/~a-changelog.txt"
                                               remote-version)
                                       #:default "Important stuff. Get it!")])
        
        (debug 'CHECK-VERSION "[~a] LOCAL [~a] REMOTE" 
               VERSION
               remote-version)
        
        (define block? true)
        (define get? false)
        
        (when (> remote-version
                 (string->number VERSION))
          (debug 'CHECK-VERSION "Newer version exists!")
          
          (define vf (new dialog%
                          [label "New Version!"]
                          [parent f]
                          ))
          (new message% 
               [label (format "You're running version ~a of Plumb." VERSION)]
               [parent vf])
          
          (new message%
               [label    (format "We recommend version ~a instead." remote-version)]
               [parent vf])
          
          (define ed (new editor-canvas%
                          [parent vf]
                          ))
          
          (define t (new text% [auto-wrap true]))
          (send ed set-editor t)
          (send ed set-line-count 6)
          (send t insert changelog 
                (send t get-end-position))
          
          
          (define h (new horizontal-panel%
                         [parent vf]
                         [stretchable-width true]))
          (define b (new button%
                         [label "Later..."]
                         [parent h]
                         [stretchable-width true]
                         [callback (λ (b e)
                                     (set! block? false)
                                     (send vf show false))]))
          (define b2 (new button%
                          [label "Take me there!"]
                          [parent h]
                          [stretchable-width true]
                          [callback (λ (b e)
                                      (set! get? true)
                                      (set! block? false)
                                      (send vf show false)
                                      )]))
          (send vf show true)
          (let loop () (when block? (yield) (loop)))
          (when get? (send-url "http://concurrency.cc/downloads/"))
          )))