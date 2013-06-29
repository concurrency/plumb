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

(require racket/gui)

(require "debug.rkt"
         "model-plumb.rkt"
         "byoe-window.rkt"
         "byoe-feedback.rkt"
         "util-gui.rkt"
         )

;; DO STUFF

(define (build-ide hardware)
  ;; Initialize the main view
  (define win-main (new win-main% [model hardware]))
  (send hardware add-view win-main)
  ;; Init the feedback view
  (define win-feedback (new win-feedback% [model hardware]))
  ;; Update everyone
  (send hardware update)
  ;; Say Hello
  (send hardware say-hello)
  ;; Show the main view
  (send win-main show true)
  
  (check-version (send win-main get-frame))
  )

(define (create)
  ;; Interaction
  ;; Need this to build the IDE
  (define hardware (new plumb%))
  (send hardware load-config)
  (send hardware enumerate-arduinos)
  (send hardware compilation-server-config)
  (enable-debug! 'ALL)
  (set-textual-debug)
  (build-ide hardware)
  )

(create)
