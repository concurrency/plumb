#lang racket

(require racket/gui
         racket/date
         net/url)

(require "debug.rkt"
         "model-plumb.rkt"
         "byoe-window.rkt"
         "byoe-feedback.rkt"
         "util.rkt"
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
