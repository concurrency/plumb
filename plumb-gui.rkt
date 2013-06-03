#lang racket

(require racket/gui)

(require "debug.rkt"
         "app-type.rkt"
         "model-plumb.rkt"
         "view-main.rkt"
         "view-feedback.rkt"
         )


(enable-debug! 'ALL)
;; Initialize the model
(define model (new plumb%))
(send model enumerate-arduinos)

;; Initialize the main view
(define win-main (new win-main% [model model]))
(send model add-view win-main)

;; Init the feedback view
(define win-feedback (new win-feedback% [model model]))

;; Update everyone
(send model update)

;; Show the main view
(send win-main show true)