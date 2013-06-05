#lang racket

(require racket/gui
          racket/date
          net/url)

(require "debug.rkt"
         "app-type.rkt"
         "model-plumb.rkt"
         "view-main.rkt"
         "view-feedback.rkt"
         "util.rkt"
         )

;; DEFINITIONS

(define (alert-dialog msg and-after)
  (define f (new frame% [label "Badness 10000"]))
  (define m (new message% [label msg] [parent f]))
  (define b (new button%
                 [label "Amazing!"]
                 [parent f]
                 [callback (λ (b e)
                             (case and-after
                               [(exit quit) (exit)]
                               [else
                                (send f show false)]))]))
  (send f show true))
                           
                                
;; Grab the host and port from the server
(define (compilation-server-config)
  (with-handlers ([exn:fail? 
                   (λ (e)
                     (alert-dialog (->string e) 'exit))])
    (define h 
      (read
       (get-pure-port
        (string->url
         "https://raw.github.com/concurrency/plumb/master/conf/conf-compilation-server.rkt"))))
    (debug 'APP-LAUNCH "HOST CONFIG: ~a" h)
    (cond
      [(hash? h)
       (let ([host (hash-ref h 'host)]
             [port (hash-ref h 'port)])
         (debug 'APP-LAUNCH "HOST ~a PORT ~a" host port)
         (send model set-remote-host host port))]
      [else (alert-dialog "Something went very wrong." 'exit)])
    ))

;; DO STUFF

(enable-debug! 'ALL)
;; Initialize the model
(define model (new plumb%))
;; Can't debug until we load the config.
(send model load-config)

;; Load the remote server config
(compilation-server-config)

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


(debug 'APP-LAUNCH "=== ~a ===" (date->string (seconds->date (current-seconds)) true))
(debug 'APP-LAUNCH "current-directory [~a]" (current-directory))
(debug 'APP-LAUNCH "find-system-path 'run-file [~a]" (find-system-path 'run-file))
(debug 'APP-LAUNCH "CONTENTS [~a]" (send model get-config 'CONTENTS))
