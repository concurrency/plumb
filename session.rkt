#lang racket

(require net/url)
(require "seq.rkt"
         "debug.rkt"
         "util.rkt"
         "response-handling.rkt")

(provide session%)


(define session%
  (class object%
    ;; Need to provide these
    (init [host "127.0.0.1"]
          [port 9000])
    
    ;; These default to false
    (field [id false]
           [conf false]
           [board false]
           [compilation-result false]
           )
    
    (define/public (get-id) id)
    
    (define/public (get-compilation-result)
      compilation-result)
    
    (define (start-session)
      ;; Create a new process object
      (define p (new process% [context 'SESSION-START]))
      
      ;; Define a sequence of operations
      (seq p
        ;; We should be in the initial state, and flag a generic error
        ;; in the event of problems.
        [(initial? 'ERROR)
         (debug 'START-SESSION "DEFAULT ERROR: ~a" (send p to-string))
         (debug 'START-SESSION "SERVER URL: ~a" 
                (url->string
                 (make-server-url host port "start-session")))
         ;; Nothing should change as a result of this operation
         NO-CHANGE]
        
        ;; We should still be in the initial state, and should
        ;; flag a bad connection if all goes wrong.
        [(initial? 'ERROR-NO-CONNECTION)
         (get-pure-port (make-server-url host port "start-session"))]
        
        ;; Now we should have a port, and flag a bad response if 
        ;; things go pear shaped.
        [(port? 'ERROR-PROCESS-RESPONSE)
         (debug 'START-SESSION "PORT: ~a" (send p to-string))
         (process-response (send p get))]
        
        ;; The response should give us a hash table; we'll pull
        ;; out the session ID.
        [(hash? 'ERROR-BAD-RESPONSE)
         (debug 'START-SESSION "RESPONSE: ~a" (send p to-string))
         (hash-ref (send p get) 'sessionid)]
        
        ;; The session ID should be a symbol. We're just displaying
        ;; it as a debug here, so this step should yield no changes.
        [(string? 'ERROR-SESSION-ID-NOT-A-STRING)
         (debug 'START-SESSION "SESSION ID: ~a" (send p get))
         NO-CHANGE])
      
      ;; Set the id according to what we retrieved
      (set! id (send p get))
      )
    
    (define (init)
      (start-session))
    
    (super-new)
    (init)))