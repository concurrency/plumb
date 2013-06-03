#lang racket

(provide plumb%)

(require racket/gui
         net/url
         net/dns)
(require net/url)
(require "arduino-interaction.rkt"
         "util.rkt"
         "mvc.rkt"
         "seq.rkt"
         "debug.rkt"
         "util.rkt"
         "response-handling.rkt"
         "code-execution.rkt"
         "config-client.rkt"
         )

(define MIN-FIRMWARE-SIZE 10000)

(define plumb%
  (class model%
    (super-new)
    (inherit update add-view)
    
    ;   ;;;;;;; ;;  ;;;;;;;  ;;      ;;;;;;       ;;   
    ;   ;;;;;;; ;;  ;;;;;;;  ;;      ;;;;;;;    ;;;;;  
    ;   ;;      ;;  ;;       ;;      ;;    ;;   ;; ;   
    ;   ;;      ;;  ;;       ;;      ;;     ;;  ;;     
    ;   ;;;;;;  ;;  ;;;;;;   ;;      ;;      ;  ;;;    
    ;   ;;;;;;  ;;  ;;;;;;   ;;      ;;      ;    ;;;  
    ;   ;;      ;;  ;;       ;;      ;;      ;      ;; 
    ;   ;;      ;;  ;;       ;;      ;;     ;;      ;; 
    ;   ;;      ;;  ;;       ;;      ;;    ;;   ;   ;; 
    ;   ;;      ;;  ;;       ;;;;;;; ;;;;;;;    ;; ;;; 
    ;   ;;      ;;  ;;;;;;;  ;;;;;;; ;;;;;;      ;;;;  
    
    (field [host false]
           [port false]
           [id false]
           
           [config false]
           [board-config false]
           
           [arduino-ports empty]
           [arduino-port false]
           [board-type false]
           
           [main-file false]
           [temp-dir false]
           [firmware-location false]
           
           [compilation-result false]
           [message "Parallel programming for makers."]
           
           [first-compilation? true]
           )
    
    
    ;     ;;    ;;;;;;;    ;;      ;;    ;;     ;;;;     ;;      ;;    ;;   
    ;   ;;;;;   ;;;;;;;  ;;;;;   ;;;;;   ;;   ;;;;;;;;   ;;;     ;;  ;;;;;  
    ;   ;; ;    ;;       ;; ;    ;; ;    ;;  ;;;    ;;;  ;;;;    ;;  ;; ;   
    ;   ;;      ;;       ;;      ;;      ;;  ;;      ;;  ;;;;;   ;;  ;;     
    ;   ;;;     ;;;;;;   ;;;     ;;;     ;;  ;        ;  ;; ;;   ;;  ;;;    
    ;     ;;;   ;;;;;;     ;;;     ;;;   ;;  ;        ;  ;;  ;;  ;;    ;;;  
    ;       ;;  ;;           ;;      ;;  ;;  ;        ;  ;;   ;; ;;      ;; 
    ;       ;;  ;;           ;;      ;;  ;;  ;;      ;;  ;;   ;;;;;      ;; 
    ;   ;   ;;  ;;       ;   ;;  ;   ;;  ;;  ;;;    ;;   ;;    ;;;;  ;   ;; 
    ;   ;; ;;;  ;;       ;; ;;;  ;; ;;;  ;;   ;;;;;;;;   ;;     ;;;  ;; ;;; 
    ;    ;;;;   ;;;;;;;   ;;;;    ;;;;   ;;     ;;;;     ;;      ;;   ;;;;  
    
    (define/public (get-id) id)
    
    (define (get-new-session-id)
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
    
    (define/public (set-remote-host h p)
      (set! host (dns-get-address 
                  (dns-find-nameserver)
                  h))
      (set! port (string->number p))
      (update))
    
    
    ;   ;;;;;;     ;;;;     ;;;;;   ;;;;;;;;    ;;   
    ;   ;;  ;;;  ;;;;;;;;   ;;;;;;; ;;;;;;;;  ;;;;;  
    ;   ;;   ;; ;;;    ;;;  ;;   ;;    ;;     ;; ;   
    ;   ;;   ;; ;;      ;;  ;;   ;;    ;;     ;;     
    ;   ;;   ;; ;        ;  ;;   ;;    ;;     ;;;    
    ;   ;;;;;;; ;        ;  ;;;;;;     ;;       ;;;  
    ;   ;;;;;;  ;        ;  ;;;;;;     ;;         ;; 
    ;   ;;      ;;      ;;  ;;  ;;     ;;         ;; 
    ;   ;;      ;;;    ;;   ;;   ;;    ;;     ;   ;; 
    ;   ;;       ;;;;;;;;   ;;    ;;   ;;     ;; ;;; 
    ;   ;;         ;;;;     ;;    ;;;  ;;      ;;;;  
    
    (define/public (enumerate-arduinos)
      (set! arduino-ports (map ->string (list-arduinos)))
      (update))
    
    (define/public (get-arduino-ports) arduino-ports)
    
    (define (port->platform-specific-port sp)
      (case (system-type)
        [(macosx unix) (format "/dev/~a" sp)]
        [(windows) sp]))
    
    (define/public (set-arduino-port p) 
      (set! arduino-port (port->platform-specific-port p)))
    
    
    ;   ;;;;;;      ;;;;        ;;     ;;;;;    ;;;;;;    
    ;   ;;  ;;    ;;;;;;;;      ;;     ;;;;;;;  ;;;;;;;   
    ;   ;;  ;;;  ;;;    ;;;    ;;;;    ;;   ;;  ;;    ;;  
    ;   ;;  ;;;  ;;      ;;    ;;;;    ;;   ;;  ;;     ;; 
    ;   ;;  ;;   ;        ;    ;  ;;   ;;   ;;  ;;      ; 
    ;   ;;;;;;;  ;        ;   ;;  ;;   ;;;;;;   ;;      ; 
    ;   ;;   ;;; ;        ;   ;;   ;   ;;;;;;   ;;      ; 
    ;   ;;   ;;; ;;      ;;  ;;;;;;;;  ;;  ;;   ;;     ;; 
    ;   ;;   ;;; ;;;    ;;   ;;;;;;;;  ;;   ;;  ;;    ;;  
    ;   ;;;;;;;   ;;;;;;;;   ;;     ;; ;;    ;; ;;;;;;;   
    ;   ;;;;;;      ;;;;    ;;      ;; ;;    ;;;;;;;;;    
    
    (define (board-choice->board-type choice)
      (case choice
        [("Arduino Duemilanove") "arduino"]
        [else "arduino"]))
    
    (define/public (set-board-type b)
      (set! board-type (board-choice->board-type b)))
    
    (define/public (get-board-type) board-type)
    
    (define (get-board-config)
      (define gbc (new process% [context 'RETRIEVE-BOARD-CONFIG]))
      (define firm (new process% [context 'RETRIEVE-BOARD-FIRMWARE]))
      
      (seq gbc
        ;; Create the URL
        [(initial? 'ERROR-GENERATING-URL)
         (make-server-url host port "board" board-type)]
        ;; Get a port
        [(url? 'ERROR-CREATING-PORT)
         (get-pure-port (send gbc get))]
        ;; Parse the response
        [(port? 'ERROR-PARSING-RESPONSE)
         (process-response (send gbc get))]
        ;; Store it
        [(hash? 'ERROR-STORING-BOARD-CONFIG)
         (debug (send gbc get-context) "~a" (filter-hash (send gbc get) 'hex))
         (set! board-config (send gbc get))
         NO-CHANGE])
      
      (seq firm
        ;; Create URL
        [(initial? 'ERROR-GENERATING-URL)
         (make-server-url host port "firmware" (hash-ref board-config 'firmware))]
        ;; Get a port
        [(url? 'ERROR-CREATING-PORT)
         (get-pure-port (send firm get))]
        ;; Parse the response
        [(port? 'ERROR-PARSING-RESPONSE)
         (process-response (send firm get))]
        ;; Check that it came down
        [(hash? 'ERROR-FIRMWARE-LOOKS-KINDA-SHORT)
         (let ([firm-leng (string-length (hash-ref (send firm get) 'hex))])
           (cond
             [(< firm-leng MIN-FIRMWARE-SIZE)
              (raise)]
             [else
              (debug (send firm get-context) "Firmware length: ~a" firm-leng)])
           NO-CHANGE)])
      )
    
    
    ;   ;;;;;;; ;;  ;;      ;;;;;;;    ;;   
    ;   ;;;;;;; ;;  ;;      ;;;;;;;  ;;;;;  
    ;   ;;      ;;  ;;      ;;       ;; ;   
    ;   ;;      ;;  ;;      ;;       ;;     
    ;   ;;;;;;  ;;  ;;      ;;;;;;   ;;;    
    ;   ;;;;;;  ;;  ;;      ;;;;;;     ;;;  
    ;   ;;      ;;  ;;      ;;           ;; 
    ;   ;;      ;;  ;;      ;;           ;; 
    ;   ;;      ;;  ;;      ;;       ;   ;; 
    ;   ;;      ;;  ;;;;;;; ;;       ;; ;;; 
    ;   ;;      ;;  ;;;;;;; ;;;;;;;   ;;;;  
    
    (define/public (set-main-file f)
      (set! main-file f)
      (update))
    
    (define/public (main-file-set?)
      (and main-file (file-exists? main-file)))
    
    (define (create-temp-dir)
      (set! temp-dir
            (case (->sym (system-type))
              [(macosx) 
               (build-path (find-system-path 'temp-dir) id)]
              [(win windows)
               (for ([p (map getenv '(TMP TEMP USERPROFILE))]
                     #:when (directory-exists? p))
                 (build-path p id))]))
      (cond
        [(directory-exists? temp-dir)
         (debug 'CREATE-TEMP-DIR "Temp dir [~a] exists" temp-dir)]
        [else 
         (debug 'CREATE-TEMP-DIR "Creating [~a]" temp-dir)
         (make-directory temp-dir)]))
    
    (define (cleanup-temp-dir)
      (define extensions '(hex))
      (when (directory-exists? temp-dir)
        (for ([f (directory-list temp-dir)])
          (debug 'TEMP-DIR "Checking [~a] for removal." f)
          (when (member (->sym (file-extension f)) extensions)
            (debug 'TEMP-DIR "Removing [~a]." f)
            (delete-file (build-path temp-dir f))))
        (debug 'TEMP-DIR "Removing temp directory [~a]" temp-dir)
        (delete-directory temp-dir)
        (set! temp-dir false)))
    
    
    ;   ;;       ;;    ;;        ;;;;;    ;;   
    ;   ;;;     ;;;  ;;;;;     ;;;  ;;; ;;;;;  
    ;   ;;;;   ;;;;  ;; ;      ;      ; ;; ;   
    ;   ;; ;; ;; ;;  ;;       ;         ;;     
    ;   ;; ;; ;; ;;  ;;;      ;         ;;;    
    ;   ;;  ;;;  ;;    ;;;    ;    ;;;;   ;;;  
    ;   ;;   ;   ;;      ;;   ;      ;;     ;; 
    ;   ;;       ;;      ;;   ;      ;;     ;; 
    ;   ;;       ;;  ;   ;;   ;;     ;; ;   ;; 
    ;   ;;       ;;  ;; ;;;    ;;;;;;;; ;; ;;; 
    ;   ;;       ;;   ;;;;      ;;;;;;   ;;;;  
    
    
    (define/public (get-message) message)
    
    (define/public (get-compilation-result) compilation-result)
    
    
    ;                   ;;   ;;;     ;;;;      ;; ;;;;;;;;     ;;     ;;     ;;;
    ;                 ;;;;;   ;;    ;; ;;;     ;; ;;;;;;;;     ;;      ;;   ;;  
    ;                 ;; ;     ;;  ;;  ;;;;    ;;    ;;       ;;;;      ;;  ;;  
    ;                 ;;        ;;;;   ;;;;;   ;;    ;;       ;;;;      ;;;;;   
    ;          ;      ;;;        ;;;   ;; ;;   ;;    ;;       ;  ;;      ;;;    
    ;          ;        ;;;      ;;    ;;  ;;  ;;    ;;      ;;  ;;      ;;;    
    ;          ;          ;;     ;;    ;;   ;; ;;    ;;      ;;   ;      ;;;;   
    ;          ;          ;;     ;;    ;;   ;;;;;    ;;     ;;;;;;;;    ;; ;;   
    ;         ;       ;   ;;     ;;    ;;    ;;;;    ;;     ;;;;;;;;   ;;   ;;  
    ;         ;       ;; ;;;     ;;    ;;     ;;;    ;;     ;;     ;; ;;     ;; 
    ;         ;        ;;;;      ;;    ;;      ;;    ;;    ;;      ;;;;;      ;;
    ;    ;    ;                                                                 
    ;   ; ;  ;                                                                  
    ;      ; ;                                                                  
    ;       ;;                                                                  
    ;       ;;                                                                  
    ;        ;                                                                  
    
    (define (any? v) v)
    
    
    ;; FIXME
    ;; Need better checks down below
    (define FIXME (λ args true))
    (define/public (check-syntax)
      'FIXME)
    
    
    
    ;   ;;;;;;; ;;  ;;;;;    ;;       ;; ;;     ;     ;;    ;;     ;;;;;    ;;;;;;; 
    ;   ;;;;;;; ;;  ;;;;;;;  ;;;     ;;;  ;    ;;;    ;     ;;     ;;;;;;;  ;;;;;;; 
    ;   ;;      ;;  ;;   ;;  ;;;;   ;;;;  ;;   ;;;   ;;    ;;;;    ;;   ;;  ;;      
    ;   ;;      ;;  ;;   ;;  ;; ;; ;; ;;  ;;   ;;;   ;;    ;;;;    ;;   ;;  ;;      
    ;   ;;;;;;  ;;  ;;   ;;  ;; ;; ;; ;;   ;  ;; ;;  ;     ;  ;;   ;;   ;;  ;;;;;;  
    ;   ;;;;;;  ;;  ;;;;;;   ;;  ;;;  ;;   ;; ;; ;; ;;    ;;  ;;   ;;;;;;   ;;;;;;  
    ;   ;;      ;;  ;;;;;;   ;;   ;   ;;   ;; ;; ;; ;;    ;;   ;   ;;;;;;   ;;      
    ;   ;;      ;;  ;;  ;;   ;;       ;;   ;;;;   ;;;    ;;;;;;;;  ;;  ;;   ;;      
    ;   ;;      ;;  ;;   ;;  ;;       ;;    ;;;   ;;;    ;;;;;;;;  ;;   ;;  ;;      
    ;   ;;      ;;  ;;    ;; ;;       ;;    ;;;   ;;;    ;;     ;; ;;    ;; ;;      
    ;   ;;      ;;  ;;    ;;;;;       ;;    ;;     ;;   ;;      ;; ;;    ;;;;;;;;;; 
    
    (define (write-firmware)
      (define p (new process% 
                     [context 'WRITE-FIRMWARE]
                     [update (λ (msg)
                               (set! message (format "~a: ~a"
                                                     (send p get-context)
                                                     (->string msg)))
                               (update))]))
      (define FIRM (make-parameter "tvm.hex")) 
      (seq p
        ;; Get the firmware on first compilation
        [(initial? 'ERROR-RETRIEVING-FIRMWARE)
         (get-board-config)
         board-config]
        
        ;; Create a temp directory
        [(any? 'ERROR-CREATE-TEMP-DIRECTORY)
         (debug (send p get-context) "Creating temporary directory")
         (create-temp-dir)
         temp-dir]
        
        ;; Remove old firmware
        [(any? 'ERROR-REMOVE-OLD-FIRMWARE)
         (FIRM (build-path temp-dir (FIRM)))
         
         (debug (send p get-context)
                "Stale firmware at [~a]?"
                (FIRM))
         
         (when (file-exists? (FIRM))
           (debug (send p get-context) "Removing old firmware.")
           (delete-file (FIRM)))
         NO-CHANGE]
        
        ;; Write new firmware
        [(any? 'ERROR-WRITE-FIRMWARE)
         (debug (send p get-context) "Writing firmware to disk.")
         (with-output-to-file (FIRM)
           (thunk 
            (printf "~a~n" (hash-ref board-config 'hex))))
         (debug (send p get-context) "Written.")
         (set! firmware-location (FIRM))
         (file-size (FIRM))]
        
        [(number? 'ERROR-FIRMWARE-SIZE)
         (debug (send p get-context) "Checking filesize.")
         (when (< (send p get) MIN-FIRMWARE-SIZE)
           (set! firmware-location false)
           (set! message (format "TVM Too Small: ~a" (send p get))))
         NO-CHANGE]
        ))
    
    (define (upload-firmware)
      (define p (new process% 
                     [context 'UPLOAD-FIRMWARE]
                     [update (λ (msg)
                               (set! message (format "~a: ~a"
                                                     (send p get-context)
                                                     (->string msg)))
                               (update))]))
      (seq p
        [(initial? 'ERROR-UPLOADING-FIRMWARE)
         (debug (send p get-context) "Uploading firmware.")
         (let ([cmd (avrdude-cmd config firmware-location board-config arduino-port)])
           (debug (send p get-context) "CMD:~n\t~a" cmd)
           (exe-in-tempdir temp-dir cmd)
           )]
        
        [(zero? 'ERROR-WITH-AVRDUDE)
         (debug (send p get-context) "Upload successful.")
         NO-CHANGE]
        ))
    
    
    ;      ;;;;       ;;;;     ;;       ;;  ;;;;;;  ;;  ;;      ;;;;;;; 
    ;    ;;;;;;;;   ;;;;;;;;   ;;;     ;;;  ;;  ;;; ;;  ;;      ;;;;;;; 
    ;   ;;;     ;  ;;;    ;;;  ;;;;   ;;;;  ;;   ;; ;;  ;;      ;;      
    ;   ;;         ;;      ;;  ;; ;; ;; ;;  ;;   ;; ;;  ;;      ;;      
    ;  ;;          ;        ;  ;; ;; ;; ;;  ;;   ;; ;;  ;;      ;;;;;;  
    ;  ;;          ;        ;  ;;  ;;;  ;;  ;;;;;;; ;;  ;;      ;;;;;;  
    ;  ;;          ;        ;  ;;   ;   ;;  ;;;;;;  ;;  ;;      ;;      
    ;  ;;          ;;      ;;  ;;       ;;  ;;      ;;  ;;      ;;      
    ;   ;;      ;  ;;;    ;;   ;;       ;;  ;;      ;;  ;;      ;;      
    ;    ;;;  ;;;   ;;;;;;;;   ;;       ;;  ;;      ;;  ;;;;;;; ;;      
    ;     ;;;;;;      ;;;;     ;;       ;;  ;;      ;;  ;;;;;;; ;;;;;;; 
    
    
    (define/public (compile)
      (define p (new process% 
                     [context 'CHECK-SYNTAX]
                     [update (λ (msg)
                               (set! message (format "~a: ~a"
                                                     (send p get-context)
                                                     (->string msg)))
                               (update))]))
      (seq p
        ;; Get a session ID
        [(initial? 'ID-FETCH)
         (get-new-session-id)
         id]
        ;; Check
        [(string? 'DEBUG)
         (set! message (format "Session ID: ~a" id))
         (update)
         NO-CHANGE]
        
        ;; Load system configuration
        [(string? 'ERROR-LOADING-SYSTEM-CONFIGURATION)
         (set! config (new client-config%))
         NO-CHANGE]
        
        ;; Write out the firmware
        [(FIXME 'ERROR-WRITING-FIRMWARE)
         (write-firmware)
         NO-CHANGE]
        
        [(FIXME 'ERROR-UPLOADING-FIRMWARE)
         (upload-firmware)
         NO-CHANGE]
        
        ;; Compile the code
        
        ;; Update the messages based on compilation results
        
        ;; Check for a temporary directory
        
        ;; Upload the firwmare if first compilation
        ;; Write out the code
        ;; Upload the code
        ))
    
    ))