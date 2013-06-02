#lang racket

(require racket/cmdline
         net/url
         net/base64
         net/dns
         json
         racket/gui
         mrlib/path-dialog
         )

(require "response-handling.rkt"
         "path-handling.rkt"
         "util.rkt"
         "debug.rkt"
         "upload.rkt"
         "session-management.rkt"
         "app-type.rkt"
         "sequential-ops.rkt"
         )

(define VERSION "1.0.0")
(define verbose-mode (make-parameter false))
(define session-id   (make-parameter false))
(define timeout (make-parameter 15))
(define first-compilation? (make-parameter true))

(define HOST (make-parameter false))
(define PORT (make-parameter 9000))
(define arduino-ports (make-parameter (list)))

(define (json-encode h)
  (let ([os (open-output-string)])
    (write-json h os)
    (get-output-string os)))

(define (add-file file-path)
  (define result (make-parameter (get-response 'OK)))
  
  ;; Check the file exists
  (try/catch result success-response?
    (get-response 'ERROR-NO-FILE)
    (unless (file-exists? file-path) (error)))
  
  ;; Read the file
  (set/catch result success-response?
    (get-response 'ERROR-CANNOT-READ)
    (file->string file-path))
  
  (set/catch result string?
    (get-response 'ERROR)
    (make-hash `((filename . ,(extract-filename file-path))
                 (code . ,(result))
                 (sessionid . ,(session-id))
                 (action . "add-file"))))
  
  (debug 'ADD-FILE "~a~n" (result))
  
  ;; Encode the jsexpr
  (set/catch result hash?
    (get-response 'ERROR-JSON-ENCODE)
    (jsexpr->string (result)))
  
  ;; Base64 encode the JSON string
  (set/catch result string?
    (get-response 'ERROR-JSON-ENCODE)
    (base64-encode (string->bytes/utf-8 (result))))
  
  ;; Do an HTTP GET
  (set/catch result bytes?
    (get-response 'ERROR-HTTP-GET)
    (get-pure-port
     (make-server-url (HOST) (PORT) "add-file" (result))))
  
  ;; Process the result
  (set/catch result port?
    (get-response 'ERROR-PROCESS-RESPONSE)
    (process-response (result)))
  
  (result))

(define (compile-code id board main)
  (debug 'COMPILE "Compiling on HOST ~a PORT ~a~n" (HOST) (PORT))
  
  (let* ([url (make-server-url (HOST) (PORT) "compile" id board (extract-filename main))]
         [resp-port (get-pure-port url)]
         [content (make-parameter (process-response resp-port))])
    
    (close-input-port resp-port)
    
    (debug 'COMPILE "CONTENT RESPONSE~n*****~n~a~n*****~n" (filter-hash (content) 'hex))
    
    (cond 
      [(or (error-response? (content))
           (eof-object? (content)))
       (content)]
      [else
       (hash-ref (content) 'hex)])
    ))

(define (show-response res)
  (printf "[~a] ~a~n"
          (hash-ref res 'code)
          (hash-ref res 'message)))


(define (build board dir main)
  (parameterize ([current-directory dir])
    (define p (new process% [context 'BUILD-BOARD]))
    
    (seq p
      [(initial? 'ERROR-START-SESSION)
       (start-session HOST PORT)]
      [(string? 'ERROR-STORE-SESSION-ID)
       (session-id (send p get))
       NO-CHANGE]
      [(pass 'ERROR-LISTING-FILES)
       (filter (λ (f)
                 (member (->sym (file-extension f))
                         '(occ inc module)))
               (filter file-exists? (directory-list)))]
      [(list? 'ERROR-ADDING-FILES)
       (for ([f (send p get)])
         (add-file f))
       NO-CHANGE]
      [(list? 'ERROR-COMPILING-CODE)
       (compile-code (session-id) board main)])
    ))
   

(define-syntax-rule (while test body ...)
  (let loop ()
    (when test
      body ...
      (loop))))

(define (retrieve-board-config board)
  (define p (new process% [context 'RETRIEVE-BOARD-CONFIG]))
 
  (seq p
    [(initial? 'ERROR-GENERATING-URL)
     (make-server-url (HOST) (PORT) "board" board)]
    
    [(url? 'ERROR-CREATING-PORT)
     (get-pure-port (send p get))]
    
    [(port? 'ERROR-PARSING-RESPONSE)
     (process-response (send p get))]
    
    [(hash? 'ERROR-STORING-BOARD-CONFIG)
     (debug 'BOARD-CONFIG "~a" (filter-hash (send p get) 'hex))
     (add-config (config) 'BOARD (send p get))
     NO-CHANGE])
  
  (send p get)
  )

;tvm-avr-atmega328p-16000000-arduino.hex
(define (retrieve-board-firmware board)
  ;; First, get the board config
  (retrieve-board-config board)
  ;; Now, fetch the firmware
  (let* ([url (make-server-url (HOST) (PORT) "firmware" (hash-ref (get-config 'BOARD) 'firmware))]
         [resp-port (get-pure-port url)]
         [content (make-parameter (process-response resp-port))])
    
    (try/catch content hash?
      (get-response 'ERROR)
      (debug 'FIRMWARE "~a" (string-length (hash-ref (content) 'hex)))
      )
    (content)))

(define plumb 
  (command-line
   #:program "plumb"
   #:multi
   [("-d" "--debug") flag
                     "Enable debug flag."
                     (enable-debug! (->sym flag))]
   
   #:once-each
   [("-v" "--version") "Display current plumb version and exit."
                       (printf "plumb version ~a~n" VERSION)
                       (exit)]
   
   [("--verbose") "Set maximum verbosity."
                  (verbose-mode true)]
   
   [("--server") host
                 "Set the server address."
                 (HOST host)
                 ]
   
   [("--start-session") "Start a session."
                        (session-id (start-session HOST PORT))
                        (printf "~a~n" (session-id))
                        (exit)]
   
   [("-s" "--session-id") id 
                          "The session ID to operate under."
                          (session-id id)]
   
   [("-a" "--add-file") filename
                        "Add a single file."
                        (show-response (add-file filename))]
   
   [("-c" "--compile") main
                       "Compile <main-file>."
                       (compile-code (session-id) main)]
   
   [("-t" "--timeout") sec
                       "Set the timeout in seconds."
                       (timeout (string->number sec))]
   
   [("--build") dir board main
                "Compile project <dir>, for <board>, using <main> as the start."
                (build dir board main)]
   
   [("--board-config") board
                       "Fetch configuration data for a given board."
                       (retrieve-board-config board)]
   
   [("--get-firmware") board
                       "Retrieve firmware for board."
                       (retrieve-board-firmware board)]
   
   #:args filenames
   (for-each (λ (f)
               (show-response (add-file f)))
             filenames)
   
   ;;(plumb-repl)
   ))


(define (options serial.port)
  (printf "Options ~a~n"
          (list-intersperse (append
                             (if (serial.port)
                                 '(f b r)
                                 '())
                             '(h a d p)) ",")))

;; I need a non-stateless thing.
(define (plumb-repl)
  
  (define board.config (make-parameter false))
  (define serial.port (make-parameter false))
  (define code.hex (make-parameter false))
  (define firmware.hex (make-parameter false))
  (load-config (system-type))
  
  ;; If no host is specified, use localhost
  ;; Otherwise, pull from the command line
  (if (equal? (HOST) false)
      (HOST "127.0.0.1")
      (add-config (config) 'SERVER-HOST (HOST)))
  
  (HOST (dns-get-address 
         (dns-find-nameserver)
         (get-config 'SERVER-HOST)))
  
  (PORT (get-config 'SERVER-PORT))
  
  ;; Needed for firmware
  (unless (directory-exists? (get-config 'TEMPDIR))
    (make-directory (get-config 'TEMPDIR)))
  
  (define session-id (make-parameter (start-session HOST PORT)))
  
  ;; Check the file exists
  (try/catch session-id success-response?
    (get-response 'ERROR-NO-CONNECTION)
    (begin
      
      ;(printf "plumb repl session: ~a~n" (session-id))
      (options serial.port)
      (printf "> ")
      
      
      (let main-loop ([cmd (read)])
        ;; Input handler
        (case (->sym cmd)
          [(h help)
           (printf "HELP~n")]
          
          [(d debug)
           (let ([flag (read)])
             (printf "Enabling debug flag: ~a~n" flag)
             (enable-debug! (->sym flag)))]
          
          [(a add-file) 
           (printf "session-id: ~a~n" (session-id))
           (let ([filename (read)])
             (show-response (add-file (->string filename))))]
          
          [(b build) 
           (let* ([board (read)]
                  [dir (read)]
                  [main-file (read)]
                  ;; Need to pass the board type here -- fix the server
                  [hex (build (->string board) (->string dir) (->string main-file))]
                  [full-config (retrieve-board-config board)]
                  )
             (board.config full-config)
             (debug 'USER-CODE "Board Config: ~a~n" (filter-hash (board.config) 'hex))
             (code.hex hex)
             (debug 'USER-CODE "LENGTH: ~a" (string-length (code.hex)))
             (avrdude-code (serial.port) (code.hex))
             )]
          
          [(f firmware)
           (let* ([board (read)]
                  [full-config (retrieve-board-firmware (->string board))])
             (board.config full-config)
             (debug 'FIRMWARE "Board Config: ~a~n" (filter-hash (board.config) 'hex))
             (firmware.hex (hash-ref (board.config) 'hex))
             (avrdude-firmware (serial.port)))]
          
          [(p port)
           (newline)
           (for ([a (list-arduinos)]
                 [n (length (list-arduinos))])
             (printf "[~a] ~a~n" n (build-port a)))
           (newline)
           (printf "Select serial port~n[port] ")
           (let ([port (read)])
             (serial.port 
              (build-port
               (list-ref 
                (list-arduinos)
                (string->number (->string port))))))]
          
          [(q quit) (printf "Exiting...~n")
                    (sleep 1)
                    (exit)])
        
        (options serial.port)
        (printf "> ")
        
        (main-loop (read))
        
        )))
  )

(define main-file (make-parameter false))
(define (board-choice->board-type choice)
  (case choice
    [("Arduino Duemilanove") "arduino"]
    [else "arduino"]))




(define (do-compilation win)
  
  (define board.config (make-parameter false))
  (define serial.port (make-parameter 
                       (build-port
                        (list-ref 
                         (arduino-ports)
                         (send 
                          (hash-ref (win) 'serial-port)
                          get-selection)))))
  
  (define code.hex (make-parameter false))
  (define firmware.hex (make-parameter false))
  (load-config (system-type))
  
  (debug 'COMPILE "Serial Port: ~a" (serial.port))
  
  ;; If no host is specified, use localhost
  ;; Otherwise, pull from the command line
  (HOST (send (hash-ref (win) 'server) get-value))
  (add-config (config) 'SERVER-HOST (HOST))
  
  (HOST (dns-get-address 
         (dns-find-nameserver)
         (get-config 'SERVER-HOST)))
  
  (PORT (get-config 'SERVER-PORT))
  
  ;; If this is the first compilation, upload the firmware
  (when (first-compilation?)
    (debug 'FIRMWARE "Uploading firmware on first compilation.")
    (first-compilation? false)
    (let* ([board (board-choice->board-type
                   (hash-ref (win) 'board))]
           [full-config (retrieve-board-firmware (->string board))])
      (board.config full-config)
      (debug 'FIRMWARE "Board Config: ~a~n" (filter-hash (board.config) 'hex))
      (firmware.hex (hash-ref (board.config) 'hex))
      (avrdude-firmware (serial.port))
      ))
  
  ;; Needed for firmware
  (unless (directory-exists? (get-config 'TEMPDIR))
    (make-directory (get-config 'TEMPDIR)))
  
  (session-id (start-session HOST PORT))
  
  (debug 'COMPILE "Session ID: ~a~n" (session-id))
  (debug 'COMPILE "Board: ~a~nDir: ~a~nName: ~a~n"
         (board-choice->board-type
          (hash-ref (win) 'board))
         (extract-filedir (main-file))
         (extract-filename (main-file)))
  
  (let* ([board (board-choice->board-type
                 (hash-ref (win) 'board))]
         ;; Need to pass the board type here -- fix the server
         [hex (build board
                     (extract-filedir (main-file))
                     (extract-filename (main-file))
                     )]
         [full-config (retrieve-board-config board)]
         )
    
    (send (hash-ref ((hash-ref (win) 'compiler-response-window)) 'f) show true)
    
    (board.config full-config)
    (debug 'USER-CODE "Board Config: ~a~n" (filter-hash (board.config) 'hex))
    (code.hex (hash-ref (board.config) 'hex))
    (debug 'USER-CODE "LENGTH: ~a" (string-length (code.hex)))
    (avrdude-code (serial.port) (code.hex))
    ))


(define (compiler-response-window)
  (define win (make-parameter (make-hash)))
  (define f (new frame% 
                 [label "Messages"]
                 [width 300]
                 [height 400]))
  
  (define editor-canvas (new editor-canvas%
                             (parent f)
                             (label "Editor Canvas")))
  (define text (new text%))
  
  
  
  (send text insert "Response from server...")
  (send editor-canvas set-editor text)
  
  (let ([w (make-hash `((f . ,f)
                        (editor-canvas . ,editor-canvas)
                        (text . ,text)))])
    (win w)
    win)
  )



(define (main-frame)
  (define win (make-parameter (make-hash)))
  
  (define f (new frame%
                 [label "Plumb GUI"]
                 [width 400]
                 [height 200]
                 ))
  (define server (new text-field% 
                      [parent f]
                      [label "Server"]
                      [init-value "ec2-54-226-131-120.compute-1.amazonaws.com"]
                      [stretchable-width true]
                      ))
  
  (define serial-port (new choice%
                           [parent f]
                           [label "Arduino Port"]
                           [choices 
                            (let ()
                              (arduino-ports (map ->string (list-arduinos)))
                              (arduino-ports))]))
  (define board (new choice% 
                     [parent f]
                     [label "Board Type"]
                     [choices (list "Arduino Duemilanove")]))
  
  (define hortz (new horizontal-panel%
                     [parent f]))
  
  (define choose-file (new button%
                           [parent hortz]
                           [label "Choose Code"]
                           [stretchable-width true]
                           [callback (λ (b e)
                                       (let ([d (new path-dialog%
                                                     [label "occam code chooser"]
                                                     [message "Choose your main .occ file."]
                                                     [parent f]
                                                     [existing? true]
                                                     [filters (list (list "occam files" "*.occ"))]
                                                     [dir? false])])
                                         (main-file (send d run))))]
                           ))
  
  (define compile (new button%
                       [parent hortz]
                       [label "Compile"]
                       [stretchable-width true]
                       [callback (λ (b e)
                                   (do-compilation win))]
                       ))
  
  (win (make-hash `((frame . ,f)
                    (server . ,server)
                    (serial-port . ,serial-port)
                    (board . ,board)
                    (choose-file . ,choose-file)
                    (compile . ,compile)
                    (compiler-response-window . ,(compiler-response-window))
                    )))
  win)



(when (not GUI)
  (plumb-repl))


(when GUI
  (enable-debug! 'ALL)
  (define window (main-frame))
  (send (hash-ref (window) 'frame) show true))