#lang racket

(require racket/cmdline
         net/url
         net/base64
         net/dns
         json
         )

(require "response-handling.rkt"
         "path-handling.rkt"
         "util.rkt"
         "debug.rkt"
         "upload.rkt"
         )

(define VERSION "1.0.0")
(define verbose-mode (make-parameter false))
(define session-id   (make-parameter false))
(define timeout (make-parameter 15))

(define HOST (make-parameter "127.0.0.1"))
(define PORT (make-parameter 9000))

(define make-server-url 
  (λ args
    (string->url
     (format "http://~a:~a~a"
             (HOST)
             (PORT)
             (apply string-append
                    (map (λ (p) (format "/~a" p)) args))))))

(define (start-session)
  
  (define response 
    (make-parameter (get-response 'ERROR)))
  
  (debug 'START-SESSION "~a" (response))
  
  (set/catch response error-response?
    (get-response 'ERROR-NO-CONNECTION)
    (get-pure-port (make-server-url "start-session")))
  
  (debug 'START-SESSION "~a" (response))
  
  (set/catch response port?
    (get-response 'ERROR-PROCESS-RESPONSE)
    (process-response (response)))
  
  (debug 'START-SESSION "~a" (response))
  
  (set/catch response hash?
    (get-response 'ERROR-BAD-RESPONSE)
    (hash-ref (response) 'sessionid))
  
  (debug 'START-SESSION "~a" (response))
  
  (session-id (response))
  
  (response))

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
     (make-server-url "add-file" (result))))
  
  ;; Process the result
  (set/catch result port?
    (get-response 'ERROR-PROCESS-RESPONSE)
    (process-response (result)))
  
  (result))

(define (compile id main)
  (let* ([url (make-server-url "compile" id (extract-filename main))]
         [resp-port (get-pure-port url)]
         [content (make-parameter (process-response resp-port))])
    
    (close-input-port resp-port)
    
    (cond 
      [(error-response? (content))
       (content)]
      [else
       (hash-ref (content) 'hex)])
    ))

(define (show-response res)
  (printf "[~a] ~a~n"
          (hash-ref res 'code)
          (hash-ref res 'message)))


(define (build dir main)
  (parameterize ([current-directory dir])
    ;; Get a new session ID
    (start-session)
    ;; Add all the relevant files
    (for ([f (directory-list)])
      (when (file-exists? f)
        (when (member (->sym (file-extension f)) '(occ inc module))
          (add-file f))))
    ;; Compile it
    (compile (session-id) main)
    ))

(define-syntax-rule (thunk body ...)
  (λ () body ...))

(define-syntax-rule (while test body ...)
  (let loop ()
    (when test
      body ...
      (loop))))

(define-syntax-rule (with-timeout* time body ...)
  (let ([run-id (thread (thunk body ...))]
        [start-time (current-seconds)]
        [current-time (make-parameter (current-seconds))])
    
    ;; Run until we timeout
    (while (> time (- (current-time) start-time))
      (sleep 1)
      (current-time (current-seconds)))
    
    (when (thread-running? run-id)
      (kill-thread run-id)
      (printf "Timed out in ~a seconds.~n" time))))

(define-syntax-rule (with-timeout body ...)
  (with-timeout* (timeout) body ...))

(define (retrieve-board-config board)
  (let* ([url (make-server-url "board" board)]
         [resp-port (get-pure-port url)]
         [content (make-parameter (process-response resp-port))])
    
    (try/catch content hash?
      (get-response 'ERROR-NO-REMOTE)
      (debug 'BOARD-CONFIG "~a" (content))
      )
    
    ;; Store the config.
    (add-config (config) 'BOARD (content))
    
    (content)))

;tvm-avr-atmega328p-16000000-arduino.hex
(define (retrieve-board-firmware board)
  ;; First, get the board config
  (retrieve-board-config board)
  ;; Now, fetch the firmware
  (let* ([url (make-server-url "firmware" (hash-ref (get-config 'BOARD) 'firmware))]
         [resp-port (get-pure-port url)]
         [content (make-parameter (process-response resp-port))])
    
    (try/catch content hash?
      (get-response 'ERROR)
      (debug 'FIRMWARE "~a" (content))
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
   
   [("--start-session") "Start a session."
                        (start-session)
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
                       (compile (session-id) main)]
   
   [("-t" "--timeout") sec
                       "Set the timeout in seconds."
                       (timeout (string->number sec))]
   
   [("--build") dir main
                "Compile project <dir>, using <main> as the start."
                (build dir main)]
   
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
   ))


(define (options serial.port)
  (printf "Options ~a~n"
          (list-intersperse (append
                             (if (serial.port)
                                 '(f b)
                                 '())
                             '(h a d p)) ",")))
  
;; I need a non-stateless thing.
(define (plumb-repl)
  
  
  (define board.config (make-parameter false))
  (define serial.port (make-parameter false))
  (define code.hex (make-parameter false))
  (define firmware.hex (make-parameter false))
  (load-config (system-type))
  
  (HOST (dns-get-address 
         (dns-find-nameserver)
         (get-config 'SERVER-HOST)))
  
  (PORT (get-config 'SERVER-PORT))
  
  (define session-id (make-parameter (start-session)))
  
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
             [hex (build (->string dir) (->string main-file))]
             [full-config (retrieve-board-config board)]
             )
         (board.config full-config)
         (debug 'USER-CODE "Board Config: ~a~n" (board.config))
         (code.hex hex)
         (debug 'USER-CODE "~a" (code.hex))
         (avrdude-code (serial.port) (code.hex))
         )]
      
      [(f firmware)
       (let* ([board (read)]
              [full-config (retrieve-board-firmware (->string board))])
         (board.config full-config)
         (debug 'FIRMWARE "Board Config: ~a~n" (board.config))
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

(plumb-repl)