#lang racket

(require racket/cmdline
         net/url
         net/base64
         json)

(require "response-handling.rkt"
         "util.rkt")

(define VERSION "1.0.0")
(define verbose-mode (make-parameter false))
(define session-id   (make-parameter false))
(define timeout (make-parameter 15))

(define HOST "127.0.0.1")
(define PORT 9000)

(define make-server-url 
  (λ args
    (string->url
     (format "http://~a:~a~a"
             HOST
             PORT
             (apply string-append
                    (map (λ (p) (format "/~a" p)) args))))))

(define (start-session)
  (define resp-port (get-pure-port (make-server-url "start-session")))
  (define response 
    (make-parameter (process-response resp-port)))
  
  (session-id (hash-ref (response) 'sessionid)))

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
    
    (cond 
      [(error-response? (content))
       (printf "[~a] ~a~n" 
               (hash-ref (content) 'code)
               (hash-ref (content) 'message))]
      [else
       (printf "~a~n" (hash-ref (content) 'hex))])
    
    (close-input-port resp-port)))

(define (show-response res)
  (printf "[~a] ~a~n"
          (hash-ref res 'code)
          (hash-ref res 'message)))


(define (build dir main)
  (with-timeout
    (parameterize ([current-directory dir])
      ;; Get a new session ID
      (start-session)
      ;; Add all the relevant files
      (for ([f (directory-list)])
        (when (file-exists? f)
          (when (member (->sym (file-extension f)) '(occ inc module))
            (add-file f))))
      ;; Compile it
      (compile (session-id) main))))

(define-syntax-rule (thunk body ...)
  (λ () body ...))

(define-syntax-rule (while test body ...)
  (let loop ()
    (when test
      body ...
      (loop))))

(define-syntax-rule (with-timeout body ...)
  (let ([run-id (thread (thunk body ...))]
        [start-time (current-seconds)]
        [current-time (make-parameter (current-seconds))])
    
    ;; Run until we timeout
    (while (> (timeout) (- (current-time) start-time))
      (sleep 1)
      (current-time (current-seconds)))
    
    (when (thread-running? run-id)
      (kill-thread run-id)
      (printf "Timed out in ~a seconds.~n" (timeout)))))



(define plumb 
  (command-line
   #:program "plumb"
   #:once-each
   [("-v" "--version") "Display current plumb version and exit."
                       (printf "plumb version ~a~n" VERSION)
                       (exit)]
   
   [("--verbose") "Set maximum verbosity."
                  (verbose-mode true)]
   
   [("--start-session") "Start a session."
                        (start-session)
                        (printf "[~a] Session ID~n" (session-id))
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
   
   [("-t" "--timeout") t
                       "Set the timeout in seconds."
                       (timeout (string->number t))]
   
   [("--build") dir main
                "Compile project <dir>, using <main> as the start."
                (build dir main)]
   
   #:args filenames
   (for-each (λ (f)
               (show-response (add-file f)))
             filenames)
   ))
