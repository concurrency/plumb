#lang racket

(require racket/cmdline
         net/url
         net/base64
         json)

(require "interface.rkt")

(define VERSION "1.0.0")
(define verbose-mode (make-parameter false))
(define session-id   (make-parameter false))

(define HOST "127.0.0.1")
(define PORT 9000)

(define (make-server-url cmd #:param [param false])
         
  (string->url 
   (if param

       (if (list? param)
           (format "http://~a:~a/~a~a"
                   HOST 
                   PORT
                   cmd
                   (apply string-append (map (λ (p) (format "/~a" p)) param)))
           (format "http://~a:~a/~a/~a"
                   HOST
                   PORT
                   cmd
                   param
                   ))
       
       (format "http://~a:~a/~a" HOST PORT cmd)
       )))

(define (start-session)
  (let ([ip (get-pure-port (make-server-url 'start-session))])
    (let ([id (read-line ip)])
      (printf "~a~n" id)
      (session-id id)
      (close-input-port ip)
      )))

(define (json-encode h)
  (let ([os (open-output-string)])
    (write-json h os)
    (get-output-string os)))

(define (b64-encode str)
  (base64-encode (string->bytes/locale str)))

(define (b64-stream filename)
  (let ([os (open-output-string)])
    (base64-encode-stream 
     (open-input-file filename)
     os)
    (get-output-string os)))

(define (read-all port)
  (let ([content ""])
    (let ([ip port])
      (let loop ([line (read-line ip)])
        (unless (eof-object? line)
          (set! content (format "~a~a~n" content line))
          (loop (read-line ip))))
        (close-input-port ip)
        )
    content))

(define (add-file filename)
  (when (file-exists? filename)
    ;; Do I need to encode the file twice?
    (let ([file-contents (b64-stream filename)]
          [h (make-hash)])
      ;; Load the hash for shipment
      (hash-set! h 'filename filename)
      (hash-set! h 'code file-contents)
      (hash-set! h 'sessionid (session-id))
      (let ([resp (get-pure-port (make-server-url "add-file" 
                                                  #:param
                                                  (b64-encode (json-encode h))))])
        (printf "[~a] add-file response~n" (read-line resp))
        (close-input-port resp)
        ))))


(define (b64-decode str)
  (format "~a" 
          (base64-decode (string->bytes/locale str))))

(define (compile id main)
  (let ([url (make-server-url "compile"
                              #:param
                              (list id main))])
    (printf "~a~n" (url->string url))
    (let ([resp (get-pure-port url)]
          [content (make-parameter "")])
      (content (read-all resp))
      (let ([json (string->jsexpr  (b64-decode (content)))])
        (printf "~a~n" (hash-ref json 'hex)))
      
      (close-input-port resp))))
  
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
                        (add-file filename)]
   
   [("-c" "--compile") main
                       "Compile <main-file>."
                       (compile (session-id) main)]
   
   
   
   #:args filenames
   (for-each (λ (f)
               (add-file f))
             filenames)
   ))
