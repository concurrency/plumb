#lang racket
(require web-server/dispatch 
         web-server/http
         web-server/servlet-env
         net/base64
         json
          racket/date
         )

(require "compile.rkt"
         "config-server.rkt"
         ;;"path-handling.rkt"
         "util.rkt"
         "response-handling.rkt"
         "session-management.rkt"
         "debug.rkt")

(define config (make-parameter false))

(define (generate-names main-file)
  (define names (make-hash))
  (hash-set! names 'namer (name-generator main-file))
  (hash-set! names 'main main-file)
  (for ([ext '(occ tce tbc hex)])
    (hash-set! names ext ((hash-ref names 'namer) ext)))
  names)


(define (guarded-compile-session req session-id board main-file)
  (define resp
    (make-parameter (get-response 'OK)))
  
  ;; Check that we have a valid session.
  (try/catch resp success-response?
    (get-response 'ERROR-SESSION-ON-WALKABOUT)
    (unless (session-exists? session-id)
      (error (format "Session on walkabout: ~a" session-id))))
  
  ;; Make sure it exists
  (try/catch resp success-response?
    (get-response 'ERROR)
    (parameterize ([current-directory (session-dir config session-id)])
      (debug 'GUARDED-COMPILE "Working on: ~a" (extract-filename main-file))
      (unless (file-exists? (extract-filename main-file))
        (error (format "File does not exist: ~a" main-file)))))
  
  ;; Make sure it is an occam file.
  (try/catch resp success-response?
    (get-response 'ERROR-NOT-OCC-FILE)
    (unless (occam-file? (extract-filename main-file))
      (error (format "Not an occam file: ~a" main-file))))
  
  
  ;; Compile. The result will be a response.
  (set/catch resp success-response?
    (get-response 'ERROR-COMPILE-UNKNOWN)
    (compile-session req config session-id board main-file))
  
  ;; Passing back to the webserver
  (encode-response (resp)))

(define (compile-session req config session-id board main-file)
  (parameterize ([current-directory (session-dir config session-id)])
    ;; Assume a successful build.
    (define response (make-parameter (get-response 'OK-BUILD)))
    (define names (generate-names main-file))
    
    ;; The server should load this, so we can compile with it.
    (send (config) add-config 'BOARD (server-retrieve-board-config board))
    
    (response (compile config session-id (compile-cmd config names)))
    
    ;; If things compiled, then we should link.
    (set/catch response success-response?
      (get-response 'ERROR-LINK)
      (plink config session-id names))
    
    ;; If things linked, we should binhex.
    (set/catch response success-response?
      (get-response 'ERROR-BINHEX)
      (binhex config session-id names))
    
    (set/catch response success-response?
      (get-response 'ERROR-READING-HEX)
      (extend-response 
       (response) 
       `((hex . ,(file->string (hash-ref names 'hex))))))
    
    ;; Destroy everything!
    (cleanup-session config session-id)
    
    ;; Cleanup old sessions, too.
    ;; (It would be nice if this was automated.)
    (cleanup-old-sessions config)
    
    ;; Return the b64 encoded JSON file
    (response)
    ))

(define (add-file req b64)
  (define result (make-parameter (process-request b64 "add-file")))
  (debug 'ADD-FILE "RAW REQ:~n~a~n" (result))
  
  ;; Make sure we have all the needed keys
  (try/catch result hash?
    (get-response 'ERROR-MISSING-KEY)
    (begin
      (hash-ref (result) 'code)
      (hash-ref (result) 'filename)
      (hash-ref (result) 'sessionid)))
  
  ;; Make sure the session exists, and it isn't a stale key
  (try/catch result hash?
    (get-response 'ERROR-SESSION-ON-WALKABOUT)
    (unless (session-exists? (hash-ref (result) 'sessionid))
      (error (format "Session ID unknown: ~a" (hash-ref (result) 'sessionid)))))
  
  (set/catch result success-response?
    (get-response 'ERROR-ADD-FILE)
    (let ([code (hash-ref (result) 'code)]
          [filename (hash-ref (result) 'filename)]
          [session-id (hash-ref (result) 'sessionid)])
      (add-session-file config session-id filename code)
      (debug 'ADD-FILE "Added: ~a~n" filename)
      (get-response 'OK-ADD-FILE)
      ))
  
  (encode-response (result)))

(define (log req metadata)
  (define d (current-date))
  (define (pad v) (if (< v 10) (format "0~a" v) v))
  (when (directory-exists? (send (config) get-config 'SERVER-LOG-DIR))
    (define path (build-path (send (config) get-config 'SERVER-LOG-DIR)
                             (format "~a~a~a-log.txt" 
                                     (date-year d)
                                     (pad (date-month d))
                                     (pad (date-day d)))))
    (with-output-to-file path #:exists 'append
      (λ ()
        (printf "~a,~a~n" 
                (request-client-ip req)
                (b64-decode metadata)))
      )))

(define (client-log req key metadata)
  (when (and (equal? key LOG-KEY)
             (< (string-length metadata) 1000))
    (log req metadata))
  (encode-response 
   (get-response 'OK)))

;; start-session :: -> int
;; Returns a unique session ID used for adding files and compiling.
(define (return-session-id req)
  (define session-id (format "jupiter-~a" (random-string 32)))
  (debug 'START-SESSION "session-id: ~a~n" session-id)
  
  (add-session session-id)
  (make-session-dir config session-id)
  ;; Return the session ID.
  (encode-response 
   (get-response 'OK-SESSION-ID #:extra `((sessionid . ,session-id))))
  )

(define (server-retrieve-board-config kind)
  (define response (make-parameter true))
  
  ;; Build a path to the config
  (set/catch response boolean?
    (get-response 'ERROR)
    (build-path (send (config) get-config 'CONFIG-BOARDS)
                (format "~a.conf" (extract-filename kind))))
  
  (debug 'BOARD-CONFIG "~a" (response))
  
  ;; Open the file
  (set/catch response path?
    (get-response 'ERROR-READ-CONFIG)
    (open-input-file (response)))
  
  (debug 'BOARD-CONFIG "~a" (response))
  
  ;; Read it; it should be a hash table.
  (set/catch response port?
    (get-response 'ERROR-CONVERT-CONFIG)
    (read (response)))
  
  (debug 'BOARD-CONFIG "~a" (response))
  (response)
  )

(define (retrieve-board-config req kind)
  (define response (make-parameter true))
  
  ;; Build a path to the config
  (set/catch response boolean?
    (get-response 'ERROR)
    (build-path (send (config) get-config 'CONFIG-BOARDS)
                (format "~a.conf" (extract-filename kind))))
  
  (debug 'BOARD-CONFIG "~a" (response))
  
  ;; Open the file
  (set/catch response path?
    (get-response 'ERROR-READ-CONFIG)
    (open-input-file (response)))
  
  (debug 'BOARD-CONFIG "~a" (response))
  
  ;; Read it; it should be a hash table.
  (set/catch response port?
    (get-response 'ERROR-CONVERT-CONFIG)
    (read (response)))
  
  (debug 'BOARD-CONFIG "~a" (response))
  
  
  ;; Encode it with an 'OK and send it back.
  (encode-response 
   (get-response 'OK #:extra (response)))
  )

(define (retrieve-firmware req firm)
  (define response (make-parameter true))
  
  ;; Build a path to the firmware
  (set/catch response boolean?
    (get-response 'ERROR)
    (build-path (send (config) get-config 'FIRMWARES) (extract-filename firm)))
  
  (debug 'FIRMWARE "FIRMWARE PATH: ~a" (response))
  
  ;; Open the file
  (set/catch response path?
    (get-response 'ERROR)
    (open-input-file (response)))
  
  (debug 'FIRMWARE "~a" (response))
  
  ;; Read it all; just a .hex file, so it is plain text.
  (set/catch response port?
    (get-response 'ERROR)
    (make-hash `((hex . ,(read-all (response))))))
  
  ;; Comes back as a hash -- if it goes well.
  (when (hash? (response))
    (debug 'FIRMWARE "~a" 
           (string-length 
            (hash-ref (response) 'hex (λ () "")))))
  
  ;; Encode it with an 'OK and send it back.
  (encode-response 
   (get-response 'OK #:extra (response)))
  )


(define-values (dispatch blog-url)
  (dispatch-rules
   [("log" (string-arg) (string-arg)) client-log]
   [("start-session") return-session-id]
   [("add-file" (string-arg)) add-file]
   [("compile" (string-arg) (string-arg) (string-arg)) guarded-compile-session]
   ;; Need guards and session ID checks on retrieve.
   [("board" (string-arg)) retrieve-board-config]
   [("firmware" (string-arg)) retrieve-firmware]
   ))

(define (init)
  (unless (directory-exists? (send (config) get-config 'TEMPDIR))
    (make-directory (send (config) get-config 'TEMPDIR)))
  (init-db config))

(define (serve)
  (init)
  (with-handlers ([exn:fail? 
                   (lambda (e) 'ServeFail)])
    (serve/servlet dispatch
                   #:launch-browser? false
                   #:port (send (config) get-config 'PORT)
                   #:listen-ip (send (config) get-config 'LISTEN-IP) ;"192.168.254.201" ; remote.org
                   #:server-root-path (current-directory)
                   #:extra-files-paths 
                   (list 
                    (build-path (current-directory) "static"))
                   #:servlet-path "/"
                   #:servlet-regexp #rx""
                   )))


(define P (make-parameter false))
(define server 
  (command-line
   #:program "server"
   #:multi
   [("-d" "--debug") flag
                     "Enable debug flag."
                     (enable-debug! (->sym flag))]
   
   #:once-each 
   [("--config") name
                 "Choose platform config."
                 (config (new server-config%))
                 ;; For time being
                 (enable-debug! 'ALL)
                 ]
   
   ;; Must come after --config on command line
   [("--port") port
               "Set the server port"
               (P (string->number port))]
   #:args () ;; No command-line args
   
   (set-textual-debug)
   (enable-debug! 'ALL)
   
   (when (not (config))
     (config (new server-config%)))
   (debug 'SERVER "PORT: ~a" (send (config) get-config 'PORT))
   (when (P)
     (debug 'SERVER "Setting port to ~a" (P))
     (send (config) add-config 'PORT (P)))
   (debug 'SERVER "PORT: ~a" (send (config) get-config 'PORT))
   
   (serve)
   ))
