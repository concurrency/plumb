#lang racket
(require web-server/dispatch 
         web-server/http
         web-server/servlet-env
         net/base64
         )

(require "compile.rkt"
         "path-handling.rkt"
         "util.rkt"
         "response-handling.rkt"
         json)

(define (guarded-compile-session req session-id main-file)
  (let ([resp (make-parameter (get-response 'ERROR-COMPILE-UNKNOWN))])

    ;; Handle error cases.
    (cond
      [(not (session-exists? session-id))
       (resp (get-response 'ERROR-BAD-ID))]
      [(not (occam-file? main-file))
       (resp (make-response 'ERROR-NOT-OCC-FILE))]
      [else
       (resp (compile-session req session-id main-file))]
      )
    
    ;; Passing back to the webserver
    (encode-response (resp))))


(define (compile-session req session-id main-file)
  (parameterize ([current-directory (session-dir session-id)])
    ;; Assume a successful build.
    (define resp (make-parameter (get-response 'OK-BUILD)))

    (let* ([namer (name-generator main-file)]
           [occ-name (namer 'occ)]
           [tce-name (namer 'tce)]
           [tbc-name (namer 'tbc)]
           [hex-name (namer 'hex)])
      
      ;; This needs to be improved.
      (printf "compile cmd: ~a~n" (compile-cmd occ-name))
      
      ;; Check and see that compilation worked.
      (when (success-response? (resp))
        (let ([result (handle-compilation session-id (compile-cmd occ-name))])
          (unless (equal? result "SUCCESS")
            (resp (get-response 'ERR-SYNTAX #:extra `((fromcompiler . ,result))))
            )))

      ;; If things compiled, then we should link.
      (when (success-response? (resp))
        (exe-in-session session-id (plinker-cmd tce-name tbc-name)))
      
      ;; If things linked, we should binhex.
      (when (success-response? (resp))
        (exe-in-session session-id (binhex-cmd tbc-name hex-name)))
      
      (when (success-response? (resp))
        (when (not (file-exists? tce-name))
          (error 'compile-handler "No TCE found: ~a" (current-seconds))))

      (when (success-response? (resp))
        (resp (extend-response 
               (resp) 
               `((hex . ,(file->string hex-name))))))
        )
      
      
      
      ;; Destroy everything!
      (cleanup-session session-id)
      ;; Return the b64 encoded JSON file
      (resp)
      ))

(define SESSIONS (make-hash))
;; This should be an order-maintained list, not a hash
(define (add-session id)
  (hash-set! SESSIONS id (current-seconds)))
(define (cleanup-session id)
  '...)
(define (session-exists? id)
  (when (hash-ref SESSIONS id (lambda () false))
    true))

;; start-session :: -> int
;; Returns a unique session ID used for adding files and compiling.
(define (start-session req)
  (let ([rs (format "jupiter-~a" (random-string 32))])
    (add-session rs)
    (make-session-dir rs)
    ;; Return the session ID.
    (response/xexpr 
     #:code 200
     rs)))



;; CONTRACT
;; b64 encoded request -> json
;; should always contain an 'action field
(define (process-request b64 action-type)
  (define result (make-parameter b64))
  
  ;; Transform the bytes to a string
  (set/catch result bytes?
    (get-response 'ERROR-BYTES-TO-STRING)
    (string->bytes/utf-8 b64))
  
  ;; Base64 decode
  (set/catch result string?
    (get-response 'ERROR-B64-DECODE)
    (format "~a" 
          (base64-decode 
           (string->bytes/utf-8 (result)))))
  
  ;; Read JSON
  (set/catch result string?
    (get-response 'ERROR-READ-JSON)
    (read-json (open-input-string (result))))
 
  ;; Check action
  (try/catch result hash?
    (get-response 'ERROR-WRONG-ACTION)
    (when (not (equal? (hash-ref (result) 'action) action-type))
      (error))
    )
  
  (result))
  
(define (add-file req b64)
  (define result (make-parameter (process-request b64 "add-file")))
  
  (try/catch result hash?
    (get-response 'ERROR-MISSING-KEY)
    (begin
      (printf "--~n~a~n--~n" (result))
      (hash-ref (result) 'code)
      (hash-ref (result) 'filename)
      (hash-ref (result) 'sessionid)))

  (when (success-response? (result))
    (let ([code (hash-ref (result) 'code)]
          [filename (hash-ref (result) 'filename)]
          [session-id (hash-ref (result) 'sessionid)])
      (add-session-file session-id filename code)
      (result (get-response 'OK-ADD-FILE))
      ))
  
  (printf "~a~n" (result))
  (encode-response (result)))

(define-values (dispatch blog-url)
  (dispatch-rules
   [("start-session") start-session]
   [("add-file" (string-arg)) add-file]
   [("compile" (string-arg) (string-arg)) guarded-compile-session]
   ))

(define (serve)
  (with-handlers ([exn:fail? 
                   (lambda (e) 'ServeFail)])
    (serve/servlet dispatch
                   #:launch-browser? false
                   #:port 9000
                   #:listen-ip #f ;"192.168.254.201" ; remote.org
                   #:server-root-path (current-directory)
                   #:extra-files-paths 
                   (list 
                    (build-path (current-directory) "ide"))
                   #:servlet-path "/"
                   #:servlet-regexp #rx""
                   )))

(serve)