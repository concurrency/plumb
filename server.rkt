#lang racket
(require web-server/dispatch 
         web-server/http
         web-server/servlet-env
         net/base64
         json
         )

(require "compile.rkt"
         "path-handling.rkt"
         "util.rkt"
         "response-handling.rkt"
         "session-management.rkt")






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

(define (generate-names main-file)
  (define names (make-hash))
  (hash-set! names 'namer (name-generator main-file))
  (hash-set! names 'main main-file)
  (for ([ext '(occ tce tbc hex)])
    (hash-set! names ext ((hash-ref names 'namer) ext)))
  names)


(define (guarded-compile-session req session-id main-file)
  (define resp
    (make-parameter (get-response 'ERROR-COMPILE-UNKNOWN)))
    
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
  (encode-response (resp)))

(define (compile-session req session-id main-file)
  (parameterize ([current-directory (session-dir session-id)])
    ;; Assume a successful build.
    (define response (make-parameter (get-response 'OK-BUILD)))
    (define names (generate-names main-file))
    (cleanup-session session-id names)
    
    ;; This needs to be improved.
    ;; (printf "compile cmd: ~a~n" (compile-cmd names))
    
    ;; #:extra `((fromcompiler . ,result))
    
    (response (compile session-id (compile-cmd names)))
 
    ;; If things compiled, then we should link.
    (set/catch response success-response?
      (get-response 'ERROR-LINK)
      (plink session-id names))
    
    ;; If things linked, we should binhex.
    (set/catch response success-response?
      (get-response 'ERROR-BINHEX)
      (binhex session-id names))
    
    (set/catch response success-response?
      (get-response 'ERROR-READING-HEX)
      (extend-response 
       (response) 
       `((hex . ,(file->string (hash-ref names 'hex))))))
    
    ;; Destroy everything!
    (cleanup-session session-id names)
 
    ;; Return the b64 encoded JSON file
    (response)
    ))

(define (add-file req b64)
  (define result (make-parameter (process-request b64 "add-file")))
  
  (try/catch result hash?
    (get-response 'ERROR-MISSING-KEY)
    (begin
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


;; start-session :: -> int
;; Returns a unique session ID used for adding files and compiling.
(define (start-session req)
  (define rs (format "jupiter-~a" (random-string 32)))
  (add-session rs)
  (make-session-dir rs)
  ;; Return the session ID.
  (encode-response 
   (get-response 'OK-SESSION-ID #:extra `((sessionid . ,rs))))
  )

(define-values (dispatch blog-url)
  (dispatch-rules
   [("start-session") start-session]
   [("add-file" (string-arg)) add-file]
   [("compile" (string-arg) (string-arg)) guarded-compile-session]
   ))

(define (init)
  (unless (directory-exists? TEMPDIR)
    (make-directory TEMPDIR))
  (init-db))

(define (serve)
  (init)
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