#lang racket
(require web-server/dispatch 
         web-server/http
         web-server/servlet-env
         web-server/servlet/servlet-structs
         net/base64
         math/base)

(require "compile.rkt"
         "path-handling.rkt"
         "util.rkt"
         json)

;; decode-json :: b64 -> hash
(define (decode-json b64)
  (let* ([b64-decoded 
          (base64-decode 
           (string->bytes/utf-8 b64))]
         [json
          (read-json (open-input-string 
                      (format "~a" b64-decoded)))])
    json))

(define (make-response http-code resp-code msg #:json [h (make-hash)])
  (define json (make-hash))
  (hash-set! json 'http http-code)
  (hash-set! json 'code (format "~a" resp-code))
  (hash-set! json 'message msg)
  
  (set! json (extend-response json h))

  json)

(define (extend-response h1 h2)
  (hash-for-each h2 (λ (k v)
                      (hash-set! h1 k v)))
  h1)

(define (encode-response json)
  (response/xexpr #:code (hash-ref json 'http)
                  (bytes->string/utf-8
                   (base64-encode
                    (string->bytes/utf-8
                     (jsexpr->string json))))))


(define (error-response err-code msg #:json [json (make-hash)])
  (make-response 400 err-code msg #:json json))

(define (success-response resp-code msg #:json [json (make-hash)])
  (make-response 200 resp-code msg #:json json))

(define SUCCESS-CODES (map ->string '(OK GOOD AWESOME)))

(define (success-response? resp)
  (and (hash? resp)
       (member (hash-ref resp 'code) SUCCESS-CODES)))

(define (guarded-compile-session req session-id main-file)
  (let ([resp 
         (make-parameter
          (error-response 'COMPILE-ERROR
                          "ERROR IN COMPILE-SESSION"))])
    ;; Handle error cases.
    (cond
      [(not (session-exists? session-id))
       (resp (error-response 'BAD-ID "Bad session id."))]
      [(not (occam-file? main-file))
       (resp (error-response 'NOT-OCC-FILE "Not an occam file."))]
      [else
       (resp (compile-session req session-id main-file))]
      )
    
    (encode-response (resp))))


(define (compile-session req session-id main-file)
  (parameterize ([current-directory (session-dir session-id)])
    ;; Assume a successful build.
    (define resp 
      (make-parameter
       (success-response 'OK "Build successful")))

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
            (resp 
             (error-response 'SYNTAX result)))))      

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
               (make-hash `((hex . ,(file->string hex-name))))))
        )
      
      
      
      ;; Destroy everything!
      (cleanup-session session-id)
      ;; Return the b64 encoded JSON file
      (resp)
      )))

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

(define (add-file req b64)
  (let ([json (decode-json b64)])
    (let ([code (b64-decode 
                 (hash-ref json 'code))]
          [filename (hash-ref json 'filename)]
          [session-id (hash-ref json 'sessionid)])
      
      (add-session-file session-id filename code)
      
      (response/xexpr 
       #:code 200 (format "OK ~a~n" session-id)
       ))))

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