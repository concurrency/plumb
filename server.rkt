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


(define (error-response msg)
  (response/xexpr #:code 400 msg))

(define (success-response msg)
  (response/xexpr #:code 200 msg))

(define (guarded-compile-session req session-id main-file)
  (let ([resp (make-parameter (response/xexpr #:code 301 "ERROR IN COMPILE-SESSION"))])
    ;; Handle error cases.
    (cond
      [(not (session-exists? session-id))
       (resp (error-response "Bad session id."))]
      [(not (occam-file? main-file))
       (resp (error-response "Not an occam file."))]
      [else
       (resp (compile-session req session-id main-file))]
      )
    
    (resp)))
                
  
(define (compile-session req session-id main-file)
  (parameterize ([current-directory (session-dir session-id)]) 
    
    (let* ([namer (name-generator main-file)]
           [occ-name (namer 'occ)]
           [tce-name (namer 'tce)]
           [tbc-name (namer 'tbc)]
           [hex-name (namer 'hex)])
      
      ;; This needs to be improved.
      (printf "compile cmd: ~a~n" (compile-cmd occ-name))
      (exe-in-session session-id (compile-cmd occ-name))
      (exe-in-session session-id (plinker-cmd tce-name tbc-name))
      (exe-in-session session-id (binhex-cmd tbc-name hex-name))
      
      (when (not (file-exists? tce-name))
        (error 'compile-handler "No TCE found: ~a" (current-seconds)))
      
      (let* ([hex (file->string hex-name)]
             [h (make-hash)]
             [json (with-output-to-string
                    (λ () 
                      (hash-set! h 'hex hex)
                      (write-json h)))]
             [xexpr
              (format "~a" 
                      (base64-encode (string->bytes/utf-8 json)))])
        
        ;; Destroy everything!
        (cleanup-session session-id)
        ;; Return the b64 encoded JSON file
        (response/xexpr #:code 200 xexpr)
      ))))

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