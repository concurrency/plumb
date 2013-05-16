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

(define (compile-handler req b64)
  (let* ([code (decode-json b64)]
         [namer (name-generator b64)]
         [occ-name (namer 'occ)]
         [tce-name (namer 'tce)]
         [tbc-name (namer 'tbc)]
         [hex-name (namer 'hex)])
    (parameterize ([current-directory TEMPDIR])
      
      ;;(printf "SERVER: ~a~n" code)
      (printf "64: ~a~nCode: ~a~n" 
              (string-length b64) 
              (string-length code)) 
      
      (with-output-to-file occ-name
        (λ () (printf "~a" code)))
      
      (exe (compile-cmd occ-name))
      (exe (plinker-cmd tce-name tbc-name))
      (exe (binhex-cmd tbc-name hex-name))
      
      (when (not (file-exists? tce-name))
        (error 'compile-handler "No TCE found: ~a" (current-seconds)))
      
      (let* ([hex (file->string hex-name)]
             [h (make-hash)]
             [json (with-output-to-string
                    (λ () 
                      (hash-set! h 'hex hex)
                      (write-json h)))])
        (response/xexpr
         #:code 200
         `(b64 ,(format "~a" 
                        (base64-encode (string->bytes/utf-8 json))))))
      )))


(define (compile-session session-id main-file)
  (parameterize ([current-directory (session-dir session-id)]) 
    (let* ([namer (name-generator main-file)]
           [occ-name (namer 'occ)]
           [tce-name (namer 'tce)]
           [tbc-name (namer 'tbc)]
           [hex-name (namer 'hex)])
      
      ;; This needs to be improved.
      (exe (compile-cmd occ-name))
      (exe (plinker-cmd tce-name tbc-name))
      (exe (binhex-cmd tbc-name hex-name))
      
      (when (not (file-exists? tce-name))
        (error 'compile-handler "No TCE found: ~a" (current-seconds)))
      
      (let* ([hex (file->string hex-name)]
             [h (make-hash)]
             [json (with-output-to-string
                    (λ () 
                      (hash-set! h 'hex hex)
                      (write-json h)))]
             [xexpr
              `(b64 ,(format "~a" 
                             (base64-encode (string->bytes/utf-8 json))))])
        
        ;; Destroy everything!
        (cleanup-session session-id)
        
        (response/xexpr #:code 200 xexpr)
      ))))

(define SESSIONS (make-hash))
;; This should be an order-maintained list, not a hash
(define (add-session id)
  (hash-set! SESSIONS id (current-seconds)))
(define (cleanup-sessions)
  '...)

;; start-session :: -> int
;; Returns a unique session ID used for adding files and compiling.
(define (start-session req)
  (let ([rs (random-string 32)])
    (add-session rs)
    (make-session-dir rs)
    rs))

(define (add-file req b64)
  (let ([json (decode-json b64)])
    (let ([code (hash-ref json 'code)]
          [filename (hash-ref json 'filename)]
          [session-id (hash-ref json 'sessionid)])
      (add-session-file session-id filename code))))

(define-values (dispatch blog-url)
  (dispatch-rules
   [("start-session") start-session]
   [("add-file" (string-arg)) add-file]
   [("compile" (string-arg) (string-arg)) compile-session]
   ))

(define (serve)
  (with-handlers ([exn:fail? 
                   (lambda (e) 'ServeFail)])
    (serve/servlet dispatch
                   #:launch-browser? #t
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