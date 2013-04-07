#lang racket
(require web-server/dispatch 
         web-server/http
         web-server/servlet-env
          web-server/servlet/servlet-structs
         net/base64)

(require "compile.rkt"
         "path-handling.rkt"
         "util.rkt")

(define (compile-handler req b64)
  (let* ([code (base64-decode 
                (string->bytes/utf-8 b64))]
         [namer (name-generator b64)]
         [occ-name (namer 'occ)]
         [tce-name (namer 'tce)]
         [tbc-name (namer 'tbc)]
         [hex-name (namer 'hex)])
    (parameterize ([current-directory TEMPDIR])
      
      (printf "SERVER: ~a~n" code)
      
      (with-output-to-file occ-name
        (λ () (printf "~a" code)))
    
      (exe (compile-cmd occ-name))
      (exe (plinker-cmd tce-name tbc-name))
      (exe (binhex-cmd tbc-name hex-name))
      
      (when (not (file-exists? tce-name))
        (error 'compile-handler "No TCE found: ~a" (current-seconds)))
      
      (let ([hex (file->string hex-name)])
        (response/xexpr
         #:code 200
         `(b64 ,(format "~a" 
                        (base64-encode (string->bytes/utf-8 hex))))))
    )))
  

(define-values (dispatch blog-url)
  (dispatch-rules
   [("compile" (string-arg)) compile-handler]
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