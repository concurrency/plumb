#lang racket
(require web-server/dispatch 
         web-server/http
         web-server/servlet-env
          web-server/servlet/servlet-structs
         net/base64)

(require "path-handling.rkt"
         )

(define (program-handler req b64)
  (let* ([code (base64-decode 
                (string->bytes/utf-8 b64))])
    (parameterize ([current-directory TEMPDIR])
      (printf "LOCAL: ~a~n" code)
      (response/xexpr
       #:code 200
       `(b64 ,code))
      )))
    
  

(define-values (dispatch blog-url)
  (dispatch-rules
   [("program" (string-arg)) program-handler]
   ))

(define (serve)
  (with-handlers ([exn:fail? 
                   (lambda (e) 'ServeFail)])
    (serve/servlet dispatch
                   #:launch-browser? #f
                   #:port 10000
                   #:listen-ip #f ;"192.168.254.200" ; local.org
                   #:server-root-path (current-directory)
                   #:extra-files-paths 
                   (list 
                    (build-path (current-directory) "ide"))
                   #:servlet-path "/"
                   #:servlet-regexp #rx""
                )))

(serve)