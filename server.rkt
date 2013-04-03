#lang racket
(require web-server/dispatch 
         web-server/http
         web-server/servlet-env
          web-server/servlet/servlet-structs
         net/base64)

(define (compile-handler req b64)
  (printf "CODE:~n~a~n" 
          (base64-decode 
           (string->bytes/utf-8 b64)))
  (response/xexpr
   #:code 200
   `(result ok)))
  

(define-values (dispatch blog-url)
  (dispatch-rules
   [("compile" (string-arg)) compile-handler]
   ))

(define (serve)
  (with-handlers ([exn:fail? 
                   (lambda (e) 'ServeFail)])
    (serve/servlet dispatch
                   #:launch-browser? #t
                   #:port 3000
                   #:server-root-path (current-directory)
                   #:extra-files-paths 
                   (list 
                    (build-path (current-directory) "ide"))
                   #:servlet-path "/"
                   #:servlet-regexp #rx""
                )))

(serve)