#lang racket
(require web-server/dispatch 
         web-server/http
         web-server/servlet-env
         net/base64)

(define-values (dispatch blog-url)
  (dispatch-rules
   [("compile" (string-arg)) 
    (λ (req s) (printf "CODE:~n~a~n" 
                       (base64-decode 
                        (string->bytes/utf-8 s)))
      (response/xexpr `(p ,s)))]
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