;; The MIT License (MIT)
;; 
;; Copyright (c) 2013 Matthew C. Jadud
;; 
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

#lang racket
(require web-server/dispatch 
         web-server/http
         web-server/servlet-env
          web-server/servlet/servlet-structs
         net/base64
         json)

(require "code-text.rkt"
         "tabbed-texts.rkt"
         "model-plumb.rkt"
         "menu-examples.rkt"
         "debug.rkt"
         "mvc.rkt"
         "util.rkt"
         "version.rkt"
         "util-gui.rkt"
         )


(define hardware 'none)

(define (decode-json b64)
  (let* ([b64-decoded 
          (base64-decode 
           (string->bytes/utf-8 b64))]
         [json
          (read-json (open-input-string 
                      (format "~a" b64-decoded)))]
         [code (hash-ref json 'code)])
    code))

(define (program-handler req b64)
  (let* ([code (decode-json b64)])
    (parameterize ([current-directory "/tmp"])
      (printf "LOCAL: ~a~n" code)
      (response/xexpr
       #:code 200
       #:headers (list (make-header #"Access-Control-Allow-Origin" #"*"))
       `(ihex "ok"))
      )))
    
  
(define (list-serial-ports req)
  (set! hardware (new plumb%))
  (send hardware load-config)
  (send hardware enumerate-arduinos)
  (let ([ls (send hardware get-arduino-ports)])
    (printf "list-serial-ports: ~a~n" ls)
    (response/xexpr
     #:code 200
     #:headers (list (make-header #"Access-Control-Allow-Origin" #"*"))
      
     `(b64 ,(bytes->string/utf-8
                   (b64-encode
                    (jsexpr->string ls)))))))

(define-values (dispatch blog-url)
  (dispatch-rules
   [("program" (string-arg)) program-handler]
   [("list-serial-ports") list-serial-ports]
   ))

(define (serve)
  (with-handlers ([exn:fail? 
                   (lambda (e) 'ServeFail)])
    (serve/servlet dispatch
                   #:launch-browser? #f
                   #:port 11000
                   #:listen-ip #f ;"192.168.254.200" ; local.org
                   #:server-root-path (current-directory)
                   #:extra-files-paths 
                   (list 
                    (build-path (current-directory) "collabedit"))
                   #:servlet-path "/"
                   #:servlet-regexp #rx""
                )))

(serve)