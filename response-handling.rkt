#lang racket

(require web-server/http
         net/base64
         json)
(require "util.rkt")

(provide (all-defined-out))


(define (encode-response json)
  (when (error-response? json)
    (printf "[~a] ~a~n~n~a~n"
            (hash-ref json 'code)
            (hash-ref json 'message)
            json))
  
  (response/xexpr #:code (if (success-response? json)
                             200 400)
                  (bytes->string/utf-8
                   (base64-encode
                    (string->bytes/utf-8
                     (jsexpr->string json))))))


  
(define RESPONSES (make-hash))
(define (add-response code h)
  (hash-set! RESPONSES code h))
(define (get-response code)
  (hash-ref RESPONSES code))

(define (base-response resp-code msg)
  (define json (make-hash))
  (hash-set! json 'code (format "~a" resp-code))
  (hash-set! json 'message msg)  
  json)

(define (extend-response h1 h2)
  (hash-for-each h2 (λ (k v)
                      (hash-set! h1 k v)))
  h1)

(define (make-response code msg #:extra [h `()])
  (define e (base-response code msg))
  (define responsetype (if (regexp-match "ERROR" (->string code)) "error" "ok"))
  (add-response code (extend-response e (make-hash (cons `(responsetype . ,responsetype)
                                                      h))))
  )

;; Default errors
(make-response 'ERROR "Something bad happened.")
(make-response 'ERROR-COMPILE-UNKNOWN
            "An unknown error occurred during compilation.")
(make-response 'ERROR-BAD-ID "Bad session ID.")
(make-response 'ERROR-NOT-OCC-FILE "Not an occam file.")
(make-response 'ERROR-SYNTAX "Syntax error.")
(make-response 'ERROR-ADD-FILE "Could not add file.")
(make-response 'ERROR-BYTES-TO-STRING "Could not convert bytestring.")
(make-response 'ERROR-B64-DECODE "Cannot decode Base64.")
(make-response 'ERROR-READ-JSON "Cannot read JSON.")
(make-response 'ERROR-WRONG-ACTION "Incorrect action for GET.")
(make-response 'ERROR-MISSING-KEY "Missing key in JSON.")

;; Default successes
(make-response 'OK "Everything's OK.")
(make-response 'OK-BUILD "Build successful.")
(make-response 'OK-ADD-FILE "File added.")
    
;; 
(define (error-response? r)
  (and (hash? r)
       (equal? "ERROR" (hash-ref r 'responsetype (λ () "SUCCESS")))))

(define (success-response? r)
  (not (error-response? r)))

(define-syntax-rule (try/catch param bool? alt body)
  (with-handlers ([exn? (λ (e)
                          (param alt))])
    body
    ))

(define-syntax-rule (set/catch param bool? alt body)
  (with-handlers ([exn? (λ (e)
                          (param alt))])
    (when (bool? (param))
      (param body))))

(define-syntax-rule (try/success? param alt body)
  (try/catch param success-response? alt body))