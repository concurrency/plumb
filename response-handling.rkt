#lang racket

(require web-server/http
         net/base64
         json)
(require "util.rkt"
         "debug.rkt")

(provide (all-defined-out))


;; CONTRACT
;; SERVER SIDE
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
            (b64-decode (result))))
  
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

;; CLIENT SIDE
(define (process-response port)
  (define result (make-parameter port))
  
  ;; Transform the bytes to a string
  (set/catch result port?
    (get-response 'ERROR-BYTES-TO-STRING)
    (let ([str (read-all (result))])
      (debug 'PROCESS-RESPONSE "read-all:~n~a~n" str)
      str))
  
  ;; Base64 decode
  (set/catch result string?
    (get-response 'ERROR-B64-DECODE)
    (let ([b64 (format "~a" 
                       (b64-decode (result)))])
      (debug 'PROCESS-RESPONSE "b64:~n~a~n" b64)
      b64))
  
  ;; Read JSON
  (set/catch result string?
    (get-response 'ERROR-READ-JSON)
    (let ([json (read-json (open-input-string (result)))])
      (debug 'PROCESS-RESPONSE "json:~n~a~n" json)
      json))
  
  (result))

(define (encode-response json)
  (when (error-response? json)
    (printf "[~a] ~a~n~n~a~n"
            (hash-ref json 'code)
            (hash-ref json 'message)
            json))
  
  (response/xexpr #:code (if (success-response? json)
                             200 400)
                  (bytes->string/utf-8
                   (b64-encode
                    (jsexpr->string json)))))


  
(define RESPONSES (make-hash))
(define (add-response code h)
  (hash-set! RESPONSES code h))

(define (get-response code #:extra [h '()])
  (extend-response 
   (hash-ref RESPONSES code)
   (cond 
     [(list? h) (make-hash h)]
     [(hash? h) h]
     [else (error "Error in get-response.")])))

(define (base-response resp-code msg)
  (define json (make-hash))
  (hash-set! json 'code (format "~a" resp-code))
  (hash-set! json 'message msg)  
  json)

(define (extend-response h1 h2)
  
  (hash-for-each (if (not (hash? h2))
                     (make-hash h2)
                     h2)
                 (λ (k v)
                   (hash-set! h1 k v)))
  h1)

(define (make-response code msg #:extra [h `()])
  (define e (base-response code msg))
  (define responsetype (if (regexp-match "ERROR" (->string code)) "ERROR" "OK"))
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
(make-response 'ERROR-LINK "Error linking code.")
(make-response 'ERROR-BINHEX "Error binhexing code.")
(make-response 'ERROR-READING-HEX "Error reading binhex file.")

(make-response 'ERROR-NO-FILE "No file found for uploading.")
(make-response 'ERROR-CANNOT-READ "Cannot read file for upload.")
(make-response 'ERROR-JSON-ENCODE "Cannot encode data for JSON upload.")
(make-response 'ERROR-HTTP-GET "Error in HTTP GET.")
(make-response 'ERROR-PROCESS-RESPONSE "Error processing response from server.")
(make-response 'ERROR-SESSION-ON-WALKABOUT "Unkown session ID.")

(make-response 'ERROR-NO-CONNECTION "Cannot make connection to compile server.")
(make-response 'ERROR-PROCESS-RESPONSE "Cannot process response from server.")
(make-response 'ERROR-BAD-RESPONSE "Response decoded, but data bad.")

(make-response 'ERROR-READ-CONFIG "Cannot read/find config file.")
(make-response 'ERROR-CONVERT-CONFIG "Cannot convert config file.")

;; Default successes
(make-response 'OK "Everything's OK.")
(make-response 'OK-BUILD "Build successful.")
(make-response 'OK-ADD-FILE "File added.")
(make-response 'OK-SESSION-ID "Session ID generated.")
    
;; 
(define (error-response? r)
  (and (hash? r)
       (equal? "ERROR" (hash-ref r 'responsetype (λ () "SUCCESS")))))

(define (success-response? r)
  (not (error-response? r)))

(define-syntax-rule (try/catch param bool? alt body)
  (with-handlers ([exn? (λ (e)
                          ;(printf "~a~n" e)
                          (if (hash? alt)
                              (param (extend-response 
                                      alt
                                      `((message . ,(exn-message e)))))
                              (param alt)))])
    (when (bool? (param))
      body
      )))

(define-syntax-rule (set/catch param bool? alt body)
  (with-handlers ([exn? (λ (e)
                               ;(printf "~a~n" e)
                               (if (hash? alt)
                              (param (extend-response 
                                      alt
                                      `((message . ,(exn-message e)))))
                              (param alt)))])
    (when (bool? (param))
      (param body))))

(define-syntax-rule (try/success? param alt body)
  (try/catch param success-response? alt body))