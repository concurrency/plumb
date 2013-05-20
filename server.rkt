#lang racket
(require web-server/dispatch 
         web-server/http
         web-server/servlet-env
         net/base64
         json
         )

(require "compile.rkt"
         "path-handling.rkt"
         "util.rkt"
         "response-handling.rkt"
         "session-management.rkt")


(define (generate-names main-file)
  (define names (make-hash))
  (hash-set! names 'namer (name-generator main-file))
  (hash-set! names 'main main-file)
  (for ([ext '(occ tce tbc hex)])
    (hash-set! names ext ((hash-ref names 'namer) ext)))
  names)


(define (guarded-compile-session req session-id main-file)
  (define resp
    (make-parameter (get-response 'OK)))
  
  ;; Check that we have a valid session.
  (try/catch resp success-response?
    (get-response 'ERROR-SESSION-ON-WALKABOUT)
    (unless (session-exists? session-id)
      (error (format "Session on walkabout: ~a" session-id))))
  
  ;; Make sure it exists
  (try/catch resp success-response?
    (get-response 'ERROR)
    (parameterize ([current-directory (session-dir session-id)])
      (unless (file-exists? (extract-filename main-file))
        (error (format "File does not exist: ~a" main-file)))))
  
  ;; Make sure it is an occam file.
  (try/catch resp success-response?
    (get-response 'ERROR-NOT-OCC-FILE)
    (unless (occam-file? (extract-filename main-file))
      (error (format "Not an occam file: ~a" main-file))))
  
  
  ;; Compile. The result will be a response.
  (set/catch resp success-response?
    (get-response 'ERROR-COMPILE-UNKNOWN)
    (compile-session req session-id main-file))
  
  ;; Passing back to the webserver
  (encode-response (resp)))

(define (compile-session req session-id main-file)
  (parameterize ([current-directory (session-dir session-id)])
    ;; Assume a successful build.
    (define response (make-parameter (get-response 'OK-BUILD)))
    (define names (generate-names main-file))
    
    (response (compile session-id (compile-cmd names)))
    
    ;; If things compiled, then we should link.
    (set/catch response success-response?
      (get-response 'ERROR-LINK)
      (plink session-id names))
    
    ;; If things linked, we should binhex.
    (set/catch response success-response?
      (get-response 'ERROR-BINHEX)
      (binhex session-id names))
    
    (set/catch response success-response?
      (get-response 'ERROR-READING-HEX)
      (extend-response 
       (response) 
       `((hex . ,(file->string (hash-ref names 'hex))))))
    
    ;; Destroy everything!
    (cleanup-session session-id)
    
    ;; Cleanup old sessions, too.
    ;; (It would be nice if this was automated.)
    (cleanup-old-sessions)
    
    ;; Return the b64 encoded JSON file
    (response)
    ))

(define (add-file req b64)
  (define result (make-parameter (process-request b64 "add-file")))
  
  ;; Make sure we have all the needed keys
  (try/catch result hash?
    (get-response 'ERROR-MISSING-KEY)
    (begin
      (hash-ref (result) 'code)
      (hash-ref (result) 'filename)
      (hash-ref (result) 'sessionid)))
  
  ;; Make sure the session exists, and it isn't a stale key
  (try/catch result hash?
    (get-response 'ERROR-SESSION-ON-WALKABOUT)
    (unless (session-exists? (hash-ref (result) 'sessionid))
      (error (format "Session ID unknown: ~a" (hash-ref (result) 'sessionid)))))
  
  (set/catch result success-response?
    (get-response 'ERROR-ADD-FILE)
    (let ([code (hash-ref (result) 'code)]
          [filename (hash-ref (result) 'filename)]
          [session-id (hash-ref (result) 'sessionid)])
      (add-session-file session-id filename code)
      (get-response 'OK-ADD-FILE)
      ))
  
  (encode-response (result)))


;; start-session :: -> int
;; Returns a unique session ID used for adding files and compiling.
(define (start-session req)
  (define rs (format "jupiter-~a" (random-string 32)))
  (add-session rs)
  (make-session-dir rs)
  ;; Return the session ID.
  (encode-response 
   (get-response 'OK-SESSION-ID #:extra `((sessionid . ,rs))))
  )

(define-values (dispatch blog-url)
  (dispatch-rules
   [("start-session") start-session]
   [("add-file" (string-arg)) add-file]
   [("compile" (string-arg) (string-arg)) guarded-compile-session]
   ))

(define (init)
  (unless (directory-exists? TEMPDIR)
    (make-directory TEMPDIR))
  (init-db))

(define (serve)
  (init)
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