#lang racket

(provide (all-defined-out))

(define BINPATH (build-path (current-directory) "bin" "macosx"))
(define LIBPATH (build-path (current-directory) "occam" "lib"))
(define TEMPDIR (build-path "/tmp"))

(define (bp cmd)
  (build-path BINPATH cmd))

(define COMPILE  (bp "occ21"))
(define LINKER   (bp "plinker.pl"))
(define BINHEX   (bp "binary-to-ihex"))

(define (occam-lib-path lib)
  (build-path LIBPATH (format "~a.lib" lib)))

(define (make-session-dir rs)
  (make-directory (session-dir rs)))

;; Make sure this is a good session ID
;; This could be the gatekeeper
(define (session-dir session-id)
  (build-path TEMPDIR session-id))

(define (add-session-file session-id filename code)
  (parameterize ([current-directory (session-dir session-id)])
    (let ([fp (open-output-file filename #:exists 'replace)])
      (display code fp)
      (newline fp)
      (close-output-port fp))))