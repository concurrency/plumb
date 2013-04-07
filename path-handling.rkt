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
