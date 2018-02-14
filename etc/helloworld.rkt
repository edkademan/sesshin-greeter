#!/usr/racket/bin/racket

#lang racket

(require json srfi/54)

(define tmp-dir    (make-parameter (build-path "/tmp")))
(define input-dir  (make-parameter #f))
(define output-dir (make-parameter #f))

(define (process-command-line)
  (command-line
   #:once-each
   [("-i" "--input-dir") d "directory containing source tables"
    (input-dir (build-path d))]
   [("-o" "--output-dir") d "directory to contain pdf output"
    (output-dir (build-path d))]
   [("-t" "--tmp-dir") d
    ("directory to contain intermediate output"
     (format "(defaults to ~a)" (tmp-dir)))
    (tmp-dir (build-path d))]))

(define (df fmt . args) (display (apply format fmt args)))

(define (test-requires)
  (let ((j (call-with-input-string
               "{\"arr\" : [1, 2, 3, 4]}"
             read-json))
        (x "hello world"))
    (df "the input string is ~a~%and a formatted string is ~a~%"
        j (cat x 20))
))

(define (main)
  (process-command-line)
  (df "tmp is ~a~%in  is ~a~%out is ~a~%"
      (tmp-dir) (input-dir) (output-dir))
  (test-requires))

(module+ main (main))
