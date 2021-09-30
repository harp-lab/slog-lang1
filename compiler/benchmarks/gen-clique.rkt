#lang racket

(require racket/cmdline)

(define num
  (string->number (command-line #:args (num) num)))

(define bijection
  (foldl (lambda (i a) (hash-set a i (random 0 (expt 2 31))))
         (hash)
         (range num)))

(for ([i (range num)])
  (for ([j (range num)])
    (displayln (format "~a\t~a" (hash-ref bijection i) (hash-ref bijection j)))))
