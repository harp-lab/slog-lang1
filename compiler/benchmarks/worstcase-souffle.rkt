#lang racket

(require racket/cmdline
         "term-generators.rkt")

;; the size of the term to generate
(define n
  (string->number
   (command-line
    #:program "worst-case binary CPS term generator (outputs Souffle code)"
    #:once-each
    #:args (size)
    size)))

(define prog (kcfa-tiny n))
(define h (gen-kcfa-tiny n))

(define formatted-program (pretty-format prog))
(define lines (string-split formatted-program "\n"))
(for ([line lines])
  (displayln (format "// ~a" line)))

(for ([key (hash-keys h)])
  (for ([tuple (hash-ref h key)])
    (displayln (format "~a(~a)." key (string-join (map number->string tuple) ", ")))))
;(pretty-print h)
