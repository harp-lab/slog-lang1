#lang racket

(require "../src/parser.rkt")
(require racket/runtime-path)
(define-runtime-path HERE ".")
(define (get-path path) (path->string (build-path HERE path)))

(require rackunit)

(define/provide-test-suite parsing-tests
  (test-case
    "fwd rule with bang"
    (parse-slog-file (get-path "fwd-rule-with-bang.slog"))
    #f))
