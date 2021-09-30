#lang racket

(require "integration-tests.rkt")
(require "../src/compile.rkt")
(require "../src/parser.rkt")
(require profile)
(require profile/render-graphviz)
; (require profile/render-text)
; (require profile/analyzer)

#;(check-slog-behavior (file->string (get-path "../benchmarks/worstcase-4-terms-2-m.slog")) (hash))


#;(define _ (slog-compile (parse-slog-file "../benchmarks/worstcase-10-terms-3-m.slog")))
(profile-thunk
  (λ _ (void (slog-compile (parse-slog-file "../benchmarks/worstcase-10-terms-3-m.slog"))))
  #:delay 0.01
  #:use-errortrace? #t)
