#lang racket
;; cmd: racket -l errortrace -t profiling.rkt
(require "integration-tests.rkt")
(require "../src/compile.rkt")
(require "../src/parser.rkt")
(require "../src/slog-params.rkt")
(require profile)
(require profile/render-graphviz)
; (require profile/render-text)
; (require profile/analyzer)

(check-cond-contracts #f)
#;(check-slog-behavior (file->string (get-path "../benchmarks/worstcase-4-terms-2-m.slog")) (hash))


#;(define _ (slog-compile (parse-slog-file "../benchmarks/worstcase-10-terms-3-m.slog")))
(profile-thunk
  (λ _ (void (slog-compile (parse-slog-file "../../benchmarks/dvh-25-3.slog"))))
  #:delay 0.001
  #:use-errortrace? #t
  #:order 'total)
