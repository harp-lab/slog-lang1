#lang racket

;; Slog's static and dynamic settings and parameters
;; Copyright (c) Thomas Gilray, et al, see License.md

(provide (all-defined-out))

(define slog-print-ir (make-parameter #f))
(define slog-print-ir-flat (make-parameter #f))
(define slog-version-major 0)
(define slog-version-minor 0)
(define slog-version-revision 0)
(define slog-copyright-authors "Thomas Gilray, Kristopher Micinski, and Sidharth Kumar")
(define slog-copyright-warning "All rights reserved.") ;; ? change to "Some rights reserved, see License.md."
(define slog-verbose-mode (make-parameter #f))
(define slog-interpret-mode (make-parameter #f))
(define slog-debug-mode (make-parameter #f))
(define slog-verify-mode (make-parameter #f))
(define slog-verify-directory (make-parameter (void)))
(define slog-dump-iters (make-parameter #f))
(define slog-compile-mode (make-parameter #t))
(define slog-souffle-mode (make-parameter #f))
(define slog-format-depth (make-parameter 25))
(define slog-grouping-cardinality (make-parameter 3))
(define slog-grouping-depth (make-parameter 10))
(define slog-err-nolines (make-parameter 2))
(define slog-output-directory (make-parameter #f))
(define slog-batch-mode (make-parameter #f))
(define slog-bucket-count (make-parameter 1024))
(define slog-subbucket-mod (make-parameter 256))
(define slog-optimize (make-parameter #f))
(define slog-debug-smt (make-parameter #f))
(define slog-merge-builtins (make-parameter #f))
(define check-cond-contracts (make-parameter #t))
(define print-relations (make-parameter 'none))
