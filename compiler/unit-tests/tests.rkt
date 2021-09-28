#lang racket
(require rackunit)
(require rackunit/text-ui)
(require racket/lazy-require)
(lazy-require [rackunit/gui (test/gui)])

(require "parsing-tests.rkt"
         "integration-tests.rkt"
         "builtins-tests.rkt"
         "../src/slog-params.rkt")

(define all-tests-list 
  (list
    parsing-tests
    builtins-tests
    integration-tests))

(define slow-tests-list
  (list
    integration-tests-slow))


(define test-gui (make-parameter #t))
(define run-slow-tests (make-parameter #f))

(command-line
   #:once-each
   [("--gui" "-g") "Show the test runner gui"
    (test-gui #t)]
   [("-c" "--no-gui" "--cli") "Run tests in the command line interface"
    (test-gui #f)]
   [("-s" "--run-slow-tests") "Run slow tests too"
    (run-slow-tests #t)]
   [("-o") "optimization pass"
    (slog-optimize #t)]
   [("--mb" "--merge-builtins") "merge multiple builtins in rule bodies into one synthetic computational relation"
    (slog-merge-builtins #t)])

(when (run-slow-tests)
  (set! all-tests-list (append all-tests-list slow-tests-list)))

(define all-tests (make-test-suite "all-tests" all-tests-list))

(cond 
  [(test-gui)  (test/gui all-tests #:wait? #t)]
  [else
    (define output (open-output-string)) 
    (define test-result (parameterize ([current-output-port output])
                          (run-tests all-tests 'verbose)))
    (cond 
      [(> test-result 0) 
        (displayln (get-output-string output) (current-error-port))
        (exit test-result)]
      [else 
        (displayln (get-output-string output))])])