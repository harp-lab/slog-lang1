#lang racket

;; Slog-lang Compile Daemon
;; Copyright (c) Thomas Gilray, Kristopher Micinski, Sidharth Kumar, see License.md

(require racket/cmdline)
(require racket/exn)
(require "src/utils.rkt")
(require "src/parser.rkt")
(require "src/compile.rkt")
(require "src/compile-souffle.rkt")
(require "src/lang-predicates.rkt")
(require "src/interpreter.rkt")
(require "src/slog-params.rkt")
(require "src/slog-debug.rkt")

(random-seed 0)

(define file-path
  (command-line
   #:program "Slog compile daemon"))

(define (time thnk)
  (define before (current-inexact-milliseconds))
  (define ret-val (thnk))
  (define elapsed-millis (- (current-inexact-milliseconds) before))
  (cons ret-val elapsed-millis))

;; Compiles a set of slog files 
(define (compile-hashes compiler-root slog-files num-processes output-cpp data-directory output-fact-directory)
  (match-define (cons program elapsed-millis) (time (lambda () (slog-compile (parse-slog-files slog-files)))))
  ;; Write the initial databse
  (define serialized-facts (materialize-facts program data-directory))
  (match-define (cons global-definitions cpp-file) (slog-compile-cpp program serialized-facts))
  (define builtins-cpp-file (file->string (build-path compiler-root "src/builtins.cpp")))
  (parameterize ([slog-bucket-count num-processes])
    (with-output-to-file output-cpp
      (lambda ()
        (define template
          (with-input-from-file (build-path compiler-root "src" "daemondriver-template.cpp")
            (lambda () (read-string 99999))))
        (display (format template builtins-cpp-file global-definitions cpp-file output-fact-directory)))
      #:exists 'replace))
  `(success ,elapsed-millis))

(define (loop)
  (writeln
   (match (read)
     [`(compile-hashes ,compiler-root (files ,source-files ...) ,num-processes ,output-cpp ,data-directory ,output-facts-directory)
      (with-handlers
        ([exn:fail?
          (lambda (e)
            `(failure ,(exn->string e)))])
        (compile-hashes compiler-root source-files num-processes output-cpp data-directory output-facts-directory))]
     [_ `(error "Could not parse command input.")]))
  (flush-output (current-output-port))
  (loop))

;; Allows Python to wait until the pipe is alive
(writeln '(ready))
(flush-output (current-output-port))

#;(compile-hashes "/home/kmicinski/projects/slog/daemon/../compiler" (files "/home/kmicinski/projects/slog/daemon/../data/sources/9a715596b57213c6828594482e6bfe06f35c8dfed2dd72f50285700f2bfd7216") 4 "/home/kmicinski/projects/slog/daemon/../data/sources/49c998b4fb4b82667a5f5a6414dbe9c4666fe02e5d75b6babec9775d22ab364c-compiled.cpp" "/home/kmicinski/projects/slog/daemon/../data/databases/49c998b4fb4b82667a5f5a6414dbe9c4666fe02e5d75b6babec9775d22ab364c" "/home/kmicinski/projects/slog/daemon/../data/databases/49c998b4fb4b82667a5f5a6414dbe9c4666fe02e5d75b6babec9775d22ab364c-output")


;; Continuously accept commands in the server process language
(loop)
