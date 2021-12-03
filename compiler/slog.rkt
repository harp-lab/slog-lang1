#lang racket

;; Slog-lang CLI
;; Copyright (c) Thomas Gilray, Kristopher Micinski, Sidharth Kumar, see License.md

(require racket/cmdline)
(require "src/utils.rkt")
(require "src/parser.rkt")
(require "src/compile.rkt")
(require "src/compile-souffle.rkt")
(require "src/lang-predicates.rkt")
(require "src/interpreter.rkt")
(require "src/slog-params.rkt")
(require "src/slog-debug.rkt")
(require "src/partitioning-pass.rkt")

(random-seed 0)

(define default-input-dir 'none)
(define input-database 'none)
(define output-database 'none)
(define output-code-loc 'none)

; Parse command-line
(define file-path
  (command-line
   #:program "Slog CLI"
   #:once-each
   [("-v" "--verbose") "Run with Verbose output."
    (slog-verbose-mode #t)]
   [("--dump") "Dump tuples for iteration of each SCC."
    (slog-dump-iters #t)]
   [("--verify") dir "Verify the execution of the interpreter against a saved execution via --dump"
    (slog-verify-mode #t)
    (slog-verify-directory dir)]
   [("-b" "--buckets") n "Set number of buckets"
    (slog-bucket-count (string->number n))]
   [("--pf" "--print-final-ir") "Print the final ir"
    (slog-print-ir #t)]
   [("--ps" "--print-ir-small") "Print ir-small"
    (slog-print-ir-small #t)]
   [("-p" "--print-ir-flat") "Print ir-flat"
    (slog-print-ir-fixed #t)]
   [("--debug-smt") "Print debug informations about smt calls"
    (slog-debug-smt #t)]
   [("-o") "Attempt to optimize the Slog IR"
    (slog-optimize #t)]
   [("--mb" "--merge-builtins") "merge multiple builtins in rule bodies into one synthetic computational relation"
    (slog-merge-builtins #t)]
   [("--mb-off" "--merge-builtins-off") "DO NOT merge multiple builtins in rule bodies into one synthetic computational relation"
    (slog-merge-builtins #f)]
   [("--input-db") input-db "Input database (directory, file name must follow relation file name rule)"
    (set! input-database input-db)]
   [("--build-input-db") input-db "New input database (directory, must be empty)"
    (set! default-input-dir input-db)]
   [("--output-db") output-db "Output database (facts / updated manifest written here)"
    (set! output-database output-db)]
   [("--printrels") rels "Print relation sizes (each iteration)"
    (print-relations (file->lines rels))]
   [("-f") "Be fast! (disable contract checking)"
           (check-cond-contracts #f)]
   [("--output-code") output-code "Output code (compiled slog file + CMake location)"
    (set! output-code-loc output-code)]
   #:once-any
   [("-c" "--compile") "Compile the program to C++"
    (slog-compile-mode #t)
    (slog-interpret-mode #f)]
   [("-s" "--souffle") "Compile the program to Souffle"
    (slog-souffle-mode #t)]
   [("-i" "--interpret") "Run via the interpreter"
    (slog-interpret-mode #t)
    (slog-compile-mode #f)]
   [("-d" "--debug") "Run via the interpreter with debugging"
    (slog-debug-mode #t)
    (slog-compile-mode #f)]
   [("--batch") "Run interpreter in batch mode and exit"
    (slog-batch-mode #t)
    (slog-compile-mode #f)]
   #:args (filename)
   filename))

(define before (current-inexact-milliseconds))
; Read all rules (forbid symbols appearing in source tree from being gensymb'ed)
(define source-tree (parse-slog-file file-path))
(define parse-time (- (current-inexact-milliseconds) before))

(when (> parse-time 5000) 
  (printf "parsing took ~a ms\n" (~r parse-time #:precision 0)))
(gensymb-forbid! (list->set (filter symbol? (flatten source-tree))))

; Compile program to the interpreted IR
(define program
  (slog-compile source-tree))

(define compilation-time (- (current-inexact-milliseconds) before))
(when (> compilation-time 0)
  (printf "parsing + compilation took ~a ms.\n" (~r compilation-time #:precision 0))) 

(when partitioning-used-randomness
  (printf "partitioning-pass used randomness!\n"))

(define (rule-type rule)
  (match rule
    [`(arule ,h ,b) 'arule]
    [`(srule ,h) 'fact]
    [`(srule ,h ,b) 'copy-rule]
    [`(srule ,h ,b1 ,b2) 'join-rule]))

(match-define `(ir-incremental ,ir-scc ,dag ,scc-map ,comp-rules-h) program)

; printing the final compiled IR
(when (slog-print-ir)
  (void (print-ir-incremental program)))

(when (slog-print-ir-fixed)
  (void (print-ir-fixed (ir->ir-fixed program))))

(match-define `(ir-scc ,ir-select ,scc-dag ,scc-h ,_comp-rules-h) ir-scc)
(match-define `(ir-select ,ir-small ,rel-h ,rules-h ,__comp-rules-h) ir-select)

(when (slog-print-ir-small)
  (void (print-ir-small ir-small)))

(define all-rules (flat-map (λ (scc) (hash-keys (fourth scc))) (hash-values scc-map)))
(define all-rule-types (map rule-type all-rules))
(printf "\nAll rules: ~a, arules: ~a, copy rules: ~a, join rules: ~a, facts: ~a\n"
    (length all-rule-types)
    (length (filter (app equal? 'arule _) all-rule-types))
    (length (filter (app equal? 'copy-rule _) all-rule-types))
    (length (filter (app equal? 'join-rule _) all-rule-types))
    (length (filter (app equal? 'fact _) all-rule-types)))

(when (> (hash-count comp-rules-h) 0)
  (printf "comp rules: ~a\n" (hash-count comp-rules-h)))

(printf "rels: ~a, sccs: ~a\n" (hash-count rel-h) (hash-count scc-h))

; location of this file so relative paths work better,
; as its probably best theyre relative to this file.
(define base-dir (path-only (path->complete-path (find-system-path 'run-file))))

; Run the REPL+Debugger or finish compiling
(define basename (first (string-split (last (string-split file-path "/")) ".")))
(when (equal? default-input-dir 'none)
  (set! default-input-dir (format "../data/~a-input" basename)))
(define o-dir (if (equal? output-database 'none) (format "../data/~a" basename) output-database))
(define code-loc (if (equal? output-code-loc 'none) o-dir output-code-loc))
(define extn (if (slog-souffle-mode) "dl" "cpp"))
(define o-path (format "~a/~a.~a" code-loc basename extn))
(define cmake-path (format "~a/CMakeLists.txt" code-loc))
(define parallel-ra-h-loc (path->string (build-path base-dir "../backend/src/parallel_RA_inc.h")))

(cond
  [(slog-souffle-mode)
   (let ([souffle (slog-compile-souffle source-tree)])
     (with-output-to-file o-path
       (lambda () (display souffle))
       #:exists 'replace))]
  [(slog-compile-mode)
   (let* ([i-dir (if (equal? input-database 'none)
                     (begin (create-initial-database program default-input-dir) default-input-dir) 
                     (begin (create-initial-database program input-database) input-database))]
          [builtins-cpp-file (file->string (path->string (build-path base-dir "src/builtins.cpp")))])
     (match-define (cons global-definitions cpp) (slog-compile-cpp program i-dir o-dir))
     (when (not (directory-exists? o-dir)) (make-directory o-dir))
     (with-output-to-file o-path
       (lambda ()
         (define template
           (with-input-from-file (path->string (build-path base-dir "src/driver-template.cpp"))
             (lambda () (read-string 99999))))
         (display (format template parallel-ra-h-loc builtins-cpp-file global-definitions i-dir o-dir cpp)))
       #:exists 'replace)
     (with-output-to-file cmake-path
       (lambda ()
         (define template
           (with-input-from-file (path->string (build-path base-dir "src/cmake-template"))
             (lambda () (read-string 99999))))
         (display (format template basename basename basename basename basename basename)))
       #:exists 'replace)
     (display (format "[wrote C++ driver and data to \"~a\"]\n" o-path)))]
  [else (slog-debug program)])
