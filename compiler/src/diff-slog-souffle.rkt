;; A library and CLI to diff the output tables of a Slog program vs. a
;; Soufflé program Copyright (c) Kristopher Micinski et al., see
;; License.md.
;;
;; Notes:
;;
;; - Currently only supports numbers, other base datatypes are TBA
;;
;; - Assumes correct output from Slog and Soufflé (i.e., assumes that
;; arities of facts are correct)
#lang racket

(provide diff-slog-souffle)

(require racket/cmdline)
(require "lang-predicates.rkt")
(require "interpreter.rkt")
(require "utils.rkt")

;; Diff output directories from Slog and Soufflé. Returns a list of three values:
;;
;; - The keys in which Slog and Soufflé differ.
;; - For each relation from Soufflé's output, the set of tuples not included in Slog's output.
;; - For each relation from Slog's output, the set of tuples not included in Soufflé's output.
(define (diff-slog-souffle souffle-fact-dir slog-fact-dir)
  (define csv-files
    (filter (lambda (path) (string-suffix? (path->string path) ".csv")) (directory-list souffle-fact-dir)))

  (define souffle-facts
    (foldl (lambda (path h)
             (define-values (base filename _) (split-path path))
             (define relation-name (first (string-split (path->string filename) ".csv")))
             (hash-set h relation-name
                       (foldl (lambda (line s)
                                (set-add s (map string->number (string-split line)))) (set) (file->lines path))))
           (hash)
           csv-files))

  (match-define `(manifest (relations ,raw-relation-info) (intern-map ,im))
    (call-with-input-file (build-path slog-fact-dir "manifest")
      (lambda (in)
        (read in))))

  (define slog-relation-info
    (foldl (lambda (relation-info relinfo)
             (match relation-info [`((rel-arity ,name ,arity) ,rel-tag)
                                   (if (hash-has-key? relinfo name)
                                       (error "Error: this manifest has the relation ~a twice with different arities. This is not supported by the diffing tool yet.")
                                       (hash-set relinfo (symbol->string name) `(,(symbol->string name) ,arity ,rel-tag)))]))
           (hash)
           raw-relation-info))

  ;; calculate the directories within slog-fact-dir that hold slog facts
  ;; according to the manifest
  (define slog-fact-files
    (filter (lambda (path) (ormap (lambda (name) 
                                    (string-prefix? (path->string path) name))
                                  (hash-keys slog-relation-info)))
            (directory-list slog-fact-dir)))

  (define slog-facts
    (foldl (lambda (fpath acc)
             (define fname (path->string fpath))
             (define relation-name
               (car (filter (lambda (name) 
                              (string-prefix? fname name))
                            (hash-keys slog-relation-info))))
             (define (reorder tuple order)
               (define lookup-list (append order (remove* order (range (length tuple)))))
               (reverse (foldl (λ (i acc) (cons (list-ref tuple (index-of lookup-list i)) acc)) '() (range (length tuple)))))
             ;; Don't repeat facts
             (if (hash-has-key? acc relation-name)
                 acc
                 (let* ([column-order (map string->number (rest (string-split (car (string-split fname relation-name)) "_")))]
                        [lines (file->lines (build-path slog-fact-dir fname))]
                        [tuples (foldl (lambda (line tuples)
                                         (let* ([raw-tuple (map string->number (string-split line))]
                                                [arity (second (hash-ref slog-relation-info relation-name))])
                                           (set-add tuples (reorder raw-tuple column-order))))
                                       (set)
                                       lines)])
                   (hash-set acc relation-name tuples))))
           (hash)
           slog-fact-files))

  (define souffle-minus-slog
    (foldl (lambda (souffle-relation-name acc)
             (unless (hash-has-key? slog-facts souffle-relation-name) (error (format "Souffle's output contains ~a but slog does not." souffle-relation-name)))
             (define corresponding-slog-tuples (hash-ref slog-facts souffle-relation-name))
             (define corresponding-slog-facts (for/set ([tuple corresponding-slog-tuples]) (rest tuple)))
             (hash-set acc souffle-relation-name (set-subtract (hash-ref souffle-facts souffle-relation-name)
                                                               corresponding-slog-facts)))
           (hash)
           (hash-keys souffle-facts)))

  (define slog-minus-souffle
    (foldl (lambda (souffle-relation-name acc)
             (define corresponding-slog-tuples (hash-ref slog-facts souffle-relation-name))
             (define corresponding-slog-facts (for/set ([tuple corresponding-slog-tuples]) (rest tuple)))
             (hash-set acc souffle-relation-name (set-subtract corresponding-slog-facts
                                                               (hash-ref souffle-facts souffle-relation-name))))
           (hash)
           (hash-keys souffle-facts))) ;; note: use Soufflé facts here to ignore intermediate relations

  (define nonempty-diffs (filter (lambda (key) (if (and (set-empty? (hash-ref souffle-minus-slog key))
                                                        (set-empty? (hash-ref slog-minus-souffle key))) #f #t))
                                 ;; note: use souffle facts here to ignore intermediate relations
                                 (hash-keys souffle-minus-slog)))
  
  `(,nonempty-diffs ,souffle-minus-slog ,slog-minus-souffle))

;;
;; CLI
;;

; Parse command-line options
(match-define `(,slog-fact-dir ,souffle-fact-dir)
  (command-line
   #:program "diff-souffle-slog: A tool to diff Soufflé vs. Slog facts"
   #:args (souffle-fact-dir slog-fact-dir)
   `(,souffle-fact-dir ,slog-fact-dir)))

(match-define `(,nonempty-diffs ,souffle-minus-slog ,slog-minus-souffle) (diff-slog-souffle souffle-fact-dir slog-fact-dir))

(if (empty? nonempty-diffs)
      (displayln "No diffs between Soufflé and Slog.")
      (for ([diff-key nonempty-diffs])
        (unless (set-empty? (hash-ref souffle-minus-slog diff-key))
          (displayln (format "Relation ~a contains the following facts in Soufflé, but not in σλoγ:" diff-key))
          (for ([fact (set->list (hash-ref souffle-minus-slog diff-key))])
            (displayln (string-join (map number->string fact) "\t"))))
        (unless (set-empty? (hash-ref slog-minus-souffle diff-key))
          (displayln (format "Relation ~a contains the following facts in σλoγ, but not in Soufflé:" diff-key))
          (for ([fact (set->list (hash-ref slog-minus-souffle diff-key))])
            (displayln (string-join (map number->string fact) "\t"))))))
