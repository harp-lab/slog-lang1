#lang racket

(require "../src/parser.rkt"
         "../src/interpreter.rkt"
         "../src/compile.rkt"
         "../src/slog-debug.rkt")
(require racket/runtime-path)
(require rackunit)

(provide 
  check-slog-behavior
  slog-behavior-test-case
  get-path)

(define-runtime-path HERE ".")
(define (get-path path) (build-path HERE path))


;; slog-code : some slog code
;; query-expected-res-hash: hash of queries and their expected results
(define (check-slog-behavior slog-code query-expected-res-hash #:file-name [file-name "unit test"])
  (define slog-src-tree (parse-slog-module slog-code file-name (hash)))
  (define slog-ir (slog-compile slog-src-tree))
  (define ir-interp (slog-interp slog-ir))
  (for ([query (hash-keys query-expected-res-hash)])
    (define expected (hash-ref query-expected-res-hash query))
    (define query-res (query-ir-interp ir-interp query))
    (define error-msg (format "query: ~a" query))
    (check-equal? (list->set query-res) expected error-msg))
  ir-interp)

(define (slog-behavior-test-case name slog-code-input . query-expected-res)
  (define query-expected-res-hash
    (match query-expected-res
      [`((file ,file-name))
       (define inp (open-input-file (get-path file-name)))
       (define (inp->queries inp hash)
         (match (cons (read inp) (read inp))
           [(cons (? eof-object?) (? eof-object?)) hash]
           [(cons q exp) (inp->queries inp (hash-set hash q (list->set exp)))]))
       (inp->queries inp (hash))]
      [_ (apply hash query-expected-res)]))
  (match-define (cons slog-code file-name) (match slog-code-input 
                      [(? string? str) (cons str "unit test")]
                      [`(file ,file-name) (cons (file->string (get-path file-name)) file-name)]))
  (test-case
   name
   (let ([ir-interp (check-slog-behavior slog-code query-expected-res-hash #:file-name file-name)])
    (printf "~a facts count: ~a\n" name (ir-interp-facts-count ir-interp)))))


(define (to-desugared-slog-list lst)
  (match lst
    [(cons h t) `($lst ,h ,(to-desugared-slog-list t))]
    ['() '($nil)]))

(define/provide-test-suite integration-tests
  (slog-behavior-test-case
   "if-then-else builtin"
   "(foo 1) (foo 0) [(foo x) (if-then-else x 100 -100 y) --> (bar x y)]"
   '(bar _ _) (set '(bar 1 100) '(bar 0 -100)))

  (slog-behavior-test-case
   "builtin with const arg"
   "(foo 1) (foo 2) [(foo x) (< x 2) --> (bar x)]"
   '(bar _) (set '(bar 1)))
  
  (slog-behavior-test-case
   "builtin with only const args"
   "[(< 1 2) --> (bar 1)] [(< 2 2) --> (bar 10)]"
   '(bar _) (set '(bar 1)))

  (slog-behavior-test-case
   "range builtin"
   "[(range 0 3 x) --> (foo x)]"
   '(foo _) (set '(foo 0) '(foo 1) '(foo 2)))
  
  #;(slog-behavior-test-case
   "BUG: unused relation"
   "(squat 42) [(foo x) --> (bar x)]")

  (slog-behavior-test-case
   "partitioning rule with builtin"
   "(foo1 1 2 3 4)
    (foo2 4 5 6)
    (foo3 6 7 8)
    (foo4 8 9 10)
    (foo5 9 10 11 12)
    (foo6 12 13 14)
    (foo7 14 15 16)
    (foo8 16 17 18)

    [(foo1 a1 a2 a3 a4)
     (foo2 a4 a5 a6)
     (foo3 a6 a7 a8)
     (foo4 a8 a9 a10)
     (foo5 a9 a10 a11 a12)
     (foo6 a12 a13 a14)
     (foo7 a14 a15 a16)
     (foo8 a16 a17 a18)
     (+ a17 x y)
     (= x a3)
     -->
     (bar y)]"
     '(bar _) (set '(bar 20)))
     
  (slog-behavior-test-case
   "builtin with nested fact"
   "(s-and (s-or (s-atom 0) (s-or (s-atom 1) (s-atom 2))) (s-atom 3))
    (s-atom-w ?(s-atom x)) ;TODO Junk to workaround a bug
    (s-or-w ?(s-or x y)) ;TODO Junk to workaround a bug
    (s-and-w ?(s-and x y)) ;TODO Junk to workaround a bug
    [(= id (s-and _ _)) (depth id x) --> (foo id x)]"
   '(foo _ _) (set '(foo (s-and (s-or (s-atom 0) (s-or (s-atom 1) (s-atom 2))) (s-atom 3)) 4)))
   
   (slog-behavior-test-case
   "number?, string? builtins"
   "(foo 1) (foo 2)
    (foo \"hi\") (foo \"there\")
    [(foo x) (number? x) --> (numberfoo x)]
    [(foo x) (string? x) --> (stringfoo x)]"
   '(numberfoo _) (set '(numberfoo 1) '(numberfoo 2))
   '(stringfoo _) (set '(stringfoo "hi") '(stringfoo "there")))
   
   (slog-behavior-test-case
   "list-sum.slog"
   '(file "list-sum.slog")
   '(sum-output _ _) 
    (set '(sum-output (cons 4 (cons 8 (cons 2 (cons 1 (cons 6 (null)))))) 21)))
    
   (slog-behavior-test-case
   "builtins with extended indices"
   "(foo 1 2 3)
    (foo 2 3 5)
    (foo 1 2 4)
    (foo 1 10 3)
    (foo 10 15 20)
    [(foo x y z) (+ x y z) --> (foo-add x y z)]
    [(foo x y z) (add1 y z) --> (bar x y z)]
    [(foo x y z) (sub1 y x) --> (quax x y z)]
    [(foo x y z) (range x z y) --> (baz x y z)]"
   '(foo-add _ _ _) (set '(foo-add 1 2 3) '(foo-add 2 3 5) )
   '(bar _ _ _) (set '(bar 1 2 3))
   '(quax _ _ _) (set '(quax 1 2 3) '(quax 2 3 5) '(quax 1 2 4))
   '(baz x y z) (set '(baz 1 2 3) '(baz 2 3 5) '(baz 1 2 4) '(baz 10 15 20)))

  (slog-behavior-test-case
   "rule with two builtins as body"
   "[(+ 1 2 x) (< x 5) --> (foo x)]"
   '(foo _) (set '(foo 3)))

  (slog-behavior-test-case
   "resolved-bug-spurious-fact.slog"
   '(file "resolved-bug-spurious-fact.slog")
   '(file "resolved-bug-spurious-fact.slog.tests"))
   
   (slog-behavior-test-case
    "sort.slog"
    '(file "sort.slog")
    '(file "sort.slog.tests"))

   (slog-behavior-test-case
    "literals in rules"
    "(bar \"yello!\")
     [(bar s) (string? s) --> (bar-has-string s)]
     [(bar \"aloha!\") --> (bar-says-aloha)]
 
     (foo 1)
     [(foo x) (< x 10) (add1 x y) --> (foo y)]
     [(foo 10) --> (foo10) (foohas \"ten\")]
     [(foo 11) --> (foo11) (foohas \"eleven\")]"
    '(foo10) (set '(foo10))
    '(foo11) (set)
    '(foohas _) (set '(foohas "ten"))
    '(bar-says-aloha) (set)
    '(bar-has-string _) (set '(bar-has-string "yello!")))

   (slog-behavior-test-case
    "=/= builtin"
    "(data \"1\") (data \"2\") 
     [(diff a b) <-- (data a) (data b) (=/= a b)]"
    '(diff _ _) (set '(diff "1" "2") '(diff "2" "1")))

    (slog-behavior-test-case
    "builtins-interning.slog"
    '(file "builtins-interning.slog")
    '(file "builtins-interning.slog.tests"))

   #;(slog-behavior-test-case
    "syntax-sugars.slog"
    '(file "syntax-sugars.slog")
    '(file "syntax-sugars.slog.tests"))
    
   (slog-behavior-test-case
    "inconsistent equalities"
    "[(= x 1) (= y 2) (= x y) --> (foo x)]"
    '(foo _) (set))

   (slog-behavior-test-case
    "var unification and inner rules"
    "(foo 1) (bar 101) (bar 102)
     [[(foo x) (bar y) --> (foobar x y)]
      (= x y)
      -->
      (foobareq x)]"
    '(foobar _ _) (set '(foobar 1 101) '(foobar 1 102))
    '(foobareq _) (set))
    
   #;(slog-behavior-test-case
    "multiple splicing args in body list clause"
    "(perms ?(perms-input []) [])
     [(= inp (perms-input [a xs ...]))
     (perms !(perms-input xs) [xs1 ... xs2 ...])
     -->
     (perms inp [xs1 ... a xs2 ...])]
     
     [(perms !(perms-input [1 2 3 4]) res)
     -->
     (foo res)]"
    '(foo _) (list->set (map (λ (lst) `(foo ,(to-desugared-slog-list lst))) (permutations '(1 2 3 4)))))

   (slog-behavior-test-case
    "repeated-vars-in-clauses.slog"
    '(file "repeated-vars-in-clauses.slog")
    '(file "repeated-vars-in-clauses.slog.tests"))
    
    #;(slog-behavior-test-case
    "multi-splicing-lists.slog"
    '(file "multi-splicing-lists.slog")
    '(file "multi-splicing-lists.slog.tests"))
    
    (slog-behavior-test-case
    "comp-rels.slog"
    '(file "comp-rels.slog")
    '(file "comp-rels.slog.tests"))
    
    (slog-behavior-test-case
    "static-unification-inner-rules.slog"
    '(file "static-unification-inner-rules.slog")
    '(file "static-unification-inner-rules.slog.tests"))
    
    #;(slog-behavior-test-case
     "curly-clauses.slog"
     '(file "curly-clauses.slog")
     '(file "curly-clauses.slog.tests"))
     
    #;(slog-behavior-test-case
     "extra-list-syntax.slog"
     '(file "extra-list-syntax.slog")
     '(file "extra-list-syntax.slog.tests"))
    
    (slog-behavior-test-case
     "builtins-behavior.slog"
     '(file "builtins-behavior.slog")
     '(file "builtins-behavior.slog.tests"))
    
    (slog-behavior-test-case
     "aggregators.slog"
     '(file "aggregators.slog")
     '(file "aggregators.slog.tests"))
    
    (slog-behavior-test-case
     "consts-repeated-vars.slog"
     '(file "consts-repeated-vars.slog")
     '(file "consts-repeated-vars.slog.tests"))

    (slog-behavior-test-case
     "aggregators with extended indices"
     "(foo 1 2) (foo 1 3)
      (bar 1 2) (bar 1 3)
      [(bar x y)
       (maximum foo x y)
       -->
       (res x y)]"
     '(res _ _) (set '(res 1 3)))
    
    (slog-behavior-test-case
    "normalizer.slog"
    '(file "normalizer.slog")
    '(file "normalizer.slog.tests"))
    
    (slog-behavior-test-case
    "big-joins.slog"
    '(file "big-joins.slog")
    '(file "big-joins.slog.tests")))


;; tests that take longer
(define/provide-test-suite integration-tests-slow
  #;(slog-behavior-test-case
    "splicing-lists.slog"
    '(file "splicing-lists.slog")
    '(file "splicing-lists.slog.tests"))
    
  (slog-behavior-test-case
   "sudoku-4x4.slog"
   '(file "sudoku-4x4.slog")
   '(solution _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) 
   (set '(solution 2 1 4 3 
                   4 3 2 1 
                   3 2 1 4 
                   1 4 3 2) ))
  
  (slog-behavior-test-case
    "cesk-symbolic.slog"
    '(file "cesk-symbolic.slog")
    '(file "cesk-symbolic.slog.tests")))
