#lang racket

;; Slog compilation pass -- partitioning larger rules into unary or binary ones
;; Copyright (c) Thomas Gilray, et al, see License.md

;; This pass breaks up large and complex rules into several smaller, unary or binary, rules
;; Every rule in ir-small is either one head implied by one body clause
;; or is one head clause implied by two body clauses

(provide partitioning-pass
         admissible-comp-rel-indices?
         clause-rel-args
         clause-rel-args-w/prov
         partitioning-used-randomness)

(require racket/hash
         racket/random)

(define partitioning-used-randomness #f)

(require "lang-predicates.rkt")
(require "utils.rkt")
(require "graphs.rkt")
(require "builtins.rkt")
(require "lang-utils.rkt")
(require "slog-params.rkt")
(require "remove-implicit-joins-pass.rkt")

;; Optimizes the flat IR by iterating unification of variables and clauses to a fixed point 
(define/contract-cond (partitioning-pass ir)
  (-> ir-fixed? ir-small?)
  (match-define `(ir-fixed ,ir-flat ,rules-h ,ir-flat-comp-rules-h) ir)
  ; (printf "partitioning pass input rules:\n~a\n" (intercalate "\n\n" (map ~v (map strip-prov (hash-keys rules-h)))))

  (define comp-rules-h 
    (hash-map-keys
      (λ (rule)
        (match-define `(crule ,head ,bodys ...) rule)
        `(crule ,(ir-fixed-head->ir-flat-head head) ,@bodys))
      ir-flat-comp-rules-h))
  (define comp-rules (hash-keys comp-rules-h))

  (for ([rule+prov (hash->list rules-h)])
    (match-define (cons rule rule-prov) rule+prov)
    (match-define `(rule-prov ir-flat ,fixed-rule ,module ,source-id) rule-prov)
    (match-define `(rule ,heads ,bodys) rule)
    (define ungrounded-vars (clause-list-ungrounded-vars (set->list bodys) comp-rules))
    (when (not (set-empty? ungrounded-vars))
      (pretty-rule-error ir rule-prov 
                         (format "ungrounded variable(s): ~a" (intercalate ", " (set->list ungrounded-vars)))
                         #:exit #t)))
  (match-define (cons rules-h+ synth-comp-rules-h)
    (if (slog-merge-builtins)
      (foldl (λ (rule+prov accu)
                (match-define (cons new-rules-h new-comp-rules-h) accu)
                (match-define (cons rule rule-prov) rule+prov)
                (match-define (cons rule+ comp-rules+) (consolidate-comp-clauses rule comp-rules))
                (cons (hash-set new-rules-h rule+ rule-prov) 
                      (hash-union new-comp-rules-h (make-immutable-hash (map (λ (r) (cons r rule-prov)) comp-rules+)))))
              (cons (hash) (hash))
              (hash->list rules-h))
      (cons rules-h (hash))))
  
  (define comp-rules-h+ (hash-union comp-rules-h synth-comp-rules-h))
  (define comp-rules+ (hash-keys comp-rules-h+))

  `(ir-small
    ,ir
    ,(foldl (lambda (rule h)
              (match (hash-ref rules-h+ rule)
                    [(and rule-prov `(rule-prov ir-flat ,fixed-rule ,module ,source-id))
                    (define rules-st (partition-rule rule rule-prov comp-rules+))
                    (for ([r rules-st])
                      (validate-ir-small-rule r rule-prov))
                    (foldl (lambda (r h)
                              (hash-set h r `(rule-prov ir-fixed ,rule ,module ,source-id)))
                            h
                            (set->list rules-st))]))
            (hash)
      (hash-keys rules-h+))
    ,comp-rules-h+))

(define (clause-args cl)
  (match-define (list id rel args) (ir-fixed-clause-rel-args cl))
  (list->set (cons id args)))

; computes a set of unique variables for the clause
(define (clause-vars cl)
  (set-filter var? (clause-args cl)))

; returns the clause id var, relation and list of args of a clause
(define clause-rel-args ir-fixed-clause-rel-args)
(define clause-rel-args-w/prov ir-fixed-clause-rel-args-w/prov)

(define (clause-rel-name cl)
 (match-define (list id rel args) (clause-rel-args cl))
 (second rel))

;; returns the set of all variables that appear in more than one clause  
(define (join-vars cls-lst)
  (foldl set-union 
         (set)
         (map (λ (cl) (set-intersect (clause-vars cl) (cl-lst-vars (remove cl cls-lst)))) cls-lst)))

; Computes the number of variables that are internal to cl-lst0 (w.r.t. cl-lst1)
(define (internal-vars-count cl-lst0 cl-lst1)
  (set-count
   (set-subtract (foldl set-union (set) (map clause-vars cl-lst0))
                 (foldl set-union (set) (map clause-vars cl-lst1)))))


(define/contract (compute-sequential-partition-affinity idp-cls-set dep-cls-set comp-rules)
  (set? set? list? . -> . any)
  (define idp-cl-lst (set->list idp-cls-set))
  (define dep-cl-lst (set->list dep-cls-set))
  (define grounded (clause-list-grounded idp-cl-lst comp-rules))
  (cond 
    [(not grounded) -inf.0]
    [(andmap comp/agg-clause? idp-cl-lst) -inf.0]
    [else 
      (+ (set-count (set-subtract (join-vars idp-cl-lst) (cl-lst-vars dep-cl-lst)))
         (set-count (apply set-intersect (map clause-vars idp-cl-lst))))]))

; computes the number of variables two clauses share in common + the vars that are internal
; This is a heuristic for clauses that should be joined earlier rather than later
(define (compute-clause-affinity cl0 cl1 other-cl-lst comp-rules)
  (compute-sequential-partition-affinity (set cl0 cl1) (list->set other-cl-lst) comp-rules))

(define (cl-lst-vars cl-list)
  (foldl set-union (set) (map clause-vars cl-list)))

(define (compute-partition-affinity part comp-rules)
  (match-define (cons inc exc) part)
  (define inc-lst (set->list inc))
  (define exc-lst (set->list exc))
  (define (grounded) (and (clause-list-grounded inc-lst comp-rules) (clause-list-grounded exc-lst comp-rules)))
  (cond 
    ;; not allowing cartesian products when partitioning rules
    [(< 0 (set-count (set-intersect (cl-lst-vars inc-lst) (cl-lst-vars exc-lst)))) -inf.0] 
    [(not (grounded)) -inf.0]
    [(or (andmap comp/agg-clause? inc-lst) (andmap comp/agg-clause? exc-lst)) -inf.0]
    [else
      ;; equal to |(inc-vars ∪ exc-vars) - (inc-vars ∩ exc-vars)|, 
      ;; which is effectively the same as - |(inc-vars ∩ exc-vars)|
      (+ (internal-vars-count inc-lst exc-lst)
          (internal-vars-count exc-lst inc-lst))]))

; Function for taking a set and returning a set of pairs of sets, for all ways of choosing n elements
(define (choose-n st n)
  (if (= n 0)
      (set (cons (set) st))
    (foldl set-union
           (set)
           (map (lambda (p) 
                  (match p
                         [(cons inc exc)
                         (foldl (lambda (ex st)
                                  (set-add st (cons (set-add inc ex) (set-remove exc ex))))
                                (set)
                                (set->list exc))]))
                (set->list (choose-n st (- n 1)))))))

;; computes C(n, k): the number of ways to pick k elements out of a set of size n.
(define (choose n k)
  (/ (foldl * 1 (range (add1 (- n k)) (add1 n)))
     (foldl * 1 (range 1 (add1 k)))))

;; given a vector and a `k`, returns the `i`th element of a list containing all ways to
;; pick k items (unordered) out of the vector. 
(define (choose-k-ith-element vec n i)
  (cond 
    [(= n 0) (list)]
    [else 
      (define choose-n-1-size (choose (sub1 (vector-length vec)) (sub1 n)))
      (cond
        [(< i choose-n-1-size) ;; first one included
        (cons (vector-ref vec 0) (choose-k-ith-element (vector-drop vec 1) (sub1 n) i))]
        [else ;; first one excluded
        (choose-k-ith-element (vector-drop vec 1) n (- i choose-n-1-size))])]))


;; breaks down a rule of the form (cl0 cl1 cl-lst... --> heads) into two rules: (cl0 cl1 --> GEN-CLAUSE) (GEN-CLAUSE cl-lst... --> heads)
(define/contract-cond (merge-two-clauses cl0 cl1 cl-lst heads rule-prov)
  (ir-fixed-clause? ir-fixed-clause? (set/c ir-fixed-clause?) (set/c ir-fixed-clause?) any/c
   . -> . (cons/c ir-fixed-rule? ir-fixed-rule?))
  (match-define `(prov ((prov = ,=pos) (prov ,x ,xpos) (prov ((prov ,rel ,relpos) ,vals ...) ,clspos)) ,pos) (set-first heads))
  (define inter-rel (gensymb (string->symbol (format "$rule~a-inter-body" (rule-prov->id rule-prov)))))
  (define external-vars
    (set->list (set-intersect (cl-lst-vars (list cl0 cl1))
                              (cl-lst-vars (append (set->list cl-lst) (set->list heads))))))
  (define inter-rel-clause
    `(prov ((prov (rel-arity ,inter-rel ,(length external-vars) db) ,pos)
                   ,@(map (lambda (x) `(prov ,x ,pos))
                                      external-vars))
                  ,pos))
  (define inter-rel-clause-with-ceremony
    `(prov ((prov = ,=pos)
            (prov ,(gensymb '$_) ,=pos)
            ,inter-rel-clause)
           ,pos))
  (define rule1
    `(rule ,(set inter-rel-clause-with-ceremony)
           ,(set cl0 cl1)))
  (define rule2
    `(rule ,heads
           ,(set-add cl-lst inter-rel-clause-with-ceremony)))
  (cons rule1 rule2))

;; if a db (non-builtin) cluase has constants in it, it cannot appear in a binary join body
(define (db-clause-has-constants cl)
  ; (printf "db-clause-has-constants input: ~a\n" (strip-prov cl))
  (match-define (list _ `(rel-arity ,rel ,arity ,kind) args) (clause-rel-args cl))
  (and (db-rel-kind? kind)
       (ormap lit? args)))

; partitions a rule into a set of unary or binary rules in the ir-small format
(define/contract-cond (partition-rule rule rule-prov comp-rules)
  (-> ir-fixed-rule? list? any/c (set/c ir-small-rule?))
  (assert (ir-fixed-rule? rule) (format "bad ir-fixed-rule: ~a" (strip-prov rule)))
  (define res (_partition-rule rule rule-prov comp-rules))
  (for ([r (set->list res)])
    (assert (ir-small-rule? r) (format "bad ir-small-rule: ~a" (strip-prov r))))
  res)

(define (_partition-rule rule rule-prov comp-rules)
  (match rule
    ;; TODO this is probably not required anymore, as the static unification pass takes care of it
   #;[`(rule ,heads ,bodys) ;; making sure binary joins don't have constants in them
   #:when (and (= 1 (set-count heads))
              (= 2 (set-count bodys))
              (ormap db-clause-has-constants (set->list bodys)))
    (define (clause->const-less-clause body-clause)
      ;(match-define (list id rel args) (clause-rel-args body-clause))
      (match-define `(prov ((prov = ,=pos) (prov ,id-arg ,id-arg-pos)
                          (prov ((prov (rel-arity ,rel ,rel-arity ,kind) ,rel-pos) (prov ,args ,arg-poss) ...)
                                ,cl-pos))
                        ,cl-pos*) body-clause)
      (define new-rel (gensymb (string->symbol (string-append "$no-const-" (symbol->string rel)))))
      (define new-rel-args0 (filter-map (λ (arg arg-pos) (if (lit? arg) #f `(prov ,arg ,arg-pos))) args arg-poss))
      (define new-rel-args (append new-rel-args0 (list `(prov ,id-arg ,id-arg-pos))))
      (define new-rel-arity (length new-rel-args))
      (define new-id-arg (gensymb '$_))
      (define const-less-clause `(prov ((prov = ,cl-pos*) (prov ,new-id-arg ,cl-pos*)
                          (prov ((prov (rel-arity ,new-rel ,new-rel-arity db) ,rel-pos) ,@new-rel-args)
                                ,cl-pos))
                        ,cl-pos*))
      (define new-rule `(rule ,(set const-less-clause) ,(set body-clause)))
      (cons new-rule const-less-clause))

    (match-define (list body1 body2) (set->list bodys))
    (match-define (cons new-rule-1 new-body1) (if (db-clause-has-constants body1) (clause->const-less-clause body1) (cons #f body1)))
    (match-define (cons new-rule-2 new-body2) (if (db-clause-has-constants body2) (clause->const-less-clause body2) (cons #f body2)))
    (define updated-rule `(rule ,heads ,(set new-body1 new-body2)))

    (set-union (partition-rule updated-rule rule-prov comp-rules) 
                (if new-rule-1 (partition-rule new-rule-1 rule-prov comp-rules) (set))
                (if new-rule-2 (partition-rule new-rule-2 rule-prov comp-rules) (set)))]
   ;; rules with purely comp clauses are given an extra dummy db clause
   [`(rule ,heads ,bodys)
   #:when (and (> (set-count bodys) 0) 
               (andmap comp/agg-clause? (set->list bodys)))
    (define pos (prov->pos (set-first bodys)))
    ;; TODO revert to (unit) once the backend is fixed
    ; (define unit-clause (give-clause-id `(prov ((prov (rel-arity $unit 0 db) ,pos)) ,pos)))
    (define unit-clause (give-clause-id `(prov ((prov (rel-arity $unit 1 db) ,pos) (prov ,(gensymb '$__dummy) ,pos)) ,pos)))
    (define unit-fact (give-clause-id `(prov ((prov (rel-arity $unit 1 db) ,pos) (prov 0 ,pos)) ,pos)))
    (set-union (partition-rule `(rule ,heads ,(set-add bodys unit-clause)) rule-prov comp-rules)
               (partition-rule `(rule ,(set unit-fact) ,(set)) rule-prov comp-rules))]
   [`(rule ,heads ,bodys)
   #:when (and (= 1 (set-count heads))
              (>= 2 (set-count bodys)))
    (match (set-first heads)
        [`(prov ((prov = ,=pos) (prov ,x ,xpos) (prov ((prov ,rel ,relpos) ,vals ...) ,clspos)) ,pos)
          (define res-rule
            `(srule (prov ((prov ,rel ,relpos) ,@vals) ,clspos)
                                ,@(set->list bodys))) 
          (set res-rule)])]
   [`(rule ,heads ,bodys)
   #:when (and (= 1 (set-count heads))
               (= 3 (set-count bodys)))
    (match-define `(prov ((prov = ,=pos) (prov ,x ,xpos) (prov ((prov ,rel ,relpos) ,vals ...) ,clspos)) ,pos) (set-first heads))
  
    (define inter-rel (gensymb (string->symbol (format "$rule~a-inter-body" (rule-prov->id rule-prov)))))
    ; Helper for emiting two srules, one of cl0 join cl1 and another for cl2 join inter-rel
    (define (emit-two-rules cl0 cl1 cl2)
      (define external-vars
        (set->list (set-intersect (cl-lst-vars (cons cl0 (cons cl1 '())))
                                  (cl-lst-vars (cons cl2 (set->list heads))))))
      (define rule0 `(rule ,(set `(prov ((prov = ,pos) (prov ,(gensymb '$_) ,pos)
                                          (prov ((prov (rel-arity ,inter-rel
                                                                  ,(length external-vars)
                                                                  db)
                                                      ,pos)
                                                ,@(map (lambda (x) `(prov ,x ,pos))
                                                        external-vars))
                                                ,pos))
                                        ,pos))
                      ,(set cl0 cl1)))
      (define rule1 
      `(rule ,(set `(prov ((prov = ,relpos)
                          (prov ,(gensymb '$_) ,relpos)
                          (prov ((prov ,rel ,relpos) ,@vals) ,clspos)) ,clspos))
              ,(set cl2
                  `(prov ((prov = ,pos)
                      (prov ,(gensymb '$_) ,pos)
                      (prov ((prov (rel-arity ,inter-rel ,(length external-vars) db) ,pos)
                              ,@(map (lambda (x) `(prov ,x ,pos))
                                    external-vars))
                            ,pos))
                      ,pos))))
      (list rule0 rule1))
    
      ; Decide which two clauses are joined first
      (define bodys-lst (set->list bodys))
      ; (assert (clause-list-grounded bodys comp-rules) (intercalate "\n" (map strip-prov (set->list bodys))))
      (define heads-lst (set->list heads))
      (define affin0 (compute-clause-affinity (first bodys-lst)
                                              (second bodys-lst)
                                              (cons (third bodys-lst) heads-lst)
                                              comp-rules))
      (define affin1 (compute-clause-affinity (first bodys-lst)
                                              (third bodys-lst)
                                              (cons (second bodys-lst) heads-lst)
                                              comp-rules))
      (define affin2 (compute-clause-affinity (second bodys-lst)
                                              (third bodys-lst)
                                              (cons (first bodys-lst) heads-lst)
                                              comp-rules))
      (define two-rules
        (cond [(and (>= affin0 affin1) (>= affin0 affin2))
                (emit-two-rules (first bodys-lst) (second bodys-lst) (third bodys-lst))]
              [(and (>= affin1 affin0) (>= affin1 affin2))
                (emit-two-rules (first bodys-lst) (third bodys-lst) (second bodys-lst))]
              [(= -inf.0 affin2)
                (error (format "ungrounded rule: ~a" (strip-prov rule)))]
              [else
                (emit-two-rules (second bodys-lst) (third bodys-lst) (first bodys-lst))]))
      (foldl set-union (set) (map (app partition-rule _ rule-prov comp-rules) two-rules))]
   [`(rule ,heads ,bodys)
   #:when (and (= 1 (set-count heads))
              (< 3 (set-count bodys)))
    ; (assert (clause-list-grounded bodys comp-rules) (intercalate "\n" (map strip-prov (set->list bodys))))
    (match-define `(prov ((prov = ,=pos) (prov ,x ,xpos) (prov ((prov ,rel ,relpos) ,vals ...) ,clspos)) ,pos) 
      (set-first heads))
    (define part-size-ideal (max 2 (min 4 (floor (/ (set-count bodys) 2)))))
    ; (define before-part (current-inexact-milliseconds))
    (define bodys-lst (set->list bodys))
    (define bodys-vec (list->vector bodys-lst))
    (define random-partiion #f)
    (match-define (list score part _)
      (foldl (λ (part-size score-part)
                (match-define (list score part keep-looking) score-part)
                (cond [(not keep-looking) score-part]
                 [else
                  ; (define before-most-parts (current-inexact-milliseconds))
                  (define parts-count (choose (set-count bodys) part-size))
                  (define most-parts (cond 
                    [(> parts-count 200)
                     (set! random-partiion #t)
                     (map (λ (ind) 
                            (define inc-set (list->set (choose-k-ith-element bodys-vec part-size ind)))
                            (define exc (filter (λ (cl) (not (set-member? inc-set cl))) bodys-lst))
                            (cons inc-set (list->set exc))) 
                         (random-sample parts-count 200 #:replacement? #f))]
                    [else (set->list (choose-n bodys part-size))]))
                  ; (define most-parts-took (- (current-inexact-milliseconds) before-most-parts))
                  (match-define (cons res-score res-part)
                    (if (empty? most-parts)
                        (cons -inf.0 'N/A)
                        (parallel-argmax2 (app compute-partition-affinity _ comp-rules) most-parts)))
                  (if (> res-score score) (list res-score res-part #t) (list score part #f))]))
             (list -inf.0 'N/A #t)
             (range 2 (add1 part-size-ideal))))
    ; (define part-took (- (current-inexact-milliseconds) before-part))
    ; (printf "part took for ~a bodys: ~a \n" (set-count bodys) (~r part-took #:precision 0))
    (cond
      [(= score -inf.0) ;; the rule cannot be partitioned into two rules and a merge
        (define parts (choose-n bodys 2))
        (match-define (cons good-score good-partition) 
          (parallel-argmax2 (λ (part) (compute-sequential-partition-affinity (car part) (cdr part) comp-rules)) 
                            (set->list parts)))
        (match-define (cons cl-chosen cl-rest) good-partition)
        (match-define (list cl0 cl1) (set->list cl-chosen))
        (match-define (cons new-rule1 new-rule2) (merge-two-clauses cl0 cl1 cl-rest heads rule-prov))
        (define srules0 (partition-rule new-rule1 rule-prov comp-rules))
        (define srules1 (partition-rule new-rule2 rule-prov comp-rules))
        (set-union srules0 srules1)]
      [else
        (when random-partiion (set! partitioning-used-randomness #t))
        (match-define (cons inc-part exc-part) part)
        (define inc-part-lst (set->list inc-part))
        (define exc-part-lst (set->list exc-part))
        (assert (and (>= (set-count inc-part) 2) (>= (set-count exc-part) 2))
                (format "bad partitioning. \ninc-part: ~a \nexc-part: ~a" inc-part exc-part))
        (assert (clause-list-grounded inc-part-lst comp-rules)
          (format "bad partitioning, clause list not grounded: \n~a" (pretty-format (strip-prov inc-part-lst))))
        (assert (clause-list-grounded exc-part-lst comp-rules)
          (format "bad partitioning, clause list not grounded: \n~a" (pretty-format (strip-prov exc-part-lst))))
        (define inter-rel0 (gensymb (string->symbol (format "$rule~a-inter-body" (rule-prov->id rule-prov)))))
        (define inter-rel1 (gensymb (string->symbol (format "$rule~a-inter-body" (rule-prov->id rule-prov)))))
        (define external-vars0
          (set->list (set-intersect (cl-lst-vars inc-part-lst)
                                    (cl-lst-vars (set->list (set-union exc-part heads))))))
        (define external-vars1
          (set->list (set-intersect (cl-lst-vars exc-part-lst)
                                    (cl-lst-vars (set->list (set-union inc-part heads))))))
        (define rules0
          (partition-rule `(rule ,(set `(prov ((prov = ,pos)
                                              (prov ,(gensymb '$_) ,pos)
                                              (prov ((prov (rel-arity ,inter-rel0
                                                                      ,(length external-vars0)
                                                                      db)
                                                            ,pos)
                                                      ,@(map (lambda (x) `(prov ,x ,pos))
                                                            external-vars0))
                                                    ,pos))
                                              ,pos))
                                ,inc-part)
                          rule-prov comp-rules))
        (define rules1
          (partition-rule `(rule ,(set `(prov ((prov = ,pos)
                                              (prov ,(gensymb '$_) ,pos)
                                              (prov ((prov (rel-arity ,inter-rel1
                                                                      ,(length external-vars1)
                                                                      db)
                                                            ,pos)
                                                      ,@(map (lambda (x) `(prov ,x ,pos))
                                                            external-vars1))
                                                    ,pos))
                                              ,pos))
                                ,exc-part)
                            rule-prov comp-rules))
        (define join-rule
          `(rule 
            ,(set `(prov ((prov = ,pos) (prov ,(gensymb '$_) ,pos) (prov ((prov ,rel ,relpos) ,@vals) ,pos)) ,pos))
            ,(set `(prov ((prov = ,pos)
                        (prov ,(gensymb '$_) ,pos)
                        (prov ((prov (rel-arity ,inter-rel0 ,(length external-vars0) db) ,pos)
                                ,@(map (lambda (x) `(prov ,x ,pos))
                                      external-vars0))
                              ,pos))
                        ,pos)
                  `(prov ((prov = ,pos)
                        (prov ,(gensymb '$_) ,pos)
                          (prov ((prov (rel-arity ,inter-rel1 ,(length external-vars1) db) ,pos)
                                ,@(map (lambda (x) `(prov ,x ,pos))
                                        external-vars1))
                                ,pos))
                        ,pos))))
    (set-union rules0 rules1 (partition-rule join-rule rule-prov comp-rules))])]
   [`(rule ,heads ,bodys)
   #:when (and (< 1 (set-count heads))
              (>= 1 (set-count bodys)))
    (define heads-lst (set->list heads))
    ;; If cl1 depends on cl2, there is an edge cl1 -> cl2
    (define dep-graph (get-dep-graph heads))
    #;(define dep-graph
      (foldl (λ (head-cl gr)
              (match-define (list id _rel xs) (clause-rel-args head-cl))
              (foldl (λ (head-cl+ gr)
                        (match-define (list id+ _rel+ xs+) (clause-rel-args head-cl+))
                        (if (member id+ xs)
                            (hash-set gr head-cl (set-add (hash-ref gr head-cl) head-cl+))
                            gr))
                     (hash-set gr head-cl (hash-ref gr head-cl set))
                     (set->list heads)))
            (hash)
            (set->list heads)))
    (define (get-deps cl) (hash-ref dep-graph cl))

    ; Check for cycles in the dep-graph and issue an error:
    #;(for ([head-cl (set->list heads)])
      (define head-cl-deps ((transitive-closure get-deps) head-cl))
      (when (set-member? head-cl-deps head-cl)
        (pretty-error-current (prov->pos head-cl) 
                              "Circular dependencies among head cluases" #:exit #t)))
    (define first-stratum (filter (λ (cl) (set-empty? (get-deps cl))) (set->list heads)))
    ; (printf "first stratum count: ~a\n" (length first-stratum))
    (when (and (empty? first-stratum) (not (empty? heads-lst)))
      (pretty-error-current
        (prov->pos (car heads-lst))
        "Circular dependencies among head clauses" #:exit #t))
    (define first-stratum-set (list->set first-stratum))
    (define rest-heads-set (set-subtract heads first-stratum-set))

    (define first-stratum-rules (map (λ (head) `(rule ,(set head) ,bodys)) first-stratum))
    (define (clause-id-args-w/prov cl) (match-define (list id _rel args) (clause-rel-args-w/prov cl)) (cons id args))

    (define first-stratum+bodys-args (flat-map clause-id-args-w/prov (append (set->list bodys) first-stratum)))
    (define remaining-heads-args (map strip-prov (flat-map clause-id-args-w/prov (set->list rest-heads-set))))
    
    (define args0 (filter (λ (arg) (member (strip-prov arg) remaining-heads-args)) first-stratum+bodys-args))
    (define args (remove-duplicates args0 #:key strip-prov))
    
    (cond
      ;; optimization for cases where the head is like this: (outer x (inner y))
      ;; ie, there are only 2 strata, and the first stratum has only 1 clause
     [(and (= 1 (length first-stratum))
           (andmap (λ (head) (subset? (get-deps head) first-stratum-set)) (set->list rest-heads-set)))
        (define rule-for-remaining-heads
          (ir-fixed-replace-repeated-vars-in-body-clauses
            `(rule ,rest-heads-set ,(set-union bodys first-stratum-set))))
        (foldl set-union (set) (map (app partition-rule _ rule-prov comp-rules) 
                                (cons rule-for-remaining-heads first-stratum-rules)))]
     [else 
      (define dummy-pos (prov->pos (car first-stratum)))
      (define first-stratum-replacement-clause 
        (give-clause-id `(prov ((prov (rel-arity ,(gensymb '$head-stratified) ,(length args) db) ,dummy-pos) ,@args) ,dummy-pos)))
      
      (define rule-for-replacement-clause
        (ir-fixed-replace-repeated-vars-in-body-clauses
          `(rule ,(set first-stratum-replacement-clause) ,(set-union bodys first-stratum-set))))
      (define rule-for-remaining-heads
        `(rule ,rest-heads-set ,(set first-stratum-replacement-clause)))
      
      (foldl set-union (set) (map (app partition-rule _ rule-prov comp-rules) 
                                  (append 
                                    (if (not (set-empty? rest-heads-set))
                                      (list rule-for-remaining-heads  rule-for-replacement-clause) (list)) 
                                    first-stratum-rules)))])]
   ; Multiple bodies and heads
   [`(rule ,heads ,bodys)
    (match (set-first heads)
          [`(prov ((prov = ,=pos) (prov ,x ,xpos) (prov ((prov ,rel ,relpos) ,vals ...) ,clspos)) ,pos)
          (define inter-head (gensymb '$inter-head))
          (define shared-vars
            (set->list (set-intersect (cl-lst-vars (set->list heads))
                                      (cl-lst-vars (set->list bodys)))))
          (define srules0
            (partition-rule `(rule ,heads
                                    ,(set `(prov ((prov = ,pos)
                                                  (prov ,(gensymb '$_) ,pos)
                                                  (prov ((prov (rel-arity ,inter-head
                                                                          ,(length shared-vars)
                                                                          db)
                                                              ,pos)
                                                        ,@(map (lambda (x) `(prov ,x ,pos))
                                                                shared-vars))
                                                        ,pos))
                                                ,pos)))
                              rule-prov comp-rules))
          (define srules1
            (partition-rule `(rule ,(set `(prov ((prov = ,pos)
                                                  (prov ,(gensymb '$_) ,pos)
                                                  (prov ((prov (rel-arity ,inter-head
                                                                          ,(length shared-vars)
                                                                          db)
                                                              ,pos)
                                                        ,@(map (lambda (x) `(prov ,x ,pos))
                                                                shared-vars))
                                                        ,pos))
                                                ,pos))
                                    ,bodys)
                              rule-prov comp-rules))
          (set-union srules0 srules1)])]))

;; creates a hash for builtins:  rel-arity? -> (set rel-version?)
(define builtin-rel-arities->rel-versions
  (foldl (λ (rel-select arities->versions)
            (match rel-select
              [`(rel-version ,rel-name ,arity ,indices comp)
                (hash-update arities->versions `(rel-arity ,rel-name ,arity comp) (λ (s) (set-add s rel-select)) (set))]))
         (hash)
         (hash-keys builtins-as-rels)))

(define (clause-list-grounded cl-lst comp-rules)
  (set-empty? (clause-list-ungrounded-vars cl-lst comp-rules)))

(define (clause-list-ungrounded-vars cl-lst comp-rules)
  (define clauses-rel-args (map clause-rel-args cl-lst))
  (define all-args (foldl (λ (rel-args accu) (match rel-args [(list id rel args) (append accu (cons id args))])) (list) clauses-rel-args))
  (define all-vars (list->set (filter (not/c lit?) all-args)))
  
  (define (compute-grounded-vars-step grounded-vars)
    (foldl (λ (rel-args grounded-vars)
             ;(displayln (format "rel-vars: ~a, grounded-vars: ~a" rel-vars grounded-vars))
             (match-define (list id rel args) rel-args)
             (match-define `(rel-arity ,rel-name ,rel-arity ,rel-kind) rel)
             (define var-id+args (filter (not/c lit?) (cons id args)))
             ;(printf "builtin-rel-arities->rel-selects: ~a" builtin-rel-arities->rel-selects)
             (cond 
              [(comp-or-agg-rel-kind? rel-kind)
                (define indices 
                  (filter-map (λ (ind arg) 
                                 (if (or (lit? arg) (set-member? grounded-vars arg)) ind #f))
                              (range 1 (add1 rel-arity))
                              (map strip-prov args)))
                (if (admissible-comp-rel-indices? rel (list->set indices) comp-rules)
                      (set-union grounded-vars (list->set var-id+args))
                      grounded-vars)]
              [else 
                (set-union grounded-vars (list->set var-id+args))]))
           grounded-vars
           clauses-rel-args))
  (define grounded-vars (iterate-to-fixed-point compute-grounded-vars-step (set)))
  (set-subtract all-vars grounded-vars))
  
(define (validate-ir-small-rule rule rule-prov)
  (match-define `(srule ,head ,bodys ...) rule)
  (define head-vars (clause-vars head))
  (define bodys-vars (apply set-union (cons (set) (map clause-args bodys))))
  (when (not (subset? head-vars bodys-vars))
    (pretty-rule-error (current-source-tree) rule-prov
                       (format "unbound variables: ~a" (intercalate ", " (set->list (set-subtract head-vars bodys-vars))))
                       #:exit #t))
  (match-define (list _ `(rel-arity ,head-rel-name ,head-rel-arity ,head-rel-kind) _) (clause-rel-args head))
  (when (set-member? all-builtin-names head-rel-name)
    (pretty-error-current (prov->pos head) (format "builtins are immutable!") #:exit #t))
  (when (comp-or-agg-rel-kind? head-rel-kind)
    (pretty-error-current (prov->pos head) (format "builtins are immutable!") #:exit #t))
  (for ([cl bodys])
    (match-define (list _ `(rel-arity ,rel-name ,arity ,kind) _) (clause-rel-args cl))
    (when (and (set-member? all-builtin-names rel-name)
               (not (hash-has-key? builtin-rel-arities->rel-versions `(rel-arity ,rel-name ,arity comp))))
      (define builtin-arities (map third (filter (λ (rel-arity) (equal? (second rel-arity) rel-name)) (hash-keys builtin-rel-arities->rel-versions))))
      (pretty-error-current (prov->pos cl)  (format "wrong builtin arity! \"~a\" has ~a of ~a" rel-name
                                                    (if (= 1 (length builtin-arities)) "an arity" "arities")
                                                    (intercalate ", " builtin-arities)) #:exit #t))))

;; returns the updated rule + a list of created comp rules
(define/contract (consolidate-comp-clauses rule comp-rules)
  (ir-fixed-rule? list? . -> . (cons/c ir-fixed-rule? (listof ir-small-comp-rule?)))
  (match-define `(rule ,heads ,bodys) rule)
  (define-values (body-comp-cls body-normal-cls)
    (partition (λ (body-cl)
               (match-define (list _ rel args) (clause-rel-args body-cl))
               (comp-rel-arity? rel))
            (set->list bodys)))
  (define comp-cls-vars
    (remove-duplicates
      (flat-map (λ (cl)
                   (match-define (list _ rel args) (clause-rel-args-w/prov cl))
                   (filter (λ (arg) (var? (strip-prov arg))) args))
                body-comp-cls)
    #:key strip-prov))
  ;; if the rule has aggregators, we can't just assume every arg to the synthetic comp rule
  ;; will be grounded by db clauses in the rule. In general, it may not be possible to consolidate all
  ;; the comp-rels into a synthetic comp rule in that case. 
  (define has-aggregators
    (ormap (λ (cl)
              (match-define (list _ rel args) (clause-rel-args cl))
              (agg-rel-kind? (rel-arity->kind rel)))
      body-normal-cls))
  (cond
    [has-aggregators (cons rule (list))]
    [(> (length body-comp-cls) 1)
      (define pos (prov->pos (car body-comp-cls)))
      (define new-comp-rel-arity `(rel-arity ,(gensymb '$combined-comp) ,(length comp-cls-vars) comp))
      (define comp-cls-replacement-cl-head `(prov ((prov ,new-comp-rel-arity ,pos) ,@comp-cls-vars) ,pos))
      (define comp-cls-replacement-cl `(prov ((prov = ,pos) (prov ,(gensymb '$_) ,pos) ,comp-cls-replacement-cl-head) ,pos))
      (define new-comp-rule
        `(crule ,comp-cls-replacement-cl-head
                ,@body-comp-cls))
      (define new-rule
          `(rule ,heads
            ,(set-add (list->set body-normal-cls) comp-cls-replacement-cl)))
      (cons new-rule (list new-comp-rule))]
    [else (cons rule (list))]))

;; Given the list of computational rules, returns whether the given comp-rel is admissible
(define (_admissible-comp-rel-indices? comp-rel-arity indices comp-rules [assume-admissible (set)])
  (assert-pred set? indices)
  (match-define `(rel-arity ,comp-rel-name ,arity ,kind) comp-rel-arity)
  (define res (cond
    [(set-member? assume-admissible (cons comp-rel-arity indices)) #t]
    ;; TODO in builtins.rkt, aggregators and builtins work slightly differently.
    ;; builtins are extended to more args there, but aggregators are not.
    ;; we can make it so builtins are not extended automatically either.
    [(builtin? comp-rel-name)
      (define rel-selects (hash-ref all-builtin-rel-arity->rel-selects comp-rel-arity #f))
      (cond 
        [(not rel-selects)
          (pretty-error-current)])
      (ormap (λ (bi-rel-select)
                (match-define `(rel-select ,n ,ar ,ind comp) bi-rel-select)
                (and (equal? (list->set ind) indices)))
             (set->list rel-selects))]
    [(agg-rel-kind? kind)
     (ormap (λ (agg-spec)
              (match-define `(aggregator-spec ,spec-name ,spec-arity ,spec-indices ,rel-arity ,rel-indices) agg-spec)
              (and (equal? (list comp-rel-name arity) (list spec-name spec-arity))
                   (subset? (list->set spec-indices) indices)))
            (hash-keys all-aggregators))]
    [else 
      (andmap 
        (λ (comp-rule)
          (match-define `(crule ,head ,bodys ...) comp-rule)
          (match-define (list _ head-rel-arity head-args) (clause-rel-args head))
          (cond
            [(equal? head-rel-arity comp-rel-arity)

             (define vars-given-indices (list->set (filter var? (map (app list-ref head-args _) (map sub1 (set->list indices))))))  
             (define head-vars (filter var? head-args))
             (define grounded-vars 
              (foldl 
              (λ (body-cl grounded-vars)
                (match-define (list _ brel-arity args) (clause-rel-args body-cl))
                (define current-vars-indices
                       (append-map (λ (arg ind) 
                                   (if (or (lit? arg) (set-member? grounded-vars arg)) (list ind) (list)))
                         (map strip-prov args)
                         (range 1 (add1 (length args)))))
                (define new-grounded-vars
                  (if (admissible-comp-rel-indices? brel-arity (list->set current-vars-indices) comp-rules (set-add assume-admissible (cons comp-rel-arity indices))) 
                         (list->set (map strip-prov args)) 
                         (set)))
              (set-union grounded-vars new-grounded-vars))
              vars-given-indices
              bodys))
             (define all-body-vars (list->set (flat-map 
              (λ (body-cl) 
                (match-define (list _ brel-arity args) (clause-rel-args body-cl))
                (filter var? (map strip-prov args))) bodys)))
            ;  (printf "all body vars: ~a; grounded vars: ~a\n" all-body-vars grounded-vars)
             (and (subset? all-body-vars grounded-vars) (subset? (list->set head-vars) grounded-vars))]
            [else #t]))
        comp-rules)]))
  ; (printf "(admissible-comp-rel-indices? ~a ~a ~v): ~a\n" comp-rel-arity indices assume-admissible res)
  res)

(define admissible-comp-rel-indices? _admissible-comp-rel-indices?)

(define test-rule
  `(rule ,(set '(prov ((prov = _) (prov _0 _) (prov ((prov (rel-arity C 3 db) _) (prov x _) (prov y _) (prov z _)) _)) _)) 
         ,(set '(prov ((prov = _) (prov _4 _) (prov ((prov (rel-arity A 1 db) _) (prov x _)) _)) _) 
               '(prov ((prov = _) (prov _2 _) (prov ((prov (rel-arity B 2 db) _) (prov y _) (prov z _)) _)) _) 
               '(prov ((prov = _) (prov _1 _) (prov ((prov (rel-arity + 3 comp) _) (prov x _) (prov y _) (prov z _)) _)) _) 
               '(prov ((prov = _) (prov _3 _) (prov ((prov (rel-arity > 2 comp) _) (prov x _) (prov 1 _)) _)) _))))
; (printf "~a\n" (consolidate-comp-clauses test-rule '()))

(define (ir-fixed-head->ir-flat-head head-cl)
  (match-define `(prov ((prov = ,=pos) ,id ,cl) ,pos) head-cl)
  cl)

(define (comp/agg-clause? cl)
  (match-define (list id rel args) (clause-rel-args cl))
  (comp-or-agg-rel-arity? rel))



;; returns the dependency graph of the set of head clauses

(define _dep-graph-cache (make-hash))
(define (_get-dep-graph heads)
  (define res
    (foldl (λ (head-cl gr)
          (match-define (list id _rel xs) (clause-rel-args head-cl))
          (foldl (λ (head-cl+ gr)
                    (match-define (list id+ _rel+ xs+) (clause-rel-args head-cl+))
                    (if (member id+ xs)
                        (hash-set gr head-cl (set-add (hash-ref gr head-cl) head-cl+))
                        gr))
                  (hash-set gr head-cl (hash-ref gr head-cl set))
                  (set->list heads)))
        (hash)
        (set->list heads)))
  (define (stratified-dep-graphs current-graph)
    (cond 
      [(hash-empty? current-graph) (void)]
      [else 
        (define current-heads (hash-keys current-graph))
        (define first-stratum (filter (λ (cl) (set-empty? (hash-ref current-graph cl))) current-heads))
        (define first-stratum-set (list->set first-stratum))
        (define rest-heads-set (set-subtract (list->set current-heads) first-stratum-set))
        (define rest-heads-graph (foldl
          (λ (head-cl new-gr) (hash-set new-gr head-cl (set-subtract (hash-ref current-graph head-cl) first-stratum-set)))
          (hash)
          (set->list rest-heads-set)))
        (hash-set! _dep-graph-cache rest-heads-set rest-heads-graph)
        (stratified-dep-graphs rest-heads-graph)]))
  (stratified-dep-graphs res)
  res)

(define (get-dep-graph heads)
  (define cached (hash-ref _dep-graph-cache heads #f))
  ; (when cached (printf "dep-graph cached for ~a heads\n" (set-count heads))) 
  (or cached
    (_get-dep-graph heads)))

(define (rule-prov->id rule-prov)
  (match-define `(rule-prov ir-flat ,fixed-rule ,module ,source-id) rule-prov)
  source-id)