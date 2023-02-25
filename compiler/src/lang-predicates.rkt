
#lang racket

;; Predicates for slog's syntax and intermediate languages
;; Copyright (c) Thomas Gilray, et al, see License.md

(require "builtins.rkt")
(require "generic-utils.rkt")
(provide (all-defined-out))

;(require racket/trace)

; Source Tree
; (input language)
(define (assert-source-tree? st)
  (match st
         [(? (and/c hash? immutable?))
          (for ([k (hash-keys st)]) (assert-pred string? k))
          (for ([m (hash-values st)]) (assert-source-tree-module? m))]))

(define source-tree? (assertion->contract assert-source-tree?))

(struct Module (raw-lines includes facts rules comp-rels) #:transparent)

(define (assert-source-tree-module? stm)
  (match stm
         [(Module #;raw-lines (? (vector-immutableof string?))
                  #;includes (? (listof string?))
                  #;facts (? (set/c source-tree-fact?))
                  #;rules (and rules-h (? (and/c hash? immutable?)))
                  #;comp-rels (? (set/c symbol?)))
          (for ([k (hash-keys rules-h)]) (assert-pred nonnegative-integer? k))
          (for ([r (hash-values rules-h)]) (assert-pred source-tree-rule? r))]))

(define source-tree-module? (assertion->contract assert-source-tree-module?))

(define (source-tree-fact? fact)
  (define (stripped-fact? f)
    (match f
         [(? lit?) #t]
         [`(,(? rel-name?) ,(? stripped-fact?) ...) #t]
         [else #f]))
  (and 
    (stripped-fact? (strip-prov fact))
    (source-tree-hclause? fact)))

(define (assert-source-tree-rule? rule)
  (define (assert-segment-rtl? s)
    (match s
    [`(,heads ... <-- ,bodys ...) 
      (for ([head heads]) (assert-pred-syntax source-tree-head-item? head))
      (for ([body bodys]) (assert-pred-syntax source-tree-body-item? body))]))
  (define (assert-segment-ltr? s)
    (match s
    [`(,bodys ... --> ,heads ...)
      (for ([head heads]) (assert-pred-syntax source-tree-head-item? head))
      (for ([body bodys]) (assert-pred-syntax source-tree-body-item? body))]))

  (define rule-has-arrow
    (match rule
      [`(prov ,xs ,pos) (or (member '--> xs) (member '<-- xs))]
      [else #f]))
  (define (segment-arrow seg)
    (cond [(member '<-- seg) '<--]
          [(member '--> seg) '-->]
          [else #f]))
  (match rule
    [`(prov ,xs ,(? pos?)) #:when rule-has-arrow
     (define segments (list-split xs '(--)))
     (assert (not (empty? segments)))
     (define segments-arrows (filter-map segment-arrow segments))
     (assert (or (andmap (λ (arr) (equal? arr '<--)) segments-arrows) (andmap (λ (arr) (equal? arr '-->)) segments-arrows)))
     (assert (not (empty? segments-arrows)))
     (match (car segments-arrows)
       ['--> (for [(seg segments)] (assert-segment-ltr? seg))]
       ['<-- (for [(seg segments)] (assert-segment-rtl? seg))])]
    [else
     (assert (source-tree-hclause? rule))]))


(define source-tree-rule? (assertion->contract assert-source-tree-rule?))
#;(define (source-tree-rule? rule)
  (define (segment-rtl? s)
    (match s
    [`(,(? source-tree-head-item?) ... <-- ,(? source-tree-body-item?) ...) #t]
    [else #f]))
  (define (segment-ltr? s)
    (match s
    [`(,(? source-tree-body-item?) ... --> ,(? source-tree-head-item?) ...) #t]
    [else #f]))
  (match rule
         [(? source-tree-hclause?) #t]
         [`(prov ,xs ,(? pos?))
          ; (printf "source-tree-rule? xs: ~a\n" (intercalate "\n" xs))
          (define segments (list-split xs '(--)))
          (and (not (empty? segments))
               (or (andmap segment-rtl? segments) (andmap segment-ltr? segments)))]
         [else #f]))

(define (source-tree-body-item? bi)
  (match bi
    [(? source-tree-bclause?) #t]
    [`(INNER-RULE ,(? source-tree-rule?)) #t]
    [else #f]))

(define (source-tree-head-item? hi)
  (match hi
    [(? source-tree-hclause?) #t]
    [`(INNER-RULE ,(? source-tree-rule?)) #t]
    [else #f]))

(define (source-tree-hclause? hc)
  (match hc
         [`(prov (= ,(? source-tree-ihclause?) ,(? source-tree-ihclause?)) ,(? pos?)) #t]
         [else
          (source-tree-ihclause? hc)]))

(define (source-tree-clause-under-or? cl)
 (match cl
  [`(prov ((prov and ,(? pos?)) ,(? source-tree-bclause?) ...) ,(? pos?)) #t]
  [(? source-tree-bclause?) #t]
  [else #f]))

(define (source-tree-bclause? bc)
  (match bc
         [`(prov ((prov = ,=pose) ,(? source-tree-ibclause?) ,(? source-tree-ibclause?)) ,(? pos?)) #t]
         [`(prov ((prov or ,orpos) ,(? source-tree-clause-under-or?) ...) ,(? pos?)) #t]
         [else (source-tree-ibclause? bc)]))

(define (source-tree-ihclause? ihc)
  (match ihc
         [`(prov (? ((prov ,(? rel-name?) ,(? pos?)) ,(? source-tree-ibclause?) ...)) ,(? pos?)) #t]
         [`(prov (?do (,(? source-tree-ibclause?) ...)) ,(? pos?)) #t]
         [`(prov (? (LIST-SYNTAX ,args ...)) ,(? pos?)) (source-tree-blist-items? args)]
         [`(prov ((prov ,(? rel-name?) ,(? pos?)) ,(? source-tree-ihclause?) ...) ,(? pos?)) #t]
         [`(prov ,(? arg?) ,(? pos?)) #t]
         [`(prov (LIST-SYNTAX ,args ...) ,(? pos?)) (source-tree-hlist-items? args)]
         [`(prov (CURLY-CLAUSE ,tag ,(? source-tree-ibclause?) ...) ,(? pos?)) #t]
         [else #f]))

(define (source-tree-ibclause? ibc) 
  (match ibc
         [`(prov (,(? source-tree-rel?) ,(? source-tree-ibclause?) ...) ,(? pos?)) #t]
         [`(prov (! ((prov ,(? rel-name?) ,(? pos?)) ,(? source-tree-ihclause?) ...)) ,(? pos?)) #t]
         [`(prov (!do (,(? source-tree-ibclause?) ...)) ,(? pos?)) #t]
         [`(prov (! (LIST-SYNTAX ,args ...)) ,(? pos?)) (source-tree-hlist-items? args)]
         [`(prov ,(? arg?) ,(? pos?)) #t]
         [`(prov (or ,(? source-tree-ibclause?) ...) ,(? pos?)) #t]
         [`(prov (LIST-SYNTAX ,args ...) ,(? pos?)) (source-tree-blist-items? args)]
         [`(prov (CURLY-CLAUSE ,tag ,(? source-tree-ibclause?) ...) ,(? pos?)) #t]
         [else #f]))

(define (source-tree-blist-items? items)
  (match items
    [`(,(? source-tree-ibclause?) ,'... ,rest ...) (source-tree-blist-items? rest)]
    [`(,(? source-tree-ibclause?) ,rest ...) (source-tree-blist-items? rest)]
    [`() #t]
    [else #f]))

(define (source-tree-hlist-items? items)
  (match items
    [`(,(? source-tree-ihclause?) ,'... ,rest ...) (source-tree-hlist-items? rest)]
    [`(,(? source-tree-ihclause?) ,rest ...) (source-tree-hlist-items? rest)]
    [`() #t]
    [else #f]))

(define (source-tree-rel? rel)
  (match rel
    [(? (prov-of rel-name?)) #t]
    [`(agg ,(? (prov-of rel-name?)) ,(? (prov-of rel-name?))) #t]
    [else #f]))

(define ir-flat-rel? source-tree-rel?)
(define (ir-flat-agg-rel? rel)
  (match rel
    [`(agg ,tag ,rel) #t]
    [else #f]))

; predicates for ir-flat (post "organize-pass")
(define (assert-ir-flat? ir)
  (match ir
         [`(ir-flat ,(? source-tree?) ,rules-h ,comp-rules-h)
           (for ([r (hash-keys rules-h)])
            (assert-pred ir-flat-rule? r))
           (for ([prov (hash-values rules-h)])
            (assert-pred rule-prov? prov))
           (for ([r (hash-keys comp-rules-h)])
            (assert-pred ir-flat-comp-rule? r))
           (for ([prov (hash-values comp-rules-h)])
            (assert-pred rule-prov? prov))]))

(define ir-flat? (assertion->contract assert-ir-flat?))


(define (ir-flat-rule? rule)
  (match rule
         [`(rule ,(? (and/c (lambda (s) (not (set-empty? s))) (set/c ir-flat-clause?)))
                 ,(? (set/c ir-flat-clause?)))
          #t]
         [else #f]))

(define (ir-flat-comp-rule? rule)
  (match rule
         [`(crule ,(? ir-flat-clause?)
                  ,(? ir-flat-clause?) ...)
          #t]
         [else #f]))

(define (ir-flat-clause? cl)
  (match cl
         [`(prov ((prov = ,(? pos?))
                  (prov ,(? var?) ,(? pos?))
                  (prov (,(? ir-flat-rel?)
                         (prov ,(? arg?) ,(? pos?))
                         ...)
                        ,(? pos?)))
                 ,(? pos?))
          #t]
         [else #f]))


; predicates for ir-fixed (post fix-arities-pass)
(define (assert-ir-fixed? ir)
  (match ir
         [`(ir-fixed ,(? ir-flat?)
                     ,rules-hash ,comp-rules-h)
          (for ([r (hash-keys rules-hash)]) (assert-pred ir-fixed-rule? r))
          (for ([prov (hash-values rules-hash)]) (assert-pred rule-prov? prov))
          (for ([r (hash-keys comp-rules-h)]) (assert-pred ir-fixed-comp-rule? r))
          (for ([prov (hash-values comp-rules-h)]) (assert-pred rule-prov? prov))]))
          
(define ir-fixed? (assertion->contract assert-ir-fixed?))

(define (ir-fixed-rule? rule)
  (match rule
         [`(rule ,(? (set/c ir-fixed-clause?))
                 ,(? (set/c ir-fixed-clause?)))
          #t]
         [else #f]))

(define (ir-fixed-comp-rule? rule)
  (match rule
         [`(crule ,(? ir-fixed-clause?)
                  ,(? ir-fixed-clause?) ...)
          #t]
         [else #f]))

(define (ir-fixed-clause? cl)
  (match cl
         [`(prov ((prov = ,(? pos?))
                  (prov ,(? var?) ,(? pos?))
                  (prov ((prov ,(? rel-arity?) ,(? pos?))
                         (prov ,(? arg?) ,(? pos?))
                         ...)
                        ,(? pos?)))
                 ,(? pos?))
          #t]
         [else #f]))

(define (ir-fixed-clause->rel cl)
  (match-define `(prov ((prov = ,=pos) ,id
                  (prov ((prov ,rel ,relpos) ,args ...)
                        ,_ipos)) ,_opos) cl)
  rel)

; predicates for ir-small (post static-unification)
(define (assert-ir-small? ir)
  (match ir
         [`(ir-small ,(? ir-fixed?)
                     ,rules-h
                     ,comp-rules-h)
          (for ([r (hash-keys rules-h)]) (assert (ir-small-rule? r) (format "bad rule: ~a" (strip-prov r))))
          (for ([r (hash-values rules-h)]) (assert-pred rule-prov? r))
          (for ([r (hash-keys comp-rules-h)]) (assert (ir-small-comp-rule? r) (format "bad comp rule: ~a" (simplify-prov r)) ))
          (for ([r (hash-values comp-rules-h)]) (assert-pred rule-prov? r))]))
(define ir-small? (assertion->contract assert-ir-small?))

(define (ir-small-comp-rule? crule)
  (match crule 
    [`(crule ,(? (app ir-small-head? _ (app equal? 'comp _)))
             ,(? ir-small-clause?) ...) #t]
    [else #f]))

(define (ir-small-rule? rule)
  (match rule
         [`(srule ,(? ir-small-head?)
                  ,(? ir-small-clause? cl1)
                  ,(? ir-small-clause? cl2))
          (or (db-rel-arity? (ir-fixed-clause->rel cl1)) (db-rel-arity? (ir-fixed-clause->rel cl2)))]
         [`(srule ,(? ir-small-head?)
                  ,(? ir-small-clause? cl1))
          (db-rel-arity? (ir-fixed-clause->rel cl1))]
         [`(srule ,(? ir-small-head?))
          #t]
         [else #f]))

(define (ir-small-head? cl (kind? db-rel-kind?))
  (match cl
         [`(prov ((prov (rel-arity ,(? rel-name?) ,arity ,(? kind?)) ,(? pos?))
                  (prov ,(? arg? args) ,(? pos?))
                  ...)
                 ,(? pos?))
          (equal? (length args) arity)]
         [else #f]))

(define (ir-small-clause? cl)
  (and (ir-fixed-clause? cl)
       ;; db ir-small body clauses can't contain repeated vars
       (match cl 
        [`(prov ((prov = ,=pos) ,id (prov (,rel ,args ...) ,(? pos?)))
                ,(? pos?)) #:when (db-rel-arity? (strip-prov rel))
          (define args-stripped (map strip-prov args))
          (equal? (remove-duplicates args-stripped) args-stripped)]
        [else #t])))

; predicates for ir-select (post selection splitting)
(define (assert-ir-select? ir)
  (match ir
         [`(ir-select ,(? ir-small?)
                      ,rel-h
                      ,rules-h
                      ,comp-rules-h)
          (for ([rel-ind (hash->list rel-h)])
            (assert-pred rel-arity? (car rel-ind))
            (match-define `(rel-arity ,rel ,arity db) (car rel-ind))
            (assert (equal? (car (cdr rel-ind)) (range 1 (add1 arity)) ))
            (assert-pred (cons/c select-order? (set/c select-order?)) (cdr rel-ind)))
          (for ([r (hash-keys rules-h)]) (assert (ir-select-rule? r) (format "bad rule: ~a\n" (strip-prov r))))
          (for ([p (hash-values rules-h)]) (assert-pred rule-prov? p))
          (for ([r (hash-keys comp-rules-h)]) (assert-pred ir-select-comp-rule? r))
          (for ([p (hash-values comp-rules-h)]) (assert-pred rule-prov? p))]))

(define ir-select? (assertion->contract assert-ir-select?))

(define (ir-select-comp-rule? rule)
  (match rule
    [`(crule ,(? ir-select-clause?)
             ,(? ir-select-clause?) ...) #t]
    [else #f]))

(define (ir-select-rule? rule)
  (match rule
         [`(srule ,(? ir-select-clause?)
                  ,(? ir-select-clause? b1)
                  ,(? ir-select-clause? b2))
           (match-define (cons `(rel-select ,rel1 ,arity1 ,sel1 ,kind1) args1) (ir-select-clause-rel-args b1))
           (match-define (cons `(rel-select ,rel2 ,arity2 ,sel2 ,kind2) args2) (ir-select-clause-rel-args b2))
           (cond 
            [(not (or (db-rel-kind? kind1) (db-rel-kind? kind2))) #f]
            [(and (not (builtin? rel1)) (not (builtin? rel2))) ;;TODO we can't enforce this with comp-rels
              ;; in binary joins, join columns must be aligned
              (define common-args (set-intersect (list->set args1) (list->set args2)))
              #;(and (equal? (list->set (take args1 (set-count common-args))) common-args )
                   (equal? (take args1 (set-count common-args)) (take args2 (set-count common-args))))
              #t]
            [else #t])]
         [`(srule ,(? ir-select-clause?)
                  ,(? ir-select-clause? b1))
           (match-define (cons `(rel-select ,rel1 ,arity1 ,sel1 ,kind1) args1) (ir-select-clause-rel-args b1))
           (db-rel-kind? kind1)]
         [`(srule ,(? ir-select-clause?))
          #t]
         [`(arule ,(? ir-select-clause?)
                  ,(? ir-select-clause?))
          #t]
         [else #f]))

(define (ir-select-clause? cl)
  (match cl
         [`(prov ((prov ,(? rel-select?) ,(? pos?))
                  (prov ,(? arg?) ,(? pos?))
                  ...)
                 ,(? pos?))
          #t]
         [else #f]))

;; extract the rel-select and args of an ir-select-clause
(define (ir-select-clause-rel-args cl)
  (match-define `(prov ((prov ,(? rel-select? rel) ,(? pos?)) (prov ,(? arg? args) ,(? pos?)) ...)
                  ,(? pos?)) 
                cl)
  (cons rel args))

; predicate for the ir-scc IR
(define (ir-scc? ir)
  (match ir
         [`(ir-scc ,(? ir-select?)
                   ,(? (and/c (lambda (h) (andmap nonnegative-integer? (hash-keys h)))
                              (lambda (h) (andmap (set/c nonnegative-integer?) (hash-values h)))))
                   ,(? (and/c (lambda (h) (andmap nonnegative-integer? (hash-keys h)))
                              (lambda (h) (andmap ir-scc-scc? (hash-values h)))))
                   ,(? (and/c (lambda (h) (andmap ir-scc-comp-rule? (hash-keys h)))
                              (lambda (h) (andmap rule-prov? (hash-values h))))))
          #t]
         [else #f]))

(define (ir-scc-scc? scc)
  (match scc
         [`(scc ,(or 'looping 'nonlooping)
                ,(? (and/c (lambda (h) (andmap rel-arity? (hash-keys h)))
                           (lambda (h) (andmap (list/c rel-use-status? rel-deletable? select-order? (set/c select-order?))
                                               (hash-values h)))))
                ,(? (and/c (lambda (h) (andmap ir-scc-rule? (hash-keys h)))
                           (lambda (h) (andmap rule-prov? (hash-values h))))))
          #t]
         [else #f]))

(define (ir-scc-rule? rule)
  (ir-select-rule? rule))

(define ir-scc-comp-rule? ir-select-comp-rule?)

; predicate for the incremental IR
(define (ir-incremental? ir)
  (match ir
         [`(ir-incremental ,(? ir-scc?)
                           ,(? (and/c (lambda (h) (andmap nonnegative-integer? (hash-keys h)))
                                      (lambda (h) (andmap (set/c nonnegative-integer?) (hash-values h)))))
                           ,(? (and/c (lambda (h) (andmap nonnegative-integer? (hash-keys h)))
                                      (lambda (h) (andmap ir-incremental-scc? (hash-values h)))))
                           ,(? (and/c (lambda (h) (andmap ir-incremental-comp-rule? (hash-keys h)))
                                      (lambda (h) (andmap rule-prov? (hash-values h))))))
          #t]
         [else #f]))

(define (ir-incremental-scc? scc)
  (match scc
         [`(scc ,(or 'looping 'nonlooping)
                ,(? (and/c (lambda (h) (andmap rel-arity? (hash-keys h)))
                           (lambda (h) (andmap (list/c rel-use-status? rel-deletable? select-order? (set/c select-order?))
                                               (hash-values h)))))
                ,(? (and/c (lambda (h) (andmap ir-incremental-rule? (hash-keys h)))
                           (lambda (h) (andmap rule-prov? (hash-values h))))))
          #t]
         [else #f]))

(define rel-use-status?
  (or/c 'static 'dynamic 'unused))

(define rel-deletable?
  (or/c 'deletable 'not-deletable))

(define (ir-incremental-rule? rule)
  (match rule
         [`(srule ,(? ir-incremental-hclause?)
                  ,(? ir-incremental-bclause?)
                  ,(? ir-incremental-bclause?))
          #t]
         [`(srule ,(? ir-incremental-hclause?)
                  ,(? ir-incremental-bclause?))
          #t]
         [`(srule ,(? ir-incremental-hclause?))
          #t]
         [`(arule ,(? ir-incremental-hclause?)
                  ,(? ir-incremental-bclause?))
          (match-define `(arule ((rel-select  ,h-rel-name ,h-arity ,h-selection ,h-kind) ,h-args ...)
                                ((rel-version ,b-rel-name ,b-arity ,b-selection ,b-version) ,b-args ...)) (strip-prov rule))
          ;; the args must be consistent with the selections
          (equal? ((unproject-then-rest-zero-base b-selection b-arity) b-args)
                  ((unproject-then-rest-zero-base h-selection h-arity) h-args))]
         [else #f]))

(define ir-incremental-comp-rule? ir-select-comp-rule?)
(define (ir-incremental-hclause? cl)
  (ir-select-clause? cl))

(define (ir-incremental-bclause? cl)
  (match cl
         [`(prov ((prov ,(? rel-version?) ,(? pos?))
                  (prov ,(? arg?) ,(? pos?))
                  ...)
                 ,(? pos?))
          #t]
         [else #f]))

; predicate for the interpreter pass
(define (ir-interp? ir)
  (match ir
         [`(ir-interp ,(? ir-incremental?)
                      ,(? (and/c (lambda (h) (andmap rel-arity? (hash-keys h)))
                                 (lambda (h) (andmap (cons/c select-order? (set/c select-order?)) (hash-values h)))))
                      ,(? db-instance?)
                      ,(? (listof nonnegative-integer?)))
          #t]
         [else #f]))

(define db-id? nonnegative-integer?)
(define fact? (listof nonnegative-integer?))

; predicate for per-relation relation facts
(define (rel-instance? ri)
  (match ri
    [`(rel-instance ,(? (and/c (lambda (h) (andmap (or/c db-id? fact?)
                                                   (hash-keys h)))
                               (lambda (h) (andmap (or/c db-id? fact?)
                                                   (hash-values h)))))
                    ,(? nonnegative-integer?)
                    ,(? nonnegative-integer?))
     #t]
    [else #f]))

;; TODO: definition is outdated
; predicate for the accumulated db structure produced by the interpreter
(define (db-instance? db)
  (error 'db-instance? "db-instance? is outdated!")
  (match db
         [`(db-instance
            ,(? (and/c hash?
                       (lambda (h) (andmap rel-arity? (hash-keys h)))
                       (lambda (h) (andmap rel-instance? (hash-values h)))))
            ,(? (and/c hash?
                       (lambda (h) (andmap rel-version? (hash-keys h)))
                       (lambda (h) #t)))
            ,(? (and/c (lambda (h) (andmap (lambda (x) (member x '(string symbol relation))) (hash-keys h)))
                       (lambda (h) (andmap nonnegative-integer? (hash-values h)))))
            ,(? (and/c (lambda (h) (andmap (or/c symbol? string? db-id?) (hash-keys h)))
                       (lambda (h) (andmap (or/c symbol? string? db-id?) (hash-values h))))))
          #t]
         [else #f]))

; predicate for debugging pass
(define (ir-debug? ir)
  (match ir
    [`(ir-debug ,(? (and/c hash?
                           (lambda (h) (andmap number? (hash-keys h)))
                           (lambda (h) (andmap (set/c number?) (hash-values h)))))
                ,(? (and/c hash? 
                           (lambda (h) (andmap number? (hash-keys h)))
                           (lambda (h) (andmap 
                                        (cons/c ir-interp? (cons/c continuation? null?))
                                        (hash-values h)))))
                ,(? debugger-state?)
                ,(? number?)
                ,(? number?)
                ,(? number?)
                ,(? hash?)) #t]))

; the configuration of the debugger
(define (debugger-state? db)
  (match db
    [`(debugger-state ,(? (set/c breakpoint?)) ,(? debug-position?)) #t]
    [else #f]))

; debugger position
(define (debug-position? dbp)
  (match dbp
    [`init-state #t]
    [`(before-scc ,scc) #t]
    [`(at-rule ,rule) #t]))

; predicates for debugging events
(define (debug-event? x)
  (define (sub-event? x)
    (match x
      [`(rule ,e) #t]
      [`(scc ,n) #t]
      [_ #f]))
  (match x 
    [`(adding ,e) #t]
    [`(finished-scc-iter ,(? number? n)) #t]
    [`(finished-scc ,(? number? n)) #t]
    [`(before ,(? sub-event? se)) #t]
    [`(after ,(? sub-event? se)) #t]
    ['initial-state #t]
    ['finished-fixed-point #t]
    [_ #f]))

; breakpoints
(define (breakpoint? x) (debug-event? x))

; predicate for rule provenance tags
(define (rule-prov? p)
  (match p
         [`(rule-prov source-tree ,(? string?) ,(? nonnegative-integer?)) #t]
         [`(rule-prov ir-flat ,(? ir-flat-rule?) ,(? string?) ,(? nonnegative-integer?)) #t]
         [`(rule-prov ir-fixed ,(? ir-fixed-rule?) ,(? string?) ,(? nonnegative-integer?)) #t]
         [`(rule-prov ir-small ,(? ir-small-rule?) ,(? string?) ,(? nonnegative-integer?)) #t]
         [`(rule-prov ir-select ,(? ir-select-rule?) ,(? string?) ,(? nonnegative-integer?)) #t]
         
         [`(rule-prov ir-flat ,(? ir-flat-comp-rule?) ,(? string?) ,(? nonnegative-integer?)) #t]
         [`(rule-prov ir-fixed ,(? ir-fixed-comp-rule?) ,(? string?) ,(? nonnegative-integer?)) #t]
         [`(rule-prov ir-small ,(? ir-small-comp-rule?) ,(? string?) ,(? nonnegative-integer?)) #t]
         [`(rule-prov ir-select ,(? ir-select-comp-rule?) ,(? string?) ,(? nonnegative-integer?)) #t]
         
         [`(rule-prov ir-scc ,(? nonnegative-integer? scc-id) ,(? ir-scc-rule?) ,(? string?) ,(? nonnegative-integer?)) #t]
         [`(rule-prov intra-relation ,(? rel-name?) ,(or (? nonnegative-integer?) 'variadic)) #t]
         [else #f]))

; predicate for position information tags
(define (pos? p)
  (match p
         [`(pos ,(? string?)
                ,(? (cons/c nonnegative-integer? nonnegative-integer?))
                ,(? (cons/c nonnegative-integer? nonnegative-integer?)))
          #t]
         ['synthetic #t]
         ['_ #t]
         [else #f]))

; predicate for a literal base value (non-variable)
(define (lit? l)
  (match l
         [(? number?) #t]
         [(? string?) #t]
        ;  ['true #t]
        ;  ['false #f]
         [else #f]))

; predicate for a literal base value (non-variable)
(define (interned-lit? l)
  (match l
         [`(,(? rel-name?)) #t]
         [(? string?) #t]
         [else #f]))

; predicate for a non-splicing variable
(define (var? l)
  (match l
         [(? symbol?) #t]
         [else #f]))

; predicate for a base value / argument (w/o splicing vars)
(define (arg? l)
  (match l
         [(? lit?) #t]
         [(? var?) #t]
         [else #f]))

; predicate for a select order
(define (select-order? sel)
  (match sel
         [`(,(? nonnegative-integer?) ...) #t]
         [else #f]))

; predicate for a relation name
(define rel-name? symbol?)

; predicate for a relation name + arity + relation kind
(define (rel-arity? r)
  (match r
    [`(rel-arity ,(? rel-name?) ,(? nonnegative-integer?) db) #t]
    [`(rel-arity ,(? rel-name?) ,(? nonnegative-integer?) comp) #t]
    [`(rel-arity ,(? rel-name?) ,(? nonnegative-integer?) (agg ,db-rel)) #t]
    [else #f]))

(define (comp-or-agg-rel-arity? r)
  (match r
    [`(rel-arity ,(? rel-name?) ,(? nonnegative-integer?) comp) #t]
    [`(rel-arity ,(? rel-name?) ,(? nonnegative-integer?) (agg ,db-rel)) #t]
    [else #f]))

(define (comp-rel-arity? r)
  (match r
    [`(rel-arity ,(? rel-name?) ,(? nonnegative-integer?) comp) #t]
    [else #f]))

(define (db-rel-arity? r)
  (match-define `(rel-arity ,(? rel-name?) ,(? nonnegative-integer?) ,kind) r)
  (db-rel-kind? kind))

(define (comp-or-agg-rel-kind? kind)
  (match kind
    ['comp #t]
    [`(agg ,_ ...) #t]
    [else #f]))

(define (comp-rel-kind? kind)
  (equal? kind 'comp))

(define (agg-rel-kind? kind)
  (match kind
    [`(agg ,_ ...) #t]
    [else #f]))

(define (db-rel-kind? kind)
  (equal? kind 'db))

(define (rel-arity->kind rel-arity)
  (match-define `(rel-arity ,name ,arity ,kind) rel-arity)
  kind)

(define (rel-select-kind? k)
  (match k
    ['db #t]
    ['comp #t]
    [`(agg ,agg-rel) #t]
         [else #f]))

; predicate for a relation name + arity + select-set
(define (rel-select? r)
  (match r
    [`(rel-select ,(? rel-name?) ,(? nonnegative-integer?) ,(? select-order?) ,(? rel-select-kind?)) #t]
         [else #f]))

(define (rel-select->kind rel-select)
 (match rel-select
  [`(rel-select ,name ,arity ,sel ,kind) kind]))

(define (db-rel-select? rel-select)
  (db-rel-kind? (rel-select->kind rel-select)))


; predicate for a relation name + arity + select-set
(define (rel-version? r)
  (match r
         [`(rel-version ,(? rel-name?) ,(or (? nonnegative-integer?) 'variadic) ,(? select-order?) ,(or 'delta 'total 'comp `(agg ,_))) #t]
         [else #f]))

;; Given a rel-version, returns if it is a db rel-version
(define (db-rel-version? r)
  (match r
    [`(rel-version ,name ,arity ,sel ,ver)
     (or (equal? ver 'total) (equal? ver 'delta))]))

; A canonical index must include all the columns from 1 to N and not include 0
(define (canonical-index? l arity)
  (equal? l (range 1 (add1 arity))))

; Strips out any/all prov information in an s-expr
(define (strip-prov e)
  (match e
         [(? set?)
          (list->set (map strip-prov (set->list e)))]
         [`(prov ,e0 ,pos)
          (strip-prov e0)]
         [(? hash?)
          (foldl (lambda (k h)
                   (hash-set h (strip-prov k) (strip-prov (hash-ref e k))))
                 (hash)
                 (hash-keys e))]
         [(? list?)
          (map strip-prov e)]
         [(cons x y) (cons (strip-prov x) (strip-prov y))]
         [else e]))

(define (map-prov e f)
  (match e
         [(? set?)
          (list->set (map (app map-prov _ f) (set->list e)))]
         [`(prov ,e0 ,pos)
          `(prov ,(map-prov e0 f) ,(f pos))]
         [(? hash?)
          (foldl (lambda (k h)
                   (hash-set h (map-prov k f) (map-prov (hash-ref e k) f)))
                 (hash)
                 (hash-keys e))]
         [(? list?)
          (map (app map-prov _ f) e)]
         [else e]))

(define (simplify-prov e [replacement '_])
  (map-prov e (λ (x) replacement)))
(struct syntax-error (syntax msg))

(define (assert-pred-syntax pred syntax)
  (match syntax
    [`(prov ,_ ,(? pos? pos))
      (when (not (pred syntax))
          (raise (syntax-error syntax (format "Assertion failed: ~a" (object-name pred)))))]
    [else (assert-pred pred syntax)]))

(define ((prov-of pred) x)
  (match x
    [`(prov ,item ,pos) (pred item)]
    [else #f]))


(define (backend-input-ir-relation-decl? e)
  (match e
    [`(relation-decl ,name ,arity ,canonical) #t]
    [else #f]))

(define (backend-input-ir-scc-order? e)
  (match e
    [`((,from ,to) ...) #t]
    [else #f]))

(define (backend-input-ir? e)
  (match e
    [`(slog-prog ,(? backend-input-ir-relation-decl? rel-decl)

       ,(? backend-input-ir-scc-order?)) #t]
    [else #f]))
