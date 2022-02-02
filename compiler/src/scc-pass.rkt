#lang racket

;; Slog compilation pass -- Splits a slog program into a DAG of SCCs
;; Copyright (c) Kristopher Micinski, Thomas Gilray, et al, see License.md

;; This pass computes strongly connected components of an influences graph
;; of the rules and relations (a rule influences a relation in its head and is
;; influenced by all relations in its body). Each relation has at most one
;; SCC in-which it is dynamic (may be updated), in all others it must be static.

(provide scc-pass)

(require "lang-predicates.rkt")
(require "utils.rkt")
(require "graphs.rkt")
(require "slog-params.rkt")

(define/contract-cond (scc-pass ir)
  (-> ir-select? ir-scc?)
  (match-define `(ir-select ,ir-old ,rel-h ,rules-h ,comp-rules-h) ir)

  (define rules-head-body-rel-selects
    (map (λ (rule)
          (let ([head-rel-select
                  (strip-prov (rule-head-rel-select-w/prov rule))]
                [body-rel-selects
                  (match rule
                    [`(srule ,head
                            (prov ((prov ,rel-selects ,_) ,_ ...) ,_) ...)
                      (foldl set-union (set) 
                            (map (λ (rel-sel) (set-union (set rel-sel) (list->set (rel-select-aggregated-rel rel-sel)))) 
                                rel-selects))]
                    [`(arule ,_    ; tom added
                            (prov ((prov ,rel-select0 ,_) ,_ ...) ,_))
                      (set rel-select0)])])
                  (list rule head-rel-select body-rel-selects)))
         (hash-keys rules-h)))
  ;; a hash from all rel-selects to rules with those rel-selects in their heads
  (define head-rel-select->rules
    (foldl (λ (rule-h-b accu)
              (match-define (list rule head _) rule-h-b)
              (hash-set accu head (set-add (hash-ref accu head set) rule)))
           (hash)
           rules-head-body-rel-selects))
  (define rule-graph
    (foldl 
      (λ (rule-h-b accu)
          (match-define (list rule _ body-rel-selects) rule-h-b)
          (foldl (λ (body-rel-select accu)
                    (foldl (λ (rule-with-head-rel-select accu)
                      (hash-set accu rule-with-head-rel-select (set-add (hash-ref accu rule-with-head-rel-select) rule)))
                      accu
                      (set->list (hash-ref head-rel-select->rules body-rel-select set))))
                  accu
                  (set->list body-rel-selects)))
      (foldl (λ (rule-h-b h) (hash-set h (car rule-h-b) (set))) (hash) rules-head-body-rel-selects)
      rules-head-body-rel-selects))
  

  (define scc-graph (calculate-sccs rule-graph))

  (for ([scc (hash-keys scc-graph)])
    (verify-aggregators-in-scc (set->list scc)))
  

  (define (set-add* st . values)
    (set-union (apply set values) st))

  ;; An SCC is marked as nonlooping when it (a) has only one
  ;; rule and (b) that rule is not recursive.
  (define (looping? scc)
    (if (= (length (set->list scc)) 1)
        (let* ([rule (set-first scc)]
                [head (strip-prov (rule-head-rel-select-w/prov rule))]
                [bodies (list->set (map strip-prov (rule-body-rel-selects-w/prov rule)))])
          (if (set-member? bodies head)
                'looping
                'nonlooping))
        'looping))
  
  ;; Walk over the SCC graph (currently filled up with raw
  ;; rules) and canonicalize the rules to an ID.
  ;; IDs are guaranteed to be in sorted order.
  (define scc->id
    (foldl (lambda (scc id scc->id) 
            (hash-set scc->id scc id))
            (hash)
            (topological-sort scc-graph)
            (range 0 (hash-count scc-graph))))
  ;; Assemble a dag of IDs by walking over the map and then
  ;; applying the map to the codomain.
  (define scc-dag 
    (foldl
      ;; For each key (an SCC) in the SCC graph
      (lambda (scc scc-dag)
        ;; Set it's key in the scc-dag
        (hash-set
        scc-dag
        (hash-ref scc->id scc)
        (list->set
          (set-map (hash-ref scc-graph scc)
                  (lambda (scc) (hash-ref scc->id scc))))))
      (hash)
      (hash-keys scc-graph)))
    
  ;; Restrict a rules-hash to an SCC
  (define (restrict rules-h scc)
    (foldl (lambda (next-rule h) (hash-set h next-rule (hash-ref rules-h next-rule)))
            (hash)
            (set->list scc)))
  ; (define before (current-inexact-milliseconds))
 
  (define rel-h-to-be-overridden 
    (foldl (λ (rel-arity accu)
            (match-define (cons csel sel-st) (hash-ref rel-h rel-arity))
            (hash-set accu rel-arity `(unused ,csel ,sel-st)))
      (hash)
      (hash-keys rel-h)))
  (define scc-h
    (foldl
      (lambda (scc h)
        (define scc-head-rel-arities
          (list->set (map (lambda (rule)
                (match-let* ([head-rel-select
                              (match rule
                                [`(srule (prov ((prov ,rel-select ,_) ,_ ...) ,_) ,_bodys ...)
                                  rel-select]
                                [`(arule (prov ((prov ,rel-select ,_) ,_ ...) ,_) ,_) ; tom added
                                  rel-select])]
                              [`(rel-select ,relname0 ,arity0 ,_ ,kind0)
                              head-rel-select])
                  `(rel-arity ,relname0 ,arity0 ,kind0))) 
              (set->list scc))))
        (define scc-body-rel-arities
          (list->set
            (flat-map (lambda (rule) 
                (match-let* ([body-rel-selects
                              (match rule
                                [`(srule ,_
                                          (prov ((prov ,rel-selects ,_) ,_ ...) ,_) ...)
                                  rel-selects]
                                [`(arule ,_  ; tom added
                                          (prov ((prov ,rel-select ,_) ,_ ...) ,_))
                                  (list rel-select)])])
                  (map (lambda (body-rel-select)
                            (match-let*
                                ([`(rel-select ,relname0 ,arity0 ,_ ,kind0)
                                  body-rel-select])
                                `(rel-arity ,relname0 ,arity0 ,kind0))) 
                       body-rel-selects)))
              (set->list scc))))
        (hash-set
        h
        (hash-ref scc->id scc)
        `(scc 
          ,(looping? scc)
          ,(foldl (lambda (rel-arity h)
                    (cond
                      [(db-rel-arity? rel-arity)
                        (match-define (cons csel sel-st) (hash-ref rel-h rel-arity))
                        ;; Calculate the tag for each given relation (really
                        ;; rel-arity) within an SCC.  The tag will be either
                        ;; 'dynamic, 'static, or 'unused if the relation is updated
                        ;; (used in a head), static (used in a body but not any
                        ;; heads), or unused (not used at all in this SCC).
                        (define tag (cond
                          [(set-member? scc-head-rel-arities rel-arity) 'dynamic]
                          [(set-member? scc-body-rel-arities rel-arity) 'static]
                          [else 'unused]))
                        (hash-set h rel-arity `(,tag ,csel ,sel-st))]
                      [else h]))
                  rel-h-to-be-overridden
                  (set->list (set-union scc-head-rel-arities scc-body-rel-arities)))
          ,(let ([restricted (restrict rules-h scc)])
              (foldl (lambda (rule h)
                      (hash-set h rule
                                (match (hash-ref restricted rule)
                                  [`(rule-prov ,ir-small ,sel-rule ,source-module ,source-id)
                                    `(rule-prov ir-select ,rule ,source-module ,source-id)]
                                  [`(rule-prov intra-relation ,rel ,arity)
                                    `(rule-prov intra-relation ,rel ,arity)])))
                    (hash)
                    (hash-keys restricted))))))
      (hash)
      (hash-keys scc-graph)))
  
  ; (printf "scc-pass partial ~a ms.\n" (- (current-inexact-milliseconds) before))
  
  ;; in updated-scc-h, temporary relations (ones that can be garbage collected)
  ;; are marked as such
  (define updated-scc-h
    (foldl (λ (scc-id accu)
        (match-define `(scc ,looping ,rel-h ,rules-h) (hash-ref scc-h scc-id))
        (define new-rel-h
          (foldl (λ (rel rel-h-accu)
              (match-define (and rel-arity `(rel-arity ,rel-name ,arity ,kind)) rel)
              (match-define (list use-status canonical-index indices) (hash-ref rel-h rel))
              (define (unused-after)
                (andmap (λ (scc-id2)
                          (match-define `(scc ,_ ,rel-h2 ,_) (hash-ref scc-h scc-id2))
                          (define rel-entry (hash-ref rel-h2 rel-arity #f))
                          (or (not rel-entry)
                            (match-let ([(list use-status2 _ _) rel-entry])
                              (equal? use-status2 'unused))))
                        (range (add1 scc-id) (hash-count scc-h))))
              (define deletable-status
                (cond
                  [(and (equal? use-status 'dynamic)
                        (internal-rel-name? rel-name)
                        (unused-after))
                   'deletable]
                  [else 'not-deletable]))
              (hash-set rel-h-accu rel (list use-status deletable-status canonical-index indices)))
            (hash)
            (hash-keys rel-h)))
        (hash-set accu scc-id `(scc ,looping ,new-rel-h ,rules-h)))
      (hash)
      (range 0 (hash-count scc-h))))
  
  (assert (equal? (list->set (hash-keys scc-h)) (list->set (hash-keys updated-scc-h))))
  
  `(ir-scc
    ,ir
    ,scc-dag
    ,updated-scc-h
    ,comp-rules-h))

(define/contract-cond (rel-select-aggregated-rel rel-select)
  (rel-select? . -> . (listof rel-select?))
  (map strip-prov (rel-select-aggregated-rel-w/prov rel-select)))

(define/contract-cond (rel-select-aggregated-rel-w/prov rel-select)
  (rel-select? . -> . (listof (prov-of rel-select?)))
  (match-define `(rel-select ,name ,arity ,sel ,kind) rel-select)
  (match kind
    [`(agg ,aggregated-rel-select) (list aggregated-rel-select)]
    [else (list)]))

(define (rule-head-rel-select-w/prov rule)
    (match rule
      [`(srule (prov (,rel-select ,_ ...) ,_) ,_bodys ...) rel-select]
      [`(arule (prov (,rel-select ,_ ...) ,_) ,_) rel-select]))

(define (rule-body-rel-selects-w/prov rule)
  (match rule
    [`(srule ,_ (prov (,bs ,_ ...) ,_) ...) bs]
    [`(arule ,_ (prov (,b0 ,_ ...) ,_)) (list b0)]))

(define (body-agg-rel-selects-w/prov rule)
  (flat-map rel-select-aggregated-rel-w/prov (map unwrap-prov (rule-body-rel-selects-w/prov rule))))

(define (verify-aggregators-in-scc scc-rules)
  (define all-head-rel-selects-prov (map rule-head-rel-select-w/prov scc-rules))
  (define all-body-agg-rel-selects-prov (flat-map body-agg-rel-selects-w/prov scc-rules))
  (for* ([body-agg-rel-select all-body-agg-rel-selects-prov]
         [head-rel-select all-head-rel-selects-prov])
    (match-define `(rel-select ,h-name ,h-arity ,h-sel db) (strip-prov head-rel-select))
    (match-define `(rel-select ,agg-name ,agg-arity ,agg-sel db) (strip-prov body-agg-rel-select))
    (when (and (equal? h-name agg-name) (equal? h-arity agg-arity))
      (pretty-error-current 
        (prov->pos body-agg-rel-select)
        (format "the aggregated relation ~a cannot be stratified" h-name)
        #:exit #t))))