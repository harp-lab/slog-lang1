#lang racket

;; Slog compilation pass -- removes all splicing variables and fixes (and annotates) arities
;; Copyright (c) Thomas Gilray, et al, see License.md

(provide fix-arities-pass)

(require "lang-predicates.rkt")
(require "utils.rkt")
(require "builtins.rkt")
(require "lang-utils.rkt")

(define/contract (fix-arities-pass ir)
  (-> ir-flat? ir-fixed?)
  (match-define `(ir-flat ,source-tree ,rules-h ,comp-rules-h) ir)
  (define comp-rules (hash-keys comp-rules-h))
  `(ir-fixed
    ,ir
    ,(foldl (lambda (rule h)
              (match (hash-ref rules-h rule)
                      [`(rule-prov source-tree ,module ,source-id)
                      (define rules-st (fix-arities-rule rule comp-rules))
                      (foldl (lambda (r h)
                                (hash-set h r `(rule-prov ir-flat ,rule ,module ,source-id)))
                              h
                              (set->list rules-st))]))
            (hash)
            (hash-keys rules-h))
    ,(foldl (λ (rule h)
                (match-define `(rule-prov source-tree ,module ,source-id) (hash-ref comp-rules-h rule))
                (define new-prov `(rule-prov ir-flat ,rule ,module ,source-id))
                (hash-set h (comp-rule-fix-arities rule comp-rules) new-prov))
            (hash)
            (hash-keys comp-rules-h))))

(define (fix-arities-rule rule comp-rules)
  (match rule
         [`(rule ,heads ,bodys)
          (set
           `(rule ,(list->set (map (app fix-simple-clause _ comp-rules) (set->list heads)))
                  ,(list->set (map (app fix-simple-clause _ comp-rules) (set->list bodys)))))]))

(define (comp-rule-fix-arities rule comp-rules)
  (match-define `(crule ,head ,bodys ...) rule)
  `(crule ,(fix-simple-clause head comp-rules) ,@(map (app fix-simple-clause _ comp-rules) bodys)))

(define (fix-simple-clause cl comp-rules)
  (match-define `(prov ((prov = ,=pos)
                  (prov ,(? var? x) ,xpos)
                  (prov (,(? ir-flat-rel? rel) ,args ...) ,iclpos))
                 ,clpos) cl)
         
  (define rel-arity (match rel
    [`(prov ,(? rel-name? r) ,rpos) 
      (define kind (cond
        [(comp-rel-name? r comp-rules) 'comp]
        [else 'db]))
      (define rel-arity `(rel-arity ,r ,(length args) ,kind))
      (when (builtin? r)
        (define exists (hash-ref all-builtin-rel-arity->rel-selects rel-arity #f))
        (when (not exists)
          (pretty-error-current 
            rpos 
            (format "Wrong arity (~a) for builtin ~a. Acceptable arities: ~a" 
                    (length args) 
                    r
                    (intercalate ", " (arities-for-builtin r))) #:exit #t)))
      `(prov ,rel-arity ,rpos)]
    [`(agg (prov ,aggregator ,aggregator-pos) (prov ,rel ,rel-pos))
      (define arity-of-rel (rel-arity-for-aggregator-arity aggregator (length args)))
      `(prov (rel-arity ,aggregator ,(length args) 
                        (agg (prov (rel-arity ,rel ,arity-of-rel db) ,rel-pos))) ,aggregator-pos)]))
          
  `(prov ((prov = ,=pos)
          (prov ,x ,xpos)
          (prov (,rel-arity
                  ,@args)
                ,iclpos))
          ,clpos))

(define (_comp-rel-name? rel-name comp-rules)
  (assert (symbol? rel-name))
  (or (builtin? rel-name)
      (ormap (λ (comp-rule)
                (match-define `(crule ,head ,bodys ...) comp-rule)
                (match-define (list _ name args) (ir-flat-clause-rel-args head))
                (equal? name rel-name))
             comp-rules)))
(define comp-rel-name? (memoize _comp-rel-name?))

(define (rel-arity-for-aggregator-arity aggregator-name arity)

  (define matching (filter-map
    (λ (agg-spec)
      (match-define `(aggregator-spec ,spec-name ,spec-arity ,indices ,rel-arity ,rel-indices) agg-spec)
      (cond
        [(equal? (cons aggregator-name arity) (cons spec-name spec-arity))
          rel-arity]
        [else #f]))
    (hash-keys all-aggregators)))
  (assert (equal? (length (remove-duplicates matching)) 1) (format "multiple candidate arities for aggregator ~a with arity ~a" aggregator-name arity))
  (car matching))