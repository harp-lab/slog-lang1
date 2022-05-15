#lang racket

;; Slog compilation pass -- splits relations into distinct select-sets (the set of join columns)
;; Copyright (c) Thomas Gilray, et al, see License.md

;; This pass breaks up relations into one version per unique set of join columns

(provide split-selections-pass)

(require racket/hash)
(require "partitioning-pass.rkt")
(require "lang-predicates.rkt")
(require "builtins.rkt")
(require "utils.rkt")
(require "slog-params.rkt")

(define/contract-cond (split-selections-pass ir)
  (-> ir-small? ir-select?)
  (match-define `(ir-small (ir-fixed (ir-flat ,source-tree ,_ ...) ,_ ...) ,rules-h ,comp-rules-h) ir)
  (define comp-rules (hash-keys comp-rules-h))
  (define facts (source-tree-facts source-tree))
  (define facts-rel-aritys (foldl set-union (set) (map source-tree-fact->rel-aritys (set->list facts))))

  (define aggregated-rel-aritys
    (flat-map rule-get-aggregated-rel-aritys (hash-keys rules-h)))

  ; (printf "aggregated-rel-aritys: ~a\n" aggregated-rel-aritys)
  (define rel-h0 (foldl (λ (rel-arity accu) (hash-set accu rel-arity (set)))
                        (hash)
                        (append (set->list facts-rel-aritys) aggregated-rel-aritys)))

  ; Call split-select-rule on every rule, extrating a rel-h
  (match-define (cons rules-h+ rel-h)
    (foldl (lambda (rule acc)
              (match-define (cons rules-h+ rel-h) acc)
              (match (hash-ref rules-h rule)
                    [`(rule-prov ,ir-fixed ,fixed-rule ,module ,source-id)
                      (match-define (cons rule+ rel-h+)
                                    (split-select-rule rule rel-h comp-rules))
                      ; (match-define (cons rule++ rel-h++)
                      ;               (pick-select-aggregated-rel rule+ rel-h+))
                      (cons (hash-set rules-h+ rule+ `(rule-prov ir-small ,rule ,module ,source-id))
                            rel-h+)])) 
            (cons (hash) rel-h0)
            (hash-keys rules-h)))
  
  ; Ensure relations only used in unary rules have a select set
  (define rel-h+ 
    (foldl (lambda (rule rel-h+)
            (match rule
              [`(srule ,(and headclause
                            `(prov ((prov ,(? rel-arity? hrel) ,hrelpos)
                                    (prov ,hargs ,hargposs)
                                    ...)
                                    ,headpos))
                      (prov ((prov = ,=0pos)
                              (prov ,b0x ,b0xpos)
                              (prov ((prov (rel-arity ,b0rel ,b0arity db) ,b0relpos)
                                    (prov ,b0args ,b0argposs)
                                    ...)
                                    ,body0clpos))
                            ,body0pos))
              (define existing-rel-index (hash-ref rel-h+ `(rel-arity ,b0rel ,b0arity db) #f))
              (cond
                [existing-rel-index rel-h+]
                [else 
                  (define b0-fixed-args (filter (lambda (i) (lit? (list-ref b0args i))) (range b0arity)))
                  (define fixed-args-index (map add1 b0-fixed-args))
                  (define default-index (range 1 (+ 1 b0arity)))
                  (extend-rel-h rel-h+ `(rel-arity ,b0rel ,b0arity db) default-index)])]
              [else rel-h+]))
          rel-h
          (hash-keys rules-h+)))
  
  ; Ensure relations with no body clauses are assigned the default selection
  (define rel-h++ 
    (foldl (lambda (rule rel-h+)
            (match rule
                    [`(srule ,(and headclause
                                  `(prov ((prov ,(? rel-arity? hrelarity) ,hrelpos)
                                          (prov ,hargs ,hargposs)
                                          ...)
                                          ,headpos))
                            ,rest ...)
                    (if (or (hash-has-key? rel-h+ hrelarity) (comp-or-agg-rel-arity? hrelarity))
                        rel-h+
                        (hash-set rel-h+ hrelarity (set)))]
                    [else rel-h+]))
           rel-h+
           (hash-keys rules-h+)))
  
  ; (printf "rel-h++: ~a\n" (pretty-format rel-h++))
  ; Pick a canonical select-set for all relations
  (define rel-h+++ 
    (foldl (lambda (r h)
            (match-define `(rel-arity ,rel-name ,arity db) r)
            (define default-index (range 1 (+ 1 arity)))
            (define candidate-indices (filter (app canonical-index? _ arity) (set->list (hash-ref rel-h++ r (set)))))
            (hash-set h r (cons default-index (set-add (hash-ref rel-h++ r) default-index))))
           (hash)
           (hash-keys rel-h++)))

  ; (printf "rel-h++: ~a\n" (pretty-format rel-h++))
  ;; fixup unary rules and aggregated rels
  (match-define (cons rules-h++ rel-h++++) 
    (foldl (λ (rule h+rel-h)
              (match-define (cons h rel-h) h+rel-h)
              (match-define (cons rule* rel-h*) (pick-select-unary-rule rule rel-h comp-rules))
              ; (printf "rel-h*: ~a\n" (pretty-format rel-h*))
              (match-define (cons rule** rel-h**)
                                    (pick-select-aggregated-rel rule* rel-h*))
              (cons (hash-set h rule** (hash-ref rules-h+ rule)) rel-h**))
           (cons (hash) rel-h+++)
           (hash-keys rules-h+)))

  ; Fixup heads to use the canonical select-set
  (define rules-h+++ 
    (foldl (lambda (rule h)
              (hash-set h (pick-select-rule rule rel-h++++) (hash-ref rules-h++ rule)))
           (hash)
           (hash-keys rules-h++)))
  ; Add synthetic propagation rules from 
  (define rules-h++++ 
    (foldl 
      (lambda (rel rules-h+)
        (match-define `(rel-arity ,relname ,relarity db) rel)
        (match-define (cons csel allsel) (hash-ref rel-h++++ rel))
        (define args0 (map (lambda (i) (string->symbol (string-append "a" (number->string i))))
                          (range (if (number? relarity) relarity 2))))
        (define args (cons 'ident args0))
        (define cargs (project-then-rest args csel))
        (foldl (lambda (ncsel rules-h+)
                 
                 (define ncargs (project-then-rest args ncsel))
                 (hash-set rules-h+
                            `(arule (prov ((prov (rel-select ,relname ,relarity ,ncsel db)
                                                synthetic)
                                          ,@(map (lambda (a) `(prov ,a synthetic)) ncargs))
                                          synthetic)
                                    (prov ((prov (rel-select ,relname ,relarity ,csel db)
                                                synthetic)
                                          ,@(map (lambda (a) `(prov ,a synthetic)) cargs))
                                          synthetic))
                            `(rule-prov intra-relation ,relname ,relarity)))
                rules-h+
                (set->list (set-remove allsel csel))))
          rules-h+++
          (filter db-rel-arity? (hash-keys rel-h++++))))
  
  (define all-comp-rel-selects0
    (remove-duplicates
      (filter-map 
        (λ (cl) 
          (match-define `(prov ((prov ,rel-select ,rel-pos) ,args ...) ,pos) cl)
          (if (equal? (rel-select->kind rel-select) 'comp) rel-select #f))
        (flat-map
          (λ (rule)
            (match rule
              [`(srule ,head ,bodys ...) bodys]
              [`(arule ,_ ,_) (list)]))
          (hash-keys rules-h++++)))))

  (define (ir-select-comp-rules-h-for-comp-rels-step inp)
    (match-define (cons comp-rel-selects-set comp-rel-select->rules-h) inp)
    (define (comp-rule-body-comp-rels crule)
      (match-define `(crule ,head ,bodys ...) crule)
      (map (λ (body)
           (match-define `(prov ((prov ,rel ,_) ,args ...) ,_) body)
           rel)
          bodys))
    (define new-ir-select-comp-rules-h
      (foldl (λ (comp-rel-select accu-hash)
            (cond 
              [(hash-has-key? accu-hash comp-rel-select) accu-hash]
              [else
                (match-define `(rel-select ,name ,arity ,indices comp) comp-rel-select)
                (define rules (get-ir-select-rules-for-comp-rel `(rel-arity ,name ,arity comp) indices comp-rules-h))
                (hash-set accu-hash comp-rel-select rules)]))
           comp-rel-select->rules-h
           (set->list comp-rel-selects-set) ))
    (define new-comp-rel-selcts 
      (flat-map comp-rule-body-comp-rels (flat-map hash-keys (hash-values comp-rel-select->rules-h))))
    (define new-comp-rel-selects-set (set-union (list->set new-comp-rel-selcts) comp-rel-selects-set))
    (cons new-comp-rel-selects-set new-ir-select-comp-rules-h))
    
  (match-define (cons all-comp-rel-selects-set all-comp-rel-select->rules-h)
    (iterate-to-fixed-point ir-select-comp-rules-h-for-comp-rels-step (cons (list->set all-comp-rel-selects0) (hash))))

  (define comp-rules-h+ (foldl hash-union (hash) (hash-values all-comp-rel-select->rules-h)))

  (for ([crule (hash-keys comp-rules-h+)]) (assert-pred ir-select-comp-rule? crule))
  ; (printf "rel-h++++: ~a\n" rel-h++++)

  `(ir-select ,ir ,rel-h++++ ,rules-h++++ ,comp-rules-h+))

(define (extract-selects args poss cxs-lst)
    (define cxs-set (list->set cxs-lst))
    (define (cxs-or-const? x) (or (lit? x) (set-member? cxs-set x)))
    (define cxs-selects0 (flat-map (λ (i) (indexes-of args i)) cxs-lst))
    (define rest-selects0 (filter (λ (i) (not (member i cxs-selects0))) (range (length args))))
    (match-define (cons const-selects rest-selects) 
      (foldl (λ (i accu)
                (match-define (cons s-const s-rest) accu)
                (if (lit? (list-ref args i))
                    (cons (append s-const (list i)) s-rest)
                    (cons s-const (append s-rest (list i)))))
             (cons '() '())
             rest-selects0))
    (define cxs-selects (append cxs-selects0 const-selects))
    (define selects (append cxs-selects rest-selects))
    (define sel-args (map (λ (i) `(prov ,(list-ref args i) ,(list-ref poss i))) cxs-selects))
    (define other-args (map (λ (i) `(prov ,(list-ref args i) ,(list-ref poss i))) rest-selects))
    (list cxs-selects sel-args other-args))

(define (extend-rel-h rel-h rel-arity sel)
    (hash-set rel-h rel-arity (set-add (hash-ref rel-h rel-arity set) sel)))

;; works on a rel-h with canonical indices
(define (extend-rel-hc rel-h rel-arity sel)
    (match-define (cons canonical-index indices) (hash-ref rel-h rel-arity))
    (hash-set rel-h rel-arity (cons canonical-index (set-add indices sel))))

; Pass makes body relations specific to their select sets, extends the relation map
(define (split-select-rule rule rel-h comp-rules)
  (match rule
         [`(srule ,(and headclause
                        `(prov ((prov ,(? rel-arity? hrel) ,hrelpos)
                                (prov ,hargs ,hargposs)
                                ...)
                               ,headpos))
                  (prov ((prov = ,=0pos)
                         (prov ,b0x ,b0xpos)
                         (prov ((prov (rel-arity ,b0rel ,b0arity ,b0kind) ,b0relpos)
                                (prov ,b0args ,b0argposs)
                                ...)
                               ,body0clpos))
                        ,body0pos)
                  (prov ((prov = ,=1pos)
                         (prov ,b1x ,b1xpos)
                         (prov ((prov (rel-arity ,b1rel ,b1arity ,b1kind) ,b1relpos)
                                (prov ,b1args ,b1argposs) 
                                ...)
                               ,body1clpos))
                        ,body1pos))
          (define cxs (set-intersect (filter var? (cons b0x b0args))
                                     (filter var? (cons b1x b1args))))
          (match-define (cons independent0 dependent0)  (if (< (length (filter (not/c lit?) (cons b0x b0args))) 
                                                               (length (filter (not/c lit?) (cons b1x b1args))))
                                                            (cons 'b0 'b1) (cons 'b1 'b0)))
          (match-define (list independent-rel-name independent-rel-arity independent-rel-kind 
                                dependent-rel-name   dependent-rel-arity   dependent-rel-kind) 
            (match independent0
              ['b0 (list b0rel b0arity b0kind b1rel b1arity b1kind)]
              ['b1 (list b1rel b1arity b1kind b0rel b0arity b0kind)]))
          (define independent-rel-selection (if (equal? independent0 'b0)
                                                (first (extract-selects (cons b0x b0args) (cons b0xpos b0argposs) (list)))
                                                (first (extract-selects (cons b1x b1args) (cons b1xpos b1argposs) (list)))))
          (define who-is-independent 
            (if (and (comp-or-agg-rel-kind? independent-rel-kind)
                     (or (not (comp-or-agg-rel-kind? dependent-rel-kind))
                         (not (admissible-comp-rel-indices? `(rel-arity ,independent-rel-name ,independent-rel-arity comp) (list->set independent-rel-selection) comp-rules))))
                dependent0
                independent0))
          (define cxs-sorted (filter (app set-member? cxs _) (if (equal? who-is-independent 'b0) (cons b0x b0args) (cons b1x b1args))))
          (match-define (list b0sel b0selxs b0oxs)
            (extract-selects (cons b0x b0args) 
                             (cons b0xpos b0argposs) 
                             (if (and (equal? who-is-independent 'b0) (comp-or-agg-rel-kind? b0kind)) (set) cxs-sorted)))
          (match-define (list b1sel b1selxs b1oxs)
            (extract-selects (cons b1x b1args) 
                             (cons b1xpos b1argposs) 
                             (if (and (equal? who-is-independent 'b1) (comp-or-agg-rel-kind? b1kind)) (set) cxs-sorted)))
          (define res-b0
            `(prov ((prov (rel-select ,b0rel ,b0arity ,b0sel ,b0kind) ,b0relpos)
                               ,@b0selxs ,@b0oxs)
                              ,body0pos))
          (define res-b1
            `(prov ((prov (rel-select ,b1rel ,b1arity ,b1sel ,b1kind) ,b1relpos)
                               ,@b1selxs ,@b1oxs)
                              ,body1pos))
          (define rel-h+ (if (db-rel-kind? b1kind) 
                            (extend-rel-h rel-h
                                            `(rel-arity ,b1rel ,b1arity ,b1kind)
                                            b1sel)
                            rel-h))
          (define rel-h++ (if (db-rel-kind? b0kind)
                          (extend-rel-h rel-h+
                              `(rel-arity ,b0rel ,b0arity ,b0kind)
                              b0sel)
                          rel-h+))
          (cons `(srule ,headclause 
                        ,@(if (equal? who-is-independent 'b0) 
                              (list res-b0 res-b1)
                              (list res-b1 res-b0)))
                rel-h++)]
         [else (cons rule rel-h)]))

(define (pick-select-unary-rule rule rel-h comp-rules)

  (define (extend-rel-h′ rel-h rel-arity sel)
    (match-define (cons canonical-index select-sets) (hash-ref rel-h rel-arity))
    (hash-set rel-h rel-arity (cons canonical-index (set-add select-sets sel))))
  
  (match rule
    [`(srule  ,headclause
              (prov ((prov = ,=0pos)
                      (prov ,b0x ,b0xpos)
                      (prov ((prov (rel-arity ,b0rel ,b0arity ,b0kind) ,b0relpos)
                            (prov ,b0args ,b0argposs)
                            ...)
                            ,body0clpos))
                    ,body0pos))
      (match-define (cons b0sel b0args+) (cond
        [(and (not (ormap lit? b0args)) (db-rel-kind? b0kind))
          (match-define (cons csel sel-st) (hash-ref rel-h `(rel-arity ,b0rel ,b0arity ,b0kind) (lambda () (error 'err0))))
          (define args+ (project-then-rest (map (λ (arg pos) `(prov ,arg ,pos)) (cons b0x b0args) (cons b0xpos b0argposs)) csel))
          (cons csel args+)]
        [else
          (match-define (list b0sel b0selxs b0oxs)
            (extract-selects (cons b0x b0args) (cons b0xpos b0argposs) (list)))
          (cons b0sel (append b0selxs b0oxs))]))
      (define rel-h+ (cond 
        [(db-rel-kind? b0kind) (extend-rel-h′ rel-h `(rel-arity ,b0rel ,b0arity ,b0kind) b0sel)]
        [else rel-h]))
      (cons `(srule ,headclause
                        (prov ((prov (rel-select ,b0rel ,b0arity ,b0sel ,b0kind) ,b0relpos)
                               ,@b0args+)
                              ,body0pos))
            rel-h+)]
    [else (cons rule rel-h)]))


; Pass picks the canonical select-set for heads
(define (pick-select-rule rule rel-h)
  (match rule
    [`(srule (prov ((prov (rel-arity ,hrel ,harity db) ,hrelpos)
                    ,hargs ...)
                  ,headpos)
            ,bodies ...)
      (match-define (cons csel sel-st) (hash-ref rel-h `(rel-arity ,hrel ,harity db) (lambda () (error 'err0))))
      (define hargs+ (project-then-rest hargs (map sub1 csel)))
      `(srule (prov ((prov (rel-select ,hrel ,harity ,csel db) ,hrelpos)
                      ,@hargs+)
                    ,headpos)
              ,@bodies)]
    [else (error (format "Bad partially updated srule: ~a" rule))]))


(define/contract (source-tree-fact->rel-aritys f)
  (source-tree-fact? . -> . (set/c rel-arity?))
  (match f
    [`(prov ((prov ,tag ,tagpos) ,args ...) ,pos)
      (define rel-arity `(rel-arity ,tag ,(length args) db))
      (define sub-rel-aritys (foldl set-union (set) (map source-tree-fact->rel-aritys (filter source-tree-fact? args))))
      (set-add sub-rel-aritys rel-arity)]
    [else (set)]))

(define (source-tree-facts source-tree)
  (foldl set-union
         (set)
         (map Module-facts (hash-values source-tree))))

(define/contract (get-ir-select-rules-for-comp-rel comp-rel-arity indices comp-rules-h)
  (rel-arity? (listof number?) hash? . -> . hash?)
  (define (get-ir-select-rule-for-comp-rule indices comp-rule)
    (match-define `(crule ,head ,bodys ...) comp-rule)
    (match-define (list _ `(prov (rel-arity ,rel-name ,rel-arity comp) ,hrelpos) args) (clause-rel-args-w/prov head))
    (define initial-grounded-vars (filter var? (map (λ (i) (strip-prov (list-ref args i))) (map sub1 indices))))
    (define comp-rules (hash-keys comp-rules-h))
    (match-define (list ir-select-bodys _ _)
      (foldl
        (λ (iter accu)
          (match-define (list ir-select-body-cls remainig-body-cls grounded-vars) accu)
          (assert (equal? iter (length ir-select-body-cls)))
          (define next 
            (findf-map 
              (λ (rem-body-cl)
                  (assert ir-small-clause? rem-body-cl)
                  (match-define (list _ bcl-rel-arity bcl-args) (clause-rel-args rem-body-cl))
                  (define cl-grounded-vars (filter (λ (arg) (or (lit? arg) (set-member? grounded-vars arg))) 
                                                   (map strip-prov bcl-args)))
                  ; (printf "cl-grounded-vars for rem-body-cl ~a: ~a\n" (strip-prov rem-body-cl) cl-grounded-vars)
                  (define indices (filter-map (λ (arg ind)
                                              (if (or (lit? arg) (set-member? grounded-vars arg))
                                                  ind
                                                  #f))
                                              (map strip-prov bcl-args)
                                              (range 1 (add1 (length bcl-args)))))
                  (assert ((listof number?) indices))
                  (if (admissible-comp-rel-indices? bcl-rel-arity (list->set indices) comp-rules)
                      (cons rem-body-cl indices)
                      #f))
              remainig-body-cls))
          (assert next (format "get-ir-select-rules-for-comp-rel: bad indices: ~a for comp-rule: ~a" indices (strip-prov comp-rule)))
          (match-define (cons body-cl body-cl-indices) next)
          (assert ((listof number?) body-cl-indices) (format "bad indices: ~a" body-cl-indices))
          (match-define (list _ `(prov (rel-arity ,rel-name ,rel-arity comp) ,relpos) args) (clause-rel-args-w/prov body-cl))
          (define body-cl-indices-lst (set->list body-cl-indices))
          (define args-reordered (project-then-rest (cons `(prov _ ,(prov->pos body-cl)) args) body-cl-indices-lst))
          (define ir-select-bcl 
            `(prov ((prov (rel-select ,rel-name ,rel-arity ,body-cl-indices-lst comp) ,relpos) ,@args-reordered) ,(prov->pos body-cl)))
          (assert (ir-select-clause? ir-select-bcl) (format "~a" (simplify-prov ir-select-bcl)))
          (define new-grounded-vars (filter var? (map strip-prov args)))
          (list (append ir-select-body-cls (list ir-select-bcl))
                (remove body-cl remainig-body-cls)
                (set-union grounded-vars (list->set new-grounded-vars))))
        (list '() bodys (list->set initial-grounded-vars))
        (range 0 (length bodys))))
    (define args-reordered (project-then-rest (cons `(prov _ ,(prov->pos head)) args) indices))
    (define crule-head `(prov ((prov (rel-select ,rel-name ,rel-arity ,indices comp) ,hrelpos) ,@args-reordered) ,(prov->pos head)))
    (assert (ir-select-clause? crule-head) (format "~a" (simplify-prov crule-head)))
    `(crule ,crule-head  ,@ir-select-bodys))
  (match-define `(rel-arity ,comp-rel-name ,arity comp) comp-rel-arity)
  (cond 
    [(builtin? comp-rel-name) (hash)]
    [else
      (make-immutable-hash
        (flat-map 
          (λ (comp-rule+prov)
              (match-define (cons comp-rule prov) comp-rule+prov)
              (match-define `(crule ,head ,bodys ...) comp-rule)
              (match-define (list _ h-rel-arity h-args) (clause-rel-args head))
              (cond
                [(equal? comp-rel-arity h-rel-arity)
                  (list (cons (get-ir-select-rule-for-comp-rule indices comp-rule) prov))]
                [else (list)]))
          (hash->list comp-rules-h)))]))

(define (rule-get-aggregated-rel-aritys rule)
  (define (clause-get-aggregated-rel-aritys cl)
    (match-define (list id rel args) (clause-rel-args-w/prov cl))
    (match-define `(prov (rel-arity ,relname ,arity ,kind) ,pos) rel)
    (match kind
      [`(agg ,agg-rel) (list (strip-prov agg-rel))]
      [else (list)]))
  (match rule
    [`(srule ,head ,bodys ...) (flat-map clause-get-aggregated-rel-aritys bodys)]
    [`(arule ,head ,body) (list)]))

(define (pick-select-aggregated-rel rule rel-h)
  (define (clause-pick-select-aggregated-rel cl)
    (match-define `(prov ((prov ,rel ,relpos) ,args ...) ,pos) cl)
    (match-define `(rel-select ,rel-name ,rel-arity ,rel-indices ,rel-kind) rel)
    (match rel-kind
      [`(agg ,aggregated-rel) #:when (equal? rel-name '~)
        (define aggregated-rel-select0 (get-aggregated-rel-select-for-partial-agg-rel-select (strip-prov rel)))
        (match-define `(rel-select ,neg-rel ,neg-rel-arity ,neg-rel-indices db) aggregated-rel-select0)
        (define aggregated-rel-select `(rel-select ,neg-rel ,neg-rel-arity ,rel-indices db))
        (define new-rel-select `(rel-select ~ ,rel-arity ,(range 1 (add1 rel-arity)) 
                                 (agg (prov ,aggregated-rel-select ,(prov->pos aggregated-rel)))))
        (assert (rel-select? new-rel-select) (format "~a" new-rel-select))
        (cons `(prov ((prov ,new-rel-select ,relpos) ,@args) ,pos) (set aggregated-rel-select))]
      [`(agg ,aggregated-rel) 
        (define aggregated-rel-select (get-aggregated-rel-select-for-partial-agg-rel-select (strip-prov rel)))
        (define new-rel-select `(rel-select ,rel-name ,rel-arity ,rel-indices (agg (prov ,aggregated-rel-select ,(prov->pos aggregated-rel)))))
        (cons `(prov ((prov ,new-rel-select ,relpos) ,@args) ,pos) (set aggregated-rel-select))]
      [else (cons cl (set))]))
  (match rule
    [`(srule ,head ,bodys ...) 
     (define updated-bodys+new-rel-selects (map clause-pick-select-aggregated-rel bodys))
     (define updated-bodys (map car updated-bodys+new-rel-selects))
     (define new-rel-selects (foldl set-union (set) (map cdr updated-bodys+new-rel-selects)))
     (define new-rel-h (foldl (λ (rel-sel rel-h) (extend-rel-hc-with-rel-select rel-h rel-sel)) rel-h (set->list new-rel-selects)))
     (cons `(srule ,head ,@updated-bodys) new-rel-h)]
    [`(arule ,head ,body) (cons rule rel-h)]))

(define (extend-rel-h-with-rel-select rel-h new-rel-select)
  ; (printf "extend-rel-h-with-rel-select input: rel-h:~a; new-rel-select: ~a\n" rel-h new-rel-select)
  (match-define `(rel-select ,name ,arity ,ind db) new-rel-select)
  (define rel-arity `(rel-arity ,name ,arity db))
  (extend-rel-h rel-h rel-arity ind))

(define (extend-rel-hc-with-rel-select rel-h new-rel-select)
  ; (printf "extend-rel-hc-with-rel-select input: rel-h:~a; new-rel-select: ~a\n" rel-h new-rel-select)
  (match-define `(rel-select ,name ,arity ,ind db) new-rel-select)
  (define rel-arity `(rel-arity ,name ,arity db))
  (extend-rel-hc rel-h rel-arity ind))

(define (get-aggregated-rel-select-for-partial-agg-rel-select rel-select)
  (match-define `(rel-select ,rel-name ,rel-arity ,rel-indices (agg ,aggregated-rel)) rel-select)
  (match-define `(rel-arity ,aggregated-rel-name ,aggregated-rel-arity db) aggregated-rel)
  (define matching-aggregators
    (filter-map 
      (λ (agg-spec)
        (match-define `(aggregator-spec ,spec-name ,spec-arity ,spec-ind ,spec-aggregated-rel-arity ,spec-aggregated-rel-indices) agg-spec)
        (cond
          [(and (equal? spec-name rel-name) (equal? spec-arity rel-arity) (equal? aggregated-rel-arity spec-aggregated-rel-arity))
            `(rel-select ,aggregated-rel-name ,aggregated-rel-arity ,spec-aggregated-rel-indices db)]
          [else #f]))
      (hash-keys all-aggregators)))
  (car matching-aggregators))

