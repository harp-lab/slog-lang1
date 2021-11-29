#lang racket

(provide remove-implicit-joins-pass
         ir-fixed-replace-repeated-vars-in-body-clauses)

(require "lang-predicates.rkt")
(require "builtins.rkt")
(require "utils.rkt")
(require "lang-utils.rkt")
(require "slog-params.rkt")
(require "static-unification-pass.rkt")


(define/contract-cond (remove-implicit-joins-pass ir)
  (-> ir-fixed? ir-fixed?)
  ; (match-define `(ir-flat ,source-tree ,rules-h ,comp-rules-h) ir)
  (match-define `(ir-fixed ,ir-flat ,rules-h ,ir-flat-comp-rules-h) ir)
  ; (define all-comp-rels (set-union all-builtin-names (ir-flat-rules-h->head-rels comp-rules-h)))
  `(ir-fixed
    ,ir-flat
    ,(hash-map-keys (compose ir-fixed-rule-remove-silly-clauses
                             ir-fixed-replace-repeated-vars-in-body-clauses 
                             replace-constants-with-equals-clauses) 
                    rules-h)
    ,ir-flat-comp-rules-h))

(define rel->name (λ (rel) (match-define `(rel-arity ,name ,ar ,kind) (strip-prov rel)) name))

;; turns rules like [(foo x x) --> (bar x)] into [(foo x x') (= x x') --> (bar x)]
(define/contract-cond (ir-fixed-replace-repeated-vars-in-body-clauses rule)
  (ir-fixed-rule? . -> . ir-fixed-rule?)
  ; we special-case negation
  (define agg? (λ (rel) 
    (match-define `(rel-arity ,name ,arity ,kind) (strip-prov rel))
    (and (agg-rel-kind? kind) (not (equal? name '~)))))

  (define (replace-repeated-var-step cl)
    (match-define `(prov ((prov = ,=pos)
                            (prov ,(? var? x) ,xpos)
                            (prov (,(? (prov-of rel-arity?) rel)
                                   ,(and vcls `(prov ,(? arg? vs) ,vposs)) ...)
                                  ,factpos))
                           ,pos) cl)
    (define vcls-vec (list->vector vcls))
    (define repeated-var-indices 
      (call/ec (λ (return)
        (for ([i (range 0 (vector-length vcls-vec))])
          (match-define `(prov ,arg ,argpos) (vector-ref vcls-vec i))
          (define arg-i-stripped (strip-prov (vector-ref vcls-vec i)))
          (for ([j (range (add1 i) (vector-length vcls-vec))])
            (when (and (symbol? arg)
                  (equal? arg-i-stripped (strip-prov (vector-ref vcls-vec j))))
              (return (cons i j)))))
        (return #f))))
    (cond 
        ;; TODO what about comp-rels?
      [(and repeated-var-indices (not (builtin? (rel->name (strip-prov rel)))) (not (agg? rel)))
        (match-define (cons i j) repeated-var-indices)
        (match-define (cons var1 var2) (cons (vector-ref vcls-vec i) (vector-ref vcls-vec j)))
        (match-define `(prov ,var-name ,var2pos) var2)
        (define var2-replacement `(prov ,(gensymb var-name) ,var2pos))
        (define vcls+ (list-set vcls j var2-replacement))
        (define cl+ 
          `(prov ((prov = ,=pos)
                  (prov ,x ,xpos)
                  (prov (,rel
                          ,@vcls+)
                        ,factpos))
            ,pos))
        (define new-side `(prov ((prov (rel-arity = 2 comp) ,var2pos) ,var2 ,var2-replacement) ,var2pos))
        (cons cl+ (set new-side))]
      [else (cons cl (set))]))
  (define (replace-repeated-var cl)
    (match-define (cons cl+ sides) (replace-repeated-var-step cl))
    (cond 
      [(set-empty? sides) (cons cl+ sides)]
      [else (iterate-to-fixed-point 
        (λ (x)
          (match-define (cons cl sides) x)
          (match-define (cons cl+ new-sides) (replace-repeated-var-step cl))
          (cons cl+ (set-union sides new-sides)))
        (cons cl+ sides))]))
  
  (match-define `(rule ,heads ,bodys) rule)
  (match-define (cons bodys-list+ new-sides)
    (foldl (λ (cl accu)
            (match-define (cons bodys new-sides) accu)
            (match-define (cons new-cl cl-new-sides) (replace-repeated-var cl))
            (cons (cons new-cl bodys) (set-union new-sides cl-new-sides)))
      (cons '() (set))
      (set->list bodys)))
  `(rule ,heads ,(list->set (append (set-map new-sides give-clause-id) bodys-list+))))


(define/contract-cond (replace-constants-with-equals-clauses rule)
  (ir-fixed-rule? . -> . ir-fixed-rule?)
  (define (clause-constants cl)
    (match-define (list id rel args) (ir-fixed-clause-rel-args-w/prov cl))
    (filter (prov-of lit?) args))

  (define (db-clause? cl) 
    (match-define (list _ rel args) (ir-fixed-clause-rel-args cl))
    (or (db-rel-arity? rel) (equal? (rel->name rel) '~)))
  
  (define (step rule)
    (match-define `(rule ,heads ,bodys) rule)
    (define (existing-var-for-const lit) 
      (foldl (λ (cl res)
              (cond 
                [res res]
                [else
                  (match-define (list _ rel args) (ir-fixed-clause-rel-args-w/prov cl))
                  (match (cons (strip-prov rel) args)
                    [(cons '(rel-arity = 2 comp) `(,v ,c)) #:when (equal? (strip-prov c) lit) v]
                    [else #f])]))
             #f
             (set->list bodys)))

    (define all-constants (flat-map clause-constants (filter db-clause? (set->list (set-union heads bodys)))))
    (cond
      ;; this is for base facts, it is not sufficient to cover all cases of base facts.
      [(and (set-empty? bodys) (= (set-count heads) 1)) rule]
      [(empty? all-constants) rule]
      [else
        (match-define (and const-prov `(prov ,const ,const-pos)) (car all-constants))
        (define const-existing-var (existing-var-for-const const))
        (define const-var (or const-existing-var 
          `(prov ,(if (number? const) (string->symbol (format "$=~a" const)) (gensymb '$const)) ,const-pos)))
        (define equals-clause
          (give-clause-id `(prov ((prov (rel-arity = 2 comp) ,const-pos) ,const-var ,const-prov) ,const-pos)))
        (assert (ir-fixed-clause? equals-clause) (strip-prov equals-clause))
        (define (update-clause cl) 
          (match-define (list _ rel _) (ir-fixed-clause-rel-args cl))
          (if (db-clause? cl) ; TODO special case ~
              (rename-arg cl const (strip-prov const-var))
              cl))
        
        (define new-rule `(rule ,(list->set (set-map heads update-clause))
                                ,(list->set (append (set-map bodys update-clause)
                                                    (if const-existing-var (list) (list equals-clause))))))
        new-rule]))

  (iterate-to-fixed-point step rule))