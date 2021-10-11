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
  (-> ir-flat? ir-flat?)
  (match-define `(ir-flat ,source-tree ,rules-h ,comp-rules-h) ir)
  (define all-comp-rels (set-union all-builtin-names (ir-flat-rules-h->head-rels comp-rules-h)))
  `(ir-flat
    ,source-tree
    ,(hash-map-keys (compose remove-silly-clauses
                             replace-repeated-vars-in-body-clauses 
                             (app replace-constants-with-equals-clauses _ all-comp-rels)) 
                    rules-h)
    ,comp-rules-h))


(define/contract-cond (ir-fixed-replace-repeated-vars-in-body-clauses rule)
  (ir-fixed-rule? . -> . ir-fixed-rule?)
  (_replace-repeated-vars-in-body-clauses rule 'ir-fixed))

(define/contract-cond (replace-repeated-vars-in-body-clauses rule)
    (ir-flat-rule? . -> . ir-flat-rule?)
    (_replace-repeated-vars-in-body-clauses rule 'ir-flat))

;; turns rules like [(foo x x) --> (bar x)] into [(foo x x') (= x x') --> (bar x)]
(define (_replace-repeated-vars-in-body-clauses rule ir-kind)
  (define rel? (match ir-kind ['ir-flat ir-flat-rel?] 
                              ['ir-fixed (prov-of rel-arity?)]))
  (define agg? (match ir-kind ['ir-flat ir-flat-agg-rel?] 
                              ['ir-fixed (λ (rel) (agg-rel-kind? (rel-arity->kind (strip-prov rel))))]))
  (define rel->name (match ir-kind 
    ['ir-flat strip-prov] 
    ['ir-fixed (λ (rel) (match-define `(rel-arity ,name ,ar ,kind) (strip-prov rel)) name)]))
  (define eq-rel (match ir-kind
    ['ir-flat '=]
    ['ir-fixed '(rel-arity = 2 comp)]))
  (define (replace-repeated-var-step cl)
    (match-define `(prov ((prov = ,=pos)
                            (prov ,(? var? x) ,xpos)
                            (prov (,(? rel? rel)
                                   ,(and vcls `(prov ,(? arg? vs) ,vposs)) ...)
                                  ,factpos))
                           ,pos) cl)
    (define repeated-var-indices 
      (call/ec (λ (return)
        (for* ([i (range 0 (length vcls))] 
               [j (range (add1 i) (length vcls))])
          (match-define `(prov ,arg ,argpos) (list-ref vcls i))
          (when (and (symbol? arg)
                  (equal? (strip-prov (list-ref vcls i)) (strip-prov (list-ref vcls j))))
            (return (cons i j))))
        (return #f))))
    (cond 
        ;; TODO what about comp-rels?
      [(and repeated-var-indices (not (builtin? (rel->name rel))) (not (agg? rel)))
        (match-define (cons i j) repeated-var-indices)
        (match-define (cons var1 var2) (cons (list-ref vcls i) (list-ref vcls j)))
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
        (define new-side `(prov ((prov ,eq-rel ,var2pos) ,var2 ,var2-replacement) ,var2pos))
        (cons cl+ (set new-side))]
      [else (cons cl (set))]))
  (define (replace-repeated-var cl) 
    (iterate-to-fixed-point 
      (λ (x)
        (match-define (cons cl sides) x)
        (match-define (cons cl+ new-sides) (replace-repeated-var-step cl))
        (cons cl+ (set-union sides new-sides)))
      (cons cl (set))))
  
  (match-define `(rule ,heads ,bodys) rule)
  (match-define (cons bodys-list+ new-sides)
    (foldl (λ (cl accu)
            (match-define (cons bodys new-sides) accu)
            (match-define (cons new-cl cl-new-sides) (replace-repeated-var cl))
            (cons (append bodys (list new-cl)) (set-union new-sides cl-new-sides)))
      (cons '() (set))
      (set->list bodys)))
  `(rule ,heads ,(list->set (append bodys-list+ (set-map new-sides give-clause-id)))))


(define/contract-cond (replace-constants-with-equals-clauses rule all-comp-rels)
  (ir-flat-rule? set? . -> . ir-flat-rule?)
  (define (clause-constants cl)
    (match-define (list id rel args) (ir-flat-clause-rel-args-w/prov cl))
    (filter (prov-of lit?) args))

  (define (db-clause? cl) 
    (match-define (list _ rel args) (ir-flat-clause-rel-args cl))
    (match rel
      [`(agg ,_agg ,_rel) #f]
      [else (not (set-member? all-comp-rels rel))]))
  
  (define (step rule)
    (match-define `(rule ,heads ,bodys) rule)
    (define (existing-var-for-const lit) 
      (foldl (λ (cl res)
              (cond 
                [res res]
                [else
                  (match-define (list _ rel args) (ir-flat-clause-rel-args-w/prov cl))
                  (match (cons (strip-prov rel) args)
                    [(cons '= `(,v ,c)) #:when (equal? (strip-prov c) lit) v]
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
          (give-clause-id `(prov ((prov = ,const-pos) ,const-var ,const-prov) ,const-pos)))
        (assert (ir-flat-clause? equals-clause) (strip-prov equals-clause))
        (define (update-clause cl) 
          (match-define (list _ rel _) (ir-flat-clause-rel-args cl))
          (if (set-member? all-comp-rels rel) 
              cl
              (rename-arg cl const (strip-prov const-var))))
        
        ;; this does not work ...
        #;(define (update-head-clause cl) 
          (match-define (list _ rel args) (ir-flat-clause-rel-args cl))
          (if (andmap lit? args)
            cl
            (rename-arg cl const (strip-prov const-var))))
        (define new-rule `(rule ,(list->set (set-map heads update-clause))
                                ,(list->set (append (set-map bodys update-clause)
                                                    (if const-existing-var (list) (list equals-clause))))))
        new-rule]))

  (iterate-to-fixed-point step rule))