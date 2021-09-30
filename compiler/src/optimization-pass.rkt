#lang racket
;; This pass removes extra intermediate rules/relations

(provide optimization-pass)

(require "lang-predicates.rkt")
(require "builtins.rkt")
(require "utils.rkt")
(require "lang-utils.rkt")
(require "static-unification-pass.rkt")
(require racket/hash)


(define (internal-rel? rel-name)
  (define rel-name-str (match (strip-prov rel-name)
    [`(agg ,aggregator ,rel) "aggregator"]
    [symbol (symbol->string symbol)]))
  (and (string-prefix? rel-name-str "$")
       (not (equal? rel-name-str "$lst"))
       (not (equal? rel-name-str "$nil"))))

(define/contract (optimization-pass ir)
  (-> ir-flat? ir-flat?)
  (match-define `(ir-flat ,source-tree ,rules-h ,comp-rules-h) ir)
  (define new-rules-h (iterate-to-fixed-point
                        (compose static-unification-rules-h
                                 (many substitute-subsumed-bodys-step) 
                                 (many remove-extra-internal-rels-step)
                                 remove-silly-rules) 
                        rules-h))
  `(ir-flat ,source-tree ,new-rules-h ,comp-rules-h))



(define (rule-has-rel-in-body rule rel)
  (match-define `(rule ,heads ,bodys) rule)
  (ormap (λ (cl) (equal? (clause-rel cl) rel)) (set->list bodys)))

(define (rule-has-rel-in-head rule rel)
  (match-define `(rule ,heads ,bodys) rule)
  (ormap (λ (cl) (equal? (clause-rel cl) rel)) (set->list heads)))

(define (rule-has-rel-aggregated rule rel)
  (match-define `(rule ,heads ,bodys) rule)
  (ormap (λ (cl) 
          (match (clause-rel cl)
            [`(agg ,aggregator ,aggregated-rel) (equal? (strip-prov aggregated-rel) rel)]
            [else #f])) 
         (set->list bodys)))

(define (is-copy-rule rule)
  (match-define `(rule ,heads ,bodys) rule)
  (cond 
    [(= 1 (set-count bodys))
      (define body (set-first bodys))
      (match-define (list bid rel args) (ir-flat-clause-rel-args body))
      (and
        ;; not constants in the body:
        (andmap var? args) 
        ;; no self-joins:
        (equal? (remove-duplicates args) args) 
        ;; no use of body fact id
        (andmap (λ (hcl) (match-define (list hid hrel hargs) (ir-flat-clause-rel-args hcl))
                         (not (member bid hargs)))  
                (set->list heads)))]
    [else #f]))

;; returns if the given rel's fact id is included in any head clauses in the rule
(define (rel-id-used-in-head rel rule)
  (match-define `(rule ,heads ,bodys) rule)
  (define rel-head-ids 
    (filter-map (λ (cl) 
                  (match-define (list clid clrel _args) (ir-flat-clause-rel-args cl))
                  (and (equal? clrel rel) clid)) 
                (set->list heads)))
  (ormap
    (λ (id)
      (ormap
        (λ (cl)
          (match-define (list _id _rel args) (ir-flat-clause-rel-args cl))
          (member id args)) 
        (set->list heads)))
    rel-head-ids))

(define (get-rel-repl-clause internal-head-cl internal-rel-copy-rule)
  ; (printf "get-rel-repl-clause, \ninternal-head-cl: ~a\ninternal-rel-copy-rule: ~a\n\n" 
  ;         (strip-prov internal-head-cl) (strip-prov internal-rel-copy-rule))
  (match-define `(rule ,heads ,bodys) internal-rel-copy-rule)
  (assert (= 1 (set-count bodys)))
  (define internal-rel-copy-body-cl (car (set->list bodys)))
  (match-define `(prov ((prov = ,_) ,_ (prov (,internal-rel ,b-args ...) ,_)) ,_) internal-rel-copy-body-cl)
  (match-define `(prov ((prov = ,_) ,_ (prov (,internal-rel2 ,h-args ...) ,_)) ,_) internal-head-cl)
  (assert (equal? (strip-prov internal-rel) (strip-prov internal-rel2)))

  (define (rename-arg arg)
    ; (printf "rename-arg: ~a\n" arg)
    (define arg-ind (index-of (map strip-prov b-args) (strip-prov arg)))
    (if arg-ind (list-ref h-args arg-ind) arg))

  (define res 
    (map (λ (h)
        (match-define `(prov ((prov = ,=pos) ,id (prov (,rel ,args ...) ,clpos)) ,pos) h)
        `(prov ((prov = ,=pos) ,id (prov (,rel ,@(map rename-arg args)) ,clpos)) ,pos))
    (set->list heads)))
  res)

(define (clause-rel cl)
  (match-define `(prov ((prov = ,=pos) ,id (prov (,rel ,args ...) ,_)) ,pos) cl)
  (strip-prov rel))

(define (rule-rels rule)
  (match-define `(rule ,heads ,bodys) rule)
  (map clause-rel (set->list (set-union heads bodys))))

(define (rule-replace-internal-head-clause rel rule internal-rel-copy-rules)
  (match-define `(rule ,heads ,bodys) rule)
  (define head-clauses (filter (λ (cl) (equal? (clause-rel cl) rel)) (set->list heads)))
  (assert (= 1 (length head-clauses)) (format "rule: ~a" (strip-prov rule)))
  (define head-clause (car head-clauses))
  (define new-clauses (flat-map (app get-rel-repl-clause head-clause _) internal-rel-copy-rules))
  (define new-rule `(rule ,(set-union (set-remove heads head-clause) (list->set new-clauses))
                          ,bodys))
  new-rule)

(define/contract (remove-internal-rel-if-possible rel rules-h)
  (any/c hash? . -> . (cons/c hash? boolean?))
  (define rel-body-rules (filter (app rule-has-rel-in-body _ rel) (hash-keys rules-h) ))
  (define rel-head-rules+prov (filter (λ (rule+prov) (rule-has-rel-in-head (car rule+prov) rel)) 
                                      (hash->list rules-h) ))
  (define rel-aggregated (ormap (app rule-has-rel-aggregated _ rel) (hash-keys rules-h)))
  (define is-rel-removable
    (and (not rel-aggregated)
         (andmap is-copy-rule rel-body-rules)
         (andmap (λ (rule) (not (rel-id-used-in-head rel rule)))  (map car rel-head-rules+prov) )))
  (cond
    [is-rel-removable
      (printf "removable rel: ~a\n" rel)
      (define new-rel-head-rules
        (map (λ (rule+prov)
              (match-define (cons rule prov) rule+prov)
              (match-define `(rule ,heads ,bodys) rule)
              (define new-rule (rule-replace-internal-head-clause rel rule rel-body-rules))
              ; (printf "rule rewritten. original rule: \n~a \nrewritten-rule: \n~a\n" (strip-prov rule) (strip-prov new-rule))
              (cons new-rule prov))
           rel-head-rules+prov))
      (define rules-h- (foldl (λ (r rules-h) (hash-remove rules-h r)) 
                              rules-h 
                              (append rel-body-rules (map car rel-head-rules+prov))))
      (cons (hash-union rules-h- (list->hash new-rel-head-rules)) #t)]
    [else (cons rules-h #f)]))


(define (remove-extra-internal-rels-step rules-h)
  (define all-rels (remove-duplicates (flat-map rule-rels (hash-keys rules-h))))
  (define internal-rels (filter internal-rel? all-rels))
  (match-define (cons new-rules-h updated)
    (foldl
      (λ (rel accu)
        (match-define (cons rules-h updated) accu)
        (cond 
          [updated accu]
          [else (remove-internal-rel-if-possible rel rules-h)]))
        (cons rules-h #f)
        internal-rels))
  new-rules-h)

(define (reverse-hash hash)
  (list->hash (map (match-lambda [(cons x y) (cons y x)]) (hash->list hash))))

(define (substitute-subsumed-bodys-step rules-h)
  (define (clause-vars-w/prov cl)
    (match-define (list id rel args) (ir-flat-clause-rel-args-w/prov cl))
    (filter (compose var? strip-prov) (cons id args)))

  ;; sort by number of body clauses
  (define rule+provs (sort (hash->list rules-h) > #:key (λ (rule+prov) 
                                                         (match-define `(rule ,heads ,bodys) (car rule+prov))
                                                         (set-count bodys))))
  (call/ec (λ (return)
    (for* [(rule2-ind (range 0 (length rule+provs)))
           (rule1-ind (range (add1 rule2-ind) (length rule+provs)))]
      (match-define (cons rule1 rule1-prov) (list-ref rule+provs rule1-ind))
      (match-define (cons rule2 rule2-prov) (list-ref rule+provs rule2-ind))
      (match-define `(rule ,heads1 ,bodys1) rule1)
      (match-define `(rule ,heads2 ,bodys2) rule2)
      (define clauses-subset?-res (clauses-subset? bodys1 bodys2))

      (when (and (> (set-count bodys1) 1) clauses-subset?-res)
        (printf "body of rule ~a (~a clauses) subsumed by body of rule ~a (~a clauses)\n" 
          rule1-prov (set-count bodys1) rule2-prov (set-count bodys2))
        ; (printf "fst rule: \n~a \nsnd rule: \n~a\n" (format-ir-rule rule1) (format-ir-rule rule2))
        (match-define (cons matching-clauses var-mapping) clauses-subset?-res)
        (define reverse-var-mapping (reverse-hash var-mapping))
        (assert (hash? var-mapping) (format "var-mapping: ~a" var-mapping))
        (define bodys2-remaining (set-subtract bodys2 (list->set matching-clauses)))
        (define matching-clauses-vars-w/prov (remove-duplicates (flat-map clause-vars-w/prov matching-clauses) #:key strip-prov))
        (define remaining-clauses-vars-w/prov 
          (remove-duplicates (flat-map clause-vars-w/prov (set->list (set-union heads2 bodys2-remaining))) #:key strip-prov))
        (define replacement-clause-vars-w/prov
          (filter (λ (var) (member var remaining-clauses-vars-w/prov (λ (x y) (equal? (strip-prov x) (strip-prov y))))) matching-clauses-vars-w/prov))
        (define replacement-clause-vars-set (list->set (map strip-prov replacement-clause-vars-w/prov)))

        (define replacement-clauses-vars-for-rule1-set 
          (list->set (map (λ (v) (hash-ref reverse-var-mapping v)) (set->list replacement-clause-vars-set))))
        (define suitable-rule1-head-cluases
          (filter (λ (hcl) 
                    (define hcl-vars (map strip-prov (clause-vars-w/prov hcl)))
                    ;; TODO this is wrong. That's why it's been disabled.
                    ;; For it to be correct, we need to make sure that the head
                    ;; clause does not appear in the head of any other rules.
                    ;; Also, maybe it's not worth it 
                    (and #f (subset? replacement-clauses-vars-for-rule1-set (list->set hcl-vars)))) 
                  (set->list heads1)))

        (match-define (cons subsumed-cls-repl-rule1 subsumed-cls-repl-rule2)
          (match suitable-rule1-head-cluases
            [(cons cl _) #:when #f
             (cons cl (rename-args cl var-mapping))]
            [else
             (define pos (prov->pos (set-first bodys2)))
             (define cl-for-rule2 
              (give-clause-id `(prov ((prov ,(gensymb '$subsumed-cls-replacement) ,pos) ,@replacement-clause-vars-w/prov) ,pos)))
             (define cl-for-rule1 (rename-args cl-for-rule2 reverse-var-mapping))
             (cons cl-for-rule1 cl-for-rule2)]))
        
        (assert (ir-flat-clause? subsumed-cls-repl-rule2) (format "bad clause: ~a\n" (simplify-prov subsumed-cls-repl-rule2)))
        (assert (ir-flat-clause? subsumed-cls-repl-rule1) (format "bad clause: ~a\n" (simplify-prov subsumed-cls-repl-rule1)))
        
        (define new-rule2 `(rule ,heads2 ,(set-union bodys2-remaining (set subsumed-cls-repl-rule2))))
        (define new-rule1 
          (cond
            [(not (empty? suitable-rule1-head-cluases)) rule1]
            [else `(rule ,(set-union heads1 (set subsumed-cls-repl-rule1)) ,bodys1)]))
        
        ; (printf "new fst rule:\n~a\nnew snd rule:\n~a\n\n" (format-ir-rule new-rule1) (format-ir-rule new-rule2))
        (define new-rule+provs (list-set (list-set rule+provs rule1-ind (cons new-rule1 rule1-prov))
                                        rule2-ind (cons new-rule2 rule2-prov)))
        (return (list->hash new-rule+provs))))
    (return rules-h))))


;; If cls1 is a subset of cls2, returns a pair: (cons matching-clauses var-mapping)
;; where matching-clauses is a list of cls2 clauses matching cls2, 
;; and var-mapping is a mapping from cls1 variables to cls2 variables.
;; Otherwise returns #f
(define (clauses-subset? cls1 cls2 [var-mapping (hash)])

  (define (get-var-mapping vars1 vars2) 
    (define reverse-assoc (remove-duplicates (map cons vars2 vars1)))
    (cond 
      ;; making sure that the mapping is one-to-one
      [(= (hash-count (list->hash reverse-assoc)) (length reverse-assoc))
        (foldl (λ (var1 var2 accu)
                (cond
                  [(not accu) accu]
                  [(and (or (lit? var1) (lit? var2)) (equal? var1 var2)) accu]
                  [(and (or (lit? var1) (lit? var2)) (not (equal? var1 var2))) #f]
                  [(equal? (hash-ref accu var1 var2) var2) (hash-set accu var1 var2)]
                  [else #f])) 
              (hash) 
              vars1 vars2)]
      [else #f]))

  (define (var-mappings-consistent mapping1 mapping2)
    (define (hashes-consistent hash1 hash2)
      (andmap (match-lambda [(cons x y) (equal? y (hash-ref hash2 x y))])
              (hash->list hash1)))
    (and mapping1 mapping2
         (hashes-consistent mapping1 mapping2)
         (hashes-consistent (reverse-hash mapping1) (reverse-hash mapping2))))
  (cond
    [(> (set-count cls1) (set-count cls2)) #f]
    [(= (set-count cls1) 0) (cons (list) var-mapping)]
    [else
      (define fst-clause (set-first cls1))
      (match-define (list id1 rel1 args1) (ir-flat-clause-rel-args fst-clause))
      (define matching-cls2-clauses
        (filter (λ (cl)
                  (match-define (list id2 rel2 args2) (ir-flat-clause-rel-args cl))
                  (and (equal? rel1 rel2) 
                       (= (length args1) (length args2))
                       (var-mappings-consistent (get-var-mapping (cons id1 args1) (cons id2 args2)) var-mapping)))
                (set->list cls2)))
      (call/ec (λ (return)
        (for ([matching-clause matching-cls2-clauses])
          (match-define (list id2 rel2 args2) (ir-flat-clause-rel-args matching-clause))
          (define new-var-mapping (hash-union (get-var-mapping (cons id1 args1) (cons id2 args2)) var-mapping #:combine (λ (x y) x)))
          (define sub-res (clauses-subset? (set-subtract cls1 (set fst-clause)) (set-subtract cls2 (set matching-clause)) new-var-mapping))
          (when sub-res 
            (match-define (cons cls2-matching var-mapping) sub-res)
            (define res (cons (cons matching-clause cls2-matching) var-mapping))
            (return res)))
        (return #f)))]))


;; Removes rules where the head is subsumed by the body
;; eg: [(foo x) (bar x) --> (foo x)]
(define (remove-silly-rules rules-h)
  (define (is-silly-rule rule)
    (match-define `(rule ,heads ,bodys) rule)
    (subset? (list->set (set-map heads strip-prov)) (list->set (set-map bodys strip-prov))))
  (list->hash (filter (compose not is-silly-rule car) (hash->list rules-h))))