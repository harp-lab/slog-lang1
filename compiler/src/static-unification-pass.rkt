#lang racket

;; Slog compilation pass -- static unification, optimization, simplification
;; Copyright (c) Thomas Gilray, et al, see License.md

;; This pass removes duplicate variables and clauses, simplifying & optimizing rules

(provide static-unification-pass
         static-unification-rules-h
         remove-silly-clauses)

(require "lang-predicates.rkt")
(require "builtins.rkt")
(require "utils.rkt")
(require "lang-utils.rkt")

;; Optimizes the flat IR by iterating unification of variables and clauses to a fixed point 
(define/contract (static-unification-pass ir)
  (-> ir-flat? ir-flat?)
  (match-define `(ir-flat ,source-tree ,rules-h ,comp-rules-h) ir)
  (define all-comp-rels (set-union all-builtin-names (ir-flat-rules-h->head-rels comp-rules-h)))
  (define (unduplicate cls-set)
    (list->set (remove-duplicates (set->list cls-set) #:key strip-prov)))
  `(ir-flat
    ,source-tree
    ,(foldl (lambda (rule h)
              (define rule+ (simplify rule))
              (define rule++
                (match rule+
                  [`(rule ,heads ,bodys)
                    `(rule ,(unduplicate heads) ,(unduplicate bodys))]))
              (when (set-empty? (second rule++))
                    (pretty-rule-error ir
                                        (hash-ref rules-h rule)
                                        "Rule has no head! Does the head also appear in the body?"))
              (hash-set h rule++ (hash-ref rules-h rule)))
            (hash)
            (hash-keys rules-h))
    ,comp-rules-h))

; Finds clauses like (= a b) and replaces a,b with just one canonical name
(define (lhs-unification rule)
  (define (find-root uf x)
    (let ([y (hash-ref uf x (lambda () x))])
      (if (equal? x y)
          x
          (find-root uf y))))
  (match-define `(rule ,heads ,bodys) rule)
  
  (define uf 
    (foldl (lambda (c uf)
              (match c
                [`(prov ((prov = ,_) ,_
                        (prov ((prov = ,=pos) (prov ,(? var? x) ,xpos) (prov ,(? var? y) ,ypos)) ,pos))
                    ,_)
                  (if (hash-has-key? uf x)
                      (if (hash-has-key? uf y)
                          (hash-set uf (find-root uf x) (find-root uf y))
                          (hash-set uf y (find-root uf x)))
                      (hash-set uf x (find-root uf y)))]
                [else uf]))
           (hash)
           (set->list bodys)))
  (define (pred clause)
    (match clause
      [`(prov ((prov = ,=pos) (prov ,(? var? x) ,xpos) (prov ,(? var? y) ,ypos)) ,pos) #f]
      [else #t]))
  (define (rename clause)
    (match clause
      [`(prov ((prov = ,=pos)
              (prov ,(? var? x) ,xpos)
              (prov (,(? ir-flat-rel? rel)
                      ,(and vcls `(prov ,(? arg? vs) ,vposs)) ...)
                    ,factpos))
              ,pos)
      `(prov ((prov = ,=pos)
              (prov ,(find-root uf x) ,xpos)
              (prov (,rel
                      ,@(map (lambda (v)
                              (match v
                                      [`(prov ,(? var? vx) ,vpos)
                                      `(prov ,(find-root uf vx) ,vpos)]
                                      [else v]))
                            vcls))
                    ,factpos))
              ,pos)]
      [`(prov ((prov = ,=pos)
              (prov ,(? var? x) ,xpos)
              (prov ,(? lit? v) ,factpos))
              ,pos)
      `(prov ((prov = ,=pos)
              (prov ,(find-root uf x) ,xpos)
              (prov ,v ,factpos))
              ,pos)]))
  `(rule
    ,(list->set (map rename (set->list heads)))
    ,(list->set (map rename (set->list bodys)))))


; Finds clauses like (= a (R x y z)), (= b (R x y z)) and replaces one with (= a b)
(define (rhs-unification rule)
  (define (process cls-st)
    (foldr (lambda (c acc)
             (match-define (list cls-st scls-st env) acc)
             (match c
                    [`(prov ((prov = ,=pos) (prov ,(? var? x) ,xpos) ,rhs+prov) ,pos)
                     (define rhs (strip-prov rhs+prov))
                     (if (hash-has-key? env rhs)
                         (list cls-st
                               (set-add scls-st
                                        `(prov ((prov = ,=pos)
                                                (prov ,x ,xpos)
                                                (prov ,(hash-ref env rhs) ,pos))
                                               ,pos))
                               env)
                         (list (set-add cls-st c)
                               scls-st
                               (hash-set env rhs x)))]
                    [else (cons (set-add cls-st c) env)]))
           (list (set) (set) (hash))
           (set->list cls-st)))
  (match rule
         [`(rule ,heads ,bodys)    
          (match-define (list heads+ hsides henv) (process heads))
          (match-define (list bodys+ bsides benv) (process bodys))
          (foldl (lambda (brhs rule)
                   (if (hash-has-key? henv brhs)
                       (foldr (lambda (c rule)
                                (match-define `(rule ,heads ,bodys) rule)
                                (match c
                                       [`(prov ((prov = ,=pos) (prov ,(? var? x) ,xpos) ,rhs+prov) ,pos)
                                        #:when (equal? (strip-prov rhs+prov) brhs)
                                        (define new-cl
                                          (give-clause-id `(prov ((prov = ,=pos)
                                                                 (prov ,x ,xpos)
                                                                 (prov ,(hash-ref benv brhs) ,xpos))
                                                                ,pos)))
                                        `(rule ,heads ,(set-add bodys new-cl))]
                                       [else `(rule ,(set-add heads c) ,bodys)]))
                              `(rule ,(set) ,bodys)
                              (set->list heads))
                       rule)) 
                 `(rule ,heads+ ,(set-union bodys+ (list->set (map give-clause-id (set->list (set-union hsides bsides))))))
                 (hash-keys benv))]))

(define (remove-silly-clauses rule)
  (match-define `(rule ,heads ,bodys) rule)
  (define (is-silly-clause cl)
    (match-define (list id rel args) (ir-flat-clause-rel-args cl))
    (match (cons rel args)
      [(cons '= (list x y)) (equal? x y)]
      [else #f]))
  `(rule ,heads ,(list->set (filter (not/c is-silly-clause) (set->list bodys)))))

; iterates *static* lhs and rhs unification to a fixed point
(define (simplify rule) 
  (iterate-to-fixed-point (compose rhs-unification remove-silly-clauses lhs-unification) rule))

(define (static-unification-rules-h rules-h)
  (hash-map-keys simplify rules-h))
