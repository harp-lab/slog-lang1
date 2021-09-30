#lang racket

;; Slog compiler -- predicates and utils for intermediate languages
;; Copyright (c) Thomas Gilray, et al, see License.md

(require "utils.rkt")
(require "lang-predicates.rkt")

(provide 
  ir-flat-clause-rel-args
  ir-fixed-clause-rel-args
  give-clause-id
  rename-arg
  rename-args
  ir-flat-clause-rel-args-w/prov
  ir-fixed-clause-rel-args-w/prov
  ir-flat-rules-h->head-rels
  ir-fixed-rules-h->head-rels
  validate-source-tree-fact)

(define (give-clause-id cl)
  (match cl
    [`(prov (,tag ,ics ...) ,pos)
      #;(define rel-name (match (strip-prov tag)
        [`(agg ,agg ,rel) (strip-prov agg)]
        [x x]))
      #;(define id-sym (if (symbol? rel-name) (string->symbol (format "$_~a-id" (symbol->string rel-name))) '$_)) 
     `(prov ((prov = ,pos) (prov ,(gensymb '$_ #;id-sym) ,pos) ,cl) ,pos)]))

;; Works for desugared source-tree clauses and ir-fixed and ir-flat clauses
(define/contract (rename-arg cl old-arg new-arg)
  (any/c arg? arg? . -> . any/c)
  (match cl
    [`(prov (,tag ,ics ...) ,pos) `(prov (,tag ,@(map (app rename-arg _ old-arg new-arg) ics)) ,pos)]
    [`(prov ,(? arg? arg) ,pos) #:when (equal? arg old-arg) `(prov ,new-arg ,pos)]
    [else cl] ))

;; Works for desugared source-tree clauses and ir-fixed and ir-flat clauses
(define/contract (rename-args cl args-mapping)
  (any/c hash? . -> . any/c)
  (match cl
    [`(prov (,tag ,ics ...) ,pos) `(prov (,tag ,@(map (app rename-args _ args-mapping) ics)) ,pos)]
    [`(prov ,(? arg? arg) ,pos) #:when (hash-has-key? args-mapping arg) `(prov ,(hash-ref args-mapping arg) ,pos)]
    [else cl] ))


(define (clause-rel-args cl [rel? any/c])
  (define my-rel? rel?)
  (match cl
    [`(prov ((prov = ,(? pos?))
                  (prov ,(? var? id) ,(? pos?))
                  (prov (,(? my-rel? rel)
                         (prov ,(? arg? xs) ,(? pos?))
                         ...)
                        ,(? pos?)))
                 ,(? pos?))
     (list id (strip-prov rel) xs)]
    [`(prov (,(? my-rel? rel)
            (prov ,(? arg? xs) ,(? pos?))
            ...)
            ,(? pos?))
     (list #f (strip-prov rel) xs)]
    [else (error "Not a valid clause: ~a" (simplify-prov cl))]))

(define (ir-fixed-clause-rel-args cl) (clause-rel-args cl (prov-of rel-arity?)))
(define (ir-flat-clause-rel-args cl) (clause-rel-args cl ir-flat-rel?))

(define (clause-rel-args-w/prov cl rel?)
  (match cl
    [`(prov ((prov = ,(? pos?))
              ,(? (prov-of var?) id)
              (prov (,(? rel? rel)
                      ,(? (prov-of arg?) xs) ...)
                    ,(? pos?)))
              ,(? pos?))
      (list id rel xs)]
    [`(prov (,(? rel? rel)
             ,(? (prov-of arg?) xs) ...)
       ,(? pos?))
      (list #f rel xs)]
    [else (error (format "bad clause: ~a" (strip-prov cl)))]))

(define (ir-fixed-clause-rel-args-w/prov cl) (clause-rel-args-w/prov cl (prov-of rel-arity?)))
(define (ir-flat-clause-rel-args-w/prov cl) (clause-rel-args-w/prov cl ir-flat-rel?))

(define (head-rels rule)
  (define heads (match rule
                  [`(rule ,heads ,bodys ,sides) (set->list heads)]
                  [`(crule ,head ,bodys ...) (list head)]))
  (map (λ (cl) (match-define (list _ rel _) (clause-rel-args cl))
               rel) 
          heads))

(define/contract (rules-h->head-rels rules-h)
  (hash? . -> . set?)
  (list->set (flat-map head-rels (hash-keys rules-h))))

(define ir-flat-rules-h->head-rels rules-h->head-rels)
(define ir-fixed-rules-h->head-rels rules-h->head-rels)

(define (validate-source-tree-fact program f)
  (match-define `(prov ((prov ,tag ,tagpos) ,args ...) ,pos) f)
  (for ([arg args])
    (cond
      [(var? (strip-prov arg)) 
        (pretty-error program (prov->pos arg) (format "unbound variable: ~a" (strip-prov arg)) #:exit #t)]
      [(lit? (strip-prov arg)) #t]
      [(source-tree-hclause? arg) (validate-source-tree-fact program arg)])))