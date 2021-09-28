#lang racket

;; Slog compilation pass -- Incrementalizes, splitting each relation into total and delta "versions"
;; Copyright (c) Thomas Gilray, et al, see License.md

;; This pass specializes the program for semi-naive evaluation.

(provide incrementalize-pass)

(require "lang-predicates.rkt")
(require "utils.rkt")
(require "slog-params.rkt")


(define/contract-cond (incrementalize-pass ir)
  (-> ir-scc? ir-incremental?)
  (match ir
         [`(ir-scc
            ,ir-old
            ,scc-dag
            ,scc-h
            ,comp-rules-h)
          `(ir-incremental
            ,ir
            ,scc-dag
            ,(foldl (lambda (id h)
                      (hash-set h id (incrementalize-scc id (hash-ref scc-h id))))
                    (hash)
                    (hash-keys scc-h))
            ,comp-rules-h)]))


(define (incrementalize-scc id scc)
  (match scc
         [`(scc ,looping ,rel-h ,rules-h)
          (define (incrementalize-rule rule)
            (match rule
                   [`(srule (prov ((prov (rel-select ,hrel ,harity ,hsel db) ,hrelpos)
                                   ,hargs ...)
                                  ,headpos)
                            (prov ((prov (rel-select ,b0rel ,b0arity ,b0sel ,b0kind) ,b0relpos)
                                   ,b0args ...)
                                  ,body0clpos)
                            (prov ((prov (rel-select ,b1rel ,b1arity ,b1sel ,b1kind) ,b1relpos)
                                   ,b1args ...)
                                  ,body1clpos))
                    (match-define `(,b0usage ,b0csel ,b0sel-st) (hash-ref rel-h `(rel-arity ,b0rel ,b0arity ,b0kind) '(static #f #f)))
                    (match-define `(,b1usage ,b1csel ,b1sel-st) (hash-ref rel-h `(rel-arity ,b1rel ,b1arity ,b1kind) '(static #f #f)))
                    (define new-b0kind (if (equal? b0kind 'db) (if (equal? b0usage 'dynamic) 'delta 'total) b0kind))
                    (define new-b1kind (if (equal? b1kind 'db) (if (equal? b1usage 'dynamic) 'delta 'total) b1kind))
                    (if (and (equal? 'dynamic b0usage) (equal? 'dynamic b1usage))
                        (set
                         `(srule (prov ((prov (rel-select ,hrel ,harity ,hsel db) ,hrelpos)
                                        ,@hargs)
                                       ,headpos)
                                 (prov ((prov (rel-version ,b0rel ,b0arity ,b0sel delta)
                                              ,b0relpos)
                                        ,@b0args)
                                       ,body0clpos)
                                 (prov ((prov (rel-version ,b1rel ,b1arity ,b1sel delta)
                                              ,b1relpos)
                                        ,@b1args)
                                       ,body1clpos))
                         `(srule (prov ((prov (rel-select ,hrel ,harity ,hsel db) ,hrelpos)
                                        ,@hargs)
                                       ,headpos)
                                 (prov ((prov (rel-version ,b0rel ,b0arity ,b0sel total)
                                              ,b0relpos)
                                        ,@b0args)
                                       ,body0clpos)
                                 (prov ((prov (rel-version ,b1rel ,b1arity ,b1sel delta)
                                              ,b1relpos)
                                        ,@b1args)
                                       ,body1clpos))
                         `(srule (prov ((prov (rel-select ,hrel ,harity ,hsel db) ,hrelpos)
                                        ,@hargs)
                                       ,headpos)
                                 (prov ((prov (rel-version ,b0rel ,b0arity ,b0sel delta)
                                              ,b0relpos)
                                        ,@b0args)
                                       ,body0clpos)
                                 (prov ((prov (rel-version ,b1rel ,b1arity ,b1sel total)
                                              ,b1relpos)
                                        ,@b1args)
                                       ,body1clpos)))
                        (set
                         `(srule (prov ((prov (rel-select ,hrel ,harity ,hsel db) ,hrelpos)
                                        ,@hargs)
                                       ,headpos)
                                 (prov ((prov (rel-version ,b0rel ,b0arity ,b0sel
                                                           ,new-b0kind)
                                              ,b0relpos)
                                        ,@b0args)
                                       ,body0clpos)
                                 (prov ((prov (rel-version ,b1rel ,b1arity ,b1sel
                                                           ,new-b1kind)
                                              ,b1relpos)
                                        ,@b1args)
                                       ,body1clpos))))] 
                   [`(srule (prov ((prov (rel-select ,hrel ,harity ,hsel db) ,hrelpos)
                                   ,hargs ...)
                                  ,headpos)
                            (prov ((prov (rel-select ,b0rel ,b0arity ,b0sel ,b0kind) ,b0relpos)
                                   ,b0args ...)
                                  ,body0clpos))
                    (match-define `(,b0usage ,b0csel ,b0sel-st) (hash-ref rel-h `(rel-arity ,b0rel ,b0arity db) '(static #f #f)))
                    (define new-b0kind (if (equal? b0kind 'db) (if (equal? b0usage 'dynamic) 'delta 'total) b0kind))
                    (set
                     `(srule (prov ((prov (rel-select ,hrel ,harity ,hsel db) ,hrelpos)
                                    ,@hargs)
                                   ,headpos)
                             (prov ((prov (rel-version ,b0rel ,b0arity ,b0sel
                                                       ,new-b0kind)
                                          ,b0relpos)
                                    ,@b0args)
                                   ,body0clpos)))]
                   [`(srule (prov ((prov (rel-select ,hrel ,harity ,hsel db) ,hrelpos)
                                   ,hargs ...)
                                  ,headpos))
                    (set
                     `(srule (prov ((prov (rel-select ,hrel ,harity ,hsel db) ,hrelpos)
                                    ,@hargs)
                                   ,headpos)))]
                   [`(arule (prov ((prov (rel-select ,hrel ,harity ,hsel db) ,hrelpos)
                                   ,hargs ...)
                                  ,headpos)
                            (prov ((prov (rel-select ,b0rel ,b0arity ,b0sel db) ,b0relpos)
                                   ,b0args ...)
                                  ,body0clpos))
                    (set
                     `(arule (prov ((prov (rel-select ,hrel ,harity ,hsel db) ,hrelpos)
                                    ,@hargs)
                                   ,headpos)
                             (prov ((prov (rel-version ,b0rel ,b0arity ,b0sel delta) ,b0relpos)
                                    ,@b0args)
                                   ,body0clpos)))]
                   [else (error (format "Unrecognized rule: ~a" rule))]))
          `(scc ,looping
                ,rel-h
                ,(foldl (lambda (rule h)
                          (define rule-prov+
                            (match (hash-ref rules-h rule)
                                   [`(rule-prov ,ir-select ,sel-rule ,source-module ,source-id)
                                    `(rule-prov ir-scc ,id ,rule ,source-module ,source-id)]
                                   [`(rule-prov intra-relation ,rel ,arity)
                                    `(rule-prov intra-relation ,rel ,arity)]))
                          (define rules-st (incrementalize-rule rule))
                          (foldl (lambda (rule+ h)
                                   (hash-set h rule+ rule-prov+))
                                 h
                                 (set->list rules-st)))
                        (hash)
                        (hash-keys rules-h)))]))


