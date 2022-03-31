#lang racket

(require "lang-predicates.rkt")
(require "utils.rkt")
(require "builtins.rkt")
(require "utils.rkt")
(require "lang-utils.rkt")
(require "slog-params.rkt")

(provide run-abstract)
(provide print-abstract-db)
(define abstract-db? any/c)

(define/contract-cond (run-abstract ir)
  (-> ir-fixed? abstract-db?)
  (match-define `(ir-fixed ,ir-flat ,rules-h ,ir-flat-comp-rules-h) ir)

  (define db (make-hash))
  (define (loop)
    (define changed #f)
    (for ([rule-prov (hash->list rules-h)])
      (match-define (cons rule prov) rule-prov)
      (define new-facts (ICr rule db))
      (define db-changed (merge-dbs! db new-facts))
      (when db-changed (set! changed #t)))
    (when changed (loop)))
  (loop)
  db)


;; merges db2 into db1
(define (merge-dbs! db1 db2)
  (define changed #f)
  (for ([pair (hash->list db2)])
    (match-define (cons rel facts) pair)
    (define db1-facts (hash-ref! db1 rel mutable-set))
    (define db1-facts-size (set-count db1-facts))
    (set-union! db1-facts facts)
    (when (> (set-count db1-facts) db1-facts-size) (set! changed #t)))
  changed)

(define (db-add-fact! db rel fact)
  (define db-facts (hash-ref! db rel mutable-set))
  (set-add! db-facts fact))

(define (ICr rule facts)
  (match-define `(rule ,heads ,bodys) rule)
  (define bodys-list (set->list bodys))
  (define (helper ind assignments)
    (cond
      [(< ind (length bodys-list))
        (define cl (list-ref bodys-list ind))
        (match-define `(prov ((prov = ,=pos) (prov ,id ,xpos) (prov ((prov ,rel ,relpos) ,vals ...) ,clspos)) ,pos) cl)
        (define new-assignments '())
        (for ([assignment assignments])
          (define matching-assignments (get-matching-assignments cl assignment facts))
          (set! new-assignments (append matching-assignments new-assignments)))
        (helper (add1 ind) new-assignments)]
      [else
        assignments]))
  (define assignments (helper 0 `(,(hash))))

  (define heads-sorted (ir-fixed-heads-sorted heads)) ;; TODO sort heads
  ; (printf "heads: ~a\nsorted: ~a\n" (strip-prov heads) (map strip-prov heads-sorted))
  (define new-facts (make-hash))
  (for ([assignment assignments])
    (for ([head-cl heads-sorted])
      (define head-cl-rel (ir-fixed-clause->rel head-cl))
      (match-define (cons fact assignemnt+) (get-fact-for-head-cl assignment head-cl))
      (db-add-fact! new-facts head-cl-rel fact)
      (set! assignment assignemnt+))
    )
  new-facts)

(define/contract (get-matching-assignments cl assignment facts)
  (any/c hash? abstract-db? . -> . list?)
  (match-define `(prov ((prov = ,=pos) (prov ,id ,xpos) (prov ((prov ,rel ,relpos) ,vals ...) ,clspos)) ,pos) cl)
  (define rel-facts (db-get-rel-facts facts rel))
  (foldl (λ (fact accu)
      (define extended-assignment (extend-assignment cl fact assignment))
      (if extended-assignment (cons extended-assignment accu) accu))
    '()
    (set->list rel-facts)))

(define (extend-assignment cl fact assignment)
  ; (printf "extend-assignment, cl: ~a, fact: ~a assignment: ~a\n" (strip-prov cl) fact assignment)
  (match-define `(prov ((prov = ,=pos) (prov ,id ,xpos) (prov ((prov ,rel ,relpos) ,vars ...) ,clspos)) ,pos) cl)
  (define (helper vars values assignment)
    (match vars
      [(cons var vars-rest)
        (match-define (cons val vals-rest) values)
        (define val-in-assignment (hash-ref assignment var '_NONE))
        (cond 
          [(equal? val-in-assignment '_NONE) (helper vars-rest vals-rest (update-assignment assignment var val))]
          [(vals-compatible? val val-in-assignment) (helper vars-rest vals-rest assignment)]
          [else #f])]
      ['() assignment]))
  (helper (strip-prov vars) fact assignment))

(define (db-get-rel-facts facts rel)
  (hash-ref facts rel mutable-set))

;; Given an assignment and a head clause, creates the tuple for the head clause.
;; Returns (cons tuple updated-assignment)
(define (get-fact-for-head-cl assignment head-cl)
  ; (printf "get-fact-for-head-cl, assignment: ~a, head-cl: ~a\n" assignment (strip-prov head-cl))
  (match-define `(prov ((prov = ,=pos) (prov ,id ,xpos) (prov ((prov ,rel ,relpos) ,vars ...) ,clspos)) ,pos) head-cl)
  (define fact (map (λ (var) (if (lit? var) (lit->abstract var) (hash-ref assignment var))) (map strip-prov vars)))
  (define fact+ (abstract-fact rel fact))
  (define assignment+ (hash-set assignment id (cons rel fact+)))
  (cons fact+ assignment+))

(define (update-assignment assignment var val)
  (hash-set assignment var val))

;;; Checks whether the abstract values are compatible
(define (vals-compatible? val1 val2)
  ;; TODO
  (equal? val1 val2))

(define (lit->abstract lit) lit)

(define max-depth 10)

(define (abstract-fact rel tuple)
  ; (printf "abstract-fact rel: ~a, tuple: ~a\n" rel tuple)
  (define abstracted (limit-depth (cons rel tuple) max-depth))
  (cdr abstracted)
  #;tuple)

;; depth conventions:
;; literals have height 0
;; a fact of the form (foo args ...) has height 1 + argmax height args
;; a 0-arity fact (eg. (foo)) has height 1 
(define (limit-depth fact depth)
  (match fact
    [(? abstract-lit?) fact]
    ['... '...]
    [`(,rel ,args ...) #:when (< depth 1) '...]
    [`(,rel ,args ...) `(,rel ,@(map (app limit-depth _ (sub1 depth)) args))]))

(define (print-abstract-db db)
  (for ([pair (hash->list db)])
    (match-define (cons rel facts) pair)
    (printf "~a:\n" rel)
    (for ([fact (set->list facts)])
      (printf "  ~a\n" (map abstract-fact->printable fact)))))

(define (abstract-fact->printable fact)
  (match fact
    [(? abstract-lit?) fact]
    ['... '...]
    [`(,rel-arity ,args ...)
      (match-define `(rel-arity ,rel ,arity ,kind) rel-arity)
       `(,rel ,@(map abstract-fact->printable args))]))

(define (abstract-lit? l)
  (match l
    [(? lit?) #t]
    ['number #t]
    ['string #t]
    [else #f]))