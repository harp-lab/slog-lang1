;; Automatic Index Selection
;; 
;; From the paper [1]:
;; Automatic Index Selection for Large-Scale Datalog Computation
;;
;; See end of file for examples
#lang racket

(require graph)

;;
;; Contracts
;;
(define index? (listof natural-number/c))

(define indices? (set/c index?))

(define columns? (set/c natural-number/c))

(define selection? columns?)

; A select, iter, ignore list is a list of sets: 
;  - A set of columns to select on
;  - A set of columns to iterate over
;  - A set of columns to ignore
(define select-iter-ignore?
  (and/c (λ (l) (= (length l) 3)) (listof (set/c natural-number/c))))

(define select-iter-ignore-set?
  (set/c select-iter-ignore?))

(define (sorted? l)
  (equal? (sort l) l))

;; 
;; Interface
;;

(provide
 (contract-out
  [min-chain-cover (-> (set/c selection?) (set/c selection?))]
  [indices-from-selections (-> (set/c selection?) columns? indices?)]
  [naive-indices (-> select-iter-ignore-set? indices?)]))

;;
;; Debugging
;; 
(define debug? #f)
(define (ddisplay x) (if debug? (display x) (void)))
(define (dpretty-print x) (if debug? (pretty-print x) (void)))

 ;; (contract-out
;; ;(-> (listof (and/c (listof natural-number/c) sorted?))
;; ;                              (listof natural-number/c))          
;;           [min-chain-cover (-> any/c any/c)])

;; 
;; Graph ceremony
;;

(struct vtx (tag contents) #:transparent)

(define (set-remove S e)
  (list->set (remove e (set->list S) equal?)))

(define (sort-sets s) (sort s proper-subset?))

;;
;; Minimum chain cover (Algorithm 1 in [1])
;;

(define example-selections (set (set 0) (set 1) (set 0 1) (set 0 1 2)))

; Calculate the minimum chain cover of a set of searches. Produce a
; set of searches that can be converted into indices.
; (-> (set/c selection?) (set/c selection?))
(define (min-chain-cover selections)

  (define (gen-vertices selections tag)
    (set-map selections (λ (selection) `(vtx ,tag ,selection))))
  (define selections-l (set->list selections))

  ; Form a bipartite graph from two copies of the set of selections.  To
  ; make vertices unique for each copy, we just create vertices of n,s
  ; for all s ∈ selections and n ∈ {0,1}
  (define U (gen-vertices selections 0))
  (define V (gen-vertices selections 1))

  ; Form edges between elements of U  and V when s1 ∈ U is
  ; lexicographically less than s2 ∈ V
  (define edges
    (map
     (match-lambda [`(,x . ,y)
                    (list (vtx 0 x) (vtx 1 y))])
     (set->list (foldl
                 (λ (selection current-edges)
                   (foldl
                    (λ (other-selection current-edges)
                      (if (proper-subset? selection other-selection)
                          (set-add current-edges (cons selection other-selection))
                          current-edges))
                    current-edges
                    selections-l))
                 (set)
                 selections-l))))

  ; Find a maximum matching of the set of verices
  (define matching (maximum-bipartite-matching (undirected-graph edges)))

  (ddisplay "matching...\n")
  (dpretty-print matching)
  
  ; Calculate the chains by finding maximal paths through unipartite
  ; graph consisting of all vertices.
  (define chain-input-edges
    (foldr
     (λ (matching-edge curr-edges)
       (ddisplay "edge\n")
       (ddisplay matching-edge)
       (cons
         (sort-sets (list (vtx-contents (first matching-edge))
                          (vtx-contents (cadr matching-edge))))
         curr-edges))
     '()
     matching))
  
  (ddisplay "chain-input-edges\n")
  (dpretty-print chain-input-edges)
  
  (ddisplay "undirected-edges graph")
  (dpretty-print (undirected-graph edges))

  (define connected-components (cc (undirected-graph chain-input-edges)))

  (ddisplay "connected components")
  (dpretty-print (undirected-graph edges))

  (define chains (map sort-sets connected-components))
  (ddisplay "chains")
  (dpretty-print chains)

  chains)

;;
;; Index selection for selection sets (Algorithm 2 in [1])
;;

; Assemble an index out of a given chain
(define ((assemble-index all-columns) chain)
  (define index '())
  (define columns-left (mutable-set))
  (set-union! columns-left all-columns)
  (define seen (mutable-set))

  (dpretty-print chain)
  (ddisplay "done calculating")

  ; Iterate over each set in the chain
  (for ([current chain])
    (define curr-iter (set-subtract (list->set current) seen))
    (set-union! seen curr-iter)
    (ddisplay "\nseen\n")
    (dpretty-print seen)
    (set-subtract! columns-left seen)
    (ddisplay "\nleft\n")
    (dpretty-print columns-left)
    (set! index (append index (set->list curr-iter)))
    (ddisplay index))

  (ddisplay "left")
  (dpretty-print  columns-left)

  ; Now add the left over stuff
  (append index (set->list columns-left)))

; Calculate a set of indices from a set of selections
(define/contract (indices-from-selections selections all-columns)
  (-> (set/c selection?) columns? indices?)
  ; First, calculate the minimum chain cover
  (define min-chain-cov (min-chain-cover selections))

  (define x (map (assemble-index all-columns) min-chain-cov))
  (ddisplay "\nblah\n")
  (dpretty-print x)

  ; Next, use the minimum chain cover
  (list->set (map (assemble-index all-columns) min-chain-cov)))

;;
;; Naive index selection for selection,iter,ignore sets
;;
;; 
(define (naive-indices sii-sets)
  (-> select-iter-ignore-set? indices?)
  (list->set
   (set-map
    sii-sets
    (match-lambda [`(,select ,iter ,ignore)
                   (append (set->list select) (set->list iter) (set->list ignore))]))))

;;
;; Examples
;; 
(module+ test
  (naive-indices (set (list (set 1 2) (set 3 5) (set 4))))
  (naive-indices (set (list (set 3 5) (set 1 4) (set 2 6))))
  (naive-indices (set (list (set) (set 1 4) (set 2 3)))))
