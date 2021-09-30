;; Graph algorithms for slog. Copyright (c) Kristopher Micinski, et
;; al, see License.md

#lang racket

(require (except-in graph transitive-closure))
(require "generic-utils.rkt")

(provide calculate-sccs
         topological-sort
         hash-graph-transitive-closure)

(module+ test
  (require rackunit))

;; Calculate SCCs
;;
;; Input: hash from nodes -> nodes (nodes are opaque and compared using equal?)
;;
;; Outputs a hash of sets of nodes to sets of sets of nodes. Each node
;; represents an SCC within the original graph and relations between
;; them represent dependencies between SCCs.
(define (calculate-sccs graph)
  ;; Track a mutable set of reamining nodes.
  (define remaining-nodes (apply mutable-set (hash-keys graph)))

  ;; Track the discovered SCCs in a mutable set. At the end we will
  ;; form inter-SCC connections and turn this into a graph.
  (define discovered-sccs (mutable-set))

  (define (add-scc scc)
    (set-add! discovered-sccs scc))
  
  ;; The current timestamp counts up as we discover new nodes
  (define cur-timestamp 0)
  (define (get-next-timestamp)
    (set! cur-timestamp (add1 cur-timestamp))
    cur-timestamp)

  ;; Explore a single node.
  ;; 
  ;; Parameters:
  ;;  - node            -- node to explore
  ;;  - node-stack      -- current stack of nodes
  ;;  - node->timestamp -- partial map from nodes to timestamps.
  ;;                       No mapping if node not yet explored.
  ;;  - node->min-successor
  ;;    A map from nodes, n, to the minimum timestamp, t, of the set
  ;;    of nodes transitively transitively reachable by n. (See
  ;;    explanation below.)
  ;; 
  ;; Returns values:
  ;;  - node-stack          (updated)
  ;;  - node->timestamp     (updated)
  ;;  - node->min-successor (updated)
  (define (visit-node node node-stack node->timestamp node->min-successor)
    ;;(displayln (format "vn\n~a\n~a\n~a\n~a\n" node node-stack node->timestamp node->min-successor))
    
    ;; Get the current timestep
    (define curr-time (get-next-timestamp))
    
    ;; Remove this from the mutable set of unexplored nodes
    (set-remove! remaining-nodes node)

    ;; Update the min-successor and timestamp maps before then
    ;; iterating with an extended stack.
    (let* ([node->min-successor (hash-set node->min-successor node curr-time)]
           [node->timestamp (hash-set node->timestamp node curr-time)]
           [node-stack (cons node node-stack)]
           [ending-state
            ;; Walk over each neighbor and possibly explore it
            (foldl
             (λ (immediate-successor state)
              (match-define `(,node-stack ,node->timestamp ,node->min-successor) state)
              (cond 
                ;; If node has not yet been explored, visit it
                [(set-member? remaining-nodes immediate-successor)
                 ;;(displayln (format "ns~a" node-stack))
                 
                 ;; Visit that node: extend the stack and pass through state
                 (define-values (node-stack-prime
                                 node->timestamp-prime
                                 node->min-successor-prime)
                   (visit-node
                    immediate-successor
                    node-stack
                    node->timestamp
                    node->min-successor))
                 ;; Now update based on the min successor map
                 `(,node-stack-prime
                   ,node->timestamp-prime
                   ,(hash-set node->min-successor-prime
                              node
                              (min (hash-ref node->min-successor-prime node curr-time)
                                   (hash-ref node->min-successor-prime immediate-successor))))]
                [(member immediate-successor node-stack)
                 ;; If this node points back to something previously on the stack 
                 `(,node-stack
                  ,node->timestamp
                  ,(hash-set
                    node->min-successor
                    node
                    (min 
                     (hash-ref node->min-successor node)
                     (hash-ref node->timestamp immediate-successor))))]
                ;; else
                [else `(,node-stack ,node->timestamp ,node->min-successor)]))
            `(,node-stack ,node->timestamp ,node->min-successor)
            (set->list (hash-ref graph node)))])
      ;; Possibly generate an SCC
      (match ending-state
        [`(,node-stack ,node->timestamp ,node->min-successor)
         (begin
            ;; Calculate the return node stack (only changes when we
            ;; find an SCC) and (if found) mutably add an SCC to the
            ;; graph.
            (define return-node-stack
              (if (equal? (hash-ref node->min-successor node)
                          (hash-ref node->timestamp     node))
                  ;; Split a list (stack) of nodes `l` into an SCC that
                  ;; starts with timestamp `i`. Return both the scc (as a
                  ;; set) and the rest of the stack.
                  (letrec ([split-stack
                            (λ (l acc)
                              (match l
                                [(cons hd tl) #:when (equal? hd node)
                                              (values (list->set (cons hd acc)) tl)]
                                [(cons hd tl) (split-stack tl (cons hd acc))]))])
                    (define-values (scc-set rest-of-stack) (split-stack node-stack '()))
                    ;; Output the SCC
                    (add-scc scc-set)
                    rest-of-stack)
                  ;; Otherwise, stack doesn't change
                  node-stack))
            
            ;; Node timestamps and successor graph doesn't change
            (values return-node-stack node->timestamp node->min-successor))])))
  
  ;; Find all of the SCCs
  (define (find-all-sccs node-stack node->timestamp node->min-successor)
    (if (set-empty? remaining-nodes)
        ;; All done, extends the set of discovered SCCs
        (void)
        ;; 
        (let-values ([(ns nt nms) 
                      (visit-node (set-first remaining-nodes) node-stack node->timestamp node->min-successor)])
          (find-all-sccs ns nt nms))))

  ;; Discover all of the SCCs in the graph, leave them in
  ;; discovered-sccs
  (find-all-sccs '() (hash) (hash))

  ;; Form edges between SCCs based on the nodes within each
  ;; component. These is an edge between two SCCs when there is an
  ;; edge from something in one to something in the other.

  ;; Accumulate a hash from nodes to their SCCs (e.g., 1 -> {1,2,3})
  (define node->scc 
    (foldl (λ (scc h)
             (foldl (λ (node h) (hash-set h node scc)) h (set->list scc)))
           (hash)
           (set->list discovered-sccs)))

  ;; Flesh out the graph by forming inter-SCC edges
  (foldl (λ (scc scc-graph)
             (let* ([neighbor-nodes (foldl
                                     set-union
                                     (set)
                                     (map
                                      (λ (node) (hash-ref graph node))
                                      (set->list scc)))]
                    [other-sccs (map (λ (succ) (hash-ref node->scc succ))
                                     (set->list neighbor-nodes))])
               (foldl (λ (other-scc scc-graph)
                        (hash-set scc-graph scc (if (equal? other-scc scc)
                                                    (hash-ref scc-graph scc (set))
                                                    (set-add (hash-ref scc-graph scc (set)) other-scc))))
                      scc-graph
                      other-sccs)))
         (foldl (λ (n h) (hash-set h n (set))) (hash) (set->list discovered-sccs))
         (set->list discovered-sccs)))

;; convert a graph given as a hash to a graph from the graph module
(define (hash-graph->graph-graph hash-graph)
 (define graph-graph (directed-graph
   (foldl (lambda (source edges)
            (foldl
             (lambda (destination edges) (cons (list source destination) edges))
             edges
             (set->list (hash-ref hash-graph source))))
          '()
          (hash-keys hash-graph))))
  (for ([k (hash-keys hash-graph)])
    (add-vertex! graph-graph k))
  graph-graph)

;; Topologically sort a graph (given as a hash)
(define (topological-sort graph)
  (tsort (hash-graph->graph-graph graph)))

;; returns the transitive closure of a graph given as a hash
(define (hash-graph-transitive-closure graph)
  (define tc (transitive-closure (app hash-ref graph _)))
  (foldl (λ (node accu) (hash-set accu node (tc node))) (hash) (hash-keys graph)))
;(pretty-print (topological-sort (hash 0 (set 2) 1 (set 2) 2 (set 3 4))))

;; 
;; Tests
;; 

;; Testing code to generate random graphs of some rank
(define (generate-random-graph num-nodes num-edges)
  (foldl
   (lambda (neighbor acc-graph)
     (let ([source (random num-nodes)])
       (hash-set acc-graph
                 source
                 (set-add (hash-ref acc-graph source) (random num-nodes))))) 
   (foldl (lambda (i h) (hash-set h i (set))) (hash) (range num-nodes))
   (range num-edges)))

(define ntests 3000)

;; 
;; Tests
;; 

;; Run stress tests
;(test)

;; Create 3000 random graphs
(define (test-tarjan)
  (displayln (format "Performing ~a tests" ntests))
  (for ([i (in-range ntests)])
    (let* ([num-nodes (add1 (random 500))] ;; Number of nodes for the graph
           [num-edges (add1 (random 2000))] ;; Number of edges for the graph
           [graph     (generate-random-graph num-nodes num-edges)]
           [sccs      (calculate-sccs graph)]
           [nodes     (hash-keys sccs)])
      (display (format "Generated random graph of ~a nodes, ~a edges:\n~a\n"
                       num-nodes
                       num-edges
                       graph))
      ;;(with-handlers ([exn:fail? (displayln "Test failed!")])
        (foldl
         (λ (next-set cur-set)
           (if (not (set-empty? (set-intersect next-set cur-set)))
               (begin
                 (displayln
                  (format "Failure: ~a and ~a intersect." next-set cur-set))
                 (error "sets intersect"))
               (set-union next-set cur-set)))
         (set)
         nodes)
        (displayln "Test passed!"))))

