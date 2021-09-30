#lang racket

(require rackunit)
(require "../src/builtins.rkt")


(define add123i (get-func-for-builtin-with-extended-indices '(rel-version + 3 (1 2 3) comp)))
(define sub231i (get-func-for-builtin-with-extended-indices '(rel-version - 3 (2 3 1) comp)))

(define less12i (get-func-for-builtin-with-extended-indices '(rel-version < 2 (1 2) comp)))
(define less21i (get-func-for-builtin-with-extended-indices '(rel-version < 2 (2 1) comp)))

(define (tag x)
  `(integer ,x))
(define (materializer x) x)
(define/provide-test-suite builtins-tests
  (test-case
    "builtins with extended/reordered args"
    
    (check-equal? (less12i materializer (tag 1) (tag 2)) (set '()))
    (check-equal? (less12i materializer (tag 2) (tag 1)) (set))
    (check-equal? (less21i materializer (tag 1) (tag 2)) (set))
    (check-equal? (less21i materializer (tag 2) (tag 1)) (set '()))
    (check-equal? (add123i materializer (tag 1) (tag 2) (tag 3)) (set '()))
    (check-equal? (add123i materializer (tag 2) (tag 3) (tag 1)) (set))
    (check-equal? (add123i materializer (tag 1) (tag 3) (tag 2)) (set))
    (check-equal? (add123i materializer (tag 3) (tag 2) (tag 1)) (set))
    (check-equal? (sub231i materializer (tag 1) (tag 2) (tag 3)) (set '()))
    (check-equal? (sub231i materializer (tag 2) (tag 3) (tag 1)) (set))
    (check-equal? (sub231i materializer (tag 1) (tag 3) (tag 2)) (set))
    (check-equal? (sub231i materializer (tag 2) (tag 1) (tag 3)) (set '()))
    (check-equal? (sub231i materializer (tag 4) (tag -1) (tag  3)) (set '())) ))
