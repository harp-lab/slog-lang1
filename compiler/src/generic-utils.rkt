#lang racket
;; Utility functions that are independent of slog
(require racket/exn)

(provide 
    iterate-to-fixed-point
    many
    intercalate
    intercalate2
    app
    define/contract-cond
    memoize
    transitive-closure
    %if
    assert
    assert-pred
    assertion->contract
    flat-map
    list-split
    hash-transform
    hash-filter-keys
    project
    project-then-rest
    unproject-then-rest
    unproject-then-rest-zero-base
    string-replace-all
    findf-map
    hash-map-keys
    list->hash)

(require (for-syntax racket))
(require (for-syntax syntax/parse))

(define (iterate-to-fixed-point f x)
  (define fx (f x))
  (if (equal? fx x)
      fx
      (iterate-to-fixed-point f fx)))

;; To curry or not to curry, that should not be the question!
(define ((many f) x)
  (iterate-to-fixed-point f x))

(define (intercalate separator list)
  (match list
    [(cons x0 (cons x1 t)) (format "~a~a~a" x0 separator (intercalate separator (cons x1 t)))]
    [(cons x0 '()) x0]
    ['() ""]))

;; eg: (intercalate2 ", " '(1 2)) =  "1, 2, "
(define (intercalate2 separator list)
  (match list
    [(cons x0 t) (format "~a~a~a" x0 separator (intercalate2 separator t))]
    ['() ""]))

;; Partial function application. eg. (app / _ 2) == (λ (x) (/ x 2))
(define-syntax (app stx)
  (define (hole? x) (equal? (syntax->datum x) '_))
  (define data (syntax-e stx))
  (define func (second data))
  (define args (cddr data))
  (define holes (filter hole? args))
  (define lambda-params (map (λ (i hole-syntax) (datum->syntax hole-syntax (gensym (string->symbol (format "x~a_" i))))) (range (length holes)) holes))
  (define (func-args app-args lambda-params)
    (match app-args
      [(cons p rest-app-args) #:when (hole? p) (cons (car lambda-params) (func-args rest-app-args (cdr lambda-params)))]
      [(cons p rest-app-args)                  (cons (car app-args) (func-args rest-app-args lambda-params))]
      ['() '()]))
  (define the-func-args (func-args args lambda-params))
  #`(lambda #,lambda-params (#,func #,@the-func-args)))

(define-syntax (define/contract-cond stx)
  (define data (syntax-e stx))
  (match-define `(,def ,name ,contract ,body ...) data)
  (define check-cond-ident (datum->syntax stx 'check-cond-contracts))
  (match-define `(,(? (λ (arr) (equal? (syntax-e arr) '->))) ,arg-contracts ... ,res-contract) (syntax-e contract))
  (define (update-contract contract)
    #`(if/c (λ _args (#,check-cond-ident)) #,contract any/c))
  (define new-contract #`(-> #,@(map update-contract arg-contracts) #,(update-contract res-contract)))
  #`(define/contract #,name #,new-contract #,@body))

(define (memoize func)
  (define cache (make-hash))
  (lambda l (hash-ref! cache l (λ () (apply func l)))))

;; returns the transitive closure of a function f: a -> Set a
(define ((transitive-closure f) x)
  (define res (set))
  (define to-be-processed (set->list (f x)))
  (define (loop)
    (match to-be-processed
      ['() (void)]
      [(cons h t)
        (set! to-be-processed t)
        (when (not (set-member? res h)) 
            (set! to-be-processed (append to-be-processed (set->list (f h))))
            (set! res (set-add res h)))
        (loop)]))
  (loop)
  res)

;; compile-time if
(define-syntax (%if stx)
  (define parts (syntax-e stx))
  (if (eval (second parts))
    (third parts)
    (fourth parts)))

(define-syntax (assert stx)
  (syntax-case stx ()
    [(_ pred msg)
     (let ([assertion-string (datum->syntax #'pred (format "~a" (syntax->datum #'pred)))]
           [location-string (datum->syntax #'pred (format "~a" (syntax-source #'pred)))]
           [line-string (datum->syntax #'pred (format "~a" (syntax-line #'pred)))]
           [column-string (datum->syntax #'pred (format "~a" (syntax-column #'pred)))])
      #`(when (not pred)
          (error (format "Assertion at ~a:~a:~a failed: ~a\n~a" 
                  #,location-string 
                  #,line-string
                  #,column-string
                  #,assertion-string msg))))]
    [(_ pred) 
     #`(assert pred "")]))

(define (assert-pred pred . args)
  (when (not (apply pred args))
    (error (format "Assertion failed: ~a\ninput: ~a" (object-name pred) (intercalate ", " args)))))

(define (assertion->contract assertion #:name [name #f])
  (define name+ (cond
    [name name]
    [else (string->symbol (string-trim (symbol->string (object-name assertion)) "assert-" #:right? #f))]))
  
  (flat-contract-with-explanation	
    (λ args (with-handlers 
              ([exn:fail?
                (λ (e) (λ (blame) (raise-blame-error blame args 
                  (format "contract failed. expected ~a\n~a" name+ (exn->string e)))))]
               [(λ _ #t)
                (λ (e) (λ (blame) (raise-blame-error blame args 
                  (format "contract failed. expected ~a\n~a" name+ e))))])
    (match (apply assertion args)
      [#f (λ (blame) (raise-blame-error blame args))]
      [_ #t]))) #:name name+))

(define (flat-map f lst)
  (apply append (map f lst)))

;; like string-split, but for lists
(define/contract (list-split lst splitter)
  (list? (and/c list? (not/c empty?)) . -> . list?)
  (cond 
    [(list-prefix? splitter lst)
      (cons '() (list-split (list-tail lst (length splitter)) splitter))]
    [else (match lst
      [(cons x xs)
        (match-define (cons fst rest) (list-split xs splitter))
        (cons (cons x fst) rest)]
      ['() '(())])]))

;; returns a hash with values transformed by f
(define/contract (hash-transform f h)
  (procedure? hash? . -> . hash?)
  (foldl (λ (key accu) (hash-set accu key (f (hash-ref h key))))
         (hash)
         (hash-keys h)))
         
(define/contract (hash-map-keys f h)
  (procedure? hash? . -> . hash?)
  (foldl (λ (key accu)
            (hash-set accu (f key) (hash-ref h key)))
         (hash)
         (hash-keys h)))

(define/contract (hash-filter-keys f h)
  (procedure? hash? . -> . hash?)
  (foldl (λ (k accu) (if (f k) (hash-set accu k (hash-ref h k)) accu))
         (hash)
         (hash-keys h)))

;;
;; Utils for manipulating tuple orderings
;;

;; Project a tuple to a specific select order.
;; E.g., (project '(20 35 12 31) '(3 1)) = '(31 35)
(define (project tuple select-order)
  (reverse (foldl
            (lambda (column acc) (cons (list-ref tuple column) acc))
            '()
            select-order)))

;; Project a tuple to a specific select order, then give back the
;; rest of the columns in order. E.g.,
;;
;; (project-then-rest '(20 35 12 31) '(3 1)) = '(31 35 20 12)
(define (project-then-rest tuple select-order)
  (project tuple
           (append select-order
                   (remove* select-order (range (length tuple))))))

;; The inverse of `project-then-reset`. Assuming that `tuple` is in
;; `select-order` (but *crucially*, also assuming that its ID column
;; does not exist), reverse it. This is useful as head clauses are in
;; this order:
;;
;; ((unproject-then-rest '(1 3) 4) '(10 20 30 40)) = '(10 30 20 40)
;; ((unproject-then-rest '(4 1) 4) '(10 20 30 40)) = '(20 30 40 10)
(define (unproject-then-rest select-order tuple-len)
  (define len (length select-order))
  (define rng (range 1 (add1 tuple-len)))
  (define total-select-order
    (append select-order
            (remove* select-order rng)))
  (lambda (tuple) 
    (map (lambda (i) (list-ref tuple (index-of total-select-order i))) rng)))


;; ((unproject-then-rest-zero-base '(2 1) 3) '(a2 a1 id)) = '(id a1 a2)
(define ((unproject-then-rest-zero-base select-order tuple-len) tuple)
  ((unproject-then-rest (map add1 select-order) tuple-len) tuple))

(define (string-replace-all str . repl)
  (match repl
    [`(,x ,y ,rest ...) (apply string-replace-all (string-replace str x y) rest)]
    [`() str]
    [else (error (format "bad input to string-replace-all"))]))

(module+ test
  (require rackunit)
  (check-equal? ((unproject-then-rest-zero-base '(2 1) 3) '(a2 a1 id)) '(id a1 a2)))

;; like findf, but returns (f x) instead of x for the first x where (f x) =/= #f
(define (findf-map f xs)
  (match xs
    [(cons x rest)
      (define fx (f x))
      (if fx fx (findf-map f rest))]
    ['() #f]))

(define (list->hash list)
  (make-immutable-hash list))