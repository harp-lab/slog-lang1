#lang racket

(require
 racket/hash
 "generic-utils.rkt"
 "slog-params.rkt")
 
(provide
 all-builtin-names
 all-builtin-rel-arity->rel-selects
 arities-for-builtin
 bi-args-equal?
 builtin?
 builtins-as-rels
 aggregator?
 all-aggregators
 all-aggregator-names
 get-func-for-builtin-with-extended-indices
 get-func-for-aggregator-with-extended-indices
 arity-of-aggregated-rel
 all-aggregators)

(define builtins-start-time (current-inexact-milliseconds))

(define (fully-materialize materializer fact-id)
  (match (materializer fact-id)
    [(? number? n) n]
    [(? string? s) s]
    [`(,rel-name ,args ...) `(,rel-name ,@(map (app fully-materialize materializer _) args))]))

(define (binary-number-pred->builtin pred-name pred)
  (hash 
    `(rel-version ,pred-name 2 (1 2) comp)
    (lambda (materializer x y) 
     (match* (x y)
      [(`(integer ,x) `(integer ,y))
      (if (pred x y) 
          (set '())
          (set))]
      [(_ _) (set)]))))

(define (unary-pred->builtin pred-name pred)
  (hash 
    `(rel-version ,pred-name 1 (1) comp)
    (lambda (materializer x) 
      (if (pred (materializer x)) 
          (set '())
          (set)))))

(define (unary-number-func->builtin pred-name fun-x->y fun-y->x)
  (hash
   `(rel-version ,pred-name 2 (1) comp)
   (lambda (materializer x)
     (match x
       [`(integer ,x)
        (set (list `(integer ,(fun-x->y x))))]
       [else (set)]))

   `(rel-version ,pred-name 2 (2) comp)
   (lambda (materializer y)
     (match y
       [`(integer ,y)
        (set (list `(integer ,(fun-y->x y))))]
       [else (set)]))))

(define (binary-number-func->builtin pred-name fun-x-y->z fun-x-z->y fun-y-z->x)
  (define hash1
    (hash 
      `(rel-version ,pred-name 3 (1 2) comp)
      (lambda (materializer x y) 
      (match* (x y)
        [(`(integer ,x) `(integer ,y))
        (set (list `(integer ,(fun-x-y->z x y))))]
        [(_ _) (set)]))))
  (define hash2
    (if fun-x-z->y
      (hash
        `(rel-version ,pred-name 3 (1 3) comp)
        (lambda (materializer x z) 
        (match* (x z)
          [(`(integer ,x) `(integer ,z))
          (set (list `(integer ,(fun-x-z->y x z))))]
          [(_ _) (set)])))
      (hash)))
  (define hash3
    (if fun-y-z->x
      (hash
      `(rel-version ,pred-name 3 (2 3) comp)
      (lambda (materializer y z) 
      (match* (y z)
        [(`(integer ,y) `(integer ,z))
        (set (list `(integer ,(fun-y-z->x y z))))]
        [(_ _) (set)])))
      (hash)))
  (hash-union hash1 hash2 hash3))

(define if-then-else-builtin
  (hash
   `(rel-version if-then-else 4 (1 2 3) comp)
   (lambda (materializer condition then-branch else-branch)
     (match condition
       ['(integer 0)
        (set (list else-branch))]
       [else
        (set (list then-branch))]))))

(define range-builtin
  (hash
   `(rel-version range 3 (1 2) comp)
   (lambda (materializer lb ub)
     (match* (lb ub)
       [(`(integer ,lb) `(integer ,ub))
        (list->set (map (λ (i) (list `(integer ,i))) (range lb ub)))]
       [(_ _) (set)]))))

(define div-rem-builtin
  (hash
  `(rel-version div-rem 4 (1 2) comp)
  (lambda (materializer x y)
    (match* (x y)
    [(`(integer ,x) `(integer ,y))
     (set `((integer ,(quotient x y) (integer ,(remainder x y)))))]
    [(_ _) (set)]))))

(define regexp-match-builtin
  (hash
   `(rel-version regexp-match 2 (1 2) comp)
   (lambda (materializer pat s)
     (define pat-m (materializer pat))
     (define s-m (materializer s))
     (if (and (string? pat-m) (string? s-m) (regexp-match (pregexp pat-m) s-m))
         (set (list))
         (set)))))

(define string-append-builtins
  (foldl hash-union (hash)
   (map (λ (args-count)
          (hash
            `(rel-version string-append ,(add1 args-count) ,(range 1 (add1 args-count)) comp)
            (lambda (materializer . strs)
                  (define str-ms (map materializer strs))
                  (if (andmap string? str-ms)
                      (set (list `(uninterned ,(apply string-append str-ms))))
                      (set)))))
    (range 2 20))))

(define string-length-builtin
  (hash
   `(rel-version string-length 2 (1) comp)
   (lambda (materializer s)
     (define s-m (materializer s))
     (if (string? s-m)
         (set (list `(integer ,(string-length s-m))))
         (set)))))

(define string-contains-builtin
  (hash
  `(rel-version string-contains? 2 (1 2) comp)
  (lambda (materializer s contained)
    (define s-m (materializer s))
    (define contained-m (materializer contained))
    (if (string-contains? s-m contained-m)
      (set (list))
      (set)))))
(define string-split-builtin
  (hash
  `(rel-version string-split 3 (1 2) comp)
  (lambda (materializer str sep)
        (define str-m (materializer str))
        (define sep-m (materializer sep))
        (cond 
          [(and (string? str-m) (string? sep-m))
            (define res-strs (string-split str-m sep-m))
            (for/set ([res res-strs])
              (list `(uninterned ,res)))]
          [else (set)]))))

(define substring-builtin
  (hash
  `(rel-version substring 4 (1 2 3) comp)
  (lambda (materializer str start end)
    (define str-m (materializer str))
    (cond 
      [(string? str-m) 
       (match (cons start end)
          [(cons `(integer ,start-v) `(integer ,end-v))
            (cond
              [(and (>= end-v start-v) (>= start-v 0))
                (set (list `(uninterned ,(substring str-m start-v (min end-v (string-length str-m))))))]
              [else 
                (eprintf "bad input to substring: ~a, ~a, ~a \n" str-m start-v end-v)
                (set)])]
          [else (set)])]
      [else
       (set)]))
  `(rel-version substring 3 (1 2) comp)
  (lambda (materializer str start)
    (define str-m (materializer str))
    (cond 
      [(string? str-m) 
       (match start
          [`(integer ,start-v)
           (set (list `(uninterned ,(substring str-m start-v))))]
          [else (set)])]
      [else
       (set)]))))

;; mostly for testing out builtins with nested facts...
(define (get-depth sexpr)
    (match sexpr
      [(list sub-exprs ...) (add1 (apply max (map get-depth sub-exprs)))]
      [else 0]))
(define depth-builtin
  (hash
   `(rel-version depth 2 (1) comp)
   (λ (materializer x) (set (list `(integer ,(get-depth (fully-materialize materializer x))))))))

(define unary-number-func-builtins
  (hash 'add1 (list add1 sub1)
        'sub1 (list sub1 add1)))

(define binary-number-func-builtins
  (hash 'max (list max #f #f)
        'min (list min #f #f)

        '+ (list + (λ (x z) (- z x)) (λ (y z) (- z y)))
        '- (list - (λ (x z) (- x z)) (λ (y z) (+ z y)))
        '* (list * #f #f)
        '/ (list quotient #f #f)
        '% (list remainder #f #f)
        'expt (list expt #f #f)
        'bitwise-and (list bitwise-and #f #f)
        'bitwise-or (list bitwise-ior #f #f)
        'bitwise-ior (list bitwise-ior #f #f)
        'bitwise-xor (list bitwise-xor #f #f)))

(define binary-number-pred-builtins
  (hash '< <
        '<= <=
        '> >
        '>= >=))

(define unary-pred-builtins
  (hash 'number? number?
        'string? string?
        'not-number? (compose not number?)
        'not-string? (compose not string?)))

(define not-equals-builtin
  (let ([func (λ (materializer x y)
          (if (bi-args-equal? materializer x y)
              (set)
              (set '())))])
    
  (hash '(rel-version =/= 2 (1 2) comp) func
        '(rel-version  /= 2 (1 2) comp) func)))

(define equals-builtin
  (hash '(rel-version = 2 (1) comp)
        (λ (materializer x) (set (list x)))
        
        '(rel-version = 2 (2) comp)
        (λ (materializer y) (set (list y)))))

(define (reorder-list l l-order desired-order)
    (map (λ (desired-order-item) (list-ref l (index-of l-order desired-order-item))) desired-order))

(define (fn-rel->index-perms rel func)
  (match-define `(rel-version ,pred-name ,arity ,indices-list ,ver) rel)
  (foldl (λ (perm accu)
           (hash-set accu `(rel-version ,pred-name ,arity ,perm ,ver) 
                     #;func
                     (λ (materializer . l) (apply func materializer (reorder-list l perm indices-list)))))
         (hash)
         (permutations indices-list)))


(define (sat-builtins)

  (define context
    '((define-fun cmp ((x Int) (y Int)) Int
        (ite (> x y) 1
             (ite (= x y) 0
                  -1)))
      (define-fun less-func ((x Int) (y Int)) Int
        (ite (< x y) 1 0))))
  (define dprintf (λ args
    (when (slog-debug-smt) (apply printf args))))
  (define (slog-formula->smt-formula f)
    (define identifiers (make-hash))
    ; https://rise4fun.com/z3/tutorial
    (define (->smt f)
      (match f
        [`(s-and ,fs ...) `(and ,@(map ->smt fs))]
        [`(s-or ,fs ...) `(or ,@(map ->smt fs))]
        [`(s=> ,f1 ,f2) `(=> ,(->smt f1) ,(->smt f2))]
        [`(s-not ,f1) `(not ,(->smt f1))]
        [`(s= ,f1 ,f2) `(= ,(->smt f1) ,(->smt f2))]
        [`(s/= ,f1 ,f2) `(not (= ,(->smt f1) ,(->smt f2)))]
        [`(s+ ,f1 ,f2) `(+ ,(->smt f1) ,(->smt f2))]
        [`(s* ,f1 ,f2) `(* ,(->smt f1) ,(->smt f2))]
        [`(s-div ,f1 ,f2) `(div ,(->smt f1) ,(->smt f2))]
        [`(s-rem ,f1 ,f2) `(rem ,(->smt f1) ,(->smt f2))]
        [`(s- ,f1) `(- ,(->smt f1))]
        [`(s-cmp ,f1 ,f2) `(cmp ,(->smt f1) ,(->smt f2))]
        [`(s-less-func ,f1 ,f2) `(less-func ,(->smt f1) ,(->smt f2))]
        [`(s-ref ,id) (hash-ref! identifiers id gensym)]
        [`(s-lit ,n) n]
        [`(s-true) 'true]
        [`(s-false) 'false]
        [(? number? n) n]))
    (define smt-formulas (->smt f))
    (define identifier-definitions
      (map (λ (id) `(declare-const ,id Int)) (hash-values identifiers)))
    `(,@identifier-definitions (assert ,smt-formulas)))

  (define z3-process-started #f)
  (define-values (z3-proc z3-out z3-in z3-err) (values #f #f #f #f))
  (define (start-z3-process!)
    (cond 
      [z3-process-started (void)]
      [else
        (define z3-output-port (if ((and/c file-stream-port? output-port?) (current-output-port)) (current-output-port) #f))
        (set!-values (z3-proc z3-out z3-in z3-err) (subprocess #f #f z3-output-port (find-executable-path "z3") "-in"))
        (for ([ctx context]) (displayln ctx z3-in))
        (dprintf "z3 status: ~a\n" (subprocess-status z3-proc))
        (set! z3-process-started #t)]))

  (define z3-calls 0)
  (define (sat0 formula)
    (start-z3-process!)
    (define smt-formula (slog-formula->smt-formula formula))
    (define input (append '((push)) smt-formula '((check-sat) (pop))))
    (define input-str (foldl (λ (line res) (format "~a~a\n" res line)) "" input))
    (dprintf "z3 status: ~a\n" (subprocess-status z3-proc))
    (dprintf "z3 input:\n~a\n" input-str)
    (displayln input-str z3-in)
    (flush-output z3-in)
    (define res (read-line z3-out 'any))
    (set! z3-calls (add1 z3-calls))
    (dprintf "z3 output: ~a, total z3 calls: ~a\n" res z3-calls)
    (equal? res "sat"))

  (define sat (memoize sat0))
  (define sat-builtins
    (hash
     `(rel-version sat 1 (1) comp)
     (λ (materializer f) (if (sat (fully-materialize materializer f)) (set '()) (set)))

     `(rel-version unsat 1 (1) comp)
     (λ (materializer f) (if (sat (fully-materialize materializer f)) (set) (set '())))))
  sat-builtins)


(define (bi-args-equal? materializer l r)
  (or (equal? l r)
      (equal? (materializer l) (materializer r))))

(define (builtin-func-extended-to-new-args bi-func indices extended-indices)
  (λ (materializer . inp)
    (define res-set (apply bi-func materializer (take inp (length indices))))
    (foldl (λ (res accu)
            (define rest-matches (andmap 
                                  (app bi-args-equal? materializer _ _)
                                  (drop inp (length indices)) 
                                  (take res (- (length extended-indices) (length indices)))))
            (if rest-matches
                (set-add accu (drop res (- (length extended-indices) (length indices))))
                accu))
          (set)
          (set->list res-set))))

(define (extend-builtin-to-more-args bi-rel-version bi-func)
  (match-define `(rel-version ,pred-name ,arity ,indices ,version) bi-rel-version)
  (define indices-diff (filter (λ (i) (not (member i indices))) (range 1 (add1 arity))))
  (define extended-indices-list (map (λ (i) (append indices (take indices-diff i))) (range 0 (add1 (length indices-diff))))) 
  (foldl (λ (extended-indices accu)
           (define new-bi-func (builtin-func-extended-to-new-args bi-func indices extended-indices))
           (hash-set accu `(rel-version ,pred-name ,arity ,extended-indices ,version) new-bi-func))
         (hash)
         extended-indices-list))


;; rel-versions of all builtins (without arg reordering)
(define builtins-as-rels
  (let* ([res
          (hash-union
           (foldl (λ (bi accu) (hash-union (apply unary-number-func->builtin (cons bi (hash-ref unary-number-func-builtins bi))) accu))
                  (hash)
                  (hash-keys unary-number-func-builtins))
           (foldl (λ (bi accu) (hash-union (binary-number-pred->builtin bi (hash-ref binary-number-pred-builtins bi)) accu))
                  (hash)
                  (hash-keys binary-number-pred-builtins))
           (foldl (λ (bi accu) (hash-union (apply binary-number-func->builtin (cons bi (hash-ref binary-number-func-builtins bi))) accu))
                  (hash)
                  (hash-keys binary-number-func-builtins))
           (foldl (λ (bi accu) (hash-union (unary-pred->builtin bi (hash-ref unary-pred-builtins bi)) accu))
                  (hash)
                  (hash-keys unary-pred-builtins))
           if-then-else-builtin
           range-builtin
           div-rem-builtin
           depth-builtin
           not-equals-builtin
           equals-builtin
           regexp-match-builtin
           string-append-builtins
           string-length-builtin
           string-split-builtin
           string-contains-builtin
           substring-builtin
           (sat-builtins))]
         [res-with-extended-args 
          (foldl (λ (rel accu) (hash-union accu (extend-builtin-to-more-args rel (hash-ref res rel)) #:combine (λ (x y) x)))
                 (hash)
                 (hash-keys res))])
    res-with-extended-args))

; (printf "all builtins: ~a" (intercalate "\n" (hash-keys builtins-as-rels)))

(define all-builtin-names
  (list->set (map second (hash-keys builtins-as-rels))))

(define all-builtin-rel-arity->rel-selects
  (foldl (λ (bi-rel-version accu)
            (match-define `(rel-version ,rel-name ,arity ,indices ,version) bi-rel-version)
            (define rel-arity `(rel-arity ,rel-name ,arity comp))
            (define rel-select `(rel-select ,rel-name ,arity ,indices comp))
            (hash-set accu rel-arity (set-add (hash-ref accu rel-arity set) rel-select)))
         (hash)
         (hash-keys builtins-as-rels)))

(define (arities-for-builtin name)
  (sort (filter-map (λ (rel-arity)
                (match-define `(rel-arity ,n ,ar comp) rel-arity)
                (if (equal? n name) ar #f))
              (hash-keys all-builtin-rel-arity->rel-selects))
        <))

(define (builtin? bi-name)
 (set-member? all-builtin-names bi-name))

(define (_get-func-for-builtin-with-extended-indices requested-bi)
 (match-define `(rel-version ,name ,arity ,indices comp) requested-bi)
 (define matching
  (findf (λ (bi-func)
            (match-define `(rel-version ,bi-name ,bi-arity ,bi-indices comp) (car bi-func))
            (equal? (list bi-name bi-arity (list->set bi-indices))
                    (list name arity (list->set indices))))
          (hash->list builtins-as-rels)))
 (when (not matching)
  (error (format "no matching builtin for ~a\n" requested-bi))) 
 (match-define (cons matching-bi matching-bi-func) matching)
  (match-define `(rel-version ,matching-name ,matching-arity ,matching-indices comp) matching-bi)
  (cond 
    [(equal? matching-indices indices) matching-bi-func]
    [else
      (λ (materializer . l) (apply matching-bi-func materializer (reorder-list l indices matching-indices)))]))

(define get-func-for-builtin-with-extended-indices (memoize _get-func-for-builtin-with-extended-indices))

;;------------------------- NEW Aggregators -------------------------
;;-------------------------------------------------------------------

(define (num-agg-input-tuple-value tuple)
  (match tuple 
    [`((integer ,n)) n]
    [`(,(? integer? n)) n]
    [else #f]))

(define new-sum-aggregator
  `(1 1
    ,(λ (tuples)
      ; (printf "sum input tuples: ~a\n" tuples)
      (define res (foldl + 0 (filter-map num-agg-input-tuple-value (set->list tuples))))
      (set `((integer ,res))))))

(define new-negation-aggregator
  `(0 0
      ,(λ (tuples)
        ; (printf "negation input tuples: ~a\n" tuples)
        (if (set-empty? tuples)
          (set `())
          (set)))))

(define new-count-aggregator
  `(0 1
    ,(λ (tuples)
      (define res (set-count tuples))
      (set `((integer ,res))))))

(define (new-extremum-aggregator join)
  `(1 1
    ,(λ (tuples)
      (define numbers (filter-map num-agg-input-tuple-value tuples))
      (cond 
        [(empty? numbers) (set)]
        [else
          (match-define (cons head tail) numbers)
          (define res (foldl join head tail))
          (set `((integer ,res)))]))))

(define all-aggregators
  (hash
    'sum new-sum-aggregator
    'count new-count-aggregator
    'count-by new-count-aggregator
    '~ new-negation-aggregator
    'minimum (new-extremum-aggregator min)
    'maximum (new-extremum-aggregator max)))

(define (extend-agg-func-to-new+reordered-indices func indices extended-indices)
  (define new-indices-list (set->list (set-subtract (list->set extended-indices) (list->set indices))))
  (define indices+new-indices-list (append indices new-indices-list))
  (λ (rel) 
    (define itermediate-func (builtin-func-extended-to-new-args (func rel) indices indices+new-indices-list))
    (λ (materializer . l) (apply itermediate-func materializer (reorder-list l extended-indices indices+new-indices-list)))))

(define (get-func-for-aggregator-with-extended-indices requested-agg-spec)
  (match-define `(aggregator-spec ,req-agg-name ,req-agg-arity ,req-agg-indices ,req-rel-arity ,req-rel-indices) requested-agg-spec)
  (define matching-aggregators
    (filter-map (λ (spec-func)
              (match-define `(aggregator-spec ,agg-name ,agg-arity ,agg-indices ,rel-arity ,rel-indices) (car spec-func))
              (cond
                ;; Hack to allow negation to make negation work with rel-selects with unordered indices
                ;; The design of aggregators likely needs to be rethought.
                [(and (equal? agg-name '~)
                      (equal? (list agg-name agg-arity rel-arity (list->set rel-indices)) (list req-agg-name req-agg-arity req-rel-arity (list->set req-rel-indices)))
                      (subset? (list->set agg-indices) (list->set req-agg-indices)))
                 spec-func]
                [(and (equal? (list agg-name agg-arity rel-arity rel-indices) (list req-agg-name req-agg-arity req-rel-arity req-rel-indices))
                      (subset? (list->set agg-indices) (list->set req-agg-indices)))
                 spec-func]
                [else #f]))
            (hash->list all-aggregators)))
  (assert (not (empty? matching-aggregators)) (format "no func for aggregator-sepc: ~a\n" requested-agg-spec))
  (match-define (cons matching-spec matching-func) (car matching-aggregators))
  (match-define `(aggregator-spec ,_ ,_ ,matching-indices ,_ ,_) matching-spec)
  (define new-func (extend-agg-func-to-new+reordered-indices matching-func matching-indices req-agg-indices))
  new-func)

; (printf "all-aggregators: \n~a\n" (pretty-format (hash-keys all-aggregators)))
(define all-aggregator-names
  (list->set (map second (hash-keys all-aggregators))))

(define (aggregator? name)
  (set-member? all-aggregator-names name))
;(printf "builtins: ~a\n" (intercalate ", " (set->list all-builtin-names)))

(define builtins-took (- (current-inexact-milliseconds) builtins-start-time))
(when (> builtins-took 100)
  (printf "evluating builtins.rkt took ~a ms!\n" (~r builtins-took #:precision 0)))