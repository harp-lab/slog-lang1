#lang racket

;; Slog compilation
;; Copyright (c) Thomas Gilray, et al, see License.md

(provide slog-compile
         slog-compile-cpp
         extend-direct-cpp-builtin-to-new-args
         generate-cpp-lambda-for-rule-with-builtin
         generate-cpp-lambda-for-rule-with-callback-builtin
         generate-cpp-lambda-for-computational-join
         generate-cpp-lambda-for-computational-copy
         generate-lambda-for-computational-relation-rule
         generate-cpp-func-for-computational-relation-rules
         generate-cpp-lambda-for-rule-with-direct-builtin)

(require
 "lang-predicates.rkt"
 "slog-params.rkt"
 "utils.rkt"
 "builtins.rkt"
 "slog-params.rkt"
 
 ;;; Passes ;;;
 ; Input: source-tree?
 "organize-pass.rkt"
 ; IR 1: ir-flat?
 "static-unification-pass.rkt"
 "optimization-pass.rkt"
 "remove-implicit-joins-pass.rkt"
 "fix-arities-pass.rkt"
 ; IR 2: ir-fixed?
 "partitioning-pass.rkt"
 ; IR 3: ir-small?
 "split-selections-pass.rkt"
 ; IR 4: ir-select?
 "scc-pass.rkt"
 ; IR 5: ir-scc?
 "incrementalize-pass.rkt"
 ; IR 6: ir-incremental?
 )

(require racket/runtime-path)
(define-runtime-path HERE ".")
(define (get-path path) (build-path HERE path))


; Run an input source-tree? through all primary compiler passes
(define (slog-compile source-tree)
  (define optimize-pass (if (slog-optimize) optimization-pass identity))
  (define (time op)
    (λ args
      (define before (current-inexact-milliseconds))
      (define res (apply op args))
      (define elapsed (- (current-inexact-milliseconds) before))
      (when (> elapsed 5000)
        (printf "~a took ~a ms!\n" (object-name op) (~r elapsed #:precision 0)))
      res))
  (parameterize ([current-source-tree source-tree])
    ((compose #;print-ir-incremental (time incrementalize-pass))
     ((time scc-pass)
      ((compose #;print-ir-select (time split-selections-pass))
       ((compose #;print-ir-small (time partitioning-pass))
        ((compose #;print-ir-fixed (time fix-arities-pass))
         ((compose #;print-ir-flat (time remove-implicit-joins-pass))
          ((time optimize-pass)
           ((time static-unification-pass)
            ((time organize-pass)
              source-tree)))))))))))
    ; (printf "static-unification-pass-res: \n~a\n" (intercalate "\n" (map strip-prov (hash-keys (third static-unification-pass-res)))))


; Complete compilation to C++
;; returns (cons global-definitions program-text)
(define (slog-compile-cpp program input-database output-database)
  (match-define `(ir-incremental ,ir-scc ,scc-graph ,scc-map ,comp-rules-h) program)
  (define manifest (read-manifest (format "~a/manifest" input-database)))
  (match-define `(manifest (relations ,relations) (strings "$strings.csv" ,n)) manifest)
  (define all-rel-selects
    (foldl (lambda (scc st)
              (match scc
                    [`(scc ,looping ,rel-h ,rules-h)
                      (foldl (lambda (rel st)
                              (match rel
                                      [`(rel-arity ,nm ,arity ,kind)
                                      (define sel-st (third (hash-ref rel-h rel)))
                                      (foldl (lambda (sel st)
                                                (set-add st `(rel-select ,nm ,arity ,sel ,kind)))
                                              st
                                              (set->list sel-st))]))
                            st
                            (hash-keys rel-h))]))
            (set)
  (hash-values scc-map)))
  (define comp-rules-by-rel
    (foldl (λ (rule accu)
            (match-define `(crule ,head ,bodys ...) rule)
            (match-define `(,head-rel ,hvars ...) (strip-prov head))
            (hash-set accu head-rel (set-add (hash-ref accu head-rel (set)) rule)))
          (hash)
          (hash-keys comp-rules-h)))
  (define comp-rels-func-names
    (foldl (λ (rel accu) (hash-set accu rel (rel->name rel))) (hash) (hash-keys comp-rules-by-rel)))
  (define rel-txt
    (foldl (lambda (rel-sel rel-txt)
             (match-define `(rel-select ,rel-name ,rel-arity ,sel ,kind) rel-sel)
             ;; find the appropriate rel-select by name
             (define (matches manifest-relation)
               (match manifest-relation
                 [`(relation ,r ,a . ,_)
                  (and (equal? r rel-name) (equal? a rel-arity))]
                 [_ #f]))
             (define maybe-relation (filter matches relations))
             (when (not (= (length maybe-relation) 1))
               (error (format "Could not find an appropriate relation ~a ~a in manifest (either 0 or >1 possible candidates)" rel-name rel-arity)))
             (define relation (first maybe-relation))
             (match-define `(relation ,_ ,_ ,rid ,_ ,index ,data ,size) relation)
             (string-append rel-txt
                            (format "relation* ~a = new relation(~a, ~a, ~a, ~a, \"~a\", slog_input_dir + \"/~a\", FULL);\n"
                                    (rel->name rel-sel)
                                    (length (rel->sel rel-sel))
                                    (if (and (not (member 0 (rel->sel rel-sel))) (= (length (rel->sel rel-sel)) (rel->arity rel-sel))) "true" "false")
                                    (rel->arity rel-sel)
                                    rid
                                    (rel->name rel-sel)
                                    (if (and (not (member 0 (rel->sel rel-sel))) (= (length (rel->sel rel-sel)) (rel->arity rel-sel)))
                                        (format "~a_~a" (rel->name rel-sel) rel-arity)
                                        (format "~a_nc_~a" (rel->name rel-sel) rel-arity))
                                    )))
           ""
           (set->list all-rel-selects)))
  (define scc-txt-h
    (foldl (lambda (scc txt-h)
             (hash-set txt-h scc (slog-compile-scc-to-cpp scc (hash-ref scc-map scc) comp-rels-func-names)))
           (hash)
           (hash-keys scc-map)))
  ;(display scc-graph)
  (define prog-txt
    (string-append  rel-txt
                    (apply string-append (map cdr (hash-values scc-txt-h)))
                    "\n"
                    "LIE* lie = new LIE();\n"
                    (foldl (lambda (rel-sel txt)
                            (string-append txt (format "lie->add_relation(~a);\n" (rel->name rel-sel))))
                          ""
                          (set->list all-rel-selects))
                    (foldl (lambda (scc txt)
                            (string-append txt (format "lie->add_scc(~a);\n" (car (hash-ref scc-txt-h scc)))))
                          ""
                          (hash-keys scc-txt-h))
                    (foldl (lambda (scc txt)
                            (foldl (lambda (scc2 txt)
                                      (string-append txt (format "lie->add_scc_dependance(~a, ~a);\n"
                                                                (car (hash-ref scc-txt-h scc))
                                                                (car (hash-ref scc-txt-h scc2)))))
                                    txt
                                    (set->list (hash-ref scc-graph scc (thunk (error "missing scc?"))))))
                          ""
                          (hash-keys scc-graph))
                    "\n"))
  (define comp-rule-funcs
    (foldl (λ (rel accu)
              (define comp-cpp-func 
                (generate-cpp-func-for-computational-relation-rules (set->list (hash-ref comp-rules-by-rel rel)) comp-rels-func-names) )
              (format "~a\n//func for comp rel ~a:\n~a" accu rel comp-cpp-func))
           ""
           (hash-keys comp-rules-by-rel)))
  ; (printf "comp-rule-funcs: \n~a" comp-rule-funcs)
  (cons comp-rule-funcs prog-txt))

(define (rel->name rel)
  (->cpp-ident
    (match rel
         [`(rel-select ,nm ,arity ,sel ,kind)
          (format "rel_~a_~a_~a" (string-join (string-split (symbol->string nm) "-") "_") arity (string-join (map number->string sel) "_"))]
         
         [`(rel-version ,nm ,arity ,sel ,ver)
          (format "rel_~a_~a_~a" (string-join (string-split (symbol->string nm) "-") "_") arity (string-join (map number->string sel) "_"))])))

(define (rel->sel rel)
  (match rel
         [`(rel-select ,nm ,arity ,sel ,kind)
          sel]
         [`(rel-version ,nm ,arity ,sel ,ver)
          sel]))

(define (rel->arity rel)
  (match rel
         [`(rel-select ,nm ,arity ,sel ,kind)
          arity]
         [`(rel-version ,nm ,arity ,sel ,ver)
          arity]))

(define (slog-compile-scc-to-cpp scc-id scc comp-rels-func-names)
  (define (all-needed-indices rules-h)
    (foldl (lambda (rule st)
             (match rule 
                [`(srule (prov ((prov ,(? rel-select? rel-sel) ,_) ,hvars ...) ,_)
                          (prov ((prov ,(? rel-version? brel-vers) ,_) ,bvars0 ...) ,_) ...)
                  (define b-rel-sels 
                    (filter-map  
                      (λ (rel-ver) (if (db-rel-version? rel-ver) `(rel-select ,@(drop (take rel-ver 4) 1) db) #f))
                      brel-vers))
                  (set-union st (set rel-sel) (list->set b-rel-sels))]
                [`(arule ,(? ir-incremental-hclause? `(prov ((prov ,(? rel-select? rel-sel) ,_) ,hvars ...) ,_))
                          ,(? ir-incremental-bclause? `(prov ((prov ,(? rel-version? rel-ver) ,_) ,bvars ...) ,_)))
                  (set-add (set-add st rel-sel)
                          `(rel-select ,@(take (drop rel-ver 1) 3) db))]))
           (set)
           (hash-keys rules-h)))
  (match scc
         [`(scc ,looping ,rel-h ,rules-h)
          (define name (symbol->string (gensym 'scc)))
          (cons name
                (string-append (format "\nRAM* ~a = new RAM(~a, ~a);\n"
                                       name
                                       (if (eq? looping 'looping) "true" "false")
                                       scc-id)
                               (foldl (lambda (rel-sel txt)
                                        (match-define `(rel-select ,rel-name ,rel-arity ,rel-selections ,rel-kind) rel-sel)
                                        (string-append txt (format "~a->add_relation(~a, ~a);\n"
                                                                   name
                                                                   (rel->name rel-sel)
                                                                   (let ([rel-a `(rel-arity ,rel-name ,rel-arity ,rel-kind)])
                                                                     (if (eq? 'dynamic (first (hash-ref rel-h rel-a)))
                                                                         "true"
                                                                         "false")))))
                                      ""
                                      (set->list (all-needed-indices rules-h)))
                               (foldl (lambda (rule txt)
                                        (string-append txt
                                                       (format "~a->add_rule(~a);\n"
                                                               name
                                                               (slog-compile-rule-to-cpp rule comp-rels-func-names))))
                                      ""
                                      (hash-keys rules-h))))]))

(define (slog-compile-rule-to-cpp rule comp-rels-func-names)
  (define (comp-rel-name? op)
    (or (builtin? op)
        (ormap (λ (rel-select) 
                  (match-define `(rel-select ,name ,arity ,ind ,comp) rel-select)
                  (equal? name op))
               (hash-keys comp-rels-func-names))))
  (match rule
         [`(srule ,(? ir-incremental-hclause? `(prov ((prov ,(? rel-select? rel-sel) ,_) ,hvars ...) ,_))
                  ,(? ir-incremental-bclause? `(prov ((prov ,(? rel-version? rel-ver0) ,_) ,bvars0 ...) ,_))
                  ,(? ir-incremental-bclause? `(prov ((prov (rel-version ,op ,op-arity ,op-indices ,ver) ,_) ,bvars1 ...) ,_)))
           #:when (comp-or-agg-rel-kind? ver)
          (define prefix-vars (let loop ([bvars0 (map second bvars0)] [bvars1 (map second bvars1)])
                                (if (equal? (first bvars0) (first bvars1))
                                    (cons (first bvars0) (loop (cdr bvars0) (cdr bvars1)))
                                    '())))
          (define bi-rel-select `(rel-select ,op ,op-arity ,op-indices comp))
          (define comp-rel-cpp-func
            (cond [(builtin? op) (generate-cpp-lambda-for-rule-with-builtin rule)]
                  [else (hash-ref comp-rels-func-names bi-rel-select)]))
          ; lambda signature: (const u64* data, u64* output) -> int
          (format "new parallel_copy_generate(~a, ~a, ~a, ~a)"
                      (rel->name rel-sel)
                      (rel->name `(rel-select ,@(take (drop rel-ver0 1) 3) db))
                      (match (last rel-ver0) ['total "FULL"] ['delta "DELTA"] ['new "NEW"])
                      comp-rel-cpp-func)]

         [`(srule ,(? ir-incremental-hclause? `(prov ((prov ,(? rel-select? rel-sel) ,_) ,hvars ...) ,_))
                  ,(? ir-incremental-bclause? `(prov ((prov ,(? rel-version? rel-ver0) ,_) ,bvars0 ...) ,_))
                  ,(? ir-incremental-bclause? `(prov ((prov ,(? rel-version? rel-ver1) ,_) ,bvars1 ...) ,_)))
          (define prefix-vars (let loop ([bvars0 (map second bvars0)] [bvars1 (map second bvars1)])
                                (if (and (cons? bvars0) (cons? bvars1) (equal? (first bvars0) (first bvars1)))
                                    (cons (first bvars0) (loop (cdr bvars0) (cdr bvars1)))
                                    '())))
          (format "new parallel_join(~a, ~a, ~a, ~a, ~a, ~a)"
                  (rel->name rel-sel)
                  (rel->name `(rel-select ,@(take (drop rel-ver0 1) 3) db))
                  (match (last rel-ver0) ['total "FULL"] ['delta "DELTA"] ['new "NEW"])
                  (rel->name `(rel-select ,@(take (drop rel-ver1 1) 3) db))
                  (match (last rel-ver1) ['total "FULL"] ['delta "DELTA"] ['new "NEW"])
                  (compute-reordering-cpp (map second hvars)
                                          (append prefix-vars
                                                  (map second (drop bvars0 (length prefix-vars)))
                                                  (map second (drop bvars1 (length prefix-vars))))))]
         [`(srule ,(? ir-incremental-hclause? `(prov ((prov ,(? rel-select? rel-sel) ,_) ,hvars ...) ,_))
                  ,(? ir-incremental-bclause? `(prov ((prov ,(? rel-version? rel-ver) ,_) ,bvars ...) ,_)))
          (format "new parallel_copy(~a, ~a, ~a, ~a)"
                  (rel->name rel-sel)
                  (rel->name `(rel-select ,@(take (drop rel-ver 1) 3) db))
                  (match (last rel-ver) ['total "FULL"] ['delta "DELTA"] ['new "NEW"])
                  (compute-reordering-cpp (map second hvars) (map second bvars)))]
         [`(srule ,(? ir-incremental-hclause? `(prov ((prov ,(? rel-select? rel-sel) ,_) ,hvars ...) ,_)))
          (format "new fact(~a, ~a)"
                  (rel->name rel-sel)
                  (format "{~a}" (intercalate ", " (map (compose literal->cpp-val strip-prov) hvars))))]
         [`(arule ,(? ir-incremental-hclause? `(prov ((prov ,(? rel-select? rel-sel) ,_) ,hvars ...) ,_))
                  ,(? ir-incremental-bclause? `(prov ((prov ,(? rel-version? rel-ver) ,_) ,bvars ...) ,_)))
          (format "new parallel_acopy(~a, ~a, ~a, ~a)"
                  (rel->name rel-sel)
                  (rel->name `(rel-select ,@(take (drop rel-ver 1) 3) db))
                  (match (last rel-ver) ['total "FULL"] ['delta "DELTA"] ['new "NEW"])
                  (compute-reordering-cpp (map second hvars) (map second bvars)))]))

(define (literal->cpp-val lit)
  (format "n2d(~a)" lit))

(define (compute-reordering-cpp to-xs from-xs)
  (define (index-of x [xs from-xs] [i 0])
    (if (null? xs)
        (error (format "No index-of: ~a in ~a (~a)" x from-xs to-xs))
        (if (equal? x (car xs))
            i
            (index-of x (cdr xs) (+ 1 i)))))
  (if (null? to-xs)
      "{}"
      (string-append "{"
                     (foldl (lambda (to-x txt)
                              (string-append txt ", " (number->string (index-of to-x))))
                            (number->string (index-of (first to-xs)))
                            (rest to-xs))
                     "}")))


(define (int-list->cpp-array lst)
  (format "std::array<int, ~a> { ~a }" (length lst) (intercalate ", " lst)))

(define (generate-cpp-lambda-for-rule-with-builtin r)
  (match-define `(srule (,rel-sel ,hvars ...)
                        (,rel-ver0 ,bvars0 ...)
                        ((rel-version ,bi-op ,op-arity ,new-indices ,ver) ,bvars1 ...)) (strip-prov r))
  (define all-builtins (append (map (app append _ '(direct)) direct-builtins)
                               (map (app append _ '(callback)) callback-builtins)))
  (define matching 
    (filter (λ (bi-md) 
            (match-define `(,name ,arity ,indices ,cpp-func-name ,kind) bi-md)
            (and (equal? name bi-op) 
                 (equal? op-arity arity)
                 (subset? (list->set indices) (list->set new-indices))))
           all-builtins))
  (cond 
    [(empty? matching) (error (format "no suitable implementation exists for builtin ~a" bi-op))]
    [else
      (define best-match 
        (argmax (λ (bi-md)
                  (match-define `(,name ,arity ,indices ,cpp-func-name ,kind) bi-md)
                  (length indices))
                 matching))
      (match best-match
        [`(,name ,arity ,indices ,cpp-func-name direct)
          (generate-cpp-lambda-for-rule-with-direct-builtin r indices cpp-func-name)]
        [`(,name ,arity ,indices ,cpp-func-name callback)
          (generate-cpp-lambda-for-rule-with-callback-builtin r indices cpp-func-name)])])) 
    

(define (generate-cpp-lambda-for-rule-with-direct-builtin r indices cpp-func-name)
  (match-define `(srule (,rel-sel ,hvars ...)
                        (,rel-ver0 ,bvars0 ...)
                        ((rel-version ,(? builtin? bi-op) ,arity ,new-indices ,ver) ,bvars1 ...)) (strip-prov r))
  
  (define new-tuple-index-to-old-tuple-index-mapping (map-new-tuple-index-to-old-tuple-index arity new-indices indices))
  (set! indices (map sub1 indices))
  (set! new-indices (map sub1 new-indices))
  (define output-indices (filter (λ (i) (not (member i indices))) (range 0 arity)))

  (string-replace-all 
    "[](const u64* data, u64* output) -> int{
      auto args_for_old_bi = std::array<u64, [old-indices-size]> {[populate-args-for-old-bi-code]};
      auto old_bi_res = [cpp-func-name](args_for_old_bi.data());

      auto tuples_count = 0;
      for (auto bi_output_tuple: old_bi_res) {
        bool compatible = [check-compatibility-code];
        if (! compatible) continue;
        
        auto head_tuple = output;
        [head-tuple-populating-code]
        tuples_count++;
        output += [head-tuple-size];
      }
      return tuples_count;
    }"

    "[head-tuple-size]" (~a (length hvars))
    "[old-indices-size]" (~a (length indices))
    "[cpp-func-name]" cpp-func-name
    "[populate-args-for-old-bi-code]"
    (intercalate ", " (map (λ (i) (format "data[~a]" (index-of new-indices (list-ref indices i)))) (range 0 (length indices))))
    "[check-compatibility-code]"
    (intercalate " && " 
      (cons "true" (filter-map (λ (i) (let ([mapped-ind (index-of new-indices (list-ref output-indices i))]) 
                          (and mapped-ind (format "bi_output_tuple[~a] == data[~a]" i mapped-ind)))) 
                  (range 0 (length output-indices)))))
    "[head-tuple-populating-code]"
    (intercalate "\n" (map 
      (λ (i)
        (define rhs (match (list-ref hvars i)
          [(? number? x) (format "number_to_datum(~a)" x)]
          [(? string? str) (error (format "string literals in cpp compiler not supported yet!")) ] ;TODO
          [(? symbol? var) #:when (member var bvars0)
            (format "data[~a]" (index-of bvars0 var))]
          [(? symbol? var) #:when (member var bvars1)
            (define bi-arg-index (index-of bvars1 var))
            (define index-in-old-bi-tuple (list-ref new-tuple-index-to-old-tuple-index-mapping bi-arg-index))
            (define index-in-old-bi-return-tuple (- index-in-old-bi-tuple (length indices) 1))
            (format "bi_output_tuple[~a]" index-in-old-bi-return-tuple)]
          [bad-arg (error (format "bad rule: ~a\nbad arg: ~a" (strip-prov r) bad-arg))]))
        (format "head_tuple[~a] = ~a;" i rhs)) 
    (range 0 (length hvars))))
  ))

(define (generate-cpp-lambda-for-rule-with-callback-builtin r indices cpp-func-name)
  ; (printf "(generate-cpp-lambda-for-rule-with-callback-builtin r indices cpp-func-name) args: ~a\n ~a ~a\n" (strip-prov r) indices cpp-func-name)
  (match-define `(srule (,rel-sel ,hvars ...)
                        (,rel-ver0 ,bvars0 ...)
                        ((rel-version ,(? builtin? bi-op) ,arity ,new-indices ,ver) ,bvars1 ...)) (strip-prov r))
  
  (define new-tuple-index-to-old-tuple-index-mapping (map-new-tuple-index-to-old-tuple-index arity new-indices indices))
  (set! indices (map sub1 indices))
  (set! new-indices (map sub1 new-indices))
  (define output-indices (filter (λ (i) (not (member i indices))) (range 0 arity)))

  (string-replace-all 
    "[](const u64* const data, u64* const output) -> int{
      auto args_for_old_bi = std::array<u64, [old-indices-size]> {[populate-args-for-old-bi-code]};
      using TState = std::tuple<const u64*,u64*>;
      TState state = std::make_tuple(data, output);
      auto callback = []([callback-params] TState state) -> TState{
        auto [data, output] = state;
        bool compatible = [check-compatibility-code];
        if (! compatible) return state;

        auto head_tuple = output;
        [head-tuple-populating-code]
        return std::make_tuple(data, output + [head-tuple-size]);
      };
      auto [_,new_ptr] = [cpp-func-name]<TState>(args_for_old_bi.data(), state, callback);
      auto tuples_count = (new_ptr - output) / [head-tuple-size];
      return tuples_count;
    }"

    "[head-tuple-size]" (~a (length hvars))
    "[old-indices-size]" (~a (length indices))
    "[cpp-func-name]" cpp-func-name
    "[populate-args-for-old-bi-code]"
    (intercalate ", " (map (λ (i) 
                            (define arg-pos-in-bvars1 (index-of new-indices (list-ref indices i)))
                            (define arg (list-ref bvars1 arg-pos-in-bvars1))
                            (match arg
                              [(? lit?) (format "n2d(~a)" arg)]
                              [else (format "data[~a]" arg-pos-in-bvars1)])) 
                        (range 0 (length indices))))
    "[callback-params]"
    (intercalate "" (map (λ (i) (format "u64 res_~a, " i)) (range 0 (length output-indices))))
    "[check-compatibility-code]"
    (intercalate " && " 
      (cons "true" (filter-map (λ (i) (let ([mapped-ind (index-of new-indices (list-ref output-indices i))]) 
                          (and mapped-ind (format "res_~a == data[~a]" i mapped-ind)))) 
                  (range 0 (length output-indices)))))
    "[head-tuple-populating-code]"
    (intercalate "\n" (map 
      (λ (i)
        (define rhs (match (list-ref hvars i)
          [(? number? x) (format "number_to_datum(~a)" x)]
          [(? string? str) (error (format "string literals in cpp compiler not supported yet!")) ] ;TODO
          [(? symbol? var) #:when (member var bvars0)
            (format "data[~a]" (index-of bvars0 var))]
          [(? symbol? var) #:when (member var bvars1)
            (define bi-arg-index (index-of bvars1 var))
            (define index-in-old-bi-tuple (list-ref new-tuple-index-to-old-tuple-index-mapping bi-arg-index))
            (define index-in-old-bi-return-tuple (- index-in-old-bi-tuple (length indices) 1))
            (format "res_~a" index-in-old-bi-return-tuple)]
          [bad-arg (error (format "bad rule: ~a\nbad arg: ~a" (strip-prov r) bad-arg))]))
        (format "head_tuple[~a] = ~a;" i rhs)) 
    (range 0 (length hvars))))
  ))

(define (extend-indices indices arity) (append indices (filter (λ (i) (not (member i indices))) (range 0 (add1 arity)))))
(define (map-new-tuple-index-to-old-tuple-index arity new-indices old-indices)
  (define new-indices-extended (extend-indices new-indices arity))
  (define old-indices-extended (extend-indices old-indices arity))
  (map (λ (i) (index-of old-indices-extended (list-ref new-indices-extended i))) (range 0 (add1 arity))))

;; TODO not used
(define (extend-direct-cpp-builtin-to-new-args arity indices new-indices cpp-func-name)
  (set! indices (map sub1 indices))
  (set! new-indices (map sub1 new-indices))
  (define output-indices (filter (λ (i) (not (member i indices))) (range 0 arity)))
  (define new-output-indices (filter (λ (i) (not (member i new-indices))) (range 0 arity)))

  (string-replace-all 
  "[](const u64* const data) -> vector<array<u64, [tuple-size]>>{
    const auto old_indices_size = [old-indices-size];
    auto old_bi_func = *[cpp-func-name];
    
    auto args_for_old_bi = std::array<u64, old_indices_size> {[populate-args-for-old-bi-code]};

    auto old_bi_res = old_bi_func(args_for_old_bi.data());
    auto res = vector<array<u64, [tuple-size]>>();
    for (auto tuple: old_bi_res) {
      bool compatible = [check-compatibility-code];
      if (! compatible) continue;
      
      auto new_tuple = array<u64, [tuple-size]> {[populate-new-tuple-code]};
      
      res.push_back(new_tuple);
    }
    return res;
  }"
    "[tuple-size]" (~a (- arity (length new-indices)))
    "[old-indices-size]" (~a (length indices))
    "[old_indices]"(int-list->cpp-array indices)
    "[new_indices]" (int-list->cpp-array new-indices)
    "[cpp-func-name]" cpp-func-name
    "[map_old_input_tuple_index_to_new_input_tuple_index]" (int-list->cpp-array (map (λ (ind) (index-of new-indices ind)) indices))
    "[map_new_return_tuple_index_to_old_return_tuple_index]" (int-list->cpp-array (map (λ (ind) (index-of output-indices ind)) new-output-indices))
    "[map_old_return_tuple_index_to_new_input_tuple_index]" (int-list->cpp-array (map (λ (ind) (or (index-of new-indices ind) -1)) output-indices))
    "[populate-args-for-old-bi-code]"
    (intercalate ", " (map (λ (i) (format "data[~a]" (index-of new-indices (list-ref indices i)))) (range 0 (length indices))))
    "[check-compatibility-code]"
    (intercalate " && " 
      (cons "true" (filter-map (λ (i) (let ([mapped-ind (index-of new-indices (list-ref output-indices i))]) 
                          (and mapped-ind (format "tuple[~a] == data[~a]" i mapped-ind)))) 
                  (range 0 (length output-indices)))))
    "[populate-new-tuple-code]"
    (intercalate ", " (map (λ (i) (format "tuple[~a]" (index-of output-indices (list-ref new-output-indices i)))) (range 0 (length new-output-indices))))
  ))

;; TODO not used
(define (extend-cpp-filtering-builtin-to-permuted-args arity indices new-indices cpp-func-name)
  (set! indices (map sub1 indices))
  (set! new-indices (map sub1 new-indices))

  (format 
  "[](const u64* const data) -> bool{
    auto old_indices = ~a;
    auto new_indices = ~a;
    auto old_bi_func = *~a;
    auto map_old_input_tuple_index_to_new_input_tuple_index = ~a;
    auto args_for_old_bi = vector<u64>();
    args_for_old_bi.reserve(old_indices.size()); //TODO perf!
    for (auto i: old_indices) {
      auto index_in_new_indices = map_old_input_tuple_index_to_new_input_tuple_index[i];
      args_for_old_bi.push_back(data[index_in_new_indices]);
    }
    return old_bi_func(args_for_old_bi.data());
  }"
    (int-list->cpp-array indices)
    (int-list->cpp-array new-indices)
    cpp-func-name
    (int-list->cpp-array (map (λ (ind) (index-of new-indices ind)) indices)) ;map_old_input_tuple_index_to_new_input_tuple_index
  ))

(define direct-builtins
  `((div-rem 4 (1 2) "builtin_div_rem")
    (range 3 (1 2) "builtin_range")))

(define callback-builtins
  `((< 2 (1 2) "builtin_less")
    (> 2 (1 2) "builtin_greater")
    (<= 2 (1 2) "builtin_le")
    (>= 2 (1 2) "builtin_ge")
    
    (=/= 2 (1 2) "builtin_neq")
    (/= 2 (1 2) "builtin_neq")
    (= 2 (1 2) "builtin_eq")
    (= 2 (1) "builtin_eq_1")
    (= 2 (2) "builtin_eq_1")


    (range 3 (1 2) "callback_builtin_range")

    (+ 3 (1 2) "builtin_add")
    (- 3 (1 2) "builtin_subtract")
    (* 3 (1 2) "builtin_multiply")
    (/ 3 (1 2) "builtin_divide")
    
    (add1 2 (1) "builtin_add1")
    (add1 2 (2) "builtin_add1_2")
    (sub1 2 (1) "builtin_sub1")
    (sub1 2 (2) "builtin_sub1_2")
    
    (number? 1 (1) "builtin_number_huh")
    (not-number? 1 (1) "builtin_not_number_huh")))

;; EXPERIMENTAL CODE


(define (cl-input-args cl)
    (match cl
      [`((rel-version ,rel ,arity ,indices ,ver) ,args ...)
        (take args (length indices))]
      [`((rel-select ,rel ,arity ,indices ,kind) ,args ...)
        (take args (length indices))]))
(define (cl-output-args cl)
    (match cl
      [`((rel-version ,rel ,arity ,indices ,ver) ,args ...)
        (drop args (add1 (length indices)))]
      [`((rel-select ,rel ,arity ,indices ,kind) ,args ...)
        (drop args (add1 (length indices)))]))

;; generates a lambda that takes input-args and "returns" output args.
;; the lambda has the same signature as the callback builtins
(define (generate-cpp-lambda-for-computational-join cl1 cl1func cl2 cl2func input-args output-args)
  
  (string-replace-all
   "[](const u64* data, auto init_state, decltype(init_state) (*callback) ([callback-args] decltype(init_state) state)) -> decltype(init_state){
     typedef decltype(init_state) (*original_callback_t) ([callback-args] decltype(init_state) state);
     typedef CL2CB_State (*cl1cb_t) ([cl1cb-params] CL2CB_State cl1cb_state);
    
     CL2CB_State cl1cb_init_state = {(void*) callback, &init_state, data, nullptr};
     u64 cl1func_input[] = {[cl1func-input]};
     auto res = [cl1func](cl1func_input, cl1cb_init_state, (cl1cb_t) []([cl1cb-params] CL2CB_State cl1cb_state){
       u64 cl1cb_args_arr[] = {[cl1cb-args-arr]};
       cl1cb_state.cl1_output_args = cl1cb_args_arr;
       u64 cl2func_input[] = {[cl2func-input]};
       typedef CL2CB_State (*cl2cb_t) ([cl2cb-params] CL2CB_State cl2cb_state);
       return [cl2func](cl2func_input, cl1cb_state, (cl2cb_t) []([cl2cb-params] CL2CB_State cl2cb_state){
         auto new_state = ((original_callback_t) cl2cb_state.original_callback)([original-callback-args] *((decltype(init_state)*) cl2cb_state.original_state));
         *((decltype(init_state)*) cl2cb_state.original_state) = new_state;
         return cl2cb_state;
       });
     });
     return *((decltype(init_state)*) res.original_state); 
    }"
    "[callback-args]" (intercalate2 ", " (map (λ (x) (format "u64 arg~a" x)) (range 0 (length output-args))))
    "[cl1func]" cl1func ; TODO ...
    "[cl2func]" cl2func ; TODO ...
    "[cl1func-input]" (intercalate ", " (map (λ (arg) (match arg
                                          [(? number? n) (format "number_to_datum(~a)" n)]
                                          [(? symbol?) (format "data[~a]" (index-of input-args arg))])) 
                                        (cl-input-args cl1)))
    "[cl1cb-params]" (intercalate2 ", " (map (app format "u64 arg~a" _) (range 0 (length (cl-output-args cl1)))))
    "[cl1cb-args-arr]" (intercalate ", " (map (app format "arg~a" _) (range 0 (length (cl-output-args cl1))))) 
    "[cl2func-input]" (intercalate ", "
                      (map (λ (arg) (match arg
                                      [(? number? n) (format "number_to_datum(~a)" n)]
                                      [(? symbol?) #:when (member arg input-args) 
                                        (format "cl1cb_state.original_data[~a]" (index-of input-args arg))]
                                      [(? symbol?) #:when (member arg (cl-output-args cl1)) 
                                        (format "arg~a" (index-of (cl-output-args cl1) arg))]))
                           (cl-input-args cl2)))
    "[cl2cb-params]" (intercalate2 ", " (map (λ (x) (format "u64 arg~a" x)) (range 0 (length (cl-output-args cl2)))))
    "[original-callback-args]" (intercalate2 ", "
                               (map (λ (arg) (match arg
                                              [(? number? n) (format "number_to_datum(~a)" n)]
                                              [(? symbol?) #:when (member arg input-args) 
                                                (format "cl2cb_state.original_data[~a]" (index-of input-args arg))]
                                              [(? symbol?) #:when (member arg (cl-output-args cl1))
                                                (format "cl2cb_state.cl1_output_args[~a]" (index-of (cl-output-args cl1) arg))]
                                              [(? symbol?) #:when (member arg (cl-output-args cl2)) 
                                                (format "arg~a" (index-of (cl-output-args cl2) arg))])) 
                                    output-args))
    
    ))

(define (generate-cpp-lambda-for-computational-copy bcl bfunc hcl)
  (define input-args (cl-input-args hcl))
  (define output-args (cl-output-args hcl))
  (string-replace-all
   "[](const u64* data, auto init_state, decltype(init_state) (*callback) ([callback-args] decltype(init_state) state)) -> decltype(init_state){
      
      typedef decltype(init_state) (*original_callback_t) ([callback-args] decltype(init_state) state);
      if (! ([check-input-compatibility] true)) return init_state;
      u64 bclfunc_input[] = {[bclfunc-input]};
      BCLCB_State bclcb_init_state = {(void*) callback, &init_state, data};
      typedef BCLCB_State (*bclcb_t) ([bclcb-params] BCLCB_State bclcb_state);
      auto res = [bclfunc](bclfunc_input, bclcb_init_state, (bclcb_t) []([bclcb-params] BCLCB_State bclcb_state){
        bool compatible = [check-compatibility-code] true;
        if (!compatible) return bclcb_state;
        auto new_state = ((original_callback_t) bclcb_state.original_callback)([original-callback-args] * ((decltype(init_state)*) bclcb_state.original_state));
        *((decltype(init_state)*) bclcb_state.original_state) = new_state;
        return bclcb_state;
      });
      return *((decltype(init_state)*) res.original_state);
    }"
   "[callback-args]" (intercalate2 ", " (map (λ (x) (format "u64 arg~a" x)) (range 0 (length (cl-output-args hcl)))))
   "[bclfunc]" bfunc
   "[bclfunc-input]" (intercalate ", " (map (λ (arg) (match arg
                                          [(? number? n) (format "number_to_datum(~a)" n)]
                                          [(? symbol?) (format "data[~a]" (index-of (cl-input-args hcl) arg))])) 
                                        (cl-input-args bcl)))
   "[bclcb-params]" (intercalate2 ", " (map (λ (x) (format "u64 arg~a" x)) (range 0 (length (cl-output-args bcl)))))
   "[check-input-compatibility]"
   (intercalate2 "&&"
    (filter-map (λ (i)
                  (define arg (list-ref input-args i))
                  (cond 
                    [(lit? arg) (format "data[~a] == ~a" i (lit->cpp-datum arg))]
                    [else #f]))
                (range 0 (length input-args))))
   "[check-compatibility-code]"
   (intercalate2 "&&" 
    (filter-map (λ (i) 
                  (define arg (list-ref (cl-output-args bcl) i))
                  (match arg
                    [(? number? n) (format "arg~a == ~a" i (lit->cpp-datum arg))]
                    [else #f]))
                (range 0 (length (cl-output-args bcl)))))
   "[original-callback-args]" (intercalate2 ", "
                               (map (λ (arg) (match arg
                                              [(? number? n) (format "number_to_datum(~a)" n)]
                                              [(? symbol?) #:when (member arg (cl-input-args hcl)) 
                                                (format "bclcb_state.original_data[~a]" (index-of input-args arg))]
                                              [(? symbol?) #:when (member arg (cl-output-args bcl)) 
                                                (format "arg~a" (index-of (cl-output-args bcl) arg))])) 
                                    output-args))
  ))

(define (generate-cpp-lambda-for-computational-head-cl hcl)
  (define dummy-bi-func-name (get-func-for-comp-rel '(rel-select = 2 (1 2) comp) (hash)))
  (define dummy-bcl '((rel-select = 2 (1 2) comp) 0 0 _))
  (generate-cpp-lambda-for-computational-copy dummy-bcl dummy-bi-func-name hcl))


;; TODO this is trash
(define (get-func-for-comp-rel bi cr-names)
  ; (printf "get-func-for-comp-rel bi : ~a, cr-names: ~a\n" bi cr-names)
  (match-define `(rel-version ,rel-name ,rel-arity ,rel-indices ,_) (->rel-version bi))
  (cond 
    [(hash-has-key? cr-names bi) 
      (hash-ref cr-names bi)]
    [else
      (define matching-spec 
        (findf (λ (bi-spec)
                (match-define `(,name ,arity ,indices ,cpp-func-name) bi-spec)
                (and (equal? rel-name name)
                    (equal? rel-arity arity)
                    (equal? rel-indices indices))) callback-builtins))
      (match-define `(,name ,arity ,indices ,cpp-func-name) matching-spec)
      cpp-func-name]))

; cr-names: a hash from computational relation rel-versions to the function names implementing them
(define (generate-lambda-for-computational-relation-rule rule cr-names)
  (match-define `(crule ,(and hcl `((rel-select ,rel-name ,rel-arity ,rel-indices comp) ,hvars ...))
                        ,bcls ...) (strip-prov rule))
  (define bcls-ordered bcls) ; TODO can we assume bcls are ordered?
  
  (match (length bcls-ordered)
    [0 
     (generate-cpp-lambda-for-computational-head-cl hcl)]
    [1
     (define bcl (car bcls-ordered))
     (match-define `(,cl-rel ,args ...) bcl)
     (generate-cpp-lambda-for-computational-copy bcl (get-func-for-comp-rel cl-rel cr-names) hcl)]
    [n #:when (> n 1)
     (match-define (cons final-lambda final-temp-clause)
      (foldl (λ (cl accu)
                (match-define (cons temp-lambda temp-cl) accu)
                (define new-temp-cl-input-args (set-intersect (cl-input-args hcl) (set-union (cl-input-args temp-cl) (cl-input-args cl))))
                (define new-temp-cl-output-args (set-union (cl-output-args temp-cl) (cl-output-args cl)))
                (match-define `(,cl-rel ,args ...) cl)
                (define new-lambda (generate-cpp-lambda-for-computational-join 
                                      temp-cl temp-lambda cl (get-func-for-comp-rel cl-rel cr-names) 
                                      new-temp-cl-input-args new-temp-cl-output-args))
                (define new-temp-cl `((rel-select 
                                       ,(gensymb 'temp-compute-rel) 
                                       ,(+ (length new-temp-cl-input-args) (length new-temp-cl-output-args))
                                       ,(range 1 (add1 (length new-temp-cl-input-args)))
                                       comp)
                                      ,@(append new-temp-cl-input-args '(_) new-temp-cl-output-args)))
                (cons new-lambda new-temp-cl))
            (cons (get-func-for-comp-rel (car (car bcls-ordered)) cr-names) (car bcls-ordered))
            (cdr bcls-ordered)))
     (generate-cpp-lambda-for-computational-copy final-temp-clause final-lambda hcl)]))

;; all rules must have the same rel in the head
(define (generate-cpp-func-for-computational-relation-rules rules cr-names)
  
  (match-define `(crule ,(and hcl `(,(and hrel `(rel-select ,rel-name ,rel-arity ,rel-indices comp)) ,_ ...))
                        ,_ ...) (strip-prov (car rules)))
  (define name (hash-ref cr-names hrel))
  (string-replace-all
  "template<typename TState>
   TState [name](const u64* data, TState init_state, TState (*callback) ([cb-params] TState state)){
     TState state = init_state;
     [codes-for-lambdas]
     return state;
   }"
   "[name]" name
   "[cb-params]" (intercalate2 ", " (map (app format "u64 arg~a" _) (range 0 (length (cl-output-args hcl)))))
   "[codes-for-lambdas]"
   (intercalate "\n"
    (map 
      (λ (rule)
        (string-replace-all
        "{
            auto lambda = [lambda];
            state = lambda(data, state, callback);
          }"
        "[lambda]" (generate-lambda-for-computational-relation-rule rule cr-names)))
      rules))
   ))

(define (lit->cpp-datum lit)
  (match lit
    [(? number?) (format "n2d(~a)" lit)]
    [(? string?) (format "throw \"cannot handle string literals yet! literal: ~a\"" lit)]))