#lang racket

;; Slog compilation pass -- organization
;; Copyright (c) Thomas Gilray, et al, see License.md

;; This pass walks over a rule and does several things
;; to organize it into a first IR.

(provide organize-pass)

(require "lang-predicates.rkt")
(require "utils.rkt")
(require racket/hash)
(require racket/runtime-path)
(require "parser.rkt")
(require "builtins.rkt")
(require "graphs.rkt")
(require "lang-utils.rkt")
(require "slog-params.rkt")

(define-runtime-path HERE ".")
(define (get-path x) (path->string (simplify-path (build-path HERE x))))

;; "Organizes", desugars, and flattens every rule 
(define/contract-cond (organize-pass source-tree)
  (-> source-tree? ir-flat?)
  (match-define (list rules-h comp-rules-h splicing-lib-required)
                (source-tree->ir-flat-rules-h+comp-rules-h source-tree))
  (match-define (list splicing-support-lib-source-tree splicing-support-lib-rules-h splicing-support-lib-comp-rules-h) 
    (cond 
      [splicing-lib-required
        (define support-lib-source-tree (parse-slog-file (get-path "splicing-support.slog") #:allow-$id #t))
        (match-define (list support-lib-rules-h support-lib-comp-rules-h _)
                      (source-tree->ir-flat-rules-h+comp-rules-h support-lib-source-tree))
        (list support-lib-source-tree support-lib-rules-h support-lib-comp-rules-h)]
      [else (list (hash) (hash) (hash))]))

  (assert-source-tree? (hash-union source-tree splicing-support-lib-source-tree))
  (define result-rules-h0 (hash-union rules-h splicing-support-lib-rules-h))
  (define (ir-flat-rule-empty-head? r)
    (match-define `(rule ,heads ,bodys) r)
    (set-empty? heads))
  (define result-rules-h (hash-filter-keys (not/c ir-flat-rule-empty-head?) result-rules-h0))
  (define result-comp-rules-h (hash-union comp-rules-h splicing-support-lib-comp-rules-h))
  (for ([r (hash-keys result-rules-h)]) (assert (ir-flat-rule? r) (format "bad rule: ~a" (strip-prov r))))
  `(ir-flat
    ,(hash-union source-tree splicing-support-lib-source-tree)
    ,result-rules-h
    ,result-comp-rules-h))

(define (desugared-bodyless-rule-as-facts rule)
    ;;returns (cons new-cl got-inlined), where got-inlined is true iff cl-to-be-inlined got inlined into cl
    (define (inline-clause cl cl-to-be-inlined)
      (match cl-to-be-inlined
        [`(prov ((prov = ,=pos) (prov ,(? var? id) ,idpos) ,cl-rhs) ,pos) #:when (not (equal? id '_))
          (define got-inlined #f)
          (define final-clause
            (visit-replace-sub-clauses cl 
              (λ (sub-cl) (if (equal? (strip-prov sub-cl) id)
                            (begin (set! got-inlined #t) cl-rhs)
                            #f))))
          (cons final-clause got-inlined)]
        [else (cons cl #f)]))

    (define (inline-all-clauses cls)
      (define cls-dep-graph (head-clauses-dependency-graph cls))
      (define cls-sorted (topological-sort cls-dep-graph))
      (define cls-inlined
        (foldl (λ (i accu)
                (define-values (before current-after) (split-at accu i))
                (match-define (cons current after) current-after)
                (define new-after0 (map (λ (cl) (inline-clause cl current)) after))
                (define new-after (map car new-after0))
                (define got-inlined (ormap cdr new-after0))
                `(,@before ,(if got-inlined #f current) ,@new-after))
                cls-sorted
                (range 0 (length cls-sorted))))
      ;; TODO maybe remove repeated clauses?
      (filter (not/c false?) cls-inlined))
      
    (define res (match rule
      [`(prov (,heads ... <-- ,bodys ..1) ,pos) (error (format "not a bodyless rule: ~a" (strip-prov rule)))]
      [`(prov (,heads ... <-- ) ,pos) 
        (map (λ (h) (match h
              [`(prov ((prov = ,=pos) ,(? (prov-of var?) id) ,cl) ,pos) cl]
              [else h])) 
             (inline-all-clauses heads))]
      [else (error (format "bad rule: ~a" rule))]))
    
    (define (sanitize-fact cl)
      (match cl
        [`(prov ((prov ,tag ,tagpos) ,args ...) ,pos) 
          (when (set-member? all-builtin-names tag)
            (pretty-error-current pos (format "~a is a builtin, and therefore immutable!" tag) #:exit #t))
          (for ([arg args]) (sanitize-fact arg))]
        [(? (prov-of lit?)) #t]
        [else (pretty-error-current (prov->pos cl) (format "Bad fact!") #:exit #t)]))
      (for ([cl res]) (sanitize-fact cl))
      res)

(define (module->ir-flat-rules-h+comp-rules-h module-name module)
  (define rules-h (Module-rules module))
  (define splicing-lib-required #f)
  (match-define (list ir-flat-rules-h ir-flat-comp-rules-h)
    (foldl 
      (lambda (id accu)
        (match-define (list h comp-h) accu)
        (match-define (cons rules0 splicing-lib) (desugar-source-tree-rule (hash-ref rules-h id)))
        (define rule-prov `(rule-prov source-tree ,module-name ,id))
        (when splicing-lib (set! splicing-lib-required #t))
        (define-values (comp-rules rules)
          (partition (app source-tree-comp-rule? _ (Module-comp-rels module)) rules0))

        (define comp-h+
          (foldl (λ (rule h) (hash-set h (source-tree-comp-rule->ir-flat-comp-rule rule) rule-prov))
                 comp-h
                 comp-rules))
        
        (define h+
          (foldl (λ (rule h) (hash-set h (organize-rule rule) rule-prov))
                h
                rules))
        
        (list h+ comp-h+))
      (list (hash) (hash))
      (hash-keys rules-h)))
  (list ir-flat-rules-h ir-flat-comp-rules-h splicing-lib-required))

(define (source-tree->ir-flat-rules-h+comp-rules-h source-tree)
  (define all-rules (flat-map (compose hash-values Module-rules) (hash-values source-tree)))
  (define all-facts (flat-map (compose set->list Module-facts) (hash-values source-tree)) )
  (assert (list? all-facts) (format "all-facts: ~a" all-facts))
  #;(define all-rel-arities (apply hash-union (map source-tree-rule-rel-arities (append all-rules all-facts)) #:combine set-union))
  #;(printf "all-rel-arities: ~a\n" 
          (intercalate "\n" (map (λ (rel-arities) 
                                    (format "~a: ~a" (car rel-arities) (set->list (cdr rel-arities)))) 
                                 (hash->list all-rel-arities))))
  (foldl 
    (λ (module-name accu)
      (define module (hash-ref source-tree module-name))
      (assert (source-tree-module? module) (format "bad module: ~a" (strip-prov module)))
      (match-define (list ir-flat-rules ir-flat-comp-rules splicing-lib-required) accu)
      (match-define (list module-ir-flat-rules module-ir-flat-comp-rules module-splicing-lib-required) 
                    (module->ir-flat-rules-h+comp-rules-h module-name module))
      (list (hash-union ir-flat-rules module-ir-flat-rules)
            (hash-union ir-flat-comp-rules module-ir-flat-comp-rules) 
            (or splicing-lib-required module-splicing-lib-required)))
    (list (hash) (hash) #f)
    (hash-keys source-tree)))

;; returns (cons desugared-rules requires-splicing-lib-support)
(define (desugar-source-tree-rule rule)
  ; (printf "input-rule: \n~a\n" (strip-prov rule))
  (define splicing-lib-required #f)
  (define normalized-rule (normalize-rule rule))
  (define do-desugared-rule (rule-desugar-bang-do-huh-do normalized-rule))
  ; (printf "do-desugared-rule: \n~a\n" (strip-prov do-desugared-rule))
  (define dehuhd-rule (move-huhs-rule do-desugared-rule))
  ; (printf "dehuhd-rule : \n~a\n" (strip-prov dehuhd-rule))
  (define desegmented-rule (desugar-segmented-rule dehuhd-rule))
  (define or-split-rules (split-rule-or desegmented-rule))
  ; (printf "or-split-rules : \n~a\n" (intercalate "\n" (map strip-prov or-split-rules)))
  (define curlies-removed-rules (map rule-desugar-curly-clauses or-split-rules))
  ; (printf "curlies-removed-rules: \n~a\n" (intercalate "\n" (map format-source-tree-rule curlies-removed-rules)))
  (define aggregator-fixed-rules (flat-map fix-aggregators curlies-removed-rules))
  ; (printf "aggregator-fixed-rules: \n~a\n" (intercalate "\n" (map format-source-tree-rule aggregator-fixed-rules)))
  (define debanged-rules (map desugar-bang-clauses aggregator-fixed-rules))
  ; (printf "debanged-rules : \n~a\n" (intercalate "\n" (map format-source-tree-rule debanged-rules)))
  (define flattened-rules (map rule-desugar-iclauses debanged-rules))
  ; (printf "flattened-rules : \n~a\n" (intercalate "\n" (map strip-prov flattened-rules)))
  (define wild-cards-removed-rules (map remove-wildcards-source-tree-rule flattened-rules))
  ; (printf "wild-cards-removed-rules : \n~a\n" (intercalate "\n" (map strip-prov wild-cards-removed-rules)))
  (define optimized-rules (map source-tree-rule-static-unification-pass wild-cards-removed-rules))
  ; (printf "optimized-rules : \n~a\n" (intercalate "\n" (map strip-prov optimized-rules)))

  (define head-inner-rules-removed-rules (flat-map desugar-head-inner-rules optimized-rules))
  ; (printf "head-inner-rules-removed-rules: \n~a\n" (intercalate "\n" (map strip-prov head-inner-rules-removed-rules)))
  (define inner-rules-removed-rules (flat-map desugar-inner-rules head-inner-rules-removed-rules))
  ; (printf "inner-rules-removed-rules: \n~a\n" (intercalate "\n" (map strip-prov inner-rules-removed-rules)))
  (define body-despliced-rules 
    (flat-map (λ (r) (define res (desplice-rule-body r))
                      ;; if desplice-rule-body generates more than one rule, we need splicing support lib
                      (when (> (length res) 1) (set! splicing-lib-required #t))
                      res) 
              inner-rules-removed-rules))
  (define despliced-rules 
    (flat-map (λ (r) (define res (desplice-rule-head r))
                      (when (> (length res) 1) (set! splicing-lib-required #t))
                      res)
              body-despliced-rules))
  (define despliced-rules+ (map remove-wildcards-source-tree-rule (map rule-desugar-iclauses despliced-rules)))
  (define desequenced-rules (map desequence-rule despliced-rules+))
  (define desequenced-rules-optimized (map source-tree-rule-static-unification-pass desequenced-rules))
  ; (printf "desequenced-rules-optimized \n~a\n" (intercalate "\n" (map strip-prov desequenced-rules-optimized)))
  (cons desequenced-rules-optimized splicing-lib-required))

(define/contract-cond (fix-aggregators rule)
  (source-tree-rule? . -> . (cons/c source-tree-rule? (listof source-tree-rule?)))
  (define (fix-clause cl)
    (match cl
      [`(prov (,tag ,cl1 ,cls ...) ,pos)
        (cond 
          [(aggregator? (strip-prov tag)) 
           (when (not (rel-name? (strip-prov cl1)))
            (pretty-error-current
             pos
             (format "bad use of aggregator ~a. Aggregators must be followed by a relation name" (strip-prov tag))
             #:exit #t))
           (match-define (cons clause-desugared new-rules)
            (cond
              [(equal? (strip-prov tag) '~)
                (fix-negation-wildcards cl)]
              [else
                (cons cl (list))]))
            (match-define `(prov (,new-tag ,new-cl1 ,new-cls ...) ,new-pos) clause-desugared)
            (cons `(prov ((agg ,new-tag ,new-cl1) ,@new-cls) ,new-pos) new-rules)]
          [else (cons cl (list))])]
      [else (cons cl (list))]))
  (match-define `(prov ,items ,pos) rule)
  (match-define (cons new-clauses extra-rules)
    (foldl 
      (λ (cl accu)
          (match-define (cons cls extra-rules) accu)
          (match-define (cons new-cl cl-extra-rules) (fix-clause cl))
          (cons (append cls (list new-cl)) (append extra-rules cl-extra-rules)))
      (cons (list) (list))
      items))
  (assert (source-tree-rule? `(prov ,new-clauses ,pos)) (format "bad rule: ~a\n" (strip-prov `(prov ,new-clauses ,pos))))
  (for ([r extra-rules]) (assert (source-tree-rule? r)))
  (cons
    `(prov ,new-clauses ,pos)
    extra-rules))

(define/contract-cond (fix-negation-wildcards cl)
 (source-tree-bclause? . -> . (cons/c source-tree-bclause? (listof source-tree-rule?)))
 (match-define `(prov (,tag ,cl1 ,cls ...) ,pos) cl)
 (cond 
  [(ormap wildcard? cls)
    (define body-args (map (λ (ind) `(prov ,(string->symbol (format "$x~a" ind)) ,pos)) (range 1 (add1 (length cls)))))
    (define head-args-indices (filter-map (λ (ind cl) (if (not (wildcard? cl)) ind #f)) (range 0 (length cls)) cls))
    (define head-args (map (app list-ref body-args _) head-args-indices))
    (define replacement-relation `(prov ,(gensymb (string->symbol (format "$~a'" (strip-prov cl1)))) ,(prov->pos cl1)))
    (define replacement-clause `(prov (,tag ,replacement-relation ,@(filter (compose not wildcard?) cls)) ,pos))
    (define new-rule 
      `(prov ((prov (,replacement-relation ,@head-args) ,pos) <--
             (prov (,cl1 ,@body-args) ,pos))  ,pos))
    (assert (source-tree-rule? new-rule))
    ; (printf "fix-negation-wildcards res: ~a \n ~a \n" (strip-prov replacement-clause) (strip-prov new-rule))
    (cons replacement-clause (list new-rule))]
  [else (cons cl (list))]))

; Normalizes head-clauses to [head <-- ]
(define/contract-cond (normalize-rule rule)
  (source-tree-rule? . -> . source-tree-rule?)
  (match rule
    [(and (? source-tree-hclause?) `(prov ,exp ,pos))
     `(prov (,rule <--) ,pos)]
    [`(prov (,head-clauses ... <-- . ,body-clauses) ,pos) rule]
    [else (error "Rule seems invalid.")]))


(define (clause-desugar-bang-do-huh-do cl)
  (define (fix-!do-?do tag cl)
    (match cl
      [`(prov (!do (,cls ...)) ,pos) 
       `(prov (! ((prov ,(string->symbol (format "do-~a" tag)) ,pos) ,@cls)) ,pos)]
      [`(prov (?do (,cls ...)) ,pos) 
       `(prov (? ((prov ,(string->symbol (format "do-~a" tag)) ,pos) ,@cls)) ,pos)]
      [else cl]))

  (match cl
    [`(prov ,(? arg? v) ,pos)
    cl]
    [`(prov (! ,banged-clause) ,pos)
     `(prov (! ,(clause-desugar-bang-do-huh-do banged-clause)) ,pos)]
    [`(prov (LIST-SYNTAX ,args ...) ,pos)
     `(prov (LIST-SYNTAX ,@(map clause-desugar-bang-do-huh-do args)) ,pos)]
    [`(prov ((prov or ,orpos) ,cls ...) ,pos)
     `(prov ((prov or ,orpos) ,@(map clause-desugar-bang-do-huh-do cls)) ,pos)]
    [`(prov (,tag ,cls ...) ,pos)
     `(prov (,tag ,@(map (app fix-!do-?do (strip-prov tag) _) cls)) ,pos)]
    [`(INNER-RULE ,inner-rule)
     `(INNER-RULE ,(rule-desugar-bang-do-huh-do inner-rule))]
    [else cl]))

(define (rule-desugar-bang-do-huh-do rule)
  (match-define `(prov ,xs ,pos) rule)
  (define new-xs (map clause-desugar-bang-do-huh-do xs))
  `(prov ,new-xs ,pos))

; Removes (or ...) clauses and splits one rule into a set of one or more rules
(define/contract-cond (split-rule-or rule)
  (source-tree-rule? . -> . (listof source-tree-rule?))
  (define (get-conjunction-clauses cl)
    (match cl
      [`(prov ((prov and ,andpos) ,cls ...) ,pos)
        cls]
      [else (list cl)]))
  ; traverses a single clause and returns a set of clauses if an internal (or ...) splits the clause
  ;; TODO we are supporting or clauses nested inside other clauses here: (foo (or 1 2))
  ;; But the parser does not allow that. We can simplify this, or change the parse,
  ;; my vote is to simplify this.
  (define/contract-cond (split-clause cl [top-level-clause #f])
    (any/c boolean? . -> . (set/c (listof source-tree-body-item?)))
    (match cl
           [`(prov ,(? arg? v) ,pos)
            (set (list cl))]
           [`(prov (! ,banged-clause) ,pos)
            (set (list cl))]
           [`(prov (LIST-SYNTAX ,args ...) ,pos)
            (set (list cl))]
           [`(prov ((prov or ,orpos) ,cls ...) ,pos)
            (cond
              [top-level-clause
                (foldl set-union (set) (map (compose cls->split get-conjunction-clauses) cls))]
              [else (foldl set-union (set) (map split-clause cls))])]
           [`(prov (,tag ,cls ...) ,pos)
            (foldl (lambda (cls+ st)
                     (set-add st (list `(prov (,tag ,@cls+) ,pos))))
                   (set)
                   (set->list (cls->split cls)))]
           [`(INNER-RULE ,inner-rule)
            (define inner-rule-split (split-rule-or inner-rule))
            (list->set (map (λ (ir) (list `(INNER-RULE ,ir))) (set->list inner-rule-split)))]))
  ; Helper for split-or-rule that takes a list of bodies and returns a set of lists of split bodies
  (define/contract-cond (cls->split cls)
    ((listof source-tree-body-item?) . -> . (set/c (listof source-tree-body-item?)))
    (foldr (lambda (cl tails-st)
            (define cl-st (split-clause cl #t))
            (foldl (lambda (cl+ tails-st+) 
                      (set-union tails-st+
                                (list->set (map (lambda (t) (append cl+ t))
                                                (set->list tails-st)))))
                    (set)
                    (set->list cl-st)))
          (set '())
          cls))
  (match rule
          [`(prov (,head-clauses ... <-- . ,body-clauses) ,pos)
          (define new-head-clauses
            (flat-map (match-lambda 
                        [`(INNER-RULE ,ir) (map (λ (r) `(INNER-RULE ,r)) (split-rule-or ir))]
                        [cl (list cl)])
                      head-clauses))
          (define body-split (cls->split body-clauses))
          (map (λ (bodys) `(prov (,@new-head-clauses <-- . ,bodys) ,pos)) 
               (set->list body-split))]))

; Moves "huh" clauses to the body of the rule and inserts references in the head
(define (move-huhs-rule rule)
  (define (pull-huhs cl)
    (match cl
            ;; dirty hack to make it work with the list syntax. 
            ['... (cons '... (set))]
            [`(prov ,(? arg? v) ,pos)
            (cons cl (set))]
            [`(prov (? ,huhcl) ,pos)
            (define hx (gensymb '$h))
            (cons `(prov ,hx ,pos)
                  (set `(prov ((prov = ,pos) (prov ,hx ,pos) (prov ,huhcl ,pos)) ,pos)))]
            [`(prov (CURLY-CLAUSE ,tag ,args ...) ,pos) (cons cl (set))]
            [`(prov (,tag ,ics ...) ,pos)
            ; handles or, and relations
            (match-define (cons ics+ huhs)
                          (foldr (lambda (cl acc)
                                    (match-define (cons heads+ huhs0) acc)
                                    (match-define (cons cl+ huhs1) (pull-huhs cl))
                                    (cons (cons cl+ heads+) (set-union huhs0 huhs1)))
                                  (cons '() (set))
                                  ics))
            (cons `(prov (,tag ,@ics+) ,pos) huhs)]))
  (match-define `(prov ,xs ,pos) rule)
  (define segments (list-split xs '(--)))

  (define (update-segment seg)
    (match-define `(,heads ... <-- ,bodys ...) seg)
    (match-define (cons heads+ huhs)
                        (foldr (lambda (cl acc)
                                  (match-define (cons heads+ huhs0) acc)
                                  (match-define (cons cl+ huhs1) (pull-huhs cl))
                                  (cons (cons cl+ heads+) (set-union huhs0 huhs1)))
                                (cons '() (set))
                                (filter (not/c inner-rule?) heads)))
    (define new-seg
      `(,@(map (transform-if-inner-rule move-huhs-rule) (filter inner-rule? heads)) ,@heads+ <-- 
        ,@(map (transform-if-inner-rule move-huhs-rule) bodys)))
    (cons new-seg huhs))
  (define updated-segments (map update-segment segments))

  (define res `(prov (,@(append* (add-between (map car updated-segments) '(--))) 
                      ,@(flat-map set->list (map cdr updated-segments))) ,pos))
  res)

(define (rule-desugar-curly-clauses rule)
  
  (define/contract-cond (extract-curly-clauses cl)
    (any/c . -> . list?)
    (match cl
      [`(prov (CURLY-CLAUSE ,tag ,args ...) ,pos)
       (define res-var `(prov ,(gensymb (string->symbol (format "$~a_res" (strip-prov tag)))) ,pos))
       (define extra-clause `(prov (,tag ,@args ,res-var) ,pos))
       (define extra-clauses (extract-curly-clauses extra-clause))
       (cons res-var extra-clauses)]
      ['... (cons '... '())]
      [`(prov ,(? arg? v) ,pos)
        (cons cl '())]
      [`(prov (! ,banged-clause) ,pos)
        (match-define (cons `(prov ,new-banged-clause ,new-pos) extracted) (extract-curly-clauses `(prov ,banged-clause ,pos)))
        (cons `(prov (! ,new-banged-clause) ,new-pos) extracted)]
      [`(prov (? ,huhd-clause) ,pos)
        (match-define (cons `(prov ,new-huhd-clause ,new-pos) extracted) (extract-curly-clauses `(prov ,huhd-clause ,pos)))
        (cons `(prov (? ,new-huhd-clause) ,new-pos) extracted)]
      [`(prov (,tag ,ics ...) ,pos) 
       (define new-ics (map extract-curly-clauses ics))
       (define new-cl `(prov (,tag ,@(map car new-ics)) ,pos))
       (for ([cllist (map cdr new-ics)])
        (assert (list? cllist) (format "~a" cllist)))
      ;  (assert (list? (flat-map cdr new-ics)))
       (cons new-cl (flat-map cdr new-ics))]
      [else (error (format "extract-curly-clauses unhandled clause: ~a" (simplify-prov cl)))]))
  

  (define (update-rule-item cl)
    (match cl
      [`(INNER-RULE ,ir) (cons `(INNER-RULE ,(rule-desugar-curly-clauses ir)) '())]
      [else (extract-curly-clauses cl)]))
  
  (match-define `(prov (,heads ... <-- ,bodys ...) ,pos) rule)
  (define updated-heads (map update-rule-item heads))
  (define res `(prov (,@(map car updated-heads) <-- ,@(flat-map cdr updated-heads) ,@(flat-map update-rule-item bodys)) ,pos))
  (assert (source-tree-rule? res) (format "bad rule:\n~a" (strip-prov res)))
  ; (printf "curly-desugared-rule: \n~a\n" (format-source-tree-rule res))
  res)

(define (source-tree-comp-rule->ir-flat-comp-rule rule)
  (match-define `(prov (,head <-- ,bodys ...) ,pos) rule)
  `(crule ,(remove-wildcards-clause head) 
          ,@(reverse (map remove-wildcards-clause bodys))))

; Organizes a single rule
; flattens all clauses, separates bodies, heads, and sides, and 
(define (organize-rule rule)
  (match rule
         [`(prov (,hcs ... <-- . ,bcs) ,pos)
          (remove-wildcards
           (separate-sides
            (foldl set-union (set) (map desugar-clause hcs))
            (foldl set-union (set) (map desugar-clause bcs))))]
         [(and (? source-tree-hclause?)
               hc
               `(prov ,exp ,pos))
          (organize-rule `(prov (,hc <--) ,pos))]
         [else (error (format "Rule ~a seems invalid." rule))]))

; Flattens internal clauses in a source-tree-rule with inner rules
(define (rule-desugar-iclauses rule)
  (define (desugar-if-clause cl)
    (match cl
      [`(INNER-RULE ,ir) (list cl)]
      [else (set->list (desugar-clause cl))]))
  (match-define `(prov (,hcs ... <-- . ,bcs) ,pos) rule)
  `(prov (,@(flat-map desugar-if-clause (map (transform-if-inner-rule rule-desugar-iclauses) hcs)) <-- 
          ,@(flat-map desugar-if-clause (map (transform-if-inner-rule rule-desugar-iclauses) bcs))) ,pos))

(define (list-syntax? cl)
  (match cl [`(prov (LIST-SYNTAX ,args ...) ,pos) #t]
            [else #f]))

; Desugars an internal clause into a cons of a variable name and set of clauses  
(define (desugar-iclause ic [cl-id #f])
  (match ic
         [`(prov (LIST-SYNTAX ,args ...) ,pos) ; CAUTION A list cannot be extracted into a variable for now
          (match-define (cons args+ extra)
            (foldr (λ (ic accu)
                    (match-define (cons args+ extra) accu)
                    (define ic-id (if (list-syntax? ic) `(prov ,(gensymb '$id) ,pos) #f))
                    (match-define (cons ic+ ic-extra) (desugar-iclause ic ic-id))
                    (cons (cons ic+ args+) (set-union extra ic-extra)))
                  (cons '() (set))
                  args))
          (define new-list-clause `(prov (LIST-SYNTAX ,@args+) ,pos))
          (if cl-id
            (cons cl-id (set-add extra `(prov ((prov = ,pos) ,cl-id ,new-list-clause) ,pos)))
            (cons new-list-clause extra))]
         [`(prov (,tag ,ics ...) ,pos)
          (match-define (cons ic+ st) 
                        (foldr (lambda (ic acc)
                                 (match-define (cons `(,tag ,ics+ ...) st0) acc)
                                 (match-define (cons ic+ st1) (desugar-iclause ic))
                                 (cons `(,tag ,ic+ . ,ics+) (set-union st0 st1)))
                               (cons `(,tag) (set))
                               ics))
          (define idc (if cl-id cl-id `(prov ,(gensymb '$id) ,pos)))
          (cons idc (set-add st `(prov ((prov = ,pos) ,idc (prov ,ic+ ,pos)) ,pos)))]
         [`(prov ,(? var? x) ,pos)
          (cons ic (set))]
         [`(prov ,(? lit? v) ,pos)
          (cons ic (set))]
         ['... (cons ic (set))]
         [else (error (format "Unrecognized iclause: ~a \n" ic))]))

; Desugars a top-level clause, returning a set of normalized (= ...) clauses
(define (desugar-clause clause)
  (match clause
         [`(prov ((prov = ,=pos) (prov ,(? var? x) ,xpos) (prov ,(? arg? y) ,ypos)) ,pos)
          (set clause)]
         [`(prov ((prov = ,=pos) (prov ,(? var? id) ,idpos) ,ic1) ,pos)
          (match-define (cons v1 st1) (desugar-iclause ic1 `(prov ,id ,idpos)))
          st1]
         [`(prov ((prov = ,=pos) ,ic0 ,ic1) ,pos)
          (match-define (cons v0 st0) (desugar-iclause ic0))
          (match-define (cons v1 st1) (desugar-iclause ic1))
          (set-union st0 st1 (set `(prov ((prov = ,=pos) ,v0 ,v1) ,pos)))]
         [`(prov (,tag ,ics ...) ,pos)
          (desugar-clause (give-clause-id clause))]
         [else (error (format "Unrecognized clause: ~a \n" clause))]))

; takes a set of heads and of bodies and produce the final output rule with heads bodies and sides
; normalizes clauses so variables go on the left
;; TODO rename, review
(define (separate-sides heads bodys)
  (define (filter-side cl acc)
    (match-define (cons st sst) acc)
    (match cl
           [`(prov ((prov = ,=pos)
                    (prov ,(? var? x) ,xpos)
                    (prov ,(? var? y) ,ypos))
                   ,pos)
            (cons st (set-add sst cl))]
           [`(prov ((prov = ,=pos)
                    (prov ,(? var? x) ,xpos)
                    (prov ((prov ,xrel ,relpos) ,vals ...) ,icpos))
                   ,pos)
            (cons (set-add st cl) sst)]
           [`(prov ((prov = ,=pos)
                    (prov ,(? var? x) ,xpos)
                    (prov ,(? lit? v) ,vpos))
                   ,pos)
            (cons st (set-add sst cl))]
           [`(prov ((prov = ,=pos)
                    (prov ,(? lit? x) ,xpos)
                    (prov ,(? lit? v) ,vpos))
                   ,pos)
            (cons (set-add st (give-clause-id cl)) sst)]
           [`(prov ((prov ,oper ,operpos) (prov ,lhs ,lhspos) (prov ,rhs ,rhspos)) ,pos)
            (cons (set-add st cl) sst)]
           ;; TODO this can be removed
           #;[`(prov ((prov ,oper ,operpos) (prov ,lhs ,lhspos) (prov ,rhs ,rhspos)) ,pos)
            ; oper is a catch all for a constraint (not primop), must be =, =/=, <=, etc
            (define gx (gensymb '$t))
            (define gy (gensymb '$t))
            (match-define (cons st+ sst+)
                          (filter-side `(prov ((prov = ,operpos) (prov ,gx ,lhspos) (prov ,lhs ,lhspos)) ,pos)
                                       (cons st (set-add sst `(prov ((prov ,oper ,operpos)
                                                                     (prov ,gx ,lhspos)
                                                                     (prov ,gy ,rhspos))
                                                                    ,pos)))))
            (filter-side `(prov ((prov = ,operpos) (prov ,gy ,rhspos) (prov ,rhs ,rhspos)) ,pos)
                         (cons st+ sst+))]
           [else (error (format "Bad clause: ~a" cl))]))
  (define (filter-sides cls-st)
    (foldl filter-side
           (cons (set) (set))
           (set->list cls-st)))
  (match-define (cons heads+ sides0) (filter-sides heads))
  (match-define (cons bodys+ sides1) (filter-sides bodys))
  (define new-sides (list->set (map give-clause-id (set->list (set-union sides0 sides1)))))
  `(rule ,heads+ ,(set-union bodys+ new-sides)))


(define (remove-wildcards-clause cl)
  (match cl
    ['... '...]
    [`(prov ,(? arg?) ,pos) (remove-wildcards-val cl)]
    [`(prov (,(? source-tree-rel? tag) ,vals ...) ,pos)
     `(prov (,tag ,@(map remove-wildcards-clause vals)) ,pos)]
    [`(prov ((prov = ,=pos)
            (prov ,(? var? x) ,xpos)
            (prov (,(? source-tree-rel? tag) ,vals ...) ,icpos)) ,pos)
     `(prov ((prov = ,=pos)
            ,(remove-wildcards-val `(prov ,x ,xpos))
            (prov (,tag ,@(map remove-wildcards-clause vals)) ,icpos)) ,pos)]
    [`(prov (LIST-SYNTAX ,args ...) ,pos)
     `(prov (LIST-SYNTAX ,@(map remove-wildcards-clause args)) ,pos)]
    [else (error (format "Unrecognized clause: ~a" cl))]))

(define (wildcard? cl)
  (match cl
    [`(prov _ ,pos) #t]
    [else #f]))

(define (remove-wildcards-val val)
  (if (wildcard? val) `(prov ,(gensymb '$_) ,(prov->pos val)) val))

; Replaces wildcard variables with a fresh variable
(define (remove-wildcards rule)
  (match rule
         [`(rule ,heads ,bodys)
          `(rule
            ,(list->set (map remove-wildcards-clause (set->list heads)))
            ,(list->set (map remove-wildcards-clause (set->list bodys))))]))

(define (remove-wildcards-source-tree-rule rule)
  (match-define `(prov (,heads ... <-- ,bodys ...) ,pos) rule)
  `(prov (,@(map (transform-rule-item remove-wildcards-clause remove-wildcards-source-tree-rule) heads) <-- 
          ,@(map (transform-rule-item remove-wildcards-clause remove-wildcards-source-tree-rule) bodys)) ,pos))

(define/contract-cond (desugar-segmented-rule rule)
  (source-tree-rule? . -> . source-tree-rule?)
  ; (printf "desugar-segmented-rule input: \n~a\n" rule)
  (match-define `(prov ,xs ,pos) rule)
  ; (printf "desugar-segmented-rule segments: \n~a\n" (intercalate "\n" (map strip-prov (list-split xs '(--)))))
  (match (list-split xs '(--))
    [`(,segments ... ,next-to-last-segment ,last-segment)
      (define new-rule `(INNER-RULE (prov ,last-segment ,pos)))
      (match-define `(,heads ... <-- ,bodys ...) next-to-last-segment)
      (define new-next-to-last-segment `(,@(map (transform-if-inner-rule desugar-segmented-rule) heads) <-- 
                                         ,@(map (transform-if-inner-rule desugar-segmented-rule) bodys) ,new-rule))
      (define new-segmented-rule `(prov ,(append* (add-between (append segments (list new-next-to-last-segment)) '(--))) ,pos))
      (desugar-segmented-rule new-segmented-rule)]
    [`(,only-segment)
      (match-define `(prov (,heads ... <-- ,bodys ...) ,pos) rule)
      `(prov (,@(map (transform-if-inner-rule desugar-segmented-rule) heads) <-- 
              ,@(map (transform-if-inner-rule desugar-segmented-rule) bodys)) ,pos)]))

; desugares !-clauses (!(r ...) in the body) by introducing an inner rule that has the bang-clause
; in the head, and clauses appearing before it in the body
(define/contract-cond (desugar-bang-clauses rule)
  (source-tree-rule? . -> . source-tree-rule?)
  (define (get-bangs clause) ;; returns a list of banged subclauses
    (match clause
          ['... '()]
          [`(prov (LIST-SYNTAX ,args ...) ,pos)
          (apply append (map get-bangs args))]
          [`(prov (! ,banged-clause) ,pos)
          (list `(prov ,banged-clause ,pos))]
          [`(prov (,tag ,cls ...) ,pos)
          (apply append (map get-bangs cls))]
          [`(prov ,(? arg? v) ,pos)
          '()]
          [`(INNER-RULE ,inner-rule)
          '()]))
  (define (remove-bangs clause)
    (match clause
          [`(prov (! ,banged-clause) ,pos)
          `(prov ,banged-clause ,pos)]
          [`(prov (,tag ,cls ...) ,pos)
          `(prov (,tag ,@(map remove-bangs cls)) ,pos)]
          [else clause]))
  (define (is-bangful clause) (not (empty? (get-bangs clause))))

  (match-define `(prov (,head-clauses ... <-- . ,body-clauses) ,pos)  rule)
  (define rule+
    `(prov (,@(map (transform-if-inner-rule desugar-bang-clauses) head-clauses) <-- 
            ,@(map (transform-if-inner-rule desugar-bang-clauses) body-clauses)) ,pos))
  
  (define (vars-outside-bang-clauses cl)
    (match cl
          ['... (set)]
          [`(prov (LIST-SYNTAX ,args ...) ,pos)
            (foldl set-union (set) (map vars-outside-bang-clauses args))]
          [`(prov (! ,banged-clause) ,pos)
            (set)]
          [`(prov (,tag ,cls ...) ,pos)
            (foldl set-union (set) (map vars-outside-bang-clauses cls))]
          [`(prov ,(? arg? v) ,pos)
            (if (var? v) (set v) (set))]
          [`(INNER-RULE ,inner-rule)
            (match-define `(prov (,ir-heads ... <-- . ,ir-bodys) ,pos) inner-rule)
            (foldl set-union (set) (map vars-outside-bang-clauses 
                                        (append ir-bodys (filter (not/c inner-rule?) ir-heads))))]))
  (define (vars-inside-bang-clauses cl)
    (define banged-clauses (get-bangs cl))
    (foldl set-union (set) (map vars-outside-bang-clauses banged-clauses)))

  ;; TODO remove at some point
  (define (step-OLD rule)
    (match-define `(prov (,head-clauses ... <-- . ,body-clauses) ,pos)  rule)
            
    (define last-bang-ind (match (indexes-where body-clauses is-bangful) ['() #f] [xs (last xs)]))
    (cond
      [last-bang-ind 
        (define after-last-bang (drop body-clauses (add1 last-bang-ind)))
        (define before-last-bang (take body-clauses last-bang-ind))
        (define last-caluse-with-bangs (list-ref body-clauses last-bang-ind))
        (define create-bangs-inner-rule 
        `(INNER-RULE (prov (,@(get-bangs last-caluse-with-bangs) <--
                            ,@after-last-bang) ,pos)))
        `(prov (,@head-clauses <-- 
                ,@before-last-bang ,(remove-bangs last-caluse-with-bangs) ,create-bangs-inner-rule) ,pos)]
      [else rule]))

  (define (step rule)
    (match-define `(prov (,head-clauses ... <-- . ,body-clauses) ,pos)  rule)
            
    (define last-bang-ind (match (indexes-where body-clauses is-bangful) ['() #f] [xs (last xs)]))
    (cond
      [last-bang-ind 
        (define after-last-bang (drop body-clauses (add1 last-bang-ind)))
        ; (define last-bang-block-start-ind last-bang-ind)
        (define vars-after-last-bang (foldl set-union (set) (map vars-outside-bang-clauses after-last-bang)))
        (define last-bang-block-start-ind (second
          (foldl (λ (ind accu) 
                    (match-define (list stop ans output-vars) accu)
                    (cond 
                      [stop accu]
                      [(not (is-bangful (list-ref body-clauses ind))) (list #t ans output-vars)]
                      [else
                        (define current-clause-vars-inside-bang-clauses (vars-inside-bang-clauses (list-ref body-clauses ind)))
                        (define current-clause-output-vars (vars-outside-bang-clauses (list-ref body-clauses ind)))
                        (define current-clause-output-vars+
                          (set-subtract current-clause-output-vars vars-after-last-bang))
                        (cond
                          [(set-empty? (set-intersect current-clause-vars-inside-bang-clauses output-vars))
                            (list #f ind (set-union current-clause-output-vars+ output-vars))]
                          [else
                            (list #t ans output-vars)])]))
                 (list #f last-bang-ind (set))
                 (range last-bang-ind -1 -1))))
        
        (define before-last-bang-block (take body-clauses last-bang-block-start-ind))
        (define last-clauses-with-bangs-block (take (drop body-clauses last-bang-block-start-ind) (add1 (- last-bang-ind last-bang-block-start-ind))))
        (define create-bangs-inner-rule 
        `(INNER-RULE (prov (,@(flat-map get-bangs last-clauses-with-bangs-block) <--
                            ,@after-last-bang) ,pos)))
        `(prov (,@head-clauses <-- 
                ,@before-last-bang-block ,@(map remove-bangs last-clauses-with-bangs-block) ,create-bangs-inner-rule) ,pos)]
      [else rule]))

  (iterate-to-fixed-point step rule+))

(define/contract-cond (desugar-inner-rules rule [context-vars '()])
  (source-tree-rule? . -> . (listof source-tree-rule?))
  (define (get-replacement-clause-for-inner-rule inner-rule context-vars)
    (match-define `(prov (,ir-head-clauses ... <-- . ,ir-body-clauses) ,ir-pos) inner-rule)

    (define all-clauses-vars (flat-map source-tree-clause-vars (append ir-head-clauses ir-body-clauses)))
    (define shared-vars (filter (λ (v) (member (strip-prov v) context-vars)) all-clauses-vars))
    (define vars (remove-duplicates shared-vars #:key strip-prov))
    (define new-rel-name (gensymb '$bir-sub))
    (define replacement-clause
      (give-clause-id `(prov ((prov ,new-rel-name ,ir-pos) ,@vars) ,ir-pos)))
    replacement-clause)

  (match-define `(prov (,head-clauses ... <-- . ,body-clauses) ,pos) rule)
  (match-define (cons body-clauses+ extracted-inner-rules)
    (foldl 
      (λ (cl accu) 
        (match-define (cons body-clauses+ extracted-inner-rules) accu)
        (match cl
          [`(INNER-RULE ,inner-rule)
            (define other-vars (map strip-prov (flat-map source-tree-clause-vars (filter (λ (x) (not (equal? x cl))) (append head-clauses body-clauses)))))
            (match-define (cons inner-rule-inner-rules-desugared0 inner-rule-extracted-inner-rules)
              (desugar-inner-rules inner-rule (append context-vars other-vars)))
            (define inner-rule-inner-rules-desugared (remove-wildcards-source-tree-rule inner-rule-inner-rules-desugared0))
            (match-define `(prov (,ir-head-clauses ... <-- . ,ir-body-clauses) ,ir-pos) inner-rule-inner-rules-desugared)
            (define replacement-clause (get-replacement-clause-for-inner-rule inner-rule-inner-rules-desugared (append context-vars other-vars)))
            (define new-inner-rule `(prov (,@ir-head-clauses ,replacement-clause <-- ,@ir-body-clauses) ,ir-pos))
            (cons (append body-clauses+ (list replacement-clause)) 
                  (append extracted-inner-rules (cons new-inner-rule inner-rule-extracted-inner-rules)))]
          [else (cons (append body-clauses+ (list cl)) extracted-inner-rules)]))
      (cons '() '())
      body-clauses))
  (define new-rule `(prov (,@head-clauses <-- ,@body-clauses+) ,pos))
  (cons new-rule extracted-inner-rules))

(define/contract-cond (desugar-head-inner-rules rule0)
  (source-tree-rule? . -> . (listof source-tree-rule?))
  (define (get-rule-vars r)
    (match-define `(prov (,hcls ... <-- . ,bcls) ,pos) r)
    (flat-map source-tree-clause-vars (append (filter (not/c inner-rule?) hcls) bcls)))

  ;; first we deal with head-inner-rules inside inner-rules  
  (match-define `(prov (,head-clauses0 ... <-- . ,body-clauses0) ,pos0) rule0)
  (define head-inner-rules-desugared
    (flat-map desugar-head-inner-rules (filter-map inner-rule? head-clauses0)))
  (assert ((listof source-tree-rule?) head-inner-rules-desugared))
  (define body-inner-rules-desugared
    (map desugar-head-inner-rules (filter-map inner-rule? body-clauses0)))
  (define (tag-inner-rule r) `(INNER-RULE ,r))
  (define rule+
    `(prov (,@(map tag-inner-rule head-inner-rules-desugared) ,@(filter (not/c inner-rule?) head-clauses0) <--
            ,@(map tag-inner-rule (map car body-inner-rules-desugared)) ,@(filter (not/c inner-rule?) body-clauses0)) ,pos0))
  (assert (source-tree-rule? rule+) (format "bad rule: ~a\n" (strip-prov rule+)))

  (define extra-desuagred-rules (flat-map cdr body-inner-rules-desugared))
  (assert ((listof source-tree-rule?) extra-desuagred-rules))
  (match-define `(prov (,head-clauses ... <-- . ,body-clauses) ,pos) rule+)
  (define head-inner-rules (filter-map inner-rule? head-clauses))
  (define (add-context-clause-to-rule ir cl)
    (match-define `(prov (,hcls ... <-- . ,bcls) ,pos) ir)
    (define has-body-inner-rules (ormap inner-rule? bcls))
    (cond
      [has-body-inner-rules
        `(prov (,@hcls <-- ,@(map (transform-if-inner-rule (app add-context-clause-to-rule _ cl)) bcls)) ,pos)]
      [else
        `(prov (,@hcls <-- ,@bcls ,cl) ,pos)]))
  (cond
    [(not (empty? head-inner-rules))
     (define head-inner-rules-vars (flat-map get-rule-vars head-inner-rules))
     (define rule-vars (get-rule-vars `(prov (,@(filter (not/c inner-rule?) head-clauses) <-- . ,body-clauses) ,pos)))
     (define head-inner-rules-vars-stripped (remove-duplicates (map strip-prov head-inner-rules-vars)))
     (define common-vars0 (filter (λ (v) (member (strip-prov v) head-inner-rules-vars-stripped)) rule-vars))
     (define common-vars (remove-duplicates common-vars0 #:key strip-prov))
     (define replacement-clause (give-clause-id `(prov ((prov ,(gensymb '$hir-sub) ,pos) ,@common-vars) ,pos)))
     (define new-rule `(prov (,replacement-clause ,@(filter (not/c inner-rule?) head-clauses) <-- . ,body-clauses) ,pos))
     (assert (source-tree-rule? new-rule) (strip-prov new-rule))
     (define new-head-inner-rules
      (map (app add-context-clause-to-rule _ replacement-clause) head-inner-rules))
     (assert ((listof source-tree-rule?) new-head-inner-rules))
     (cons new-rule (append new-head-inner-rules extra-desuagred-rules))]
    [else (cons rule+ extra-desuagred-rules )]))

;; tunrns [a b c ... d e] into (cons a (cons b [c ... d e]))
(define (list-clause-pull-out-pre-splice-vars lst-cl)
  (match lst-cl
      [`(prov (LIST-SYNTAX ,largs ... ,splice-arg ,'... ,rargs ...) ,pos)
       (define list-tail `(prov (LIST-SYNTAX ,splice-arg ,'... ,@rargs) ,pos))
       (gather-into-slog-list-with-tail largs list-tail pos)]
      [else lst-cl]))

(define (not-dots? x) (not (equal? x '...)))

(define/contract-cond (desplice-rule-body rule)
  (source-tree-rule? . -> . (listof source-tree-rule?))
  ;; replace splices and return a pair of the new clause + mapping from variable names to splcies
  (define(cl-replace-splices cl)
    ; (printf "cl-replace-splices input: \n~a\n" (pretty-format cl))
    (match cl
      [`(prov (LIST-SYNTAX ,largs ... ,splice-var ,'... ,rargs ..1) ,pos)
        ; (define list-tail-var `(prov ,(gensymb '__list-var) ,pos))
        ; (define list-tail `(prov (LIST-SYNTAX ,splice-var ,'... ,@rargs) ,pos))
        ; (define replacement-clause (gather-into-slog-list-with-tail largs list-tail-var pos))
        ; (cons replacement-clause (hash list-tail-var list-tail))
        (define splice-var `(prov ,(gensymb '$lstvar) ,pos))
        (cons splice-var (hash splice-var cl))
        ]
      [`(prov (LIST-SYNTAX ,args ...) ,pos)
        (cons cl (hash))]
      [`(prov ,(? arg? v) ,pos) (cons cl (hash))]
      [`(prov (! ,banged-clause) ,pos) (cons cl (hash))]
      [`(prov (,tag ,cls ...) ,pos) 
        (match-define (cons new-cls mappings) 
          (foldl (λ (cl accu)
                    (match-define (cons cl-lst mapping) accu)
                    (match-define (cons new-cl new-mapping) (cl-replace-splices cl))
                    (cons (append cl-lst (list new-cl)) (hash-union mapping new-mapping)))
            (cons '() (hash))
            cls))
        (cons `(prov (,tag ,@new-cls) ,pos) mappings)]
      [`(prov (? ,huhcl) ,pos) 
        (error "huh in body?")]
      [else (cons cl (hash))])) ;; TOOD why the else case?

  (define (get-clause-tag cl)
    (match cl 
      [`(prov ((prov = ,=pos) (prov ,id ,idpos) (prov ((prov ,tag ,tagpos) ,args ...) ,pos1)) ,pos0) tag]
      [`(prov ((prov ,tag ,tagpos) ,args ...) ,pos) tag]
      [else #f]))
      
  (define (get-rules-for-splice var splice relevant-clauses)
    (define relevant-clauses+ (filter (λ (cl) (define tag (get-clause-tag cl)) (and tag (not (builtin? tag)))) relevant-clauses))
    ; (define relevant-clauses++ (cons (assert-var-is-list-clause var (prov->pos var)) relevant-clauses+))
    (define relevant-clauses++ (if (empty? relevant-clauses+) (list (assert-var-is-list-clause var (prov->pos var))) relevant-clauses+))

    (match splice
      [`(prov (LIST-SYNTAX ,(? not-dots? largs) ... ,splice-arg1 ,'... ,(? not-dots? margs) ... ,splice-arg2 ,'... ,rargs ...) ,pos)
       (get-rules-for-list-clause-with-multiple-splicing-vars var splice relevant-clauses++)]
      [else 
       (get-rules-for-list-clause-with-one-splicing-var var splice relevant-clauses++)]))

  (define (assert-var-is-list-clause var pos) 
    `(prov ((prov = ,pos) ,var (prov ((prov $lst ,pos) (prov ,(gensymb '$_) ,pos) (prov ,(gensymb '$_) ,pos)) ,pos)) ,pos))

  (define (get-splice-components splice) 
    (match splice [`(prov (LIST-SYNTAX ,largs ... ,splice-arg ,'... ,rargs ...) ,pos) (list largs splice-arg rargs)]))
  (define (get-rules-for-list-clause-with-one-splicing-var var splice relevant-clauses)
    (match-define (list pre-splice-args splice-var post-splice-args) (get-splice-components splice) )
    (define pos (prov->pos splice))
    (define rule1-head `(prov ((prov ,(gensymb '$splice) ,pos) ,var ,@pre-splice-args ,splice-var ,@post-splice-args) ,pos))
    (define rule1-body 
    `(prov ((prov breaks-into ,pos) (prov (! ((prov breaks-into-input ,pos)
                                              (prov ,(length pre-splice-args) ,pos)
                                              (prov ,(length post-splice-args) ,pos) ,var)) ,pos) 
                                    ,(gather-into-slog-list pre-splice-args pos) 
                                    ,splice-var 
                                    ,(gather-into-slog-list post-splice-args pos)) ,pos))
    (define rule1 `(prov (,rule1-head <-- ,rule1-body ,@relevant-clauses) ,pos))
    (assert (source-tree-rule? rule1) (format "bad rule: ~a" (strip-prov rule1)))
    ; (printf "rule1: ~a\n" (strip-prov rule1))
    (define rule1-debanged (desugar-inner-rules (rule-desugar-iclauses (desugar-bang-clauses rule1))))
    (for ([r rule1-debanged]) (assert (source-tree-rule? r)))
    (cons rule1-debanged rule1-head))
  
  (define (get-rules-for-list-clause-with-multiple-splicing-vars var list-clause relevant-clauses)
    (match-define `(prov (LIST-SYNTAX ,(? not-dots? largs) ... ,splice-arg ,'... ,rargs ...) ,pos) list-clause)
    (define post-splice-args-list `(prov (LIST-SYNTAX ,@rargs) ,pos))
    (define post-splice-args-dots-removed (filter (λ (x) (not (equal? x '...))) rargs))
    (define rule1-head `(prov ((prov ,(gensymb '$splice) ,pos) ,var ,@largs ,splice-arg ,@post-splice-args-dots-removed) ,pos))
    (define rule1-body 
    `(prov ((prov $lst-split ,pos) (prov (! ((prov $lst-split-input ,pos)
                                              ,var)) ,pos) 
                                    ,(gather-into-slog-list-with-tail largs splice-arg pos) 
                                    ,post-splice-args-list) ,pos))
    
    (define rule1 `(prov (,rule1-head <-- ,rule1-body ,@relevant-clauses) ,pos))
    (assert (source-tree-rule? rule1) (printf "bad rule: ~a\n" (strip-prov rule1)))
    (define rule1-debanged (desugar-inner-rules (rule-desugar-iclauses (desugar-bang-clauses rule1))))
    (define rule1-body_despliced (flat-map desplice-rule-body rule1-debanged))
    (for ([r rule1-body_despliced]) (assert (source-tree-rule? r)))
    (cons rule1-body_despliced rule1-head))
  
  (match-define `(prov (,head-clauses ... <-- . ,body-clauses) ,pos) rule)
  (match-define (cons despliced-clauses var-splice-mappings)
    (foldl (λ (cl accu)
                (match-define (cons cls mappings) accu)
                (match-define (cons new-cl cl-mappings) (cl-replace-splices cl))
                (cons (append cls (list new-cl)) (hash-union cl-mappings mappings)))
            (cons '() (hash))
            body-clauses))
  (match-define (cons splice-rules new-clauses)
    (foldl (λ (var accu)
              (match-define (cons splice-rules new-clauses) accu)
              (define clauses-containing-var (apply append (map (app sub-clauses-containing-var _ var) despliced-clauses)))
              (match-define (cons var-splice-rules var-new-clause) (get-rules-for-splice var (hash-ref var-splice-mappings var) clauses-containing-var))
              (cons (append splice-rules var-splice-rules) (append new-clauses (list var-new-clause))))
            (cons '() '())
            (hash-keys var-splice-mappings)))
  (define new-rule `(prov (,@head-clauses <--  ,@(append despliced-clauses new-clauses)) ,pos))
  (assert (source-tree-rule? new-rule) (format "bad new-rule: ~a" new-rule))
  (for ([sr splice-rules])
    (assert (source-tree-rule? sr) (format "bad splice-rule: \n~v\n stripped:\n~a\n" sr (strip-prov sr))))
  (cons new-rule splice-rules))

(define/contract-cond (desplice-rule-head rule)
  (source-tree-rule? . -> . (listof source-tree-rule?))

  ;; returns a list of list syntaxes in the clause
  (define (get-list-syntaxes-in-clause cl)
    (match cl
      [`(prov (LIST-SYNTAX ,args ...) ,pos)
        (append (list cl) (flat-map get-list-syntaxes-in-clause (filter not-dots? args)))]
      [`(prov ,(? arg? v) ,pos) '()]
      [`(prov (! ,banged-clause) ,pos) (get-list-syntaxes-in-clause `(prov ,banged-clause ,pos))]
      [`(prov (,tag ,cls ...) ,pos) (flat-map get-list-syntaxes-in-clause cls)]
      [`(prov (? ,huhcl) ,pos) (get-list-syntaxes-in-clause `(prov ,huhcl ,pos))]))
  
  ;; we could turn it into has-splicing-vars-in-non-tail-position
  (define (has-splicing-vars list-syntax)
    (match list-syntax
      [`(prov (LIST-SYNTAX ,vars ... ,lst ,'... ,after-lst ,rest ...) ,pos) #t]
      [else #f]))

  
  ;; for the given list of head clauses containing list syntax with splicing vars, return a list of
  ;; clauses to replace them, and a list of rules to create the list and the clauses
  (define (head-cls-replace-splices cls)
    (define cls-dep-graph (head-clauses-dependency-graph cls))
    (define cls-sorted (topological-sort cls-dep-graph))
    (define split-point (index-where cls-sorted (λ (cl) (ormap has-splicing-vars (get-list-syntaxes-in-clause cl)))))
    (cond 
      [split-point
        (define cl-with-splicing-vars (list-ref cls-sorted split-point))
        (define all-dependents (hash-ref (hash-graph-transitive-closure cls-dep-graph) cl-with-splicing-vars))
        (define-values (after-split0 before-split) 
          (partition (app set-member? all-dependents _) (remove cl-with-splicing-vars cls-sorted)))
        (define after-split (cons cl-with-splicing-vars after-split0))
        (assert (= (+ (length after-split) (length before-split)) (length cls)))
        (define splicing-clause (findf has-splicing-vars (get-list-syntaxes-in-clause cl-with-splicing-vars)))
        (match-define `(prov (LIST-SYNTAX ,@splicing-clause-elements) ,pos) splicing-clause)
        (define replace-rel-name (gensymb '$head-splice))
        (define clause-vars-before-split (append (flat-map get-flattened-clause-args before-split) (flat-map get-clause-id before-split)))
        (define clause-vars-after-split 
          (remove* (flat-map get-clause-id after-split) 
                   (flat-map get-flattened-clause-args after-split)
                   (λ (x y) (equal? (strip-prov x) (strip-prov y)))))
        ; (printf "clause-vars-before-split: ~a, after-split: ~a\n" (map strip-prov clause-vars-before-split) (map strip-prov clause-vars-after-split))
        (define replace-clause-vars (append #;clause-vars-before-split clause-vars-after-split))
        (define replace-clause `(prov ((prov ,replace-rel-name ,pos) ,@replace-clause-vars) ,pos))
        
        (match-define (list stuff lst1 lst2)
          (match splicing-clause-elements
            [`(,stuff ... ,lst1 ,'... ,(? not-dots? xs) ..1)
              (define xs-as-slog-list (gather-into-slog-list xs pos))
              (list stuff lst1 xs-as-slog-list)]
            [`(,stuff ... ,'... ,(? not-dots? xs) ..1 ,lst2 ,'...)
              (define xs-as-slog-list (gather-into-slog-list xs pos))
              (list (append stuff '(...)) xs-as-slog-list lst2)]
            [`(,stuff ... ,lst1 ,'... ,lst2 ,'...)
              (list stuff lst1 lst2)]
            [else (error "bad splicing-clause: ~a\n" (strip-prov splicing-clause))]))
        (cond
          [(not stuff) (cons cls '())]
          [else
            (define combined-lst1-lst2-var 
              `(prov ,(gensymb (string->symbol (format "$~a-~a" (var-name-for-clause lst1) (var-name-for-clause lst2)))) ,pos))
            (define new-splicing-clause 
              (if (empty? stuff) combined-lst1-lst2-var 
                                `(prov (LIST-SYNTAX ,@stuff ,combined-lst1-lst2-var ,'...) ,pos)))
            (define updated-cl (map (app replace-sub-clauses _ splicing-clause new-splicing-clause) after-split))
            (match-define (cons updated-cl-processed new-side-rules) (head-cls-replace-splices updated-cl))
            (define rule1 
              `(prov [,@updated-cl-processed
                      <--
                      (prov ((prov $lst-append ,pos) (prov (! ((prov $lst-append-input ,pos) ,lst1 ,lst2)) ,pos) ,combined-lst1-lst2-var) ,pos)
                      ,replace-clause] ,pos))
            (define rule1-debanged
              (flat-map desplice-rule-head (flat-map desplice-rule-body (desugar-inner-rules (desugar-bang-clauses rule1)))))
            (cons (append before-split (list replace-clause)) (append rule1-debanged new-side-rules))])]
      ['() (cons cls '())]))

  
  (match-define `(prov (,head-clauses ... <-- . ,body-clauses) ,pos) rule)
  (match-define (cons new-head-clauses new-rules-0)
    (head-cls-replace-splices head-clauses))
  (define updated-rule0 `(prov [,@new-head-clauses <-- . ,body-clauses] ,pos))
  (match-define (cons updated-rule new-rules-1) 
    (if (ormap has-splicing-vars (flat-map get-list-syntaxes-in-clause new-head-clauses))
      (desplice-rule-head updated-rule0)
      (cons updated-rule0 '())))
  (define new-rules (append new-rules-0 new-rules-1))

  (match-define `(prov [,new-head-clauses+ ... <-- . ,_] ,_) updated-rule)
  (for* ([cl new-head-clauses+]
         [list-cl (get-list-syntaxes-in-clause cl)])
    (assert (not (has-splicing-vars list-cl)) 
            (format "bad head clause generated by desplice-rule-head:\n~a\ninput: ~a\nres: ~a" 
                    (strip-prov cl) (strip-prov rule) (strip-prov updated-rule))))
  
  (assert (source-tree-rule? updated-rule) (format "updated-rule: \n~a\n" (strip-prov updated-rule)))
  (cons updated-rule new-rules))

;; creates a builtin slog list with the args
(define (gather-into-slog-list args pos)
  (gather-into-slog-list-with-tail args `(prov ((prov $nil ,pos)) ,pos) pos))
;; crates the builtin slog list [arg1 arg2 ... tail ...]
(define (gather-into-slog-list-with-tail args tail pos)
  (foldr (λ (arg accu) `(prov ((prov $lst ,pos) ,arg ,accu) ,pos)) tail args))  

;; remove sequence syntax and replaces it with the builtin slog list relation
(define/contract-cond (desequence-rule rule)
  (source-tree-rule? . -> . source-tree-rule?)
  (define (desequence-clause cl)
    (match cl
      [`(prov (LIST-SYNTAX ,args ... ,lst ,'...) ,pos) 
        (gather-into-slog-list-with-tail (map desequence-clause args) (desequence-clause lst) pos)]
      [`(prov (LIST-SYNTAX ,args ...) ,pos) 
        (gather-into-slog-list (map desequence-clause args) pos)]
      [`(prov ,(? arg? v) ,pos) cl]
      [`(prov (! ,banged-clause) ,pos) 
        (match-define `(prov ,deseq ,posn) (desequence-clause `(prov ,banged-clause ,pos))) 
        `(prov (! ,deseq) ,posn)]
      [`(prov (,tag ,cls ...) ,pos) `(prov (,tag ,@(map desequence-clause cls)) ,pos)]
      [`(prov (? ,huhcl) ,pos) 
        (match-define `(prov ,deseq ,posn) (desequence-clause `(prov ,huhcl ,pos)))
        `(prov (? ,deseq) ,posn)]
      [else cl])) ;; not sure about this case ...
  
  (match-define `(prov (,head-clauses ... <-- . ,body-clauses) ,pos) rule)
  (define res `(prov (,@(map desequence-clause head-clauses) <-- ,@(map desequence-clause body-clauses)) ,pos))
  (assert (source-tree-rule? res) (format "bad rule: ~a\ninput rule: ~a\n" (strip-prov res) (strip-prov rule)))
  res)

;; in `cl`, replace all subclauses matching `sub-cl` with `replacement`
(define (replace-sub-clauses cl sub-cl replacement)
  (visit-replace-sub-clauses cl (λ (x) (if (equal? x sub-cl) replacement #f))))

;; the `visitor` is a function that either returns a new clause to be replaced with the current clause,
;; or `#f` or `(void)` to indicate that we should keep looking at subclauses
(define (visit-replace-sub-clauses cl visitor)
  (define visitor-res (visitor cl))
  (match cl
    [_ #:when (and visitor-res (not (void? visitor-res))) visitor-res]
    [`(prov (LIST-SYNTAX ,args ...) ,pos)
      `(prov (LIST-SYNTAX 
             ,@(map (λ (sub-cl) (if (equal? sub-cl '...) '... (visit-replace-sub-clauses sub-cl visitor))) args)) 
        ,pos)]
    [`(prov ,(? arg? v) ,pos) cl]
    [`(prov (! ,banged-clause) ,pos) `(prov (! ,(visit-replace-sub-clauses banged-clause visitor)) ,pos)]
    [`(prov (,tag ,cls ...) ,pos) `(prov (,tag ,@(map (app visit-replace-sub-clauses _ visitor) cls)) ,pos)]
    [`(prov (? ,huhcl) ,pos) `(prov (? ,(visit-replace-sub-clauses huhcl visitor)) ,pos)]))

(define/contract (rule-visit-replace-sub-clauses rule visitor)
  (source-tree-rule? procedure? . -> . source-tree-rule?)
  (match-define `(prov ,rule-items ,pos) rule)
  `(prov ,(map (λ (cl) 
                (match cl
                  ['<-- '<--]
                  [`(INNER-RULE ,ir) `(INNER-RULE ,(rule-visit-replace-sub-clauses ir visitor))]
                  [x (visit-replace-sub-clauses x visitor)])) rule-items) ,pos))

;; return if the cluase has this var as one of its **CHILDREN**
(define (cluase-has-var cl var-prov)
  (match-define `(prov ,var ,pos) var-prov)
  (match cl
    [`(prov (LIST-SYNTAX ,args ...) ,pos)
      (ormap (λ (arg) (equal? (strip-prov arg) var)) args)]
    [`(prov ,(? arg? v) ,pos) #f]
    [`(prov (! ,banged-clause) ,pos) (cluase-has-var `(prov ,banged-clause ,pos) var-prov)]
    [`(prov (,tag ,cls ...) ,pos) (ormap (λ (arg) (equal? (strip-prov arg) var)) cls)]
    [`(prov (? ,huhcl) ,pos) (cluase-has-var `(prov ,huhcl ,pos) var-prov)]
    [else #f])) ;;TODO why the else case?

(define (sub-clauses-containing-var cl var)
  (match cl
    [`(prov (LIST-SYNTAX ,args ...) ,pos)
      (if (cluase-has-var cl var) (list cl) '())]
    [`(prov ,(? arg? v) ,pos) (list)]
    [`(prov (! ,banged-clause) ,pos) 
      (append (if (cluase-has-var cl var) (list cl) '()) 
              (filter (λ (subcl) (not (equal? cl subcl))) (sub-clauses-containing-var `(prov ,banged-clause ,pos) var)))]
    [`(prov (,tag ,cls ...) ,pos) 
      (append (if (cluase-has-var cl var) (list cl) '()) (apply append (map (app sub-clauses-containing-var _ var) cls)))]
    [`(prov (? ,huhcl) ,pos)
      (append (if (cluase-has-var cl var) (list cl) '()) 
              (filter (λ (subcl) (not (equal? cl subcl))) (sub-clauses-containing-var `(prov ,huhcl ,pos) var)))]
    [else '()])) ;; TODO why the else case?

(define/contract-cond (source-tree-clause-vars cl)
  ((or/c source-tree-hclause? source-tree-body-item?) . -> . list?)
  (match cl
    [`(prov (LIST-SYNTAX ,args ...) ,pos)
      (flat-map source-tree-clause-vars (filter (λ (x) (not (equal? x '...))) args))]
    [`(prov ,(? symbol? x) ,pos) (list cl)]
    [`(prov ,(? lit? v) ,pos) (list)]
    [`(prov (! ,banged-clause) ,pos) 
      (source-tree-clause-vars `(prov ,banged-clause ,pos))]
    [`(prov (? ,huhcl) ,pos)
      (source-tree-clause-vars `(prov ,huhcl ,pos))]
    [`(prov (,tag ,cls ...) ,pos) 
      (flat-map source-tree-clause-vars cls)]
    [`(INNER-RULE ,inner-rule) 
      (match-define `(prov (,heads ... <-- ,bodys ...) ,pos) inner-rule)
      (flat-map source-tree-clause-vars (append (filter (not/c inner-rule?) heads) bodys))]
    [else '()]))

(define (var-name-for-clause cl [fallback '__var])
  (match cl
    [`(prov ,(? symbol? s) ,pos) s]
    [`(prov ,(? lit? l) ,pos) (string->symbol (format "lit_~a" l))]
    [else fallback]))

(define ((transform-if-inner-rule transformer) body-item)
  (match body-item
    [`(INNER-RULE ,inner-rule) `(INNER-RULE ,(transformer inner-rule))]
    [else body-item]))

;; takes two args, first for transforming clauses, second for transforming inner rules
(define ((transform-rule-item clause-transformer rule-transformer) body-item)
  (match body-item
    [`(INNER-RULE ,inner-rule) `(INNER-RULE ,(rule-transformer inner-rule))]
    [else (clause-transformer body-item)]))

(define (inner-rule? cl)
  (match cl [`(INNER-RULE ,ir) ir] [else #f]))

(define/contract-cond (source-tree-rule-static-unification-pass rule)
  (source-tree-rule? . -> . source-tree-rule?)
  (define (clauses-equal cl1 cl2)
    ;;TODO is it good enough?
    (equal? (strip-prov cl1) (strip-prov cl2)))
  
  (define (constraints-for-equal-clauses cl1 cl2)
    (match-define `(prov ((prov ,cl1tag ,cl1tagpos) ,cl1-args ...) ,cl1pos) cl1)
    (match-define `(prov ((prov ,cl2tag ,cl2tagpos) ,cl2-args ...) ,cl2pos) cl2)
    (cond 
      [(and (equal? cl1tag cl2tag) (= (length cl1-args) (length cl2-args)))
       (map (λ (arg1 arg2) `(prov ((prov = ,cl2pos) ,arg1 ,arg2) ,cl2pos)) cl1-args cl2-args)]
      [else
       #f]))

  (define/contract-cond (repeated-clause-unification-step rule)
    (source-tree-rule? . -> . source-tree-rule?)
    (define (clause-rhs-unified-with-body-item clause body-item)
      (match body-item
        [`(INNER-RULE (prov (,ir-heads ... <-- ,ir-bodys ...) ,ir-pos))
          (foldl (λ (ir-item updated-caluses) (flat-map (app clause-rhs-unified-with-body-item _ ir-item) updated-caluses))
                 (list clause)
                 (append ir-heads ir-bodys))]
        [`(prov ((prov = ,=pos) (prov ,id ,idpos) ,body-item-rhs) ,pos)
         (match clause
           [`(prov ((prov = ,cl=pos) (prov ,clid ,clidpos) ,cl-rhs) ,clpos) 
            #:when (and (clauses-equal body-item-rhs cl-rhs) (not (arg? (strip-prov cl-rhs))))
            (list `(prov ((prov = ,cl=pos) (prov ,clid ,clidpos) (prov ,id ,idpos)) ,clpos))]
           [`(prov ((prov = ,cl=pos) (prov ,clid ,clidpos) ,cl-rhs) ,clpos) 
            #:when (and (equal? clid id) (not ((prov-of arg?) cl-rhs)) (not ((prov-of arg?) body-item-rhs)))
            (constraints-for-equal-clauses body-item-rhs cl-rhs)]
           [else (list clause)])]))
    (match-define `(prov (,heads ... <-- ,bodys0 ...) ,pos) rule)
    (define bodys (map (transform-if-inner-rule repeated-clause-unification-step) bodys0))
    (define rule-items `(,@heads <-- ,@bodys))
    (define new-rule-items
      (foldl 
        (λ (idx accu)
          (define cl (list-ref rule-items idx))
          (define updated-cl (match cl
            ['<-- (list '<--)]
            [`(INNER-RULE ,_) (list cl)]
            [else
              (define new-cls?
                (foldl (λ (rule-item cl?)
                        (match cl?
                          [`(not-updated ,cl)
                            (define constraints (clause-rhs-unified-with-body-item cl rule-item))
                            (when (not constraints) 
                              (pretty-error-current pos (format "inconsistent constraints: ~a, ~a" (strip-prov rule-item) (strip-prov cl))
                                #:exit #t))
                            (if (equal? (list cl) constraints) `(not-updated ,cl) `(updated ,constraints))]
                          [else cl?]))
                      `(not-updated ,cl)
                      (remove '<-- (drop rule-items (add1 idx)))))
              (match new-cls? [`(not-updated ,cl) (list cl)] [`(updated ,cls) cls])]))
          (append accu updated-cl))
        '()
        (range 0 (length rule-items))))
    (define res `(prov ,new-rule-items ,pos))
    (assert (source-tree-rule? res) (format "~a" (simplify-prov res)))
    res)


  ;; returns a list of variables equal to the given var in the given rule-item
  (define (equal-args var rule-item)
    (match rule-item
      [`(INNER-RULE ,ir) (rule-equal-args var ir)]
      [`(prov ((prov = ,=pos) (prov ,(? arg? x) ,xpos) (prov ,(? arg? y) ,ypos)) ,pos) #:when (equal? x var) (list y)]
      [`(prov ((prov = ,=pos) (prov ,(? arg? x) ,xpos) (prov ,(? arg? y) ,ypos)) ,pos) #:when (equal? y var) (list x)]
      [else '()]))

  (define (rule-equal-args var rule)
    (match-define `(prov (,heads ... <-- ,bodys ...) ,pos) rule)
    (flat-map (app equal-args var _) (append heads bodys)))

  (define (get-canonical-var vars)
    (define (var-score v)
      (assert (arg? v))
      (define var-str (if (symbol? v) (symbol->string v) #f))
      (cond
        [(not (var? v)) 1]
        [(string-prefix? var-str "$_") -3]
        [(string-prefix? var-str "$") -2]
        [(string-prefix? var-str "_") -1]
        [else 0]))
    (argmax var-score vars))

  (define (cl-replace-var cl old-var new-var)
    (visit-replace-sub-clauses cl
            (λ (cl) (match cl
                      [`(prov ,(? var? v) ,pos) #:when (equal? v old-var) `(prov ,new-var ,pos)]
                      [else #f]))))
  (define (rule-replace-var rule old-var new-var)
      (rule-visit-replace-sub-clauses 
        rule
        (λ (cl) (match cl
                  [`(prov ,(? var? v) ,pos) #:when (equal? v old-var) `(prov ,new-var ,pos)]
                  [else #f]))))

  (define (rule-vars rule) 
    (match-define `(prov (,heads ... <-- ,bodys ...) ,pos) rule)
    (map strip-prov (flat-map source-tree-clause-vars (append heads bodys))))

  (define (body-item-replace-var-if-safe body-item old-var new-var)
    (match body-item
      [`(INNER-RULE ,ir)
        (define inner-rule-vars (rule-vars ir))
        (define is-safe (not (and (member old-var inner-rule-vars) (member new-var inner-rule-vars))))
        (if is-safe (cons `(INNER-RULE ,(rule-replace-var ir old-var new-var)) #t) (cons body-item #f))]
      [else
        (cons (cl-replace-var body-item old-var new-var) #t)]))

  ;; Finds a pair of vars to unify and returns (cons updated-rule (cons old-var new-var)).
  ;; If it fails to find any vars that can be unified, returns (cons rule #f)
  (define (var-unification-step rule [context-vars (set)])
    (match-define `(prov (,heads ... <-- ,bodys ...) ,pos) rule)
    (match-define (cons updated-body-items matched-vars)
      (foldr (λ (body-item accu)
               (match-define (cons updated-body-items matched-vars) accu)
               (match matched-vars
                [(cons old new) (cons (cons body-item updated-body-items) matched-vars)]
                [#f
                  (match body-item
                    [`(INNER-RULE ,ir) 
                     (match-define (cons updated-ir ir-matched-vars) (var-unification-step ir))
                     (cons (cons `(INNER-RULE ,updated-ir) updated-body-items) ir-matched-vars)]
                    [else (cons (cons body-item updated-body-items) matched-vars)])]))
             (cons '() #f)
             bodys))
    (match matched-vars
      [(cons old-var new-var)
       (define replacer (app cl-replace-var _ old-var new-var))
       #;(define new-rule `(prov (,@(map (transform-rule-item replacer (app rule-replace-var _ old-var new-var)) heads) <-- 
                                ,@(map (transform-rule-item replacer identity) updated-body-items)) ,pos))
       (define new-rule `(prov (,@heads <-- 
                                (prov ((prov = ,pos) (prov ,old-var ,pos) (prov ,new-var ,pos)) ,pos) 
                                ,@updated-body-items) ,pos))
       (cons new-rule matched-vars)]
      [#f
       ; TODO rule-vars looks wrong, body innder rules vars are also in scope in the outer rule!
       (define rule-vars (map strip-prov (flat-map source-tree-clause-vars (filter (not/c inner-rule?) (append heads bodys)))))
       (define body-inner-rules-vars (map strip-prov (flat-map source-tree-clause-vars (filter inner-rule? bodys))))
       (define context-vars+ (set-union context-vars (list->set body-inner-rules-vars)))
       (match-define (cons updated-body-items matched-vars)
        (foldl (λ (var accu)
                (match-define (cons updated-body-items matched-vars) accu)
                (match matched-vars
                  [(cons old new) accu]
                  [#f
                   (define all-equal-args (flat-map (app equal-args var _) (filter (not/c inner-rule?) (append heads bodys)))) 
                   
                   (define-values (non-renamable-equal-args renamable-equal-args) 
                      (partition (λ (v) (or (lit? v) (set-member? context-vars+ v))) (cons var all-equal-args)))
                  ;  (printf "equal args for ~a, renamable: ~a, non-renamable: ~a\n" var renamable-equal-args non-renamable-equal-args)
                   (cond 
                    [(empty? renamable-equal-args) accu]
                    [else
                     (define canonical-var 
                      (if (empty? non-renamable-equal-args) 
                        (get-canonical-var renamable-equal-args)
                        (get-canonical-var non-renamable-equal-args)))
                     (define to-be-renamed-vars (remove canonical-var renamable-equal-args))
                    ;  (printf "canonical-var: ~a, to-be-renamed-vars: ~a\n" canonical-var to-be-renamed-vars)
                     (match to-be-renamed-vars
                      ['() accu]
                      [(cons old-var _)
                       (define updated-body-items
                        (map (transform-rule-item (app cl-replace-var _ old-var canonical-var) identity) bodys))
                        (cons updated-body-items (cons old-var canonical-var))])]) ]))
                 (cons bodys #f)
                 rule-vars))
       (match matched-vars
        [(cons old-var new-var)
          (define replacer (app cl-replace-var _ old-var new-var))
          (define rule-replacer (app rule-replace-var _ old-var new-var))
          (define new-rule `(prov (,@(map (transform-rule-item replacer rule-replacer) heads) <-- 
                                   ,@updated-body-items) ,pos))
          (cons new-rule matched-vars)]
        [#f
          (define hir-context-vars (set-union context-vars+ (list->set rule-vars)))
          (define (hir-unification hir) (car (var-unification-step hir hir-context-vars)))
          (define new-rule
            `(prov (,@(map (transform-if-inner-rule hir-unification) heads) <-- ,@bodys) ,pos)) 
          (cons new-rule #f)])]))

  (define (is-silly-clause cl)
    (match cl
      [`(prov ((prov = ,=pos) (prov ,(? arg? x) ,xpos) (prov ,(? arg? y) ,ypos)) ,pos) (equal? x y)]
      [else #f]))
  (define (remove-silly-clauses rule)
    (match-define `(prov (,heads ... <-- ,bodys ...) ,pos) rule)
    `(prov (,@(filter (not/c is-silly-clause) heads) <-- 
            ,@(filter (not/c is-silly-clause) (map (transform-if-inner-rule remove-silly-clauses) bodys))) ,pos))
  
  (define (renormalize-rule rule)
    (match-define `(prov (,heads ... <-- ,bodys ...) ,pos) rule)
    (define (belongs-to-body cl)
      (match cl
        [`(prov ((prov = ,=pos) (prov ,(? var?) ,_) (prov ,(? arg?) ,_)) ,pos) #t]
        [else #f]))
    (define-values (heads->body new-heads) (partition belongs-to-body heads))
    `(prov (,@new-heads <-- ,@heads->body ,@bodys) ,pos))
  
  (define (var-unification-step2 rule) (car (var-unification-step rule)))

  (define res0 (iterate-to-fixed-point 
                (λ (x) (remove-silly-clauses (var-unification-step2 (repeated-clause-unification-step x))))
                rule))
  (define (cl-check-lit-not-unified-with-fact cl)
    (match cl
      [`(prov ((prov = ,=pos) (prov ,(? lit? lit) ,litpos) ,(and subcl `(prov (,tag ,args ...) ,subclpos))) ,pos)
       (pretty-error-current pos (format "clause ~a cannot be unified with literal ~v." (strip-prov subcl) lit) #:exit #t)
       cl]
      [else cl]))

  (define (rule-check-lit-not-unified-with-fact rule)
    (match-define `(prov (,heads ... <-- ,bodys ...) ,pos) rule)
    `(prov (,@(map (transform-rule-item cl-check-lit-not-unified-with-fact rule-check-lit-not-unified-with-fact) heads) <-- 
            ,@(map (transform-rule-item cl-check-lit-not-unified-with-fact rule-check-lit-not-unified-with-fact) bodys)) ,pos))

  (rule-check-lit-not-unified-with-fact (renormalize-rule res0)))

;; return a list of all variables in a clause (including variables in list syntaxes)
(define (get-flattened-clause-args cl)
  (match cl
    [`(prov ((prov = ,=pos) ,id ,cl-rhs) ,pos)
      (get-flattened-clause-args cl-rhs)]
    [`(prov (LIST-SYNTAX ,args ...) ,pos)
      (apply append (map get-flattened-clause-args (filter (λ (x) (not (equal? x '...))) args)))]
    [`(prov ,(? arg? v) ,pos) (list cl)]
    [`(prov (! ,banged-clause) ,pos) (get-flattened-clause-args `(prov ,banged-clause ,pos))]
    [`(prov (,tag ,cls ...) ,pos) (apply append (map get-flattened-clause-args cls))]
    [`(prov (? ,huhcl) ,pos) (get-flattened-clause-args `(prov ,huhcl ,pos))]))

(define (get-clause-id cl)
  (match cl
    [`(prov ((prov = ,=pos) ,id ,cl-rhs) ,pos) (list id)]
    [else (list)]))

;; returns the dependency graph of a list of head clauses (as a hash). A -> B means B depends on A.
;; NOTE only works on flattened clauses!!
(define (head-clauses-dependency-graph cls)
  (define cls-with-ids-args (map (λ (cl) (list cl (map strip-prov (get-clause-id cl)) (filter var? (map strip-prov (get-flattened-clause-args cl))))) cls))
  (foldl (λ (cl-with-id-args accu)
          (match-define (list cl id args) cl-with-id-args)
          (define dependents (foldl (λ (other-cl-with-id-args accu)
                                        (match-define (list other-cl other-id other-args) other-cl-with-id-args)
                                        (if (not (empty? (set-intersect id other-args)))
                                          (set-add accu other-cl)
                                          accu))
                                    (set)
                                    cls-with-ids-args))
          (hash-set accu cl dependents))
          (hash)
          cls-with-ids-args))

;; works on flattened rules
(define (source-tree-comp-rule? rule comp-rels)
  (assert ((set/c symbol?) comp-rels))
  (match-define `(prov (,heads ... <-- ,bodys ...) ,pos) rule)
  (for/or ([head heads])
    (match head
      [`(prov ((prov = ,=pos) (prov ,id ,idpos) (prov ((prov ,rel ,relpos) ,args ...) ,clpos)) ,pos)
    (cond 
      [(set-member? comp-rels rel)
       (when (> (length heads) 1)
         (pretty-error-current pos "Multiple head clauses in a comp rule are not allowed." #:exit #t))
       (for ([bcl bodys])
        (match-define `(prov ((prov = ,=pos) (prov ,id ,idpos) (prov ((prov ,rel ,relpos) ,args ...) ,clpos)) ,pos) bcl)
        (when (and (not (set-member? comp-rels rel)) (not (builtin? rel)))
          (pretty-error-current pos "DB relations in the body of comp rules are not allowed." #:exit #t)))
       #t]
          [else #f])]
      [else #f])))


;; works on raw rules
(define/contract-cond (source-tree-rule-rel-arities rule)
  (source-tree-rule? . -> . (hash/c symbol? (set/c number?)))
  (define normalized-rule (normalize-rule rule))
  (define do-desugared-rule (rule-desugar-bang-do-huh-do normalized-rule))
  (define dehuhd-rule (move-huhs-rule do-desugared-rule))
  (define desegmented-rule (desugar-segmented-rule dehuhd-rule))
  (define or-split-rules (split-rule-or desegmented-rule))
  (define curlies-removed-rules (map rule-desugar-curly-clauses or-split-rules))

  (define (desugared-rule-rel-arities rule)
    (match-define `(prov (,heads ... <-- ,bodys ...) ,pos) rule)
    (apply hash-union (hash) (map source-tree-clause-rel-arities (append heads bodys)) #:combine set-union))
  
  (define (source-tree-clause-rel-arities cl)
    (match cl
      ['... (hash)]
      [`(prov (LIST-SYNTAX ,args ...) ,pos)
        (apply hash-union (hash) (map source-tree-clause-rel-arities args) #:combine set-union)]
      [`(prov (! ,banged-clause) ,pos)
        (source-tree-clause-rel-arities `(prov ,banged-clause ,pos))]
      [`(prov ((prov ,rel ,_relpos) ,cls ...) ,pos)
        (apply hash-union (hash rel (set (length cls))) (map source-tree-clause-rel-arities cls) #:combine set-union)]
      [`(prov ,(? arg? v) ,pos)
        (hash)]
      [`(INNER-RULE ,inner-rule)
        (source-tree-rule-rel-arities inner-rule)]))

  (apply hash-union (map desugared-rule-rel-arities curlies-removed-rules) #:combine set-union))