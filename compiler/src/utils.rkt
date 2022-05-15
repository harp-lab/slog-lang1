#lang racket

;; Slog misc utilities and predicates
;; Copyright (c) Thomas Gilray, et al, see License.md

(require "slog-params.rkt")
(require "generic-utils.rkt")
(require "lang-predicates.rkt")
(require csv-reading)

(provide read-all
         get-arity
         strip-arity
         gensymb
         gensymb-forbid!
         find-suitable-index
         pretty-rule-error
         pretty-highlight
         ir->source-tree
         extend-provenance-graph
         set->nat-bijection
         graph->dot
         pretty-error
         pretty-error-current
         current-source-tree
         switch-to-err-bg
         switch-from-err-bg
         switch-to-info-bg
         switch-from-info-bg
         pretty-format-nested-fact
         strip-prov
         iterate-to-fixed-point
         prov->pos
         unwrap-prov
         format-config
         strip-all-prov
         transitive-closure
         peel-back-nested-fact-one-layer
         prov-of
         ->rel-select
         ->cpp-ident
         format-source-tree-rule
         format-ir-rule
         ir->ir-flat
         ir->ir-fixed
         compilation-error
         pretty-syntax-error
         print-ir-flat
         print-ir-fixed
         print-ir-select
         print-ir-small
         print-ir-incremental
         print-ir-scc
         internal-rel-name?
         facts-dir-facts-files
         facts-file-facts
         facts-file-rel-name
         (all-from-out "generic-utils.rkt"))

; Helper for reading in all s-exprs from STDIN
(define (read-all port)
  (let ([next (read port)])
    (if (eof-object? next)
        '()
        (cons next (read-all port)))))

; Returns the user-facing name of a relation (removing the arity)
(define (strip-arity r)
  (define sp (string-split (symbol->string r) "-"))
  (string->symbol (string-join (take sp (- (length sp) 1)) "-")))

; Returns the arity of a relation based on its partitioned name
(define (get-arity r)
  (define sp (string-split (symbol->string r) "-"))
  (string->number (last sp)))

(define (strip-rule-prov e)
  (match e
    [(? set?)
      (list->set (map strip-rule-prov (set->list e)))]
    [(? hash?)
      (match (hash-values e)
        [`((rule-prov ,_ ...) ,_ ...) (map strip-rule-prov (hash-keys e))]
        [_ (foldl (lambda (k accu) (hash-set accu k (strip-prov (hash-ref e k)))) (hash) (hash-keys e))])]
    [(? list?)
      (map strip-rule-prov e)]
    [else e]))

(define (strip-all-prov e)
 (strip-rule-prov (strip-prov e)))

; State for the gensym facility
(define gensymb-counts (make-hash))
(define (gensymb-inc! base-symb)
  (define ind (hash-ref! gensymb-counts base-symb 0))
  (hash-set! gensymb-counts base-symb (add1 ind))
  (if (= ind 0) "" (number->string ind)))

(define gensymb-forbidden (set))
(define (gensymb-forbid! st) (set! gensymb-forbidden (set-union gensymb-forbidden st)))
(define (gensymb b)
  (define (makenext!) (string->symbol (string-append (symbol->string b) (gensymb-inc! b))))
  (let loop ([next (makenext!)])
    (if (set-member? gensymb-forbidden next)
        (loop (makenext!))
        next)))


; Returns a suitable index for a given indices-map and sii triple
(define (find-suitable-index ver+rel indices-map sel itr ign)
  (define (suitable? idx)
    (and (= (length idx) (+ (set-count sel) (set-count itr) (set-count ign)))
         (equal? sel (list->set (take idx (set-count sel))))
         (equal? itr (list->set (take (drop idx (set-count sel)) (set-count itr))))
         (equal? ign (list->set (take (drop idx (+ (set-count sel) (set-count itr))) (set-count ign))))))
  (let loop ([ind (set->list (hash-ref indices-map ver+rel))])
    (if (null? ind)
        (error (format "Could not find suitable index for ~a with (~a, ~a, ~a) in ~a" ver+rel sel itr ign (hash-ref indices-map ver+rel)))
        (if (suitable? (car ind))
            (car ind)
            (loop (cdr ind))))))

;; 
;; ANSI terminal pretty error handling
;;

;; Is the error background on?
(define errbg-on? #f)

;; Switch to the error background, if not already.
(define (switch-to-err-bg)
  (if errbg-on?
      ""
      (begin (set! errbg-on? #t) "\u001b[41;1m")))

;; Switch away from the error background
(define (switch-from-err-bg)
  (if errbg-on?
      (begin (set! errbg-on? #f) "\u001b[0m")
      ""))

;; Is the error background on?
(define infobg-on? #f)

;; Switch to the error background, if not already.
(define (switch-to-info-bg)
  (if infobg-on?
      ""
      (begin (set! infobg-on? #t) "\u001b[46;1m")))

;; Switch away from the error background
(define (switch-from-info-bg)
  (if infobg-on?
      (begin (set! infobg-on? #f) "\u001b[0m")
      ""))


;; digs through an IR to find the original source-tree?
(define (ir->source-tree ir)
  (match ir
         [(? source-tree? st) st]
         [else (ir->source-tree (second ir))]))

(define (ir->ir-flat ir)
  (match ir
    [`(ir-flat ,_ ...) ir]
    [else (ir->ir-flat (second ir))]))

(define (ir->ir-fixed ir)
  (match ir
    [`(ir-fixed ,_ ...) ir]
    [else (ir->ir-fixed (second ir))]))

;; digs through an IR, taking a root rule and extends a hash
;; (representing a graph of rules in the entire flattened IR, from
;; each pass) with a set of key,value pairs so that the resulting hash
;; builds up the transitive links from the root rule to its original
;; provenance in source-tree?. Also adds edges: for each source /
;; destination rule pair, there is also `(edge ,src ,dst) to the
;; provenance information from src to dst. This assumes that no key
;; will have more than one parent.
(define (extend-provenance-graph prov-graph ir root-rule)
  (define (hash-ext h x y)
    ;; Do not add self-edges
    (if (equal? x y)
        h
        (hash-set h x (set-add (hash-ref h x (set)) y))))
  (define (lookup-source-tree-rule source-tree fname i)
    (define rules-hash (Module-rules (hash-ref source-tree fname)))
    (hash-ref rules-hash i))
  (match ir
    [`(ir-incremental ,ir-scc ,dag ,scc-map ,comp-rules-h)
     (foldl (lambda (scc-num h)
              (match-define `(scc ,l ,ram ,rule-map) (hash-ref scc-map scc-num))
              (foldl (lambda (rule h)
                       (if (equal? rule root-rule)
                           (begin
                             ;;(displayln "matched")
                             ;;(pretty-print (hash-ref rule-map rule))
                             (match (hash-ref rule-map rule)
                              [`(rule-prov ir-scc ,scc-id ,ir-scc-rule ,s ,i)
                               (extend-provenance-graph (hash-set (hash-ext h rule ir-scc-rule)
                                                                  `(edge ,rule ,ir-scc-rule)
                                                                  (hash-ref rule-map rule)) ir-scc
                                                        ir-scc-rule)]
                              [_ h]))
                           h))
                     h
                     (hash-keys rule-map)))
            prov-graph
            (hash-keys scc-map))]
    [`(ir-scc ,ir-select ,scc-dag ,scc-map ,comp-rules-h)
     (foldl (lambda (scc-num h)
              (match-define `(scc ,l ,ram ,rule-map) (hash-ref scc-map scc-num))
              (foldl (lambda (rule h)
                       (if (equal? rule root-rule)
                           (match (hash-ref rule-map rule)
                                    [`(rule-prov ir-select ,ir-select-rule ,s ,i)
                                     (extend-provenance-graph (hash-set
                                                               (hash-ext h rule ir-select-rule)
                                                               `(edge ,rule ,ir-select-rule)
                                                               (hash-ref rule-map rule)) ir-select
                                                              ir-select-rule)]
                                    [_ h])
                           h))
                     h
                     (hash-keys rule-map)))
            prov-graph
            (hash-keys scc-map))]
    [`(ir-select ,ir-small ,rel-h ,rules-h ,comp-rules-h) 
     (foldl (lambda (rule h)
              (if (equal? rule root-rule)
                  (match (hash-ref rules-h rule)
                    [`(rule-prov ir-small ,ir-small-rule ,s ,i)
                     (extend-provenance-graph (hash-set (hash-ext h rule ir-small-rule)
                                                        `(edge ,rule ,ir-small-rule)
                                                        (hash-ref rules-h rule)) ir-small ir-small-rule)]
                    [_ h])
                  h))
            prov-graph
            (hash-keys rules-h))]
    [`(ir-small ,ir-fixed ,rules-h ,comp-rules-h)
     (foldl (lambda (rule h)
              (if (equal? rule root-rule)
                  (match (hash-ref rules-h rule)
                      [`(rule-prov ir-fixed ,ir-fixed-rule ,s ,i)
                       (extend-provenance-graph (hash-set (hash-ext h rule ir-fixed-rule)
                                                          `(edge ,rule ,ir-fixed-rule)
                                                          (hash-ref rules-h rule)) ir-fixed ir-fixed-rule)]
                      [_ h])
                  h))
            prov-graph
            (hash-keys rules-h))]
    [`(ir-fixed ,ir-flat ,rules-h ,comp-rules-h) 
     (foldl (lambda (rule h)
              (if (equal? rule root-rule)
                  (match (hash-ref rules-h rule)
                    [`(rule-prov ir-flat ,ir-flat-rule ,s ,i)
                     (extend-provenance-graph (hash-set
                                               (hash-ext h rule ir-flat-rule)
                                               `(edge ,rule ,ir-flat-rule)
                                               (hash-ref rules-h rule)) ir-flat ir-flat-rule)]
                    [_ h])
                  h))
            prov-graph
            (hash-keys rules-h))]
    [`(ir-flat ,source-tree ,rules-h ,comp-rules-h) 
     (foldl (lambda (rule h)
              (if (equal? rule root-rule)
                  (match (hash-ref rules-h rule)
                    [`(rule-prov source-tree ,fname ,i)
                     (let ([source-rule (lookup-source-tree-rule source-tree fname i)])
                       (hash-set (hash-ext h rule source-rule) `(edge ,rule ,source-rule) (hash-ref rules-h rule)))]
                    [_ h])
                  h))
            prov-graph
            (hash-keys rules-h))]
    [_ (error (format "Got an unexpected ir ~a" ir))]))

;; Define a bijection between a finite set and the naturals. Returns a
;; hash whose domain is nonnegative-integer?
(define (set->nat-bijection s)
  (define lst (set->list s))
  (foldl (lambda (i h) (hash-set h i (list-ref lst i))) (hash) (range (length lst))))

;; render a graph in dot format
(define (graph->dot graph [port (current-output-port)])
  (define nodes (foldl (lambda (k s) (set-add (set-union (hash-ref graph k) s) k)) (set) (hash-keys graph)))
  (define num->node (set->nat-bijection nodes))
  (define node->num (foldl (lambda (n h) (hash-set h (hash-ref num->node n) n)) (hash) (range (set-count nodes))))
  (displayln "digraph sloggraph {" port)
  (for ([num (range (set-count nodes))])
    (displayln (format "\tn~a [label=\"~a\" shape=box];" num
                       (string-replace
                        (string-replace (pretty-format (hash-ref num->node num)) "\n" "\\l") "\"" "\\\"")) port))
  (for ([key (hash-keys graph)])
    (for ([value (set->list (hash-ref graph key))])
      (displayln (format "\tn~a -> n~a [color=blue];" (hash-ref node->num key) (hash-ref node->num value)) port)))
  (displayln "}" port))

;; pretty-rule-error
(define (pretty-rule-error program rule-prov error-string #:exit [exit? #f])
  (define (helper module module-id)
          (define rules-h (Module-rules (hash-ref (ir->source-tree program) module)))
          (match-define `(prov ,_ ,pos)
            (hash-ref rules-h module-id))
          (pretty-error (hash-ref (ir->source-tree program) module) pos error-string #:exit exit?))
  (match rule-prov
         [`(rule-prov source-tree ,module ,module-id)
          (helper module module-id)]
         [`(rule-prov ir-flat ,(? ir-flat-rule?) ,module ,module-id)
          (helper module module-id)]
         [`(rule-prov ir-fixed ,(? ir-fixed-rule?) ,module ,module-id)
          (helper module module-id)]
         [`(rule-prov ir-small ,(? ir-small-rule?) ,module ,module-id)
          (helper module module-id)]
         [`(rule-prov ir-select ,(? ir-select-rule?) ,module ,module-id)
          (helper module module-id)]
         [`(rule-prov ir-scc ,(? nonnegative-integer? scc-id) ,(? ir-scc-rule?) ,module ,module-id)
          (helper module module-id)]
         [`(rule-prov intra-relation ,(? rel-name?) ,(or (? positive-integer?) 'variadic))
          (error "pretty-rule-error for internal rule (not yet implemented)")]
         [else (error "Error while trying to handle pretty-rule-error")]))

(define (pretty-rule-highlight program rule)
  (match rule))

;; pretty printing of program source, including error listing and
;; debugging listing.


;; Helper function for printing a portion of a program--possibly
;; interspersed with error / information highlighting and
;; information. Used in printing errors and also listings in the
;; debugger.
;; 
;; Inputs:
;;   - program, the program being printed
;;   - position of the program to print
;;   - thunk to be called before anything else (e.g., good for displaying an error)
;;   - thunk to be called before displaying the region of interest
;;   - thunk to be called after displaying the region of interest
;;   - thunk which gets invoked on the beginning of each
;;   line. Receives a symbol satisfying (symbols 'before 'after
;;   'single-line 'first-line 'last-line 'middle-line).
;;   - number of lines of context (i.e., print this many lines before/after);;
(define (pretty-highlight program position before-everything-thunk before-position-thunk after-position-thunk before-line-thunk ctx-lines)
  (match-define `(pos ,filename
                      (,start-line . ,start-col)
                      (,end-line   . ,end-col))
                position)
  (define (highlight-vector lines-vector)
    (define (split txt col) (cons (substring txt 0 col) (substring txt col)))
    (define first-err-line (split (vector-ref lines-vector (sub1 start-line)) start-col))
    (define last-err-line (split (vector-ref lines-vector (sub1 end-line)) end-col))
    ; Utility to format lines w/ nos
    (define (display-line no str)
      (define lino (~a (format "~a." no) #:min-width 6 #:align 'left #:right-pad-string " "))
      (displayln (format "~a~a" lino str)))
    ; Utility to display a block of code lines
    (define (display-block i j thnk)
      (let ([s (max i 1)]
            [e (min (vector-length lines-vector) j)])
        (for ([i (in-range s e)])
          (thnk) (display-line i (vector-ref lines-vector (sub1 i))))))
    (before-everything-thunk)
    ; Display the beginning dots
    (unless (= start-line 0)
      (before-line-thunk 'before)
      (displayln "      ..."))
    ; Display the lines leading up to the offending line
    (display-block (- start-line ctx-lines) start-line (lambda () (before-line-thunk 'before)))
    ; Now, actually display the offending lines
    (if (= start-line end-line)
        ; As a special case, handle when start line = end line
        (begin
          (before-line-thunk 'single-line)
          (display-line
           start-line
           (format "~a~a~a~a~a" (car first-err-line) (before-position-thunk)
                   (substring (vector-ref lines-vector (sub1 start-line)) start-col end-col)
                   (after-position-thunk) (cdr last-err-line))))
        ; Otherwise display a multi-line highlighted region
        (begin
          ; Display the first line of the error
          (before-line-thunk 'first-line)
          (display-line start-line (format "~a~a~a" (car first-err-line)
                                           (before-position-thunk) (cdr first-err-line)))
          ; Display the middle lines
          (for ([i (in-range (add1 start-line) end-line)])
            (before-line-thunk 'middle-line)
            (display-line i (vector-ref lines-vector (sub1 i))))
          ; Display the last line
          (before-line-thunk 'last-line)
          (display-line end-line (format "~a~a~a" (car last-err-line)
                                         (after-position-thunk) (cdr last-err-line)))))
    (display-block (+ end-line 1) (+ end-line 1 ctx-lines) (lambda () (before-line-thunk 'after)))
    ; Ending dots
    (unless (= end-line (vector-length lines-vector))
      (before-line-thunk 'after)
      (displayln "      ...")))
  (match program
    [(? vector? lines-vector) (highlight-vector lines-vector)]
    [(? Module?) (highlight-vector (Module-raw-lines program))]
    [(? source-tree? st)
     (pretty-highlight (hash-ref st filename) position before-everything-thunk
                       before-position-thunk after-position-thunk before-line-thunk ctx-lines)]
    [else (error (format "\n pretty-highlight: unrecognized program:\n~a\n" program))]))

;; Display a source error
;; pretty-error : (or/c source-tree? (vectorof string?))  * pos * string? -> void
;; 
;; This function displays a source error given:
;;   - either a vector of strings or a source-tree?
;;   - erroneous source position
;;   - (optional) error string
;; 
;; It then prints the error string (if one is provided) and prints a
;; line of leading dots before then displaying each of the erroneous
;; lines (highlighted in red background), followed by a set of ending
;; dots.
(define (pretty-error program position [error-string ""] #:exit [exit? #f])
  (match-define `(pos ,filename
                      (,start-line . ,start-col)
                      (,end-line   . ,end-col))
                position)
  (pretty-highlight program position
                    (lambda () (displayln (format "Error between ~a,~a and ~a,~a in ~a:~a~a"
                                                  start-line start-col end-line end-col filename start-line
                                                  (if (equal? error-string "") "" (format ": ~a" error-string)))))
                    (thunk (switch-to-err-bg))
                    (thunk (switch-from-err-bg))
                    (lambda (x) (void))
                    (slog-err-nolines))
  (when exit? (raise (compilation-error program position error-string))))

(struct compilation-error (program position error-string))

(define current-source-tree (make-parameter '()))
(define (pretty-error-current pos [msg "" ] #:exit [exit? #f])
  (pretty-error (current-source-tree) pos msg #:exit exit?))

;;
;; Fact formatting and pretty-printing
;;

(define prim-types '(float string symbol bool integer))

;; Calculate the depth of a given fact fact via its ID
(define (fact-depth id relation-map rel-id->rel-arity [starting-depth 0])
  (match id
    ;; Base types
    [`(tag ,(? (lambda (x) (member x prim-types))) ,_) starting-depth]
    ;; Relations
    [`(tag ,(? number? n) ,val)
     (match-let* ([`(rel-arity ,name ,arity db) (hash-ref rel-id->rel-arity n)]
                  [`(rel-instance ,fact-map ,rel-num ,num-facts) (hash-ref relation-map `(rel-arity ,name ,arity db))]
                  [fact (hash-ref fact-map id)])
       (apply max (map (lambda (id) (fact-depth id relation-map rel-id->rel-arity (+ starting-depth 1))) fact)))]))

(define (prim-type? x) (member x '(float string symbol bool integer)))

;; Count the number of times each ID appears in this nested fact. This
;; can be used for things like pretty-printing.
(define (count-ids-in-fact id relation-map rel-id->rel-arity id-map)
  (match id
    ;; Base types
    [`(interned ,interned-tag ,(? number?)) id-map]
    [`(integer ,(? integer?)) id-map]
    [`(bool ,(? boolean?)) id-map]
    
    ;; Relations
    [`(tag ,rel-id ,bucket ,tuple-id)
     (match-let*
         ([updated-id-map
           (hash-set id-map id (add1 (hash-ref id-map id 0)))]
          [`(rel-arity ,name ,arity db) (hash-ref rel-id->rel-arity rel-id)]
          [`(rel-instance ,fact-map ,rel-num ,num-facts)
           (hash-ref relation-map `(rel-arity ,name ,arity db))]
          [fact (hash-ref fact-map id)])
       (foldl
        (lambda (id id-map)
          (count-ids-in-fact id relation-map rel-id->rel-arity id-map))
        updated-id-map
        fact))]))



;;TODO: unused function?
;; Format a nested fact
;; - id is the fact id, tagged in a way generated by interpreter.rkt (see that file)
;; - intern-map is a map from id -> interned values
;; - relation-map is a relation map containing rel-instances
;; - rel-id->rel-arity is a map from relation IDs to rel-arity instances
(define (fully-format-nested-fact id intern-map relation-map rel-id->rel-arity)
  (match id
    ;; Integers
    [`(tag integer ,val) val]
    ;; Floats
    [`(tag float ,val) val]
    ;; Strings
    [`(tag string ,val) (hash-ref intern-map id)]
    ;; Symbols
    [`(tag symbol ,val) (hash-ref intern-map id)]
    ;; Bools
    [`(tag bool 0) #f]
    [`(tag bool 1) #t]
    ;; Must be a relation
    [`(tag ,(? number? n) ,val)
     (match-let* ([`(rel-arity ,name ,arity db) (hash-ref rel-id->rel-arity n)]
                  [`(rel-instance ,fact-map ,rel-num ,num-facts)
                   (hash-ref relation-map `(rel-arity ,name ,arity db))]
                  [fact (hash-ref fact-map id)])
       `(,name ,@(map (lambda (id) (fully-format-nested-fact id intern-map relation-map rel-id->rel-arity)) fact)))]))

;; Assign each ID an intermediate variable if it appears at least
;; once, and at an appropriate depth. 
(define (assign-variables id depth-to-group relation-map rel-id->rel-arity)
  (let ([num-vars 0])
    ;; Recursively crawl through variables and begin forming
    ;; subvariables after a depth-to-group. Note that depth grows
    ;; from high to low.
    (define (h id depth id->variable)
      (match id
        [`(interned ,i ,n) id->variable]
        [`(integer ,i) id->variable]
        [`(bool ,b) id->variable]
        ;; Otherwise, a recursive fact
        [`(tag ,n ,b ,i)
         (cond
           ;; If we've already assigned this a variable #, skip it
           [(hash-has-key? id->variable id) id->variable]
           ;; We haven't assigned a variable #, but we should, do so
           [(<= depth 0)
            ;; Assign a variable to this ID
            (let* ([num (begin (set! num-vars (add1 num-vars)) num-vars)]
                   [var-name (string->symbol (format "#~a" num))]
                   [new-id->variable (hash-set id->variable id var-name)])
              ;; Recursively examine all subfacts
              (h id depth-to-group new-id->variable))]
           ;; Otherwise, examine all subgoals
           [else
            (match-let*
                ([`(rel-arity ,name ,arity db) (hash-ref rel-id->rel-arity n)]
                 [`(rel-instance ,fact-map ,rel-num ,num-facts)
                  (hash-ref relation-map `(rel-arity ,name ,arity db))]
                 [fact (hash-ref fact-map id)]
                 [new-depth (if (equal? name '$lst) depth (sub1 depth))])
              (foldl
               (lambda (id id->variable) (h id new-depth id->variable))
               id->variable
               fact))])]))
    (h id depth-to-group (hash))))


;; (format-depth) - When formatting facts, just display ... after
;; this depth.
;; (grouping-depth) - Only group facts which are this deep.
;; (grouping-cardinality) - Only group facts when there are at
;; least this many of them.
(struct format-config (format-depth grouping-depth grouping-cardinality) #:transparent)


;; Attractively format a nested fact. This will traverse the fact and
;; pull out common subterms and also cut off formatting the fact after
;; reaching a certain depth. The 'config' argument controls the printing of facts. If
;; this arguemnt is not provided, these parameters are used instead:
;; 
;; (slog-format-depth) - When formatting facts, just display ... after
;; this depth.
;; (slog-grouping-depth) - Only group facts which are this deep.
;; (slog-grouping-cardinality) - Only group facts when there are at
;; least this many of them.
(define (pretty-format-nested-fact id intern-map relation-map rel-id->rel-arity [config 'from-params])
  (set! config (if (equal? config 'from-params)
                   (format-config (slog-format-depth) (slog-grouping-depth) (slog-grouping-cardinality))
                   config))

  ;; Calculate the number of time each fact occurs
  (define id->num-times (count-ids-in-fact id relation-map rel-id->rel-arity (hash)))

  ;; Using this, form intermediate variables after a certain depth.
  (define id->var (assign-variables id (format-config-grouping-depth config) relation-map rel-id->rel-arity))

  ;; Helper to recursively print facts. Takes into accounts grouping
  ;; of subfacts and prints '... when reaching format depth.
  (define (h id depth parent-ids)
    (match id
      [`(integer ,i) i]
      [`(bool ,b) b]
      [`(interned string ,val) (hash-ref intern-map id)]
      [`(interned symbol ,val) `(,(hash-ref intern-map id))]
      ;; Must be a relation
      [`(tag ,rel-id ,bucket ,tuple-id)
       (cond
         [(and (hash-has-key? id->var id)
               (not (set-member? parent-ids id)))
          ;; Print out a subfact instead
          (hash-ref id->var id)]
         ;; We're at maximmum formatting depth
         [(= depth (format-config-format-depth config)) '...]
         ;; Recursively format the fact
         [else 
          (match-let* ([`(rel-arity ,name ,arity db) (hash-ref rel-id->rel-arity rel-id)]
                       [`(rel-instance ,fact-map ,rel-num ,num-facts)
                        (hash-ref relation-map `(rel-arity ,name ,arity db))]
                       [fact (hash-ref fact-map id)]
                       [new-depth (if (equal? name '$lst) depth (add1 depth))])
            `(,name ,@(map (lambda (next-id) (h next-id new-depth (set-add parent-ids id))) fact)))])]))
  
  ;; Format the top-level fact, and then print equalities for each of
  ;; the subfacts.
  (cons (h id 0 (set)) (map (lambda (id) `(= ,(hash-ref id->var id) ,(h id 0 (set id)))) (hash-keys id->var))))

(define (peel-back-nested-fact-one-layer id intern-map relation-map rel-id->rel-arity)
  (match id
      [`(integer ,i) i]
      [`(bool ,b) b]
      [`(interned string ,val) (hash-ref intern-map id)]
      [`(interned symbol ,val) `(,(hash-ref intern-map id))]
      ;; Must be a relation
      [`(tag ,rel-id ,bucket ,tuple-id)
        (match-let* ([`(rel-arity ,name ,arity db) (hash-ref rel-id->rel-arity rel-id)]
                      [`(rel-instance ,fact-map ,rel-num ,num-facts)
                      (hash-ref relation-map `(rel-arity ,name ,arity db))]
                      [fact (hash-ref fact-map id)])
          `(,name ,@fact))]))

(define (prov->pos prov)
  (match prov
    [`(prov ,_ ,pos) pos]))

(define (unwrap-prov prov)
  (match prov
    [`(prov ,x ,pos) x]))

(define (->rel-select rel-version)
  (match rel-version
    [`(rel-version ,name ,arity ,indices ,ver)
      (define kind
        (if (comp-or-agg-rel-kind? ver) ver 'db)) 
     `(rel-select ,name ,arity ,indices ,kind)]
    [`(rel-select ,_ ...) rel-version]))

(define/contract (->cpp-ident name)
  ((or/c symbol? string?) . -> . string?)
  (match name
    [(? symbol?) (->cpp-ident (symbol->string name))]
    [(? string?)
     (apply string-append 
      (map 
        (λ (c) (match c
          [#\` "_backtick"]
          [#\~ "_tilde"]
          [#\! "_bang"]
          [#\@ "_at"]
          [#\# "_pound"]
          [#\$ "_dollor"]
          [#\% "_percent_"]
          [#\^ "_caret"]
          [#\& "_ampersand"]
          [#\* "_star"]
          [#\- "_dash"]
          [#\= "_eq"]
          [#\+ "_plus"]
          [#\/ "_slash"]
          [#\' "_prime"]
          [#\\ "_bslash"]
          [#\. "_dot"]
          [#\: "_colon"]
          [#\_ "__"]
          [#\? "_qmark"]
          [#\< "_less"]
          [#\> "_greater"]
          [#\| "_pipe"]
          [#\⊥ "_bot"]
          [#\⊤ "_top"]
          [#\⋄ "_diamond"]
          [(? char-symbolic?) (error (format "bad character: ~a" c))]
          [else (string c)]))
        (string->list name)))]))

(define (format-ir-rule rule)
  (match rule
    [`(rule ,heads ,bodys)
      (format-source-tree-rule `(prov (,@(set->list heads) <-- ,@(set->list bodys)) _) #f)
      #;(format "[~a <-- \n ~a]" (intercalate "\n " (map strip-prov (set->list heads)))
                               (intercalate "\n " (map strip-prov (set->list bodys))))]))
(define (format-ir-fixed-rule rule)
  (match rule
    [`(rule ,heads ,bodys)
      (format "[ ~a\n <-- \n ~a]" (intercalate "\n " (map strip-prov (set->list heads)))
                               (intercalate "\n " (map strip-prov (set->list bodys))))]))
(define (format-source-tree-rule rule [hide-wildcard-ids #t])
 (define (wildcard? sym) 
  (and hide-wildcard-ids
       (string-prefix? (symbol->string sym) "$_")
       (= 1 (length (filter (λ (item) (equal? item sym)) (flatten rule))))))
 (define (format-clause cl)
  (match cl
    [`(agg ,tag ,rel) `(agg ,(strip-prov tag) ,(strip-prov rel))]
    [(? rel-arity? r) r]
    [`(INNER-RULE ,ir) (format-source-tree-rule ir)]
    [`(prov ((prov = ,=pos) (prov ,(? var? x) ,xpos) ,cl2) ,pos)
      #:when (wildcard? x)
      (format-clause cl2)]
    [`(prov (LIST-SYNTAX ,args ...) ,pos)
      (format "[~a]" (map format-clause args))]
    [`(prov (! ,banged-clause) ,pos) (format "!~a)" (format-clause `(prov ,banged-clause ,pos)))]
    [`(prov (,tag ,ics ...) ,pos)
      (format "(~a)"  (intercalate " " (map format-clause (cons tag ics))))]
    
    [`(prov ,(? arg? arg) ,pos)
          (format "~a" arg)]
    ['... "..."]
    ['-- "--"]
    ['<-- "<--"]))

 (match-define `(prov (,heads ... <-- ,bodys ...) ,pos) rule)
 (define (lines str) (string-split str "\n"))
 (format "[~a\n <--\n ~a]" (intercalate "\n " (flat-map (compose lines format-clause) heads)) 
                           (intercalate "\n " (flat-map (compose lines format-clause) bodys))))

(define (pretty-syntax-error synerr)
  (match-define (syntax-error `(prov ,_ ,pos) msg) synerr)
  (pretty-error-current pos msg))

(define (rule-prov<? prov1 prov2)
  (match* (prov1 prov2) 
    [(`(rule-prov ,kind1 ,rule1 ,(? string? file1) ,id1) 
      `(rule-prov ,kind2 ,rule2 ,(? string? file2) ,id2))
      (if (equal? file1 file2) (< id1 id2) (string<? file1 file2))]
    [(_ _) #f]))

(define (print-ir-flat ir)
  (match-define `(ir-flat ,_ ,ir-flat-rules-h ,ir-flat-comp-rules-h) ir)
  (printf "ir-flat rules:\n")
  (define rules-grouped (group-by cdr (hash->list ir-flat-rules-h)))
  (for ([group (sort rules-grouped rule-prov<? #:key (compose cdr first))])
    (printf "------------------------------------------------------\n")
    (printf "~a:\n" (cdr (first group)))
    (for ([r (map car group)])
      (printf "~a\n" (format-ir-rule r))
      (newline)))
  (when (> (hash-count ir-flat-comp-rules-h) 0) (printf "COMP RULES:\n"))
  (for ([r (hash-keys ir-flat-comp-rules-h)])
    (printf "~a\n" (strip-prov r)))
  (newline)
  ir)

(define (print-ir-fixed ir)
  (match-define `(ir-fixed ,_ ,rules-h ,ir-flat-comp-rules-h) ir)
  (printf "ir-fixed rules:\n")
  (define rules-grouped (group-by cdr (hash->list rules-h)))
  (for ([group (sort rules-grouped rule-prov<? #:key (compose cdr first))])
    (printf "------------------------------------------------------\n")
    (match-define `(rule-prov ir-flat ,rule ,file ,id) (cdr (first group)))
    (printf "~a ~a:\n" file id)
    (for ([r (map car group)])
      (printf "~a\n" (format-ir-fixed-rule r))
      (newline)))
  (when (> (hash-count ir-flat-comp-rules-h) 0) (printf "COMP RULES:\n"))
  (for ([r (hash-keys ir-flat-comp-rules-h)])
    (printf "~a\n" (strip-prov r)))
  (newline)
  ir)

(define (print-ir-small ir)
  (match-define `(ir-small (ir-fixed (ir-flat ,source-tree ,_ ...) ,_ ...) ,rules-h ,comp-rules-h) ir)
  (printf "ir-small: \n")
  (when (> (hash-count rules-h) 0) (printf "RULES:\n"))
  (define rules-grouped (group-by cdr (hash->list rules-h)))
  (for ([group (sort rules-grouped rule-prov<? #:key (compose cdr first))])
    (printf "------------------------------------------------------\n")
    (match-define `(rule-prov ir-fixed ,rule ,module ,source-id) (cdr (first group)))
    ; (printf "~a:\n" (strip-prov `(rule-prov ir-fixed ,rule ,module ,source-id)))
    (printf "~a ~a:\n" module source-id)
    (for ([r (map car group)])
      (match-define `(srule ,head ,bodys ...) r)
      (printf "[~a <--\n  ~a]\n" (strip-prov head) (intercalate "\n  " (map strip-prov bodys)))))
  (when (> (hash-count comp-rules-h) 0) (printf "COMP RULES:\n"))
  (for ([r (hash-keys comp-rules-h)])
    (printf "~a\n" (strip-prov r)))
  (newline)
  ir)

(define (print-ir-select ir)
  (match-define `(ir-select ,ir-old ,rel-h ,rules-h ,comp-rules-h) ir)
  (printf "ir-select: \n")
  (for ([rel (hash-keys rel-h)])
    (printf "~a: ~a\n" rel (hash-ref rel-h rel)))
  (when (> (hash-count rules-h) 0) (printf "RULES:\n"))
  (for ([r (hash-keys rules-h)])
    (printf "~a\n" (strip-prov r)))
  (when (> (hash-count comp-rules-h) 0) (printf "COMP RULES:\n"))
  (for ([r (hash-keys comp-rules-h)])
    (printf "~a\n" (strip-prov r)))
  (newline)
  ir)

(define (print-ir-scc ir)
  (match-define `(ir-scc ,ir-old ,scc-dag ,scc-h ,comp-rules-h) ir)
  (printf "ir-scc: \n")

  (pretty-print scc-dag)         
  (for ([scc-id (hash-keys scc-h)])
    (match-define `(scc ,looping ,rel-h ,rules-h) (hash-ref scc-h scc-id))
    (pretty-print `(scc ,scc-id ,looping ... ...))
    (printf "rel-h: \n")
    (pretty-print rel-h)
    (map pretty-print
          (map strip-prov (hash-keys rules-h)))
    (newline))
  (when (not (hash-empty? comp-rules-h))
    (printf "\nCOMP RULES: \n")
    (for ([comp-rule (hash-keys comp-rules-h)])
      (pretty-print (strip-prov comp-rule))
      (newline)))
  ir)

(define (print-ir-incremental ir)
  (match-define `(ir-incremental ,ir-scc ,dag ,scc-map ,comp-rules-h) ir)
  (printf "ir-incremental: \n")

  (pretty-print dag)         
  (for ([scc-id (hash-keys scc-map)])
    (match-define `(scc ,looping ,rel-h ,rules-h) (hash-ref scc-map scc-id))
    (pretty-print `(scc ,scc-id ,looping ... ...))
    (map pretty-print
          (map strip-prov (hash-keys rules-h)))
    (newline))
  (when (not (hash-empty? comp-rules-h))
    (printf "\nCOMP RULES: \n")
    (for ([comp-rule (hash-keys comp-rules-h)])
      (pretty-print (strip-prov comp-rule))
      (newline)))
  ir)

(define (internal-rel-name? rel-name)
  (define rel-name-str (match (strip-prov rel-name)
    [`(agg ,aggregator ,rel) "aggregator"]
    [symbol (symbol->string symbol)]))
  (and (string-prefix? rel-name-str "$")
       (not (equal? rel-name-str "$lst"))
       (not (equal? rel-name-str "$nil"))))


(define (facts-dir-facts-files dir)
  (filter-map 
    (λ (path)
      (if (and (equal? (file-or-directory-type path) 'file)
                (member (path-get-extension path) (list #".csv" #".facts")))
          path
          #f))
    (directory-list dir #:build? #t)))


(define make-facts-csv-reader
  (make-csv-reader-maker
   '((separator-chars            #\tab)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))

(define (facts-file-facts filename)
  (define rel-name (facts-file-rel-name filename))
  (define rel-name-symbol (string->symbol rel-name))
  (define row-reader (make-facts-csv-reader (open-input-file filename)))
  (printf "reading facts file: ~a\n" filename)
  (csv-map 
    (λ (row) (cons  rel-name-symbol 
                    (map (λ (x)
                          (define xn (string->number x))
                          (if xn xn x)) 
                         row)))
    row-reader))

(define (facts-file-rel-name filename)
  (path->string (file-name-from-path (path-replace-extension filename ""))))