;; Slog-lang Interpreter.
;;
;; Copyright (c) Kristopher Micinski, Thomas Gilray, Sidharth Kumar,
;; see License.md
#lang racket

(provide slog-interp
         debug-tag
         val->number
         get-id->rel-arity
         ir-interp-facts-count
         create-initial-database
         check-compatible-database
         (struct-out Ir-interp)
         copy-ir-interp)

(require
  racket/hash
  binaryio               ;; For write-tuple
  math/number-theory     ;; For with-modulus in the hash function
  "lang-predicates.rkt"
  "utils.rkt"
  "graphs.rkt"
  "slog-params.rkt"
  "builtins.rkt")

;;
;; Top-level prompt tag for interpreter
;;
(define debug-tag (make-continuation-prompt-tag 'debug))

(struct Ir-interp (ir scc-order rel-arity-map db-instance comp-rel-funcs) #:transparent)
(define-syntax (copy-ir-interp stx)
  (syntax-case stx ()
    [(_ ir-interp-instance fields ...)
     #`(struct-copy Ir-interp ir-interp-instance fields ...)]))
;;
;; Binary format
;; 

;; 
;; Facts are represented in the frontend as tagged values that conform
;; to the tval? predicate. This representation allows ease of
;; debugging the interpreter. These facts are then organized into an
;; indices map as specified below.
;; 
;; Facts are represented in the backend as 64-bit tagged IDs
;; that encode either primitive values (possibly interned) or tuple
;; IDs. The format is represented as follows:
;; 
;; +-------------------+---------------------+--------------------+
;; | Rel. ID (18 bits) | Bucket ID (18 bits) | Tuple ID (28 bits) |
;; +-------------------+---------------------+--------------------+
;; 
;; The tag is one of the following values:
;;  - 0 Integer 
;;  - 1 Float
;;  - 2 String
;;  - 3 Symbol
;;  - 4 Bool
;;  - N > 255. Relation IDs.
;;
;; Each relation is assigned an ID. Buckets and subbuckets are
;; compuated via a hash specified in the add-flat-fact
;; function. Tuples are assigned IDs via a "bump-pointer" allocation
;; strategy. In the interpreter, we count down from 2**28-1, as the
;; MPI backend counts up from 0 as it generates new tuples.

;; Tags for primitive tags
(define integer-tag 0)
(define float-tag 1)
(define string-tag 2)
(define symbol-tag 3)
(define boolean-tag 4)

;; Reserve the first 256 tags for primitives
(define min-relation-tag 256)

;; All versions
(define all-versions '(new delta total))

;; Tagged values
(define (tval? x)
  (define (interned? x) (match x ['string #t] ['symbol #t] [_ #f]))
  (match x
    [`(tag ,(? number? relation-id) ,(? number? bucket-id) ,(? number? id)) #t]
    [`(interned ,(? interned?) ,(? number?)) #t]
    [`(integer ,(? integer?)) #t]
    [`(bool ,(? boolean?)) #t]
    [else #f]))

;; 
;; Interning
;;

(define (extend-intern-map obj tag-kind db-instance)
  (match-define `(db-instance ,rm ,im ,tc ,intern-map ,added-facts) db-instance)
  (let ([possible-id (hash-ref intern-map obj #f)])
       (if possible-id
           (values possible-id db-instance)
           ;; Bump the tag-kind for the given thing and then add it to the
           ;; intern map
           (let* ([updated-tc (hash-set tc tag-kind (add1 (hash-ref tc tag-kind 0)))]
                  [intern-id (sub1 (hash-ref updated-tc tag-kind))]
                  [id `(interned ,tag-kind ,intern-id)]
                  [updated-intern-map (hash-set (hash-set intern-map id obj) obj id)])
             (values id `(db-instance ,rm ,im ,updated-tc ,updated-intern-map ,added-facts))))))

;; Code that overlaps interning logic between symbols and strings.
(define (extend-intern-map-for-ir-interp obj tag-kind ir-interp)
  (match-define (Ir-interp ir scc-order ram db-instance _) ir-interp)
  (define-values (id updated-db-instance) (extend-intern-map obj tag-kind db-instance))
  (values id (copy-ir-interp ir-interp [db-instance updated-db-instance])))

;; Utilities for interning strings and symbols.
(define (add-string str ir-interp)
  (extend-intern-map-for-ir-interp str 'string ir-interp))

(define (add-symbol sym ir-interp)
  (extend-intern-map-for-ir-interp sym 'symbol ir-interp))

;; The following do not need to be interned as they are represented
;; literally.
(define (tag-integer int) `(integer ,int))
(define (tag-bool bool) `(bool ,(if bool 1 0)))

;; Useful utility to convert an argument to either a tagged value or
;; leave it alone (if it's just a variable).
(define (arg->var-or-tagged-value x ir-interp)
  (match x
    ;; If it's an interned literal, look up 
    [(? lit?) (lit->tagged-value x ir-interp)]
    [(? var?) x]))

;; Lookup an interned literal value (assuming it has been interned in
;; the intern map)
(define (lookup-interned-lit prim ir-interp)
  (match (Ir-interp-db-instance ir-interp)
    [`(db-instance ,_ ,_ ,_ ,intern-map ,_)
     (let ([x (match prim
                [(? string?)       prim ]
                [`(,(? symbol? s)) s])])
       ;; todo: should we need this?
       (if (hash-has-key? intern-map x)
           (hash-ref intern-map x)
           (error (format "invalid literal expression ~a" x))))]))

;; Translate a literal value to a tagged value
(define (lit->tagged-value lit ir-interp)
  (match lit
    [(? interned-lit?) (lookup-interned-lit lit ir-interp)]
    [(? integer?) (tag-integer lit)]
    ['true        (tag-bool lit)]
    ['false       (tag-bool lit)]))

;;
;; Global interpreter state
;;

;; We track a single boolean that asks whether the database has been
;; updated at all. This is easier to keep global than to thread
;; through the IR.
(define added-any-new-facts #f)
(define (reset-added-any-new-facts) (set! added-any-new-facts #f))
(define (added-a-fact) (set! added-any-new-facts #t))

;;
;; Adding and interning facts
;;

;; Add a flat (not recursive) fact of a form:
;; 
;; `(,rel-name ,id0 ...) <-- Adds the fact to each version (in the
;; database) for the given rel-name
;;
;; `(rel-select ,h-rel-name ,h-arity ,h-selection) <-- Adds the fact
;; to only the specified version of the relation
;; 
;; Return values:
;;  - New (or tabled) fact ID
;;  - Updated interpreter data structure
(define (add-flat-fact fact ir-interp version [count-down #f])
  (match-define `(db-instance ,rel-map ,indices-map ,tag-map ,intern-map ,added-facts) (Ir-interp-db-instance ir-interp))

  ;; Computes the tuple ID for a given fact and canonical index
  (define (compute-tuple-id relation-id canonical-index tuple-id) ;; TODO SLOW
    (match-define `(,name ,args ...) fact)
    (define nfact (map (lambda (arg) (val->number arg ir-interp)) args)) ;; convert the flat fact to numbers
    ;; We assume that canonical indices will not include 0
    (when (ormap (lambda (x) (equal? x 0)) canonical-index)
      (error (format "canonical index ~a contains 0. We assume this will not happen" canonical-index)))
    (define ordered-fact (project-then-rest nfact (map sub1 canonical-index)))
    (define bucket (modulo (tuple-hash (list->vector ordered-fact) (length canonical-index)) (slog-bucket-count))) ;; TODO modulo is SLOW
    (define nonjoin-cols (drop ordered-fact (length canonical-index)))
    `(tag ,relation-id ,bucket ,tuple-id))

  ;; This helper function allows handling inserting into either a
  ;; single selection or all selections at the same time.
  (define (helper rel-instance ids rel-name rel-arity select-orders)
    (match-define `(,_ ,canonical-index ,_) (hash-ref (Ir-interp-rel-arity-map ir-interp) rel-arity))
    (match-define `(rel-instance ,fact-map ,relation-id ,next-id) rel-instance)
    (if (hash-ref fact-map ids #f)
        ;; If so, simply return that fact
        (values (hash-ref fact-map ids #f) ir-interp)
        ;; Else, add it to the fact database
        (let* ([tuple-id next-id]
               [id (compute-tuple-id relation-id canonical-index tuple-id)]
               ;; The tuple is the fact prefixed w/ ID
               [tuple (cons id ids)]
               [next-id ((if count-down sub1 add1) next-id)]
               ;; Updated the (bijective) fact map. Add a mapping for (id0 ...) -> new-id
               [updated-fact-map (hash-set (hash-set fact-map ids id) id ids)]
               [new-rel-instance `(rel-instance ,updated-fact-map ,relation-id ,next-id)]
               [new-rel-map (hash-set rel-map rel-arity new-rel-instance)]
               ;; For each select order, update the indices
               ;; map accordingly. This fact was not
               ;; previously in the database, so no need to
               ;; check for its existence.
               [new-indices-map
                (begin 
                  (foldl
                   (λ (next-select-order indices-map)
                     (let ([key-tuple (project tuple next-select-order)]
                           [whole-tuple (project-then-rest tuple next-select-order)])
                       (let* ([rel-version `(rel-version ,rel-name ,(length ids) ,next-select-order ,version)]
                              [index-map (hash-ref indices-map rel-version)]
                              [new-index-map (hash-set index-map
                                                       key-tuple
                                                       (set-add
                                                        (hash-ref index-map key-tuple (set))
                                                        whole-tuple))])
                         (hash-set indices-map rel-version new-index-map))))
                   indices-map
                   select-orders))])
          ;; Mutably signal we've added a fact
          (added-a-fact)
          (values id (copy-ir-interp ir-interp [db-instance `(db-instance
                                  ,new-rel-map
                                  ,new-indices-map
                                  ,tag-map
                                  ,intern-map
                                  ,added-facts)]) ))))

  (match fact
    [`((rel-select ,rel-name ,arity ,selection db) ,ids ...)
     (let* ([rel-arity `(rel-arity ,rel-name ,arity db)]
            [rel-instance (hash-ref rel-map rel-arity)])
       (helper rel-instance ids rel-name rel-arity (list selection)))]
    #;
    [`(,rel-name ,ids ...)
     (let* ([rel-arity `(rel-arity ,rel-name ,(length ids))]
            [rel-instance (hash-ref rel-map rel-arity)]
            [select-orders (set->list (match (hash-ref ram rel-arity)
                                        [`(,_ ,_ ,select-orders) select-orders]))])
       (helper rel-instance ids rel-name rel-arity select-orders))]))

;; Recursively add a fact (from a source-tree?) in a starting IR
;; state. Returns values:
;;
;; - A u64 (encoding the value, either an ID in the case of a
;; recursive fact, atom, etc or a tagged number, string, etc...)
;; 
;; - An updated IR (adds values to things like the atom / tag map,
;; etc...)
(define (recursively-add-fact fact ir-interp)
  (match fact
    [(? integer?) (values (tag-integer fact) ir-interp)]
    ['true        (values (tag-bool fact) ir-interp)]
    ['false       (values (tag-bool fact) ir-interp)]
    [(? string?)  (add-string fact ir-interp)]
    [`(,(? symbol? rel-name) ,args ...)
    (define arity (sub1 (length fact)))
    (match-define `(,_ ,canonical-index ,_) (hash-ref (Ir-interp-rel-arity-map ir-interp) `(rel-arity ,rel-name ,arity db)))
    (match-let ([(cons id-list updated-ir-interp)
                  ;; Walk over each of the subfacts and generate
                  ;; IDs for them after adding them to the
                  ;; database.
                  (foldl (λ (next-arg accum)
                          (match accum
                            [(cons accumulated-args ir-interp*)
                              (let-values ([(id ir-interp**)
                                            (recursively-add-fact next-arg ir-interp*)])
                                (cons (cons id accumulated-args) ir-interp**))]))
                        (cons '() ir-interp)
                        args)])
      (add-flat-fact `((rel-select ,rel-name ,arity ,canonical-index db) . ,(reverse id-list))
                      updated-ir-interp
                      'total
                      #t))]))

;; Recursively traverses rules to ensure that if they use strings or
;; symbols that those get added to the intern map.
(define (intern-within-rule rule ir-interp)
  (define (h cl ir-interp)
    (define (k lit)
      (match lit
        [(? string?) (let-values ([(s ir-interp) (add-string lit ir-interp)]) ir-interp)]
        [`(,(? symbol? s)) (let-values ([(s ir-interp) (add-symbol s ir-interp)]) ir-interp)]
        [_ ir-interp]))
    (match cl
      [`(,rel-name ,args ...) (foldl (λ (arg ir-interp) 
                                        (let-values ([(s ir-interp) (add-string arg ir-interp)]) ir-interp))
                                      ir-interp
                                      (filter string? args))]))
  (match (Ir-interp-db-instance ir-interp)
    [`(db-instance ,relation-map ,indices-map ,tag-count ,string-symbol-id-map ,added-facts)
     (match (strip-prov rule)
       [`(srule ,hcl ,bcls ... ) (foldl h ir-interp (cons hcl bcls))]
       [`(arule ,_ ,_) ir-interp]
       [`(crule ,hcl ,bcls ...) (foldl h ir-interp (cons hcl bcls))])]))

;; Given a source tree (the program), initialize a facts database,
;; return an updated ir-interp.
(define (interning-pass source-tree ir-interp scc-map)
  (define (set-all-keys-to relation-map value)
    (foldl (lambda (rarity-key rel-map-acc)
             (match (hash-ref relation-map rarity-key)
               [`(rel-instance ,h ,tag ,num) (hash-set rel-map-acc rarity-key `(rel-instance ,h ,tag ,value))]))
           relation-map
           (hash-keys relation-map)))
  (define after-adding-facts
    (match (Ir-interp-db-instance ir-interp)
      [`(db-instance ,relation-map ,indices-map ,tag-count ,string-symbol-id-map ,added-facts)
       ;; Reset the relation map so that the starting keys begin at
       ;; 2**28-1, then count down.
       (define first-tuple-id (- (expt 2 28) 1))
       (define reset-initial-tuple-ids
          (copy-ir-interp ir-interp [db-instance `(db-instance ,(set-all-keys-to relation-map first-tuple-id) ,indices-map ,tag-count ,string-symbol-id-map ,added-facts)]))
       (define new-ir-interp
         (foldl (λ (mod ir-interp)
                  (foldl (λ (fact ir-interp) 
                            (let-values ([(out updated-ir-interp) (recursively-add-fact fact ir-interp)])
                              updated-ir-interp))
                         ir-interp
                         (map strip-prov (set->list (Module-facts mod)))))
                reset-initial-tuple-ids
                (hash-values source-tree)))
       ;; Now set all keys back to 0
       (match (Ir-interp-db-instance new-ir-interp)
         [`(db-instance ,relation-map ,indices-map ,tag-count ,string-symbol-id-map ,added-facts)
          (copy-ir-interp new-ir-interp 
            [db-instance `(db-instance ,(set-all-keys-to relation-map 0) ,indices-map ,tag-count ,string-symbol-id-map ,added-facts)])])]))
  (define after-interning-in-rules
    (foldl (λ (scc ir-interp)
           (match scc [`(scc ,_ ,_ ,rule-map)
                       (foldl intern-within-rule ir-interp (hash-keys rule-map))]))
         after-adding-facts
         (hash-values scc-map)))
  (define comp-rules-h (ir-incremental-comp-rules-h (Ir-interp-ir after-interning-in-rules)))
  (foldl intern-within-rule after-interning-in-rules (hash-keys comp-rules-h)))

(define (ir-incremental-comp-rules-h ir-incremental)
  (match-define `(ir-incremental ,ir-scc ,dag ,scc-map ,comp-rules-h) ir-incremental)
  comp-rules-h)

;; create initial interpreter IR from an ir-incremental
(define (slog-initialize-interpreter ir-incremental)
  (match-define `(ir-incremental ,ir-scc ,dag ,scc-map ,comp-rules-h) ir-incremental)
  (when (= (length (hash-values scc-map)) 0) (raise 'no-rules))
  (define first-scc (first (hash-values scc-map)))
  (define next-relation-tag min-relation-tag)
  (define relation-map
    (match first-scc
      [`(scc ,_ ,rel-arity-map ,_)
       (foldl (λ (rel-arity h) 
                (let ([relation-tag next-relation-tag])
                  (set! next-relation-tag (add1 next-relation-tag))
                  ;; `(rel-instance fact<->id map (bijective),
                  ;; relation tag, next ID to use)
                  (hash-set h rel-arity `(rel-instance ,(hash) ,relation-tag 0))))
              (hash)
              (hash-keys rel-arity-map))]))
  (define rel-arity-map (match first-scc [`(scc ,_ ,rel-arity-map ,_) rel-arity-map]))
  (define indices-map
    (match first-scc
      ;; Pull out the rel-arity map
      [`(scc ,_ ,rel-arity-map ,_)
       (foldl
        (λ (rel-arity h)
          (match rel-arity
            [`(rel-arity ,rel-name ,arity ,kind)
             (match (hash-ref rel-arity-map rel-arity)
               [`(,_ ,canonical-select-set ,select-orders)
                (foldl
                 (λ (select-order h)
                   (foldl
                    (λ (version h)
                      (hash-set h
                                `(rel-version ,rel-name ,arity ,select-order ,version)
                                (hash)))
                    h
                    all-versions))
                 h
                 (set->list select-orders))])]))
        (hash)
        (hash-keys rel-arity-map))]))
  ;; Topologically sort the SCCs in order
  (define ordered-sccs (if (empty? (topological-sort dag)) (hash-keys dag) (topological-sort dag)))
  (define comp-rel-funcs-hash (create-comp-rel-funcs-given-rules (hash-keys comp-rules-h)))
  (Ir-interp
    ir-incremental
    ordered-sccs
    rel-arity-map
    `(db-instance 
     ; Relation map: rel-arity? -> `(rel-instance (hashof (or/c id
     ; (fact ...)) (or/c id? fact?)) ,tag ,local-id-count)
     ,relation-map
     ; Indices map: rel-version? -> (hashof (listof
     ; nonnegative-integer?) (set/c (listof nonnegative-integer?))
     ,indices-map
     ; Tag id count:
     ,(hash 'relation next-relation-tag 'string 0 'symbol 0)
     ; string?, symbol?, id? map:
     ; (hash/c (or/c string? symbol? id?) (or/c string? symbol? id?))
     ,(hash)
     ; added facts:
     (set))
     comp-rel-funcs-hash))

;; Hash a vector of numbers (all interpreted to be unsigned 64-bit
;; values) based on the first `len` entries in u64-vector. This uses
;; the same hash function as the backend's tuple_hash.
;; (tuple-hash '#(18374969058471772671
;;                18374969058471772670
;;                18374969058471772669
;;                18374969058471772668
;;                18374969058471772667) 3) is 17972185552811111622
(define (tuple-hash u64-vector prefix-len) ;; TODO  SLOW
  (define base 14695981039346656037)
  (define prime 1099511628211)
  (with-modulus (expt 2 64)
    (mod 
     (foldl
      (lambda (i cur-hash)
        (let* ([chunk (mod (vector-ref u64-vector i))]
               [h0 (mod (bitwise-xor cur-hash (mod (bitwise-and chunk 255))))]
               [h1 (mod (* h0 prime))])
          ;; Fold accumulates (cons chunk hash)
          (cdr
           (foldl (lambda (j chunk-hash)
                    (let* ([next-chunk (mod (arithmetic-shift (car chunk-hash) -8))]
                           [h0 (mod (bitwise-xor (cdr chunk-hash) (mod (bitwise-and next-chunk 255))))]
                           [h1 (mod (* h0 prime))])
                      (cons next-chunk h1)))
                  (cons chunk h1)
                  (range 7))))) base (range prefix-len)))))


;; Converts a tagged value to a numeric representation. Requires an IR
;; to pull the intern map for strings and symbols.
;; 
;; +-------------------+------------------+--------------------+
;; | Rel. ID (16 bits) | Bucket (16 bits) | Tuple ID (32 bits) |
;; +-------------------+------------------+--------------------+
(define (val->number val ir)
  (define (generate-tag v0 v1 v2)
    (bitwise-ior
     (arithmetic-shift v0 48) (bitwise-ior (arithmetic-shift v1 32) v2)))
  (match val
    [`(tag ,rel-id ,bucket-id ,id) (generate-tag rel-id bucket-id id)]
    [`(interned ,kind ,(? number? n)) 
     (match-define `(db-instance ,_ ,_ ,_ ,intern-map ,_) (Ir-interp-db-instance ir))
     (define rel-id (match kind ['symbol 3] ['string 2] [_ (error (format "unexpected kind ~a" kind))]))
     (generate-tag rel-id 0 n)]
    [`(integer ,(? integer? n)) (generate-tag integer-tag 0 n)]
    [`(boolean ,(? boolean? tf)) (generate-tag boolean-tag 0 tf)]
    [else (error (format "expected a value, got ~a" val))]))

;; Create an initial database suitable for this program with empty
;; files (containing the correct structure), along with a manifest
;; specifying the canonical indices.
(define (create-initial-database program directory)
  ;(displayln (format "Creating initial database in directory ~a" directory))
  
  ;; run the interning pass on this program to populate pools of static constants (strings, ..)
  (define post-interning
    (match program
      [`(ir-incremental ,ir-scc ,dag ,scc-map ,comp-rules-h)
       (interning-pass (ir->source-tree ir-scc) (slog-initialize-interpreter program) scc-map)]))
  (match-define `(ir-incremental ,ir-scc ,dag ,scc-map ,comp-rules-h) (Ir-interp-ir post-interning))
  (match-define `(db-instance ,rm ,im ,tc ,intern-map ,added-facts) (Ir-interp-db-instance post-interning))
  ;; get rel-h helper map from ir-scc
  (match-define `(ir-scc (ir-select ,oir ,rel-h ,rules-h ,_comp-rules-h) . ,_) (second program))
  (define rel-arity->canonical-index
    (foldl (lambda (rel-arity h) (hash-set h rel-arity (car (hash-ref rel-h rel-arity))))
         (hash)
         (hash-keys rel-h))) 
  
  ;; create the input directory if it doesn't exist
  (make-directory* directory)

  ;; Write an empty file for this specific rel-arity
  (define (touch-empty-files rel-arity)
    (match-define `(rel-arity ,rel ,arity ,kind) rel-arity)
    ;; touch empty output/size files
    (define extn ".dat")
    (define output-file (format "~a/~a_~a" directory rel arity))
    (define out (open-output-file output-file #:exists 'replace))
    (close-output-port out)
    (define output-size-file (format "~a.size" output-file))
    (define size-out (open-output-file output-size-file #:exists 'replace))
    (define rows 0)
    (define columns (add1 arity))
    (display (format "~a\n~a\n" rows columns) size-out)
    (close-output-port size-out)
    `(,output-file ,output-size-file))
  
  ;; build up a list of metatdata for each relation: name, arity, ID,
  ;; #facts (starts at 0, incremented by other tools), data file, and
  ;; data size file.
  (define relations
    (foldl
     (lambda (rel-arity relations)
       (match-define `(rel-instance ,_ ,relation-id ,_) (hash-ref rm rel-arity))
       (match-define `(,data-file ,size-file) (touch-empty-files rel-arity)) 
       (match-define `(rel-arity ,rel ,arity ,_) rel-arity)
       (define canonical-index (hash-ref rel-arity->canonical-index rel-arity))
       (define num-facts 0)
       `((relation ,rel ,arity ,relation-id ,num-facts ,canonical-index ,data-file ,size-file) . ,relations))
     '()
     (hash-keys rm)))
  
  (define (write-strings)
    (define output-file (format "~a/$strings.csv" directory))
    (define out (open-output-file output-file #:exists 'replace))
    (define x 0)
    (for ([key (hash-keys intern-map)])
      (match key
        [`(interned string ,n)
         (set! x (max x n))
         (displayln (format "~a\t~a" n (hash-ref intern-map key)) out)]
        [_ (void)]))
    (close-output-port out)
    x)
  
  (define max-strings (write-strings)) 
  
  ;; write the manifest file
  (define manifest
    `(manifest
      (relations ,relations)
      (strings "$strings.csv" ,max-strings)))
  
  (define out-port (open-output-file (format "~a/manifest" directory) #:exists 'replace))
  (write manifest out-port)
  (close-output-port out-port))

;; Create an initial database suitable for this program with empty
;; files (containing the correct structure), along with a manifest
;; specifying the canonical indices.
(define (check-compatible-database directory ir-interp)
  (void))
  ;; (match-define `(ir-incremental ,ir-scc ,dag ,scc-map ,comp-rules-h) (Ir-interp-ir ir-interp))
  ;; (match-define `(db-instance ,rm ,im ,tc ,intern-map ,added-facts) (Ir-interp-db-instance ir-interp))

  ;; ;; Calculate canonical indices by digging into original IR
  ;; (match-define `(ir-incremental (ir-scc (ir-select ,oir ,rel-h ,rules-h ,_comp-rules-h) . ,_) . ,_)
  ;;   (Ir-interp-ir ir-interp))
  
  ;; (define rel-arity->canonical-index
  ;;   (foldl (lambda (rel-arity h) (hash-set h rel-arity (car (hash-ref rel-h rel-arity))))
  ;;          (hash)
  ;;          (hash-keys rel-h))) 
  
  ;; ;; create the input directory if it doesn't exist
  ;; (make-directory* directory)

  ;; ;; Write an empty file for this specific rel-arity
  ;; (define (touch-empty-files rel-arity)
  ;;   (match-define `(rel-arity ,rel ,arity ,kind) rel-arity)
  ;;   ;; touch empty output/size files
  ;;   (define extn ".dat")
  ;;   (define output-file (format "~a/~a_~a" directory rel arity))
  ;;   (define out (open-output-file output-file #:exists 'replace))
  ;;   (close-output-port out)
  ;;   (define output-size-file (format "~a.size" output-file))
  ;;   (define size-out (open-output-file output-size-file #:exists 'replace))
  ;;   (define rows 0)
  ;;   (define columns (add1 arity))
  ;;   (close-output-port size-out)
  ;;   `(,output-file ,output-size-file))
  
  ;; ;; build up a list of metatdata for each relation: name, arity, ID,
  ;; ;; #facts (starts at 0, incremented by other tools), data file, and
  ;; ;; data size file.
  ;; (define relations
  ;;   (foldl
  ;;    (lambda (rel-arity relations)
  ;;      (match-define `(rel-instance ,_ ,relation-id ,_) (hash-ref rm rel-arity))
  ;;      (match-define `(,data-file ,size-file) (touch-empty-files rel-arity)) 
  ;;      (match-define `(rel-arity ,rel ,arity ,_) rel-arity)
  ;;      (define canonical-index (hash-ref rel-arity->canonical-index))
  ;;      (define num-facts 0)
  ;;      `((relation ,rel ,arity ,relation-id ,num-facts ,data-file ,size-file) . ,relations))
  ;;    '()
  ;;    (hash-keys rm)))
  
  ;; (define (write-strings)
  ;;   (define output-file (format "~a/$strings.csv" directory))
  ;;   (define out (open-output-file output-file #:exists 'replace))
  ;;   (for ([key (hash-keys intern-map)])
  ;;     (match key
  ;;       [`(interned string ,n)
  ;;        (displayln (format "~a\t~a" n (hash-ref intern-map key)))]
  ;;       [_ (void)]))
  ;;   (close-output-port out))
  
  ;; (write-strings)
  
  ;; ;; write the manifest file
  ;; (define manifest
  ;;   `(manifest
  ;;     (relations ,description)
  ;;     (strings "$strings.csv")))
  
  ;; (define out-port (open-output-file (format "~a/manifest" directory) #:exists 'replace))
  ;; (write manifest out-port)
  ;; (close-output-port out-port))
  


;; Interpret a rule within the context of a database. Update the
;; database accordingly and hand it back.
;; 
;; The basic idea of our approach is to--for each rule--perform a
;; selection / join and then build up a set of constraints on each of
;; the variables in the rule. For example (ignoring the complexities
;; of versions for this high-level explanation), the following rule:
;; 
;;     (R x x y) <-- (T x 2 x y)
;;
;; would loop through each of the tuples for `T`, and--for each
;; tuple--generate a set of constraints on x and y before finally
;; placing them in the map (assuming the constraint could be
;; satisfied). For example, say that T contained the two tuples:
;;     (1 2 3 4)
;;     (3 2 3 4)
;; 
;; The first would generate the set of constraints:
;;     x = 1 /\ x = 3
;;
;; Which is unsatisfiable. Note that we could also have generated the
;; constraint 2 = 2, however we did not. In cases where we fail to
;; unify two atoms we fail immediately (as an optimization).
;; 
;; Joins work similarly, for example:
;;     (R x x y) <-- (T x 2 x y), (R x x)
;;
;; Will perform selection from T on the common columns for T and R,
;; gather up constraints shared between the two, solve them, and then
;; insert into R.
(define (interpret-rule raw-rule ir-interp)
  (define rule (strip-prov raw-rule))
  (match-define `(db-instance ,relation-map ,indices-map ,tag-counts ,intern-map ,added-facts) (Ir-interp-db-instance ir-interp))
  (match-define `(ir-incremental ,ir-scc ,dag ,scc-map ,comp-rules-h) (Ir-interp-ir ir-interp))
  ;; Potentially add a fact to the database given a head clause and an
  ;; assignment. If `ordering` is included, it is assumed to be the
  ;; select ordering to use for `hclause`. So, for example, if
  ;; `ordering` is (3 4) and `hclause` is `'(x y 1 z)` then `hclause`
  ;; will be add `(1 z x y)` (after applying the assignment).
  (define (potentially-add ir-interp0 hclause assignment ordering)
    (match-define `((rel-select ,h-rel-name ,h-arity ,h-selection db) ,h-args ...) hclause)
    (define ordering* (remv 0 ordering))
    (define fact0 (map (λ (h-arg)
                        (match h-arg
                          [(? lit? l) (lit->tagged-value l ir-interp0)]
                          [(? var? x) (hash-ref assignment x)]))
                      h-args))
    (match-define (cons (list fact) ir-interp) (intern-objects-given-tuples (list fact0) ir-interp0))
    
    (match-define `(db-instance ,rm ,im ,tc ,ssidmap ,added-facts) (Ir-interp-db-instance ir-interp)) ;; TODO should this be ir-interp or ir-interp0?
    (match-define `(rel-instance ,facts-map ,_ ,_) (hash-ref rm `(rel-arity ,h-rel-name ,h-arity db)))
    (define fact-reordered ((unproject-then-rest ordering* (length fact)) fact))
    ;; Check to see if the fact is already in the database
    (if (hash-ref facts-map fact-reordered #f)
        ;; Already in the database, don't add it
        ir-interp
        ;; Not in the database, add it to new
        (let-values ([(_ ir-interp)
                      (add-flat-fact
                       `((rel-select ,h-rel-name ,h-arity ,h-selection db)
                         . ,fact-reordered)
                        ir-interp
                        'new)])
          (yield-to-debugger
           ir-interp
           `(adding ((rel-select ,h-rel-name ,h-arity ,h-selection db) . ,fact-reordered))))))

    #;(validate-indices-map indices-map)
  ;; Match on the different types of rules to implement each.
  (match rule
    ;; fact
    [`(srule 
       ((rel-select ,h-rel-name ,h-arity ,h-selection db) ,h-args ...))
     (let ([assignment (gather-constraints ir-interp (hash) (list) (list))])
        (if assignment
          (potentially-add
              ir-interp
              `((rel-select ,h-rel-name ,h-arity ,h-selection db) . ,h-args)
              assignment
              h-selection)
          ir-interp))]
    ;; Selection
    [`(srule 
       ((rel-select ,h-rel-name ,h-arity ,h-selection db) ,h-args ...)
       ,(and b `((rel-version ,b-rel-name ,b-arity ,b-selection ,b-version) ,b-args ...)))
     (let* ([b-func (get-func-for-rel-version `(rel-version ,b-rel-name ,b-arity ,b-selection ,b-version) ir-interp)]
            [tuple-inputs (make-tuple-inputs-for-join-outer-relation b ir-interp)]
            [tuples (foldl set-union (set) (map b-func tuple-inputs))])
       ;; For each of the tuples in the index map at that
       ;; selection key. Note: these tuples will be ordered
       ;; by the order specified in b-selection and must be
       ;; adjusted.
       (foldl (λ (tuple ir-interp)
                (let ([assignment (gather-constraints ir-interp (hash) tuple b-args)])
                  (if assignment
                      (potentially-add
                       ir-interp
                       `((rel-select ,h-rel-name ,h-arity ,h-selection db) . ,h-args)
                       assignment
                       h-selection)
                      ir-interp)))
              ir-interp
              (set->list tuples)))]
    ;; Binary join
    [`(srule 
       ((rel-select ,h-rel-name ,h-arity ,h-selection db) ,h-args ...)
       ,(and b0 `((rel-version ,b0-rel-name ,b0-arity ,b0-selection ,b0-version) ,b0-args ...))
       ,(and b1 `((rel-version ,b1-rel-name ,b1-arity ,b1-selection ,b1-version) ,b1-args ...)))
     (match-define (cons outer-relation inner-relation)
      (if (and (not (comp-or-agg-rel-kind? b0-version)) 
               (not (comp-or-agg-rel-kind? b1-version))
                (> (rel-version-facts-count ir-interp `(rel-version ,b0-rel-name ,b0-arity ,b0-selection ,b0-version))
                   (rel-version-facts-count ir-interp `(rel-version ,b1-rel-name ,b1-arity ,b1-selection ,b1-version))))
        (cons b1 b0)
        (cons b0 b1)))
     (match-let* ([(cons (and b0 `((rel-version ,b0-rel-name ,b0-arity ,b0-selection ,b0-version) ,b0-args ...))
                         (and b1 `((rel-version ,b1-rel-name ,b1-arity ,b1-selection ,b1-version) ,b1-args ...)))
                       (cons outer-relation inner-relation)]
                  [b0-func (get-func-for-rel-version `(rel-version ,b0-rel-name ,b0-arity ,b0-selection ,b0-version) ir-interp)]
                  [b1-func (get-func-for-rel-version `(rel-version ,b1-rel-name ,b1-arity ,b1-selection ,b1-version) ir-interp)]
                  [b0-tuple-inputs (make-tuple-inputs-for-join-outer-relation b0 ir-interp)]
                  [b0-tuples (foldl set-union (set) (map b0-func b0-tuple-inputs))])
           (foldl
            (λ (b0-tuple ir-interp)
                 (define b1-tuple-input (make-tuple-input b0-tuple b0 b1 ir-interp))
                 (define b1-tuples (b1-func b1-tuple-input))
                 (foldl
                  ;; For each corresponding tuple in b1
                  (λ (b1-tuple ir-interp)
                    ;; Now we have two tuples, b0-tuple and b1-tuple,
                    ;; we gather constraints across both and potentially
                    ;; insert into the database.
                    (let* ([assignment-b0 (gather-constraints ir-interp (hash) b0-tuple b0-args)]
                           [assignment (gather-constraints ir-interp assignment-b0 b1-tuple b1-args)])
                      (if assignment
                          (potentially-add ir-interp `((rel-select ,h-rel-name ,h-arity ,h-selection db) . ,h-args)
                                           assignment h-selection)
                          ir-interp)))
                  ir-interp
                  (set->list b1-tuples)))
            ir-interp
            (set->list b0-tuples)))]
    
    ;; Administrative rules
    [`(arule
       ((rel-select ,h-rel-name ,h-arity ,h-selection db) ,h-args ...)
       ((rel-version ,b-rel-name ,b-arity ,b-selection ,b-version) ,b-args ...))
     (match-define `(rel-instance ,h ,tag ,num) (hash-ref relation-map `(rel-arity ,b-rel-name ,b-arity db)))
    (define (reproject select-order tuple-len)
      (define len (length select-order))
      (define rng (range 0 tuple-len))
      (define total-select-order (append select-order (remove* select-order rng)))
      (λ (tuple) 
        (map (λ (i) (list-ref tuple (index-of total-select-order i))) rng)))
     (define b-delta (hash-ref indices-map `(rel-version ,b-rel-name ,b-arity ,b-selection ,b-version)))
     (define total (hash-ref indices-map `(rel-version ,h-rel-name ,h-arity ,h-selection total)))
     (define delta (hash-ref indices-map `(rel-version ,h-rel-name ,h-arity ,h-selection delta)))
     (define tuples
       (foldl (lambda (key tuples-acc)
                (foldl (lambda (tuple tuples-acc)
                         (set-add tuples-acc ((reproject b-selection (length tuple)) tuple)))
                       tuples-acc
                       (set->list (hash-ref b-delta key))))
              (set)
              (hash-keys b-delta)))
     (define index-map-piece (foldl (lambda (tuple h-acc)
                                      (let* ([projected-tuple (project-then-rest tuple h-selection)]
                                             [index (take projected-tuple (length h-selection))])
                                        (hash-set h-acc index (set-add (hash-ref h-acc index (set)) projected-tuple))))
                                    (hash)
                                    (set->list tuples)))
     (define new-indices-map
       (hash-set indices-map
                 `(rel-version ,h-rel-name ,h-arity ,h-selection new)
                 index-map-piece))
     (yield-to-debugger
      (copy-ir-interp ir-interp 
        [db-instance `(db-instance ,relation-map ,new-indices-map ,tag-counts ,intern-map ,added-facts)])
      `(finished arule ,index-map-piece))]))

(define (validate-indices-map indices-map)
  (for ([key (hash-keys indices-map)])
    (match-define `(rel-version ,rel-name ,arity ,selection ,version) key)
    (define map (hash-ref indices-map key)) 
    (for ([index (hash-keys map)])
      (when (not (= (length index) (length selection)))
        (error 'bad-indices-map "for ~a: ~a" key index)))))

;; Merge new into delta, delta into into total, empty new
(define (merge-new ir-interp rel-arity-map)
  (match-define `(db-instance ,relation-map ,indices-map ,tag-counts ,intern-map ,added-facts) (Ir-interp-db-instance ir-interp))
  (let* ([new-indices-map
          (foldl
          ;; For each rel-arity?
          (lambda (ra indices-map)
            (match-define `(rel-arity ,name ,arity ,kind) ra)
            ;; For each rel-arity key
            (match (hash-ref rel-arity-map ra)
              [`(,_ ,canonical-index ,all-selections)
                (foldl
                ;; For each selection
                (lambda (selection indices-map)
                  (let* ([new-version `(rel-version ,name ,arity ,selection new)]
                          [delta-version `(rel-version ,name ,arity ,selection delta)]
                          [total-version `(rel-version ,name ,arity ,selection total)]
                          ;; First, swap new into delta
                          [indices-map-after-setting-delta
                          (hash-set indices-map delta-version (hash-ref indices-map new-version))]
                          ;; Now, set new to just be empty
                          [indices-map-after-clearing-new
                          (hash-set indices-map-after-setting-delta new-version (hash))]
                          ;; Now, merge delta from the old map into the new map
                          [indices-map-after-merging-new
                          (foldl
                            ;; For each key into the index map for this version, copy
                            ;; the data at that key into the index map at 'delta or
                            ;; 'new as appropriate.
                            (lambda (index-in-delta acc-indices-map)
                              ;; TODO: this seems wrong. Careful: need to ensure we say explicitly that we added facts.
                              ;; We statefully track whether we've added facts through the
                              ;; entire module, which should perhaps be refactored.
                              (added-a-fact)
                              (hash-set acc-indices-map
                                        total-version
                                        (hash-set (hash-ref acc-indices-map total-version)
                                                  index-in-delta
                                                  (set-union
                                                  (hash-ref (hash-ref indices-map total-version)
                                                            index-in-delta
                                                            (set))
                                                  (hash-ref (hash-ref indices-map delta-version)
                                                            index-in-delta)))))
                            ;; Start with indices-map except replace its subhash at this-version w/ the
                            ;; empty hash, clearing 'new / 'delta.
                            indices-map-after-clearing-new
                            ;; For each of the indices at *this* version (which may not be
                            ;; the same as copy).
                            (hash-keys (hash-ref indices-map delta-version)))])
                    indices-map-after-merging-new))
                indices-map
                ;; For each column selection in the set of selections
                (set->list all-selections))]))
          indices-map
          (hash-keys rel-arity-map))])
    (copy-ir-interp ir-interp [db-instance `(db-instance ,relation-map ,new-indices-map ,tag-counts ,intern-map ,added-facts)])))

;;
;; Constraints
;; 

;; Helper function to gather assignments for a given tuple and a given
;; body clause. Returns either an updated assignment or #f (if no
;; assignment exists because we hit a contradiction).
(define (gather-constraints ir-interp assignment tuple body-args) ;; TODO kind of slow
  (foldl (lambda (next-datum next-body-arg assignment)
           (match next-body-arg
             [(? lit? l)
              ;; Then l and next-datum had better be equal
              (if (equal? (lit->tagged-value l ir-interp) next-datum) assignment #f)]
             [(? var? x)
              (cond [(equal? assignment #f) #f]
                    [(not (hash-has-key? assignment x)) (hash-set assignment x next-datum)]
                    [(equal? (hash-ref assignment x) next-datum) assignment]
                    [else #f])]))
         assignment
         tuple
         body-args))

;; Iterate the next SCC in the ir.
(define (iterate-next-scc ir-interp)
  (match-define `(ir-incremental ,ir-scc ,dag ,scc-map ,comp-rules-h) (Ir-interp-ir ir-interp)) 
  (define this-scc (first (Ir-interp-scc-order ir-interp)))
  (match-define `(db-instance ,relation-map ,indices-map ,tag-counts ,intern-map ,added-facts) (Ir-interp-db-instance ir-interp))
  (match-define `(scc ,looping ,rel-arity-map ,rule-map)
    (hash-ref scc-map this-scc))
  (define rules (hash-keys rule-map))

  ;; To form the initial IR (just for the first time through this
  ;; SCC), we copy the indices map from total to detal for every
  ;; relation that is used within this SCC. Otherwise, we end up in
  ;; a situation where a rule in this SCC might use a previous
  ;; database's delta, which will then be empty by the time we
  ;; reach this SCC (as all 'delta versions are emptied as they are
  ;; propagated into 'total).
  (define initial-ir 
    ;; build initial IR by copying tuples from total into delta
    (let ([post-delta-shuffle
          (foldl
            (lambda (rel-version acc)
              (match rel-version
                [`(rel-version ,name ,arity ,select-order ,version)
                (if (equal? version 'delta)
                    acc
                    (let* ([h (hash-ref indices-map rel-version)]
                            [after-copying-this-hash (hash-set acc rel-version h)]
                            [delta-version `(rel-version ,name ,arity ,select-order delta)])
                      (if (equal? version 'total)
                          (hash-set after-copying-this-hash delta-version h)
                          ;; Otherwise, simply leave alone
                          after-copying-this-hash)))]))
            (hash)
            (hash-keys indices-map))])
      (copy-ir-interp ir-interp
      [db-instance `(db-instance ,relation-map ,post-delta-shuffle ,tag-counts ,intern-map ,added-facts)])))
  
  (define (subtract-this-scc ir)
  (copy-ir-interp ir [scc-order (cdr (Ir-interp-scc-order ir))]))
  
  (define (iterate ir)
    ;; Interpret each individual rule in order
    (reset-added-any-new-facts)
    (let* ([next-ir
            (foldl (lambda (next-rule ir)
                    (before-and-after ir (λ (ir) (interpret-rule next-rule ir)) `(rule ,(strip-prov next-rule))))
                  ir
                  rules)])
      ;; Check to see if we have added any new facts during this
      ;; iteration.
      (if added-any-new-facts
          ;; Added some facts, go around again.
          (let* ([next (yield-to-debugger (merge-new next-ir rel-arity-map)
                                          `(finished-scc-iter ,this-scc))])
            (iterate next))
          ;; If there have been no new facts added, we still must
          ;; consider that we might need to propagate
          ;; 'delta->'total, and 'new->'delta.
          (let* ([before-propagating (yield-to-debugger next-ir 'propagating)]
                [ir-after-propagating (yield-to-debugger
                                        (merge-new before-propagating rel-arity-map)
                                        `(finished-scc ,this-scc))])
            (if (equal? next-ir ir-after-propagating)
                ;; No, we didn't need to merge, now we're done
                next-ir
                ;; Merge and iterate
                (iterate ir-after-propagating))))))
  (subtract-this-scc (iterate initial-ir)))

;; loop over each of the sccs in the ir
(define (loop-sccs ir)
  (define (h ir)
    (if (empty? (Ir-interp-scc-order ir))
        ir
        (h (before-and-after ir (λ (ir) (iterate-next-scc ir)) `(scc ,(first (Ir-interp-scc-order ir)))))))
  (h ir))

;; yield to the debugger via a continuation
(define (yield-to-debugger ir-interp info)
  (if (continuation-prompt-available? debug-tag)
      (call-with-composable-continuation
       (λ (k)
         (abort-current-continuation debug-tag
                                     k
                                     ir-interp
                                     info))
       debug-tag)
      ir-interp))

(define (before-and-after before thnk info)
  (yield-to-debugger before        `(before ,info))
  (yield-to-debugger (thnk before) `(after  ,info)))

;; Run the interpreter
(define/contract (slog-interp ir)
  (-> ir-incremental? any/c)
  (match ir
    [`(ir-incremental ,ir-scc ,dag ,scc-map ,comp-rules-h)
     (let* ([initial-state (yield-to-debugger
                            (interning-pass
                             (ir->source-tree ir-scc)
                             (slog-initialize-interpreter ir)
                             scc-map)
                            'initial-state)]
            [final-state
             (yield-to-debugger (loop-sccs initial-state) 'finished-fixed-point)])
       final-state)]))

;; given tuple 'tuple' from relation b0, create an input tuple for relation b1
(define (make-tuple-input tuple b0 b1 ir-interp)
  (match (cons b0 b1)
    [(cons `((rel-version ,b0-rel-name ,b0-arity ,b0-selection ,b0-version) ,b0-args ...)
           `((rel-version ,b1-rel-name ,b1-arity ,b1-selection ,b1-version) ,b1-args ...))
     (map (λ (i) (let ([b1-arg (list-ref b1-args i)])
                   (if (lit? b1-arg)
                       (lit->tagged-value b1-arg ir-interp)
                       (list-ref tuple (index-of b0-args b1-arg)))))
          (range (length b1-selection)))]))


;; TODO performance could be improved here
(define (make-tuple-inputs-for-join-outer-relation b0 ir-interp)
  (match-define `(db-instance ,rel-map ,indices-map ,tag-map ,intern-map ,added-facts) (Ir-interp-db-instance ir-interp))
  (match-define `(ir-incremental ,ir-scc ,dag ,scc-map ,comp-rules-h) (Ir-interp-ir ir-interp))
  (match-define `(,(and rel-version `(rel-version ,b0-rel-name ,b0-arity ,b0-selection ,b0-version)) ,b0-args ...) b0)
  (cond 
    [(comp-or-agg-rel-kind? b0-version)
      ;; if it is a comp rel, the args can only be literals 
      (list (map (app lit->tagged-value _ ir-interp) (takef b0-args lit?)))]
    [else
      (define selection-tuples (hash-keys (hash-ref indices-map rel-version)))
      (define args (take b0-args (length b0-selection)))
      (define selection-tuples-filtered
        (filter (λ (tuple) (andmap (λ (tuple-elem arg) 
                                      (if (lit? arg) (equal? (lit->tagged-value arg ir-interp) tuple-elem) #t))
                                    tuple args))
          selection-tuples))
      selection-tuples-filtered]))

(define (rel-version-facts-count ir-interp rel-version)
  (match-define `(db-instance ,rel-map ,indices-map ,tag-map ,intern-map ,added-facts) (Ir-interp-db-instance ir-interp))
  (match-define `(rel-version ,rel-name ,arity ,selection ,version) rel-version)
  (cond
   [(equal? version 'total)
    (match-define `(rel-instance ,facts-map ,_relation-id ,_next-id) (hash-ref rel-map `(rel-arity ,rel-name ,arity db)))
    (/ (hash-count facts-map) 2)]
   [else
    (define rel-version-map (hash-ref indices-map rel-version))
    (define count 0)
    (hash-for-each rel-version-map 
      (λ (_ v) (set! count (+ count (set-count v)))))
    count]))

(define ((db-val->builtin-arg ir-interp) db-val)
  (format-nested-fact db-val ir-interp)) 

(define (get-func-for-rel-version rel-version ir-interp)
  (match-define `(db-instance ,rel-map ,indices-map ,tag-map ,intern-map ,added-facts) (Ir-interp-db-instance ir-interp))
  (match-define `(ir-incremental ,ir-scc ,dag ,scc-map ,comp-rules-h) (Ir-interp-ir ir-interp))
  (match-define `(rel-version ,b-rel-name ,b-arity ,b-selection ,b-version) rel-version)
  (cond
    [(comp-or-agg-rel-kind? b-version) 
     (define bi-func (match b-version
      ['comp (get-func-for-comp-rel rel-version (Ir-interp-comp-rel-funcs ir-interp))]
      [`(agg ,rel) (get-func-for-aggregator-rel rel-version ir-interp)]))
     (λ (inp)
       (define res-set (apply bi-func (db-val->flat-fact ir-interp) inp))
       (define res-tuples (set-map res-set (λ (res) (append inp '(no-tag) res))))
       (list->set res-tuples))]
    [else
     (define rel-version-map (hash-ref indices-map rel-version))
     (λ (inp)
        (define inp-interned (call/ec (λ (return)
          (map (λ (elem) (match elem
                  [(or `(uninterned ,x) (? string? x)) (match (hash-ref intern-map x #f) [#f (return inp)] [interned interned])]
                  [else elem]))
               inp))))
        (hash-ref rel-version-map inp-interned set ;;TODO this does not look like an error
                 #;(λ () (error 'get-func-for-rel-version "no value for key: ~a \nin rel-version-map for rel: ~a" inp rel-version))))]))

;; Calculate a map from relation IDs to rel-select instances
(define (get-id->rel-arity rm)
       (foldl
        (lambda (rel-arity h)
          (match-define `(rel-instance ,_ ,rel-num ,num-facts) (hash-ref rm rel-arity))
          (hash-set h rel-num rel-arity))
        (hash)
        (hash-keys rm)))

(define (format-nested-fact id ir-interp)
  (match-define `(db-instance ,rel-map ,indices-map ,tag-map ,intern-map ,added-facts) (Ir-interp-db-instance ir-interp))
  (define id->rel-arity (get-id->rel-arity rel-map))
  (define fmt (format-config +inf.0 +inf.0 +inf.0))
  (car (pretty-format-nested-fact id intern-map rel-map id->rel-arity fmt)))

(define (db-val->flat-fact ir-interp)
  (match-define `(db-instance ,rel-map ,indices-map ,tag-map ,intern-map ,added-facts) (Ir-interp-db-instance ir-interp))
  (define id->rel-arity (get-id->rel-arity rel-map))
  (λ (id) (match id
    [`(uninterned ,x) x]
    [(? string? str) str]
    [else (peel-back-nested-fact-one-layer id intern-map rel-map id->rel-arity)])))

(define (intern-objects-given-tuples tuples ir-interp)
  (match-define (cons interned-tuples new-db-instance)
    (foldl (λ (tuple tuples+db-instance)
              (match-define (cons tuples db-instance) tuples+db-instance)
              (match-define (cons interned-tuple new-db-instance) (intern-tuple-objects tuple db-instance))
              (cons (append tuples (list interned-tuple)) new-db-instance))
           (cons '() (Ir-interp-db-instance ir-interp))
           tuples))
  (define new-ir-interp (copy-ir-interp ir-interp [db-instance new-db-instance]))
  (cons interned-tuples new-ir-interp))

;; Given a tuple with potentially uninterned elements, and the db-instance, intern the uninterned elements
;; and return a cons of the updated tuple and db-instance
(define (intern-tuple-objects tuple db-instance)
  (foldl (λ (tuple-elem tuple+db-instance)
            (match-define (cons tuple db-instance) tuple+db-instance)
            (match tuple-elem
            [(or `(uninterned ,str) (? string? str))
              (define-values (id new-db-instance) (extend-intern-map str 'string db-instance))
              (cons (append tuple (list id)) new-db-instance)]
            [else (cons (append tuple (list tuple-elem)) db-instance)]))
         (cons '() db-instance)
         tuple))

(define (ir-interp-facts-count ir-interp)
  (match-define `(db-instance ,rm ,indices-map ,tag-map ,intern-map ,added-facts) (Ir-interp-db-instance ir-interp))

  (define num-facts
       (foldl (lambda (key num) (+ num
                                   (match (hash-ref rm key)
                                     [`(rel-instance ,h ,id ,num) (/ (length (hash-keys h)) 2)])))
              0
              (hash-keys rm)))
  num-facts)

;; returns the function for the given comp-rel, whether it's a builtin or user-defined.
(define (get-func-for-comp-rel comp-rel comp-rel-funcs-hash)
  (define comp-rel-version
    (match comp-rel
      [`(rel-select ,rel-name ,rel-arity ,rel-indices comp) `(rel-version ,rel-name ,rel-arity ,rel-indices comp)]
      [`(rel-version ,_ ...) comp-rel]))
  (match-define `(rel-version ,rel-name ,rel-arity ,rel-indices comp) comp-rel-version)
  (cond
    [(builtin? rel-name)
      (get-func-for-builtin-with-extended-indices comp-rel-version)]
    [else
      (define comp-rel-select
        (match comp-rel
          [`(rel-select ,_ ...) comp-rel]
          [else `(rel-select ,rel-name ,rel-arity ,rel-indices comp)]))
      (hash-ref comp-rel-funcs-hash comp-rel-select)]))

(define (get-func-for-aggregator-rel agg-rel ir-interp)
  (match-define `(rel-version ,agg-rel-name ,agg-rel-arity ,agg-rel-indices (agg ,rel)) agg-rel)
  (match-define `(rel-select ,rel-name ,rel-arity ,rel-indices db) (strip-prov rel))
  (match-define `(db-instance ,rm ,indices-map ,tag-map ,intern-map ,added-facts) (Ir-interp-db-instance ir-interp))
  (define rel-version `(rel-version ,rel-name ,rel-arity ,rel-indices total))
  (define rel-version-map (hash-ref indices-map rel-version))
  (define agg-spec `(aggregator-spec ,agg-rel-name ,agg-rel-arity ,agg-rel-indices ,rel-arity ,rel-indices))
  (define agg-func (get-func-for-aggregator-with-extended-indices agg-spec))
  (agg-func rel-version-map))

(define (create-func-for-comp-rule comp-rule comp-rel-funcs-hash)
  (match-define `(crule ,head ,bodys ...) (strip-prov comp-rule))
  (match-define `((rel-select ,hrel-name ,hrel-arity ,hrel-indices comp) ,hargs ...) head)
  (define (tag lit)
    (match lit
      [(? integer?) `(integer ,lit)]
      [(? string?) lit]
      [(? tval?) lit]))

  (define h-input-params (take hargs (length hrel-indices)))
  (define h-input-params-has-literals (ormap (not/c var?) h-input-params))
  (λ (materializer . input-args)
    (define initial-args-hash
      (make-immutable-hash (map cons h-input-params input-args)))
    (define h-input-params-literals-match
      (if h-input-params-has-literals
        (andmap (λ (param value) 
                  ; (printf "param: ~a, value: ~a\n" param value)
                  (or (var? param) (bi-args-equal? materializer (tag param) value))) 
                h-input-params input-args)
        #t))
    (cond
      [h-input-params-literals-match
        (define final-args-hashes
          (foldl (λ (cl args-hashes)
                    (match-define `((rel-select ,clrel ,clrel-arity ,clrel-indices comp) ,clargs ...) cl)
                    (define cl-func (get-func-for-comp-rel `(rel-select ,clrel ,clrel-arity ,clrel-indices comp) comp-rel-funcs-hash))
                    (flat-map 
                      (λ (args-hash) 
                          (define cl-func-input (map (λ (param) 
                                                      (if (var? param) 
                                                          (hash-ref args-hash param)
                                                          (tag param))) 
                                                    (take clargs (length clrel-indices))))
                          (define cl-func-output-list (set->list (apply cl-func materializer cl-func-input)))
                          (filter-map (λ (cl-func-output)
                                  (call/ec (λ (return)
                                    (foldl (λ (param value args-hash) 
                                              (define current-val (hash-ref args-hash param 'no-val))
                                              (cond 
                                                [(equal? current-val 'no-val) (hash-set args-hash param value)]
                                                [else 
                                                  (if (bi-args-equal? materializer current-val value)
                                                      args-hash
                                                      (return #f))]))
                                          args-hash
                                          (drop clargs (add1 (length clrel-indices)))
                                          cl-func-output))))
                              cl-func-output-list))
                      args-hashes)
                    )
                (list initial-args-hash)
                bodys))
        (list->set
          (map (λ (final-args-hash)
                  (map (λ (arg) (if (var? arg) (hash-ref final-args-hash arg) (tag arg)))
                      (drop hargs (add1 (length hrel-indices)))))
              final-args-hashes))]
      [else (set)])))

(define (create-func-for-comp-rel comp-rel comp-rules comp-rel-funcs-hash)
  (define comp-rel-version
    (match comp-rel
      [`(rel-select ,rel-name ,rel-arity ,rel-indices comp) `(rel-version ,rel-name ,rel-arity ,rel-indices comp)]
      [`(rel-version ,_ ...) comp-rel]))
  (match-define `(rel-version ,rel-name ,rel-arity ,rel-indices comp) comp-rel-version)
  (cond
    [(builtin? rel-name)
      (get-func-for-builtin-with-extended-indices comp-rel-version)]
    [else
      (define relevant-rules
        (filter (λ (rule)
                  (match-define `(crule ,head ,bodys ...) (strip-prov rule))
                  (match-define `((rel-select ,n ,ar,ind ,kind) ,args ...) head)
                  (and (equal? n rel-name) (equal? ar rel-arity) (equal? ind rel-indices)))
                comp-rules))
      (define relevant-rules-funcs (map (app create-func-for-comp-rule _ comp-rel-funcs-hash) relevant-rules))
      (λ (materializer . args)
        (apply set-union (set) (map (λ (func) (apply func materializer args))
                                    relevant-rules-funcs)))]))
                                  
(define (create-comp-rel-funcs-given-rules comp-rules)
  (define comp-rels
    (remove-duplicates
      (map (λ (comp-rule)
            (match-define `(crule (,hrel-select ,hargs ...) ,bodys ...) (strip-prov comp-rule))
            hrel-select)
           comp-rules)))
  (define comp-rel-funcs-hash (make-hash))
  (for ([comp-rel comp-rels])
    (define func (create-func-for-comp-rel comp-rel comp-rules comp-rel-funcs-hash))
    (hash-set! comp-rel-funcs-hash comp-rel func))
  comp-rel-funcs-hash)

(module+ test
  (require rackunit)
  (test-case "test-make-func-for-comp-rule"
    (define comp-rule
      '(crule
        ((rel-select __comp-rel5 3 (3 2) comp) z y _ x)
        ((rel-select + 3 (2 3) comp) y z _ x)
        ((rel-select > 2 (1 2) comp) x 1 _)))
    (define func (create-func-for-comp-rule comp-rule (list)))
    func))
