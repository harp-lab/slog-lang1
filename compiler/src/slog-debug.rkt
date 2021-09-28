;; Slog-lang Debugger and REPL
;; Copyright (c) Kristopher Micinski, Thomas Gilray, Sidharth Kumar, see License.md
#lang racket

;; Readline gives us up/down arrows properly, tab-complete, etc...
(require "readline-wrapper.rkt")

;; If profiling
(require profile)

(require
 "lang-predicates.rkt"
 "interpreter.rkt"
 "slog-params.rkt"
 "graphs.rkt"
 "utils.rkt"
 "builtins.rkt")

(provide slog-debug
         query-ir-interp)

;; Builtin commands
(define commands (set "overview" "overview-sorted" "display-db" "quit" "continue" "break" "goto" "help" "indices-map" "list" "break" "save-indices-map" "diff-indices-map" "list-breakpoints" "prov" "compiled-rules" "source-rules"))

;; Boot up readline and attach it to stdin
#;(install-readline!)

;; 
;; Timer
;;
(define curr-time (current-inexact-milliseconds))
(define (reset-timer)
  (set! curr-time (current-inexact-milliseconds)))
(define (read-timer)
  (- (current-inexact-milliseconds) curr-time))

;; Print a banner
(define (ascii-art)
  (for-each
   display
   `(" ______     __         ______     ______    \n"
     "/\\  ___\\   /\\ \\       /\\  __ \\   /\\  ___\\   \n"
     "\\ \\___  \\  \\ \\ \\____  \\ \\ \\/\\ \\  \\ \\ \\__ \\  \n"
     " \\/\\_____\\  \\ \\_____\\  \\ \\_____\\  \\ \\_____\\ \n"
     ,(format "  \\/_____/   \\/_____/   \\/_____/   \\/_____/    Version ~a.~a.~a\n"
              slog-version-major
              slog-version-minor
              slog-version-revision)
     "\n"))
  (display "Slog (symbolic-expression logic) engine and REPL\n")
  (display (format "Copyright (c) 2018-~a ~a. ~a\n" (date-year (seconds->date (current-seconds))) slog-copyright-authors slog-copyright-warning))
  (newline))

;; Complain at the user
(define (complain s)
  (switch-to-err-bg)
  (displayln s)
  (switch-from-err-bg))

;;
;; Begin Mutable State
;; 

;; Saving intermediate states / facts
(define num-processed-sccs 0)
(define num-processed-iterations 0)
(define num-iterations 0)
(define curr-scc '())
(define curr-scc-iteration 0)

;;
;; End Mutable State
;; 

;; calculates the base directory where the dumped output will be written
(define (dump-output-directory event)
  (match event
    ['initial-state "initial-facts"]
    [`(before (scc ,n)) #:when (slog-dump-iters) ;; n is the number of the SCC, not the total iteration number
                        (set! curr-scc n)
                        (set! curr-scc-iteration 0)
                        (format "scc-~a-initial-facts" curr-scc)]
    [`(after (scc ,n)) #:when (slog-dump-iters) ;; n is the number of the SCC, not the total iteration number
                       (format "scc-~a-output-facts" curr-scc)]
    ['finished-fixed-point "output-facts"]
    [`(finished-scc-iter ,n) #:when (slog-dump-iters) ;; n is the number of the SCC, not the total iteration number
                             (define c curr-scc-iteration)
                             (set! curr-scc-iteration (add1 curr-scc-iteration))
                             (format "scc-~a-iteration_~a" curr-scc c)]
    [else #f]))

;; Possibly write out a set of output facts to the disk.
;; currently turned off, as we now output no u64-encoded files for facts
(define (possibly-output-facts event ir-interp)
  (void))
  #;(pretty-print event)
  ;; (define base-output-directory (dump-output-directory event))
  ;; (define output-directory
  ;;   (if base-output-directory (string-append "output/" base-output-directory) #f))
  ;; (match-define `(ir-incremental ,ir-scc ,dag ,scc-map ,comp-rules-h) (Ir-interp-ir ir-interp))
  ;; (when output-directory
  ;;   (define relevant-relations
  ;;     (match event
  ;;       ['initial-state 'all]
  ;;       ['finished-fixed-point 'all]
  ;;       [_ 
  ;;        (let* ()
  ;;          (match-define `(scc ,looping ,rel-arity-map ,rule-map) (hash-ref scc-map curr-scc))
  ;;          (foldl (lambda (rel-arity-key relevant-relations)
  ;;                   (match-define `(rel-arity ,rel ,arity ,kind) rel-arity-key)
  ;;                   (match (hash-ref rel-arity-map rel-arity-key)
  ;;                     [`(dynamic ,canonical-index ,all-selects)
  ;;                      (foldl (lambda (select relevant-relations) (set-add relevant-relations `(rel-select ,rel ,arity ,select)))
  ;;                             relevant-relations
  ;;                             (set->list all-selects))]
  ;;                     [_ relevant-relations]))
  ;;                 (set)
  ;;                 (hash-keys rel-arity-map)))]))
  #;(serialize-relations-to-directory output-directory ir-interp #t relevant-relations)

;; 
;; Debugging handler
;;

(define say (lambda (fstr . fargs) (displayln (apply format `(,fstr ,@fargs)))))

;; does a breakpoint match a debugging event
(define ((matches-bp? event) breakpoint)
  (define (match-item bitem bexpr assignment)
    (match (cons bitem bexpr)
      [(cons '_ _) (cons #t assignment)]
      [(cons (? symbol? x) lit) (if (hash-has-key? assignment x)
                                    (cons (equal? (hash-ref assignment x) lit) assignment)
                                    (cons #t (hash-set assignment x lit)))]
      [(cons lit0 lit1) (equal? lit0 lit1)]))
  (define (match-list break-exprs exprs assignment)
    (match (cons break-exprs exprs)
      [`(() . ()) #t]
      [`((,bhd . ,btl) . (,ehd . ,etl))
       (match-define (cons tf updated-asn) (match-item bhd ehd assignment))
       (and tf
            (match-list btl etl updated-asn))]
      [_ #f]))
  (match-list breakpoint event (hash)))

;; Calculate a source rule for a given compiled rule, given the rule provenance
(define (calculate-source-rule rule-provenance compiled-rule)
  (letrec ([f (lambda (x)
                (if (hash-has-key? rule-provenance x)
                    (f (set-first (hash-ref rule-provenance x)))
                    x))])
    (f compiled-rule)))

;; Handle a debugging event. This is the toplevel driver for the entry
;; hooks into the debugger. Various events in the interpreter 
(define ((handle-debugging-event ir) continuation ir-interp event)
  (define debug-ir (if (null? ir)
                       (make-initial-debug-ir ir-interp continuation)
                       ir))
  
  (match-define `(ir-debug (timestamp-state ,tsgraph ,tsmap ,root ,max-timestamp ,focus ,timestamp<->symbol)
                           (metadata ,rule-provenance ,compiled-rule<->id ,source-rule<->id)
                           ,breakpoints
                           ,current-state) debug-ir)
  
  ;; Uses the debugging state to decide when we should break
  (define (break? event)
    (if (slog-batch-mode)
        #f
        (or
         (ormap (λ (bp) (match event
                          [`(before (rule (srule ((rel-select ,h ,_ ,_) ,h-args ...) . ,_))) #:when (equal? h bp) #t]
                          [`(before (rule (arule ((rel-select ,h ,_ ,_) ,h-args ...) . ,_))) #:when (equal? h bp) #t]
                          [_ #f])) (set->list breakpoints))
         (ormap (matches-bp? event) (set->list breakpoints))
         (match event
           ['initial-state (slog-debug-mode)]
           ['finished-fixed-point #t]
           [else #f]))))

  ;; Write facts to the disk
  (possibly-output-facts event ir-interp)
  
  ;; Handle the debugging event
  (cond 
    [(equal? event 'switched-times)
     ;; Back after switching times
     (say (format "Now back at timestamp: ~a." focus))
     (repl ir-interp debug-ir continuation)]
    [(break? event)
     ;; Break
     (pretty-print event)
     (say (format "Operation took: ~a millseconds" (~r (read-timer) #:precision 0)))
     (match-let*
         ([new-node (add1 max-timestamp)]
          [new-tsgraph (hash-set (hash-set tsgraph focus (set-add (hash-ref tsgraph focus)
                                                                  `(,event ,new-node)))
                                 new-node (set))]
          [new-tsmap (hash-set tsmap new-node `(,ir-interp ,continuation))]
          [new-debug-ir
           `(ir-debug (timestamp-state ,new-tsgraph ,new-tsmap ,root ,(add1 max-timestamp)
                                       ,focus ,timestamp<->symbol)
                      (metadata ,rule-provenance ,compiled-rule<->id ,source-rule<->id)
                      ,breakpoints
                      ,current-state)])
       (displayln (format "Created new timestamp: ~a, child of timestamp: ~a" new-node focus))
       ;; Now run the REPL with the new debug IR.
       (repl ir-interp new-debug-ir continuation))]
    [else
     (continue (thunk (continuation ir-interp))
                   debug-ir
                   (format "Continuing..."))]))

;; Make the initial debugging IR
(define (make-initial-debug-ir ir-interp continuation)
  (match-define `(db-instance ,_ ,im ,_ ,_ ,_) (Ir-interp-db-instance ir-interp))
  (match-define `(ir-incremental ,ir-scc ,dag ,sccs ,comp-rules-h) (Ir-interp-ir ir-interp))
  ;; Calculate the provenance graph for each of the rules
  (define rule-provenance
    (foldl (lambda (n h)
             (match (hash-ref sccs n)
               [`(scc ,_ ,_ ,rules-prov)
                (foldl (lambda (ir-incremental-rule h)
                         (extend-provenance-graph h (Ir-interp-ir ir-interp) ir-incremental-rule))
                       h
                       (hash-keys rules-prov))]))
           (hash)
           (hash-keys sccs)))

  (define sccs-rules (foldl (lambda (n h) (match (hash-ref sccs n)
                                            [`(scc ,_ ,_ ,rules-prov)
                                             (hash-set h n (set-union (hash-ref h n (set))
                                                                      (list->set (hash-keys rules-prov))))]))
                            (hash)
                            (hash-keys sccs)))
  
  ;; Name each of the compiled rules 'crN
  (define compiled-rules
    (let ([x 0])
      (foldl (lambda (n h)
               (hash-set h n (foldl (lambda (rule h)
                                      (set! x (add1 x))
                                      (hash-set h (format "cr~a" x) rule))
                                    (hash)
                                    (set->list (hash-ref sccs-rules n)))))
             (hash)
             (hash-keys sccs-rules))))

  (define source-rules
    (foldl (lambda (n h)
             (hash-set h n (foldl (lambda (rule-id h)
                                    (define source-rule (calculate-source-rule
                                                         rule-provenance
                                                         (hash-ref (hash-ref compiled-rules n) rule-id)))
                                    (hash-set h (format "sr~a" (substring rule-id 2)) source-rule))
                                  (hash)
                                  (hash-keys (hash-ref compiled-rules n)))))
           (hash)
           (hash-keys compiled-rules)))


  #;
  (define roots (filter (lambda (node)
                          (andmap (lambda (othernode)
                                    (not (equal? node (hash-ref rule-provenance othernode))))
                                  (hash-keys rule-provenance)))
                        (hash-keys rule-provenance)))
  `(ir-debug
    (timestamp-state
     ;; Timestamp graph: maps timestamps to predecessor timestamps
     ,(hash 0 (set))
     ;; Timestamp map: maps timestamps to their contents
     ,(hash 0 `(,ir-interp ,continuation))
     ;; Root timestamp
     0
     ;; Max timestamp
     0
     ;; Focus timestamp
     0
     ;; Timestamp names
     ,(hash 0 'start 'start 0))
    ;; Debugging metadata
    (metadata ,rule-provenance ,source-rules ,compiled-rules)
    ;; Current breakpoints
    ,(set)
    ;; Current debugging state
    before-interpretation))

;;
;; REPL
;;

;; Helper function
(define (continue thnk ir-debug tag)
  #;(reset-timer)
  (call-with-continuation-prompt
   thnk
   debug-tag
   ;; Handler for the abort after establishing the initial state
   (handle-debugging-event ir-debug)))

;; Initialize and run the REPL
(define (repl ir-interp ir-debug continuation)
  (match-define `(ir-debug (timestamp-state ,tsgraph ,tsmap ,root ,max-timestamp ,focus ,timestamp<->symbol)
                           (metadata ,rule-provenance ,id->source-rule ,id->compiled-rule)
                           ,breakpoints
                           ,current-state)
    ir-debug)
  (match (Ir-interp-db-instance ir-interp)
    [`(db-instance ,rm ,im ,tc ,ssidmap ,added-facts)
     (say (format "Current logical timestamp: ~a" focus))
     (say (format "--- Current database has ~a facts. ---" (ir-interp-facts-count ir-interp)))
     
     ;; Autocompletion stuff
     (define fact-names
       (list->set (hash-map (Ir-interp-rel-arity-map  ir-interp) (match-lambda** [(`(rel-arity ,rel-name ,_ ,_) value) rel-name]))))
     (define completion-set (set-union commands fact-names))
     
     ;; Get the set of arities corresponding to a relation.
     (define (get-arities rel-name)
       (list->set (flatten (map
                            (match-lambda [`(rel-arity ,name ,arity ,kind)
                                           (if (equal? name rel-name)
                                               (list arity) '())]
                                          [else '()])
                            (hash-keys (Ir-interp-rel-arity-map ir-interp))))))

     ;; Calculate a map from relation IDs to rel-select
     ;; instances. This is useful for looking up IDs later during fact
     ;; printing.
     (define id->rel-arity (get-id->rel-arity rm))

     ;; Completion function for readline. Takes a prefix of a string and a
     ;; set and returns the set of strings that could be possible
     ;; completions.
     ;; Output: a list of possible suffixes of str within completion-set
     (define ((completion-function completion-set) str)
       (append
        (filter (lambda (x) (and (string-prefix? x str) (not (equal? x str))))
                (map symbol->string (set->list fact-names)))
        (let ([matching-keys (filter
                              (lambda (possible-completion)
                                (equal? (symbol->string possible-completion) str))
                              (set->list fact-names))])
          (flatten 
           (for/list ([key matching-keys])
             (set->list
              (for/set ([arity (get-arities key)])
                (string-append (symbol->string key)
                               " "
                               (string-join (build-list arity (lambda (x) "_")) " ")
                               ")"))))))
        (filter
         (lambda (possible-completion)
           (string-prefix? possible-completion str))
         (set->list commands))))
     
     ;; Set the completion function
     (set-completion-function! (completion-function completion-set))

     (define (loop ir-debug)
       (match-define
         `(ir-debug (timestamp-state ,tsgraph ,tsmap ,root ,max-timestamp ,focus ,timestamp<->symbol)
                    (metadata ,rule-prov ,source-rules ,compiled-rules)
                    ,breakpoints
                    ,current-state) ir-debug)
       (define timestamp-state `(timestamp-state ,tsgraph ,tsmap ,root ,max-timestamp ,focus ,timestamp<->symbol))
       (define input (readline "σλoγ > "))
       (if (equal? eof input) (exit 0) (void))
       ;; For now, we just parse this as an S-expression
       (define cmd (with-handlers ([exn:fail? (λ (e) '(bad-input))]) 
                     (read (open-input-string input))))
       (match cmd
         ['(bad-input) (say "Bad input!")]
         [(? (lambda (x) (member x '(continue c))))
          (say ">>> Continuing execution...")
          (match-let ([`(,ir-interp ,continuation) (hash-ref tsmap focus)])
            (continue (thunk (continuation ir-interp))
                      `(ir-debug ,timestamp-state (metadata ,rule-prov ,id->source-rule ,id->compiled-rule) ,breakpoints ,current-state)
                      (format "Continuing execution from state ~a" focus)))]
         
         ['(compiled-rules)
          (displayln "Compiled rules (view provenance via `(prov crN)`)")
          (for ([scc-n (hash-keys compiled-rules)])
            (say "SCC ~a:" scc-n)
            (for ([rule-id (hash-keys (hash-ref compiled-rules scc-n))])
              (displayln (format "~a" rule-id))
              (pretty-print (strip-prov (hash-ref (hash-ref compiled-rules scc-n) rule-id))))
            (display "\n"))]
         
         ['(source-rules)
          (displayln "Source rules:")
          (for ([scc-n (hash-keys source-rules)])
            (say "SCC ~a:" scc-n)
            (for ([rule-id (hash-keys (hash-ref source-rules scc-n))])
              (displayln (format "~a" rule-id))
              (let* ([source-rule (hash-ref (hash-ref source-rules scc-n) rule-id)]
                     [source-rule-formatted (string-split (pretty-format (strip-prov source-rule)) "\n")])
                (define (before-line-thunk s)
                  (match s
                    ['before (display "  ")]
                    ['single-line (display "-->")]
                    ['first-line (display "-->")]
                    [_ (display "   ")]))
                (match source-rule
                  [`(prov ,r ,pos)
                   (pretty-highlight (ir->source-tree ir-interp) pos (thunk (void))
                                     (thunk (switch-to-info-bg))
                                     (thunk (switch-from-info-bg)) before-line-thunk 1)]
                  [_ (displayln "Could not render source rule")])))
            (display "\n"))]

         ;; Display the full provenance graph
         [`(full-prov-graph)
          (letrec ([nodes (foldl (lambda (k h) (match k
                                                 [`(edge . ,_) h]
                                                 [_ (hash-set h k (hash-ref rule-prov k))]))
                                 (hash)
                                 (hash-keys rule-prov))]
                   [prov-removed
                    (foldl (lambda (k h)
                             (hash-set h (strip-prov k) (for/set ([node (hash-ref rule-prov k)])
                                                          (strip-prov node))))
                           (hash)
                           (hash-keys nodes))])
            (define out (open-output-file "out.dot" #:exists 'replace))
            (graph->dot prov-removed out)
            (say "Written to ./out.dot")
            (close-output-port out))]

         ;; Calculate the provenance for a compiled rule
         [`(prov ,rule-id)
          (define (lookup-rule srule-id)
            (define rule-id (symbol->string srule-id))
            (define id->sccnum
              (foldl (lambda (n h) (foldl (lambda (id h) (hash-set h id n))
                                          h
                                          (hash-keys (hash-ref compiled-rules n))))
                     (hash)
                     (hash-keys compiled-rules)))
            (if (hash-has-key? id->sccnum rule-id)
                (hash-ref (hash-ref compiled-rules (hash-ref id->sccnum rule-id)) rule-id)
                null))
          (define rule (lookup-rule rule-id))
          (if (null? rule)
              (say "No compiled rule named ~a" rule-id)
              (let ([source-rule (calculate-source-rule rule-prov rule)]
                    [compiled-rule-formatted (string-split (pretty-format (strip-prov rule)) "\n")])
                (say "+-~a" (first compiled-rule-formatted))
                (for ([i (range 1 (length compiled-rule-formatted))])
                  (say "| ~a" (list-ref compiled-rule-formatted i)))
                (say "|")
                (define (before-line-thunk s)
                  (match s
                    ['before (display "|  ")]
                    ['single-line (display "+->")]
                    ['first-line (display "+->")]
                    [_ (display "   ")]))
                (match source-rule
                  [`(prov ,r ,pos)
                   (pretty-highlight (ir->source-tree ir-interp) pos (thunk (void))
                                     (thunk (switch-to-info-bg))
                                     (thunk (switch-from-info-bg)) before-line-thunk 1)]
                  [_ (displayln "Could not reverse arule")])))]
         
         ;; Step
         [(? (lambda (x) (member x '(step s))))
          ;; Look up the current continuation to resume the interpreter
          (say ">>> Stepping execution...")
          (match-let ([`(,ir-interp ,continuation) (hash-ref tsmap focus)])
            (continue (thunk (continuation ir-interp))
                      ir-debug
                      (format "Continuing execution from state ~a" focus)))]
         [`(goto ,timestamp)
          (let ([ts (hash-ref timestamp<->symbol timestamp timestamp)])
            ;; Does this timestamp exist?
            (if (hash-has-key? tsmap ts)
                ;; Yes, timetravel there!
                (match-let ([`(,t-ir-interp ,t-continuation) (hash-ref tsmap ts)])
                  ((handle-debugging-event ir-debug)
                   t-continuation
                   t-ir-interp
                   'switched-times))
                ;; Complain to the user
                (complain (format "No such timestamp: ~a" ts))))]
         ['quit (exit 0)]
         ;; Print the current control point
         ['list (void)
          #;
          (let ([highlightpos (match pos ['init-state `(pos 'foo ,(cons 0 0) ,(cons 0 0))])])
            (pretty-highlight 
             ir-interp
             highlightpos
             (thunk (displayln "Currently at line n, k\n"))
             (thunk (write "🛑"))
             (lambda () (void))
             10))]
         ;; Print a topological history of the graph nodes.
         ['history 
          (pretty-print tsgraph)]
         [(or 'overview 'overview-sorted)
          (say "Relations and associated arities:\n")
          (let ([relations-and-arities
                 (foldl
                  (match-lambda** [(`(rel-arity ,rel ,arity ,kind) h)
                                   (hash-set h
                                             rel
                                             (cons (set-add (car (hash-ref h rel (cons (set) 0))) arity)
                                                   (+ (cdr (hash-ref h rel (cons (set) 0)))
                                                      (/ (length (hash-keys
                                                                  (match (hash-ref rm `(rel-arity ,rel ,arity ,kind))
                                                                    [`(rel-instance ,h ,num ,ids) h]))) 2))))])
                  (hash)
                  (hash-keys rm))])
            (define relations-and-arities-sorted
              (if (equal? cmd 'overview-sorted)
                (sort (hash->list relations-and-arities) > #:key (λ (rarr) (cdr (cdr rarr)))) ;;sort by size of relations
                (sort (hash->list relations-and-arities) string<? #:key (λ (rarr) (symbol->string (car rarr)))))) ;;sort by relation name
            (for ([relation (map car relations-and-arities-sorted)])
              (say
               (format
                "~a: {~a}. (~a total facts)"
                relation
                (string-join
                 (map number->string
                      (set->list (car (hash-ref relations-and-arities relation))))
                 ", ")
                (cdr (hash-ref relations-and-arities relation))))))]
         ['ir-interp
          (pretty-print ir-interp)]
         ['display-db
          (pretty-print `(db-instance ,rm ,im ,tc ,ssidmap))]
         ['indices-map (pretty-print im)]
         [`list-breakpoints
          (if (> (length breakpoints) 0)
              (begin (say "Current breakpoints:")
                     (for ([bp breakpoints]
                           [i  (range 0 (length breakpoints))])
                       (say (format "~a. ~a" i (pretty-format bp)))))
              (say "No current breakpoints."))]
         [`(break ,break-expression)
          (if #t #;(breakpoint? break-expression)
              (let* ([new-bps (cons break-expression breakpoints)]
                     [new-ir `(ir-debug )])
                (loop new-ir))
              (complain (format "Invalid breakpoint expression ~a" (pretty-format break-expression))))]
         [`(,rel . ,args)
          ;; Query. This is inefficient, and only allows top-level
          ;; queries.
          (with-handlers ([any/c (λ (e) (say (format "error: ~a" e)))])
            (let ([sorted-print-sets (query-ir-interp-pretty ir-interp `(,rel . ,args))])
              ;; For each tuple
              (for ([print-set sorted-print-sets])
                (let* ([print-set (map prettify-lists-in-fact print-set)]
                       [main (car print-set)]
                       [rst (cdr print-set)])
                  (pretty-write main)
                  ;; For each variable assignment, print it out slightly-tabbed
                  (for ([subvar-clause rst])
                    (display "  ")
                    (pretty-write subvar-clause))))))]
         [else (void)])
       (loop ir-debug))
     ;; Actually kick the loop off
     (loop ir-debug)]))

;; returns a set of tuples of the form (cons main-fact variable-assignments)
;; The intention is to allow more sophisticated query forms here
(define (query-ir-interp-pretty ir-interp query [config 'from-params])
  (match-define `(db-instance ,rm ,im ,tc ,ssidmap ,added-facts) (Ir-interp-db-instance ir-interp))
  (match-define `(,rel . ,args) query)
  (define id->rel-arity (get-id->rel-arity rm))
  (define rel-arity `(rel-arity ,rel ,(length args) db))
  (unless (hash-has-key? rm rel-arity) (raise 'no-such-rel-arity))
  (match-let* ([`(rel-instance ,h ,tag ,next-id) (hash-ref rm rel-arity)]
               [rel-keys (hash-keys h)]
               [ids (filter (match-lambda [`(tag ,rel ,bkt ,id) #t] [else #f]) rel-keys)]
               [print-sets (map (lambda (id) (pretty-format-nested-fact id ssidmap rm id->rel-arity config)) ids)])
    (sort print-sets (lambda (l0 l1) (< (length l0) (length l1))))))

;; returns a list of sexprs corresponding to the query
(define (query-ir-interp ir-interp query)
  (define fmt (format-config +inf.0 +inf.0 +inf.0))
  (define ans (query-ir-interp-pretty ir-interp query fmt))
  (map car ans))

(define (prettify-lists-in-fact fact)
  (define ($lst? fact)
    (match fact
      [`($lst ,h ,t) 
        (define t-list ($lst? t))
        (cond
          [t-list (cons (prettify-lists-in-fact h) t-list)]
          [(symbol? t) (list (prettify-lists-in-fact h) t '... )]
          [else #f])]
      [`($nil) '()]
      [else #f]))
  (define fact-list ($lst? fact))
  (match fact
    [_ #:when fact-list `(LIST ,@fact-list)]
    [`(,tag ,args ...) `(,tag ,@(map prettify-lists-in-fact args))]
    [else fact]))

;; Initiate the debugger
(define (slog-debug ir)
  ;; Call slog-interp with an initial IR. It will abort to a prompt to
  ;; yield the initial state and a continuation to start the program.
  (call-with-continuation-prompt
    (lambda ()
      (reset-timer)
      (slog-interp ir)
      (displayln "Exiting.")
      #;
      (parameterize ([uncaught-exception-handler
                      ;; Set the toplevel exception handler to throw out
                      ;; to the REPL. So that we don't, e.g., lock up the
                      ;; keyboard and crash the app.
                      (lambda (exn)
                        (abort-current-continuation
                         debug-tag
                         null
                         null
                         'evaluation-error))])))
    debug-tag
    ;; Handler for the abort after establishing the initial state
    (handle-debugging-event null)))
    
