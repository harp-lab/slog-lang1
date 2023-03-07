;;Slog-lang parser
;; Copyright (c) Kristopher Micinski, et al, see License.md
#lang racket

(require "slog-params.rkt"
         "utils.rkt"
         "lang-predicates.rkt"
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/lex
         parser-tools/yacc)
(provide parse-slog-file
         parse-slog-files
         parse-slog-module)

;;
;; Slog lexer
;; 

;; Defining token classes

;; Non empty tokens
(define-tokens toks
  (integer string id rel larr rarr dash-dash-token eq neq qlp notlp qp qb bp bdop qdop bb dots tor tand comp colon))

;; Empty tokens
(define-empty-tokens empty-toks (lb rb lp rp lc rc EOF))

;; Define some lexer macros and translations

(define-lex-trans uinteger
  (syntax-rules ()
    ((_ digit) (re-+ digit))))

(define-lex-trans number
  (syntax-rules ()
    ((_ digit)
     (re-: (re-? (re-or "-" "+")) (uinteger digit)))))

(define-lex-abbrevs
  (digit10 (char-range "0" "9"))
  (comment (re-: ";" (complement (re-: any-string "\n" any-string)) (re-? "\n")))
  (strlit  (re-: "\"" (re-* (re-or (re-: "\\" any-char) (char-complement "\""))) "\""))
  (num (number digit10))
  (identifier-characters
   (re-or (char-range "a" "z") (char-range "A" "Z") (char-range "α" "ω") (char-range "Α" "Ω") digit10 
          "_" "-" "+" "/" "*" "." "@" "$" "%" "^" "~" "&" "|" "<" ">" "=" "?" "!" "'" ":"
           "⊥" "⊤" "⋄")) 
  (identifier (re-+ identifier-characters)))

;; Lexer error handling
(define (format-position p)
  (format "(~a,~a)" (position-line p) (position-col p)))

(define (lex-error lines filename lexeme start-pos end-pos)
  (pretty-error lines
                `(pos ,filename
                      ,(cons (position-line start-pos) (position-col start-pos))
                      ,(cons (position-line end-pos) (position-col end-pos)))
                (format "Unrecognized sequence '~a' between ~a and ~a"
                        lexeme
                        (format-position start-pos)
                        (format-position end-pos))))

;;
;; Slog parser
;;

;; Error handler
(define (parse-error lines filename tok-name tok-value start-pos end-pos)
  (pretty-error lines
                `(pos ,filename
                      ,(cons (position-line start-pos) (position-col start-pos))
                      ,(cons (position-line end-pos) (position-col end-pos)))
                (format "Parse error. Trying to match ~a (~a)"
                        tok-name
                        tok-value)))

(define current-filename (make-parameter ""))

;;
;; Parser helpers -- mainly related to adding and manipulating
;; positional information.
;;

;; Wrap the result of a thunk in a provenance tag along with starting
;; and ending position.
(define (wrap-prov start-pos end-pos thunk)
  `(prov ,(thunk) (pos ,(current-filename)
                       ,(cons (position-line start-pos) (position-col start-pos))
                       ,(cons (position-line end-pos)   (position-col end-pos)))))

;; Project the contents of a provenance-annotated piece of tagged
;; data.
(define (prov-contents data)
  (match data
    [`(prov ,c _) c]
    [else data]))

;; Takes a position-annotated statement (fact or rule) and produces
;; either #f if it is a rule (not a rule) or a fact stripped of
;; positional information.
(define (remove-prov t)
  (match t
    [`(prov ,e ,_) (remove-prov e)]
    [`(,es ...)    (map remove-prov es)]
    [else t]))

;; Is it either a fact or a base value
(define (fact-or-base? a)
  (match (remove-prov a)
    ; Facts
    [(? fact?)   a]
    ; Base values
    [(? number?) a]
    [(? string?) a]
    ; Not identifiers though.
    [else #f]))

;; Check if it is a fact (basically: something fully ground).
(define (fact? a)
  (match (remove-prov a)
    [`(? ,cl) #f]
    [`(?do ,cl) #f]
    [`(,(? symbol? rel) ,(? fact-or-base? args) ...) `(,rel ,@args)]
    [else #f]))

;; mutable hash -> immutable hash
(define (copy-hash h)
  (foldl (λ (key partial-hash) (hash-set partial-hash key (hash-ref h key)))
         (hash)
         (hash-keys h)))

(define (reverse-arrow datum)
  (match datum
    ['--> '<--]
    ['<-- '-->]
    [x x]))

(define (normalize-arrow-direction rule-elements)
  (cond 
    [(member '--> rule-elements) (map reverse-arrow (reverse rule-elements))]
    [else rule-elements]))

(define (normalize-segments rule-elements)
  (match-define (cons final-state res)
    (foldr (λ (elem accu)
            (match-define (cons state res) accu)
            (match (cons elem state)
              [(cons '-- 'body) (cons 'body `(-- <-- ,@res))]
              [(cons '<-- 'body) (cons 'head `(<-- ,@res))]
              [(cons '-- 'head) (cons 'body `(-- ,@res))]
              [(cons '<-- 'head) (cons 'head `(<-- ,@res))] ;; BAD RULE!
              [else (cons state `(,elem ,@res))]))
           (cons 'body '())
           rule-elements))
  (when (equal? final-state 'body) 'BAD-RULE!)
  res)

;; Parse a slog string, add to a a hash h of modules
(define (parse-slog-module input filename h #:allow-$id [allow-$id #f])
  (parameterize ([current-filename filename])
    (with-handlers () ;; Do we need a handler for this?
      (define input-port (open-input-string input))
      (define file-lines (vector->immutable-vector (list->vector (append (port->lines (open-input-string input)) (list "")))))
      (port-count-lines! input-port)

      ;; The set of facts
      (define facts '())

      ;; The hash of rules
      (define rules (make-hash))
      (define comp-rels-set (set))
      (define new-rule-key
        (let ([x 0]) (lambda () (set! x (+ x 1)) x)))
      
      ;; Wrap a single token in a prov tag
      (define (mk-prov token start-pos end-pos) (wrap-prov start-pos end-pos (lambda () token)))

      ;; Build a lexer using Racket's lexer library:
      ;; https://docs.racket-lang.org/parser-tools/Lexers.html Note that
      ;; we're using `lexer-src-pos`, which wraps each of the corresponding
      ;; values generated from each action with a position-token tag. We
      ;; later convert this into a slog-specific tag (as that struct is
      ;; specific to that module).
      (define slog-lexer
        (lexer-src-pos
         ("["          (token-lb))
         ("]"          (token-rb))
         ("("          (token-lp))
         (")"          (token-rp))
         ("{"          (token-lc))
         ("}"          (token-rc))
         ("--"         (token-dash-dash-token "--"))
         ("<--"        (token-larr "<--"))
         ("-->"        (token-rarr "-->"))
         ("="          (token-eq "="))
         ("=/="        (token-neq "=/="))
         ("?("         (token-qp "?("))
         ("?do("         (token-qdop "?do("))
         ("?["         (token-qb "?["))
         ("!["         (token-bb "!["))
         ("!("         (token-bp "!("))
         ("!do("       (token-bdop "!do("))
         ("~("         (token-notlp "~("))
         ("comp"       (token-comp "comp keyword"))
         (":"          (token-colon "colon"))
         ("or"         (token-tor "or"))
         ("and"        (token-tand "and"))
         ;;("not"        (token-not "not"))
         ("..."        (token-dots "..."))
         (comment      (return-without-pos (slog-lexer input-port)))
         (num          (token-integer (string->number lexeme)))
         (strlit       (token-string (let ([xlen (string-length lexeme)])
                                       (substring lexeme 1 (sub1 xlen)))))
         (identifier   (if (or allow-$id (not (string-contains? lexeme "$")))
                          (token-id (string->symbol lexeme))
                          (lex-error file-lines "file" lexeme start-pos end-pos)))
         (whitespace   (return-without-pos (slog-lexer input-port)))
         ((eof)        (token-EOF))
         (any-char     (lex-error file-lines "file" lexeme start-pos end-pos))))
      
      ;; The main parser grammar spec.
      (define slog-parser
        (parser 
         (src-pos)
         ;(if (slog-debug-mode)
         ;(debug "parser-output.txt") (void))
         (start program)
         (end   EOF)
         (error
          (lambda (tok-ok? tok-name tok-value start-pos end-pos)
            (parse-error file-lines filename tok-name tok-value start-pos end-pos)))
         (tokens toks empty-toks)
         (grammar
          ;; toplevel facts and rules are useful for installing those
          ;; rules in the hash table. Note: since the actual work
          ;; happens mutably via each of the toplevel-* productions,
          ;; both of these produce void.
          [program   ([toplevel-stmt program] (void))
                     ([toplevel-stmt] (void))]

          [rule
            ([lb rule-element+ rb] (wrap-prov
              $1-start-pos
              $3-end-pos
              (λ () (normalize-segments (normalize-arrow-direction $2)))))]

          [rule-element+
            ([rule-element rule-element+] (cons $1 $2))
            ([rule-element] (list $1))]

          [rule-element
            ([clause] $1)
            ([dash-dash] $1)
            ([larrow] $1)
            ([rarrow] $1)
            ([rule] `(INNER-RULE ,$1))]
          
          [clause
           ([lp prov-neq iclause iclause rp]
            (wrap-prov $1-start-pos $5-end-pos (lambda () `(,$2 ,$3 ,$4))))
           ([lp prov-eq  iclause iclause rp]
            (wrap-prov $1-start-pos $5-end-pos (lambda () `(,$2 ,$3 ,$4))))
           ([lp prov-id iclause* rp] 
            (wrap-prov $1-start-pos $4-end-pos (lambda () `(,$2 ,@$3))))
           ([lp prov-tor clause-under-or clause-under-or* rp]
            (wrap-prov $1-start-pos $5-end-pos (lambda () `(,$2 ,@(cons $3 $4)))))
           ([notlp prov-id iclause* rp]
            (wrap-prov $1-start-pos $4-end-pos (lambda () `(,(wrap-prov $1-start-pos $4-end-pos (lambda () '~)) ,$2 ,@$3))))]

          [clause-under-or
            ([lp prov-and clause clause* rp] 
             (wrap-prov $1-start-pos $5-end-pos (lambda () `(,$2 ,@(cons $3 $4)))))
            ([clause] $1)]

          [iclause
           ([lst-clause] $1)
           ([qlst-clause] $1)
           ([lp prov-id iclause* rp]
            (wrap-prov $1-start-pos $4-end-pos (lambda () `(,$2 ,@$3))))
           ([bp prov-id iclause* rp]
            (wrap-prov $1-start-pos $4-end-pos (lambda () `(! (,$2 ,@$3)))))
           ([bdop iclause* rp]
            (wrap-prov $1-start-pos $3-end-pos (lambda () `(!do (,@$2)))))
           ([blst-clause] $1)
           ([curly-clause] $1)
           ([bval] $1)
           ([lp prov-tor iclause iclause* rp]
            (wrap-prov $1-start-pos $5-end-pos (lambda () `(,$2 ,(cons $3 $4)))))
           ([qp prov-id iclause* rp]
            (wrap-prov $1-start-pos $4-end-pos (lambda () `(? (,$2 ,@$3)))))
           ([qdop iclause* rp]
            (wrap-prov $1-start-pos $3-end-pos (lambda () `(?do (,@$2)))))
           ([qp prov-tor iclause iclause* rp]
            (wrap-prov $1-start-pos $5-end-pos (lambda () `(or (,$2 ,@(cons $3 $4))))))]

          [curly-clause
           ([lc prov-id iclause* rc]
            (wrap-prov $1-start-pos $3-end-pos (lambda () `(CURLY-CLAUSE ,$2 ,@$3) )))]
            
          [comp-spec
            ([lp comp id* rp] $3)]
          [rule-or-fact
           ([rule] $1)
           ([clause] $1)]

          [lst-clause
           ([lb lst-clause-arg* rb] 
            (wrap-prov $1-start-pos $3-end-pos (lambda () `(LIST-SYNTAX ,@$2))))]
          [qlst-clause
            ([qb lst-clause-arg* rb] 
            (wrap-prov $1-start-pos $3-end-pos (lambda () `(? (LIST-SYNTAX ,@$2)))))]
          [blst-clause
            ([bb lst-clause-arg* rb]
            (wrap-prov $1-start-pos $3-end-pos (lambda () `(! (LIST-SYNTAX ,@$2)))))]

          [bval
           ([prov-lit] $1)
           ([prov-id] $1)]
          
          [lst-clause-arg
            ([bval] $1)
            ([lst-clause] $1)
            ([blst-clause] $1)
            ([qlst-clause] $1)
            ([curly-clause] $1)]
          
          [lst-clause-arg*
            ([lst-clause-arg dots lst-clause-arg*] `(,$1 ,(string->symbol $2) ,@$3))
            ([iclause lst-clause-arg*] `(,$1 ,@$2))
            ([] '())]

          ;; The arrows
          [larrow     ([larr] (string->symbol $1))]
          [rarrow     ([rarr] (string->symbol $1))]
          [dash-dash  ([dash-dash-token] (string->symbol $1))]
          ;; Literals
          [lit      ;;([str] $1)
           ([string]  (if (string? $1) $1 #f))
           ([integer] $1)]

          ;; Helpers
          [iclause*  ([iclause iclause*] (cons $1 $2))
                     ([] '())]
          [clause*  ([clause clause*] (cons $1 $2))
                     ([] '())]
          [clause-under-or*  ([clause-under-or clause-under-or*] (cons $1 $2))
                              ([] '())]
          [id* ([id id*] (cons $1 $2))
               ([] '())]

          ;; Wrappers around other nonterminals that wrap them w/ prov info
          [prov-eq   ([eq] (mk-prov (string->symbol $1) $1-start-pos $1-end-pos))]
          [prov-neq  ([neq] (mk-prov (string->symbol $1) $1-start-pos $1-end-pos))]
          [prov-id   ([id]  (mk-prov $1 $1-start-pos $1-end-pos))]
          [prov-lit  ([lit] (mk-prov $1 $1-start-pos $1-end-pos))]
          [prov-tor  ([tor] (mk-prov 'or $1-start-pos $1-end-pos))]
          [prov-and  ([tand] (mk-prov 'and $1-start-pos $1-end-pos))]

          ;; The toplevel rules wrap their underlying rules and then
          ;; install the proper entries in the rules hash.
          [toplevel-stmt ([rule-or-fact]
                          (let ([fact (fact? $1)])
                            (if #;fact #f
                                ;; It was a fact, treat it like a fact
                                ;; by adding it to the list of facts
                                (set! facts (cons $1 facts))
                                ;; Otherwise, it's a rule, add it to
                                ;; the set of rules.
                                (let ([k (new-rule-key)])
                                  (hash-set! rules k $1)))))
                         ([comp-spec]
                          (set! comp-rels-set (set-union (list->set $1) comp-rels-set))) ])))
         
      (define parsed-slog
        (slog-parser (lambda () (slog-lexer input-port))))
      
      (define has-errors #f)
      (for ([rule (hash-values rules)])
        (when (not (source-tree-rule? rule))
          (set! has-errors #t)
          (printf "~a\n" (simplify-prov rule))
          (pretty-error file-lines 
                        (prov->pos rule) 
                        (format "Invalid rule"))
          (with-handlers
            ([syntax-error?
              (λ (err) 
                (pretty-error file-lines (prov->pos (syntax-error-syntax err)) (syntax-error-msg err)))])
            (assert-source-tree-rule? rule))))
      (when has-errors (error "Encountered invalid rules"))

      (define-values (comp-facts facts+)
        (partition
          (λ (f)
            (match-define `(,tag ,args ...) (strip-prov f))
            (set-member? comp-rels-set tag))
          facts))

      (for ([comp-fact comp-facts])
        (let ([k (new-rule-key)])
          (hash-set! rules k comp-fact)))

      (hash-set
       h
       filename
       (Module
         #;raw-lines file-lines
         #;includes '()  ;; TODO: properly traverse to find includes, add them here, parse them too
         #;facts (list->set facts+)
         #;rules (copy-hash rules)
         #;comp-rels comp-rels-set)))))

;; Parse a single slog file into an output hash of modules.
(define (parse-slog-file filename #:allow-$id [allow-$id #f])
  (parse-slog-module (file->string filename) filename (hash) #:allow-$id allow-$id))

(define (parse-slog-files files #:allow-$id [allow-$id #f])
  (foldl (lambda (filename h) (parse-slog-module (file->string filename) filename h #:allow-$id allow-$id)) (hash) files))
