;;Slog-lang parser
;; Copyright (c) Kristopher Micinski, et al, see License.md
#lang racket

(require "slog-params.rkt"
         "utils.rkt"
         racket/hash
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/lex
         parser-tools/yacc
         parser-tools/cfg-parser)
(provide parse-slog-file
         parse-slog-string)

;;
;; Slog lexer
;; 

;; Defining token classes

;; Non empty tokens
(define-tokens toks
  (integer string id rel larr rarr dash-dash-token eq neq qlp notlp qp bp dots tor))

;; Empty tokens
(define-empty-tokens empty-toks (lb rb lp rp EOF))

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
          "_" "-" "+" "/" "*" "." "@" "$" "%" "^" "~" "&" "|" "<" ">" "=" "?" "!"))
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

;; Parse a slog string into an output hash of modules.
(define (parse-slog-string input filename)
  (parameterize ([current-filename filename])
    (with-handlers () ;; Do we need a handler for this?
      (define input-port (open-input-string input))
      (define file-lines (vector->immutable-vector (list->vector (append (port->lines (open-input-string input)) (list "")))))
      (port-count-lines! input-port)

      ;; The set of facts
      (define facts '())

      ;; The hash of rules
      (define rules (make-hash))
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
         ("--"         (token-dash-dash-token "--"))
         ("<--"        (token-larr "<--"))
         ("-->"        (token-rarr "-->"))
         ("="          (token-eq "="))
         ("=/="        (token-neq "=/="))
         ("?("         (token-qp "?("))
         ("!("         (token-bp "!("))
         ("~("         (token-notlp "~("))
         ("or"         (token-tor "or"))
         ;;("not"        (token-not "not"))
         ("..."        (token-dots "..."))
         (comment      (return-without-pos (slog-lexer input-port)))
         (num          (token-integer (string->number lexeme)))
         (strlit       (token-string (let* ([x (string-replace lexeme "\\" "")]
                                            [xlen (string-length x)])
                                       (substring x 1 (sub1 xlen)))))
         (identifier   (token-id (string->symbol lexeme)))
         (whitespace   (return-without-pos (slog-lexer input-port)))
         ((eof)        (token-EOF))
         (any-char     (lex-error file-lines "file" lexeme start-pos end-pos))))
      
      ;; The main parser grammar spec.
      ;; the LALR(1) parser could not handle forwards and backwards rules, so we are using the backtracking parser
      (define slog-parser
        (cfg-parser 
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

          [rule-segment-ltr
            ([body-item body-item* rarrow hclause hclause*] `(,$1 ,@$2 ,$3 ,$4 ,@$5))]
          [rule-segment-ltr-dash-dash*
            ([rule-segment-ltr dash-dash rule-segment-ltr-dash-dash*] `(,@$1 ,$2 ,@$3))
            ([body-item body-item* dash-dash rule-segment-ltr-dash-dash*] `(,$1 ,@$2 --> ,$3 ,@$4))
            ([] '())]

          [rule-segment-rtl
            ([hclause hclause* larrow body-item body-item*] `(,$1 ,@$2 ,$3 ,$4 ,@$5))]
          
          [rule-segment-rtl-dash-dash+
            ([rule-segment-rtl] $1)
            ([rule-segment-rtl-dash-dash+ dash-dash rule-segment-rtl] `(,@$1 ,$2 ,@$3))
            ([rule-segment-rtl-dash-dash+ dash-dash body-item body-item*] `(,@$1 ,$2 <-- ,$3 ,@$4))]

          [rule
           ([lb rule-segment-rtl-dash-dash+ rb]
            (wrap-prov
              $1-start-pos
              $3-end-pos
              (λ () $2)))
           ([lb rule-segment-ltr-dash-dash* rule-segment-ltr rb]
            (wrap-prov
              $1-start-pos
              $4-end-pos
              (λ () `(,@(reverse (map reverse-arrow $3)) ,@(reverse (map reverse-arrow $2))))))]
          
          [rule-or-fact
           ([rule] $1)
           ([hclause] $1)]

          [hclause
           ([lp prov-neq ihclause ihclause rp]
            (wrap-prov $1-start-pos $5-end-pos (lambda () `(,$2 ,$3 ,$4))))
           ([lp prov-eq  ihclause ihclause rp]
            (wrap-prov $1-start-pos $5-end-pos (lambda () `(,$2 ,$3 ,$4))))
           ([lp prov-id ihclause* rp] 
            (wrap-prov $1-start-pos $4-end-pos (lambda () `(,$2 ,@$3))))]
          
          [ihclause
           ([lst-clause] $1)
           ([lp prov-id ihclause* rp]
            (wrap-prov $1-start-pos $4-end-pos (lambda () `(,$2 ,@$3))))
           ([qp prov-id iclause* rp]
            (wrap-prov $1-start-pos $4-end-pos (lambda () `(? (,$2 ,@$3)))))
           ([qp prov-tor iclause iclause* rp]
            (wrap-prov $1-start-pos $5-end-pos (lambda () `(or (,$2 ,@(cons $3 $4))))))
           ([bval] $1)]

          [lst-clause
           ([lb lst-clause-arg* rb] 
            (wrap-prov $1-start-pos $3-end-pos (lambda () `(LIST-SYNTAX ,@$2))))]

          [body-item
            ([bclause] $1)
            ([rule] `(INNER-RULE ,$1))]

          [body-item*
           ([body-item body-item*] (cons $1 $2))
           ([] '())]

          [bclause
           ([lp prov-neq iclause iclause rp]
            (wrap-prov $1-start-pos $5-end-pos (lambda () `(,$2 ,$3 ,$4))))
           ([lp prov-tor bclause bclause* rp]
            (wrap-prov $1-start-pos $5-end-pos (lambda () `(,$2 ,@(cons $3 $4)))))
           ([lp prov-eq  iclause iclause rp]
            (wrap-prov $1-start-pos $5-end-pos (lambda () `(,$2 ,$3 ,$4))))
           ([lp prov-id iclause* rp]
            (wrap-prov $1-start-pos $4-end-pos (lambda () `(,$2 ,@$3))))
           ([notlp prov-id iclause* rp]
            (wrap-prov $1-start-pos $4-end-pos (lambda () `(not (,$2 ,@$3)))))]

          [iclause
           ([lst-clause] $1)
           ([lp prov-id iclause* rp]
            (wrap-prov $1-start-pos $4-end-pos (lambda () `(,$2 ,@$3))))
           ([bp prov-id iclause* rp]
            (wrap-prov $1-start-pos $4-end-pos (lambda () `(! (,$2 ,@$3)))))
           ([bval] $1)
           ([lp prov-tor iclause iclause* rp]
            (wrap-prov $1-start-pos $5-end-pos (lambda () `(,$2 ,(cons $3 $4)))))]

          [bval
           ([prov-lit] $1)
           ([prov-id] $1)]
          
          [lst-clause-arg
            ([bval] $1)
            ([lst-clause] $1)]
          
          [lst-clause-arg*
            ([lst-clause-arg lst-clause-arg*] (cons $1 $2))
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
          [hclause*  ([hclause hclause*] (cons $1 $2))
                     ([] '())]
          [iclause*  ([iclause iclause*] (cons $1 $2))
                     ([] '())]
          [ihclause* ([ihclause ihclause*] (cons $1 $2))
                     ([] '())]
          [bclause*  ([bclause bclause*] (cons $1 $2))
                     ([] '())]

          ;; Vars
          [prov-var  ([id]  (mk-prov $1 $1-start-pos $1-end-pos))
                     ([id dots]  (mk-prov `(splice ,$1) $1-start-pos $1-end-pos))]

          ;; Wrappers around other nonterminals that wrap them w/ prov info
          [prov-eq   ([eq] (mk-prov (string->symbol $1) $1-start-pos $1-end-pos))]
          [prov-neq  ([neq] (mk-prov (string->symbol $1) $1-start-pos $1-end-pos))]
          [prov-id   ([id]  (mk-prov $1 $1-start-pos $1-end-pos))]
          [prov-lit  ([lit] (mk-prov $1 $1-start-pos $1-end-pos))]
          [prov-tor  ([tor] (mk-prov 'or $1-start-pos $1-end-pos))]

          ;; The toplevel rules wrap their underlying rules and then
          ;; install the proper entries in the rules hash.
          [toplevel-stmt ([rule-or-fact]
                          (let ([fact (fact? $1)])
                            (if fact
                                ;; It was a fact, treat it like a fact
                                ;; by adding it to the list of facts
                                (set! facts (cons $1 facts))
                                ;; Otherwise, it's a rule, add it to
                                ;; the set of rules.
                                (let ([k (new-rule-key)])
                                  (hash-set! rules k $1)))))])))
         
      (define parsed-slog
        (slog-parser (lambda () (slog-lexer input-port))))
      (hash filename
            `(module 
              [raw-lines ,file-lines]
              [includes ()]  ;; TODO: properly traverse to find includes, add them here, parse them too
              [facts ,(list->set facts)]
              [rules ,(copy-hash rules)])))))

;; Parse a single slog file into an output hash of modules.
(define (parse-slog-file filename)
  (parse-slog-string (file->string filename) filename))