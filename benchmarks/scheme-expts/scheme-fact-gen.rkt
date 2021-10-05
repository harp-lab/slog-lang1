;; a racket script to generate fact from a core scheme  program

;; Yihao Sun
;; Syracuse 2021

#lang racket

(require racket/cmdline)
(require racket/sequence)

;; predicate for Davis's core scheme

;; only fix arity lambda
(define (lam? e)
  (match e
    [`(λ (,(? symbol? x₀) ...)  ,(? exp? ebody)) #t]
    [_ #f]))

(define (let? e)
  (match e
    [`(let ((,(? symbol? xs) ,(? exp? ebinds)) ...)
        ,(? exp? let-body))
     #t]
    [_ #f]))

(define (call? e)
  (match e
    [`(,(? prim?) ,(? exp? args) ...) #t]
    [`(,(? exp? efunc) ,(? exp? args) ...)  #t]
    [_ #f]))

; (define (apply? e)
;   (match e
;     [`(apply ,(? exp? efunc) ,(? exp? eargs)) #t]
;     [else #f]))

;; add more prim here
(define (prim? e)
  (member e '(cons + - * /)))


(define var? symbol?)

(define num? number?)

(define bool? boolean?)

(define (aexp? ae)
  (match ae
    [(? var?) #t]
    [(? lam?) #t]
    [(? prim?) #t]
    [`(quote ,(? exp? e)) #t]
    [(? bool?) #t]
    [(? num?) #t]
    [_ #f]))

(define (exp? e)
  (match e
    [(? aexp?) #t]
    [`(if ,(? exp? eguard)
          ,(? exp? etrue)
          ,(? exp? efalse))
     #t]
    [`(set! ,(? var? x) ,(? exp? ebind)) #t]
    [`(call/cc ,(? exp? ebody)) #t]
    ; [(? apply?) #t]
    [(? let?) #t]
    [(? call?) #t]
    [_ #f]))


(define souffle-fact-cat
  '(lambda lambda_arg_list let let_list call call_arg_list prim prim_call
     var num bool quotation if setb callcc))


(define (souffle-facts? sf)
  (if (hash? sf)
      (and (andmap (λ (c) (member c souffle-fact-cat)) (hash-keys sf))
           (andmap list? (hash-values sf)))
      #f))

(define (gen-var sf id e)
  (match e
    [(? var?)
     (define flist (hash-ref sf 'var))
     (hash-set sf 'var (cons `(,id ,e) flist))]
    [_ sf]))

(define (gen-arg-list sf id lst)
  (foldl (λ (v pos res)
           (let* ([flist (hash-ref res 'lambda_arg_list)])
             (hash-set res 'lambda_arg_list
                       (cons `(,id ,pos ,v) flist))))
         sf
         lst  
         (sequence->list (in-range (length lst)))))

(define (gen-lam sf id e)
  (match e
    [`(λ (,(? symbol? xs) ...) ,(? exp? ebody))
     (define body-id (gensym))
     (define arg-list-id (gensym))
     (define sf-arg-list (gen-arg-list sf arg-list-id xs))
     (define sf-body (gen-exp sf-arg-list body-id ebody))
     (hash-set sf-body 'lambda
               (cons `(,id ,arg-list-id ,body-id)
                     (hash-ref sf-body 'lambda)))]
    #;[`(λ ,(? symbol? x) ,(? exp? ebody))
       (define v-id (gensym))
       (define sf-var (gen-var sf v-id x))
       (define body-id (gensym))
       (define sf-body (gen-exp sf-var body-id ebody))
       (define flist (hash-ref sf-body 'lambda_l))
       (hash-set sf-body 'lambda_l
                 (cons `(,id ,v-id ,body-id)))]))


(define (gen-let-list sf id lst)
  (match lst
    ['() sf]
    [`((,x ,b) ,res ...)
     (define b-id (gensym))
     (define sf-b (gen-exp sf b-id b))
     (define sf-next (gen-let-list sf-b id res))
     (define flist (hash-ref sf-next 'let_list))
     (hash-set sf-next 'let_list
               (cons `(,id ,x ,b-id) flist))]))

(define (gen-let sf id e)
  (match e
    [`(let ,bind-list ,(? exp? let-body))
     (define bind-list-id (gensym))
     (define sf-bind-list (gen-let-list sf bind-list-id bind-list))
     (define let-body-id (gensym))
     (define sf-let-body (gen-exp sf-bind-list let-body-id let-body))
     (define flist (hash-ref sf-let-body 'let))
     (hash-set sf-let-body 'let
               (cons `(,id ,bind-list-id ,let-body-id) flist))]))

(define (gen-call sf id e)
  (match e
    [`(,(? prim? efunc) ,(? exp? es) ...)
     (define efunc-id (gensym))
     (define sf-efunc (gen-prim sf efunc-id efunc))
     (define e0-id (gensym))
     (define sf-args (gen-call-args sf-efunc e0-id es))
     (define flist (hash-ref sf-args 'prim_call))
     (hash-set sf-args 'prim_call
               (cons `(,id ,efunc-id ,e0-id) flist))]
    [`(,(? exp? efunc) ,(? exp? es) ...)
     (define efunc-id (gensym))
     (define sf-efunc (gen-exp sf efunc-id efunc))
     (define e0-id (gensym))
     (define sf-args (gen-call-args sf-efunc e0-id es))
     (define flist (hash-ref sf-args 'call))
     (hash-set sf-args 'call
               (cons `(,id ,efunc-id ,e0-id) flist))]))

(define (gen-call-args sf id lst)
  (foldl (λ (e pos res)
           (let* ([e-id (gensym)]
                  [sf-e (gen-exp res e-id e)]
                  [flist (hash-ref sf-e 'call_arg_list)])
             (hash-set sf-e 'call_arg_list
                       (cons `(,id ,pos ,e-id) flist))))
         sf
         lst  
         (sequence->list (in-range (length lst)))))

; (define (gen-apply sf id e)
;   (match e
;     [`(apply ,(? exp? efunc) ,(? exp? eargs))
;      (define efunc-id (gensym))
;      (define sf-efunc (gen-exp sf efunc-id efunc))
;      (define eargs-id (gensym))
;      (define sf-eargs (gen-exp sf-efunc eargs-id eargs))
;      (define flist (hash-ref sf-eargs 'apply))
;      (hash-set sf-eargs 'apply
;                (cons `(,id ,efunc-id ,eargs-id) flist))]))

(define (gen-prim sf id e)
  (define flist (hash-ref sf 'prim))
  (hash-set sf 'prim
            (cons `(,id ,(symbol->string e)) flist)))

(define (gen-num sf id e)
  (define flist (hash-ref sf 'num))
  (hash-set sf 'num
            (cons `(,id ,(number->string e)) flist)))

(define (gen-bool sf id e)
  (define flist (hash-ref sf 'bool))
  (hash-set sf 'bool
            (cons `(,id ,(if e "#t" "#f")) flist)))

(define (gen-quotation sf id e)
  (match e
    [`(quote ,(? exp? ex))
     (define exp-id (gensym))
     (define sf-exp (gen-exp sf exp-id ex))
     (define flist (hash-ref sf-exp 'quotation))
     (hash-set sf 'quotation
               (cons `(,id  ,sf-exp) flist))]))

(define (gen-if sf id e)
  (match e
    [`(if ,(? exp? eguard)
          ,(? exp? etrue)
          ,(? exp? efalse))
     (define eguard-id (gensym))
     (define sf-eguard (gen-exp sf eguard-id eguard))
     (define etrue-id (gensym))
     (define sf-etrue (gen-exp sf-eguard etrue-id etrue))
     (define efalse-id (gensym))
     (define sf-efalse (gen-exp sf-etrue efalse-id efalse))
     (define flist (hash-ref sf-efalse'if))
     (hash-set sf-efalse 'if
               (cons `(,id ,eguard-id ,etrue-id ,efalse-id) flist))]))

(define (gen-set! sf id e)
  (match e
    [`(set! ,(? var? x) ,(? exp? ebind))
     (define var-id (gensym))
     (define sf-var (gen-var sf var-id x))
     (define ebind-id (gensym))
     (define sf-ebind (gen-exp sf-var ebind-id ebind))
     (define flist (hash-ref sf-ebind 'setb))
     (hash-set sf-ebind 'setb
               (cons `(,id ,var-id ,ebind-id) flist))]))

(define (gen-callcc sf id e)
  (match e
    [`(call/cc ,(? exp? ebody))
     (define ebody-id (gensym))
     (define sf-ebody (gen-exp sf ebody-id ebody))
     (define flist (hash-ref sf-ebody 'callcc))
     (hash-set sf-ebody 'callcc
               (cons `(,id ,ebody-id) flist))]))

(define (gen-aexp sf id ae)
  (match ae
    [(? var?) (gen-var sf id ae)]
    [(? lam?) (gen-lam sf id ae)]
    [(? prim?) (gen-prim sf id ae)]
    [`(quote ,(? exp? e)) (gen-quotation sf id ae)]
    [(? bool?) (gen-bool sf id ae)]
    [(? num?) (gen-num sf id ae)]))

(define (gen-exp sf id e)
  (match e
    [(? aexp?) (gen-aexp sf id e)]
    [`(if ,(? exp? eguard)
          ,(? exp? etrue)
          ,(? exp? efalse))
     (gen-if sf id e)]
    [`(set! ,(? var? x) ,(? exp? ebind)) (gen-set! sf id e)]
    [`(call/cc ,(? exp? ebody)) (gen-callcc sf id e)]
    ; [(? apply?) (gen-apply sf id e)]
    [(? let?) (gen-let sf id e)]
    [(? call?) (gen-call sf id e)]))

#;(define (annote top-id sf)
  (define (find-type-fact id sf)
    (map (λ (type)
           (let* ([facts (hash-ref type sf)])
             (map (λ (f) (if (equal? (first f) id)
                             `(,type ,f)
                             #f))
                  facts)))
         (hash-keys sf)))
  (match-define `(,type ,fact-line) (find-type-fact top-id sf))
  (cond
    [(equal? top-id 'var)
     (first fact-line)]
    [(equal? top-id 'lambda)
     (match facts
       [`()])]))

;;
;; CLI
;;

(match-define `(,scheme-program ,fact-dir)
  (command-line
   #:program "generate souffle facts for a core scheme program"
   #:args (scheme-program fact-dir)
   `(,scheme-program ,fact-dir)))

(define scheme-facts
  (let ([top-id (gensym)])
    (gen-exp (foldl (λ (cat res)
                                 (hash-set res cat '()))
                               (hash 'top_exp `((,top-id)))
                               souffle-fact-cat)
                        top-id
                        (car (file->list scheme-program)))))

(for-each
 (λ (cat)
   (with-output-to-file (build-path fact-dir (string-append (symbol->string cat) ".facts"))
     #:exists 'truncate
     (thunk
      (display
       (string-join
        (map (λ (l)
               (string-join
                (map (λ (x)
                       (cond
                         [(symbol? x) (symbol->string x)]
                         [(number? x) (number->string x)]
                         [else x]))
                     l)
                "\t"))
             (hash-ref scheme-facts cat))
        "\n")))))
 (hash-keys scheme-facts))

;; we will intern strings by beginning with this identifier
(define min-intern-num 524280)
(define cur-intern-num min-intern-num)
(define intern-pool (make-hash))
(define (intern s)
  (if (hash-has-key? intern-pool s)
      (hash-ref intern-pool s) 
      (let ([x cur-intern-num])
        (hash-set! intern-pool s x)
        (set! cur-intern-num (add1 cur-intern-num))
        x)))

(let ([write-slog-facts
       (thunk
        (displayln ";; Program is...")
        (for-each (λ (line) (displayln (format ";; ~a" line))) (file->lines scheme-program))
        (displayln ";; Facts are...")
        (for-each (λ (rel)
                    ;;(displayln "here it is..")
                    ;;(displayln rel)
                    ;;(flush-output)
                    (if (> (length (hash-ref scheme-facts rel)) 0)
                        (displayln
                         (string-join
                          (map (λ (l)
                                 (string-append
                                  "("
                                  (symbol->string rel)
                                  "\t"
                                  (string-join
                                   (map (λ (x)
                                          ;;(displayln x)
                                          (cond
                                            [(symbol? x) (number->string (intern x))]
                                            [(number? x) (number->string x)]
                                            [(and (string? x) (string->number x)) x]
                                            [(and (string? x) (prim? (string->symbol x))) (number->string (intern x))]
                                            [#t "1"]
                                            [#f "0"]))
                                        l)
                                   "\t")
                                  ")"))
                               (hash-ref scheme-facts rel))
                          "\n"))
                        (void)))
                  (hash-keys scheme-facts))
        (displayln (format ";; Strings/symbols from the program have been interned to the following values, starting at ~a" min-intern-num))
        (hash-for-each intern-pool (λ (key value) (displayln (format ";; ~a <-> ~a" key value)))))])
  (with-output-to-file (build-path fact-dir "slog-facts.slog") #:exists 'truncate write-slog-facts)
  ;; to stdout
  (write-slog-facts))






