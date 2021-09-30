#lang racket

(require racket/cmdline)

(define (intern term map)
  (define x 0)
  (define (newid) (set! x (add1 x)) x)
  (define (h term map)
    (match term
      [(? string?) (cons term map)]
      [`(,rel ,args ...) 
       (match-define (cons rids map-prime)
         (foldl (lambda (arg rids+map)
                  (define newid+newmap (h arg (cdr rids+map)))
                  (cons (cons (car newid+newmap) (car rids+map))
                        (cdr newid+newmap)))
                (cons '() map)
                args))
       (define ids (reverse rids))
       (define new-id (newid))
       (cons new-id (hash-set map-prime rel (set-add (hash-ref map-prime rel (set)) `(,rel ,new-id ,@ids))))]))
  (cdr (h term map)))

(define (freevars depth env)
  (if (equal? depth 0)
      (begin
        (if (= 0 (random 0 2))
            `(VarRef ,(symbol->string (set-first env)))
            `(VarRef ,(symbol->string (gensym)))))
      (let ([x (gensym)])
        `(Lambda ,(symbol->string x)
                 (App ,(freevars (sub1 depth) (set-add env x))
                      ,(freevars (sub1 depth) (set-add env x)))))))

(define (gen-souffle n)
  (define out (open-output-file (format "./freevars-~a.dl" n) #:exists 'replace))
  (define (dump-facts h)
    (for ([k (hash-keys h)])
      (for ([v (set->list (hash-ref h k))])
        (displayln (format "~a(~a)."
                           (symbol->string k)
                           (string-join (map (lambda (tup-elt) (pretty-format tup-elt)) (cdr v)) ","))
                   out))))
  (for ([line (file->lines "./freevars-template.dl")])
    (displayln line out))
  (dump-facts (intern (freevars n (set)) (hash)))
  (close-output-port out))

(define (gen-slog n)
  (define out (open-output-file (format "./freevars-~a.slog" n) #:exists 'replace))
  (define (dump-facts h)
    (for ([k (hash-keys h)])
      (for ([v (set->list (hash-ref h k))])
        (displayln (format "(~a ~a)"
                           (symbol->string k)
                           (string-join (map (lambda (tup-elt) (pretty-format tup-elt)) (cdr v)) " "))
                   out))))
  (for ([line (file->lines "./freevars-template.slog")])
    (displayln line out))
  (dump-facts (intern (freevars n (set)) (hash)))
  (close-output-port out))

;; the size of the term to generate
(define n
  (string->number
   (command-line
    #:program "Freevars generator for some term depth n (produces term of size 2^n)"
    #:once-each
    #:args (n)
    n)))

(gen-souffle n)
(gen-slog n)
