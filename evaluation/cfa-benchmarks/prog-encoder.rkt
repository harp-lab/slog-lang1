;; term generator
#lang racket

(define counter 0)
(define (next) (set! counter (add1 counter)) counter)

(define out-dir "syntax-edb")

(define (render tables)
  (for ([table (hash-keys tables)])
    (define f (open-output-file (format "~a/~a.facts" out-dir (symbol->string table))
                                #:exists 'replace))
    (for ([tuple (hash-ref tables table)])
      (displayln (string-join (map pretty-format tuple) "\t") f))
    (close-output-port f)))

(define (add tables table . rest)
  (hash-set tables table (set-union (hash-ref tables table (set)) (set rest))))

(define (gen term tables)
  (match term
    [(? symbol? x) (let ([id (next)]) `(,id ,(add tables 'syn_ref id (symbol->string x))))]
    [`(,e0 ,e1) (match (gen e0 tables)
                  [`(,ide0 ,tables+)
                   (match (gen e1 tables+)
                     [`(,ide1 ,tables++)
                      (let ([id (next)]) `(,id ,(add tables++ 'syn_app id ide0 ide1)))])])]
    [`(lambda (,x) ,e-body)
     (match (gen e-body tables)
       [`(,idebody ,tables+)
        (let ([id (next)]) `(,id ,(add tables+ 'syn_lam id (symbol->string x) idebody)))])]))

(define (gen-program term)
  (match (gen term (hash))
    [`(,idtop ,tables) (let ([n (next)]) (add tables 'program n idtop))]))

(define (main infile)
  (when (not (directory-exists? out-dir))
    (make-directory out-dir))
  (define in (open-input-file infile))
  (define term (read in))
  (close-input-port in)
  (let ([tables (gen-program term)])
    (render tables)))

(define input-file
  (command-line
   #:program "term-generator.rkt" 
   #:args (input-file)
   input-file))

(main input-file)
