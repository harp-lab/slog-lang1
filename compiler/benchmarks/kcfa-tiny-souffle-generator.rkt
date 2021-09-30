#lang racket

(require racket/cmdline)

;; the size of the term to generate
(define m
  (string->number
   (command-line
    #:program "m-CFA tiny generator for binary CPS, of some polyvariance m"
    #:once-each
    #:args (m)
    m)))

(define entryt (string-join (map (lambda (i) "e") (range 0 m)) ","))
(define entryr (string-join (map (lambda (i) "e") (range 0 (add1 m))) ","))
(define t0t1 (string-join (map (lambda (i) (format "t~a" i)) (range 0 m)) ","))
(define dt0t1 (string-join (map (lambda (i) (format "dt~a:number" i)) (range 0 m)) ","))
(define ot0t1 (string-join (map (lambda (i) (format "ot~a" i)) (range 0 m)) ","))
(define dot0t1 (string-join (map (lambda (i) (format "dot~a:number" i)) (range 0 m)) ","))
(define tot0t1 (string-join (map (lambda (i) (format "to_t~a" i)) (range 0 m)) ","))
(define dtot0t1 (string-join (map (lambda (i) (format "to_t~a:number" i)) (range 0 m)) ","))
(define dat0t1 (string-join (map (lambda (i) (format "da_t~a:number" i)) (range 0 m)) ","))
(define da1t0t1 (string-join (map (lambda (i) (format "da1_t~a:number" i)) (range 0 m)) ","))
(define lamt0t1 (string-join (map (lambda (i) (format "lam_t~a" i)) (range 0 m)) ","))
(define a0t0t1 (string-join (map (lambda (i) (format "a0_t~a" i)) (range 0 m)) ","))
(define a1t0t1 (string-join (map (lambda (i) (format "a1_t~a" i)) (range 0 m)) ","))
(define idt0 (string-join `("id" ,@(map (lambda (i) (format "t~a" i)) (range 0 (- m 1)))) ","))
(define tclot0t1 (string-join (map (lambda (i) (format "to_clo_t~a" i)) (range 0 m)) ","))
(define clot0t1 (string-join (map (lambda (i) (format "clo_t~a" i)) (range 0 m)) ","))
(define dclot0t1 (string-join (map (lambda (i) (format "dclo_t~a:number" i)) (range 0 m)) ","))
(define underscores (string-join (map (lambda (i) "_") (range 0 m)) ","))

(define (convert line)
  (foldl (match-lambda** [(`(,find . ,replace) str) (string-replace str find replace)])
         line
         `(("ENTRYT" . ,entryt)
           ("ENTRYR" . ,entryr)
           ("TCLOT0T1" . ,tclot0t1)
           ("DCLOT0T1" . ,dclot0t1)
           ("CLOT0T1" . ,clot0t1)
           ("LAMT0T1" . ,lamt0t1)
           ("DAT0T1" . ,dat0t1)
           ("DOT0OT1" . ,dot0t1)
           ("DTOT0T1" . ,dtot0t1)
           ("OT0OT1" . ,ot0t1)
           ("DA1T0T1" . ,da1t0t1)
           ("TOT0T1" . ,tot0t1)
           ("A0T0T1" . ,a0t0t1)
           ("A1T0T1" . ,a1t0t1)
           ("IDT0" . ,idt0)
           ("DT0T1" . ,dt0t1)
           ("T0T1" . ,t0t1)
           ("UNDERSCORES" . ,underscores))))

(for ([line (file->lines "kcfa-tiny-template.dl")])
  (displayln (convert line)))
