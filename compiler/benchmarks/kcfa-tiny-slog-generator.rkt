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

(define entryt (string-join (map (lambda (i) "e") (range 0 m)) " "))
(define entryr (string-join (map (lambda (i) "e") (range 0 (add1 m))) " "))
(define stepu (string-join (map (lambda (i) "_") (range 0 m)) " "))
(define app0t0 (string-join `("app0" ,@(map (lambda (i) (format "t~a" i)) (range 0 (- m 1)))) " "))
(define stepl0l1 (string-join (map (lambda (i) (format "lam_~a" i)) (range 0 m)) " "))
(define t0t1 (string-join (map (lambda (i) (format "t~a" i)) (range 0 m)) " "))
(define vt0t1 (string-join (map (lambda (i) (format "v_t~a" i)) (range 0 m)) " "))
(define xt0t1 (string-join (map (lambda (i) (format "x_t~a" i)) (range 0 m)) " "))
(define yt0t1 (string-join (map (lambda (i) (format "y_t~a" i)) (range 0 m)) " "))
(define zt0t1 (string-join (map (lambda (i) (format "z_t~a" i)) (range 0 m)) " "))
(define et0t1 (string-join (map (lambda (i) (format "e_t~a" i)) (range 0 m)) " "))
(define e0t0t1 (string-join (map (lambda (i) (format "e0_t~a" i)) (range 0 m)) " "))
(define e1t0t1 (string-join (map (lambda (i) (format "e1_t~a" i)) (range 0 m)) " "))
(define e2t0t1 (string-join (map (lambda (i) (format "e2_t~a" i)) (range 0 m)) " "))
(define ot0t1 (string-join (map (lambda (i) (format "o_t~a" i)) (range 0 m)) " "))
(define tot0t1 (string-join (map (lambda (i) (format "to_t~a" i)) (range 0 m)) " "))
(define lamt0t1 (string-join (map (lambda (i) (format "lam_t~a" i)) (range 0 m)) " "))
(define a0t0t1 (string-join (map (lambda (i) (format "a0_t~a" i)) (range 0 m)) " "))
(define a1t0t1 (string-join (map (lambda (i) (format "a1_t~a" i)) (range 0 m)) " "))
(define v1t0t1 (string-join (map (lambda (i) (format "v1_t~a" i)) (range 0 m)) " "))
(define idt0 (string-join `("id" ,@(map (lambda (i) (format "t~a" i)) (range 0 (- m 1)))) " "))
(define tclot0t1 (string-join (map (lambda (i) (format "to_clo_t~a" i)) (range 0 m)) " "))
(define clot0t1 (string-join (map (lambda (i) (format "clo_t~a" i)) (range 0 m)) " "))

(define (convert line)
  (foldl (match-lambda** [(`(,find . ,replace) str) (string-replace str find replace)])
         line
         `(("ENTRYT" . ,entryt)
           ("STEPU" . ,stepu)
           ("APP0T0" . ,app0t0)
           ("ENTRYR" . ,entryr)
           ("TCLOT0T1" . ,tclot0t1)
           ("CLOT0T1" . ,clot0t1)
           ("STEPL0L1" . ,stepl0l1)
           ("LAMT0T1" . ,lamt0t1)
           ("OT0OT1" . ,ot0t1)
           ("TOT0T1" . ,tot0t1)
           ("A0T0T1" . ,a0t0t1)
           ("ET0T1" . ,et0t1)
           ("E0T0T1" . ,e0t0t1)
           ("E1T0T1" . ,e1t0t1)
           ("E2T0T1" . ,e2t0t1)
           ("A1T0T1" . ,a1t0t1)
           ("V1T0T1" . ,v1t0t1)
           ("VT0T1" . ,vt0t1)
           ("XT0T1" . ,xt0t1)
           ("YT0T1" . ,yt0t1)
           ("ZT0T1" . ,zt0t1)
           ("IDT0" . ,idt0)
           ("T0T1" . ,t0t1))))

(for ([line (file->lines "kcfa-tiny-template.slog")])
  (displayln (convert line)))
