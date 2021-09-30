;; This is a kcfa-tiny generator. It uses interned facts to build up
;; times and is *not* in sync with the souffle implementation. We keep
;; it here for comparison with the flattened version.
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

(displayln "; run the program:")
(displayln (format "[(control e (time~a e)) <--" (foldl string-append "" (map (lambda _ " e") (range 0 (- m 1))))))
(displayln " (program e)]")

(displayln "\n; with an app step,")
(displayln (format "[(= t (time ap~a))" (foldl string-append "" (map (lambda (i) (format " e~a" i)) (range 0 (- m 1))))))
(displayln " (store p0 t av0)\n (store p1 t av1)\n (step st\n       (control body t)\n       cl) <--\n (= st (control ap t0))\n (= ap (app ae-f ae-a0 ae-a1))")
(displayln (format " (= t0 (time~a))" (foldl string-append "" (reverse (map (lambda (i) (format " e~a" i)) (range 0 m))))))
(displayln " (= cl (clo (lam p0 p1 body) _)) \n (ae-eval ae-f t0 cl)\n (ae-eval ae-a0 t0 av0)\n (ae-eval ae-a1 t0 av1)]")

(define ts (foldl string-append "" (reverse (map (lambda (i) (format " e~a" i)) (range 0 m)))))

(displayln (format "\n; and eval of lambdas,\n(ae-eval ?(lam p0 p1 body) ?(time~a)\n         (clo (lam p0 p1 body)\n              (time~a)))" ts ts))

(displayln "; and eval of local parameter x at t,\n[(ae-eval ?(ref x) t clo) <-- (store x t clo)]\n\n; otherise, propagate free variables\n[(ae-eval (ref x) t clo) <--\n (ae-eval (ref x) tlam clo)\n (step (control ecall _)\n       (control ebody t)\n      (clo (lam p0 p1 ebody) tlam))\n (=/= p0 x)\n (=/= p1 x)]\n")
