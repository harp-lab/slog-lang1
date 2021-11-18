#lang racket

(define (generate-term t k [i 0])
  (define (generate-clo-app j)
    (if (= j 0)
	`(app (ref "w") (ref "z0"))
	`(app ,(generate-clo-app (- j 1)) (ref ,(format "z~a" j)))))
  (define (generate-padding k)
    (if (= k 0) 
	`(lam "w" ,(generate-clo-app (- t 1)))
	`(app (lam "x" ,(generate-padding (- k 1))) (lam "x" (ref "x")))))
  (define fx (format "f~a" i))
  (define zx (format "z~a" i))
  (if (>= i t)
      (generate-padding k)
      `(app
	(lam ,fx
	     (seq (app (ref ,fx) (int 0))
		  (app (ref ,fx) (int 1))))
	(lam ,zx
	     ,(generate-term t k (+ i 1))))))

(pretty-print
 `(entry-point
   ,(generate-term 2 3)))
