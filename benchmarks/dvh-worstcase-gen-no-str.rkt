#lang racket

(define (generate-term t k [i 0])
  (define (generate-clo-app j)
    (if (= j 0)
	`(app (ref 2) (ref 4))
	`(app ,(generate-clo-app (- j 1)) (ref ,(+ 4 j)))))
  (define (generate-padding k)
    (if (= k 0)
	`(lam 0 ,(generate-clo-app (- t 1)))
	`(app (lam 3 ,(generate-padding (- k 1))) (lam 3 (ref 3)))))
  (define fx (+ 1000 i))
  (define zx (+ 4 i))
  (if (>= i t)
      (generate-padding k)
      `(app
	(lam ,fx
	     (seq (app (ref ,fx) (int 0))
		  (app (ref ,fx) (int 1))))
	(lam ,zx
	     ,(generate-term t k (+ i 1))))))

(pretty-write
 `(entry-point
   ,(generate-term 2 3)))
