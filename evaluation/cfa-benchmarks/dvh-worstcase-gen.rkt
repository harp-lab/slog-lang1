;; written by kris and tom
#lang racket

(define (generate-term t k [i 0])
  (define (generate-clo-app j)
    (if (= j 0)
	`(w z0)
	`(,(generate-clo-app (- j 1))
          ,(string->symbol (format "z~a" j)))))
  (define (generate-padding k)
    (if (= k 0) 
	`(lambda (w) ,(generate-clo-app (- t 1)))
	`((lambda (x) ,(generate-padding (- k 1))) (lambda (x) x))))
  (define fx (string->symbol (format "f~a" i)))
  (define zx (string->symbol (format "z~a" i)))
  (if (>= i t)
      (generate-padding k)
      `((lambda (,fx)
          ((lambda (,(gensym 'a)) (,fx (lambda (a) a)))
           (,fx (lambda (b) b))))
	(lambda (,zx)
          ,(generate-term t k (+ i 1))))))

(match
    (command-line #:args (tsize padding) (list tsize padding))
  [(list tsize padding)
   (pretty-display (generate-term (string->number tsize)
                                  (string->number padding)))])
