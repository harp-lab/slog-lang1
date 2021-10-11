;; generate test cases to explode CFA
;;
;; Yihao Sun
#lang racket

;;
;; CLI
;;

(match-define `(,file-out ,M ,K)
  (command-line
   #:program "generate souffle a core scheme program has lots of if branches"
   #:args (file-out M K)
   `(,file-out ,(string->number M) ,(string->number K))))

; (define M 128)
;; (define file-out (format "explode-~a.scm" M))

(define vars (map (λ (n) (gensym 'm)) (range (- M 1))))
(define last-v (gensym 'm))

(define test-case
  `((λ (f)
      (let ,(foldl (λ (v idx res)
                     (cons `(,v (f ,idx)) res))
                   `((,last-v (f 0)))
                   vars
                   (range (- M 1)))
        ,last-v))
    (λ (z)
      ,(foldl (λ (idx res)
                `((λ (x) ,res) (λ (x) x)))
              '(+ z (+ z z))
              (range K)))))



(with-output-to-file file-out
  #:exists	 'truncate
  (thunk (pretty-display test-case)))
