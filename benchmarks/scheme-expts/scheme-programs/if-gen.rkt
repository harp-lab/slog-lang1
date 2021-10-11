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

(define vars (map (λ (n) (gensym 'z)) (range (- M 1))))
(define last-v (gensym 'z))

(define imp-js (map (λ (n) (gensym 'j)) (range (- M 1))))
(define last-j (gensym 'j))

(define test-case
  `(let ([imp (λ (,@imp-js ,last-j)
                ,(foldl (λ (v res) `(if ,v ,res #f))
                        `(if ,last-j ,last-j #f)
                        imp-js))])
     ,(foldr (λ (x res-flow)
                (let ([f (gensym 'f)])
                  `((λ (,f) (let ([m (,f #t)]
                                  [n (,f #f)]) n))
                    (λ (,x) ,res-flow))))
              (foldl (λ (idx res-pad)
                       `((λ (x) ,res-pad) (λ (x) x)))
                     `(imp ,@vars ,last-v)
                     (range K))
              `(,@vars ,last-v))
      ))

(with-output-to-file file-out
  #:exists	 'truncate
  (thunk (pretty-display test-case)))
