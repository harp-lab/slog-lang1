;; generate test cases to explode CFA
;;
;; Yihao Sun
#lang racket

(match-define `(,file-out ,M ,N)
  (command-line
   #:program "generate souffle a core scheme program has lots of if id call"
   #:args (file-out M N)
   `(,file-out ,(string->number M) ,(string->number N))))

;; (define file-out (format "explode-cc-~a.scm" M))

(define vars (map (λ (n) (gensym 'z)) (range M)))

(define test-case
  `(let ([imp (λ (a b) (if a b #f))])
     ,(foldr (λ (x res-flow)
               (let ([f (gensym 'f)]
                     [ms (map (λ (x) (gensym 'm)) (range N))])
                 `((λ (,f) ,(foldl (λ (m res-let)
                                     `(let ([,m (,f (λ (k) k))])
                                        ,res-let))
                                   (last ms)
                                   ms))
                   (λ (,x) ,res-flow))))
             (foldr (λ (z res-call) `(,z ,res-call))
                    1
                    vars)
             vars)))

(with-output-to-file file-out
  #:exists 'truncate
  (thunk (pretty-display test-case)))