;; generate test cases to explode CFA
;;
;; Yihao Sun
#lang racket

(match-define `(,file-out ,M ,N)
  (command-line
   #:program "generate souffle a core scheme program has lots of if call/cc"
   #:args (file-out M N)
   `(,file-out ,(string->number M) ,(string->number N))))

;; (define file-out (format "explode-cc-~a.scm" M))

(define vars (map (λ (n) (gensym 'z)) (range M)))

(define test-case
  (foldr (λ (x res-flow)
           (let ([f (gensym 'f)]
                 [ms (map (λ (x) (gensym 'm)) (range N))])
             `((λ (,f) ,(foldl (λ (m res-let)
                                 `(let ([,m (,f (call/cc (λ (k) k)))])
                                    ,res-let))
                               (last ms)
                               ms))
               (λ (,x) ,res-flow))))
         (foldr (λ (z res-call) `(,z ,res-call))
                '(λ (x) x)
                vars)
         vars))

(with-output-to-file file-out
  #:exists 'truncate
  (thunk (pretty-display test-case)))
