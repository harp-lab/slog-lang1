;; generate test cases to explode CFA
;;
;; Yihao Sun
#lang racket

(match-define `(,file-out ,M ,N)
  (command-line
   #:program "generate souffle a core scheme program has lots of let + set! undefine behavoir"
   #:args (file-out M N)
   `(,file-out ,(string->number M) ,(string->number N))))


(define test-case
  `(let ([func (λ (x) x)])
     ,(foldl (λ (x res-let)
               `(let ,(foldr (λ (x res-set)
                               (let ([x-sym (gensym 'x)])
                                 (append
                                  (list`(,(gensym) (set! func ,(foldl (λ (idx res)
                                                                        `(λ (,x-sym) ,res))
                                                                      `(λ (,x-sym) ,x-sym)
                                                                      (range x)))))
                                  res-set)))
                             '()
                             (range N))
                  ,res-let))
             (foldl (λ (idx res-call)
                      `(func ,res-call))
                    1
                    (range M))
             (range M))))

(with-output-to-file file-out
  #:exists 'truncate
  (thunk (pretty-display test-case)))
