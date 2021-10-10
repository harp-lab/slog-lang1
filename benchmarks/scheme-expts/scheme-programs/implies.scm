((λ (f) (let ([a (f #t)])
          (let ([b (f #f)])
            b)))
 (λ (w) ((λ (x) (λ (z) (if w w))) (λ (x) x))))