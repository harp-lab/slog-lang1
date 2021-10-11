(let ([unbound 12])
  ((λ (f) (f #t))
   (λ (z) unbound)))
