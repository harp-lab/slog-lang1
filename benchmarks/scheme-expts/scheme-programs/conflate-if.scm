(let ([val
  (let ([f (λ (x) x)])
    (let ([a (f #t)]
          [b (f #f)])
      (if a 4 5)))])
  val)
