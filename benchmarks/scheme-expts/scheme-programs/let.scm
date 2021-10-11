(let ([a #t]
      [b 1]
      [c 77])
  (let ([f (λ (x) x)])
      (f (if a b c))))
