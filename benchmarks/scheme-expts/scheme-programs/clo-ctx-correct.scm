(let ([a 5])
	(let ([f (λ (q) a)])
		(let ([a 6])
			(f 12))))
