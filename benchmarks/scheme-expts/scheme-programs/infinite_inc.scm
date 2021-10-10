; church encoded increment a value infinitely.
; ((λ (u x) (u u (add1 x))) (λ (u x) (u u (add1 x))) 0)
(((λ (u) (λ (x) ((u u) ((λ (g) (λ (n) (λ (f) (λ (z) ((n f) z))))) x))))
 (λ (u) (λ (x) ((u u) ((λ (g) (λ (n) (λ (f) (λ (z) ((n f) z))))) x)))))
 (λ (f) (λ (z) z)))

