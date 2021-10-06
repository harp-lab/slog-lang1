(is-unsat
 (s-and (s= (s-ref (var 1)) (s-lit 42)) (s/= (s-ref (var 1)) (s-lit 42))))

(is-sat
 (s-and
  (s-true)
  (s-and (s= (s-ref (var 1)) (s-lit 42)) (s/= (s-ref (var 2)) (s-lit 42)))))
(is-sat
 (s-and
  (s-true)
  (s/= (s-lit 42) (s-lit 54))
  (s= (s-ref (var 1)) (s-lit 42))
  (s/= (s-ref (var 2)) (s-lit 42))))
