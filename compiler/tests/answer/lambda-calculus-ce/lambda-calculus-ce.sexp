
(eval
 (do-eval (ref "x") (LIST "x" (clo (lambda "x" (ref "x")) (LIST))))
 (clo (lambda "x" (ref "x")) (LIST)))
(eval
 (do-eval
  (app (ref "x") (ref "x"))
  (LIST "x" (clo (lambda "x" (ref "x")) (LIST))))
 (clo (lambda "x" (ref "x")) (LIST)))
(eval
 (do-eval
  (app (lambda "x" (app (ref "x") (ref "x"))) (lambda "x" (ref "x")))
  (LIST))
 (clo (lambda "x" (ref "x")) (LIST)))
(eval
 (do-eval (lambda "x" (ref "x")) (LIST))
 (clo (lambda "x" (ref "x")) (LIST)))
(eval
 (do-eval (lambda "x" (app (ref "x") (ref "x"))) (LIST))
 (clo (lambda "x" (app (ref "x") (ref "x"))) (LIST)))
