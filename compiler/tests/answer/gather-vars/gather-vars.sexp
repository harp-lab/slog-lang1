(lambda (args "x")
  (app (lambda (args "y") (var-ref "w"))
       (lambda (args "w") (app (var-ref "z")
                               (var-ref "w")))))

(var "w")
(var "x")
(var "z")
(var "y")
