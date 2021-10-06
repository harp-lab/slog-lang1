(program
 (app (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
      (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
      (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))))

(ae-eval
 (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))
 (time (app (ref "x") (ref "y") (ref "x")) (app (ref "y") (ref "x") (ref "y")))
 (clo
  (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))
  (time
   (app (ref "x") (ref "y") (ref "x"))
   (app (ref "y") (ref "x") (ref "y")))))
(ae-eval
 (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
 (time (app (ref "x") (ref "y") (ref "x")) (app (ref "y") (ref "x") (ref "y")))
 (clo
  (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
  (time
   (app (ref "x") (ref "y") (ref "x"))
   (app (ref "y") (ref "x") (ref "y")))))
(ae-eval
 (ref "x")
 (time (app (ref "y") (ref "x") (ref "y")) (app (ref "y") (ref "x") (ref "y")))
 (clo
  (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))
  (time
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x"))))
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))))))
(ae-eval
 (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
 (time (app (ref "x") (ref "y") (ref "x")) (app (ref "y") (ref "x") (ref "y")))
 (clo
  (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
  (time
   (app (ref "x") (ref "y") (ref "x"))
   (app (ref "y") (ref "x") (ref "y")))))
(ae-eval
 (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))
 (time
  (app (ref "x") (ref "x") (ref "y"))
  (app
   (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
   (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
   (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))))
 (clo
  (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))
  (time
   (app (ref "x") (ref "x") (ref "y"))
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))))))
(ae-eval
 (ref "y")
 (time (app (ref "y") (ref "x") (ref "y")) (app (ref "x") (ref "y") (ref "x")))
 (clo
  (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
  (time
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x"))))
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))))))
(ae-eval
 (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
 (time
  (app
   (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
   (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
   (lam "x" "y" (app (ref "x") (ref "y") (ref "x"))))
  (app
   (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
   (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
   (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))))
 (clo
  (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
  (time
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x"))))
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))))))
(ae-eval
 (ref "y")
 (time
  (app (ref "x") (ref "x") (ref "y"))
  (app
   (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
   (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
   (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))))
 (clo
  (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))
  (time
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x"))))
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))))))
(ae-eval
 (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
 (time (app (ref "y") (ref "x") (ref "y")) (app (ref "y") (ref "x") (ref "y")))
 (clo
  (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
  (time
   (app (ref "y") (ref "x") (ref "y"))
   (app (ref "y") (ref "x") (ref "y")))))
(ae-eval
 (ref "y")
 (time (app (ref "y") (ref "x") (ref "y")) (app (ref "x") (ref "x") (ref "y")))
 (clo
  (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))
  (time
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x"))))
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))))))
(ae-eval
 (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
 (time (app (ref "y") (ref "x") (ref "y")) (app (ref "x") (ref "x") (ref "y")))
 (clo
  (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
  (time
   (app (ref "y") (ref "x") (ref "y"))
   (app (ref "x") (ref "x") (ref "y")))))
(ae-eval
 (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
 (time (app (ref "y") (ref "x") (ref "y")) (app (ref "x") (ref "x") (ref "y")))
 (clo
  (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
  (time
   (app (ref "y") (ref "x") (ref "y"))
   (app (ref "x") (ref "x") (ref "y")))))
(ae-eval
 (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
 (time (app (ref "y") (ref "x") (ref "y")) (app (ref "x") (ref "y") (ref "x")))
 (clo
  (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
  (time
   (app (ref "y") (ref "x") (ref "y"))
   (app (ref "x") (ref "y") (ref "x")))))
(ae-eval
 (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
 (time
  (app (ref "x") (ref "x") (ref "y"))
  (app
   (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
   (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
   (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))))
 (clo
  (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
  (time
   (app (ref "x") (ref "x") (ref "y"))
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))))))
(ae-eval
 (ref "x")
 (time (app (ref "y") (ref "x") (ref "y")) (app (ref "x") (ref "y") (ref "x")))
 (clo
  (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))
  (time
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x"))))
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))))))
(ae-eval
 (ref "x")
 (time (app (ref "x") (ref "y") (ref "x")) (app (ref "y") (ref "x") (ref "y")))
 (clo
  (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))
  (time
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x"))))
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))))))
(ae-eval
 (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
 (time
  (app (ref "x") (ref "x") (ref "y"))
  (app
   (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
   (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
   (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))))
 (clo
  (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
  (time
   (app (ref "x") (ref "x") (ref "y"))
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))))))
(ae-eval
 (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))
 (time (app (ref "y") (ref "x") (ref "y")) (app (ref "x") (ref "x") (ref "y")))
 (clo
  (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))
  (time
   (app (ref "y") (ref "x") (ref "y"))
   (app (ref "x") (ref "x") (ref "y")))))
(ae-eval
 (ref "x")
 (time (app (ref "y") (ref "x") (ref "y")) (app (ref "x") (ref "x") (ref "y")))
 (clo
  (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
  (time
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x"))))
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))))))
(ae-eval
 (ref "y")
 (time
  (app
   (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
   (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
   (lam "x" "y" (app (ref "x") (ref "y") (ref "x"))))
  (app
   (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
   (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
   (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))))
 (clo
  (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))
  (time
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x"))))
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))))))
(ae-eval
 (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))
 (time
  (app
   (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
   (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
   (lam "x" "y" (app (ref "x") (ref "y") (ref "x"))))
  (app
   (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
   (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
   (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))))
 (clo
  (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))
  (time
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x"))))
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))))))
(ae-eval
 (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
 (time (app (ref "y") (ref "x") (ref "y")) (app (ref "y") (ref "x") (ref "y")))
 (clo
  (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
  (time
   (app (ref "y") (ref "x") (ref "y"))
   (app (ref "y") (ref "x") (ref "y")))))
(ae-eval
 (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
 (time
  (app
   (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
   (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
   (lam "x" "y" (app (ref "x") (ref "y") (ref "x"))))
  (app
   (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
   (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
   (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))))
 (clo
  (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
  (time
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x"))))
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))))))
(ae-eval
 (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
 (time (app (ref "y") (ref "x") (ref "y")) (app (ref "x") (ref "y") (ref "x")))
 (clo
  (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
  (time
   (app (ref "y") (ref "x") (ref "y"))
   (app (ref "x") (ref "y") (ref "x")))))
(ae-eval
 (ref "y")
 (time (app (ref "y") (ref "x") (ref "y")) (app (ref "y") (ref "x") (ref "y")))
 (clo
  (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
  (time
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x"))))
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))))))
(ae-eval
 (ref "x")
 (time
  (app (ref "x") (ref "x") (ref "y"))
  (app
   (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
   (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
   (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))))
 (clo
  (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
  (time
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x"))))
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))))))
(ae-eval
 (ref "x")
 (time
  (app
   (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
   (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
   (lam "x" "y" (app (ref "x") (ref "y") (ref "x"))))
  (app
   (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
   (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
   (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))))
 (clo
  (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
  (time
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x"))))
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))))))
(ae-eval
 (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))
 (time (app (ref "y") (ref "x") (ref "y")) (app (ref "x") (ref "y") (ref "x")))
 (clo
  (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))
  (time
   (app (ref "y") (ref "x") (ref "y"))
   (app (ref "x") (ref "y") (ref "x")))))
(ae-eval
 (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))
 (time (app (ref "y") (ref "x") (ref "y")) (app (ref "y") (ref "x") (ref "y")))
 (clo
  (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))
  (time
   (app (ref "y") (ref "x") (ref "y"))
   (app (ref "y") (ref "x") (ref "y")))))
(ae-eval
 (ref "y")
 (time (app (ref "x") (ref "y") (ref "x")) (app (ref "y") (ref "x") (ref "y")))
 (clo
  (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
  (time
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x"))))
   (app
    (lam "x" "y" (app (ref "x") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "y") (ref "x") (ref "y")))
    (lam "x" "y" (app (ref "x") (ref "y") (ref "x")))))))
