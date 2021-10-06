(aeval
 (time (ref "x") (ref "x") (ref "x"))
 (reclam "f" "x" (app (ref "f") (ref "x")))
 (clo
  (reclam "f" "x" (app (ref "f") (ref "x")))
  (time (ref "x") (ref "x") (ref "x"))))
(aeval
 (time (ref "x") (ref "x") (lam "y" (app (ref "y") (ref "y"))))
 (reclam "f" "x" (app (ref "f") (ref "x")))
 (clo
  (reclam "f" "x" (app (ref "f") (ref "x")))
  (time (ref "x") (ref "x") (lam "y" (app (ref "y") (ref "y"))))))
(aeval
 (time (null) (null) (null))
 (lam "y" (app (ref "y") (ref "y")))
 (clo (lam "y" (app (ref "y") (ref "y"))) (time (null) (null) (null))))
(aeval
 (time (ref "x") (ref "x") (lam "y" (app (ref "y") (ref "y"))))
 (ref "x")
 (clo (lam "y" (app (ref "y") (ref "y"))) (time (null) (null) (null))))
(aeval
 (time (ref "x") (lam "y" (app (ref "y") (ref "y"))) (null))
 (ref "x")
 (clo (lam "y" (app (ref "y") (ref "y"))) (time (null) (null) (null))))
(aeval
 (time (lam "y" (app (ref "y") (ref "y"))) (null) (null))
 (ref "x")
 (clo (lam "y" (app (ref "y") (ref "y"))) (time (null) (null) (null))))
(aeval
 (time (ref "x") (lam "y" (app (ref "y") (ref "y"))) (null))
 (lam "y" (app (ref "y") (ref "y")))
 (clo
  (lam "y" (app (ref "y") (ref "y")))
  (time (ref "x") (lam "y" (app (ref "y") (ref "y"))) (null))))
(aeval
 (time (ref "x") (lam "y" (app (ref "y") (ref "y"))) (null))
 (reclam "f" "x" (app (ref "f") (ref "x")))
 (clo
  (reclam "f" "x" (app (ref "f") (ref "x")))
  (time (ref "x") (lam "y" (app (ref "y") (ref "y"))) (null))))
(aeval
 (time (ref "x") (ref "x") (ref "x"))
 (lam "y" (app (ref "y") (ref "y")))
 (clo
  (lam "y" (app (ref "y") (ref "y")))
  (time (ref "x") (ref "x") (ref "x"))))
(aeval
 (time (ref "x") (ref "x") (ref "x"))
 (ref "f")
 (clo
  (reclam "f" "x" (app (ref "f") (ref "x")))
  (time (ref "x") (ref "x") (ref "x"))))
(aeval
 (time (ref "x") (ref "x") (lam "y" (app (ref "y") (ref "y"))))
 (lam "y" (app (ref "y") (ref "y")))
 (clo
  (lam "y" (app (ref "y") (ref "y")))
  (time (ref "x") (ref "x") (lam "y" (app (ref "y") (ref "y"))))))
(aeval
 (time (ref "x") (ref "x") (ref "x"))
 (ref "x")
 (clo (lam "y" (app (ref "y") (ref "y"))) (time (null) (null) (null))))
(aeval
 (time (null) (null) (null))
 (reclam "f" "x" (app (ref "f") (ref "x")))
 (clo (reclam "f" "x" (app (ref "f") (ref "x"))) (time (null) (null) (null))))
(aeval
 (time (lam "y" (app (ref "y") (ref "y"))) (null) (null))
 (ref "f")
 (clo
  (reclam "f" "x" (app (ref "f") (ref "x")))
  (time (lam "y" (app (ref "y") (ref "y"))) (null) (null))))
(aeval
 (time (ref "x") (ref "x") (lam "y" (app (ref "y") (ref "y"))))
 (ref "f")
 (clo
  (reclam "f" "x" (app (ref "f") (ref "x")))
  (time (ref "x") (ref "x") (lam "y" (app (ref "y") (ref "y"))))))
(aeval
 (time (ref "x") (lam "y" (app (ref "y") (ref "y"))) (null))
 (ref "f")
 (clo
  (reclam "f" "x" (app (ref "f") (ref "x")))
  (time (ref "x") (lam "y" (app (ref "y") (ref "y"))) (null))))
(aeval
 (time (lam "y" (app (ref "y") (ref "y"))) (null) (null))
 (lam "y" (app (ref "y") (ref "y")))
 (clo
  (lam "y" (app (ref "y") (ref "y")))
  (time (lam "y" (app (ref "y") (ref "y"))) (null) (null))))
(aeval
 (time (lam "y" (app (ref "y") (ref "y"))) (null) (null))
 (reclam "f" "x" (app (ref "f") (ref "x")))
 (clo
  (reclam "f" "x" (app (ref "f") (ref "x")))
  (time (lam "y" (app (ref "y") (ref "y"))) (null) (null))))

(control
 (ref "f")
 (kont-addr
  (time (ref "x") (ref "x") (lam "y" (app (ref "y") (ref "y"))))
  (app (ref "f") (ref "x")))
 (time (ref "x") (ref "x") (lam "y" (app (ref "y") (ref "y")))))
(control
 (ref "x")
 (kont-addr (time (lam "y" (app (ref "y") (ref "y"))) (null) (null)) (ref "f"))
 (time (lam "y" (app (ref "y") (ref "y"))) (null) (null)))
(control
 (app (ref "f") (ref "x"))
 (mt-addr)
 (time (ref "x") (ref "x") (ref "x")))
(control
 (ref "f")
 (kont-addr
  (time (lam "y" (app (ref "y") (ref "y"))) (null) (null))
  (app (ref "f") (ref "x")))
 (time (lam "y" (app (ref "y") (ref "y"))) (null) (null)))
(control
 (ref "f")
 (kont-addr (time (ref "x") (ref "x") (ref "x")) (app (ref "f") (ref "x")))
 (time (ref "x") (ref "x") (ref "x")))
(control
 (app (ref "f") (ref "x"))
 (mt-addr)
 (time (ref "x") (ref "x") (lam "y" (app (ref "y") (ref "y")))))
(control
 (app
  (reclam "f" "x" (app (ref "f") (ref "x")))
  (lam "y" (app (ref "y") (ref "y"))))
 (mt-addr)
 (time (null) (null) (null)))
(control
 (ref "x")
 (kont-addr
  (time (ref "x") (lam "y" (app (ref "y") (ref "y"))) (null))
  (ref "f"))
 (time (ref "x") (lam "y" (app (ref "y") (ref "y"))) (null)))
(control
 (reclam "f" "x" (app (ref "f") (ref "x")))
 (kont-addr
  (time (null) (null) (null))
  (app
   (reclam "f" "x" (app (ref "f") (ref "x")))
   (lam "y" (app (ref "y") (ref "y")))))
 (time (null) (null) (null)))
(control
 (app (ref "f") (ref "x"))
 (mt-addr)
 (time (lam "y" (app (ref "y") (ref "y"))) (null) (null)))
(control
 (ref "x")
 (kont-addr
  (time (ref "x") (ref "x") (lam "y" (app (ref "y") (ref "y"))))
  (ref "f"))
 (time (ref "x") (ref "x") (lam "y" (app (ref "y") (ref "y")))))
(control
 (lam "y" (app (ref "y") (ref "y")))
 (kont-addr
  (time (null) (null) (null))
  (reclam "f" "x" (app (ref "f") (ref "x"))))
 (time (null) (null) (null)))
(control
 (ref "f")
 (kont-addr
  (time (ref "x") (lam "y" (app (ref "y") (ref "y"))) (null))
  (app (ref "f") (ref "x")))
 (time (ref "x") (lam "y" (app (ref "y") (ref "y"))) (null)))
(control
 (app (ref "f") (ref "x"))
 (mt-addr)
 (time (ref "x") (lam "y" (app (ref "y") (ref "y"))) (null)))
(control
 (ref "x")
 (kont-addr (time (ref "x") (ref "x") (ref "x")) (ref "f"))
 (time (ref "x") (ref "x") (ref "x")))
