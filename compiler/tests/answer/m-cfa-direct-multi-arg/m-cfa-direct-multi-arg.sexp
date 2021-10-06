(aeval
 (time (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x")))) (null) (null))
 (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
 (clo
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (time
   (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
   (null)
   (null))))
(aeval
 (time
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x")))))
 (ref "x")
 (clo
  (lam (LIST "y") (app (ref "y") (LIST (ref "y"))))
  (time (null) (null) (null))))
(aeval
 (time (null) (null) (null))
 (lam (LIST "y") (app (ref "y") (LIST (ref "y"))))
 (clo
  (lam (LIST "y") (app (ref "y") (LIST (ref "y"))))
  (time (null) (null) (null))))
(aeval
 (time
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (null))
 (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
 (clo
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (time
   (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
   (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
   (null))))
(aeval
 (time (null) (null) (null))
 (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
 (clo
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (time (null) (null) (null))))
(aeval
 (time
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x")))))
 (ref "f")
 (clo
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (time (null) (null) (null))))
(aeval
 (time
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (null))
 (ref "x")
 (clo
  (lam (LIST "y") (app (ref "y") (LIST (ref "y"))))
  (time (null) (null) (null))))
(aeval
 (time
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x")))))
 (lam (LIST "y") (app (ref "y") (LIST (ref "y"))))
 (clo
  (lam (LIST "y") (app (ref "y") (LIST (ref "y"))))
  (time
   (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
   (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
   (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x")))))))
(aeval
 (time (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x")))) (null) (null))
 (ref "x")
 (clo
  (lam (LIST "y") (app (ref "y") (LIST (ref "y"))))
  (time (null) (null) (null))))
(aeval
 (time (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x")))) (null) (null))
 (ref "f")
 (clo
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (time (null) (null) (null))))
(aeval
 (time
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (null))
 (ref "f")
 (clo
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (time (null) (null) (null))))
(aeval
 (time
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (null))
 (lam (LIST "y") (app (ref "y") (LIST (ref "y"))))
 (clo
  (lam (LIST "y") (app (ref "y") (LIST (ref "y"))))
  (time
   (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
   (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
   (null))))
(aeval
 (time (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x")))) (null) (null))
 (lam (LIST "y") (app (ref "y") (LIST (ref "y"))))
 (clo
  (lam (LIST "y") (app (ref "y") (LIST (ref "y"))))
  (time
   (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
   (null)
   (null))))
(aeval
 (time
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x")))))
 (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
 (clo
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (time
   (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
   (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
   (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x")))))))

(control
 (ref "x")
 (kont-addr
  (time
   (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
   (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
   (null))
  (ref "x"))
 (time
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (null)))
(control
 (app (ref "f") (LIST (ref "x")))
 (mt-addr)
 (time
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (null)))
(control
 (app (ref "f") (LIST (ref "x")))
 (mt-addr)
 (time (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x")))) (null) (null)))
(control
 (ref "f")
 (kont-addr
  (time (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x")))) (null) (null))
  (app (ref "f") (LIST (ref "x"))))
 (time (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x")))) (null) (null)))
(control
 (ref "f")
 (kont-addr
  (time
   (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
   (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
   (null))
  (app (ref "f") (LIST (ref "x"))))
 (time
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (null)))
(control
 (app
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (LIST (lam (LIST "y") (app (ref "y") (LIST (ref "y"))))))
 (mt-addr)
 (time (null) (null) (null)))
(control
 (lam (LIST "y") (app (ref "y") (LIST (ref "y"))))
 (kont-addr
  (time (null) (null) (null))
  (lam (LIST "y") (app (ref "y") (LIST (ref "y")))))
 (time (null) (null) (null)))
(control
 (ref "x")
 (kont-addr
  (time
   (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
   (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
   (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x")))))
  (ref "x"))
 (time
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))))
(control
 (ref "x")
 (kont-addr
  (time (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x")))) (null) (null))
  (ref "x"))
 (time (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x")))) (null) (null)))
(control
 (ref "f")
 (kont-addr
  (time
   (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
   (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
   (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x")))))
  (app (ref "f") (LIST (ref "x"))))
 (time
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))))
(control
 (app (ref "f") (LIST (ref "x")))
 (mt-addr)
 (time
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
  (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))))
(control
 (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
 (kont-addr
  (time (null) (null) (null))
  (app
   (reclam "f" (LIST "x") (app (ref "f") (LIST (ref "x"))))
   (LIST (lam (LIST "y") (app (ref "y") (LIST (ref "y")))))))
 (time (null) (null) (null)))
