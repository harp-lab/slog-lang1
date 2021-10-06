(reachable
 (app (app (lam "a0" (lam "a1" (lam "a2" (const 2)))) (ref "x0")) (ref "x1"))
 (app (ref "f2") (const 0))
 (app (ref "f1") (const 1)))
(reachable
 (app
  (app (app (lam "a0" (lam "a1" (lam "a2" (const 2)))) (ref "x0")) (ref "x1"))
  (ref "x2"))
 (app (ref "f2") (const 0))
 (app (ref "f1") (const 1)))
(reachable
 (app (ref "f2") (const 0))
 (app (ref "f1") (const 0))
 (app (ref "f0") (const 0)))
(reachable
 (app (app (lam "a0" (lam "a1" (lam "a2" (const 2)))) (ref "x0")) (ref "x1"))
 (app (ref "f2") (const 1))
 (app (ref "f1") (const 1)))
(reachable (const 1) (app (ref "f1") (const 0)) (app (ref "f0") (const 0)))
(reachable
 (let "f2"
   (lam
    "x2"
    (app
     (app
      (app (lam "a0" (lam "a1" (lam "a2" (const 2)))) (ref "x0"))
      (ref "x1"))
     (ref "x2")))
   (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1))))
 (app (ref "f1") (const 0))
 (app (ref "f0") (const 0)))
(reachable (ref "f2") (app (ref "f1") (const 1)) (app (ref "f0") (const 1)))
(reachable (const 1) (app (ref "f1") (const 0)) (app (ref "f0") (const 1)))
(reachable
 (lam
  "x2"
  (app
   (app (app (lam "a0" (lam "a1" (lam "a2" (const 2)))) (ref "x0")) (ref "x1"))
   (ref "x2")))
 (app (ref "f1") (const 0))
 (app (ref "f0") (const 1)))
(reachable
 (app (ref "f2") (const 0))
 (app (ref "f1") (const 1))
 (app (ref "f0") (const 0)))
(reachable
 (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))
 (app (ref "f1") (const 1))
 (app (ref "f0") (const 1)))
(reachable (ref "x1") (app (ref "f2") (const 0)) (app (ref "f1") (const 0)))
(reachable
 (app (app (lam "a0" (lam "a1" (lam "a2" (const 2)))) (ref "x0")) (ref "x1"))
 (app (ref "f2") (const 0))
 (app (ref "f1") (const 0)))
(reachable (const 1) (app (ref "f1") (const 1)) (app (ref "f0") (const 1)))
(reachable
 (app
  (app (app (lam "a0" (lam "a1" (lam "a2" (const 2)))) (ref "x0")) (ref "x1"))
  (ref "x2"))
 (app (ref "f2") (const 0))
 (app (ref "f1") (const 0)))
(reachable
 (const 2)
 (app
  (app (app (lam "a0" (lam "a1" (lam "a2" (const 2)))) (ref "x0")) (ref "x1"))
  (ref "x2"))
 (app (ref "f2") (const 1)))
(reachable
 (lam "a1" (lam "a2" (const 2)))
 (app (lam "a0" (lam "a1" (lam "a2" (const 2)))) (ref "x0"))
 (app (ref "f2") (const 1)))
(reachable
 (app (ref "f2") (const 0))
 (app (ref "f1") (const 1))
 (app (ref "f0") (const 1)))
(reachable
 (lam "a0" (lam "a1" (lam "a2" (const 2))))
 (app (ref "f2") (const 0))
 (app (ref "f1") (const 1)))
(reachable (ref "x0") (app (ref "f2") (const 1)) (app (ref "f1") (const 0)))
(reachable
 (app (ref "f2") (const 1))
 (app (ref "f1") (const 0))
 (app (ref "f0") (const 1)))
(reachable (ref "f2") (app (ref "f1") (const 1)) (app (ref "f0") (const 0)))
(reachable
 (lam "a0" (lam "a1" (lam "a2" (const 2))))
 (app (ref "f2") (const 0))
 (app (ref "f1") (const 0)))
(reachable
 (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))
 (app (ref "f1") (const 0))
 (app (ref "f0") (const 1)))
(reachable
 (app (ref "f2") (const 1))
 (app (ref "f1") (const 0))
 (app (ref "f0") (const 0)))
(reachable
 (lam
  "x2"
  (app
   (app (app (lam "a0" (lam "a1" (lam "a2" (const 2)))) (ref "x0")) (ref "x1"))
   (ref "x2")))
 (app (ref "f1") (const 1))
 (app (ref "f0") (const 1)))
(reachable (ref "x1") (app (ref "f2") (const 0)) (app (ref "f1") (const 1)))
(reachable
 (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))
 (app (ref "f1") (const 0))
 (app (ref "f0") (const 0)))
(reachable
 (app
  (app (app (lam "a0" (lam "a1" (lam "a2" (const 2)))) (ref "x0")) (ref "x1"))
  (ref "x2"))
 (app (ref "f2") (const 1))
 (app (ref "f1") (const 0)))
(reachable (const 0) (app (ref "f1") (const 1)) (app (ref "f0") (const 0)))
(reachable
 (app (lam "a0" (lam "a1" (lam "a2" (const 2)))) (ref "x0"))
 (app (ref "f2") (const 0))
 (app (ref "f1") (const 1)))
(reachable
 (lam "a2" (const 2))
 (app (app (lam "a0" (lam "a1" (lam "a2" (const 2)))) (ref "x0")) (ref "x1"))
 (app (ref "f2") (const 1)))
(reachable (ref "f2") (app (ref "f1") (const 0)) (app (ref "f0") (const 1)))
(reachable (ref "x0") (app (ref "f2") (const 0)) (app (ref "f1") (const 0)))
(reachable
 (lam "a1" (lam "a2" (const 2)))
 (app (lam "a0" (lam "a1" (lam "a2" (const 2)))) (ref "x0"))
 (app (ref "f2") (const 0)))
(reachable
 (lam
  "x2"
  (app
   (app (app (lam "a0" (lam "a1" (lam "a2" (const 2)))) (ref "x0")) (ref "x1"))
   (ref "x2")))
 (app (ref "f1") (const 0))
 (app (ref "f0") (const 0)))
(reachable
 (app (app (lam "a0" (lam "a1" (lam "a2" (const 2)))) (ref "x0")) (ref "x1"))
 (app (ref "f2") (const 1))
 (app (ref "f1") (const 0)))
(reachable (ref "x2") (app (ref "f2") (const 1)) (app (ref "f1") (const 0)))
(reachable (ref "x1") (app (ref "f2") (const 1)) (app (ref "f1") (const 1)))
(reachable
 (lam "a2" (const 2))
 (app (app (lam "a0" (lam "a1" (lam "a2" (const 2)))) (ref "x0")) (ref "x1"))
 (app (ref "f2") (const 0)))
(reachable
 (app (lam "a0" (lam "a1" (lam "a2" (const 2)))) (ref "x0"))
 (app (ref "f2") (const 0))
 (app (ref "f1") (const 0)))
(reachable
 (app (lam "a0" (lam "a1" (lam "a2" (const 2)))) (ref "x0"))
 (app (ref "f2") (const 1))
 (app (ref "f1") (const 1)))
(reachable
 (let "f2"
   (lam
    "x2"
    (app
     (app
      (app (lam "a0" (lam "a1" (lam "a2" (const 2)))) (ref "x0"))
      (ref "x1"))
     (ref "x2")))
   (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1))))
 (app (ref "f1") (const 1))
 (app (ref "f0") (const 0)))
(reachable (ref "x1") (app (ref "f2") (const 1)) (app (ref "f1") (const 0)))
(reachable (const 0) (app (ref "f1") (const 0)) (app (ref "f0") (const 1)))
(reachable
 (lam "a0" (lam "a1" (lam "a2" (const 2))))
 (app (ref "f2") (const 1))
 (app (ref "f1") (const 0)))
(reachable (ref "x2") (app (ref "f2") (const 0)) (app (ref "f1") (const 0)))
(reachable (ref "x0") (app (ref "f2") (const 0)) (app (ref "f1") (const 1)))
(reachable (const 1) (app (ref "f1") (const 1)) (app (ref "f0") (const 0)))
(reachable (ref "f2") (app (ref "f1") (const 0)) (app (ref "f0") (const 0)))
(reachable (const 0) (app (ref "f1") (const 0)) (app (ref "f0") (const 0)))
(reachable (ref "x0") (app (ref "f2") (const 1)) (app (ref "f1") (const 1)))
(reachable (const 0) (app (ref "f1") (const 1)) (app (ref "f0") (const 1)))
(reachable
 (app (lam "a0" (lam "a1" (lam "a2" (const 2)))) (ref "x0"))
 (app (ref "f2") (const 1))
 (app (ref "f1") (const 0)))
(reachable
 (app (ref "f2") (const 1))
 (app (ref "f1") (const 1))
 (app (ref "f0") (const 0)))
(reachable (ref "x2") (app (ref "f2") (const 0)) (app (ref "f1") (const 1)))
(reachable
 (lam "a0" (lam "a1" (lam "a2" (const 2))))
 (app (ref "f2") (const 1))
 (app (ref "f1") (const 1)))
(reachable
 (let "f2"
   (lam
    "x2"
    (app
     (app
      (app (lam "a0" (lam "a1" (lam "a2" (const 2)))) (ref "x0"))
      (ref "x1"))
     (ref "x2")))
   (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1))))
 (app (ref "f1") (const 0))
 (app (ref "f0") (const 1)))
(reachable
 (app
  (app (app (lam "a0" (lam "a1" (lam "a2" (const 2)))) (ref "x0")) (ref "x1"))
  (ref "x2"))
 (app (ref "f2") (const 1))
 (app (ref "f1") (const 1)))
(reachable
 (lam
  "x2"
  (app
   (app (app (lam "a0" (lam "a1" (lam "a2" (const 2)))) (ref "x0")) (ref "x1"))
   (ref "x2")))
 (app (ref "f1") (const 1))
 (app (ref "f0") (const 0)))
(reachable
 (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))
 (app (ref "f1") (const 1))
 (app (ref "f0") (const 0)))
(reachable (ref "x2") (app (ref "f2") (const 1)) (app (ref "f1") (const 1)))
(reachable
 (const 2)
 (app
  (app (app (lam "a0" (lam "a1" (lam "a2" (const 2)))) (ref "x0")) (ref "x1"))
  (ref "x2"))
 (app (ref "f2") (const 0)))
(reachable
 (app (ref "f2") (const 1))
 (app (ref "f1") (const 1))
 (app (ref "f0") (const 1)))
(reachable
 (app (ref "f2") (const 0))
 (app (ref "f1") (const 0))
 (app (ref "f0") (const 1)))
(reachable
 (let "f2"
   (lam
    "x2"
    (app
     (app
      (app (lam "a0" (lam "a1" (lam "a2" (const 2)))) (ref "x0"))
      (ref "x1"))
     (ref "x2")))
   (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1))))
 (app (ref "f1") (const 1))
 (app (ref "f0") (const 1)))
(reachable
 (const 0)
 (app (ref "f0") (const 1))
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#1| |#2|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1)))))
  (= |#2| (ref "x0"))
  (= |#1| (lam "a0" (lam "a1" (lam "a2" (const 2)))))
(reachable
 (ref "f1")
 (app (ref "f0") (const 1))
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#1| |#2|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1)))))
  (= |#2| (ref "x0"))
  (= |#1| (lam "a0" (lam "a1" (lam "a2" (const 2)))))
(reachable
 (app (ref "f1") (const 0))
 (app (ref "f0") (const 0))
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#1| |#2|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1)))))
  (= |#2| (ref "x0"))
  (= |#1| (lam "a0" (lam "a1" (lam "a2" (const 2)))))
(reachable
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#1| |#2|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1))))
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#1| |#2|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1))))
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#1| |#2|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1)))))
  (= |#2| (ref "x0"))
  (= |#1| (lam "a0" (lam "a1" (lam "a2" (const 2)))))
(reachable
 (app (ref "f1") (const 0))
 (app (ref "f0") (const 1))
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#1| |#2|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1)))))
  (= |#2| (ref "x0"))
  (= |#1| (lam "a0" (lam "a1" (lam "a2" (const 2)))))
(reachable
 (ref "f0")
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#1| |#2|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1))))
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#1| |#2|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1)))))
  (= |#2| (ref "x0"))
  (= |#1| (lam "a0" (lam "a1" (lam "a2" (const 2)))))
(reachable
 (app (ref "f0") (const 0))
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#1| |#2|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1))))
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#1| |#2|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1)))))
  (= |#2| (ref "x0"))
  (= |#1| (lam "a0" (lam "a1" (lam "a2" (const 2)))))
(reachable
 (const 1)
 (app (ref "f0") (const 1))
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#1| |#2|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1)))))
  (= |#2| (ref "x0"))
  (= |#1| (lam "a0" (lam "a1" (lam "a2" (const 2)))))
(reachable
 (const 0)
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#1| |#2|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1))))
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#1| |#2|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1)))))
  (= |#2| (ref "x0"))
  (= |#1| (lam "a0" (lam "a1" (lam "a2" (const 2)))))
(reachable
 (const 0)
 (app (ref "f0") (const 0))
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#1| |#2|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1)))))
  (= |#2| (ref "x0"))
  (= |#1| (lam "a0" (lam "a1" (lam "a2" (const 2)))))
(reachable
 (app (ref "f1") (const 1))
 (app (ref "f0") (const 1))
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#1| |#2|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1)))))
  (= |#2| (ref "x0"))
  (= |#1| (lam "a0" (lam "a1" (lam "a2" (const 2)))))
(reachable
 (app (ref "f1") (const 1))
 (app (ref "f0") (const 0))
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#1| |#2|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1)))))
  (= |#2| (ref "x0"))
  (= |#1| (lam "a0" (lam "a1" (lam "a2" (const 2)))))
(reachable
 (const 1)
 (app (ref "f0") (const 0))
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#1| |#2|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1)))))
  (= |#2| (ref "x0"))
  (= |#1| (lam "a0" (lam "a1" (lam "a2" (const 2)))))
(reachable
 (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))
 (app (ref "f0") (const 0))
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#1| |#2|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1)))))
  (= |#2| (ref "x0"))
  (= |#1| (lam "a0" (lam "a1" (lam "a2" (const 2)))))
(reachable
 (const 1)
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#1| |#2|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1))))
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#1| |#2|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1)))))
  (= |#2| (ref "x0"))
  (= |#1| (lam "a0" (lam "a1" (lam "a2" (const 2)))))
(reachable
 (app (ref "f0") (const 1))
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#1| |#2|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1))))
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#1| |#2|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1)))))
  (= |#2| (ref "x0"))
  (= |#1| (lam "a0" (lam "a1" (lam "a2" (const 2)))))
(reachable
 (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1)))
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#1| |#2|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1))))
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#1| |#2|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1)))))
  (= |#2| (ref "x0"))
  (= |#1| (lam "a0" (lam "a1" (lam "a2" (const 2)))))
(reachable
 (ref "f1")
 (app (ref "f0") (const 0))
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#1| |#2|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1)))))
  (= |#2| (ref "x0"))
  (= |#1| (lam "a0" (lam "a1" (lam "a2" (const 2)))))
(reachable
 (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))
 (app (ref "f0") (const 1))
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#1| |#2|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1)))))
  (= |#2| (ref "x0"))
  (= |#1| (lam "a0" (lam "a1" (lam "a2" (const 2)))))
(reachable
 (lam
  "x1"
  (let "f2"
    (lam "x2" (app (app (app |#2| |#3|) (ref "x1")) (ref "x2")))
    (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
 (app (ref "f0") (const 1))
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#2| |#3|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1)))))
  (= |#3| (ref "x0"))
  (= |#2| (lam "a0" (lam "a1" (lam "a2" |#1|))))
  (= |#1| (const 2))
(reachable
 (lam
  "x0"
  (let "f1"
    (lam
     "x1"
     (let "f2"
       (lam "x2" (app (app (app |#2| |#3|) (ref "x1")) (ref "x2")))
       (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
    (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#2| |#3|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1))))
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#2| |#3|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1)))))
  (= |#3| (ref "x0"))
  (= |#2| (lam "a0" |#1|))
  (= |#1| (lam "a1" (lam "a2" (const 2))))
(reachable
 (let "f1"
   (lam
    "x1"
    (let "f2"
      (lam "x2" (app (app (app |#2| |#3|) (ref "x1")) (ref "x2")))
      (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
   (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1))))
 (app (ref "f0") (const 1))
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#2| |#3|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1)))))
  (= |#3| (ref "x0"))
  (= |#2| (lam "a0" (lam "a1" |#1|)))
  (= |#1| (lam "a2" (const 2)))
(reachable
 (lam
  "x1"
  (let "f2"
    (lam "x2" (app (app (app |#2| |#3|) (ref "x1")) (ref "x2")))
    (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
 (app (ref "f0") (const 0))
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#2| |#3|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1)))))
  (= |#3| (ref "x0"))
  (= |#2| (lam "a0" (lam "a1" (lam "a2" |#1|))))
  (= |#1| (const 2))
(reachable
 (let "f1"
   (lam
    "x1"
    (let "f2"
      (lam "x2" (app (app (app |#2| |#3|) (ref "x1")) (ref "x2")))
      (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
   (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1))))
 (app (ref "f0") (const 0))
 (let "f0"
   (lam
    "x0"
    (let "f1"
      (lam
       "x1"
       (let "f2"
         (lam "x2" (app (app (app |#2| |#3|) (ref "x1")) (ref "x2")))
         (seq (app (ref "f2") (const 0)) (app (ref "f2") (const 1)))))
      (seq (app (ref "f1") (const 0)) (app (ref "f1") (const 1)))))
   (seq (app (ref "f0") (const 0)) (app (ref "f0") (const 1)))))
  (= |#3| (ref "x0"))
  (= |#2| (lam "a0" (lam "a1" |#1|)))
  (= |#1| (lam "a2" (const 2)))
