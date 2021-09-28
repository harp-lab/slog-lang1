#lang racket

;; some temporary examples



(provide tc-input-program)



(define tc-input-program
  `(slog-input
    [raw-lines
     ,(vector-immutable
       "(edge 0 1)"
       "(edge 1 2)"
       "[(path x y) <-- (edge x y)]"
       "[(path x z) <-- (edge x y) (path y z)]")]
    [facts
     ,(set `(edge 0 1)
           `(edge 1 2))]
    [rules
     ,(hash 0 `(prov [(prov ((prov path
                                   (pos (2 . 2) (2 . 5)))
                             (prov x (pos (2 . 7) (2 . 7)))
                             (prov z (pos (2 . 9) (2 . 9))))
                            (pos (2 . 1) (2 . 10)))
                      <--
                      (prov ((prov edge
                                   (pos (2 . 17) (2 . 20)))
                             (prov x (pos (2 . 22) (2 . 22)))
                             (prov y (pos (2 . 24) (2 . 24))))
                            (pos ,(cons 2 16) ,(cons 2 25)))]
                     (pos ,(cons 2 0) ,(cons 2 26)))
            1 `(prov [(prov ((prov path
                                   (pos (3 . 2) (3 . 5)))
                             (prov x (pos (3 . 7) (3 . 7)))
                             (prov y (pos (3 . 9) (3 . 9))))
                            (pos (3 . 1) (3 . 10)))
                      <--
                      (prov ((prov edge
                                   (pos (3 . 17) (3 . 20)))
                             (prov x (pos (3 . 22) (3 . 22)))
                             (prov y (pos (3 . 24) (3 . 24))))
                            (pos ,(cons 3 16) ,(cons 3 25)))
                      (prov ((prov path
                                   (pos (3 . 28) (3 . 31)))
                             (prov y (pos (3 . 33) (3 . 33)))
                             (prov z (pos (3 . 35) (3 . 35))))
                            (pos ,(cons 3 27) ,(cons 3 36)))]
                     (pos ,(cons 3 0) ,(cons 3 37))))]))




