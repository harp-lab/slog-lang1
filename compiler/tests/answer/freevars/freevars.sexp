(lambda (args 0)
  (app (lambda (args 1) (var-ref 2))
       (lambda (args 2) (app (var-ref 3)
                               (var-ref 2)))))

(free 3 (app (var-ref 3) (var-ref 2)))
(free 2 (var-ref 2))
(free
 2
 (lambda (args 0)
   (app
    (lambda (args 1) (var-ref 2))
    (lambda (args 2) (app (var-ref 3) (var-ref 2))))))
(free 2 (app (var-ref 3) (var-ref 2)))
(free
 2
 (app
  (lambda (args 1) (var-ref 2))
  (lambda (args 2) (app (var-ref 3) (var-ref 2)))))
(free
 3
 (app
  (lambda (args 1) (var-ref 2))
  (lambda (args 2) (app (var-ref 3) (var-ref 2)))))
(free 3 (lambda (args 2) (app (var-ref 3) (var-ref 2))))
(free 3 (var-ref 3))
(free 2 (lambda (args 1) (var-ref 2)))
(free
 3
 (lambda (args 0)
   (app
    (lambda (args 1) (var-ref 2))
    (lambda (args 2) (app (var-ref 3) (var-ref 2))))))
