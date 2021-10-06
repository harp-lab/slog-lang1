(foo 1)
(foo "hello")
;; this nested fact causes an error in the interpreter
(foo (bar 1))

