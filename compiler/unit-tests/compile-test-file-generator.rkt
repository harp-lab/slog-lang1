#lang racket
; LINK ./builtins-tests-template.cpp
; LINK ./builtins-tests2-template.cpp

(require "../src/compile.rkt")
(require "../src/generic-utils.rkt")

(require racket/runtime-path)

(define-runtime-path HERE ".")
(define (get-path path) (build-path HERE path))

(let* 
 ([rule1 '(srule
          ((rel-select bar 3 (1 2 3) db) x y y+x w 42)
          ((rel-version foo 3 (2 1) total) y x w _2)
          ((rel-version + 3 (2 1) comp) y x _1 y+x))]
  [lam1 (generate-cpp-lambda-for-rule-with-builtin rule1)]

  [rule2 '(srule
          ((rel-select bar 2 (1 2) db) w ans)
          ((rel-version foo 3 (1 2) total) x y _2 w)
          ((rel-version range 3 (1 2) comp) x y _1 ans))]
  [lam2 (generate-cpp-lambda-for-rule-with-callback-builtin rule2 '(1 2) "callback_builtin_range")]

  [rule3 '(srule
            ((rel-select bar 5 (1 2) db) y x 42 rem div)
            ((rel-version foo 5 (1 2 3) total) x y rem _2 w1 w2)
            ((rel-version div-rem 4 (1 2 4) comp) x y rem _1 div))]
  [lam3 (generate-cpp-lambda-for-rule-with-builtin rule3)]

  [rule4 '(srule
            ((rel-select bar 3 (1 2) db) y z 42)
            ((rel-version foo 3 (1 2 3) total) x y z)
            ((rel-version =/= 2 (1 2) comp) x y))]
  [lam4 (generate-cpp-lambda-for-rule-with-builtin rule4)]

  [rule5 '(srule
          ((rel-select bar 4 (1 2 3 4) db) z w x y)
          ((rel-version foo 4 (1 3) total) x y _2 w z)
          ((rel-version > 2 (2 1) comp) x y _1))]
  [lam5 (generate-cpp-lambda-for-rule-with-callback-builtin rule5 '(1 2) "builtin_greater")]

  [rule6 '(srule
          ((rel-select bar 4 (1 2 3 4) db) z w x y)
          ((rel-version foo 4 (1 3) total) x y _2 w z)
          ((rel-version > 2 (2 1) comp) 101 102 _1))]
  [lam6 (generate-cpp-lambda-for-rule-with-callback-builtin rule6 '(1 2) "builtin_greater")]
  
  [rule7 '(srule
          ((rel-select bar 4 (1 2 3 4) db) x ans y w ans z)
          ((rel-version foo 4 (1 3) total) x y _2 w z)
          ((rel-version = 2 (1) comp) 42 _1 ans))]
  [lam7 (generate-cpp-lambda-for-rule-with-callback-builtin rule7 '(1) "builtin_eq_1")]

  [rule8 '(srule
          ((rel-select bar 4 (1 2 3 4) db) 1001 x 1002 y 1003 z)
          ((rel-version foo 3 (1) total) x _2 y z)
          ((rel-version < 2 (2 1) comp) x 100 _1))]
  [lam8 (generate-cpp-lambda-for-rule-with-callback-builtin rule8 '(1 2) "builtin_less")]

  [rule9 '(srule
         ((rel-select $inter-body4 3 (1 2 3) db) $id1 $id2 e)
         ((rel-version $inter-body3 3 (1) total) $id1 $_14 $id2 e)
         ((rel-version = 2 (1 2) comp) $id1 $id1 $_8))]
  [lam9 (generate-cpp-lambda-for-rule-with-callback-builtin rule9 '(1 2) "builtin_eq")]


  [builtins-test-template-file (file->string (get-path "./builtins-tests-template.cpp"))]
  [output-file (format builtins-test-template-file lam1 lam2 lam3 lam4 lam5 lam6 lam7 lam8 lam9)])
  (display-to-file output-file (get-path "./output/builtins-tests-generated.cpp") 	#:exists 'replace))

(let* 
 ([cpp-file-contents (file->string (get-path "./builtins-tests2-template.cpp"))]
  [bi-extended-lam (extend-direct-cpp-builtin-to-new-args 3 '(1 2) '(1 2 3) "test_bi_func")]
  [bi-extended-lam2 (extend-direct-cpp-builtin-to-new-args 4 '(1 2) '(4 2 1) "builtin_div_rem")]
  [join-lam (generate-cpp-lambda-for-computational-join
              '((rel-select > 2 (1 2) comp) x 0 _) "builtin_greater"
              '((rel-select + 3 (1 2) comp) x 10 _ y) "builtin_add"
              '(x) '(y))]
  [copy-lam (generate-cpp-lambda-for-computational-copy
              '((rel-select < 2 (1 2) comp) 0 x _) "builtin_less"
              '((rel-select sign 2 (1) comp) x _ 1))]
  
  [copy-lam2 (generate-cpp-lambda-for-computational-copy
                '((rel-select range 3 (1 2) comp) x y _ 10) "callback_builtin_range"
                '((rel-select my-crel 2 (1 2) comp) x y _))]
  ;[(* b b b2)
  ; (* 4 a a*4)
  ; (* a*4 c a*4*c)
  ; (- b2 a*4*c res)
  ; -->
  ; (delta a b c res)]
  [computational-relation-func 
    (generate-cpp-func-for-computational-relation-rules
      '{(crule ((rel-select delta 4 (1 2 3) comp) a b c _ res)
        
        ((rel-select * 3 (1 2) comp) b b _ b2)
        ((rel-select * 3 (1 2) comp) 4 a _ a*4)
        ((rel-select * 3 (1 2) comp) a*4 c _ a*4*c)
        ((rel-select - 3 (1 2) comp) b2 a*4*c _ res))}
      (hash '(rel-select delta 4 (1 2 3) comp) "delta_func"))]

  ; (factorial 0 1)
  ; [(> x 0)
  ; (- x 1 x-1)
  ; (factorial x-1 x-1fac)
  ; (* x x-1fac res)
  ; -->
  ; (factorial x res)]
  [factorial-func
    (generate-cpp-func-for-computational-relation-rules 
     '{
       (crule ((rel-select factorial 2 (1) comp) x _ res)
          ((rel-select > 2 (1 2) comp) x 0 _)
          
          ((rel-select - 3 (1 2) comp) x 1 _ x-1)
          ((rel-select factorial 2 (1) comp) x-1 _ x-1fac)
          ((rel-select * 3 (1 2) comp) x x-1fac _ res))
       
       (crule ((rel-select factorial 2 (1) comp) 0 _ 1))}
      (hash '(rel-select factorial 2 (1) comp) "cpp_factorial"))]
  [cpp-file-contents-filled-in 
    (string-replace-all cpp-file-contents 
                        "[BI_EXTENDED_LAM]" bi-extended-lam
                        "[BI_EXTENDED_LAM2]" bi-extended-lam2
                        "[JOIN_LAM]" join-lam
                        "[COPY_LAM]" copy-lam
                        "[COPY_LAM2]" copy-lam2
                        "[COMPUTATIONAL_RELATION_FUNC]" computational-relation-func
                        "[FACTORIAL_FUNC]" factorial-func
                        )])

  (display-to-file cpp-file-contents-filled-in (get-path "./output/builtins-tests2-generated.cpp") #:exists 'replace))