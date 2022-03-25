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
  [lam2 (generate-cpp-lambda-for-rule-with-builtin-impl rule2 '(1 2) "callback_builtin_range")]

  [rule4 '(srule
            ((rel-select bar 3 (1 2) db) y z 42)
            ((rel-version foo 3 (1 2 3) total) x y z)
            ((rel-version =/= 2 (1 2) comp) x y))]
  [lam4 (generate-cpp-lambda-for-rule-with-builtin rule4)]

  [rule5 '(srule
          ((rel-select bar 4 (1 2 3 4) db) z w x y)
          ((rel-version foo 4 (1 3) total) x y _2 w z)
          ((rel-version > 2 (2 1) comp) x y _1))]
  [lam5 (generate-cpp-lambda-for-rule-with-builtin-impl rule5 '(1 2) "builtin_greater")]

  [rule6 '(srule
          ((rel-select bar 4 (1 2 3 4) db) z w x y)
          ((rel-version foo 4 (1 3) total) x y _2 w z)
          ((rel-version > 2 (2 1) comp) 101 102 _1))]
  [lam6 (generate-cpp-lambda-for-rule-with-builtin-impl rule6 '(1 2) "builtin_greater")]
  
  [rule7 '(srule
          ((rel-select bar 4 (1 2 3 4) db) x ans y w ans z)
          ((rel-version foo 4 (1 3) total) x y _2 w z)
          ((rel-version = 2 (1) comp) 42 _1 ans))]
  [lam7 (generate-cpp-lambda-for-rule-with-builtin-impl rule7 '(1) "builtin_eq_1")]

  [rule8 '(srule
          ((rel-select bar 4 (1 2 3 4) db) 1001 x 1002 y 1003 z)
          ((rel-version foo 3 (1) total) x _2 y z)
          ((rel-version < 2 (2 1) comp) x 100 _1))]
  [lam8 (generate-cpp-lambda-for-rule-with-builtin-impl rule8 '(1 2) "builtin_less")]

  [rule9 '(srule
         ((rel-select $inter-body4 3 (1 2 3) db) $id1 $id2 e)
         ((rel-version $inter-body3 3 (1) total) $id1 $_14 $id2 e)
         ((rel-version = 2 (1 2) comp) $id1 $id1 $_8))]
  [lam9 (generate-cpp-lambda-for-rule-with-builtin-impl rule9 '(1 2) "builtin_eq")]

  [rule10 '(srule
         ((rel-select $inter-body4 3 (1 2 3) db) a b c)
         ((rel-version foo 3 (1 2 3) total) a b c $_1)
         ((rel-version range 3 (3 2 1) comp) c b a $_2))]
  [lam10 (generate-cpp-lambda-for-rule-with-builtin rule10)]

  [rule11 '(srule
         ((rel-select foo 1 (1) db) df)
         ((rel-version do-foo 1 (1) total) $=0 df)
         ((rel-version = 2 (1 2) comp) $=0 0 $_2))]
  [lam11 (generate-cpp-lambda-for-rule-with-builtin rule11)]

  [rule12 '(srule
         ((rel-select bar 3 (1 2 3) db) x y z)
         ((rel-version foo 3 (2 3 1) total) y z x)
         ((rel-version - 3 (2 3 1) comp) y z 4))]
  [lam12 (generate-cpp-lambda-for-rule-with-builtin rule12)]

  [rule13 '(srule
        ((rel-select $inter-head 2 (1 2) db) e $=0)
        ((rel-version foo 1 () total) $_1 e)
        ((rel-version = 2 (2) comp) 0 $_3 $=0))]
  [lam13 (generate-cpp-lambda-for-rule-with-builtin rule13)]

  [rule14 '(srule
            ((rel-select $inter-body1 2 (1 2) db) $id3 $id1)
            ((rel-version $inter-body 3 (1 3) total) $id11 $id1 $_9 $id3)
            ((rel-version = 2 (2 1) comp) $id11 $id1 $_8))]
  [lam14 (generate-cpp-lambda-for-rule-with-builtin rule14)]

  [rule15 '(srule
            ((rel-select $inter-body1 2 (1 2) db) $id3 $id1)
            ((rel-version $inter-body 3 (1 3) total) $id11 $id1 $_9 $id3)
            ((rel-version = 2 (2 1) comp) $id11 $id1 $_8))]
  [lam15 (generate-cpp-lambda-for-rule-with-builtin rule15)]

  [rule16 '(srule
            ((rel-select $inter-body 3 (1 2 3) db) $id11 $id3 $id1)
            ((rel-version ctx 3 (1 3) total) $id1 $id12 $id3 $id11)
            ((rel-version = 2 (1 2) comp) $id1 $id12 $_7))]
  [lam16 (generate-cpp-lambda-for-rule-with-builtin rule16)]

  [rule17 '(srule
            ((rel-select $inter-body2 2 (1 2) db) $id3 $id2)
            ((rel-version $inter-body1 2 (2) total) $id2 $_10 $id3)
            ((rel-version = 2 (1 2) comp) $id2 $id2 $_7))]
  [lam17 (generate-cpp-lambda-for-rule-with-builtin rule17)]
  
  [rule18 '(srule
            ((rel-select bar 3 (1 2 3) db) z x y)
            ((rel-version foo 2 (2) total) x y _1)
            ((rel-version + 3 (1 2) comp) x x _2 z))]
  [lam18 (generate-cpp-lambda-for-rule-with-builtin rule18)]

  [builtins-test-template-file (file->string (get-path "./builtins-tests-template.cpp"))]
  [output-file (format builtins-test-template-file 
                  lam1 lam2 lam4 lam5 lam6 lam7 lam8 lam9 lam10 lam11 lam12 lam13 lam14 lam15 lam16 lam17 lam18)])
  (display-to-file output-file (get-path "./output/builtins-tests-generated.cpp") 	#:exists 'replace))



(let* 
 ([cpp-file-contents (file->string (get-path "./builtins-tests2-template.cpp"))]
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
  
  [func3
    (generate-cpp-func-for-computational-relation-rules
      '{(crule ((rel-select comp_rel3 3 (1 2) comp) 42 inp _ res)
               ((rel-select + 3 (1 2) comp) inp 1 _ inp+1)
               ((rel-select > 2 (1 2) comp) inp+1 inp _)
               ((rel-select < 2 (2 1) comp) inp+1 inp _)
               ((rel-select * 3 (1 2) comp) inp+1 2 _ res))}
      (hash '(rel-select comp_rel3 3 (1 2) comp) "comp_rel3"))]
  
  [func4
    (generate-cpp-func-for-computational-relation-rules
      '{(crule ((rel-select comp_rel4 3 (1 2) comp) x y _ 100)
               ((rel-select < 2 (2 1) comp) y x _))}
      (hash '(rel-select comp_rel4 3 (1 2) comp) "comp_rel4"))]

  [func5
    (generate-cpp-func-for-computational-relation-rules
      '{(crule ((rel-select comp_rel5 3 (1 2) comp) x y _ z)
               ((rel-select - 3 (2 1) comp) y x _ z)
               ((rel-select = 2 (1 2) comp) x 100 _))}
       (hash '(rel-select comp_rel5 3 (1 2) comp) "comp_rel5"))]

  [in_range-func
    (generate-cpp-func-for-computational-relation-rules
      '{(crule ((rel-select in_range 3 (1 2 3) comp) x y z _)
               ((rel-select range 3 (1 2) comp) x y _ z))}
      (hash '(rel-select in_range 3 (1 2 3) comp) "in_range"))]

  [cpp-file-contents-filled-in 
    (string-replace-all cpp-file-contents 
                        "[JOIN_LAM]" join-lam
                        "[COPY_LAM]" copy-lam
                        "[COPY_LAM2]" copy-lam2
                        "[COMPUTATIONAL_RELATION_FUNC]" computational-relation-func
                        "[FACTORIAL_FUNC]" factorial-func
                        "[FUNC3]" func3
                        "[FUNC4]" func4
                        "[FUNC5]" func5
                        "[IN_RANGE]" in_range-func
                        )])

  (display-to-file cpp-file-contents-filled-in (get-path "./output/builtins-tests2-generated.cpp") #:exists 'replace))

(match-let* 
 ([rule1 '(srule
          ((rel-select bar 2 (1 2) db) x y)
          ((rel-version foo 2 (1 2) total) x y $_id)
          ((rel-version ~ 2 (1 2) (agg (rel-version aggregated 2 (1 2) total))) x 100 $_1))]
  [(list local-lam1 _ _ global-lam1) (generate-cpp-lambdas-for-rule-with-aggregator rule1)]

  [rule2 '(srule
          ((rel-select bar 2 (1 2) db) 1000 y x 10000 y)
          ((rel-version foo 2 (1 2) total) x y $_id)
          ((rel-version ~ 2 (1 2) (agg (rel-version aggregated 2 (1 2) total))) x 100 $_1))]
  [(list local-lam2 _ _ global-lam2) (generate-cpp-lambdas-for-rule-with-aggregator rule2)]

  [aggregators-test-template-file (file->string (get-path "./aggregators-tests-template.cpp"))]
  [output-file (format aggregators-test-template-file 
                       local-lam1 global-lam1
                       local-lam2 global-lam2)])
  (display-to-file output-file (get-path "./output/aggregators-tests-generated.cpp") 	#:exists 'replace))