#lang racket

(provide gen-kcfa-tiny
         kcfa-tiny
         kcfa-tiny-direct
         term->facts)



(define (kcfa-tiny-direct N)
  (define (f n) (format "f~a" n))
  (define (x n) (format "x~a" n))
  (define (a n) (format "a~a" n))
  (define (gen-lam n)
    (if (= n 0)
        `(const 2)
        `(lam ,(a n) ,(gen-lam (sub1 n)))))
  (define (gen-app n)
    (if (= n 0)
        (gen-lam N)
        `(app ,(gen-app (sub1 n)) (ref ,(x n)))))
  (define (gen n)
    (if (= n 0)
        (gen-app N)
        `(let ,(f n) (lam ,(x n)
                          ,(gen (sub1 n)))
              (seq (app (ref ,(f n)) (const 0))
                   (app (ref ,(f n)) (const 1))))))
  `(program ,(gen N)))


;; Generate a k-CFA-tiny term
(define (kcfa-tiny n)
  (define (z i) (format "z_~a" i))
  (define (f i) (format "f_~a" i))

  (define (wrap body i)
    (if (zero? i)
        body
        (wrap
         `(app (lam ,(f i) "k"
                   (app (ref ,(f i))
                        (lam "x" "k" (app (ref "k") (ref "x") (ref "x")))
                        (lam "na0" "na1" (app (ref ,(f i)) (lam "y" "k" (app (ref "k") (ref "y") (ref "y"))) (ref "k")))))
              (lam ,(z i) "k" ,body)
              (ref "k"))
         (sub1 i))))
  
  #;
  (define (app-wrap body i)
    (if (zero? i)
        body
        (app (lam "x" "k" ))

        )
    )

  (define (wrap-w i)
    (if (zero? i)
        `(lam "v" "na" (app (ref "k") (ref "v") (ref "v")))
        `(lam "w" "na" (app (ref "w") (ref ,(z i)) ,(wrap-w (sub1 i))))))
  
  `(program
    (app
     (lam "x" "k"
          ,(wrap `(app 
                   (ref "k")
                   (lam "w" "k" (app (ref "w") (ref ,(z n)) ,(wrap-w (sub1 n))))
                   (ref "k")) n))
     (lam "u" "k" (app (ref "u") (ref "u") (ref "k")))
     (lam "na0" "na1" (app (lam "u0" "k" (app (ref "u0") (ref "u0") (ref "k")))
                           (lam "u1" "k" (app (ref "u1") (ref "u1") (ref "k")))
                           (lam "u2" "k" (app (ref "u2") (ref "u2") (ref "k"))))))))

(define strings (make-hash))

;; Convert a term 
;; Note: currently only handles variables x and y
(define (term->facts term)
  (define h (make-hash))
  (define c 0)
  (define (next) (set! c (add1 c)) c)
  
  (define (traverse t)
    (match t
      [(? string? s) 
       (define n (next))
       ;;(displayln (format ";; assigning ~a" n))
       ;;(pretty-print t)
       (if (hash-has-key? strings s)
           (hash-ref strings s)
           (begin
             (hash-set! strings s n)
             (hash-ref strings s)))]
      [`(program ,e) 
       (hash-set! h 'Prog (set `(,(traverse e)))) 
       (next)]
      [`(ref ,xy)
       (define n (next))
       ;;(displayln (format "assigning ~a" n))
       ;;(pretty-print t)
       (let* ([v (hash-ref strings xy)])
         (hash-set! h 'Var (set-add (hash-ref h 'Var (set)) `(,n ,v)))
         n)]
      [`(app ,e0 ,e1 ,e2)
       (let* ([this (next)]
              [x (traverse e0)]
              [y (traverse e1)]
              [z (traverse e2)])
         ;;(displayln (format "assigning ~a" this))
         ;;(pretty-print t)
         (hash-set! h 'App (set-add (hash-ref h 'App (set)) `(,this ,x ,y ,z)))
         this)]
      [`(lam ,x ,y ,body)
       (let* ([this (next)]
              [x (traverse x)]
              [y (traverse y)]
              [body (traverse body)])
         ;;(displayln (format "assigning ~a" this))
         ;;(pretty-print t)
         (hash-set! h 'Lam (set-add (hash-ref h 'Lam (set)) `(,this ,x ,y ,body)))
         this)]))
  
  (traverse term)
  
  ;; return the traversed term
  h)

(define (gen-kcfa-tiny n)
  ;;(displayln ";;intern table...")
  (let* ([h (term->facts (kcfa-tiny n))]
         [s (string-split (pretty-format h) "\n")])
    (for ([line s])
      ;;(displayln (format ";; ~a" line)))
      (void))
    h))

;;(pretty-print (kcfa-tiny 2))



