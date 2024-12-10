(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'term-rewriting)

;; b
(define algebra-2
  (rule-simplifier
    (list

      ;; Sums

      (rule `(+ (? a)) a)

      (rule `(+ (?? a) (+ (?? b)) (?? c))
            `(+ ,@a ,@b ,@c))

      (rule `(+ (?? a) (? y) (? x) (?? b))
            `(+ ,@a ,x ,y ,@b))


      ;; Products

      (rule `(* (? a)) a)

      (rule `(* (?? a) (* (?? b)) (?? c))
            `(* ,@a ,@b ,@c))

      (rule `(* (?? a) (? y) (? x) (?? b))
            (and (expr<? x y)
                 `(* ,@a ,x ,y ,@b)))


      ;; Distributive law

      (rule `(* (?? a) (+ (?? b)) (?? c))
            `(+ ,@(map (lambda (x) `(* ,@a ,x ,@c)) b)))


      ;; Numerical simplifications below

      (rule `(+ 0 (?? x)) `(+ ,@x))

      (rule `(+ (? x ,number?) (? y ,number?) (?? z))
            `(+ ,(+ x y) ,@z))


      (rule `(* 0 (?? x)) 0)

      (rule `(* 1 (?? x)) `(* ,@x))

      (rule `(* (? x ,number?) (? y ,number?) (?? z))
            `(* ,(* x y) ,@z))

      )))
;; again loop
; (algebra-2 '(+ (* 3 (+ x 1)) -3))

;; The problem assumes loop is ignored
(define algebra-2
  (rule-simplifier
    (list
      (rule `(* (? a)) a)

      ;; We can't assume number first now...
      ;; > Explain why numerical simplification would become very expensive.
      ;; The pattern is less deterministic.
      (rule `(+ (?? y) 0 (?? x)) `(+ ,@y ,@x))
      (rule `(+ (?? a) (? x ,number?) (?? b) (? y ,number?) (?? z))
            `(+ ,(+ x y) ,@a ,@b ,@z))
      (rule `(* (?? a) (? x ,number?) (?? b) (? y ,number?) (?? z))
            `(* ,(* x y) ,@a ,@b ,@z))
      )))

;; I just use + as one demo, * is similar.
(algebra-2 '(+ (* 3 x) (* 3 1) -3))
;Value: (+ (* 3 x))