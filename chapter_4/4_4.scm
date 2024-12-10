(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'term-rewriting)

; https://www.khanacademy.org/math/algebra2/x2ec2f6f830c9fb89:poly-factor/x2ec2f6f830c9fb89:common-factor/a/taking-common-factors
;; just reverse of "the distributive property"

(define algebra-3
  (rule-simplifier
    (list

      ;; Sums

      (rule `(+ (? a)) a)

      ;; This can recursively remove all nested sums until only one + is reserved.
      (rule `(+ (?? a) (+ (?? b)) (?? c))
            `(+ ,@a ,@b ,@c))

      ;; Keep one consistent order.
      (rule `(+ (?? a) (? y) (? x) (?? b))
            ;; https://en.wikipedia.org/wiki/Bubble_sort#Pseudocode_implementation
            ;; 0. bubble sort since we swaps adjacent elements recursively.
            ;; The difference is here we will scan from the start while the wikipedia won't.
            ;; 1. Both generates the non-decreasing order.
            (and (expr<? x y)
                 `(+ ,@a ,x ,y ,@b)))


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
            ;; will calculate the value.
            `(+ ,(+ x y) ,@z))


      (rule `(* 0 (?? x)) 0)

      (rule `(* 1 (?? x)) `(* ,@x))

      (rule `(* (? x ,number?) (? y ,number?) (?? z))
            `(* ,(* x y) ,@z))

      ;; corresponding + nested in * has been included by "Distributive law".
      ;; 0. added after being reordered and all numbers are calculated.
      ;; IGNORE: 1. Here due to reordering (? x) must be at the same location
      ;; 1. Here the rule using (? x) can't be combined with rules using * in the corresponding part.
      ;; 2. IGNORE: Not use (?? x) due to "counteracted" like (* 2 (+ x y)).
      ;; 2. IGNORE: Here (?? x) can only match symbol or list etc.
      ;; It can't match (x (* y z)) due to reordering.
      ;; 2. Here (?? x) can match nothing... So not use it.
      ;; ?? can't use predicate. So no easy way to avoid that case.
      ;; Anyway here duplicate things can only have length 1, otherwise the ordering is not met.
      ; (rule `(+ (?? a) (?? x) (?? x) (?? c)) ; This considers (+ w w (* -2 w))
      ;       ;; This will be reordered at the next invocation for all rules.
      ;       `(+ ,@a (* 2 ,@x) ,@c))
      (rule `(+ (?? a) (? x) (? x) (?? c)) ; This considers (+ w w (* -2 w))
            ;; This will be reordered at the next invocation for all rules.
            `(+ ,@a (* 2 ,x) ,@c))
      ;; IGNORE: The above can't make sense when we only have (+ w w)

      ;; only number can be simplified.
      (rule `(+ (?? a) (? x) (?? d) (* (? n ,number?) (? x)) (?? c))
            ;; This will be reordered at the next invocation for all rules.
            `(+ ,@a ,@d (* (+ ,n 1) ,x) ,@c))
      ;; We should not use ?? since 2 "x w" in (... x w ... (* 2 x w)) can't be combined.
      ; (rule `(+ (?? a) (?? x) (?? d) (* (? n ,number?) (?? x)) (?? c))
      ;       ;; This will be reordered at the next invocation for all rules.
      ;       `(+ ,@a ,@d (* (+ ,n 1) ,@x) ,@c))

      ;; 0. IGNORE: Only with this, Distributive law will counteract what this does.
      ;; see "(algebra-3 '(+ y z (* 5 z) (* x (+ w w (* -2 w) (* 7 y)))))".
      ;; 0. This may combine to "(* x (+ w (* 7 y)))" which will be then counteracted by Distributive law.
      ;; So wrong.
      ;; 1. Here maybe (... (* -2 x w) (* 5 z) (* x w) ...)
      ; (rule `(+ (?? a) (* (?? b1) (? x) (?? e1)) (?? d) (* (?? b2) (? x) (?? e2)) (?? c))
      ;       ;; This will be reordered at the next invocation for all rules.
      ;       `(+ ,@a ,@d ,@c (* ,x (+ (* ,@b1 ,@e1) (* ,@b2 ,@e2)))))

      ;; 0. only terms with number can be simplified.
      ;; 1. We only combine those terms with same vars, otherwise we can't simplify further as the above "counteracted" shows.
      ;; 2. This also manipulates with case "(* (? n1 ,number?) (? x)) (* (? n2 ,number?) (? x))"
      (rule `(+ (?? a) (* (? n1 ,number?) (?? x)) (* (? n2 ,number?) (?? x)) (?? c))
            ;; This will be reordered at the next invocation for all rules.
            `(+ ,@a (* (+ ,n1 ,n2) ,@x) ,@c))
      ;; length ordering
      (rule `(+ (?? a) (* (?? x)) (* (? n1 ,number?) (?? x)) (?? c))
            ;; This will be reordered at the next invocation for all rules.
            `(+ ,@a (* (+ 1 ,n1) ,@x) ,@c))
      ;; see SDF_exercises/software/sdf/automatic-differentiation/simplifier.scm
      ;; This is included in the above (? x)
      ;; so that (* 2 (* (?? x))) -> (* 2 (?? x)), i.e. (* 2 ,@x).
      ; (rule `(+ (?? a) (* (?? x)) (* (?? x)) (?? c))
      ;       ;; This will be reordered at the next invocation for all rules.
      ;       `(+ ,@a (* 2 ,@x) ,@c))

      )))

(algebra-3 '(+ (* 4 x) (* 3 x)))
; (algebra-3 '(+ (* (+ 4 3) x)))
;Value: (* 7 x)

(algebra-3 '(+ y (* x -2 w) (* x 4 y) (* w x) z (* 5 z) (* x w) (* x y 3)))
;Value: (+ y (* 6 z) (* 7 x y))

;; should be reordered to
; (algebra-3 '(+ y z (* -2 x w) (* 3 x y) (* 4 x y) (* 5 z) (* x w) (* x w)))
; (algebra-3 '(+ y z (* x (+ (* -2 w) (* 3 y))) (* 4 x y) (* 5 z) (* x w) (* x w)))
; (algebra-3 '(+ y z (* 4 x y) (* 5 z) (* x (+ (* -2 w) (* 3 y))) (* x w) (* x w)))
; (algebra-3 '(+ y z (* 5 z) (* x (+ (* 4 y) (* (+ (* -2 w) (* 3 y))))) (* x w) (* x w)))
; (algebra-3 '(+ y z (* 5 z) (* x (+ (* 4 y) (+ (* -2 w) (* 3 y)))) (* x w) (* x w)))
; (algebra-3 '(+ y z (* 5 z) (* x (+ (* 4 y) (* -2 w) (* 3 y))) (* x w) (* x w)))

; (algebra-3 '(+ y z (* 5 z) (* x (+ (* 4 y) (* -2 w) (* 3 y) w w))))
; (algebra-3 '(+ y z (* 5 z) (* x (+ w w (* -2 w) (* 3 y) (* 4 y)))))
; (algebra-3 '(+ y z (* 5 z) (* x (+ w w (* -2 w) (* 7 y)))))
; (algebra-3 '(+ y z (* 5 z) (* x (+ (* 2 w) (* 4 y) (* -2 w) (* 3 y)))))
; (algebra-3 '(+ y (* 6 z) (* x (+ (* 2 w) (* 4 y) (* -2 w) (* 3 y)))))

; (algebra-3 '(+ y (* 6 z) (* x (+ (* 4 y) (* 3 y)))))
; (algebra-3 '(+ y (* 6 z) (* x (* 7 y))))
; (algebra-3 '(+ y (* 6 z) (* 7 x y)))

;; variant
(algebra-3 '(+ y z (* 5 z) (* x (+ w (* 7 y)))))
;Value: (+ y (* 6 z) (* w x) (* 7 x y))
