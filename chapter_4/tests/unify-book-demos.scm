(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'unification)

;; > unless, somehow, x=(+ x y)
;; 0. fail for (not (match:occurs-in? var term*))...
;; 1. Later in book, we don't have (+ x y) or (? x) (? y)
(trace match:occurs-in?)
(unifier
  '(+ (* (? a) (? b)) (* (? a) (? c)))
  '(+ (* (cos (? x)) (exp (? y))) (* (cos (+ (? x) (? y))) (sin z)))
  )

;; To make it work, we should assert (? x)=(+ (? x) (? y)), then we do (- (+ (? x) (? y)) (? x))=0 which can be solved with modification of algebra-2.
(load "../software/sdf/term-rewriting/rule-implementation.scm")
(load "../software/sdf/design-of-the-matcher/matcher.scm")
(load "../software/sdf/term-rewriting/rules.scm")
(define algebra-2
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
    
    ;; added
    ;; just one demo to make the above work but maybe not the general for all cases.
    (rule `(= (?? a) (+ (?? a) (?? b)))
          `(= 0 (- ,@a (+ ,@a ,@b))))
    (rule `(= 0 (- (?? a) (+ (?? a) (?? b))))
          `(= 0 ,@b))

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

    )))

(algebra-2 `(= (? x) (+ (? x) (? y))))

(define lhs cadr)
(define rhs caddr)
(let ((equation (algebra-2 `(= (? x) (+ (? x) (? y))))))
  (write-line (list (lhs equation) (rhs equation)))
  (unifier
    (lhs equation)
    (rhs equation)
    ))
; (0 (? y))
;Value: 0
