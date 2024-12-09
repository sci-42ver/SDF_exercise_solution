(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'term-rewriting)

(algebra-1 '(* (+ z w) x))

(define algebra-1
  (rule-simplifier
   (list
    ;; Associative law of addition
    (rule '(+ (? a) (+ (? b) (? c)))
          ;; IGNORE: SDF_exercises TODO why use , here and how is rule defined? 
          ;; See SDF_exercises/software/sdf/term-rewriting/test-rule-implementation.scm
          `(+ (+ ,a ,b) ,c))

    ;; Commutative law of multiplication
    (rule '(* (? b) (? a))
          `(* ,a ,b))

    ;; Distributive law of multiplication over addition
    (rule '(* (? a) (+ (? b) (? c)))
          `(+ (* ,a ,b) (* ,a ,c))) )))

;; loop
(algebra-1 '(* (+ z w) x))
