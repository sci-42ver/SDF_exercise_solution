(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'term-rewriting)

;; This will match '(+ (? a) (+ (? b) (? c)))

;; Here '(+ y (+ z w)) will have itself as subexpressions-simplified.
;; So try-rule -> (rule '(+ y (+ z w))) succeed fail)
;; 0. Then run-matcher will *encapsulates* by (list datum)
;; so (match-procedure (list '(+ y (+ z w))) ...)
;; Then match:eqv can match (car '(+ y (+ z w))).
;; 1. Then (? a) is bound to 'y.
;; 2. Then match:list with (+ (? b) (? c))<->((+ z w)).
;; the logic is similar.
;; 2.a. After 2 (null? data-list)
;; We go back to (rule data succeed fail) (i.e. (rule '(+ y (+ z w))) succeed fail)).
;; We (simplify-expression result) again.
;; 3. Then (succeed result ...) in make-rule returns the transformed result.

;; 4. Then we (simplify-expression result) again for '(+ (+ y z) w).
;; This time no rule applies (notice each rule uses one new dictionary by run-matcher).
;; It fails to match (w) with (matcher-(+ (? b) (? c))) -> w with matcher-+.
;; Then it calls (fail) in make-rule.
;; This calls (per-rule (cdr rules))
;; Again fails for the rest rules.
;; So calls (lambda () subexpressions-simplified) at last to return the final result.
(algebra-1 '(+ y (+ z w)))


(define -
  (make-pattern-operator
    (rule '((? x)) (n:- 0 x))
    (rule '((? x) (?? y)) (n:- x (apply n:+ y)))))

(- 1 2 3)

(define demo-1
  (rule-simplifier
    (list
      (rule '(a (?? x) (?? y) (?? x) c)
            `(,y))
      )))
(demo-1 '(a b b b b b b c))


