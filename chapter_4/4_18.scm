;;; a.
;;; annotation
;; Here assume n to be the object number in the program including nested object like 4 for (list 1 2 3).
;; IGNORE: > the size of the program being analyzed
  ;; compared with big open source project, this overhead can be ignored.
;; annotate-expr
;; 1. boolean?, number? 
;; time: O(1) space: 0
;; 2. symbol?
;; env may have level O(n)
;; time: O(n) space: O(1)
;; 3. if-expr?
;; time: the worst of others space: the worst of others
;; 4. lambda-expr?
;; time: O(n)[(lambda-bvl expr)] * O(n)[get-var-type] + begin-expr?'s complexity
;; space(lambda arg count may be large depening on the program): O(n)+begin-expr?'s complexity
;; 5. combination-expr?
;; time, space: O(n)[(combination-operands expr)]*the worst of others
;; 6. define-expr?
;; time, space: O(1)+the worst of others[(define-value expr)]
;; 7. begin-expr?
;; time, space: O(n)[(begin-exprs expr)]*the worst of others

;; IGNORE: The key problems here is that O(n) with "the worst of others" is one too vague estimation.
  ;; So we may have
  (lambda ()
    (foo (lambda () 
      (begin
        (foo (lambda () ...1))
        ...2
        )
      ))
    )
  ;; Then we keep O(n)*O(n)*O(n)*...

;; Here assume combination-operands have no begin-expr (this is the normal case for procedure application), if-expr and define-expr.
;; So the worst case is that
(begin
  (foo ...)
  (bar ...)
  ...
  )
;; So O(n)*O(n)*...
;; But if arguments are lambda-expr's, then the infinite nesting case occurs again.
;; Maybye the latter O(n) should divide n, so O(1), then O(1/n)?
;; Emm... Better to do this after CRLS...

;;; constraints
;; 1. boolean? number? symbol?
;; time, space: O(1)
;; 2. if-expr?
;; time, space: O(1)+the worst of others
;; 3. lambda-expr?
;; time, space: O(1)+begin-expr?'s complexity
;; 4. combination-expr?
;; time, space: O(1)+O(n)[(combination-operands expr)]*the worst of others
;; So O(n)[(combination-operands expr)]*the worst of others
;; 5. define-expr?
;; time, space: O(1)+the worst of others
;; 6. begin-expr?
;; similar to combination-expr?.
;; time, space: O(n)[begin-exprs]*the worst of others

;; The same problems as the above...

;;; b. > the giant unification phase 
;; i.e. unify-constraints
;; 0. Here constraints lhr/rhs are all type-expression? like texpr-type.
;; 0.a. type-variable? like (? test:35)
;; 0.b. primitive-type? like (numeric-type)
;; 0.c. parametric-type? like (type:procedure ((? y:33) (numeric-type)) (? type:37)) where (co)domain may be large.
;; 1. complexity: contraint-size*base-complexity
;; base-complexity using convention (time-complexity, space-complexity): 
  ;; unify:constant-terms -> (O(1), 0)
  ;; unify:list-terms -> O(n)*the worst of others
  ;; maybe-substitute -> the worst case is that both (terms1, terms2) are var's
    ;; 0. (match:has-binding? var dict)
    ;; It is possible that this value (i.e. (car terms2)) is still var
    ;; Then we run unify:dispatch for (terms2-transformed-once, terms1)
    ;; Then we may again (terms1-transformed-once, terms2-transformed-once)
    ;; ...
    ;; This transformation seq may have O(n) length.
    ;; 0.a. Then we have (terms1*, terms2*) or (terms2*, terms1*).
    ;; Assume (terms1*, terms2*).
    ;; (car terms2*) has no binding, we do (do-substitute (car terms2*) (car terms1*) dict) where (car terms1*) may be still var.
    ;; Then (car terms1*) may have value still being var.
    ;; TODO Here `(match:satisfies-restriction? var term*)` may reject unexpectedly if (car terms1*) value will get its actual non-var value later.
      ;; This is related to 4.12 partially. I skip this.
    ;; 0.a.0. Here I assume find in match:dict-substitution to be O(n) complexity where dict entry count is O(n)
    ;; match:occurs-in? also has O(n) complexity depending on term* which may be list of O(n) len.
    ;; match:extend-dict has O(1) complexity.
    ;; match:map-dict-values has O(n^2) complexity due to (match:single-substitution var term*) may operate on one length O(n) term*.
    ;; So (do-substitute var term dict) has O(n^2) time complexity and O(1) space complexity.
    ;; Then the total maybe-substitute has O(n)+O(n^2)=O(n^2) time complexity and O(1) space complexity.
  ;; Here the problem is again that unify:list-terms may be nested...
;; All in all, contraint-size is unknown due to problems in a. and see "again that ...".

;; > What about the best known algorithms for unification?
;; See https://dl.acm.org/doi/pdf/10.1145/357162.357169 and https://nms.kcl.ac.uk/maribel.fernandez/papers/slides-TCS-SOUP.pdf p14
;; Martelli and Montanari’s algorithm is "the most general unifier".
;; https://en.wikipedia.org/wiki/Unification_(computer_science)#Unification_algorithms
;; is just same as the book.
;; delete: unify:constant-terms and the context of (match:vars-equal? var term*).
;; decompose: unify:list-terms
;; conflict: implied in the overall structure that we remove simultaneously for both terms.
  ;; Then only (and (null? terms1) (null? terms2)) will succeed but not for the case where one is null (i.e. lens of these 2 terms are not same)
;; swap (f to be an uninterpreted function): 2 maybe-substitute's.
;; eliminate, check: (match:occurs-in? var term*)
;; Here x\in var(G) is assumed.

;;; c. 
;; TODO what is "asymptotic behavior"?
;; https://www.cs.cmu.edu/~fp/courses/15317-s23/lectures/21-unification.pdf shows "asymptotic complexity".
