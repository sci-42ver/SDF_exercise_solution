(load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
(manage 'new 'automatic-differentiation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; preface
(register-predicate! differential? 'differential)
(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))
;; See `predicates-match?` and `apply-predicate`.
;; So the above < is not counted.
; (with-predicate-counts (lambda () (fib 20)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; exercise preface
(load "~/SICP_SDF/SDF_exercises/software/sdf/common/stormer2.scm")
(define (test-stormer-counts)
  (define (F t x) (- x))
  (define numeric-s0
    (make-initial-history 0 .01 (sin 0) (sin -.01) (sin -.02)))
  (with-predicate-counts
    (lambda ()
      (x 0 ((evolver F 'h stormer-2) numeric-s0 1)))))

(define full-generic-arithmetic
  (let ((g (make-generic-arithmetic make-simple-dispatch-store)))
    (add-to-generic-arithmetic! g numeric-arithmetic)
    (extend-generic-arithmetic! g function-extender)
    (add-to-generic-arithmetic! g
                                (symbolic-extender numeric-arithmetic))
    g))
(install-arithmetic! full-generic-arithmetic)
(test-stormer-counts)

(define trie-full-generic-arithmetic
  (let ((g (make-generic-arithmetic make-trie-dispatch-store)))
    (add-to-generic-arithmetic! g numeric-arithmetic)
    (extend-generic-arithmetic! g function-extender)
    (add-to-generic-arithmetic! g
                                (symbolic-extender numeric-arithmetic))
    g))
(install-arithmetic! trie-full-generic-arithmetic)
;; I won't dig into the complex computation process. Here it is as expected with only `any-object` count greater.
;; > We expect that the performance will be better with the trie if we have a large number of rules with the same initial segment.
(test-stormer-counts)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; exercise
;; --> means called only by.
;; Here we only uses the 2nd.
;; predicates-match? -> is-applicable? --> is-operation-applicable? --> operation-union-dispatch --> operation-union* (I only consider combining-arithmetics) --> operation-union --> add-arithmetics* --> add-arithmetics --> extend-arithmetic
;;                  |-> get-handler (only in make-simple-dispatch-store) --> generic-metadata-getter --> get-generic-procedure-handler --> generic-procedure-dispatch --> the-generic-procedure
;; So one get-handler calls one `find` for rules
(define test-rules (generic-procedure-rules cos))
(display test-rules)
(with-predicate-counts 
  (lambda ()
    (define args (list 'u))
    ;; short circuit
    (find 
      (lambda (rule)
        ; (display "run")
        ;; IGNORE: TODO why here it returns 2 when args length is 1.
        (predicates-match? (car rule) args))
      test-rules)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; > For an additional insight, look at the performance of (fib 20) in
;; apply-predicate -> %find-all-edges --> get-matching-tries  -> get-all-values
;;                |                                          |-> get-a-value-by-filtering
;;                |-> %try-edge (called many times) --> get-a-value-by-searching --> get-a-value --> get-handler
;; one get-handler calls one `get-a-value-by-searching`

;; Here we only does (number? number?). Then for the 1st arg, we try symbolic? -> function? -> any-object?
;; Then for the 2nd, we try function?. But it failed. Since we tried all edges of any-object? (see %try-edges), 
;; then we go back and try number?.
;; Then for the 2nd, we try symbolic? -> number?.
(display (generic-procedure-rules <))
(with-predicate-counts (lambda () 1))
(with-predicate-counts (lambda () (fib 1))) ; calls for <.

;; IGNORE: TODO based on the above analysis, symbolic? is same as number?. But why is function? also same?
(install-arithmetic! full-generic-arithmetic)
(display (generic-procedure-rules <))
;; For (number? number?), 3 rules of (symbolic-extender numeric-arithmetic) fail.
;; Then 3 rules of function-extender fail.
;; Then one from numeric-arithmetic works.
;; So we have for each +/< for (number? number?):
;; IGNORE: symbolic?: 1+1+2 number?: 1+1+2 any: 1+1 function:1+1+2

;; Due to every in predicates-match?, 
;; symbolic?: 1+1+1, i.e. (symbolic? symbolic?) + (symbolic? number?) + (number? symbolic?). (others are similar)
;; Here the 1st will fail, so we will only call the 1st symbolic?
;; The 2nd is similar.
;; The 3rd will succeed for the 1st number?, so it will try the 2nd symbolic?.
(with-predicate-counts (lambda () (fib 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; > Understanding this is important, because the fact that sometimes the trie does not help with the performance appears counterintuitive. We explicitly introduced the trie to avoid redundant calls. Explain this phenomenon in a concise paragraph. 
;; IMHO if the arithmetic orders of the 2 full-generic-arithmetic are same, then since both prioritizes the latter addition,
;; if full-generic-arithmetic can be quick, then trie-full-generic-arithmetic will be also quick.
;; And full-generic-arithmetic will probably have redundant calls for the 1st arg (see the above example), 
;; so if the `applicability` traversal is same, then trie-full-generic-arithmetic will avoid redundancy.

;; Intuitively, we can think of full-generic-arithmetic as one tree with no branches except for the root.
;; So it have redundancy.
;; trie-full-generic-arithmetic will combine same predicates at each level.
(install-arithmetic! trie-full-generic-arithmetic)
(with-predicate-counts (lambda () (+ 'a 'b)))
(install-arithmetic! full-generic-arithmetic)
(with-predicate-counts (lambda () (+ 'a 'b)))