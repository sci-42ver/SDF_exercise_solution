;; By searching "(test-stormer-counts)" with "*.rkt,*.scm" in VSCode, code base are reviewed.
(load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
(manage 'new 'automatic-differentiation)

(load "generic-procedure-lib/performance-tool.scm")
(install-arithmetic! full-generic-arithmetic)
(book-fib-test)
(test-stormer-counts)

(install-arithmetic! trie-full-generic-arithmetic)
(book-fib-test)
;; This is same as efficient-generic-procedures/microbench.scm for "Tries (searched)" and Cache. But here I made one small modification of increment-predicate-count!.
;; But that file have no explanation of the result.
; (54726 any-object)
; (109452 function)
; (109452 number)
; (109452 symbolic)

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
(display (generic-procedure-rules +))
(generic-procedure? +)
(with-predicate-counts (lambda () 1))
(<-fib-test) ; calls for <.
; (1 any-object)
; (2 function)
; (2 number)
; (2 symbolic)

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
(<-fib-test)

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