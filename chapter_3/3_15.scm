;; ;; By searching "(test-stormer-counts)" with "*.rkt,*.scm" in VSCode, code base (see 3_14.scm) and 6.945_assignment_solution are reviewed.
(load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
(manage 'new 'automatic-differentiation)

(load "generic-procedure-lib/performance-tool.scm")

(define (make-cached-dispatch-store)
  (cache-wrapped-dispatch-store (make-trie-dispatch-store)
                                implementation-type-name))

(define cache-full-generic-arithmetic
  (let ((g (make-generic-arithmetic make-cached-dispatch-store)))
    (add-to-generic-arithmetic! g numeric-arithmetic)
    (extend-generic-arithmetic! g function-extender)
    (add-to-generic-arithmetic! g
                                (symbolic-extender numeric-arithmetic))
    g))
(install-arithmetic! cache-full-generic-arithmetic)

; (<-fib-test)
; ;; same as 3.14 make-trie-dispatch-store
; ; (2 number)
; ; (1 any-object)
; ; (2 symbolic)
; ; (2 function)

; (<-fib-test) ; should 0 predicate test.

;; > make measurements for execution of (test-stormer-counts) and (fib 20) in the cached version of dispatch with the same generic arithmetics explored in exercise 3.14.
(test-stormer-counts)
;; See `make-generic-arithmetic` where each operator has `make-generic-procedure`.
;; So here + will call once more which has the same effects for predicate calls as <.
; (<+-_fib-test)
;; This is less than 3.14 with additions of + and - corresponding counts based on already having called `(<-fib-test)`.
; (4 number)
; (2 any-object)
; (4 symbolic)
; (4 function)

;; if called the first time without having called fib, just (6 number) ... very huge differences.
;; if having called (<+-_fib-test), then 0 count.
(book-fib-test)
