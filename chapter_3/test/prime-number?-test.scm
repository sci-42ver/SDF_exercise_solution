(load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
(manage 'new 'user-defined-types)

(define (slow-prime? n)
  (display "slow-prime?")
  (and (n:exact-positive-integer? n)
       (n:>= n 2)
       (let loop ((k 2))
         (or (n:> (n:square k) n)
             (and (not (n:= (n:remainder n k) 0))
                  (loop (n:+ k 1)))))))

(define prime-number?
  (simple-abstract-predicate 'prime-number slow-prime?))

(define make-prime-number
(predicate-constructor prime-number?))
(define short-list-of-primes
(list (make-prime-number 2)
(make-prime-number 7)
(make-prime-number 31)))

;; fails for `(tagged-data? object)`
(prime-number? 5)
(prime-number? 2)
;; `tag<=` -> `(eqv? tag1 tag2)`
;; will redo `slow-prime?`.
(prime-number? (car short-list-of-primes))

;; > adding some metadata to the predicates themselves:
(set-predicate<=! prime-number? exact-integer?)
;; Here both prime-number? and exact-integer? are not special tags like parametric-tag?. So `generic-tag<=` calls `false-tag<=`.
;; tag<=-cache has been cleared so we use `uncached-tag<=`.
(exact-integer? (make-prime-number 2))