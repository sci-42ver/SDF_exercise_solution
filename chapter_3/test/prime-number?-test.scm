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

;;; fails for `(tagged-data? object)`
(prime-number? 5)
(prime-number? 2)
; #f
(prime-number? (make-prime-number 2))
; #t
; (prime-number? (make-prime-number 4))
;Ill-formed data for prime-number: 4

;; so we need to use tagging-strategy:optional
(exact-integer? 2)
; #t

;;; `tag<=` -> `(eqv? tag1 tag2)`
;; will redo `slow-prime?`.
(prime-number? (car short-list-of-primes))

;; > adding some metadata to the predicates themselves:
(set-predicate<=! prime-number? exact-integer?)
;; Here both prime-number? and exact-integer? are not special tags like parametric-tag?. So `generic-tag<=` calls `false-tag<=`.
;; tag<=-cache has been cleared so we use `uncached-tag<=`.
(exact-integer? (make-prime-number 2))

;;; test for hash-table-clear! in set-tag<=!
;; 0. So we need to use tag<=-cache which implies cached-tag<=, then tag<=, then predicate<=, 
;; 0.a. exact-integer? is one primitive-predicate which returns predicate in tagging-strategy:optional
;; Then (tag<= (tagged-data-tag (make-prime-number 2)) tag) will return #t.
(define (set-tag<=! tag superset)
  (if (tag>= tag superset)
      (error "Not allowed to create a superset loop:"
             tag superset))
  (if (not (tag<= tag superset))
      ;; > modifies the metadata of its argument predicates
      (((tag-supersets tag) 'add-element!) superset))
  (hash-table-clear! tag<=-cache)
  )
(set-predicate<=! prime-number? exact-integer?)
(define (cached-tag<= tag1 tag2)
  (hash-table-intern! tag<=-cache
                      (cons tag1 tag2)
                      (lambda () 
                        (write-line (list "use uncached-tag<= for" tag1 tag2))
                        (uncached-tag<= tag1 tag2))))
(define prime1 (make-prime-number 2))
(exact-integer? prime1)
;; removing (hash-table-clear! tag<=-cache) won't make the test fail.
;; otherwise we have
; ("use uncached-tag<= for" #[<simple-tag> prime-number] #[<simple-tag> exact-integer])
; ("use uncached-tag<= for" #[<simple-tag> exact-integer] #[<simple-tag> exact-integer])
; ;Value: #t
