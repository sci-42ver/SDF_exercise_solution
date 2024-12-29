#| -*-Scheme-*-

Copyright (C) 2019, 2020, 2021 Chris Hanson and Gerald Jay Sussman

This file is part of SDF.  SDF is software supporting the book
"Software Design for Flexibility", by Chris Hanson and Gerald Jay
Sussman.

SDF is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SDF is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with SDF.  If not, see <https://www.gnu.org/licenses/>.

|#

(cd "~/SICP_SDF/SDF_exercises/")
(load "software/sdf/manager/load.scm")
(manage 'new 'efficient-generic-procedures:trie)
(load "software/sdf/common/stormer2.scm")

(define (run-test test)
  (test)                                ;warm up
  (let loop ((n 3) (time 0))
    (if (= n 0)
        (/ time 3.)
        (begin
          (gc-flip)
          (let ((increment))
            (with-timings test
              (lambda (scheme-time gc-time real-time)
                (set! increment scheme-time)
                (write-line (list scheme-time gc-time real-time))))
            (loop (- n 1)
                  (+ time increment)))))))

(define (run-arith-test arithmetic test)
  (with-arithmetic arithmetic (lambda () (run-test test))))

(define (microbench-full-generic-arith dispatcher)
  (let ((g (make-generic-arithmetic dispatcher)))
    ;; 0. define-generic-procedure-handler->add-handler! is *first* called here.
    ;; The above make-generic-arithmetic just creates that generic-procedure.
    ;; So number? is added before any-object?.
    (add-to-generic-arithmetic! g numeric-arithmetic)
    (extend-generic-arithmetic! g function-extender)
    (add-to-generic-arithmetic! g (symbolic-extender numeric-arithmetic))
    g))

(define default-microbench-full-generic-arith
  (microbench-full-generic-arith make-default-dispatch-store))

(define (test-one-arg)
  (let ((g ((generic-procedure-constructor make-default-dispatch-store) 'g 1 #f)))
    (define-generic-procedure-handler g (match-args number?)
      (lambda (x)
        x))
    (do ((i 0 (fix:+ i 1)))
        ((fix:= i 1000000))
      (g 13))))

(define (test-small-+)
  (let ((g ((generic-procedure-constructor make-default-dispatch-store) 'g 2 #f)))
    (define-generic-procedure-handler g (match-args number? number?)
      (lambda (x y)
        (fix:+ x y)))
    (do ((i 0 (fix:+ i 1)))
        ((fix:= i 1000000))
      (g 13 17))))

(define (test-full-+)
  (do ((i 0 (fix:+ i 1)))
      ((fix:= i 1000000))
    (+ 13 17)))

(define (test-stormer-crunch)
  (define (F t x) (- x))
  (define numeric-s0
    (make-initial-history 0 .01 (sin 0) (sin -.01) (sin -.02)))
  (let ((e (evolver F 'h stormer-2)))
    (do ((i 0 (fix:+ i 1)))
        ((fix:= i 100000))
      (x 0 (e numeric-s0 1)))))

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1))
         (fib (- n 2)))))

(define (test-fib)
  (do ((i 0 (fix:+ i 1)))
      ((fix:= i 100))
    (fib 20)))

(define (test-fib-counts)
  (with-predicate-counts (lambda () (fib 20))))

(define (memoized-register-compound-predicate! predicate type components)
  (let ((key (cons type (map predicate-name components))))
    (hash-table-intern! memoized-compound-predicate-keys
                        key
                        (lambda ()
                          (register-predicate! predicate key)
                          predicate))))

(define memoized-compound-predicate-keys
  (make-equal-hash-table))

;; (set! register-compound-predicate! memoized-register-compound-predicate!)

;;; IGNORE: SDF_exercises TODO I don't know how the following "Rule lists" is got.
;; Rule list is the normal make-simple-dispatch-store.
;;; IGNORE: When run, no results with 'procedure? "uproc"'
;; #[compound-procedure 95 symbolic?] etc are shown when using mere car in with-predicate-counts.
(set! make-default-dispatch-store make-simple-dispatch-store)
(define default-microbench-full-generic-arith
  (microbench-full-generic-arith make-default-dispatch-store))
; (install-arithmetic! default-microbench-full-generic-arith)
; (test-fib-counts)

; ;;; 0. "filter" and "search" are used by get-a-value-by-filtering etc.
; ;; 1. Here BFS should still use all predicates before getting one cand, while DFS may not.

(define get-a-value-old get-a-value)
; ;; Tries (filtered)
; (define (get-a-value trie features)
;   ; (write-line "calls new get-a-value")
;   (get-a-value-by-filtering trie features))
; (install-arithmetic! (microbench-full-generic-arith make-trie-dispatch-store))
; (test-fib-counts)
; ;; Tries (searched)
; (define get-a-value get-a-value-old)
; (install-arithmetic! (microbench-full-generic-arith make-trie-dispatch-store))
; (test-fib-counts)

#|
;; Rule lists
((#[compound-procedure 96] . 54727)
 (#[compiled-procedure 53 (procedure? "uproc" #x1) #x1a #x19409c2] . 109453)
 (#[compiled-procedure 43 (complex:complex? "arith" #x9d) #x14 #x178657c] . 109453)
 (#[compound-procedure 95 symbolic?] . 109453)
 (#[compound-procedure 94] . 54727))

;; Tries (filtered)
((#[compiled-procedure 53 (procedure? "uproc" #x1) #x1a #x19409c2] . 109453)
 (#[compound-procedure 82] . 54727)
 (#[compound-procedure 81] . 54727)
 (#[compiled-procedure 43 (complex:complex? "arith" #x9d) #x14 #x178657c] . 109453)
 (#[compound-procedure 80 symbolic?] . 109453))

;; Tries (searched)
((#[compiled-procedure 53 (procedure? "uproc" #x1) #x1a #x19409c2] . 109453)
 (#[compound-procedure 102] . 54727)
 (#[compiled-procedure 43 (complex:complex? "arith" #x9d) #x14 #x178657c] . 109453)
 (#[compound-procedure 104] . 54727)
 (#[compound-procedure 103 symbolic?] . 109453))

;; Cache
((#[compound-procedure 90] . 4)
 (#[compound-procedure 89 symbolic?] . 7)
 (#[compiled-procedure 53 (procedure? "uproc" #x1) #x1a #x19409c2] . 7)
 (#[compound-procedure 88] . 4)
 (#[compiled-procedure 43 (complex:complex? "arith" #x9d) #x14 #x178657c] . 7))
|#

(define (test-stormer-counts)
  (define (F t x) (- x))
  ;; This will also add to %predicate-counts.
  (define numeric-s0
    (make-initial-history 0 .01 (sin 0) (sin -.01) (sin -.02)))
  (with-predicate-counts
   (lambda ()
     (let ((history ((evolver F 'h stormer-2) numeric-s0 1)))
      ; (* 2 (x 0 history))
      ; (write-line (list "after history" numeric-s0 (get-predicate-count any-object?)))
      (x 0 history)
      ))))

;; not created one new (test test-proc) due to 
;; > ;duplicate internal definitions for (#[uninterned-symbol .get-a-value.1]) in tests-for-rule-list-and-trie
(define test-proc test-stormer-counts)
(install-arithmetic! default-microbench-full-generic-arith)
(test-proc)

(define (stormer-2 F h)
  (lambda (history)
    (let ((sub-sum1 
            (+ (* 2 (x 0 history))
              (* -1 (x 1 history))))
          (sub-sum2
            (+ (* 13 (F (t 0 history) (x 0 history)))
             (* -2 (F (t 1 history) (x 1 history)))
             (F (t 2 history) (x 2 history)))
            )
          )
      (write-line (list "sub-sum1,2" (list sub-sum1 sub-sum2)))
      (+ sub-sum1
       (* (/ (expt h 2) 12)
          sub-sum2))
      )))
; (trace get-a-value)
; (trace %try-edges)
;; Tries (filtered)
(define (get-a-value trie features)
  ; (write-line "calls new get-a-value")
  (get-a-value-by-filtering trie features))
(install-arithmetic! (microbench-full-generic-arith make-trie-dispatch-store))
(test-proc)
;; Tries (searched)
(define get-a-value get-a-value-old)
(install-arithmetic! (microbench-full-generic-arith make-trie-dispatch-store))
(test-proc)
;; 24<27<29 implied every which has possibly short circuit in predicates-match?.
; (29 #[compiled-procedure ("arith" #xf0) #x1c #xb75eb4])
; (24 #[compound-procedure function?])
; (7 #[compound-procedure any-object?])
; (39 #[compound-procedure symbolic?])
;; any-object? trace
; ("rule-list check any-object? for" -2 "in (predicates args)" ((#[compound-procedure any-object?] #[compound-procedure function?]) (-2 9.999833334166664e-3)) ". now count:" 1)
; ("rule-list check any-object? for" 13 "in (predicates args)" ((#[compound-procedure any-object?] #[compound-procedure function?]) (13 0)) ". now count:" 2)
; ("rule-list check any-object? for" 0 "in (predicates args)" ((#[compound-procedure any-object?] #[compound-procedure function?]) (0 -.01999966666833333)) ". now count:" 3)
; ("rule-list check any-object? for" -.01999966666833333 "in (predicates args)" ((#[compound-procedure any-object?] #[compound-procedure function?]) (-.01999966666833333 .01999866669333308)) ". now count:" 4)
; ("rule-list check any-object? for" -1 "in (predicates args)" ((#[compound-procedure any-object?] #[compound-procedure function?]) (-1 -9.999833334166664e-3)) ". now count:" 5)
; ("rule-list check any-object? for" 2 "in (predicates args)" ((#[compound-procedure any-object?] #[compound-procedure function?]) (2 0)) ". now count:" 6)
; ("rule-list check any-object? for" 0 "in (predicates args)" ((#[compound-procedure any-object?] #[compound-procedure function?]) (0 9.999833334166664e-3)) ". now count:" 7)

;; implied by BFS, 3 same 27's.
; (27 #[compiled-procedure ("arith" #xf0) #x1c #xb75eb4])
; (27 #[compound-procedure function?])
; (12 #[compound-procedure any-object?])
; (27 #[compound-procedure symbolic?])
;; 0. at least better than BFS (for all counts), but any-object? is worse than the normal one due to the latter having also short circuit.
;; 1. 2 more any-object?'s are due to something like (+ 9.999833334166664e-3 (* (/ (expt h 2) 12) -9.999750002487318e-7)).
;; i.e. (number? symbolic?)
;; For the normal case, it will be matched with the latter added (number? symbolic?).
;; But for trie, it will try matching the 1st elem.
;; Then based on comments in microbench-full-generic-arith, we will try any-object? before number?...
; (22 #[compiled-procedure ("arith" #xf0) #x1c #xb75eb4])
; (21 #[compound-procedure function?])
; (9 #[compound-procedure any-object?])
; (27 #[compound-procedure symbolic?])

#|
;; Rule lists
((#[compound-procedure 96] . 8)
 (#[compiled-procedure 53 (procedure? "uproc" #x1) #x1a #x19409c2] . 18)
 (#[compiled-procedure 43 (complex:complex? "arith" #x9d) #x14 #x178657c] . 18)
 (#[compound-procedure 95 symbolic?] . 25)
 (#[compound-procedure 94] . 13))

;; Tries (filtered)
((#[compiled-procedure 53 (procedure? "uproc" #x1) #x1a #x19409c2] . 28)
 (#[compound-procedure 82] . 13)
 (#[compound-procedure 81] . 16)
 (#[compiled-procedure 43 (complex:complex? "arith" #x9d) #x14 #x178657c] . 25)
 (#[compound-procedure 80 symbolic?] . 28))

;; Tries (searched)
((#[compiled-procedure 53 (procedure? "uproc" #x1) #x1a #x19409c2] . 18)
 (#[compiled-procedure 43 (complex:complex? "arith" #x9d) #x14 #x178657c] . 18)
 (#[compound-procedure 104] . 16)
 (#[compound-procedure 103 symbolic?] . 26)
 (#[compound-procedure 102] . 8))

;; Cache
((#[compound-procedure 90] . 12)
 (#[compound-procedure 89 symbolic?] . 19)
 (#[compiled-procedure 53 (procedure? "uproc" #x1) #x1a #x19409c2] . 19)
 (#[compound-procedure 88] . 9)
 (#[compiled-procedure 43 (complex:complex? "arith" #x9d) #x14 #x178657c] . 16))
|#