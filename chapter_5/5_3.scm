;; > identify a tuple of functions with a function that returns a tuple of values
;; tuple diff list https://stackoverflow.com/a/1708538/21294350, i.e. immutable vs mutable
;; So vector in MIT/GNU Scheme is not one tuple

;; > Although we had an exercise 3.2 to extend the arithmetic to vectors, those extensions did not modify the underlying language evaluator.
;; 0. exercise 3.2 doesn't allow ((vector cos sin) 0.6) at all.
;; That is done by searching "(vector " and "(#" in SDF_exercises/chapter_3/3_2.scm and SDF_exercises/software/sdf/combining-arithmetics/vector-arith.scm
;; 1. "the underlying language evaluator" -> g:eval instead of "the underlying Scheme environment" in exercise 5.1
;; 2. trivially chapter 3 won't change eval in the underlying Scheme (actually also for at least section 5.1).

;; > those extensions did not modify the underlying language evaluator. This behavior needs an extension to g:apply so it can handle vectors of functions as a kind of function. Make this extension
(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'generic-interpreter)
(define-generic-procedure-handler g:apply
                                  (match-args vector?
                                              operands?
                                              environment?)
                                  (lambda (procedure operands calling-environment)
                                    ;; same as 6.945_assignment_solution/ps06/ps06.scm but using vector-map directly.
                                    (vector-map 
                                      (lambda (proc-elem) (g:apply proc-elem operands calling-environment)) 
                                      procedure)
                                    ))

;; > demonstrate it
(init)
((vector cos sin) 0.6)
((vector (lambda (num) (* num num)) sin) 0.6)

;; > show that it interoperates with more conventional code
;; the original eval can't have (+ proc1 proc2 ...). 
;; One way to incorporate procedure is to use lambda to create compound proc (IMHO trivially this is supported due to evaluate-sequence for lambda-body).
;; Or we just use the result of (#(proc1 proc2) args ...) with other vectors using vector primitives (trivially this is supported by the underlying Scheme).
;; The 1st case at last will be reduced to the 2nd.

;; fail just as Exercise 5.5 shows.
; (vector-map (lambda (num) (n:+ num 1)) ((vector cos sin) 0.6))
(vector-length ((vector cos sin) 0.6))


