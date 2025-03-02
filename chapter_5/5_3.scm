;; > identify a tuple of functions with a function that returns a tuple of values
;; tuple diff list https://stackoverflow.com/a/1708538/21294350, i.e. immutable vs mutable

;; > Although we had an exercise 3.2 to extend the arithmetic to vectors, those extensions did not modify the underlying language evaluator.
;; 0. exercise 3.2 doesn't allow ((vector cos sin) 0.6) at all.
;; That is done by searching "(vector " and "(#" in SDF_exercises/chapter_3/3_2.scm and SDF_exercises/software/sdf/combining-arithmetics/vector-arith.scm
;; 1. "the underlying language evaluator" -> g:eval instead of "the underlying Scheme environment" in exercise 5.1

;; > show that it interoperates with more conventional code
;; the original eval can't have (+ proc1 proc2 ...). One way to incorporate procedure is to use lambda to create compound proc (IMHO trivially this is supported due to evaluate-sequence for lambda-body).
;; Or we just use the result of (#(proc1 proc2) args ...) with other vectors using vector primitives (trivially this is supported by the underlying Scheme).

