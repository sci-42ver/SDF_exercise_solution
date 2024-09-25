(cd "~/SICP_SDF/exercise_codes/SICP")
(load "lib.scm")

(assert-throws
  ((named-lambda (fib x) 
                 (cond 
                   ((<= x 1) x)
                   (else (+ (fib (- x 1)) (fib (- x 2))))))
   4))
