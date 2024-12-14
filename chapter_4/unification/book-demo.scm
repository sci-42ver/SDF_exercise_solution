(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'unification)

;; 1
; (trace procedure-type)

; (trace get-var-type)
(pp (noisy-infer-program-types '(g (< x (f y)))))
(pp (infer-program-types '(g (< x (f y)))))

;; 2
;; from book SDF_exercises/software/sdf/unification/test-type-resolver.scm
(define fact-iterative
  '(define fact
     (lambda (n)
       (begin
         (define iter
           (lambda (product counter)
             (if (> counter n)
               product
               (iter (* product counter)
                     (+ counter 1)))))
         (iter 1 1)))))

(pp (noisy-infer-program-types fact-iterative))

; (define fact-iterative-not-use-begin
;   '(define fact
;       (lambda (n)
;         (define iter
;           (lambda (product counter)
;             (if (> counter n)
;                 product
;                 (iter (* product counter)
;                       (+ counter 1)))))
;         (iter 1 1))))
; (pp (noisy-infer-program-types fact-iterative-not-use-begin))
; ((lambda (n) (define iter (lambda (product counter) (if ... product ...))) (iter 1 1)) 
;   (((fact ? fact:29) (+ type:procedure (... ...) (numeric-type)) (- type:procedure (... ...) (numeric-type)) (* type:procedure (... ...) (numeric-type)) (/ type:procedure (... ...) (numeric-type)) ...)))

;; 3
;; > But notice that the identity procedure has been typed as having a numeric argument and a numeric value
;; "argument" type is got from combination-expr? and value type is got from lambda-expr?.
;; > should not depend on its use in an example
;; So "(= (? id:29) (type:procedure ((numeric-type)) (? type:32)))" is wrong.
(pp (noisy-infer-program-types
      '(begin (define id (lambda (x) x))
              (id 2))))
