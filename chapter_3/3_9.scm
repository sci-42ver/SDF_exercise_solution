(load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
(manage 'new 'automatic-differentiation)

;; IGNORE: test mere generic procedure without using arithmetic
;; See automatic-differentiation/utils.scm we have already full-arithmetic.
; (define make-generic-procedure (generic-procedure-constructor make-simple-dispatch-store))
; (define atan1 
;   (make-generic-procedure 'atan1 1 #f))

;; a
(define diff:tan
  (diff:unary-proc tan
                  ;; https://www.afralisp.net/archive/lisp/bulge.htm https://www.cuemath.com/calculus/derivative-of-tan-x/#:~:text=The%20formula%20for%20differentiation%20of,%3D%20sec2x%20(or)
                   (lambda (x) (/ 1 (expt (cos x) 2)))))
;; TODO to test
(assign-handler!  tan     diff:tan    differential?)

;; IGNORE: to make get-implementation-value work.
; (environment-define system-global-environment 'atan1 atan1)
;; IGNORE: Here we need to register atan1.
; (set! %arithmetic-operator-alist (cons '(atan1 (domain) domain) %arithmetic-operator-alist))
; (load "~/SICP_SDF/SDF_exercises/software/sdf/automatic-differentiation/utils.scm")

(define diff:atan1
  (diff:unary-proc atan
                   (lambda (x) (/ 1 (+ 1 (expt x 2))))))
;; > atan1 is a function of one argument
;; > using atan1 for one argument and atan2 if given two arguments
(assign-handler!  atan     diff:atan1    differential?)

;; b
;; IGNORE: IMHO we need 2 `operator-arity`.

;; when calling atan, we use `value` done in install-arithmetic! -> `install-package!`.
;; `value` is assigned in `make-installable-procedure`. Since atan is not overrided, `operation-procedure` is used which is defined by `arithmetic`.
;; Here arithmetic is just `generic-arithmetic`.
;; Here atan is not defined in `numerical-simplifier-wrapper`, so all are done by `add-to-generic-arithmetic!` -> ``add-generic-arith-operations!``.
;; Then `define-generic-procedure-handler` just calls `add-handler!` which doesn't influence the outer operation interface.

;; NOTICE: `operation-applicability` is only used in operation-union and "add-generic-arith-operations!" handler.
;;          So it *doesn't do any filtering*, i.e. it will always find handler.
;; IGNORE: here it doesn't filter any procedure added to operation.

;; So we can just define handler which is enough.
(define atan1
  ;; Here I explicitly show argument number although we can just define it as n:atan.
  (lambda (x) (n:atan x)))
(define atan2
  (lambda (y x) (n:atan (/ y x))))
(define diff:atan2
  (diff:unary-proc atan
                   (lambda (y x) (/ 1 (+ 1 (expt (/ y x) 2))))))

;; notice not use (list a b) since `preds` will be auto list.
;; > using atan1 for one argument and atan2 if given two arguments
(assign-handler!  atan     atan2    number? number?)
(assign-handler!  atan     atan1    number?)
;; > install an atan2 handler for differentials. Remember, it must coexist with the atan1 handler.
(assign-handler!  atan     diff:atan2    differential? differential?)

(assert (= (atan 1) (n:atan 1)))
(assert (= (atan 1 1) (n:atan 1)))