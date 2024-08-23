(load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
(manage 'new 'generic-procedures)
(load "~/SICP_SDF/SDF_exercises/software/sdf/common/stormer2.scm")

(let ((g
        (make-generic-arithmetic make-simple-dispatch-store)))
  (add-to-generic-arithmetic! g numeric-arithmetic)
  (extend-generic-arithmetic! g function-extender)
  (add-to-generic-arithmetic! g
                              (symbolic-extender numeric-arithmetic))
  (install-arithmetic! g))

(pp (x 0 ((evolver (literal-function 'F) 'h stormer-2)
          (make-initial-history 't 'h 'xt 'xt-h 'xt-2h)
          2)))

(+
  (+ (* 2 (+ (+ (* 2 xt) (* -1 xt-h)) (* (/ (expt h 2) 12) (+ (+ (* 13 (f t xt)) (* -2 (f (- t h) xt-h))) (f (- t (* 2 h)) xt-2h))))) (* -1 xt))
  (*
    (/ (expt h 2) 12)
    (+ (+ (* 13 (f (+ t h) (+ (+ (* 2 xt) (* -1 xt-h)) (* (/ (expt h 2) 12) (+ (+ (* 13 (f t xt)) (* -2 (f (- t h) xt-h))) (f (- t (* 2 h)) xt-2h)))))) (* -2 (f t xt))) (f (- t h) xt-h))))
