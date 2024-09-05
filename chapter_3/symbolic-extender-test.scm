(cd "~/SICP_SDF/SDF_exercises/chapter_3")
(load "../software/sdf/manager/load.scm")
(manage 'new 'combining-arithmetics)

;; See comment of add-arithmetics*. Here whether using (disjoin base-predicate symbolic?) doesn't influence install-arithmetic!.
(define combined-arithmetic
  (extend-arithmetic old-symbolic-extender
                     numeric-arithmetic))

(install-arithmetic! combined-arithmetic)
(+ 1 2)

(install-arithmetic! (old-symbolic-extender numeric-arithmetic))
(symbolic? 1)
;; The above +-like doesn't check operation-applicability. So just cons as symbolic-extender does although we have no (base-predicate base-predicate).
(+ 1 2)

(load "../software/sdf/combining-arithmetics/vector-arith.scm")
(install-arithmetic! (vector-extender (old-symbolic-extender numeric-arithmetic)))
(* 2 (vector 1 2))

;; trivially, here although `(match-args component-predicate vector?)` will match. But the base proc for (* 2 1) is same as (old-symbolic-extender numeric-arithmetic).
(install-arithmetic! (vector-extender (symbolic-extender numeric-arithmetic)))
(* 2 (vector 1 2))

;; When we don't have numeric-arithmetic before, the above implies we should always use extend-arithmetic if we want to use the mere numeric-arithmetic.
(install-arithmetic! (vector-extender (extend-arithmetic symbolic-extender numeric-arithmetic)))
(* 2 (vector 1 2))