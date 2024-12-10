(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'term-rewriting)

(load "../software/sdf/automatic-differentiation/simplifier.scm")

;; This also includes factors which is not needed by the exercise
;; > as a *sum* of terms

;; IGNORE: compared with mine, it lacks the last rule
; (algebra-3 '(+ (* x w) (* x w)))
