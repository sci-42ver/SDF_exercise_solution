;; > But it did not allow the use of unbound variables as literal numbers.
;; See SDF_exercises/software/sdf/combining-arithmetics/test-standard-arith.scm
;; where we only consider something like 'a but not a.
;; That can be checked by
#|
# https://stackoverflow.com/a/58379307/21294350
# https://stackoverflow.com/a/2962015/21294350
[hervey_arch ~/SICP_SDF/SDF_exercises/software/sdf/combining-arithmetics]$ find . -type f -exec sed -nE 's/.*extend-arithmetic ([^ ]*).*/\1/p' {} \; | grep extender | sort -u
function-extender
function-extender-with-coercion
pure-function-extender
symbolic-extender
vector-extender
# https://stackoverflow.com/a/7451456/21294350
# https://stackoverflow.com/a/17991467/21294350
[hervey_arch ~/SICP_SDF/SDF_exercises/software/sdf/combining-arithmetics]$ find . -type f -exec awk '/extend-arithmetic/{getline; if(/arithmetic\)/){print $1}}' {} \; | sort -u
combined-arithmetic))
numeric-arithmetic))

# So here we only use 3 extenders:
# function-extender
# symbolic-extender
# vector-extender
# with 2 arithmetics where combined-arithmetic is again based on numeric-arithmetic
# So actually just one base-arithmetic

# Obviously they can't recognize *unbound* variable (if bound, predicate will work for the variable value) due to their predicates
# TODO that *unbound* variable seems to be recognized only in one customized parser (similar to SICP chapter 4 interpreter) https://stackoverflow.com/a/27227374/21294350
# function?, symbolic?, vector?, number?
(define (function? object)
  (and (procedure? object)
       (not (bundle? object))))
(define (symbolic? object)
  (or (symbol? object)
      (pair? object)))
|#

;; a
;;; > Make a generic extension to eval to allow this kind of behavior.
;; just change self-evaluating? to allow unbound variables

;; These won't work due to lacking env to check unbound?.
; (define (unbound? exp)
;   (and (variable? exp)
;     ))
; (define (self-evaluating? exp)
;   (or (number? exp)
;       (boolean? exp)
;       (string? exp)
;       ))   ; Our prompt (viz., "EVAL==> ") is a string.
; ;; re-register due to re-definition.
; (register-predicate! self-evaluating? 'self-evaluating)

(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'generic-interpreter 'combining-arithmetics)
(define lookup-scheme-value
  (let ((env (the-environment)))
    (named-lambda (lookup-scheme-value var)
      ;; Using allow-self-evaluating-symbols only here is enough to decide whether unbound-var can be var symbol.
      (if allow-self-evaluating-symbols
        ;; https://stackoverflow.com/a/23502038/21294350
        (if (environment-bound? env var)
          (lexical-reference env var)
          var)
        (lexical-reference env var))
      )))

;; > work with the numerical primitives ( +, *, - , /) it is necessary to extend their behavior as well
;; Then allow strict-primitive-procedure? (compound-procedure will at last call strict-primitive-procedure)
;; to use these unbound variables (IMHO just make them as symbol).
;; This is already done in chapter 3.
;; > ... in the underlying Scheme environment.
;; because they are primitive
;; IMHO just install-arithmetic
(install-arithmetic! combined-arithmetic)

;;; > As in chapter 3, the generic operator mechanism may be given handlers that work in the underlying Scheme system.
;;; IGNORE: TODO I don't why author says this here because this sentence has no relation with sentences before seemingly.
;; Anyway handlers won't always "work in the underlying Scheme system" because it may use the different data structure.
;; 0. IMHO this is more appropriate for the SICP chapter 4 interpreter because we define primitives `primitive-procedures` *independent* of "the underlying Scheme".
;; So that handlers based on these customized primitive-procedures may not "work in the underlying Scheme".
;; 0.a. Here we can do similarly by define-initial-env-binding although that haven't been taught in the book.
;;; TODO Maybe my understanding is wrong. Anyway "may" implies we can ignore this sentence.

; (init)
; (+ (* 2 3) (* 4 5))
; (+ (* a 3) (* 4 5))
;; results
; eval> (+ (* 2 3) (* 4 5))
; 26

; eval> (+ (* a 3) (* 4 5))
; (+ (* a 3) 20)

;; b
;; just add (define-generic-procedure-handler g:apply ...)
;; more trivial than the above
(define make-combination cons)
(define-generic-procedure-handler g:apply
  (match-args variable?
              operands?
              environment?)
  (lambda (procedure operands calling-environment)
    (declare (ignore calling-environment))
    (make-combination procedure operands)
    ))

;; > Make them contingent on the value of a user-settable variable: allow-self- evaluating-symbols.
;; see the above for usage.
(define allow-self-evaluating-symbols #t)

(init)
(+ (* 2 3) (* 4 5))
(+ (* a 3) (* 4 5))
(+ (f 3) (* 4 5))
