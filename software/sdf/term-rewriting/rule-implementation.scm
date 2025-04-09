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

;;; A rule is a procedure constructed from a pattern and a
;;; consequent procedure (the handler).  A rule takes data that
;;; the rule may apply to and a success and failure continuation.
;;; The consequent procedure binds the variables named in the
;;; pattern.  It produces the result of the rule as a function of
;;; the values of those variables that matched the data
;;; presented.

(define (make-rule pattern handler)
  (let ((match-procedure (match:compile-pattern pattern)))
    (define (the-rule data succeed fail)
      (or (run-matcher match-procedure data
            (lambda (dict)
              (let ((result
                     (apply handler
                            (match:all-values dict))))
                (and result
                    ;; > If the match succeeds, the rule
                    ;; > evaluates its consequent in an environment in which the pattern
                    ;; > variables are bound to their matched data.
                     (succeed result
                              ;; 0. IGNORE SDF_exercises TODO when this is used
                              ;; Based on search in "sdf/**/*.scm"
                              ;; make-rule is only used by (define-syntax rule ...).
                              ;; The latter is only used by rule-simplifier in SICP_SDF/SDF_exercises/software/sdf/term-rewriting/rules.scm and SICP_SDF/SDF_exercises/software/sdf/automatic-differentiation/simplifier.scm.
                              ;; 0.a. See rule-simplifier (result fail) succeed doesn't use fail.
                              ;; 0.a.0. try-rules fail uses that passed by rule-simplifier but not succeed (similar for try-rule).
                              (lambda () #f))))))
          ;; > The system backtracks into the matcher to look for an alternative match
          ;; i.e. (per-rule (cdr rules)) which matches between rule and data
          ;; > if none are forthcoming, the rule is not applicable.
          ;; IGNORE: when nested (see (map simplify-expression expression) in )
          ;; 1. IGNORE SDF_exercises TODO IMHO should be "rules are ..."
          ;; Fine (Maybe one typo to be "rule is ..."). Anyway
          ;; > The rules are gathered in a list for the rule-simplifier procedure.
          ;; and only rule-simplifier will offer the fail to be (per-rule (cdr rules)) which does "backtrack"ing.
          (fail)))
    the-rule))

;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Explicit-Renaming.html#index-er_002dmacro_002dtransformer
;; 0. > The expression is expanded
;; IGNORE: TODO just one lambda, so what does "expand" mean?
;; Maybe this https://www.gnu.org/software/guile/docs/docs-2.2/guile-ref/Macro-Expansion.html#Macro-Expansion
;; where + is one procedure.
;; 1. transformer environment see https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/SC-Transformer-Definition.html and lecs/6.001_fall_2007_recitation/codes/rec20/amb-defined-by-syntactic-closure-macro-transformer/sc-macro-transformer-demo.scm
;; 
(define-syntax rule
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((pattern (cadr form))
           (handler-body (caddr form))
           (r-make-rule (rename 'make-rule))
           (r-lambda (rename 'lambda)))
       `(,r-make-rule ,pattern
                      (,r-lambda ,(match:pattern-names pattern)
                        ,handler-body))))))

#|
;;; Alternate implementation:

;; IMHO similar to SDF_exercises/chapter_4/tests/capture-syntactic-environment.scm
;; make-rule may be rebound in one outer lambda for rule. So it is bound based on "transformer environment". We also need to do the similar thing for that to lambda.
;; Actually special parts manipulated by rename in the above er-macro-transformer are just like those if using rsc-macro-transformer.
(define-syntax rule
  (sc-macro-transformer
   (lambda (form use-env)
     (let ((pattern (cadr form))
           (handler-body (caddr form)))
       `(make-rule
         ,(close-syntax pattern use-env)
         ,(compile-handler handler-body use-env
                           (match:pattern-names pattern)))))))

(define (compile-handler form env names)
  ;; See magic in utils.scm
  (make-lambda names env
    (lambda (env*) (close-syntax form env*))))
|#
