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

;;;; Core of extended Scheme interpreter


;;; In Lisp languages an application is not distinguished by
;;; a special form.  Thus it must be the default.

(define (default-eval expression environment)
  (cond ((application? expression)
         ;; IGNORE SDF_exercises TODO what does "g:advance" do?
         (g:apply (g:advance
                   (g:eval (operator expression)
                           environment))
                  (operands expression)
                  environment))
        (else
         (error "Unknown expression type"
                expression))))

;;; EVAL takes an expression and an environment for the
;;; interpretation of the variables in the expression.

(define g:eval
  (simple-generic-procedure 'eval 2 default-eval))

;; SDF_exercises TODO this is similar to force-it in SICP.
;; no other define-generic-procedure-handler etc related with g:advance.
(define g:advance
  (simple-generic-procedure 'g:advance 1 (lambda (x) x)))

;; coderef: self-evaluating
(define-generic-procedure-handler g:eval
  (match-args self-evaluating? environment?)
  (lambda (expression environment)
    (declare (ignore environment))
    expression))

;; coderef: variable
(define-generic-procedure-handler g:eval
  (match-args variable? environment?)
  lookup-variable-value)

;; coderef: quoted
(define-generic-procedure-handler g:eval
  (match-args quoted? environment?)
  (lambda (expression environment)
    (text-of-quotation expression)))

;; coderef: lambda
(define-generic-procedure-handler g:eval
  (match-args lambda? environment?)
  (lambda (expression environment)
    ; (write-line "call g:eval for lambda")
    (make-compound-procedure
     (lambda-parameters expression)
     (lambda-body expression)
     environment)))

;; coderef: if
(define-generic-procedure-handler g:eval
  (match-args if? environment?)
  (lambda (expression environment)
    ;; different from SICP due to assuming using true/false in the underlying Scheme.
    (if (g:advance
         (g:eval (if-predicate expression)
               environment))
        (g:eval (if-consequent expression)
              environment)
        (g:eval (if-alternative expression)
              environment))))

;; coderef: cond
(define-generic-procedure-handler g:eval
  (match-args cond? environment?)
  (lambda (expression environment)
    (g:eval (cond->if expression) environment)))

;; coderef: let
(define-generic-procedure-handler g:eval
  (match-args let? environment?)
  (lambda (expression environment)
    (g:eval (let->combination expression) environment)))

;; coderef: begin
(define-generic-procedure-handler g:eval
  (match-args begin? environment?)
  (lambda (expression environment)
    (evaluate-sequence (begin-actions expression)
                       environment)))

(define (evaluate-sequence actions environment)
  ;; more robust than SICP
  (cond ((null? actions)
         (error "Empty sequence"))
        ;; no abstraction compared with SICP
        ((null? (cdr actions))
         (g:eval (car actions) environment))
        (else
         ;; SDF_exercises TODO why no g:advance?
         (g:eval (car actions) environment)
         (evaluate-sequence (cdr actions)
                            environment))))

;; coderef: definition
(define-generic-procedure-handler g:eval
  (match-args definition? environment?)
  (lambda (expression environment)
    ; (write-line "inside g:eval for definition")
    (define-variable! (definition-variable expression)
      (g:eval (definition-value expression) environment)
      environment)
    ;; different from SICP due to conforming to MIT/GNU Scheme convention of the return value of (define ...)
    (definition-variable expression)))

;; coderef: assignment
;; different from SICP due to the return value.
(define-generic-procedure-handler g:eval
  (match-args assignment? environment?)
  (lambda (expression environment)
    (set-variable-value! (assignment-variable expression)
      (g:eval (assignment-value expression) environment)
      environment)))

;;; Unlike the traditional Scheme APPLY, this APPLY takes
;;; three arguments: the procedure to be applied, the
;;; unevaluated operands, and the calling environment.  This
;;; arrangement enables easy implementation of applicative
;;; order, normal order, and even dynamic variables.

(define (default-apply procedure operands calling-environment)
  (declare (ignore operands calling-environment))
  (error "Unknown procedure type" procedure))

(define g:apply
  (simple-generic-procedure 'apply 3 default-apply))

;;; Strict primitives evaluate all args before applying

;; coderef: strict-primitive-procedure
(define-generic-procedure-handler g:apply
  (match-args strict-primitive-procedure?
              operands?
              environment?)
  (lambda (procedure operands calling-environment)
    (apply-primitive-procedure procedure
      (eval-operands operands calling-environment))))

;; 0. see SICP p498 for why SICP doesn't use map for list-of-arg-values etc.
(define (eval-operands operands calling-environment)
  (map (lambda (operand)
         (g:advance (g:eval operand calling-environment)))
       operands))

;;; Application generally requires extending the environment for
;;; the evaluation of the body of the procedure.  The extension
;;; gives meaning to the occurrences of the procedure formal
;;; parameters in the body by binding them to the evaluation of
;;; the corresponding operands in the calling environment.

(define (strict-compound-procedure? object)
  (and (compound-procedure?* object)
       (every symbol? (procedure-parameters object))))

;; coderef: strict-compound-procedure
(define-generic-procedure-handler g:apply
  (match-args strict-compound-procedure? operands? environment?)
  (lambda (procedure operands calling-environment)
    ;; > We can grab the formal parameter specifications
    (if (not (n:= (length (procedure-parameters procedure))
                  (length operands)))
        (error "Wrong number of operands supplied"))
    ;; > We also can extract the body of the procedure, which
    (g:eval (procedure-body procedure)
            ;; > we will pass to eval with an environment that includes the formal parameter bindings.
            (extend-environment
             (procedure-parameters procedure)
             (eval-operands operands calling-environment)
             ;; > built on the environment packaged with the procedure
             (procedure-environment* procedure)))))