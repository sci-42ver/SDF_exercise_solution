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

;;;; Numeric arithmetic

(define numeric-arithmetic
  (make-arithmetic 'numeric number? '()
    (lambda (name)
      (case name
        ((additive-identity) 0)
        ((multiplicative-identity) 1)
        (else (default-object))))
    (lambda (operator)
      (simple-operation operator
                        number?
                        ;; allows `(+ 1 2)` -> 3 based on `operation-union-dispatch`.
                        (get-implementation-value
                          ;; here negate is just -.
                          (operator->procedure-name operator))))))

(define generic-numeric-arithmetic
  (make-arithmetic 'numeric number? '()
    (lambda (name)
      (case name
        ((additive-identity) 0)
        ((multiplicative-identity) 1)
        (else (default-object))))
    (lambda (operator)
      (make-operation operator
                        ;; IGNORE: TODO why this any-arg will throw errors?
                        ;; See 3_4_general_vector.scm `((vector cos sin sqrt) 3)` will throw "implied by debugger which ...".
                        ;; i.e. it chooses one wrong generic proc, but `((vector cos sin sqrt) 3)` should call all primitives.
                        ;; This doesn't avoid calling primitives...
                        
                        ;; The above doesn't choose at all. It is stuck in find lp (loop). Here it just uses one wrong predicate
                        ;; since we should not use predicate list with simple-operation.

                        (any-arg (operator-arity operator)
                                 number?
                                 any-object?)
                        ; number?
                        (get-implementation-value
                          (operator->procedure-name operator))))))

;; 1. based on function-extender
;; Also see 3_4.scm
;; 2. maybe no use since `get-implementation-value` depends the calling env.
(define (numeric-extender codomain-arithmetic)
  (let ((codomain-predicate
         (arithmetic-domain-predicate codomain-arithmetic)))
    (make-arithmetic 'numeric
                     (disjoin codomain-predicate number?)
                     (list codomain-arithmetic)
      (lambda (name codomain-constant)
        (case name
          ((additive-identity) 0)
          ((multiplicative-identity) 1)
          (else (default-object))))
      (lambda (operator codomain-operation)
        (make-operation operator
                        ;; changed
                        (any-arg (operator-arity operator)
                                 number?
                                 codomain-predicate)
                        ;; IMHO This should be done in lib since only here env is right with the primitive implementation. 
                        ;; See https://stackoverflow.com/q/78908527/21294350. If `manager-env` doesn't influence `system-global-environment` and the corresponding `(environment-define environment name value)` is done in `manager-env`,
                        ;; then it is fine to use this anywhere after the arithmetic is redefined 

                        ;; when using simple-operation: debugger finds one weird proc in `get-handler`.
                        ;;     ('((#[compound-procedure any-object?] #[compiled-procedure ("arith" #xf0) #x1c #xb75eb4])
                        ;;        (#[compiled-procedure ("arith" #xf0) #x1c #xb75eb4] #[compound-procedure any-object?])
                        ;;        (#[compiled-procedure ("arith" #xf0) #x1c #xb75eb4] #[compiled-procedure ("arith" #xf0) #x1c #xb75eb4]))
                        ;;      #[compound-procedure the-generic-procedure])
                        ;; Here (pp #[compiled-procedure ("arith" #xf0) #x1c #xb75eb4]) fails. https://stackoverflow.com/a/8824620/21294350
                        (get-implementation-value
                          (operator->procedure-name operator)))))))