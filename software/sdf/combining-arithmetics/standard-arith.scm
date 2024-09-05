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

;;;; Symbolic arithmetic

(define (symbolic? object)
  (or (symbol? object)
      (pair? object)))
(register-predicate! symbolic? 'symbolic)

;; > We will also find it useful to have a domain predicate that is true for the objects (such as functions or matrices) that a given arithmetic's operations take as arguments
;; > To allow the coercion of codomain quantities, such as numbers, to
;; > constant functions, the domain of the new function arithmetic must
;; > contain both the functions and the elements of the codomain of the
;; > functions
;; SDF_exercises TODO IMHO here we should use `(disjoin base-predicate symbolic?)` since `any-arg ...` (see vector-extender and also see the difference from pure-function-extender.)
;; If we use `(extend-arithmetic symbolic-extender numeric-arithmetic)`, then it is fine to just use symbolic?.
;; Notice this change only has effects when functioning as the base (see add-arithmetics* comment).
;; So 3_2.scm (not as base), 3_4.scm (use extend-arithmetic and not as base), 3_12.scm (see code comments and not as base), 
;; 3_13.scm (not as base), 3_15.scm (not as base).
(define (symbolic-extender base-arithmetic)
  (let ((base-predicate
          (arithmetic-domain-predicate base-arithmetic)))
    (make-arithmetic 'symbolic (disjoin base-predicate symbolic?) (list base-arithmetic)
      ;; IGNORE: TODO this will throw error for `(apply (lambda (x y) (list x y)) 2 '(x y))` but `base-constants` in `make-arithmetic` may be one list.
      ;;          make-arithmetic-1 corresponding part is even more interesting with only one arg.
      ;; See "Another difference you may have noticed ..." where we assumes one base have only one corresponding constant for each type.
      (lambda (name base-constant)
        base-constant)
      (let ((base-predicate
            (arithmetic-domain-predicate  base-arithmetic)))
        (lambda (operator base-operation)
          (make-operation operator
                          (any-arg (operator-arity operator)
                                  symbolic?
                                  base-predicate)
                          ;; define what to do with such an operator. Here we just does `(+ a b)` for `(+ ’a ’b)` 
                          (lambda args (cons operator args))))))))

(define (old-symbolic-extender base-arithmetic)
  (make-arithmetic 'symbolic symbolic? (list base-arithmetic)
    (lambda (name base-constant)
      base-constant)
    (let ((base-predicate
          (arithmetic-domain-predicate  base-arithmetic)))
      (lambda (operator base-operation)
        (make-operation operator
                        (any-arg (operator-arity operator)
                                symbolic?
                                base-predicate)
                        (lambda args (cons operator args)))))))

;;;; Function arithmetic

(define (function? object)
  (and (procedure? object)
       (not (bundle? object))))
(register-predicate! function? 'function)

(define (function-extender codomain-arithmetic)
  (let ((codomain-predicate
         (arithmetic-domain-predicate codomain-arithmetic)))
    (make-arithmetic 'function
                     (disjoin codomain-predicate function?)
                     (list codomain-arithmetic)
      (lambda (name codomain-constant)
        codomain-constant)
      (lambda (operator codomain-operation)
        (make-operation operator
                        (any-arg (operator-arity operator)
                                 function?
                                 codomain-predicate)
          (lambda things
            (lambda args
              ;; > (+ 1 (cos a))
              ;; works since "codomain-operation" allows '(cos a) as the symbolic.
              (apply-operation codomain-operation
                               (map (lambda (thing)
                                      (if (function? thing)
                                          ;; small changes
                                          (apply thing args)
                                          thing))
                                    things)))))))))

;;;; Book examples

;; here bases are nil, so `operation-alist, constant-alist` are both default `operator-names` etc. due to both (length bases) and (length base-operations) are 0 ~~both nil by `base-operations`, etc~~.
;; So `arithmetic->bindings` have nil overrides.
(define (make-arithmetic-1 name get-operation)
  ;; default to use `%arithmetic-operator-alist` operator list.
  (make-arithmetic name any-object? '()
    (lambda (name)
      (case name
        ((additive-identity) 0)
        ((multiplicative-identity) 1)
        (else (default-object))))
    (lambda (operator)
      (simple-operation operator
                        any-object? ; allow (+ ’a ’b)
                        ;; func by `(lambda args (cons operator args))`.
                        (get-operation operator)))))

(define symbolic-arithmetic-1
  (make-arithmetic-1 'symbolic
    (lambda (operator)
      (lambda args (cons operator args)))))

(define combined-arithmetic
  (extend-arithmetic symbolic-extender
                     numeric-arithmetic))

(define (literal-function name)
  (lambda args
    (cons name args)))
