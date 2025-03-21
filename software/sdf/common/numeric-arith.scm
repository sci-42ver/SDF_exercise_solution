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

;; arithmetic-constant-alist: all are (default-object), so `arithmetic-constant` throws errors.
(define numeric-arithmetic-without-constant
  (make-arithmetic 'numeric number? '()
    (lambda (name)
      (default-object))
    (lambda (operator)
      (simple-operation operator
                        number?
                        (get-implementation-value
                          ;; here negate is just -.
                          (operator->procedure-name operator))))))