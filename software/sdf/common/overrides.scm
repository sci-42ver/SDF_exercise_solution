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

(let-syntax
    ((define-override
       (er-macro-transformer
        ;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Explicit-Renaming.html#index-er_002dmacro_002dtransformer
        ;; > The expression is expanded in the syntactic environment of the er-macro-transformer expression, and the expanded expression is evaluated in the transformer environment to yield a macro transformer as described below.
        ;; as the normal lambda expression evaluation (here expand just means that expansion by macro).
        (lambda (form rename compare)
          ;; > The expression returned by the transformation procedure will be expanded in the syntactic environment obtained from the syntactic environment of the macro application 
          ;; i.e. the following (define-override *) etc to ensure rename can work.
          ;; > by binding any fresh identifiers generated by the renaming procedure to the denotations of the original identifiers in the syntactic environment in which the macro was *defined*.
          ;; get lambda from definition syntactic environment for lexical scope.
          ;; Trivially at least define etc are not redefined before, so just the normal usage for them.
          `(,(rename 'define)
            ;; no need to rename here since define will recognize this as one symbol always.
            ,(symbol 'n: (cadr form))
            (,(rename 'access)
             ;; 0. Assume * etc are not redefined before. So no rename.
             ;; 1. Similarly, this must be symbol, so no need to rename.
             ,(cadr form)
             ,(rename 'system-global-environment)))))))
  (define-override *)
  (define-override +)
  (define-override -)
  (define-override /)
  (define-override <)
  (define-override <=)
  (define-override =)
  (define-override >)
  (define-override >=)
  (define-override abs)
  (define-override acos)
  (define-override angle)
  (define-override asin)
  (define-override atan)
  (define-override boolean?)
  (define-override ceiling)
  (define-override cell?)
  (define-override complex?)
  (define-override cos)
  (define-override exact-integer?)
  (define-override exact-nonnegative-integer?)
  (define-override exact-positive-integer?)
  (define-override exact-rational?)
  (define-override exp)
  (define-override expt)
  (define-override floor)
  (define-override imag-part)
  (define-override integer?)
  (define-override list?)
  (define-override log)
  (define-override magnitude)
  (define-override make-bundle-predicate)
  (define-override make-cell)
  (define-override make-polar)
  (define-override make-rectangular)
  (define-override max)
  (define-override min)
  (define-override negative?)
  (define-override non-empty-list?)
  (define-override null?)
  (define-override number?)
  (define-override pair?)
  (define-override positive?)
  (define-override pp)
  (define-override pretty-print)
  (define-override procedure?)
  (define-override rational?)
  (define-override real-part)
  (define-override real?)
  (define-override remainder)
  (define-override round)
  (define-override sin)
  (define-override sqrt)
  (define-override square)
  (define-override string?)
  (define-override symbol?)
  (define-override tan)
  (define-override truncate)
  (define-override vector?)
  (define-override zero?))

(define (n:sign n)
  (guarantee n:real? n 'n:sign)
  (cond ((n:positive? n) +1)
        ((n:negative? n) -1)
        (else 0)))

(define (n:negate x)
  (n:- 0 x))

(define (n:invert x)
  (n:/ 1 x))