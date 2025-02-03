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

;;; This is the literal function mechanism

(define (literal-function fexp)
  (define (the-function . args)
    (if (any differential? args) ; See make-differential.
        ;; Think of f(x+Dx,y+Dy,z+Dz) where Dx is general which can be dx or dx*dy, etc.
        ;; Then maximal-factor will only choose one.
        ;; Assume it chooses Dx, then fxs is f(x,y+Dy,z+Dz) (NOTICE this will again call the-function -> `(any differential? args)`. Based on induction, assume it returns right.)
        ;; Here `(map d:* partials deltargs)` will do the Dx part since all the other `deltargs` are 0's.
        (let ((n (length args))
              (factor (apply maximal-factor args)))
          (let ((realargs
                 (map (lambda (arg)
                        (finite-part arg factor))
                      args))
                (deltargs
                 (map (lambda (arg)
                        (infinitesimal-part arg factor))
                      args)))
            (let ((fxs (apply the-function realargs))
                  ;; For Dx, it will get f_x(x,y+Dy,z+Dz) where f_x is ((partial 0) f).
                  (partials
                   (map (lambda (i)
                          (apply (literal-function
                                  (deriv-expr i n fexp))
                                 realargs))
                        (iota n))))
              (fold d:+ fxs
                (map d:* partials deltargs)))))
        ;; 1. See `((literal-function 'f) 'x)` in examples.scm
        ;; 2. See ``(,@(list 1 2))` compared with ``(,(list 1 2))`
        ;; 3. This does the same as `(cons name args)` in 3.1.4.
        `(,fexp ,@args)))
  the-function)

(define (deriv-expr i n fexp)
  (if (= n 1)
      `(derivative ,fexp)
      `((partial ,i) ,fexp)))
