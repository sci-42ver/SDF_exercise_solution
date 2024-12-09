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

(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'term-rewriting)
(load "~/SICP_SDF/SDF_exercises/software/sdf/common/testing.scm")

(define this-env (the-environment))

;; > each unsyntax or unsyntax-splicing taking away a level of quotation
;; unsyntax just unwraps.
; (pp (syntax '(rule '(* (? b) (? a))
;                                   (and (expr<? a b)
;                                         ;; ` is similar to ' except that ,a won't be quoted.
;                                         ;; So (list '* a b).
;                                        `(* ,a ,b)))
;                            this-env))

(define-test 'rule-syntax
  (lambda ()
    (assert-equal (unsyntax
                   ;; https://www.scheme.com/csug7/syntax.html
                   ;; > A syntax expression is like a quote expression except that the values of pattern variables appearing within template are inserted into template
                   ;; input just means input var's by context
                   (syntax '(rule '(* (? b) (? a))
                                  (and (expr<? a b)
                                        ;; ` is similar to ' except that ,a won't be quoted.
                                        ;; So (list '* a b).
                                       `(* ,a ,b)))
                           this-env))
                  '(make-rule '(* (? b) (? a))
                              (lambda (b a)
                                (and (expr<? a b)
                                     (list '* a b)))))))