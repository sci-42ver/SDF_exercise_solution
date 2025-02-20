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

;;; Unifier testing

(define *results-from-last-test*)

;; 0. https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Lambda-Expressions.html#index-_0023_0021optional-1
;; expected-bindings verbose? are all optional
;; 1. checked briefly since this is just one testing helper.
(define (unify-test p1 p2 #!optional expected-bindings verbose?)
  (define (result-dict result) (list-ref result 0))
  (define (result-ok? result) (list-ref result 3))
  (define (result-expected? result) (list-ref result 4))

  (if (match:dict? expected-bindings)
      (let ((subst (match:dict-substitution expected-bindings)))
        (let ((p1* (subst p1))
              (p2* (subst p2)))
          (write-line (list "expected-bindings results:" p1* p2*))
          (if (not (equal? p1* p2*))
              (error "Expected dictionary is invalid:"
                     p1* p2* expected-bindings)))))

  (let ((results
         (map (lambda (dict)
                (let ((mysubst (match:dict-substitution dict)))
                  (let ((p1* (mysubst p1))
                        (p2* (mysubst p2)))
                    (let ((ok? (equal? p1* p2*))
                          (expected?
                           (and (match:dict? expected-bindings)
                                (unify:alpha-equivalent?
                                 p1*
                                 ((match:dict-substitution
                                   expected-bindings)
                                  p1)))))
                      (list dict p1* p2* ok? expected?)))))
              (unify-all p1 p2))))
    (set! *results-from-last-test* results)
    (if (and (not (default-object? verbose?))
             verbose?)
        (pp results))
    (cond ((pair? (remove result-ok? results))
           'incorrect-matches)
          ((null? results)
           (if expected-bindings
               'no-matches-but-expected-some
               'no-matches-as-expected))
          ((match:dict? expected-bindings)
           (if (any result-expected? results)
               'matches-including-expected
               'matches-excluding-expected))
          ((not expected-bindings)
           'matches-but-expected-none)
          (else
           'matches-and-expected-some))))

(define (unify-all x y)
  (let ((results '()))
    ;; IMHO no need to add one more list wrapper since unify:internal will do that.
    (unify:internal (list x) (list y)
     (match:new-dict)
     (lambda (dict)
       (set! results (cons dict results))
       #f))
    (reverse! results)))

(define (unify:alpha-equivalent? p1 p2)
  (run-matcher
    ;;; IGNORE: why (recursive-substitute '? '?? p1) is same as SDF_exercises/chapter_4/4_19_rename_dict_lib.scm
    ;; (type-variable ...) to "think of each var as one *element var*".
    ;;; 0. SKIPPED (these tests are not shown in the book) SDF_exercises TODO Here we only do substitution for one side instead of two as 4_19_rename_dict_lib does.
    ;; So why?
    ;; IMHO if we want to check matches-including-expected, then we need to ensure p1* can contain the substitution by expected-bindings.
    ;; So we should use the original one.
    ;; 0.a. Also see SDF_exercises/chapter_4/4_19_rename_dict_lib.scm for possible problems.
   (match:compile-pattern (recursive-substitute '? '?? p1))
   p2
   (lambda (dict) dict)))