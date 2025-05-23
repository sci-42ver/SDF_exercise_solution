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

;;; utils.scm -- Hairy utility functions for implementing the special
;;; rule syntax for pattern-directed invocation.
;;;

;;; This procedure was dredged from the dark recesses of Edwin.  Many
;;; computer scientists would claim that it should never have been
;;; allowed to see the light of day.

(define (procedure-bound-variables proc #!optional default-argl)
  "Returns the arg list of PROC.
   Grumbles if PROC is an undocumented primitive."
  (if (primitive-procedure? proc)
      (let ((doc-string
             (primitive-procedure-documentation proc)))
        (if doc-string
            (let ((newline
                   (string-find-next-char doc-string #\newline)))
              (if newline
                  (string-head doc-string newline)
                  doc-string))
            (string-append
             (write-to-string proc)
             " has no documentation string.")))
      (let ((code (procedure-lambda proc)))
        (if code
            (lambda-components* code
              (lambda (name required optional rest body)
                name body
                (append required
                 (if (null? optional) '() `(#!OPTIONAL ,@optional))
                 (if rest `(#!REST ,rest) '()))))
            (if (default-object? default-argl)
                "No debugging information available for this procedure."
                default-argl)))))

;;; Magic!

;; see SDF_exercises/chapter_4/tests/capture-syntactic-environment.scm
(define (make-lambda bvl use-env generate-body)
  ;; use the original lambda.
  (capture-syntactic-environment
    ;;; IGNORE IMHO since here it is using transform-env which is implied by that caller introduces no new binding when calling,
    ;; we
    ;;; Here bvl needs use-env similar to pattern, while lambda needs transform-env similar to make-rule.
    ;; use-env* is needed for bindings introduced by lambda here.
    (lambda (transform-env)
      (close-syntax
        `(,(close-syntax 'lambda transform-env)
          ,bvl
          ;; allow capture bvl
          ,(capture-syntactic-environment
            (lambda (use-env*)
              ;; IMHO no need for one extra close-syntax, see https://stackoverflow.com/q/79559737/21294350.
              (close-syntax (generate-body use-env*)
                            transform-env))))
        use-env))))
