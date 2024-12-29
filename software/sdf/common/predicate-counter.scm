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

;;;; Log number of uses of registered predicates

(define %predicate-counts
  ;; IGNORE: TODO call this will always return one already defined value for `(%predicate-counts)`?
  ;; See https://srfi.schemers.org/srfi-39/srfi-39.html
  ;; > When it is called with no argument, the content of the cell bound to this parameter object *in the current dynamic environment* is returned.
  (make-parameter (make-key-weak-eqv-hash-table)))

(define (reset-predicate-counts!)
  (hash-table-clear! (%predicate-counts)))

(reset-predicate-counts!)

(define (increment-predicate-count! predicate)
  ;; see https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Basic-Hash-Table-Operations.html#index-hash_002dtable_002dupdate_0021
  ;; > Applies procedure to the datum associated with key in hash-table or to the value of calling get-default if there is no association for key
  ;; here `(lambda (count) (fix:+ count 1))` will call for count=(lambda () 1) "if there is no association for key".
  ;; So `(get-predicate-count predicate)` is either 0 or 2 without being 1.
  (hash-table-update! (%predicate-counts)
                      predicate
                      ;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Fixnum-Operations.html#index-fix_003a_002b
                      (lambda (count) (fix:+ count 1))
                      (lambda () 0) ; changed. originally (lambda () 1).
                      ))

(define (get-predicate-count predicate)
  (hash-table-ref/default (%predicate-counts) predicate 0))

(define (get-predicate-counts)
  (hash-table->alist (%predicate-counts)))

(define (with-predicate-counts thunk)
  (parameterize ((%predicate-counts (make-key-weak-eqv-hash-table)))
    (let ((value (thunk)))
      (for-each (lambda (p)
                  (write-line (list (cdr p)
                                    ;; 0. > The reason for this is obscure, and the curious are welcome to track it down in the code.
                                    ;; get metadata. See `(register-predicate! symbolic? 'symbolic)`.
                                    ;; 1. See SDF_exercises/software/sdf/efficient-generic-procedures/microbench.scm
                                    ;; for why I commented out the original codes temporarily.
                                    ; (or (predicate-name (car p))
                                    ;     (car p))
                                    (car p)
                                    )
                              (notification-output-port)))
                (get-predicate-counts))
      value)))
