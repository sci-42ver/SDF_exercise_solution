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

;;;; Simple predicate metadata

;; See `predicate-description` for how to use name.
(define (register-predicate! predicate name)
  ;; -> make-metadata-association
  (set-predicate-metadata! predicate name)
  predicate)

;; checked
;; Just register and then return predicate.
(define (register-compound-predicate! predicate type components)
  (register-predicate! predicate
                       (cons type
                             (map predicate-name components))))

(define predicate-name get-predicate-metadata)


;;;(define any-object? (conjoin))

(define (any-object? object) #t)
(register-predicate! any-object? 'any-object)

(register-predicate! number? 'number)
(register-predicate! symbol? 'symbol)
(register-predicate! boolean? 'boolean)
