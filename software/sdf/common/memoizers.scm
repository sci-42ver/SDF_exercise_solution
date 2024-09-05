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

;;;; Memoizers

;; For cache-wrapped-dispatch-store, it will use dispatch-store at the first time.
;; And then hash-table for the latter access (this will call *no predicate tests*. See Exercise 3.14).
(define (make-list-memoizer make-list= dedup?)
  ;; elt= is used by both comparing key list elem and comparing args elem where args are used to generate key.
  (lambda (elt= get-key get-datum)
    (let ((table (make-memoizer-table make-list= elt=)))
      (lambda (list)
        (let ((list
               (if dedup?
                   (delete-duplicates list elt=)
                   list)))
          (hash-table-intern! table
                              (get-key list)
                              (lambda () (get-datum list))))))))

(define (make-memoizer-table make-list= elt=)
  (cond ((eqv? eq? elt=)
          ;; ref1: https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Construction-of-Hash-Tables.html#index-make_002dhash_002dtable
          ;; Here make-list= is one proc, so it is not comparator https://srfi.schemers.org/srfi-128/srfi-128.html.
          ;; see https://srfi.schemers.org/srfi-69/srfi-69.html
         (make-hash-table (make-list= eq?)
                          ;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Address-Hashing.html
                          ;; > it is necessary to tell the hash-table implementation (by means of the 
                          ;; > rehash-after-gc? argument to the hash-table type constructors) that the hash numbers computed by your key-hashing procedure \
                          ;; > must be recomputed after a garbage collection.
                          (make-list-hash eq-hash)
                          ;; ref1
                          ;; > The arg arguments are allowed but are implementation dependent; do not provide them.
                          ;; SDF_exercises TODO #t meaning?
                          'rehash-after-gc? #t))
        ((eqv? eqv? elt=)
         (make-hash-table (make-list= eqv?)
                          (make-list-hash eqv-hash)
                          'rehash-after-gc? #t))
        ((eqv? equal? elt=)
         (make-hash-table (make-list= eqv?)
                          (make-list-hash equal-hash)
                          'rehash-after-gc? #t))
        (else
         (error "Don't know how to memoize this:" elt=))))

(define (make-list= elt=)
  (define (list= a b)
    (if (n:pair? a)
        (and (n:pair? b)
             (elt= (car a) (car b))
             (list= (cdr a) (cdr b)))
        (not (n:pair? b))))
  list=)

(define (make-lset= elt=)
  (define (list= a b)
    (lset= elt= a b))
  list=)

(define (make-list-hash elt-hash)
  (define (list-hash list #!optional modulus)
    (let ((hash
           (apply n:+
                  ;; elt abbr -> Element https://www.acronymfinder.com/ELT.html 
                  (map (lambda (elt)
                         (elt-hash elt))
                       list))))
      (if (default-object? modulus)
          hash
          (modulo hash modulus))))
  list-hash)

(define list-memoizer (make-list-memoizer make-list= #f))
(define lset-memoizer (make-list-memoizer make-lset= #t))

;; For cache-wrapped-dispatch-store,
;; It use arg type list as key and handler as the key by calling `(get-datum args)`.
(define (make-simple-list-memoizer list-memoizer)
  (lambda (elt= get-key get-datum)
    (let ((memoizer
           (list-memoizer elt=
                          ;; for cache-wrapped-dispatch-store, its get-key just accepts the list as the single arg.
                          ;; Same for get-datum.
                          (lambda (args)
                            (apply get-key args))
                          ;; args is list implied by make-list-memoizer and we call `(memoizer args)`.
                          (lambda (args)
                            (apply get-datum args)))))
      (lambda args
        (memoizer args)))))

(define simple-list-memoizer
  (make-simple-list-memoizer list-memoizer))

(define simple-lset-memoizer
  (make-simple-list-memoizer lset-memoizer))

;;; This is intended to weakly match a list of items, where each
;;; item is distinguished by eqv?, and ideally where the items
;;; themselves are held weakly.  This is kind of difficult to do
;;; without doing a bunch of implementation-specific hacking, so
;;; for now this is implemented as a strong hash.
(define (memoize-multi-arg-eqv procedure)
  (simple-list-memoizer eqv? list procedure))

(define (memoize-multi-arg-equal procedure)
  (simple-list-memoizer equal? list procedure))
