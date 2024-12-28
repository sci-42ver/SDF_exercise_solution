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

;;;; Arithmetic abstraction

(define-record-type <arithmetic>
    (%make-arithmetic name bases domain-predicate constant-alist
                      operation-alist)
    arithmetic?
  (name arithmetic-name)
  (bases arithmetic-bases) ; never used here
  (domain-predicate arithmetic-domain-predicate)
  (constant-alist arithmetic-constant-alist)
  (operation-alist arithmetic-operation-alist))

;; > has a name that is useful for debugging.
;; here name is just to debug by `(cons name (map arithmetic-name bases)`.
(define (make-arithmetic name
                         domain-predicate
                         bases
                         get-constant ; as its naming, this is the func to return the constant values for one `name`.
                         get-operation)
  (guarantee predicate? domain-predicate)
  (guarantee-list-of arithmetic? bases)
  (%make-arithmetic
   (cons name (map arithmetic-name bases))
   bases
   domain-predicate
   ;; TODO(cph): Eliding these calls when the number of results
   ;; doesn't equal the number of bases is arbitrary and should
   ;; be reconsidered.
   (filter-map (lambda (name)
                 (let ((base-constants
                        (arithmetic-constants-for name bases)))
                    ;; if no bases, then both are nil.
                   (and (n:= (length bases)
                             (length base-constants))
                        (cons name
                              (apply get-constant
                                     name
                                     base-constants)))))
               (arithmetic-constant-names-for bases))
   (filter-map (lambda (operator)
                 (let ((base-operations
                        (arithmetic-operations-for operator
                                                   bases)))
                   (and (n:= (length bases)
                             (length base-operations))
                        (cons operator
                              (apply get-operation
                                     operator
                                     base-operations)))))
               (arithmetic-operators-for bases))))

(define (arithmetic-constant-names-for bases)
  (if (n:pair? bases)
      ;; eq? as the 1st arg https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Procedure-Operations.html#index-apply.
      (apply lset-union eq?
             (map arithmetic-constant-names bases))
      ;; default: '(additive-identity multiplicative-identity)
      (constant-names)))

(define (arithmetic-constants-for name bases)
  (remove default-object?
          (map (lambda (base)
                 (find-arithmetic-constant name base))
               bases)))

(define (arithmetic-operators-for bases)
  (if (n:pair? bases)
      (apply lset-union eq?
             (map arithmetic-operators bases))
      (operator-names)))

(define (arithmetic-operations-for operator bases)
  (filter-map (lambda (base)
                (find-arithmetic-operation operator base))
              bases))

(define (arithmetic-constant-names arithmetic)
  (map car (arithmetic-constant-alist arithmetic)))

(define (arithmetic-constant name arithmetic)
  (let ((constant (find-arithmetic-constant name arithmetic)))
    (if (default-object? constant)
        (error "Unknown constant name:" name arithmetic))
    constant))

;; For use only by generic arithmetic.
(define (arithmetic-constant-binding name arithmetic)
  (let ((binding
         (assq name (arithmetic-constant-alist arithmetic))))
    (if (not binding)
        (error "Unknown constant name:" name arithmetic))
    binding))

(define (find-arithmetic-constant name arithmetic)
  (let ((p (assq name (arithmetic-constant-alist arithmetic))))
    (if p
        (cdr p)
        (default-object))))

(define (arithmetic-operators arithmetic)
  (map car (arithmetic-operation-alist arithmetic)))

(define (arithmetic-operation operator arithmetic)
  (let ((operation
         (find-arithmetic-operation operator arithmetic)))
    (if (not operation)
        (error "Unknown operator:" operator))
    operation))

(define (arithmetic-procedure operator arithmetic)
  (operation-procedure
   (arithmetic-operation operator arithmetic)))

(define (find-arithmetic-operation operator arithmetic)
  ;; may return #f
  (let ((p (assq operator
                 (arithmetic-operation-alist arithmetic))))
    (and p
         (cdr p))))

(define (add-arithmetics . arithmetics)
  (add-arithmetics* arithmetics))

(define (add-arithmetics* arithmetics)
  (if (n:null? (cdr arithmetics))
      (car arithmetics)
      (make-arithmetic 'add
                       (disjoin*
                        ;; This is used when functioning as the base arithmetic but not used for install-arithmetic!.
                        (map arithmetic-domain-predicate
                             arithmetics))
                       arithmetics
                       constant-union
                       ;; at last, each operator will have *one* operation which may be union of operations by `operation-union-dispatch`.
                       ;; Here operations are ordered by arithmetics (see arithmetic-operations-for). So the left arithmetic is prioritized.
                       operation-union)))

;; base-arithmetic are prioritized in operation-union.
(define (extend-arithmetic extender base-arithmetic)
  (add-arithmetics base-arithmetic (extender base-arithmetic)))

;;;; Installation

(define *current-arithmetic* #f)

(define (install-arithmetic! arithmetic)
  (set! *current-arithmetic* arithmetic)
  (install-package! (arithmetic->package arithmetic)))

(define (with-arithmetic arithmetic thunk)
  (with-installed-package! (arithmetic->package arithmetic)
                           thunk))

;; Notice here we don't override all operators.
;; here actually we doesn't have negate operator *installed*.
(define (arithmetic->package arithmetic)
  (make-package (arithmetic-name arithmetic)
    (arithmetic->bindings arithmetic
                          ;; return arithmetic defined operations
                          ;; I only checked the former 4 cases when finish reading 3.1
                          (+-like '+ 'additive-identity)
                          (--like '- 'negate)
                          (+-like '* 'multiplicative-identity)
                          (--like '/ 'invert)
                          (comparator '<)
                          (comparator '=)
                          (comparator '>)
                          (comparator '<=)
                          (comparator '>=)
                          (min-like 'min)
                          (min-like 'max))))

(define (arithmetic->bindings arithmetic . modifications)
  (let ((overrides
         (filter-map (lambda (modification)
                       (and modification
                            ;; this may return #f
                            (modification arithmetic)))
                     modifications)))
    (map (lambda (operator)
            ; (display 
            ;   (list 
            ;     operator
            ;     (generic-procedure?
            ;       (make-installable-procedure operator
            ;                                  arithmetic
            ;                                  overrides))))
            
            ;; > the installer will bind the name to the procedure.
           (cons operator
                 (make-installable-procedure operator
                                             arithmetic
                                             overrides)))
         (filter operator-installable?
                 (arithmetic-operators arithmetic)))))

(define arithmetic-procedure?)
(define arithmetic-procedure-metadata)
(define set-arithmetic-procedure-metadata!)
(let ((association (make-metadata-association)))
  (set! arithmetic-procedure? (association 'has?))
  (set! arithmetic-procedure-metadata (association 'get))
  (set! set-arithmetic-procedure-metadata! (association 'put!)))

;; overrides is something like (cons operator ...) in +-like.
(define (make-installable-procedure operator arithmetic
                                    overrides)
  (let* ((operation
          (arithmetic-operation operator arithmetic))
         (procedure (operation-procedure operation)))
    (let ((override
           (and (not (eqv? (get-implementation-value operator)
                           procedure))
                (assq operator overrides))))
      (if override
          ;; either return modifed `operation-procedure` by override or return `(get-implementation-value operator)`
          (let ((procedure*
                  ;; just return (cdr override)
                 (make-installable-operation-procedure
                  procedure
                  (cdr override))))
            (set-arithmetic-procedure-metadata! procedure*
                                                procedure)
            procedure*)
          procedure))))

;; checked
;; compared with arithmetic original operation, we adds identity related proc.
(define (+-like operator identity-name)
  (lambda (arithmetic)
    (let ((binary-operation
           (find-arithmetic-operation operator arithmetic)))
      (and binary-operation
           (let ((binary
                  (operation-procedure binary-operation))
                 (get-identity
                  ;; this just return constant which is unchanged when re-call.
                  (identity-name->getter identity-name
                                         arithmetic)))
             (cons operator
                   (lambda args
                     (case (length args)
                       ((0) (get-identity))
                       ((1) (car args))
                       (else (pairwise binary args))))))))))

(define (identity-name->getter identity arithmetic)
  (let ((constant
         (find-arithmetic-constant identity arithmetic)))
    (if (default-object? constant)
        (lambda ()
          (error "No identity for this arithmetic:" identity))
        (lambda ()
          constant))))

;; See `numeric-arithmetic` where negate is just -, so `unary` and `binary` are same here.
(define (--like operator inversion-operator)
  (lambda (arithmetic)
    (let ((binary-operation
           (find-arithmetic-operation operator arithmetic))
          (unary-operation
           (find-arithmetic-operation inversion-operator
                                      arithmetic)))
      (and binary-operation
           unary-operation
           (let ((binary
                  (operation-procedure binary-operation))
                 (unary
                  (operation-procedure unary-operation)))
             (cons operator
                   (lambda (arg . args)
                     (if (n:null? args)
                         (unary arg)
                         (pairwise binary
                                   (cons arg args))))))))))

(define (comparator operator)
  (lambda (arithmetic)
    (let ((operation
           (find-arithmetic-operation operator arithmetic)))
      (and operation
           (let ((binary
                  (operation-procedure operation)))
             (cons operator
                   (lambda args
                     (or (n:< (length args) 2)
                         (let loop ((args args))
                           (and (binary (car args) (cadr args))
                                (or (not (n:pair? (cddr args)))
                                    (loop (cdr args)))))))))))))

(define (min-like operator)
  (lambda (arithmetic)
    (let ((operation
           (find-arithmetic-operation operator arithmetic)))
      (and operation
           (let ((binary (operation-procedure operation)))
             (cons operator
                   (lambda (arg . args)
                     (if (n:null? args)
                         arg
                         (pairwise binary
                                   (cons arg args))))))))))

(define (pairwise binary args)
  (let loop
      ;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Iteration.html#index-let-6
      ;; > Named let has the same syntax and semantics as ordinary let
      ;; So here `(car args)` corresponds to the original args.
      ((args (cddr args))
       (result (binary (car args) (cadr args))))
    (if (n:null? args)
        result
        (loop (cdr args)
              ;; IGNORE: SDF_exercises TODO here seems to accumulate `(car args)` twice?
              ;; This args are `(cddr args)` of the original args.
              (binary result (car args))))))