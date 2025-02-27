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

;;; -*- Mode:Scheme -*-

(define the-unspecified-value (list 'the-unspecified-value))

(define (true? x)
  (if x #t #f))

(define (false? x)
  (if x #f #t))

(register-predicate! true? 'true)
(register-predicate! false? 'false)

;;; Primitive procedures are inherited from Scheme.

;; different from SICP due to using lookup-scheme-value => lexical-reference. 
(define (strict-primitive-procedure? p)
  (procedure? p))
(register-predicate! strict-primitive-procedure?
                     'strict-primitive-procedure)

;; better than SICP due to keeping the underlying apply definition.
(define apply-primitive-procedure apply)

;;; Compound procedures

;;;; https://srfi.schemers.org/srfi-9/srfi-9.html
;;; implementation of predicate => (record-predicate type)
;; 0. type is also one record with tag record-marker. It then encapsulates (record-type-tag record-type record-data).
;; 1. related with record-predicate:
;; record? is based on vector which does similar to tagged-list.
;; record-type => record-ref which is again similar to tagged-list.
;; 1.a. record-constructor just constructs tagged-list 
;; (except that it is actually vector so the underlying accessor is different that it is faster for accessing constant-len data like in C) 
;; as the above shows which will satisfy (eq? (record-type thing) type).

;; so compound-procedure? (record-predicate) is similar to the SICP version but based on vector.

;;; implementation of accessor (e.g. procedure-parameters here)
;; 0. Notice here constructor-tag *must* be same as field-tag which is shown in Specification.
;; This is needed for field-index to work.
;; 1. Based on 0, if (record-predicate type) is met, just record-ref, so similar to SICP.
(define-record-type <*compound-procedure>
    (make-compound-procedure vars bproc env)
    compound-procedure?
  (vars  procedure-parameters)
  (bproc procedure-body)
  (env   procedure-environment))
(register-predicate! compound-procedure? 'compound-procedure?)

(define (procedure-printable-representation procedure)
  `(compound-procedure
    ,(procedure-parameters procedure)
    ,(procedure-body procedure)
    <procedure-environment>))

;;; An ENVIRONMENT is a chain of FRAMES, made of vectors.

(define (environment? x) #t)     ;make better!
(register-predicate! environment? 'environment)

;; added besides the book
(define (environment? x)
  ;; see extend-environment, the-empty-environment
  (or 
    (eq? x the-empty-environment)
    (and 
      (vector? x)  
      (n:= 3 (vector-length x))
      )
    )
  )

(define (extend-environment variables values base-environment)
  ;; see https://srfi.schemers.org/srfi-143/srfi-143.html for why we use fixnum
  ;; > fixnums can be represented more compactly and operated on more efficiently than bignums
  (if (not (fix:= (length variables) (length values)))
      (if (fix:< (length variables) (length values))
          (error "Too many arguments supplied" variables values)
          (error "Too few arguments supplied" variables values)))
  (vector variables values base-environment))

;; similar to SICP but uses the different structure.
(define (environment-variables env) (vector-ref env 0))
(define (environment-values env) (vector-ref env 1))
(define (environment-parent env) (vector-ref env 2))

(define the-empty-environment (list '*the-empty-environment*))

;; 0. almost same basic ideas as SICP
;; 1. > If no value is found, an Unbound variable error is signaled.
;; SDF_exercises TODO `~/SICP_SDF/SDF_exercises/software/sdf/generic-interpreter$ grep "Unbound variable" -r .`
;; only shows one record "./shared-rtdata.scm:        (error "Unbound variable -- SET!" var))".
(define (lookup-variable-value var env)
  (let plp ((env env))
    (if (eq? env the-empty-environment)
        ;; different from SICP
        (lookup-scheme-value var)
        (let scan
            ;; IGNORE SDF_exercises TODO no enclosing-environment.
            ;; 0. better to use environment-parent abstraction etc.
            ;; 1. similar to SICP but uses the different structure.
            ((vars (vector-ref env 0))
             (vals (vector-ref env 1)))
          (cond ((null? vars) (plp (vector-ref env 2)))
                ((eq? var (car vars)) (car vals))
                (else (scan (cdr vars) (cdr vals))))))))

;;; Extension to make underlying Scheme values available to
;;; interpreter

(define lookup-scheme-value
  (let ((env (the-environment)))
    (named-lambda (lookup-scheme-value var)
      ;; not in MIT_Scheme_Reference
      (lexical-reference env var))))

(define (define-variable! var val env)
  (let scan
      ((vars (vector-ref env 0))
       (vals (vector-ref env 1)))
    (cond ((null? vars)
           (vector-set! env 0 (cons var (vector-ref env 0)))
           (vector-set! env 1 (cons val (vector-ref env 1))))
          ((eq? var (car vars))
           (set-car! vals val))
          (else
           (scan (cdr vars) (cdr vals))))))

;; similar comments to lookup-variable-value
(define (set-variable-value! var val env)
  (let plp ((env env))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var))
    (let scan
        ((vars (vector-ref env 0))
         (vals (vector-ref env 1)))
      (cond ((null? vars) (plp (vector-ref env 2)))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))))

(define initial-env-bindings '())

(define (define-initial-env-binding name value)
  (let ((p (assq name initial-env-bindings)))
    (if p
        (set-cdr! p value)
        (set! initial-env-bindings
              (cons (cons name value) initial-env-bindings))))
  name)

(define (make-global-environment)
  (extend-environment (map car initial-env-bindings)
                      (map cdr initial-env-bindings)
                      the-empty-environment))

;;; run-time-data extension

(define (postpone expression environment)
  (vector 'postponed expression environment))

(define (postpone-memo expression environment)
  (vector 'postponed-memo expression environment))

(define (postponed-expression x)
  (vector-ref x 1))

(define (postponed-environment x)
  (vector-ref x 2))

(define (postponed? x)
  (and (vector? x)
       (eq? (vector-ref x 0) 'postponed)))

(define (postponed-memo? x)
  (and (vector? x)
       (eq? (vector-ref x 0) 'postponed-memo)))

(define (advanced-memo? x)
  (and (vector? x)
       (eq? (vector-ref x 0) 'continued-memo)))

(define (advance-memo! x value)
  (vector-set! x 0 'continued-memo)
  (vector-set! x 1 value)
  (vector-set! x 2 the-empty-environment))

(define (advanced-value x)
  (vector-ref x 1))

(define deferred?
  (simple-generic-procedure 'deferred? 1
    (constant-generic-procedure-handler #f)))

(define-generic-procedure-handler deferred?
  (match-args postponed?)
  (lambda (x) #t))

(define-generic-procedure-handler deferred?
  (match-args postponed-memo?)
  (lambda (x) #t))

(define-generic-procedure-handler deferred?
  (match-args advanced-memo?)
  (lambda (x) #t))