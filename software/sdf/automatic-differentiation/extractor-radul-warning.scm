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

;;; Complicated extractor, from scmutils.  Fixes Radul bug.
;;; Simplified, and API changed, by work with Sam Ritchie.

;;; extract-dx-part must be generic to handle compound values.

(define (extract-dx-default value dx)
  0)

(define extract-dx-part
  (simple-generic-procedure 'extract-dx-part 2
                            extract-dx-default))

(define (extract-dx-differential-1 value dx)
  ;; SKIPPED SDF_exercises TODO: since saved-total-index doesn't define it, extract-dx-coefficient-from is just one concept?
  ;; The following extract-dx-differential just extracts from all diff-terms and then checks whether `(memv dx factors)` to extract from the appropriate terms.
  (extract-dx-coefficient-from (infinitesimal-part value) dx))

(define *active-tags* '())

;; Based on the following usage and Exercise 3.11, it may mean this dx tag is already used in the current env.
(define (tag-active? tag)
  (memq tag *active-tags*))

(define (with-active-tag tag f args)
  ;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Dynamic-Binding.html#index-fluid_002dlet
  ;; > Unlike let, fluid-let creates no new bindings;
  ;; IMHO it changes the global value.
  (fluid-let ((*active-tags*
               (cons tag *active-tags*)))
    (apply f args)))

(define (extract-dx-differential value dx)
  ;; This is not contained in the book.
  ;; commented out since IMHO this conflicts with extract-dx-function checking `tag-active?`.
  ; (if (tag-active? dx)
  ;     (warn "Entering Radul Territory" *active-tags*))

  ; (display "extract-dx-differential")
  ; (display value)
  (let ((dx-diff-terms
         (filter-map
          (lambda (term)
            (let ((factors (diff-factors term)))
              (and (memv dx factors)
                   (make-diff-term
                    (diff-coefficient term)
                    (delv dx factors)))))
          (diff-terms value))))
    ; (bkpt "test" dx-diff-terms)
    ; (display dx-diff-terms)

    ;; this part similar to make-differential.
    (cond ((null? dx-diff-terms) 0)
          ((and (null? (cdr dx-diff-terms))
                (null? (diff-factors (car dx-diff-terms))))
           (diff-coefficient (car dx-diff-terms)))
          (else
           (make-differential dx-diff-terms)))))

;; coderef: extract-dx-part:differential
(define-generic-procedure-handler extract-dx-part
  (match-args differential? diff-factor?)
  extract-dx-differential)

#|
(define (extract-dx-function fn dx)
  (lambda args
    (extract-dx-part (with-active-tag dx fn args)
                     dx)))
|#

(define (extract-dx-function fn dx)
  ; (display *active-tags*)
  (display "extract-dx-function")
  ; (display (tag-active? dx))
  (lambda args
    ; (newline)
    ; (display args)
    ; (newline)
    (if (tag-active? dx)
        (let ((eps (make-new-dx)))
          ; (display "tag active")
          ;; diff-factor? diff-factor? 
          (replace-dx dx eps
                      ;; Here (tag-active? dx) is #t, so if this call `extract-dx-differential`, it will show warning.
                      (extract-dx-part
                        ;; Take "In a case where a function is returned, as in" as the example
                        ;; If we do (d:* (+ (u '()) (1 dx1)) (+ v dx1)) (here I only give the detailed representation in the 1st arg) where dx2 becomes dx1.
                        ;; Then we will get extract-dx-part in the 2nd derivative to be (+ u v) (dx1^2 becomes 0).
                        ;; The correct should be (+ u dx1).

                        ;; The above may be due to 
                        ;; > using the tag that was created for the outer derivative calculation
                       (with-active-tag dx fn
                                        (map (lambda (arg)
                                               (replace-dx eps dx arg))
                                             args))
                       dx)))
        (extract-dx-part (with-active-tag dx fn args)
                         dx))))



;; coderef: extract-dx-part:function
(define-generic-procedure-handler extract-dx-part
  (match-args function? diff-factor?)
  extract-dx-function)

;;; Below is old code
#|
;; coderef: extract-dx-function-wrong
(define (extract-dx-function fn dx)
  (lambda args
    (extract-dx-part (apply fn args) dx)))


(define (extract-dx-function fn dx)
  (lambda args
    (let ((eps (make-new-dx)))
       (replace-dx dx eps
        (extract-dx-part
         (apply fn
           (map (lambda (arg)
                  (replace-dx eps dx arg))
                args))
         dx)))))
|#

(define (replace-dx-default new-dx old-dx object)
  (if (not (and (diff-factor? new-dx)
                (diff-factor? old-dx)))
      (error "Bad args to REPLACE-DX"
             new-dx old-dx object))
  object)

(define replace-dx
  (simple-generic-procedure 'replace-dx 3
                            replace-dx-default))


;; I won't dig into another bug patch skip-due-to-general-strategy-target

;;; This has a bug, reported by Sam Richie <sritchie09@gmail.com>
;;;  see replace-dx-differential.scm for a better version.
(define (replace-dx-differential new-dx old-dx object)
  (make-differential
    ;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Miscellaneous-List-Operations.html#index-sort
    ;; > So, for example, if the elements of sequence are numbers, and procedure is <, then the resulting elements are sorted in monotonically nondecreasing order.
    ;; Here the former diff-term>? the latter. 
   (sort (map (lambda (term)
                (make-diff-term
                 (diff-coefficient term)
                 ;; See (*diff-terms x y) for why to use diff-factor>?.
                 (sort (substitute new-dx old-dx
                         (diff-factors term))
                       diff-factor>?)))
              (diff-terms object))
         diff-term>?)))

;; coderef: replace-dx:differential
(define-generic-procedure-handler replace-dx
  (match-args diff-factor? diff-factor? differential?)
  replace-dx-differential)


(define (replace-dx-function new-dx old-dx fn)
  (lambda args
    (let ((eps (make-new-dx)))
      (replace-dx old-dx eps
        (replace-dx new-dx old-dx
          (apply fn
            (map (lambda (arg)
                    ;; Here we won't replace args.
                   (replace-dx eps old-dx arg))
                 args)))))))

;; coderef: replace-dx:function
(define-generic-procedure-handler replace-dx
  (match-args diff-factor? diff-factor? function?)
  replace-dx-function)

;;; Simple substitution
(define (substitute new old x)
  (cond ((pair? x)
         (cons (substitute new old (car x))
               (substitute new old (cdr x))))
        ;; See Exercise 3.12 where replace-dx-vector is similar to what here does.
        ;; But due to convenience, I use `replace-dx` instead of substitute since the former is will do something like sort.
        ((vector? x)
         (make-initialized-vector (vector-length x)
           (lambda (i)
             (substitute new old
                         (vector-ref x i)))))
        ;; The latter 2 are bases.
        ((eqv? old x) new)
        (else x)))
