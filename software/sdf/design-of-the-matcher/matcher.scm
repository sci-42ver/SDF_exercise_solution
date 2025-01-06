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

;;;; Matcher based on match combinators, CPH/GJS style.
;;;     Idea is in Hewitt's PhD thesis (1969).

;;; There are match procedures that can be applied to data items.
;;; A match procedure either accepts or rejects the data it is
;;; applied to.  Match procedures can be combined to apply to
;;; compound data items.

;;; A match procedure takes a list containing a data item, a
;;; dictionary, and a success continuation.  The dictionary
;;; accumulates the assignments of match variables to values
;;; found in the data.  The success continuation takes two
;;; arguments: the new dictionary, and the number of items
;;; absorbed from the list by the match.  If a match procedure
;;; fails it returns #f.

;;; Primitive match procedures:

(define (match:eqv pattern-constant)
  (define (eqv-match data dictionary succeed)
    (and (pair? data)
         ;; IGNORE: Since symbol, eq? is also fine IMHO.
          ;; Anyway this is just one trivial comparison issue.
         ;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Equivalence-Predicates.html#index-eqv_003f-2
         ;; > eq?’s behavior on *numbers* and characters is implementation-dependent, but it will always return either true or false
         ;; > eq? may also behave differently from eqv? on empty vectors and empty strings.
         (eqv? (car data) pattern-constant)
         ;; > backtrack into the consequent or pattern-match part of a rule. 
         ;; When called by match:list, here "backtrack" to "pattern-match part of" the parent part.
         (succeed dictionary 1)))
  (trace eqv-match)
  eqv-match)
; (trace match:eqv)

(define (match:element variable)
  (define (element-match data dictionary succeed)
    (and (pair? data)
         (match:satisfies-restriction? variable (car data))
         (let ((binding (match:lookup variable dictionary)))
           (if binding
               ;; Use equal? since (eqv? (list 1) (list 1)) etc will fail.
               (and (equal? (match:binding-value binding)
                            (car data))
                    (succeed dictionary 1))
               (succeed (match:extend-dict variable
                                           (car data)
                                           dictionary)
                        1)))))
  element-match)

;;; Used in text only
;; just as naming, the difference from match:element is no "(match:satisfies-restriction? variable (car data))".
(define (match:element-no-restriction variable)
  (define (element-match data dictionary succeed)
    (and (pair? data)
         (let ((binding (match:lookup variable dictionary)))
           (if binding
               (and (equal? (match:binding-value binding)
                            (car data))
                    (succeed dictionary 1))
               (succeed (match:extend-dict variable
                                           (car data)
                                           dictionary)
                        1)))))
  element-match)

(define (match:segment variable)
  (define (segment-match data dictionary succeed)
    (and (list? data) ; can match null list.
         (let ((binding (match:lookup variable dictionary)))
           (if binding
               (match:segment-equal? data 
                                     (match:binding-value binding)
                                     (lambda (n)
                                       (succeed dictionary n))) 
               (let ((n (length data)))
                 (let lp ((i 0))
                  ;  (trace lp)
                  ;  (trace succeed)
                  ;  (write-line (list data dictionary))
                   (and (<= i n)
                        ;; See SDF_exercises/chapter_4/term-rewriting/trace-demo.scm
                        ;; demo-1 can only find one solution
                        ;; where (?? x) first matches no data (i.e. i=0)
                        ;; Then (?? y) will fails (i.e. succeed here returns #f) for i=0~5 (This implies backtracking)
                        ;; Then (?? y) succeeds for i= 6.
                        (or (succeed (match:extend-dict
                                      variable
                                      (list-head data i)
                                      dictionary)
                                     i)
                            (lp (+ i 1))))))))))
  ; (trace segment-match)
  segment-match)

(define (match:segment-equal? data value ok)
  (let lp ((data data) (value value) (n 0))
    (cond ((pair? value)
           (if (and (pair? data)
                    (equal? (car data) (car value)))
               (lp (cdr data) (cdr value) (+ n 1))
               #f))
          ((null? value) (ok n))
          (else #f))))

(define (match:list matchers)
  (define (list-match data dictionary succeed)
    ; (write-line (list "call list-match with matchers" matchers))
    (and (pair? data)
         (let lp ((data-list (car data))
                  (matchers matchers)
                  (dictionary dictionary))
           (cond ((pair? matchers)
                  ((car matchers)
                   ;; IGNORE: SDF_exercises TODO why always list here?
                   data-list
                   dictionary
                   (lambda (new-dictionary n)
                     ;; SDF_exercises TODO when happens
                     ;; not in book...
                     (if (> n (length data-list))
                         (error "Matcher ate too much."
                                n))
                    ;  (write-line (list "new-dictionary" new-dictionary))
                     (lp (list-tail data-list n)
                         (cdr matchers)
                         new-dictionary))))
                 ((pair? data-list) #f) ;unmatched data
                 ((null? data-list)
                  ;; 0. > backtrack into the consequent or pattern-match part of a rule. 
                  ;; "consequent"
                  ;; 1. eat one list data like (+ z w) in (+ y (+ z w)).
                  (succeed dictionary 1))
                 (else #f)))))
  ; (trace list-match)
  list-match)

;;;; Pattern syntax

(define (matcher pattern)
  (let ((match-procedure (match:compile-pattern pattern)))
    (lambda (datum)
      (run-matcher match-procedure
                   datum
                   match:bindings))))

(define (run-matcher match-procedure datum succeed)
  (match-procedure (list datum)
                   (match:new-dict)
                   (lambda (dict n) 
                    ;; (= n 1) means manipulating with one datum in (list datum)
                     (and (= n 1)
                          (succeed dict)))))

(define (print-all-matches dict)
  (pp (match:bindings dict))
  ;; by returning #f we force backtracking.
  #f)

;; trace (match:compile-pattern '(+ (? a) (+ (? b) (? c)))) with (+ y (+ z w)) in '(* (+ y (+ z w)) x).
;; see SDF_exercises/chapter_4/term-rewriting/trace-demo.scm
(define (match:compile-pattern pattern)
  (cond ((match:var? pattern)
         (case (match:var-type pattern)
           ((?) (match:element pattern))
           ((??) (match:segment pattern))
           (else (error "Unknown var type:" pattern))))
        ((list? pattern)
         (match:list (map match:compile-pattern pattern)))
        (else
          ;; IGNORE: SDF_exercises TODO what does this purpose to do?
         (match:eqv pattern))))


;;; Nice pattern inspection procedure that will be used by the
;;; pattern-directed invocation system.

;; just return pattern-names list.
(define (match:pattern-names pattern)
  (reverse
   (let loop ((pattern pattern) (names '()))
     (cond ((match:var? pattern)
            (let ((name (match:var-name pattern)))
              (if (memv name names)
                  names
                  (cons name names))))
           ((list? pattern)
            (let elt-loop ((elts pattern) (names names))
              (if (pair? elts)
                  (elt-loop (cdr elts)
                            (loop (car elts) names))
                  names)))
           (else names)))))