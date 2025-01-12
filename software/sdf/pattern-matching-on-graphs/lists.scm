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

;;; Experimental list generators

(define (list->graph list)
  (if (pair? list)
      (g:cons (car list) (list->graph (cdr list)))
      (g:null)))

(define (list->lazy-graph list)
  (if (pair? list)
      (g:cons (delay (car list))
              (delay (list->lazy-graph (cdr list))))
      (g:null)))

;;; IGNORE: IMHO this is just same as list->lazy-graph but
;; 0. not allow null list (and then also skipped the end null for each list).
;; 1. no abstraction of g:cons.
;;; See g:append! for how this is extensible.
(define (list->extensible-lazy-graph list)
  (if (not (pair? list))
      (error "Can't implement empty extensible list."))
  (let loop ((list list))
    (let ((head (make-graph-node 'pair)))
      (head 'connect! 'car (delay (car list)))
      (if (pair? (cdr list))
          (head 'connect! 'cdr (delay (loop (cdr list)))))
      head)))

(define nil
  (make-graph-node 'nil))

;; eqv? & eq? returns #t https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Equivalence-Predicates.html
(define (g:null)
  nil)

(define (g:null? object)
  (eqv? object nil))

(define (g:cons car cdr)
  (let ((pair (make-graph-node 'pair)))
    (pair 'connect! 'car car)
    (pair 'connect! 'cdr cdr)
    pair))

(define (g:car pair)
  (pair 'edge-value 'car))

(define (g:cdr pair)
  (pair 'edge-value 'cdr))

(define (g:has-cdr? pair)
  (pair 'has-edge? 'cdr))

(define (g:last-pair list)
  (if (g:has-cdr? list)
      (let ((cdr (g:cdr list)))
        (if (g:null? cdr)
            list
            (g:last-pair cdr)))
      list))

(define (g:last list)
  (g:car (g:last-pair list)))

(define (g:append! l1 l2)
  ((g:last-pair l1) 'connect! 'cdr l2))
