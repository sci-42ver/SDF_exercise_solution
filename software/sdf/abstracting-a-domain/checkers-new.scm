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

(define checkers
  ;; See checkers-shared.scm
  (make-checkers generate-moves-using-rule-interpreter))

;; coderef: evolution-rule:simple-move
(define-evolution-rule 'simple-move checkers
  (lambda (pmove)
    ;; simple-move can be only done at first but not after jump.
    (if (is-pmove-empty? pmove)
        (get-simple-moves pmove) ; may return one list
        '())))

;; combine compute-next-steps and try-step
(define (get-simple-moves pmove)
  (filter-map
   (lambda (direction)
     (let ((landing
            (compute-new-position direction 1 pmove))
           (board (current-board pmove)))
       (and (is-position-on-board? landing board)
            (is-position-unoccupied? landing board)
            (finish-move (new-piece-position landing pmove)))))
   (possible-directions (current-piece pmove))))

;; coderef: evolution-rule:jump
(define-evolution-rule 'jump checkers
  (lambda (pmove)
    ;; See generate-moves-using-rule-interpreter where for each piece evolution-rule is called at most once at each iteration.
    ;; So jump after simple move is impossible since it is already marked as finished. 
    (let ((jumps (get-jumps pmove)))
      (cond ((not (null? jumps))
             jumps)
            ((is-pmove-empty? pmove)
              ;; Here return '() to make `append-map (lambda (evolution-rule)` won't have duplicate inited pmove's.
             '())
            (else
             (list (finish-move pmove)))))))

;; See `try-step`
(define (get-jumps pmove)
  (filter-map
   (lambda (direction)
     (let ((possible-jump
            (compute-new-position direction 1 pmove))
           (landing (compute-new-position direction 2 pmove))
           (board (current-board pmove)))
       (and (is-position-on-board? landing board)
            (is-position-unoccupied? landing board)
            (is-position-occupied-by-opponent? possible-jump
                                               board)
            (capture-piece-at possible-jump
                              (new-piece-position landing
                                                  pmove)))))
   (possible-directions (current-piece pmove))))

;; checked.
;; coderef: aggregate-rule:coronation
(define-aggregate-rule 'coronation checkers
  (lambda (pmoves)
    (map (lambda (pmove)
           (let ((piece (current-piece pmove)))
             (if (should-be-crowned? piece)
                 (update-piece crown-piece pmove)
                 pmove)))
         pmoves)))

;; See mandate-jumps.
;; coderef: aggregate-rule:require-jumps
(define-aggregate-rule 'require-jumps checkers
  (lambda (pmoves)
    (let ((jumps (filter captures-pieces? pmoves)))
      (if (null? jumps)
          pmoves
          jumps))))