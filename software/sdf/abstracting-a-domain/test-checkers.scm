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

; (load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
; (manage 'new 'abstracting-a-domain:factoring) ; notice not use the bare `'abstracting-a-domain` which lacks many things.
; (load "~/SICP_SDF/SDF_exercises/software/sdf/common/testing.scm")

(define-test 'checkers
  (lambda ()
    (let ((board (make-board checkers)))
      (assert-board 'black
                    (list (man 'black 0 0) (man 'black 2 0)
                          (man 'black 4 0) (man 'black 6 0)
                          (man 'black 1 1) (man 'black 3 1)
                          (man 'black 5 1) (man 'black 7 1)
                          (man 'black 0 2) (man 'black 2 2)
                          (man 'black 4 2) (man 'black 6 2)
                          (man 'red 0 0) (man 'red 2 0)
                          (man 'red 4 0) (man 'red 6 0)
                          (man 'red 1 1) (man 'red 3 1)
                          (man 'red 5 1) (man 'red 7 1)
                          (man 'red 0 2) (man 'red 2 2)
                          (man 'red 4 2) (man 'red 6 2))
                    board)
      (set! board
            (assert-moves 1
                          '(((0 . 2) (1 . 3))
                            ((2 . 2) (3 . 3))
                            ((2 . 2) (1 . 3))
                            ((4 . 2) (5 . 3))
                            ((4 . 2) (3 . 3))
                            ((6 . 2) (7 . 3))
                            ((6 . 2) (5 . 3)))
                          board))
      (assert-board 'red
                    (list (man 'black 0 0) (man 'black 2 0)
                          (man 'black 4 0) (man 'black 6 0)
                          (man 'black 1 1) (man 'black 3 1)
                          (man 'black 5 1) (man 'black 7 1)
                          (man 'black 0 2) (man 'black 3 3)
                          (man 'black 4 2) (man 'black 6 2)
                          (man 'red 0 0) (man 'red 2 0)
                          (man 'red 4 0) (man 'red 6 0)
                          (man 'red 1 1) (man 'red 3 1)
                          (man 'red 5 1) (man 'red 7 1)
                          (man 'red 0 2) (man 'red 2 2)
                          (man 'red 4 2) (man 'red 6 2))
                    board)
      (set! board
            (assert-moves 1
                          '(((0 . 2) (1 . 3))
                            ((2 . 2) (3 . 3))
                            ((2 . 2) (1 . 3))
                            ((4 . 2) (5 . 3))
                            ((4 . 2) (3 . 3))
                            ((6 . 2) (7 . 3))
                            ((6 . 2) (5 . 3)))
                          board))
      (assert-board 'black
                    (list (man 'black 0 0) (man 'black 2 0)
                          (man 'black 4 0) (man 'black 6 0)
                          (man 'black 1 1) (man 'black 3 1)
                          (man 'black 5 1) (man 'black 7 1)
                          (man 'black 0 2) (man 'black 3 3)
                          (man 'black 4 2) (man 'black 6 2)
                          (man 'red 0 0) (man 'red 2 0)
                          (man 'red 4 0) (man 'red 6 0)
                          (man 'red 1 1) (man 'red 3 1)
                          (man 'red 5 1) (man 'red 7 1)
                          (man 'red 0 2) (man 'red 3 3)
                          (man 'red 4 2) (man 'red 6 2))
                    board)
      (set! board (assert-moves 0 '(((3 . 3) (5 . 5))) board))
      (assert-board 'red
                    (list (man 'black 0 0) (man 'black 2 0)
                          (man 'black 4 0) (man 'black 6 0)
                          (man 'black 1 1) (man 'black 3 1)
                          (man 'black 5 1) (man 'black 7 1)
                          (man 'black 0 2) (man 'black 5 5)
                          (man 'black 4 2) (man 'black 6 2)
                          (man 'red 0 0) (man 'red 2 0)
                          (man 'red 4 0) (man 'red 6 0)
                          (man 'red 1 1) (man 'red 3 1)
                          (man 'red 5 1) (man 'red 7 1)
                          (man 'red 0 2)
                          (man 'red 4 2) (man 'red 6 2))
                    board)
      (set! board
            (assert-moves 1
                          '(((1 . 1) (3 . 3))
                            ((3 . 1) (1 . 3)))
                          board))
      (assert-board 'black
                    (list (man 'black 0 0) (man 'black 2 0)
                          (man 'black 4 0) (man 'black 6 0)
                          (man 'black 1 1) (man 'black 3 1)
                          (man 'black 5 1) (man 'black 7 1)
                          (man 'black 0 2)
                          (man 'black 4 2) (man 'black 6 2)
                          (man 'red 0 0) (man 'red 2 0)
                          (man 'red 4 0) (man 'red 6 0)
                          (man 'red 1 1) (man 'red 1 3)
                          (man 'red 5 1) (man 'red 7 1)
                          (man 'red 0 2)
                          (man 'red 4 2) (man 'red 6 2))
                    board)
      (set! board
            (assert-moves 5
                          '(((1 . 1) (2 . 2))
                            ((0 . 2) (1 . 3))
                            ((3 . 1) (2 . 2))
                            ((4 . 2) (5 . 3))
                            ((4 . 2) (3 . 3))
                            ((6 . 2) (7 . 3))
                            ((6 . 2) (5 . 3)))
                          board))
      (assert-board 'red
                    (list (man 'black 0 0) (man 'black 2 0)
                          (man 'black 4 0) (man 'black 6 0)
                          (man 'black 1 1) (man 'black 3 1)
                          (man 'black 5 1) (man 'black 7 1)
                          (man 'black 0 2)
                          (man 'black 4 2) (man 'black 7 3)
                          (man 'red 0 0) (man 'red 2 0)
                          (man 'red 4 0) (man 'red 6 0)
                          (man 'red 1 1) (man 'red 1 3)
                          (man 'red 5 1) (man 'red 7 1)
                          (man 'red 0 2)
                          (man 'red 4 2) (man 'red 6 2))
                    board)
      (set! board
            (assert-moves 3
                          '(((1 . 1) (2 . 2))
                            ((2 . 0) (3 . 1))
                            ((1 . 3) (2 . 4))
                            ((4 . 0) (3 . 1))
                            ((4 . 2) (5 . 3))
                            ((4 . 2) (3 . 3))
                            ((6 . 2) (7 . 3))
                            ((6 . 2) (5 . 3)))
                          board))
      (assert-board 'black
                    (list (man 'black 0 0) (man 'black 2 0)
                          (man 'black 4 0) (man 'black 6 0)
                          (man 'black 1 1) (man 'black 3 1)
                          (man 'black 5 1) (man 'black 7 1)
                          (man 'black 0 2)
                          (man 'black 4 2) (man 'black 7 3)
                          (man 'red 0 0) (man 'red 2 0)
                          (man 'red 3 1) (man 'red 6 0)
                          (man 'red 1 1) (man 'red 1 3)
                          (man 'red 5 1) (man 'red 7 1)
                          (man 'red 0 2)
                          (man 'red 4 2) (man 'red 6 2))
                    board)
      (set! board
            (assert-moves 0
                          '(((7 . 3) (5 . 5) (3 . 7)))
                          board))
      (assert-board 'red
                    (list (man 'black 0 0) (man 'black 2 0)
                          (man 'black 4 0) (man 'black 6 0)
                          (man 'black 1 1) (man 'black 3 1)
                          (man 'black 5 1) (man 'black 7 1)
                          (man 'black 0 2)
                          (man 'black 4 2) (king 'black 3 7)
                          (man 'red 0 0) (man 'red 2 0)
                          (man 'red 6 0)
                          (man 'red 1 1)
                          (man 'red 5 1) (man 'red 7 1)
                          (man 'red 0 2)
                          (man 'red 4 2) (man 'red 6 2))
                    board))))

(define (man color column row)
  (make-piece color 'man (make-coords column row)))

(define (king color column row)
  (make-piece color 'king (make-coords column row)))

(define (assert-board expected-color expected-pieces board)
  (assert-equal expected-color (current-color board))
  ;; So `initial-pieces-generator` doesn't need to care about the order.
  (assert-lset= piece=? expected-pieces (board-pieces board)))

(define (assert-moves index expected-moves board)
  ;; generate-legal-moves -> moves-generator -> generate-moves-using-rule-interpreter -> initial-pmove
  ;; So here pmove's are not remembered.
  (let ((moves (generate-legal-moves board)))
    (assert-lset= equal? expected-moves (map summarize-move moves))
    ;; board-end-turn changes the color
    (get-final-board
     (find (lambda (move)
             (equal? (list-ref expected-moves index) (summarize-move move)))
           moves))))

; (run-tests "" #t)

(define (generate-moves-using-rule-interpreter board piece-idx)
  (display (list (initial-pmove board (list-ref (current-pieces board) piece-idx))))
  (execute-rules (list (initial-pmove board (list-ref (current-pieces board) piece-idx)))
                 (get-evolution-rules (board-game board))
                 (get-aggregate-rules (board-game board))))
; (generate-moves-using-rule-interpreter (make-board checkers) 1)