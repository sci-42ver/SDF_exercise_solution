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

;;;; Pieces

(define-record-type <piece>
    (make-piece color type coords)
    piece?
  (color piece-color)
  (type piece-type)
  (coords piece-coords))

(define-record-printer <piece>
  (lambda (piece)
    ;; Don't reverse the order of the first two here.
    ;; Checkers terminology makes it offensive then.
    (list (piece-type piece)
          (piece-color piece)
          (piece-coords piece))))

(define (piece=? p1 p2)
  (and (eq? (piece-color p1) (piece-color p2))
       (eq? (piece-type p1) (piece-type p2))
       (coords=? (piece-coords p1) (piece-coords p2))))

#|;;; Never used in any of our code!
(define (piece<? p1 p2)
  (coords<? (piece-coords p1) (piece-coords p2)))
|#

(define (piece-move piece coords)
  (make-piece (piece-color piece)
              (piece-type piece)
              coords))

(define (piece-new-type piece type)
  (make-piece (piece-color piece)
              type
              (piece-coords piece)))

(define (same-color? p1 p2)
  (eq? (piece-color p1) (piece-color p2)))