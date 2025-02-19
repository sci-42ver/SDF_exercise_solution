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

;;; A place on the board is represented as
;;; a node.  Directions on the board are
;;; edges.  In chess there are 8 directions:
;;; north, south, east, west,
;;; northeast, northwest, southeast, southwest.

(define (rewrite-path-edges transform-edge-label)
  (define (transformer path)
    (cons (car path)
          (transform-elts (cdr path))))

  (define (transform-elts path-elts)
    (cond ((and (pair? path-elts)
                (symbol? (car path-elts))
                (pair? (cdr path-elts))
                (match:var? (cadr path-elts)))
           ;; different from SDF_exercises/software/sdf/unification/type-resolver.scm
           ;; more than 2, we need cons.
           (cons* (transform-edge-label (car path-elts))
                  (cadr path-elts)
                  (transform-tail (cddr path-elts))))
          ;; IGNORE SDF_exercises TODO when happens
          ;; see book p279
          ((and (pair? path-elts)
                (pair? (car path-elts))
                (memq (caar path-elts) '(* + opt)))
           (cons (cons (caar path-elts)
                       (transform-elts (cdar path-elts)))
                 (transform-tail (cdr path-elts))))
          ((and (pair? path-elts)
                (pair? (car path-elts))
                (memq (caar path-elts) '(and or)))
           (cons (cons (caar path-elts)
                       (map transform-elts (cdar path-elts)))
                 (transform-tail (cdr path-elts))))
          (else
           (error "Unknown path elements:" path-elts))))

  ;; not combined into transform-elts since path-elts is not allowed to be empty.
  ;; For simple graph https://math.stackexchange.com/a/3260124/1059606, the mere vertex can't be one edge, also for path https://en.wikipedia.org/wiki/Path_(graph_theory).
  (define (transform-tail tail)
    (if (pair? tail)
        (transform-elts tail)
        '()))

  transformer)

;;; To collect all the appropriate transformations
;; 0. whether the order matters
;; 0.a. Reflection matrix derivation https://math.stackexchange.com/questions/3865442/reflection-in-the-line-y-mxc-via-matrices/3865554#3865554
;; Also see Discrete_Mathematics_and_Its_Applications/DMIA_Exercises_2.md
;; 0.b. We can think as the following:
;;; IGNORE: Assume after some transformations, we get moves1 which are distinct.
;; If with one new transformation we get moves1* and moves1-0 becomes same as some in moves1*,
;; then we must have Mat1*move.
;;; For n transformations, we have 2^n results if not removing duplication.
;; We can index these by binary number.
;; Then 2 results' difference may differ by k digits with 0<=k<=n.
;; If they are same, we may have Mat1*Mat2^{-1}*...*move1=move1.
;; Based on associative property of matrix multiplication https://stackoverflow.com/q/16952546/21294350
;; When calculating from right to left, we can always calculate the left matrix product and leave the move1 vector unchanged.
;; So we need to check whether Mat1*Mat2^{-1}*...*Matk=I (here the exponent can be only 1 or -1 or 0 which can be checked by 4 cases of 0/1 pairs) is impossible.
;;;; IGNORE
;; 0.b.0. We can think of reflection matrix as one special rotation matrix whose angle varies based on the move.
;; So we just think about the special case for knight-move.
;; Denote alpha as the angle that tan(alpha)=1/2, 0<alpha<pi/2
;; 0.b.0.a. reflection as the leftmost matrix or the rightmost matrix
;;; IGNORE the following
;; 0.b.0.a.0. only 2 mat's in total
;; +/-90 or +/-180 all can't be a multiple of 360 degrees minus 2*alpha.
;; 0.b.0.a.1. only 1 mat, trivially not a multiple of 360 degrees minus 2*alpha.
;; 0.b.0.a.2. 3 mats
;; similar to 0, we have +/-270, again unable to be a multiple of 360 degrees minus 2*alpha.
;;; Here we need to multiple rotation matrix (the inverse of rotation matrix is also one rotation matrix https://en.wikipedia.org/wiki/Rotations_and_reflections_in_two_dimensions#Mathematical_expression) 
;; to get one reflection matrix Mat1^{-1}
;; But we can assume Rot(Theta1)=Ref(Theta2/2) where Theta2 is pi here.
;; Then we find cos(Theta1)=cos(Theta2) => Theta1=Theta2+2k_1*pi or Theta1=2k_1*pi-Theta2
;; sin(Theta1)=sin(Theta2) => Theta1=Theta2+2k_2*pi or Theta1=2k_2*pi+pi-Theta2
;;; So 
;; Theta1=Theta2+2k_1*pi (k_1=k_2) or 
;; Theta2+2k_1*pi=2k_2*pi+pi-Theta2 => Theta2=(k_2-k_1)*pi+pi/2 (impossible) or
;; Theta2=(k_1-k_2)*pi (so k_1-k_2=1) or
;; 2k_1*pi=2k_2*pi+pi (impossible)
;;; sin(Theta1)=-sin(Theta2)
;;; cos(Theta1)=-cos(Theta2) (impossible which implies the above can't hold for the case when Theta2=pi).
;; Actually here we have sin(Theta2)=cos(Theta2)=0 which can't hold for all Theta2's.
;;; 0.b.0.b. reflection as the middle matrix
;;;; IGNORE ended
;; Here I just uses knight-move as one demo.
;; 0.b.0. If 0 denotes no matrix, we can actually think this as one rotation matrix of 0 degree.
;; the inverse of rotation matrix is also one rotation matrix https://en.wikipedia.org/wiki/Rotations_and_reflections_in_two_dimensions#Mathematical_expression
;; 0.b.1. Whatever location the reflection matrix is in, the result must be 2 cases.
;; 0.b.1.0. no reflection matrix (3!*3^2 situations in 0.b.2.). Then the results are all just one rotation matrix.
;; trivially (0 or 1)*+/-90+(0 or 1)*+/-180 can't be a multiple of 360 degrees.
;; 0.b.1.1. one reflection matrix (reflection matrix inverse is itself)
;; (3!*3^2*2 situations in 0.b.2.)
;; Here I uses the 3rd and 4th identities in the above wikipedia link.
;; So finally we will have one Ref matrix as the multiplication result for 3 cases where Ref is at.
;; But cos(Theta2)=-cos(Theta2)=0 can't be one identity matrix.
;; 0.b.2. considered in 0.b.1., we considered at most P_{3}^{3}*3^3 situations where (0,0,0) is considered duplicately etc.
;; Here P_{3}^{3}=3! considers the possible permutation for 3 transformations.
;; 0.b.3. Based on the above contexts of "can't", no duplication in 2^n results at least for knight-move.
;; IGNORE 0.b.0.c. reflection as the rightmost matrix
(define (symmetrize-move move . transformations)
  (let loop ((xforms transformations) (moves (list move)))
    (if (null? xforms)
        moves
        (loop (cdr xforms)
              ;; at each time, map multiplies the number of moves.
              (append moves
                      (map (rewrite-path-edges (car xforms))
                           moves))))))

(define (occupied-by type)
  (lambda (place-node dict)
    (let ((piece (piece-in place-node dict)))
      (and piece
           (eq? type (piece-type piece))))))

(define (maybe-opponent place-node dict)
  (no-piece-or-opponent?
   (piece-in place-node dict)
   (piece-in (match:get-value 'source-node dict) dict)))

(define (unoccupied place-node dict)
  ; (write-line (list "unoccupied returns" (not (piece-in place-node dict)) "for" place-node))
  (not (piece-in place-node dict)))

(define (piece-in place-node dict)
  ;; SDF_exercises TODO where is chess-dict:board inited?
  ((chess-dict:board dict) 'piece-in place-node))

;;; Basic knight move is NNE.

(define basic-knight-move
  `((? source-node ,(occupied-by 'knight))
    north (?)
    north (?)
    east (? target-node ,maybe-opponent)))

(define all-knight-moves
  (symmetrize-move basic-knight-move
                   reflect-ew rotate-90 rotate-180))

#|
(pp all-knight-moves)
((<source> north (?) north (?) east <target>)
 (<source> north (?) north (?) west <target>)
 (<source> east (?) east (?) south <target>)
 (<source> east (?) east (?) north <target>)
 (<source> south (?) south (?) west <target>)
 (<source> south (?) south (?) east <target>)
 (<source> west (?) west (?) north <target>)
 (<source> west (?) west (?) south <target>))
|#

;;; A knight fork is a knight in a position
;;; that threatens more than one opponent
;;; piece.

;;; To be specified...

;;; Basic rook move is N*.

(define basic-rook-move
  `((? source-node ,(occupied-by 'rook))
    (* north (?* ,unoccupied))
    north (? target-node ,maybe-opponent)))

(define all-rook-moves
  (symmetrize-move basic-rook-move
                   rotate-90 rotate-180))

#|
(pp all-rook-moves)
((<source> (* north <unoccupied>) north <target>)
 (<source> (* east <unoccupied>) east <target>)
 (<source> (* south <unoccupied>) south <target>)
 (<source> (* west <unoccupied>) west <target>))
|#

(define basic-bishop-move
  `((? source-node ,(occupied-by 'bishop))
    (* northeast (?* ,unoccupied))
    northeast (? target-node ,maybe-opponent)))

(define all-bishop-moves
  (symmetrize-move basic-bishop-move
                   rotate-90 rotate-180))

#|
(pp all-bishop-moves)
((<source> (* northeast <unoccupied>) northeast <target>)
 (<source> (* southeast <unoccupied>) southeast <target>)
 (<source> (* southwest <unoccupied>) southwest <target>)
 (<source> (* northwest <unoccupied>) northwest <target>))
|#

(define basic-king-move
  `((? source-node ,(occupied-by 'king))
    north (? target-node ,maybe-opponent)))

(define all-king-moves
  (symmetrize-move basic-king-move
                   rotate-45 rotate-90 rotate-180))

#|
(pp all-king-moves)
((<source> north <target>)
 (<source> northeast <target>)
 (<source> east <target>)
 (<source> southeast <target>)
 (<source> south <target>)
 (<source> southwest <target>)
 (<source> west <target>)
 (<source> northwest <target>))
|#


(define basic-queen-move
  `((? source-node ,(occupied-by 'queen))
    (* north (?* ,unoccupied))
    north (? target-node ,maybe-opponent)))

;; similar to knight-move, the absolute of the maximum angle is less than 360 degrees, so no duplication caused by identity.
(define all-queen-moves
  (symmetrize-move basic-queen-move
                   rotate-45 rotate-90 rotate-180))

#|
(pp all-queen-moves)
((<source> (* north <unoccupied>) north <target>)
 (<source> (* northeast <unoccupied>) northeast <target>)
 (<source> (* east <unoccupied>) east <target>)
 (<source> (* southeast <unoccupied>) southeast <target>)
 (<source> (* south <unoccupied>) south <target>)
 (<source> (* southwest <unoccupied>) southwest <target>)
 (<source> (* west <unoccupied>) west <target>)
 (<source> (* northwest <unoccupied>) northwest <target>))
|#
