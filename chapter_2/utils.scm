;;; discard-arguments

(define (displayln x)
      (newline)
      (display x))
(define (check_all_conds condition lst)
  (assert (fold 
            (lambda (x y) (and x y)) 
            #t 
            (map condition lst))))

(define (assert-close z1 tolerance z2)
  (assert (< (- z1 z2) tolerance)))

; (define-library (common displayln)
;   (export displayln check_all_conds)
;   (import (scheme base))
;   (begin
;     (define (displayln x)
;       (newline)
;       (display x))
;     (define (check_all_conds condition lst)
;       (assert (fold 
;                 (lambda (x y) (and x y)) 
;                 #t 
;                 (map condition lst))))))