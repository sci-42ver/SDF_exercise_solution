(define (compose f g)
  (define (the-composition . args)
    (call-with-values (lambda () (apply g args))
      f))
  the-composition)

(define (iterate n)
  (define (the-iterator f)
    (if (= n 0)
        identity
        (compose f ((iterate (- n 1)) f))))
  the-iterator)

(define (identity x) x)

(define (func_polynomial_minimal_unit . func_pow_pair_lst)
  (if (null? (car func_pow_pair_lst)) ; contain one null pair
    identity
    (let ((cur_func_pow (caar func_pow_pair_lst)))
      (newline)
      (display cur_func_pow)
      (compose 
        ((iterate (cdr cur_func_pow)) (car cur_func_pow)) 
        (func_polynomial_minimal_unit (cdr func_pow_pair_lst))))))

;; equivalent lambda for the following `func_polynomial_minimal_unit`.
((lambda (x) (expt (* x 9) (expt 2 3))) 3)
;; these works
((compose ((iterate 3) (lambda (x) (square x))) (lambda (x) (* 3 x))) 3)
((compose ((iterate 3) square) (lambda (x) (* 3 x))) 3)

;; these doesn't work
((func_polynomial_minimal_unit '((square . 3) ((lambda (x) (* 3 x)) . 2))) 3)
((func_polynomial_minimal_unit '(((lambda (x) (square x)) . 3) ((lambda (x) (* 3 x)) . 2))) 3)