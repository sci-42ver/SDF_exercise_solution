(load "2_4_chebert_utils.scm")
(load "../common-lib/utils.scm")

(define ((curry-arguments . to_add_arg_indices) . fixed_args)
  (lambda (f)
    (let ((arg_len (length fixed_args)))
      (assert (= arg_len (- (get-arity f) (length to_add_arg_indices))))
      (check_all_conds (lambda (i) (assert (and (>= i 0) (<= i arg_len)))) to_add_arg_indices)
      ;; to_add_arg_indices should be sorted in parallel with `args` since it may have one-to-one correspondence with `to_add_args`.
      ;; TODO If using unsorted data, then we need methods to insert them all at once.

      ;; fold here is similar to https://rosettacode.org/wiki/Loop_over_multiple_arrays_simultaneously#Scheme but more elegant.
      (compose-args f (lambda args 
                        (let* ((combined_to_add (map cons to_add_arg_indices args))
                               (combined_to_add (sort combined_to_add > car))
                               ;; TODO what is the syntax to combine the following 2 operations  
                               (to_add_arg_indices (map car combined_to_add))
                               (args (map cdr combined_to_add)))
                          (display combined_to_add)
                          (display to_add_arg_indices)
                          (fold
                            ;; https://stackoverflow.com/a/36121211/21294350 or https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Folding-of-Lists.html `(x count)`.
                            (lambda (i x args)
                              (list-insert args i x))
                            fixed_args
                            to_add_arg_indices
                            args)))))))
; ((((curry-arguments 2)
;    'a 'b 'c)
;   (lambda (x y z w)
;     (list 'foo x y z w)))
;  'd)
; 'expect-value: '(foo a b d c)

; ((((curry-arguments 1 2)
;    'a 'b 'c)
;   (lambda (x y z w a)
;     (list 'foo x y z w a)))
;  'd 'e)
; 'expect-value: '(foo a d b e c)

(define (iterate n)
  (define (the-iterator f)
    (if (= n 0)
      identity
      (compose f ((iterate (- n 1)) f))))
  the-iterator)

(define (identity x) x)

(define (func_polynomial_minimal_unit . func_pow_pair_lst)
  (if (null? (car func_pow_pair_lst)) ; contain one null pair
    (begin 
      ; (display "base")
      identity)
    (let ((cur_func_pow (caar func_pow_pair_lst)))
      (newline)
      (display cur_func_pow)
      (compose 
        ((iterate (cdr cur_func_pow)) (car cur_func_pow)) 
        (func_polynomial_minimal_unit (cdr func_pow_pair_lst))))))

((lambda (x) (expt (* x 9) (expt 2 3))) 3)
; (trace func_polynomial_minimal_unit)
((compose ((iterate 3) (lambda (x) (square x))) (lambda (x) (* 3 x))) 3)
((func_polynomial_minimal_unit '(((lambda (x) (square x)) . 3) ((lambda (x) (* 3 x)) . 2))) 3)
