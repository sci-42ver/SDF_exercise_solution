(load "../software/sdf/combinators/function-combinators.scm")
(load "utils.scm")

;; https://stackoverflow.com/questions/47048405/removing-n-number-of-elements-from-a-list-in-racket
(define (list-remove* args_lst idx_lst)
  ;; here recursive is more strict due to the order problem.
  (define (recur idx rest_lst)
    (if (null? rest_lst)
      '()
      (let ((rest_filtered_lst (recur (+ idx 1) (cdr rest_lst))))
        (if (member idx idx_lst)
          rest_filtered_lst
          (cons (car rest_lst) rest_filtered_lst)))))
  (recur 0 args_lst))
;; based on code base
(define (discard-arguments . args)
  (check_all_conds (lambda (i) (assert (exact-nonnegative-integer? i))) args)
  (lambda (f)
    (let ((m (+ (get-arity f) (length args))))
      (define (the-combination . orig_args)
        (assert (= (length orig_args) m))
        ;; We can use iter although it is not elegant.
        ;; Use the same naming convention as mbillingr
        (apply f (list-remove* orig_args args)))
      (check_all_conds (lambda (i) (assert (< i m))) args)
      (restrict-arity the-combination m))))

(((discard-arguments 2)
  (lambda (x y z)
    (list 'foo x y z)))
 'a 'b 'c 'd)
'expect-value: '(foo a b d)

(((discard-arguments 2 3)
  (lambda (x y)
    (list 'foo x y)))
 'a 'b 'c 'd)

;; error test
; (((discard-arguments 2 'a)
;   (lambda (x y)
;     (list 'foo x y)))
;  'a 'b 'c 'd)

; (((discard-arguments 2 3)
;   (lambda (x y)
;     (list 'foo x y)))
;  'a 'b 'c 'd 'e)

; (((discard-arguments 2 4)
;   (lambda (x y)
;     (list 'foo x y)))
;  'a 'b 'c 'd)

(load "2_4_chebert_utils.scm")
(define ((discard-arguments . discard-spec) f)
  ;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Miscellaneous-List-Operations.html
  (let ((discard-spec (sort discard-spec >)))
    (restrict-arity
      (compose-args f (lambda args (fold (swap-args list-remove) args discard-spec)))
      (+ (get-arity f) (length discard-spec)))))

(((discard-arguments 2 3)
  (lambda (x y)
    (list 'foo x y)))
 'a 'b 'c 'd)

;;; curry-arguments
;; Again use chebert's elegant structure.
(define ((curry-arguments . to_add_arg_indices) . fixed_args)
  (lambda (f)
    (let ((arg_len (length fixed_args))
          (to_add_cnt (length to_add_arg_indices)))
      (assert (= arg_len (- (get-arity f) to_add_cnt)))
      (check_all_conds (lambda (i) (assert (and (>= i 0) (<= i arg_len)))) to_add_arg_indices)
      ;; to_add_arg_indices should be sorted in parallel with `args` since it may have one-to-one correspondence with `to_add_args`.
      ;; TODO If using unsorted data, then we need methods to insert them all at once.
      
      ;; fold here is similar to https://rosettacode.org/wiki/Loop_over_multiple_arrays_simultaneously#Scheme but more elegant.
      (define the-combination (compose-args f (lambda args 
                        (let* ((combined_to_add (map cons to_add_arg_indices args))
                               (combined_to_add (sort combined_to_add > car))
                               ;; TODO what is the syntax to combine the following 2 operations  
                               (to_add_arg_indices (map car combined_to_add))
                               (args (map cdr combined_to_add)))
                          (fold
                            ;; https://stackoverflow.com/a/36121211/21294350 or https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Folding-of-Lists.html `(x count)`.
                            (lambda (i x args)
                              (list-insert args i x))
                            fixed_args
                            to_add_arg_indices
                            args)))))
      (restrict-arity the-combination to_add_cnt))))
((((curry-arguments 2)
   'a 'b 'c)
  (lambda (x y z w)
    (list 'foo x y z w)))
 'd)
'expect-value: '(foo a b d c)

(define test_ca (((curry-arguments 1 2)
                  'a 'b 'c)
                  (lambda (x y z w a)
                    (list 'foo x y z w a))))

(test_ca
 'd 'e)
'expect-value: '(foo a d b e c)

(get-arity test_ca)

;; error test
; (((curry-arguments 1 2)
;   'a 'b 'c)
;   (lambda (x y z w)
;     (list 'foo x y z w)))

;;; b: I tried searching "combinator" in this chapter and find compose can be generalized as c says (already done in chebert) before reading c.
;; We can generalize 
;; 1. `iterate` to make something like f^2\circ g^3...
;; 2. `parallel-combine` with any number of f, g... Similar for `spread-combine`.

;; No reference answers since I don't know how to search when all available reference answer repo isn't based on exercise number.

;;; iterate generalization
;; from mbillingr which is same as the book
(define (iterate n)
  (define (the-iterator f)
    (if (= n 0)
        identity
        (compose f ((iterate (- n 1)) f))))
  the-iterator)

(define (identity x) x)

(define (func_polynomial_minimal_unit . func_pow_pair_lst)
  (if (null? (car func_pow_pair_lst))
    identity
    (let ((cur_func_pow (car func_pow_pair_lst)))
      (compose 
        ((iterate (cdr cur_func_pow)) (car cur_func_pow)) 
        (func_polynomial_minimal_unit (cdr func_pow_pair_lst))))))

((lambda (x) (expt (* x 9) (expt 2 3))) 3)
; ((func_polynomial_minimal_unit '((square . 3) ((lambda (x) (* 3 x)) . 2))) 3)

;;; parallel-combine generalization
(define (parallel-apply-variant . funcs)
  (define (the-combination . args)
    ;; here we use one list, so not use `let-values`.
    ;; But we must use call-with-values since `apply` returns `values`.
    ; (let ((func_results (map (lambda (func) (apply func args)) funcs)))
    ;   (displayln func_results)
    ;   (displayln (fold-right append '() func_results))
    ;   (apply values (fold-right append '() func_results))))
    (apply values (fold-right 
                    (lambda (func cur_lst)
                      (call-with-values (lambda () (apply func args))
                        ;; here x auto transform the param to list.
                        (lambda x
                          (append x cur_lst)))) 
                    '() 
                    funcs)))
  (let ((arities (map (lambda (func) (get-arity func)) funcs)))
    ;; https://stackoverflow.com/a/7323738/21294350
    (assert (apply = arities))
    (restrict-arity the-combination (car arities))))

;; for simplicity, here I didn't check arity of h although we can do that as Exercise 2.2 does.
(define (parallel-combine-variant h . funcs)
  ;; use apply to avoid passing list as one single parameter.
  (displayln (get-arity (apply parallel-apply-variant funcs)))
  (compose h (apply parallel-apply-variant funcs)))

(define test_pc (parallel-combine-variant list
                    (lambda (x y z)
                      (values x y z))
                    (lambda (u v w)
                      (values w v u))
                    (lambda (u v w)
                     (values w v u))
                    (lambda (u v w)
                     (values w v u))))
(test_pc
 'a 'b 'c)
'expect-value: '(a b c c b a c b a ...)

;; here due to we use `procedure-arity` in arity which doesn't check hash-table. This can be fixed like 2.2 does.
(get-arity test_pc)

((parallel-combine-variant list
                    (lambda (x y z)
                      (values x y z))
                    (lambda (u v w)
                      (values w v u))
                    (lambda (u v w)
                     (values w v u))
                    (lambda (u v w a)
                     (values w v u))) 'a 'b 'c)