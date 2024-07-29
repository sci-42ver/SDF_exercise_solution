;; https://stackoverflow.com/q/78800750/21294350
(load "utils.scm")
(define (var_arg . lst)
  (if (null? lst)
    (displayln "nothing")
    (displayln lst)))
(var_arg)
(define (nested_var_arg . (arg_1 . args))
  (if (list? args)
    (begin
      (displayln "args")
      (displayln args))
    (if (arg_1)
      (displayln arg_1)
      (displayln "nothing"))))
; (nested_var_arg)
(nested_var_arg "1")
(nested_var_arg "1" "2")
(nested_var_arg "1" "2" "3")

;; test `define make-equal-hash-table` in MIT_Scheme_Reference
(define test
  (lambda (x) x))
(test 1)

;; here quote 'inch needs to be unquoted/evaluated.
(define (symbol_in_list symbol)
  (displayln `(expt symbol))
  (displayln `(expt ,symbol)))
(symbol_in_list 'inch)