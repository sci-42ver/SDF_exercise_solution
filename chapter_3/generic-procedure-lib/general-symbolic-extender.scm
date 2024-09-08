(define (symbolic-1? object)
  (and 
    (not (matrix? object))
    (not (general-vector? object))
    (or (symbol? object)
        (pair? object))))
(register-predicate! symbolic-1? 'symbolic-1)

(define (symbolic-2? object)
  (and
    (not (matrix? object))
    (not (literal-matrix? object))
    (not (literal-vec? object))
    (not (general-vector? object))
    (or (symbol? object)
        (pair? object))))
(register-predicate! symbolic-2? 'symbolic-2)

(define (symbolic-extender-pred pred base-arithmetic)
  (make-arithmetic 'symbolic pred (list base-arithmetic)
                   (lambda (name base-constant)
                     base-constant)
                   (let ((base-predicate
                           (arithmetic-domain-predicate  base-arithmetic)))
                     (lambda (operator base-operation)
                       (make-operation operator
                                       (any-arg (operator-arity operator)
                                                pred
                                                base-predicate)
                                       (lambda args (cons operator args)))))))
