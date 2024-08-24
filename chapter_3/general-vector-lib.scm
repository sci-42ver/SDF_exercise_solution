(define (arithmetic->package arithmetic)
  (make-package (arithmetic-name arithmetic)
    (arithmetic->bindings arithmetic
                          (+-like '+ 'additive-identity)
                          ;; added
                          (lambda (arithmetic)
                            (let* ((operator 'vector)
                                    (binary-operation
                                      (find-arithmetic-operation operator arithmetic)))
                                (cons operator
                                  (lambda args
                                    ;; for simplicity, here I only give one demo of args>=2.
                                    (pairwise (operation-procedure binary-operation) args)))))
                          (--like '- 'negate)
                          (+-like '* 'multiplicative-identity)
                          (--like '/ 'invert)
                          (comparator '<)
                          (comparator '=)
                          (comparator '>)
                          (comparator '<=)
                          (comparator '>=)
                          (min-like 'min)
                          (min-like 'max))))

(define (install-specific-generic-arithmetic-2)
  (let ((g
        (make-generic-arithmetic make-simple-dispatch-store)))
    (extend-generic-arithmetic! g function-extender)
    (add-to-generic-arithmetic! g
                                (extend-arithmetic symbolic-extender (extend-arithmetic vector-extender numeric-arithmetic)))
    (extend-generic-arithmetic! g vector-extender)
    ; (add-to-generic-arithmetic! g generic-numeric-arithmetic)
    ;; notice this may be too general.
    ;; put after vector-extender to capture (any-object? number?)
    (extend-generic-arithmetic! g numeric-extender)
    (install-arithmetic! g)))