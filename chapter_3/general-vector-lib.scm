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

;; NOTICE https://stackoverflow.com/q/78908527/21294350:
;; Here `install-arithmetic!` will be installed in the env created by `(manage 'new 'combining-arithmetics)` (see software-manager-doc).
;; That env is `make-top-level-environment` (see `(define-command '(new-environment . flavors)` -> make-working-env-model)
;; https://stackoverflow.com/a/78908764/21294350 So `get-implementation-value` -> `(environment-lookup system-global-environment name)` won't be influenced since top-level-environment is not the ancestor of system-global-environment.
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