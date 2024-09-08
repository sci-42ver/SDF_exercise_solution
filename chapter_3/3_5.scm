(load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
(manage 'new 'combining-arithmetics)

(install-arithmetic! (extend-arithmetic function-extender numeric-arithmetic-without-constant))

((+ cos sin) 2)

(load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
(manage 'new 'combining-arithmetics)

(define (+-like operator identity-name)
  (lambda (arithmetic)
    (let ((binary-operation
            (find-arithmetic-operation operator arithmetic)))
      (and binary-operation
           (let ((binary
                   (operation-procedure binary-operation))
                 (identity
                   ;; here we either return constant or (error ...).
                   ;; the former is fine, but the latter will throw error *earlier* when calling `+-like`.
                   ;; So this means arithmetic must have identity but this is not that case.

                   ;; Also see https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Condition-Signalling.html#index-error-2
                   ;; > Under normal circumstances error will not return a value (although an interactive debugger can be used to *force this to occur*).
                   ;; get-identity may be nothing.
                   ((identity-name->getter identity-name
                                           arithmetic))))
             (cons operator
                   (lambda args
                     (case (length args)
                       ((0) identity)
                       ((1) (car args))
                       (else (pairwise binary args))))))))))

;; if we just want to call (pairwise binary args), it should work. But errors are thrown beforehand.
; (install-arithmetic! (extend-arithmetic function-extender numeric-arithmetic-without-constant))
; ((+ cos sin) 2)
