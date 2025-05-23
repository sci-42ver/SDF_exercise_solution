;;; from doc https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/SC-Transformer-Definition.html#index-close_002dsyntax
;; What the problem is
;; > if the user of loop-until just happens to use, say, if for the identifier, it will be *inadvertently captured*.
;; which can't be solved with "env" of sc-macro-transformer due to needing "loop" binding
;; > but after the identifier loop has been added.
(define-syntax loop-until
  (sc-macro-transformer
   (lambda (exp env)
     (let ((id (cadr exp))
           (init (caddr exp))
           (test (cadddr exp))
           (return (cadddr (cdr exp)))
           (step (cadddr (cddr exp)))
           (close
            (lambda (exp free)
              (make-syntactic-closure env free exp))))
       `(letrec ((loop
                  (lambda (,id)
                    (if ,(close test (list id))
                        ,(close return (list id))
                        (loop ,(close step (list id)))))))
          (loop ,(close init '())))))))
(letrec ((loop
          (lambda (if)
            (if (= if 5)
                (write-line (list "if:" if))
                (loop (+ if 1))))))
  (loop 0))
;; This is due to if is not one special form now.
;; Then applicative order implies keeping (loop (+ if 1))...
;Aborting!: maximum recursion depth exceeded
(loop-until if 0 (= if 5) (write-line (list "if:" if)) (+ if 1))
;; same as the above
;Aborting!: maximum recursion depth exceeded

(define-syntax loop-until
  (sc-macro-transformer
   (lambda (exp env)
     (let ((id (cadr exp))
           (init (caddr exp))
           (test (cadddr exp))
           (return (cadddr (cdr exp)))
           (step (cadddr (cddr exp)))
           (close
            (lambda (exp free)
              (make-syntactic-closure env free exp))))
       `(letrec ((loop
                  ,(capture-syntactic-environment
                    (lambda (env)
                      `(lambda (,id)
                         ;; 0. Use the original if and the defined loop.
                         ;; But we allow redefinition in test, return etc.
                         ;; 1. Here we use nested Quasiquote, so this `if is level (3-2)=1, so not evaluated.
                         (,(make-syntactic-closure env '() `if)
                          ,(close test (list id))
                          ,(close return (list id))
                          (,(make-syntactic-closure env '() `loop)
                           ,(close step (list id)))))))))
          (loop ,(close init '())))))))
(loop-until if 0 (= if 5) (write-line (list "if:" if)) (+ if 1))

;;; transformer environment
;; In a nutshell, 
;; > transformer environment, which is the syntactic environment in which the transformer expression occurred
;; i.e. where sc-macro-transformer occurs.
;; > syntactic environment in which the input form occurred
;; i.e. where (push ...) occurs for (define-syntax push ...).
(define-syntax push
  (sc-macro-transformer
   (lambda (exp env)
     (capture-syntactic-environment
      (lambda (transformer-env)
        (let ((item (make-syntactic-closure env '() (cadr exp)))
              (list (make-syntactic-closure env '() (caddr exp))))
          `(,(make-syntactic-closure transformer-env '() `set!) ,list (cons ,item ,list)))
        )))))
; (define-syntax push
;   (sc-macro-transformer
;    (lambda (exp env)
;      (let ((item (make-syntactic-closure env '() (cadr exp)))
;            (list (make-syntactic-closure env '() (caddr exp))))
;        `(set! ,list (cons ,item ,list))))))

;; Emm... This will actually changes the *global* binding (as SICP teaches), so also for the transformer-env above.
; (define set! 1)
; (define lst (list 0 2))
; (push set! lst)
;The object 1 is not applicable.

(define lst (list 0 2))
;; this won't change the global set! binding.
((lambda (set!) (push set! lst)) 1)
lst

;; similarly, expectedly (rename 'set!) *points* to the global set!.
;; still failure.
(define-syntax push
  (er-macro-transformer
   (lambda (form rename compare)
     (let ((item (cadr form))
            (list (caddr form))
            (r-make-rule (rename 'set!))
            )
        `(,r-make-rule ,list (cons ,item ,list))))))
; (define set! 1)
; (define lst (list 0 2))
; (push set! lst)

;;; make-syntactic-closure free-names usage
;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/SC-Transformer-Definition.html
;; 0. (make-syntactic-closure env '(exit) ...)
;; To explicitly use exit introduced by call-with-current-continuation regardless of what exit binding may be introduced in body.
;; This is just like aif it (see https://stackoverflow.com/q/79195343/21294350).
;; 1. > the examples are similar except for the source of the identifier being left free. 
;; i.e. one is from call/cc while the other is from lambda.
;; > the identifier that is to be bound by let1 must be left free, so that it can be properly captured by the lambda in the output form. 
;; same as "it" in aif.
