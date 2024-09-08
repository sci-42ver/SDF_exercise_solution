;; By searching "trie-full-generic-arithmetic" with "*.rkt,*.scm" in VSCode, 6.945_assignment_solution has one sample implementation.
(load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
(manage 'new 'automatic-differentiation)

;; a
(define trie-full-generic-arithmetic
  (let ((g (make-generic-arithmetic make-trie-dispatch-store)))
    (add-to-generic-arithmetic! g numeric-arithmetic)
    (extend-generic-arithmetic! g function-extender)
    (add-to-generic-arithmetic! g
                                (symbolic-extender numeric-arithmetic))
    g))
(install-arithmetic! trie-full-generic-arithmetic)

(define (book-order-test)
  (* 'b ((+ 'c cos sin) (+ 3 'a))))
(book-order-test)

(define (book-order-test-mod)
  (* 'b ((+ cos 'c sin) (+ 3 'a))))

(define (install-arithmetic-func-before-symbolic)
  (let ((g
          (make-generic-arithmetic make-trie-dispatch-store)))
    ;; root trie addition order: 
    (add-to-generic-arithmetic! g numeric-arithmetic) ; number?
    (extend-generic-arithmetic! g symbolic-extender) ; any-object?, symbolic?
    ;; See SDF_notes "... prioritized over ..."
    (extend-generic-arithmetic! g function-extender) ; function?
    (install-arithmetic! g)))
(install-arithmetic-func-before-symbolic)
;; This will fail since 'c will try handler predicates: function? -> symbolic?
;; Then it must calls symbolic-extender.
; (pp (caar (generic-procedure-rules cos)))
(book-order-test)

;; this will work
(load "../common-lib/test-lib.scm")
(define (assert-test-mod)
  (assert-predicate equal? (book-order-test-mod) '(* b (+ (+ (cos (+ 3 a)) c) (sin (+ 3 a))))))
(assert-test-mod)

(define (install-arithmetic-symbolic-before-func)
  (let ((g
          (make-generic-arithmetic make-trie-dispatch-store)))
    (add-to-generic-arithmetic! g numeric-arithmetic)
    (extend-generic-arithmetic! g function-extender) ;*
    (extend-generic-arithmetic! g symbolic-extender) ;*
    (install-arithmetic! g)))
(install-arithmetic-symbolic-before-func)
;; it must try symbolic? first.
(book-order-test) ; this should fail. See `add-edge-to-trie` comment.

;; symbolic? -> function?
;; Then the 2nd will match (function? any-object?) together with the 1st arg.
(assert-test-mod)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; If we change the above addition order of (any-object?, symbolic?), then its behavior is same as `make-simple-dispatch-store`.
(define (any-arg arity predicate base-predicate)
  (if (n:= 0 arity)
    (list)
    (remove
      (lambda (arg) (equal? arg (make-list arity base-predicate)))
      (all-sequences-of arity predicate base-predicate))))
(install-arithmetic-func-before-symbolic)
(book-order-test)
(assert-test-mod)
(install-arithmetic-symbolic-before-func)
(book-order-test)
;; But this will make the above 2nd predicate fail since here it will try symbolic? -> any-object? for cos.
;; Then it will match (any-object? symbolic?).
(assert-test-mod)

;; > Does this make any change to the dependence on order that we wrestled with in section 3.2.2?
;; Based on the above: Yes.
;; This is due to we are matching arguments one by one instead of matching the whole part each time.
;; So the trie addition order matters.

;; b
;; > In general, what characteristics of the predicates could produce situations where there is more than one appropriate handler for a sequence of arguments?
;; predicate which can match more than one args offered.
;; c
;; Yes. any-object?
