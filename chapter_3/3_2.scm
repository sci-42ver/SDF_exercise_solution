;; 1. chebert: none by vector
;; 2. 6.945_assignment_solution
;; - we better use n:...
;; - it lacks d. numerical operator magnitude
;; for e. it uses `v:*-maker`.
;; - Here the add-arithmetics order doesn't matter.
;; 3. combining-arithmetics/vector-arith.scm mainly based on maker's.
;; 4. I skipped most of user-defined-types/vector-arith.scm since it uses one totally different structure not introduced in the book.
(load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
(manage 'new 'combining-arithmetics)

;; code base
(load "~/SICP_SDF/SDF_exercises/software/sdf/combining-arithmetics/vector-arith.scm")
(define numerical-sqrt sqrt)
(install-arithmetic!
  (extend-arithmetic vector-extender
                     combined-arithmetic))

(negate #(1 2))
;; > When creating new arithmetics, you do not need to write n-ary procedures—n-ary procedures
;; NOTICE: Here we all assume we only need to define binary or unary operations in arithmetic func.
;; See `%arithmetic-operator-alist` negate only allows one arg with arity got by operator-arity.
;; TODO how to assert this must throw errors https://man.scheme.org/error.3scm.
; (negate #(1 2) #(1 2))

;; a
(define (vector-element-wise element-procedure)
  (lambda vecs
    ; Note: this takes multiple vectors
    (ensure-vector-lengths-match vecs)
    (apply vector-map element-procedure vecs)))

;; > Addition (and subtraction) is defined only for vectors of the same dimension, so your arithmetic must know about dimensions.
(define (ensure-vector-lengths-match vecs)
  (let ((first-vec-length (vector-length (car vecs))))
    (if (any (lambda (v)
               (not (n:= (vector-length v)
                         first-vec-length)))
             vecs)
      (error "Vector dimension mismatch:" vecs))))

; (set-predicate-metadata! vector? (get-implementation-value vector?))
; (register-predicate! symbolic? 'symbolic)
;; See `predicate-name` for why we pass 'vector.
(register-predicate! vector? 'vector)
(arithmetic? combined-arithmetic)

;; mimic boolean-arithmetic definition
(define vector-arithmetic
  (make-arithmetic 'vector vector? 
                   (list combined-arithmetic); To pass the arithmetic object, not use quote. 
                   (lambda (name base-constant)
                     base-constant) ; as the book footnote 10 says, this can't be easily generalized since we don't know dimension.
                   (lambda (operator base-operation)
                     (let* ((base-proc (operation-procedure base-operation))
                            (procedure
                              (case operator
                                ((+) base-proc)
                                ((-) base-proc)
                                ;  ((*) *)
                                ; ((negate) (lambda (arg) (base-proc arg))) ; when passing arg, this will possibly throw error ";The procedure #[compound-procedure 12] has been called with 2 arguments; it requires exactly 1 argument.".
                                ((negate) (lambda arg (apply base-proc arg))) ; when calling base-proc, this will possibly throw error ";Inapplicable operation: negate (1 1)".
                                (else
                                  (lambda args
                                    (error "Operator undefined in vector"
                                           operator))))))
                       (simple-operation operator vector? 
                                         ;; Here vecs are list, so use apply to avoid adding one more list wrapper.
                                         (lambda vecs (apply (vector-element-wise procedure) vecs))))
                     ; (simple-operation operator vector? (lambda args (map not args)))
                     )))

(install-arithmetic! combined-arithmetic)
(- 'a)

(install-arithmetic! vector-arithmetic)

(load "test-lib.scm")
(define (test)
  ;; > only addition, negation, and subtraction
  ;; >  it works for numerical vectors and for vectors with mixed numerical and symbolic coordinates.
  (assert-predicate equal? (+ #(1 2) #(3 4)) #(4 6))
  ;; > for vectors with mixed numerical and symbolic coordinates.
  (assert-predicate equal? (+ '#(1 c) '#(2 b)) '#(3 (+ c b)))
  ;; Here we use `inversion-operator` for a.
  (assert-predicate equal? (- #(a 1)) '#((negate a) -1))
  (assert-predicate equal? (- #(a 1) #(b 2)) '#((- a b) -1))
  )
(test)
;; > Applying any other operation to a vector should report an error.
; (cos #(a 1)) ; error
; (negate #(1 2) #(1 2)) ; error -> ;Inapplicable operation: negate (1 1)

;; b
;; > Which did you use?
;; The latter since IMHO this is general without requiring outside environment assumption.
;; > Show how to implement the other choice.
(install-arithmetic! combined-arithmetic)
; (apply vector-map + (list #(1 2) #(3 4)))
; (define combined-arithmetic-add +)
; (define combined-arithmetic-minus -)
(define vector-arithmetic
  (make-arithmetic 'vector vector? 
                   (list combined-arithmetic); To pass the arithmetic object, not use quote. 
                   (lambda (name base-constant)
                     base-constant) ; as the book footnote 10 says, this can't be easily generalized since we don't know dimension.
                   (lambda (operator base-operation)
                     (let* ((base-proc (operation-procedure base-operation))
                            (procedure
                              ;; only change these
                              (case operator
                                ((+) +)
                                ((-) -)
                                ;  ((*) *)
                                ;; not use lambda since here - will be changed later.
                                ; ((negate) (lambda (arg) (- arg)))
                                ; ((negate) -)
                                ((negate) (operation-procedure (car (arithmetic-operations-for 'negate (list combined-arithmetic)))))
                                (else
                                  (lambda args
                                    (error "Operator undefined in vector"
                                           operator))))))
                       (simple-operation operator vector? 
                                         ;; Here vecs are list, so use apply to avoid adding one more list wrapper.
                                         (lambda vecs (apply (vector-element-wise procedure) vecs))))
                     ; (simple-operation operator vector? (lambda args (map not args)))
                     )))

; (apply vector-map combined-arithmetic-add (list #(1 2) #(3 4)))
; (+ #(1 2) #(3 4))

;; needs this, otherwise "(error "Inapplicable operation:" operator args)".
(install-arithmetic! vector-arithmetic)

(test)
; (negate #(1 2) #(1 2))

;; > How does this choice affect your ability to make future extensions to this system?
;; both will not change vector-arithmetic if combined-arithmetic is changed.
;; The latter will use static combined-arithmetic to define static operation-alist.
;; > Hint: ...
;; Still has the above problem.

;; See 6.945_assignment_solution: We can use `extend-arithmetic` to simplify make process of vector-arithmetic
;; and `v:*-maker` etc. to allow rewriting the whole make-arithmetic each time.

;; c
(define (dot-product-maker add mul)
  (lambda (x y) 
    (apply add (vector->list (vector-map mul x y)))))

(define vector-arithmetic
  (let ((bases (list combined-arithmetic)))
    (make-arithmetic 'vector vector? 
                     bases ; To pass the arithmetic object, not use quote. 
                     (lambda (name base-constant)
                       base-constant) ; as the book footnote 10 says, this can't be easily generalized since we don't know dimension.
                     (lambda (operator base-operation)
                       (let* ((base-proc (operation-procedure base-operation))
                              (procedure
                                (case operator
                                  ((+) base-proc)
                                  ((-) base-proc)
                                  ((negate) (lambda (arg) (base-proc arg)))
                                  (else
                                    (lambda args
                                      (error "Operator undefined in vector"
                                             operator))))))
                         (simple-operation operator vector? 
                                           (case operator
                                             ((*) (apply 
                                                    dot-product-maker
                                                    ;; IGNORE: here bases is defined when calling `make-arithmetic` and then call `get-operation`.
                                                    (map 
                                                      (lambda (operator) (operation-procedure (car (arithmetic-operations-for operator bases)))) 
                                                      '(+ *))))
                                             (else (lambda vecs (apply (vector-element-wise procedure) vecs))))))
                       ; (simple-operation operator vector? (lambda args (map not args)))
                       )))
  )

(install-arithmetic! vector-arithmetic)

;; > Show that your dot product works.
(define (dot-product-test)
  (assert-predicate equal? (* #(1 2) #(3 4)) 11))
(dot-product-test)

;; d

(define numeric-arithmetic
  (make-arithmetic 
    'numeric 
    (disjoin vector? number?) 
    '()
    (lambda (name)
      (case name
        ((additive-identity) 0)
        ((multiplicative-identity) 1)
        (else (default-object))))
    (lambda (operator)
      (case operator
        ((magnitude) 
         (simple-operation operator
                           vector?
                           vector-length))
        (else (simple-operation operator
                                number?
                                ;; allows `(+ 1 2)` -> 3 based on `operation-union-dispatch`.
                                (get-implementation-value
                                  (operator->procedure-name operator))))))))

(define combined-arithmetic
  (extend-arithmetic symbolic-extender
                     numeric-arithmetic))

(install-arithmetic! combined-arithmetic)
;; > extending the numerical operator magnitude to give the length of a vector.
(assert-predicate equal? (magnitude #(1 2)) 2)
; (magnitude #(1 2) #(1 2)) ; error due to the same reasons as negate.

(define (vector-magnitude-maker + * sqrt)
  (let ((dot-product (dot-product-maker + *)))
    ;; pass local value to outside.
    (define (vector-magnitude v)
      (sqrt (dot-product v v)))
    vector-magnitude))

;; here only magnitude and negate are unary.
(define vector-arithmetic
  (let ((bases (list combined-arithmetic)))
    (make-arithmetic 'vector vector? 
                     bases ; To pass the arithmetic object, not use quote. 
                     (lambda (name base-constant)
                       base-constant) ; as the book footnote 10 says, this can't be easily generalized since we don't know dimension.
                     (lambda (operator base-operation)
                       (let* ((base-proc (operation-procedure base-operation))
                              (procedure
                                (case operator
                                  ((+) base-proc)
                                  ((-) base-proc)
                                  ((negate) (lambda (arg) (base-proc arg)))
                                  (else
                                    (lambda args
                                      (error "Operator undefined in vector"
                                             operator))))))
                         (simple-operation operator vector? 
                                           (let* ((+*-proc-lst 
                                                    (map 
                                                      (lambda (operator) (operation-procedure (car (arithmetic-operations-for operator bases)))) 
                                                      '(+ *)))
                                                  (base-+ (car +*-proc-lst))
                                                  (base-* (cadr +*-proc-lst))
                                                  )
                                             (case operator
                                               ;; TODO here it doesn't check input size similar to `vector-extender` `component-proc`.
                                               ((magnitude)
                                                ;; it is better to use base-sqrt which allows symbolic.
                                                ;; See combining-arithmetics/vector-arith.scm `(sqrt (component-proc 'sqrt))`
                                                (vector-magnitude-maker base-+ base-* sqrt))
                                               ((*) (apply 
                                                      dot-product-maker
                                                      ;; IGNORE: here bases is defined when calling `make-arithmetic` and then call `get-operation`.
                                                      +*-proc-lst))
                                               ;; this can be simplified by removing lambda.
                                               (else (lambda vecs (apply (vector-element-wise procedure) vecs)))))
                                           ))
                       ; (simple-operation operator vector? (lambda args (map not args)))
                       )))
  )

(install-arithmetic! vector-arithmetic)
;; > Add vector magnitude to your vector arithmetic
;; if using changed sqrt, then error is thrown before calling `(apply vector-map element-procedure vecs)`.
(assert-predicate equal? (magnitude #(1 2)) (numerical-sqrt 5))
; (magnitude #(1 2) #(1 2)) ; error due to the same reasons as negate.

;; e
(define vector-arithmetic
  (let ((bases (list combined-arithmetic))
        (base-predicate (arithmetic-domain-predicate combined-arithmetic)))
    ;; here better to use `(disjoin vector? (arithmetic-domain-predicate base-arithmetic))` to allow further extension.
    (make-arithmetic 'vector vector? 
                     bases ; To pass the arithmetic object, not use quote. 
                     (lambda (name base-constant)
                       base-constant) ; as the book footnote 10 says, this can't be easily generalized since we don't know dimension.
                     (lambda (operator base-operation)
                       (let* ((base-proc (operation-procedure base-operation))
                              (procedure
                                (case operator
                                  ((+) base-proc)
                                  ((-) base-proc)
                                  ((negate) (lambda (arg) (base-proc arg)))
                                  (else
                                    (lambda args
                                      (error "Operator undefined in vector"
                                             operator))))))
                         (let* ((+*-proc-lst 
                                  (map 
                                    (lambda (operator) (operation-procedure (car (arithmetic-operations-for operator bases)))) 
                                    '(+ *)))
                                (base-+ (car +*-proc-lst))
                                (base-* (cadr +*-proc-lst))
                                (scalar-vec-product 
                                  (lambda (scalar vec) 
                                    (apply 
                                      (vector-element-wise 
                                        (lambda (elem) (base-* scalar elem))) 
                                      (list vec))))
                                )
                           ; (trace scalar-vec-product)
                           ;; Here we use the above modified numeric-arithmetic which allows vector?.
                           ; (if (base-predicate #(1 2))
                           ;   (display "weird")                              
                           ;   (display "fine"))
                           (case operator
                             ((*) 
                              ;; Here due to operation-union implied order, we use (vector? vector?) by simple-operation instead of the 2nd.
                              ;; Also any-arg removes the case of (base-predicate base-predicate), so only 3 are left.
                              (operation-union operator 
                                               (simple-operation operator vector?
                                                                 (apply 
                                                                   dot-product-maker
                                                                   ;; IGNORE: here bases is defined when calling `make-arithmetic` and then call `get-operation`.
                                                                   +*-proc-lst))
                                               ;; similar to symbolic-extender
                                               (make-operation operator
                                                               (any-arg (operator-arity operator)
                                                                        vector?
                                                                        base-predicate)
                                                               ;; to be clear, I use explicit 2 args which is compatible with `pairwise` proc.
                                                               (lambda (arg1 arg2) 
                                                                 ;; not use this based on the above modified `numeric-arithmetic`.
                                                                 ; (if (base-predicate arg1)
                                                                 (if (vector? arg2)
                                                                   (scalar-vec-product arg1 arg2)
                                                                   (scalar-vec-product arg2 arg1))))))
                             (else 
                               (simple-operation operator vector?
                                                 (case operator
                                                   ;; TODO here it doesn't check input size similar to `vector-extender` `component-proc`.
                                                   ((magnitude)
                                                    (vector-magnitude-maker base-+ base-* sqrt))
                                                   (else (lambda vecs (apply (vector-element-wise procedure) vecs))))
                                                 ))))))
                     ; (simple-operation operator vector? (lambda args (map not args)))
                     ))
  )
(install-arithmetic! vector-arithmetic)

;; > Show that your vector arithmetic can handle both dot product and scalar product.
(dot-product-test)
(assert-predicate equal? (* #(1 2) 3) #(3 6))
(assert-predicate equal? (* 3 #(1 2)) #(3 6))
; (assert-predicate equal? (* 3 6) 18) ; should throw errors.
(assert-predicate equal? (* #(1 2) 'a) '#((* a 1) (* a 2)))
