;; By searching "extract-dx-vector", no sample implementation.
(load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
(manage 'new 'automatic-differentiation)

;; by referring to `(derivative f)`, here `value` is one vector.

;; IGNORE: TODO I don't know what I should return for something like the following.
;; Here it will call replace-dx-function -> replace-dx where arg is still differential due to derivative.
;; The 2nd extract-dx-part will match vector? diff-factor?.

;; IMHO it will just do extract-dx-part for each part of the vector just similar to what `(substitute new old x)` does.
; (((derivative
;    (lambda (x)
;      (derivative
;       (lambda (y)
;         (vector (* x y) (+ x y))))))
;   'u)
;  (vector 1 2))

(load "~/SICP_SDF/SDF_exercises/software/sdf/combining-arithmetics/vector-arith.scm")

(define vector-full-arithmetic
  (let ((g (make-generic-arithmetic make-simple-dispatch-store)))
    (add-to-generic-arithmetic! g numeric-arithmetic)
    (extend-generic-arithmetic! g function-extender)
    (add-to-generic-arithmetic! g
                                (vector-extender (function-extender (symbolic-extender numeric-arithmetic))))
    ;; put after the above to avoid capture vector? symbolic?
    (add-to-generic-arithmetic! g
                                (symbolic-extender (vector-extender numeric-arithmetic)))
    (numerical-simplifier-wrapper g)))

(install-arithmetic! vector-full-arithmetic)

(load "~/SICP_SDF/SDF_exercises/software/sdf/automatic-differentiation/extractor.scm")
(load "~/SICP_SDF/SDF_exercises/software/sdf/automatic-differentiation/handlers.scm")

(define (extract-dx-vector value dx)
  (vector-map 
    (lambda (elem) 
      (cond 
        ((differential? elem) 
         (display "edv1")
         ; (display elem)
         ; (bkpt "edv1" elem)
         ;; https://stackoverflow.com/q/78930923/21294350 If I inserts one dot after this, this will return 0 and there is no syntax errors.
         ; (newline)
         (extract-dx-part elem dx))
        ((function? elem) 
         (display "edv2")
         (extract-dx-part elem dx))
        ((or (number? elem) (symbolic? elem))
         (display "edv3")
         0)
        ))
    value))

(define-generic-procedure-handler extract-dx-part
                                  (match-args vector? diff-factor?)
                                  extract-dx-vector)

(define (replace-dx-vector new-dx old-dx object)
  (newline)
  (display "replaced vec:")
  (vector-map 
    (lambda (elem) 
      (cond 
        ((differential? elem) 
         (display "rdv1")
         (replace-dx new-dx old-dx elem))
        ((function? elem) 
         (display "rdv2")
         (replace-dx new-dx old-dx elem))
        ((or (number? elem) (symbolic? elem))
         (display "rdv3")
         elem)
        ))
    object))

(define-generic-procedure-handler replace-dx
                                  (match-args diff-factor? diff-factor? vector?)
                                  replace-dx-vector)

;; Use this to check the intermediate results.
; (trace *)

(assert (equal? (* (vector 3 4) 'u) '(* #(3 4) u)))

(load "test-lib.scm")
;; Here we gets ((x*y)_y)_x=1. Similarly, we should have value (1 0) 
(assert-predicate
  equal?
  #(1 0)
  (((derivative
      (lambda (x)
        ;; > work with derivatives of functions that return vectors
        (derivative
          (lambda (y)
            (vector (* x y) (+ x y))))))
    'u)
   (vector 3 4)))

; (define test-diff-1 
;   (make-differential 
;     (sort 
;       (list (make-diff-term 'u '()) (make-diff-term 1 (list (make-new-dx))))
;       diff-term>?)))
;
; (define test-diff-2 
;   (make-differential 
;     (sort 
;       (list (make-diff-term (vector 1 2) '()) (make-diff-term 1 (list (make-new-dx))))
;       diff-term>?)))
; (+ test-diff-1 test-diff-2)

;; The above `derivative` will always pass differential to f, so we need to explicitly test other cases similar to 3.11 where it always abstracts dx in `derivative`.
(define (extract-dx-vector-test)
  (let* ((dx (make-new-dx))
         (diff-dx (make-infinitesimal dx)))
    ;; test extract-dx-vector
    (define res1
      (extract-dx-part 
        (vector
          1
          (lambda (x)
            (display x)
            ; (* x diff-dx)
            (+ x diff-dx)
            ))
        dx))
    (assert-predicate = 0 (vector-ref res1 0))

    (define res-func (vector-ref res1 1))
    ;; notice to use `with-active-tag` to call replace-dx.
    (define res-func-res (with-active-tag dx res-func (list (vector 1 diff-dx (lambda (x) (+ diff-dx x))))))
    ;; we extracts dx coefficient from (x+dx) where x is one vector.
    (assert (= 1 res-func-res)) ; this already extracts, so the returned func by the inner replace-dx-function is lost.
    ; (define res-func-func (vector-ref res-func-res 2))
    ; (res-func-func diff-dx)
    ))

(extract-dx-vector-test)
