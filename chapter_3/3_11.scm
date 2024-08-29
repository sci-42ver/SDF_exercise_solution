(load "~/SICP_SDF/SDF_exercises/software/sdf/manager/load.scm")
(manage 'new 'automatic-differentiation)


;; By reading https://engineering.purdue.edu/~qobi/papers/ifl2005.pdf
;; Here I give "2 Perturbation Confusion" which is similar to what `(extract-dx-function fn dx)` says "... (+ u v) (dx1^2 becomes 0) ...".

;; Here we only consider derivative with one argument x.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; The wrapped block is one try with the wrong result.
; (define (2-level-derivative f)
;   (define (the-derivative x)
;     (let* ((dx (make-new-dx))
;            (value (f (d:+ x (make-infinitesimal dx))))
;            (derivative-value 
;             (lambda args 
;               (apply value (list (d:+ (car args) (make-infinitesimal dx)))))))
;       (extract-dx-part derivative-value dx)))
;   the-derivative)

; ; (define (extract-dx-function fn dx)
; ;   (display "extract-dx-function")
; ;   (lambda args
; ;     (extract-dx-part (apply fn args) dx)))

; ; (define-generic-procedure-handler extract-dx-part
; ;   (match-args function? diff-factor?)
; ;   extract-dx-function)

; ; (trace extract-dx-part)

; (((2-level-derivative
;    (lambda (x)
;       (lambda (y)
;         (* x y))))
;   'u)
;  'v)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

;; By seeing the asserted right extract-dx-function, it will only transform args.
;; But the example `(* x y)` always pass 'v which has no dx.
;; So here we explicitly construct one wrong func.

(define (no-tag-derivative arg1 arg2)
  (let* ((dx (make-new-dx))
          (diff-dx (make-infinitesimal dx))
          (g 
            (lambda (y)
              (* (d:+ arg1 diff-dx) y))))
    (newline)
    (display "dx1 is ")
    (display dx)
  
    ; ((with-active-tag dx extract-dx-part (list g dx)) (d:+ 'v diff-dx))
    
    ;; 1. `(tag-active? dx)` is checked when calling with args. So we should change the wrapping position.
    ;; 2. Here 'v diff-dx -> 'v diff-dx2 temporarily.
    ;; So (with-active-tag dx fn ...) will have (u+dx1)*(v+dx2), then `extract-dx-part` gets (v+dx2), then replace we get (v+dx1).
    
    ;; Here we think about g: (lambda arg1 (lambda arg2 (* arg1 arg2))). Then call (g'_{arg1})'_{arg2} which should be 1.
    (extract-dx-part (with-active-tag dx (extract-dx-part g dx) (list (d:+ arg2 diff-dx))) dx)
    ))

(assert (= 1 (no-tag-derivative 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; misc
;; Here make-new-dx will also consider the created variable in let.
;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Symbols.html#index-generate_002duninterned_002dsymbol
;; > that is *guaranteed* to be different from any other object
;; Maybe it considering passing argument recursively.
(make-new-dx)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(define (use-wrong-extract-dx-function)
  (define (extract-dx-function fn dx)
    (display "extract-dx-function")
    (lambda args
      (extract-dx-part (apply fn args) dx)))

  (define-generic-procedure-handler extract-dx-part
    (match-args function? diff-factor?)
    extract-dx-function))

(use-wrong-extract-dx-function)

;; unfortunately here dx1^2 is 0, so after one extraction we have no dx part.
;; This is different from the above where we have dx1*dx2.
(assert (= 0 (no-tag-derivative 1 1)))

;; Notice in reference, it does the extraction before finishing the multiplication, so it can extract 2 from 2*\epsilon^2 when not using tag.
;; See test-1, if the original implementation just doesn't use tag, it can't extract as the reference does.
(define (use-correct-extract-dx-function)
  (define (extract-dx-function fn dx)
    (lambda args
      (if (tag-active? dx)
          (let ((eps (make-new-dx)))
            (replace-dx dx eps
                        (extract-dx-part
                          (with-active-tag dx fn
                                          (map (lambda (arg)
                                                ;; maybe diff-factor? diff-factor? symbolic?
                                                (replace-dx eps dx arg))
                                              args))
                        dx)))
          (extract-dx-part (with-active-tag dx fn args)
                          dx))))

  (define-generic-procedure-handler extract-dx-part
    (match-args function? diff-factor?)
    extract-dx-function))

(define (no-tag-derivative-reference arg1 arg2)
  (let* ((dx (make-new-dx))
          (diff-dx (make-infinitesimal dx))
          (g 
            (lambda (y)
              (* (d:+ arg1 diff-dx) (+ (d:+ arg1 diff-dx) y)))))
    (newline)
    (display "dx1 is ")
    (display dx)
  
    ; ((with-active-tag dx extract-dx-part (list g dx)) (d:+ 'v diff-dx))
    
    ;; 1. `(tag-active? dx)` is checked when calling with args. So we should change the wrapping position.
    ;; 2. Here 'v diff-dx -> 'v diff-dx2 temporarily.
    ;; So (with-active-tag dx fn ...) will have (u+dx1)*(v+dx2), then `extract-dx-part` gets (v+dx2), then replace we get (v+dx1).
    
    ;; Here we think about g: (lambda arg1 (lambda arg2 (* arg1 arg2))). Then call (g'_{arg1})'_{arg2} which should be 1.
    (extract-dx-part (with-active-tag dx (extract-dx-part g dx) (list (d:+ arg2 diff-dx))) dx)
    ))

;; > constructing a function whose derivative is wrong with this earlier version of extract-dx-part but is correct in the fixed version
(use-correct-extract-dx-function)
(assert (= 1 (no-tag-derivative-reference 1 1)))

(use-wrong-extract-dx-function)
(assert (= 0 (no-tag-derivative-reference 1 1)))

;; test-1
(define (make-new-dx)
  'delta-1)

(define (diff-factor? object)
  (symbol? object))

;; we need to register with the new diff-factor?.
(define-generic-procedure-handler extract-dx-part
  (match-args differential? diff-factor?)
  extract-dx-differential)
(use-wrong-extract-dx-function)

(define (test-1)
  (((derivative
      (lambda (x)
        (derivative
        (lambda (y)
          (* x y)))))
    'u)
  'v))
(assert (= 0 (test-1)))

;; This has no effects at all, since `((derivative ...) 'u)` calls `(extract-dx-function fn dx)` which returns lambda.
;; This lambda takes arg 'v which has no dx.
(use-correct-extract-dx-function)
(assert (= 0 (test-1)))

;; process to find the above registration problem.
; ((derivative
;     (lambda (y)
;       (* (d:+ 'u (make-infinitesimal (make-new-dx))) y)))
;   'v)

; (define test-differential (* (d:+ 'u (make-infinitesimal (make-new-dx))) (d:+ 'v (make-infinitesimal (make-new-dx)))))
; (differential? test-differential)
; (diff-factor? (make-new-dx))

; (trace extract-dx-differential)
; (extract-dx-part 
;   test-differential
;   (make-new-dx))
