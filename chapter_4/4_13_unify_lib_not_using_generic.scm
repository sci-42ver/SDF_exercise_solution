;; 0. this is modification of SDF_exercises/software/sdf/unification/unify.scm
;; 1. check whether do "syntactic analysis" at runtime by "match:(list|element|eqv|compile-pattern) ".

;; IGNORE: move outside to make maybe-substitute convenient.
; (define (unify-constants first1 terms2 dict succeed fail)
;   (if (eqv? first1 (car terms2))
;       ;; Here rest1 rest2 will be manipulated by match:list.
;       ; (succeed dict fail rest1 rest2)
;       ;; compatible with matcher
;       (succeed dict 1 fail)
;       (fail)))
(define (unify:constant-terms first1 terms2)
  (define (unify-constants dict succeed fail)
    (if (eqv? first1 (car terms2))
        ;; Here rest1 rest2 will be manipulated by match:list.
        ; (succeed dict fail rest1 rest2)
        ;; compatible with matcher
        (succeed dict 1 fail)
        (fail)))
  unify-constants)

;; already done in match:list
; (define (unify:list-terms first1 terms2)
;   (let ((matcher (match:compile-pattern first1)))
;     (define (unify-lists dict succeed fail)
;       (matcher 
;         ; (car terms2)
;         terms2
;         dict
;         ;; 
;         (lambda (dict* n fail*)
;           ;; the 2nd is done by match:list
;           ;; the 1st is done when "syntactic analysis".
;           ; (assert (null? null1))
;           ; (assert (null? null2))
;           ; (succeed dict* 1 fail*)
;           (succeed dict* n fail*)
;           )
;         fail))
;     unify-lists)
;   )

;;; This is the syntactic equation solver for element vars.

;; added
;; 0. Here (match:vars-equal? var term) is similar to (eqv? first1 (car terms2))
;; (match:get-value var dict), (do-substitute var term dict) are also contained similarly in matcher.scm
;; They are not "syntactic analysis".
(define (maybe-substitute var term succeed dict fail)
  (cond ((and (match:element-var? term)
              (match:vars-equal? var term))
          ;; similar to the following action for match:vars-equal? in do-substitute.
          (succeed dict 1 fail))
        ((match:has-binding? var dict)
          ;; modified
          ;; 0. Here var is always as pattern, which is fine due to bidirectional match.
          ;; This is just how unify:gdispatch does for match:element-var?.
          ;; 1. *NOTICE* this has problems since it will call match:compile-pattern at runtime which will do "syntactic analysis".
          ;; IMHO this can't be avoided since 
          ;; 1.a. we need to decide what matcher to call depending on the var val.
          ;; the original matcher.scm doesn't need since term must be constant.
          ;; And the val is also must constant. So we *must* check like match:eqv although using equal?.
          ;; 1.b. match:get-value can be only done at runtime.
          ((match:compile-pattern (match:get-value var dict)) term succeed dict fail))
        (else
          (let ((dict* (do-substitute var term dict)))
            (if dict*
                (succeed dict* 1 fail)
                (fail))))))

(define (maybe-substitute-var-pattern var terms)
  (define (unify-substitute dict succeed fail)
    (let ((term (car terms)))
      (maybe-substitute var term succeed dict fail)))
  unify-substitute)
(define (maybe-substitute-var-data term var-first)
  (define (unify-substitute dict succeed fail)
    (let ((var (car var-first)))
      (maybe-substitute var term succeed dict fail)))
  unify-substitute)

;;; Segment variable extensions
;; ignored