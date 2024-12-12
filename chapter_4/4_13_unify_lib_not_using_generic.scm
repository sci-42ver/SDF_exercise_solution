;; this is modification of SDF_exercises/software/sdf/unification/unify.scm

; (define (unifier pattern1 pattern2)
;   (let ((dict (unify pattern1 pattern2)))
;     (and dict
;          ((match:dict-substitution dict) pattern1))))

; (define (unify pattern1 pattern2)
;   (unify:internal pattern1 pattern2
;                   (match:new-dict)
;                   (lambda (dict) dict)))

; ;;; This is the programmer's interface to the unifier.

; (define (unify:internal pattern1 pattern2 dict succeed)
;   ((match:list (list pattern1) (list pattern2))
;    dict
;    (lambda (dict fail)
;      (assert (list? rest1))
;      (assert (list? rest2))
;      ;; 0. > we will be able to extract multiple
;      ;; > matches by returning #f from succeed, indicating that the result
;      ;; > was not the one wanted.
;      ;; similar to match:segment formerly and also SICP ones which passes fail along.
;      ;; Here is similar to SICP to use fail passed by grab-segment.
;      ;; 1. > The ability to backtrack into the matcher
;      ;; i.e. to some *former* point of the matcher.
;      ;; > algebraic expressions and equation solving
;      ;; allow considering all possibilities like amb in SICP.
;      ;; IMHO at least for (?? ...) which is shown before in book.
;      (or (and (null? rest1) (null? rest2)
;               (succeed dict))
;          ;; SDF_exercises TODO when this happens.
;          (fail)))
;    ;; > If not, unify returns #f, indicating a failure.
;    (lambda () #f)))

; ;;; first1 and terms2 are lists of terms to be equated.

; ;; Here we have 5 data:
; ;; 2 term lists, dict, succeed, fail.
; (define (match:list first1 terms2)
;   (assert (list? first1))
;   (assert (list? terms2))
;   (define (unify-dispatcher dict succeed fail)
;     (if (and (null? first1) (null? terms2))
;         (succeed dict fail first1 terms2)
;         ((unify:gdispatch first1 terms2)
;          dict
;          ;; Why not pass succeed is same as SICP since we *only* need fail to try the next alternative.
;          ;; The other 3 args are needed for later matches.
;          (lambda (dict* fail*)
;            ((match:list)
;             dict* succeed fail*))
;          fail)))
;   unify-dispatcher)

; (define (unify:fail first1 terms2)
;   (define (unify-fail dict succeed fail)
;     (fail))
;   unify-fail)

; (define unify:gdispatch
;   (simple-generic-procedure 'unify 2 unify:fail))

; (define (car-satisfies pred)
;   (lambda (terms)
;     (and (pair? terms)
;          (pred (car terms)))))

;; move outside to make maybe-substitute convenient.
(define (unify-constants first1 terms2 dict succeed fail)
  (if (eqv? first1 (car terms2))
      ;; Here rest1 rest2 will be manipulated by match:list.
      ; (succeed dict fail rest1 rest2)
      ;; compatible with matcher
      (succeed dict 1 fail)
      (fail)))
(define (unify:constant-terms first1 terms2)
  unify-constants)

; (define (constant-term? term)
;   (and (not (match:var? term))
;        (not (list? term))))

; ;; coderef: unify-constant-terms
; (define-generic-procedure-handler unify:gdispatch
;   (match-args (car-satisfies constant-term?)
;               (car-satisfies constant-term?))
;   unify:constant-terms)

(define (unify:list-terms first1 terms2)
  (define (unify-lists dict succeed fail)
    ((match:list first1 (car terms2))
      dict
      (lambda (dict* fail*)
        ;; the 2nd is done by match:list
        ;; the 1st is done when "syntactic analysis".
        ; (assert (null? null1))
        ; (assert (null? null2))
        (succeed dict* 1 fail*))
      fail))
  unify-lists)

; (define (list-term? term)
;   (and (not (match:var? term))
;        (list? term)))

; ;; coderef: unify-list-terms
; (define-generic-procedure-handler unify:gdispatch
;   (match-args (car-satisfies list-term?)
;               (car-satisfies list-term?))
;   unify:list-terms)

;;; This is the syntactic equation solver for element vars.

;; added
(define (maybe-substitute var term succeed dict fail)
  (cond ((and (match:element-var? term)
              (match:vars-equal? var term))
          ;; similar to the following action for match:vars-equal? in do-substitute.
          (succeed dict 1 fail))
        ((match:has-binding? var dict)
          ;; modified
          ;; Here var is always as pattern, which is fine due to bidirectional match.
          ;; This is just how unify:gdispatch does for match:element-var?.
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

; #|
; ;;; A student in 6.5150, Andres D Buritica Monroy
; ;;; discovered this has a bug in 2024.  

; (define (do-substitute var term dict)
;   (let ((term* ((match:dict-substitution dict) term)))
;     (and (match:satisfies-restriction? var term*)
;          (or (and (match:var? term*)
;                   (match:vars-equal? var term*))
;              (not (match:occurs-in? var term*)))
;          (match:extend-dict var term*
;            (match:map-dict-values
;             (match:single-substitution var term*)
;             dict)))))
; |#

; ;; substitute var with the *value* of term in dict *recursively* when *necessary*.
; (define (do-substitute var term dict)
;   ;; This substitution is needed for `match:satisfies-restriction?`.
;   (let ((term* ((match:dict-substitution dict) term)))
;     (and (match:satisfies-restriction? var term*)
;          (if (and (match:var? term*)
;                   (match:vars-equal? var term*))
;              ;; difference
;              ;; No need to replace by itself.
;              dict
;              ;; Same as SICP depends-on?
;              (and (not (match:occurs-in? var term*))
;                   (match:extend-dict var term*
;                     (match:map-dict-values
;                      ;; 0. change val to new val.
;                      ;; recursion is done by match:map-vars.
;                      ;; 1. This may be not needed since match:dict-substitution at last will also do this.
;                      ;; This may be put here for efficiency.
;                      ;; The book just says...
;                      ;; > must also be cleaned of references to var .
;                      (match:single-substitution var term*)
;                      dict)))))))

; (define (element-1? term)
;   (any-object? term))

; (define (element? term)
;   (not (match:segment-var? term)))

; ;; coderef: element-var-thing
; (define-generic-procedure-handler unify:gdispatch
;   (match-args (car-satisfies match:element-var?)
;               (car-satisfies element?))
;   (lambda (var-first terms)
;     (maybe-substitute var-first terms)))

; ;; coderef: thing-element-var
; (define-generic-procedure-handler unify:gdispatch
;   (match-args (car-satisfies element?)
;               (car-satisfies match:element-var?))
;   (lambda (terms var-first)
;     (maybe-substitute var-first terms)))

;;; Segment variable extensions
;; ignored