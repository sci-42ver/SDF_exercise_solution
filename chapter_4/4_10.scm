(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'unification)

;; compared with 4.8
;; In there, match:list uses `(length data-list)`, `((pair? data-list) #f)` etc which assumes list.
;; But here `unify:list-terms` will just let *generic* unify:dispatch manipulate with these.

;; But here we still uses car etc which assumes list...
;; IGNORE: Since the exercise doesn't allow transformation into list, I will just 
;; But here all are dispatched by unify:gdispatch after being dispatched by unify:dispatch.
;; So we can use wrapper to use new car etc.

; (define old-unify:dispatch unify:dispatch)
; (define (unify:dispatch-fail terms1 terms2)
;   (error (list "unify:dispatch unsupported for:" terms1 terms2)))

; (define unify:dispatch
;   (simple-generic-procedure 'unify:dispatch 2 unify:dispatch-fail))
; (define-generic-procedure-handler unify:dispatch
;   (match-args list? list?)
;   old-unify:dispatch)

(load "4_10_vec_lib.scm")

; (define (unify:dispatch-vec terms1 terms2)
;   (assert (vector? terms1))
;   (assert (vector? terms2))
;   (define (unify-dispatcher dict succeed fail)
;     (if (and (null? terms1) (null? terms2))
;         (succeed dict fail terms1 terms2)
;         ((unify:gdispatch terms1 terms2)
;          dict
;          ;; Why not pass succeed is same as SICP since we *only* need fail to try the next alternative.
;          ;; The other 3 args are needed for later matches.
;          (lambda (dict* fail* rest1 rest2)
;            ((unify:dispatch rest1 rest2)
;             dict* succeed fail*))
;          fail)))
;   unify-dispatcher)

; (define-generic-procedure-handler unify:dispatch
;   (match-args vector? vector?)
;   unify:dispatch-vec)

;; gdispatch modification
; (define (vec-car-satisfies pred)
;   (lambda (terms)
;     (and (vec-not-null? terms)
;          (pred (car terms)))))
; (define-generic-procedure-handler unify:gdispatch
;   (match-args (car-satisfies constant-term?)
;               (car-satisfies constant-term?))
;   unify:constant-terms)

;; all are done by 4_10_vec_lib except for cons.
(define (maybe-substitute var-first terms)
  (define (unify-substitute dict succeed fail)
    (let ((var (car var-first)) (rest1 (cdr var-first))
          (term (car terms)) (rest2 (cdr terms)))
      (cond ((and (match:element-var? term)
                  (match:vars-equal? var term))
             ;; similar to the following action for match:vars-equal? in do-substitute.
             (succeed dict fail rest1 rest2))
            ((match:has-binding? var dict)
             ;; will check consistency later.
             ((unify:dispatch (general-cons (match:get-value var dict) rest1)
                              terms)
              dict succeed fail))
            (else
             (let ((dict* (do-substitute var term dict)))
               (if dict*
                   (succeed dict* fail rest1 rest2)
                   (fail)))))))
  unify-substitute)

;; test considering same as 4.8
;; 0. all 4 nested cases are considered (i.e. vector/list in vector/list)
;; 1. # can correspond to list.
(define a
  '(#((? gn) franklin) (? bdate) ((? dmo) (? dday) 1790)))

(define b
  ;; not list.
  '#((ben franklin) ((? bmo) 6 1705) #(apr 17 (? dyear))))

(define c
  '((ben (? fn)) (jan (? bday) 1705) (apr 17 (? dyear))))

a
b
(trace unify:gdispatch)
(trace unify:dispatch)
(unifier a b)
; (unifier c (unifier a b))
