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
(load "4_10_unify_lib_generic.scm")

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
;; This will trace all the-generic-procedure's...
; (trace unify:gdispatch)

; (trace maybe-substitute)
; (trace do-substitute)
; (trace match:dict-substitution)
; (trace unify:dispatch)
(unifier a b)
; (#(ben franklin) ((? bmo) 6 1705) (apr 17 1790))
(unifier c (unifier a b))
; ((ben franklin) (jan 6 1705) (apr 17 1790))

