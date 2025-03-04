(define list-of-lists '((1 2) (3 4) (5 6)))
(reduce-right append '() list-of-lists)

(fold-right append '() list-of-lists)
;; srfi1 is wrong
(fold-right append (car list-of-lists) (cdr list-of-lists))
;; MIT/GNU Scheme doc is also wrong
(append (car list-of-lists) (reduce append '() (cdr list-of-lists)))

; (cd "~/SICP_SDF/SDF_exercises/chapter_5")
; (load "../software/sdf/manager/load.scm")
; (manage 'new 'generic-interpreter)
; ;; 
; (define arg1 (lambda (a) (* a a)))
; (define arg2 (lambda (a) (+ a a)))
; (arg2 (arg1 3))
; ((compose arg1 arg2) 3)
