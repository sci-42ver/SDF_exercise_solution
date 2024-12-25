; (define (tree-partial-map f tree)
;   ;; https://stackoverflow.com/a/35301730/21294350
;   (cond 
;     ((match:segment-var? tree) (f tree))
;     ((pair? tree) 
;       ;; see SICP_SDF/lecs/6.001_spring_2007_recitation/codes/rec14/tree-ops.scm
;       ; (cons 
;       ;   (tree-partial-map f (car tree)) 
;       ;   (map (lambda (t) (tree-partial-map f t)) (cdr tree)))
;       (map (lambda (t) (tree-partial-map f t)) tree)
;       )
;     (else tree)
;     )
;   )

;; https://stackoverflow.com/a/35530909/21294350
;; better to use filter
(define (tree-fold* non-base-f base-f n sxp base?)
  (let loop ((sxp sxp) (res n))
    (cond
      ((null? sxp) res)
      ((base? sxp) (base-f sxp res))
      ((pair? sxp) (loop (car sxp) (loop (cdr sxp) res)))
      (else        (non-base-f sxp res))))
  )

