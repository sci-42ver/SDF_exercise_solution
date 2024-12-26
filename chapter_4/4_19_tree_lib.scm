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
;; TODO better to use filter for var-cnt
;; basic ideas: 
  ;; use one local list to collect filtered elem's in one flattened style
  ;; Then length.
(define (tree-fold* non-base-f base-f n sxp base?)
  (let loop ((sxp sxp) (res n))
    (cond
      ((null? sxp) res)
      ((base? sxp) (base-f sxp res))
      ((pair? sxp) (loop (car sxp) (loop (cdr sxp) res)))
      (else        (non-base-f sxp res))))
  )

;; https://stackoverflow.com/q/62952288/21294350
;; OP has the right ideas but with syntax errors.
;; > But, it fails when the item to be replaced is a list.
(define (replace old new xs)
  (if (null? xs)
      xs
      (let ((head (car xs))
            (tail (cdr xs)))
        (cond ((equal? head old)
               (cons new (replace old new tail)))
              ((atom? head)
               (cons head (replace old new tail)))
              (else
               (cons (replace old new head)
                     (replace old new tail)))))))
