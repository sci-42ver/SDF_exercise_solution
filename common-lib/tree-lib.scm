;; IGNORE similar to tree-copy
;; see SICP_SDF/lecs/6.001_spring_2007_recitation/codes/rec14/tree-ops.scm
(define (tree-map proc tree elem?)
  (if (elem? tree)
    (begin
      ; (pp elem?)
      (proc tree))
    (begin
      ; (write-line "tree-map recursive call")
      (map (lambda (e) (tree-map proc e elem?)) tree)
      ; (map (lambda (e) (tree-map proc e)) tree)
      )
    )
  )