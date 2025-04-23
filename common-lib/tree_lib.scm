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

(define tree? list?)
(define (last-in-tree tree)
  (assert (tree? tree))
  (let lp ((final (last tree)))
    (if (tree? final)
      (lp (last final))
      final
      )
    )
  )
