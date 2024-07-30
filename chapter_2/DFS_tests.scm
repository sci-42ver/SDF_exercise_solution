(load "DFS_demo.scm")
(assert (equal? (list 'A 'B 'E 'F 'G) (find-route 'A 'G Graph)))
; = (list 'A 'B 'E 'F 'G)

(assert (equal? '(c b e f g) (find-route 'C 'G Graph)))
(assert (not (find-route 'G 'C Graph)))
; = #f

;; Here DFS doesn't ensure to find the shortest path. Here I didn't intend to find that since it is beyond what SDf intends to teach.
(assert (equal? '(a b e) (find-route 'A 'E Graph)))