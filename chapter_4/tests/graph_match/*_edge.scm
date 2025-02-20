(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'pattern-matching-on-graphs)

;; > An edge may be labeled by any symbol that is not one of the special ...
(define node1 (make-graph-node "1"))
(define node2 (make-graph-node "2"))
(define node3 (make-graph-node "3"))

(node1 'connect! '* node2)
(node2 'connect! '* node3)

(define (node-has-name? name)
  (lambda (place-node dict)
    (equal? (list name) (place-node 'summarize-self))
    ))

(define path1 `((? source-node ,(node-has-name? "1"))
    (* * (?* middle)) ; here (?) is also fine since no binding is considered (see gmatch:var-matcher where var-name is #f).
    * (? target-node)))

(graph-match path1 (match:new-dict) node1)
