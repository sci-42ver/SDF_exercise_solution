;; https://htdp.org/2003-09-26/Solutions/find-route4.html
(define (printf . args)
  (newline)
  (for-each 
    display
    args))

#| ---------------------------------------------------------------------------------

Find Route in Graph: Tests

find-route : node node graph -> (listof node) or #f
Purpose: produce a list of nodes, starting with origination 
and ending destination. The list represent a path from 
the origination node to the destination node in a-graph.
If there is no path, the function produces #f. 
|#

;; We need `visited` https://stackoverflow.com/a/23677118/21294350, i.e. `discovered` https://en.wikipedia.org/wiki/Depth-first_search#Pseudocode
(define (set-minus set-1 set-2)
  (remove (lambda (x) (member x set-2)) set-1))
(define visited '())

(define (find-route origination destination graph)
  (set! visited '())
  (find-route-helper origination destination graph))

(define (find-route-helper origination destination graph)
  (printf "(find-route ~s ~s cyclic-graph)~n" origination destination)
  (cond
    ((eq? origination destination) (list destination))
    (else 
      (if (member origination visited)
        '()
        (begin
          (printf "neighbors of " origination " are " (neighbors origination graph))
          ;; > label v as discovered
          (set! visited (cons origination visited))
          (let ((possible-route 
                  (find-route/list (set-minus (neighbors origination graph) visited) destination graph)))
            (cond
              ((boolean? possible-route) #f)
              (else (cons origination possible-route)))))))))

#| find-route/list : (listof node) node graph -> (listof node) or #f
Purpose: produce a list of nodes, starting with one node on lo-originations 
and ending destination. The list represent a path from 
the node on lo-originations to destination in a-graph.
If there is no path, the function produces #f.     |#
(define (find-route/list lo-Os D graph)
  (printf "(find-route ~s ~s cyclic-graph)~n" lo-Os D)
  (cond
    ((null? lo-Os) #f)
    (else 
      (let ((next-node (first lo-Os)))
        (if (member next-node visited)
          '()
          ;; > if vertex w is not labeled as discovered then
          (begin
            ;; comment the following out to avoid inserting twice.
            ; (set! visited (cons next-node visited))
            (let ((possible-route (find-route-helper next-node D graph)))
              (cond
                ((boolean? possible-route) (find-route/list (cdr lo-Os) D graph))
                (else possible-route)))))))))

(define (neighbors a-node a-graph)
  (second (assq a-node a-graph)))

#| ---------------------------------------------------------------------------------
Tests: data followed by expessions |#

(define Graph 
  '((A (B E))
    (B (E F))
    (C (B D))
    (D ())
    (E (C F))
    (F (D G))
    (G ())))

(assert (equal? (list 'A 'B 'E 'F 'G) (find-route 'A 'G Graph)))
(assert (equal? '(c b e f g) (find-route 'C 'G Graph)))
(assert (not (find-route 'G 'C Graph)))

;; Here DFS doesn't ensure to find the shortest path. Here I didn't intend to find that since it is beyond what SDf intends to teach.
;; See https://medium.com/@buketsenturk/dfs-vs-bfs-51cae3ff881a#:~:text=DFS%20uses%20a%20stack%20(either,not%20guarantee%20an%20optimal%20solution.
(assert (equal? '(a b e) (find-route 'A 'E Graph)))
