;; I learned BFS and DFS when learning DMIA. The basic ideas are not hard. I won't code from scratch since this is not what SDF wants to teach.
;; https://stackoverflow.com/q/7805005/21294350
; (define net
; '(("A" "B")
;   ("B" "A" "C")
;   ("C" "B" "D")
;   ("D" "C" "E" "F")
;   ("F" "I" "D")
;   ("I" "F")
;   ("E" "D" "J")
;   ("J" "E" "G")
;   ("G" "J" "H"))) 

; (define (path-demo start finish)
;         (for-each (lambda (x) (display x) (display " "))
;                   (cons "Route:" (shortest-path start finish net))))

; (define (shortest-path start end net)
;   (bfs end (list (list start)) net))

; ;; Breadth-first search
; (define (bfs end queue net)
;   (display queue) (newline) (newline) ; entertainment
;   (if (null? queue)
;     '()
;     (let ((path (car queue)))
;       (let ((node (car path)))
;         (if (equal? node end) ;; Graham used CL eql
;             (reverse path)
;             (bfs end 
;                 (append (cdr queue)
;                         (new-paths path node net))
;                 net))))))

; (define (new-paths path node net)
;   (map (lambda (n) (cons n path)) (cdr (assoc node net))))

; ;;
; (path-demo "J" "I")

;; https://stackoverflow.com/a/9035697/21294350
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

;; See https://en.wikipedia.org/wiki/Breadth-first_search#Pseudocode
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
          ;; > label v as discovered
          (set! visited (cons origination visited))
          (printf "neighbors of " origination " are " (neighbors origination graph))
          (printf "visited: " visited)
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

#| neighbors: node graph -> (listof node)
   (define (neighbors a-node a-graph) ...)
   Purpose: compute a-node's neighbors in a-graph
|#
(define (neighbors a-node a-graph)
  (cond
    ((null? a-graph) (error 'neighbors "can't happen"))
    (else (cond
	    ((eq? (first (first a-graph)) a-node)
	     (second (first a-graph)))
	    (else (neighbors a-node (cdr a-graph)))))))

;; equivalently, with Scheme's built-in lookup function:
(define (neighbors a-node a-graph)
  (second (assq a-node a-graph)))

#| ---------------------------------------------------------------------------------
   Tests: data followed by expessions |#

;; Here assumes cdr of each pair is always one list.
(define Graph 
  '((A (B E))
    (B (E F))
    (C (B D))
    (D ())
    (E (C F))
    (F (D G))
    (G ())))