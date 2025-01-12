(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'pattern-matching-on-graphs)

;; google "game tree structure"
;; http://cgm.cs.mcgill.ca/~avis/Kyoto/courses/ia/2013/notes/luc_devroye.htm#:~:text=The%20game%20tree%20consists%20of,of%20the%20game%20as%20desired.
;; See "MiniMax Game Tree"
;; 0. Bounded Lookahead or Alpha-Beta are both said in SICP reference https://stackoverflow.com/q/79150263/21294350 -> https://www.cs.kent.ac.uk/people/staff/dat/miranda/whyfp90.pdf.
;; the former
;; > prune ...
;; the latter
;; > Strangely enough, it is unnecessary to know the value of the question mark in order to evaluate the tree.

;; google "game tree "structure""
;; https://www.scaler.com/topics/game-tree-in-ai/
;; still uses "minimax algorithm".

;; So we just use "ordered labeled trees" structure
;; > treeof ∗ ::= Node ∗ (listof (treeof ∗))
;; > This definition says that a tree of ∗s is a node, with a label which is a ∗, and a list of subtrees which are also trees of ∗s.
;; 0. This is similar to Binary Search Tree implementation https://stackoverflow.com/a/67098247/21294350 but with more than 2 children.
;; 1. Also see https://en.wikipedia.org/wiki/Game_tree#Randomized_algorithms_version
;; although it doesn't use minimax

;; > Make an example of such a tree that can be
;; > extended as more plausible moves are considered at each level, and
;; > as more levels are added for consideration.
;; Both can be done by (add-child sub-root child loc-idx)
;; where sub-root can be given location by idx-seq like (3 5) which means the 5th child of the 3rd child of root from left to right.
;; 0. Obviously we need to check whether loc-idx, idx-seq are valid.
;; 1. Here idx-seq can be more abstract to one unique id *number* etc https://stackoverflow.com/a/28696286/21294350
;; That is to *accelerate searching*. I won't do that here.
;; 1.a. Anyway path is better https://community.opentext.com/devops-cloud/uft-one/f/discussions/206064/javatree---select-a-node-by-its-index.

;;; implementation
;; 0. As the above says, we use tree structure where car is val and cdr is subtree children.
(define (make-tree val subtrees)
  (cons val subtrees))
(define get-val car)
(define get-subtrees cdr)
(define tree? pair?)

;; same as g:cons but with the different naming.
(define (g:tree val subtree-graphs)
  (let ((tree (make-graph-node 'tree)))
    (tree 'connect! 'val val)
    (let lp ((idx 0) (rest-trees subtree-graphs))
      (if (null? rest-trees)
        'finished
        (begin
          (tree 'connect! (symbol-append 'subtree (symbol idx)) (car rest-trees))
          (lp (n:+ 1 idx) (cdr rest-trees))    
          ))
      )
    ;; IGNORE: To help check loc-idx in add-subtree
    ;; no use due to not "immutable". See the following.
    ; (tree 'connect! 'subtree-num (length subtrees))
    tree))
(define (g:tree-val tree-lazy-graph)
  (tree-lazy-graph 'edge-value 'val))

(define (tree->lazy-graph tree)
  (write-line (list "to create subtree with val" (get-val tree)))
  (if (pair? tree)
      (g:tree (delay (get-val tree))
              (map 
                (lambda (subtree) (delay (tree->lazy-graph subtree)))
                (get-subtrees tree)
                )
              )
      (g:null)))

;; 0. may throw error by edge-value.
;; 1. Here (assert (graph-node? tree-lazy-graph)) is safe but most of time we assume the arg passed in satisfies this requirement.
(define (get-subtree-by-one-step tree-lazy-graph idx)
  (assert (graph-node? tree-lazy-graph))
  (tree-lazy-graph 'edge-value (symbol-append 'subtree (symbol idx)))
  )
(define (get-subtree-by-mul-steps tree-lazy-graph idx-lst)
  (assert (graph-node? tree-lazy-graph))
  (if (null? idx-lst)
    tree-lazy-graph
    (get-subtree-by-mul-steps 
      (get-subtree-by-one-step tree-lazy-graph (car idx-lst)) 
      (cdr idx-lst)))
  )

;; similar to g:last-pair
(define (get-subtree-num tree-lazy-graph)
  (assert (graph-node? tree-lazy-graph))
  (let lp ((idx 0))
    (if (tree-lazy-graph 'has-edge? (symbol-append 'subtree (symbol idx)))
      (lp (n:+ 1 idx))
      idx)
    ))
(define (get-subtree-lst tree-lazy-graph)
  (let ((cnt (get-subtree-num tree-lazy-graph)))
    (fold-right
      (lambda (idx res)
        (cons (get-subtree-by-one-step tree-lazy-graph idx) res))
      '()
      (iota cnt)
      )
    )
  )

;; 0. Here I assert child to be one tree which may be one leaf.
;; Leaf is considered as one special tree which has null subtrees.
;; 1. > Our graphs are immutable in the sense that once a node or an edge is added, it cannot be modified;
;; so loc-idx must append.
;; So I just drop that arg since the location is implicitly decided by the subtree.
(define (add-subtree tree-lazy-graph subtree-idx-lst child)
  (assert (graph-node? tree-lazy-graph))
  (assert (list? subtree-idx-lst))
  (assert (tree? child))
  (let* ((subtree (get-subtree-by-mul-steps tree-lazy-graph subtree-idx-lst))
         ;; children means subtrees of this subtree
         ;  (subtree-children-num (subtree 'edge-value 'subtree-num))
          (subtree-children-num (get-subtree-num subtree))
         )
    ; (assert (n:))
    (subtree 
      'connect!
      (symbol-append 
        'subtree
        (symbol subtree-children-num)) 
      (tree->lazy-graph child))
    )
  )

;; test1
;; 0
;; /\
;; 1 2
;;  / 
;; 3
;; /\
;;4 5
(define test-tree1
  (make-tree 0
    (list
      (make-tree 1
        (list)
        )
      (make-tree 2
        (list
          (make-tree 3
            (list
              (make-tree 4
                (list)
                )
              (make-tree 5
                (list)
                )
              )
            )
          )
        )
      )
    )
  )

(define graph1 (tree->lazy-graph test-tree1))

;; error expectedly
; (add-subtree graph1 '(0) 1)
;; > as more levels are added for consideration
(add-subtree graph1 '(0) 
  (make-tree 6
    (list)
    ))
;; > more plausible moves are considered at each level
(add-subtree graph1 '() 
  (make-tree 7
    (list)
    ))
(add-subtree graph1 '(1 0) 
  (make-tree 10
    (list)
    ))

;; error expectedly
; (add-subtree graph1 '(3) 1)

(add-subtree graph1 '(1 0 0) 
  (make-tree 8
    (list)
    ))

(add-subtree graph1 '(1 0 1) 
  (make-tree 9
    (list)
    ))

;;     0
;;   / | \
;;  1  2 7
;; /   | 
;; 6  3
;;  / | \
;; 4 5 10
;; / /
;; 8 9

;;; verification of the whole graph similar to the book demo
(define (make-idx-seq-paired-with-graph idx-seq tree-lazy-graph)
  (list idx-seq tree-lazy-graph))
(define get-idx-seq car)
(define get-graph cadr)
(define (get-subtree-lst* idxed-tree-lazy-graph)
  (let ((idx-seq (get-idx-seq idxed-tree-lazy-graph))
        (tree-lazy-graph (get-graph idxed-tree-lazy-graph)))
    (let ((cnt (get-subtree-num tree-lazy-graph)))
      (fold-right
        (lambda (idx res)
          (cons 
            (make-idx-seq-paired-with-graph 
              (append idx-seq (list idx))
              (get-subtree-by-one-step tree-lazy-graph idx)) 
            res))
        '()
        (iota cnt)
        )
      )
    )
  )
(define (display-tree-graph idxed-tree-lazy-graph-lst level)
  (if (null? idxed-tree-lazy-graph-lst)
    'finished
    (begin
      (write-line (list "at level" level))
      (for-each
        (lambda (idxed-tree-lazy-graph)
          (display (list (get-idx-seq idxed-tree-lazy-graph) (g:tree-val (get-graph idxed-tree-lazy-graph))))
          (display " ")
          )
        idxed-tree-lazy-graph-lst
        )
      ; (write-line "")
      (newline)
      (display-tree-graph 
        (append-map
          get-subtree-lst*
          idxed-tree-lazy-graph-lst
          )
        (n:+ level 1))
      )
    )
  )
(display-tree-graph (list (make-idx-seq-paired-with-graph '() graph1)) 0)
; ("at level" 0)
; (() 0) 
; ("at level" 1)
; ((0) 1) ((1) 2) ((2) 7) 
; ("at level" 2)
; ((0 0) 6) ((1 0) 3) 
; ("at level" 3)
; ((1 0 0) 4) ((1 0 1) 5) ((1 0 2) 10) 
; ("at level" 4)
; ((1 0 0 0) 8) ((1 0 1 0) 9) 
