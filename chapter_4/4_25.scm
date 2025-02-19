;; https://www-users.york.ac.uk/~djp10/Papers/gragra.94.pdf i.e. https://link.springer.com/chapter/10.1007/3-540-61228-9_80
;;; See 3 Unification
;; 0*. Hypergraph https://en.wikipedia.org/wiki/Hypergraph
;; we just change the edge-value to be possibly one node-list instead of one single node
;; 1*. segment in Figure 2
;; 2. p10 Example 16 is about transformation instead of Unification.
;; 3. Most of them are about theorem proof which I won't dig into.
;; I checked it roughly and checked all figures. 
;; 4*. we can unify the whole graph as Figure 3 shows.
;; We can just represent one graph as tagged path lists in matcher although a bit great space complexity.
;; 5. double pushout is skipped

;;; https://arxiv.org/pdf/2104.06186
;; is about unification of 2 problems instead of graphs.

;;; possible different basic ideas:
;; 0. https://discuss.ocaml.org/t/expression-tree-and-graph-pattern-matching/10911/3
;; edges table -> Adjacency matrix https://stackoverflow.com/questions/4780027/how-can-i-represent-edges-for-a-graph-in-terms-of-a-list/4780085#4780085

;;; Google: the most complex graph in "graph theory"
;; https://math.stackexchange.com/questions/1418340/extremely-difficult-graph-theory-question
;; https://www.quora.com/Whats-the-hardest-problem-in-graph-theory
;; https://www.reddit.com/r/math/comments/15rneke/comment/jw9if5f/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button
;; all of them are about graph theory problem instead of one specific graph.
;; https://www.sciencedirect.com/science/article/abs/pii/S0378437108000319 is not about structure.
;; > For all measures, the most complex graph has a medium number of edges

;;; Google: the most complex graph "structure" in "graph theory"
;; https://www.geeksforgeeks.org/graph-types-and-applications/
;; 0*. Undirected Graph: so we need to check the edge backwards for target
;; 1*. Weighted Graphs (so we need change north to something like (north (1 4))) to mean weight between (1 4)
;; 2*. Trees/Cycles can be checked with dict.
;; skipped: point 5,9,10 (about edge number), 6 (restricted for nodes of each edge)
;; https://mathoverflow.net/a/25930
;; is about "concise description", so skipped. TODO https://en.wikipedia.org/wiki/Voltage_graph

;;; In summary
;; we can do
;; 0. Hypergraph
;; where succeed is (lambda (node-object/nodes-object dict*) ...).
;; We can use predicate list-graph-node? to differentiate them.
;; 1. segment
;; 2. See the above (regex [0-9]\*) etc.

;; Here segment is a bit different from match:segment in SDF_exercises/software/sdf/design-of-the-matcher/matcher.scm
;; since graph is not 1d.
;; Normally segment is denoted with (start (?? ...) end), so we need to check all possibilities between "start" and "end". 

;;; related with 4.23
;; 0. En passant
;; We can add more tags to piece like (advances-two-squares-initial)
;; so pawn can do path ((? source... (occupied-by-and-west/east-occupied-by-advances-two-squares-initial-just-now pawn)) northwest/northeast (? unoccupied)) 
;; Here unoccupied is implied the recent pawn enemy move (i.e. advances two squares).
;; 1. promotion can be done by change-piece-type which can be used with set-piece-at.
;; So we uses set-piece-at* which checks whether address is "eighth rank" and piece type is pawn.
;; If so, we do promotion depending one the (read) result.

;;;; Implementation
;;; the 1st paragraph
;; 0. Hypergraph just use one node-list as the value for (connect! label value).
;; Again this is directed.
;; So it is same as https://www.sciencedirect.com/science/article/pii/S0304397516002097#:~:text=A%20directed%20hypergraph%20consists%20of,from%20digraphs%20to%20directed%20hypergraphs.
;; > A directed hypergraph consists of a set of vertices V and a set of hyperarcs H, where a hyperarc is a pair <S,v>, S non empty subset of V and $v \in V$.
;; 1. As the above shows, "segment" needs to "find all paths between 2 nodes not allowing cycle".
;; So just DFS https://stackoverflow.com/a/7283870/21294350 or BFS https://stackoverflow.com/a/7283870/21294350
;; For the latter, cyclic graph needs "visited" to avoid "infinite number of paths" as the former link says.
;; 1.a. "uniform cost search" is used for "weighted version" as https://web.archive.org/web/20121113111112/http://en.wikipedia.org/wiki/Uniform-cost_search#Relationship_to_other_algorithms 
;; > Breadth-first search (BFS) is a special case of uniform-cost search when all edge costs are positive and *identical*.
;; 1.a.0. It uses "priority queue" to optimize queue base operation https://en.wikipedia.org/wiki/Priority_queue#Operations.
;; > In a priority queue, elements with high priority are served before elements with low priority
;; 1.b. So in summary, here DFS or BFS is chosen for this non-weighted graph.
;; 4 (skipped due to being too general). This is more about graph similarity IMHO where https://en.wikipedia.org/wiki/Edge_contraction (w can be one element variable) etc may be allowed (I learnt about this before in DMIA but a bit forgotten about the name. This link is offered by google AI summary.).
;; The naive thoughts may be unifying many edge pairs, but how to construct these pairs (see https://en.wikipedia.org/wiki/Graph_isomorphism)?
;; 4.a. "Inexact graph matching" https://en.wikipedia.org/wiki/Graph_matching is just like the above "segment".
;; 4.a.0. "attributed graphs" is one a bit special type of graph, so skipped.
;; 4.b. one possible open-source implementation https://mathoverflow.net/a/224390 although not meaning unification.

;;; the 2nd paragraph
;; 0. just do bidirectional connect! for each connect!.
;; 1. So we need to unify between weighted edges.
;; IGNORE: One simple way to do that is incorporate the edge weight into the edge value.
;; The above IGNORE part is wrong since "edge value" is one node which may be connected with *multiple* nodes.
;; 1.a. So we need to make edge also as one object.
;; Then the total SDF_exercises/software/sdf/pattern-matching-on-graphs/graph-match.scm lib needs big changes. But the basic ideas are same.
;; 2. This is similar to SICP 3.19.
