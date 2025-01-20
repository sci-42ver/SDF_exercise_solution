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
