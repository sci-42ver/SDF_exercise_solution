(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'pattern-matching-on-graphs)

;;; 1
(pp all-knight-moves)
;; not <source> etc since no specific overloading of pp by define-pp-describer as done in SDF_exercises/software/sdf/user-defined-types/tagging.scm.

;;; 2
;; set-piece-at may call connect! in graph-node-view created by node-at.
;; This will only occur for black-move.
;; 0. (board 'set-piece-at to my-piece)
;; This fails for graph-node?, so not call graph-node-view.
;; 0.a. So only connect-up-square's 2nd (node 'connect! label...) will call graph-node-view.
;; 
