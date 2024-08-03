(load "minimal_debug_unspecific.scm")

;;; No idea why these aren't provided by MIT Scheme already
(define (wt-tree->alist wt-tree)
  (wt-tree/fold (lambda (key value list) (cons (cons key value) list))
                '()
                wt-tree))

(define (wt-tree/update wt-tree key f default)
  (let ((current (wt-tree/lookup wt-tree key default)))
    (wt-tree/add wt-tree key (f current))))

(define symbol-wt-tree-type (make-wt-tree-type symbol<?))

(define (adjacency-pairs->wt-tree pairs)
  (fold
   (lambda (p wt-tree)
     (wt-tree/update wt-tree (car p) (lambda (list) (cons (cdr p) list)) '()))
   (make-wt-tree symbol-wt-tree-type)
   pairs))

(displayln (wt-tree->alist (adjacency-pairs->wt-tree unit-conversion-pairs))) 