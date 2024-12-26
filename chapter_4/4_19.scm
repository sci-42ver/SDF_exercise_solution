(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'unification)

;;; 3 problems
;; > We may miss some matches; 
;; See d.
;; > we may generate multiple copies of the same solution; 
;; See b.
;; > and some of the solutions, although
;; > valid solutions of the problem to make the input patterns equal, are
;; > not maximally general.
;; See c.

;;; a.
;; > This is not hard if you use assignments, but it might
;; > be more fun to look for a functional solution—but don't try too
;; > hard!
;; If using "functional solution", we need to pass that dict collector to succeed and fail so that it can be accumulated.
;; This is one routine work to do interface changes, maybe there are other problems...

;; IMHO here we just add the new dict into the collector for each top succeed call.
;; There is no need to pass that collector around since all other interfaces don't need that.
(define (add-data-to-tagged-list lst data)
  (set-cdr! lst (cons data (cdr lst)))
  lst
  )
(define (unify:internal-functional-wrapper pattern1 pattern2 dict succeed)
  (let ((collector (list 'collector)))
    (unify:internal pattern1 pattern2 dict
                    (lambda (dict)
                      (add-data-to-tagged-list collector dict)
                      (succeed dict)
                      #f ; to ensure collector can get all solutions.
                      ))
    collector
    )
  )

(let ((p1 '(a (?? x) (?? y) (?? x) c))
      (p2 '(a b b b (?? w) b b b c)))
  (pp (unify:internal-functional-wrapper p1 p2 (match:new-dict)
                                     (lambda (dict)
                                       (pp (match:bindings dict))
                                       #f))))
; (collector (dict (w () ??) (y () ??) (x (b b b) ??))
;            (dict (w ((?? y)) ??) (x (b b b) ??))
;            (dict (y () ??) (w () ??) (x (b b b) ??))
;            (dict (y ((?? w)) ??) (x (b b b) ??))
;            (dict (w () ??) (y () ??) (x (b b b) ??))
;            (dict (w () ??) (y () ??) (x (b b b) ??))
;            (dict (y (b (?? w) b) ??) (x (b b) ??))
;            (dict (y (b b (?? w) b b) ??) (x (b) ??))
;            (dict (y (b b b (?? w) b b b) ??) (x () ??)))


(let ((pattern '(* (?? a) (+ (?? b)) (?? c)))
      (expression '(* x y (+ z w) m (+ n o) p)))
  (pp (unify:internal-functional-wrapper pattern expression (match:new-dict)
                                     (lambda (dict)
                                       (pp (match:bindings dict))
                                       #f))))
; (collector 
;   (dict (c (p) ??) (b (n o) ??) (a (x y (+ z w) m) ??)) 
;   (dict (c (m (+ n o) p) ??) (b (z w) ??) (a (x y) ??)))

(let ((p1 '(a (?? x) (?? y) (?? x) c))
      (p2 '(a b b b (?? w) b b b c)))
  (unify:internal p1 p2 (match:new-dict)
                  (lambda (dict)
                    (and dict
                         (let ((subst (match:dict-substitution dict)))
                           (let ((p1* (subst p1)) (p2* (subst p2)))
                             (if (not (equal? p1* p2*))
                               (error "Bad dictionary"))
                             (pp p1*))))
                    #f)))

;;; b.
;; > The name of a variable
;; > doesn't matter, so two resulting dictionaries represent the same
;; > solution if you can get one by uniformly renaming variables in
;; > the other.
;; i.e. ((y ((?? w)) ??) (x (b b b) ??)) with ((w ((?? y)) ??) (x (b b b) ??))
(define (new-collector data)
  (cons 'collector data))
(define tagged-list-data cdr)
(define (unify:internal-functional-wrapper-with-substitution pattern1 pattern2 dict succeed)
  (let ((collector (unify:internal-functional-wrapper pattern1 pattern2 dict succeed)))
    (new-collector 
      (map 
        (lambda (dict) 
          ;; same as the above.
          (and dict
              (let ((subst (match:dict-substitution dict)))
                (let ((p1* (subst pattern1)) (p2* (subst pattern2)))
                  ;; > You can check that the results of the two substitutions are equal
                  (if (not (equal? p1* p2*))
                    (error "Bad dictionary"))
                  p1*)))
          ) 
        (tagged-list-data collector)))
    )
  )
;; I assume substitution is the result of (subst p1)
;; and "result" is dict.
;; > Now save the pair of one substitution and one result for each distinct result.
(define (make-solution-pair substitution result-dict)
  (cons substitution result-dict))
(define substitution-in-pair car)
(define result-dict-in-pair cdr)

(load "4_19_rename_dict_lib.scm")
(define (sort-dict dict)
  (match:new-bindings dict (sort (match:bindings dict) symbol<? car)))
;; unify-proc is to allow further extension
(define (same-pair-for-solution? pair1 pair2 unify-proc)
  (let ((pair1-subst (substitution-in-pair pair1))
        (pair2-subst (substitution-in-pair pair2))
        (pair1-result (result-dict-in-pair pair1))
        (pair2-result (result-dict-in-pair pair2))
        )
    (or 
      ;; substitution match
      (equal? pair1-subst pair2-subst)
      ;; > uniformly renaming variables
      ;; unify will ensure constant terms are correspondingly same.
      ;; And var's bindings implies "uniformly renaming" where uniform is implied by that we check unify consistency after substitution with binding value.
      (unify-proc pair1-subst pair2-subst)
      ;; dict match
      (let ((pair1-result* (sort-dict pair1-result))
            (pair2-result* (sort-dict pair2-result))
            )
        (equal? pair1-result* pair2-result*))
      (same-dict? pair1-result pair2-result)
      )
    )
  )
(define (new-pairs data)
  (cons 'pairs data))
(define (unify:internal-functional-wrapper-with-substitution-unique-pairs pattern1 pattern2 dict succeed unify-proc)
  (let ((collector (unify:internal-functional-wrapper pattern1 pattern2 dict succeed))
        (pairs (new-pairs '()))
        )
    (for-each ; modified 
      (lambda (dict) 
        ;; same as the above.
        (and dict
            (let ((subst (match:dict-substitution dict)))
              (let ((p1* (subst pattern1)) (p2* (subst pattern2)))
                ;; > You can check that the results of the two substitutions are equal
                (if (not (equal? p1* p2*))
                  (error "Bad dictionary"))
                ;; modified
                (let ((solution (make-solution-pair p1* dict)))
                  ;; Here if same, we keep pair's in (tagged-list-data pairs).
                  ;; Then based on substitution-instance?, when same we want to keep substitution1.
                  ;; So we make pair as pair1 for same-pair-for-solution?.
                  (if (any (lambda (pair) (same-pair-for-solution? pair solution unify-proc)) (tagged-list-data pairs))
                    'skipped
                    (add-data-to-tagged-list pairs solution))
                  )
                )))
        ) 
      (tagged-list-data collector))
    pairs
    )
  )

; (trace same-pair-for-solution?)
(let ((pattern '(* (?? a) (+ (?? b)) (?? c)))
      (expression '(* x y (+ z w) m (+ n o) p)))
  (pp (unify:internal-functional-wrapper-with-substitution-unique-pairs pattern expression (match:new-dict)
                                     (lambda (dict)
                                       (pp (match:bindings dict))
                                       #f)
                                      unify
                                      )))
;; The above can't be checked for sameness by dict
;; since they have non-compatible values for the same var-seq.
(define (main-test unify-proc)
  (let ((p1 '(a (?? x) (?? y) (?? x) c))
        (p2 '(a b b b (?? w) b b b c)))
    (unify:internal-functional-wrapper-with-substitution-unique-pairs p1 p2 (match:new-dict)
                                      (lambda (dict)
                                        (pp (match:bindings dict))
                                        #f)
                                      unify-proc
                                      ))
  )
(pp (main-test unify))
;; can recognize (y ((?? w)) ??) with (w ((?? y)) ??).
; [Entering #[compound-procedure same-pair-for-solution?]
;     Args: ((a b b b (?? w) b b b c) dict (y ((?? w)) ??) (x (b b b) ??))
;           ((a b b b (?? y) b b b c) dict (w ((?? y)) ??) (x (b b b) ??))]
; ("same-dict?" ((dict (y ((?? w)) ??) (x (b b b) ??)) (dict (w ((?? y)) ??) (x (b b b) ??)))
;               ((dict ((?? y) ((?? w))) ((?? x) (b b b))) (dict ((?? w) ((?? y))) ((?? x) (b b b))))
;               ((dict (x ((? x:24)) ??) (w ((? w:23)) ??) (y ((? y:22)) ??)) (dict (x ((? x:21)) ??) (w ((? w:20)) ??) (y ((? y:19)) ??)))
;               ((dict ((? y:22) ((? w:23))) ((? x:24) (b b b))) (dict ((? w:20) ((? y:19))) ((? x:21) (b b b)))))
; [#t
;       <== #[compound-procedure same-pair-for-solution?]
;     Args: ((a b b b (?? w) b b b c) dict (y ((?? w)) ??) (x (b b b) ??))
;           ((a b b b (?? y) b b b c) dict (w ((?? y)) ??) (x (b b b) ??))]
;; result when only using "dict match".
; (pairs ((a b b b (?? w) b b b c) dict (y (b b b (?? w) b b b) ??) (x () ??))
;        ((a b b b (?? w) b b b c) dict (y (b b (?? w) b b) ??) (x (b) ??))
;        ((a b b b (?? w) b b b c) dict (y (b (?? w) b) ??) (x (b b) ??))
;        ((a b b b (?? y) b b b c) dict (w ((?? y)) ??) (x (b b b) ??))
;        ((a b b b b b b c) dict (w () ??) (y () ??) (x (b b b) ??)))
;; result when also using "substitution match". This can be improved with c.
; (pairs ((a b b b b b b c) dict (w () ??) (y () ??) (x (b b b) ??)))

;;; c.
;; > If any result in the collection is a substitution instance of
;; > another result, it is not a most general common specialization of
;; > the two inputs.
;; i.e. one can be got by another result by substituting some vars in that result with some values uniformly.
;; e.g. (a b b b (?? y) b b b c)-((?? w):(c))>(a b b b b b b c)
;; Again unify but we keep the general one.
(load "4_19_tree_lib.scm")
(define (var-cnt pattern)
  (tree-fold* 
    (lambda (elm res) (declare (ignore elm)) (+ 0 res)) 
    (lambda (elm res) (declare (ignore elm)) (+ 1 res))
    0
    pattern 
    match:var?)
  )
(var-cnt '(a b b b (?? w) ((?? w) a (b (?? w) c)) b b c))
;; TODO Here I just use var-cnt to compare, this is not fine-grained for some cases.
;; IGNORE: Anyway this is not what this sub-problem tries to deal with.
(define (general>=? subst1 subst2)
  (n:>= (var-cnt subst1) (var-cnt subst2))
  )
;; check whether pair2 is substitution-instance of pair1.
; (define (substitution-instance? pair1 pair2)
;   (let ((pair1-subst (substitution-in-pair pair1))
;         (pair2-subst (substitution-in-pair pair2))
;         )
;     (and (general>=? pair1-subst pair2-subst)
;       (unify pair1-subst pair2-subst)
;       )
;     )
;   )
;; Here I assume dict checking is already done in b.
;; So here it should check between substitutions.
(define (substitution-instance? substitution1 substitution2)
  (and (general>=? substitution1 substitution2)
    (unify substitution1 substitution2)
    )
  )
; (trace substitution-instance?)
; (trace general>=?)
(pp (main-test substitution-instance?))

;; similar to unify:internal-functional-wrapper-with-substitution-unique-pairs
;; but uses fold.
(define (sort-and-remove-substitution-instance-for-pairs pairs unify-proc)
  (let ((sorted (new-pairs (sort (tagged-list-data pairs) general>=? car))))
    ;; We try to add elem's from the most general to the least. So we will only keep the most general ones.
    (fold 
      (lambda (elm res) 
        (if (any (lambda (pair) (same-pair-for-solution? pair elm unify-proc)) (tagged-list-data res))
          res
          (add-data-to-tagged-list res elm)))
      (new-pairs '())
      (tagged-list-data sorted)
      )
    )
  )
(pp 
  (sort-and-remove-substitution-instance-for-pairs
    (main-test substitution-instance?)
    substitution-instance?
    ))

;;; d.
;; 0. IMHO this can be done based on 4.20.
;; So (?? x)->(4 (?? z)) and (?? y)->((?? z) 3)
(unify:internal '(((?? x) 3) ((?? x)))
'((4 (?? y)) (4 5))
(match:new-dict)
(lambda (dict)
(pp (match:bindings dict))
#f))
; ((x (4 5) ??) (y (5 3) ??))
;; This is just one special case.
;; 1. Here we can still use classification by unify:gdispatch (i.e. 3 cases related with match:segment-var?)
;; 1.a. unify:segment-var-var is same since these 2 vars must have the same *starting loc*.
;; So either one contains the other or they are same.
;; 1.b. maybe-grab-segment
;; Here we can use the above notation in 0. to denote the *intermediate* state which is between 2 states in the original slp.
;; That is (?? x)->(4 (?? z)) and (?? y)->((?? z) (?? y-internal:1))
;; Then succeed is called with 2 starting terms 3 and (?? y-internal:1).
;; 1.b.0. Notice here adjacent terms in one pattern should not have intersection.
;; So ((?? x) (?? z)) matched with (4 (?? y)) won't make (?? x) and (?? z) both have (?? z) (however notice they may share the same sub-value which are got from different vars).
;; 1.b.1. IMHO the above are all cases for 2 sub-sequences, i.e. no intersection or partial intersection or total containment.
;; Here only the last is not one symmetric relation. So we have totally 4 relations.


