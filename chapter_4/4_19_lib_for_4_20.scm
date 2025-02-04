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
(define (unify:collector-wrapper pattern1 pattern2 dict succeed)
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

;;; total example bindings
(let ((p1 '(a (?? x) (?? y) (?? x) c))
      (p2 '(a b b b (?? w) b b b c)))
  (pp (unify:collector-wrapper p1 p2 (match:new-dict)
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
  (pp (unify:collector-wrapper pattern expression (match:new-dict)
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
;; 0. i.e. ((y ((?? w)) ??) (x (b b b) ??)) with ((w ((?? y)) ??) (x (b b b) ??))
;; 1. SKIPPED due to about maths: TODO IMHO this "... if ..." needs maths proof.
;; The book assumes that is true.
;; Maybe we should prove first that all solution *structures* are same.
;; So no (a b b b (?? w) b b b c) with (a b b b (?? y) a k b c) etc.
(define (new-collector data)
  (cons 'collector data))
(define tagged-list-data cdr)
(define (unify:collector-wrapper-with-substitution pattern1 pattern2 dict succeed)
  (let ((collector (unify:collector-wrapper pattern1 pattern2 dict succeed)))
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
  ; (pp (list "call same-pair-for-solution? with" pair1 pair2 unify-proc))
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
        ;; Will remove 3 "(w () ??) (y () ??)"'s from 9 bindings in "total example bindings"
        (equal? pair1-result* pair2-result*))
      ;; IMHO just checking the resulting substitution renaming by unify-proc is enough.
      ; (same-dict? pair1-result pair2-result)
      )
    )
  )
(define (new-pairs data)
  (cons 'pairs data))
(define (unify:collector-wrapper-with-substitution-unique-pairs pattern1 pattern2 dict succeed unify-proc)
  ; (pp (list "unify:collector-wrapper-with-substitution-unique-pairs calls with " pattern1 pattern2 dict succeed unify-proc))
  (let ((collector (unify:collector-wrapper pattern1 pattern2 dict succeed))
        (pairs (new-pairs '()))
        )
    (write-line "finish collector in unify:collector-wrapper-with-substitution-unique-pairs")
    (for-each ; modified 
      (lambda (dict) 
        ;; same as the above.
        (and dict
             (let ((subst (match:dict-substitution dict)))
               (let ((p1* (subst pattern1)) (p2* (subst pattern2)))
                 ;; > You can check that the results of the two substitutions are equal
                 (if (not (equal? p1* p2*))
                   (begin
                    (pp p1*)
                    (pp p2*)
                    (pp dict)
                    (error (list "Bad dictionary" p1* p2* dict))))
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
(write-line "4.19 b test1")
(let ((pattern '(* (?? a) (+ (?? b)) (?? c)))
      (expression '(* x y (+ z w) m (+ n o) p)))
  (pp (unify:collector-wrapper-with-substitution-unique-pairs pattern expression (match:new-dict)
                                                                        (lambda (dict)
                                                                          (pp (match:bindings dict))
                                                                          #f)
                                                                        unify
                                                                        )))
;; 2 results with the same substitution
; ((c (m (+ n o) p) ??) (b (z w) ??) (a (x y) ??))
; ((c (p) ??) (b (n o) ??) (a (x y (+ z w) m) ??))
; (pairs ((* x y (+ z w) m (+ n o) p) dict (c (p) ??) (b (n o) ??) (a (x y (+ z w) m) ??)))

;; The above can't be checked for sameness by dict
;; since they have non-compatible values for the same var-seq.
(define (main-test unify-proc)
  (let ((p1 '(a (?? x) (?? y) (?? x) c))
        (p2 '(a b b b (?? w) b b b c)))
    (unify:collector-wrapper-with-substitution-unique-pairs p1 p2 (match:new-dict)
                                                                      (lambda (dict)
                                                                        (pp (match:bindings dict))
                                                                        #f)
                                                                      unify-proc
                                                                      ))
  )
(write-line "4.19 b test2")
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
  (elem-cnt pattern match:var?))
(define (elem-cnt tree predicate?)
  (tree-fold* 
    (lambda (elm res) (declare (ignore elm)) (+ 0 res)) 
    (lambda (elm res) (declare (ignore elm)) (+ 1 res))
    0
    tree
    predicate?)
  )
(var-cnt '(a b b b (?? w) ((?? w) a (b (?? w) c)) b b c))
;; SKIPPED due to the book doesn't tell about the exact definition for "general": TODO Here I just use var-cnt to compare, this is not fine-grained for some cases.
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
;; Compared with (main-test unify), won't remove the more general (a b b b (?? y) b b b c) part.
; (pairs 
;   ((a b b b (?? y) b b b c) dict (w ((?? y)) ??) (x (b b b) ??)) 
;   ((a b b b b b b c) dict (w () ??) (y () ??) (x (b b b) ??)))

;; similar to unify:collector-wrapper-with-substitution-unique-pairs
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
(define (most-general-pair pairs)
  (car (tagged-list-data pairs))
  )
(pp 
  (sort-and-remove-substitution-instance-for-pairs
    (main-test substitution-instance?)
    substitution-instance?
    ))
; (pairs ((a b b b (?? y) b b b c) dict (w ((?? y)) ??) (x (b b b) ??)))

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

;;;; related resources
;;; skipped
;;; google: unification "segment" variable general match
;; 0. https://digitalcommons.bard.edu/cgi/viewcontent.cgi?article=1092&context=senproj_s2021
;; related with knot theory
;; 1. https://pdf.sciencedirectassets.com/272313/1-s2.0-S0747717100X00843/1-s2.0-S074771718371059X/main.pdf?X-Amz-Security-Token=IQoJb3JpZ2luX2VjEFAaCXVzLWVhc3QtMSJHMEUCIQCB2XbxX%2FNCB6ieJzwW4rP4LwO7r7n3XTEoLlttde25pgIgZJ1RkQmL23%2BvrCMbktWziINHGBalN52XB1K5Gik0dEYqswUIKRAFGgwwNTkwMDM1NDY4NjUiDEUZ6l6AiJhDs1WN3iqQBQGZmxs7taiMoyHgS4QVSpac144xXoIIAUAo3uWb7nH%2FcTHFpHoDzb3R7Gf%2FlOqtWqcGx1O%2FrTySrC%2BlX4E%2FoPDR9FXag6QQcox6%2Fl0fjx6KXqF5ND6bvwrZKAtILSqc%2B2jYlRsKkn4PqVqr4guPZOU3HHMRfqJElQolZ8ZXYcXIyi9ohuxizi7x07ZHaWirygUYstQ6DNcfhm4fJxrYFt1jTNqY6Q4MCGfijobdrFu5P01jlOi3Qg1mB5sP%2BnOQk900BH%2B2OoFCaHxwwTbFbJQ90fzndMZ%2BDwFX4d9FQ8mJDFKUhAAiKQr%2F1EjIOyFkB6BfdLvyO2fvIv0daSRGhkgTPvUBSZ7liFHwLeS0qRmX017BKT%2BZUMEBK5RW90aDMy7yCFybbU8bcpHKvoU96cwiPEaSfhaYW5otlUXFvjb0uf6DrwrBwQNIgvazii2MeCJyVwBLvx5xV8Vzu00%2FV8%2FyLvJWODCW1XPrCiINflsRho4ii6lF%2F2jbhTpJMk4vrJOawy0TB5WgdpaX2TePAmbK%2FpffoyBmnIrUlMGRDnIxTDd6Q%2FcTQhQDGCL1w2C1bWwJt5lecheSkA65NY%2B5ljIG8zxwEcu%2B0zA2xbN6iPnl9ddZWj4AunRwxpvXhx9s5ZTBwCjMj9Qiznc5nBzGgcScofQPWfZ0%2B6x8hQliRz3rebNTdqOkGvirhejnS%2BaGUqyB%2BEr%2B5LTDrgHYjMOeGAVz0vV5pUE74GK2mSyyqcYDxOHuIDhX1c5hF6UZrlE86Jbcg2s%2FWFCcZKVlUsRloXZqwtD0rJ%2BPEl6esGHh8zVUm%2FkMF%2FbfKCIe%2FHhcnKl%2BS699HXU5Al1PILwF8N8SBg7nEgJ5mkiUcgxPMaDx%2FSXwMLqLtLsGOrEBkWPZc7O6S%2BFSGYV0RyFAEqtdauzaOOPQowb6skXd%2B0eBZBGfW5pRK%2FSK7zSow61eDVyeGqUz%2BwRF4dXpopzPRpNBGCllSc27YBl0xVsknodXimgwwcTG2iylBX0rkKPteHHWzvPabdiecF4qXkZiaeS84T5UfENJGvIvhgRvsIOMn0DyZ9Y4lyquB6LPdvbwV7ghfi%2BzJf17PArz8NmbH4XQ3bvj%2F2kwgiXGSguwPz4M&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20241226T082749Z&X-Amz-SignedHeaders=host&X-Amz-Expires=300&X-Amz-Credential=ASIAQ3PHCVTYXFG7FHKI%2F20241226%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=75ca1e9a0584f3fc046bf5eec5d37a74e02b9960b7f3c660e4be26c1bdeba4e4&hash=e443c70442bc5c3d0b5099af1cd73d94c1af73d95c60c1045bfb7c95653c540d&host=68042c943591013ac2b2430a89b270f6af2c76d8dfd086a07176afe7c76c2c61&pii=S074771718371059X&tid=spdf-0760b691-ad7a-4d60-8471-d7d7e2a3f335&sid=b659fb6b528244423b980b8819aeb03bd5eegxrqa&type=client&tsoh=d3d3LnNjaWVuY2VkaXJlY3QuY29t&ua=0f1558045404065056035c&rr=8f7fb19e3b69f7e5&cc=us
;; only cares about initial segment.
;; 2. https://www3.risc.jku.at/publications/download/risc_5001/proceedings-UNIF2014.pdf
;; border segment
;; 3. https://www.researchgate.net/figure/Relationships-between-segments_fig2_47805814
;; > A segment hp, qi is a pair of positions such that p <= q.
;; > identify positions in terms by their relative address from the root
;; is one different definition.
;; same as https://www.iiia.csic.es/~levy/papers/semCADE.pdf
;; 4. https://legacy.cs.indiana.edu/ftp/techreports/TR566.pdf
;; is about graph "path segment"
;; 5. https://pdf.sciencedirectassets.com/272313/1-s2.0-S0747717189X80117/1-s2.0-S0747717189800124/main.pdf?X-Amz-Security-Token=IQoJb3JpZ2luX2VjEFAaCXVzLWVhc3QtMSJIMEYCIQDjznUV6JCtTJKAC9PP057oMG7SO6VDHRmhUYMNs3ElhAIhAJg4hOX%2BeVmY9SW9qXu9yLXIrRUnE8mAEUb%2B3qMBORO6KrMFCCkQBRoMMDU5MDAzNTQ2ODY1Igw49qqskiazyWYpuEEqkAVwQJxAaEzkF%2FivS4Od4EHjXlfQOp%2F9vpNHrVqH0liEC%2FvL55j8KCsP4vZ8njQyMFn9CMJUtwSVXaQ0HxuXbv475YVj3jou3DTNd5yvvqCwgp4rEueIWkqgP9xu7bcsiOXcT9OTRnpvgnX7RzXvI5aBqX%2FdH%2BSjDb04if5FVQoECMKpk3t88wLQUDjBmwoNzfmySFCgJdk8cl4Z9qTvIE86mBFsGLXm4k3rDbDQSVSYxCEPXz61cD2kPbAHcMV%2BpzJjf1DQvHthQlMIqH4u%2Bvm0Ab98LNWhkH2pTWKDlcHMyUig38TZr%2F5DcBKQVJLZ%2B%2FQmq%2BAXJJgpvDznOBPQqyip7oPs%2BQNpaUk4KPwoANEeU0Ij%2BFWkymHdBLuI8meozTDg5DGfSefOsxm7L37Tx44nbyyGPbBxcCaEpBtQI4gexmGlGQs1z5%2FLK0g%2BxnEtcgmOUhCHBx9c70R4fi66EfVhLtxmVXMRhy8tKkD5XTwB59dteNFs3oTAGp6GuxFrXOeDUZQT3ERfmcH4kmQ4MfxaOgzq19MqGsssHTQcInHYZDOocKV63J7I9wt%2Bbn%2FR4y83AybHeDj1VdQUM%2FCnVg9OeOWS4t1xbe4pu%2FawQAoN%2BZ7OSSeWUoxtPNCtf2zL3bm7rH3P9FV6VQOOApxxkzbqU2utED%2BkxwOBeiykSSge%2F8TlNV%2FPxzT%2BLesPL0BIF9xdMrTkaHGoXeU6ODgIOg9umQKCeRlMh%2BZDiLP495rdze1hOtc%2BpoKhrIj0GlZ1MYba1XsHV3zyB0VaTUXV4n53FmMjpmbM5Jxx0TlRZ1vzdWpjxda32eBRQHutA8aUDj7MufykESnjz3xlHORuLszhufiFzuKn4SF8g%2BHGWyfSjTDBlLS7BjqwAYuQl9UuBQFNWdiozgfo6cmzLUYiBVB02bZSoZlUFnO05sHylcSMU0lpSySGQe26Bi3K1EVDP%2Fdybbj8mAR5aqv%2FZuF0qJu6S6A7%2FhQK58hWxpjoJSG4NJR9Nsqg%2F200ELpbfjAQkR4NyUL%2BwIMaEPSrNARPYIvodJXGtCoq5yRa%2Fv2szzv5E4Uyf0rnxEoRWMXpTGl0B2l0XtBb69sg3aFVSyC4dtzJrc1ZeSFKLOM7&X-Amz-Algorithm=AWS4-HMAC-SHA256&X-Amz-Date=20241226T082734Z&X-Amz-SignedHeaders=host&X-Amz-Expires=300&X-Amz-Credential=ASIAQ3PHCVTYYNY4YB2B%2F20241226%2Fus-east-1%2Fs3%2Faws4_request&X-Amz-Signature=e2c902495a49e432a8570bc0b83363443c9e5ad87f94595bc7f2c7025064a16f&hash=fe75f058bab42397ba24791250d92bf0445ee99e11eb653027019521dc6e0805&host=68042c943591013ac2b2430a89b270f6af2c76d8dfd086a07176afe7c76c2c61&pii=S0747717189800124&tid=spdf-92671763-a075-462f-be7f-5e83a2fde4f7&sid=7327c38d289d734b58590a83e1d88f22471dgxrqa&type=client&tsoh=d3d3LnNjaWVuY2VkaXJlY3QuY29t&ua=0f155804540406505b5259&rr=8f7fb1446dd87ca3&cc=us
;; uses one different definition.
;; > A subset U c S is called an (upper) segment or afilter of S, iff for o e S and x e U and x -< t~ we have cre U. 
;; which is related with ordering.
;;; scholar.google
;; includes the above 5.
;; 0. https://arxiv.org/pdf/1907.10333
;; is about Anti-unification
;; > generalizing two (or more) goals into a single, more general, goal
;; > that captures some of the structure that is *common to all initial goals*
;; 1. https://proceedings.mlr.press/v108/khemakhem20a/khemakhem20a.pdf
;; related with ICA "Independent Component Analysis".
;; > divide the sources into M segments of L samples each
;; 2. https://journals.plos.org/ploscompbiol/article/file?id=10.1371/journal.pcbi.1004503&type=printable
;; is about "receptor segment".
;; 3. https://www.weizmann.ac.il/math/irani/sites/math.irani/files/publications/good_image_segment.pdf
;; is about "Image Segment".
;; n. https://www.worldscientific.com/doi/epdf/10.1142/S012905419200019X
;; no free pdf.
;; https://www.tesble.com/10.1142/S012905419200019X
;; no "Segment" context.

;; As the above says, unify:segment-var-var doesn't need changes.
;; IGNORE: maybe-grab-segment is same since we only need to change the grab mechanism.
(define (make-sub-segment-var name)
  ;; Here we can also use the naming convention like ?:ref used in 4.7 to
  ;; > distinguish such references from literal symbols like a and from pattern variables like (? x)
  (match:make-var '??? name)
  ; (match:make-var '?? name)
  )
; (define (match:sub-segment-var? object)
;   (and (match:var? object)
;        (let ((type (match:var-type object)))
;          (eq? '??? type))))

(define match:var-types '(? ?? ???))
; (define match:strict-segment-var? match:segment-var?)
;; Here sub-segment-var can also has nested sub-segment-var inside.
(define (match:segment-var? object)
  (and (match:var? object)
       (let ((type (match:var-type object)))
         (or (eq? '??? type) (eq? '?? type)))))
;; re-register since preds are changed.
(define-generic-procedure-handler unify:gdispatch
                                  (match-args (car-satisfies match:segment-var?)
                                              (car-satisfies match:segment-var?))
                                  unify:segment-var-var)
(define-generic-procedure-handler unify:gdispatch
                                  (match-args (car-satisfies match:segment-var?)
                                              (complement (car-satisfies match:segment-var?)))
                                  (lambda (var-first terms)
                                    (maybe-grab-segment var-first terms)))
(define-generic-procedure-handler unify:gdispatch
                                  (match-args (complement (car-satisfies match:segment-var?))
                                              (car-satisfies match:segment-var?))
                                  (lambda (terms var-first)
                                    (maybe-grab-segment var-first terms)))

;; similar to generate-unique-name
(define generate-unique-internal-name
  (let ((n 0))
    (lambda (prefix)
      (set! n (+ n 1))
      ;; modified
      (symbol prefix "-internal:" n))))
(define (add-left-suffix prefix)
  (symbol prefix "-left"))
(define (add-right-suffix prefix)
  (symbol prefix "-right"))
;; use list since segment uses that.
(define (segment-var->sub-segment-vars-list segment-var)
  (assert (match:segment-var? segment-var))
  (let ((name (match:var-name segment-var)))
    (list 
      (make-sub-segment-var (add-left-suffix (generate-unique-internal-name name)))
      (make-sub-segment-var (add-right-suffix (generate-unique-internal-name name)))
      ))
  )
(define left-internal car)
(define right-internal cadr)

(define create-var-substitution-binding list)
(define get-var-from-binding car)
(define get-substitution-from-binding cadr)

(define (create-initial-terms-binding-list initial terms binding)
  (list initial terms binding))
(define get-initial car)
(define get-terms cadr)
(define get-binding caddr)
(define (add-term-to-initial initial terms)
  (let ((term (car terms)))
    (cond 
      ((null? initial) 
       (create-initial-terms-binding-list (append initial (list term)) (cdr terms) '()))
      ;; 0. for partial intersection, we need to add 2 bindings for each of 2 vars.
      ;; e.g. (?? x)->(4 (?? z)) and (?? y)->((?? z) (?? y-internal:1))
      ;; 0.a. Then terms* needs to use (?? y-internal:1).
      ;; 0.b. better to use ??? etc to ensure no name conflict.
      ;; 1. Here we can assume rhs var has length fixed, then lhs var len can be variant.
      ;; It can have only 2 cases: either intersect or contain rhs var.
      ((match:segment-var? term)
       (let ((sub-segment-vars-list (segment-var->sub-segment-vars-list term)))
         (create-initial-terms-binding-list 
           (append initial (list (left-internal sub-segment-vars-list))) 
           (cons (right-internal sub-segment-vars-list) (cdr terms))
           (list (create-var-substitution-binding term sub-segment-vars-list)))
         )
       )
      ;; manipulated in slp since it is closely related with the possible-term-var-binding-list addition.
      ;; If we check here, we may 
      ; ;; i.e. get 
      ; ((match:sub-segment-var? term)
      ;   )

      ;; grab another constant term etc.
      (else 
        ;; same as the 1st clause.
        (create-initial-terms-binding-list (append initial (list term)) (cdr terms) '()))
      )
    )
  )
;; added
;; 0. since term may also have split binding as the above (?? y)->((?? z) (?? y-internal:1)) shows.
;; The original maybe-substitute and maybe-grab-segment doesn't have this split binding 
;; (i.e. seeing the term as one whole un-splitable object), so no need for checking term binding.
;; 0.a. anyway whether bounded to term or term value is all fine satisfying consistency.
;; 0.b. since only segment var can have split binding
;; and only segment var can have segment var *value* (match:lookup is not bidirectional since it is one dict. Otherwise it will be a bit messy),
;; So we only need to check for parts related with segment var, which are all done in maybe-grab-segment.
;; IGNORE: (temporarily I just did this for segment var part in my implementation. But this can be done similarly for the rest parts)
(define (check-car-term-binding terms dict)
  (if (null? terms)
    terms
    (let ((term (car terms)))
      (if (match:has-binding? term dict)
        (append (match:get-value term dict)
                (cdr terms))
        terms
        )
      ))
  )
(define (maybe-grab-segment var-first terms)
  (define (maybe-grab dict succeed fail)
    (let ((var (car var-first))
          (terms* (check-car-term-binding terms dict))
          )
      (cond 
        ((match:has-binding? var dict)
         ((unify:dispatch
            (append (match:get-value var dict)
                    (cdr var-first))
            terms*)
          dict succeed fail))
        (else 
          ((grab-segment var-first terms*)
           dict succeed fail)))))
  maybe-grab)

(define (grab-segment var-first terms)
  (define (grab dict succeed fail)
    ; (trace succeed)
    (let ((var (car var-first)))
      ;; here possible-term-var-binding-list is just one tmp var, it should be cleared when necessary and with binding moved to dict.
      (let slp ((initial '()) (terms* terms) (possible-term-var-binding-list '()))
        (assert (n:>= 1 (length possible-term-var-binding-list)))
        (define (continue)
          (if (null? terms*)
            (begin
              ; (write-line (list "call grab-segment fail for" var-first terms dict))
              (fail))
            ;; modified
            (if (null? possible-term-var-binding-list)
              (let ((initial-terms-binding-list (add-term-to-initial initial terms*)))
                (slp 
                  (get-initial initial-terms-binding-list)
                  (get-terms initial-terms-binding-list)
                  (get-binding initial-terms-binding-list)
                  )
                )
              (let* ((binding (car possible-term-var-binding-list))
                     (term-var (get-var-from-binding binding)))
                ;; we have just added left-internal, it should occur once.
                (let* ((added-left-internal (last initial))
                       (left-internal-check-pred (lambda (elm) (equal? elm added-left-internal))))
                  (assert (n:= 1 (elem-cnt initial left-internal-check-pred)))
                  (let ((new-initial 
                          ;; same as what the original unify does for segment-var.
                          (append (remove left-internal-check-pred initial) (list term-var))
                          ))
                    ; (write-line (list "call after split for var" var "with initial" new-initial))
                    (slp
                      new-initial
                      (cdr terms*) ; with right-internal removed
                      '()
                      )
                    )
                  )
                )
              )
            ))
        (let ((dict* (do-substitute var initial dict)))
          (let ((dict**
                  (if (null? possible-term-var-binding-list)
                    dict*
                    (and
                      dict*
                      (let ((binding (car possible-term-var-binding-list)))
                        (do-substitute 
                          (get-var-from-binding binding)
                          (get-substitution-from-binding binding)
                          dict*)
                        )))
                  ))
            (if dict**
              ;; Use dict** to inclued possible split binding.
              (succeed dict** continue (cdr var-first) (check-car-term-binding terms* dict**))
              (continue))
            )))))
  grab)

; (trace unify:dispatch)
;; test1
(unify:internal '(((?? x) 3) ((?? x)))
                '((4 (?? y)) (4 5))
                (match:new-dict)
                (lambda (dict)
                  (pp (match:bindings dict))
                  #f))
; ((y-internal:2 (5) ???) (y-internal:1 (3) ???) (y (5 3) ??) (x (4 5) ??))

;; can only output the 1st cand
(unifier '((?? x) 3) '(4 (?? y)))

;; test2 same as SDF_exercises/software/sdf/unification/gjs-test.scm first ((?? x) 3)
(let ((pattern1 '((?? x) 3)))
  (unify:internal pattern1 '(4 (?? y))
                  (match:new-dict)
                  (lambda (dict)
                    (pp (match:bindings dict))
                    (pp
                      ;; from unifier
                      (and dict
                           ((match:dict-substitution dict) pattern1))
                      )
                    #f)
                  )
  )

(load "4_19_d_tests.scm")
