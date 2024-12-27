;; > What should happen if a restricted variable is matched against another restricted variable?
;; i.e. ensure restrictions are satisfied.
;; IGNORE:
;; So IMHO it is fine to check that after getting term val and then checking the var restriction as code does by (match:dict-substitution dict) before (match:satisfies-restriction? var term*).
;; Here term restriction has been checked in the former binding process.
;; 0. one has binding (including both have bindings) which is bound probably before by left terms.
;; Then `(match:satisfies-restriction? var term*)` checks as one-sided case.
;; 0.a. The tricky thing is that unify:gdispatch has 2 pred pairs to match with this.
;; Due to ordering it will first match `(... (car-satisfies element?) (car-satisfies match:element-var?))`
;; 0.a.0. the 2nd has binding 
;; IGNORE: (assuming not var val, otherwise goes to case 1 if recursively that value is reduced to var still)
;; This binding can't be var, i.e. restriction can't be match:element-var?, otherwise that has no use at all.
;; So assume this binding value is constant.
;; Then we go to the normal matching between "a restricted variable" and a constant.
;; 0.a.1. the 2nd has no binding
;; either same (not checking assertion equality. So assume same vars have the same assertion)
;; or use the value of the 1st var, then again "the normal matching between "a restricted variable" and a constant".
;; But here we directly checks "(match:satisfies-restriction? var term*)" instead of calling unify:dispatch.
;; Actually in the former case, unify:dispatch for ""a restricted variable" and a constant" will call "(do-substitute var term dict)".
;; So they did the same thing at all.
;; 0.a.n. Here ((? y) (? x)) matched with ((? x) 1) can work to be reduced to (1 1).

;; 0.b. It is possible that 2 vars are matched before one or both get their value bindings.
;; If so, IMHO we should just check whether these restrictions are matched bidirectionally.
;; 0.b.0. Notice if (number?) is matched with (number? or string?) then the latter can't be string.
;; So we need to *wait* for the binding for (number? or string?) to decide whether they match.
;; If no binding for that, then at least for now no conflict occurs.
;; 0.b.0.a. Again, wait implies we need to change the unify internal structure...
(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'unification)
(unifier '((? y) (? x)) '((? x) 1))
;; 1. Both have no bindings, then pred is done as before but applied on var.
;; 1.a. Here it can't get values bound by right due to the left to right mechanism.
;; 1.b. it will fail to match (? x ,pred?) with itself if (pred? (? x ,pred?)) fails.
;; This is expected just due to pred? acting as one filter.
;; 1.c. Based on the above predicate assumption, this won't match at all.

;; > it is uniformly eliminated
;; i.e. dict will think var as its val, so no var at all.
;; > But the restriction is then lost, preventing it from killing an unsuitable later part of the match
;; Since it *has got val*, there is no need to check pred later at all.

