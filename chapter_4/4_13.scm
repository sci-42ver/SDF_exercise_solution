;; What to achieve?
;; > avoids the syntactic analysis of the patterns
;; i.e. just run matchers like analyzer in SICP to run procs.
(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'unification)
(load "../software/sdf/unification/unify-testing.scm")
(load "../software/sdf/unification/text-examples.scm")
(load "4_13_unify_lib_not_using_generic.scm")

;;; IGNORE the following (I made a mistake that these are all based on both pattern and data...):
;; > Can the
;; > unification matcher be broken up in a similar way? If not, why not?
;; > Is it a good idea to do so? If not, why not? If so, do it! (This is hard!)
;; IMHO can and 
;; IGNORE: good just like analyzer in SICP.
;; Here we have no recursive calls, so may be not same as analyzer optimization.
;; IMHO this "broken up" can avoid the dispatch overheads (checking matcher list is lighter without additional calls for generic helpers).
;; and it is also more appropriate for interaction as the book says
;; > But a system of
;; > match procedures is potentially more efficient, because it avoids the
;; > syntactic analysis of the patterns *while matching*.

;; Here all matchers can be determined except for (car-satisfies match:element-var?) case.
;; See SDF_exercises/chapter_4/4_12.scm
;; The original one will actually only call one handler and is fine as 4_12.scm shows.
;;;

;;; Here constant-term? can match constant-term?/match:element-var?
;; list-term? can match list-term?/match:element-var?
;; match:element-var? can match all the above.
;; (Here not consider match:segment-var? although this can be done probably similarly)
;; So IMHO we can use SDF_exercises/software/sdf/design-of-the-matcher/matcher.scm interfaces
;; but uses SDF_exercises/software/sdf/unification/unify.scm to do actual manipulation.

;;; just modification of matcher.scm
;; (search "pattern/variable" correspondingly) Here all possible "syntactic analysis" are done in SDF_exercises/chapter_4/4_13_unify_lib_not_using_generic.scm
(define (match:eqv pattern-constant)
  ;; 0. Here we need to change succeed interface and add fail for matcher.
  ;; 1. Here I won't change unify:constant-terms to avoid passing pattern around.
  ;; This is one routine work with much duplicate same sub-work...
  (define (check data pattern dict succeed fail)
    (cond 
      (((car-satisfies constant-term?) data) 
       ((unify:constant-terms pattern data) dict succeed fail))
      (((car-satisfies match:element-var?) data) 
       ((maybe-substitute-var-data pattern data) dict succeed fail))))
  (define (eqv-match data dictionary succeed fail) ; modified
    (and (pair? data)
         (check data pattern-constant dictionary succeed fail)
         ))
  eqv-match)

(define (match:element variable)
  (define (element-match data dictionary succeed fail)
    (and (pair? data)
         ;; 0. the original codes here are done in do-substitute
         ;; 1. As 4_12.scm shows, when both pattern and (car data) are var's.
         ;; It doesn't matter which is considered as term.
         ;; This is due to:
         ;; Here we only to ensure when var has val, it can be got actually.
         ;; let the pair be ((? x) (? y)) and (? x) is bound to (? y)
         ;; 1.a. (? x) is to be bound, then maybe-substitute will then do that for (? y).
         ;; So both are bound.
         ;; 1.b. (? y) is to be bound. Then match:map-dict-values will bind both again.
         ;; 1.c. The above also works for the cases when one of them has already been bound.
         ((maybe-substitute-var-pattern variable data) dictionary succeed fail)
         ))
  element-match)

;; segment ignored

(define (match:list matchers pattern)
  (define (check data dict succeed fail)
    (cond 
      (((car-satisfies list-term?) data) 
       (list-match data dict succeed fail))
      (((car-satisfies match:element-var?) data) 
       ((maybe-substitute-var-data pattern data) dict succeed fail))))
  ;; add action to pass around fail.
  (define (list-match data dictionary succeed fail)
    (and (pair? data)
         (let lp ((data-list (car data))
                  (matchers matchers)
                  (dictionary dictionary)
                  (fail fail))
           (cond ((pair? matchers)
                  ((car matchers)
                   ;; IGNORE: SDF_exercises TODO why always list here?
                   data-list
                   dictionary
                   (lambda (new-dictionary n fail)
                     ;; SDF_exercises TODO when happens
                     ;; not in book...
                     (if (> n (length data-list))
                       (error "Matcher ate too much."
                              n))
                     ;  (write-line (list "new-dictionary" new-dictionary))
                     (lp (list-tail data-list n)
                         (cdr matchers)
                         new-dictionary
                         fail
                         ))
                   ;; added
                   fail
                   ))
                 ;; modified
                 ((pair? data-list) (fail)) ;unmatched data
                 ((null? data-list)
                  ;; 0. > backtrack into the consequent or pattern-match part of a rule. 
                  ;; "consequent"
                  ;; 1. eat one list data like (+ z w) in (+ y (+ z w)).
                  (succeed dictionary 1 fail))
                 (else (fail))))))
  check)

;;;; Pattern syntax

;; matcher is just for one wrapper of run-matcher.

(define (run-matcher match-procedure datum succeed)
  (match-procedure (list datum)
                   (match:new-dict)
                   ;; just finish, so fail is not used.
                   (lambda (dict n fail)
                     ;; (= n 1) means manipulating with one datum in (list datum)
                     (and (= n 1)
                          (succeed dict)))
                   ;; added
                   (lambda () #f)
                   ))

;; print-all-matches removed since (?? ...) is not considered.

;; trace (match:compile-pattern '(+ (? a) (+ (? b) (? c)))) with (+ y (+ z w)) in '(* (+ y (+ z w)) x).
;; see SDF_exercises/chapter_4/term-rewriting/trace-demo.scm
(define (match:compile-pattern pattern)
  (cond ((match:var? pattern)
         (case (match:var-type pattern)
           ((?) (match:element pattern))
           ;  ((??) (match:segment pattern))
           (else (error "Unknown var type:" pattern))))
        ((list? pattern)
         ;; modified
         (match:list (map match:compile-pattern pattern) pattern))
        (else
          ;; IGNORE: SDF_exercises TODO what does this purpose to do?
          (match:eqv pattern))))

(run-matcher
  (match:compile-pattern '((? a) (? b)))
  '((? b) 1)
  match:bindings)
;Value: ((b 1 ?) (a 1 ?))

;; a brief test considering all pairs as the above "constant-term? can match constant-term?/match:element-var? ..." shows.
(run-matcher
  (match:compile-pattern '(1 3 (4) (5) (? a) (? b) (? y)))
  '(1 (? x) (4) (? z) (? b) 1 (2))
  match:bindings)
;Value: ((y (2) ?) (b 1 ?) (a 1 ?) (z (5) ?) (x 3 ?))

(define (unifier* pattern1)
  (lambda (data)
    (let ((dict 
            (run-matcher
              (match:compile-pattern pattern1) 
              data (lambda (x) x))))
      (and dict
           ((match:dict-substitution dict) pattern1)))))

(run-matcher
  (match:compile-pattern a) 
  b match:bindings)
((unifier* a) b)
; ((ben franklin) ((? bmo) 6 1705) (apr 17 1790))
((unifier* c) ((unifier* a) b))
; ((ben franklin) (jan 6 1705) (apr 17 1790))
