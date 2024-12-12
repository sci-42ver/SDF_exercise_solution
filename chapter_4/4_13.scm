(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'unification)
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
  ; (trace eqv-match)
  eqv-match)
; (trace match:eqv)

(define (match:element variable)
  (define (element-match data dictionary succeed fail)
    (and (pair? data)
        ;; done in do-substitute
        ;  (match:satisfies-restriction? variable (car data))
        ;  (let ((binding (match:lookup variable dictionary)))
        ;    (if binding
        ;        ;; Use equal? since (eqv? (list 1) (list 1)) etc will fail.
        ;        (and (equal? (match:binding-value binding)
        ;                     (car data))
        ;             (succeed dictionary 1))
        ;        (succeed (match:extend-dict variable
        ;                                    (car data)
        ;                                    dictionary)
        ;                 1)))
        ;; As 4_12.scm shows, when both pattern and (car data) are var's.
        ;; It doesn't matter
        ((maybe-substitute-var-pattern variable data) dictionary succeed fail)
                        ))
  element-match)

;; segment ignored

(define (match:list matchers)
  ;; TODO add check.
  
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
  ; (trace list-match)
  list-match)

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
         (match:list (map match:compile-pattern pattern)))
        (else
          ;; IGNORE: SDF_exercises TODO what does this purpose to do?
         (match:eqv pattern))))

(run-matcher
  (match:compile-pattern '((? a) (? b)))
  '((? b) 1)
  match:bindings)
