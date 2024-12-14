(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'term-rewriting)

;; IMHO this can be done by generic procedure.
;; Notice the basic ideas of generic procedure are that it dispatches based on types of args.
;; So it won't work for one arbitrarily long arg sequence since it can't decide the seq type...

;; > Or arbitrary sequences? What sorts of changes are required?
;; Use generic correspondingly.
;; > Do you need to change the interface to a match procedure?
;; No.

;; https://stackoverflow.com/a/67912104/21294350
;; Actually here vector is more appropriate.
;; > you cannot grow a vector's size
;; And more efficient at least for length. I won't dig into checking others.
;; > accessing an element given its index is a constant O(1) operation, whereas in a list the same operation is O(n). 
;; > Similarly for the length operation.

;; main
(define (->list data)
  (cond 
    ((vector? data) (vector->list data))
    ((list? data) data)
    (else (error (list "wrong data types" data)))
    ))

(define (match:vector matchers)
  (define (list-match data dictionary succeed)
    ;; IGNORE: for simplicity, all args are transformed into list corresponding ones although this can be also done by generic.
    ;; Here data internal structure is unexpected, so not transform it into list.
    ; (let ((data (->list data))
    ;       (matchers (->list matchers))
    ;       )
    ;   (write-line (list "call match:vector" data matchers))
    ;   ;; unchanged
    ;   )

    (and (sequence? data)
         ;; IGNORE: list-tail similar to cdr is inefficient for 
         (let lp ((data-list (match:general-car data))
                  ;;  0. here cdr is slow for vector https://people.eecs.berkeley.edu/~bh/ssch23/vectors.html
                  ;; > it would have to make a one-smaller vector and copy the elements one at a time
                  ;; and matchers must be *level-one* structure without considering the nested case.
                  ;; So ->list is fine.
                  ;; 1. Although https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Cutting-Vectors.html
                  ;; offers .
                  (matchers (->list matchers))
                  (dictionary dictionary))
           (cond ((sequence? matchers)
                  ((match:general-car matchers)
                   ;; here needs all the rest APIs to be changed...
                   data-list
                   dictionary
                   (lambda (new-dictionary n)
                     ;; SDF_exercises TODO when happens
                     ;; not in book...
                     (if (> n (match:general-length data-list))
                       (error "Matcher ate too much."
                              n))
                     (lp (match:general-tail data-list n)
                         (cdr matchers)
                         new-dictionary))))
                 ((match:general-have-data data-list) #f) ;unmatched data
                 ((match:general-null? data-list)
                  ;; 0. > backtrack into the consequent or pattern-match part of a rule. 
                  ;; "consequent"
                  ;; 1. eat one list data like (+ z w) in (+ y (+ z w)).
                  (succeed dictionary 1))
                 (else #f))))
    )
  list-match)

(load "4_8_generic_procs.scm")

(define (match:compile-pattern pattern)
  (cond ((match:var? pattern)
         (case (match:var-type pattern)
           ((?) (match:element pattern))
           ((??) (match:segment pattern))
           (else (error "Unknown var type:" pattern))))
        ((sequence? pattern)
         (match:general-list (match:unary-map match:compile-pattern pattern)))
        (else
          ;; IGNORE: SDF_exercises TODO what does this purpose to do?
          (match:eqv pattern))))

(run-matcher
  (match:compile-pattern (list->vector '(a ((? b) 2 3) (? b) c)))
  (list->vector '(a (1 2 3) 2 c))
  match:bindings)

(run-matcher
  (match:compile-pattern '(a ((? b) 2 3) (? b) c))
  '(a (1 2 3) 1 c)
  match:bindings)

(run-matcher
  ;; here only one level of vector (see (vector (vector 1))).
  (match:compile-pattern (list->vector '(a ((? b) 2 3) (? b) c)))
  (list->vector '(a (1 2 3) 1 c))
  match:bindings)
