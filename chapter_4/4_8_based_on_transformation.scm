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
    ;; consider nested case
    ((vector? data) (map ->list (vector->list data)))
    ((list? data) data)
    (else 
      ; (error (list "wrong data types" data))
      data
      )
    ))

(define (match:vector matchers)
  (define (list-match data dictionary succeed)
    (and (pair? data)
         ;; only here we change to use ->list.
         ;; So all data will have no vector.
         (let lp ((data-list (->list (car data)))
                  (matchers (->list matchers))
                  (dictionary dictionary))
           ;  (write-line (list data-list))
           (cond ((pair? matchers)
                  ((car matchers)
                   ;; IGNORE: SDF_exercises TODO why always list here?
                   data-list
                   dictionary
                   (lambda (new-dictionary n)
                     ;; SDF_exercises TODO when happens
                     ;; not in book...
                     (if (> n (length data-list))
                       (error "Matcher ate too much."
                              n))
                     ;  (write-line (list "new-dictionary" new-dictionary))
                     (lp (list-tail data-list n)
                         (cdr matchers)
                         new-dictionary))))
                 ((pair? data-list) #f) ;unmatched data
                 ((null? data-list)
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

;; test1
(run-matcher
  (match:compile-pattern (list->vector '(a ((? b) 2 3) (? b) c)))
  (list->vector '(a (1 2 3) 2 c))
  match:bindings)

;; All the rest gives "((b 1 ?))" result.
;; test2
(run-matcher
  (match:compile-pattern '(a ((? b) 2 3) (? b) c))
  '(a (1 2 3) 1 c)
  match:bindings)

(run-matcher
  ;; here only one level of vector (see (vector (vector 1))).
  (match:compile-pattern (list->vector '(a ((? b) 2 3) (? b) c)))
  (list->vector '(a (1 2 3) 1 c))
  match:bindings)

;; test3
;; 0. Here # doesn't need to also correspond to #, also for list.
;; 1. vector/list nested in vector
(run-matcher
  (match:compile-pattern (list->vector '(a #((? b) 2 3) ((? b) 4) c)))
  (list->vector '(a (1 2 3) #(1 4) c))
  match:bindings)

;; test4
;; 1. vector/list nested in list like 
;; vector nested in list ((? b) #((? b) 5)).
(run-matcher
  (match:compile-pattern (list->vector '(a #((? b) 2 3) ((? b) #((? b) 5)) c)))
  (list->vector '(a (1 2 3) #(1 (1 5)) c))
  match:bindings)
