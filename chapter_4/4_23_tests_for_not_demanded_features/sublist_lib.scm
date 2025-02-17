;; https://stackoverflow.com/a/63947533/21294350
(define (sublist? lst1 lst2)
  (cond
    ;; recursion to find the possible starting location of lst1 inside lst2.
    ;; "starting" implies prefix?.
    ((prefix? lst1 lst2) #t)
    ((> (length lst1) (length lst2)) #f)
    (else (sublist? lst1 (cdr lst2))))
  )
(define (prefix? lst1 lst2)
  (cond
    ;; Here assume '() is always one prefix of any sublist.
    ((null? lst1) #t)
    ;; lst1 is non-null, but lst2 becomes null, so length of lst2 is less.
    ((null? lst2) #f)
    ;; base test
    ((equal? (car lst1) (car lst2)) (prefix? (cdr lst1) (cdr lst2)))
    ;; some elems in lst1 doesn't satisfy the base test
    (else #f)))

; (rest '(a b c a b d e))
(trace sublist?)
(sublist? '(a b d) '(a b c a b d e))
(untrace sublist?)

(trace prefix?)
(prefix? '(a b) '(w a d f s))
(untrace prefix?)
; [Entering #[compound-procedure 13 prefix?]
;     Args: (a b)
;           (w a d f s)]
; [#f
;       <== #[compound-procedure 13 prefix?]
;     Args: (a b)
;           (w a d f s)]

(define sub?
  (lambda (l sub)
    (let ((sub-len (length sub)))
      (define test (lambda (x) (equal? x sub)))
      (define base-s
        (lambda (s l k)
          (and (pair? l)
            ;; no duplicate base '(). So avoid "(test '()) will be called multiple times".
            (or (k (list (car l)))
              (s s (cdr l)
                  (lambda (r)
                    (or (test r)
                        ;; Avoid "O(n) start pos addition".
                        ;; Here assume append has the same speed as cons although not.
                        (and (< (length r) sub-len) (k (append (list (car l)) r)))))))
            ))
        )
      ; (trace base-s)
      ; (trace test)
      (or (test '())
        ((lambda (s) (s s l test)) base-s))
      )
    ))

;;; trace
;; k1=test
;; k2=(or ...)
;; k3=...

;; For each k-n, (k-n '())->(k-[n-1] (cons (car l) '()))->(k-[n-2] (cons (car l') (cons (car l) '())))
(assert (sub? '(a b c a b d e) '(b d e) ))
(assert (sub? '(a b c a b d e) '(c a b) ))
(assert (sub? '(a b c a b d e) '(a b c) ))
(assert (not (sub? '(a b c a b x e) '(a b d) )))
;; (k1 '()) -> (test '()) -> #f
;; (k2 '()) -> (test '()), (k1 '(a)) -> #f
;; (k3 '()) -> (test '()), (k2 '(b)) -> (test '(b)), (k1 '(a b)) 
(assert (not (sub? '(a b c a b d e) '(b a))))
