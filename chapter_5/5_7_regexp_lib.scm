(cd "~/SICP_SDF/SDF_exercises/chapter_5")
(load "5_7_basic_pred_lib.scm")

(define (make-or sre-lst)
  (assert (and (list? sre-lst) (every valid-sre? sre-lst)))
  (cons 'or sre-lst)
  )

;;; (see git history) refactored for modularity https://stackoverflow.com/questions/1025844/what-is-refactoring-and-what-is-only-modifying-code/1025867#1025867
;; > altering its internal structure without changing its external behavior

;; 0. See SDF_exercises/chapter_5/5_7_precedence_lib.scm
;; Here "," is not one separtor among statements in Python which uses line (see https://docs.python.org/3/reference/lexical_analysis.html#line-structure and https://stackoverflow.com/a/35053210/21294350)
(define primitive-op-lst '("*" ":" "-" "+" "/" "="))

;; can't be splitted into ("*" "*") etc.
; (define unsplittable-primitive-op-lst '("**" ":=" "--" "++" "=="))
;; no ++ in Python https://stackoverflow.com/a/1485854/21294350. See SDF_exercises/chapter_5/5_7_precedence_lib.scm
(define unsplittable-primitive-op-lst '("**" ":=" "=="))
;; no further partitions done for them.
(define primitive-symbol-re-lst `(,(make-or unsplittable-primitive-op-lst)))
(define partition-separtor-lst 
  `((or ,left-parenthesis ,right-parenthesis) 
    ,(car primitive-symbol-re-lst)
    ,(make-or primitive-op-lst)))
(define split-lst '((+ space)))

;; just as one demo
(define keyword-lst 
  (append 
    primitive-op-lst
    unsplittable-primitive-op-lst
    (list left-parenthesis right-parenthesis)
    ))
(define (keyword? str)
  (any (lambda (cmp) (equal? str cmp)) keyword-lst)
  )

(define (exp-split exp split-lst)
  (let lp ((res-lst (list exp)) (rest-split-lst split-lst))
    (if (null? rest-split-lst)
      res-lst
      (let ((res-lst*
              (append-map 
                (lambda (res)
                  (assert (string? res))
                  (regexp-split (car rest-split-lst) res)) 
                res-lst)
              ))
        (lp res-lst* (cdr rest-split-lst))))
    )
  )

(define (exp-partition exp separtor-lst skipped-re-lst)
  (let lp ((res-lst (list exp)) (rest-separtor-lst separtor-lst))
    (if (null? rest-separtor-lst)
      res-lst
      (let ((res-lst-cluster
              (map 
                (lambda (res)
                  (assert (string? res))
                  (if (any
                        ;; see https://srfi.schemers.org/srfi-115/srfi-115.html#proc-_2b
                        ;; > matches the entire string
                        (lambda (re) (regexp-matches? re res))    
                        skipped-re-lst)
                    (list res)
                    (regexp-partition (car rest-separtor-lst) res))) 
                res-lst)
              ))
        (lp (apply append res-lst-cluster) (cdr rest-separtor-lst))))
    )
  )

(regexp-partition '(or "*" "**") "b**2-4*a*c")

;; misc string lib
(define (empty-str? str)
  (assert (string? str))
  (n:= 0 (string-length str))
  )

(define test-exp3
  "fact := lambda n:
  if n == 0
  then 1
  else n*fact(n-1)"
  )

(exp-split test-exp3 split-lst)

