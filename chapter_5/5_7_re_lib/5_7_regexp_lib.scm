;;; Similar to tokenize in https://eli.thegreenplace.net/2010/01/02/top-down-operator-precedence-parsing
;; but not uses generator https://stackoverflow.com/a/1073582/21294350
(cd "~/SICP_SDF/SDF_exercises/chapter_5")
(load "5_7_common_lib/5_7_basic_pred_lib.scm")

(load "5_7_common_lib/re/re_syntax_lib.scm")

;;; (see git history) refactored for modularity https://stackoverflow.com/questions/1025844/what-is-refactoring-and-what-is-only-modifying-code/1025867#1025867
;; > altering its internal structure without changing its external behavior

;; 0. See SDF_exercises/chapter_5/5_7_precedence_lib.scm
;; Here "," is not one separtor among statements in Python which uses line 
;; (see https://docs.python.org/3/reference/lexical_analysis.html#line-structure 
;; and https://stackoverflow.com/a/35053210/21294350)
;; But it is used to delimit list elements etc.
(define primitive-op-lst '("*" ":" "-" "+" "/" "=" ","))

;; no use to make neg-look-behind/look-ahead etc work.
; (cd "~/SICP_SDF/SDF_exercises/chapter_5/re_lib")
; (load "regexp.sld")

(load "5_7_common_lib/re/general_re_lib.scm")
(define unsplitted-var 
  `(: 
    ;; IGNORE 0. only consider unary
    ;; 0.a. word is not with "fixed length".
    ; (? (or (: (neg-look-behind alphanumeric) "*") (: (neg-look-behind alphanumeric) "**")))
    ;; 1. Here we only need to ensure *args etc won't be splitted further.
    ;; Normal var "arg" etc trivially won't be splitted.
    ; (? (or "*" "**"))
    ;; 2. IGNORE Default to use greedy, so ** will always match ** instead of * followed by *.
    ;; greedy only has influences when we can have more than one possible match results. See https://stackoverflow.com/a/11899069/21294350
    (or "*" "**")
    ;; 0. Not use - here since here we doesn't use Scheme syntax which must use space to delimit var
    ;; See SDF_exercises/chapter_5/5_7.scm where 
    ; (+ (or alphanumeric "_" "-"))
    ;; 1. IGNORE Here we should not use (word (: "*" (+ (or alphanumeric "_")))) since * must be followed by bow 
    ; (+ (or alphanumeric "_"))
    ;; 2. See SDF_exercises/chapter_5/5_7_re_tests/miscs.scm
    ;; The point 1 is also related with bow/eow probably.
    ,word-corrected
    ))
; (assert (and (valid-sre? '(neg-look-behind word)) (valid-sre? var)))

;; 0. can't be splitted into ("*" "*") etc.
;; 1. no ++ in Python https://stackoverflow.com/a/1485854/21294350. See SDF_exercises/chapter_5/5_7_precedence_lib.scm
; (define unsplittable-primitive-op-lst '("**" ":=" "--" "++" "=="))
(define unsplittable-primitive-op-lst `("**" ":=" "=="))
(define skipped-primitive-re-lst `(,@unsplittable-primitive-op-lst ,unsplitted-var))

;; 0. only the alone *args etc are not splittable, but a*b etc can.
;; 1. Expected behavior: (?<!\w)(\*|\*\*)?\w+
;; In Python (see https://stackoverflow.com/a/18425460/21294350 for why using ?: for findall, https://stackoverflow.com/a/2136580/21294350 for partition behavior):
; import re
; pat=r'(?<!\w)(?:\*|\*\*)\w+'
; [print(re.findall(pat, str),re.split('('+pat+')', str)) for str in ["k*c+a*b","b**2-4*a*c","n*fact(n-1)","a,b=0,/,c,*args,*,kwarg1,**kwargs"]]
;; results (the 2rd implies)
; [] ['k*c+a*b']
; ['*2'] ['b*', '*2', '-4*a*c']
; [] ['n*fact(n-1)']
; ['*args', '**kwargs'] ['a,b=0,/,c,', '*args', ',*,kwarg1,', '**kwargs', '']
(define (get-args-kwargs exp)
  (arbitrary-len-neg-look-behind word-corrected unsplitted-var exp))
(define n:+ +)
(define n:- -)
(define n:= =)
(get-args-kwargs "k*c+a*b")
(get-args-kwargs "b**2-4*a*c")
(get-args-kwargs "n*fact(n-1)")
(get-args-kwargs "a,b=0,/,c,*args,*,kwarg1,**kwargs")

;; 0. no further partitions done for them.
;; 1. From larger pat matching **kwargs etc to smaller **.
(define primitive-symbol-re-pat-or-proc-lst `(,get-args-kwargs ,(make-or unsplittable-primitive-op-lst)))
(define partition-separtor-lst 
  ;; IGNORE "," is to prepare for split of "*args," etc.
  `((or ,left-parenthesis ,right-parenthesis) 
    ,@primitive-symbol-re-pat-or-proc-lst
    ,(make-or primitive-op-lst)))
;; 0. > In an ASCII context this corresponds to space, tab, line feed, form feed, and carriage return. 
;; So both " " and "\n" will be matched.
;; 1. Default to use Unicode
;; > This is the default.
;; 1.a. see https://www.compart.com/en/unicode/category/Zs#UNC_CAT
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

(define (apply-separtor pat exp)
  (assert (string? exp))
  (cond 
    ((valid-sre? pat) (regexp-partition pat exp))
    ((procedure? pat) (pat exp))
    (else (error (list "unrecognized pat" pat)))))

(define (exp-partition exp separtor-lst skipped-re-lst)
  (let lp ((res-lst (list exp)) (rest-separtor-lst separtor-lst))
    (if (null? rest-separtor-lst)
      res-lst
      (let ((res-lst-cluster
              (append-map 
                (lambda (res)
                  (assert (string? res))
                  (if (any
                        ;; see https://srfi.schemers.org/srfi-115/srfi-115.html#proc-_2b
                        ;; > matches the entire string
                        (lambda (re) (regexp-matches? re res))    
                        skipped-re-lst)
                    (list res)
                    (apply-separtor (car rest-separtor-lst) res)
                    )) 
                res-lst)
              ))
        (lp res-lst-cluster (cdr rest-separtor-lst))))
    )
  )

; (regexp-partition '(or "*" "**") "b**2-4*a*c")

(define test-exp3
  "fact := lambda n:
  if n == 0
  then 1
  else n*fact(n-1)"
  )

(exp-split test-exp3 split-lst)

