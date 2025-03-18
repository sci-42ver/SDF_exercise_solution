;;; Similar to tokenize in https://eli.thegreenplace.net/2010/01/02/top-down-operator-precedence-parsing
;; but not uses generator https://stackoverflow.com/a/1073582/21294350
(cd "~/SICP_SDF/SDF_exercises/chapter_5")
(load "5_7_common_lib/5_7_basic_pred_lib.scm")

;; https://srfi.schemers.org/srfi-115/srfi-115.html#Types-and-Naming-Conventions
;; > re: an SRE or pre-compiled regexp object
;; Here I just use SRE for simplicity without using (regexp re) to compile.
(define (sre-lst? sre-lst)
  (and (list? sre-lst) (every valid-sre? sre-lst)))
(define (make-or sre-lst)
  (assert (sre-lst? sre-lst))
  (cons 'or sre-lst)
  )

;;; (see git history) refactored for modularity https://stackoverflow.com/questions/1025844/what-is-refactoring-and-what-is-only-modifying-code/1025867#1025867
;; > altering its internal structure without changing its external behavior

;; 0. See SDF_exercises/chapter_5/5_7_precedence_lib.scm
;; Here "," is not one separtor among statements in Python which uses line (see https://docs.python.org/3/reference/lexical_analysis.html#line-structure and https://stackoverflow.com/a/35053210/21294350)
(define primitive-op-lst '("*" ":" "-" "+" "/" "="))

;; no use to make neg-look-behind/look-ahead etc work.
; (cd "~/SICP_SDF/SDF_exercises/chapter_5/re_lib")
; (load "regexp.sld")

(define word-corrected '(or word (+ numeric)))
(define unsplitted-var 
  `(: 
    ;; IGNORE 0. only consider unary
    ;; 0.a. word is not with "fixed length".
    ; (? (or (: (neg-look-behind alphanumeric) "*") (: (neg-look-behind alphanumeric) "**")))
    ;; 1. Here we only need to ensure *args etc won't be splitted further.
    ;; Normal var "arg" etc trivially won't be splitted.
    ; (? (or "*" "**"))
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
;; 2. 
(define unsplittable-primitive-op-lst `("**" ":=" "==" ,unsplitted-var))

;; One workaround for neg-look-behind.
;; i.e. only the alone *args etc are not splittable, but a*b etc can.
(define (string-null? str)
  (assert (string? str))
  (n:= 0 (string-length str))
  )
(define (optional-arg-lst arg)
  (if (default-object? arg)
    '()
    (list arg))
  )
(define (substring-lst str #!optional start end)
  (let ((res 
          (apply 
            substring 
            (append (list str) (optional-arg-lst start) (optional-arg-lst end)))))
    (if (string-null? res)
      '()
      (list res))
    )
  )
(define (get-args-kwargs exp)
  (assert (string? exp))
  (let ((last-end 0)
        (len (string-length exp)))
    (let ((res 
            (regexp-fold unsplitted-var
                          ;; IGNORE Here i is the index before m-start or 0 if m-start is 0.
                          ;; Emm... sometimes i is the index of m-start inclusive, while sometimes exclusive.
                          (lambda (i m str acc)
                            (let* ((s (regexp-match-submatch m 0))
                                    (start (regexp-match-submatch-start m 0))
                                    (end (regexp-match-submatch-end m 0))
                                    ; (corrected-i 
                                    ;   (if (n:= start 0)
                                    ;       start
                                    ;       (n:- start 1))
                                    ;   )
                                    (str-before (substring str 0 start)))
                              (let ((end-with-word (regexp-search `(: ,word-corrected eos) str-before))
                                    (match (regexp-match-submatch m 0))
                                    )
                                ; (write-line (list "last-end" last-end "i" i "match" match "str-before" str-before))
                                (if end-with-word
                                  (begin
                                    (write-line 
                                      (list 
                                        match
                                        "with"
                                        (regexp-match-submatch end-with-word 0) 
                                        "before"
                                        "is not one *args or **kwargs"
                                        ))
                                    acc
                                    )
                                  (let ((intermediate-str 
                                          (substring-lst 
                                            str 
                                            last-end 
                                            ; (if (n:= corrected-i 0) corrected-i (n:+ corrected-i 1))
                                            start
                                            )))
                                    ; (write-line (list "last-end" last-end "i" i "append" intermediate-str match))
                                    (set! last-end end)
                                    (append
                                      (if 
                                        ; (default-object? acc)
                                        (eq? #!unspecific acc)
                                        '()
                                        acc)
                                      intermediate-str
                                      (list match))
                                    )
                                  ))
                              ))
                          '()
                          exp)
            ))
      (append res (substring-lst exp last-end))
      )
    )
  )
(define n:+ +)
(define n:- -)
(define n:= =)
(get-args-kwargs "k*c+a*b")
(get-args-kwargs "b**2-4*a*c")
(get-args-kwargs "n*fact(n-1)")
(get-args-kwargs "a,b=0,/,c,*args,*,kwarg1,**kwargs")

;; 0. no further partitions done for them.
(define primitive-symbol-re-lst `(,unsplitted-var ,(make-or unsplittable-primitive-op-lst)))
(define partition-separtor-lst 
  ;; IGNORE "," is to prepare for split of "*args," etc.
  `((or ,left-parenthesis ,right-parenthesis) 
    ,@primitive-symbol-re-lst
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
              (append-map 
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
        (lp res-lst-cluster (cdr rest-separtor-lst))))
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

