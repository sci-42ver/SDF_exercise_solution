;;;; Just one demo, complex version please see compiler book...

;;; IGNORE
;; Here I follow https://en.cppreference.com/w/c/language/operator_precedence
;; and try implementating the 1st 2 Precedence and other used by tests in SDF_exercises/chapter_5/5_7.scm as one demo.

;; The basic ideas are that we follow the Associativity order for each Precedence and try starting at each elem in str-lisr.
;; Take Precedence 1 as one example:
;; we may have
;; 0. sym++
;; 1. proc(...)
;; 2. arr[]
;; 3. union.inner-data
;; 4. union_ptr->inner-data
;; 5. (type){list}
;; So the detection order for them doesn't matter.
;; For Precedence 2 (Right-to-left) (Here obj can be anything like f(a) etc):
;; 0. ++obj
;; 1. +obj
;; 2. !obj
;; 3. (type)obj
;; 4. *obj
;; 5. &obj
;; 6. sizeof(type) or sizeof expression	
;; 7. _Alignof(type)

;;; Due to **, python is more appropriate here. Actually it is compatible with all tests offered by the book author.
;; https://docs.python.org/3/reference/expressions.html#operator-precedence
;; > Operators in the same box group left to right (except for exponentiation and conditional expressions, which group from right to left).
;;; IGNORE Better see https://techvidvan.com/tutorials/python-operator-precedence/ which adds "Parentheses (Highest precedence)".
;; "parenthesized expression" just means that.
;;; 0. Here for is directly related with in https://docs.python.org/3/tutorial/controlflow.html#for-statements
;; 1. > The power operator binds more tightly than unary operators on its left
;; "more tightly" means higher precedence.
;; n. examples
;; IMHO here we need to consider both group and precedence
;; n.0. b**2-4*a*c: **, - (binary), *
;; n.1. (-b+sqrt(discriminant))/(2*a): (), -x, +, Binding, x(arguments...), /, *
;; See https://stackoverflow.com/a/68896193/21294350 or doc for Binding meaning
;; > The first thing the code block will do is bind the formal parameters to the arguments
;; n.2. fact := lambda n: if n == 0 then 1 else n*fact(n-1)
;; n.2.a. here := is a bit different from that in Python, anyway it is like define but can't be used alone https://docs.python.org/3/whatsnew/3.8.html#assignment-expressions
;; Additionally, it is same that evaluated at last when define-value is got.
;; n.2.b. So it uses
;; :=, lambda, if statement https://docs.python.org/3/reference/compound_stmts.html#the-if-statement, ==, *, -, Binding, x(arguments...)
;; n.2.b.0. Here (n-1) is not one tuple
;; > Note that tuples are not formed by the parentheses, but rather by use of the comma.

(define (name parameters)
  body)
(define precedence-list
  (list
    ;; Notice all these pred's/str/func should return one object for consistency.
    (new-tagged-lst 'from-left-to-right 
      ;; 0. For simplicity, comprehension is skipped https://docs.python.org/3/reference/expressions.html#list-displays.
      ;; So only "a comma-separated list of expressions".
      ;; 1. Here I also consider "call" which must be related with Binding 
      ;; when we have manipulated with parenthesized expression in combine-non-application-parentheses.
      (new-pair 'delimit-then-divide (new-pair (list variable? left-parenthesis right-parenthesis) ","))
      ;; Dict https://docs.python.org/3/reference/expressions.html#dictionary-displays
      ;; 0. '"**" or_expr' is skipped.
      ;; 1. key: value is manipulated in infix->polish
      )
    (new-tagged-lst 'from-left-to-right 
      ;; Use obj? because `[1,2][1]` is valid in Python.
      (new-pair 'delimit (list obj? "[" "]"))
      ;; skipped due to being related with class https://docs.python.org/3/reference/datamodel.html#custom-classes
      ; (new-pair 'exact (list variable? "." variable?))
      )
    (new-tagged-lst 'from-right-to-left 
      ;; Notice iterate from-right-to-left
      (new-pair 'exact (list obj? "**" obj?))
      )
    (new-tagged-lst 'from-left-to-right 
      (new-pair 'non-obj-before (list "-" obj?))
      )
    (new-tagged-lst 'from-left-to-right 
      (new-pair 'exact (list obj? "*" obj?))
      (new-pair 'exact (list obj? "/" obj?))
      )
    (new-tagged-lst 'from-left-to-right 
      (new-pair 'exact (list obj? "-" obj?))
      (new-pair 'exact (list obj? "+" obj?))
      )
    (new-tagged-lst 'from-left-to-right 
      (new-pair 'exact (list obj? "==" obj?))
      )
    ;; Not in Python doc
    ;; Python uses logical line to catch if statement. But here we can't do that in one str easily... 
    ;; Here I assume it is similar to if – else https://docs.python.org/3/reference/expressions.html#if-expr
    (new-tagged-lst 'from-left-to-right
      (new-pair 'delimit (list "if" "then" "else"))
      )
    (new-tagged-lst 'from-left-to-right
      ;; 0. skip "/" in parameter_list https://docs.python.org/3/reference/compound_stmts.html#grammar-token-python-grammar-parameter_list
      ;; 0.a. so only something like (lambda a,b,c: a+b+c)(1,2,3) is considered https://docs.python.org/3/tutorial/controlflow.html#lambda-expressions.
      ;; Then "a,b,c" can be manipulated like Binding,
      ;; i.e. comma-param-lst? should return (a,b,c)=> (a b c) parameter_list in Scheme.
      (new-pair 'delimit (list "lambda" comma-param-lst? ":" obj?))
      )
    (new-tagged-lst 'from-left-to-right 
      (new-pair 'exact (list obj? ":=" obj?))
      )
    )
  )
;;; What to do
;; All non-unary/binary operators are in precedence1&2 by checking "...".
;; precedence1
;; (expressions...) Binding: we do infix->polish for each elem and then combine with ","=>" ".
;; [expressions...]: similar to (expressions...) and then (list expressions...).
;; {key: value...} (differentiate from the next based on interval of ","): (list (cons key value) ...) https://stackoverflow.com/questions/61178300/scheme-dict-operations/61180123#61180123
;; {expressions...}: (delete-duplicates (list expressions...)).
;; Compared with parenthesized expression, here all can be splitted based on comma ",".

;; precedence2
;;; x(arguments...): based on the former, we just get (x arguments...).
;; IMHO this can be do together with the above (expressions...) since no others use () and they are always used together.
;;; 

