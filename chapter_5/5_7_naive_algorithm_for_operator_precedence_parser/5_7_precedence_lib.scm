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
;; "parenthesized expression" just means that using parenthesis to change the evaluation order.
;;; 0. IGNORE (I don't know why I say about for statement.): Here for is directly related with in https://docs.python.org/3/tutorial/controlflow.html#for-statements
;; 1. > The power operator binds more tightly than unary operators on its left
;; "more tightly" means higher precedence.
;; n. examples
;; IMHO here group and precedence means same.
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

(cd "~/SICP_SDF/SDF_exercises/chapter_5")
(load "5_7_parenthesis_syntax_lib.scm")
(load "../common-lib/op_lib.scm")

(define (%manipulate-precedence order-tag operator-lst item-list full-precedence-list)
  ;; Use Interned symbols https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Symbols.html
  (let ((is-left-to-right-order (eq? from-left-to-right-tag order-tag))
        (is-right-to-left-order (eq? from-right-to-left-tag order-tag))
        (item-cnt (length item-list)))
    (and (not is-left-to-right-order) (not is-right-to-left-order)
      (error "unrecognized precedence order"))
    (let ((start-idx 
            (cond 
              (is-left-to-right-order 0)
              (is-right-to-left-order item-cnt)
              ))
          (next-op
            (cond 
              (is-left-to-right-order inc)
              (is-right-to-left-order dec)
              )
            )
          )
      (let lp ((cur-idx start-idx))
        (find-op-and-transform operator-lst item-list full-precedence-list)
        )
      ))
  )
(define (manipulate-precedence item-list precedence-list)
  (assert (item-list? item-list))
  ;; Ensure manipulation order by the explicit loop
  (let lp ((transformed-item-list item-list) (rest-precedence-list precedence-list))
    (if (null? precedence-list)
      transformed-item-list
      (let ((cur-precedence-items (car rest-precedence-list)))
        (lp
          (%manipulate-precedence 
            (get-tagged-lst-tag cur-precedence-items)  
            (get-tagged-lst-data cur-precedence-items) 
            transformed-item-list 
            precedence-list)
          (cdr precedence-list)
          )
        ))
    )
  )
(define precedence-list
  (list
    ;; Notice all these pred's/str/func should return one object for consistency.
    (new-from-left-to-right-precedence-items 
      ;; 0. For simplicity, comprehension is skipped for list https://docs.python.org/3/reference/expressions.html#list-displays.
      ;; So only "a comma-separated list of expressions".
      ;; 1. Here I also consider "call" which must be related with Binding 
      ;; when we have manipulated with parenthesized expression in combine-non-application-parentheses.
      ;; 2. IGNORE Here I skipped tuple since it is immutable.
      ;; > if the list contains at least one comma, it yields a tuple; otherwise, it yields the single expression that makes up the expression list.
      ;; Here we can assume Scheme has one (tuple ...) procedure and set! syntax redefinition which can check tuple tag to ensure immutability.
      (new-delimit-then-divide-exp (new-pair (list variable? left-parenthesis right-parenthesis) ","))
      ;; Dict https://docs.python.org/3/reference/expressions.html#dictionary-displays
      ;; 0. '"**" or_expr' is skipped.
      ;; 1. key: value is manipulated in infix->polish
      )
    (new-from-left-to-right-precedence-items 
      ;; Use obj? because `[1,2][1]` is valid in Python.
      (new-delimit-exp (list obj? "[" "]"))
      ;; skipped due to being related with class https://docs.python.org/3/reference/datamodel.html#custom-classes
      ; (new-exact-exp (list variable? "." variable?))
      )
    (new-from-right-to-left-precedence-items 
      ;; Notice iterate from-right-to-left
      (new-exact-exp (list obj? "**" obj?))
      )
    (new-from-left-to-right-precedence-items 
      (new-non-obj-before-exp (list "-" obj?))
      )
    (new-from-left-to-right-precedence-items 
      (new-exact-exp (list obj? "*" obj?))
      (new-exact-exp (list obj? "/" obj?))
      )
    (new-from-left-to-right-precedence-items 
      (new-exact-exp (list obj? "-" obj?))
      (new-exact-exp (list obj? "+" obj?))
      )
    (new-from-left-to-right-precedence-items 
      (new-exact-exp (list obj? "==" obj?))
      )
    ;; Not in Python doc
    ;; Python uses logical line to catch if statement. But here we can't do that in one str easily... 
    ;; Here I assume it is similar to if – else https://docs.python.org/3/reference/expressions.html#if-expr
    (new-from-left-to-right-precedence-items
      (new-delimit-exp (list "if" "then" "else"))
      )
    (new-from-left-to-right-precedence-items
      ;; 0. skip "/" in parameter_list https://docs.python.org/3/reference/compound_stmts.html#grammar-token-python-grammar-parameter_list
      ;; See https://peps.python.org/pep-0570/#function-examples
      ;; See 0.a.: Due to no keyword parameter, so all are positional-only... 
      ;; And 0.n. for summary.
      ;; 0.a. And also for parameter_star_kwargs etc https://stackoverflow.com/a/36908/21294350.
      ;; ~~Anyway~~ Although Scheme ~~doesn't have~~ has one native way to do unpack *hint* like "*" does (i.e. (proc . args)), but not for keyword parameter restriction "**".
      ;; The latter can be done in https://srfi.schemers.org/srfi-177/srfi-177.html (not in MIT/GNU Scheme) where "(c d e)" must be kw.
      ;; So the former can't be generalized as Python does (see SDF_exercises/chapter_5/5_7_related_python_behavior/parameter_list.py).
      ;; IGNORE: The former can be done with no hint in procedure definition https://stackoverflow.com/a/30522731/21294350.
      ;; 0.b. For "parameter ["=" expression]", Racket supports default value better https://stackoverflow.com/a/36220746/21294350
      ;; but not for MIT/GNU Scheme.
      ;; 0.c. Similarly no type hints "identifier [":" expression]".
      ;; 0.d. identifier checking seems off topic... https://docs.python.org/3/reference/lexical_analysis.html#grammar-token-python-grammar-identifier
      ;; https://unicode.org/reports/tr15/
      ;; 0.n. SO only something like (lambda a,b,c: a+b+c)(1,2,3) is considered https://docs.python.org/3/tutorial/controlflow.html#lambda-expressions.
      ;; Then "a,b,c" can be manipulated like Binding,
      ;; i.e. comma-param-lst? should return (a,b,c)=> (a b c) parameter_list in Scheme.
      ;; 0.n+1. ALL IN ALL, https://tdop.github.io/ just uses the simplest lambda argument list where '",", getlist 25' should be '"," getlist 25' by (a getlist b) and 'is "EXPT"' notation.
      ;; See https://docs.python.org/3/reference/compound_stmts.html#function-definitions
      ;; Trivially parameter_list, parameter_list_no_posonly are all delimited by ",".
      ;; See SDF_exercises/chapter_5/5_7_related_python_behavior/parameter_list.py
      ;; where [","] in parameter_star_kwargs is only omitted when at the end and actually it must be at the end...
      ;; 1. For define, skip Type parameter lists "func[T](a: T, b: T) -> T" (i.e. type_params) https://peps.python.org/pep-0695/#summary-examples
      (new-delimit-exp (list "lambda" comma-param-lst? ":" obj?))
      )
    (new-from-left-to-right-precedence-items 
      (new-exact-exp (list obj? ":=" obj?))
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

