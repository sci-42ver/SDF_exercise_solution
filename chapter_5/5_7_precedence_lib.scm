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
;; Better see https://techvidvan.com/tutorials/python-operator-precedence/ which adds "Parentheses (Highest precedence)".
;; 0. Here for is directly related with in https://docs.python.org/3/tutorial/controlflow.html#for-statements
;; 1. > The power operator binds more tightly than unary operators on its left
;; "more tightly" means higher precedence.
;; n. examples
;; IMHO here we need to consider both group and precedence
;; n.0. b**2-4*a*c: **, -, *
;; n.1. (-b+sqrt(discriminant))/(2*a): (), -x, +, Binding, x(arguments...), /, *
;; See https://stackoverflow.com/a/68896193/21294350 or doc for Binding meaning
;; > The first thing the code block will do is bind the formal parameters to the arguments
;; n.2. fact := lambda n: if n == 0 then 1 else n*fact(n-1)
;; n.2.a. here := is a bit different from that in Python, anyway it is like define but can't be used alone https://docs.python.org/3/whatsnew/3.8.html#assignment-expressions
;; Additionally, it is same that evaluated at last when define-value is got.
;; n.2.b. So :=, lambda, if statement https://docs.python.org/3/reference/compound_stmts.html#the-if-statement, ==, *, -, Binding, x(arguments...)
;; n.2.b.0. Here (n-1) is not one tuple
;; > Note that tuples are not formed by the parentheses, but rather by use of the comma.

