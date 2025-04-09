;; tokenizer tests
(define test-exp1 "b**2-4*a*c")
(define test-exp2 "(-b+sqrt(discriminant))/(2*a)")
(define expected-parsed-test-exp2 '("(" "-" "b" "+" "sqrt" "(" "discriminant" ")" ")" "/" "(" "2" "*" "a" ")"))

(define test-book-exp
  "fact := lambda n:
  if n == 0
  then 1
  else n*fact(n-1)"
  )
(define test-exp4 "b**m-n*a*c")
(define test-exp5 "fact := lambda a, b = 0, /, c, *args, *, kwarg1, **kwargs:
  if n == 0
  then 1
  else n*fact(n-1)")

(assert (equal? (parse test-exp1) '("b" "**" "2" "-" "4" "*" "a" "*" "c")))
(assert (equal? (parse test-exp2) expected-parsed-test-exp2))
(assert (equal? (parse test-book-exp) '("fact" ":=" "lambda" "n" ":" "if" "n" "==" "0" "then" "1" "else" "n" "*" "fact" "(" "n" "-" "1" ")")))
(assert (equal? (parse test-exp4) '("b" "**" "m" "-" "n" "*" "a" "*" "c")))

(parse test-exp5)
(assert 
  (equal? 
    (parse test-exp5) 
    '("fact" ":=" "lambda" 
      "a" "," 
      "b" "=" "0" "," 
      "/" "," 
      "c" "," 
      "*args" "," 
      "*" "," 
      "kwarg1" "," 
      "**kwargs"
      ":" "if" "n" "==" "0" "then" "1" "else" "n" "*" "fact" "(" "n" "-" "1" ")")))
