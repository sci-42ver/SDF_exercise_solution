(define exp1 '("b" "**" "2" "-" "4" "*" "a" "*" "c"))
(define exp2 '("(" "-" "b" "+" "sqrt" "(" "discriminant" ")" ")" "/" "(" "2" "*" "a" ")"))
(define exp3 '("fact" ":=" "lambda" "n" ":" "if" "n" "==" "0" "then" "1" "else" "n" "*" "fact" "(" "n" "-" "1" ")"))

(define (%exp->siod exp)
  (assert string? exp)
  (cond
    ; '#.OPEN-PAREN can't be accepted in MIT/GNU Scheme.
    ((equal? "(" exp) "#.OPEN-PAREN")
    ((equal? ")" exp) "#.CLOSE-PAREN")
    ((equal? "==" exp) "=")
    (else 
      ; (string->symbol exp)
      exp
      ))
  )

(define (exp->siod exp-lst)
  (assert (every string? exp-lst))
  (let ((lst* (map %exp->siod exp-lst)))
    (write-line lst*)
    (string-append
      "'("
      ; (string-join lst* " ")
      (apply (string-joiner 'infix " ") lst*)
      ")"
      )
    )
  )

; (for-each (lambda (exp) (write-line (exp->siod exp))) (list exp1 exp2 exp3))
(for-each (lambda (exp) (write-line (exp->siod exp))) (list exp1 exp2 exp3))
