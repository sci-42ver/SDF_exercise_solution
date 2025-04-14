;;; Similar to SDF_exercises/chapter_5/5_7_re_lib/5_7_regexp_lib_simplified_based_on_effbot.rkt
(cd "~/SICP_SDF/SDF_exercises/chapter_5/MIT_GNU_Scheme_re_lib/")
(load "irregex_lib.scm")

(cd "~/SICP_SDF/SDF_exercises/")
(load "common-lib/re_lib.scm")

; #px"\\s*(?:(\\d+)|(\\w+)|((?<!\\w)(?:\\*|\\*\\*)\\w+)|([\\-\\+\\*/%!~<>=&^|?:]+)|([\\(\\)\\[\\]~^!?:,]))"
;; IGNORE I don't see any necessity for space here. Space there is kept to show as migration from SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/python_demo/pratt-parsing-demo/tdop.py.
;; IMHO the python doc https://docs.python.org/3/library/re.html#writing-a-tokenizer structure is better.
(define WORDS `(+ ,word-corrected))
(define pat
  `(or
    ;; See SDF_exercises/chapter_5/5_7_re_tests/optional.scm for "?" behavior.
    (=> NUMBER (: (+ numeric) (? "." (* numeric)))) ; better than tdop
    (=> STAR-ARG (: (neg-look-behind ,word-corrected) (or "*" "**") ,WORDS))
    ;; From tdop: maybe fail for some corner cases due to generality.
    (=> OPERATOR-LEN-GREATER-THAN-ONE
      ;; (Not for irregex) Use (intern "|") for or.
      (+ (or "-" "+" "*" "/" "%" "!" "~" "<" ">" "=" "&" "^" "|" "?" ":")))
    ;; Since 5.7 only needs "infix expression", no "Statement terminator" is needed.
    ;; Also for "Line endings" (but we may introduce implicit newline, see SDF_exercises/chapter_5/5_7_tokenize_tests.scm).
    (=> ID ,WORDS)
    ;; regex replacement: '(.)' => '"$1" '
    (=> OPERATOR-LEN-ONE (or "(" ")" "[" "]" "~" "^" "!" "?" ":" ","))
    ;; Add "Line endings", more general than the Python doc example.
    (=> SKIP (+ space))
    (=> MISMATCH nonl)
    )
  )

(cd "~/SICP_SDF/SDF_exercises/common-lib/")
(load "iterator_lib.scm")

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell/")
(load "data_type_lib.scm")

(define field-names 
  '(NUMBER
    STAR-ARG
    OPERATOR-LEN-GREATER-THAN-ONE
    ID
    OPERATOR-LEN-ONE
    SKIP
    MISMATCH
    ))

(define (Tokenize pat field-names input)
  (define keywords '("if" "lambda" "then" "else"))
  (coroutine*
    (for-each-elm-in-iterator 
      (let ((line-num 1) (line-start 0))
        (lambda (match)
          (let ((value (regexp-match-submatch match 0))
                (column (- (regexp-match-submatch-start match 0) line-start))
                (continue #f)
                (kind (match-lastgroup match field-names))
                )
            (cond 
              ((regexp-match-submatch match 'NUMBER)
                (set! value (string->number value))
                ;; Use lowercase to be compatible with string->symbol.
                (set! kind "number")
                )
              ;; tdop doesn't consider keywords for expression
              ((regexp-match-submatch match 'ID)
                (if (member value keywords)
                  (set! kind value)
                  (set! kind "id")))
              ((regexp-match-submatch match 'SKIP)
                (set! continue #t)
                )
              ((regexp-match-submatch match 'MISMATCH)
                ;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Taxonomy.html#index-condition_002dtype_003aarithmetic_002derror
                ;; For simplicity, just use error instead of using one specific condition-type.
                (error (list value "is unexpected on line" line-num))
                )
              )
            (yield (Token kind value (Loc line-num column)))
            )
          ))
      (regexp-finditer pat input))
    )
  )

(define test-lexer 
  (Tokenize 
    pat
    field-names
    "fact := lambda a,b=0,/,c,*args,*,kwarg1,**kwargs:
      if n == 0
      then 1
      else n*fact(n-1)"
    ))

(cd "~/SICP_SDF/SDF_exercises/common-lib")
(load "loop_lib.scm")

;; iterator always ends with FINISH-MARK.
(assert
  (equal?
    (let ((res '()))
      (while (test-lexer 'alive?)
        (let ((next-elm (test-lexer 'next)))
          ; (write-line next-elm)
          (set! res (cons next-elm res)))
        ;; IGNORE Here continuation in coroutine is a bit complex to debug.
        ;; This will always have '() for res in (cons (test-lexer 'next) res).
        ;; That is due to continuation (set! res (cons _ res)) is stored *with res '()* by (test-lexer 'next).
        ;; Then at the next iteration, (test-lexer 'next) implicitly returns to that caller with res *still* as '().
        ;; But for the above, (next-elm _) won't have state *across iteration*, so it won't be influenced.
        ; (write-line res)
        ; (set! res (cons (test-lexer 'next) res))
        ; (write-line res)
        )
      (reverse res))
    '(("id" "fact") 
      (skip " ")
      (operator-len-greater-than-one ":=")
      (skip " ")
      ("lambda" "lambda")
      (skip " ")
      ("id" "a")
      (operator-len-one ",")
      ("id" "b")
      (operator-len-greater-than-one "=")
      ("number" 0)
      (operator-len-one ",")
      (operator-len-greater-than-one "/")
      (operator-len-one ",")
      ("id" "c")
      (operator-len-one ",")
      (star-arg "*args")
      (operator-len-one ",")
      (operator-len-greater-than-one "*")
      (operator-len-one ",")
      ("id" "kwarg1")
      (operator-len-one ",")
      (star-arg "**kwargs")
      (operator-len-greater-than-one ":")
      (skip "\n      ")
      ("if" "if")
      (skip " ")
      ("id" "n")
      (skip " ")
      (operator-len-greater-than-one "==")
      (skip " ")
      ("number" 0)
      (skip "\n      ")
      ("then" "then")
      (skip " ")
      ("number" 1)
      (skip "\n      ")
      ("else" "else")
      (skip " ")
      ("id" "n")
      (operator-len-greater-than-one "*")
      ("id" "fact")
      (operator-len-one "(")
      ("id" "n")
      (operator-len-greater-than-one "-")
      ("number" 1)
      (operator-len-one ")")
      finish-routine)
    ))
