;;; Similar to SDF_exercises/chapter_5/5_7_re_lib/5_7_regexp_lib_simplified_based_on_effbot.rkt
;; But uses IDENTIFIER.
(cd "~/SICP_SDF/SDF_exercises/chapter_5/MIT_GNU_Scheme_re_lib/")
(load "irregex_lib.scm")

(cd "~/SICP_SDF/SDF_exercises/")
(load "common-lib/re_lib.scm")

; #px"\\s*(?:(\\d+)|(\\w+)|((?<!\\w)(?:\\*|\\*\\*)\\w+)|([\\-\\+\\*/%!~<>=&^|?:]+)|([\\(\\)\\[\\]~^!?:,]))"
;; IGNORE I don't see any necessity for space here. Space there is kept to show as migration from pratt-parsing-demo/tdop.py.
;; IMHO the python doc https://docs.python.org/3/library/re.html#writing-a-tokenizer structure is better.
(define WORDS `(+ ,word-corrected))
;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Identifiers.html
;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Strings.html
(define DELIMITER-LIST '("(" ")" ";" "\"" "'" "`" "\|" "[" "]" "{" "}"))
;; 0. I use Scheme identifier convention here for simplicity.
;; 1. > not a syntactically valid number
;; detection is a bit too complex (skipped) TODO
(define NUMBER-PATTERN '(: (+ numeric) (? "." (* numeric))))
(define IDENTIFIER
  `(: 
     (neg-look-ahead ,NUMBER-PATTERN)
     (: (~ ,@DELIMITER-LIST "#" ",") (* (~ ,@DELIMITER-LIST)))
     ))
;; TODO here tokenizer is not good since only considering arglist case for STAR-ARG.
(define IDENTIFIER-AS-ARG
  `(: 
     (neg-look-ahead ,NUMBER-PATTERN)
     ;; IGNORE This alone at the end will contain the former negative pattern, and the former negated is then positive...
     ;; The above is due to for "(: (neg-look-behind ,word-corrected) (or "*" "**") ,IDENTIFIER-AS-ARG)", "*" is matched originally, and then "*45.23".
     (: (~ ,@DELIMITER-LIST "#" ",") (* (~ ,@DELIMITER-LIST "," ":")))
     ;; IGNORE So add (although very inefficient...) TODO
     ; (neg-look-behind ,NUMBER-PATTERN)
     )
  )
(define ADDITIONAL-OPERATORS (list "{" ";" "}" "."))

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_re_lib/")
(load "group_name_lib.scm")

(define pat
  `(or
     ;; See SDF_exercises/chapter_5/5_7_re_tests/optional.scm for "?" behavior.
     (=> ,NUMBER-TAG ,NUMBER-PATTERN) ; better than tdop.py
     ;; added
     (=> OPERATOR-WITH-SPACE-INSIDE
         (or "is not" "not in")
         )
     (=> ,STAR-ARG-TAG 
         (: (look-behind ",") 
            (or 
              (: "*" (neg-look-ahead "*") ,IDENTIFIER-AS-ARG) 
              (: "**" ,IDENTIFIER-AS-ARG))))
     ;; From tdop.py: maybe fail for some corner cases due to generality.
     (=> OPERATOR-LEN-POSSIBLY-GREATER-THAN-ONE
         ;; (Not for irregex) Use (intern "|") for or.
         (+ (or "-" "+" "*" "/" "%" "!" "~" "<" ">" "=" "&" "^" "|" "?" ":")))
     ;; IGNORE Since SDF exercise 5.7 only needs "infix expression", no "Statement terminator" is needed.
     ;; Also for "Line endings" (but we may introduce implicit newline, see SDF_exercises/chapter_5/5_7_tokenize_tests.scm).

     (=> ,ID-TAG ,WORDS)
     (=> OPERATOR-LEN-ONE (or "(" ")" "[" "]" ,@ADDITIONAL-OPERATORS "~" "^" "!" "?" ":" ","))
     ;; Add "Line endings", more general than the Python doc example.
     (=> SKIP (+ space))
     (=> MISMATCH nonl)
     )
  )

(cd "~/SICP_SDF/SDF_exercises/common-lib/")
(load "iterator_lib.scm")

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell/")
(load "DataTypeLib.scm")

(define FIELD-NAMES
  `(NUMBER
     OPERATOR-WITH-SPACE-INSIDE
     ,STAR-ARG-TAG
     OPERATOR-LEN-POSSIBLY-GREATER-THAN-ONE
     ,ID-TAG
     OPERATOR-LEN-ONE
     SKIP
     MISMATCH
     ))

(define (%Tokenize pat field-names input)
  (define keywords '("if" "lambda" "then" "else" "is" "or" "and" "not" "in"))
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
               ;; Use lowercase since keywords like lambda etc default to be that.
               (set! kind "number")
               )
              ;; tdop doesn't consider keywords for expression
              ((regexp-match-submatch match ID-TAG)
               (if (member value keywords)
                 (set! kind value)
                 (set! kind ID-TAG-STR))
               (set! value (string->symbol value))
               )
              ((regexp-match-submatch match 'SKIP)
               (set! continue #t)
               )
              ((regexp-match-submatch match 'MISMATCH)
               ;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Taxonomy.html#index-condition_002dtype_003aarithmetic_002derror
               ;; For simplicity, just use error instead of using one specific condition-type.
               (error (list value "is unexpected on line" line-num))
               )
              )
            ;; consistently make all types string.
            (and (symbol? kind) (set! kind (symbol->string kind)))
            (yield (Token kind value (Loc line-num column)))
            )
          ))
      (regexp-finditer pat input))
    (yield EOF-TOKEN)
    )
  )

(define lexer? coroutine?)

(define test-lexer 
  (%Tokenize 
    pat
    FIELD-NAMES
    "fact := lambda a,b=0,/,c,*args,*,kwarg1,**kwargs:
    if n == 0
    then 1
    else n*fact(n-1)"
    ))

(cd "~/SICP_SDF/SDF_exercises/common-lib")
(load "loop_lib.scm")

;; iterator always ends with FINISH-MARK.
(define (lexer-contents lexer)
  (let ((res '()))
    (while (lexer 'alive?)
           (let ((next-elm (lexer 'next)))
             ; (write-line next-elm)
             (set! res (cons next-elm res)))
           ;; IGNORE Here continuation in coroutine is a bit complex to debug.
           ;; This will always have '() for res in (cons (lexer 'next) res).
           ;; That is due to continuation (set! res (cons _ res)) is stored *with res '()* by (lexer 'next).
           ;; Then at the next iteration, (lexer 'next) implicitly returns to that caller with res *still* as '().
           ;; But for the above, (next-elm _) won't have state *across iteration*, so it won't be influenced.
           ; (write-line res)
           ; (set! res (cons (lexer 'next) res))
           ; (write-line res)
           )
    (reverse res))
  )
(define (meet-finish-elem? elem)
  (equal? elem FINISH-MARK)
  )
(define (assert-lexer lexer expected)
  (assert
    (equal?
      (lexer-contents lexer)
      expected
      )))
; (pp (lexer-contents test-lexer))
(assert-lexer
  test-lexer
  '((token "id" fact)
    (token "skip" " ")
    (token "operator-len-possibly-greater-than-one" ":=")
    (token "skip" " ")
    (token "lambda" lambda)
    (token "skip" " ")
    (token "id" a)
    (token "operator-len-one" ",")
    (token "id" b)
    (token "operator-len-possibly-greater-than-one" "=")
    (token "number" 0)
    (token "operator-len-one" ",")
    (token "operator-len-possibly-greater-than-one" "/")
    (token "operator-len-one" ",")
    (token "id" c)
    (token "operator-len-one" ",")
    (token "star-arg" "*args")
    (token "operator-len-one" ",")
    (token "operator-len-possibly-greater-than-one" "*")
    (token "operator-len-one" ",")
    (token "id" kwarg1)
    (token "operator-len-one" ",")
    (token "star-arg" "**kwargs")
    (token "operator-len-possibly-greater-than-one" ":")
    (token "skip" "\n      ")
    (token "if" if)
    (token "skip" " ")
    (token "id" n)
    (token "skip" " ")
    (token "operator-len-possibly-greater-than-one" "==")
    (token "skip" " ")
    (token "number" 0)
    (token "skip" "\n      ")
    (token "then" then)
    (token "skip" " ")
    (token "number" 1)
    (token "skip" "\n      ")
    (token "else" else)
    (token "skip" " ")
    (token "id" n)
    (token "operator-len-possibly-greater-than-one" "*")
    (token "id" fact)
    (token "operator-len-one" "(")
    (token "id" n)
    (token "operator-len-possibly-greater-than-one" "-")
    (token "number" 1)
    (token "operator-len-one" ")")
    (token "eof" "eof")
    finish-routine)
  )
(define test-lexer2
  (%Tokenize 
    pat
    FIELD-NAMES
    ;; Just one artifical and a bit nonsense demo
    "fact := lambda a,**45.23:
    a"
    ))
;; lookahead should be always preceded by something.
;; See `man perlre`
;; > "/(?!foo)bar/" will not do what you want
(irregex-search `(: (neg-look-ahead ,NUMBER-PATTERN) (* (~ "+"))) "45.23")
;; Not same as perl... see `perl -e 'print "find" if "45.23"=~/^(?![\d.]+)[^+]*$/'` which returns nothing.
;; (see perlrequick for =~, https://perldoc.perl.org/builtin#false for false https://stackoverflow.com/a/1036353/21294350 which has no one perl... manpage).
(irregex-match-substring 
  (irregex-match `(posix-string "(?![\d.]+)[^+]*") "45.23")
  0)
;Value: "45.23"

; (lexer-contents test-lexer2)
(assert-lexer
  test-lexer2
  '((token "id" fact)
    (token "skip" " ")
    (token "operator-len-possibly-greater-than-one" ":=")
    (token "skip" " ")
    (token "lambda" lambda)
    (token "skip" " ")
    (token "id" a)
    (token "operator-len-one" ",")
    (token "operator-len-possibly-greater-than-one" "**")
    (token "number" 45.23)
    (token "operator-len-possibly-greater-than-one" ":")
    (token "skip" "\n      ")
    (token "id" a)
    (token "eof" "eof")
    finish-routine))

(define test-lexer3
  (%Tokenize 
    pat
    FIELD-NAMES
    ;; Just one artifical and a bit nonsense demo
    "fact := lambda a,**45.23:
    {a+a;a*2}"
    ))
; (pp (lexer-contents test-lexer3))
(assert-lexer
  test-lexer3
  '((token "id" fact)
    (token "skip" " ")
    (token "operator-len-possibly-greater-than-one" ":=")
    (token "skip" " ")
    (token "lambda" lambda)
    (token "skip" " ")
    (token "id" a)
    (token "operator-len-one" ",")
    (token "operator-len-possibly-greater-than-one" "**")
    (token "number" 45.23)
    (token "operator-len-possibly-greater-than-one" ":")
    (token "skip" "\n      ")
    (token "operator-len-one" "{")
    (token "id" a)
    (token "operator-len-possibly-greater-than-one" "+")
    (token "id" a)
    (token "operator-len-one" ";")
    (token "id" a)
    (token "operator-len-possibly-greater-than-one" "*")
    (token "number" 2)
    (token "operator-len-one" "}")
    (token "eof" "eof")
    finish-routine))

(define test-lexer4
  (%Tokenize 
    pat
    FIELD-NAMES
    ;; Just test _ recognition inside id.
    "lambda a_b: a_b"
    ))
; (pp (lexer-contents test-lexer4))
(assert-lexer
  test-lexer4
  '((token "lambda" lambda)
    (token "skip" " ")
    (token "id" a_b)
    (token "operator-len-possibly-greater-than-one" ":")
    (token "skip" " ")
    (token "id" a_b) (token "eof" "eof")
    finish-routine))

(define test-lexer5
  (%Tokenize 
    pat
    FIELD-NAMES
    ;; Just test _ recognition inside id.
    "a is not b not in c <= d"
    ))
; (pp (lexer-contents test-lexer5))
(assert-lexer
  test-lexer5
  '((token "id" a) (token "skip" " ")
                   (token "operator-with-space-inside" "is not")
                   (token "skip" " ")
                   (token "id" b)
                   (token "skip" " ")
                   (token "operator-with-space-inside" "not in")
                   (token "skip" " ")
                   (token "id" c)
                   (token "skip" " ")
                   (token "operator-len-possibly-greater-than-one" "<=")
                   (token "skip" " ")
                   (token "id" d)
                   (token "eof" "eof")
                   finish-routine))

(define test-lexer6
  (%Tokenize 
    pat
    FIELD-NAMES
    ;; Just test _ recognition inside id.
    "3.14==f(b).val"
    ))
; (pp (lexer-contents test-lexer6))
(assert-lexer
  test-lexer6
  '((token "number" 3.14) (token "operator-len-possibly-greater-than-one" "==")
                          (token "id" f)
                          (token "operator-len-one" "(")
                          (token "id" b)
                          (token "operator-len-one" ")")
                          (token "operator-len-one" ".")
                          (token "id" val)
                          (token "eof" "eof")
                          finish-routine))

(define (Tokenize str)
  (%Tokenize pat FIELD-NAMES str)  
  )

(define (pop lexer)
  (assert (lexer? lexer))
  (if (lexer 'alive?)
    (lexer 'next)
    (error "no more elements in lexer"))
  )
