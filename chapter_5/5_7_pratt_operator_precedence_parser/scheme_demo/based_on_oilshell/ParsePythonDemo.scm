(cd "~/SICP_SDF/SDF_exercises/chapter_5")
(load "5_7_tokenize_lib.scm")
(load "5_7_tokenize_tests.scm")

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell")
(load "data_type_lib.scm")
;; For implementation directly based on one-pass regex same as oilshell: see SDF_exercises/chapter_5/5_7_re_lib/5_7_regexp_lib_simplified_based_on_effbot.rkt.
(define (tokenize* exp)
  (map (lambda (elm) (Token-with-type elm)) (tokenize exp)))
(define pat
  "\\s*(?:(\\d+)|(\\w+)|((?<!\\w)(?:\\*|\\*\\*)\\w+)|([\\-\\+\\*/%!~<>=&^|?:]+)|([\\(\\)\\[\\]~^!?:,]))"
  )
(define (Token-with-type elm)
  (define (get-type str)
    (cond 
      (predicate1 consequent1)
      (predicate2 consequent2)))
  )

(define (MakePythonParserSpec)
  body)
(define (MakeParser str)
  (Parser (MakePythonParserSpec) (tokenize* str))
  )
(define (ParsePythonDemo str #!optional expected)
  (let ((res ((MakeParser str) 'Parse)))
    
    )
  )