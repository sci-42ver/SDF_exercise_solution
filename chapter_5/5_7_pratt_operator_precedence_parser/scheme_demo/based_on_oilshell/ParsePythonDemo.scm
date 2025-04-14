(cd "~/SICP_SDF/SDF_exercises/chapter_5")
(load "5_7_tokenize_lib.scm")
(load "5_7_tokenize_tests.scm")

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell")
(load "data_type_lib.scm")

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_re_lib/")
(load "5_7_regexp_lib_simplified_based_on_effbot_based_on_irregex.scm")

(define (MakePythonParserSpec)
  body)
(define (MakeParser str)
  (Parser (MakePythonParserSpec) (Tokenize str))
  )
(define (ParsePythonDemo str #!optional expected)
  (let ((res ((MakeParser str) 'Parse)))
    
    )
  )