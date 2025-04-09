
;; This is similar to the demo in https://docs.python.org/3/library/re.html#writing-a-tokenizer
;; You can use named group here https://docs.racket-lang.org/rhombus-reference/regexp.html#:~:text=The%20rx%20and%20rx_in%20binding%20forms%20match,string%20can%20force%20a%20choice%20of%20mode.
;; Interested ones can implement this. Anyway the basic ideas are same as SDF_exercises/chapter_5/5_7_re_lib/5_7_regexp_lib_simplified_based_on_effbot.rkt.
;; The advantage is obvious that token type information is contained in regex pat, so that it is more modular to make related things *all in one place*.