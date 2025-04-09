(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_common_lib/re/")
(load "general_re_lib.scm")
(define pat
  ;; from SDF_exercises/chapter_5/5_7_re_lib/5_7_regexp_lib_simplified_based_on_effbot.rkt
  ; "\\s*(?:(\\d+)|(\\w+)|((?<!\\w)(?:\\*|\\*\\*)\\w+)|([\\-\\+\\*/%!~<>=&^|?:]+)|([\\(\\)\\[\\]~^!?:,]))"
  `(or 
    (-> number (+ numeric)) 
    (-> var ,word-corrected)
    )
  )

(cd "~/SICP_SDF/SDF_exercises/common-lib")
(load "re_lib.scm")
(regexp-extract pat "192.168.0.1abdasdsa448cas1ca1sc8asc1" 1 3)

;; Here regexp-match->list has the same structure as .
;; > beginning with the entire match 0.
;; https://docs.racket-lang.org/reference/regexp.html#%28def._%28%28lib._racket%2Fprivate%2Fbase..rkt%29._regexp-match%2A%29%29
;; > The default of car means that the result is the list of matches without returning parenthesized sub-patterns.
(map
  (lambda (obj)
    ;; Here regexp-match? just checks regexp-match object.
    ;; 0. > Returns true iff obj is a successful match from regexp-matches
    ;; 1. > Returns an regexp-match object if re successfully matches the entire string str from start (inclusive) to end (exclusive), 
    ;; > or #f is the match fails.
    (if (regexp-match? obj)
      (regexp-match->list obj)
      obj)
    )
  (regexp-extract* pat "192.168.0.1abdasdsa448cas1ca1sc8asc1"))
(regexp-search pat "192.168.0.1abdasdsa448cas1ca1sc8asc1")