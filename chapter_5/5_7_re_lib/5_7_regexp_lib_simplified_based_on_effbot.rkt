#lang racket
;;; This is just one demo for simulating Tokenize in SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/python_demo/pratt-parsing-demo/tdop.py.
;; But it can't be combined with the code base due to the latter are all written in MIT/GNU Scheme...
;; So I won't use this for ex-5.7.

;; I use Racket here since MIT/GNU Scheme doesn't support ?<! (see SDF_exercises/chapter_5/5_7_common_lib/re/general_re_lib.scm)
;; Although we can do manual-defined ?<! first and then the following groups matching,
;; that is so inconvenient and less modular. (Anyway we can define ?<! pat, but that failed. See SDF_exercises/chapter_5/re_lib/regexp.scm)

(define pat
  ;; Use ([^\\])(\\[^\\])=>$1\\$2 to do replacement from Python raw string.
  ;; 0. Not put unnecessary spaces inside (different from Python re.VERBOSE).
  ;; 1. Notice the order where the larger one is first.
  ;; 2. Racket doesn't support named group.
  #px"\\s*(?:(\\d+)|(\\w+)|((?<!\\w)(?:\\*|\\*\\*)\\w+)|([\\-\\+\\*/%!~<>=&^|?:]+)|([\\(\\)\\[\\]~^!?:,]))"
  )
(define test-book-exp
  "fact := lambda n, a,**kwargs,*args:
  if n == 0
  then 1
  else n*fact(n-1)"
  )

;; https://stackoverflow.com/q/79535844/21294350. Also see SDF_exercises/chapter_5/5_7_re_lib/racket_tests/require_behavior.scm
(require "racket_lib/tests_lib.rkt")

(define (re-findall-groups pat str)
  (regexp-match* pat str #:match-select cdr)
  )

; https://docs.racket-lang.org/reference/streams.html#%28def._%28%28lib._racket%2Fstream..rkt%29._gen~3astream%29%29
(require racket/stream)
(struct list-stream (v)
  #:methods gen:stream
  [(define (stream-empty? stream)
      (empty? (list-stream-v stream)))
    (define (stream-first stream)
      (first (list-stream-v stream)))
    (define (stream-rest stream)
      (list-stream (rest (list-stream-v stream))))])
(define group-size 5)
(define (token type val) (list type val))

; https://stackoverflow.com/a/58684537/21294350
(define (check-for-substring string substring)
  (check-for-substring/list (string->list string) (string->list substring)))

(define (check-for-substring/list loc loc-to-find)
  (cond [(empty? loc-to-find) true]
        [(empty? loc) false]
        [(check-one loc loc-to-find) true]
        [else (check-for-substring/list (rest loc) loc-to-find)]))

(define (check-one loc loc-to-find)
  (cond [(empty? loc-to-find) true]
        [(empty? loc) false]
        [(not (char=? (first loc) (first loc-to-find))) false]
        [else (check-one (rest loc) (rest loc-to-find))]))

(define (string->symbol* str)
  (cond
    ;; Python only supports | https://docs.python.org/3/library/operator.html#mapping-operators-to-functions
    ((equal? "|" str) 'BITWISE-OR)
    ;; others with | are unallowed.
    ((check-for-substring str "|") (error "unrecognized string for string->symbol*"))
    ((equal? "(" str) 'OPEN-PAREN)
    ((equal? ")" str) 'CLOSE-PAREN)
    ((equal? "," str) 'COMMA)
    ;; TODO rest

    (else (string->symbol str))
    )
  )
(define (specific-token idx captured-res)
  (cond
    ;; based on the above re pat.
    ((= idx 0) (token "number" (string->number captured-res)))
    ;; be compatible with SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/pratt_new_compatible_with_MIT_GNU_Scheme.scm
    ((= idx 1) (token "var" (string->symbol captured-res)))
    ((= idx 2) (token "star-arg" (string->symbol captured-res)))
    ((= idx 3) (token "operator" (string->symbol* captured-res)))
    ((= idx 4) (token "len-1-operator" (string->symbol* captured-res)))
    (else (error (list idx "idx is out of range")))
    )
  )
(define (%tokenize groups)
  (let lp ((idx 0))
    (if (= idx group-size)
      (error "wrong group capture result with all #f")
      (begin
        (let ((elm (list-ref groups idx)))
          (if elm
            (specific-token idx elm)
            (lp (+ idx 1))
            ))
        )
      )
    )
  )
;; __next__ can be implemented like SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/pratt_new_compatible_with_MIT_GNU_Scheme.scm
;; does for l in (pl l).
(define (tokenize str)
  (stream-map
    %tokenize
    (list-stream (re-findall-groups pat str))
    )
  )

(re-findall-groups pat test-book-exp)
;; https://stackoverflow.com/a/38712295/21294350
;; cdr corresponds to findall with group. Default car corresponds to findall without group.
(assert-equal
  (re-findall-groups pat test-book-exp)
  '((#f "fact" #f #f #f)
    (#f #f #f ":=" #f)
    (#f "lambda" #f #f #f)
    (#f "n" #f #f #f)
    (#f #f #f #f ",")
    (#f "a" #f #f #f)
    (#f #f #f #f ",")
    (#f #f "**kwargs" #f #f)
    (#f #f #f #f ",")
    (#f #f "*args" #f #f)
    (#f #f #f ":" #f)
    (#f "if" #f #f #f)
    (#f "n" #f #f #f)
    (#f #f #f "==" #f)
    ("0" #f #f #f #f)
    (#f "then" #f #f #f)
    ("1" #f #f #f #f)
    (#f "else" #f #f #f)
    (#f "n" #f #f #f)
    (#f #f #f "*" #f)
    (#f "fact" #f #f #f)
    (#f #f #f #f "(")
    (#f "n" #f #f #f)
    (#f #f #f "-" #f)
    ("1" #f #f #f #f)
    (#f #f #f #f ")"))
  )

(assert-equal
  (stream->list (tokenize test-book-exp))
  '(("var" fact)
    ("operator" :=)
    ("var" lambda)
    ("var" n)
    ("len-1-operator" COMMA)
    ("var" a)
    ("len-1-operator" COMMA)
    ("star-arg" **kwargs)
    ("len-1-operator" COMMA)
    ("star-arg" *args)
    ("operator" :)
    ("var" if)
    ("var" n)
    ("operator" ==)
    ("number" 0)
    ("var" then)
    ("number" 1)
    ("var" else)
    ("var" n)
    ("operator" *)
    ("var" fact)
    ("len-1-operator" OPEN-PAREN)
    ("var" n)
    ("operator" -)
    ("number" 1)
    ("len-1-operator" CLOSE-PAREN))
  )
