(cd "~/SICP_SDF/SDF_exercises/common-lib")
(load "logic_lib.scm")
(load "string_lib.scm")

;; This is needed for MIT/GNU Scheme, but not irregex.
;; see (irregex-search '(: bow (+ (& (or alphanumeric "_") (or any)))) "3") and (irregex-search 'word "3").
;; which is same as `perl -e 'print "3"=~/\b(?:\d|\w)+\b/'`.
;; The problem here for MIT/GNU Scheme is bow is a bit wrong.
(define word-corrected '(or word (+ (or alphanumeric "_"))))

; (cd "~/SICP_SDF/SDF_exercises/chapter_5/MIT_GNU_Scheme_re_lib")
; (load "regexp.sld")
(define (regexp-extract* re string #!optional start end)
  (let ((last-match-end 0))
    (regexp-fold re
      ;; See SDF_exercises/chapter_5/5_7_re_tests/miscs.scm
      ;; IMHO it is better to not use `i` due to its value depends on the context.
      (lambda (i m str acc)
        (let ((sub-string-before (substring* str last-match-end (regexp-match-submatch-start m 0))))
          (set! last-match-end (regexp-match-submatch-end m 0))
          (append acc 
            (if sub-string-before
              (list sub-string-before m)
              (list m))
            )
          )
        )
      '()
      string
      (lambda (i m str acc)
        (let ((end-str (substring* str last-match-end)))
          (if end-str 
            (append acc (list end-str))
            acc)
          )
        )
      (or* start 0)
      (or* end (string-length string))
      ))
  )

(define (regexp-extract** re string match-transformer str-transformer #!optional start end)
  (define (assert-list-and-return-obj obj)
    (assert (list? obj))
    obj
    )
  (append-map
    (lambda (elm)
      (cond 
        ((regexp-match? elm) 
          (assert-list-and-return-obj (match-transformer elm))
          )
        ((string? elm) (assert-list-and-return-obj (str-transformer elm)))
        (else (error (list "unrecognized elm" elm)))
        )
      )
    (regexp-extract* 
      re
      string 
      (or* start 0)
      (or* end (string-length string))))
  )

;; Use ~~stream~~ iterator for space efficiency https://stackoverflow.com/a/32314899/21294350 (same https://discuss.python.org/t/what-are-the-advantage-of-using-iter-over-list/23873/4)
;; and time efficiency https://stackoverflow.com/a/631619/21294350
;; > but the smart allocator saves some of that time when it can reuse recently discarded objects.

;; See lecs/6.001_fall_2007_recitation/codes/rec20/coroutine/demo-implementation.scm
(cd "~/SICP_SDF/lecs/6.001_fall_2007_recitation/codes/rec20/coroutine/")
(load "demo-implementation.scm")
(define (regexp-finditer re string #!optional start end)
  (coroutine
    (lambda (yield)
      (regexp-fold 
        re
        (lambda (i m str acc)
          (yield m)
          'acc-unused
          )
        'acc-unused
        string
        (lambda (i m str acc) acc)
        (or* start 0)
        (or* end (string-length string))
        )
      )
    )
  )
(define test-iter (regexp-finditer 'numeric "123"))
; (regexp-match->list (test-iter 'next))
; ;Value: ("1")
; (regexp-match->list (test-iter 'next))
; ;Value: ("2")
; (regexp-match->list (test-iter 'next))
; ;Value: ("3")

;;; IGNORE Emm... It is a bit overkill to parse regex pat and translate it...
;; First we need to match parenthesis (see SDF_exercises/chapter_5/5_7_naive_algorithm_for_operator_precedence_parser/5_7_parenthesis_lib.scm)
;; And then manipulate inside.
;;; This is just like writing one new parser...
;;; IMHO it is better to use one hand-written RD parser instead of the above one based on my 5_7_naive_algorithm_for_operator_precedence_parser...
;; The reasons have been said in SDF_exercises/chapter_5/5_7_naive_algorithm_for_operator_precedence_parser/README.md.
; (cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_naive_algorithm_for_operator_precedence_parser/")
; (load "5_7_parenthesis_lib.scm")
; (define (python-pat->scheme-pat str)
;   (let (variables)
;     ())
;   (let lp ()
;     ())
;   )

;; Same behavior as Python
(define (match-lastgroup match field-names)
  ; (assert (regexp-match? match))
  (if (regexp-match? match)
    'expected
    (error (list "unexpected" match)))
  (last
    (filter
      (lambda (field)
        (let ((res (regexp-match-submatch match field)))
          (and res
            (symbol->string field)
            ))
        )
      field-names
      ))
  )
