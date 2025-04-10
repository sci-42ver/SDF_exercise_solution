(cd "~/SICP_SDF/SDF_exercises/common-lib")
(load "logic_lib.scm")
(load "string_lib.scm")
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
