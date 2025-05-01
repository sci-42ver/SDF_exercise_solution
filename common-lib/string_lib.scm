;; drop ""
(define (substring* str #!optional start end)
  (let* ((start (or* start 0))
         (end (or* end (string-length str)))
         (res (substring str start end)))
    (and
      (not (equal? "" res))
      res)
    )
  )

;; less general than string-joiner https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Strings.html#index-string_002djoiner
(cd "~/SICP_SDF/SDF_exercises/common-lib")
(load "logic_lib.scm")
(define (string-join string-list #!optional delimiter grammar)
  (and* grammar (assert (symbol? grammar)))
  ((string-joiner*
    ;; same as (string-join '("foo" "bar" "baz")) in https://srfi.schemers.org/srfi-140/srfi-140.html
    (or* grammar 'infix)
    (or* delimiter " ")
    )
    string-list
    )
  )

(define (string->char str)
  (assert (and (string? str) (= 1 (string-length str))))
  (car (char-set->list (string->char-set str)))
  )
(define (string-replace* string char-str1 char-str2)
  (string-replace string (string->char char-str1) (string->char char-str2))
  )

(define (->str obj)
  (cond 
    ((symbol? obj) (symbol->string obj))
    ((string? obj) obj)
    (else (error (list "->str can't recognize" obj))))
  )
