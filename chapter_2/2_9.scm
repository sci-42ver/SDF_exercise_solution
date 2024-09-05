(load "../software/sdf/manager/load")
(manage 'new 'regular-expressions)
(load "../software/sdf/common/testing.scm")
(load "../software/sdf/regular-expressions/test-regexp.scm")
(load "../common-lib/utils.scm")
(load "regex-lib/regex_utils.scm")

;; (Done) TODO `(load "2_8.scm")` seems to use one env by "../software/sdf/manager/load" and can't pass `r:seq` definition.
;; See 3_7.scm
(load "regex-lib/regex_grouping_utils.scm")
; (define (r:seq . exprs)
;   (apply string-append exprs))

;; just the original r:seq
;; no solution from mbillingr and nbardiuk.
;; See chebert: we better use r:seq abstraction 
(define (r:group . exprs)
  (r:seq "\\(" (apply string-append exprs) "\\)"))

(define test_str_group (r:group (r:quote "a")
                                (r:dot)
                                (r:quote "c")))

(assert (equal? test_str_group
                "\\(a.c\\)"))

;; Here I implement mcs Problem 7.20. similar to https://www.geeksforgeeks.org/check-for-balanced-parentheses-in-python/ where poping one empty stack will throw errors.
(define (balanced_parentheses? expr)
  (= (fold (lambda (elem res) 
             (if (< res 0)
               res
               ;; Here I assume the user correctly escapes ( and ).
               (cond ((equal? elem #\() (+ res 1))
                     ((equal? elem #\)) (- res 1))
                     (else res))))
           0
           (string->list expr)) 0))

;; Assume to_escape is one character.
(define (check_escape? to_escape expr)
  ;; 1. follow the idea in exercise 2.5
  ;; > We can map over adjacent pairs using the original list and the shifted list.
  ;; 2. We can also use list-ref although it is heavy to call that for each element. https://stackoverflow.com/a/40365420/21294350 
  (let* ((lst (string->list expr))
         (shifted_lst (cdr lst)))
    (if (not (null? shifted_lst))
      ;; Here step is 1 instead of 2 but with the same \Theta(n) iterations to terminate.
      (fold (lambda (elem_1 elem_2 res)
              ; (displayln "iter:")
              ; (displayln elem_1)
              ; (displayln elem_2)
              ; (displayln res)
              (if (equal? elem_2 to_escape)
                (and res (equal? elem_1 #\\))
                res))
            #t
            lst
            shifted_lst)
      (not (equal? (car lst) to_escape))))
  )

(assert (not (check_escape? #\( "(")))
(assert (check_escape? #\( test_str_group))
(assert (check_escape? #\) test_str_group))

;; https://pubs.opengroup.org/onlinepubs/9699919799/nframe.html 9.3.6
;; Needed features: 
;; > The character 'n' shall be a digit from 1 through 9
;; > The expression is invalid if less than n subexpressions precede the '\n'

;; This is robuster than chebert although not considering bre and ere. Also for .
;; nbardiuk and mbillingr don't implement this.
(define (r:back-references idx . exprs)
  (assert (and (<= 1 idx) (<= idx 9)))
  (let ((expr_str (apply r:seq exprs)))
    (assert (check_escape? #\( expr_str))
    (assert (check_escape? #\) expr_str))
    (assert (balanced_parentheses? expr_str))
    (let ((left_parentheses_cnt (length (string-search-all "\\(" expr_str)))
          (right_parentheses_cnt (length (string-search-all "\\)" expr_str)))
          )
      (assert (<= idx left_parentheses_cnt))
      (string-append expr_str "\\" (number->string idx)))))

(define test_str_br (r:seq (r:group 
                             (r:group 
                               (r:quote "a")
                               (r:dot)
                               (r:quote "c"))
                             (r:quote "d")) 
                           (r:quote "e")))
(r:back-references 2 test_str_br)

(r:back-references 10 "")
(r:back-references 3 test_str_br)
(r:back-references 2 "\\(\\)a\\)")
