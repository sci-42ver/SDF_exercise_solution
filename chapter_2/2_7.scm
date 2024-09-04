;; a. Louis Reasoner: This is wrong since at last it will still call `( r:alt expr "")`.
;; b. Alyssa P. Hacker: really not elegant when the range is large.
;; Eva Lu Ator: Fine although not elegant when the range is *very very* large.
;; The above is about data.
;; For code, IMHO they are similar. Alyssa needs recursive process to output each xxx... while Eva can call (r:repeat min min x) first then concatenate with (r:repeat 0 (- max min) x?) which is again recursive.
;; We can also say the latter has better abstraction.
;; c. 9.4.6.5 is almost same as 9.3.6.5. This is most elegant without recursive.
;; |,? are not supported by BRE.
;; trivially smaller size and see above "... without recursive"

;; See 6.945_assignment_solution:
;; a. The above a is wrong.
;; b. > The code would also be easier to implement since Bonnie is proposing a one-line replacement
;; maybe it assume `string-repeat` (not in MIT-Scheme as MIT_Scheme_Reference shows) is ok.
;; c. > This *may* make the implementation more robust / portable.
;; > making the compiled expressions easier to read

;; d
(load "../software/sdf/manager/load")
(manage 'new 'regular-expressions)
(load "../software/sdf/common/testing.scm")
(load "../software/sdf/regular-expressions/test-regexp.scm")
(load "../common-lib/utils.scm")
(load "regex-lib/regex_utils.scm")

(define (r:repeat min max expr)
  (apply r:seq
         ;; 1. same as nbardiuk but nbardiuk doesn't capture group in r:seq.
         ;; `(and max (= min max)` gives one small optimization when max is #f.
         ;; 2. mbillingr didn't implement this.
         (append (list expr 
                       "\\{" ; Caveat: ... 
                       (number->string min))
                 (append 
                   (if (= max min)
                     (list "")
                     (list ","
                           (if (not max) ; By `((lambda (max) (if max 1 0)) #f)`, here max will just be the arg instead of the related procedure in `(if max 1 0)`.
                             ""
                             (number->string max))))
                   (list "\\}")
                   ))))

(define test_str (r:alt (r:quote "cat") (r:quote "dog")))
(displayln (r:repeat 3 5 test_str))

(assert (equal? '("[09]. catdogcat" "[10]. catcatdogdog" "[11]. dogdogcatdogdog" "[12]. catcatcatdogdogdog" "[13]. acatdogdogcats" "[14]. ifacatdogdogs" "[15]. acatdogdogsme")
                (r:grep (r:repeat 3 5 test_str) tests-file)))
