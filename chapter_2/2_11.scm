;; a,b are already in units.scm.
;; a: > Here's an example of how the table is created
;; > takes two unit expressions, and returns a *converter procedure* that converts data in the first unit to the second unit.
;; b:
;; > For convenience, we will add the following, which are easily derived from unit:* and unit:invert
;; contract: (unit:/ (unit:* a b) b) -> a. Here / b since (unit:* a b) calls a first.

;; c
;; https://extension.psu.edu/conversion-factors-for-english-and-si-metric-units https://en.wikipedia.org/wiki/SI_base_unit#:~:text=The%20units%20and%20their%20physical,the%20candela%20for%20luminous%20intensity. https://en.wikipedia.org/wiki/Mass#Units_of_mass

;; https://github.com/search?q=repo%3Ambillingr%2Fsdf+kilo&type=code mbillingr only has one python implementation.
;; chebert and nbardiuk both don't have such an implementation.
(load "../software/sdf/manager/load")
(manage 'new 'wrappers)
;; length see inch-to-meter.
(define kg-to-tonne
  (let ((tonnes-per-kg 1000))
    (make-unit-conversion (lambda (kgs)
                            (* kgs tonnes-per-kg))
                          (lambda (tonnes)
                            (/ tonnes tonnes-per-kg)))))
(register-unit-conversion 'kg 'tonne kg-to-tonne)
(assert (= 1000 (kg-to-tonne 1)))

;; d
;; velocity (m/s . inch/min). Similar to psi-to-nm2
(define second-to-min
  (let ((mins-per-second (/ 60)))
  ; (let ((mins-per-second 60))
    (make-unit-conversion (lambda (seconds)
                            (* seconds mins-per-second))
                          (lambda (mins)
                            (/ mins mins-per-second)))))
(register-unit-conversion 's 'min second-to-min)

(register-unit-conversion 'mps 'ipm
  (unit:/ (unit:invert inch-to-meter)
    second-to-min))
;; 1/0.0254*60=2362.2047244094488
(define mps-to-ipm (make-converter 'mps 'ipm))
;; parameter from test-units.scm
(load "utils.scm")
(define tolerance 1e-4)
(define (assert-close z1 tolerance z2)
  (assert (close-enuf? z1 z2 tolerance)))
(assert-close (mps-to-ipm 1) tolerance 2362.2047244094488)

;; acceleration similar.
(register-unit-conversion 'mps2 'ipm2
  (unit:/ (unit:invert inch-to-meter)
    (unit:expt second-to-min 2)))
(define mps2-to-ipm2 (make-converter 'mps2 'ipm2))
(assert-close (mps2-to-ipm2 1) tolerance (* 60 2362.2047244094488))

;; e https://www.astera.com/type/blog/data-conversion/
;; Here I won't dig into complex conversion like conversion of File Format / Character Encoding, etc.
;; Here I give "Cleaning the Data" which is used probably frequently.
;; "Error Handling" is also used frequently.
;; > Logging involves documenting *each conversion step*

;; https://stackoverflow.com/a/4181729/21294350 doesn't support appending.
;; https://stackoverflow.com/a/35191561/21294350
(define log-name "2_11.log")
(define (clear-file filename)
  (define log-file (open-output-file filename #f))
  (display "" log-file)
  (close-output-port log-file)
  )
(clear-file log-name)

(define (write-data-list filename . data)
  (define log-file (open-output-file filename #t))
  (for-each 
    (lambda (l)
      ; again, I don't know the actual structure outside of a list
      (display l log-file)
      (newline log-file))
    data)
  (close-output-port log-file)
  )

(define (clean_list lst)
  (if (list? lst)
    ;; https://stackoverflow.com/questions/8382296/scheme-remove-duplicated-numbers-from-list#comment138940848_8382296
    (delete-duplicates lst)
    (write-data-list log-name "Output to log" (string-append "clean_list with wrong arg: " lst))
    ))
(assert (equal? '(1 2 3) (clean_list '(1 2 3 2 1))))
(clean_list "test_clean_list")

;; "Adjusting Date and Time Formats" can be done easily by `split` in python https://docs.python.org/3/library/stdtypes.html.
; (manage 'new 'combinators)
(load "../software/sdf/combinators/function-combinators.scm")
(define hyphen-time-to-slash-time
  ;; Here the interface can be still used.
  (make-unit-conversion (lambda (hyphen-time)
                          (apply (string-joiner 'infix "/") ((make-permutation '(1 2 0)) ((string-splitter 'delimiter #\-) hyphen-time))))
                        (lambda (mins)
                          ;; https://www.ucd.ie/msc/t4media/Multiplying%20Permutations.pdf
                          ;; Here '(2 0 1)*'(1 2 0) should be identity permutation.
                          ;; i.e. permutation (0,1,2)*(0,2,1).
                          ;; TODO I didn't dig into mathematical derivation of the inverse permutation.
                          (apply (string-joiner 'infix "-") ((make-permutation '(2 0 1)) ((string-splitter 'delimiter #\/) hyphen-time))))))
(hyphen-time-to-slash-time "2023-09-21")
(assert (equal? (hyphen-time-to-slash-time "2023-09-21") "09/21/2023"))

;; f
(define unit-conversion-pairs (map car (hash-table->alist unit-conversion-table)))
(displayln unit-conversion-pairs)
;; https://stackoverflow.com/a/7382392/21294350
(define (list-set! list k val)
    (if (zero? k)
        (set-car! list val)
        (list-set! (cdr list) (- k 1) val)))

;; https://cookbook.scheme.org/find-index-of-element-in-list/
(define (list-index fn list)
  (displayln list)
  (let iter ((list list) (index 0))
    (if (null? list)
        -1
        (let ((item (car list)))
          (if (fn item)
              index
              (iter (cdr list) (+ index 1)))))))

(define (adjacency-pairs-to-adjacency-list adjacency-pairs)
  ; (fold 
  ;   (lambda (adjacency-pair res) 
  ;     (let* ((from (car adjacency-pair))
  ;           (to (cdr adjacency-pair))
  ;           (from-idx 
  ;             (list-index 
  ;               (lambda (adjacent-list-elem) (equal? from (car adjacent-list-elem))) 
  ;               res)))
  ;       (if (>= from-idx 0)
  ;         ;; Here is based on no key duplicity in hash table. https://www.quora.com/Can-Hashtable-have-duplicate-keys-in-Java#:~:text=A%20Hashtable%20does%20not%20accept,does%20not%20accept%20duplicate%20keys.
  ;         (begin 
  ;           (displayln from-idx) 
  ;           (list-set! res from-idx (list from (list (cadr (list-ref res from-idx)) to)))
  ;           (displayln res)
  ;           (displayln "ending"))
  ;         (cons (list from to) res))))
  ;   '()
  ;   adjacency-pairs)
  
  (let iter ((rest-adjacency-pairs adjacency-pairs)
              (res '()))
    (if (null? rest-adjacency-pairs)
      res
      (let* ((adjacency-pair (car rest-adjacency-pairs)))
        (let* ((from (car adjacency-pair))
              ;; to be compatible with DFS_demo.scm
              (to (list (cdr adjacency-pair)))
              (from-idx 
                (list-index 
                  (lambda (adjacent-list-elem) (equal? from (car adjacent-list-elem))) 
                  res)))
          (let ((rest-adjacency-pairs (cdr rest-adjacency-pairs)))
            (if (>= from-idx 0)
              (begin 
                (displayln from-idx) 
                (list-set! res from-idx (list from (append (cadr (list-ref res from-idx)) to)))
                (displayln res)
                (displayln "ending")
                (iter rest-adjacency-pairs res))
              (iter rest-adjacency-pairs (cons (list from to) res))))))))
)

(define unit-conversion-key-graph (adjacency-pairs-to-adjacency-list unit-conversion-pairs))
(displayln unit-conversion-key-graph)
;; Here only `fahrenheit (kelvin celsius)` and 2 related have degree greater than 1.
; ((mps ipm) 
;   (mps2 ipm2) 
;   (s min) 
;   (fahrenheit (kelvin celsius)) 
;   (ipm mps) 
;   ((/ pound (expt inch 2)) (/ newton (expt meter 2))) 
;   (min s) 
;   (inch meter) 
;   (kg tonne) 
;   (meter inch) 
;   ((expt inch 3) (expt meter 3)) 
;   (ipm2 mps2) 
;   (pound newton) 
;   ((/ newton (expt meter 2)) (/ pound (expt inch 2))) 
;   (newton pound) 
;   ((expt meter 3) (expt inch 3)) 
;   (kelvin (celsius fahrenheit)) 
;   (tonne kg) 
;   (celsius (kelvin fahrenheit)))

(define (unit-conversion-pair-and-proc-hashtable-to-graph hashtable)
  (adjacency-pairs-to-adjacency-list (map car (hash-table->alist hashtable))))

(define hour-to-min
  (let ((mins-per-hour 60))
    (make-unit-conversion (lambda (hours)
                            (* hours mins-per-hour))
                          (lambda (mins)
                            (/ mins mins-per-hour)))))
(register-unit-conversion 'h 'min hour-to-min)
(define unit-conversion-key-graph (unit-conversion-pair-and-proc-hashtable-to-graph unit-conversion-table))
(displayln unit-conversion-key-graph)

(load "DFS_demo.scm")
(load "2_4_chebert_utils.scm")
(define derived-hour-to-s
  (let ((route (find-route 'h 's unit-conversion-key-graph)))
    (if route
      ;; similar to 2.9
      ;; here route must have length >= 2, so `shifted_route` is always one list.
      (let* ((shifted_route (cdr route))
              (hour-to-s-apply-hook-route (map (lambda (from to) (make-converter from to)) route shifted_route))
              (s-to-hour-apply-hook-route (map (lambda (apply-hook) (unit:invert apply-hook)) hour-to-s-apply-hook-route))
              (hour-to-s (apply compose-multiple hour-to-s-apply-hook-route))
              (s-to-hour (apply compose-multiple s-to-hour-apply-hook-route)))
        (make-unit-conversion hour-to-s s-to-hour))
      (error "unable to derive"))))
(register-unit-conversion 'h 's derived-hour-to-s)
(assert (= 3600 (derived-hour-to-s 1)))
(assert (= 1 ((unit:invert derived-hour-to-s) 3600)))