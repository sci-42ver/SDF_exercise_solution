;; just use and which is different from exercise_codes/SICP/book-codes/ch4-mceval.scm
(define (tagged-list? exp tag)
  ;; similar to match:dict?
  (and (pair? exp)
    (eq? (car exp) tag)))
