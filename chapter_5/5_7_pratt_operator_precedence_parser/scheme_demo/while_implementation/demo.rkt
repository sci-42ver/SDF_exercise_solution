#lang racket
(define-syntax while*
  ;; https://www.gnu.org/software/guile/manual/html_node/Syntax-Case.html#index-with_002dsyntax
  ;; https://stackoverflow.com/q/79195343/21294350
  ;; Both sc-macro-transformer and syntax-case can implementation aif.
  (lambda (x)
    (syntax-case x ()
      ((_ pred body ...)
        ;; 0. automatic hygiene expect for the explicit exit here.
        ;; 1. > The issue is that with-syntax creates a separation between the point of definition of a value and its point of substitution. 
        ;; TODO IMHO it is more like connection since (it test) definition can be used in "then" eyc.
        ;; 2. Here substitution for break is done just like the normal pattern inside the syntax object.
        ;; > Bind patterns pat from their corresponding values val
        (with-syntax ((break (datum->syntax x 'break)))
         #'(call-with-current-continuation
            (lambda (exit)
              (let ((break (lambda () (exit 'finished)))) ; based on lexical scope, it should not be influenced by "env".
                (let f ()
                  (if (not pred)
                    (break)
                    'continue
                    )
                  body ...
                  (f))
                )
              )))
        )
      )
    )
  )
(define num 3)(while* #t (displayln num) (set! num (- num 1)) (if (= num 0) (break) 'continue))

;; here (#:when #t) doesn't mean (while* #t ...).
(set! num 3)(for (#:when #t) (displayln num) (set! num (- num 1)) #:break (= num 0) 'ignore)
; 3
(set! num 3)(for ([i '(1 2 3 4 5)] #:when #t) (displayln num) (set! num (- num 1)) #:break (= num 0) 'ignore)
; 3
; 2
; 1
