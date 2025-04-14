; https://stackoverflow.com/a/42487783/21294350
(define-syntax while
  (syntax-rules ()
    ;; Here the original form is (_ stack) here.
    ((_ pred body ...) 
      (do () 
        ((not pred) (begin)) ; implicit "Unspecified return value"
        body ...
        )
      )
    ))

;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/SC-Transformer-Definition.html
(define-syntax while*
  (sc-macro-transformer
    (lambda (exp env)
      (let ((pred (cadr exp))
            (body (cddr exp)))
        `(call-with-current-continuation
            (lambda (exit)
              (let ((break (lambda () (exit 'finished)))) ; based on lexical scope, it should not be influenced by "env".
                (let f ()
                  (if (not ,(make-syntactic-closure env '() pred))
                    (break))
                  ,@(map (lambda (exp)
                            (make-syntactic-closure env '(break)
                              exp))
                          body)
                  (f))
                )
              ))))))
; (define num 3)(while* #t (write-line num) (set! num (- num 1)) (if (= num 0) (break)))
; 1 ]=> 3
; 2
; 1
; ;Value: ignore
