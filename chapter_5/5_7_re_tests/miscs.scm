;; regexp-fold
(regexp-fold 'word
              (lambda (i m str acc)
                (newline)
                (let ((start (regexp-match-submatch-start m 0)) (end (regexp-match-submatch-end m 0)))
                  (write-line 
                  (list i str
                    (regexp-match-submatch m 0)
                    start
                    end
                    ;; end is exclusive.
                    (substring str i end)
                    )))
                (let ((s (regexp-match-submatch m 0)))
                  (cond ((assoc s acc)
                        => (lambda (x) (set-cdr! x (+ 1 (cdr x))) acc))
                        (else `((,s . 1) ,@acc)))))
              '()
              "to be or not to be")

;; word test
(define unsplitted-var 
  '(: 
    (? (or "*" "**"))
    (or word (+ numeric))
    ))
(regexp-extract unsplitted-var "b**2-4*a*c")
; TODO why this can't extract 2 when a can be extracted...
(regexp-extract 'word "b**2-4*a*c")
(regexp-extract '(or word (+ numeric)) "b**2-4*a*c")
