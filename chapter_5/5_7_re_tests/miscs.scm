;;; regexp-fold
(regexp-fold 'word
              (lambda (i m str acc)
                (let ((start (regexp-match-submatch-start m 0)) (end (regexp-match-submatch-end m 0)))
                  (newline)
                  (write 
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
              "to    be or not to be")
;; 0. For i,
;; > where i is the index since the last match (beginning with start)
;; so either 0 or the end index of the last match.
; (0 "to    be or not to be" "to" 0 2 "to")
; (2 "to    be or not to be" "be" 6 8 "    be")
; (8 "to    be or not to be" "or" 9 11 " or")
; (11 "to    be or not to be" "not" 12 15 " not")
; (15 "to    be or not to be" "to" 16 18 " to")
; (18 "to    be or not to be" "be" 19 21 " be")

;;; word test
(define unsplitted-var 
  '(: 
    (? (or "*" "**"))
    (or word (+ numeric))
    ))
(regexp-extract unsplitted-var "b**2-4*a*c")
; TODO why this can't extract 2 when a can be extracted...
(regexp-extract 'word "b**2-4*a*c")
(regexp-extract '(or word (+ numeric)) "b**2-4*a*c")
