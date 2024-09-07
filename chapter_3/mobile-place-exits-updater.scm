;; state updater
(define (init-exits mobile-place)
  (for-each
    (lambda (fpde) 
      (set-fpde-bidirectional-exit!
        fpde
        (create-bidirectional-exit (fpde-place fpde) (fpde-inward-direction fpde) mobile-place)))
    (all-possible-dests mobile-place)))

;; Trivially, mobile-place like elevator should allow both inward and outward operations..
; (define (add-mobile-place-entrance-exits mobile-place)
;   (for-each
;     (lambda (fpde)
;       ;; notice this uses property-adder -> (lset-adjoin eqv? ...), so no duplicity.
;       (add-exit! (fpde-place fpde) (outside-to-mobile-place (fpde-bidirectional-exit fpde))))
;     (get-mobile-entrances mobile-place)))

(define (add-mobile-place-bidirectional-exit floor-num mobile-place)
  (for-each
    (lambda (fpde) 
      (let ((outside (fpde-place fpde))
            (bidirectional-exit-pair (fpde-bidirectional-exit fpde))
            )
        (add-exit! outside (outside-to-mobile-place bidirectional-exit-pair))
        (add-exit! mobile-place (mobile-place-to-outside bidirectional-exit-pair)))
    (floor-fpdes floor-num mobile-place))))

(define remove-exit!
  (property-remover place:exits place? exit?))

(define (remove-mobile-place-inward-exits! floor-num mobile-place)
  (for-each
    (lambda (fpde)
      (remove-exit! (fpde-place fpde) (outside-to-mobile-place (fpde-bidirectional-exit fpde))))
    (floor-fpdes floor-num mobile-place)))