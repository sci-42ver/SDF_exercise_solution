;; state updater
(define (init-exits mobile-place)
  (for-each
    (lambda (fpde) 
      (set-fpde-bidirectional-exit!
        fpde
        (create-bidirectional-exit (fpde-place fpde) (fpde-inward-direction fpde) mobile-place)))
    (get-possible-fpdes mobile-place)))

;; Trivially, mobile-place like elevator should allow both inward and outward operations..
; (define (add-mobile-place-entrance-exits mobile-place)
;   (for-each
;     (lambda (fpde)
;       ;; notice this uses property-adder -> (lset-adjoin eqv? ...), so no duplicity.
;       (add-exit! (fpde-place fpde) (outside-to-mobile-place (fpde-bidirectional-exit fpde))))
;     (get-mobile-entrances mobile-place)))

(define (add-mobile-place-bidirectional-exit floor-num mobile-place)
  ; (narrate! (list mobile-place "will add exit with current length" (number->string (length (get-exits mobile-place)))) mobile-place)
  ;; if using the wrong paired parentheses, "(#[compiled-procedure ("list" #x5b) #x1c #xe39f1c] #[arity-dispatched-procedure 13] #[compound-procedure 14])" will throw error "The procedure #[arity-dispatched-procedure 13] has been called with 1 argument; it requires at least 2 arguments.".
  (for-each
    (lambda (fpde)
      (let ((outside (fpde-place fpde))
            (bidirectional-exit-pair (fpde-bidirectional-exit fpde))
            )
        (add-exit! outside (outside-to-mobile-place bidirectional-exit-pair))
        (add-exit! mobile-place (mobile-place-to-outside bidirectional-exit-pair))))
    ;; may be nil, then do nothing.
    (floor-fpdes floor-num mobile-place))
  ; (narrate! (list mobile-place "current length:" (number->string (length (get-exits mobile-place)))) mobile-place)
  )

(define remove-exit!
  (property-remover place:exits place? exit?))

(define (remove-mobile-place-bidirectional-exits! floor-num mobile-place)
  ; (narrate! (list mobile-place "will remove exit with current length" (number->string (length (get-exits mobile-place)))) mobile-place)
  (for-each
    (lambda (fpde)
      (let ((outside (fpde-place fpde))
            (bidirectional-exit-pair (fpde-bidirectional-exit fpde))
            )
        (remove-exit! outside (outside-to-mobile-place bidirectional-exit-pair))
        (remove-exit! mobile-place (mobile-place-to-outside bidirectional-exit-pair))))
    (floor-fpdes floor-num mobile-place))
  ; (narrate! (list mobile-place "current length:" (number->string (length (get-exits mobile-place)))) mobile-place)
  )