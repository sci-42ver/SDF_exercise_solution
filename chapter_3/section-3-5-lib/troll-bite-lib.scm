(define (create-trolls places)
  (displayln "call new create-trolls")
  (map (lambda (name)
         (create-troll name
                       (random-choice places)
                       (random-bias 3)
                       0 ; Only change. To accelerate eat-people!.
                       ))
       '(grendel registrar)))

;; notice to call `(define failure-to-survive #t)` before.
(define (retry-until-survive pred my-name what-to-do)
  (do 
    ; ((failure-to-survive #t)) ; will have ";Unbound variable: failure-to-survive" if no define.
    ()
    ((not failure-to-survive) 'done)
    (restart-game-until-pred pred my-name what-to-do)))

;; what-to-do-when-troll-bite must not have failure-to-survive passed in since that is not the global value.
;; So it should be something like `(what-to-do-when-troll-bite person)`.
(define (wait-for-troll-bite person what-to-do-when-troll-bite)
  (tell-health person)
  (do ((i 0 (n:+ i 1))
       (max-hang-out 10))
    ((or (n:= i max-hang-out) (n:< (get-health person) *max-health*))
     (if (n:< (get-health person) *max-health*)
       (begin
         (displayln "got eaten")
         (tell-health person)
         (if (n:> (get-health person) 0)
           (what-to-do-when-troll-bite person)
           (set! failure-to-survive #t))))
     )
    (displayln "wait to be eaten")
    (hang-out 1))
  )
