(define (create-presidents place-name)
  (create-president 'rafael-reif
                    (find-place-name place-name all-places)
                    (random-bias 3)
                    (random-bias 3)))
(define (create-president name home restlessness acquisitiveness)
  (make-president 'name name
                'location home
                'restlessness restlessness
                'acquisitiveness acquisitiveness))
(define president?
  (make-type 'president '()))
(set-predicate<=! president? autonomous-agent?)

(define make-president
  (type-instantiator president?))

