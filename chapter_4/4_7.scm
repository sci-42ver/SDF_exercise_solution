(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'term-rewriting)

; (load "4_7_description.scm")

;;; What to be done
;; 0. See "To do:" where the book method (straightforward) is similar to 4.6
;;; What to be done in 4.9
;; 1. > introduce distinct contour lines for scoping. But the control structure of our pattern matcher does not make this easy.
;; i.e. outer letrec can't use inner lecrec definitions. See 4.9.
;; 1.a. That is due to we keep passing dictionary arg all around with new-dictionary help, i.e.
;; > This is achieved by threading the dictionary through the depth-first control path
;; 1.b. Also see
;; > in an implicit flat global namespace, with all subsequent downstream appearances being constraining instances.
;; i.e. although depth-first, we don't use stack structure. More specifically something can happen like that leaf can influence each other.
;; 2. > The matcher procedures traverse the pattern and data in left-to-right depth-first order
;; see match:list
;; > binding the first textual appearance of each
;; > distinct pattern variable (like (? x) ) to its corresponding datum
;; > and then treating each subsequent textual appearance in the pattern
;; > as a constraining instance
;; see 4.9 for why this fails for ?:pletrec.

;;; IGNORE: We can use let to create one new dict to store these subpattern definitions.
;; That can be modified to influence latter references (similar to letrec implementation https://web.archive.org/web/20221112234303/http://community.schemewiki.org/?sicp-ex-4.20).
;; Then (?:ref ...) should be one procedure which lookups *later* just like letrec.

;;; IGNORE: We can parse ref in match:pletrec
;; Then ?:ref can be only either isolated or inside list like ?:choice.
;; We do nothing when match:ref? in match:compile-pattern.
;;; The above parse will mess matchers up since we need to traverse through pletrec one more before that done by match:compile-pattern.

;;; 0. > Specifically, just as pattern variables all share a common global namespace, so too can your pattern definitions.
;; So we just add that in the dict with type ?:ref
;; > ?:ref substitutes a defined pattern in place (in order to distinguish such references ...
;; 0.a. Then "(?:ref ...) should be one procedure..."
;; So it does ((match:lookup variable dictionary) data dictionary succeed) where dict use the dict when *application* which implicitly delays.

;; The above dict in let "let to create one new dict" can't be accessed later due to procedure env is based on the env when defined as SICP says.
; (define (test-inner)
;   (write-line outer))
; (let ((outer (list 5)))
;   (test-inner))

;;; Actual implementation
(load "common_lib.scm")
(load "4_6_lib.scm")

(define (match:compile-pattern pattern)
  (cond ((match:ref? pattern)
         ;; added before match:var? to avoid overload
         (match:ref pattern))
        ((match:var? pattern)
         (case (match:var-type pattern)
           ((?) (match:element pattern))
           ((??) (match:segment pattern))
           (else (error "Unknown var type:" pattern))))
        ((match:choice? pattern)
         (match:choice pattern))
        ;; added before general list
        ((match:pletrec? pattern)
         (match:pletrec pattern))
        ((list? pattern)
         (match:list (map match:compile-pattern pattern)))
        (else
          (match:eqv pattern))))

(define match:ref-name cadr)
(define (match:ref? object)
  (and
    (tagged-list? object '?:ref)
    ;; This is safer than https://web.archive.org/web/20221112234303/http://community.schemewiki.org/?sicp-ex-4.20
    (n:= 2 (length object))
    (symbol? (match:ref-name object))
    )
  )
(define (match:ref variable)
  (define (ref-match data dictionary succeed)
    ;; IGNORE: similar to match:choice to allow null list.
    ; (write-line (list "call ref-match with" data dictionary))
    (and (pair? data)
         ;; similar to match:element
         (let ((binding (match:lookup variable dictionary)))
           (if binding
              (begin
                ; (write-line (list "call ref-match of" variable "for data & dict" data dictionary))
                ;; ref is just one medium to call its value. So not change data, e.g. using (car data).
                ((match:binding-value binding) data dictionary succeed))
              (error (list "ref undefined for:" variable))
              ))
         ))
  ref-match)

(define (match:pletrec-bindings object)
  (cadr object))
(define match:pletrec-binding-name car)
(define match:pletrec-binding-val-pattern cadr)

;; see test-pat1 comments.
(define (match:pletrec-body object)
  (let ((body-block (cddr object)))
    ;; effects should be done in let init.
    (assert (n:= 1 (length body-block)))
    (car body-block)
    )
  )
(define (match:pletrec? object)
  (and
    (tagged-list? object '?:pletrec)
    ;; This is safer than https://web.archive.org/web/20221112234303/http://community.schemewiki.org/?sicp-ex-4.20
    (n:= 3 (length object))
    (let ((bindings (match:pletrec-bindings object)))
      (and 
        (list? bindings)
        (every (lambda (obj) (n:= 2 (length obj))) bindings))
      )
    ))

(define (match:extend-dict* bindings dict)
  (match:new-bindings dict
                      (append bindings
                            (match:bindings dict)))
  )
;; similar to match:element
(define match:var-types '(? ?? ?:ref))
(define (make-ref name)
  (list '?:ref name))
(define (match:pletrec-add-bindings-to-dict bindings)
  (let ((bindings-with-proc-vals 
          (map 
            (lambda (elm) 
              (match:make-binding 
                (make-ref (match:pletrec-binding-name elm))
                (match:compile-pattern (match:pletrec-binding-val-pattern elm))
                )) 
            bindings)))
    (lambda (dict)
      (match:extend-dict* bindings-with-proc-vals dict)
      ))
  ; (fold
  ;   (lambda (elm res)
  ;     (match:extend-dict (match:pletrec-binding-name elm)
  ;                       (match:compile-pattern (match:pletrec-val-pattern elm))
  ;                       res)
  ;     )
  ;   dictionary
  ;   bindings
  ;   )
  )

;;; For test-pat1, 
;; here (?:ref odd-even-etc) means (?:choice () (1 (?:ref even-odd-etc))) which then may mean ()
;; So we should just 
;; 0. not wrap (?:ref odd-even-etc) in match:pletrec-body.
;; 1. just pass data intact around in match:ref.

;; Here we do all match:compile-pattern outside the created matcher procedure as Exercise 4.13 says.
(define (match:pletrec pattern)
  (let* ((bindings (match:pletrec-bindings pattern))
         (dict*-proc (match:pletrec-add-bindings-to-dict bindings))
         (body-proc (match:compile-pattern (match:pletrec-body pattern)))
         )
    (define (pletrec-match data dictionary succeed)
      ;; (and (pair? data) ...) etc will be checked in body-proc
      (let ((dict* (dict*-proc dictionary)))
        ; (write-line (list "match:pletrec call with dict" dict*))
        (body-proc data dict* succeed))
      )
    pletrec-match
    ))

; (trace match:ref)
; (trace match:pletrec)
; (trace match:choice)

(define test-pat1
  `(?:pletrec ((odd-even-etc (?:choice () (1 (?:ref even-odd-etc))))
               (even-odd-etc (?:choice () (2 (?:ref odd-even-etc)))))
    ;; Here the mere (?:ref odd-even-etc) matches () etc instead of ((?:ref odd-even-etc)).
    (?:ref odd-even-etc))
  )
; (match:pletrec? test-pat1)
; (match:pletrec-bindings test-pat1)

;; should return dict.
(run-matcher
  (match:compile-pattern test-pat1)
  '()
  match:bindings)
(run-matcher
  (match:compile-pattern test-pat1)
  '(1 (2 (1 (2 (1)))))
  match:bindings)
;; Here we don't match (1 2 1 ...) so we use cons instead of append for (1 (?:ref even-odd-etc)) etc.
(run-matcher
  (match:compile-pattern test-pat1)
  '(1 (2 (1 (2 (1 ())))))
  match:bindings)

;; (?/?? x) in ?:choice are shown in 4.9.
;; Test that here may be inappropriate since all (? x) etc are the *same* thing...
;; Anyway we can show that
(define test-pat2
  `(?:pletrec ((odd-even-etc (?:choice () ((? x) (?:ref even-odd-etc))))
               (even-odd-etc (?:choice () ((? y) (?:ref odd-even-etc)))))
    ;; Here the mere (?:ref odd-even-etc) matches () etc instead of ((?:ref odd-even-etc)).
    (?:ref odd-even-etc))
  )
(run-matcher
  (match:compile-pattern test-pat2)
  '(1 (2 (1 (2 (1 ())))))
  match:bindings)

;; use print-all-matches for ??.
(define test-pat3
  `(?:pletrec ((odd-even-etc (?:choice () ((? x) (?:ref even-odd-etc))))
               (even-odd-etc (?:choice () ((?? y) (?:ref odd-even-etc)))))
    ;; Here the mere (?:ref odd-even-etc) matches () etc instead of ((?:ref odd-even-etc)).
    (?:ref odd-even-etc))
  )
(run-matcher
  (match:compile-pattern test-pat3)
  '(1 (2 (1 (2 (1 ())))))
  print-all-matches)

;; fail
(run-matcher
  (match:compile-pattern test-pat3)
  '(1 (2 (1 (4 (1 ())))))
  print-all-matches)

;; nested
(define test-pat4
  `(?:pletrec ((odd-even-etc* (?:choice () (5 (?:ref even-odd-etc*))))
               (even-odd-etc* (?:choice () (4 (?:ref odd-even-etc*)))))
    (,test-pat3
      (?:ref odd-even-etc*)))
  )
(run-matcher
  (match:compile-pattern test-pat4)
  '((1 (2 (1 (2 (1 ()))))) (5 (4 (5 ()))))
  print-all-matches)

;; Here (?? y) is got from test-pat3, so really messed up due to "flat global namespace".
(define test-pat5
  `(?:pletrec ((odd-even-etc* (?:choice () (5 (?:ref even-odd-etc*))))
               (even-odd-etc* (?:choice () ((?? y) (?:ref odd-even-etc*)))))
    (,test-pat3
      (?:ref odd-even-etc*)))
  )
(run-matcher
  (match:compile-pattern test-pat5)
  '((1 (2 (1 (2 (1 ()))))) (5 (2 (5 ()))))
  print-all-matches)
;; fail
(run-matcher
  (match:compile-pattern test-pat5)
  '((1 (2 (1 (2 (1 ()))))) (5 (4 (5 ()))))
  print-all-matches)
