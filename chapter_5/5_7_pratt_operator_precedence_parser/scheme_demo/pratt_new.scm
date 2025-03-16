;; -*-mode:lisp-*-
;;
;; A simple Pratt-Parser for SIOD: 2-FEB-90, George Carrette, GJC@PARADIGM.COM
;; Siod may be obtained by anonymous FTP to FTP.STD.COM
;; Look in the directory pub/gjc
;;
;;                   COPYRIGHT (c) 1990 BY                       
;;     PARADIGM ASSOCIATES INCORPORATED, CAMBRIDGE, MASSACHUSETTS.
;;         See the source file SLIB.C for more information. 
;;
;;
;; Based on a theory of parsing presented in:                       
;;                                                                      
;;  Pratt, Vaughan R., ``Top Down Operator Precedence,''         
;;  ACM Symposium on Principles of Programming Languages         
;;  Boston, MA; October, 1973.                                   
;;                                                                      
;; $Id: pratt.scm,v 1.5 1996/03/01 19:39:36 gjc Exp $
;;

;; The following terms may be useful in deciphering this code:

;; NUD -- NUll left Denotation (op has nothing to its left (prefix))
;; LED -- LEft Denotation      (op has something to left (postfix or infix))

;; LBP -- Left Binding Power  (the stickiness to the left)
;; RBP -- Right Binding Power (the stickiness to the right)
;;
;;

;; Example calls
;;
;; (pl '(f #.OPEN-PAREN a #.CLOSE-PAREN = a + b / c)) => (= (f a) (+ a (/ b c)))
;;
;; (pl '(if g #.OPEN-PAREN a #.COMMA b #.CLOSE-PAREN then a > b else k * c + a * b))
;;  => (if (g a b) (> a b) (+ (* k c) (* a b)))
;;
;; Notes: 
;;
;;   This code must be used with siod.scm loaded, in siod version 2.3
;;
;;   For practical use you will want to write some code to
;;   break up input into tokens.

; (load "siod/siod.scm")

; added
; (defmac (pop form)
;   (list 'let (list (list 'tmp (cadr form)))
; 	(list 'set! (cadr form) '(cdr tmp))
; 	'(car tmp)))
; (define (pop form)
;   (let ((tmp (cadr form)))
;     (set! (cadr form) (cdr tmp))
;     (car tmp))
;   ; (let ((tmp (cadr form)))
;   ;   (set! (cadr form) (cdr tmp))
;   ;   (car tmp))
;   ; (list 'let (list (list 'tmp (cadr form)))
;   ; (list 'set! (cadr form) '(cdr tmp))
;   ; '(car tmp))
;   )

;; borrowed from siod/siod.scm
(define (replace before after)
  (set-car! before (car after))
  (set-cdr! before (cdr after))
  after)

(define (prognify forms)
  (if (null? (cdr forms))
    (car forms)
    (cons 'begin forms)))

(define (defmac-macro form)
  (let ((sname (car (cadr form)))
        (argl (cdr (cadr form)))
        (fname nil)
        (body (prognify (cddr form))))
    (set! fname (symbolconc sname '-macro))
    (list 'begin
          (list 'define (cons fname argl)
                ; (list 'replace (car argl) body)
                body
                )
          (list 'define sname (list 'quote fname)))))

(define defmac 'defmac-macro)

;; 0. either tagged-list https://stackoverflow.com/a/1048403/21294350 https://docs.racket-lang.org/reference/boxes.html
;; or macro https://stackoverflow.com/a/11872479/21294350
;; 1. See SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/siod/siod.scm
;; Similar to push, here (pop stack) will does (let ((tmp stack)) (set! stack (cdr tmp)) (car tmp))) *inline*.
;; So here stack just modifies the input arg in place due to using macro, instead of the local lexical created by the procedure call.
(defmac (pop form)
        (list 'let (list (list 'tmp (cadr form)))
              (list 'set! (cadr form) '(cdr tmp))
              '(car tmp)))

;;; orig

;; These are not used
(define COMMA (intern ","))
(define OPEN-PAREN (intern "("))
(define CLOSE-PAREN (intern ")"))
(define SEMICOLON (intern ";"))
(define QUOTE (intern "'"))

(define (pl l)
  ;; parse a list of tokens
  (set! l (append l '($)))
  (toplevel-parse (lambda (op arg)
                    (cond ((eq? op 'peek)
                           (if l (car l) (eof-val)))
                          ((eq? op 'get)
                           (if l
                             (let ((pop-val (pop l)))
                               (writes nil l "poped elem:" pop-val "\n")
                               pop-val
                               )
                             (eof-val))
                           )))))

(define (token-peek stream)
  (stream 'peek nil))

(define (token-read stream)
  (stream 'get nil))

(define (toplevel-parse stream)
  (if (eq? (eof-val) (token-peek stream))
    (token-read stream)
    (prog1 (parse -1 stream)
      (if (eq? '$ (token-peek stream)) (token-read stream)))))

(define (value-if-symbol x)
  (if (symbol? x)
    (symbol-value x)
    x))

(define (nudcall token stream)
  (if (symbol? token)
    (if (get-syntax token 'nud)
      ;; Compared with defsyntax-macro returning (list 'proc ...) which probably implicitly calls "symbol-value",
      ;; here we explicitly do that.
      ((value-if-symbol (get-syntax token 'nud)) token stream)
      (if (get-syntax token 'led)
        ;; 0. See original paper https://tdop.github.io/, infix is one special prefix.
        ;; 1. See the above, here postfix is not considered.
        (error 'not-a-prefix-operator token)
        ;; TODO why here it prefers choosing led instead of orig token
        token)
      ;; IGNORE How can this work when (if predicate-form true-form false-form)...
      ;; see SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/siod_tests/if_with_more_args.scm
      ; token
      )
    ;; TODO when is used
    token))

(define (ledcall token left stream)
  ((value-if-symbol (or (and (symbol? token)
                             (get-syntax token 'led))
                        ;; Since postfix is not considered, so infix when having left.
                        (error 'not-an-infix-operator token)))
   token
   left
   stream))

(define (lbp token)
  (or (and (symbol? token) (value-if-symbol (get-syntax token 'lbp)))
      ;; TODO Why 200
      200))

(define (rbp token)
  (or (and (symbol? token) (get-syntax token 'rbp))
      200))

(define (parse rbp-level stream)
  (define (parse-loop translation)
    (if (< rbp-level (lbp (token-peek stream)))
      (parse-loop (ledcall (token-read stream) translation stream))
      translation))
  ;; Compared with thegreenplace
  ;; 0. token-read => t = token; token = next()
  ;; 1. left is implicitly in translation arg.
  ;; Anyway same basic ideas.
  (parse-loop (nudcall (token-read stream) stream)))

(define (header token)
  (or (get-syntax token 'header) token))

(define (parse-prefix token stream)
  (list (header token)
        (parse (rbp token) stream)))

(define (parse-infix token left stream)
  (list (header token)
        left
        (parse (rbp token) stream)))

(define (parse-nary token left stream)
  (cons (header token) (cons left (prsnary token stream))))

(define (parse-matchfix token stream)
  (cons (header token)
        (prsmatch (or (get-syntax token 'match) token)
                  (or (get-syntax token 'comma) '#.COMMA)
                  stream)))

(define (parse-matchfix-modified token stream)
  (cons (header token)
        (prsmatch-modified (or (get-syntax token 'match) token)
                           (or (get-syntax token 'comma) '#.COMMA)
                           stream)))

;; No need for comma if ending with "etc." https://www.grammarly.com/blog/commonly-confused-words/et-cetera-etc/ .
;; will make a - b - c => (- a b c) etc.
(define (prsnary token stream)
  (define (loop l)
    (if (eq? token (token-peek stream))
      (begin (token-read stream)
             (loop (cons (parse (rbp token) stream) l)))
      (reverse l)))
  (loop (list (parse (rbp token) stream))))

;; Similar to Python (),[],{} using , as the delimiter.
(define (prsmatch token comma stream)
  (cond ((eq? token (token-peek stream))
         ;; null argument
         (token-read stream)
         nil)
        ('else
         (define (loop l)
           (cond ((eq? token (token-peek stream))
                  (token-read stream)
                  (reverse l))
                 ((eq? comma (token-peek stream))
                  (token-read stream)
                  (loop (cons (parse comma-lbp stream) l)))
                 ('else
                  (error 'comma-or-match-not-found (token-read stream)))))
         (loop (list (parse comma-lbp stream))))))

(define (prsmatch* token stream)
  (cond ((eq? token (token-peek stream))
         ;; null argument
         (error 'empty-parenthesized-expression (token-read stream))
         )
        ('else
         (define (loop elm)
           (cond ((eq? token (token-peek stream))
                  (token-read stream)
                  elm)
                 ('else
                  (error 'match-not-found (token-read stream)))))
         (loop (parse comma-lbp stream)))))

(define (prsmatch-modified token comma stream)
  (cond ((eq? token (token-peek stream))
         (token-read stream)
         nil)
        ('else
         (define (loop l)
           (cond ((eq? token (token-peek stream))
                  (token-read stream)
                  (reverse l))
                 ((eq? comma (token-peek stream))
                  (token-read stream)
                  (cond ((eq? token (token-peek stream))
                         (token-read stream)
                         (reverse l))
                        ('else
                         (loop (cons (parse comma-lbp stream) l)))))
                 ('else
                  (error 'comma-or-match-not-found (token-read stream)))))
         (loop (list (parse comma-lbp stream))))))

(define (delim-err token stream)
  (error 'illegal-use-of-delimiter token))

(define (erb-err token left stream)
  (error 'too-many token))

(define (premterm-err token stream)
  (error 'premature-termination-of-input token))

(define *syntax-table* (cons-array 10))

(define (get-syntax token key)
  ;; See SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/siod_tests/href.scm
  ;; default to return ().
  (href (or (href *syntax-table* key) #(())) token))

(define (set-syntax token key value)
  (hset (or (href *syntax-table* key)
            ;; > The hash table is a *one dimensional* array of association lists.
            (hset *syntax-table* key (cons-array 10)))
        token
        value))

(define (defsyntax-macro form)
  (writes nil "defsyntax-macro (cdr form)=>" (cdr form))
  (list '*defsyntax (list 'quote (cdr form))))
; ;; IMHO the above means
; (*defsyntax (list 'quote (cdr form)))
; ;; So (I use MIT/GNU Scheme syntax here) for defsyntax:
; (*defsyntax `(,args))

(define defsyntax 'defsyntax-macro)
;; IMHO the above means (Wrong. TODO (Maybe Done))
;; 0. Emm... I don't know how to pass $ etc without quote as quote elem's when not using the above macro.
;; 1. Here dot is fine. See mapcar
; (define (defsyntax . args)
;   (defsyntax-macro (cons 'defsyntax args)))
; (define (defsyntax . args)
;   (*defsyntax `(,args)))

(define (*defsyntax input)
  (let ((l (cdr input)))
    (while l
           (set-syntax (car input) (car l) (cadr l))
           (set! l (cddr l)))))

; will call (defsyntax-macro '(defsyntax $ lbp -1 nud premterm-err)) =>
; (*defsyntax (list 'quote ($ lbp -1 nud premterm-err)))
(defsyntax $
           lbp -1
           nud premterm-err)
(writes nil "$ lbp" (get-syntax '$ 'lbp) "\n")

(define comma-lbp 10)
(defsyntax #.COMMA
           lbp comma-lbp
           nud delim-err)

;; lbp order
;; -1<5<10<60<65<70<80<100<120<140<200

(defsyntax #.CLOSE-PAREN
           nud delim-err
           led erb-err
           lbp 5)

(defsyntax #.OPEN-PAREN
           nud open-paren-nud
           led open-paren-led
           lbp 200)

(defsyntax #.SEMICOLON
           lbp -1
           nud delim-err)

(defsyntax {
           header begin
           comma #.SEMICOLON
           match }
           nud parse-matchfix-modified
           lbp 200)

(defsyntax }
           nud delim-err
           led erb-err
           lbp 5)

(defsyntax if
           ;; we can add led as Python allows.
           nud if-nud
           rbp 45)

(defsyntax then
           ;; better with led-error.
           nud delim-err
           lbp 5
           rbp 25)

(defsyntax else
           ;; better with led-error as "then".
           nud delim-err
           lbp 5
           rbp 25)

(defsyntax -
           nud parse-prefix
           led parse-nary
           lbp 100
           rbp 100)

(defsyntax +
           nud parse-prefix
           led parse-nary
           lbp 100
           rbp 100)

(defsyntax *
           led parse-nary
           lbp 120)

(defsyntax =
           ;;; IGNORE IMHO better with define/set! header.
           ;; Which one to choose depends one the program structure...
           ;;; The C = is := here. 
           led parse-infix
           lbp 80
           rbp 80)

(defsyntax **
           lbp 140
           rbp 139
           led parse-infix)

(defsyntax :=
           led parse-infix
           header define
           lbp 80
           rbp 80)


(defsyntax /
           led parse-infix
           lbp 120
           rbp 120)

(defsyntax >
           led parse-infix
           lbp 80
           rbp 80)

(defsyntax <
           led parse-infix
           lbp 80
           rbp 80)

(defsyntax >=
           led parse-infix
           lbp 80
           rbp 80)

(defsyntax <=
           led parse-infix
           lbp 80
           rbp 80)

(defsyntax not
           nud parse-prefix
           lbp 70
           rbp 70)

(defsyntax #.QUOTE
           nud parse-prefix
           header quote)

(defsyntax and
           led parse-nary
           lbp 65)

(defsyntax or
           led parse-nary
           lbp 60)

;; IMHO here it must be "parenthesized expression" in Python.
;; So no #.COMMA at all.
(define (open-paren-nud token stream)
  (cond ((eq? (token-peek stream) '#.CLOSE-PAREN)
         (token-read stream)
         nil)
        ('else
         (writes nil "call open-paren-nud with" token "\n" stream "\n")
         (prsmatch* '#.CLOSE-PAREN stream))))

(define (open-paren-led token left stream)
  (cons (header left) (prsmatch '#.CLOSE-PAREN '#.COMMA stream)))


(define (if-nud token stream)
  (define pred (parse (rbp token) stream))
  (define then (if (eq? (token-peek stream) 'then)
                 (parse (rbp (token-read stream)) stream)
                 (error 'missing-then)))
  (if (eq? (token-peek stream) 'else)
    (list 'if
          pred
          then
          (parse (rbp (token-read stream)) stream))
    (list 'if
          pred
          then)))

; (trace pop token-read)

; Here #.OPEN-PAREN will be printed as (.
(writes nil '(if g #.OPEN-PAREN a #.COMMA b #.CLOSE-PAREN then a > b else k * c + a * b))

;; Trace
;; it will call if-nudcall with token 'if
;; Then g has no nud/led, so return itself
;; Then #.OPEN-PAREN has the larger lbp 200 > if-rbp 45.
;; So it binds g.
;; Then (prsmatch ...) returns (g a b)
;; Then then-lbp 5 < if-rbp 45, so finish for pred.
;; Then (parse then-rbp=25 ...)
;; Similarly >-lbp > then-rbp (binds a), but else-lbp < >-rbp, so binds b.
;; So return (> a b) for then's parse.
;; Similarly (* k c) is returned to parse-loop of else.
;; Again +-lbp 100 > else-rbp 25, so + grabs (* k c).
;; Then (* a b) is got in (loop (list (parse (rbp token) stream))) where we finish at $ (i.e. "yield end_token()" in thegreenplace. This is not said explicitly in the paper.).
;; Then parse-nary returns (+ (* k c) (* a b)) due to no more +.
;; $ has lbp -1, so grabs nothing.
;; So we then call (if (eq? '$ (token-peek stream)) (token-read stream)).

;; So here the main problem is to define rbp/lbp based on precedence. 
;; Then define led (only allowed if it can bind left-argument), nud (only allowed if this can be at the beginning) based on structure.
(pl '(if g #.OPEN-PAREN a #.COMMA b #.CLOSE-PAREN then a > b else k * c + a * b))

;; Trace is similar to the above.
;; #.OPEN-PAREN will grabs f and the latter until #.CLOSE-PAREN.
;; That returns as the result of ledcall.
;; Then = is the new ledcall.
;; =-rbp 80 < +-lbp 100 = +-rbp 100 < /-lbp 120 = /-rbp 120 > $-lbp
;; So + grabs a, / grabs b and c.
;; So (= ... (+ a (/ b c)))
(pl '(f #.OPEN-PAREN a #.CLOSE-PAREN = a + b / c))

(pl '(g #.OPEN-PAREN #.CLOSE-PAREN))

;; tests from SDF_exercises/chapter_5/5_7.scm
(pl '(b ** 2 - 4 * a * c))
(pl '(#.OPEN-PAREN - b + sqrt #.OPEN-PAREN discriminant #.CLOSE-PAREN #.CLOSE-PAREN / #.OPEN-PAREN 2 * a #.CLOSE-PAREN))
; (lambda (n) )
; (pl ''(fact := lambda n : if n = 0 then 1 else n * fact #.OPEN-PAREN n - 1 #.CLOSE-PAREN))
