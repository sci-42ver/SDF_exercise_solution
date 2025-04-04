(define-syntax pop
  ;; 0. ellipsis usage https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-6.html#TAG:__tex2page_sec_4.3.2
  ;; > P is of the form (P1 … Pk Pe <ellipsis> Pm+1 … Pn) ...
  ;; 1. No need for literal since pop doesn't use any special characters like =>/else in cond etc.
  (syntax-rules ()
    ;; Here the original form is (_ stack) here.
    ((_ stack) 
      ;; prognify is implicitly done in template.
      (let ((tmp stack))
        ;; IMHO this is also ok.
        ; (set! stack (cdr stack))
        (set! stack (cdr tmp))
        (car tmp)
        )
      )))

(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/")
(load "compatible_lib.scm")

;;; orig
; (define COMMA (intern ","))
(define OPEN-PAREN (intern "("))
(define CLOSE-PAREN (intern ")"))
(define SEMICOLON (intern ";"))
; Emm... In Scheme QUOTE is thought as quote...
(define QUOTE-SYMBOL (intern "'"))
(define LEFT-BRACE (intern "{"))
(define RIGHT-BRACE (intern "}"))

(define (pl l)
  ;; parse a list of tokens
  (set! l (append l '($)))
  (toplevel-parse (lambda (op arg)
                    (cond ((eq? op 'peek)
                           (if l (car l) (eof-val)))
                          ((eq? op 'get)
                           (if l
                             (let ((pop-val (pop l)))
                              ;  (writes nil l "poped elem:" pop-val "\n")
                               pop-val
                               )
                             (eof-val))
                           )))))

(define (token-peek stream)
  (stream 'peek nil))

(define (token-read stream)
  (stream 'get nil))

(define (toplevel-parse stream)
  (if 
    ; (eq? (eof-val) (token-peek stream))
    (eof-object? (token-peek stream))
    (token-read stream)
    ;; The start should bind nothing.
    (prog1 (parse end-lbp stream)
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
                  (or (get-syntax token 'comma) 'COMMA)
                  stream)))

(define (parse-matchfix-modified token stream)
  (cons (header token)
        (prsmatch-modified (or (get-syntax token 'match) token)
                           (or (get-syntax token 'comma) 'COMMA)
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
(define uninterned-delimeters '(: COMMA))
(define (value-if-symbol* token)
  (if (any (lambda (sym) (eq? sym token)) uninterned-delimeters)
    token
    (value-if-symbol token))
  )
(define (prsmatch token comma stream)
  ;; 0. added due to using interned symbol value.
  ;; 1. Better to use let, I use set! here for simplicity.
  (set! token (value-if-symbol* token))
  (set! comma (value-if-symbol* comma))
  (define (loop l)
    (cond ((eq? token (token-peek stream))
          (token-read stream)
          (reverse l))
          ((eq? comma (token-peek stream))
          (token-read stream)
          ;; the reason is similar to the following.
          (loop (cons (parse (lbp comma) stream) l)))
          (else
          (error 'comma-or-match-not-found (token-read stream)))))
  (cond ((eq? token (token-peek stream))
         ;; null argument
         (token-read stream)
         nil)
        (else
         ;; to stop when encountering one comma.
         (loop (list (parse (lbp comma) stream))))))

(define (prsmatch* token comma stream)
  (set! token (value-if-symbol* token))
  (set! comma (value-if-symbol* comma))
  (define (loop l)
    (cond ((eq? token (token-peek stream))
          (token-read stream)
          (let* ((l* (reverse l))
                  (len (length l*)))
            (cond
              ;; > if the list contains at least one comma, it yields a tuple; 
              ;; > otherwise, it yields the single expression that makes up the expression list.
              ((= len 1) (car l*))
              ((> len 1) (cons 'tuple l*))
              (else
                ;; just for safety here.
                ;; IMHO `(list (parse comma-lbp stream))` implies >= 1.
                (error 'this-should-not-happen l))
              )
            ))
          ((eq? comma (token-peek stream))
          (token-read stream)
          (loop (cons (parse (lbp comma) stream) l)))
          (else
          (error 'comma-or-match-not-found (token-read stream)))))
  ;; Based on https://docs.python.org/3/reference/expressions.html#parenthesized-forms
  (cond ((eq? token (token-peek stream))
         ;; null argument
         (token-read stream)
         ; > An empty pair of parentheses yields an empty tuple object.
         (cons 'tuple nil))
        (else
         (loop (list (parse (lbp comma) stream))))))

;; This doesn't consider tuple.
; (define (prsmatch* token stream)
;   (cond ((eq? token (token-peek stream))
;          ;; null argument
;          (error 'empty-parenthesized-expression (token-read stream))
;          )
;         (else
;          (define (loop lst)
;            (cond ((eq? token (token-peek stream))
;                   (token-read stream)
;                   lst)
;                  (else
;                   (error 'match-not-found (token-read stream)))))
;          (loop (list (parse comma-lbp stream))))))

(define (prsmatch-modified token comma stream)
  (set! token (value-if-symbol* token))
  (set! comma (value-if-symbol* comma))
  (define (loop l)
    (cond ((eq? token (token-peek stream))
          (token-read stream)
          (reverse l))
          ((eq? comma (token-peek stream))
          (token-read stream)
          ;; Difference from the above, allowing {stmt;}
          (cond ((eq? token (token-peek stream))
                  (token-read stream)
                  (reverse l))
                (else
                  (loop (cons (parse comma-lbp stream) l)))))
          (else
          (error 'comma-or-match-not-found (token-read stream)))))
  (cond ((eq? token (token-peek stream))
         (token-read stream)
         nil)
        (else
         (loop (list (parse comma-lbp stream))))))

(define (delim-err token stream)
  (error 'illegal-use-of-delimiter token))

(define (erb-err token left stream)
  (error 'too-many token))

(define (premterm-err token stream)
  (error 'premature-termination-of-input token))

; 0. eq? is fine for comparing intern symbols https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Symbols.html#index-intern
; 1. For simplicity, I use make-strong-eq-hash-table here. See replacement in SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/compatible.sed
(define *syntax-table* (make-strong-eq-hash-table 10))

(define (get-syntax token key)
  ;; See SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/siod_tests/href.scm
  ;; default to return nothing which is thought as #f.
  (href (or (href *syntax-table* key) empty-hash-table) token))

(define (set-syntax token key value)
  (hset (or (href *syntax-table* key)
            ;; > The hash table is a *one dimensional* array of association lists.
            ;; see SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/siod_tests/hset.scm
            ;; Here it returns the value.
            (hset *syntax-table* key (make-strong-eq-hash-table 10)))
        token
        value))

(define (*defsyntax input)
  (write-line (list "call *defsyntax with" input))
  (let ((l (cdr input)))
    ;; modified since Scheme doesn't consider nil as #f.
    (while (not (null? l))
           (set-syntax (car input) (car l) (cadr l))
           (set! l (cddr l)))))

; (for-each (lambda (proc) (trace proc)) (list set-syntax get-syntax))

; will call (defsyntax-macro '(defsyntax $ lbp -1 nud premterm-err)) =>
; (*defsyntax (list 'quote ($ lbp -1 nud premterm-err)))
(define end-lbp -1)
; (define start-rbp end-lbp)
(defsyntax $
           lbp end-lbp
           nud premterm-err)
(writes nil "$ lbp" (get-syntax '$ 'lbp) "\n")

(define comma-lbp 10)
(defsyntax COMMA
           lbp comma-lbp
          ;  rbp comma-lbp
           nud delim-err)

;; lbp order
;; -1<5[),},then,else]<10[',',]<60[or]<65[and]<70<80<100<120<140<200

;; rbp order
;; IMHO since here 45 and 25 are greater than the same group of lbp's, it is fine to define if-rbp as 25.
;; 25[then, else]<45[if]<70<80<100<120<139

;;; Notice then-rbp > ,-lbp doesn't imply then can grab something from ",". See "(eq? (token-peek stream) 'else)".
;; Similarly "if a ," (similar for ')' etc) is not allowed by if-nud, so if won't grab a.
;; So it is better to use nud/led procedure for ensurance of operand-operator relation
;; instead of just using rbp/lbp to ensure that.

(defsyntax CLOSE-PAREN
           nud delim-err
           led erb-err
           lbp 5)

(defsyntax OPEN-PAREN
           nud open-paren-nud
           led open-paren-led
           lbp 200)

(defsyntax SEMICOLON
           lbp -1
           nud delim-err)

(defsyntax LEFT-BRACE
           header begin
           comma SEMICOLON
           match RIGHT-BRACE
           nud parse-matchfix-modified
           lbp 200)

(defsyntax RIGHT-BRACE
           nud delim-err
           led erb-err
           lbp 5)

;; 
; (defsyntax if
;            ;; we can add led as Python allows.
;            nud if-nud
;            rbp 45)

;; workaround for the error of (quasiquote ((unquote if))).
(defsyntax if*
           ;; we can add led as Python allows.
           nud if-nud
           rbp 45)

(defsyntax then
           ;; better with led-error.
           nud delim-err
           lbp 5
           rbp 25)

(defsyntax else*
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
           nud identity
           lbp 120
           ;; Must define rbp < (-lbp=200 here to avoid using default 200 which may grab proc-name unexpectedly.
           rbp 120
           )

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
           nud identity
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

(defsyntax QUOTE-SYMBOL
           nud parse-prefix
           header quote)

(defsyntax and*
           led parse-nary
           lbp 65)

(defsyntax or*
           led parse-nary
           lbp 60)

(defsyntax lambda*
           nud lambda-nud
           led delim-err
           ;; no led, so no lbp.
          ;  lbp 60
           ;; See the above "So it is better to use nud/led procedure ..."
           ;; All related objects with lower lbp can be avoided by nud procedure.
           rbp 45
           )

;; 0. similar to then
;; 1. lbp is less then all possible rbp's, IGNORE LATER but greater than comma-lbp.
;; so it binds nothing IGNORE LATER except when competing with $.
;; 2. rbp may be used by the related nud.
;; But related unexpected behaviors are excluded by that nud procedure
;; instead of rbp/lbp ordering (see "Notice then-rbp ...").
(defsyntax :
           led delim-err
           nud delim-err
           lbp 5
           rbp 25)

(define (identity token stream)
  token)

(define (lambda-nud token stream)
  ;; 0. token must be lambda implied by nudcall.
  ;; 1. Here a,b,: is allowed as Python (see SDF_exercises/chapter_5/5_7_related_python_behavior/parameter_list.py).
  (define params (prsmatch-modified ': 'COMMA stream))
  ;; 0. To allow multiple statements inside the body
  ;; I use {stmt1; stmt2; stmt3; ...} similar to perl 
  ;; (not use Python because it uses NEWLINE etc as the delimeter https://docs.python.org/3/reference/compound_stmts.html)
  ;; but here I enforce "foreach ... {}" etc to also end with ";".
  ;; Actually, this is already done in SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/pratt_new_orig.scm.
  ;; 0.a. For {stmt1; stmt2; stmt3; ...}
  ;; Similar to code base lambda-body, body is just one begin.
  ;; 0.b. So body {a; b} is different from "a; b" where the latter only grabs "a" as the body.
  ;; 0.c. Notice in Python, lambda doesn't support that "multiple statements"
  ;; https://stackoverflow.com/questions/28429680/lambda-and-multiple-statements-in-python/28429947#comment140227809_28429680
  ;; 1. (rbp ':) is needed when rhs is "a or b" etc.
  (define body (parse (rbp ':) stream))
  (list 'lambda params body)
  )

;; IMHO here it must be "parenthesized expression" in Python.
;; So no COMMA at all.
(define (open-paren-nud token stream)
  ;; The 1st is already done in prsmatch*.
  ; (cond ((eq? (token-peek stream) 'CLOSE-PAREN)
  ;        (token-read stream)
  ;        nil)
  ;       (else
  ;        ))
  (writes nil "call open-paren-nud with" token "\n" stream "\n")
  (prsmatch* 'CLOSE-PAREN 'COMMA stream)
  )

(define (open-paren-led token left stream)
  (cons (header left) (prsmatch 'CLOSE-PAREN 'COMMA stream)))


(define (if-nud token stream)
  (define pred (parse (rbp token) stream))
  (define then (if (eq? (token-peek stream) 'then)
                 (parse (rbp (token-read stream)) stream)
                 (error 'missing-then)))
  (if (eq? (token-peek stream) 'else*)
    (list 'if
          pred
          then
          (parse (rbp (token-read stream)) stream))
    (list 'if
          pred
          then)))

; (trace pop token-read)
; (for-each (lambda (proc) (trace proc)) (list value-if-symbol get-syntax href hset))

(define (pl-assert expected exp)
  (assert (equal? expected (pl exp))))

(pl-assert 
  '(if (g a b) (> a b) (+ (* k c) (* a b)))
  `(if* g ,OPEN-PAREN a comma b ,CLOSE-PAREN then a > b else* k * c + a * b))

(pl-assert 
  '(= (f a) (+ a (/ b c))) 
  `(f ,OPEN-PAREN a ,CLOSE-PAREN = a + b / c))

(pl-assert 
  '(g) 
  `(g ,OPEN-PAREN ,CLOSE-PAREN))

;; tests from SDF_exercises/chapter_5/5_7.scm
(pl-assert 
  '(- (** b 2) (* 4 a c)) 
  `(b ** 2 - 4 * a * c))
(pl-assert 
  '(/ (+ (- b) (sqrt discriminant)) (* 2 a))
  `(,OPEN-PAREN - b + sqrt ,OPEN-PAREN discriminant ,CLOSE-PAREN ,CLOSE-PAREN / ,OPEN-PAREN 2 * a ,CLOSE-PAREN))

;; tuple
(pl-assert 
  '(+ 3 2)
  `(,OPEN-PAREN 3 ,CLOSE-PAREN + ,OPEN-PAREN 2 ,CLOSE-PAREN))
(pl-assert 
  '(+ (tuple) 2)
  `(,OPEN-PAREN ,CLOSE-PAREN + ,OPEN-PAREN 2 ,CLOSE-PAREN))
(pl-assert 
  '(+ (tuple 1 3 7) 2)
  `(,OPEN-PAREN 1 COMMA 3 COMMA 7 ,CLOSE-PAREN + ,OPEN-PAREN 2 ,CLOSE-PAREN))

(pl-assert 
  '(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))
  `(fact := lambda* n : if* n = 0 then 1 else* n * fact ,OPEN-PAREN n - 1 ,CLOSE-PAREN))
(pl-assert 
  '(define fact (lambda (a (= b 0) / c *args * kwarg1 **kwargs) (if (= n 0) 1 (* n (fact (- n 1))))))
  `(fact := lambda* a COMMA b = 0 COMMA / COMMA c COMMA *args COMMA * COMMA kwarg1 COMMA **kwargs : if* n = 0 then 1 else* n * fact ,OPEN-PAREN n - 1 ,CLOSE-PAREN))
