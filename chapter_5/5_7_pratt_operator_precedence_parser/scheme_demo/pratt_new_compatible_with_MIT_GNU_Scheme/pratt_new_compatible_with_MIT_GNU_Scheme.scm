;; Port from SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/orig/pratt_new.scm
;; but with some correction like open-paren-nud allowing the trailing comma to construct one tuple.
(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/common_lib/")
(load "base_lib.scm")
(load "common_lib_for_MIT_GNU_Scheme.scm")
(cd "~/SICP_SDF/SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/")
(load "pratt_new_compatible_with_MIT_GNU_Scheme/compatible_lib.scm")

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
      ;; IGNORE TODO Why 200 which is the greatest used number here.
      ;; This is wrong. see the original "and" implementation.
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

;; 0. will make a - b - c => (- a b c) etc (No need for comma if ending with "etc." https://www.grammarly.com/blog/commonly-confused-words/et-cetera-etc/ .).
;; 1. Based on the structure shown in paper, here the led-corresponding token has been consumed.
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
           ;; reverse is already done by prsnary.
           l)
          ((eq? comma (token-peek stream))
           (token-read stream)
           ;; prsnary will consume all comma's (but not supporting the trailing commma same as )
           (loop (cons (car l) (prsnary comma stream))))
          (else
            (error 'comma-or-match-not-found (token-read stream)))))
  (cond ((eq? token (token-peek stream))
         ;; null argument
         (token-read stream)
         nil)
        (else
          ;; to stop when encountering one comma.
          (loop (list (parse (lbp comma) stream))))))

(define (prsmatch-modified token comma stream)
  (set! token (value-if-symbol* token))
  (set! comma (value-if-symbol* comma))
  (define (loop l)
    (cond ((eq? token (token-peek stream))
           (token-read stream)
           (reverse l))
          ((eq? comma (token-peek stream))
           (token-read stream)
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
;; -1<5[),},then,else,:]<10[',',]<60[or]<65[and]<70[not]<80[<=]<100[left-minus & null-minus]<120[left-*]<140[**]<200[]

;; rbp order
;; IMHO since here 45 and 25 are >= the same group of lbp's (in the same group delimited by lbp), it is fine to define if-rbp as 25.
;; 25[then,else,:]<45[if]<60<65<70<80<100<120<139

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
           ;; used for comparison with others' rbp's.
           ;; But that is used for led...
           ;; since no such a{...} for (proc (begin ...)) where the latter should be a({...}).
           ;; we don't need lbp here.
           ;  lbp 200
           )

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
           lbp 65
           rbp 65
           )

(defsyntax or*
           led parse-nary
           lbp 60
           rbp 60
           )

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
  (writes nil "call open-paren-nud with" token "\n" stream "\n")
  (let ((intermediate (prsmatch-modified 'CLOSE-PAREN 'COMMA stream)))
    (assert (list? intermediate))
    (cond 
      ;; Based on https://docs.python.org/3/reference/expressions.html#parenthesized-forms
      ((= 1 (length intermediate)) (car intermediate))
      (else (cons 'tuple intermediate)))
    )
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

(cd "~/SICP_SDF/SDF_exercises/common-lib/")
(load "test_lib.scm")
(define (pl-assert expected exp)
  (let ((res (pl exp)))
    (assert* (equal? expected res) (list "unequal" expected res))))

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
  '(+ (tuple 1 3 7) 2)
  `(,OPEN-PAREN 1 COMMA 3 COMMA 7 COMMA ,CLOSE-PAREN + ,OPEN-PAREN 2 ,CLOSE-PAREN))

(pl-assert 
  '(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))
  `(fact := lambda* n : if* n = 0 then 1 else* n * fact ,OPEN-PAREN n - 1 ,CLOSE-PAREN))

;; Here all possible elements for parameter_list in SDF_exercises/chapter_5/5_7_naive_algorithm_for_operator_precedence_parser/5_7_precedence_lib.scm
;; are included here (Based on memory for what is done to this program at some time after finishing this program).
(pl-assert
  ;; Here we assume define returns the value instead of that symbol, although not this case in MIT/GNU Scheme.
  '(define fact (lambda (a (define k 2) (= b 0) / c *args * kwarg1 **kwargs) (if (= n 0) 1 (* n (fact (- n 1))))))
  ;; test from SDF_exercises/chapter_5/5_7_re_lib/5_7_regexp_lib.scm
  `(fact := lambda* a COMMA k := 2 COMMA b = 0 COMMA / COMMA c COMMA *args COMMA * COMMA kwarg1 COMMA **kwargs : if* n = 0 then 1 else* n * fact ,OPEN-PAREN n - 1 ,CLOSE-PAREN))

;;;; Compatibility with Python
;;; 0. Call https://docs.python.org/3/reference/expressions.html#calls
;; Possible arg types:
;; a. assignment_expression := (see SDF_exercises/chapter_5/5_7_related_python_behavior/assignment_expression_arg.py for its difference from keyword_item)
;; [identifier ":="] expression
;; b. *expression
;; c. identifier=expression
;; d. **expression
;; In Scheme only the mere expression is supported natively.
;; For parsing, all can be supported although := inside arglist is a bit weird.

; (pl '(lambda* a :)) ; throw error consistent with MIT/GNU Scheme convention
; premature-termination-of-input $

(pl-assert
  '(lambda (a) (begin (** a a) (* a 5)))
  `(lambda* a : ,LEFT-BRACE a ** a ,SEMICOLON a * 5 ,RIGHT-BRACE))

(pl-assert '(and* 1 2 3) '(1 and* 2 and* 3))
;; here and* should not use the greatest rbp...
(pl-assert '(and* 1 (+ 2 3) 3) '(1 and* 2 + 3 and* 3))

;; Although := in Python has the lowest precedence, it won't does something like "1 and 2 := 3".
;; TODO That is due to the grammar and its related constraint (https://stackoverflow.com/a/79544622/21294350), so the actual parser is complexer...
