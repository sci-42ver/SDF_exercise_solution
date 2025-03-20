;; -*-mode:lisp;parser:read-*-

;; To use this syntax in a file, put the following in the file "mode line"
;; -*-parser:pratt-*-
;;; mode line is similar to https://stackoverflow.com/a/1931807/21294350

;; TODO require-so is unavailable although in "Reference Section for built-in procedures" https://www.nongnu.org/muesli/ulsiod.html#ext
; > (so-ext'parser_pratt)
; Evaluation took 0 seconds (0 in gc) 4 siod_cons work, 8.10623e-06 real.
; "parser_pratt.so"
; > (require-so 'a)
; in err(unbound variable)
; ERROR: unbound variable (errobj require-so)

; (require-so (so-ext'parser_pratt))
(require'pratt.scm)

(define (setup-tokenizer table . rest)
  (define (norm e)
    (cond ((pair? e)
           (cons (norm (car e)) (norm (cdr e))))
          ((string? e)
           (aref e 0))
          (e)))
  (let ((l rest))
    (while l
           (let ((j 0)
                 (str (car l))
                 (val (cadr l)))
             (while (< j (length str))
                    (aset table (aref str j) (norm val))
                    (set! j (+ 1 j))))
           (set! l (cddr l))))
  table)

(define *standard-tokenizer-table*
  (setup-tokenizer (cons-array 256)
                   "abcdefghijlkmnopqrstuvwxyz" 'regular
                   "ABCDEFGHIJLKMNOPQRSTUVWXYZ" 'regular
                   "0123456789" 'regular
                   "_" 'regular
                   " \r\t\n" 'whitespace
                   "\\" 'back-slash
                   "#" 'nl-whitespace
                   "\"" 'string-delim
                   "*" '(("*"))
                   ">:<" '(("="))))

(define (te)
  ;; I didn't dig into the C codes parser_pratt.c. 
  ;; Anyway based on its API, it probably uses one buffer to store these tokens extracted from stream based on the table.
  ;; Probably similar to regex-based tokenize in SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/python_demo/pratt.py
  (pratt_read_token (cons-array 100 'string)
                    *standard-tokenizer-table*
                    nil))

(define (parser_pratt ignore)
  (let ((buffer (cons-array 100 'string))
        (peek nil))
    (lambda (io-stream)
      (toplevel-parse (lambda (op arg)
                        (cond ((eq? op 'peek)
                               (or peek
                                   (set! peek
                                     (pratt_read_token
                                       buffer
                                       *standard-tokenizer-table*
                                       io-stream))))
                              (peek
                                (prog1 peek
                                  (set! peek nil)))
                              ((pratt_read_token
                                 buffer
                                 *standard-tokenizer-table*
                                 io-stream))))))))

(define (mread . io-stream)
  ((parser_pratt nil) (car io-stream)))


