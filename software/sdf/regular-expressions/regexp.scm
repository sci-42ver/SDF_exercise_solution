#| -*-Scheme-*-

Copyright (C) 2019, 2020, 2021 Chris Hanson and Gerald Jay Sussman

This file is part of SDF.  SDF is software supporting the book
"Software Design for Flexibility", by Chris Hanson and Gerald Jay
Sussman.

SDF is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SDF is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with SDF.  If not, see <https://www.gnu.org/licenses/>.

|#

;;;; Scheme Regular Expression Language Implementation -- regexp.scm

(define (r:dot) ".")
(define (r:bol) "^")
(define (r:eol) "$")

(define (r:quote string)
  (r:seq
   (list->string
    ;; https://stackoverflow.com/a/14617333/21294350 here we need append to manipulate with nested lists.
    (append-map (lambda (char)
                  ;; As MIT_Scheme_Reference 3 Equivalence Predicates shows, `eqv?` can check case "obj1 and obj2 are both characters".
                  ;; equal? only checks display by "generally equal? if they print the same" which is too general.
                  (if (memv char chars-needing-quoting)
                      (list #\\ char)
                      (list char)))
                (string->list string)))))

;; See POSIX_regex 9.3.3 BRE Special Characters
(define chars-needing-quoting
  ;; See MIT_Scheme_Reference 5 Characters based on index.
  '(#\. #\[ #\\ #\^ #\$ #\*))

(define (r:char-from string)
  (case (string-length string)
    ((0) (r:seq))
    ((1) (r:quote string))
    (else
     (bracket string
              (lambda (members)
                (if (lset= eqv? '(#\- #\^) members)
                    ;; This may be due to `quote-bracketed-contents` will output [^-] instead of [-^] although this can be easily avoided by swapping.
                    '(#\- #\^)
                    (quote-bracketed-contents members)))))))

(define (r:char-not-from string)
  (bracket string
           (lambda (members)
             (cons #\^ (quote-bracketed-contents members)))))

(define (bracket string procedure)
  (list->string
   (append '(#\[)
           (procedure (string->list string))
           '(#\]))))

(define (quote-bracketed-contents members)
  (define (optional char)
    (if (memv char members) (list char) '()))
  ;; > by placing them in positions where they are not operators
  (append (optional #\]) ; []str...] works fine in https://regex101.com/
          (remove (lambda (c)
                    (memv c chars-needing-quoting-in-brackets))
                  members)
          (optional #\^)
          (optional #\-)))

(define chars-needing-quoting-in-brackets
  '(#\] #\^ #\-))

;;; Means of combination for patterns

(define (r:seq . exprs)
  (string-append "\\(" (apply string-append exprs) "\\)"))

;;; An extension to POSIX basic regular expressions.
;;; Supported by GNU grep and possibly others.
(define (r:alt . exprs)
  (if (pair? exprs) ; i.e. list in the current context.
      (apply r:seq
             (cons (car exprs) ; For (element list), append does same as cons.
                   (append-map (lambda (expr)
                                 (list "\\|" expr))
                               (cdr exprs))))
      (r:seq)))

(define (r:repeat-from-text min max expr)
  (apply r:seq
         (append (make-list min expr)
                 (cond ((not max) (list expr "*"))
                       ((= max min) '())
                       (else
                        (make-list (- max min)
                                   (r:alt expr "")))))))
(define r:repeat r:repeat-from-text)

;;; Using system's grep.
(define (write-bourne-shell-grep-command expr filename)
  (display (bourne-shell-grep-command-string expr filename)))

(define (bourne-shell-grep-command-string expr filename)
  ; (newline)
  ; (display (bourne-shell-quote-string expr))
  (string-append "grep -e "
                 (bourne-shell-quote-string expr)
                 " "
                 filename))

;;; Works for any string without newlines.
(define (bourne-shell-quote-string string)
  (list->string
   (append (list #\')
           (append-map (lambda (char)
                         (if (char=? char #\')
                              ;; Use `echo "'" | grep -e ''\''' -` to check this. https://stackoverflow.com/a/6641753/21294350
                             (list #\' #\\ char #\')
                             (list char)))
                       (string->list string))
           (list #\'))))


;;; This is MIT/Scheme specific and compatible with grep for the
;;; purposes of this code.

(load-option 'synchronous-subprocess)

(define (r:grep expr filename)
  (let ((port (open-output-string)))
    (and (= (run-shell-command
             (bourne-shell-grep-command-string expr filename)
             'output port)
            0)
         (r:split-lines (get-output-string port)))))

(define (r:split-lines string)
  (reverse
   (let ((end (string-length string)))
     (let loop ((i 0) (lines '()))
       (if (< i end)
           (let ((j
                  ;; https://groups.csail.mit.edu/mac/ftpdir/scheme-7.4/doc-html/scheme_7.html
                  (substring-find-next-char string i end #\newline)))
             (if j
                 (loop (+ j 1)
                       (cons (substring string i j) lines))
                 (cons (substring string i end) lines)))
           lines)))))

;;; book examples
(r:alt (r:quote "foo") (r:quote "bar") (r:quote "baz"))