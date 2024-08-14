;; a. 3rd para GNU https://www.gnu.org/software/sed/manual/html_node/BRE-vs-ERE.html
;; Similar for POSIX https://www.baeldung.com/linux/bre-ere-pcre-syntax "One distinct characteristic ..."
;;    But plus "In addition to these operators ..." (+,|,? are not supported by BRE)
;; Also see https://en.wikipedia.org/wiki/Regular_expression#POSIX_basic_and_extended (same as link 1) from https://eli.thegreenplace.net/2012/11/14/some-notes-on-posix-regular-expressions
;;    lazy match depends on implementation.
;;    "or lack of \d instead of POSIX [:digit:]": This is in 9.3.5 so both support it. `echo 1 | grep -e '[[:digit:]]' -` or `... -E ...` both work.
;; b. Use one arg.
;; > How can we maintain the abstract layer that is independent of the target regular expression language?
;;    IMHO use one regex_cfg to cfg all internal language implementation, then call with no language arg later.

;; by searching "ere", nbardiuk and mbillingr don't have implementation.

;; SEE 6.945_assignment_solution "ERE does not support back-references" which can be manipulated similar to |.
;; Based on a,b), same as 6.945_assignment_solution here we only consider escape difference.

;; Same as chebert, we can list those characters needed with backslash in BRE but not in ERE to have special meanings
;; Compare 9.3.3 BRE Special Characters and 9.4.3 ERE Special Characters
;; IMPLEMENTATION AND TEST: In the following, I use r:escape to check them and give one related r:seq implementation
(define ERE_Special_Characters_Addition
  ;; if \} the error may be delayed until #\\

  ;; 6.945_assignment_solution lacks } in ps01.scm but `make-special` includes it.
  ;; grep seems to not be exactly BRE since `echo "aa" | grep -e 'a\+' -` works.
  '(#\( #\{ #\) #\}))
;; only #\| is used by r:alt and others are not used in code base.
;; IMPLEMENTATION AND TEST: I only give the test of #\|, the rest should be similar.
(define BRE_not_supported_special_characters
  '(#\| #\+ #\?))

;; same as 6.945_assignment_solution before implementing ere
;; wrong. see chebert where we need to consider bre/ere. Also 6.945_assignment_solution quote-char
(define (r:quote string)
  (list->string
    (append-map (lambda (char)
                  (if (memv char chars-needing-quoting)
                    (list #\\ char)
                    (list char)))
                (string->list string))))

;; naming same as chebert
(define regex_type 'bre)

;; same as 6.945_assignment_solution. chebert is wrong as https://stackoverflow.com/q/78792939/21294350 shows.
(define (r:seq . exprs)
  (let ((escape_parenthesis 
          ;; Here I only consider 2 types, i.e. ere / bre.
          (if (equal? regex_type 'bre)
            #t
            #f)))
    (if escape_parenthesis
      (string-append "\\(" (apply string-append exprs) "\\)")
      (string-append "(" (apply string-append exprs) ")"))))

;; diff pair means corresponding interface between 2 implementations.
;; simialr to r:special-char with diff pair (list->string,char->name). Also for make-special.

;; Here we need to use capture the special meaning of char instead of ordinary char.
(define (r:escape char)
  (let ((use_bre 
          (if (equal? regex_type 'bre)
            #t
            #f))
        (char_str (char->name char)))
    (if use_bre
      (if (member char BRE_not_supported_special_characters)
        (error "not available in BRE")
        ;; not use (member char chars-needing-quoting) since something like \. corresponds to ordinary char.
        (if (member char ERE_Special_Characters_Addition)
          (string-append "\\" char_str)
          char_str))
      char_str)))

;; same as 6.945_assignment_solution
(define (r:alt . exprs)
  (if (pair? exprs) ; i.e. list in the current context.
    ;; https://unix.stackexchange.com/a/526274/568529
    (apply r:seq
           (cons (car exprs) ; For (element list), append does same as cons.
                 (append-map (lambda (expr)
                               (list (r:escape #\|) expr))
                             (cdr exprs))))
    (r:seq)))

(load "utils.scm")
;; Here the abstraction works since we don't need to change each levels to *add the arg* as 6.945_assignment_solution and chebert do.
;; So we don't need `add-args` in 6.945_assignment_solution.
;; basic ideas same as 6.945_assignment_solution and chebert.
(define (bourne-shell-grep-command-string expr filename)
  (let ((escape_parenthesis 
          ;; Here I only consider 2 types, i.e. ere / bre.
          (if (equal? regex_type 'bre)
            #t
            #f)))
    (string-append (if escape_parenthesis
                     "grep -e "
                     (begin
                       (displayln "use egrep")
                       ;; See chebert we should use -Ee
                       "grep -E "))
                   (bourne-shell-quote-string expr)
                   " "
                   filename)))
