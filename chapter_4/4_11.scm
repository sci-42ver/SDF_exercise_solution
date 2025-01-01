;; > This could be fun,
;; > but you need to invent a syntactic mechanism for representing
;; > string variables inside a string. This is pretty delicate, because you
;; > may have to represent a string variable with a string expression.
;; 0. delicate  means subtle.
;; 1. Python uses {} like {num} to mean possible var *implicitly* https://stackoverflow.com/a/52155770/21294350.
;; https://stackoverflow.com/a/2962966/21294350 also shows % similarly like %(num).
;; So similarly we can use (?/?? ...) to *implicitly* denote that var.
;;;; > This gets into quotation problems
;;; TODO: I don't what the author means by "quotation problem". 
;; maybe this https://courses.grainger.illinois.edu/cs421/sp2023/exams/Sample-Midterm3-Sol-sp23.pdf
;; > So """" represents a string that has one character which is a double quotes.
;; In Scheme we use \" https://edoras.sdsu.edu/doc/mit-scheme-9.2/mit-scheme-ref/Strings.html
;;; IMHO just use string comparison with string.
;; > please try not to invent a baroque mechanism.
;; i.e. not too complex https://www.reddit.com/r/compsci/comments/1mli7h/it_is_wellknown_that_the_x86_instruction_set_is/?utm_source=share&utm_medium=web3x&utm_name=web3xcss&utm_term=1&utm_content=share_button.
;; the similar meaning as "*contrast*, movement, *exuberant* detail," https://en.wikipedia.org/wiki/Baroque\

;; IMHO string is just list of characters. So we can reuse the original implementation.
(regexp-partition '(: "(" (or "?" "??") space (+ alphanumeric) ")") "(?? seg1)(? seg1)")
;; only the 1st
(regexp-match->list
 (regexp-search '(: "(" (or "?" "??") space (+ alphanumeric) ")") "(?? seg1)(? seg2)"))
(regexp-extract '(: "(" (or "?" "??") space (+ alphanumeric) ")") "(?? seg1)(? seg2)")
(define (string->list-terms string)
  
  )
