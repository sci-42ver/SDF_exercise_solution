(cd "~/SICP_SDF/SDF_exercises/chapter_5")
(load "../common-lib/stack_lib.scm")
(load "../common-lib/pair_lib.scm")

(load "5_7_re_lib/5_7_regexp_lib.scm")
(define (customized-stack-elem? data)
  (and (pair?* data) 
       (pair?* (get-left data))))
;; For safety, all add assertion which may be redundant in some way.
(define (customized-stack-elem-tag data) 
  (assert (customized-stack-elem? data))
  (get-left (get-left data)))
(define (new-customized-stack-elem left-idx right-idx pair)
  (assert (customized-stack-elem? pair))
  (new-pair 
    (new-pair (customized-stack-elem-tag pair) left-idx) 
    right-idx))
(define (non-application? data)
  (assert (customized-stack-elem? data))
  (equal? 'non-application (customized-stack-elem-tag data))
  )
(define (get-left-idx data)
  (assert (customized-stack-elem? data))
  (get-right (get-left data))
  )
(define (get-right-idx data)
  (assert (customized-stack-elem? data))
  (get-right data)
  )
;; similar to naming of match:map-binding-value
(define (map-idx-pair proc idx-pair)
  (assert (customized-stack-elem? idx-pair))
  (new-customized-stack-elem 
    (proc (get-left-idx idx-pair))
    (proc (get-right-idx idx-pair))
    idx-pair)
  )
(define (parenthesis-idx-pair-stack str-lst)
  (assert (every string? str-lst))
  ;; just based on match-parentheses?
  (let ((str-cnt (length str-lst)))
    (let lp 
      ((paren-cnt 0) 
       (idx 0) 
       (paren-to-match (list))
       ;; 0. use 2 stacks to get the pairs `[ pop @stack, $pos ]`s https://stackoverflow.com/a/56239802/21294350
       ;; > push @output, [ pop @stack, $pos ]
       ;; 0. @ https://stackoverflow.com/questions/5553898/what-are-the-differences-between-in-perl-variable-declaration
       ;; 1. pop https://perlmaven.com/manipulating-perl-arrays
       ;; Here sort can be ignored
       ;; 2. @$ https://stackoverflow.com/a/37208206/21294350
       ;; 3. <=> https://www.shlomifish.org/lecture/Perl/Newbies/lecture2/useful_funcs/sort/cmp.html
       ;; sort https://perldoc.perl.org/functions/sort
       ;;;; 4. For details, see perlfunc for use, my, split (also see perlretut), push, pop, sort, say.
       ;; > # sort numerically ascending
       ;; perlop for qw, '', eq, =, ++, <=>, "" (qq//).
       ;; perlintro for $, @, ;, and 
       ;; > You can use parentheses for functions' arguments or omit them according to your personal taste.  They are only required occasionally to clarify issues of precedence.
       ;; perlref for [], ->[], @$_.
       ;;; perlsyn for "for", "if" (see Compound Statements). Notice LIST meaning "any combination of *scalar arguments* or list values".
       ;; elsif is not detailedly descried maybe assumingly functioning like other programming languages.
       ;; See "Statement Modifiers" for "for sort ...".
       ;; 0.a. One stack is at least not straightforward.
       ;; With two, one stack can be pushed and poped which just like +/- paren-cnt here.
       ;; 1. For simplicity, here only consider "(" and ")".
       ;; So just one stack without using dict.
       (left-pos-stack (make-new-stack))
       (pos-pair-stack (make-new-stack))
       )
      (if (n:<= str-cnt idx)
        (if (n:> paren-cnt 0)
          (error "redundant parentheses")
          pos-pair-stack)
        (let ((cur (list-ref str-lst idx)))
          (let ((next-idx (n:+ idx 1)))
            (cond 
              ((left-parenthesis? cur)
               (let ((left-pos-stack*
                       (if (or (n:= idx 0) (and (n:> idx 0) (keyword? (list-ref str-lst (n:- idx 1)))))
                         (push! left-pos-stack (new-pair 'non-application idx))
                         (push! left-pos-stack (new-pair 'application idx))
                         )))
                 (lp 
                   (n:+ paren-cnt 1) 
                   next-idx 
                   (append paren-to-match (list left-parenthesis))
                   left-pos-stack*
                   pos-pair-stack
                   ))
               )
              ((right-parenthesis? cur)
               (let ((paren-cnt* (n:- paren-cnt 1)))
                 (if 
                   (or 
                     (n:< paren-cnt* 0)
                     (not (equal? left-parenthesis (list-ref paren-to-match paren-cnt*))))
                   (error "matched with the wrong right str")
                   )
                 (let ((paired-left-pos (pop! left-pos-stack)))
                   (lp 
                     paren-cnt* 
                     next-idx 
                     (drop-right paren-to-match 1)
                     left-pos-stack
                     (push! pos-pair-stack (new-pair paired-left-pos idx))
                     ))
                 )
               )
              (else
                (lp paren-cnt next-idx paren-to-match left-pos-stack pos-pair-stack)
                )
              ))
          ))
      ))
  )

;; 0. As the above Stack Overflow link shows, here we better sort by left-pos.
;; Then we must manipulate outer before inner parentheses
;; 0.a. If parentheses are matched, then only one can contain the other totally but not partially.
;; i.e. there are only 3 relations with 2 for containment relation.
(define (combine-non-application-parentheses str-lst)
  (let* ((parenthesis-idx-pair-lst (get-tagged-lst-data (parenthesis-idx-pair-stack str-lst)))
         (non-application-parenthesis-idx-pair-lst
           (filter 
             (lambda (pair) (non-application? pair)) 
             parenthesis-idx-pair-lst))
         (sorted-idx-pair-lst 
           (get-tagged-lst-data
             (new-stack
               (sort 
                 non-application-parenthesis-idx-pair-lst
                 n:< 
                 get-left-idx)))))
    (%combine-non-application-parentheses str-lst sorted-idx-pair-lst)
    ;; 0. After combination, application pairs will be located inside different non-application-parenthesis-pair's.
    ;; To use that in the later manipulation for each non-application-parenthesis-pair, we need to extract the corresponding one and update the rest for each infix->polish possibly.
    ;; IMHO that seems to incur more calculation than that we do iteration for the parenthesis again...
    ;; 0.a. This is similar for [],{} locations.
    )
  )
(define (inside? inner outer)
  (and (customized-stack-elem? inner) (customized-stack-elem? outer)
       (n:> (get-left-idx inner) (get-left-idx outer))
       (n:< (get-right-idx inner) (get-right-idx outer))
       )
  )
(define (outside? elm1 elm2)
  (and (customized-stack-elem? elm1) (customized-stack-elem? elm2)
       (or 
         (n:< (get-right-idx elm1) (get-left-idx elm2))
         (n:< (get-right-idx elm2) (get-left-idx elm1))
         )
       )
  )
(define (%combine-non-application-parentheses str-lst sorted-idx-pair-lst)
  (if (null? sorted-idx-pair-lst)
    str-lst
    (let ((first-pair (car sorted-idx-pair-lst))
          (rest-pairs (cdr sorted-idx-pair-lst)))
      (let ((first-left (get-left-idx first-pair))
            (first-right (get-right-idx first-pair))
            (inner-pairs-for-first (filter (lambda (pair) (inside? pair first-pair)) rest-pairs))
            (outer-pairs-for-first (filter (lambda (pair) (outside? pair first-pair)) rest-pairs))
            )
        (assert 
          (n:= 
            (n:+ (length inner-pairs-for-first) (length outer-pairs-for-first))
            (length rest-pairs)
            ))
        (append 
          (take str-lst first-left) 
          (list 
            (append 
              (list left-parenthesis)
              (%combine-non-application-parentheses
                ;; drop the first parenthesis pair
                (drop
                  (take str-lst (n:+ first-right))
                  (n:+ first-left 1)
                  ) 
                (map 
                  (lambda (idx-pair) 
                    (map-idx-pair
                      (lambda (idx) (n:- idx 1))
                      idx-pair)) 
                  inner-pairs-for-first))
              (list right-parenthesis)))
          (%combine-non-application-parentheses 
            (drop str-lst (n:+ first-right 1)) 
            (map 
              (lambda (idx-pair) 
                (map-idx-pair
                  (lambda (idx) (n:- idx first-right 1))
                  idx-pair)) 
              outer-pairs-for-first)))
        )
      ))
  )

;; 0. https://stackoverflow.com/a/60325128/21294350
;; Search for matching parenthesis
;; DMIA and mcs gives one algorithm and doesn't offer one code example.
;; 1. For combine-non-application-parentheses, here idx is kept to allow back-ref and then check whether it is one application.
(define (match-parentheses? str-lst)
  (let ((str-cnt (length str-lst)))
    (let lp ((paren-cnt 0) (idx 0) (paren-to-match (list)))
      (if (n:<= str-cnt idx)
        (if (n:> paren-cnt 0)
          (error "redundant parentheses")
          'matched)
        (let ((cur (list-ref str-lst idx)))
          (let ((next-idx (n:+ idx 1)))
            (cond 
              ((left-parenthesis? cur)
               (lp (n:+ paren-cnt 1) next-idx (append paren-to-match (list left-parenthesis))))
              ((right-parenthesis? cur)
               (let ((paren-cnt* (n:- paren-cnt 1)))
                 (if 
                   (or 
                     (n:< paren-cnt* 0)
                     (not (equal? left-parenthesis (list-ref paren-to-match paren-cnt*))))
                   (begin
                     ; (write-line (list "match-parentheses?" paren-cnt* (list-ref paren-to-match paren-cnt*)))
                     (error "matched with the wrong right str"))
                   )
                 (lp paren-cnt* next-idx (drop-right paren-to-match 1))
                 )
               )
              (else
                (lp paren-cnt next-idx paren-to-match)
                )
              ))
          ))
      ))
  )

;; from SO link
; (trace %combine-non-application-parentheses)
(combine-non-application-parentheses (parse "((((((...)))(((...))))))"))
; (((
;   (((...)))
;   (((...))))))
;; corresponds to 
; (("(" 
;   ("(" 
;     ("(" 
;       ("(" 
;         ("(" 
;           ("(" "..." ")") ")") ")") 
;       ("(" 
;         ("(" 
;           ("(" "..." ")") ")") ")") ")") ")") ")"))
