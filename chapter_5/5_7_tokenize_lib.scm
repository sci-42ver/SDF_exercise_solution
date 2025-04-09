(cd "~/SICP_SDF/SDF_exercises/chapter_5")
(load "5_7_re_lib/5_7_regexp_lib.scm")
;; tokenizer
;; 0. Here I keep string to avoid manipulating with "," etc which can't be directly represented by symbols.
;; Then we make it infix just using SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/pratt_new_compatible_with_MIT_GNU_Scheme.scm
;; but making all "defsyntax" defined upon str instead of symbol.
;; Here we have all strings which can be "directly represented by symbols".
;; Then we make all the rest string to symbol or number where the latter is done by checking whether "\d+".
(define (tokenize exp)
  (%tokenize exp split-lst partition-separtor-lst skipped-primitive-re-lst))
(define (check-split-lst-and-partition-lst . sre-lsts)
  (for-each 
    (lambda (sre-lst)
      (assert (sre-lst? sre-lst))
      )
    sre-lsts)
  )
(define (%tokenize exp split-lst partition-separtor-lst skipped-re-lst)
  (assert (string? exp))
  (check-split-lst-and-partition-lst split-lst partition-separtor-lst skipped-re-lst)
  (let* ((split-res (exp-split exp split-lst))
         (partition-res 
          (append-map 
            (lambda (res)
              (exp-partition 
                res 
                partition-separtor-lst 
                skipped-re-lst)
              )
            split-res
            )
          ))
    (remove empty-str? partition-res))
  )