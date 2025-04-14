;;; See irregex which implements neg-look-behind *internally* out of box.
(cd "~/SICP_SDF/SDF_exercises/chapter_5")
(load "5_7_common_lib/string_lib.scm")

(cd "~/SICP_SDF/SDF_exercises/")
(load "common-lib/re_lib.scm")

;; 0. One workaround for neg-look-behind.
;; (i.e. `(: (neg-look-behind ,neg-pat) normal-pat)
;; 0.a. Actually here is more general since it has no restriction of "fixed length" for neg-pat.
(define (arbitrary-len-neg-look-behind neg-pat normal-pat exp)
  (assert (string? exp))
  (let ((last-end 0)
        (len (string-length exp)))
    (let ((res 
            ;; This can't be wrapped with regexp-extract* since we need to *internal* neg-look-behind logic.
            (regexp-fold normal-pat
                          ;; IGNORE Here i is the index before m-start or 0 if m-start is 0.
                          ;; Emm... sometimes i is the index of m-start inclusive, while sometimes exclusive.
                          (lambda (i m str acc)
                            (let* ((s (regexp-match-submatch m 0))
                                    ;; If m matches "ab" in "mnabc", then start is 2 with index 0 for the leftmost.
                                    (start (regexp-match-submatch-start m 0))
                                    ;; For the former example, end is 4 which is the index *past* the match end.
                                    (end (regexp-match-submatch-end m 0))
                                    ;; Returns "" if start=0. Anyway this returns str-before due to exclusive end argument "start".
                                    (str-before (substring str 0 start)))
                              (let ((end-with-word (regexp-search `(: ,neg-pat eos) str-before))
                                    (match (regexp-match-submatch m 0))
                                    )
                                ; (write-line (list "last-end" last-end "i" i "match" match "str-before" str-before))
                                (if end-with-word
                                  (begin
                                    (write-line 
                                      (list 
                                        match
                                        "with"
                                        (regexp-match-submatch end-with-word 0) 
                                        "before"
                                        "is not one *args or **kwargs"
                                        ))
                                    ;; 0. arbitrary-len-neg-look-behind done here, i.e. not consider this match (negative).
                                    ;; 1. Notice to return one valid object here for the usage in the next fold iteration.
                                    acc
                                    )
                                  (let ((intermediate-str 
                                          (substring-lst 
                                            str 
                                            last-end 
                                            start
                                            )))
                                    ; (write-line (list "last-end" last-end "i" i "append" intermediate-str match))
                                    ;; As the above shows, index end elem is not consumed. So it will be used as the next inclusive start.
                                    (set! last-end end)
                                    (append
                                      acc
                                      intermediate-str
                                      (list match))
                                    )
                                  ))
                              ))
                          '()
                          exp)
            ))
      ;; consume the rest substring.
      (append res (substring-lst exp last-end))
      )
    )
  )
