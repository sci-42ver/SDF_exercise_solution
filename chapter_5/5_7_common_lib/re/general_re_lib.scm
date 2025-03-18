(cd "~/SICP_SDF/SDF_exercises/chapter_5")
(load "5_7_common_lib/string_lib.scm")
(define word-corrected '(or word (+ numeric)))
;; One workaround for neg-look-behind.
;; (i.e. `(: (neg-look-behind ,neg-pat) normal-pat)
(define (neg-look-behind neg-pat normal-pat exp)
  (assert (string? exp))
  (let ((last-end 0)
        (len (string-length exp)))
    (let ((res 
            (regexp-fold normal-pat
                          ;; IGNORE Here i is the index before m-start or 0 if m-start is 0.
                          ;; Emm... sometimes i is the index of m-start inclusive, while sometimes exclusive.
                          (lambda (i m str acc)
                            (let* ((s (regexp-match-submatch m 0))
                                    (start (regexp-match-submatch-start m 0))
                                    (end (regexp-match-submatch-end m 0))
                                    ; (corrected-i 
                                    ;   (if (n:= start 0)
                                    ;       start
                                    ;       (n:- start 1))
                                    ;   )
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
                                    acc
                                    )
                                  (let ((intermediate-str 
                                          (substring-lst 
                                            str 
                                            last-end 
                                            ; (if (n:= corrected-i 0) corrected-i (n:+ corrected-i 1))
                                            start
                                            )))
                                    ; (write-line (list "last-end" last-end "i" i "append" intermediate-str match))
                                    (set! last-end end)
                                    (append
                                      (if 
                                        ; (default-object? acc)
                                        (eq? #!unspecific acc)
                                        '()
                                        acc)
                                      intermediate-str
                                      (list match))
                                    )
                                  ))
                              ))
                          '()
                          exp)
            ))
      (append res (substring-lst exp last-end))
      )
    )
  )
