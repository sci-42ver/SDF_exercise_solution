;; https://github.com/barak/mit-scheme/blob/56e1a12439628e4424b8c3ce2a3118449db509ab/src/runtime/srfi-115.scm#L149
;; Not use https://github.com/jaseemabid/mit-scheme which is a bit too old.
(define (regexp-fold re kons knil string #!optional finish start end ignore?)
  (let ((regexp (regexp re))
        (end (fix:end-index end (string-length string) 'regexp-fold))
        (ignore? (if (default-object? ignore?) #f ignore?)))
    (let ((start (fix:start-index start end 'regexp-fold)))

      (define (loop index last-match-end acc)
        (let ((match (%regexp-search regexp index string start end)))
          (if match
            (if (and ignore? (ignore? match))
              (skip index last-match-end match acc)
              (continue index match (kons last-match-end match string acc)))
            (done last-match-end acc))))

      (define (skip index last-match-end match acc)
        (loop (fix:max (regexp-match-end match) (fix:+ index 1))
              last-match-end
              acc))

      (define (continue index match acc)
        (let ((last-match-end (regexp-match-end match)))
          (loop (fix:max last-match-end (fix:+ index 1))
                last-match-end
                acc)))

      (define (done last-match-end acc)
        (if (default-object? finish)
          acc
          (finish last-match-end #f string acc)))

      (loop start start knil))))
