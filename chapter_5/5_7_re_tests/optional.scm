(regexp-match-submatch (regexp-search '(? "es" "ab") "esab") 0)
; can't implement or
(regexp-match-submatch (regexp-search '(? "es" "ab") "esab") 1)

(regexp-match-submatch (regexp-search '(? (or "es" "ab")) "esab") 0)
(regexp-match-submatch (regexp-search '($ (? (or "es" "ab"))) "esab") 0)
(regexp-match-submatch (regexp-search '($ (? (or "es" "ab"))) "esab") 1)
(regexp-match-submatch (regexp-search '($ (? (or "es" "ab"))) "esab") 2)

;; regexp-search can't get all "substrings". 
(regexp-extract '($ (? (or "es" "ab"))) "esab")
(regexp-extract '(? (or "es" "ab")) "esab")
; implicit concatenation.
(regexp-extract '(? "es" "ab") "esab")
