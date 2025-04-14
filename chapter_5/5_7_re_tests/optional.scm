;; can't implement or but implement implicit concatenation.
; (regexp-match-submatch (regexp-search '(? "es" "ab") "esab") 0)
; (regexp-match-submatch (regexp-search '($ (? "es" "ab")) "esab") 0)
(regexp-match-submatch (regexp-search '($ (? "es" "ab")) "esab") 1)

(regexp-match-submatch (regexp-search '(? (or "es" "ab")) "esab") 0)
(regexp-match-submatch (regexp-search '($ (? (or "es" "ab"))) "esab") 0)
(regexp-match-submatch (regexp-search '($ (? (or "es" "ab"))) "esab") 1)
(regexp-match-submatch (regexp-search '($ (? (or "es" "ab"))) "esab") 2)
;The object 2, passed as an argument to regexp-match-submatch, is not in the correct range.

;; regexp-search can't get all "substrings". 
(regexp-extract '($ (? (or "es" "ab"))) "esab")
(regexp-extract '(? (or "es" "ab")) "esab")
; implicit concatenation as the above says.
(regexp-extract '(? "es" "ab") "esab")
