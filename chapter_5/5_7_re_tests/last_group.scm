;; compared with re.Match.lastgroup https://docs.python.org/3/library/re.html#re.Match.lastgroup

(cd "~/SICP_SDF/SDF_exercises/common-lib")
(load "re_lib.scm")
;; Same behavior as Python with the last one for the last matched ")".
(map regexp-match->list (regexp-extract* '($ ($ "a") ($ "b")) "ab"))
