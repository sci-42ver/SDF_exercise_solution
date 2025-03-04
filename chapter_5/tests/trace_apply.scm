;; apply can't be traced maybe due to being implemented by syntax.
(trace apply)
(apply + '(2 3))