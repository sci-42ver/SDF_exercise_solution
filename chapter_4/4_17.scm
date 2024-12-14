;; Here I won't consider the case where one local procedure changes the global variable binding.
;; Otherwise, we need to track the program execution order at runtime which is even more complex when with stream or lazy evaluation as SICP says.
;; TODO This is said in one QA discussion.
;; So only allow set! etc to change local bindings.