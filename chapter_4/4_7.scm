;; substitute just means replace https://www.merriam-webster.com/dictionary/substitute#:~:text=Examples%20of%20substitute%20in%20a,real%20candles%20with%20electric%20ones.
;; i.e. replace "a defined pattern" with its actual definition.

;; > in an implicit flat global namespace, with all subsequent downstream appearances being constraining instances.
;; i.e. although depth-first, no stack structure. More specifically something can happen like that leaf can influence each other.
;; > This is achieved by threading the dictionary through the depth-first control path.
;; passing dictionary arg all around with new-dictionary help.

;; > In a proper environment-based letrec-like implementation, nested ?:pletrec instances would introduce distinct contour lines for scoping.
;; i.e. outer letrec can't use inner lecrec definitions.

(?:pletrec ((odd-even-etc (?:choice () (1 (?:ref even-odd-
                                                 etc))))
            (even-odd-etc (?:choice () (2 (?:ref odd-even-
                                                 etc)))))
           (?:ref odd-even-etc))

;; 4.9
;; will always bind x.
(?:pletrec ((palindrome
              (?:pnew (x)
                      (?:choice ()
                                ((? x ,symbol?)
                                 (?:ref palindrome)
                                 (? x))))))
           (?:ref palindrome))
;; compared with
(?:pletrec ((palindrome
              (?:choice ()
                        ((? x ,symbol?)
                          (?:ref palindrome)
                          (? x)))))
           (?:ref palindrome))
;; If using the latter, (?:ref palindrome) will have x binding.
;; So at last the (? x) sequence will only match the *last* x binding.
