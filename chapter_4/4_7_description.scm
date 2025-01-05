;; substitute just means replace https://www.merriam-webster.com/dictionary/substitute#:~:text=Examples%20of%20substitute%20in%20a,real%20candles%20with%20electric%20ones.
;; i.e. replace "a defined pattern" with its actual definition.

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