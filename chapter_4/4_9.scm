

(?:pletrec ((palindrome
              (?:pnew (x)
                      (?:choice ()
                                ((? x ,symbol?)
                                 (?:ref palindrome)
                                 (? x))))))
           (?:ref palindrome))

