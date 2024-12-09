; sed -i -f format.sed peephole.scm
(define peephole (make-pattern-operator))
; https://en.wikipedia.org/wiki/Peephole_optimization#
;; just means local optimization
;; https://en.wikipedia.org/wiki/Peephole_optimization#Implementation
(attach-rule! peephole
              (rule '((push (? reg1))
                      (pop (? reg2)))
                    (if (eqv? reg1 reg2)
                      ;; > Instead of pushing a register onto the stack and then immediately popping the value back into the register, remove both instructions
                      '()
                      ;; https://en.wikipedia.org/wiki/Peephole_optimization#Replacements
                      ;; > Combine operations – Replace several operations with one equivalent
                      `((move ,reg1 ,reg2)))))
(attach-rule! peephole
              (rule `((or (? reg) (? const1 ,unsigned-integer?))
                      (or (? reg) (? const2 ,unsigned-integer?)))
                    ;; 0. https://en.cppreference.com/w/c/language/operator_logical
                    ;; only all 0 can output 0.
                    ;; So bitwise-or is fine.
                    ;; 1. https://stackoverflow.com/a/11052460/21294350
                    ;; 1.a. https://sdremthix.medium.com/branchless-programming-why-your-cpu-will-thank-you-5f405d97b0c8#:~:text=Bitwise%20operations%20are%20typically%20faster,not%20requiring%20additional%20computing%20cycles.
                    ;; 1.a.0. see "The mask will have *all* bits set to 1 if the difference is negative"
                    ;; > int greater = a - ((a - b) & ((a - b) >> (sizeof(int) * CHAR_BIT - 1)));
                    ;; 1.a.1. based on https://en.cppreference.com/w/c/language/operator_comparison
                    ;; > int mask = (a > b) - 1; 
                    ;; 1.a.2. 
                    ;; see 2's complement https://en.wikipedia.org/wiki/Two%27s_complement#Why_it_works related with modulo arithmetic. (said in DMIA notes https://math.stackexchange.com/a/1920795/1059606 3rd part)
                    ;; So 0=256 (based on 8 bit number) = (255-x) + 1 + x
                    ;; so "inverting" and then plus 1.
                    ;; 1.a.2.0. https://stackoverflow.com/a/22634965/21294350
                    ;; > absX = (x + mask) ^ mask;
                    ;; mask=0 trivial doing nothing
                    ;; mask=-1 just means ~absX=x-1->~absX+1=-absX
                    ;; 1.b. https://softwareengineering.stackexchange.com/a/381829
                    ;; 1.b.0. based on ! returns 0/1.
                    ;; 2. see the 3rd quote in https://stackoverflow.com/a/7415759/21294350
                    `((or ,reg
                          ,(bitwise-or const1 const2)))))
