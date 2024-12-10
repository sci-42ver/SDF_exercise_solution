(cd "~/SICP_SDF/SDF_exercises/chapter_4")
(load "../software/sdf/manager/load.scm")
(manage 'new 'term-rewriting)

; https://stackoverflow.com/a/10605065/21294350
;; > You also can't sort faster than O(NlogN) if you sort by comparing
;; 0. I won't dig into comparing sorting methods. Here I just choose Quicksort which is Average O(n*logn)
;; also recommended by https://w3.cs.jmu.edu/spragunr/CS240/labs/sorting_lab/sorting.shtml#:~:text=Quicksort%20is%20the%20fastest%20known,nearly%20in%2Dplace)%20sort.
;; https://builtin.com/machine-learning/fastest-sorting-algorithm
;; 0.a. https://en.wikipedia.org/wiki/Sorting_algorithm#Comparison_sorts
;; "Non-comparison sorts" -> "integer sorting" possibly is inappropriate here.
;; > Some algorithms are slow compared to those discussed above
;; 1. Radix sort is skipped since it can't work for symbol etc. https://stackoverflow.com/a/10604869/21294350

;; Quicksort https://en.wikipedia.org/wiki/Quicksort#Lomuto_partition_scheme
;; 0. (A, lo, p - 1) will be all less than pivot (i.e. A[hi])
;; 1. Here (?? a) etc can't decide element number.
;; One straightforward way may be just match whole and quicksort for it...
;; This can work since at that time the former rule will make all nested sum/product's disappear.

(define algebra-2
  (rule-simplifier
    (list

      ;; Sums

      (rule `(+ (? a)) a)

      ;; This can recursively remove all nested sums until only one + is reserved.
      (rule `(+ (?? a) (+ (?? b)) (?? c))
            `(+ ,@a ,@b ,@c))

      ;; modified
      (rule `(+ (?? a))
            ;; 0. But each time we need to do quick sort again.
            ;; Anyway the original one also has this problem where (?? a) may match from 0 to all which implicitly does bubble sort each time.
            ;; 0.a. Here actually also avoids the overhead of applying rules many times to swap.
            ;; It just swaps all in one rule invocation.
            (let ((sorted-a (quick-sort a expr<?)))
              (and 
                (not (equal? sorted-a a))
                `(+ ,@sorted-a)
                )))


      ;; Products

      (rule `(* (? a)) a)

      (rule `(* (?? a) (* (?? b)) (?? c))
            `(* ,@a ,@b ,@c))

      (rule `(* (?? a))
            (let ((sorted-a (quick-sort a expr<?)))
              (and 
                (not (equal? sorted-a a))
                `(* ,@sorted-a)
                )))
      ; (rule `(* (?? a) (? y) (? x) (?? b))
      ;       (and (expr<? x y)
      ;            `(* ,@a ,x ,y ,@b)))


      ;; Distributive law

      (rule `(* (?? a) (+ (?? b)) (?? c))
            `(+ ,@(map (lambda (x) `(* ,@a ,x ,@c)) b)))


      ;; Numerical simplifications below

      (rule `(+ 0 (?? x)) `(+ ,@x))

      (rule `(+ (? x ,number?) (? y ,number?) (?? z))
            ;; will calculate the value.
            `(+ ,(+ x y) ,@z))


      (rule `(* 0 (?? x)) 0)

      (rule `(* 1 (?? x)) `(* ,@x))

      (rule `(* (? x ,number?) (? y ,number?) (?? z))
            `(* ,(* x y) ,@z))

      )))

;; both * and + needs reordering.
(algebra-2 '(+ (* (+ x 1) 3) -3))
;Value: (* 3 x)