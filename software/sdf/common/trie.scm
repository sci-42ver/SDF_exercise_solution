#| -*-Scheme-*-

Copyright (C) 2019, 2020, 2021 Chris Hanson and Gerald Jay Sussman

This file is part of SDF.  SDF is software supporting the book
"Software Design for Flexibility", by Chris Hanson and Gerald Jay
Sussman.

SDF is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SDF is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with SDF.  If not, see <https://www.gnu.org/licenses/>.

|#

(define-record-type <trie>
    (%make-trie value edge-alist)
    trie?
  (value %trie-value set-trie-value!)
  (edge-alist trie-edge-alist set-trie-edge-alist!))

(define (make-trie)
  (%make-trie #f '()))

(define (trie-has-value? trie)
  (%trie-value trie))

(define (trie-value trie)
  (or (%trie-value trie)
      (error "Trie node has no value:" trie)))

;; Fetches a node based on a sequence of predicates (path),
;; creating it if needed.
(define (intern-path-trie trie path)
  (let loop ((trie trie) (path path))
    (if (n:pair? path)
        (loop (add-edge-to-trie trie (car path))
              (cdr path))
        trie)))

;; Gets the edge of TRIE for PREDICATE, creating it if needed.
(define (add-edge-to-trie trie predicate)
  (let ((p (assv predicate (trie-edge-alist trie))))
    (if p
        (cdr p)
        (let ((successor (make-trie)))
          (set-trie-edge-alist! trie
                                (cons (cons predicate successor)
                                      (trie-edge-alist trie)))
          successor))))

;; Sets the value of the node identified by PATH to VALUE.
(define (set-path-value! trie path value)
  (set-trie-value! (intern-path-trie trie path) value))

;;; These operations fetch a value based on a sequence of
;;; features.

;; checked
(define (get-matching-tries trie features)
  (let loop ((tries (list trie)) (features features))
    (if (n:pair? features)
        (loop (append-map (lambda (trie)
                            (%find-all-edges trie
                                             (car features)))
                          tries)
              (cdr features))
        tries)))

(define (apply-predicate predicate feature)
  (increment-predicate-count! predicate)
  (predicate feature))

(define (get-all-values trie features)
  (map trie-value (get-matching-tries trie features)))

(define (get-a-value trie features)
  (get-a-value-by-searching trie features))

(define (get-a-value-by-filtering trie features)
  (let ((nodes
         (filter trie-has-value?
                 (get-matching-tries trie features))))
    (if (n:pair? nodes)
        (trie-value (car nodes))
        (error "Unable to match features:" features))))

(define (get-a-value-by-searching trie features)
  (let loop
      ((trie trie)
       (features features)
       ;; This is passed without changes.
       (succeed
        (lambda (value fail)
          (declare (ignore fail))
          value))
       (fail
        (lambda ()
          (error "Unable to match features:" features))))
    (if (n:pair? features)
        (%try-edges (trie-edge-alist trie)
                    (car features)
                    (lambda (trie* fail*)
                      ;; induction: assume fail* will try the adjacent edges sharing the same parent.
                      (loop trie* (cdr features) succeed fail*))
                    ;; The top fail may be called after failing all pred paths.
                    fail)
        ;; Here assume only one path is possible, so if features are all used, then we we finish the loop.
        (if (trie-has-value? trie)
            (succeed (trie-value trie) fail)
            (fail)))))

;; checked
(define (%find-all-edges trie feature)
  (map cdr
       (filter (lambda (p)
                 (apply-predicate (car p) feature))
               (trie-edge-alist trie))))

(define (%try-edges edges feature succeed fail)
  (if (n:pair? edges)
      ;; This implies we will choose the 1st candidate based on Pre-order https://en.wikipedia.org/wiki/Tree_traversal#Pre-order,_NLR.
      (%try-edge (car edges)
                 feature
                 succeed
                 ;; this may be passed down by succeed in `%try-edge` which uses the new trie
                 ;; and may go back to the upper trie by the fail.
                 (lambda ()
                   (%try-edges (cdr edges)
                               feature
                               succeed
                               fail)))
      ;; will try the adjacent trie of the trie with `edges` if calling fail*.
      (fail)))

(define (%try-edge edge feature succeed fail)
  (if (apply-predicate (car edge) feature)
      (succeed (cdr edge) fail)
      ;; will try other edges of the trie.
      (fail)))

(define (trie-entries trie)
  (map (lambda (edge)
         (cons (predicate-name (car edge))
               (trie-entries (cdr edge))))
       (trie-edge-alist trie)))

#|
;;; For example...
;;;                      a-trie
;;;                    /   |   \
;;;                nn?/  s?|  e?\
;;;                  /     |     \
;;;                 nn     s      e
;;;                       / \
;;;                    n?/ s?\
;;;                     /     \
;;;                    sn     ss


(define a-trie (make-trie))

(define (negative-number? x)
  (and (number? x) (negative? x)))

(define nn (add-edge-to-trie a-trie negative-number?))

(define s (add-edge-to-trie a-trie symbol?))

(define (even-number? x)
  (and (number? x) (even? x)))

(define e (add-edge-to-trie a-trie even-number?))

(define sn (add-edge-to-trie s number?))

(define ss (add-edge-to-trie s symbol?))

(set-trie-value! s '(symbol))

(set-trie-value! ss '(symbol symbol))

(get-a-value a-trie '(a 1))
;Trie node has no value: #[trie 69]

(get-a-value a-trie '(a b))
;Value: (symbol symbol)

(get-a-value a-trie '(c))
;Value: (symbol)

(get-all-values a-trie '(3))
;Value: ()

(get-all-values a-trie '(a b))
;Value: ((symbol symbol))

(set-trie-value! sn '(symbol number))

(set-trie-value! nn '(negative-number))

(set-trie-value! e '(even-number))

(get-all-values a-trie '(-4))
;Value: ((even-number) (negative-number))
|#
