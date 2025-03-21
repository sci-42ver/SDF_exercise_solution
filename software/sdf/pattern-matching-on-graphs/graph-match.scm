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

;;; Graph pattern matcher

;;; <edge> = <edge-label> <target>
;;; <edge-label> = <symbol>
;;; <target> = <node-var> | <object-var> | <constant>
;;; <node-var> = <single-node-var>
;;; <single-node-var> = <single-var>
;;; <object-var> = <single-var> | <sequence-var>
;;; <single-var> = "(?" <var-name>? <unary-predicate>? ")"
;;; <sequence-var> = "(?*" <var-name>? <unary-predicate>? ")"
;;; <var-name> = <symbol>
;;;
;;; <path> = <node-var> <path-elements>
;;; <path-elements> = <path-element>*
;;;
;;; <path-element> =
;;;     <edge>
;;;   | (* <path-elements>)
;;;   | (+ <path-elements>)
;;;   | (opt <path-elements>)
;;;   | (or <ppath-elements>+)
;;;   | (and  <ppath-elements>+)
;;;   | (each <var-name> <path>) -- not implemented
;;;   | (any <var-name> <path>) -- not implemented
;;;
;;; <ppath-elements> = "(" <path-elements> ")"
;;;
;;; (and (* cdr (?* p))
;;;      (each p (car (?* v))))
;;;
;;; (and (* cdr (?* p))
;;;      (any p (car (?* v))))
;;;
;;; (and (x (?) y (? v))
;;;      (y (?) x (? v)))
;;;
;;; (or (a 3)
;;;     (b 3))
;;;
;;; each/any not implemented because they use the value of a
;;; match variable in a way that's different from all other uses.
;;; This is a consequence of there being no explicit scope for
;;; sequence variables, which are implicitly scoped to the
;;; surrounding repeat combinators, so it's never possible to
;;; know when a sequence variable is finished accumulating
;;; values.
;;;
;;; This shouldn't be a major problem since the same behavior can
;;; be implemented using code.

(define (graph-match path dict object)
  ((gmatch:compile-path path) object dict
   (lambda (object* dict*)
     (declare (ignore object*))
     dict*)))

;;;; Compiler

(define (gmatch:var-type? x)
  (or (eq? '? x)
      (eq? '?* x)))

(define (gmatch:compile-path path)
  (if (and (pair? path) (match:element-var? (car path)))
      (gmatch:finish-compile-path (cdr path)
        (gmatch:compile-var (car path)))
      (error "Ill-formed path:" path)))

(define (gmatch:finish-compile-path rest-elts matcher)
  (if (null? rest-elts)
      matcher
      ;; similar to match:list and SICP exercise_codes/SICP/book-codes/ch4-ambeval.scm analyze-sequence.
      (gmatch:seq2 matcher
                   (gmatch:compile-path-elts rest-elts))))

(define (gmatch:anonymous-var? object)
  (and (pair? object)
       (gmatch:var-type? (car object))
       (or (null? (cdr object))
           ;; SKIPPED SDF_exercises TODO when happens
           ;; The book doesn't show this case in the context of "anonymous". 
           (and (pair? (cdr object))
                (not (symbol? (cadr object)))))))

(define (gmatch:named-sequence-var? object)
  (and (list? object)
       (n:>= (length object) 2)
       (eq? '?* (car object))
       (symbol? (cadr object))))

;; added based on SDF_exercises/software/sdf/common/match-utils.scm
(define-generic-procedure-handler match:var?
  (match-args gmatch:anonymous-var?)
  (constant-generic-procedure-handler #t))

(define-generic-procedure-handler match:var?
  (match-args gmatch:named-sequence-var?)
  (constant-generic-procedure-handler #t))

(define-generic-procedure-handler match:var-name
  (match-args gmatch:named-sequence-var?)
  cadr)

(define-generic-procedure-handler match:binding-value
  (match-args
   (lambda (binding)
     (eq? '?* (match:binding-type binding))))
  (lambda (binding)
    (reverse (cadr binding))))

(define (gmatch:compile-path-elts elts)
  (let ((elt (car elts))
        (rest (cdr elts)))
    (cond ((and (symbol? elt) (pair? rest))
           ;; > An edge may be labeled by any symbol that is not one of the special
           ;; > symbols (*, + , opt, or, and) used by graph-matcher patterns.
           ;; 0. IMHO the reasons are that edge related procedures may interfere with those related with the special symbols. 
           ;; SKIPPED SDF_exercises TODO IMHO ((?) * (?)) can be differentiated from ((?) (* ...) (?)).
           ;; So why must "not one of the special symbols"?
           ;; 0.a. For example, opt (will call `gmatch:compile-path-elts`) is called in `gmatch:compile-path-elt` called by `gmatch:compile-path-elts`.
           ;; So only (opt ...) will call the special gmatch:compile-opt.
           ;; But the mere "opt" will match "(symbol? elt)" so that it will call gmatch:compile-edge expectedly.
           ;; 0.b. Also see SDF_exercises/chapter_4/tests/graph_match/*_edge.scm.
           (gmatch:finish-compile-path (cdr rest)
             (gmatch:compile-edge elt (car rest))))
          ((pair? elt)
           (gmatch:finish-compile-path rest
             (gmatch:compile-path-elt elt)))
          (else
           (error "Ill-formed path elements:" elts)))))

(define (gmatch:compile-path-elt elt)
  (let ((keyword (car elt))
        (args (cdr elt)))
    (case keyword
      ((*) (gmatch:compile-* args))
      ((+) (gmatch:compile-+ args))
      ((opt) (gmatch:compile-opt args))
      ((or) (gmatch:compile-or args))
      ((and) (gmatch:compile-and args))
      (else (error "Ill-formed path element:" elt)))))

;; calls gmatch:compile-edge at last.
(define (gmatch:compile-* elts)
  (gmatch:* (gmatch:compile-path-elts elts)))

(define (gmatch:compile-+ elts)
  (let ((matcher (gmatch:compile-path-elts elts)))
    (gmatch:seq2 matcher (gmatch:* matcher))))

(define (gmatch:compile-opt elts)
  (let ((matcher (gmatch:compile-path-elts elts)))
    (define (match-opt object dict succeed)
      (or (matcher object dict succeed)
          (succeed object dict)))
    match-opt))

(define (gmatch:compile-or elt-lists)
  (gmatch:or (map gmatch:compile-path-elts elt-lists)))

(define (gmatch:compile-and elt-lists)
  (gmatch:and (map gmatch:compile-path-elts elt-lists)))

(define (gmatch:* matcher)
  (define (match-* object dict succeed)
    ;; match as more as possible
    (or (matcher object dict
                 (lambda (object* dict*)
                   (match-* object* dict* succeed)))
        (succeed object dict)))
  ; (trace match-*)
  match-*)

;; SKIPPED SDF_exercises TODO what does this comment mean?
;; IMHO it means we always uses (node1 (* ...) node2) for one element-list edge but not (node1 * ... node2).
;; > (* (?)), which will be interpreted as an edge with label *
;; means (gmatch:compile-path-elts (* (?)))
;; > but is really a repeat missing the extra parens.
;; SKIPPED TODO meaning.
;;; Each element-list must be parenthesized, but this means
;;; there's an ambiguity with an edge-like construct of the form
;;; (* (?)), which will be interpreted as an edge with label *
;;; but is really a repeat missing the extra parens.  We can't
;;; detect when the pattern author mis-types such a pattern

(define (gmatch:seq2 match-first match-rest)
  (define (match-seq object dict succeed)
    ;; Here succeed may be not called if failure
    ;; which then returns #f like gmatch:var-matcher does.
    (match-first object dict
                 (lambda (object* dict*)
                  ;  (write-line (list "call match-rest" object* dict*))
                   (match-rest object* dict* succeed))))
  match-seq)

(define (gmatch:or matchers)
  (lambda (object dict succeed)
    (let loop ((matchers matchers))
      (if (pair? matchers)
          (or ((car matchers) object dict succeed)
              (loop (cdr matchers)))
          #f))))

(define (gmatch:and matchers)
  (lambda (object dict succeed)
    (if (null? matchers)
        (succeed object dict)
        (let loop ((matchers matchers) (dict dict))
          ((car matchers) object dict
           (if (null? (cdr matchers))
               succeed
               (lambda (object* dict*)
                 ;; 0. trivially here we call matchers on object instead of object* since we continues to check object.
                 ;; 1. Similar to exercise_codes/SICP/book-codes/ch4-query.scm conjoin, here former bindings may be used later.
                 (loop (cdr matchers) dict*))))))))

;; > starting with the edge object*
;; IGNORE SDF_exercises TODO IMHO here object is node instead of edge.
;; See (succeed object dict*) in match-var where object is the node satisfying restriction which is passed to (lambda (object* dict*) ...).
(define (gmatch:compile-edge label target)
  (let ((match-target (gmatch:compile-target target)))
    (define (match-edge object dict succeed)
      (and (graph-node? object)
           (object 'has-edge? label)
           ;; Update to the next node object.
           (match-target (object 'edge-value label)
                         dict succeed)))
    ; (trace match-edge)
    match-edge))

(define (gmatch:compile-target elt)
  (if (match:var? elt)
      (gmatch:compile-var elt)
      ;; IGNORE SDF_exercises TODO why not use "begin"?
      ;; See begin_diff_let.scm
      (let ()
        ;; compared with SDF_exercises/software/sdf/design-of-the-matcher/matcher.scm
        ;; Here object structure is not one list since we are using *graph* which is connected by bundle.
        (define (match-constant object dict succeed)
          (and (eqv? elt object)
               (succeed object dict)))
        match-constant)))

(define (gmatch:compile-var var)
  (cond ((match-list? var gmatch:var-type?)
         (gmatch:var-matcher (car var) #f #f))
        ((match-list? var gmatch:var-type? symbol?)
         (gmatch:var-matcher (car var) (cadr var) #f))
        ((match-list? var gmatch:var-type? symbol? procedure?)
         (gmatch:var-matcher (car var) (cadr var) (caddr var)))
        ((match-list? var gmatch:var-type? procedure?)
         (gmatch:var-matcher (car var) #f (cadr var)))
        (else
         (error "Ill-formed variable:" var))))

(define (match-list? datum . preds)
  (let loop ((preds preds) (datum datum))
    (if (pair? preds)
        (and (pair? datum)
             ((car preds) (car datum))
             (loop (cdr preds) (cdr datum)))
        ;; better to ensure preds are also empty.
        (null? datum))))

(define (gmatch:var-matcher var-type var-name restriction)
  (define (match-var object dict succeed)
    ; (and (not (restriction object dict))
    ;   (write-line (list "(restriction object dict)" restriction object var-type var-name (restriction object dict))))
    (and (or (not restriction)
             (restriction object dict))
         (if var-name
             (let ((dict*
                    (gmatch:bind var-type var-name object
                                 dict)))
               (and dict*
                    (succeed object dict*)))
             (succeed object dict))))
  match-var)

;;;; Binding

(define (gmatch:bind var-type var-name value dict)
  (let* ((var (match:make-var var-type var-name))
         (binding (match:lookup var dict)))
    ;; These 2 "not" predicates seem to just ensure (eq? ...) able to work.
    ;; Just see how this pred can be true.
    ;; That only happens when binding, (match:var-type var) are both not false.
    ;; And (eq? (match:var-type var) (match:binding-type binding)) is false,
    ;; i.e. adding one binding with the different type.
    (if (not (or (not binding)
                 ;; 0. IGNORE SDF_exercises TODO when this will be #t...
                 ;; 0.a. IMHO this (not (match:var-type var)) is just as the above says, it is one safe programming convention,
                 ;; although it won't happen at all.
                 ;; 0.b. See the above for what this whole pred intends to do.
                 ;; 1. See gmatch:compile-var where only var-name can be #f but it is avoided by the predicate var-name.
                 (not (match:var-type var))
                 (eq? (match:var-type var)
                      (match:binding-type binding))))
        (error "Can't mix variable types:" var binding))
    (case (match:var-type var)
      ((?)
       (if binding
           ;; > bind returns #f to indicate a match failure.
           (and (eqv? value (match:binding-value binding))
                dict)
           (match:extend-dict var value dict)))
      ((?*)
       (if binding
           (match:map-bindings
            (lambda (binding)
              (if (eq? (match:binding-name binding)
                       (match:var-name var))
                  ;; IGNORE SDF_exercises TODO just as * does, we add one more element.
                  ;; In book, ?* is only used for "(* north (?* ,unoccupied))" etc.
                  ;; Also see SICP_SDF/SDF_exercises/chapter_4/4_24_based_on_graph_match_lib.scm.
                  (match:map-binding-value
                   (lambda (values) (cons value values))
                   binding)
                  binding))
            dict)
           (match:extend-dict var (list value) dict)))
      (else
       (error "Unknown variable type:" var)))))