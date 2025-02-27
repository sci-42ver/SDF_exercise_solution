;; https://srfi.schemers.org/srfi-9/srfi-9.html
;; whether "constructor-tag *must* be same as field-tag"
(define-record-type <*compound-procedure>
    (make-compound-procedure vars* bproc* env*)
    compound-procedure?
  (vars  procedure-parameters)
  (bproc procedure-body)
  (env   procedure-environment))
;No slot named vars* in records of type #[record-type 12 *compound-procedure].

;; The behaviors of 'type etc.
(define-syntax define-record-type
  (syntax-rules ()
    ((define-record-type type
       (constructor constructor-tag ...)
       predicate
       (field-tag accessor . more) ...)
     ;; 0. https://stackoverflow.com/questions/79098453/how-to-implement-one-anonymous-loop-form-like-do-in-the-evaluator-as-a-derived-e#comment139473764_79098453
     ;; 1. Here 'type is (quote type) for Scheme reader, so it will be also substituted.
     '(begin
       (define type
         (make-record-type 'type '(field-tag ...)))
       (define constructor
         (record-constructor type '(constructor-tag ...)))
       (define predicate
         (record-predicate type))
       (define-record-field type field-tag accessor . more)
       ...))))

(define-record-type <*compound-procedure>
    (make-compound-procedure vars bproc env)
    compound-procedure?
  (vars  procedure-parameters)
  (bproc procedure-body)
  (env   procedure-environment))