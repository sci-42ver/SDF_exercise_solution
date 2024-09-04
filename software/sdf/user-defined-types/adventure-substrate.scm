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

;;;; Properties

(define (make-property name . plist)
  (guarantee n:symbol? name)
  (guarantee property-list? plist)
  (%make-property name
                  (get-predicate-property plist)
                  (get-default-supplier-property plist)))

(define (property-list? object)
  (and (plist? object)
        ;; SDF_exercises TODO when will this be used?
       (<= (count (lambda (keyword)
                    (not (default-object?
                           (plist-value object keyword))))
                  property-default-keywords)
           1)))

(define (get-predicate-property plist)
   (let ((predicate (plist-value plist 'predicate)))
     (if (not (default-object? predicate))
         predicate
         any-object?)))

;; See `(type-instantiator troll?)` where `(property-optional? property)` is #f since we have no default for fallbakc.
(define (get-default-supplier-property plist)
  (let ((value (plist-value plist 'default-value))
        (supplier (plist-value plist 'default-supplier))
        (property (plist-value plist 'default-to-property)))
    ;; Here we have 3 choices for the default case with implied order by cond.
    (cond ((not (default-object? value))
           (lambda (lookup) value))
          ((not (default-object? supplier))
           (lambda (lookup) (supplier)))
          ((not (default-object? property))
           (lambda (lookup) (lookup property)))
          (else #f))))

(define property-default-keywords
  '(default-value default-supplier default-to-property))

(define property-keywords
  `(predicate ,@property-default-keywords))

(define-record-type <property>
    (%make-property name predicate default-supplier)
    property?
  (name property-name)
  (predicate property-predicate) ; this is noe used.
  (default-supplier property-default-supplier))

(define (property-optional? property)
  (if (property-default-supplier property) #t #f))

(define-record-printer <property>
  (lambda (property)
    (list (property-name property))))

;;;; Types

;; checked
;; notice if this is called multiple times with the same name, it will always return one new `type` implied by %make-simple-tag in `simple-abstract-predicate`.
;; So the following `%set-type-properties!` will always work without `(not (eqv? metadata* metadata))`.
(define (make-type name properties)
  (guarantee-list-of property? properties)
  (let ((type (simple-abstract-predicate name instance-data?)))
    (%set-type-properties! type properties)
    type))

(define (get-binding property instance)
  ;; For troll, tagged-data-data is instance-data whose predicate-accessor will return proc when called.
  (instance-data-binding property (tagged-data-data instance)))


;;; Simplified interface for text -- GJS

;; For troll, here object is troll.
(define (get-property-value property object)
  ((get-binding property object)))

(define (set-property-value! property object value)
  ((get-binding property object) value))


;; For autonomous-agent, it may have thing:location.
;; IMHO this is similar to OOP Inheritance.
(define (type-properties type)
  (append-map %type-properties
              (cons type (all-supertypes type))))

(define (all-supertypes type)
  ;; call `get-all-tag-supersets` which recursively gets supersets of all levels up.
  (filter type? (all-predicate-supersets type)))

(define type?)
(define %type-properties)
(define %set-type-properties!)
(let ((association (make-metadata-association)))
  (set! type? (association 'has?))
  (set! %type-properties (association 'get))
  (set! %set-type-properties! (association 'put!)))

;;;; Instantiation

;; create-troll is the following object (tag instance-data) where instance-data is (tag proc) where proc is to modify property.
(define (type-instantiator type)
  (let ((constructor (predicate-constructor type)) ; accepts instance-data?
        (properties (type-properties type)))
    (lambda plist
      ; (display (list (predicate-name type) properties))
      ; (newline)
      (let ((object
            ;; see `troll?` which uses `instance-data?`
             (constructor (parse-plist plist properties))))
        ;; set-up! only called here.
        (set-up! object)
        object))))

;;; TODO: use properties as the keys in the plist.
(define (parse-plist plist properties)
  (define (lookup-value property)
    ;; For troll, property is troll:hunger and property-name is hunger.
    (let ((value (plist-value plist (property-name property))))
      (if (default-object? value)
          (begin
            (if (not (property-optional? property))
                (error "Missing required property:"
                       (property-name property)
                       plist))
            ;; if optional, fallback to default.
            ((property-default-supplier property) lookup-value))
          value)))
  ; (display
  ;   (map (lambda (property)
  ;         (cons property (lookup-value property)))
  ;       properties))
  ; (newline)
  (make-instance-data
   (map (lambda (property)
          (cons property (lookup-value property)))
        properties)))

(define set-up!
  (chaining-generic-procedure 'set-up! 1
    (constant-generic-procedure-handler #f)))

(define tear-down!
  (chaining-generic-procedure 'tear-down! 1
    (constant-generic-procedure-handler #f)))

;;;; Instance data

(define instance-data?
  (simple-abstract-predicate 'instance-data procedure?))

(define make-instance-data
  (let ((constructor
         (predicate-constructor instance-data?)))
    (lambda (bindings)
      (constructor
        ;; > It is an association from the properties of this type to their values.
        ;; This is different from `make-metadata-association` which is based on hash table.
        ;; Here it is just based on `bindings` which is one alist which may be similar to hash table implied by `	hash-table->alist`.
       (lambda (#!optional property)
         (if (default-object? property)
             (map car bindings) ; 1st binding
             (let ((p (assv property bindings)))
               (if (not p)
                   (error "Unknown property:" property))
               (lambda (#!optional new-value)
                ;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Lambda-Expressions.html#index-_0023_0021optional-1
                ;; > If there are fewer arguments than optional parameters, the unmatched parameters are bound to special objects called default objects.
                ;; Here `(default-object? new-value)` is #t when no new-value is got.

                ;; IMHO here we need to check new-value with property-predicate
                 (if (default-object? new-value)
                     (cdr p)
                     (set-cdr! p new-value))))))))))

(define instance-data-bindings
  ;; i.e. `(lambda (#!optional property) ...)`
  (predicate-accessor instance-data?))

(define (instance-data-properties instance-data)
  ((instance-data-bindings instance-data)))

;; returns one one-arg proc which either gets the data or modify.
;; See `get-property-value`.
(define (instance-data-binding property instance-data)
  ((instance-data-bindings instance-data) property))

;;;; Methods

;; checked
;; property may be something like thing:location.
;; get-property-value -> get-binding extracts instance-data (see type-instantiator, this will have property related infos).
;; Then call instance-data-binding which will return one proc.
(define (property-getter property type)
  (let ((procedure
         (most-specific-generic-procedure
          (symbol 'get- (property-name property))
          1
          #f)))
    (define-generic-procedure-handler procedure
      (match-args type)
      (lambda (object)
        (get-property-value property object)))
    procedure))

;; checked
(define (property-setter property type value-predicate)
  (let ((procedure
         (most-specific-generic-procedure
          (symbol 'set- (property-name property) '!)
          2
          #f)))
    (define-generic-procedure-handler procedure
      (match-args type value-predicate)
      (lambda (object value)
        ;; See get-property-value
        (let ((binding (get-binding property object)))
          (%binding-set-prefix property value (binding) object)
          (binding value))))
    procedure))

;; checked
(define (%binding-set-prefix property new-value old-value object)
  (if debug-output
      (begin
        (send-message! (list ";setting" (possessive object)
                             (property-name property)
                             "to" new-value)
                       debug-output)
        (send-message! (list ";previous value was" old-value)
                       debug-output))))

(define (property-modifier property type value-predicate
                           noun modifier)
  (let ((procedure
         (most-specific-generic-procedure
          (symbol (property-name property) '- noun)
          2
          #f)))
    (define-generic-procedure-handler procedure
      (match-args type value-predicate)
      (lambda (object item)
        (let* ((binding (get-binding property object))
               (old-value (binding))
               (new-value (modifier item old-value)))
          (%binding-set-prefix property new-value old-value
                               object)
          (binding new-value))))
    procedure))

;; here we adds object meeting value-predicate to object meeting type.
(define (property-adder property type value-predicate)
  (property-modifier property type value-predicate 'adder
                     (lambda (value values)
                       (lset-adjoin eqv? values value))))

;; create proc with name `(property-name property)`-remover.
(define (property-remover property type value-predicate)
  (property-modifier property type value-predicate 'remover
                     (lambda (value values)
                       (delv value values))))

;;; Misc

(define (direction? object)
  (if (memv object known-directions) #t #f))
(register-predicate! direction? 'direction)

(define known-directions
  '(north south east west in out up down skew))

(define (display-to-string object)
  ;; https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/String-Ports.html#index-call_002dwith_002doutput_002dstring
  ;; > When procedure returns, call-with-output-string returns the port’s accumulated output as a string.
  (call-with-output-string
    (lambda (port)
      (display object port))))

(define (random-choice items)
  (guarantee n:list? items)
  (and (n:pair? items)
       (list-ref items (random (length items)))))

(define (random-number n)
  ;; > random-bias chooses a number (in this case 1, 2, or 3)
  (n:+ (random n) 1))

(define (bias? object)
  (and (n:real? object)
       (n:<= 0 object 1)))
(register-predicate! bias? 'bias)

(define (random-bias weight)
  (n:/ 1 (random-number weight)))

(define (flip-coin bias)
  (n:>= (random 1.0) bias))

;;; Base object type

(define object:name
  (make-property 'name))

(define object:description
  (make-property 'description
                 'default-to-property object:name))

(define object?
  (make-type 'object (list object:name object:description)))

;; checked
(define get-name
  (property-getter object:name object?))

(define get-description
  (property-getter object:description object?))

(define (find-object-by-name name objects)
  (find (lambda (object)
          (eqv? name (get-name object)))
        objects))

(define-generic-procedure-handler tagged-data-representation
  (match-args object?)
  (lambda (super object)
    (append (super object)
            (list (get-name object)))))

(define-generic-procedure-handler tagged-data-description
  (match-args object?)
  (lambda (object)
    (let ((instance-data (tagged-data-data object)))
      (map (lambda (property)
             (list (property-name property)
                   ((instance-data-binding property
                                           instance-data))))
           (instance-data-properties instance-data)))))

;;; Messaging

(define send-message!
  (most-specific-generic-procedure 'send-message! 2 #f))

(define (narrate! message person-or-place)
  ;; will call (message? place?) -> (message? avatar?) -> (message? screen?)
  ;; where the last will add " " between elements of `message`.
  
  ;; Here it just display the message for each adjacent person.
  (send-message! message
                 (if (person? person-or-place)
                     (get-location person-or-place)
                     person-or-place))
  ;; See (enable-debugging) -> screen:port which is (current-output-port).
  (if debug-output
      (send-message! message debug-output)))

;; checked: i.e. tell message to person only
(define (tell! message person)
  ;; See (match-args message? avatar?) here we only show message for `person`.
  (send-message! message person)
  (if debug-output
      (send-message! message debug-output)))

;; checked: i.e. person say's message to others nearby.
(define (say! person message)
  (narrate! (append (list person "says:") message)
            person))

(define (announce! message)
  (for-each (lambda (place)
              (send-message! message place))
            (get-all-places))
  (if debug-output
      (send-message! message debug-output)))

(define debug-output #f)

(define (enable-debugging)
  (if (not debug-output)
      ;; Here for screen:port we uses (default-supplier), i.e. (current-output-port).
      ;; See parse-plist
      (set! debug-output (make-screen 'name 'debug))))

(define (disable-debugging)
  (if debug-output
      (set! debug-output #f)))

(define (display-message message port)
  (guarantee message? message 'display-message)
  (if (pair? message)
      (begin
        (fresh-line port)
        (display-item (car message) port)
        (for-each (lambda (item)
                    (display " " port)
                    (display-item item port))
                  (cdr message)))))

(define (display-item item port)
  (display (if (object? item) (get-name item) item) port))

(define (message? object)
  (n:list? object))
(register-predicate! message? 'message)

(define (possessive person)
  (string-append (display-to-string (get-name person))
                 "'s"))

;;; Screen

(define screen:port
  (make-property 'port
                 'predicate output-port?
                 ;; we need to call (current-output-port) https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Ports.html#index-current_002doutput_002dport
                 ;; > These procedures are parameter objects
                 'default-supplier current-output-port))

(define screen?
  (make-type 'screen (list screen:port)))
(set-predicate<=! screen? object?)

(define make-screen
  (type-instantiator screen?))

(define get-port
  (property-getter screen:port screen?))

(define-generic-procedure-handler send-message!
  (match-args message? screen?)
  (lambda (message screen)
    (display-message message (get-port screen))))

;;; Clock

(define (make-clock)
  (%make-clock 0 '()))

(define-record-type <clock>
    (%make-clock current-time things)
    clock?
  (current-time current-time set-current-time!)
  (things clock-things set-clock-things!))

(define (register-with-clock! thing clock)
  (set-clock-things! clock
                     (lset-adjoin eqv?
                                  (clock-things clock)
                                  thing)))

(define (unregister-with-clock! thing clock)
  (set-clock-things! clock
                     (delv thing (clock-things clock))))

(define (tick! clock)
  (set-current-time! clock (n:+ (current-time clock) 1))
  (for-each (lambda (thing) (clock-tick! thing))
            (clock-things clock)))

(define clock-tick!
  (chaining-generic-procedure 'clock-tick! 1
    (constant-generic-procedure-handler #f)))

;; See 3_17.scm, it seems "action" object is stored. So later redefined action needs to be reloaded by calling `define-clock-handler` again.
(define (define-clock-handler type action)
  (define-generic-procedure-handler clock-tick!
    (match-args type)
    (lambda (super object)
      (super object)
      (action object))))