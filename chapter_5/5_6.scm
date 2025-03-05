;;;; based on https://mdl-language.readthedocs.io/en/latest/01-basic-introduction/

;;; > a symbol is assumed to be self evaluating
;; "symbol" not found

;;; > variables to be looked up are distinguished with a prefix character.
;; https://mdl-language.readthedocs.io/en/latest/04-values-of-atoms/#432-lval-1
;; IGNORE i.e. at least LVAL/GVAL
;; https://mdl-language.readthedocs.io/en/latest/04-values-of-atoms/#44-value
;; no prefix...

;;; > Also, in MDL a combination is a special form, but with an implied keyword.

;; https://mdl-language.readthedocs.io/en/latest/03-built-in-functions/#31-representation-1
;; > A MDL object which is used to represent the application of a function to its arguments is an argument of TYPE FORM
;; again is one form as Scheme

;; https://mdl-language.readthedocs.io/en/latest/03-built-in-functions/#33-built-in-functions-type-subr-type-fsubr-1
;; "implied keyword" => subr/fsubr which is same as Scheme, i.e. no explicit sign about whether applicative order.
;; https://mdl-language.readthedocs.io/en/latest/05-simple-functions/#55-examples-comments-1
;; is just based on FSUBR/SUBR

;; "an implied keyword" may mean something like #IMPLIED in https://www.w3schools.com/Xml/xml_dtd_attributes.asp
;; but the above 1st link only shows
;; > < func arg-1 arg-2 ... arg-N >

;;;; After all based on the obscure doc and no available downloads in 'mdl "model development language" download'...
;; also for Arch Linux https://aur.archlinux.org/packages?O=0&SeB=nd&K=Model+Development+Language&outdated=&SB=p&SO=d&PP=50&submit=Go https://archlinux.org/packages/?sort=&arch=any&q=Model+Development+Language&maintainer=&flagged=
;; also for github https://github.com/search?q=language%3Amdl&type=repositories
;; also implied by wikipedia
;; > Although MDL is obsolete

;; I don't know what to do at all...
;; Anyway
;; > just by changing the *syntax* definitions
;; This should be much easier than other exercises IMHO.
