;; a. 3rd para GNU https://www.gnu.org/software/sed/manual/html_node/BRE-vs-ERE.html
;; Similar for POSIX https://www.baeldung.com/linux/bre-ere-pcre-syntax "One distinct characteristic ..."
;;    But plus "In addition to these operators ..." (+,|,? are not supported by BRE)
;; Also see https://en.wikipedia.org/wiki/Regular_expression#POSIX_basic_and_extended (same as link 1) from https://eli.thegreenplace.net/2012/11/14/some-notes-on-posix-regular-expressions
;;    lazy match depends on implementation.
;;    "or lack of \d instead of POSIX [:digit:]": This is in 9.3.5 so both support it. `echo 1 | grep -e '[[:digit:]]' -` or `... -E ...` both work.
;; b. Use one arg.
;; > How can we maintain the abstract layer that is independent of the target regular expression language?
;;    IMHO use one regex_cfg to cfg all internal language implementation, then call with no language arg later.
