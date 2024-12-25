s/(first2 (car terms2)) (rest2 (cdr terms2))//g
s/ terms2//g
s/ first2//g
s/ rest2//g
s/(assert (null? null2))//g
s/ null2//g
# https://www.gnu.org/software/sed/manual/html_node/Regular-Expressions.html
s/^ \+(car-satisfies .*))/)/g
# https://stackoverflow.com/a/1252010
# s/\n)/)/g
