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

;;; An example of looking for lines with r:seq in the regexp.scm file

(pp (r:grep (r:quote "r:seq") "regexp.scm"))
("  (r:seq"
 "    ((0) (r:seq))"
 "(define (r:seq . exprs)"
 "      (apply r:seq"
 "      (r:seq)))"
 "  (apply r:seq"
 "(pp (r:grep (r:seq (r:quote \"a\") (r:dot) (r:quote \"c\")) \"tests.txt\"))"
 " (r:grep (r:seq \" \""
 "    (r:seq (r:bol)")
;Unspecified return value
