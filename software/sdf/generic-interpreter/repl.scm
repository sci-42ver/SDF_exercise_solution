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

;;;; Read-eval-print loop for extended Scheme interpreter

(define (repl)
  (check-repl-initialized)
  (let ((input (g:read)))
    ;; added
    (newline)
    ;; different from SICP due to
    ;; 0. no output-prompt
    ;; 1. IGNORE SDF_exercises TODO how to handle the possible loop in env.
    ;; see SDF_exercises/chapter_5/tests/write_line_loop.scm
    (write-line (g:eval input the-global-environment))
    (repl)))

(define (load-library filename)
  (check-repl-initialized)
  (call-with-input-file filename
    (lambda (port)
      (let lp ()
        ;; 0. >  It returns the next object parsable from the given textual input port, updating port to point to the first character past the end of the external representation of the object.
        ;; So this "read" will finally read all definitions etc in the file.
        ;; 1. Compared with g:read=>prompt-for-command-expression, this lacks "flush the output buffer".
        (let ((input (read port)))
          ;; > If an end of file is encountered after the beginning of an object’s written representation, but the written representation is incomplete and therefore not parsable, an error is signalled. 
          ;; "written representation" is used for output instead of input (see https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref.pdf p215).
          ;; It is also not in R7RS https://standards.scheme.org/corrected-r7rs/r7rs-Z-H-8.html#TAG:__tex2page_index_1000.
          (if (not (eof-object? input))
              (begin
                ;; similar to the above.
                (write-line
                 (g:eval input the-global-environment))
                (lp))
              'done))))))