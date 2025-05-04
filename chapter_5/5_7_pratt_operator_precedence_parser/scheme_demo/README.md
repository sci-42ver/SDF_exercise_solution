- Download siod in https://download-mirror.savannah.gnu.org/releases/muesli/
  - orig/ is run in siod
  - pratt is from https://aur.archlinux.org/packages/siod#comment-872653 but actually both github and Upstream links there are fine.
- Syntax compatibility with *Python*
  - See SDF_exercises/chapter_5/5_7_naive_algorithm_for_operator_precedence_parser/5_7_precedence_lib.scm for the original intended compatibility
    - i.e. at least one for each level and have the detailed implementation for the top two.
  - See SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/pratt_new_compatible_with_MIT_GNU_Scheme.scm for the compatibility implemented later.
    - just port from SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/orig/pratt_new.scm with `lambda` additions etc.
  - TODO
    - ~~add "optional trailing comma" support.~~ All done by `LeftComma` explicitly or implicitly with `ParseUntil`.
    - ~~See SDF_exercises/chapter_5/5_7_pratt_operator_precedence_parser/scheme_demo/based_on_oilshell/README.md.~~ (finished)
- Python doc description
  - [continuation clause](https://docs.python.org/3/reference/compound_stmts.html#grammar-token-python-grammar-parameter_list) [see](https://web.archive.org/web/20250130172238/https://elhacker.info/manuales/OReilly%204%20GB%20Collection/O'Reilly%20-%20Python%20Cookbook.pdf)
    > beginning statement and continuation clause, respectively, where the latter normally makes sense only if it's an *else or elif*
- **What to do next**
  - See based_on_oilshell
    - [x] https://www.engr.mun.ca/~theo/Misc/pratt_parsing.htm `r` in `p ≤ c.LBP ≤ r`.
    - [x] Implement "language L" in https://tdop.github.io/
      All the related base procedures have been implemented in based_on_oilshell. The detailed led/nud procedures depend on the grammar (especially bp relation) but we don't know about that of "language L".
      - nilfix ~~=> `Null` in `ParserSpec`~~
        - advance => `Eat`
        - right => inherent calls of `ParseUntil` in nud etc
      - prefix => `Null` with maybe more than one `a`
      - infix => `Left`
      - infixr => `LeftRightAssoc`
      - delim is similarly done by `(spec 'Null UNUSED-BASE-BP NullError Null-Error-List)`
        except with `-1` nud rbp prec and the default `0` led lbp prec.
      - `(a getlist b)` => PrsNary/PrsNary*.