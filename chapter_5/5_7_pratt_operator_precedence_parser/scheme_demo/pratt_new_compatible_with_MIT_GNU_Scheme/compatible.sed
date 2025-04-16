s/'else/else/g
s/(token-peek stream) else/(token-peek stream) 'else/g
# '(,) can't work. Similarly for '(()
s/#\.COMMA/COMMA/g
# IGNORE default to use BRE, so ) is the bare ).
s/#\.CLOSE-PAREN/CLOSE-PAREN/g
s/#\.OPEN-PAREN/OPEN-PAREN/g
s/#\.QUOTE/QUOTE-SYMBOL/g
s/#\.SEMICOLON/SEMICOLON/g
s/cons-array/make-strong-eq-hash-table/g
s/#(())/empty-hash-table/g

# (OPEN-PAREN|CLOSE-PAREN|COMMA) => ,$1 for some places.
