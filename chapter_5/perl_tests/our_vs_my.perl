# https://stackoverflow.com/a/17710075/21294350
# TODO "fields pragma"

use feature qw/say/;

# our $foo = 42; # give some value
$foo = 42; # give some value
my  $foo = -1; # same name, different value
say "my  gives $foo";
# our $foo;      # reintroduce the alias; shadow lexical
# $foo = 42;
$foo;
say "our gives $foo";

# https://stackoverflow.com/q/79492046/21294350
sub foo { print "foo: $x\n"; }
my $x = 7;
for $x (1 .. 3) {  # Implicit localisation happens here.
  print "$x\n";
  print "global $::x\n"; # Prints nothing for $::x.
  foo(); # Prints nothing for $x.
}
print $x;  # Prints 7.

$x = 7;
for $x (1 .. 3) {  # Implicit localisation happens here.
  print "$x\n";
  print "global $::x\n";  # Prints 1 .. 3 correspondingly.
  foo(); # Prints 1 .. 3 correspondingly.
}
print $x;  # Prints 7.

# https://stackoverflow.com/a/79492975/21294350
# > then sub f { say $x } would capture, which would greatly complicate the explanation
my $x = 9;

sub f { say $x; }         # Accesses the only `$x`

for $x ( 1 .. 3 ) {       # Backs up and restores the SV associated with `$x`.
   say $x;                # Accesses the only `$x`
   f();
}

say $x;                   # Accesses the only `$x`

our $x = 9;

sub f { say $x; }         # Accesses the only `$x`

for $x ( 1 .. 3 ) {       # Backs up and restores the SV associated with `$x`.
   say $x;                # Accesses the only `$x`
   f();
}

say $x;                   # Accesses the only `$x`

# > use experimental qw( declared_refs defer refaliasing );
# used by `my \$backup = \$x;` where refaliasing mean ref aliasing.
