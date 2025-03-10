my $x = 7;

sub foo { print "foo: $x\n"; }

# code block2
$x = 7;
for $x (1 .. 3) {  # Implicit localisation happens here.
  print "$x\n";
  print "global $::x\n";  # Prints nothing for $::x.
  # TODO why not use dynamic scope here, i.e. 1 .. 3?
  # see https://stackoverflow.com/questions/79495318/what-is-the-difference-between-alias-our-and-the-original-global-variable#comment140196015_79495779
  # > foo() is bound to the 1st lexical at compile time
  foo(); # Prints 7.
}
print $x;  # Prints 7.

# code block3
$x = 7;
for my $x (1 .. 3) {
  print "$x\n";
  print "global $::x\n";  # Prints nothing for $::x.
  foo(); # Always prints 7.
}
print $x;  # Prints 7.