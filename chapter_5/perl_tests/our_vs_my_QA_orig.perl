#### CHECKED
sub foo { print "foo: $x\n"; }

## Description
# Here all "$x = 7" and "$x (1 .. 3)" points to global.
# $::x points to global trivially.
# https://stackoverflow.com/questions/79495318/what-is-the-difference-between-alias-our-and-the-original-global-variable/79495779#comment140200515_79495779
# No lexical here, $x in foo can only access global in the scope [my $x (1 .. 3) is beyond the scope].

# code block2
$x = 7;
for $x (1 .. 3) {  # Implicit localisation happens here.
  print "$x\n";
  print "global $::x\n";  # Prints 1 .. 3 correspondingly.
  foo(); # Prints 1 .. 3 correspondingly.
}
print "outside1: $x\n";  # Prints 7.

# code block3
$x = 7;
for my $x (1 .. 3) {
  print "$x\n";
  print "global $::x\n";  # Always prints 7.
  foo(); # Always prints 7.
}
print "outside2: $x\n";  # Prints 7.
