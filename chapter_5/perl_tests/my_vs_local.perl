# https://www.perlmonks.org/?node_id=94007
$a = 3.14159;
{
  local $a = 3;
  print "In block, \$a = $a\n";
  print "In block, \$::a = $::a\n";
}
print "Outside block, \$a = $a\n";
print "Outside block, \$::a = $::a\n";

# https://stackoverflow.com/q/79492046/21294350
sub foo { print "foo: $x\n"; }

# > If we prepend:
my $x = 7;
for $x (1 .. 3) {  # Implicit localisation happens here.
  print "$x\n";
	print "global $::x\n"; # Prints nothing for $::x.
	foo(); # Prints nothing for $x.
}
print "$x\n";  # Prints 7.

print "\n\n2nd:\n";

# https://stackoverflow.com/a/79492975/21294350
# > But if we define the latter defined var as our $x = 7;
our $x = 7;
# $x = 7;
for $x (1 .. 3) {  # Implicit localisation happens here.
  print "$x\n";
	print "global $::x\n";  # Prints 1 .. 3 correspondingly.
	foo(); # Prints 1 .. 3 correspondingly.
}
print "$x\n";  # Prints 7.

print "\n\n3rd:\n";

$x = 7;
for my $x (1 .. 3) {
  print "$x\n";
	print "global $::x\n";  # Always prints 7.
	foo(); # Always prints 7.
}
print "$x\n";  # Prints 7.
