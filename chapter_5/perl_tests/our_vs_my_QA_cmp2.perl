sub foo { print "foo: $x\n"; }

# With this assignment added, all the following $::x and $x of foo in block2 and block3 are all empty.
# But without it, those work as comments there show.
my $x = 7;

# code block2
# implicitly set the global var $::x
our $x = 7;
for $x (1 .. 3) {  # Implicit localisation happens here.
  print "$x\n";
  print "global $::x\n";  # Prints 1 .. 3 correspondingly.
  foo(); # Prints 1 .. 3 correspondingly.
}
print $x;  # Prints 7.

# code block3
$x = 7;
for my $x (1 .. 3) {  # Implicit localisation happens here.
  print "$x\n";
  print "global $::x\n";  # Always prints 7.
  foo(); # Always prints 7.
}
print $x;  # Prints 7.