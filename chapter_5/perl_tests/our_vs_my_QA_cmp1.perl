#### CHECKED
sub foo { print "foo: $x\n"; }

## Description
# Here "my $x = 7;" points to lexical.
# https://stackoverflow.com/questions/79495318/what-is-the-difference-between-alias-our-and-the-original-global-variable/79495779#comment140196015_79495779
# "our $x = 7;" and the latter "$x = 7;" both mean "alias to the package variable".
# "$x (1 .. 3)" https://chat.stackoverflow.com/transcript/message/57949475#57949475 maybe https://stackoverflow.com/questions/79492046/the-scope-for-my-var-in-the-non-c-style-for-loop#comment140285067_79492975 helps
# ~~TODO~~ $::x points to package variable trivially. So 7 for code block3 where my won't influence package variable. But for code block2, $x refers to "our $x", so package variable is changed in the loop.
# https://chat.stackoverflow.com/transcript/message/57947591#57947591
# No lexical here, $x in foo points to global.

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
print "outside1: $x\n";  # Prints 7.

# code block3
$x = 8;
for my $x (1 .. 3) {
  print "$x\n";
  print "global $::x\n";  # Always prints 8.
  foo(); # Always prints 8.
}
print "outside2: $x\n";  # Prints 8.