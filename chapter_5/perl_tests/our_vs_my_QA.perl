#### CHECKED
sub foo { print "foo: $x\n"; }

## Description
# Here all "$x = 7", "my $x = 7;" and "$x (1 .. 3)" points to lexical. (The 3rd see https://chat.stackoverflow.com/transcript/message/57948075#57948075)
# $::x and $x in foo points to global trivially. So undefined at least for code block3.
# https://stackoverflow.com/questions/79495318/what-is-the-difference-between-alias-our-and-the-original-global-variable/79495779#comment140200515_79495779
# No lexical here, $x in foo can only access global in the scope [my $x (1 .. 3) is beyond the scope].

# With this assignment added, all the following $::x and $x of foo in block2 and block3 are all empty.
# But without it, those work as comments there show.
my $x = 7;

# code block2
$x = 7;
for $x (1 .. 3) {  # Implicit localisation happens here.
  print "$x\n";
  print "global $::x\n";  # Prints nothing for $::x.
  foo(); # Prints nothing for $x.
}
print $x;  # Prints 7.

# code block3
$x = 7;
for my $x (1 .. 3) {
  print "$x\n";
  print "global $::x\n";  # Prints nothing for $::x.
  foo(); # Prints nothing for $x.
}
print $x;  # Prints 7.