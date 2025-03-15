use feature qw/say/;
# https://chat.stackoverflow.com/transcript/message/57952011#57952011
# man perlsyn
# > Any simple statement may optionally be followed by a SINGLE modifier
# my res1 = $$_ for map { my $x = $_; \$x } 1..3;
# say res1;

say $$_ for map { my $x = $_; \$x } 1..3;
# i.e.
my $x = 1; 
# > at scope exit, if there are any references to it remaining, a new one is created
# "put in place of the old one" is implicitly shown by `our $x_ref1 = \$x;` etc although being one unidirectional assignment to $x_ref1 instead of $x.
$x_ref1 = \$x;
# > the old one lives a separate life.
# i.e. can't be used later explicitly.
my $x = 2; $x_ref2 = \$x;
my $x = 3; $x_ref3 = \$x;
map {say $$_ } ($x_ref1, $x_ref2, $x_ref3);

use feature qw/say/;{my $x = 1; our $x_ref1 = \$x;} {my $x = 2; our $x_ref2 = \$x;} {my $x = 3; our $x_ref3 = \$x;} map {say $$_ } ($x_ref1, $x_ref2, $x_ref3, $x);
