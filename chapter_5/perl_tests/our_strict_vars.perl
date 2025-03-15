# from `man perlfunc`
# > with the exception that it will still satisfy strict 'vars' and interpret that variable as the newly aliased package variable if it was *not yet declared* in that scope
package main;
sub foo {print "$_[0],$_[1],$_[2];"}
foo($x, my $x = $x + 1, $x); # fail
foo($x, our $x = $x + 1, $x);

package main; my $x = 2; foo($x, our $x = $x + 1, $x); foo($x, our $z = 5, $z); # the 1st foo() receives (2, 3, 2) while the 2nd receives (3, 5, 5)
