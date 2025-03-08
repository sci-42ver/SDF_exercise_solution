# not see https://stackoverflow.com/a/24161889/21294350 which is about raku, a.k.a. perl 6.

# https://stackoverflow.com/a/30978813/21294350
use Data::Dumper;
use feature 'say';

say "ref";
my $a_ref = [1,2,3,4,5,6];
# > Because @a is a single element array, containing a reference
say $a_ref;
say Dumper $a_ref;
# dereference (notice to use @ to do dereference for)
# say Dumper ${$a_ref};
say Dumper @{$a_ref};

say "array";
my @a = (1,2,3,4,5,6);
print "a: ".Dumper(@a)."\n";

say "1-elem array";
my @a = (1);
# pass the entire array
print "a: ".Dumper(@a)."\n";
# pass elems in range 0 (i.e. the 1st elem) in array
print "a: ".Dumper(@a[0])."\n";
# pass the 1st elem in array
print "a: ".Dumper($a[0])."\n";

say "1-elem array with ref-elem";
my @a = [2,2,3,4,5,6];
# man perlintro
# > using @array where Perl expects to find a scalar value ("in scalar context") will give you the number of elements in the array
print @a."\n";
print "a: ".Dumper(@a)."\n";
print "a: ".Dumper(@a[0])."\n";
print "a: ".Dumper($a[0])."\n";

# perlref
# > which is then dereferenced by "@{...}"
say Dumper @{$a[0]};

