# https://stackoverflow.com/a/56239802/21294350

use warnings;
use strict;
use feature qw{ say };

my $string = '((((((...)))(((...))))))';

my @output;
my @stack;

my $pos = 0;
for my $char (split //, $string) {
    if ($char eq '(') {
        push @stack, $pos;
    } elsif ($char eq ')') {
        push @output, [ pop @stack, $pos ];
    }
    ++$pos;
}
# See @$name in perlref, IMHO same as @{$_}.
say "@$_" for sort { $a->[0] <=> $b->[0] } @output;
# say "$_" for sort { $a->[0] <=> $b->[0] } @output;
# say "@{$_}" for sort { $a->[0] <=> $b->[0] } @output;
