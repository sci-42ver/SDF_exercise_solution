my $undef;
print $undef."\n";
# https://stackoverflow.com/a/35124662/21294350
# perl defaults to have undef for all uninitiated array elems.
# See https://en.wikipedia.org/wiki/Autovivification# for difference from other programming languages.
print $undef || [];
