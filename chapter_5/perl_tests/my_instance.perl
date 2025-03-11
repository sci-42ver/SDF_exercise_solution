# https://stackoverflow.com/questions/79495318/what-is-the-difference-between-alias-our-and-the-original-global-variable/79495779#comment140203984_79495779
my $x = 10;sub bumpx { print $x++ };$x = 20;bumpx();
my $x = 10;sub bumpx { print $x++ };my $x = 20;bumpx();
# my $x = 20;
