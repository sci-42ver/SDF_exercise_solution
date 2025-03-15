# https://stackoverflow.com/questions/79495318/what-is-the-difference-between-alias-our-and-the-original-global-variable/79495779#comment140203984_79495779
# TODO why the latter redefinition will make this fail to output 20. 
my $x = 10;sub bumpx { print "$x++ \n" };$x = 20;bumpx();
my $x = 10;sub bumpx1 { print "$x++ \n" };$x = 20;bumpx1();
my $x = 10;sub bumpx { print "$x++ \n" };my $x = 20;bumpx();
# ++ 
# 20++ 
# 10++ 
