# modification of the original top example in https://stackoverflow.com/q/79492046/21294350

$i = 'samba';
# > If the variable is preceded with the keyword "my", then it is lexically scoped, and is therefore visible only within the loop.
for my $i (1 .. 4) {
    print "$i\n";
}
print "$i\n";
# 1
# 2
# 3
# 4
# samba

# > Otherwise, the variable is implicitly local to the loop and regains its former value upon exiting the loop.
$i = 'samba';
for $i (1 .. 4) {
    print "$i\n";
}
print "$i\n";
# 1
# 2
# 3
# 4
# samba

$i = 'former';
my $i = 'samba';
for $i (1 .. 4) {
    print "$i\n";
}
print "$i\n"; # not use 'former'
# 1
# 2
# 3
# 4
# samba