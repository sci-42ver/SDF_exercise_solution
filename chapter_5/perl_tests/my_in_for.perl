# https://stackoverflow.com/q/79492046/21294350

$i = 'samba';
# > If the variable is preceded with the keyword "my", then it is lexically scoped, and is therefore visible only within the loop.
for (my $i = 1; $i <= 4; $i++) {
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
# $inner = 'samba';
# see https://stackoverflow.com/a/79492063/21294350
my $inner = 'samba';
for ($i = 1; $i <= 4; $i++) {
    $inner = $i + 1;
}
# > "former" means the last loop iteration.
print "inner: $inner\n";
# inner: 5

$i = 'samba';
$inner = 'samba';
for ($i = 1; $i <= 4; $i++) {
    my $inner = $i + 1;
}
# > but it's still localized to the loop.
# i.e. $inner can't be accessed outside.
print "inner: $inner\n";
# inner: samba
