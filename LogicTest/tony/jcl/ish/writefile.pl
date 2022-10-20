#!c:/bat/perl5go.bat
#
#/usr/bin/perl
#

# Write the word test to a file.
use strict;

print '$ENV{COBDIR}: '."$ENV{'COBDIR'}\n";
print '$ENV{TESTVAR}: '."$ENV{'TESTVAR'}\n";

my $dsno1 = '/export/home/mtpadmin/testfile.djna';
$dsno1 =~ tr/A-Z/a-z/;

unless (open(OUT1,">$dsno1")) {
	print "Couldn't open '$dsno1' for output: $!\n";
	exit 8;
}
print OUT1 'test';
close OUT1;

exit 0; 
