# Perl script to add newlines to a fixed format text file
if ($#ARGV < 1) {
   print "SCRIPT requires two input parameters\n";
   print "   an input file name and an output file name\n";
   print "   and an optional third parameter giving\n";
   print "   output line size (default 80).\n";
   print "\n";
   exit 8;
}
if ($#ARGV >= 2) {
   $glgth = $ARGV[2];
} else {
   $glgth = 80;
}
unless (open(IN,"<$ARGV[0]")) {
   print "Couldn't open file '$ARGV[0]' for input: $!\n";
   exit 8;
}
binmode IN;
unless (open(OT,">$ARGV[1]")) {
   print "Couldn't open file '$ARGV[1]' for output: $!\n";
   exit 8;
}
while ($got = read(IN,$buf,$glgth)) {
   print OT "$buf\x0a";
}
close IN;
close OT;
exit 0;

