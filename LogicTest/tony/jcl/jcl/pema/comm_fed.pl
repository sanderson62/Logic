$filein = $ARGV[0];
$fileot = $ARGV[1];
open(COMMIN,"$filein") || die "Open in failed on $filein: $!";
open(COMMOT, ">$fileot") || die "Open out failed on $fileot: $!";
$in_temp = "A56 A13 A11 A9 A6 A11 A172 A11 A11 A10 A10 A8 A3 A11 A11 A70 A52 A7";
$spaces = "     ";
while (<COMMIN>) {
   chomp($in_rec = $_);
   @fields = unpack($in_temp, $in_rec);
   $fields[1] =~ s/\.//;
   $fields[2] =~ s/\.//;
   $fields[3] =~ s/\.//;
   $fields[5] =~ s/\.//;
   $fields[7] =~ s/\.//;
   $fields[8] =~ s/\.//;
   $fields[9] =~ s/\.//;
   $fields[10] =~ s/\.//;
   $fields[11] =~ s/\.//;
   $fields[13] =~ s/\.//;
   $fields[14] =~ s/\.//;
   printf COMMOT "%56s%011u%09u%07u%6s%09u%172s%09u%09u%08u%08u%06u%3s%09u%09u%69s%52s%7s%341s\n", @fields, $spaces;
}
close COMMOT;
close COMMIN;
#print("Press Enter to End"); <stdin>;
