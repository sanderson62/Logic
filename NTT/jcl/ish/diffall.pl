#!/usr/perl5/bin/perl
foreach my $file (glob "*") {
#	print "one file is $file\n";
#	$test_file = $file;
#	$file =~ s/T\./\./;
#	print " prod name is $file\n";
	$difil = "$file"."."."diff";
#	print " diff file is $difil\n";
  if (-e "/apps/test/mdoff/jcl/daily_jobs/$file") {
        system "diff /apps/test/mdoff/jcl/daily_jobs/$file $file > $difil";
  } else {
  	 print ("File does not exist in mdof "."$file\n");
  }
}
print("Press Enter to End"); <stdin>;
