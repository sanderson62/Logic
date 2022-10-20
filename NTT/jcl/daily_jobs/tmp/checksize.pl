#!/usr/perl5/bin/perl
foreach my $file (glob "*.diff") {

  if (-z "$file") {
#  	  print ("File does exist but has zero size "."$file\n");
      $dfile = $file;
#     $file =~ s/cbl\.diff$/\*/;
#  	  print ("About to delete "."$file\n");
#     system "rm $file";
#    	print ("About to delete "."$dfile\n"); <stdin>;
      system "rm $dfile";
  } else {
  	 print ("File does exist with size > 0 "."$file\n");
  }

}
print("Press Enter to End"); <stdin>;
