#opendir DH, $dir or die "cannot open $dir; $!";
foreach my $file (glob "*.cbl") {
   $difil = "$file"."."."diff";
#	print " diff file is $difil\n";
   if (-e "/slunikix/apps/prod/cid1p/src/batch/$file") {
      system "diff /slunikix/apps/prod/cid1p/src/batch/$file $file > $difil";
#      if (-z "$difil") {
#         system "rm $difil";
#      }
   } else {
      print ("File does not exist in prod "."$file\n");
   }
}
#}   
print("Press Enter to End"); <stdin>;
