$indir = "/apps/test/cid1t/src/batch";
chdir($indir);
while (defined($filename = (*T.cbl))) {
   $cntr = 0;
   print ("$filename");
}   
print("Press Enter to End"); <stdin>;
