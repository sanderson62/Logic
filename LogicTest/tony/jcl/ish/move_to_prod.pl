#!/usr/perl5/bin/perl
#$indir = "/apps/test/cid1t/staging/exe_bat";
#$outdir = "/apps/test/cid1t/exe/bat";
#$outdir = "/apps/test/cid1t/exe/bat";
$mdate = `date +"%m/%d/%Y"`;
chomp($mdate);
print("$mdate\n");
system "ftp -n ntcso1 < /apps/test/cid1t/jcl/ish/ftp-get-mtp";
system "cp /apps/test/cid1t/jcl/ish/cps_moves_to_production.htm /apps/test/cid1t/jcl/ish/filein";
read_and_write_firsthalf();
print ("end first half\n");
$movedfiles = "";
$indir = "/apps/test/cid1t/staging/exe_bat";
chdir($indir);
while (defined ($filename = <*.gnt>)) {
    print ("$filename\n");
    if ($movedfiles ne "") {
       $movedfiles = $movedfiles.", ".$filename;
    } else {
       $movedfiles = $filename;
    }
    print (NEWHTM "<tr>\n");
    print (NEWHTM "<td valign=\"top\" width=\"50%\"><p align=\"center\"><font size=\"3\">$mdate</font></td>\n");
    print (NEWHTM "<td width=\"3%\"></td>\n");
    print (NEWHTM "<td valign=\"top\" width=\"50%\"><p align=\"center\"><font size=\"3\">$filename</font></td>\n");
    print (NEWHTM "</tr>\n");
    system "mv $filename /apps/test/cid1t/exe/bat";
}
read_and_write_secondhalf();


sub read_and_write_firsthalf {
   open (OLDHTM,"/apps/test/cid1t/jcl/ish/filein") || die "open of in htm failed: $!";
   open (NEWHTM,">/apps/test/cid1t/jcl/ish/fileot") || die "open of out htm failed: $!";
   $compstr = "insert new moves here";
   $frsthalf = "y";
#   while ($frsthalf eq "y" && ! (eof(OLDHTM))) {
    while ($frsthalf eq "y") {
      $rec1 = <OLDHTM>;
      chomp($rec1);
      if ($rec1 =~ /$compstr/) {
      	 print ("found match\n");
      	 print ("$rec1\n");
      	 print ("$compstr\n");
         $frsthalf = "n";
         print (NEWHTM "$rec1\n");
         $rec1 = <OLDHTM>;
         chomp($rec1);
         print (NEWHTM "$rec1\n");
      } else {
         print (NEWHTM "$rec1\n");
      }
   }
}

sub read_and_write_secondhalf {
   print("begin second half\n");
   until (eof(OLDHTM)) {
      $rec1 = <OLDHTM>;
      chomp($rec1);
      print (NEWHTM "$rec1\n");
   }
}

close(OLDHTM) || die "close of in htm failed: $!";
close(NEWHTM) || die "close of new htm failed: $!";
$infilesz = -s "/apps/test/cid1t/jcl/ish/filein";
print("in file size "."$infilesz\n");
$otfilesz = -s "/apps/test/cid1t/jcl/ish/fileot";
print("out file size "."$otfilesz\n");
print("$movedfiles\n");
if ($infilesz != $otfilesz) {
   print("file size differs\n");
   system "cp /apps/test/cid1t/jcl/ish/fileot /apps/test/cid1t/jcl/ish/cps_moves_to_production.htm";
   system "ftp -n ntcso1 < /apps/test/cid1t/jcl/ish/ftp-put-mtp";
   $cmd = "smtp -f slunikix -t pema -s "."'$movedfiles'"." -mf /apps/test/cid1t/emails/move_to_prod.txt";
   print("$cmd\n");
   system $cmd;
}
print("Press Enter to End"); <stdin>;
