#!/usr/perl5/bin/perl
$mdate = `date +"%Y%m%d"`;
chomp($mdate);
print("$mdate\n");
system "rm /data/test/seqfiles/cidjvin";
system "rm /data/test/seqfiles/cidjvtmpin";
system "rm /data/test/seqfiles/*_CID_JV_EXTRACT1.Logic";
print("finish removes\n");
system "touch /data/test/seqfiles/cidjvin";
system "touch /data/test/seqfiles/cidjvtmpin";
print("finish touches\n");
system "ftp -n ntheat < /apps/prod/cid1p/jcl/ish/ftp-getcidjv";
print("finish ftp\n");
system "cp /data/test/seqfiles/*.Logic /data/test/seqfiles/cidjvtmpin";
print("finish copy\n");
system "tr -d '\015' < /data/test/seqfiles/cidjvtmpin > /data/test/seqfiles/cidjvin";
#system "tr -d '\015' < /data/test/seqfiles/*.Logic > /data/test/seqfiles/cidjvin";
print("finish tr\n");
if (-s '/data/test/seqfiles/cidjvin') {
   print(" file has non zero length\n");
   system "cp /data/test/seqfiles/cidjvin /data/test/seqfiles/cidjvin.$mdate";
}
#print("Press Enter to End"); <stdin>;
