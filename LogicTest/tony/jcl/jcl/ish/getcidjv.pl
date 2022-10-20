#!/usr/perl5/bin/perl
$mdate = `date +"%Y%m%d"`;
chomp($mdate);
print("$mdate\n");
system "rm /data/seqfiles/cidjvin";
system "rm /data/seqfiles/cidjvtmpin";
system "rm /data/seqfiles/*_CID_JV_EXTRACT*.Logic";
print("finish removes\n");
system "touch /data/seqfiles/cidjvin";
system "touch /data/seqfiles/cidjvtmpin";
print("finish touches\n");
system "ftp -n ntheat < /apps/prod/cid1p/jcl/ish/ftp-getcidjv > /apps/prod/cid1p/jcl/ish/cilgdbld.prt";
print("finish ftp\n");
system "cat /data/seqfiles/*_CID_JV_EXTRACT*.Logic > /data/seqfiles/cidjvtmpin";
print("finish copy\n");
system "tr -d '\015' < /data/seqfiles/cidjvtmpin > /data/seqfiles/cidjvin";
#system "tr -d '\015' < /data/test/seqfiles/*.Logic > /data/test/seqfiles/cidjvin";
print("finish tr\n");
if (-s '/data/seqfiles/cidjvin') {
   print(" file has non zero length\n");
   system "cp /data/seqfiles/cidjvin /data/seqfiles/cidjvin.$mdate";
}
#print("Press Enter to End"); <stdin>;
