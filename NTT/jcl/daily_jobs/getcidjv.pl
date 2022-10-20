#!/usr/perl5/bin/perl
$mdate = `date +"%Y%m%d"`;
chomp($mdate);
print("$mdate\n");
system "rm $SEQFILES/cidjvin";
system "rm $SEQFILES/cidjvtmpin";
system "rm $SEQFILES/*_CID_JV_EXTRACT*.Logic";
print("finish removes\n");
system "touch $SEQFILES/cidjvin";
system "touch $SEQFILES/cidjvtmpin";
print("finish touches\n");
system "ftp -n ntheat < $JCLLIB/ftp-getcidjv > $JCLLIB/cilgdbld.prt";
print("finish ftp\n");
system "cat $SEQFILES/*_CID_JV_EXTRACT*.Logic > $SEQFILES/cidjvtmpin";
print("finish copy\n");
system "tr -d '\015' < $SEQFILES/cidjvtmpin > $SEQFILES/cidjvin";
#system "tr -d '\015' < /data/test/seqfiles/*.Logic > /data/test/seqfiles/cidjvin";
print("finish tr\n");
if (-s '$SEQFILES/cidjvin') {
   print(" file has non zero length\n");
   system "cp $SEQFILES/cidjvin $SEQFILES/cidjvin.$mdate";
}
#print("Press Enter to End"); <stdin>;
