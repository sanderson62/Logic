#!/usr/perl5/bin/perl
$file1 = $ARGV[0];
open (NAMEFIL,"$file1") || die "open of hash failed: $!";
$difil = "diftst";
while (defined ($rec1 = <NAMEFIL>)) {
   chomp($rec1);
   $difil = "$rec1"."."."diff";
   system "diff /apps/prod/cid1p/src/batch/$rec1 /apps/test/cid1t/src/batch/$rec1 > $difil";
}
close(NAMEFIL) || die "close of hash failed: $!";
print("FINISHED");
