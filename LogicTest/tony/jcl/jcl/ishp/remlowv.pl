$infile = $ARGV[0];
$otfile = $ARGV[1];
print $ARGV[0];
    open (INFILE,$infile) ||
         die "can't open $infile:$!";
    open (OUTFILE,">".$otfile) ||
	 die "can't open $otfile:$!";
    $Y = "\x00" ;
    $J = "\x20" ;
    while (<INFILE>) {
      s/$Y//g;
      print OUTFILE $_;
     }
     close (INFILE);
     close (OUTFILE);	

