$testjob = $ARGV[0];
chdir "/apps/test/cid1t/jcl";
system "unikixjob", "$testjob", "-kcid1t";
