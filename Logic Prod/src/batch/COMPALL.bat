#!/bin/ksh
for i in `ls *.cbl`
do
     echo about to compile `basename $i`
     cob -W s -uPV -C "directives=cobopt.bth" `basename $i`
     ReturnCode=$?
     if [ $ReturnCode -ne 0 ]
       then
           echo "Error detected during cob of `basename $i`">> COMPALL.err
     else
           echo "Compile completed for `basename $i`" >> COMPALL.err
           mv `basename $i .cbl`.gnt /apps/prod/cid1p/exe/bat
     fi
done
