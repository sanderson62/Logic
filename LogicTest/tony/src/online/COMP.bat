#!/bin/ksh
for i in $1
do
     rm -f `basename .lst`.lst
     echo about to compile `basename $i`
     cob -W s -uPV -C "directives=cobopt.bth" `basename $i`
     ReturnCode=$?
     if [ $ReturnCode -ne 0 ]
       then
           echo "Error detected during cob of `basename $i`">> COMP.err
     else
           echo "Compile completed for `basename $i`" >> COMP.err
           mv `basename $i .cbl`.gnt /apps/test/cid1t/exe/bat
     fi
done
