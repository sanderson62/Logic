#!/bin/ksh
for i in $1
do
     rm -f `basename .lst`.lst
     echo about to compile `basename $i`
     cob -W s -uPV -C "directives=cobopt.bth" `basename $i`
     ReturnCode=$?
     if [ $ReturnCode -ne 0 ]
       then
           echo "Error detected during cob of `basename $i`">> comp.err
     else
           echo "Compile completed for `basename $i`" >> comp.err
           mv `basename $i .cbl`.gnt /apps/prod/cid1p/staging/exe_bat
     fi
done
