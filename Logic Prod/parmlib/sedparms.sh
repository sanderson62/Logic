#!/bin/bash
     for f2 in *.parms; do
     mv $f2 $f2.old
     sed 's/QSS_PRTQ/IS_PARALLEL/g' $f2.old > $f2
     rm -f $f2.old
     done
     mv /export/home/mtpadmin/bin/print_el538_state.pl pelst.tmp
     sed 's/QSS_PRTQ/IS_PARALLEL/g' pelst.tmp > /export/home/mtpadmin/bin/print_el538_state.pl
     rm -f pelst.tmp

