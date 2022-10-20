BEGINJOB mode='MVS' jobclass='Y' 

if ($EBMSYS != "tony") then
   echo "Job aborting, not Tony "
   exit 1
endif

unikixbld -t s -d DLYACTV -s /data/test/tony/seqfiles/dlyactv.backup
unikixbld -t s -d ELACTQ -s /data/test/tony/seqfiles/elactq.backup
unikixbld -t s -d ELALPH -s /data/test/tony/seqfiles/elalph.backup
unikixbld -t s -d ELARCH -s /data/test/tony/seqfiles/elarch.backup
unikixbld -t s -d ELBENE -s /data/test/tony/seqfiles/elbene.backup
unikixbld -t s -d ELCERT -s /data/test/tony/seqfiles/elcert.backup
unikixbld -t s -d ELCHKQ -s /data/test/tony/seqfiles/elchkq.backup
unikixbld -t s -d ELCIIR -s /data/test/tony/seqfiles/elciir.backup
unikixbld -t s -d ELCISB -s /data/test/tony/seqfiles/elcisb.backup
unikixbld -t s -d ELCISC -s /data/test/tony/seqfiles/elcisc.backup
unikixbld -t s -d ELCIST -s /data/test/tony/seqfiles/elcist.backup
unikixbld -t s -d ELCNTL -s /data/test/tony/seqfiles/elcntl.backup
unikixbld -t s -d ELCRTO -s /data/test/tony/seqfiles/elcrto.backup
unikixbld -t s -d ELCRTT -s /data/test/tony/seqfiles/elcrtt.backup
unikixbld -t s -d ELDENY -s /data/test/tony/seqfiles/eldeny.backup
unikixbld -t s -d ELENCC -s /data/test/tony/seqfiles/elencc.backup
unikixbld -t s -d ELEOBC -s /data/test/tony/seqfiles/eleobc.backup
unikixbld -t s -d ELERRS -s /data/test/tony/seqfiles/elerrs.backup
unikixbld -t s -d ELFORM -s /data/test/tony/seqfiles/elform.backup
unikixbld -t s -d ELHELP -s /data/test/tony/seqfiles/elhelp.backup
unikixbld -t s -d ELLETR -s /data/test/tony/seqfiles/elletr.backup
unikixbld -t s -d ELMSTR -s /data/test/tony/seqfiles/elmstr.backup
unikixbld -t s -d ELNAPS -s /data/test/tony/seqfiles/elnaps.backup
unikixbld -t s -d ELPGMN -s /data/test/tony/seqfiles/elpgmn.backup
unikixbld -t s -d ELPGMO -s /data/test/tony/seqfiles/elpgmo.backup
unikixbld -t s -d ELPGMS -s /data/test/tony/seqfiles/elpgms.backup
unikixbld -t s -d ELPURG -s /data/test/tony/seqfiles/elpurg.backup
unikixbld -t s -d ELREPT -s /data/test/tony/seqfiles/elrept.backup
unikixbld -t s -d ELRETR -s /data/test/tony/seqfiles/elretr.backup
unikixbld -t s -d ELTRLR -s /data/test/tony/seqfiles/eltrlr.backup
unikixbld -t s -d ERACCT -s /data/test/tony/seqfiles/eracct.backup
unikixbld -t s -d ERACCTT -s /data/test/tony/seqfiles/eracctt.backup
unikixbld -t s -d ERACNT -s /data/test/tony/seqfiles/eracnt.backup
unikixbld -t s -d ERAGTC -s /data/test/tony/seqfiles/eragtc.backup
unikixbld -t s -d ERARCH -s /data/test/tony/seqfiles/erarch.backup
unikixbld -t s -d ERARCT -s /data/test/tony/seqfiles/erarct.backup
unikixbld -t s -d ERBILL -s /data/test/tony/seqfiles/erbill.backup
unikixbld -t s -d ERBXRF -r recordv -s /data/test/tony/seqfiles/erbxrf.backup
unikixbld -t s -d ERCHEK -s /data/test/tony/seqfiles/erchek.backup
unikixbld -t s -d ERCHKQ -s /data/test/tony/seqfiles/erchkq.backup
unikixbld -t s -d ERCNOT -s /data/test/tony/seqfiles/ercnot.backup
unikixbld -t s -d ERCOMM -s /data/test/tony/seqfiles/ercomm.backup
unikixbld -t s -d ERCOMP -s /data/test/tony/seqfiles/ercomp.backup
unikixbld -t s -d ERCOBI -s /data/test/tony/seqfiles/ercobi.backup
unikixbld -t s -d ERCONT -s /data/test/tony/seqfiles/ercont.backup
unikixbld -t s -d ERCRTC -s /data/test/tony/seqfiles/ercrtc.backup
unikixbld -t s -d ERCTBL -s /data/test/tony/seqfiles/erctbl.backup
unikixbld -t s -d ERCTBLT -s /data/test/tony/seqfiles/erctblt.backup
unikixbld -t s -d ERDUEP -s /data/test/tony/seqfiles/erduep.backup
unikixbld -t s -d EREADR -s /data/test/tony/seqfiles/ereadr.backup
unikixbld -t s -d ERENDR -s /data/test/tony/seqfiles/erendr.backup
unikixbld -t s -d ERENDT -s /data/test/tony/seqfiles/erendt.backup
unikixbld -t s -d ERFORM -s /data/test/tony/seqfiles/erform.backup
unikixbld -t s -d ERGXRF -r recordv -s /data/test/tony/seqfiles/ergxrf.backup
unikixbld -t s -d ERLOFC -s /data/test/tony/seqfiles/erlofc.backup
unikixbld -t s -d ERLOSS -s /data/test/tony/seqfiles/erloss.backup
unikixbld -t s -d ERMAIL -s /data/test/tony/seqfiles/ermail.backup
unikixbld -t s -d ERNAME -s /data/test/tony/seqfiles/ername.backup
unikixbld -t s -d ERNOTE -s /data/test/tony/seqfiles/ernote.backup
unikixbld -t s -d ERPDEF -s /data/test/tony/seqfiles/erpdef.backup
unikixbld -t s -d ERPLAN -s /data/test/tony/seqfiles/erplan.backup
unikixbld -t s -d ERPNDB -s /data/test/tony/seqfiles/erpndb.backup
unikixbld -t s -d ERPNDC -s /data/test/tony/seqfiles/erpndc.backup
unikixbld -t s -d ERPNDM -s /data/test/tony/seqfiles/erpndm.backup
unikixbld -t s -d ERPYAJ -s /data/test/tony/seqfiles/erpyaj.backup
unikixbld -t s -d ERRATE -s /data/test/tony/seqfiles/errate.backup
unikixbld -t s -d ERRATET -s /data/test/tony/seqfiles/erratet.backup
unikixbld -t s -d ERREIN -s /data/test/tony/seqfiles/errein.backup
unikixbld -t s -d ERREPY -s /data/test/tony/seqfiles/errepy.backup
unikixbld -t s -d ERRPTC -s /data/test/tony/seqfiles/errptc.backup
unikixbld -t s -d ERRTBLT -s /data/test/tony/seqfiles/errtblt.backup
unikixbld -t s -d MICRDRFT -r recordv -s /data/test/tony/seqfiles/micrdrft.backup
unikixbld -t s -d MICRFLAG -s /data/test/tony/seqfiles/micrflag.backup
unikixbld -t s -d NSASEXTR -s /data/test/tony/seqfiles/nsasextr.backup

ENDJOB 


