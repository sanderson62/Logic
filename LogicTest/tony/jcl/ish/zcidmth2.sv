################################################################################
BEGINJOB mode='MVS'

if ($EBMSYS != 'tony') then
   echo "Job aborting, not tony"
   exit 1
endif

################################################################################

#cilgm15
################################################################################
LABEL name=EL521 
################################################################################
#  *****************************************************************     
#  *****                                                                 
#  *****              MONTHLY FILE EXTRACT FROM ONLINE                   
#  *****          SEE RESTART INSTRUCTIONS BEFORE RESTARTING             
#  *****                                                                 
#  *****************************************************************     
#  ************************************************* INPUT FILES ***     
ASSGNDD ddname='ERRQST' type='DUMMY' 
ASSGNDD ddname='ERPNDB' dataset='CI.DD.LG.ERPNDB' type='VS' filename='ERPNDB' disp='i-o' 
ASSGNDD ddname='ERPNDC' dataset='CI.DD.LG.ERPNDC' type='VS' filename='ERPNDC' disp='i-o' 
ASSGNDD ddname='ERCRTC' dataset='CI.DD.LG.ERCRTC' type='VS' filename='ERCRTC' disp='i-o' 
ASSGNDD ddname='ERPYAJ' dataset='CI.DD.LG.ERPYAJ' type='VS' filename='ERPYAJ' disp='i-o' 
ASSGNDD ddname='ERCHKQ' dataset='CI.DD.LG.ERCHKQ' type='VS' filename='ERCHKQ' disp='i-o' 
ASSGNDD ddname='ERREPY' dataset='CI.DD.LG.ERREPY' type='VS' filename='ERREPY' disp='i-o' 
ASSGNDD ddname='ELCNTL' dataset='CI.DD.LG.ELCNTL' type='VS' filename='ELCNTL' disp='i-o' 
ASSGNDD ddname='SYS019' dataset='ZI.DD.ER.DATECARD' filename='\${SEQFILES}/ZI.DD.ER.DATECARD' disp='i-o' 
#  ************************************************** OUTPUT FILES ***   
ASSGNDD  ddname='SYS008' type='SYSOUT' class='A' recfmt='F' recsize='133' 
ASSGNDD ddname='SYS010' dataset='ZI.XX.EXTR521.NEW' filename='\${SEQFILES}/ZI.XX.EXTR521.NEW' disp='o' normal='k' abend='d' recfmt='F' recsize='629' 
ASSGNDD ddname='SORTWK01' type='TEMP' 
ASSGNDD ddname='SORTWK02' type='TEMP' 
ASSGNDD ddname='SORTWK03' type='TEMP' 

EXECPGM pgmname='EL521' stepname='EL521' 

################################################################################
LABEL name=CPY521
################################################################################
ASSGNDD ddname='SYS010' filename='\$SYSOUTDIR/$JOBNAME/EL521/SYS008_\${JON}' disp='i-o'
ASSGNDD ddname='SYS011' filename='\${SEQFILES}/ZI.DL.CIADM.FICH521' disp='o' normal='k' abend='d' recfmt='F' recsize='132' 

EXECPGM pgmname='CIB009L' stepname='CPY521' parm='ALL' 

################################################################################
LABEL name=EL522 
################################################################################
#  ******************************************************************    
#  ***                                                                   
#  ***                   MONTHLY FILE VERIFICATION                       
#  ***            SEE RESTART INSTRUCTIONS BEFORE RESTARTING             
#  ***                                                                   
#  ******************************************************************    
#  ****************************************************  INPUT FILES     
ASSGNDD ddname='SYS010' dataset='ZI.XX.EXTR521.NEW' filename='\${SEQFILES}/ZI.XX.EXTR521.NEW' disp='i-o' normal='k' abend='k' 
ASSGNDD ddname='SYS019' filename='\${SEQFILES}/ZI.DD.ER.DATECARD' disp='i-o' 
#  **************************************************** OUTPUT FILES     
ASSGNDD ddname='ELREPT' dataset='CI.DD.LG.ELREPT' type='VS' filename='ELREPT' disp='i-o' 
#  **************************************************** I/O FILES        
ASSGNDD ddname='ERMEBL' dataset='CI.DD.LG.ERMEBL' type='VS' filename='ERMEBL' disp='i-o'
#  *********************************MONTHLY ACTIVITY INTO ECS010*******  
ASSGNDD ddname='SYS003' filename='\${SEQFILES}/ZI.WW.VALTRANS' disp='o' normal='k' abend='d' recsize='588' recfmt='F' 
ASSGNDD  ddname='SYS008' type='SYSOUT' recfmt='F' recsize='133' 
ASSGNDD ddname='SYS012' filename='\${SEQFILES}/ZI.WW.PAYADJS' disp='o' normal='k' abend='d' recsize='80' recfmt='F' 

#  ***                                                                   
#  ***  **  **  **  **  **  **  **  **  **  **  ** RETROS INTO ECS061    
#  ***                                                                   
ASSGNDD ddname='SYS013' filename='\${SEQFILES}/ZI.WW.RETROS' disp='o' normal='k' abend='d' recsize='200' recfmt='F' 
ASSGNDD ddname='SYS020' filename='\${SEQFILES}/ZI.EX.FICH522' disp='o' normal='k' abend='d' recsize='133' recfmt='F' 
ASSGNDD ddname='SYS011' filename='\${SEQFILES}/\${JOBNAME}.EL522.ME50.BAL.AMTS' disp='o' normal='k' abend='k' recfmt='F'
ASSGNDD ddname='SORTWK01' type='TEMP' 
ASSGNDD ddname='SORTWK02' type='TEMP' 
ASSGNDD ddname='SORTWK03' type='TEMP' 

EXECPGM pgmname='EL522' stepname='EL522' 

################################################################################
LABEL name=CPY522
################################################################################
ASSGNDD ddname='SYS010' filename='\$SYSOUTDIR/$JOBNAME/EL522/SYS008_\${JON}' disp='i-o'
ASSGNDD ddname='SYS011' filename='\${SEQFILES}/ZI.DL.CIADM.FICH522' disp='o' normal='k' abend='d' recfmt='F' recsize='132' 

EXECPGM pgmname='CIB009L' stepname='CPY522' parm='ALL' 

################################################################################
LABEL name=ECS010 
################################################################################
#LIBDEF scope='STEP' type='PGM' lib='/apps/prod/cid1p/src/batch'
#  ******************************************************************    
#  ***                                                                   
#  ***                 BATCH CERTIFICATE MASTER UPDATE                   
#  ***                                                                   
#  ******************************************************************    
#  ****************************************************  INPUT FILES     
ASSGNDD ddname='SYS005' dataset='ZI.WW.VALTRANS' filename='\${SEQFILES}/ZI.WW.VALTRANS' disp='i-o' normal='k' abend='k' 
#ASSGNDD ddname='SYS010' dataset='ZI.XX.CERT' filename='\${SEQFILES}/ZI.XX.CERT' disp='i-o' normal='k' abend='k' 
ASSGNDD ddname='SYS010' dataset='CI.XX.CERT_00' filename='/slunikix/data/seqfiles/CI.XX.CERT_00' disp='i-o' normal='k' abend='k' 
ASSGNDD ddname='ERAGTC' type='VS' filename='ERAGTC' disp='i-o' 
ASSGNDD ddname='ELCRTT' type='VS' filename='ELCRTT' disp='i-o' 
ASSGNDD ddname='ERRTBLT' dataset='CI.WW.LG.ERREIN' type='VS' filename='ERRTBLT' disp='i-o' 
ASSGNDD ddname='ERACCTT' dataset='CI.WW.LG.ERACCT' type='VS' filename='ERACCTT' disp='i-o' 
ASSGNDD ddname='ERCTBLT' dataset='CI.WW.LG.ERCTBL' type='VS' filename='ERCTBLT' disp='i-o' 
ASSGNDD ddname='ERRATE' dataset='CI.DD.LG.ERRATE' type='VS' filename='ERRATE' disp='i-o' 
ASSGNDD ddname='ERPDEF' type='VS' filename='ERPDEF' disp='i-o'
ASSGNDD ddname='SYS019' dataset='ZI.DD.ER.DATECARD' filename='\${SEQFILES}/ZI.DD.ER.DATECARD' disp='i-o' 
#  **************************************************** OUTPUT FILES     
ASSGNDD ddname='SYS012' filename='\${SEQFILES}/\${JOBNAME}.ECS010.ME.BAL.AMTS' disp='o' normal='k' abend='k' recfmt='F'
ASSGNDD ddname='SYS013' filename='\${SEQFILES}/\${JOBNAME}.ECS010.ME50.BAL.AMTS' disp='o' normal='k' abend='k' recfmt='F'
ASSGNDD  ddname='SYS008' type='SYSOUT' class='A' recfmt='F' recsize='133'  
ASSGNDD  ddname='SYS009' filename='\${SEQFILES}/ZI.ME.MISMATCH' disp='o' normal='k' abend='d' recsize='133'
ASSGNDD ddname='SYS011' dataset='ZI.XX.CERT.NEW' filename='\${SEQFILES}/ZI.XX.CERT.NEW' disp='o' normal='k' abend='d' recsize='1056' recfmt='F' 
ASSGNDD ddname='SYS017' dataset='ZI.DD.DET010.TEMP' filename='\${SEQFILES}/ZI.DD.DET010.TEMP' disp='o' normal='k' abend='d' recfmt='F' recsize='510' 
ASSGNDD ddname='SYS018' dataset='ZI.DD.SUM010.TEMP' filename='\${SEQFILES}/ZI.DD.SUM010.TEMP' disp='o' normal='k' abend='d' recfmt='F' recsize='325' 
ASSGNDD ddname='SYS020' dataset='ZI.DD.FICH010.TEMP' filename='\${SEQFILES}/ZI.DD.FICH010.TEMP' disp='o' normal='k' abend='d' recsize='133' recfmt='F' 
ASSGNDD  ddname='SYS022' type='SYSOUT' class='A' recfmt='F' recsize='133' 
ASSGNDD ddname='ERMEBL' dataset='CI.DD.LG.ERMEBL' type='VS' filename='ERMEBL' disp='i-o'
ASSGNDD ddname='ACCTBILL' type='DUMMY' 

EXECPGM pgmname='ECS010' stepname='ECS010' 

################################################################################
LABEL name=CPY010A
################################################################################
ASSGNDD ddname='SYS010' dataset='ZI.DD.FICH010.TEMP' filename='\${SEQFILES}/ZI.DD.FICH010.TEMP' disp='i-o'
ASSGNDD ddname='SYS011' dataset='ZI.DL.CIADM.FICH010' filename='\${SEQFILES}/ZI.DL.CIADM.FICH010' disp='o' normal='k' abend='d' recfmt='F' recsize='132' 

EXECPGM pgmname='CIB009L' stepname='CPY010A' parm='ALL' 

################################################################################
LABEL name=CPY010B
################################################################################
ASSGNDD ddname='SYS010' filename='\${SEQFILES}/ZI.ME.MISMATCH' disp='i-o'
ASSGNDD ddname='SYS011' filename='\${SEQFILES}/ZI.DL.CIADM.ECS010.MISMATCH' disp='o' normal='k' abend='d' recfmt='F' recsize='132' 

EXECPGM pgmname='CIB009L' stepname='CPY010B' parm='ALL' 

################################################################################
LABEL name=ECS010B 
################################################################################
ONRETCODE ECS010 NE 0 BYPASS scope='STEP'  

ASSGNDD ddname='INPUT1' dataset='ZI.DD.FICH010.TEMP' filename='\${SEQFILES}/ZI.DD.FICH010.TEMP' disp='i-o' normal='k' abend='k' 
ASSGNDD ddname='OUTPUT1' dataset='ZI.EX.FICH010' filename='\${SEQFILES}/ZI.EX.FICH010' recsize='133' recfmt='F' disp='o' normal='k' abend='d' 
ASSGNDD ddname='INPUT2' dataset='ZI.DD.DET010.TEMP' filename='\${SEQFILES}/ZI.DD.DET010.TEMP' disp='i-o' normal='k' abend='k' 
ASSGNDD ddname='OUTPUT2' dataset='ZI.XX.DET010' filename='\${SEQFILES}/ZI.XX.DET010' disp='o' normal='k' abend='d' recfmt='F' recsize='510' 
ASSGNDD ddname='INPUT3' dataset='ZI.DD.SUM010.TEMP' filename='\${SEQFILES}/ZI.DD.SUM010.TEMP' disp='i-o' normal='k' abend='k' 
ASSGNDD ddname='OUTPUT3' dataset='ZI.XX.SUM010' filename='\${SEQFILES}/ZI.XX.SUM010' disp='o' normal='k' abend='d' recfmt='F' recsize='325' 
ASSGNDD ddname='SYSIN' type='INSTREAM'  << !
  REPRO  IFILE(INPUT1) OFILE(OUTPUT1)
  REPRO  IFILE(INPUT2) OFILE(OUTPUT2)
  REPRO  IFILE(INPUT3) OFILE(OUTPUT3)
  SET MAXCC=0
!

EXECPGM pgmname='IDCAMS' stepname='ECS010B' 

################################################################################
LABEL name=DELETE 
################################################################################
ONRETCODE ECS010B NE 0 BYPASS scope='STEP'  

ASSGNDD ddname='SYSIN' type='INSTREAM'  << !
        DELETE  ZI.DD.FICH010.TEMP
        SET MAXCC=0
!

EXECPGM pgmname='IDCAMS' stepname='DELETE' 

################################################################################
LABEL name=ECS015 
################################################################################
#  ********************************************************************  
#  ****                                                                  
#  ***                  REINSURANCE ACTIVITY REPORT                      
#  ***            SEE RESTART INSTRUCTIONS BEFORE RESTARTING             
#  ****                                                                  
#  ********************************************************************  
#  ****************************************************  INPUT FILES     
ASSGNDD ddname='ERRTBLT' dataset='CI.WW.LG.ERREIN' type='VS' filename='ERRTBLT' disp='i-o' 
ASSGNDD ddname='SYS018' dataset='ZI.DD.DET010.TEMP' filename='\${SEQFILES}/ZI.DD.DET010.TEMP' disp='i-o' normal='k' abend='k' 
ASSGNDD ddname='SYS019' dataset='ZI.DD.ER.DATECARD' filename='\${SEQFILES}/ZI.DD.ER.DATECARD' disp='i-o' 
#  **************************************************** OUTPUT FILES     
ASSGNDD ddname='SYS008' type='DUMMY' 
ASSGNDD ddname='SYS020' dataset='ZI.EX.FICH015' filename='\${SEQFILES}/ZI.EX.FICH015' disp='o' normal='k' abend='d' recsize='133' recfmt='F' 
ASSGNDD ddname='SYS022' filename='\${SEQFILES}/ZI.EX.ECS015.EXTRACT' disp='o' normal='k' abend='d' recsize='192' recfmt='F' 
ASSGNDD ddname='SORTWK01' type='TEMP' 
ASSGNDD ddname='SORTWK02' type='TEMP' 
ASSGNDD ddname='SORTWK03' type='TEMP' 

EXECPGM pgmname='ECS015' stepname='ECS015' 

################################################################################
LABEL name=CPY015
################################################################################
ASSGNDD ddname='SYS010' dataset='ZI.EX.FICH015' filename='\${SEQFILES}/ZI.EX.FICH015' disp='i-o'
ASSGNDD ddname='SYS011' dataset='ZI.DL.CIADM.FICH015' filename='\${SEQFILES}/ZI.DL.CIADM.FICH015' disp='o' normal='k' abend='d' recfmt='F' recsize='132' 

EXECPGM pgmname='CIB009L' stepname='CPY015' parm='ALL' 

################################################################################
LABEL name=ECS017 
################################################################################
#LIBDEF scope='STEP' type='PGM' lib='/apps/prod/cid1p/src/batch'
#  ********************************************************************  
#  ***                                                                   
#  ***--               CREATE COMPENSATION TRANSACTIONS           --***  
#  ***--          SEE RESTART INSTRUCTIONS BEFORE RESTARTING      --***  
#  ***                                                                   
#  ********************************************************************  
#  ****************************************************  INPUT FILES     
ASSGNDD ddname='ERACCTT' dataset='CI.WW.LG.ERACCT' type='VS' filename='ERACCTT' disp='i-o' 
ASSGNDD ddname='ERPDEF' type='VS' filename='ERPDEF' disp='i-o'
ASSGNDD ddname='SYS018' dataset='ZI.DD.DET010.TEMP' filename='\${SEQFILES}/ZI.DD.DET010.TEMP' disp='i-o' normal='k' abend='k' 
ASSGNDD ddname='SYS019' dataset='ZI.DD.ER.DATECARD' filename='\${SEQFILES}/ZI.DD.ER.DATECARD' disp='i-o' 
#  **************************************************** OUTPUT FILES     
ASSGNDD ddname='SYS004' dataset='ZI.WW.COMRCALC' filename='\${SEQFILES}/ZI.WW.COMRCALC' disp='o' normal='k' abend='d' recfmt='F' recsize='165' 
ASSGNDD  ddname='SYS008' type='SYSOUT' class='A' recfmt='F' recsize='133' 
ASSGNDD ddname='SYS013' dataset='ZI.XX.CTRN' filename='\${SEQFILES}/ZI.XX.CTRN' disp='o' normal='k' abend='d' recfmt='F' recsize='270' 
ASSGNDD ddname='SYS014' dataset='ZI.WW.PRCMEXTR' filename='\${SEQFILES}/ZI.WW.PRCMEXTR' disp='o' normal='k' abend='d' recfmt='F' recsize='165' 
ASSGNDD ddname='SYS020' dataset='ZI.EX.FICH017' filename='\${SEQFILES}/ZI.EX.FICH017' disp='o' normal='k' abend='d' recsize='133' recfmt='F' 

EXECPGM pgmname='ECS017' stepname='ECS017' 

################################################################################
LABEL name=CPY017
################################################################################
ASSGNDD ddname='SYS010' filename='\${SYSOUTDIR}/$JOBNAME/ECS017/SYS008_${JON}' disp='i-o'
ASSGNDD ddname='SYS011' filename='\${SEQFILES}/ZI.DL.CIADM.FICH017' disp='o' normal='k' abend='d' recfmt='F' recsize='133'

EXECPGM pgmname='CIB009L' stepname='CPY017' parm='ALL'

################################################################################
LABEL name=ECS018 
################################################################################
#  ********************************************************************  
#  ******--                                                    --******  
#  ***--        RECALCULATED PREMIUM/COMPENSATION DISTRIB.        --***  
#  ******--                                                    --******  
#  ********************************************************************  
#  **************************************************** INPUT FILES      
ASSGNDD ddname='ERACCTT' dataset='CI.WW.LG.ERACCT' type='VS' filename='ERACCTT' disp='i-o' 
ASSGNDD ddname='SYS016' filename='\${SEQFILES}/ZI.WW.COMRCALC' disp='i-o' normal='k' abend='k' 
ASSGNDD ddname='SYS019' filename='\${SEQFILES}/ZI.DD.ER.DATECARD' disp='i-o' 
#  **************************************************** OUTPUT FILES     
ASSGNDD  ddname='SYS008' type='SYSOUT' class='B'  
ASSGNDD ddname='SYS020' filename='\${SEQFILES}/ZI.EX.FICH018' disp='o' normal='k' abend='d' recsize='133' recfmt='F' 
ASSGNDD ddname='ERMEBL' dataset='CI.DD.LG.ERMEBL' type='VS' filename='ERMEBL' disp='i-o'
#  **************************************************** WORK FILES       
ASSGNDD ddname='SORTWK01' type='TEMP' 
ASSGNDD ddname='SORTWK02' type='TEMP' 
ASSGNDD ddname='SORTWK03' type='TEMP' 

EXECPGM pgmname='ECS018' stepname='ECS018' 

################################################################################
LABEL name=RDS018
################################################################################
ASSGNDD ddname='SYS010' filename='/$SYSOUTDIR/$JOBNAME/ECS018/SYS008_$JON' disp='i-o'
ASSGNDD ddname='SYS011' filename='/$RDSFILES/ZI.RDS.PAPER.ECS018' disp='o' normal='k' abend='d' recfmt='F' recsize='132' 

EXECPGM pgmname='CIB009L' stepname='RDS018' parm='ALL'

################################################################################
LABEL name=CPY018
################################################################################
ASSGNDD ddname='SYS010' filename='\${SEQFILES}/ZI.EX.FICH018' disp='i-o'
ASSGNDD ddname='SYS011' filename='\${SEQFILES}/ZI.DL.CIADM.FICH018' disp='o' normal='k' abend='d' recfmt='F' recsize='132' 

EXECPGM pgmname='CIB009L' stepname='CPY018' parm='ALL'

################################################################################
LABEL name=ECS019 
################################################################################
#  ********************************************************************  
#  ******--                                                    --******  
#  ***--              PREMIUM & COMPENSATION DISTRIBUTION         --***  
#  ***--          SEE RESTART INSTRUCTIONS BEFORE RESTARTING      --***  
#  ******--                                                    --******  
#  ********************************************************************  
#  ****************************************************  INPUT FILES     
ASSGNDD ddname='SYS006' type='DUMMY' 
ASSGNDD ddname='SYS010' dataset='ZI.XX.EXTR019' filename='\${SEQFILES}/ZI.XX.EXTR019' disp='i-o' normal='k' abend='k' 
ASSGNDD ddname='ERACCTT' dataset='CI.WW.LG.ERACCT' type='VS' filename='ERACCTT' disp='i-o' 
ASSGNDD ddname='SYS016' dataset='ZI.WW.PRCMEXTR' filename='\${SEQFILES}/ZI.WW.PRCMEXTR' disp='i-o' normal='k' abend='k' 
ASSGNDD ddname='SYS019' dataset='ZI.DD.ER.DATECARD' filename='\${SEQFILES}/ZI.DD.ER.DATECARD' disp='i-o' 
#  **************************************************** OUTPUT FILES     
ASSGNDD ddname='SYS011' dataset='ZI.XX.EXTR019.NEW' filename='\${SEQFILES}/ZI.XX.EXTR019.NEW' disp='o' normal='k' abend='d' recsize='165' recfmt='F' 
ASSGNDD  ddname='SYS008' type='SYSOUT' class='B' 
ASSGNDD ddname='SYS020' dataset='ZI.EX.FICH019' filename='\${SEQFILES}/ZI.EX.FICH019' disp='o' normal='k' abend='d' recsize='133' recfmt='F' 
ASSGNDD ddname='ERMEBL' dataset='CI.DD.LG.ERMEBL' type='VS' filename='ERMEBL' disp='i-o'
ASSGNDD ddname='SORTWK01' type='TEMP' 
ASSGNDD ddname='SORTWK02' type='TEMP' 
ASSGNDD ddname='SORTWK03' type='TEMP' 

EXECPGM pgmname='ECS019' stepname='ECS019' 

################################################################################
LABEL name=RDS019A
################################################################################
ASSGNDD ddname='SYS010' filename='/$SYSOUTDIR/$JOBNAME/ECS019/SYS008_$JON' disp='i-o'
ASSGNDD ddname='SYS011' filename='/$RDSFILES/ZI.RDS.PAPER.ECS019' disp='o' normal='k' abend='d' recfmt='F' recsize='132' 

EXECPGM pgmname='CIB009L' stepname='RDS019A' parm='ALL'

################################################################################
LABEL name=RDS019B
################################################################################
ASSGNDD ddname='SYS010' filename='\${SEQFILES}/ZI.EX.FICH019' disp='i-o'
ASSGNDD ddname='SYS011' filename='/$RDSFILES/ZI.RDS.FICHE.ECS019' disp='o' normal='k' abend='d' recfmt='F' recsize='132' 

EXECPGM pgmname='CIB009L' stepname='RDS019B' parm='ALL'

################################################################################
LABEL name=CPY019
################################################################################
ASSGNDD ddname='SYS010' dataset='ZI.EX.FICH019' filename='\${SEQFILES}/ZI.EX.FICH019' disp='i-o'
ASSGNDD ddname='SYS011' dataset='ZI.DL.CIADM.FICH019' filename='\${SEQFILES}/ZI.DL.CIADM.FICH019' disp='o' normal='k' abend='d' recfmt='F' recsize='132' 

EXECPGM pgmname='CIB009L' stepname='CPY019' parm='ALL' 

################################################################################
LABEL name=CPY019OB
################################################################################

#*******************************************************************************
#* SYS011 is an onbase file that is ftp'd to //ntcso1/userdata/crp_grp/onbase
#*******************************************************************************

ASSGNDD ddname='SYS010' dataset='ZI.EX.FICH019' filename='\${SEQFILES}/ZI.EX.FICH019' disp='i-o'
ASSGNDD ddname='SYS011' filename='\${SEQFILES}/ZI.FICH019.CIDTXT' disp='o' normal='k' abend='d' recfmt='F' recsize='132' 

EXECPGM pgmname='CIB009L' stepname='CPY019OB' parm='ALL' 

################################################################################
LABEL name=ECS030 
################################################################################
#  ********************************************************************  
#  ******--                                                    --******  
#  ***--                       CLAIMS REGISTER                    --***  
#  ***--          SEE RESTART INSTRUCTIONS BEFORE RESTARTING      --***  
#  ******--                                                    --******  
#  ********************************************************************  
#  ****************************************************  INPUT FILES     
ASSGNDD ddname='SYS018' dataset='ZI.DD.DET010.TEMP' filename='\${SEQFILES}/ZI.DD.DET010.TEMP' disp='i-o' normal='k' abend='k' 
ASSGNDD ddname='SYS019' dataset='ZI.DD.ER.DATECARD' filename='\${SEQFILES}/ZI.DD.ER.DATECARD' disp='i-o' 
#  **************************************************** OUTPUT FILES     
ASSGNDD  ddname='SYS008' type='SYSOUT' class='A'  
ASSGNDD ddname='SYS015' type='TEMP' disp='o' normal='k' abend='d' recfmt='F' recsize='510' 
ASSGNDD ddname='SYS020' dataset='ZI.EX.FICH030' filename='\${SEQFILES}/ZI.EX.FICH030' disp='o' normal='k' abend='d' recsize='133' recfmt='F' 
ASSGNDD ddname='ERMEBL' dataset='CI.DD.LG.ERMEBL' type='VS' filename='ERMEBL' disp='i-o'
ASSGNDD ddname='SORTWK01' type='TEMP' 
ASSGNDD ddname='SORTWK02' type='TEMP' 
ASSGNDD ddname='SORTWK03' type='TEMP' 

EXECPGM pgmname='ECS030' stepname='ECS030' 

################################################################################
LABEL name=CPY030
################################################################################
ASSGNDD ddname='SYS010' dataset='ZI.EX.FICH030' filename='\${SEQFILES}/ZI.EX.FICH030' disp='i-o'
ASSGNDD ddname='SYS011' dataset='ZI.DL.CIADM.FICH030' filename='\${SEQFILES}/ZI.DL.CIADM.FICH030' disp='o' normal='k' abend='d' recfmt='F' recsize='132' 

EXECPGM pgmname='CIB009L' stepname='CPY030' parm='ALL' 

################################################################################
LABEL name=EL539 
################################################################################
#LIBDEF scope='STEP' type='PGM' lib='/apps/prod/cid1p/src/batch'
#  ******************************************************************    
#  ***                                                                   
#  ***                   MONTHLY FILE VERIFICATION                       
#  ***            SEE RESTART INSTRUCTIONS BEFORE RESTARTING             
#  ***                                                                   
#  ******************************************************************    
#  ****************************************************  INPUT FILES     
ASSGNDD ddname='SYS010' dataset='ZI.XX.EXTR521.NEW' filename='\${SEQFILES}/ZI.XX.EXTR521.NEW' disp='i-o' normal='k' abend='k' 
ASSGNDD ddname='SYS019' dataset='ZI.DD.ER.DATECARD' filename='\${SEQFILES}/ZI.DD.ER.DATECARD' disp='i-o' 
ASSGNDD ddname='ERPNDB2' dataset='CI.DD.LG.ERPNDB2' type='VS' filename='ERPNDB2' disp='i-o' 
ASSGNDD ddname='ERLOFC' dataset='CI.DD.LG.ERLOFC' type='VS' filename='ERLOFC' disp='i-o' 
ASSGNDD ddname='ERACCT' dataset='CI.DD.LG.ERACCT' type='VS' filename='ERACCT' disp='i-o' 
ASSGNDD ddname='SYSIN' type='INSTREAM'  << !
!
#  **************************************************** OUTPUT FILES     
ASSGNDD ddname='ELREPT' type='DUMMY'
#  **************************************************** I/O FILES        
ASSGNDD  ddname='SYS008' type='SYSOUT' class='A' recfmt='F' recsize='133' 
ASSGNDD ddname='SORTWK01' type='TEMP' 
ASSGNDD ddname='SORTWK02' type='TEMP' 
ASSGNDD ddname='SORTWK03' type='TEMP' 

EXECPGM pgmname='EL539' stepname='EL539' 

################################################################################
LABEL name=CPY539
################################################################################
ASSGNDD ddname='SYS010' filename='\$SYSOUTDIR/$JOBNAME/EL539/SYS008_\${JON}' disp='i-o'
ASSGNDD ddname='SYS011' filename='\${SEQFILES}/ZI.DL.CIADM.FICH539' disp='o' normal='k' abend='d' recfmt='F' recsize='132' 

EXECPGM pgmname='CIB009L' stepname='CPY539' parm='ALL' 

################################################################################

#cilgm16
################################################################################
LABEL name=ECS038 
################################################################################
#  ********************************************************************  
#  **--                                                                  
#  **--           CLAIM HISTORY MERGE                                    
#  **--                                                                  
#  *******************************************************************   
#  ****************************************************  INPUT FILES     
ASSGNDD ddname='SYS010' dataset='ZI.XX.CLMS' filename='\${SEQFILES}/ZI.XX.CLMS' disp='i-o' normal='k' abend='k' 
ASSGNDD ddname='SYS018' dataset='ZI.DD.DET010.TEMP' filename='\${SEQFILES}/ZI.DD.DET010.TEMP' disp='i-o' normal='k' abend='k' 
ASSGNDD ddname='SYS019' dataset='ZI.DD.ER.DATECARD' filename='\${SEQFILES}/ZI.DD.ER.DATECARD' disp='i-o' 
#  **************************************************** OUTPUT FILES     
ASSGNDD  ddname='SYS008' type='SYSOUT' class='A' recfmt='F' recsize='133' 
ASSGNDD ddname='SYS011' dataset='ZI.XX.CLMS.NEW' filename='\${SEQFILES}/ZI.XX.CLMS.NEW' disp='o' normal='k' abend='d' recfmt='F' recsize='510' 
ASSGNDD ddname='SYS020' dataset='ZI.EX.FICH038' filename='\${SEQFILES}/ZI.EX.FICH038' disp='o' normal='k' abend='d' recsize='133' recfmt='F' 
ASSGNDD ddname='ERMEBL' dataset='CI.DD.LG.ERMEBL' type='VS' filename='ERMEBL' disp='i-o'
#  ************************************************   SORT WORK FILES    
ASSGNDD ddname='SORTWK01' type='TEMP' 
ASSGNDD ddname='SORTWK02' type='TEMP' 
ASSGNDD ddname='SORTWK03' type='TEMP' 

EXECPGM pgmname='ECS038' stepname='ECS038' 

################################################################################
LABEL name=CPY038
################################################################################
ASSGNDD ddname='SYS010' dataset='ZI.EX.FICH038' filename='\${SEQFILES}/ZI.EX.FICH038' disp='i-o'
ASSGNDD ddname='SYS011' dataset='ZI.DL.CIADM.FICH038' filename='\${SEQFILES}/ZI.DL.CIADM.FICH038' disp='o' normal='k' abend='d' recfmt='F' recsize='132' 

EXECPGM pgmname='CIB009L' stepname='CPY038' parm='ALL' 

################################################################################
LABEL name=ECS048 
################################################################################
#  *******************************************************************   
#  **--                                                                  
#  **--           CREDIT - RESERVES HISTORY MERGE                        
#  **--                                                                  
#  *******************************************************************   
#  ****************************************************  INPUT FILES     
ASSGNDD ddname='SYS010' dataset='ZI.XX.RESERVES' filename='\${SEQFILES}/ZI.XX.RESERVES' disp='i-o' 
ASSGNDD ddname='SYS018' dataset='ZI.XX.DET010' filename='\${SEQFILES}/ZI.XX.DET010' disp='i-o' 
ASSGNDD ddname='SYS019' dataset='ZI.DD.ER.DATECARD' filename='\${SEQFILES}/ZI.DD.ER.DATECARD' disp='i-o' 
#  **************************************************** OUTPUT FILES     
ASSGNDD  ddname='SYS008' type='SYSOUT' class='A' recfmt='F' recsize='133'
ASSGNDD ddname='SYS011' dataset='ZI.XX.RESERVES.NEW' filename='\${SEQFILES}/ZI.XX.RESERVES.NEW' disp='o' normal='k' abend='d' recfmt='F' recsize='510' 
ASSGNDD ddname='SYS020' type='DUMMY' 
ASSGNDD ddname='ERMEBL' dataset='CI.DD.LG.ERMEBL' type='VS' filename='ERMEBL' disp='i-o'
#  ************************************************   SORT WORK FILES    
ASSGNDD ddname='SORTWK01' type='TEMP' 
ASSGNDD ddname='SORTWK02' type='TEMP' 
ASSGNDD ddname='SORTWK03' type='TEMP' 
ASSGNDD ddname='SORTWK04' type='TEMP' 

EXECPGM pgmname='ECS048' stepname='ECS048' 

################################################################################
LABEL name=CPY048
################################################################################
ASSGNDD ddname='SYS010' filename='\$SYSOUTDIR/$JOBNAME/ECS048/SYS008_\${JON}' disp='i-o'
ASSGNDD ddname='SYS011' filename='\${SEQFILES}/ZI.DL.CIADM.FICH048' disp='o' normal='k' abend='d' recfmt='F' recsize='132' 

EXECPGM pgmname='CIB009L' stepname='CPY048' parm='ALL' 

################################################################################
LABEL name=ECS050 
################################################################################
#  *******************************************************************   
#  **--                                                                  
#  **--             ALPHA/GAAP EXTRACTS                                  
#  **--                                                                  
#  *******************************************************************   
#  ****************************************************  INPUT FILES     
ASSGNDD ddname='SYS011' dataset='ZI.XX.CERT.NEW' filename='\${SEQFILES}/ZI.XX.CERT.NEW' disp='i-o' normal='k' abend='k' 
ASSGNDD ddname='ERACCTT' dataset='CI.WW.LG.ERACCT' type='VS' filename='ERACCTT' disp='i-o' 
ASSGNDD ddname='ERRTBLT' dataset='CI.WW.LG.ERREIN' type='VS' filename='ERRTBLT' disp='i-o' 
ASSGNDD ddname='ERRATE' dataset='CI.DD.LG.ERRATE' type='VS' filename='ERRATE' disp='i-o' 
ASSGNDD ddname='ERPDEF' type='VS' filename='ERPDEF' disp='i-o'
ASSGNDD ddname='SYS019' dataset='ZI.DD.ER.DATECARD' filename='\${SEQFILES}/ZI.DD.ER.DATECARD' disp='i-o' 
#  **************************************************** OUTPUT FILES     
ASSGNDD  ddname='SYS008' type='SYSOUT' class='A' recfmt='F' recsize='133'
ASSGNDD ddname='SYS012' dataset='ZI.XX.GAAP.TEMP.NEW' filename='\${SEQFILES}/ZI.XX.GAAP.TEMP.NEW' disp='o' normal='k' abend='d' recfmt='F' recsize='365' 
ASSGNDD ddname='SYS013' dataset='ZI.XX.AFEX.NEW' filename='\${SEQFILES}/ZI.XX.AFEX.NEW' disp='o' normal='k' abend='d' recfmt='F' recsize='300' 
ASSGNDD ddname='SYS020' dataset='ZI.EX.FICH050' filename='\${SEQFILES}/ZI.EX.FICH050' disp='o' normal='k' abend='d' recsize='133' recfmt='F' 
ASSGNDD ddname='ERMEBL' dataset='CI.DD.LG.ERMEBL' type='VS' filename='ERMEBL' disp='i-o'
#  ************************************************       WORK FILES     
ASSGNDD ddname='SORTWK01' type='TEMP' 
ASSGNDD ddname='SORTWK02' type='TEMP' 
ASSGNDD ddname='SORTWK03' type='TEMP' 

EXECPGM pgmname='ECS050' stepname='ECS050' 

################################################################################
LABEL name=CPY050
################################################################################
ASSGNDD ddname='SYS010' filename='\${SEQFILES}/ZI.EX.FICH050' disp='i-o'
ASSGNDD ddname='SYS011' dataset='ZI.DL.CIADM.FICH050' filename='\${SEQFILES}/ZI.DL.CIADM.FICH050' disp='o' normal='k' abend='d' recfmt='F' recsize='133' 

EXECPGM pgmname='CIB009L' stepname='CPY050' parm='ALL'

################################################################################

#cilgm17
################################################################################
LABEL name=ECS016 
################################################################################
#  ********************************************************************  
#  ***--                                                                 
#  ***               EPEC FILE UPDATE                                    
#  ***  ACCT MSTR MUST BE RELOADED IF THIS STEP ABENDS.                  
#  ***--                                                                 
#  ********************************************************************  
#  ************************************************     INPUT FILES      
ASSGNDD ddname='ERACCTT' dataset='CI.WW.LG.ERACCT' type='VS' filename='ERACCTT' disp='i-o' 
ASSGNDD ddname='SYS019' dataset='ZI.DD.ER.DATECARD' filename='\${SEQFILES}/ZI.DD.ER.DATECARD' disp='i-o' 
ASSGNDD ddname='SYS010' dataset='ZI.XX.EPEC' filename='\${SEQFILES}/ZI.XX.EPEC' disp='i-o' normal='k' abend='k' 
#  *-----------------------------------------------------------------    
#  (TEMP COPY OF CI.XX.SUM010 CART) 00000157
ASSGNDD ddname='SYS018' dataset='ZI.DD.SUM010.TEMP' filename='\${SEQFILES}/ZI.DD.SUM010.TEMP' disp='i-o' normal='k' abend='k' 
#  ************************************************      OUTPUT FILES    
ASSGNDD  ddname='SYS008' type='SYSOUT' class='A' recfmt='F' recsize='133'
ASSGNDD ddname='SYS012' dataset='ZI.XX.EPEC.TEMP.NEW' filename='\${SEQFILES}/ZI.XX.EPEC.TEMP.NEW' disp='o' normal='k' abend='d' recfmt='F' recsize='325' 
ASSGNDD ddname='SYS014' dataset='ZI.XX.ACCT.NEW' filename='\${SEQFILES}/ZI.XX.ACCT.NEW' disp='o' normal='k' abend='d' recfmt='F' recsize='2000' 
ASSGNDD ddname='SYS020' dataset='ZI.EX.FICH016' filename='\${SEQFILES}/ZI.EX.FICH016' disp='o' normal='k' abend='d' recsize='133' recfmt='F' 
ASSGNDD  ddname='SYS022' type='SYSOUT' class='A' recfmt='F' recsize='133'
#  ************************************************   SORT WORK FILES    
ASSGNDD ddname='SORTWK01' type='TEMP' 
ASSGNDD ddname='SORTWK02' type='TEMP' 
ASSGNDD ddname='SORTWK03' type='TEMP' 

EXECPGM pgmname='ECS016' stepname='ECS016' 

################################################################################
LABEL name=CPY016
################################################################################
ASSGNDD ddname='SYS010' filename='\$SYSOUTDIR/$JOBNAME/ECS016/SYS008_\${JON}' disp='i-o'
ASSGNDD ddname='SYS011' filename='\${SEQFILES}/ZI.DL.CIADM.FICH016' disp='o' normal='k' abend='d' recfmt='F' recsize='132' 

EXECPGM pgmname='CIB009L' stepname='CPY016' parm='ALL' 

################################################################################
LABEL name=CPY016B
################################################################################
ASSGNDD ddname='SYS010' filename='\$SYSOUTDIR/$JOBNAME/ECS016/SYS022_\${JON}' disp='i-o'
ASSGNDD ddname='SYS011' filename='\${SEQFILES}/ZI.DL.CIADM.FICH016.ERR' disp='o' normal='k' abend='d' recfmt='F' recsize='132' 

EXECPGM pgmname='CIB009L' stepname='CPY016B' parm='ALL' 

################################################################################
LABEL name=ECS080 
################################################################################
#  *****************************************************************     
#  ***--                                                                 
#  ***        CALCULATE MORTALITY RESERVES                               
#  ***--                                                                 
#  ********************************************************************  
#  ****************************************************  INPUT FILES     
ASSGNDD ddname='SYS010' dataset='ZI.XX.RFAC' filename='\${SEQFILES}/ZI.XX.RFAC' disp='i-o' normal='k' abend='k' 
ASSGNDD ddname='SYS012' dataset='ZI.XX.GAAP.TEMP.NEW' filename='\${SEQFILES}/ZI.XX.GAAP.TEMP.NEW' disp='i-o' normal='k' abend='k' 
ASSGNDD ddname='SYS016' type='DUMMY' 
ASSGNDD ddname='SYS017' type='DUMMY' 
ASSGNDD ddname='ERRTBLT' dataset='CI.WW.LG.ERREIN' type='VS' filename='ERRTBLT' disp='i-o' 
ASSGNDD ddname='ERACCTT' dataset='CI.WW.LG.ERACCT' type='VS' filename='ERACCTT' disp='i-o' 
ASSGNDD ddname='SYS019' filename='\${SEQFILES}/ZI.DD.ER.DATECARD' disp='i-o' 
#  **************************************************** OUTPUT FILES     
ASSGNDD ddname='SYS011' filename='\${SEQFILES}/\${JOBNAME}.ECS080.ME.BAL.AMTS' disp='o' normal='k' abend='k' recfmt='F'
ASSGNDD  ddname='SYS008' type='SYSOUT' class='A' recfmt='F' recsize='133'
ASSGNDD ddname='SYS013' dataset='ZI.XX.GAAP.NEW' filename='\${SEQFILES}/ZI.XX.GAAP.NEW' recfmt='F' recsize='365' disp='o' normal='k' abend='d' 
ASSGNDD ddname='SYS020' dataset='ZI.EX.FICH080' filename='\${SEQFILES}/ZI.EX.FICH080' disp='o' normal='k' abend='d' recsize='133' recfmt='F' 
ASSGNDD  ddname='SYS022' type='SYSOUT' class='A' recfmt='F' recsize='133'
ASSGNDD ddname='ERMEBL' dataset='CI.DD.LG.ERMEBL' type='VS' filename='ERMEBL' disp='i-o'
ASSGNDD ddname='SYSIN' type='DUMMY' 

EXECPGM pgmname='ECS080' stepname='ECS080' 

################################################################################
LABEL name=CPY080
################################################################################
ASSGNDD ddname='SYS010' filename='\$SYSOUTDIR/$JOBNAME/ECS080/SYS008_\${JON}' disp='i-o'
ASSGNDD ddname='SYS011' filename='\${SEQFILES}/ZI.DL.CIADM.FICH080' disp='o' normal='k' abend='d' recfmt='F' recsize='132' 

EXECPGM pgmname='CIB009L' stepname='CPY080' parm='ALL' 

################################################################################
LABEL name=CPY080B
################################################################################
ASSGNDD ddname='SYS010' filename='\$SYSOUTDIR/$JOBNAME/ECS080/SYS022_\${JON}' disp='i-o'
ASSGNDD ddname='SYS011' filename='\${SEQFILES}/ZI.DL.CIADM.FICH080B' disp='o' normal='k' abend='d' recfmt='F' recsize='132' 

EXECPGM pgmname='CIB009L' stepname='CPY080B' parm='ALL' 

################################################################################
LABEL name=ECS040 
################################################################################
#  ********************************************************************  
#  ***--                                                                 
#  ***        MORTALITY RESERVE EXTRACT SUMMARY                          
#  ***--                                                                 
#  *******************************************************************   
#  ****************************************************  INPUT FILES     
ASSGNDD ddname='SYS010' dataset='ZI.XX.GAAP.NEW' filename='\${SEQFILES}/ZI.XX.GAAP.NEW' disp='i-o' normal='k' abend='k' 
ASSGNDD ddname='SYS019' dataset='ZI.DD.ER.DATECARD' filename='\${SEQFILES}/ZI.DD.ER.DATECARD' disp='i-o' 
#  **************************************************** OUTPUT FILES     
ASSGNDD  ddname='SYS008' type='SYSOUT' class='A' recfmt='F' recsize='133'
ASSGNDD ddname='SYS004' dataset='ZI.WW.INPUT041' filename='\${SEQFILES}/ZI.WW.INPUT041' disp='o' normal='k' abend='d' recfmt='F' recsize='92' 
ASSGNDD ddname='SYS020' dataset='ZI.EX.FICH040' filename='\${SEQFILES}/ZI.EX.FICH040' disp='o' normal='k' abend='d' recsize='133' recfmt='F' 

EXECPGM pgmname='ECS040' stepname='ECS040' 

################################################################################
LABEL name=CPY040
################################################################################
ASSGNDD ddname='SYS010' filename='\${SYSOUTDIR}/$JOBNAME/ECS040/SYS008_${JON}' disp='i-o'
ASSGNDD ddname='SYS011' filename='\${SEQFILES}/ZI.DL.CIADM.FICH040' disp='o' normal='k' abend='d' recfmt='F' recsize='133'

EXECPGM pgmname='CIB009L' stepname='CPY040' parm='ALL'

################################################################################
LABEL name=ECS041 
################################################################################
#  ********************************************************************  
#  ***--                                                                 
#  ***        MISC. ADJUSTMENTS POSTING SUMMARY                          
#  ***--                                                                 
#  ********************************************************************* 
#  ***                                                                   
#  ***     * * *    ECS041 DOES NOT USE INPUT FROM ECS039    * * *       
#  ***                                                                   
#  ********************************************************************* 
#  ****************************************************  INPUT FILES     
ASSGNDD ddname='SYS002' type='DUMMY' 
ASSGNDD ddname='SYS004' dataset='ZI.WW.INPUT041' filename='\${SEQFILES}/ZI.WW.INPUT041' disp='i-o' normal='k' abend='k' 
ASSGNDD ddname='SYS012' dataset='ZI.WW.RETROS' filename='\${SEQFILES}/ZI.WW.RETROS' disp='i-o' normal='k' abend='k' 
ASSGNDD ddname='SYS010' dataset='ZI.XX.EPEC.TEMP.NEW' filename='\${SEQFILES}/ZI.XX.EPEC.TEMP.NEW' disp='i-o' normal='k' abend='k' 
ASSGNDD ddname='ERACCTT' dataset='CI.WW.LG.ERACCT' type='VS' filename='ERACCTT' disp='i-o' 
ASSGNDD ddname='SYS019' dataset='ZI.DD.ER.DATECARD' filename='\${SEQFILES}/ZI.DD.ER.DATECARD' disp='i-o' 
#  **************************************************** OUTPUT FILES     
ASSGNDD  ddname='SYS008' type='SYSOUT' class='A' recfmt='F' recsize='133'
ASSGNDD ddname='SYS011' dataset='ZI.XX.EPEC.NEW' filename='\${SEQFILES}/ZI.XX.EPEC.NEW' disp='o' normal='k' abend='d' recfmt='F' recsize='325' 
ASSGNDD ddname='SYS020' dataset='ZI.EX.FICH041' filename='\${SEQFILES}/ZI.EX.FICH041' disp='o' normal='k' abend='d' recsize='133' recfmt='F' 
ASSGNDD ddname='ERMEBL' dataset='CI.DD.LG.ERMEBL' type='VS' filename='ERMEBL' disp='i-o'
#  ****************************************************  WORK FILES      
ASSGNDD ddname='SYS003' filename='/tmp/${JOBNAME}_WORK041' disp='o' normal='k' abend='d' recfmt='F' recsize='98' 
ASSGNDD ddname='SORTWK01' type='TEMP' 
ASSGNDD ddname='SORTWK02' type='TEMP' 
ASSGNDD ddname='SORTWK03' type='TEMP' 
ASSGNDD ddname='SORTWK04' type='TEMP' 

EXECPGM pgmname='ECS041' stepname='ECS041' 

################################################################################
LABEL name=CPY041
################################################################################
ASSGNDD ddname='SYS010' filename='\$SYSOUTDIR/$JOBNAME/ECS041/SYS008_\${JON}' disp='i-o'
ASSGNDD ddname='SYS011' filename='\${SEQFILES}/ZI.DL.CIADM.FICH041' disp='o' normal='k' abend='d' recfmt='F' recsize='132' 

EXECPGM pgmname='CIB009L' stepname='CPY041' parm='ALL' 

################################################################################

ENDJOB 
################################################################################