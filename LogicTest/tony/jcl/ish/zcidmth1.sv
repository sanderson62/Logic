################################################################################
BEGINJOB mode='MVS'

if ($EBMSYS != 'tony') then
   echo "Job aborting, not tony"
   exit 1
endif
#cicrmbk
################################################################################
LABEL name=RPYBKP 
################################################################################
ONRETCODE MAXRC GT '11' BYPASS scope='STEP'  

#  ************************* BACKUP ERREPY FILE *********************    
ASSGNDD ddname='IN1' dataset='CI.DD.LG.ERREPY' type='VS' filename='ERREPY' disp='i-o' 
ASSGNDD ddname='OUT1' dataset='ZI.ME.LG.ERREPY' filename='\${SEQFILES}/ZI.ME.LG.ERREPY' disp='o' normal='k' abend='d' recfmt='F' recsize='200' 
ASSGNDD ddname='SYSIN' type='INSTREAM'  << !
  REPRO  IFILE(IN1)  -
         OFILE(OUT1)
!

EXECPGM pgmname='IDCAMS' stepname='RPYBKP' 

################################################################################
#cilgdat
################################################################################
LABEL name=EL300A
################################################################################
#  ****     THE FOLLOWING JCL BUILDS THE SYSTEM ENVIRONMENT ( DATE       
#  ****     CARD) FILE FOR THE NEW CREDIT SYSTEM.                        
#  **************************************************** INPUT FILES      
ASSGNDD ddname='ELCNTL' dataset='CI.DD.LG.ELCNTL' type='VS' filename='ELCNTL' disp='i-o' 
ASSGNDD ddname='ELPGMN' dataset='CI.DD.LG.ELPGMN' type='VS' filename='ELPGMN' disp='i-o' 
ASSGNDD ddname='ELPGMS' dataset='CI.DD.LG.ELPGMS' type='VS' filename='ELPGMS' disp='i-o' 
ASSGNDD ddname='ELPGMO' dataset='CI.DD.LG.ELPGMO' type='VS' filename='ELPGMO' disp='i-o' 
#  **************************************************** OUTPUT FILES     
ASSGNDD ddname='SYS008' type='SYSOUT' class='A' 
ASSGNDD ddname='SYS019' filename='$SEQFILES/ZI.DD.ER.DATECARD' disp='o' normal='k' abend='d' recsize='100' recfmt='F' 

ASSGNDD ddname='SYS006' type='INSTREAM'  << !
COLCCID1
CLAS033118                          1
PRTO-524-B
!

EXECPGM pgmname='EL300' stepname='EL300A'

################################################################################
LABEL name=CPY300A
################################################################################
ASSGNDD ddname='SYS010' filename='\${SYSOUTDIR}/$JOBNAME/EL300A/SYS008_${JON}' disp='i-o'
ASSGNDD ddname='SYS011' filename='\${SEQFILES}/ZI.DL.CIADM.FICH300A' disp='o' normal='k' abend='d' recfmt='F' recsize='133'

EXECPGM pgmname='CIB009L' stepname='CPY300A' parm='ALL'

################################################################################
LABEL name=EL300B
################################################################################
#  ****     THE FOLLOWING JCL BUILDS THE SYSTEM ENVIRONMENT ( DATE       
#  ****     CARD) FILE FOR THE NEW CREDIT SYSTEM.                        
#  **************************************************** INPUT FILES      
ASSGNDD ddname='ELCNTL' dataset='CI.DD.LG.ELCNTL' type='VS' filename='ELCNTL' disp='i-o' 
ASSGNDD ddname='ELPGMN' dataset='CI.DD.LG.ELPGMN' type='VS' filename='ELPGMN' disp='i-o' 
ASSGNDD ddname='ELPGMS' dataset='CI.DD.LG.ELPGMS' type='VS' filename='ELPGMS' disp='i-o' 
ASSGNDD ddname='ELPGMO' dataset='CI.DD.LG.ELPGMO' type='VS' filename='ELPGMO' disp='i-o' 
#  **************************************************** OUTPUT FILES     
ASSGNDD ddname='SYS008' type='SYSOUT' class='A' 
ASSGNDD ddname='SYS019' filename='$SEQFILES/ZI.DD.EL582.DATECARD' disp='o' normal='k' abend='d' recsize='100' recfmt='F' 

ASSGNDD ddname='SYS006' type='INSTREAM'  << !
COLCCID1
CLAS033118                          1
PRCO-582-2
PRTO-582-B
FMTO-031-2
PRCO-082-7
PRTO-082-B
!

EXECPGM pgmname='EL300' stepname='EL300B'

################################################################################
LABEL name=EL300C
################################################################################
#  ****                                                                  
#  ****     THE FOLLOWING JCL BUILDS THE SYSTEM ENVIRONMENT ( DATE       
#  ****     CARD) FILE FOR THE NEW CREDIT SYSTEM.                        
#  ****                                                                  
#  ***--------------------------------------------------------------***  
#  ***--             DATE CARD LOAD                               --***  
#  ***--             RESTARTABLE THIS JOB STEP                    --***  
#  ***----------------------------------------------------------------*  
ASSGNDD ddname='SYSOUT' type='SYSOUT' class='J' 
ASSGNDD ddname='SYSLOG' type='SYSOUT' class='J' 
ASSGNDD ddname='SYSLST' type='SYSOUT' class='J' 
#  **************************************************** INPUT FILES      
ASSGNDD ddname='ELCNTL' dataset='CI.DD.LG.ELCNTL' type='VS' filename='ELCNTL' disp='i-o' 
ASSGNDD ddname='ELPGMN' dataset='CI.DD.LG.ELPGMN' type='VS' filename='ELPGMN' disp='i-o' 
ASSGNDD ddname='ELPGMS' dataset='CI.DD.LG.ELPGMS' type='VS' filename='ELPGMS' disp='i-o' 
ASSGNDD ddname='ELPGMO' dataset='CI.DD.LG.ELPGMO' type='VS' filename='ELPGMO' disp='i-o' 
#  **************************************************** OUTPUT FILES     
ASSGNDD ddname='SYS008' type='SYSOUT' class='J' 
ASSGNDD ddname='SYS019' filename='$SEQFILES/ZI.DD.ER.TOTONLY.DATECARD' disp='o' normal='k' abend='d' recsize='100' recfmt='F' 

ASSGNDD ddname='SYS006' type='INSTREAM'  << !
COLCCID1
CLAS033118
PRCO-082-5
PRTO-082-P
PRCO-045-1
!

EXECPGM pgmname='EL300' stepname='EL300C'

################################################################################
LABEL name=EL300D
################################################################################
ASSGNDD ddname='SYSOUT' type='SYSOUT' class='J' 
ASSGNDD ddname='SYSLOG' type='SYSOUT' class='J' 
ASSGNDD ddname='SYSLST' type='SYSOUT' class='J' 
ASSGNDD ddname='ELCNTL' dataset='CI.DD.LG.ELCNTL' type='VS' filename='ELCNTL' disp='i-o' 
ASSGNDD ddname='ELPGMN' dataset='CI.DD.LG.ELPGMN' type='VS' filename='ELPGMN' disp='i-o' 
ASSGNDD ddname='ELPGMS' dataset='CI.DD.LG.ELPGMS' type='VS' filename='ELPGMS' disp='i-o' 
ASSGNDD ddname='ELPGMO' dataset='CI.DD.LG.ELPGMO' type='VS' filename='ELPGMO' disp='i-o' 
ASSGNDD ddname='SYS008' type='SYSOUT' class='J' 
ASSGNDD ddname='SYS019' filename='$SEQFILES/ZI.DD.ER.DATECARD.EC045.YTD.ITD' disp='o' normal='k' abend='d' recsize='100' recfmt='F' 

ASSGNDD ddname='SYS006' type='INSTREAM'  << !
COLCCID1
CLAS033118
PRCO-045-3
!

EXECPGM pgmname='EL300' stepname='EL300D'

################################################################################
LABEL name=EL300E
################################################################################
#  ****     THE FOLLOWING JCL BUILDS THE SYSTEM ENVIRONMENT ( DATE       
#  ****     CARD) FILE FOR THE PEOPLES TRUST MID MONTH BILLING           
#  **************************************************** INPUT FILES      
ASSGNDD ddname='ELCNTL' dataset='CI.DD.LG.ELCNTL' type='VS' filename='ELCNTL' disp='i-o' 
ASSGNDD ddname='ELPGMN' dataset='CI.DD.LG.ELPGMN' type='VS' filename='ELPGMN' disp='i-o' 
ASSGNDD ddname='ELPGMS' dataset='CI.DD.LG.ELPGMS' type='VS' filename='ELPGMS' disp='i-o' 
ASSGNDD ddname='ELPGMO' dataset='CI.DD.LG.ELPGMO' type='VS' filename='ELPGMO' disp='i-o' 
#  **************************************************** OUTPUT FILES     
ASSGNDD ddname='SYS008' type='SYSOUT' class='A' 
ASSGNDD ddname='SYS019' filename='\${SEQFILES}/ZI.DD.ER.DATECARD.PT' disp='o' normal='k' abend='d' recsize='100' recfmt='F' 

ASSGNDD ddname='SYS006' type='INSTREAM'  << !
COLCCID1
CLAS033118                                        PEOPLES TRUST            
PRCO-010-1
PRTO-010-B
!

EXECPGM pgmname='EL300' stepname='EL300E'

################################################################################
LABEL name=EL300F
################################################################################
#  ****     THE FOLLOWING JCL BUILDS THE SYSTEM ENVIRONMENT ( DATE       
#  ****     CARD) FILE FOR THE FIRST PREMIER MID MONTH BILLING           
#  **************************************************** INPUT FILES      
ASSGNDD ddname='ELCNTL' type='VS' filename='ELCNTL' disp='i-o' 
ASSGNDD ddname='ELPGMN' type='VS' filename='ELPGMN' disp='i-o' 
ASSGNDD ddname='ELPGMS' type='VS' filename='ELPGMS' disp='i-o' 
ASSGNDD ddname='ELPGMO' type='VS' filename='ELPGMO' disp='i-o' 
#  **************************************************** OUTPUT FILES     
ASSGNDD ddname='SYS008' type='SYSOUT' class='A' 
ASSGNDD ddname='SYS019' filename='$SEQFILES/ZI.DD.ER.DATECARD.FP' disp='o' normal='k' abend='d' recsize='100' recfmt='F' 

ASSGNDD ddname='SYS006' type='INSTREAM'  << !
COLCCID1
CLAS033118                                        FIRST PREMIER BANK       
PRCO-010-1
PRTO-010-B
!

EXECPGM pgmname='EL300' stepname='EL300F'

################################################################################
LABEL name=EL300G
################################################################################
#  ****     THE FOLLOWING JCL BUILDS THE SYSTEM ENVIRONMENT ( DATE       
#  ****     CARD) FILE FOR THE SUN BANK MID MONTH BILLING                
#  **************************************************** INPUT FILES      
ASSGNDD ddname='ELCNTL' dataset='CI.DD.LG.ELCNTL' type='VS' filename='ELCNTL' disp='i-o' 
ASSGNDD ddname='ELPGMN' dataset='CI.DD.LG.ELPGMN' type='VS' filename='ELPGMN' disp='i-o' 
ASSGNDD ddname='ELPGMS' dataset='CI.DD.LG.ELPGMS' type='VS' filename='ELPGMS' disp='i-o' 
ASSGNDD ddname='ELPGMO' dataset='CI.DD.LG.ELPGMO' type='VS' filename='ELPGMO' disp='i-o' 
#  **************************************************** OUTPUT FILES     
ASSGNDD ddname='SYS008' type='SYSOUT' class='A' 
ASSGNDD ddname='SYS019' filename='\${SEQFILES}/ZI.DD.ER.DATECARD.SB' disp='o' normal='k' abend='d' recsize='100' recfmt='F' 

ASSGNDD ddname='SYS006' type='INSTREAM'  << !
COLCCID1
CLAS033118                                        SUNFLOWER BANK           
PRCO-010-1
PRTO-010-B
!

EXECPGM pgmname='EL300' stepname='EL300G'

################################################################################
LABEL name=EL300H
################################################################################
#  ****     THE FOLLOWING JCL BUILDS THE SYSTEM ENVIRONMENT ( DATE       
#  ****     CARD) FILE FOR THE LOAN OFFICER REPORT EL539
#  **************************************************** INPUT FILES      
ASSGNDD ddname='ELCNTL' dataset='CI.DD.LG.ELCNTL' type='VS' filename='ELCNTL' disp='i-o' 
ASSGNDD ddname='ELPGMN' dataset='CI.DD.LG.ELPGMN' type='VS' filename='ELPGMN' disp='i-o' 
ASSGNDD ddname='ELPGMS' dataset='CI.DD.LG.ELPGMS' type='VS' filename='ELPGMS' disp='i-o' 
ASSGNDD ddname='ELPGMO' dataset='CI.DD.LG.ELPGMO' type='VS' filename='ELPGMO' disp='i-o' 
#  **************************************************** OUTPUT FILES     
ASSGNDD ddname='SYS008' type='SYSOUT' class='A' 
ASSGNDD ddname='SYS019' filename='\${SEQFILES}/ZI.DD.LO.DATECARD' disp='o' normal='k' abend='d' recsize='100' recfmt='F' 

ASSGNDD ddname='SYS006' type='INSTREAM'  << !
COLCCID1
CLAS033118
FMTO-539-1
PRCO-539-1
PRTO-539-P
!

EXECPGM pgmname='EL300' stepname='EL300H'

################################################################################
LABEL name=EL300I
################################################################################
#  ****     THE FOLLOWING JCL BUILDS THE SYSTEM ENVIRONMENT ( DATE       
#  ****     CARD) FILE FOR SUN BANK'S EL539
#  **************************************************** INPUT FILES      
ASSGNDD ddname='ELCNTL' dataset='CI.DD.LG.ELCNTL' type='VS' filename='ELCNTL' disp='i-o' 
ASSGNDD ddname='ELPGMN' dataset='CI.DD.LG.ELPGMN' type='VS' filename='ELPGMN' disp='i-o' 
ASSGNDD ddname='ELPGMS' dataset='CI.DD.LG.ELPGMS' type='VS' filename='ELPGMS' disp='i-o' 
ASSGNDD ddname='ELPGMO' dataset='CI.DD.LG.ELPGMO' type='VS' filename='ELPGMO' disp='i-o' 
#  **************************************************** OUTPUT FILES     
ASSGNDD ddname='SYS008' type='SYSOUT' class='A' 
ASSGNDD ddname='SYS019' filename='\${SEQFILES}/ZI.DD.SUN.DATECARD' disp='o' normal='k' abend='d' recsize='100' recfmt='F' 

ASSGNDD ddname='SYS006' type='INSTREAM'  << !
COLCCID1
CLAS033118
FMTO-539-1
PRCO-539-1
PRTO-539-P
!

EXECPGM pgmname='EL300' stepname='EL300I'

################################################################################
LABEL name=EL300J
################################################################################
#  ****     THE FOLLOWING JCL BUILDS THE SYSTEM ENVIRONMENT ( DATE       
#  ****     CARD) FILE FOR PEOPLE'S TRUST BANK EL539
#  **************************************************** INPUT FILES      
ASSGNDD ddname='ELCNTL' dataset='CI.DD.LG.ELCNTL' type='VS' filename='ELCNTL' disp='i-o' 
ASSGNDD ddname='ELPGMN' dataset='CI.DD.LG.ELPGMN' type='VS' filename='ELPGMN' disp='i-o' 
ASSGNDD ddname='ELPGMS' dataset='CI.DD.LG.ELPGMS' type='VS' filename='ELPGMS' disp='i-o' 
ASSGNDD ddname='ELPGMO' dataset='CI.DD.LG.ELPGMO' type='VS' filename='ELPGMO' disp='i-o' 
#  **************************************************** OUTPUT FILES     
ASSGNDD ddname='SYS008' type='SYSOUT' class='A' 
ASSGNDD ddname='SYS019' filename='\${SEQFILES}/ZI.DD.PTB.DATECARD' disp='o' normal='k' abend='d' recsize='100' recfmt='F' 

ASSGNDD ddname='SYS006' type='INSTREAM'  << !
COLCCID1
CLAS033118
FMTO-539-1
PRCO-539-1
PRTO-539-P
!

EXECPGM pgmname='EL300' stepname='EL300J'

################################################################################
LABEL name=EL300K
################################################################################
#  ****     THE FOLLOWING JCL BUILDS THE SYSTEM ENVIRONMENT ( DATE       
#  ****     CARD) FILE FOR FIRST PREMIER  BANK EL539
#  **************************************************** INPUT FILES      
ASSGNDD ddname='ELCNTL' type='VS' filename='ELCNTL' disp='i-o' 
ASSGNDD ddname='ELPGMN' type='VS' filename='ELPGMN' disp='i-o' 
ASSGNDD ddname='ELPGMS' type='VS' filename='ELPGMS' disp='i-o' 
ASSGNDD ddname='ELPGMO' type='VS' filename='ELPGMO' disp='i-o' 
#  **************************************************** OUTPUT FILES     
ASSGNDD ddname='SYS008' type='SYSOUT' class='A' 
ASSGNDD ddname='SYS019' filename='$SEQFILES/ZI.DD.FPB.DATECARD' disp='o' normal='k' abend='d' recsize='100' recfmt='F' 

ASSGNDD ddname='SYS006' type='INSTREAM'  << !
COLCCID1
CLAS033118
FMTO-539-1
PRCO-539-1
PRTO-539-P
!

EXECPGM pgmname='EL300' stepname='EL300K'

################################################################################
LABEL name=EL300L
################################################################################
ASSGNDD ddname='ELCNTL' dataset='CI.DD.LG.ELCNTL' type='VS' filename='ELCNTL' disp='i-o' 
ASSGNDD ddname='ELPGMN' dataset='CI.DD.LG.ELPGMN' type='VS' filename='ELPGMN' disp='i-o' 
ASSGNDD ddname='ELPGMO' dataset='CI.DD.LG.ELPGMO' type='VS' filename='ELPGMO' disp='i-o' 
ASSGNDD ddname='ELPGMS' dataset='CI.DD.LG.ELPGMS' type='VS' filename='ELPGMS' disp='i-o' 
ASSGNDD ddname='SYS019' filename='$SEQFILES/ZI.DD.LG.VENDOR.DATECARD' disp='o' normal='k' abend='k' recfmt='F' recsize='100' 
ASSGNDD ddname='SYS008' type='SYSOUT' class='M' 
ASSGNDD ddname='SYS006' type='INSTREAM'  << !
COLCCID1
CLAS033118                          1
!

EXECPGM pgmname='EL300' stepname='EL300L'

################################################################################
LABEL name=EL300M
################################################################################
#  ***--------------------------------------------------------------***  
#  ***--                                                          --***  
#  ***--             DATE CARD LOAD                               --***  
#  ***--             RESTARTABLE THIS JOB STEP                    --***  
#  ***----------------------------------------------------------------*  
#  **************************************************** INPUT FILES      
ASSGNDD ddname='ELCNTL' dataset='CI.DD.LG.ELCNTL' type='VS' filename='ELCNTL' disp='i-o' 
ASSGNDD ddname='ELPGMN' dataset='CI.DD.LG.ELPGMN' type='VS' filename='ELPGMN' disp='i-o' 
ASSGNDD ddname='ELPGMS' dataset='CI.DD.LG.ELPGMS' type='VS' filename='ELPGMS' disp='i-o' 
ASSGNDD ddname='ELPGMO' dataset='CI.DD.LG.ELPGMO' type='VS' filename='ELPGMO' disp='i-o' 
#  **************************************************** OUTPUT FILES     
ASSGNDD ddname='SYS008' type='SYSOUT' class='J' 
ASSGNDD ddname='SYS019' filename='\${SEQFILES}/ZI.DD.ER333A.DATECARD' disp='o' normal='k' abend='d' recsize='100' recfmt='F' 

ASSGNDD ddname='SYS006' type='INSTREAM'  << !
COLCCID1
CLAS033118
PRCO-333-1
!

EXECPGM pgmname='EL300' stepname='EL300M'

################################################################################
LABEL name=EL300N
################################################################################
#  ****                                                                  
#  ****     THE FOLLOWING JCL BUILDS THE SYSTEM ENVIRONMENT ( DATE       
#  ****     CARD) FILE FOR THE NEW CREDIT SYSTEM.                        
#  ****                                                                  
#  ***--------------------------------------------------------------***  
#  ***--             DATE CARD LOAD                               --***  
#  ***--             RESTARTABLE THIS JOB STEP                    --***  
#  ***----------------------------------------------------------------*  
#  **************************************************** INPUT FILES      
ASSGNDD ddname='ELCNTL' dataset='CI.DD.LG.ELCNTL' type='VS' filename='ELCNTL' disp='i-o' 
ASSGNDD ddname='ELPGMN' dataset='CI.DD.LG.ELPGMN' type='VS' filename='ELPGMN' disp='i-o' 
ASSGNDD ddname='ELPGMS' dataset='CI.DD.LG.ELPGMS' type='VS' filename='ELPGMS' disp='i-o' 
ASSGNDD ddname='ELPGMO' dataset='CI.DD.LG.ELPGMO' type='VS' filename='ELPGMO' disp='i-o' 
#  **************************************************** OUTPUT FILES     
ASSGNDD ddname='SYS008' type='SYSOUT' class='J' 
ASSGNDD ddname='SYS019' filename='\${SEQFILES}/ZI.DD.ER333B.DATECARD' disp='o' normal='k' abend='d' recsize='100' recfmt='F' 

ASSGNDD ddname='SYS006' type='INSTREAM'  << !
COLCCID1
CLAS033118
PRCO-333-2
!

EXECPGM pgmname='EL300' stepname='EL300N'

################################################################################
LABEL name=EL300O
################################################################################
#  ****                                                                  
#  ****     THE FOLLOWING JCL BUILDS THE SYSTEM ENVIRONMENT ( DATE       
#  ****     CARD) FILE FOR THE NEW CREDIT SYSTEM.                        
#  ****                                                                  
#  ***--------------------------------------------------------------***  
#  ***--             DATE CARD LOAD                               --***  
#  ***--             RESTARTABLE THIS JOB STEP                    --***  
#  ***----------------------------------------------------------------*  
#  **************************************************** INPUT FILES      
ASSGNDD ddname='ELCNTL' dataset='CI.DD.LG.ELCNTL' type='VS' filename='ELCNTL' disp='i-o' 
ASSGNDD ddname='ELPGMN' dataset='CI.DD.LG.ELPGMN' type='VS' filename='ELPGMN' disp='i-o' 
ASSGNDD ddname='ELPGMS' dataset='CI.DD.LG.ELPGMS' type='VS' filename='ELPGMS' disp='i-o' 
ASSGNDD ddname='ELPGMO' dataset='CI.DD.LG.ELPGMO' type='VS' filename='ELPGMO' disp='i-o' 
#  **************************************************** OUTPUT FILES     
ASSGNDD ddname='SYS008' type='SYSOUT' class='J' 
ASSGNDD ddname='SYS019' filename='\${SEQFILES}/ZI.DD.ER333C.DATECARD' disp='o' normal='k' abend='d' recsize='100' recfmt='F' 

ASSGNDD ddname='SYS006' type='INSTREAM'  << !
COLCCID1
CLAS033118
PRCO-333-3
!

EXECPGM pgmname='EL300' stepname='EL300O'

################################################################################
LABEL name=EL300P
################################################################################
#  ****                                                                  
#  ****     THE FOLLOWING JCL BUILDS THE SYSTEM ENVIRONMENT ( DATE       
#  ****     CARD) FILE FOR THE NEW CREDIT SYSTEM.                        
#  ****                                                                  
#  ***--------------------------------------------------------------***  
#  ***--             DATE CARD LOAD                               --***  
#  ***--             RESTARTABLE THIS JOB STEP                    --***  
#  ***----------------------------------------------------------------*  
#  **************************************************** INPUT FILES      
ASSGNDD ddname='ELCNTL' dataset='CI.DD.LG.ELCNTL' type='VS' filename='ELCNTL' disp='i-o' 
ASSGNDD ddname='ELPGMN' dataset='CI.DD.LG.ELPGMN' type='VS' filename='ELPGMN' disp='i-o' 
ASSGNDD ddname='ELPGMS' dataset='CI.DD.LG.ELPGMS' type='VS' filename='ELPGMS' disp='i-o' 
ASSGNDD ddname='ELPGMO' dataset='CI.DD.LG.ELPGMO' type='VS' filename='ELPGMO' disp='i-o' 
#  **************************************************** OUTPUT FILES     
ASSGNDD ddname='SYS008' type='SYSOUT' class='J' 
ASSGNDD ddname='SYS019' filename='\${SEQFILES}/ZI.DD.ER333D.DATECARD' disp='o' normal='k' abend='d' recsize='100' recfmt='F' 

ASSGNDD ddname='SYS006' type='INSTREAM'  << !
COLCCID1
CLAS033118
PRCO-333-4
!

EXECPGM pgmname='EL300' stepname='EL300P'

################################################################################
LABEL name=EL300Q
################################################################################
#  ***--------------------------------------------------------------***  
#  ***--             DATE CARD LOAD                               --***  
#  ***--             RESTARTABLE THIS JOB STEP                    --***  
#  ***----------------------------------------------------------------*  
#  **************************************************** INPUT FILES      
ASSGNDD ddname='ELCNTL' dataset='CI.DD.LG.ELCNTL' type='VS' filename='ELCNTL' disp='i-o' 
ASSGNDD ddname='ELPGMN' dataset='CI.DD.LG.ELPGMN' type='VS' filename='ELPGMN' disp='i-o' 
ASSGNDD ddname='ELPGMS' dataset='CI.DD.LG.ELPGMS' type='VS' filename='ELPGMS' disp='i-o' 
ASSGNDD ddname='ELPGMO' dataset='CI.DD.LG.ELPGMO' type='VS' filename='ELPGMO' disp='i-o' 
#  **************************************************** OUTPUT FILES     
ASSGNDD ddname='SYS008' type='SYSOUT' class='J' 
ASSGNDD ddname='SYS019' filename='\${SEQFILES}/ZI.DD.ER333E.DATECARD' disp='o' normal='k' abend='d' recsize='100' recfmt='F' 

ASSGNDD ddname='SYS006' type='INSTREAM'  << !
COLCCID1
CLAS033118
PRCO-333-5
!

EXECPGM pgmname='EL300' stepname='EL300Q'

################################################################################

#cilgm355
################################################################################
LABEL name=EL355
################################################################################
ONRETCODE MAXRC GT '11' BYPASS scope='STEP'  

ASSGNDD ddname='SORTWK01' type='TEMP' 
ASSGNDD ddname='SORTWK02' type='TEMP' 
ASSGNDD ddname='SORTWK03' type='TEMP' 
ASSGNDD ddname='ELMSTR' type='VS' filename='ELMSTR' disp='i-o' 
ASSGNDD ddname='ELTRLR' type='VS' filename='ELTRLR' disp='i-o' 
ASSGNDD ddname='ELDENY' type='VS' filename='ELDENY' disp='i-o' 
ASSGNDD ddname='SYS019' filename='$SEQFILES/ZI.DD.ER.DATECARD' disp='i-o' 
ASSGNDD ddname='SYS008' type='SYSOUT' class='A' 

EXECPGM pgmname='EL355' stepname='EL355'

################################################################################
LABEL name=CPY355
################################################################################
ASSGNDD ddname='SYS010' filename='$SYSOUTDIR/$JOBNAME/EL355/SYS008_$JON' disp='i-o'
ASSGNDD ddname='SYS011' filename='$SEQFILES/ZI.DL.CIADM.FICH355' disp='o' normal='k' abend='d' recfmt='F' recsize='132'

EXECPGM pgmname='CIB009L' stepname='CPY355' parm='ALL'

################################################################################
LABEL name=PERL355
################################################################################
#
# This step reads the files from above and creates pdf files
#
ASSGNDD ddname='SYSIN' type='INSTREAM'  << !
/export/home/mtpadmin/bin/elrpt2pdf $SEQFILES/ZI.DL.CIADM.FICH355 $SEQFILES/ZI.EL355.pdf
!
EXECPGM pgmname='BPXBATCH' stepname='PERL355' parm='SH'

################################################################################

#cilgm356
################################################################################
LABEL name=EL356
################################################################################
ONRETCODE MAXRC GT '11' BYPASS scope='STEP'  

ASSGNDD ddname='SORTWK01' type='TEMP' 
ASSGNDD ddname='SORTWK02' type='TEMP' 
ASSGNDD ddname='SORTWK03' type='TEMP' 
ASSGNDD ddname='ELMSTR' type='VS' filename='ELMSTR' disp='i-o' 
ASSGNDD ddname='ELTRLR' type='VS' filename='ELTRLR' disp='i-o' 
ASSGNDD ddname='ELDENY' type='VS' filename='ELDENY' disp='i-o' 
ASSGNDD ddname='SYS019' filename='$SEQFILES/ZI.DD.ER.DATECARD' disp='i-o' 
ASSGNDD ddname='SYS008' filename='$SEQFILES/ZI.EX.FICH356' disp='o' normal='k' abend='k' recfmt='F' recsize='133'

EXECPGM pgmname='EL356' stepname='EL356'

################################################################################
LABEL name=CPY356
################################################################################
#ASSGNDD ddname='SYS010' filename='$SYSOUTDIR/$JOBNAME/EL356/SYS008_$JON' disp='i-o'
ASSGNDD ddname='SYS010' filename='$SEQFILES/ZI.EX.FICH356' disp='i-o'
ASSGNDD ddname='SYS011' filename='$SEQFILES/ZI.DL.CIADM.FICH356' disp='o' normal='k' abend='d' recfmt='F' recsize='132'

EXECPGM pgmname='CIB009L' stepname='CPY356' parm='ALL'

################################################################################
LABEL name=PERL356
################################################################################
#
# This step reads the files from above and creates pdf files
#
ASSGNDD ddname='SYSIN' type='INSTREAM'  << !
/export/home/mtpadmin/bin/elrpt2pdf $SEQFILES/ZI.DL.CIADM.FICH356 $SEQFILES/ZI.EL356.pdf
!
EXECPGM pgmname='BPXBATCH' stepname='PERL356' parm='SH'

################################################################################

#cilgm542
################################################################################
LABEL name=EL542 
################################################################################
#  ***                                                                   
#  *** CREATE MONTH-END BALANCE COMPANY RECORD FOR JOBS THAT USE ERMEBL FILE    
#  ***                                
#  ******************************************************************    
ASSGNDD ddname='SYS019' dataset='ZI.DD.ER.DATECARD' filename='\${SEQFILES}/ZI.DD.ER.DATECARD' disp='i-o' 

ASSGNDD ddname='ERMEBL' dataset='CI.DD.LG.ERMEBL' type='VS' filename='ERMEBL' disp='i-o'

EXECPGM pgmname='EL542' stepname='EL542' 


################################################################################

#ciclmo3g
################################################################################
LABEL name=STEP1 
################################################################################
ASSGNDD ddname='SYSUT1' disp='i-o' dataset='ZI.XX.CLMDLY.EXTR' filename='\${SEQFILES}/ZI.XX.CLMDLY.EXTR' 
ASSGNDD ddname='SYSUT2' disp='o' normal='k' abend='d' dataset='ZI.ME.CLMDLY.EXTR' filename='\${SEQFILES}/ZI.ME.CLMDLY.EXTR' recfmt='F' recsize='319' 
ASSGNDD ddname='SYSIN' type='DUMMY' 

EXECPGM pgmname='IEBGENER' stepname='STEP1' 

################################################################################
LABEL name=EL588
################################################################################
#
# Creates Open Credit Life Claims report and reports the distribution of 
#    all open claims between Life and Disability
#
################################################################################
ASSGNDD ddname='SYS020' type='DUMMY' 
ASSGNDD ddname='SYS019' dataset='ZI.DD.ER.DATECARD' filename='\${SEQFILES}/ZI.DD.ER.DATECARD' disp='i-o' 
ASSGNDD ddname='SORTLIB' dataset='SYS1.SORTLIB' filename='\${SEQFILES}/SYS1.SORTLIB' disp='i-o' 
ASSGNDD ddname='SORTWK01' type='TEMP' 
ASSGNDD ddname='SYS010' dataset='ZI.ME.CLMDLY.EXTR' filename='\${SEQFILES}/ZI.ME.CLMDLY.EXTR' disp='i-o' normal='k' abend='k' recfmt='F' recsize='319' 
ASSGNDD  ddname='SYS008' type='SYSOUT' 

EXECPGM pgmname='EL588' stepname='EL588' 

################################################################################
LABEL name=CPY588
################################################################################
ASSGNDD ddname='SYS010' filename='$SYSOUTDIR/$JOBNAME/EL588/SYS008_\${JON}' disp='i-o'
ASSGNDD ddname='SYS011' filename='\${SEQFILES}/ZI.DL.CIADM.FICH588' disp='o' normal='k' abend='d' recfmt='F' recsize='132'

EXECPGM pgmname='CIB009L' stepname='CPY588' parm='ALL' 

################################################################################
LABEL name=EL585M 
################################################################################
#
# Creates a MTD Claims Activity Summary for Actuary
#
################################################################################
ASSGNDD ddname='SYS019' dataset='ZI.DD.ER.DATECARD' filename='\${SEQFILES}/ZI.DD.ER.DATECARD' disp='i-o' 
ASSGNDD ddname='ELMSTR' dataset='CI.DD.LG.ELMSTR' type='VS' filename='ELMSTR' disp='i-o' 
ASSGNDD ddname='ELTRLR' dataset='CI.DD.LG.ELTRLR' type='VS' filename='ELTRLR' disp='i-o' 
ASSGNDD  ddname='SYS008' type='SYSOUT'

EXECPGM pgmname='EL585M' stepname='EL585M' parm='2018030120180331'

################################################################################
LABEL name=CPY585M
################################################################################
ASSGNDD ddname='SYS010' filename='$SYSOUTDIR/$JOBNAME/EL585M/SYS008_\${JON}' disp='i-o'
ASSGNDD ddname='SYS011' filename='\${SEQFILES}/ZI.DL.CIADM.FICH585M' disp='o' normal='k' abend='d' recfmt='F' recsize='132'

EXECPGM pgmname='CIB009L' stepname='CPY585M' parm='ALL' 

################################################################################

#cilgm05
################################################################################
LABEL name=BACKUP 
################################################################################
#  ********************************************************************* 
#  **--      BACKUP PENDING NEW BUSINESS FILE 
#  **--      BACKUP PENDING FILE
#  **--      BACKUP ACCOUNT MASTER 
#  **--      BACKUP COMMISSION TABLE 
#  **--      BACKUP RATE TABLE 
#  **--      BACKUP REINSURANCE TABLE 
#  ********************************************************************* 
ASSGNDD ddname='ERPNDB' dataset='CI.DD.LG.ERPNDB' type='VS' filename='ERPNDB' disp='i-o' 
ASSGNDD ddname='ERPNDBO' dataset='ZI.ME.ERPNDB.M05' filename='\${SEQFILES}/ZI.ME.ERPNDB.M05' disp='o' normal='k' abend='d' recfmt='F' recsize='585' 
ASSGNDD ddname='ERACCT' dataset='CI.DD.LG.ERACCT' type='VS' filename='ERACCT' disp='i-o' 
ASSGNDD ddname='ERACCTO' dataset='ZI.ME.ERACCT.M05' filename='\${SEQFILES}/ZI.ME.ERACCT.M05' disp='o' normal='k' abend='d' recfmt='F' recsize='2000' 
ASSGNDD ddname='ERPNDC' dataset='CI.DD.LG.ERPNDC' type='VS' filename='ERPNDC' disp='i-o' 
ASSGNDD ddname='ERPNDCO' dataset='ZI.ME.ERPNDC.M05' filename='\${SEQFILES}/ZI.ME.ERPNDC.M05' disp='o' normal='k' abend='d' recfmt='F' recsize='500' 
ASSGNDD ddname='ERRATE' dataset='CI.DD.LG.ERRATE' type='VS' filename='ERRATE' disp='i-o' 
ASSGNDD ddname='ERRATEO' dataset='ZI.ME.ERRATE.M05' filename='\${SEQFILES}/ZI.ME.ERRATE.M05' disp='o' normal='k' abend='d' recfmt='F' recsize='1765'
ASSGNDD ddname='ERCTBL' dataset='CI.DD.LG.ERCTBL' type='VS' filename='ERCTBL' disp='i-o' 
ASSGNDD ddname='ERCTBLO' dataset='ZI.ME.ERCTBL.M05' filename='\${SEQFILES}/ZI.ME.ERCTBL.M05' disp='o' normal='k' abend='d' recfmt='F' recsize='200' 
ASSGNDD ddname='ERREIN' dataset='CI.DD.LG.ERREIN' type='VS' filename='ERREIN' disp='i-o' 
ASSGNDD ddname='ERREINO' dataset='ZI.ME.ERREIN.M05' filename='\${SEQFILES}/ZI.ME.ERREIN.M05' disp='o' normal='k' abend='d' recfmt='F' recsize='4000' 
ASSGNDD ddname='SYSIN' type='INSTREAM'  << !
  REPRO  IFILE(ERPNDB) OFILE(ERPNDBO)                                   
  REPRO  IFILE(ERPNDC) OFILE(ERPNDCO)                                   
  REPRO  IFILE(ERACCT) OFILE(ERACCTO)                                   
  REPRO  IFILE(ERCTBL) OFILE(ERCTBLO)                                   
  REPRO  IFILE(ERRATE) OFILE(ERRATEO)                                   
  REPRO  IFILE(ERREIN) OFILE(ERREINO)                                   
!

EXECPGM pgmname='IDCAMS' stepname='BACKUP' 

################################################################################
LABEL name=DEFCTBL
################################################################################
ASSGNDD ddname='SYSIN' type='INSTREAM'  << !
unikixbld -i -dERCTBLT
!
EXECPGM pgmname='BPXBATCH' stepname='DEFCTBL' parm='SH'

################################################################################
LABEL name=EL506 
################################################################################
ASSGNDD ddname='ERCTBL' dataset='CI.DD.LG.ERCTBL' type='VS' filename='ERCTBL' disp='i-o' 
ASSGNDD ddname='ELCNTL' dataset='CI.DD.LG.ELCNTL' type='VS' filename='ELCNTL' disp='i-o' 
ASSGNDD ddname='SYS019' dataset='ZI.DD.ER.DATECARD' filename='\${SEQFILES}/ZI.DD.ER.DATECARD' disp='i-o' 
ASSGNDD ddname='ELREPT' type='DUMMY' 
ASSGNDD  ddname='SYS008' type='SYSOUT' class='A'  
ASSGNDD ddname='ERCTBLT' dataset='CI.WW.LG.ERCTBL' type='VS' filename='ERCTBLT' disp='i-o' 
ASSGNDD ddname='SYS020' dataset='ZI.EX.FICH506' filename='\${SEQFILES}/ZI.EX.FICH506' disp='o' normal='k' abend='d' recfmt='F' recsize='133' 

EXECPGM pgmname='EL506' stepname='EL506' 

################################################################################
LABEL name=CPY506
################################################################################
ASSGNDD ddname='SYS010' filename='\${SYSOUTDIR}/$JOBNAME/EL506/SYS008_${JON}' disp='i-o'
ASSGNDD ddname='SYS011' filename='\${SEQFILES}/ZI.DL.CIADM.FICH506' disp='o' normal='k' abend='d' recfmt='F' recsize='133'

EXECPGM pgmname='CIB009L' stepname='CPY506' parm='ALL' 

################################################################################
LABEL name=DEFRTBL
################################################################################
ASSGNDD ddname='SYSIN' type='INSTREAM'  << !
unikixbld -i -dERRTBLT
!
EXECPGM pgmname='BPXBATCH' stepname='DEFRTBL' parm='SH'

################################################################################
LABEL name=EL508 
################################################################################
ASSGNDD ddname='ERREIN' dataset='CI.DD.LG.ERREIN' type='VS' filename='ERREIN' disp='i-o' 
ASSGNDD ddname='ELCNTL' dataset='CI.DD.LG.ELCNTL' type='VS' filename='ELCNTL' disp='i-o' 
ASSGNDD ddname='SYS019' dataset='ZI.DD.ER.DATECARD' filename='\${SEQFILES}/ZI.DD.ER.DATECARD' disp='i-o' 
ASSGNDD ddname='ELREPT' type='DUMMY' 
ASSGNDD  ddname='SYS008' type='SYSOUT' class='A'  
ASSGNDD ddname='ERRTBLT' dataset='CI.WW.LG.ERREIN' type='VS' filename='ERRTBLT' disp='i-o' 
ASSGNDD ddname='SYS020' dataset='ZI.EX.FICH508' filename='\${SEQFILES}/ZI.EX.FICH508' disp='o' normal='k' abend='d' recfmt='F' recsize='133' 

EXECPGM pgmname='EL508' stepname='EL508' 

################################################################################
LABEL name=CPY508
################################################################################
ASSGNDD ddname='SYS010' filename='\${SYSOUTDIR}/$JOBNAME/EL508/SYS008_${JON}' disp='i-o'
ASSGNDD ddname='SYS011' filename='\${SEQFILES}/ZI.DL.CIADM.FICH508' disp='o' normal='k' abend='d' recfmt='F' recsize='133'

EXECPGM pgmname='CIB009L' stepname='CPY508' parm='ALL' 

################################################################################
LABEL name=DEFRATE
################################################################################
ASSGNDD ddname='SYSIN' type='INSTREAM'  << !
unikixbld -i -dERRATET
!
EXECPGM pgmname='BPXBATCH' stepname='DEFRATE' parm='SH'

################################################################################
LABEL name=EL504 
################################################################################
ASSGNDD ddname='ERRATE' dataset='CI.DD.LG.ERRATE' type='VS' filename='ERRATE' disp='i-o' 
ASSGNDD ddname='ELCNTL' dataset='CI.DD.LG.ELCNTL' type='VS' filename='ELCNTL' disp='i-o' 
ASSGNDD ddname='SYS019' dataset='ZI.DD.ER.DATECARD' filename='\${SEQFILES}/ZI.DD.ER.DATECARD' disp='i-o' 
ASSGNDD ddname='ELREPT' type='DUMMY' 
ASSGNDD  ddname='SYS008' type='SYSOUT' class='A'  
ASSGNDD ddname='ERRATET' dataset='CI.WW.LG.ERRATE' type='VS' filename='ERRATET' disp='i-o' 
ASSGNDD ddname='SYS020' dataset='ZI.EX.FICH504' filename='\${SEQFILES}/ZI.EX.FICH504' disp='o' normal='k' abend='d' recfmt='F' recsize='133' 

EXECPGM pgmname='EL504' stepname='EL504' 

################################################################################
LABEL name=CPY504
################################################################################
ASSGNDD ddname='SYS010' filename='\${SYSOUTDIR}/$JOBNAME/EL504/SYS008_${JON}' disp='i-o'
ASSGNDD ddname='SYS011' filename='\${SEQFILES}/ZI.DL.CIADM.FICH504' disp='o' normal='k' abend='d' recfmt='F' recsize='133'

EXECPGM pgmname='CIB009L' stepname='CPY504' parm='ALL' 

################################################################################
LABEL name=DEFACCT
################################################################################
ASSGNDD ddname='SYSIN' type='INSTREAM'  << !
unikixbld -i -dERACCTT
!
EXECPGM pgmname='BPXBATCH' stepname='DEFACCT' parm='SH'

################################################################################
LABEL name=EL502 
################################################################################
ASSGNDD ddname='ERACCT' dataset='CI.DD.LG.ERACCT' type='VS' filename='ERACCT' disp='i-o' 
ASSGNDD ddname='ELCNTL' dataset='CI.DD.LG.ELCNTL' type='VS' filename='ELCNTL' disp='i-o' 
ASSGNDD ddname='ERCTBL' dataset='CI.DD.LG.ERCTBL' type='VS' filename='ERCTBL' disp='i-o' 
ASSGNDD ddname='ERREIN' dataset='CI.DD.LG.ERREIN' type='VS' filename='ERREIN' disp='i-o' 
ASSGNDD ddname='SYS019' dataset='ZI.DD.ER.DATECARD' filename='\${SEQFILES}/ZI.DD.ER.DATECARD' disp='i-o' 
ASSGNDD ddname='ELREPT' type='DUMMY' 
ASSGNDD  ddname='SYS008' type='SYSOUT' class='A' 
ASSGNDD ddname='ERACCTT' dataset='CI.WW.LG.ERACCT' type='VS' filename='ERACCTT' disp='i-o' 
ASSGNDD ddname='SYS020' dataset='ZI.EX.FICH502' filename='\${SEQFILES}/ZI.EX.FICH502' disp='o' normal='k' abend='d' recfmt='F' recsize='133' 

EXECPGM pgmname='EL502' stepname='EL502' 

################################################################################
LABEL name=CPY502
################################################################################
ASSGNDD ddname='SYS010' filename='\${SYSOUTDIR}/$JOBNAME/EL502/SYS008_${JON}' disp='i-o'
ASSGNDD ddname='SYS011' filename='\${SEQFILES}/ZI.DL.CIADM.FICH502' disp='o' normal='k' abend='d' recfmt='F' recsize='133'

EXECPGM pgmname='CIB009L' stepname='CPY502' parm='ALL' 

################################################################################
LABEL name=EL510 
################################################################################
#  ******************************************************************    
#  ***--  RESTART AT EL510 STEP.                                         
#  ******************************************************************    
ASSGNDD ddname='ERCOMP' dataset='CI.DD.LG.ERCOMP' type='VS' filename='ERCOMP' disp='i-o' 
ASSGNDD ddname='ELCNTL' dataset='CI.DD.LG.ELCNTL' type='VS' filename='ELCNTL' disp='i-o' 
ASSGNDD ddname='SYS019' dataset='ZI.DD.ER.DATECARD' filename='\${SEQFILES}/ZI.DD.ER.DATECARD' disp='i-o' 
ASSGNDD ddname='ELREPT' type='DUMMY' 
ASSGNDD  ddname='SYS008' type='SYSOUT' class='A' 
ASSGNDD ddname='SYS010' dataset='ZI.DD.COMM' filename='\${SEQFILES}/ZI.DD.COMM' disp='o' normal='k' abend='d' recsize='700' recfmt='F' 
ASSGNDD ddname='SYS020' dataset='ZI.EX.FICH510' filename='\${SEQFILES}/ZI.EX.FICH510' disp='o' normal='k' abend='d' recfmt='F' recsize='133' 

EXECPGM pgmname='EL510' stepname='EL510' 

################################################################################
LABEL name=CPY510
################################################################################
ASSGNDD ddname='SYS010' filename='\${SYSOUTDIR}/$JOBNAME/EL510/SYS008_${JON}' disp='i-o'
ASSGNDD ddname='SYS011' filename='\${SEQFILES}/ZI.DL.CIADM.FICH510' disp='o' normal='k' abend='d' recfmt='F' recsize='133'

EXECPGM pgmname='CIB009L' stepname='CPY510' parm='ALL' 

################################################################################
LABEL name=BKPACCT 
################################################################################
#  ********************************************************************* 
#  **--                                                                  
#  **--      BACKUP BATCH ACCOUNT MASTER FILE 
#  **--                                                                  
#  **-- RESTART: DELETE THE TAPE CREATED IN THIS STEP BEFORE RESTARTING  
#  **--                                                                  
#  ********************************************************************* 
ASSGNDD ddname='ERACCTT' dataset='CI.WW.LG.ERACCT' type='VS' filename='ERACCTT' disp='i-o' 
ASSGNDD ddname='OUTPUT' dataset='ZI.XX.ACCT' filename='\${SEQFILES}/ZI.XX.ACCT' disp='o' normal='k' abend='d' recfmt='F' recsize='2000' 
ASSGNDD ddname='SYSIN' type='INSTREAM'  << !
  REPRO  IFILE(ERACCTT) -                                               
         OFILE(OUTPUT)                                                  
!

EXECPGM pgmname='IDCAMS' stepname='BKPACCT' 

################################################################################
LABEL name=BKPCOMM 
################################################################################
#  ********************************************************************* 
#  **--                                                                  
#  **--      BACKUP BATCH COMM FILE
#  **--                                                                  
#  **-- RESTART: DELETE THE TAPE CREATED IN THIS STEP BEFORE RESTARTING  
#  **--                                                                  
#  ********************************************************************* 
ASSGNDD ddname='INPUT' dataset='ZI.DD.COMM' filename='\${SEQFILES}/ZI.DD.COMM' disp='i-o' 
ASSGNDD ddname='OUTPUT' dataset='ZI.XX.COMM' filename='\${SEQFILES}/ZI.XX.COMM' disp='o' normal='k' abend='d' recfmt='F' recsize='700' 
ASSGNDD ddname='SYSIN' type='INSTREAM'  << !
  REPRO  IFILE(INPUT) -                                                 
         OFILE(OUTPUT)                                                  
!

EXECPGM pgmname='IDCAMS' stepname='BKPCOMM' 

################################################################################

#cilgm10
################################################################################
LABEL name=EL524 
################################################################################
#  ******************************************************************    
#  **--                                                                  
#  **--          PENDING CLAIMS FILE LOAD                                
#  **--                                                                  
#  ******************************************************************    
#  ****************************************************  INPUT FILES     
ASSGNDD ddname='ELREPT' dataset='CI.DD.LG.ELREPT' type='VS' filename='ELREPT' disp='i-o' 
ASSGNDD ddname='ERACCT' dataset='CI.DD.LG.ERACCT' type='VS' filename='ERACCT' disp='i-o' 
ASSGNDD ddname='ELCERT' dataset='CI.DD.LG.ELCERT' type='VS' filename='ELCERT' disp='i-o' 
ASSGNDD ddname='ELCNTL' dataset='CI.DD.LG.ELCNTL' type='VS' filename='ELCNTL' disp='i-o' 
ASSGNDD ddname='SYS010' dataset='ZI.ME.CLMDLY.EXTR' filename='\${SEQFILES}/ZI.ME.CLMDLY.EXTR' disp='i-o' normal='k' abend='k' 
ASSGNDD ddname='SYS019' filename='\${SEQFILES}/ZI.DD.ER.DATECARD' disp='i-o' 
#  **************************************************** I/O FILES        
ASSGNDD ddname='ERPNDB' dataset='CI.DD.LG.ERPNDB' type='VS' filename='ERPNDB' disp='i-o' 
ASSGNDD ddname='ERPNDC' dataset='CI.DD.LG.ERPNDC' type='VS' filename='ERPNDC' disp='i-o' 
ASSGNDD ddname='ERMEBL' dataset='CI.DD.LG.ERMEBL' type='VS' filename='ERMEBL' disp='i-o'
#  **************************************************** OUTPUT FILES     
ASSGNDD ddname='SYS011' filename='\${SEQFILES}/\${JOBNAME}.EL524.ME.BAL.AMTS' disp='o' normal='k' abend='k' recfmt='F'
ASSGNDD  ddname='SYS008' type='SYSOUT' class='A'  
ASSGNDD ddname='SYS020' dataset='ZI.EX.FICH524' filename='\${SEQFILES}/ZI.EX.FICH524' disp='o' normal='k' abend='d' recsize='133' recfmt='F' 

EXECPGM pgmname='EL524' stepname='EL524' 

################################################################################
LABEL name=CPY524
################################################################################
ASSGNDD ddname='SYS010' filename='\${SEQFILES}/ZI.EX.FICH524' disp='i-o'
ASSGNDD ddname='SYS011' filename='\${SEQFILES}/ZI.DL.CIADM.FICH524' disp='o' normal='k' abend='d' recfmt='F' recsize='132' 

EXECPGM pgmname='CIB009L' stepname='CPY524' parm='ALL' 

################################################################################
LABEL name=EL517 
################################################################################
#  ********************************************************************* 
#  **--                                                                  
#  **--          EL517 - BATCH EDIT PROGRAM                              
#  **--  SEE RESTART INSTRUCTIONS BEFORE RESTARTING                      
#  **--                                                                  
#  ******************************************************************    
ASSGNDD ddname='ELCNTL' dataset='CI.DD.LG.ELCNTL' type='VS' filename='ELCNTL' disp='i-o' 
ASSGNDD ddname='ERPLAN' dataset='CI.DD.LG.ERPLAN' type='VS' filename='ERPLAN' disp='i-o' 
ASSGNDD ddname='ERFORM' type='DUMMY' 
ASSGNDD ddname='ELERRS' dataset='CI.DD.LG.ELERRS' type='VS' filename='ELERRS' disp='i-o' 
ASSGNDD ddname='ERACCT2' dataset='CI.DD.LG.ERACCT2' type='VS' filename='ERACCT2' disp='i-o' 
ASSGNDD ddname='ELMSTR5' dataset='CI.DD.LG.ELMSTR5' type='VS' filename='ELMSTR5' disp='i-o' 
ASSGNDD ddname='ELCERT' dataset='CI.DD.LG.ELCERT' type='VS' filename='ELCERT' disp='i-o' 
ASSGNDD ddname='ERREIN' dataset='CI.DD.LG.ERREIN' type='VS' filename='ERREIN' disp='i-o' 
ASSGNDD ddname='ERRATE' dataset='CI.DD.LG.ERRATE' type='VS' filename='ERRATE' disp='i-o' 
ASSGNDD ddname='ERPNDB' dataset='CI.DD.LG.ERPNDB' type='VS' filename='ERPNDB' disp='i-o' 
ASSGNDD ddname='ERPNDB1' dataset='CI.DD.LG.ERPNDB2' type='VS' filename='ERPNDB2' disp='i-o' 
ASSGNDD ddname='ERCTBL' dataset='CI.DD.LG.ERCTBL' type='VS' filename='ERCTBL' disp='i-o' 
ASSGNDD ddname='ELREPT' dataset='CI.DD.LG.ELREPT' type='VS' filename='ELREPT' disp='i-o' 
ASSGNDD ddname='ERMAIL' dataset='CI.DD.LG.ERMAIL' type='VS' filename='ERMAIL' disp='i-o' 
ASSGNDD ddname='ERPNDM' dataset='CI.DD.LG.ERPNDM' type='VS' filename='ERPNDM' disp='i-o' 
ASSGNDD ddname='ELCRTT' dataset='CI.DD.LG.ELCRTT' type='VS' filename='ELCRTT' disp='i-o'
ASSGNDD ddname='ERPDEF' type='VS' filename='ERPDEF' disp='i-o'
ASSGNDD ddname='ELSTAT' type='VS' filename='ELSTAT' disp='i-o'
ASSGNDD ddname='ELCRTO' type='VS' filename='ELCRTO' disp='i-o'
ASSGNDD ddname='SYS020' type='DUMMY' 
ASSGNDD ddname='SYS019' dataset='ZI.DD.ER.DATECARD' filename='\${SEQFILES}/ZI.DD.ER.DATECARD' disp='i-o' 
ASSGNDD  ddname='SYS008' type='SYSOUT' class='A'  

EXECPGM pgmname='EL517' stepname='EL517' 

################################################################################
LABEL name=COPY517   
################################################################################
ASSGNDD  ddname='SYSUT1'  filename='\$SYSOUTDIR/$JOBNAME/EL517/SYS008_\${JON}' disp='i-o' normal='k' abend='k'
ASSGNDD  ddname='SYSUT2'  filename='\${SEQFILES}/ZI.DL.CIADM.FICH517'  disp='o' normal='k' abend='d'
ASSGNDD  ddname='SYSIN' type='INSTREAM'  << !
  REPRO IFILE(SYSUT1) OFILE(SYSUT2)
  SET MAXCC = 0
!
EXECPGM  pgmname='IDCAMS' stepname='COPY517'

################################################################################
LABEL name=EL515 
################################################################################
#  ******************************************************************    
#  **--                                                                  
#  **--          LIST OF PENDING BUSINESS                                
#  **--  SEE RESTART INSTRUCTIONS BEFORE RESTARTING                      
#  **--                                                                  
#  ******************************************************************    
#  ****************************************************  INPUT FILES     
ASSGNDD ddname='ERPNDB' dataset='CI.DD.LG.ERPNDB' type='VS' filename='ERPNDB' disp='i-o' 
ASSGNDD ddname='ERPNDC' dataset='CI.DD.LG.ERPNDC' type='VS' filename='ERPNDC' disp='i-o' 
ASSGNDD ddname='ERCRTC' dataset='CI.DD.LG.ERCRTC' type='VS' filename='ERCRTC' disp='i-o' 
ASSGNDD ddname='ERACCT2' dataset='CI.DD.LG.ERACCT2' type='VS' filename='ERACCT2' disp='i-o' 
ASSGNDD ddname='ELCNTL' dataset='CI.DD.LG.ELCNTL' type='VS' filename='ELCNTL' disp='i-o' 
ASSGNDD ddname='ELERRS' dataset='CI.DD.LG.ELERRS' type='VS' filename='ELERRS' disp='i-o' 
ASSGNDD ddname='SYS019' dataset='ZI.DD.ER.DATECARD' filename='\${SEQFILES}/ZI.DD.ER.DATECARD' disp='i-o' 
#  **************************************************** OUTPUT FILES     
ASSGNDD ddname='ELREPT' dataset='CI.DD.LG.ELREPT' type='VS' filename='ELREPT' disp='i-o' 
ASSGNDD  ddname='SYS008' type='SYSOUT' class='A'  
ASSGNDD ddname='SYS020' filename='$SEQFILES/ZI.EX.FICH515.m10' disp='o' normal='k' abend='d' recfmt='F' recsize='133' 
#  ************************************************   SORT WORK FILES    
ASSGNDD ddname='SORTWK01' type='TEMP' 
ASSGNDD ddname='SORTWK02' type='TEMP' 
ASSGNDD ddname='SORTWK03' type='TEMP' 

EXECPGM pgmname='EL515' stepname='EL515' 

################################################################################
LABEL name=CIB003
################################################################################
#  ******************************************************************** 
#  *                                                                  * 
#  *   READ THE PRINT LINES AND CREATE CSR REPORT FILE                * 
#  *                                                                  * 
#  ******************************************************************** 
ASSGNDD ddname='SYS010' filename='$SEQFILES/ZI.EX.FICH515.m10' disp='i-o' normal='k' abend='k' 
ASSGNDD ddname='SYS019' filename='$SEQFILES/ZI.DD.ER.DATECARD' disp='i-o' 
ASSGNDD ddname='SYS011' filename='$SEQFILES/ZI.WW.CSR.REPORT1' disp='o' normal='k' abend='d' recsize='146' recfmt='F' 

EXECPGM pgmname='CIB003' stepname='CIB003' 


################################################################################
LABEL name=SORT01
################################################################################
ASSGNDD ddname='SORTIN' filename='$SEQFILES/ZI.WW.CSR.REPORT1' disp='i-o' normal='k' abend='k' recsize='146' recfmt='F'
ASSGNDD ddname='SORTOUT' filename='$SEQFILES/ZI.WW.CSR.REPORT1.SRTD' disp='o' normal='k' abend='d' recfmt='F' recsize='146'
ASSGNDD ddname='SORTWK01' type='TEMP' 
ASSGNDD ddname='SORTWK02' type='TEMP' 
ASSGNDD ddname='SORTWK03' type='TEMP' 
ASSGNDD ddname='SYSIN' type='INSTREAM'  << !
RECORD:
       KEYS=(1 13)
ENDSORT:
!

EXECPGM pgmname='SORT' stepname='SORT01'

################################################################################
LABEL name=CPY515
################################################################################
ASSGNDD ddname='SYS010' filename='$SEQFILES/ZI.EX.FICH515.m10' disp='i-o'
ASSGNDD ddname='SYS011' filename='$SEQFILES/ZI.DL.CIADM.FICH515.WRAP' disp='o' normal='k' abend='d' recfmt='F' recsize='132' 

EXECPGM pgmname='CIB009L' stepname='CPY515' parm='ALL' 

################################################################################
LABEL name=EL523 
################################################################################
#  ******************************************************************    
#  **--                                                                  
#  **--              NET PREMIUM REPORT                                  
#  **--  SEE RESTART INSTRUCTIONS BEFORE RESTARTING                      
#  **--                                                                  
#  ******************************************************************    
#  ****************************************************  INPUT FILES     
ASSGNDD ddname='ERPNDB' dataset='CI.DD.LG.ERPNDB' type='VS' filename='ERPNDB' disp='i-o' 
ASSGNDD ddname='ERACCT' dataset='CI.DD.LG.ERACCT' type='VS' filename='ERACCT' disp='i-o' 
ASSGNDD ddname='ERACCT1' dataset='CI.DD.LG.ERACCT2' type='VS' filename='ERACCT2' disp='i-o' 
ASSGNDD ddname='SYS019' filename='\${SEQFILES}/ZI.DD.ER.DATECARD' disp='i-o' 
#  **************************************************** OUTPUT FILES     
ASSGNDD ddname='SYS011' filename='\${SEQFILES}/\${JOBNAME}.EL523.ME.BAL.AMTS' disp='o' normal='k' abend='k' recfmt='F'
ASSGNDD ddname='ELREPT' dataset='CI.DD.LG.ELREPT' type='VS' filename='ELREPT' disp='i-o' 
ASSGNDD  ddname='SYS008' type='SYSOUT'
ASSGNDD ddname='SYS017' filename='/tmp/${JOBNAME}_WORK1' disp='o' normal='k' abend='d' recsize='374' recfmt='F' 
ASSGNDD ddname='SYS020' dataset='ZI.EX.FICH523' filename='\${SEQFILES}/ZI.EX.FICH523' disp='o' normal='k' abend='d' recfmt='F' recsize='133' 
#  ************************************************   SORT WORK FILES    
ASSGNDD ddname='SORTWK01' type='TEMP' 
ASSGNDD ddname='SORTWK02' type='TEMP' 
ASSGNDD ddname='SORTWK03' type='TEMP' 

EXECPGM pgmname='EL523' stepname='EL523' 

################################################################################
LABEL name=CPY523
################################################################################
ASSGNDD ddname='SYS010' dataset='ZI.EX.FICH523' filename='\${SEQFILES}/ZI.EX.FICH523' disp='i-o' normal='k'
ASSGNDD ddname='SYS011' filename='\${SEQFILES}/ZI.DL.CIADM.FICH523' disp='o' normal='k' abend='d' recfmt='F' recsize='132' 

EXECPGM pgmname='CIB009L' stepname='CPY523' parm='ALL' 

################################################################################
LABEL name=EL540 
################################################################################
#  ********************************************************************  
#  **--                                                                  
#  **--         DELINQUENT PREMIUM REPORT                                
#  **--  SEE RESTART INSTRUCTIONS BEFORE RESTARTING                      
#  **--                                                                  
#  ******************************************************************    
#  ****************************************************  INPUT FILES     
ASSGNDD ddname='ERACCT2' dataset='CI.DD.LG.ERACCT2' type='VS' filename='ERACCT2' disp='i-o' 
ASSGNDD ddname='ERACCT' dataset='CI.DD.LG.ERACCT' type='VS' filename='ERACCT' disp='i-o' 
ASSGNDD ddname='ERPNDB2' dataset='CI.DD.LG.ERPNDB2' type='VS' filename='ERPNDB2' disp='i-o' 
ASSGNDD ddname='SYS010' dataset='ZI.XX.EXTR521' filename='\${SEQFILES}/ZI.XX.EXTR521' disp='i-o' normal='k' abend='k' 
ASSGNDD ddname='SYS019' dataset='ZI.DD.ER.DATECARD' filename='\${SEQFILES}/ZI.DD.ER.DATECARD' disp='i-o' 
#  **************************************************** OUTPUT FILES     
ASSGNDD ddname='ELREPT' dataset='CI.DD.LG.ELREPT' type='VS' filename='ELREPT' disp='i-o' 
ASSGNDD ddname='SYS008' type='DUMMY' 
ASSGNDD ddname='SYS020' dataset='ZI.EX.FICH540' filename='\${SEQFILES}/ZI.EX.FICH540' disp='o' normal='k' abend='d' recfmt='F' recsize='133' 
#  ************************************************   SORT WORK FILES    
ASSGNDD ddname='SORTWK01' type='TEMP' 
ASSGNDD ddname='SORTWK02' type='TEMP' 
ASSGNDD ddname='SORTWK03' type='TEMP' 

EXECPGM pgmname='EL540' stepname='EL540' 

################################################################################
LABEL name=EL582 
################################################################################
#  ******** -------------------------------------------------- ********\ 
#  **--                                                                  
#  **--         PAYMENT AND ADJUSTMENT SUMMARY                           
#  **--  YOU MAY RESTART AT THE PREVIOUS STEP THAT DELETES               
#  **--  THE FICHE FILE.                                                 
#  **--                                                                  
#  ******** -------------------------------------------------- ********\ 
ASSGNDD ddname='ELCNTL' dataset='CI.DD.LG.ELCNTL' type='VS' filename='ELCNTL' disp='i-o' 
ASSGNDD ddname='SYS019' dataset='ZI.DD.EL582.DATECARD' filename='\${SEQFILES}/ZI.DD.EL582.DATECARD' disp='i-o' 
ASSGNDD  ddname='SYS008' type='SYSOUT' class='A'  
ASSGNDD ddname='ERPYAJ' dataset='CI.DD.LG.ERPYAJ' type='VS' filename='ERPYAJ' disp='i-o' 
ASSGNDD ddname='ELREPT' type='DUMMY' 
ASSGNDD ddname='SYS020' dataset='ZI.DL.FICH582' filename='\${SEQFILES}/ZI.DL.FICH582' disp='o' normal='k' abend='d' recsize='133' recfmt='F' 

EXECPGM pgmname='EL582' stepname='EL582' parm='033018033118' 

################################################################################
LABEL name=CPY582
################################################################################
ASSGNDD ddname='SYS010' filename='\${SEQFILES}/ZI.DL.FICH582' disp='i-o'
ASSGNDD ddname='SYS011' filename='\${SEQFILES}/ZI.DL.CIADM.FICH682' disp='o' normal='k' abend='d' recfmt='F' recsize='132' 

EXECPGM pgmname='CIB009L' stepname='CPY582' parm='ALL' 

################################################################################
LABEL name=ECS096 
################################################################################
ASSGNDD ddname='SYS011' dataset='ZI.XX.RFAC' filename='\${SEQFILES}/ZI.XX.RFAC' disp='o' normal='k' abend='d' recfmt='F' recsize='1240' 
ASSGNDD ddname='ELCNTL' dataset='CI.DD.LG.ELCNTL' type='VS' filename='ELCNTL' disp='i-o' 
ASSGNDD ddname='SYS019' dataset='ZI.DD.ER.DATECARD' filename='\${SEQFILES}/ZI.DD.ER.DATECARD' disp='i-o' 
ASSGNDD  ddname='SYS008' type='SYSOUT' class='A'  
ASSGNDD ddname='SYS020' type='DUMMY' 
ASSGNDD ddname='SORTWK01' type='TEMP' 

EXECPGM pgmname='ECS096' stepname='ECS096' 

################################################################################
LABEL name=CPY096
################################################################################
ASSGNDD ddname='SYS010' filename='$SYSOUTDIR/$JOBNAME/ECS096/SYS008_\${JON}' disp='i-o'
ASSGNDD ddname='SYS011' filename='\${SEQFILES}/ZI.DL.CIADM.FICH096' disp='o' normal='k' abend='d' recfmt='F' recsize='132' 

EXECPGM pgmname='CIB009L' stepname='CPY096' parm='ALL'


################################################################################

ENDJOB 
################################################################################