       IDENTIFICATION DIVISION.                                         
NTTDel*PROGRAM-ID.          VPP017T.
NTTIns PROGRAM-ID.          VPP017.
      *AUTHOR.        Cowtown.
      *               Omaha, NE.
      *DATE-COMPILED.                                                   
      *SECURITY.   *****************************************************
      *            *   THIS PROGRAM IS THE PROPERTY OF CSO             *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF CSO         IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
      *            *****************************************************
      *REMARKS.                                                         
      *        CREATE STANDARD COMMISSION TRANSACTIONS                  
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 011116  CR2015082400003  PEMA  NEW PROGRAM
      ******************************************************************

       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
      
           SELECT END-PRINT        ASSIGN TO SYS008.
           SELECT EXTR-FILE        ASSIGN TO SYS014.
           SELECT CERT-IN          ASSIGN TO SYS010.
           SELECT EP-EXTR          ASSIGN TO SYS018.
           SELECT ERACCTT          ASSIGN TO ERACCTT
                                   ORGANIZATION IS INDEXED              
                                   ACCESS IS SEQUENTIAL                 
                                   RECORD KEY IS AM-CONTROL-PRIMARY     
                                   FILE STATUS IS ERACCTT-FILE-STATUS.  

           SELECT DISK-DATE        ASSIGN TO SYS019.
           SELECT FICH             ASSIGN TO SYS020.
      
       DATA DIVISION.                                                   
       FILE SECTION.                                                    

       FD  END-PRINT                                                    
                                       COPY ELCPRTFD.                       

       FD  EXTR-FILE                                                    
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.                                            
       01  EXTR-RECORD                 PIC X(116).
                                                                        
       FD  ERACCTT.

                                   COPY ERCACCT.

       FD  EP-EXTR                                                      
                                   COPY ECSEXTFD.                       
                                                                        
                                   COPY ECSEXT01.                       

       FD  CERT-IN.                                                      
                                   COPY ECSCRT01.

       FD  DISK-DATE                                                    
                                   COPY ELCDTEFD.                       
                                                                        
       FD  FICH                                                         
                                   COPY ELCFCHFD.                       
       WORKING-STORAGE SECTION.                                         

       EXEC SQL
          INCLUDE SQLCA
       END-EXEC

       77  FILLER PIC X(32) VALUE '********************************'.   
       77  FILLER PIC X(32) VALUE '*   VPP017  WORKING STORAGE    *'.
       77  FILLER PIC X(32) VALUE '********** VMOD=2.030 **********'.   
                                                                        
       77  ACCT-SW                 PIC S9       COMP-3 VALUE +0.        
       77  NDX                     PIC S999     COMP-3 VALUE +0.        
       77  WS-SAVE-NDX             PIC S999     COMP-3 VALUE +0.
       77  RV-NDX                  PIC S999     COMP-3 VALUE +0.        
       77  PGM-SUB                 PIC S999     COMP-3 VALUE +017.      
       77  PRCM-LF-OW-COMM         PIC S9(7)V99 COMP-3 VALUE +0.        
       77  PRCM-LF-OW-SF           PIC S9(7)V99 COMP-3 VALUE +0.        
       77  PRCM-AH-OW-COMM         PIC S9(7)V99 COMP-3 VALUE +0.        
       77  PRCM-AH-OW-SF           PIC S9(7)V99 COMP-3 VALUE +0.        
       77  PRCM-DLR-INC            PIC S9(7)V99 COMP-3 VALUE +0.
       77  PRCM-LF-LMBA-FEE        PIC S9(7)V99 COMP-3 VALUE +0.
       77  PRCM-AH-LMBA-FEE        PIC S9(7)V99 COMP-3 VALUE +0.
       77  PRCM-BANK-FEE           PIC S9(7)V99 COMP-3 VALUE +0.
       77  PRCM-CSO-ADMIN          PIC S9(7)V99 COMP-3 VALUE +0.
       77  RECALC-LF-OW-COMM       PIC S9(7)V99 COMP-3 VALUE +0.        
       77  RECALC-LF-OW-SF         PIC S9(7)V99 COMP-3 VALUE +0.        
       77  RECALC-AH-OW-COMM       PIC S9(7)V99 COMP-3 VALUE +0.        
       77  RECALC-AH-OW-SF         PIC S9(7)V99 COMP-3 VALUE +0.        
       77  CNC-FACT                PIC S9(3)V9(7) COMP-3 VALUE +0.
       77  WS-WORK-REF             PIC S9(9)V99 COMP-3 VALUE +0.
       77  WS-WORK-fee             PIC S9(7)V99 COMP-3 VALUE +0.
       77  X                       PIC X               VALUE SPACE.     
       77  a1                      PIC S999 COMP-3 VALUE +0.
       77  c1                      PIC S999 COMP-3 VALUE +0.
       77  S1                      PIC S999 COMP-3 VALUE +0.
      
       77  WS-DUP-AGT-SW           PIC X  VALUE ' '.
           88  DUP-AGENT                VALUE 'Y'.
       77  WS-DONE-SW              PIC X  VALUE ' '.
           88  ALREADY-DONE             VALUE 'Y'.
       77  B-SUB                   PIC S9(4)   COMP.
       77  D1                      PIC S9(5) COMP-3 VALUE +0.
       77  P1                      PIC S999 COMP-3 VALUE +0.
       77  P2                      PIC S999 COMP-3 VALUE +0.
       77  CNC-WK                  PIC S9(7)V99 COMP-3 VALUE +0.
       77  WS-FACT-CERT            PIC X(11)   VALUE SPACES.
       77  WS-MONTH                PIC S999     COMP-3 VALUE +0.
       77  WS-HI-UEF               PIC S9V999   COMP-3 VALUE +0.
       77  WS-LO-UEF               PIC S9V999   COMP-3 VALUE +0.
       77  WS-CLP-MO3              PIC S9(7)V99 COMP-3 VALUE +0.
       77  WS-COMM-MO3             PIC S9(7)V99 COMP-3 VALUE +0.
       77  DD-IU-SW                    PIC X   VALUE ' '.
           88  DD-IU-PRESENT                 VALUE 'Y'.
       77  W-FACTOR            PIC S9(09)V9(08) COMP-3 VALUE +0.
       77  WS-DDF-TERM             PIC S999     COMP-3 VALUE +0.
       77  WS-WORK-REF                 PIC S9(9)V99 COMP-3 VALUE +0.
       77  ws-eof-sw                   pic x value ' '.
           88  end-of-input               value 'Y'.
       77  ws-cert-sw                  pic x value ' '.
           88  end-of-cert                value 'Y'.
       77  ws-recs-in                  pic 9(7) value zeros.
       77  rec-cnt                     pic 9(9) value zeros.
       77  ws-sql-code                 pic s9(7) value zeros.
       77  ws-dis-sql-code             pic -9999999 value zeros.
       77  cert-in-recs                pic 9(9) value zeros.
       77  ws-bin-eff-date             pic xx value low-values.
       77  ws-bin-cancel-date          pic xx value low-values.
       77  ws-elapsed-months           pic s999 comp-3 value +0.

       01  P pointer.
       01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
       01  var-ptr pointer.
       01  env-var-len                 pic 9(4)  binary.
       01  rc                          pic 9(9)  binary.
      
       01  WS-KIXSYS.
           05  WS-KIX-FIL1             PIC X(10).
           05  WS-KIX-APPS             PIC X(10).
           05  WS-KIX-ENV              PIC X(10).
           05  WS-KIX-MYENV            PIC X(10).
           05  WS-KIX-SYS              PIC X(10).

       01  WS-lf-cnc-fact            COMP-3 PIC S9(03)V9(07) VALUE +0.
       01  WS-ah-cnc-fact            COMP-3 PIC S9(03)V9(07) VALUE +0.
       01  WS-DCC-PRODUCT-CODE         PIC XXX  VALUE SPACES.
       
                                                                        
       01  ws-prev-lf-ben              pic xx value spaces.
       01  ws-prev-ah-ben              pic xx value spaces.
       01  ws-prev-clp-state           pic xx value spaces.
       01  ws-prev-elapsed-months      pic 999 value zeros.
       01  ws-prev-cancel-dte          pic 9(11) comp-3 value zeros.
       01  ws-prev-cntrl1.
           05  ws-prev-carrier         pic x     value spaces.
           05  ws-prev-group           pic x(6)  value spaces.
           05  ws-prev-state           pic xx    value spaces.
           05  ws-prev-account         pic x(10) value spaces.
       01  ws-prev-cntrl2.
           05  ws-prev-eff-dt          pic 9(11) comp-3 value zeros.
           05  ws-prev-cert-no         pic x(11) value spaces.
       01  ws-work-date-n              pic 9(8) value zeros.
       01  ws-work-date redefines ws-work-date-n.
           05  ws-wd-ccyy              pic x(4).
           05  ws-wd-mm                pic xx.
           05  ws-wd-dd                pic xx.
       01  BINARY-WORK-AREA    COMP    SYNC.                            
           12  X1                  PIC S999            VALUE +0.        
           12  X4                  PIC S999            VALUE +0.        
           12  B1                  PIC S999            VALUE +1.        
           12  B2                  PIC S999            VALUE +2.        
           12  FAL                 PIC S999            VALUE +0.        
           12  AGTNDX              PIC S999            VALUE +0.        
                                                                        
       01  WS.                                                          
           12  WS-RETURN-CODE         PIC S9(4) COMP.                   
           12  WS-ABEND-MESSAGE       PIC X(80).                        
           12  WS-ABEND-FILE-STATUS   PIC XX  VALUE ZEROS.              
           12  WS-ZERO                PIC S9  VALUE ZERO COMP-3.        
           12  WS-DTE-PGM-OPT         PIC 9   VALUE 0.                  
           12  ERACCTT-FILE-STATUS    PIC XX  VALUE ZEROS.              
           12  B-CNT                  PIC S999 VALUE +0 COMP-3.
                                                                        
       01  WS-RUN-DATE-R.                                               
           12  WS-RUN-YR              PIC 99.                           
           12  WS-RUN-MO              PIC 99.                           
           12  WS-RUN-DA              PIC 99.                           
                                                                        
       01  ERROR-MESSAGES.                                              
           12  ER-0504             PIC X(4)            VALUE '0504'.    
           12  ER-0514             PIC X(4)            VALUE '0514'.    
                                                                        
       01  WS-ERCOMP-KEY.
           12  WS-ERCOMP-COMP-CD   PIC X.
           12  WS-ERCOMP-CARRIER   PIC X.
           12  WS-ERCOMP-GROUPING  PIC X(6).
           12  WS-ERCOMP-FIN-RESP  PIC X(10).
           12  WS-ERCOMP-ACCOUNT   PIC X(10).
           12  WS-ERCOMP-TYPE      PIC X.
           
       01  MISC-WORK-AREA.                                              
           12  STRIP-SIGN          PIC 9V9(5).                          
           12  OTHER-WORK.                                              
               16  WK-CTL.                                              
                   20  W-CARRIER   PIC X.                               
                   20  W-GROUPING  PIC X(6).                            
                   20  W-ST        PIC XX.                              
                   20  W-ACCT      PIC X(10).                           
                   20  W-DATE      PIC 9(11)    COMP-3.                 
               16  SV-ST           PIC XX.                              
                                                                        
           12  MONTHS-DIFF-LF      PIC S9(5).                           
           12  MONTHS-DIFF-AH      PIC S9(5).                           
                                                                        
           12  SAVE-NCL-REGION     PIC X(6).                            
           12  SAVE-NCL-POOL-CODE  PIC XXX.                             

       01  COMP-3-WORK-AREA    COMP-3.                                  
           12  KX                  PIC S9.                              
           12  L-OW                PIC S9(9)V99.                        
           12  L-OW-ALT            PIC S9(9)V99.                        
           12  A-OW                PIC S9(7)V99.                        
           12  TEST-ZERO.                                               
               16  TEST-IS-ZERO-1.                                      
                   20  FILLER      PIC S9(9)V99    VALUE ZERO.          
                   20  FILLER      PIC S9(7)V99    VALUE ZERO.          
                   20  FILLER      PIC S9(7)V99    VALUE ZERO.          
                   20  FILLER      PIC S9(9)V99    VALUE ZERO.          
                   20  FILLER      PIC S9(7)V99    VALUE ZERO.          
                   20  FILLER      PIC S9(7)V99    VALUE ZERO.
                   20  FILLER      PIC S9          VALUE ZERO.
               16  TEST-IS-ZERO-2.                                      
                   20  FILLER      PIC S9(7)V99    VALUE ZERO.          
                   20  FILLER      PIC S9(7)V99    VALUE ZERO.          
                   20  FILLER      PIC S9(7)V99    VALUE ZERO.          


      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-**
      ***                                                             **
      ***                                                             **
      ***                                                             **
      ***                                                             **
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-**

       01  w-work-area.
           05  ws-cso-admin-fee        pic s9(9)v99 comp-3 value +0.
           05  ws-mktg-fee             pic s9(9)v99 comp-3 value +0.
           05  ws-ga-fee               pic s9(9)v99 comp-3 value +0.
           05  ws-acct-fee             pic s9(9)v99 comp-3 value +0.
           05  ws-lf-ben-code          pic xx value '00'.
           05  ws-ah-ben-code          pic xx value '00'.
           05  ws-msrp                 pic s9(9)v99 comp-3 value +0.
           05  ws-iss-retail           pic s9(9)v99 comp-3 value +0.
           05  ws-ref-retail           pic s9(9)v99 comp-3 value +0.
           05  ws-iss-clp              pic s9(9)v99 comp-3 value +0.
           05  ws-ref-clp              pic s9(9)v99 comp-3 value +0.
           05  ws-iss-ow               pic s9(9)v99 comp-3 value +0.
           05  ws-ref-ow               pic s9(9)v99 comp-3 value +0.
           05  ws-iss-pm-fee           pic s9(9)v99 comp-3 value +0.
           05  ws-ref-pm-fee           pic s9(9)v99 comp-3 value +0.
           05  ws-iss-inc              pic s9(9)v99 comp-3 value +0.
           05  ws-ref-inc              pic s9(9)v99 comp-3 value +0.
           05  ws-iss-cso-admin        pic s9(9)v99 comp-3 value +0.
           05  ws-ref-cso-admin        pic s9(9)v99 comp-3 value +0.
           05  ws-lf-claims            pic s9(9)v99 comp-3 value +0.
           05  ws-ah-claims            pic s9(9)v99 comp-3 value +0.
           05  ws-cancel-fee           pic s9(9)v99 comp-3 value +0.
           05  ws-net-cancel-fee       pic s9(9)v99 comp-3 value +0.
           05  ws-iss-wholesale-fee    pic s9(9)v99 comp-3 value +0.
           05  ws-ref-wholesale-fee    pic s9(9)v99 comp-3 value +0.
           05  ws-customer-refund      pic s9(9)v99 comp-3 value +0.
           05  ws-iss-acct-fee         pic s9(9)v99   comp-3 value +0.
           05  ws-ref-acct-fee         pic s9(9)v9(5) comp-3 value +0.
           05  ws-iss-mktg-fee         pic s9(9)v99   comp-3 value +0.
           05  ws-ref-mktg-fee         pic s9(9)v9(5) comp-3 value +0.
           05  ws-total-fees           pic s9(9)v9(5) comp-3 value +0.
           05  ws-clp-prem             pic s9(9)v9(5) comp-3 value +0.
           05  ws-net-gain-acct-fee    pic s9(9)v9(5) comp-3 value +0.
           05  ws-refund-short-due-ncb pic s9(9)v9(5) comp-3 value +0.


       EXEC SQL
          BEGIN DECLARE SECTION
       END-EXEC

       01  svr                         pic x(32).
       01  usr                         pic x(32).
       01  pass                        pic x(32).
       01  usr-pass                    pic x(64).
       01  ws-disp-code                pic s9(11).
       01  ws-begin-dt                 pic x(10).
       01  ws-end-dt                   pic x(10).
       01  ws-compid                   pic xxx.

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  These indicators are used to determine if a variable      ***
      ***  is passed nulls from sql. The indicator will be -1        ***
      ***  if the value on sql is nulls and +0 if the value is       ***
      ***  something other than nulls. Here is an example on how     ***
      ***  to use the indicator variables.                           ***
      ***                                                            ***
      ***     EXEC SQL                                               ***
      ***        fetch checkapp into                                 ***
      ***           :db-app-status :nu-app-status,                   ***
      ***           :db-app-by     :nu-app-by,                       ***
      ***           :db-app-date   :nu-app-date,                     ***
      ***           :db-app-batch  :nu-app-batch                     ***
      ***     END-EXEC                                               ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

       01  indicator-vaiables-for-nulls.
           05  nu-cancel-dte           pic s9(4) comp value +0.
           05  nu-checkdate            pic s9(4) comp value +0.
           05  nu-checkstatus          pic s9(4) comp value +0.
           05  nu-fincar               pic s9(4) comp value +0.
           05  nu-fingrp               pic s9(4) comp value +0.

       01  ws-rec-cntr                 pic s9(4) comp value +0.
       01  ws-test-date                pic x(10) value spaces.

       01  ws-extract.
           05  db-month-end-dt         pic x(10).
           05  db-carrier              pic x.
           05  db-grouping             pic x(6).
           05  db-acct-state           pic xx.
           05  db-clp-state            pic xx.
           05  db-account              pic x(10).
           05  db-eff-dte              pic x(10).
           05  db-cert-no              pic x(11).
           05  db-ben-type             pic x.
           05  db-lf-ben-code          pic xx.
           05  db-ah-ben-code          pic xx.
           05  db-term                 pic 999.
           05  db-cancel-dte           pic x(10).
           05  db-elapsed-months       pic 999.
           05  db-msrp                 pic -9(9).99.
           05  db-msrp-a redefines
               db-msrp                 pic x(13).
           05  db-retail               pic -9(9).99.
           05  db-retail-a redefines
               db-retail               pic x(13).
           05  db-wholesale            pic -9(9).99.
           05  db-wholesale-a redefines
               db-wholesale            pic x(13).
           05  db-customer-ref         pic -9(9).99.
           05  db-customer-ref-a redefines
               db-customer-ref         pic x(13).
           05  db-cancel-fee           pic -9(9).99.
           05  db-cancel-fee-a redefines
               db-cancel-fee           pic x(13).
           05  db-acct-comp            pic -9(9).99.
           05  db-acct-comp-a redefines
               db-acct-comp            pic x(13).
           05  db-clp                  pic -9(9).99.
           05  db-clp-a             redefines
               db-clp                  pic x(13).
           05  db-pm-fee               pic -9(9).99.
           05  db-pm-fee-a          redefines
               db-pm-fee               pic x(13).
           05  db-inc                  pic -9(9).99.
           05  db-inc-a             redefines
               db-inc                  pic x(13).
           05  db-cso-admin            pic -9(9).99.
           05  db-cso-admin-a        redefines
               db-cso-admin            pic x(13).
           05  db-cancel-fee-income    pic -9(9).99.
           05  db-cancel-fee-income-a redefines
               db-cancel-fee-income    pic x(13).
           05  db-cancel-fee-expense   pic -9(9).99.
           05  db-cancel-fee-expense-a redefines
               db-cancel-fee-expense   pic x(13).
           05  db-net-lf-claims        pic -9(9).99.
           05  db-net-lf-claims-a   redefines
               db-net-lf-claims        pic x(13).
           05  db-net-ah-claims        pic -9(9).99.
           05  db-net-ah-claims-a   redefines
               db-net-ah-claims        pic x(13).

       EXEC SQL
          END DECLARE SECTION
       END-EXEC

       01  P-LINE.                                                      
           12  FILLER      PIC X(24)   VALUE 'COMMISSION FILE HAS BEEN'.
           12  FILLER      PIC X(13)   VALUE ' CREATED FOR '.           
           12  P-RUN-DATE  PIC X(87)   VALUE SPACES.                    
           12  FILLER      PIC X(8)    VALUE 'ECS-017 '.                
                                                                        
       01  HDNG-1.                                                      
           12  FILLER      PIC X(52)   VALUE SPACES.                    
           12  FILLER      PIC X(20)   VALUE 'COMPENSATION EXTRACT'.    
           12  FILLER      PIC X(52)   VALUE SPACES.                    
           12  FILLER      PIC X(8)    VALUE 'ECS-017'.                 
                                                                        
       01  HDNG-2.                                                      
           12  FILLER      PIC X(47)   VALUE SPACES.                    
           12  HD-CLIENT   PIC X(30).                                   
           12  FILLER      PIC X(47)   VALUE SPACES.                    
           12  HD-RUN      PIC X(8).                                    
                                                                        
       01  HDNG-3.                                                      
           12  FILLER      PIC X(52)   VALUE SPACES.                    
           12  HD-DATE     PIC X(18).                                   
           12  FILLER      PIC X(42)   VALUE SPACES.                    
           12  FILLER      PIC X(11)   VALUE 'PAGE      1'.             
                                                                        
       01  COMP-MESS.                                                   
           12  FILLER      PIC X(40)   VALUE SPACES.                    
           12  FILLER      PIC X(17)   VALUE '*** COMPENSATION '.       
           12  FILLER      PIC X(75)   VALUE 'EXTRACTS GENERATED ***'.  
      
       01  max-pdef                    pic s9(5) comp-3 value +5000.
       01  DCC-DDF-WORK-AREA.
           05  F OCCURS 5000.
               10  DD-STATE                 PIC XX.
               10  DD-PRODUCT-CD            PIC XXX.
               10  DD-FILLER                PIC X(7).
               10  DD-BEN-TYPE              PIC X.
               10  DD-BEN-CODE              PIC XX.
               10  DD-PROD-EXP-DT           PIC XX.
               10  DD-1ST-YR-ALLOW          PIC S999V99 COMP-3.
               10  DD-PROD-DATA OCCURS 8.
                   15  DD-PROD-CODE         PIC X.
               10  DD-EARN-FACTORS.                                     
                   15  F OCCURS 15.
                       20  F OCCURS 15.
                           25  DD-UEP-FACTOR    PIC S9V9(3)     COMP-3.
               10  DD-TRUNCATED             PIC X.

       01  ws-extract-rec-out.
           05  ex-carrier              pic x.
           05  ex-grouping             pic x(6).
           05  ex-clp-state            pic xx.
           05  ex-account              pic x(10).
           05  ex-eff-dte              pic 9(8).
           05  ex-cert-no              pic x(11).
           05  ex-ben-type             pic x.
               88  ben-type-lf           value '1'.
               88  ben-type-ah           value '2'.
           05  ex-ben-code             pic xx.
           05  ex-cancel-dte           pic x(10).
           05  ex-elapsed-months       pic 999.
           05  ex-net-clp              pic s9(9)v99 comp-3 value +0.
           05  ex-net-ow               pic s9(9)v99 comp-3 value +0.
           05  ex-net-pgm-mgt-fee      pic s9(9)v99 comp-3 value +0.
           05  ex-net-incentive        pic s9(9)v99 comp-3 value +0.
           05  ex-net-cont-fee         pic s9(9)v99 comp-3 value +0.
           05  ex-net-cso-admin-fee    pic s9(9)v99 comp-3 value +0.
           05  ex-net-ga-sales         pic s9(9)v99 comp-3 value +0.
           05  ex-claim-pmts           pic s9(9)v99 comp-3 value +0.
           05  ex-cancel-fee-income    pic s9(9)v99 comp-3 value +0.
           05  ex-cancel-fee-expense   pic s9(9)v99 comp-3 value +0.
           05  ex-acct-state           pic xx.

                                                                        
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.
                                       COPY ELCDATE.
      *                                COPY ELCACCTV.
       01  sqlconnect-parms.
           05  p-sql-server            PIC X(30).
           05  p-sql-database          PIC X(30).
           05  p-connect-return-code   pic s9(5) comp-5.
           05  p-sql-return-message    pic x(256).

       LINKAGE SECTION.                                                 
       01  parm.
           05  parm-length             pic s9(4) comp.
           05  parm-current-month-end  pic 9(8).
           05  parm-program-option     pic x.

       01  var                         pic x(30).

       PROCEDURE DIVISION using parm.
                                       COPY ELCDTERX.
                                                                        
           display ' Begin Program VPP017 '

           set P to address of KIXSYS
           CALL "getenv" using by value P returning var-ptr
           if var-ptr = null then
              display ' kixsys not set '
           else
              set address of var to var-ptr
              move 0                   to env-var-len
              inspect var tallying env-var-len
                for characters before X'00' 
              unstring var (1:env-var-len) delimited by '/'
                 into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
                    WS-KIX-SYS
              end-unstring
           end-if

           display ' KIXSYS  ' ws-kix-myenv

           perform 3000-connect-to-logic
                                       thru 3000-exit
      *    perform 3020-drop-table     thru 3020-exit
      *    perform 3010-create-table   thru 3010-exit
      *    perform 3030-truncate-table thru 3030-exit
           perform 0000-open-files     thru 0000-exit
           perform 0010-init           thru 0010-exit
           perform 0020-read-extract   thru 0020-exit
           perform 0050-process-input  thru 0050-exit
              until end-of-input
           perform 0300-cert-break     thru 0300-exit
           perform 0400-acct-break     thru 0400-exit
           perform 3040-finish-up      thru 3040-exit

           display ' Records inserted ' rec-cnt

           goback
           .
       0000-OPEN-FILES.

           OPEN
              INPUT
                 EP-EXTR
                 ERACCTT
                 CERT-IN
              OUTPUT
                 EXTR-file
                 END-PRINT

           IF ERACCTT-FILE-STATUS  = '00' OR '97'
              CONTINUE
           ELSE
              MOVE ERACCTT-FILE-STATUS TO WS-ABEND-FILE-STATUS          
              MOVE ' ERACCTT OPEN ERROR- '                             
                                       TO WS-ABEND-MESSAGE              
              GO TO ABEND-PGM
           END-IF

           .
       0000-EXIT.
           EXIT.

       0010-init.

           string
              parm-current-month-end (5:4)
              parm-current-month-end (1:2)
              parm-current-month-end (3:2)
              delimited by size into dc-greg-date-cymd-r
           end-string           

           move 'L'                    to dc-option-code
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           if no-conversion-error
              move dc-greg-date-a-edit to db-month-end-dt
              display ' init month end date ' db-month-end-dt
              string
                 dc-greg-date-a-edit (7:4) '.'
                 dc-greg-date-a-edit (1:2) '.'
                 dc-greg-date-a-edit (4:2)
                    delimited by size into ws-test-date
              end-string
              display ' test date ' ws-test-date
           else
              display ' invalid parm date - aborting '
                 parm-current-month-end
              perform abend-pgm
           end-if

           MOVE RUN-YR                 TO WS-RUN-YR
           MOVE RUN-MO                 TO WS-RUN-MO
           MOVE RUN-DA                 TO WS-RUN-DA
           MOVE ALPH-DATE              TO P-RUN-DATE.                   

           MOVE '1'                    TO X.                            
           MOVE HDNG-1                 TO P-DATA.                       
           PERFORM 8000-PRT-RTN THRU 8099-PRT-XIT.                      
                                                                        
           MOVE ' '                    TO X.                            
           MOVE COMPANY-NAME           TO HD-CLIENT.                    
           MOVE WS-CURRENT-DATE        TO HD-RUN.                       
           MOVE HDNG-2                 TO P-DATA.                       
           PERFORM 8000-PRT-RTN THRU 8099-PRT-XIT.                      
                                                                        
           MOVE ' '                    TO X.                            
           MOVE ALPH-DATE              TO HD-DATE.                      
           MOVE HDNG-3                 TO P-DATA.                       
           PERFORM 8000-PRT-RTN THRU 8099-PRT-XIT.                      
                                                                        
           move low-values             to am-control-primary
           start eracctt key >= am-control-primary
           if eracctt-file-status = '00'
              read eracctt next record
           else
              MOVE ERACCTT-FILE-STATUS TO WS-ABEND-FILE-STATUS          
              MOVE ' ERACCTT start ERROR- '                             
                                       TO WS-ABEND-MESSAGE              
              GO TO ABEND-PGM
           END-IF

           perform 3050-check-for-rerun thru 3050-exit
           perform 0220-read-cert      thru 0220-exit

           .
       0010-EXIT.
           EXIT.
                                                                        
       0020-READ-EXTRACT.                                               

           READ EP-EXTR AT END
              set end-of-input to true
           end-read

           if not end-of-input
              add 1 to ws-recs-in
           end-if

           .
       0020-exit.
           exit.

       0030-init-work-area.

           move zeros                  to ws-cso-admin-fee        
                                          ws-mktg-fee             
                                          ws-ga-fee               
                                          ws-acct-fee             
                                          ws-lf-ben-code          
                                          ws-ah-ben-code          
                                          ws-msrp                 
                                          ws-iss-retail           
                                          ws-ref-retail           
                                          ws-iss-clp              
                                          ws-ref-clp              
                                          ws-iss-ow               
                                          ws-ref-ow               
                                          ws-iss-pm-fee           
                                          ws-ref-pm-fee           
                                          ws-iss-inc              
                                          ws-ref-inc              
                                          ws-iss-cso-admin        
                                          ws-ref-cso-admin        
                                          ws-lf-claims            
                                          ws-ah-claims            
                                          ws-cancel-fee           
                                          ws-net-cancel-fee       
                                          ws-iss-wholesale-fee    
                                          ws-ref-wholesale-fee    
                                          ws-customer-refund      
                                          ws-iss-acct-fee         
                                          ws-ref-acct-fee         
                                          ws-iss-mktg-fee         
                                          ws-ref-mktg-fee         
                                          ws-total-fees           
                                          ws-clp-prem             
                                          ws-net-gain-acct-fee    
                                          ws-refund-short-due-ncb 

           .
       0030-exit.
           exit.

       0050-process-input.

           if (de-rein = ' ')
              and (de-trans = 'I' or 'C' or 'X')
              continue
           else
              go to 0050-continue
           end-if
                                                                        
           IF (DE-ENTRY-STATUS = 'D' OR 'V' or '9')
                       or
              ((de-trans = 'I')
              and (de-entry-status = '5' or 'M'))
                       or
              ((de-trans = 'C')
              and (de-entry-status = 'M'))
              GO TO 0050-continue
           end-if

          if de-cntrl1 not = ws-prev-cntrl1
             perform 0300-cert-break   thru 0300-exit
             perform 0400-acct-break   thru 0400-exit
             move de-cntrl1            to ws-prev-cntrl1
             move de-cntrl2            to ws-prev-cntrl2
          else
             if de-cntrl2 not = ws-prev-cntrl2
                perform 0300-cert-break thru 0300-exit
                move de-cntrl2         to ws-prev-cntrl2
             end-if
          end-if

          perform 0060-sync-to-acct    thru 0060-exit

          move zeros                   to ws-prev-cancel-dte
                                          ws-prev-elapsed-months
           
          if de-clp-state = spaces
             move de-state             to de-clp-state
          end-if

          move de-clp-state            to ws-prev-clp-state
          move de-state                to ws-prev-state
          move de-lf-type              to ws-prev-lf-ben
          move de-ah-type              to ws-prev-ah-ben

          if de-trans = 'X'
             perform 0150-claim-pmt    thru 0150-exit
          else
             if de-trans = 'C'
                perform 0200-cancel    thru 0200-exit
             else
               perform 0100-issue     thru 0100-exit
             end-if
          end-if

           .
       0050-continue.

           perform 0020-read-extract   thru 0020-exit

           .
       0050-exit.
           exit.
                                                                        
       0060-sync-to-acct.

           if am-control-a < de-cntrl1
              perform 0070-read-acct   thru 0070-exit
              go to 0060-sync-to-acct
           else
              if am-control-a > de-cntrl1
                 display ' no acct for detail ' am-control-a ' '
                    de-cntrl1
                 perform abend-pgm
              end-if
           end-if

           if am-expire-dt <= de-eff
              perform 0070-read-acct   thru 0070-exit
              go to 0060-sync-to-acct
           end-if

           if (de-eff < am-expire-dt)
              and (de-eff >= am-effect-dt)
              continue
           else
              display ' no acct for cert ' de-cntrl1 ' '
                 de-cert-no ' ' de-eff
              perform abend-pgm
           end-if

           .
       0060-exit.
           exit.

       0070-read-acct.

           READ ERACCTT next record

           IF ERACCTT-FILE-STATUS = '10'                                
              MOVE HIGH-VALUES         TO ACCOUNT-MASTER                
           ELSE                                                        
              IF ERACCTT-FILE-STATUS NOT = ZERO                        
                 MOVE ERACCTT-FILE-STATUS                             
                                       TO WS-ABEND-FILE-STATUS              
                 MOVE ' ERACCTT READ ERROR- '                         
                                       TO WS-ABEND-MESSAGE                  
                 GO TO ABEND-PGM
              end-if
           end-if

           .
       0070-EXIT.                                                       
           EXIT.                                                        

       0100-issue.

      **** lf clp, fees and o/w

           move de-lf-ben              to ws-msrp
           move de-lf-type             to ws-lf-ben-code
           move de-ah-type             to ws-ah-ben-code

      **** ah clp, fees and o/w

           if de-ah-type not = '00'
              if de-rei-ahprm not = zeros
                 move de-rei-ahprm     to ws-iss-clp
              else
                 move de-lf-prm-alt    to ws-iss-clp
              end-if
              perform varying a1 from +1 by +1 until a1 > +10
                 evaluate true
                    when de-agt-type (a1) = 'C' OR 'D'
                       if ws-iss-clp = zeros
                          compute ws-iss-clp rounded =
                             de-ah-prm * (1 - de-a-pc(a1))
                       end-if
                    when de-agt-type (a1) = 'L'
                       compute ws-iss-pm-fee rounded =
                          ws-iss-pm-fee + (de-a-pc(a1) * +1000)
                    when de-agt-type(a1) = 'I' or 'J'
                       compute ws-iss-inc rounded =
                          ws-iss-inc + (de-a-pc(a1) * +1000)
                    when de-agt-type(a1) = 'N'
                       compute ws-iss-cso-admin rounded =
                          ws-iss-cso-admin + (de-a-pc(a1) * +1000)
                    when de-agt-type (a1) = 'S'
                       compute ws-iss-cso-admin rounded =
                          ws-iss-cso-admin +
                             (de-ah-prm * de-a-pc(a1))
                 end-evaluate
              end-perform
           end-if

           perform 0310-build-issue    thru 0310-exit

           .
       0100-exit.
           exit.

       0150-claim-pmt.

           if de-death
              compute ws-lf-claims = ws-lf-claims + de-claim-amt
           else
              if de-disability
                 compute ws-ah-claims = ws-ah-claims + de-claim-amt
              end-if
           end-if

           perform 0332-build-claim    thru 0332-exit

           .
       0150-exit.
           exit.

       0200-cancel.

      **** lf clp, fees and o/w

           perform 0210-get-cert-rec   thru 0210-exit

           move de-lf-ben              to ws-msrp
           if de-lf-prm not = zero
              compute ws-lf-cnc-fact = de-lf-rfnd / de-lf-prm
           end-if

           if de-ah-prm not = zeros
              compute ws-ah-cnc-fact rounded =
                 de-ah-rfnd / de-ah-prm
           end-if

           move cr-cancel-fee          to ws-cancel-fee
           if de-ah-rfnd = de-ah-prm
              move zeros               to ws-cancel-fee
           end-if
           if de-ah-rfnd < ws-cancel-fee
              move de-ah-rfnd          to ws-cancel-fee
           end-if

           move de-ah-rfnd             to ws-ref-retail
           compute ws-customer-refund = ws-ref-retail - ws-cancel-fee

           if ws-customer-refund > 0
              compute ws-ref-acct-fee rounded =
                 ws-acct-fee * ws-ah-cnc-fact
           else
              move +0                  to ws-ref-acct-fee
           end-if

           if ws-customer-refund > 0
              compute ws-ref-wholesale-fee =
                 ws-ref-retail - ws-ref-acct-fee
           else
              move +0                  to ws-ref-wholesale-fee
           end-if

           if ws-cancel-fee < ws-ref-wholesale-fee
      *       move ws-cancel-fee       to ws-net-cancel-fee
              move ws-cancel-fee       to ws-cancel-fee
           else
      *       move ws-ref-wholesale-fee to ws-net-cancel-fee
              move ws-ref-wholesale-fee to ws-cancel-fee
           end-if
           display ' stuff ' de-cert ' ' ws-cancel-fee ' '
           ws-customer-refund ' ' ws-acct-fee ' ' ws-ref-wholesale-fee
             ' ' de-ah-rfnd

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-**
      ***                                                             **
      ***          Let's start the no charge back stuff               **
      ***         to calculate the unearned marketing fee             **
      ***                                                             **
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-**

           move zeros                  to ws-mktg-fee
           move de-lf-canc-dte         to dc-greg-date-cymd
                                          ws-prev-cancel-dte
           display ' canc dates ' de-lf-canc-dte ' ' de-ah-canc-dte
           if (de-ah-canc-dte not = zeros)
              and (de-ah-canc-dte > dc-greg-date-cymd)
              move de-ah-canc-dte      to dc-greg-date-cymd
                                          ws-prev-cancel-dte
           end-if
           move 'L'                    to dc-option-code
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           if no-conversion-error
              move dc-bin-date-1       to ws-bin-cancel-date
      *        move dc-greg-date-a-edit to tb-cancel-date
           else
              display ' invalid cancel date - aborting ' cr-state ' '
                 cr-account ' ' cr-cert-no ' ' 
                 dc-greg-date-cymd ' ' dc-error-code
              perform abend-pgm
           end-if

           move ws-bin-eff-date        to dc-bin-date-1
           move ws-bin-cancel-date     to dc-bin-date-2
           move '1'                    to dc-option-code
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           if no-conversion-error
              display ' elapsed months/days b4 ' dc-elapsed-months
                 ' ' dc-odd-days-over
              move dc-elapsed-months   to ws-elapsed-months
              if (dc-odd-days-over > +1)
                 and (dc-elapsed-months > +0)
                 add +1 to ws-elapsed-months
              end-if
              display ' elapsed mos ' cr-cert-no ' ' ws-elapsed-months
              move ws-elapsed-months to ws-prev-elapsed-months
           else
              display ' invalid dates - aborting ' cr-state ' '
                 cr-account ' ' cr-cert-no ' ' cr-dt ' ' de-lf-canc-dte
                 ' ' de-ah-canc-dte ' ' dc-greg-date-cymd ' '
                 dc-error-code
              perform abend-pgm
           end-if

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-**
      ***                                                             **
      ***    Some assumptions here. Only calculate the refund short   **
      ***  due to to ncb on the I or J types where there is a NCB     **
      ***                                                             **
      ***                                                             **
      ***                                                             **
           if ws-customer-refund > zeros
           perform varying a1 from +1 by +1 until a1 > +10
              if am-comm-chargeback (a1) not numeric
                 move zeros            to am-comm-chargeback (a1)
              end-if
              if (cr-agt-type (a1) = 'J' or 'I')
                 and (am-comm-chargeback(a1) <> zeros)
                 and (ws-elapsed-months > am-comm-chargeback (a1))
                 compute ws-refund-short-due-ncb rounded =
                    ws-refund-short-due-ncb +
                    ((cr-lcom-ah (a1) * +1000) * ws-ah-cnc-fact)
              end-if
              if cr-agt-type (a1) = 'J' or 'I'
      *          move 3 to am-comm-chargeback (a1)
                 if (am-comm-chargeback (a1) = zeros)
                    or (ws-elapsed-months <= am-comm-chargeback (a1))
                    display ' computing prorata fee '
                    cr-cert-no ' ' am-comm-chargeback (a1)
                    compute ws-mktg-fee rounded =
                       ws-mktg-fee + ((cr-lcom-ah (a1) * +1000) *
                       ws-ah-cnc-fact)
                 end-if
              end-if
           end-perform
           end-if
      ***                                                             **
      ***                                                             **
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-**

           MOVE DE-LF-TYPE             TO WS-LF-BEN-CODE

      **** ah clp, fees and o/w

           if (de-ah-type not = '00')
              and (ws-customer-refund > 0)
              display ' ah canc ' de-cert ' ' de-rei-ahrfnd ' '
                de-lf-prm-alt ' ' ws-ah-cnc-fact
              compute ws-ref-clp rounded =
                 ws-ref-clp + (de-lf-prm-alt * ws-ah-cnc-fact)
              perform varying a1 from +1 by +1 until a1 > +10
                 evaluate true
                    when de-agt-type (a1) = 'L'
                       compute ws-ref-pm-fee rounded =
                          ws-ref-pm-fee +
                          (de-a-pc(a1) * +1000 * ws-ah-cnc-fact)
                    when de-agt-type(a1) = 'J' or 'I'
                       compute ws-ref-inc rounded =
                          ws-ref-inc +
                          (de-a-pc(a1) * +1000 * ws-ah-cnc-fact)
                    when de-agt-type(a1) = 'N'
                       compute ws-ref-cso-admin rounded =
                          ws-ref-cso-admin +
                          (de-a-pc(a1) * +1000 * ws-ah-cnc-fact)
                    when de-agt-type (a1) = 'S'
                       compute ws-ref-cso-admin rounded =
                          ws-ref-cso-admin +
                             (de-ah-rfnd * de-a-pc(a1))
                 end-evaluate
              end-perform
           end-if

           perform 0325-build-cancel   thru 0325-exit

           .
       0200-exit.
           exit.

       0210-get-cert-rec.

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-**
      ***                                                             **
      ***    The goal here is to get the net cancel fee (income) and  **
      *** the "short on refunds due to chgback" (expense)             **
      ***                                                             **
      ***    In order to do that, I need to re-calc the account       **
      *** commissions then apply the refund factor. Next, calc the    **
      *** ue wholesale fee = refund(ue retail - ue acct fee).         **
      ***                                                             **
      ***                                                             **
      ***                                                             **
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-**

           if de-control > cr-full-control
              perform 0220-read-cert   thru 0220-exit
              go to 0210-get-cert-rec
           else
              if de-control < cr-full-control
                 display ' no cert for extract ' 
                 display '      extr = ' de-cntrl1 ' ' de-eff ' '
                    de-cert
                 display '      cert = ' cr-acct-control ' ' cr-dt ' '
                    cr-cert-no
                 perform abend-pgm
              end-if
           end-if

           move +50.00                 to cr-cancel-fee
           move cr-dt                  to dc-greg-date-cymd
           move 'L'                    to dc-option-code
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           if no-conversion-error
              move dc-bin-date-1       to ws-bin-eff-date
           else
              display ' invalid effective date - aborting ' cr-state
                 ' ' cr-account ' ' cr-cert-no ' ' cr-dt ' '
                 dc-error-code
              perform abend-pgm
           end-if

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-**
      ***                                                             **
      ***          Compute original account fees                      **
      ***                                                             **
      ***                                                             **
           perform varying c1 from +1 by +1 until c1 > +10
              evaluate true
                 when cr-agt-type (c1) = 'N'
                    compute ws-cso-admin-fee rounded =
                       ws-cso-admin-fee + (cr-lcom-ah (c1) * +1000)
                 when cr-agt-type (c1) = 'J' or 'I'
                    compute ws-mktg-fee rounded =
                       ws-mktg-fee + (cr-lcom-ah (c1) * +1000)
                 when cr-agt-type (c1) = 'O' OR 'P'
                    compute ws-ga-fee rounded =
                       ws-ga-fee + (cr-ahprm * cr-lcom-ah (c1))
              end-evaluate
           end-perform
           move cr-ah-clp              to ws-clp-prem
           compute ws-acct-fee =
              cr-ahprm - ws-clp-prem - ws-cso-admin-fee - ws-mktg-fee
           display ' acct fee stuff ' de-cert ' ' ws-clp-prem ' '
           ws-cso-admin-fee ' ' ws-mktg-fee ' ' ws-acct-fee
      ***                                                             **
      ***                                                             **
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-**

           .
       0210-exit.
           exit.

       0220-read-cert.

           read cert-in at end
              set end-of-cert to true
           end-read

           if not end-of-cert
              add 1 to cert-in-recs
           end-if

           .
       0220-exit.
           exit.

       0300-cert-break.

           if ws-prev-cntrl1 = spaces
              go to 0300-exit
           end-if

      *     if ws-prev-cancel-dte = zeros
      *        move -1                  to nu-cancel-dte
      *     else
      *        move +0                  to nu-cancel-dte
      *     end-if
      *     move ws-prev-carrier        to db-carrier           
      *     move ws-prev-group          to db-grouping          
      *     move ws-prev-state          to db-state             
      *     move ws-prev-account        to db-account           
      *     move ws-prev-eff-dt         to ws-work-date-n
      *     string
      *        ws-wd-mm     '/'
      *        ws-wd-dd     '/'
      *        ws-wd-ccyy
      *        delimited by size        into db-eff-dte
      *     end-string
      *     move ws-prev-cert-no        to db-cert-no           
      *     move ws-prev-lf-ben         to db-lf-ben-code
      *     move ws-prev-ah-ben         to db-ah-ben-code
      *     move spaces                 to db-cancel-dte
      *     if ws-prev-cancel-dte not = zeros
      *        move ws-prev-cancel-dte  to ws-work-date-n
      *        string
      *           ws-wd-mm     '/'
      *           ws-wd-dd     '/'
      *           ws-wd-ccyy
      *           delimited by size     into db-cancel-dte
      *        end-string
      *     end-if
      *     move ws-prev-elapsed-months to db-elapsed-months
      *     move ws-net-lf-clp          to db-net-lf-clp        
      *     move ws-net-ah-clp          to db-net-ah-clp        
      *     move ws-net-lf-ow           to db-net-lf-ow         
      *     move ws-net-ah-ow           to db-net-ah-ow         
      *     move ws-net-lf-pm-fee       to db-net-lf-pm-fee     
      *     move ws-net-ah-pm-fee       to db-net-ah-pm-fee     
      *     move ws-net-lf-inc          to db-net-lf-inc        
      *     move ws-net-ah-inc          to db-net-ah-inc        
      *     move ws-net-lf-cont-fee     to db-net-lf-cont-fee   
      *     move ws-net-ah-cont-fee     to db-net-ah-cont-fee   
      *     move ws-net-lf-cso-admin    to db-net-lf-cso-admin  
      *     move ws-net-ah-cso-admin    to db-net-ah-cso-admin  
      *     move ws-net-lf-ga-sales     to db-net-lf-ga-sales   
      *     move ws-net-ah-ga-sales     to db-net-ah-ga-sales
      *     move ws-net-cancel-fee      to db-cancel-fee-income
      *     move ws-refund-short-due-ncb
      *                                 to db-cancel-fee-expense
      *     move ws-lf-claims           to db-net-lf-claims     
      *     move ws-ah-claims           to db-net-ah-claims

           perform 0340-write-extract  thru 0340-exit
           perform 0030-init-work-area thru 0030-exit

           .
       0300-exit.
           exit.

       0310-build-issue.

           move de-carrier             to db-carrier           
           move de-grouping            to db-grouping          
           move de-state               to db-acct-state
           move de-clp-state           to db-clp-state
           move de-account             to db-account
           move de-eff                 to ws-work-date-n
           string
              ws-wd-mm     '/'
              ws-wd-dd     '/'
              ws-wd-ccyy
              delimited by size        into db-eff-dte
           end-string
           move de-cert                to db-cert-no           
           move de-lf-type             to db-lf-ben-code
           move de-ah-type             to db-ah-ben-code
           move ws-msrp                to db-msrp
           move de-ah-term             to db-term
           move de-ah-prm              to db-retail
           move ws-iss-clp             to db-clp
           move ws-iss-pm-fee          to db-pm-fee
           move ws-iss-inc             to db-inc        
           move ws-iss-cso-admin       to db-cso-admin  
           move zeros                  to db-cancel-dte
           move -1                     to nu-cancel-dte
           compute ws-iss-acct-fee =
              de-ah-prm - ws-iss-clp - ws-iss-cso-admin - ws-iss-inc
           move ws-iss-acct-fee        to db-acct-comp
           compute ws-iss-wholesale-fee = de-ah-prm - ws-iss-acct-fee
           move ws-iss-wholesale-fee   to db-wholesale

           perform 0320-insert-issue-row
                                       thru 0320-exit

           .
       0310-exit.
           exit.

       0320-insert-issue-row.

           EXEC SQL
              insert into VPP_DETAIL_EXTRACTS (
                MonthEndDte       ,
                Carrier           ,
                Grouping          ,
                AcctState         ,
                ClpState          ,
                Account           ,
                EffectDte         ,
                CertNo            ,
                TranType          ,
                LfBenCode         ,
                AhBenCode         ,
                MSRP              ,
                Term              ,
                CancelDte         ,
                ElapsedMonths     ,
                Retail            ,
                CancelFee         ,
                CustomerRef       ,
                AcctComp          ,
                TotalWholeSale    ,
                CLP               ,
                CSOAdmin          ,
                PgmMgtFee         ,
                Incentive         ,
                CancelFeeIncome   ,
                CancelFeeExpense  ,
                Claims)
              values (
                :db-month-end-dt     ,
                :db-carrier          ,
                :db-grouping         ,
                :db-acct-state       ,
                :db-clp-state        ,
                :db-account          ,
                :db-eff-dte          ,
                :db-cert-no          ,
                '1'                  ,
                :db-lf-ben-code      ,
                :db-ah-ben-code      ,
                :db-msrp-a           ,
                :db-term             ,
                :db-cancel-dte :nu-cancel-dte,
                '0'                  ,
                :db-retail-a         ,
                '0'                  ,
                '0'                  ,
                :db-acct-comp-a      ,
                :db-wholesale-a      ,
                :db-clp-a            ,
                :db-cso-admin-a      ,
                :db-pm-fee-a         ,
                :db-inc-a            ,
                '0',
                '0',
                '0')
           end-exec
           if sqlcode not = 0
              display "Error: cannot insert issue row "
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' sql err mess    ' sqlerrmc
              display ' in out cnt ' ws-recs-in ' ' rec-cnt
              display ' offending rec ' ws-extract
           else
              add 1 to rec-cnt
           end-if

           .
       0320-exit.
           exit.

       0325-build-cancel.

           move de-carrier             to db-carrier           
           move de-grouping            to db-grouping          
           move de-state               to db-acct-state             
           move de-clp-state           to db-clp-state
           move de-account             to db-account           
           move de-eff                 to ws-work-date-n
           string
              ws-wd-mm     '/'
              ws-wd-dd     '/'
              ws-wd-ccyy
              delimited by size        into db-eff-dte
           end-string
           move de-cert                to db-cert-no           
           move de-lf-type             to db-lf-ben-code
           move de-ah-type             to db-ah-ben-code
           move de-ah-term             to db-term
           move de-ah-rfnd             to ws-ref-retail
           compute db-retail = ws-ref-retail * -1
           compute db-clp = ws-ref-clp * -1
           compute db-pm-fee = ws-ref-pm-fee * -1
           compute db-inc = ws-ref-inc * -1
           compute db-cso-admin = ws-ref-cso-admin * -1
           move ws-msrp                to db-msrp
           move spaces                 to db-cancel-dte
           move -1                     to nu-cancel-dte
           if de-ah-canc-dte not = zeros
              move +0                  to nu-cancel-dte
              move de-ah-canc-dte      to ws-work-date-n
              string
                 ws-wd-mm     '/'
                 ws-wd-dd     '/'
                 ws-wd-ccyy
                 delimited by size     into db-cancel-dte
              end-string
           end-if
      *    compute ws-ref-acct-fee =
      *       ws-iss-acct-fee * ws-ah-cnc-fact
      *    compute ws-ref-wholesale-fee =
      *       ws-ref-retail - ws-ref-acct-fee
           compute db-wholesale = ws-ref-wholesale-fee * -1
           compute db-acct-comp = ws-ref-acct-fee * -1
           move ws-elapsed-months      to db-elapsed-months
      *    move cr-cancel-fee          to ws-cancel-fee
      *    compute ws-customer-refund =
      *       ws-ref-retail - ws-cancel-fee
           move ws-customer-refund     to db-customer-ref
           move ws-cancel-fee          to db-cancel-fee
                                          db-cancel-fee-income
      *    move ws-net-cancel-fee      to db-cancel-fee-income
           move ws-refund-short-due-ncb
                                       to db-cancel-fee-expense
           perform 0330-insert-cancel-row
                                       thru 0330-exit

           .
       0325-exit.
           exit.

       0330-insert-cancel-row.

           EXEC SQL
              insert into VPP_DETAIL_EXTRACTS (
                MonthEndDte       ,
                Carrier           ,
                Grouping          ,
                AcctState         ,
                ClpState          ,
                Account           ,
                EffectDte         ,
                CertNo            ,
                TranType          ,
                LfBenCode         ,
                AhBenCode         ,
                MSRP              ,
                Term              ,
                CancelDte         ,
                ElapsedMonths     ,
                Retail            ,
                CancelFee         ,
                CustomerRef       ,
                AcctComp          ,
                TotalWholeSale    ,
                CLP               ,
                CSOAdmin          ,
                PgmMgtFee         ,
                Incentive         ,
                CancelFeeIncome   ,
                CancelFeeExpense  ,
                Claims)
              values (
                :db-month-end-dt     ,
                :db-carrier          ,
                :db-grouping         ,
                :db-acct-state       ,
                :db-clp-state        ,
                :db-account          ,
                :db-eff-dte          ,
                :db-cert-no          ,
                '2'                  ,
                :db-lf-ben-code      ,
                :db-ah-ben-code      ,
                :db-msrp-a           ,
                :db-term             ,
                :db-cancel-dte :nu-cancel-dte,
                :db-elapsed-months   ,
                :db-retail-a         ,
                :db-cancel-fee-a     ,
                :db-customer-ref-a   ,
                :db-acct-comp-a      ,
                :db-wholesale-a      ,
                :db-clp-a            ,
                :db-cso-admin-a      ,
                :db-pm-fee-a         ,
                :db-inc-a            ,
                :db-cancel-fee-income-a,
                :db-cancel-fee-expense-a,
                '0')
           end-exec
           if sqlcode not = 0
              display "Error: cannot insert cancel row "
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' sql err mess    ' sqlerrmc
              display ' in out cnt ' ws-recs-in ' ' rec-cnt
              display ' offending rec ' ws-extract
           else
              add 1 to rec-cnt
           end-if

           .
       0330-exit.
           exit.

       0332-build-claim.

           move de-carrier             to db-carrier           
           move de-grouping            to db-grouping          
           move de-state               to db-acct-state
           move de-clp-state           to db-clp-state
           move de-account             to db-account
           move de-eff                 to ws-work-date-n
           string
              ws-wd-mm     '/'
              ws-wd-dd     '/'
              ws-wd-ccyy
              delimited by size        into db-eff-dte
           end-string
           move de-cert                to db-cert-no           
           move de-lf-type             to db-lf-ben-code
           move de-ah-type             to db-ah-ben-code
           move de-ah-term             to db-term
           move zeros                  to db-elapsed-months
           move zeros                  to db-cancel-dte
           compute db-net-ah-claims = ws-lf-claims + ws-ah-claims
           move -1                     to nu-cancel-dte

           perform 0334-insert-claim-row
                                       thru 0334-exit

           .
       0332-exit.
           exit.

       0334-insert-claim-row.

           EXEC SQL
              insert into VPP_DETAIL_EXTRACTS (
                MonthEndDte       ,
                Carrier           ,
                Grouping          ,
                AcctState         ,
                ClpState          ,
                Account           ,
                EffectDte         ,
                CertNo            ,
                TranType          ,
                LfBenCode         ,
                AhBenCode         ,
                MSRP              ,
                Term              ,
                CancelDte         ,
                ElapsedMonths     ,
                Retail            ,
                CancelFee         ,
                CustomerRef       ,
                AcctComp          ,
                TotalWholeSale    ,
                CLP               ,
                CSOAdmin          ,
                PgmMgtFee         ,
                Incentive         ,
                CancelFeeIncome   ,
                CancelFeeExpense  ,
                Claims)
              values (
                :db-month-end-dt     ,
                :db-carrier          ,
                :db-grouping         ,
                :db-acct-state       ,
                :db-clp-state        ,
                :db-account          ,
                :db-eff-dte          ,
                :db-cert-no          ,
                '3'                  ,
                :db-lf-ben-code      ,
                :db-ah-ben-code      ,
                '0'                  ,
                :db-term             ,
                :db-cancel-dte  :nu-cancel-dte,
                :db-elapsed-months,
                '0',
                '0',
                '0',
                '0',
                '0',
                '0',
                '0',
                '0',
                '0',
                '0',
                '0',
                :db-net-ah-claims-a)
           end-exec
           if sqlcode not = 0
              display "Error: cannot insert claim  row "
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' sql err mess    ' sqlerrmc
              display ' in out cnt ' ws-recs-in ' ' rec-cnt
              display ' offending rec ' ws-extract
           else
              add 1 to rec-cnt
           end-if

           .
       0334-exit.
           exit.

       0340-write-extract.

           if zeros = ws-lf-ben-code and ws-lf-claims
              continue
           else
              move spaces              to ws-extract-rec-out
              move '1'                 to ex-ben-type
              perform 0350-write-extract thru 0350-exit
           end-if

           if zeros = ws-ah-ben-code and ws-iss-clp and ws-ref-clp
                 and ws-iss-pm-fee and ws-ref-pm-fee
                 and ws-iss-inc and ws-ref-inc
                 and ws-iss-cso-admin and ws-ref-cso-admin
                 and ws-ah-claims
              continue
           else
              move spaces              to ws-extract-rec-out
              move '2'                 to ex-ben-type
              perform 0350-write-extract thru 0350-exit
           end-if

           .
       0340-exit.
           exit.

                                                                        
       0350-write-extract.

           move ws-prev-carrier        to ex-carrier
           move ws-prev-group          to ex-grouping
           move ws-prev-state          to ex-acct-state
           move ws-prev-clp-state      to ex-clp-state
           move ws-prev-account        to ex-account
           move ws-prev-eff-dt         to ex-eff-dte
           move ws-prev-cert-no        to ex-cert-no
           move ws-prev-cancel-dte     to ex-cancel-dte
           move ws-prev-elapsed-months to ex-elapsed-months
           if ex-ben-type = '1'  *> 1 = life
              move ws-prev-lf-ben      to ex-ben-code
              move zeros               to ex-net-clp
                                          ex-net-ow
                                          ex-net-pgm-mgt-fee
                                          ex-net-incentive
                                          ex-net-cont-fee
                                          ex-net-cso-admin-fee
                                          ex-net-ga-sales
                                          ex-cancel-fee-income
                                          ex-cancel-fee-expense
              move ws-lf-claims        to ex-claim-pmts
           else               *> 2 = disab
              move ws-prev-ah-ben      to ex-ben-code
              move zeros               to ex-net-ow
                                          ex-net-cont-fee
                                          ex-net-ga-sales
              compute ex-net-clp =
                 ws-iss-clp - ws-ref-clp
              compute ex-net-pgm-mgt-fee =
                 ws-iss-pm-fee - ws-ref-pm-fee
              compute ex-net-incentive =
                 ws-iss-inc - ws-ref-inc
              compute ex-net-cso-admin-fee =
                 ws-iss-cso-admin - ws-ref-cso-admin
              move ws-ah-claims        to ex-claim-pmts
              compute ex-cancel-fee-expense rounded =
                 ws-refund-short-due-ncb * +1
              compute ex-cancel-fee-income rounded =
                 ws-cancel-fee * +1
      *       compute ex-cancel-fee-income rounded =
      *          ws-net-cancel-fee * +1
              move ws-ah-claims        to ex-claim-pmts
           end-if

           write extr-record from ws-extract-rec-out           

           .
       0350-exit.
           exit.

       0400-acct-break.

           .
       0400-exit.
           exit.

       3000-connect-to-logic.

           display ' about to connect to Logic '

      ****  The below code is for when the db has been
      ****  converted to sql server 2016
           evaluate ws-kix-myenv
              when 'cid1p'
                 move '//sdv-db01.cso.local:1433;'
                                       to p-sql-server
      *       when 'mdoff'
      *          move '//hov-tstdb01.cso.local:55330;'
      *                                to p-sql-server
              when other
                 move '//hov-tstdb01.cso.local:1433;'
                                       to p-sql-server
           end-evaluate

           move 'Logic'                to p-sql-database

           CALL 'SQLCONNECT' USING sqlconnect-parms
           display ' ret code ' p-connect-return-code
           move p-connect-return-code  to sqlcode
           move p-sql-return-message   to sqlerrmc

NTTDel**       CONNECT TO :svr
NTTDel**             USER :usr-pass
NTTIns*        CONNECT TO :svr
NTTIns*          USER     :usr
NTTIns*          USING    :pass
      *     END-EXEC

           if sqlcode not = 0
              display "Error: cannot connect to Logic"
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display sqlerrmc
              perform abend-pgm
           end-if

           display ' Successful connect ' svr

           .
       3000-exit.
           exit.

       3010-create-table.

           display ' about to create table VPP_DETAIL_EXTRACTS '
           EXEC SQL
              create table VPP_DETAIL_EXTRACTS (
                MonthEndDte        datetime NOT null,
                Carrier            char(1)  NOT NULL,
                Grouping           char(6)  NOT NULL,
                AcctState          char(2)  NOT NULL,
                ClpState           char(2)  NOT NULL,
                Account            char(10) NOT NULL,
                EffectDte          datetime NOT NULL,
                CertNo             char(11) NOT NULL,
                TranType           char(1)  NOT NULL,
                LfBenCode          char(2)  NOT NULL,
                AhBenCode          char(2)  NOT NULL,
                MSRP               decimal(11,2),
                Term               int,
                CancelDte          datetime,
                ElapsedMonths      int,
                Retail             decimal(11,2),
                CancelFee          decimal(7,2),
                CustomerRef        decimal(11,2),
                AcctComp           decimal(11,2),
                TotalWholeSale     decimal(11,2),
                CLP                decimal(11,2),
                CSOAdmin           decimal(11,2),
                PgmMgtFee          decimal(11,2),
                Incentive          decimal(11,2),
                CancelFeeIncome    decimal(11,2),
                CancelFeeExpense   decimal(11,2),
                Claims             decimal(11,2),
                 CONSTRAINT PK_VPP_DETAIL_EXTRACTS
                    PRIMARY KEY CLUSTERED
                   (MonthEndDte, Carrier, Grouping, AcctState,
                    Account, EffectDte, CertNo, TranType)
             	   )
           END-EXEC


           if sqlcode not = 0
              display "Error: cannot create table "
              move sqlcode             to ws-disp-code
              display ' sql return code ' ws-disp-code
              display ' sql err mess    ' sqlerrmc
              perform abend-pgm
           end-if

           display ' Table created '

           .
       3010-exit.
           exit.

       3020-drop-table.

           display 'Begin Drop table'
           EXEC SQL
               drop table VPP_DETAIL_EXTRACTS
           END-EXEC
           if sqlcode not = 0
              display "Error(anticipated) : cannot drop table "
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' message ' sqlerrmc
           end-if

           .
       3020-exit.
           exit.

       3030-truncate-table.

           display 'Begin Truncate table'
           EXEC SQL
NTTDel*        truncate table VPP_DETAIL_EXTRACTS
NTTIns         EXECUTE IMMEDIATE 'truncate table VPP_DETAIL_EXTRACTS'
           END-EXEC

           if sqlcode not = 0
              display "Error : cannot truncate table "
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' message ' sqlerrmc
           end-if

           .
       3030-exit.
           exit.

       3040-finish-up.

           EXEC SQL
NTTDel*        commit work release
NTTIns         commit work
           END-EXEC

           if sqlcode not = 0
              display "Error: commit work release "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if

           EXEC SQL
              DISCONNECT
           END-EXEC

           if sqlcode not = 0
              display "Error: disconnect  "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if

           .
       3040-exit.
           exit.

       3050-check-for-rerun.

           exec sql
              SELECT
                 MONTHENDDTe,
                 Count(*)
              INTO
                 :db-MONTH-END-DT,
                 :WS-REC-CNTR
              FROM
                 vpp_detail_extracts
              GROUP BY MONTHENDDTe
              HAVING convert(varchar(10),MONTHENDDTe,102)
                    = :ws-test-date
           end-exec

           if sqlcode not = 0 and 1
              display "Error : check for rerun  "
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' message ' sqlerrmc
           end-if

           display ' counter ' ws-rec-cntr
           if ws-rec-cntr > 0
              perform 3060-delete-rows thru 3060-exit
           end-if

           .
       3050-exit.
           exit.

       3060-delete-rows.

           exec sql delete
              FROM
                 vpp_detail_extracts
              where convert(varchar(10),MONTHENDDTe,102)
                    = :ws-test-date
           end-exec

           if sqlcode not = 0 and 1
              display "Error : deleting rows  "
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' message ' sqlerrmc
              go to 3060-exit
           end-if

           EXEC SQL
               commit
           END-EXEC

           if sqlcode not = 0 and 1
              display "Error : commit of delete  "
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' message ' sqlerrmc
           end-if

           .
       3060-exit.
           exit.

       6300-CHECK-TYPES.
      
      *    MOVE SPACES                 TO EX-AZ EX-LZ
           MOVE CLAS-STARTL            TO CLAS-INDEXL
           MOVE CLAS-STARTA            TO CLAS-INDEXA
      
           IF DE-LF-TYPE = '  ' OR '00' OR 'DD' or 'CU'
              GO TO 6330-CHECK-AH
           END-IF
           .
                                                                        
       6310-CHECK-LIFE.                                                 
           MOVE DE-LF-TYPE             TO CLAS-LOOK
      
           IF CLAS-STARTL = ZEROS                                       
              GO TO 6330-CHECK-AH
           END-IF
           .
                                                                        
       6320-LOOP-LIFE.                                                  
           IF CLAS-INDEXL GREATER THAN CLAS-MAXL                        
              GO TO 6330-CHECK-AH
           END-IF
                                                                        
           IF CLAS-LOOK NOT = CLAS-I-BEN (CLAS-INDEXL)                  
              ADD 1 TO CLAS-INDEXL                                      
              GO TO 6320-LOOP-LIFE
           END-IF
           .
                                                                        
       6330-CHECK-AH.                                                   
           IF DE-AH-TYPE = '  ' OR '00'
              GO TO 6399-EXIT
           END-IF
                                                                        
           MOVE DE-AH-TYPE             TO CLAS-LOOK

           IF CLAS-STARTA = ZEROS                                       
              GO TO 6399-EXIT
           END-IF
           .
       6340-LOOP-AH.                                                    
      
           IF CLAS-INDEXA GREATER THAN CLAS-MAXA
              GO TO 6399-EXIT
           END-IF
      
           IF CLAS-LOOK NOT = CLAS-I-BEN (CLAS-INDEXA)
              ADD 1 TO CLAS-INDEXA
              GO TO 6340-LOOP-AH
           END-IF
      
           .
       6399-EXIT.                                                       
           EXIT.
       8000-PRT-RTN.
                                   COPY ELCPRT2.

       8099-PRT-XIT.
           EXIT.

       9900-END-JOB.                                                    
                                                                        
           MOVE '-'                    TO P-CTL.                        
           MOVE P-LINE                 TO P-DATA.                       
           MOVE P-CTL                  TO X.                            
           PERFORM 8000-PRT-RTN THRU 8099-PRT-XIT.                      
                                                                        
           MOVE '-'                    TO X.                            
           MOVE COMP-MESS              TO P-DATA.                       
           PERFORM 8000-PRT-RTN THRU 8099-PRT-XIT.                      

           CLOSE
              EP-EXTR
              ERACCTT
              EXTR-file
              END-PRINT
              CERT-IN

           .                                                                        
       9920-CLOSE-FICH.                                                 
                                   COPY ELCPRTC.                        
                                                                        
       9999-STOP-RUN.                                                   
           MOVE ZEROS  TO RETURN-CODE.                                  
           GOBACK.                                                      
                                                                        
                                                                        
       8500-DATE-CONVERT.              
                                       COPY ELCDCS.

       abend-pgm.

            call 'ABORTME'.
            
            goback.
                                                                        
