      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
      *****************************************************************
      *                                                               *
      * Copyright (c) 2015 by Central States Health and Life          *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SQLBPROCID.
       AUTHOR.   Cowtown.

       DATE-COMPILED.
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
062117* 062117   2017032900001   PEMA  ADD COMPANY VPP PROCESSING
102717* 102717 CR2017101800002   PEMA  Correct bug when apostrophe in name
091219* 091219 CR2018050300001   PEMA  Add Update of create date to table
070221* 070221 IR2021070200001   PEMA  Add FNL processing
      ******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ELCNTL           ASSIGN TO ELCNTL
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS CF-CONTROL-PRIMARY
                                   FILE STATUS IS ELCNTL-FILE-STATUS.

           SELECT DISK-DATE        ASSIGN TO SYS019.

       DATA DIVISION.
       FILE SECTION.

       FD  ELCNTL.

                                       COPY ELCCNTL.

       FD  DISK-DATE
                                       COPY ELCDTEFD.

       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '  SQLBPROCID WORKING STORAGE    '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW                   PIC X VALUE SPACES.
           88  END-OF-ELCNTL                 VALUE 'Y'.
       77  ELCNTL-RECS-IN              PIC 9(9) VALUE ZEROS.
       77  S1                          PIC S999  VALUE +0 COMP-3.
       77  S2                          PIC S999  VALUE +0 COMP-3.
       77  C1                          PIC S999  VALUE +0 COMP-3.
102717 77  i1                          PIC S999  VALUE +0 COMP-3.
102717 77  o1                          PIC S999  VALUE +0 COMP-3.
       77  ws-sql-code                 pic s9(7) value zeros.
       77  ws-dis-sql-code             pic -9999999 value zeros.
       77  PGM-SUB                     PIC S9(5) COMP-3 VALUE +515.
062117 77  ws-records-inserted         pic 9(9) value zeros.
062117 77  ws-sql-date-time            pic x(24) value spaces.
102717 77  ws-hyphen-ctr               pic s999 comp-3 value zeros.
102717 77  ws-work-processor           pic x(30) value spaces.


       EXEC SQL
          INCLUDE SQLDA
       END-EXEC
      
       EXEC SQL
          INCLUDE SQLCA
       END-EXEC
      
021714 01  P pointer.
021714 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
021714 01  var-ptr pointer.
021714 01  env-var-len                 pic 9(4)  binary.
021714 01  rc                          pic 9(9)  binary.
021714
021714 01  WS-KIXSYS.
021714     05  WS-KIX-FIL1             PIC X(10).
021714     05  WS-KIX-APPS             PIC X(10).
021714     05  WS-KIX-ENV              PIC X(10).
021714     05  WS-KIX-MYENV            PIC X(10).
021714     05  WS-KIX-SYS              PIC X(10).



       01  WS-ABEND-AREA.
           05  WS-ABEND-FILE-STATUS    PIC X(02).
           05  WS-ABEND-MESSAGE        PIC X(80) VALUE SPACES.
           05  WS-RETURN-CODE          PIC S9(04)  COMP VALUE +0.
           05  WS-ZERO                 PIC S9(01) VALUE +0 COMP-3.

       EXEC SQL
          BEGIN DECLARE SECTION
       END-EXEC

       01  sqlcmd                      pic x(5120).
       01  WS-MOE-DATE                 pic x(10).
       01  svr                         pic x(32).
       01  usr                         pic x(32).
       01  pass                        pic x(32).
       01  usr-pass                    pic x(64).
       01  ws-disp-code                pic s9(11).
       01  ws-proc-table               pic x(20).
       01  ws-claim-table              pic x(20).
       01  ws-credit-table             pic x(20).

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  These indicators are used to tell sql server that i am    ***
      ***  passing it a null value.  The indicator will be -1        ***
      ***  if the value is nulls and +0 if the value is other than   ***
      ***  nulls.  Here is a sample on how to use it.                ***
      ***                                                            ***
      ***      if db-date1 = spaces move -1 to nu-date1 end-if       *** 
      ***     EXEC SQL                                               ***
      ***        insert into ERMEBL (                                ***
      ***           date1,                                           ***
      ***           date2)                                           ***
      ***        values (                                            ***
      ***           :db-date1      :nu-date1,                        ***
      ***           :db-date2      :nu-date2)                        ***
      ***     END-EXEC                                               ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

       01  indicator-vaiables-for-nulls.
           05  NU-RECORDED-DT          PIC s9(4) comp value +0.
           05  NU-CHECK-WRITTEN-DT     PIC s9(4) comp value +0.
           05  nu-check-cashed-dt      pic s9(4) comp value +0.
           05  NU-CREDIT-SELECT-DT     PIC s9(4) comp value +0.
           05  NU-CREDIT-ACCEPT-DT     PIC s9(4) comp value +0.
           05  NU-VOID-DT              PIC s9(4) comp value +0.
           05  NU-APPROVAL-DT          PIC s9(4) comp value +0.
           05  NU-CANC-DT              PIC s9(4) comp value +0.


       01  processor-table.
           05  pt-procid               pic x(4).
           05  pt-name                 pic x(30).
           05  pt-title                pic x(26).
           05  pt-claim-access         pic x.
           05  pt-credit-access        pic x.
           05  pt-security-off         pic x.
           05  pt-csr-supr             pic x.
           05  pt-crd-sys-cntrls       pic xx.
           05  pt-crd-force-cap        pic x.
           05  pt-clm-sys-cntrls       pic xx.
           05  pt-clm-force-cap        pic x.
           05  pt-tol-n                pic 9(5).99.
           05  pt-tol redefines
               pt-tol-n                pic x(8).
           05  pt-max-reg-pmt-n        pic 9(7).99.
           05  pt-max-reg-pmt redefines
               pt-max-reg-pmt-n        pic x(10).
           05  pt-max-reg-days         pic 999.
           05  pt-max-auto-pmt-n       pic 9(7).99.
           05  pt-max-auto-pmt redefines
               pt-max-auto-pmt-n       pic x(10).
           05  pt-max-auto-mos         pic 999.
           05  pt-days-tol             pic 999.
           05  pt-max-lf-pmt-n         pic 9(7).99.
           05  pt-max-lf-pmt redefines
               pt-max-lf-pmt-n         pic x(10).
           05  pt-approval-lvl         pic x.
           05  pt-max-exp-pmt-n        pic 9(7).99.
           05  pt-max-exp-pmt redefines
               pt-max-exp-pmt-n        pic x(10).
091219     05  pt-create-dt            pic x(10).

       01  credit-table.
           05  cr-procid               pic x(4).
           05  filler occurs 44 times.
               10  cr-credit-app       pic 99.
               10  cr-app-desc         pic x(30).
               10  cr-browse-cap       pic x.
               10  cr-update-cap       pic x.
           
       01  claim-table.
           05  cl-procid               pic x(4).
           05  filler occurs 44 times.
               10  cl-claim-app        pic 99.
               10  cl-app-desc         pic x(30).
               10  cl-browse-cap       pic x.
               10  cl-update-cap       pic x.

062117 01  ws-stuff-for-files-upload-data.
062117     05  ws-up-table-name        pic x(15).
062117     05  ws-up-file-name         pic x(15).
062117     05  ws-up-create-date       pic x(25).
062117     05  ws-up-modify-date       pic x(25).
062117     05  ws-up-full-path         pic x(25).
062117     05  ws-up-rec-count         pic 9(9).

       EXEC SQL
          END DECLARE SECTION
       END-EXEC

       01  WS-EX-INIT-REC1            PIC X(95) VALUE LOW-VALUES.
       01  EX-USER-REC1.
           12  EX1-COMP-ID            PIC XXX.

           12  EX1-USER-ID            PIC X(4).
           12  EX1-TAB2               PIC X.
           12  EX1-USER-PASSWORD      PIC X(11).
           12  EX1-TAB3               PIC X.
           12  EX1-USER-NAME          PIC X(30).
           12  EX1-TAB4               PIC X.
           12  EX1-USER-TITLE         PIC X(26).
           12  EX1-TAB5               PIC X.
           12  EX1-LAST-MAINT-DT      PIC X(10).
           12  EX1-TAB6               PIC X.
           12  EX1-CR-FORCE           PIC X.
           12  EX1-TAB7               PIC X.
           12  EX1-CL-FORCE           PIC X.
           12  EX1-TAB8               PIC X.
           12  EX1-EOR                PIC X.

       01  WS-EX-INIT-REC2            PIC X(44) VALUE LOW-VALUES.
       01  EX-USER-REC2.
           12  F                      PIC X.
           12  EX2-TAB1               PIC X.
           12  F                      PIC X.
           12  EX2-TAB2               PIC X.
           12  F                      PIC X.
           12  EX2-TAB3               PIC X.
           12  EX2-REC-TYPE           PIC X(6).
           12  EX2-TAB4               PIC X.
           12  EX2-SYS-DESC           PIC X(25).
           12  EX2-TAB5               PIC X.
           12  EX2-APP-B              PIC X.
           12  EX2-TAB6               PIC X.
           12  EX2-APP-U              PIC X.
           12  EX2-TAB7               PIC X.
           12  EX2-EOR                PIC X.



       01  CREDIT-DESCRIPT-TABLE.
           05  CR-001 PIC X(25) VALUE 'END USER REPORTING       '.
           05  CR-002 PIC X(25) VALUE 'PROGRAM OPTIONS          '.
           05  CR-003 PIC X(25) VALUE 'TEXT FILE (LETTERS FORMS)'.
           05  CR-004 PIC X(25) VALUE 'ACCOUNT MASTERS          '.
           05  CR-005 PIC X(25) VALUE 'COMPENSATION MASTERS     '.
           05  CR-006 PIC X(25) VALUE 'RATE MASTERS             '.
           05  CR-007 PIC X(25) VALUE 'REINSURANCE MASTERS      '.
           05  CR-008 PIC X(25) VALUE 'COMMISSION TABLES        '.
           05  CR-009 PIC X(25) VALUE 'MORTALITY TABLE CONTROLS '.
           05  CR-010 PIC X(25) VALUE 'LOAN OFFICERS            '.
           05  CR-011 PIC X(25) VALUE 'DATA ENTRY               '.
           05  CR-012 PIC X(25) VALUE 'REVIEW AND CORRECTION    '.
           05  CR-013 PIC X(25) VALUE 'FULL FILE EDIT           '.
           05  CR-014 PIC X(25) VALUE 'CLAIMS AND RESERVES      '.
           05  CR-015 PIC X(25) VALUE 'COMPENSATIONS (PYMT/ADJ) '.
           05  CR-016 PIC X(25) VALUE 'RETRO/REINS   (PYMT/ADJ) '.
           05  CR-017 PIC X(25) VALUE 'CHECK MAINTENANCE(CREDIT)'.
           05  CR-018 PIC X(25) VALUE 'ACCOUNT STATEMENTS       '.
           05  CR-019 PIC X(25) VALUE 'GENERAL AGENT STATEMENTS '.
           05  CR-020 PIC X(25) VALUE 'LOSS RATIO SELECTION     '.
           05  CR-021 PIC X(25) VALUE 'ONLINE STATEMENT PRINTING'.
           05  CR-022 PIC X(25) VALUE 'CHECKS TO PRINT  (CREDIT)'.
           05  CR-023 PIC X(25) VALUE 'CHECK RELEASE    (CREDIT)'.
           05  CR-024 PIC X(25) VALUE 'PRINT RELEASED CHECKS    '.
           05  CR-025 PIC X(25) VALUE 'ACCOUNT NOTEPAD          '.
           05  CR-026 PIC X(25) VALUE 'RETRO MASTER      (EL606)'.
           05  CR-027 PIC X(25) VALUE 'RETRO HISTORY     (EL607)'.
           05  CR-028 PIC X(25) VALUE 'BANK MASTER MAINTENANCE  '.
           05  CR-029 PIC X(25) VALUE 'STATE CONTROLS (EL106A)  '.
           05  CR-030 PIC X(25) VALUE 'REPORT CUSTOMIZATION 604A'.
           05  CR-031 PIC X(25) VALUE 'CERTIFICATE LOOK-UP      '.
           05  CR-032 PIC X(25) VALUE 'CERTIFICATE NOTES        '.
           05  CR-033 PIC X(25) VALUE 'CERTIFICATE CHANGES      '.
           05  CR-034 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CR-035 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CR-036 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CR-037 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CR-038 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CR-039 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CR-040 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CR-041 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CR-042 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CR-043 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CR-044 PIC X(25) VALUE '****** FUTURE USE  ******'.
       01  FILLER REDEFINES CREDIT-DESCRIPT-TABLE.
           05  FILLER OCCURS 44.
               10  CREDIT-DES          PIC X(25).


       01  CLAIM-DESCRIPT-TABLE.
           05  CL-001 PIC X(25) VALUE 'NEW CLAIM SETUP          '.
           05  CL-002 PIC X(25) VALUE 'RECORD MAIL RECEIVED     '.
           05  CL-003 PIC X(25) VALUE 'CLAIM AUDIT              '.
           05  CL-004 PIC X(25) VALUE 'BENEFICIARY MASTER       '.
           05  CL-005 PIC X(25) VALUE 'CLAIM MAINTENANCE        '.
           05  CL-006 PIC X(25) VALUE 'DENIAL PROCESSING        '.
           05  CL-007 PIC X(25) VALUE 'CLAIMS LETTER WRITER     '.
           05  CL-008 PIC X(25) VALUE 'NOTE / REMINDER RECORDING'.
           05  CL-009 PIC X(25) VALUE 'SETUP AUTOMATIC PAYMENT  '.
           05  CL-010 PIC X(25) VALUE 'PAYMENT PROCESSING       '.
           05  CL-011 PIC X(25) VALUE 'CHECKS TO PRINT  (CLAIMS)'.
           05  CL-012 PIC X(25) VALUE 'CHECK RELEASE    (CLAIMS)'.
           05  CL-013 PIC X(25) VALUE 'PRINT RELEASED CHECKS    '.
           05  CL-014 PIC X(25) VALUE 'CLAIM ADDRESS MAINTENANCE'.
           05  CL-015 PIC X(25) VALUE 'CLAIM TRAILER MAINTENANCE'.
           05  CL-016 PIC X(25) VALUE 'CLAIM STATUS/DISPOSITION '.
           05  CL-017 PIC X(25) VALUE 'SUPERVISOR REQUEST REPORT'.
           05  CL-018 PIC X(25) VALUE 'FILE FOLDER LABEL PRINT  '.
           05  CL-019 PIC X(25) VALUE 'CLAIMS STATUS PRINT      '.
           05  CL-020 PIC X(25) VALUE 'LETTER/ADDR LABEL PRINT  '.
           05  CL-021 PIC X(25) VALUE 'CLAIM LOOK-UP            '.
           05  CL-022 PIC X(25) VALUE 'REVIEW PENDING ACTIVITY  '.
           05  CL-023 PIC X(25) VALUE 'CHECK RECON      (EL146) '.
           05  CL-024 PIC X(25) VALUE 'POLICY FORM MSTR (EL1582)'.
           05  CL-025 PIC X(25) VALUE 'AUTO ACT MAINT   (EL145) '.
           05  CL-026 PIC X(25) VALUE 'INITIAL/PROG FORMS PRINT '.
           05  CL-027 PIC X(25) VALUE 'ONLINE REPORTS           '.
           05  CL-028 PIC X(25) VALUE 'PAYMENT APPROVAL         '.
           05  CL-029 PIC X(25) VALUE 'DENIAL MAINTENANCE       '.
           05  CL-030 PIC X(25) VALUE 'REJECT MAINTENANCE       '.
           05  CL-031 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CL-032 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CL-033 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CL-034 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CL-035 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CL-036 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CL-037 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CL-038 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CL-039 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CL-040 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CL-041 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CL-042 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CL-043 PIC X(25) VALUE '****** FUTURE USE  ******'.
           05  CL-044 PIC X(25) VALUE '****** FUTURE USE  ******'.
       01  FILLER REDEFINES CLAIM-DESCRIPT-TABLE.
           05  FILLER OCCURS 44.
               10  CLAIM-DES           PIC X(25).


      ******************************************************************
       01  WS-MISC.
           05  WS-CNTR                 PIC 9(4) VALUE ZEROS.
           05  ELCNTL-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.

                                       COPY ELCDATE.
062117                                 copy ELCFUNDT.
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.

       LINKAGE SECTION.                                                 

       01  var  pic x(30).


       PROCEDURE DIVISION.

                                       COPY ELCDTERX.

       0000-here-we-go.

           display ' Begin Program '

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

062117     move FUNCTION CURRENT-DATE  TO FUNCTION-DATE
062117     display ' function date = ' function-date
062117
062117     string
062117        ws-fn-ccyr '-'
062117        ws-fn-mo   '-'
062117        ws-fn-da   '  '
062117        ws-fn-hours ':'
062117        ws-fn-minutes ':'
062117        ws-fn-seconds '.000'
062117           delimited by size into ws-sql-date-time
062117     end-string
062117
062117     display ' sql date time ' ws-sql-date-time

           perform 0100-open-files     thru 0100-exit

           PERFORM 0110-INITIALIZE     THRU 0110-EXIT

           PERFORM 0500-PROCESS-ELCNTL THRU 0500-EXIT UNTIL
              (END-OF-ELCNTL)
PEMTST*       OR (CLM-RECS-IN > 1000)

      *    MOVE WS-EX2-INIT            TO EX-USER-REC
      *    MOVE 'TOTAL COUNT '         TO EX2-USER-NAME
      *    MOVE WS-CNTR                TO EX2-CNT
      *    PERFORM 0300-WRITE-ELCNTL   THRU 0300-EXIT

           perform 0600-finish-up thru 0600-exit
           PERFORM 5000-CLOSE-FILES    THRU 5000-EXIT

           GOBACK

           .
       0100-OPEN-FILES.

           OPEN INPUT ELCNTL

           if elcntl-file-status not = '00' and '97'
              display ' Error-ELCNTL-Open ' elcntl-file-status
              perform abend-pgm
           end-if

           .
       0100-EXIT.
           EXIT.

       0110-INITIALIZE.

           move 'Processor'            to ws-proc-table
           move 'ProcClaimApps'        to ws-claim-table
           move 'ProcCreditApps'       to ws-credit-table

           if dte-client = 'DCC'
              move 'DCC_Processor'      to ws-proc-table
              move 'DCC_ProcClaimApps'  to ws-claim-table
              move 'DCC_ProcCreditApps' to ws-credit-table
062117     else
062117        if dte-client = 'VPP'
062117           move 'VPP_Processor'  to ws-proc-table
062117           move 'VPP_ProcClaimApps'
062117                                 to ws-claim-table
062117           move 'VPP_ProcCreditApps'
062117                                 to ws-credit-table
070221        else
070221           if dte-client = 'FNL'
070221              move 'FNL_Processor'
070221                                 to ws-proc-table
070221              move 'FNL_ProcClaimApps'
070221                                 to ws-claim-table
070221              move 'FNL_ProcCreditApps'
070221                                 to ws-credit-table
070221           end-if
062117        end-if
           end-if

           perform 0120-connect-to-db  thru 0120-exit
           perform 0130-truncate-table thru 0130-exit

           PERFORM 0140-START-ELCNTL   THRU 0140-EXIT
           PERFORM 0150-READ-ELCNTL    THRU 0150-EXIT

           .
       0110-EXIT.
           EXIT.

       0120-connect-to-db.

           display ' about to connect to Logic '

           if dte-client = 'AHL' OR 'FNL'
              move 'NTSQLTST2_Logic_TPA'
                                       to svr
              move 'appuser'           to usr
              move 'appuser@cso'       to pass
           else
              move 'NTSQLTST2_Logic'   to svr
              move 'appuser'           to usr
              move 'appuser@cso'       to pass
           end-if

           if ws-kix-myenv = 'cid1p'
              if dte-client = 'AHL' OR 'FNL'
                 move 'NTCSO2_Logic_TPA'
                                       to svr
                 move 'appuser'        to usr
                 move 'appuser@cso'    to pass
              else
                 move 'NTCSO2_Logic'   to svr
                 move 'appuser'        to usr
                 move 'appuser@cso'    to pass
              end-if
           end-if

           string
               usr delimited space
               "." delimited size
               pass delimited space into usr-pass
           end-string

           EXEC SQL
              SET OPTION logintime 5
           END-EXEC

           EXEC SQL
              CONNECT TO :svr
                    USER :usr-pass
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot connect to " svr
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display sqlerrmc
              perform abend-pgm
           end-if

           .
       0120-exit.
           exit.

       0130-truncate-table.

           display ' Begin Truncate Processor table '
           move spaces to sqlcmd
           string
              ' truncate table '
              ws-proc-table
              delimited by size into sqlcmd
           end-string

           EXEC SQL
               execute immediate :sqlcmd
           END-EXEC

           if sqlcode not = 0
              display "Error : cannot truncate table - " ws-proc-table
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' message ' sqlerrmc
           end-if

           display ' Begin Truncate Claim Apps '

           move spaces to sqlcmd
           string
              ' truncate table '
              ws-claim-table
              delimited by size into sqlcmd
           end-string

           EXEC SQL
               execute immediate :sqlcmd
           END-EXEC

           if sqlcode not = 0
              display "Error : cannot truncate table - " ws-claim-table
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' message ' sqlerrmc
           end-if

           display ' Begin Truncate Credit Apps '

           move spaces to sqlcmd
           string
              ' truncate table '
              ws-credit-table
              delimited by size into sqlcmd
           end-string

           EXEC SQL
               execute immediate :sqlcmd
           END-EXEC

           if sqlcode not = 0
              display "Error : cannot truncate table - " ws-credit-table
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' message ' sqlerrmc
           end-if

           .
       0130-exit.
           exit.

       0140-START-ELCNTL.

           MOVE LOW-VALUES             TO CF-CONTROL-PRIMARY
           MOVE DTE-CLIENT             TO CF-COMPANY-ID
           MOVE '2'                    TO CF-RECORD-TYPE
           START ELCNTL KEY >= CF-CONTROL-PRIMARY

           IF ELCNTL-FILE-STATUS = '10' OR '23'
              SET END-OF-ELCNTL        TO TRUE
           ELSE
              IF ELCNTL-FILE-STATUS NOT = '00'
                 DISPLAY 'ELCNTL START     ' ELCNTL-FILE-STATUS
                 SET END-OF-ELCNTL     TO TRUE
              END-IF
           END-IF

           .
       0140-EXIT.
           EXIT.

       0150-READ-ELCNTL.

           READ ELCNTL NEXT RECORD

           IF (ELCNTL-FILE-STATUS = '10' OR '23')
              or (cf-company-id <> dte-client)
              or (cf-record-type <> '2')
              SET END-OF-ELCNTL        TO TRUE
           ELSE
              IF ELCNTL-FILE-STATUS NOT = '00'
                 DISPLAY 'ELCNTL READ NEXT ' ELCNTL-FILE-STATUS
                 SET END-OF-ELCNTL     TO TRUE
              END-IF
           END-IF

           IF NOT END-OF-ELCNTL
              ADD 1 TO ELCNTL-RECS-IN
           END-IF

           .
       0150-EXIT.
           EXIT.


       0500-PROCESS-ELCNTL.

           if cf-record-type = '2'
091219        if cf-sequence-no = zeros
091219           perform 0510-processed
091219                                 thru 0510-exit
091219        else
091219           perform 0515-update-create-dt
091219                                 thru 0515-exit
091219        end-if
           end-if

           PERFORM 0150-READ-ELCNTL    THRU 0150-EXIT

           .
       0500-EXIT.
           EXIT.

       0510-processed.

           perform 0520-build-proc     thru 0520-exit

           .
       0510-exit.
           exit.

091219 0515-update-create-dt.
091219
091219     move cf-last-maint-dt       to dc-bin-date-1
091219     move ' '                    to dc-option-code
091219     perform 8510-date-conversion
091219                                 thru 8590-exit
091219     if not no-conversion-error
091219        go to 0515-exit
091219     end-if
091219
091219     move spaces to sqlcmd
091219     string
091219        'UPDATE '
091219        ws-proc-table
091219        ' set CreateDate = '
091219        "'" dc-greg-date-a-edit "'"
091219        ' where UserCd = '
091219        "'" cf-processor "'"
091219        delimited by size into sqlcmd
091219     end-string
091219
091219     EXEC SQL
091219         execute immediate :sqlcmd
091219     END-EXEC
091219
091219     if sqlcode not = 0
091219        display "Error: cannot update row " ws-proc-table
091219        move sqlcode             to ws-sql-code
091219        move ws-sql-code         to ws-dis-sql-code
091219        display ' sqlcode ' ws-dis-sql-code
091219        display ' sql err mess    ' sqlerrmc
091219        display ' offending rec ' cf-processor ' '
091219           dc-greg-date-a-edit
091219     end-if
091219
091219     .
091219 0515-exit.
091219     exit.

       0520-build-proc.

           move spaces                 to processor-table
102717     move zeros                  to ws-hyphen-ctr
           inspect cf-processor-name
              replacing all ',' by ' '
           inspect cf-processor-title
              replacing all ',' by ' '
102717     inspect cf-processor-name tallying ws-hyphen-ctr 
102717        for all "'"
102717     if ws-hyphen-ctr > 0
102717        move spaces              to ws-work-processor
102717        move +1                  to o1
102717        perform varying i1 from +1 by +1 until i1 > +30
102717           if cf-processor-name (i1:1) = "'"
102717              move "'"           to ws-work-processor (o1:1)
102717              add +1             to o1
102717           end-if
102717           move cf-processor-name (i1:1)
102717                                 to ws-work-processor (o1:1)
102717           add +1                to o1
102717        end-perform
102717        move ws-work-processor   to cf-processor-name
102717     end-if
           move cf-processor           to pt-procid
           move cf-processor-name      to pt-name
           move cf-processor-title     to pt-title
           move cf-proc-sys-access-credit
                                       to pt-credit-access
           move cf-proc-sys-access-claims
                                       to pt-claim-access
102717     move cf-processor-user-almighty
102717                                 to pt-security-off
           move cf-csr-ind             to pt-csr-supr
           move cf-administration-controls (1)
                                       to pt-crd-sys-cntrls
           move cf-administration-controls (2)
                                       to pt-clm-sys-cntrls
           move cf-application-force (1)
                                       to pt-crd-force-cap
           move cf-application-force (2)
                                       to pt-clm-force-cap
           move cf-proc-calc-amt-tol   to pt-tol-n
           move cf-proc-max-reg-pmt    to pt-max-reg-pmt-n
           move cf-proc-max-reg-days   to pt-max-reg-days
           move cf-proc-max-auto-pmt   to pt-max-auto-pmt-n
           move cf-proc-max-auto-mos   to pt-max-auto-mos
           move cf-proc-calc-days-tol  to pt-days-tol
           move cf-proc-max-lf-pmt     to pt-max-lf-pmt-n
           move cf-approval-level      to pt-approval-lvl
           move cf-proc-max-exp-pmt    to pt-max-exp-pmt-n

           move spaces                 to sqlcmd

           string
              'INSERT INTO '
               ws-proc-table
               ' (UserCd, Name, Title, CreditAdmin, ClaimsAdmin,'
               ' ApprovalLevel,'
               ' SecurityOfficer, CSR_Supervisor, CRSysControls,'
               ' CRForceCap, ClmSysControls, ClmForceCap, DiffInAHAmt,'
               ' MaxRegDays, MaxRegAHPmt, MaxRegLfPmt, MaxAutoMonths,'
               ' MaxAutoPmt, DiffInDays,'
               ' MaxExpPmt)'
              ' VALUES ('
                "'" pt-procid "', '"
                    pt-name "', '"
                    pt-title "', '"
                    pt-credit-access "', '"
                    pt-claim-access "', '"
                    pt-approval-lvl "', '"
                    pt-security-off "', '"
                    pt-csr-supr "', '"
                    pt-crd-sys-cntrls "', '"
                    pt-crd-force-cap "', '"
                    pt-clm-sys-cntrls "', '"
                    pt-clm-force-cap "', '"
                    pt-tol "', '"
                    pt-max-reg-days "', '"
                    pt-max-reg-pmt "', '"
                    pt-max-lf-pmt "', '"
                    pt-max-auto-mos "', '"
                    pt-max-auto-pmt "', '"
                    pt-days-tol "', '"
                    pt-max-exp-pmt "')"
           delimited by size into sqlcmd
           end-string

           display ' sql command = ' sqlcmd
           

           EXEC SQL
               execute immediate :sqlcmd
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot insert row " ws-proc-table
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' sql err mess    ' sqlerrmc
              display ' offending rec ' processor-table
062117     else
062117        add 1 to ws-records-inserted
           end-if

           move cf-processor           to cr-procid
           perform varying c1 from +1 by +1 until c1 > +44
              move c1                  to cr-credit-app (c1)
              move credit-des  (c1)    to cr-app-desc (c1)
              move cf-browse-app (1 c1)
                                       to cr-browse-cap (c1)
              move cf-update-app (1 c1)
                                       to cr-update-cap (c1)
           end-perform

           move spaces                 to sqlcmd

           string
                 'INSERT INTO '
                  ws-credit-table
                  ' (UserCd,'
                  ' App01, Desc01, Browse01, Update01, '
                  ' App02, Desc02, Browse02, Update02, '
                  ' App03, Desc03, Browse03, Update03, '
                  ' App04, Desc04, Browse04, Update04, '
                  ' App05, Desc05, Browse05, Update05, '
                  ' App06, Desc06, Browse06, Update06, '
                  ' App07, Desc07, Browse07, Update07, '
                  ' App08, Desc08, Browse08, Update08, '
                  ' App09, Desc09, Browse09, Update09, '
                  ' App10, Desc10, Browse10, Update10, '
                  ' App11, Desc11, Browse11, Update11, '
                  ' App12, Desc12, Browse12, Update12, '
                  ' App13, Desc13, Browse13, Update13, '
                  ' App14, Desc14, Browse14, Update14, '
                  ' App15, Desc15, Browse15, Update15, '
                  ' App16, Desc16, Browse16, Update16, '
                  ' App17, Desc17, Browse17, Update17, '
                  ' App18, Desc18, Browse18, Update18, '
                  ' App19, Desc19, Browse19, Update19, '
                  ' App20, Desc20, Browse20, Update20, '
                  ' App21, Desc21, Browse21, Update21, '
                  ' App22, Desc22, Browse22, Update22, '
                  ' App23, Desc23, Browse23, Update23, '
                  ' App24, Desc24, Browse24, Update24, '
                  ' App25, Desc25, Browse25, Update25, '
                  ' App26, Desc26, Browse26, Update26, '
                  ' App27, Desc27, Browse27, Update27, '
                  ' App28, Desc28, Browse28, Update28, '
                  ' App29, Desc29, Browse29, Update29, '
                  ' App30, Desc30, Browse30, Update30, '
                  ' App31, Desc31, Browse31, Update31, '
                  ' App32, Desc32, Browse32, Update32, '
                  ' App33, Desc33, Browse33, Update33, '
                  ' App34, Desc34, Browse34, Update34, '
                  ' App35, Desc35, Browse35, Update35, '
                  ' App36, Desc36, Browse36, Update36, '
                  ' App37, Desc37, Browse37, Update37, '
                  ' App38, Desc38, Browse38, Update38, '
                  ' App39, Desc39, Browse39, Update39, '
                  ' App40, Desc40, Browse40, Update40, '
                  ' App41, Desc41, Browse41, Update41, '
                  ' App42, Desc42, Browse42, Update42, '
                  ' App43, Desc43, Browse43, Update43, '
                  ' App44, Desc44, Browse44, Update44)'

              ' VALUES ('
                "'" cr-procid "', '"
                  cr-credit-app(01) "', '" cr-app-desc(01) "', '"
                  cr-browse-cap(01) "', '" cr-update-cap(01) "', '"
                  cr-credit-app(02) "', '" cr-app-desc(02) "', '"
                  cr-browse-cap(02) "', '" cr-update-cap(02) "', '"
                  cr-credit-app(03) "', '" cr-app-desc(03) "', '"
                  cr-browse-cap(03) "', '" cr-update-cap(03) "', '"
                  cr-credit-app(04) "', '" cr-app-desc(04) "', '"
                  cr-browse-cap(04) "', '" cr-update-cap(04) "', '"
                  cr-credit-app(05) "', '" cr-app-desc(05) "', '"
                  cr-browse-cap(05) "', '" cr-update-cap(05) "', '"
                  cr-credit-app(06) "', '" cr-app-desc(06) "', '"
                  cr-browse-cap(06) "', '" cr-update-cap(06) "', '"
                  cr-credit-app(07) "', '" cr-app-desc(07) "', '"
                  cr-browse-cap(07) "', '" cr-update-cap(07) "', '"
                  cr-credit-app(08) "', '" cr-app-desc(08) "', '"
                  cr-browse-cap(08) "', '" cr-update-cap(08) "', '"
                  cr-credit-app(09) "', '" cr-app-desc(09) "', '"
                  cr-browse-cap(09) "', '" cr-update-cap(09) "', '"
                  cr-credit-app(10) "', '" cr-app-desc(10) "', '"
                  cr-browse-cap(10) "', '" cr-update-cap(10) "', '"
                  cr-credit-app(11) "', '" cr-app-desc(11) "', '"
                  cr-browse-cap(11) "', '" cr-update-cap(11) "', '"
                  cr-credit-app(12) "', '" cr-app-desc(12) "', '"
                  cr-browse-cap(12) "', '" cr-update-cap(12) "', '"
                  cr-credit-app(13) "', '" cr-app-desc(13) "', '"
                  cr-browse-cap(13) "', '" cr-update-cap(13) "', '"
                  cr-credit-app(14) "', '" cr-app-desc(14) "', '"
                  cr-browse-cap(14) "', '" cr-update-cap(14) "', '"
                  cr-credit-app(15) "', '" cr-app-desc(15) "', '"
                  cr-browse-cap(15) "', '" cr-update-cap(15) "', '"
                  cr-credit-app(16) "', '" cr-app-desc(16) "', '"
                  cr-browse-cap(16) "', '" cr-update-cap(16) "', '"
                  cr-credit-app(17) "', '" cr-app-desc(17) "', '"
                  cr-browse-cap(17) "', '" cr-update-cap(17) "', '"
                  cr-credit-app(18) "', '" cr-app-desc(18) "', '"
                  cr-browse-cap(18) "', '" cr-update-cap(18) "', '"
                  cr-credit-app(19) "', '" cr-app-desc(19) "', '"
                  cr-browse-cap(19) "', '" cr-update-cap(19) "', '"
                  cr-credit-app(20) "', '" cr-app-desc(20) "', '"
                  cr-browse-cap(20) "', '" cr-update-cap(20) "', '"
                  cr-credit-app(21) "', '" cr-app-desc(21) "', '"
                  cr-browse-cap(21) "', '" cr-update-cap(21) "', '"
                  cr-credit-app(22) "', '" cr-app-desc(22) "', '"
                  cr-browse-cap(22) "', '" cr-update-cap(22) "', '"
                  cr-credit-app(23) "', '" cr-app-desc(23) "', '"
                  cr-browse-cap(23) "', '" cr-update-cap(23) "', '"
                  cr-credit-app(24) "', '" cr-app-desc(24) "', '"
                  cr-browse-cap(24) "', '" cr-update-cap(24) "', '"
                  cr-credit-app(25) "', '" cr-app-desc(25) "', '"
                  cr-browse-cap(25) "', '" cr-update-cap(25) "', '"
                  cr-credit-app(26) "', '" cr-app-desc(26) "', '"
                  cr-browse-cap(26) "', '" cr-update-cap(26) "', '"
                  cr-credit-app(27) "', '" cr-app-desc(27) "', '"
                  cr-browse-cap(27) "', '" cr-update-cap(27) "', '"
                  cr-credit-app(28) "', '" cr-app-desc(28) "', '"
                  cr-browse-cap(28) "', '" cr-update-cap(28) "', '"
                  cr-credit-app(29) "', '" cr-app-desc(29) "', '"
                  cr-browse-cap(29) "', '" cr-update-cap(29) "', '"
                  cr-credit-app(30) "', '" cr-app-desc(30) "', '"
                  cr-browse-cap(30) "', '" cr-update-cap(30) "', '"
                  cr-credit-app(31) "', '" cr-app-desc(31) "', '"
                  cr-browse-cap(31) "', '" cr-update-cap(31) "', '"
                  cr-credit-app(32) "', '" cr-app-desc(32) "', '"
                  cr-browse-cap(32) "', '" cr-update-cap(32) "', '"
                  cr-credit-app(33) "', '" cr-app-desc(33) "', '"
                  cr-browse-cap(33) "', '" cr-update-cap(33) "', '"
                  cr-credit-app(34) "', '" cr-app-desc(34) "', '"
                  cr-browse-cap(34) "', '" cr-update-cap(34) "', '"
                  cr-credit-app(35) "', '" cr-app-desc(35) "', '"
                  cr-browse-cap(35) "', '" cr-update-cap(35) "', '"
                  cr-credit-app(36) "', '" cr-app-desc(36) "', '"
                  cr-browse-cap(36) "', '" cr-update-cap(36) "', '"
                  cr-credit-app(37) "', '" cr-app-desc(37) "', '"
                  cr-browse-cap(37) "', '" cr-update-cap(37) "', '"
                  cr-credit-app(38) "', '" cr-app-desc(38) "', '"
                  cr-browse-cap(38) "', '" cr-update-cap(38) "', '"
                  cr-credit-app(39) "', '" cr-app-desc(39) "', '"
                  cr-browse-cap(39) "', '" cr-update-cap(39) "', '"
                  cr-credit-app(40) "', '" cr-app-desc(40) "', '"
                  cr-browse-cap(40) "', '" cr-update-cap(40) "', '"
                  cr-credit-app(41) "', '" cr-app-desc(41) "', '"
                  cr-browse-cap(41) "', '" cr-update-cap(41) "', '"
                  cr-credit-app(42) "', '" cr-app-desc(42) "', '"
                  cr-browse-cap(42) "', '" cr-update-cap(42) "', '"
                  cr-credit-app(43) "', '" cr-app-desc(43) "', '"
                  cr-browse-cap(43) "', '" cr-update-cap(43) "', '"
                  cr-credit-app(44) "', '" cr-app-desc(44) "', '"
                  cr-browse-cap(44) "', '" cr-update-cap(44) "')"
              delimited by size into sqlcmd
           end-string

           display ' sql crd cmd ' sqlcmd

           EXEC SQL
               execute immediate :sqlcmd
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot insert row " ws-credit-table
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' sql err mess    ' sqlerrmc
              display ' offending rec ' credit-table
           end-if

           move cf-processor           to cl-procid
           perform varying c1 from +1 by +1 until c1 > +44
              move c1                  to cl-claim-app (c1)
              move claim-des  (c1)     to cl-app-desc (c1)
              move cf-browse-app (2 c1)
                                       to cl-browse-cap (c1)
              move cf-update-app (2 c1)
                                       to cl-update-cap (c1)
           end-perform

           move spaces                 to sqlcmd

           string
                 'INSERT INTO '
                  ws-claim-table
                  ' (UserCd,'
                  ' App01, Desc01, Browse01, Update01, '
                  ' App02, Desc02, Browse02, Update02, '
                  ' App03, Desc03, Browse03, Update03, '
                  ' App04, Desc04, Browse04, Update04, '
                  ' App05, Desc05, Browse05, Update05, '
                  ' App06, Desc06, Browse06, Update06, '
                  ' App07, Desc07, Browse07, Update07, '
                  ' App08, Desc08, Browse08, Update08, '
                  ' App09, Desc09, Browse09, Update09, '
                  ' App10, Desc10, Browse10, Update10, '
                  ' App11, Desc11, Browse11, Update11, '
                  ' App12, Desc12, Browse12, Update12, '
                  ' App13, Desc13, Browse13, Update13, '
                  ' App14, Desc14, Browse14, Update14, '
                  ' App15, Desc15, Browse15, Update15, '
                  ' App16, Desc16, Browse16, Update16, '
                  ' App17, Desc17, Browse17, Update17, '
                  ' App18, Desc18, Browse18, Update18, '
                  ' App19, Desc19, Browse19, Update19, '
                  ' App20, Desc20, Browse20, Update20, '
                  ' App21, Desc21, Browse21, Update21, '
                  ' App22, Desc22, Browse22, Update22, '
                  ' App23, Desc23, Browse23, Update23, '
                  ' App24, Desc24, Browse24, Update24, '
                  ' App25, Desc25, Browse25, Update25, '
                  ' App26, Desc26, Browse26, Update26, '
                  ' App27, Desc27, Browse27, Update27, '
                  ' App28, Desc28, Browse28, Update28, '
                  ' App29, Desc29, Browse29, Update29, '
                  ' App30, Desc30, Browse30, Update30, '
                  ' App31, Desc31, Browse31, Update31, '
                  ' App32, Desc32, Browse32, Update32, '
                  ' App33, Desc33, Browse33, Update33, '
                  ' App34, Desc34, Browse34, Update34, '
                  ' App35, Desc35, Browse35, Update35, '
                  ' App36, Desc36, Browse36, Update36, '
                  ' App37, Desc37, Browse37, Update37, '
                  ' App38, Desc38, Browse38, Update38, '
                  ' App39, Desc39, Browse39, Update39, '
                  ' App40, Desc40, Browse40, Update40, '
                  ' App41, Desc41, Browse41, Update41, '
                  ' App42, Desc42, Browse42, Update42, '
                  ' App43, Desc43, Browse43, Update43, '
                  ' App44, Desc44, Browse44, Update44)'

              ' VALUES ('
                "'" cl-procid "', '"
                  cl-claim-app(01) "', '" cl-app-desc(01) "', '"
                  cl-browse-cap(01) "', '" cl-update-cap(01) "', '"
                  cl-claim-app(02) "', '" cl-app-desc(02) "', '"
                  cl-browse-cap(02) "', '" cl-update-cap(02) "', '"
                  cl-claim-app(03) "', '" cl-app-desc(03) "', '"
                  cl-browse-cap(03) "', '" cl-update-cap(03) "', '"
                  cl-claim-app(04) "', '" cl-app-desc(04) "', '"
                  cl-browse-cap(04) "', '" cl-update-cap(04) "', '"
                  cl-claim-app(05) "', '" cl-app-desc(05) "', '"
                  cl-browse-cap(05) "', '" cl-update-cap(05) "', '"
                  cl-claim-app(06) "', '" cl-app-desc(06) "', '"
                  cl-browse-cap(06) "', '" cl-update-cap(06) "', '"
                  cl-claim-app(07) "', '" cl-app-desc(07) "', '"
                  cl-browse-cap(07) "', '" cl-update-cap(07) "', '"
                  cl-claim-app(08) "', '" cl-app-desc(08) "', '"
                  cl-browse-cap(08) "', '" cl-update-cap(08) "', '"
                  cl-claim-app(09) "', '" cl-app-desc(09) "', '"
                  cl-browse-cap(09) "', '" cl-update-cap(09) "', '"
                  cl-claim-app(10) "', '" cl-app-desc(10) "', '"
                  cl-browse-cap(10) "', '" cl-update-cap(10) "', '"
                  cl-claim-app(11) "', '" cl-app-desc(11) "', '"
                  cl-browse-cap(11) "', '" cl-update-cap(11) "', '"
                  cl-claim-app(12) "', '" cl-app-desc(12) "', '"
                  cl-browse-cap(12) "', '" cl-update-cap(12) "', '"
                  cl-claim-app(13) "', '" cl-app-desc(13) "', '"
                  cl-browse-cap(13) "', '" cl-update-cap(13) "', '"
                  cl-claim-app(14) "', '" cl-app-desc(14) "', '"
                  cl-browse-cap(14) "', '" cl-update-cap(14) "', '"
                  cl-claim-app(15) "', '" cl-app-desc(15) "', '"
                  cl-browse-cap(15) "', '" cl-update-cap(15) "', '"
                  cl-claim-app(16) "', '" cl-app-desc(16) "', '"
                  cl-browse-cap(16) "', '" cl-update-cap(16) "', '"
                  cl-claim-app(17) "', '" cl-app-desc(17) "', '"
                  cl-browse-cap(17) "', '" cl-update-cap(17) "', '"
                  cl-claim-app(18) "', '" cl-app-desc(18) "', '"
                  cl-browse-cap(18) "', '" cl-update-cap(18) "', '"
                  cl-claim-app(19) "', '" cl-app-desc(19) "', '"
                  cl-browse-cap(19) "', '" cl-update-cap(19) "', '"
                  cl-claim-app(20) "', '" cl-app-desc(20) "', '"
                  cl-browse-cap(20) "', '" cl-update-cap(20) "', '"
                  cl-claim-app(21) "', '" cl-app-desc(21) "', '"
                  cl-browse-cap(21) "', '" cl-update-cap(21) "', '"
                  cl-claim-app(22) "', '" cl-app-desc(22) "', '"
                  cl-browse-cap(22) "', '" cl-update-cap(22) "', '"
                  cl-claim-app(23) "', '" cl-app-desc(23) "', '"
                  cl-browse-cap(23) "', '" cl-update-cap(23) "', '"
                  cl-claim-app(24) "', '" cl-app-desc(24) "', '"
                  cl-browse-cap(24) "', '" cl-update-cap(24) "', '"
                  cl-claim-app(25) "', '" cl-app-desc(25) "', '"
                  cl-browse-cap(25) "', '" cl-update-cap(25) "', '"
                  cl-claim-app(26) "', '" cl-app-desc(26) "', '"
                  cl-browse-cap(26) "', '" cl-update-cap(26) "', '"
                  cl-claim-app(27) "', '" cl-app-desc(27) "', '"
                  cl-browse-cap(27) "', '" cl-update-cap(27) "', '"
                  cl-claim-app(28) "', '" cl-app-desc(28) "', '"
                  cl-browse-cap(28) "', '" cl-update-cap(28) "', '"
                  cl-claim-app(29) "', '" cl-app-desc(29) "', '"
                  cl-browse-cap(29) "', '" cl-update-cap(29) "', '"
                  cl-claim-app(30) "', '" cl-app-desc(30) "', '"
                  cl-browse-cap(30) "', '" cl-update-cap(30) "', '"
                  cl-claim-app(31) "', '" cl-app-desc(31) "', '"
                  cl-browse-cap(31) "', '" cl-update-cap(31) "', '"
                  cl-claim-app(32) "', '" cl-app-desc(32) "', '"
                  cl-browse-cap(32) "', '" cl-update-cap(32) "', '"
                  cl-claim-app(33) "', '" cl-app-desc(33) "', '"
                  cl-browse-cap(33) "', '" cl-update-cap(33) "', '"
                  cl-claim-app(34) "', '" cl-app-desc(34) "', '"
                  cl-browse-cap(34) "', '" cl-update-cap(34) "', '"
                  cl-claim-app(35) "', '" cl-app-desc(35) "', '"
                  cl-browse-cap(35) "', '" cl-update-cap(35) "', '"
                  cl-claim-app(36) "', '" cl-app-desc(36) "', '"
                  cl-browse-cap(36) "', '" cl-update-cap(36) "', '"
                  cl-claim-app(37) "', '" cl-app-desc(37) "', '"
                  cl-browse-cap(37) "', '" cl-update-cap(37) "', '"
                  cl-claim-app(38) "', '" cl-app-desc(38) "', '"
                  cl-browse-cap(38) "', '" cl-update-cap(38) "', '"
                  cl-claim-app(39) "', '" cl-app-desc(39) "', '"
                  cl-browse-cap(39) "', '" cl-update-cap(39) "', '"
                  cl-claim-app(40) "', '" cl-app-desc(40) "', '"
                  cl-browse-cap(40) "', '" cl-update-cap(40) "', '"
                  cl-claim-app(41) "', '" cl-app-desc(41) "', '"
                  cl-browse-cap(41) "', '" cl-update-cap(41) "', '"
                  cl-claim-app(42) "', '" cl-app-desc(42) "', '"
                  cl-browse-cap(42) "', '" cl-update-cap(42) "', '"
                  cl-claim-app(43) "', '" cl-app-desc(43) "', '"
                  cl-browse-cap(43) "', '" cl-update-cap(43) "', '"
                  cl-claim-app(44) "', '" cl-app-desc(44) "', '"
                  cl-browse-cap(44) "', '" cl-update-cap(44) "')"
              delimited by size into sqlcmd
           end-string

           display ' sql clm cmd ' sqlcmd

           EXEC SQL
               execute immediate :sqlcmd
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot insert row " ws-claim-table
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' sql err mess    ' sqlerrmc
              display ' offending rec ' claim-table
           end-if

           .
       0520-exit.
           exit.

       0600-finish-up.

062117    move ws-proc-table           to ws-up-table-name
062117    move 'ELCNTL'                to ws-up-file-name
062117    move '/data/vsam'            to ws-up-full-path
062117    move ws-records-inserted     to ws-up-rec-count
062117    move ws-sql-date-time        to ws-up-create-date
062117                                    ws-up-modify-date
062117
062117    display ' ws table name ' ws-up-table-name
062117    display ' file name     ' ws-up-file-name
062117    display ' create date   ' ws-up-create-date
062117    display ' modify date   ' ws-up-modify-date
062117    display ' full path     ' ws-up-full-path
062117    display ' record count  ' ws-up-rec-count
062117
062117    EXEC SQL
062117       CALL logic_Insert_FilesUploadedData
062117                @table_name          = :ws-up-table-name,
062117                @file_name           = :ws-up-file-name,
062117                @file_created_date   = :ws-up-create-date,
062117                @file_modified_date  = :ws-up-modify-date,
062117                @file_full_path      = :ws-up-full-path,
062117                @loaded_record_count = :ws-up-rec-count
062117    END-EXEC
062117
062117     IF SQLCODE NOT = 0
062117        DISPLAY "ERROR: DID NOT update upload info "
062117        DISPLAY ' SQL RETURN CODE ' SQLCODE
062117        DISPLAY ' SQL ERR MESS    ' SQLERRMC
062117     END-IF

           EXEC SQL
               commit work release
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
       0600-exit.
           exit.

       5000-CLOSE-FILES.

           CLOSE ELCNTL

           .
       5000-EXIT.
           EXIT.


       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.

       ABEND-PGM.
                                       COPY ELCABEND.
