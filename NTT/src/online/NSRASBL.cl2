       IDENTIFICATION DIVISION.                                         

       PROGRAM-ID.   NSRASBL.

      *AUTHOR.     PABLO
      *            COLLEYVILLE, TEXAS.                                       

      *REMARKS.    EXECUTED FROM NSRASLTR

      ******************************************************************
      *                   C H A N G E   L O G
      *
      * ChangeS ARE MARKED BY THE Change EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  Change   Change REQUEST PGMR  DESCRIPTION OF Change
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 060611    2011022800001  PEMA  NEW PROGRAM
122911* 122911    2011022800001  AJRA  ADD VA DISCLOSURE TO CERT PRINT 
071212* 071212    2011022800001  AJRA  ADD UNDW ID
072312* 072312    2011022800001  AJRA	 ADD NEW FIELDS
090612* 090612  IR2012090600001  AJRA  NAPERSOFT
091112* 091112  IR2012091100002  AJRA  FIX AKA
091312* 091312  IR2012091100004  AJRA  FIX JOINT NAME
092412* 092412  IR2012092350004  AJRA  FIX CANCEL AMT
100112* 100112  IR2012100100002  AJRA  FIX ADDRESS WITH 2 SPACES
100412* 100412  IR2012100300002  AJRA  FIX VA DISC FOR DISAB ONLY CERT
101412* 101412    2011022800001  AJRA  POST IMPLEMENTATION ITEMS
101812* 101812    2012101700002  AJRA  READ ENDT BY ARCHIVE NO
110612* 110612    2012101700002  AJRA  ADD NEW FIELDS
121112* 121112    2012101700002  AJRA  HANDLE .01 PREM AND 9999999 BEN
121712* 121712    2012101700002  AJRA  ADD DEFAULT AGE FLAG
050213* 050213    2013050100001  AJRA  FIX AH REFUND ON DEATH CLAIM
091213* 091213    2013090300001  AJRA  NAPERSOFT PHASE 2
123113* 123113    2013090300001  AJRA  USE NEXT BUS DT FOR RESEND DT CALC
011314* 011314  IR2014011300001  AJRA  FIX NEXT BUS DT AND ENDORSEMENT AGE
011514* 011514    2013090300001  AJRA  CALL SQL STORED PROC FOR NEXT BUS DT
021214* 021214  IR2014021200002  AJRA  ADD SQL DISCONNECT 
121015* 121015  CR2015100900001  TANA  ADD VIN NUMBER
020916* 020916  CR2016010700001  TANA  CALL SQL LOOKUP NAPERSOFT TABLE
062017* 062017  CR2015091000001  PEMA  ADD PROCESSING FOR TN REF INTEREST
061821* 061821  CR2017031500001  TANA  FIX LOOKUP TABLE READ FOR CCM8
070622* 070622  CR2020061200002  TANA  ADD CANCEL REAS, PAYEE, CHK AMT
063022* 063022  CR2019012500003  PEMA  Migrate DB's to SQLSERVER 2016
080522* 080522  CR2019120300001  PEMA  Prevent zero UE Commission.
      ******************************************************************
       ENVIRONMENT DIVISION.                                            

       DATA DIVISION.                                                   

       working-storage section.
       
       77  SAVE-DATE                   PIC X(8)    VALUE SPACES.
       77  WS-SAVE-EDIT-A-DATE         PIC X(10)   VALUE SPACES.
       77  SAVE-BIN-DATE               PIC XX      VALUE SPACES.
123113 77  SAVE-BIN-NEXT-BUS-DT        PIC XX      VALUE SPACES.
011514 77  SAVE-NEXT-BUS-DT-EDIT-A     PIC X(10)   VALUE SPACES.
011514 77  SAVE-CYCLE-DAY-OF-WEEK      PIC S9   COMP-3 VALUE +0.
       77  S1                          PIC S999 COMP-3 VALUE +0.
       77  E1                          PIC S999 COMP-3 VALUE +0.
       77  A1                          PIC S9(5) COMP-3 VALUE +0.
       77  M1                          PIC S9(5) COMP-3 VALUE +0.
       77  WS-WORK-FIELD               PIC X(80)    VALUE SPACES.
       77  WS-ARCHIVE-NO               PIC S9(8)  COMP VALUE +0.
       77  WS-FOLLOW-UP-DT             PIC XX  VALUE LOW-VALUES.
       77  WS-RESEND-DT                PIC XX  VALUE LOW-VALUES.
       77  WS-AUTO-LAST-SCHED-DT       PIC XX  VALUE LOW-VALUES.
       77  WS-LAST-ACT-DT              PIC XX  VALUE LOW-VALUES.
       77  WS-ACTIVITY-DT              PIC XX  VALUE LOW-VALUES.
       77  WS-FORM                     PIC XXXX  VALUE SPACES.
       77  WS-LAST-ACT-TYPE            PIC X   VALUE ' '.
       77  WS-FOUND-BENE-SW            PIC X   VALUE ' '.
           88  FOUND-BENE                  VALUE 'Y'.
       77  WS-TALLY                    PIC S999 COMP-3 VALUE +0.
       77  WS-TALLY1                   PIC S999 COMP-3 VALUE +0.
       77  NS-LEN                      PIC S9(5) COMP-3 VALUE +0.
       77  WS-ERPNDB-SW                PIC X  VALUE ' '.
           88  END-OF-ERPNDB              VALUE 'Y'.
       77  WS-ERACCT-SW                PIC X  VALUE ' '.
           88  ACCT-FOUND                 VALUE 'Y'.
       77  WS-ELCERT-SW                PIC X  VALUE ' '.
           88  CERT-FOUND                 VALUE 'Y'.
           88  CERT-NOT-FOUND             VALUE 'N'.
       77  WS-ERENDT-SW                PIC X  VALUE ' '.
           88  ENDT-FOUND                 VALUE 'Y'.
           88  ENDT-NOT-FOUND             VALUE 'N'.
121712 77  WS-CERT-TRL-REC-NOT-FOUND   PIC S9       VALUE +0.
121712     88  CERT-TRL-REC-NOT-FOUND     VALUE +1.
070622 77  WS-CONNECT-SW               PIC X  VALUE ' '.
070622     88  CONNECTED-TO-DB             VALUE 'Y'.


       77  WS-LF-ABBRV                 PIC X(10) VALUE SPACES.
       77  WS-LF-DESC                  PIC X(02) VALUE SPACES.
       77  WS-LF-EARN                  PIC X     VALUE ' '.
       77  WS-WAIT-PER                 PIC X(03) VALUE ZEROS.
       77  WS-RET-ELIM                 PIC X(01) VALUE SPACES.
       77  WS-AH-DESC                  PIC X(35) VALUE SPACES.
       77  WS-BEN-DAYS                 PIC X(02) VALUE ZEROS.


       77  WS-CSR-ID                   PIC XXXX  VALUE SPACES.
       77  WS-LF-BENCD                 PIC XX    VALUE '00'.
       77  WS-AH-BENCD                 PIC XX    VALUE '00'.
       77  WS-SAVE-ERACCT-KEY          PIC X(20)  VALUE SPACES.
       77  WS-COMPANY-CD               PIC X  VALUE LOW-VALUES.
       77  WS-WORK-PREM                PIC S9(9)V99 COMP-3 VALUE +0.
       77  WS-WORK-LF-PREM             PIC S9(9)V99 COMP-3 VALUE +0.
110612 77  WS-WORK-ALT-LF-PREM         PIC S9(9)V99 COMP-3 VALUE +0.
110612 77  WS-WORK-CMB-LF-PREM         PIC S9(9)V99 COMP-3 VALUE +0.
       77  WS-WORK-AH-PREM             PIC S9(9)V99 COMP-3 VALUE +0.
092412 77  WS-WORK-REF                 PIC S9(9)V99 COMP-3 VALUE +0.
092412 77  WS-WORK-LF-REF              PIC S9(9)V99 COMP-3 VALUE +0.
092412 77  WS-WORK-AH-REF              PIC S9(9)V99 COMP-3 VALUE +0.
       77  WS-ISSUE-CANCEL-SW          PIC X  VALUE SPACES.
           88  PROCESS-ISSUE               VALUE 'I'.
           88  PROCESS-CANCEL              VALUE 'C'.
       77  WS-SAVE-OUT-LETTER          PIC X(6)  VALUE SPACES.
       77  WS-WORK-COMM                PIC 9(7)V99 VALUE ZEROS.
       77  WS-DIFF                     PIC 999 VALUE ZEROS.
072312 77  WS-CHGBACK                  PIC 99  VALUE ZEROS.
120412 77  WS-CHGBK-LIFE-PCT           PIC S9V9(5) COMP-3 VALUE +0.
120412 77  WS-CHGBK-AH-PCT             PIC S9V9(5) COMP-3 VALUE +0.
120412 77  WS-CERT-EFF-DT              PIC XX  VALUE SPACES.
122911 77  LINK-ELVADS                 PIC X(8) VALUE 'ELVADS'.

NTTDel*EXEC SQL
NTTDel*   INCLUDE SQLDA
NTTDel*END-EXEC
011514
011514 EXEC SQL
011514    INCLUDE SQLCA
011514 END-EXEC
011514
011514 EXEC SQL
011514    BEGIN DECLARE SECTION
011514 END-EXEC
011514
011514 01  SQLCMD                      PIC X(1024).
011514 01  SVR                         PIC X(32).
       01  database                    pic x(32).
011514 01  USR                         PIC X(32).
011514 01  PASS                        PIC X(32).
011514 01  USR-PASS                    PIC X(64).
011514
011514 01  WS-SQL-DATA.
011514     05  WS-CYCLE-DATE           PIC X(10).
011514     05  WS-NEXT-BUS-DT          PIC X(10).
020916     05  WS-LOOKUPID             PIC X(4).
061821     05  WS-LOOKUPNAME           PIC X(4).
020916     05  WS-LOOKUP-VALUE         PIC X(100).
070622     05  WS-CARRIER              PIC X.
070622     05  WS-GROUP                PIC X(6).
070622     05  WS-STATE                PIC XX.
070622     05  WS-ACCOUNT              PIC X(10).
070622     05  WS-EFF-DT               PIC XX.
070622     05  WS-CERT-NO              PIC X(10).
070622     05  WS-CERT-NO-SUF          PIC X(01).
070622
070622 01  indicator-vaiables-for-nulls.
070622     05  nu-app-status           pic s9(4) comp value +0.
070622     05  nu-app-by               pic s9(4) comp value +0.
070622     05  nu-app-date             pic s9(4) comp value +0.
070622     05  nu-app-batch            pic s9(4) comp value +0.
070622
070622 01  daily-check-request-rec.
070622     05  db-compid               pic xxx.
070622     05  db-carrier              pic x.
070622     05  db-grouping             pic x(6).
070622     05  db-state                pic xx.
070622     05  db-account              pic x(10).
070622     05  db-effdate              pic x(10).
070622     05  db-certificate          pic x(10).
070622     05  db-cert-sfx             pic x.
070622     05  db-seq-no               pic 999.
070622     05  db-type                 pic x.
070622     05  db-amount-n             PIC S9(13)V99   COMP-3.
070622*    05  db-amount redefines db-amount-n
070622*                                pic x(10).
070622     05  db-checkno              pic x(15).
070622     05  db-checkdate            pic x(10).
070622     05  db-checkstatus          pic 9(5).
070622     05  db-releasebatch         pic 9(5).
070622     05  db-releasedt            pic x(10).
070622     05  db-releaseby            pic x(4).
070622     05  db-payeename1           pic x(30).
070622     05  db-payeename2           pic x(30).
070622     05  db-payeeaddr1           pic x(30).
070622     05  db-payeeaddr2           pic x(30).
070622     05  db-payeecity            pic x(30).
070622     05  db-payeest              pic xx.
070622     05  db-payeezip             pic x(10).
070622     05  db-fincar               pic x.
070622     05  db-fingrp               pic x(6).
070622     05  db-finresp              pic x(10).
070622     05  db-finacct              pic x(10).
070622     05  db-preparer             pic x(4).
070622     05  db-app-status           pic x(9).
070622     05  dp-app-status-n redefines db-app-status
070622                                 pic 9(9).
070622     05  db-app-by               pic x(20).
070622     05  db-app-date             pic x(30).
070622     05  db-app-batch            pic x(10).
070622     05  db-return-to            pic x(30).
070622     05  db-insured-name         pic x(30).
070622     05  db-check-sub-type       pic x.
070622     05  db-payeecode            pic x(10).

011514
011514 EXEC SQL
011514    END DECLARE SECTION
011514 END-EXEC
011514
011514
011514 01  P pointer.
011514 01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
011514 01  KIXHOST             pic x(9) value Z"HOSTNAME".
011514 01  var-ptr pointer.
011514 01  env-var-len                 pic 9(4)  binary.
011514 01  rc                          pic 9(9)  binary.
011514
011514 01  WS-KIXHOST                  PIC X(10).
011514 01  WS-KIXSYS.
011514     05  WS-KIX-FIL1             PIC X(10).
011514     05  WS-KIX-APPS             PIC X(10).
011514     05  WS-KIX-ENV              PIC X(10).
011514     05  WS-KIX-MYENV            PIC X(10).
011514     05  WS-KIX-SYS              PIC X(10).
011514
110612 01  WS-CALC-FIELDS.
110612     12  WS-WRK-AMOUNT           PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-WRK-AMOUNT2          PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-CSO-LF-PORTION       PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-CSO-ALT-LF-PORTION   PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-CSO-CMB-LF-PORTION   PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-CSO-AH-PORTION       PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-CSO-PORTION          PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-ACCT-LF-PORTION      PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-ACCT-ALT-LF-PORTION  PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-ACCT-CMB-LF-PORTION  PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-ACCT-AH-PORTION      PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-ACCT-PORTION         PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-CSO-ORIG-LF-PORT     PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-CSO-ORIG-ALT-LF-PORT PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-CSO-ORIG-CMB-LF-PORT PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-CSO-ORIG-AH-PORT     PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-CSO-ORIG-PORTION     PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-ACCT-ORIG-LF-PORT    PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-ACCT-ORIG-ALT-LF-PORT PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-ACCT-ORIG-CMB-LF-PORT PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-ACCT-ORIG-AH-PORT    PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-ACCT-ORIG-PORTION    PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-CSO-NEW-LF-PORT      PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-CSO-NEW-ALT-LF-PORT  PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-CSO-NEW-CMB-LF-PORT  PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-CSO-NEW-AH-PORT      PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-CSO-NEW-PORTION      PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-ACCT-NEW-LF-PORT     PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-ACCT-NEW-ALT-LF-PORT PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-ACCT-NEW-CMB-LF-PORT PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-ACCT-NEW-AH-PORT     PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-ACCT-NEW-PORTION     PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-CSO-LF-PORT-CHG      PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-CSO-ALT-LF-PORT-CHG  PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-CSO-CMB-LF-PORT-CHG  PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-CSO-AH-PORT-CHG      PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-CSO-PORTION-CHG      PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-ACCT-LF-PORT-CHG     PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-ACCT-ALT-LF-PORT-CHG PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-ACCT-CMB-LF-PORT-CHG PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-ACCT-AH-PORT-CHG     PIC S9(7)V99 COMP-3 VALUE +0.
110612     12  WS-ACCT-PORTION-CHG     PIC S9(7)V99 COMP-3 VALUE +0.


       01  WS-SAVE-ERACCT-REC          PIC X(2000).
       01  WS-XML-WORK                 PIC X(4200)  VALUE SPACES.
       01 response-code         pic s9(8) comp.
       01 display-response      pic 9(8).
       01 bl-index              pic 9(8) comp.
       01 max-last-name         pic x(18).
       01 first-initial         pic x.
       01 name-in-range-flag    pic 9.
       01 max-entries           pic s9(8) comp value 100.
       
       01 WS-PASSED-DT          PIC XX   VALUE LOW-VALUES.

       01 lower-case    pic x(26) value
                  "abcdefghijklmnopqrstuvwxyz".
       01 upper-case    pic x(26) value
                  "ABCDEFGHIJKLMNOPQRSTUVWXYZ".

122911                                 
122911                                 COPY ELCVADS.
122911 01  WS-PASS-AREA.
122911     05  WS-PASS-AREA-LENGTH PIC S9(4) COMP VALUE +352.
122911     05  WS-PASS-VADS-REC    PIC X(350).
122911
      *** Z CONTROL LAYOUT MOVED TO COPYBOOK ELCZREC
                                COPY ELCZREC.

       01  MISC.
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL                  VALUE +00.
               88  RESP-NOTFND                  VALUE +13.
               88  RESP-DUPREC                  VALUE +14.
               88  RESP-DUPKEY                  VALUE +15.
               88  RESP-NOTOPEN                 VALUE +19.
               88  RESP-ENDFILE                 VALUE +20.
011514
011514     12  WS-EDIT-A-DATE.
011514         16  WS-EDIT-A-MM                 PIC X(2).
011514         16  FILLER                       PIC X(1).
011514         16  WS-EDIT-A-DD                 PIC X(2).
011514         16  FILLER                       PIC X(1).
011514         16  WS-EDIT-A-CCYY.
011514             20  WS-EDIT-A-CC             PIC X(2).
011514             20  WS-EDIT-A-YY             PIC X(2).


       01  srch-commarea.
                                       copy ELCADLTRSPI.
       01  filler                      pic x(500).


       01  WS-SAVE-CARRIER-INFO.
           05  WS-PREV-CARRIER         PIC X      VALUE SPACES.
           05  WS-CARR-NAME            PIC X(30)  VALUE SPACES.
           05  WS-CARR-ICO             PIC X(30)  VALUE SPACES.
           05  WS-CARR-ADDR1           PIC X(30)  VALUE SPACES.
           05  WS-CARR-ADDR2           PIC X(30)  VALUE SPACES.
           05  WS-CARR-CITYST          PIC X(30)  VALUE SPACES.
           05  WS-CARR-ZIP             PIC X(9)   VALUE SPACES.
           05  WS-CARR-PHONE           PIC X(11)  VALUE SPACES.

       01  WS-SAVE-STATE-INFO.
           05  WS-PREV-STATE           PIC XX     VALUE SPACES.
           05  WS-STATE-NAME           PIC X(25)  VALUE SPACES.

       01  WS-SAVE-LFBEN-INFO.
           05  WS-PREV-LFBEN           PIC XX     VALUE SPACES.
           05  WS-LFBEN-ALPH           PIC XXX    VALUE SPACES.
           05  WS-LFBEN-DESC           PIC X(10)  VALUE SPACES.

       01  WS-SAVE-AHBEN-INFO.
           05  WS-PREV-AHBEN           PIC XX     VALUE SPACES.
           05  WS-AHBEN-DESC           PIC X(10)  VALUE SPACES.
           05  WS-AHBEN-RET-ELIM       PIC X      VALUE SPACES.
           05  WS-AHBEN-DAYS           PIC XX     VALUE SPACES.

       01  WS-ERPNDB-KEY.
           05  WS-ERPNDB-COMPANY-CD    PIC X.
           05  WS-ERPNDB-BATCH-NO      PIC X(6).
           05  WS-ERPNDB-BATCH-SEQ     PIC S9(4) COMP  VALUE +0.
           05  WS-ERPNDB-CHG-SEQ       PIC S9(4) COMP  VALUE +0.
           

       01  WS-ELENCC-KEY.
           05  WS-ELENCC-COMPANY-CD    PIC X.
           05  WS-ELENCC-REC-TYPE      PIC X.
           05  WS-ELENCC-ENC-CODE      PIC X(5).
           05  F                       PIC X(9).

       01  WS-ELCERT-KEY.
           05  WS-ELCERT-COMPANY-CD    PIC X.
           05  WS-ELCERT-CARRIER       PIC X.
           05  WS-ELCERT-GROUP         PIC X(6).
           05  WS-ELCERT-STATE         PIC XX.
           05  WS-ELCERT-ACCOUNT       PIC X(10).
           05  WS-ELCERT-EFF-DT        PIC XX.
           05  WS-ELCERT-CERT-NO       PIC X(11).

121712 01  WS-ELCRTT-KEY.
121712     05  WS-ELCRTT-PRIMARY       PIC X(33).
121712     05  WS-ELCRTT-REC-TYPE      PIC X(1).
121712
       01  WS-ERENDT-KEY.
           05  WS-ERENDT-COMPANY-CD    PIC X.
           05  WS-ERENDT-CARRIER       PIC X.
           05  WS-ERENDT-GROUP         PIC X(6).
           05  WS-ERENDT-STATE         PIC XX.
           05  WS-ERENDT-ACCOUNT       PIC X(10).
           05  WS-ERENDT-EFF-DT        PIC XX.
           05  WS-ERENDT-CERT-NO       PIC X(11).
           05  WS-ERENDT-REC-TYPE      PIC X.
           05  WS-ERENDT-SEQ-NO        PIC 9(4) BINARY.
101812
101812 01  WS-ERENDT-KEY-BY-ARCH.
101812     05  WS-ERENDT-COMPANY-CD-A1 PIC X.
101812     05  WS-ERENDT-ARCHIVE       PIC 9(8) BINARY.

       01  WS-ERCOMP-KEY.
           05  WS-ERCOMP-COMPANY-CD    PIC X.
           05  WS-ERCOMP-CARRIER       PIC X.
           05  WS-ERCOMP-GROUP         PIC X(6).
           05  WS-ERCOMP-RESP-NO       PIC X(10).
           05  WS-ERCOMP-ACCOUNT       PIC X(10).
           05  WS-ERCOMP-REC-TYPE      PIC X.

       01  WS-ERMAIL-KEY.
           05  WS-ERMAIL-COMPANY-CD    PIC X.
           05  WS-ERMAIL-CARRIER       PIC X.
           05  WS-ERMAIL-GROUP         PIC X(6).
           05  WS-ERMAIL-STATE         PIC XX.
           05  WS-ERMAIL-ACCOUNT       PIC X(10).
           05  WS-ERMAIL-EFF-DT        PIC XX.
           05  WS-ERMAIL-CERT-NO       PIC X(11).

       01  WS-ERACCT-KEY.
           05  WS-ERACCT-COMPANY-CD    PIC X.
           05  WS-ERACCT-CARRIER       PIC X.
           05  WS-ERACCT-GROUP         PIC X(6).
           05  WS-ERACCT-STATE         PIC XX.
           05  WS-ERACCT-ACCOUNT       PIC X(10).
           05  WS-ERACCT-EXP-DT        PIC XX.
           05  FILLER                  PIC XXXX.
       01  WS-ELLETR-KEY.
           05  WS-ELLETR-COMPANY-CD    PIC X.
           05  WS-ELLETR-LETTER-ID     PIC X(12).
           05  WS-ELLETR-SEQ-NO        PIC S9(4) COMP VALUE +0.
       01  WS-ELCNTL-KEY.
           05  WS-ELCNTL-COMPANY-ID    PIC XXX.
           05  WS-ELCNTL-REC-TYPE      PIC X.
           05  WS-ELCNTL-GENL.
               10  WS-ELCNTL-STATE     PIC XX  VALUE SPACES.
               10  WS-ELCNTL-BEN-CD.
                   15  F               PIC X.
                   15  WS-ELCNTL-CARR  PIC X.
           05  WS-ELCNTL-SEQ-NO        PIC S9(4) COMP.

                                       COPY ELCDATE.

                                       COPY NSCASEXTR.
                                       COPY ERCPNDB.
                                       COPY ERCENDT.
                                       COPY ERCMAIL.
                                       COPY ERCCOMP.
                                       COPY ELCCERT.
                                       COPY ELCCRTO.
                                       COPY ERCACCT.
                                       COPY ELCTEXT.
                                       COPY ELCCNTL.
                                       COPY NSCASVARS.
                                       COPY ERCARCH.
121712                                 COPY ELCCRTT.

       01  FILLER                      PIC X(500)  VALUE SPACES.

                                       COPY ELCENCC.

       01  connect-string              pic x(60) value spaces.
       01  sqlconnect-parms.
           05  p-sql-server            PIC X(30).
           05  p-sql-database          PIC X(30).
           05  p-connect-return-code   pic s9(5) comp-5.
           05  p-sql-return-message    pic x(256).

       linkage section.
       
110612 01 dfhcommarea                  PIC X(6500).
011514
011514 01  var  pic x(30).

       procedure division.

           display ' entering pgm nsrasbl '
           display ' commarea ' dfhcommarea
           MOVE EIBDATE                TO DC-JULIAN-YYDDD
           MOVE '5'                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           MOVE DC-GREG-DATE-1-EDIT    TO SAVE-DATE
           MOVE DC-GREG-DATE-A-EDIT    TO WS-SAVE-EDIT-A-DATE
           MOVE DC-BIN-DATE-1          TO SAVE-BIN-DATE
011514     MOVE DC-DAY-OF-WEEK         TO SAVE-CYCLE-DAY-OF-WEEK
           move dfhcommarea            to srch-commarea

      *****************************************************
      * The full       key has been established and passed
      * to this program via NSRASLTR
      *****************************************************

           PERFORM 1115-MATCH-COMPANY  THRU 1115-EXIT

      *    display ' input data ' bl-carrier ' ' bl-group ' '
      *       bl-account ' ' bl-eff-dt ' ' bl-cert-no

           MOVE BL-LETTER-ID           TO OUT-LETTER
           MOVE BL-PROC-ID             TO OUT-PROC-ID
           MOVE BL-CARRIER             TO OUT-CARRIER
           MOVE BL-GROUP               TO OUT-GROUPING
           MOVE BL-ACCOUNT             TO OUT-ACCOUNT
           MOVE BL-PRINT-NOW-SW        TO OUT-PRINT-NOW
110612     MOVE BL-CERT-FORM-ID        TO OUT-CERTIFICATE-ID
110612     MOVE BL-SOURCE-SCREEN       TO OUT-SCREEN-ID

           STRING DC-EDITA-CCYY DC-EDITA-MONTH 
              DC-EDITA-DAY  DELIMITED BY SIZE
              INTO OUT-CYCLE-DT
           END-STRING
011514
011514     set P to address of KIXSYS
011514     CALL "getenv" using by value P returning var-ptr
011514     if var-ptr = null then
011514        display ' kixsys not set '
011514     else
011514        set address of var to var-ptr
011514        move 0 to env-var-len
011514        inspect var tallying env-var-len
011514          for characters before X'00' 
011514        unstring var (1:env-var-len) delimited by '/'
011514        into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
011514              WS-KIX-SYS
011514        end-unstring
011514     end-if
011514
011514     set P to address of KIXHOST
011514     CALL "getenv" using by value P returning var-ptr
011514     if var-ptr = null then
011514        display ' kixhost not set '
011514     else
011514        set address of var to var-ptr
011514        move 0 to env-var-len
011514        inspect var tallying env-var-len
011514          for characters before X'00' 
011514        MOVE var(1:env-var-len)  to ws-kixhost
011514        DISPLAY ' WS KIX HOST ' WS-KIXHOST
011514     end-if
011514
020916     MOVE SPACES TO SVR
020916
011514**** This routine will connect to the Logic Database on SQL Server
011514**** and call a stored procedure to determine the next business date
011514
011514     PERFORM 4100-CONNECT-TO-DB  THRU 4100-EXIT
011514     IF SQLCODE = 0
011514        PERFORM 4200-GET-NEXT-BUS-DT  THRU 4200-EXIT
011514        IF SQLCODE = 0
011514            MOVE WS-NEXT-BUS-DT TO SAVE-NEXT-BUS-DT-EDIT-A
011514                                   WS-EDIT-A-DATE
011514            MOVE WS-EDIT-A-YY   TO DC-YMD-YEAR
011514            MOVE WS-EDIT-A-MM   TO DC-YMD-MONTH
011514            MOVE WS-EDIT-A-DD   TO DC-YMD-DAY
011514            MOVE '3'            TO DC-OPTION-CODE
011514            PERFORM 9700-DATE-LINK THRU 9700-EXIT
011514            IF NO-CONVERSION-ERROR
011514               MOVE DC-BIN-DATE-1 TO SAVE-BIN-NEXT-BUS-DT
011514            ELSE
011514               MOVE SAVE-BIN-DATE TO DC-BIN-DATE-1
011514               MOVE '6'         TO DC-OPTION-CODE
011514               MOVE ZEROS       TO DC-ELAPSED-MONTHS
011514               IF SAVE-CYCLE-DAY-OF-WEEK = 6
011514                  MOVE 3        TO DC-ELAPSED-DAYS
011514               ELSE 
011514                 IF SAVE-CYCLE-DAY-OF-WEEK = 7
011514                    MOVE 2     TO DC-ELAPSED-DAYS
011514                 ELSE
011514                    MOVE 1     TO DC-ELAPSED-DAYS
011514                 END-IF
011514               END-IF
011514               PERFORM 9700-DATE-LINK   THRU 9700-EXIT
011514               IF NO-CONVERSION-ERROR
011514                   MOVE DC-BIN-DATE-2 TO SAVE-BIN-NEXT-BUS-DT
011514                   MOVE DC-GREG-DATE-A-EDIT TO 
011514                                SAVE-NEXT-BUS-DT-EDIT-A
011514               ELSE
011514                   MOVE SAVE-BIN-DATE TO SAVE-BIN-NEXT-BUS-DT
011514                   MOVE WS-SAVE-EDIT-A-DATE TO 
011514                                SAVE-NEXT-BUS-DT-EDIT-A
011514               END-IF
011514            END-IF
011514        ELSE
011514            MOVE SAVE-BIN-DATE TO DC-BIN-DATE-1
011514            MOVE '6'         TO DC-OPTION-CODE
011514            MOVE ZEROS       TO DC-ELAPSED-MONTHS
011514            IF SAVE-CYCLE-DAY-OF-WEEK = 6
011514               MOVE 3        TO DC-ELAPSED-DAYS
011514            ELSE 
011514              IF SAVE-CYCLE-DAY-OF-WEEK = 7
011514                 MOVE 2     TO DC-ELAPSED-DAYS
011514              ELSE
011514                 MOVE 1     TO DC-ELAPSED-DAYS
011514              END-IF
011514            END-IF
011514            PERFORM 9700-DATE-LINK   THRU 9700-EXIT
011514            IF NO-CONVERSION-ERROR
011514                MOVE DC-BIN-DATE-2 TO SAVE-BIN-NEXT-BUS-DT
011514                MOVE DC-GREG-DATE-A-EDIT TO 
011514                                SAVE-NEXT-BUS-DT-EDIT-A
011514            ELSE
011514                MOVE SAVE-BIN-DATE TO SAVE-BIN-NEXT-BUS-DT
011514                MOVE WS-SAVE-EDIT-A-DATE TO 
011514                                SAVE-NEXT-BUS-DT-EDIT-A
011514            END-IF
011514        END-IF
011514     ELSE
011514        MOVE SAVE-BIN-DATE TO DC-BIN-DATE-1
011514        MOVE '6'         TO DC-OPTION-CODE
011514        MOVE ZEROS       TO DC-ELAPSED-MONTHS
011514        IF SAVE-CYCLE-DAY-OF-WEEK = 6
011514           MOVE 3        TO DC-ELAPSED-DAYS
011514        ELSE 
011514          IF SAVE-CYCLE-DAY-OF-WEEK = 7
011514             MOVE 2     TO DC-ELAPSED-DAYS
011514          ELSE
011514             MOVE 1     TO DC-ELAPSED-DAYS
011514          END-IF
011514        END-IF
011514        PERFORM 9700-DATE-LINK   THRU 9700-EXIT
011514        IF NO-CONVERSION-ERROR
011514            MOVE DC-BIN-DATE-2 TO SAVE-BIN-NEXT-BUS-DT
011514            MOVE DC-GREG-DATE-A-EDIT TO 
011514                                SAVE-NEXT-BUS-DT-EDIT-A
011514        ELSE
011514            MOVE SAVE-BIN-DATE TO SAVE-BIN-NEXT-BUS-DT
011514            MOVE WS-SAVE-EDIT-A-DATE TO 
011514                                SAVE-NEXT-BUS-DT-EDIT-A
011514        END-IF
011514     END-IF
021214     PERFORM 4300-DISCONNECT THRU 4300-EXIT
011514
011514     MOVE SAVE-NEXT-BUS-DT-EDIT-A  TO WS-EDIT-A-DATE
011514     STRING WS-EDIT-A-CCYY '-' WS-EDIT-A-MM '-'
011514           WS-EDIT-A-DD ' 00:00:00.0' DELIMITED BY SIZE
011514              INTO OUT-NEXT-BUS-DT
011514     END-STRING

072312     IF BL-EFF-DT = '99/99/99' OR '99/99/9999'
              MOVE HIGH-VALUES         TO WS-PASSED-DT
           ELSE
              IF BL-EFF-DT = SPACES
                 display ' bad eff data ' bl-eff-dt ' ' bl-cert-no
              ELSE
072312           IF BL-EFF-DT (9:2) = SPACES                 
072312              STRING BL-EFF-DT (7:2) BL-EFF-DT (1:2) 
072312                     BL-EFF-DT (4:2)
072312                DELIMITED BY SIZE INTO DC-GREG-DATE-1-YMD-R
072312              END-STRING
072312           ELSE
072312              STRING BL-EFF-DT (9:2) BL-EFF-DT (1:2) 
072312                     BL-EFF-DT (4:2)
072312                DELIMITED BY SIZE INTO DC-GREG-DATE-1-YMD-R
072312              END-STRING
072312           END-IF
                 MOVE '3'              TO DC-OPTION-CODE
                 PERFORM 9700-DATE-LINK THRU 9700-EXIT
                 IF NO-CONVERSION-ERROR
                    MOVE DC-BIN-DATE-1 TO WS-PASSED-DT
                 ELSE
                    MOVE 'BAD EFF DATE CONVERT ' TO BL-MESSAGE
                    SET BL-FAIL TO TRUE
                    GO TO 0000-RETURN
                 END-IF
              END-IF
           END-IF

           perform 0800-init-out-data  thru 0800-exit

           SET BL-FAIL TO TRUE
      ******************************************************************
      * DATA SOURCE MEANINGS  BL-DATA-SRCE
      *    1) FROM ACCT MAINT
      *    2) FROM CERT UPDATE
      *    3) FROM COMP MAINT
      *    4) FROM REVIEW AND CORRECTIONS
      ******************************************************************

           EVALUATE BL-DATA-SRCE
              WHEN '1'
                 PERFORM 0100-DATA-SRCE-1 THRU 0100-EXIT
              WHEN '2'
                 PERFORM 0200-DATA-SRCE-2 THRU 0200-EXIT
              WHEN '3'
                 PERFORM 0300-DATA-SRCE-3 THRU 0300-EXIT
              WHEN '4'
                 PERFORM 0400-DATA-SRCE-4 THRU 0400-EXIT
              WHEN OTHER
                 MOVE ' BAD DATA SOURCE CODE ' TO BL-MESSAGE
           END-EVALUATE

           IF BL-OK
              display ' bl write erarch ' bl-write-erarch
              PERFORM 1250-GET-ELLETR  THRU 1250-EXIT
              PERFORM 1700-GET-ELENCC  THRU 1700-EXIT
              IF BL-ARCHIVE-NO = ZEROS
                 PERFORM 1500-GET-ARCH-NO THRU 1500-EXIT
                 MOVE WS-ARCHIVE-NO    TO BL-ARCHIVE-NO
                 display ' arch zeros, new arch ' bl-archive-no
              END-IF
              MOVE BL-ARCHIVE-NO       TO OUT-ARCH-NO
              PERFORM 0500-SCRUB-DATA  THRU 0500-EXIT
              display ' bl write erarch ' bl-write-erarch
              IF BL-WRITE-ERARCH = 'T' OR 'B'
                 PERFORM 0700-WRITE-EXTR THRU 0700-EXIT
                 IF BL-FAIL
                    GO TO 0000-RETURN
                 END-IF
              END-IF
              MOVE W-LETTER-TO-ACCT    TO BL-LETTER-TO-ACCT
              MOVE W-LETTER-TO-BENE    TO BL-LETTER-TO-BENE
           END-IF

           .
       0000-RETURN.

           move srch-commarea          to dfhcommarea

           exec cics
              return
           end-exec

           .
       0100-DATA-SRCE-1.

           PERFORM 1120-MATCH-CARRIER  THRU 1120-EXIT
           PERFORM 1125-MATCH-STATE    THRU 1125-EXIT
           MOVE ' '                    TO WS-ERACCT-SW
           MOVE LOW-VALUES             TO WS-ERACCT-KEY
           MOVE WS-COMPANY-CD          TO WS-ERACCT-COMPANY-CD
           MOVE BL-CARRIER             TO WS-ERACCT-CARRIER
           MOVE BL-GROUP               TO WS-ERACCT-GROUP
           MOVE BL-STATE               TO WS-ERACCT-STATE
           MOVE BL-ACCOUNT             TO WS-ERACCT-ACCOUNT
           MOVE WS-PASSED-DT           TO WS-ERACCT-EXP-DT
120412                                    WS-CERT-EFF-DT
           PERFORM 1200-GET-ERACCT     THRU 1200-EXIT
           IF ACCT-FOUND
              MOVE AM-CSR-CODE         TO WS-CSR-ID
              PERFORM 1025-GET-CSR-INFO
                                       THRU 1025-EXIT
072312        PERFORM 1300-DETERMINE-COMM
072312                                 THRU 1300-EXIT
              SET BL-OK TO TRUE
           ELSE
              MOVE ' ACCOUNT MASTER NOT FOUND ' TO BL-MESSAGE
              SET BL-FAIL TO TRUE
           END-IF

           .
       0100-EXIT.
           EXIT.

       0200-DATA-SRCE-2.

           display ' made it to data srce 2 '
           PERFORM 1120-MATCH-CARRIER  THRU 1120-EXIT
           PERFORM 1125-MATCH-STATE    THRU 1125-EXIT
           MOVE ' '                    TO WS-ELCERT-SW
           MOVE WS-COMPANY-CD          TO WS-ELCERT-COMPANY-CD
           MOVE BL-CARRIER             TO WS-ELCERT-CARRIER
           MOVE BL-GROUP               TO WS-ELCERT-GROUP
           MOVE BL-STATE               TO WS-ELCERT-STATE
           MOVE BL-ACCOUNT             TO WS-ELCERT-ACCOUNT
           MOVE WS-PASSED-DT           TO WS-ELCERT-EFF-DT
120412                                    WS-CERT-EFF-DT
           MOVE BL-CERT-NO             TO WS-ELCERT-CERT-NO
120412
120412     MOVE ' '                 TO WS-ERACCT-SW
120412     MOVE LOW-VALUES          TO WS-ERACCT-KEY
120412     MOVE WS-ELCERT-KEY (1:22) TO WS-ERACCT-KEY (1:22)
120412     PERFORM 1200-GET-ERACCT  THRU 1200-EXIT
120412     IF ACCT-FOUND
120412        MOVE AM-CSR-CODE      TO WS-CSR-ID
120412        PERFORM 1025-GET-CSR-INFO THRU 1025-EXIT
120412     END-IF

           PERFORM 1100-GET-ELCERT     THRU 1100-EXIT

           IF CERT-FOUND
              PERFORM 1130-MATCH-BENCDS THRU 1130-EXIT
              MOVE WS-LF-ABBRV         TO OUT-LF-ABBRV
              MOVE WS-LF-DESC          TO OUT-LF-DESC
              IF WS-LF-EARN = 'B'
                 MOVE 'B'              TO OUT-BALLOON-IND
              END-IF
              MOVE WS-WAIT-PER         TO OUT-WAIT-PER
              MOVE WS-RET-ELIM         TO OUT-RET-ELIM
              MOVE WS-AH-DESC          TO OUT-AH-DESC
              MOVE WS-BEN-DAYS         TO OUT-BEN-DAYS
101812
101812        IF BL-ENDT-ARCH-NO > ZERO
101812           MOVE WS-COMPANY-CD      TO WS-ERENDT-COMPANY-CD-A1
101812           MOVE BL-ENDT-ARCH-NO    TO WS-ERENDT-ARCHIVE
101812           PERFORM 0600-GET-ERENDT THRU 0600-EXIT
101812        ELSE
101812           IF BL-ARCHIVE-NO > ZERO
101812              MOVE WS-COMPANY-CD      TO WS-ERENDT-COMPANY-CD-A1
101812              MOVE BL-ARCHIVE-NO      TO WS-ERENDT-ARCHIVE
101812              PERFORM 0600-GET-ERENDT THRU 0600-EXIT
101812           END-IF
101812        END-IF
101812
020916        PERFORM 4500-CHECK-BALLOON-STATE THRU 4500-EXIT
020916*072312 IF (CM-STATE = 'KY' OR 'MD' OR 'WI')
020916        IF WS-LOOKUP-VALUE > SPACES
                AND (OUT-BALLOON-IND = 'B')
                 MOVE 'B'              TO OUT-FORM (3:1)
              END-IF
              SET BL-OK TO TRUE
           ELSE
              display ' cert not found ' ws-elcert-cert-no
              MOVE ' CERTIFICATE NOT FOUND ' TO BL-MESSAGE
              SET BL-FAIL TO TRUE
           END-IF

           .
       0200-EXIT.
           EXIT.

       0300-DATA-SRCE-3.

           PERFORM 1120-MATCH-CARRIER  THRU 1120-EXIT

           MOVE WS-COMPANY-CD          TO WS-ERCOMP-COMPANY-CD
           MOVE BL-CARRIER             TO WS-ERCOMP-CARRIER
           MOVE BL-GROUP               TO WS-ERCOMP-GROUP
           MOVE BL-RESP-NO             TO WS-ERCOMP-RESP-NO
           MOVE BL-ACCOUNT             TO WS-ERCOMP-ACCOUNT
           IF WS-ERCOMP-ACCOUNT = SPACES
              MOVE LOW-VALUES          TO WS-ERCOMP-ACCOUNT
              MOVE 'G'                 TO WS-ERCOMP-REC-TYPE
           ELSE
              MOVE 'A'                 TO WS-ERCOMP-REC-TYPE
           END-IF

           PERFORM 1225-GET-ERCOMP     THRU 1225-EXIT
           IF RESP-NORMAL
              MOVE CO-CSR-CODE         TO WS-CSR-ID
              PERFORM 1025-GET-CSR-INFO
                                       THRU 1025-EXIT
              SET BL-OK TO TRUE
           ELSE
              MOVE ' COMPENSATION MASTER NOT FOUND ' TO BL-MESSAGE
              SET BL-FAIL TO TRUE
           END-IF

           .
       0300-EXIT.
           EXIT.

       0400-DATA-SRCE-4.

           MOVE WS-COMPANY-CD          TO WS-ERPNDB-COMPANY-CD
           MOVE BL-BATCH-NO            TO WS-ERPNDB-BATCH-NO
           MOVE BL-BATCH-SEQ           TO WS-ERPNDB-BATCH-SEQ
           MOVE +0                     TO WS-ERPNDB-CHG-SEQ
           MOVE ' '                    TO WS-ERPNDB-SW WS-ERACCT-SW
                                          WS-ELCERT-SW WS-ERENDT-SW

           EXEC CICS READ                                               
                DATASET    ('ERPNDB') 
                INTO       (PENDING-BUSINESS)
                RIDFLD     (WS-ERPNDB-KEY)                                   
                RESP       (WS-RESPONSE)
           END-EXEC


           IF RESP-NORMAL
              IF PB-RECORD-TYPE = '1' OR '2'
                 MOVE PB-CARRIER       TO BL-CARRIER
                 MOVE PB-GROUPING      TO BL-GROUP
                 MOVE PB-STATE         TO BL-STATE
                 MOVE PB-ACCOUNT       TO BL-ACCOUNT
                 MOVE PB-CERT-NO       TO BL-CERT-NO
                 MOVE PB-CSR-ID        TO WS-CSR-ID
                 IF PB-ISSUE
                    SET PROCESS-ISSUE TO TRUE
                    MOVE PB-I-LF-BENEFIT-CD TO WS-LF-BENCD
                    MOVE PB-I-AH-BENEFIT-CD TO WS-AH-BENCD
                    MOVE PB-I-LF-PREM-CALC TO OUT-LF-CALC-PRM
                    MOVE PB-I-AH-PREM-CALC TO OUT-AH-CALC-PRM
                 ELSE
                    SET PROCESS-CANCEL TO TRUE
                    MOVE PB-CI-LF-BENEFIT-CD TO WS-LF-BENCD
                    MOVE PB-CI-AH-BENEFIT-CD TO WS-AH-BENCD
                    MOVE PB-C-LF-REF-CALC TO OUT-LF-CALC-REF
                    MOVE PB-C-AH-REF-CALC TO OUT-AH-CALC-REF
                 END-IF
                 PERFORM 1120-MATCH-CARRIER
                                       THRU 1120-EXIT
                 PERFORM 1125-MATCH-STATE
                                       THRU 1125-EXIT
                 PERFORM 1130-MATCH-BENCDS
                                       THRU 1130-EXIT
                 MOVE WS-LF-ABBRV      TO OUT-LF-ABBRV
                 MOVE WS-LF-DESC       TO OUT-LF-DESC
072312                                    OUT-ORIG-LF-DESC
                 IF WS-LF-EARN = 'B'
                    MOVE 'B'           TO OUT-BALLOON-IND
                 END-IF
                 MOVE WS-WAIT-PER      TO OUT-WAIT-PER
072312                                    OUT-ORIG-WAIT-PER
                 MOVE WS-RET-ELIM      TO OUT-RET-ELIM
072312                                    OUT-ORIG-RET-ELIM
                 MOVE WS-AH-DESC       TO OUT-AH-DESC
072312                                    OUT-ORIG-AH-DESC
                 MOVE WS-BEN-DAYS      TO OUT-BEN-DAYS
072312                                    OUT-ORIG-BEN-DAYS
                 PERFORM 1025-GET-CSR-INFO
                                       THRU 1025-EXIT
120412
120412           MOVE LOW-VALUES       TO WS-ERACCT-KEY
120412           MOVE PB-CONTROL-BY-ACCOUNT (1:22)
120412                                 TO WS-ERACCT-KEY (1:22)
120412           MOVE PB-CERT-EFF-DT   TO WS-CERT-EFF-DT
120412           PERFORM 1200-GET-ERACCT THRU 1200-EXIT
120412           IF ACCT-FOUND
120412              MOVE AM-CSR-CODE      TO WS-CSR-ID
120412              PERFORM 1025-GET-CSR-INFO
120412                                    THRU 1025-EXIT
120412              PERFORM 1300-DETERMINE-COMM
120412                                    THRU 1300-EXIT
120412           END-IF
120412
                 MOVE PB-CONTROL-BY-ACCOUNT (1:33)
                                       TO WS-ELCERT-KEY
                 PERFORM 1100-GET-ELCERT
                                       THRU 1100-EXIT
080522           IF (BL-DATA-SRCE = '4')
080522             AND (PROCESS-CANCEL)
070622              MOVE PB-C-CANCEL-REASON  TO OUT-CANCEL-REASON
070622              DISPLAY ' CANCEL-REASON ' OUT-CANCEL-REASON
070622              IF PB-C-REFUND-CREATED
070622                 PERFORM 2000-GET-CANCEL-REFUND THRU 2000-EXIT
070622              END-IF
070622           END-IF

101812
101812           IF BL-ENDT-ARCH-NO > ZERO
101812              MOVE PB-COMPANY-CD   TO WS-ERENDT-COMPANY-CD-A1
101812              MOVE BL-ENDT-ARCH-NO TO WS-ERENDT-ARCHIVE
101812              PERFORM 0600-GET-ERENDT THRU 0600-EXIT
101812           ELSE
101812              IF BL-ARCHIVE-NO > ZERO
101812                 MOVE PB-COMPANY-CD TO WS-ERENDT-COMPANY-CD-A1
101812                 MOVE BL-ARCHIVE-NO TO WS-ERENDT-ARCHIVE
101812                 PERFORM 0600-GET-ERENDT THRU 0600-EXIT
101812              END-IF
101812           END-IF
101812
                 SET BL-OK TO TRUE
020916           PERFORM 4500-CHECK-BALLOON-STATE THRU 4500-EXIT
020916*072312     IF (CM-STATE = 'KY' OR 'MD' OR 'WI')
020916           IF WS-LOOKUP-VALUE > SPACES
020916             AND (OUT-BALLOON-IND = 'B')
                    MOVE 'B'           TO OUT-FORM (3:1)
                 END-IF
              END-IF
           END-IF

           .
       0400-EXIT.
           EXIT.

       0500-SCRUB-DATA.

      *   THE DECODER DOES NOT LIKE & AND I TRIED TO USE
      *   A DIFFERENT DELIMITER IN PROGRAM NSRASLTR BUT
      *   COULD NOT GET IT TO WORK SO I AM CONVERTING IT
      *   TO A HEX 26 TO GET BY THE DECODER

           MOVE FUNCTION LENGTH(OUT-ACCT-NAME)
                                       TO M1
           MOVE OUT-ACCT-NAME          TO WS-WORK-FIELD
           PERFORM VARYING A1 FROM +1 BY +1 UNTIL 
              (A1 > M1)
              OR (WS-WORK-FIELD (A1:1) = '*')
091112        OR (WS-WORK-FIELD (A1:4) = ' DBA' OR ' AKA')
              OR (WS-WORK-FIELD (A1:4) = '(DBA' OR '(AKA')
           END-PERFORM
           IF A1 > M1
              CONTINUE
           ELSE
              MOVE WS-WORK-FIELD (1:A1 - 1)
                                       TO OUT-ACCT-NAME
           END-IF

           MOVE FUNCTION LENGTH(OUT-BENE-NAME)
                                       TO M1
           MOVE OUT-BENE-NAME          TO WS-WORK-FIELD
           PERFORM VARYING A1 FROM +1 BY +1 UNTIL 
              (A1 > M1)
              OR (WS-WORK-FIELD (A1:1) = '*')
091112        OR (WS-WORK-FIELD (A1:4) = ' DBA' OR ' AKA')
              OR (WS-WORK-FIELD (A1:4) = '(DBA' OR '(AKA')
           END-PERFORM
           IF A1 > M1
              CONTINUE
           ELSE
              MOVE WS-WORK-FIELD (1:A1 - 1)
                                       TO OUT-BENE-NAME
           END-IF
           
           IF OUT-CERT-NO = LOW-VALUES OR SPACES
               MOVE '0000000000 '  TO OUT-CERT-NO
           END-IF

           MOVE +1 TO M1
           MOVE SPACES TO BL-RECORD-PASSED-DATA
           MOVE FUNCTION LENGTH(NAPER-OUTPUT-DATA)
                                       TO NS-LEN
           display ' naper output data length ' ns-len
           PERFORM VARYING A1 FROM +1 BY +1 UNTIL
              (NAPER-OUTPUT-DATA (A1:11) = 'EndOfString')
              OR (A1 > NS-LEN)
              IF NAPER-OUTPUT-DATA (A1:3) = SPACES
                 CONTINUE
              ELSE
                 IF NAPER-OUTPUT-DATA (A1:1) = '~'
                    MOVE '&'           TO BL-RECORD-PASSED-DATA (M1:1)
                    MOVE ' '           TO NAPER-OUTPUT-DATA (A1:1)
                    ADD +1 TO M1
                 ELSE
                    IF NAPER-OUTPUT-DATA (A1:1) = '&'
                       MOVE '%26'      TO BL-RECORD-PASSED-DATA (M1:3)
                       ADD +3 TO M1
                    ELSE
                       MOVE NAPER-OUTPUT-DATA (A1:1)
                                       TO BL-RECORD-PASSED-DATA (M1:1)
                       ADD +1 TO M1
                    END-IF
                 END-IF
              END-IF
           END-PERFORM
           .
       0500-EXIT.
           EXIT.

       0600-GET-ERENDT.

           EXEC CICS READ
101812          DATASET    ('ERENDT2')
                INTO       (ENDORSEMENT-RECORD)
101812          RIDFLD     (WS-ERENDT-KEY-BY-ARCH)
                RESP       (WS-RESPONSE)
           END-EXEC

           DISPLAY ' JUST READ ENDT ' WS-RESPONSE ' '
               WS-ERENDT-ARCHIVE
           IF (RESP-NORMAL)
080522        and (en-company-cd  = cm-company-cd)
080522        and (en-carrier     = cm-carrier)
080522        and (en-state       = cm-state)
080522        and (en-account     = cm-account)
080522        and (en-cert-eff-dt = cm-cert-eff-dt)
080522        and (en-cert-no     = cm-cert-no)
              SET ENDT-FOUND TO TRUE
              PERFORM 0620-BUILD-ENDT  THRU 0620-EXIT
              PERFORM 0650-PROCESS-NO-Change
                                       THRU 0650-EXIT
           END-IF

           .
       0600-EXIT.
           EXIT.

       0620-BUILD-ENDT.

110612     MOVE SPACES                 TO OUT-ORIG-LF-CAN-DT
110612                                    OUT-NEW-LF-CAN-DT
110612                                    OUT-ORIG-AH-CAN-DT
110612                                    OUT-NEW-AH-CAN-DT
110612                                    OUT-ORIG-CAN-DT
110612                                    OUT-NEW-CAN-DT

           IF EN-REC-TYPE = 'C'
              GO TO 0620-PROCESS-CANCEL
           END-IF

           MOVE EN-LF-ORIG-BENCD       TO WS-LF-BENCD
           MOVE EN-AH-ORIG-BENCD       TO WS-AH-BENCD
           PERFORM 1130-MATCH-BENCDS   THRU 1130-EXIT
           MOVE WS-LF-DESC             TO OUT-ORIG-LF-DESC
072312     IF WS-LF-EARN = 'B'
072312        MOVE 'B'                 TO OUT-BALLOON-IND
072312     END-IF
           MOVE WS-AH-DESC             TO OUT-ORIG-AH-DESC
           MOVE WS-RET-ELIM            TO OUT-ORIG-RET-ELIM
           MOVE WS-BEN-DAYS            TO OUT-ORIG-BEN-DAYS
           MOVE WS-WAIT-PER            TO OUT-ORIG-WAIT-PER
           MOVE EN-INS-ORIG-LAST-NAME    TO OUT-ORIG-ILNAME
           MOVE EN-INS-ORIG-FIRST-NAME   TO OUT-ORIG-IFNAME
           MOVE EN-INS-ORIG-MIDDLE-INIT  TO OUT-ORIG-MINIT
121712     IF EN-INS-ORIG-AGE-DEF-FLAG = 'Y'
121712         MOVE ZERO                 TO OUT-ORIG-IAGE
121712     ELSE
121712         MOVE EN-INS-ORIG-AGE      TO OUT-ORIG-IAGE
121712     END-IF
           MOVE EN-JNT-ORIG-LAST-NAME    TO OUT-ORIG-JLNAME
           MOVE EN-JNT-ORIG-FIRST-NAME   TO OUT-ORIG-JFNAME
           MOVE EN-JNT-ORIG-MIDDLE-INIT  TO OUT-ORIG-JMINIT
121712     IF EN-JNT-ORIG-AGE-DEF-FLAG = 'Y'
121712         MOVE ZERO                 TO OUT-ORIG-JAGE
121712     ELSE
121712         MOVE EN-JNT-ORIG-AGE      TO OUT-ORIG-JAGE
121712     END-IF

           MOVE EN-LF-ORIG-BENCD         TO OUT-ORIG-LF-BENCD
           MOVE EN-LF-ORIG-TERM          TO OUT-ORIG-LF-TERM
           MOVE EN-LF-ORIG-BEN-AMT       TO OUT-ORIG-LF-BEN
           MOVE EN-LF-ORIG-PRM-AMT       TO OUT-ORIG-LF-PRM
           MOVE EN-LF-ORIG-ALT-BEN-AMT   TO OUT-ORIG-LF-ALT-BEN
           MOVE EN-LF-ORIG-ALT-PRM-AMT   TO OUT-ORIG-LF-ALT-PRM
110612     COMPUTE WS-WRK-AMOUNT = EN-LF-ORIG-PRM-AMT +
110612                             EN-LF-ORIG-ALT-PRM-AMT
110612     MOVE WS-WRK-AMOUNT          TO OUT-ORIG-LF-CMB-PREM
           IF EN-LF-ORIG-BENCD EQUAL '00' OR SPACES
               MOVE SPACES             TO OUT-ORIG-LF-BEN-A
                                          OUT-ORIG-LF-PRM-A
                                          OUT-ORIG-LF-ALT-BEN-A
                                          OUT-ORIG-LF-ALT-PRM-A
110612                                    OUT-ORIG-COVERAGE-IND
110612     ELSE
110612         MOVE 'LIFE'             TO OUT-ORIG-COVERAGE-IND
091213         IF EN-LF-ORIG-PRM-AMT = .01
121112             MOVE SPACES         TO OUT-ORIG-LF-PRM-A
121112         END-IF
091213         IF EN-LF-ORIG-BEN-AMT = 999999999.99
121112             MOVE SPACES         TO OUT-ORIG-LF-BEN-A
121112         END-IF
091213         IF EN-LF-ORIG-ALT-PRM-AMT = .01
091213             MOVE SPACES         TO OUT-ORIG-LF-ALT-PRM-A
091213         END-IF
091213         IF EN-LF-ORIG-ALT-BEN-AMT = 999999999.99
091213             MOVE SPACES         TO OUT-ORIG-LF-ALT-BEN-A
091213         END-IF
           END-IF

           MOVE EN-LF-ORIG-EXP-DT      TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           IF NO-CONVERSION-ERROR
              STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
                 DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
                    INTO OUT-ORIG-LF-EXP-DT
              END-STRING
           END-IF

           MOVE EN-AH-ORIG-BENCD       TO OUT-ORIG-AH-BENCD
           MOVE EN-AH-ORIG-TERM        TO OUT-ORIG-AH-TERM
           MOVE EN-AH-ORIG-BEN-AMT     TO OUT-ORIG-AH-BEN
           MOVE EN-AH-ORIG-PRM-AMT     TO OUT-ORIG-AH-PRM
           IF EN-AH-ORIG-BENCD EQUAL '00' OR SPACES
               MOVE SPACES             TO OUT-ORIG-AH-BEN-A
                                          OUT-ORIG-AH-PRM-A
110612     ELSE
110612        IF OUT-ORIG-COVERAGE-IND = 'LIFE'
110612            MOVE 'BOTH'          TO OUT-ORIG-COVERAGE-IND
110612        ELSE
110612            MOVE 'AH  '          TO OUT-ORIG-COVERAGE-IND
110612        END-IF
091213        IF EN-AH-ORIG-PRM-AMT = .01
121112            MOVE SPACES          TO OUT-ORIG-AH-PRM-A
121112        END-IF
091213        IF EN-AH-ORIG-BEN-AMT = 9999999.99
121112            MOVE SPACES          TO OUT-ORIG-AH-BEN-A
121112        END-IF
           END-IF

           COMPUTE WS-WORK-PREM =
072312        EN-LF-ORIG-PRM-AMT + EN-LF-ORIG-ALT-PRM-AMT
072312           + EN-AH-ORIG-PRM-AMT
           MOVE WS-WORK-PREM           TO OUT-ORIG-TOT-PREM

           MOVE EN-AH-ORIG-EXP-DT      TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           IF NO-CONVERSION-ERROR
              STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
                 DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
                    INTO OUT-ORIG-AH-EXP-DT
              END-STRING
           END-IF
           MOVE EN-AH-ORIG-CP          TO OUT-ORIG-CRIT-PER
                                          OUT-ORIG-MAX-PMTS
           IF EN-AH-ORIG-BENCD EQUAL '00' OR SPACES
               MOVE SPACES             TO OUT-ORIG-MAX-PMTS-A
           END-IF

           MOVE EN-INS-NEW-LAST-NAME    TO OUT-NEW-ILNAME
           MOVE EN-INS-NEW-FIRST-NAME   TO OUT-NEW-IFNAME
           MOVE EN-INS-NEW-MIDDLE-INIT  TO OUT-NEW-IMINIT
121712     IF EN-INS-NEW-AGE-DEF-FLAG = 'Y'
121712         MOVE ZERO                TO OUT-NEW-IAGE
121712     ELSE
121712         MOVE EN-INS-NEW-AGE      TO OUT-NEW-IAGE
121712     END-IF
           MOVE EN-JNT-NEW-LAST-NAME    TO OUT-NEW-JLNAME
           MOVE EN-JNT-NEW-FIRST-NAME   TO OUT-NEW-JFNAME
           MOVE EN-JNT-NEW-MIDDLE-INIT  TO OUT-NEW-JMINIT
121712     IF EN-JNT-NEW-AGE-DEF-FLAG = 'Y'
121712         MOVE ZERO                TO OUT-NEW-JAGE
121712     ELSE
121712         MOVE EN-JNT-NEW-AGE      TO OUT-NEW-JAGE
121712     END-IF

           MOVE EN-LF-NEW-BENCD        TO WS-LF-BENCD
           MOVE EN-AH-NEW-BENCD        TO WS-AH-BENCD
           PERFORM 1130-MATCH-BENCDS   THRU 1130-EXIT
           MOVE WS-LF-DESC             TO OUT-NEW-LF-DESC
072312     IF WS-LF-EARN = 'B'
072312        MOVE 'B'                 TO OUT-BALLOON-IND
072312     END-IF
           MOVE WS-AH-DESC             TO OUT-NEW-AH-DESC
           MOVE WS-RET-ELIM            TO OUT-NEW-RET-ELIM
           MOVE WS-BEN-DAYS            TO OUT-NEW-BEN-DAYS
           MOVE WS-WAIT-PER            TO OUT-NEW-WAIT-PER
           MOVE EN-LF-NEW-BENCD         TO OUT-NEW-LF-BENCD
           MOVE EN-LF-NEW-TERM          TO OUT-NEW-LF-TERM
           MOVE EN-LF-NEW-BEN-AMT       TO OUT-NEW-LF-BEN
           MOVE EN-LF-NEW-PRM-AMT       TO OUT-NEW-LF-PRM
           MOVE EN-LF-NEW-ALT-BEN-AMT   TO OUT-NEW-LF-ALT-BEN
           MOVE EN-LF-NEW-ALT-PRM-AMT   TO OUT-NEW-LF-ALT-PRM
110612     COMPUTE WS-WRK-AMOUNT = EN-LF-NEW-PRM-AMT +
110612                             EN-LF-NEW-ALT-PRM-AMT
110612     MOVE WS-WRK-AMOUNT           TO OUT-NEW-LF-CMB-PREM 
           IF EN-LF-NEW-BENCD EQUAL '00' OR SPACES
               MOVE SPACES             TO OUT-NEW-LF-BEN-A
                                          OUT-NEW-LF-PRM-A
                                          OUT-NEW-LF-ALT-BEN-A
                                          OUT-NEW-LF-ALT-PRM-A
110612                                    OUT-NEW-LF-CMB-PREM-A
110612                                    OUT-NEW-COVERAGE-IND
110612     ELSE
110612         MOVE 'LIFE'             TO OUT-NEW-COVERAGE-IND
           END-IF
          MOVE EN-LF-NEW-EXP-DT        TO DC-BIN-DATE-1
           MOVE ' '                     TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK       THRU 9700-EXIT
           IF NO-CONVERSION-ERROR
              STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
                 DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
                    INTO OUT-NEW-LF-EXP-DT
              END-STRING
           END-IF

           MOVE EN-AH-NEW-BENCD         TO OUT-NEW-AH-BENCD
           MOVE EN-AH-NEW-TERM          TO OUT-NEW-AH-TERM
           MOVE EN-AH-NEW-BEN-AMT       TO OUT-NEW-AH-BEN
           MOVE EN-AH-NEW-PRM-AMT       TO OUT-NEW-AH-PRM

072312     COMPUTE WS-WORK-PREM =
072312        EN-LF-NEW-PRM-AMT + EN-LF-NEW-ALT-PRM-AMT
072312           + EN-AH-NEW-PRM-AMT
072312     MOVE WS-WORK-PREM           TO OUT-NEW-TOT-PREM

           IF EN-AH-NEW-BENCD EQUAL '00' OR SPACES
               MOVE SPACES             TO OUT-NEW-AH-BEN-A
                                          OUT-NEW-AH-PRM-A
110612     ELSE
110612        IF OUT-NEW-COVERAGE-IND = 'LIFE'
110612            MOVE 'BOTH'          TO OUT-NEW-COVERAGE-IND
110612        ELSE
110612            MOVE 'AH  '          TO OUT-NEW-COVERAGE-IND
110612        END-IF
           END-IF
           MOVE EN-AH-NEW-EXP-DT        TO DC-BIN-DATE-1
           MOVE ' '                     TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK       THRU 9700-EXIT
           IF NO-CONVERSION-ERROR
              STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
                 DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
                    INTO OUT-NEW-AH-EXP-DT
              END-STRING
           END-IF
           MOVE EN-AH-NEW-CP            TO OUT-NEW-CRIT-PER
                                           OUT-NEW-MAX-PMTS
           IF EN-AH-NEW-BENCD EQUAL '00' OR SPACES
               MOVE SPACES             TO OUT-NEW-MAX-PMTS-A
           END-IF

           IF EN-LF-ORIG-EXP-DT > EN-AH-ORIG-EXP-DT
              MOVE OUT-ORIG-LF-EXP-DT  TO OUT-ORIG-SCHED-EXP-DT
           ELSE
              MOVE OUT-ORIG-AH-EXP-DT  TO OUT-ORIG-SCHED-EXP-DT
           END-IF

           IF EN-LF-NEW-EXP-DT > EN-AH-NEW-EXP-DT
              MOVE OUT-NEW-LF-EXP-DT   TO OUT-NEW-SCHED-EXP-DT
           ELSE
              MOVE OUT-NEW-AH-EXP-DT   TO OUT-NEW-SCHED-EXP-DT
           END-IF

           IF EN-LF-ORIG-TERM > EN-AH-ORIG-TERM
              MOVE EN-LF-ORIG-TERM     TO OUT-ORIG-SCHED-TERM
           ELSE
              MOVE EN-AH-ORIG-TERM     TO OUT-ORIG-SCHED-TERM
           END-IF

           IF EN-LF-NEW-TERM > EN-AH-NEW-TERM
              MOVE EN-LF-NEW-TERM      TO OUT-NEW-SCHED-TERM
           ELSE
              MOVE EN-AH-NEW-TERM      TO OUT-NEW-SCHED-TERM
           END-IF
           COMPUTE WS-WORK-PREM =
              (EN-LF-NEW-PRM-AMT + EN-LF-NEW-ALT-PRM-AMT + 
              EN-AH-NEW-PRM-AMT) -
              (EN-LF-ORIG-PRM-AMT + EN-LF-ORIG-ALT-PRM-AMT +
              EN-AH-ORIG-PRM-AMT)
           MOVE WS-WORK-PREM           TO OUT-TOT-PRM-CHG

           IF EN-CSO-PORTION NOT NUMERIC
              MOVE ZEROS               TO EN-CSO-PORTION
           END-IF
           IF EN-ACCT-PORTION NOT NUMERIC
              MOVE ZERO                TO EN-ACCT-PORTION
           END-IF
110612*    MOVE EN-CSO-PORTION         TO OUT-CSO-PORTION
110612*    MOVE EN-ACCT-PORTION        TO OUT-ACCT-PORTION
110612     IF WS-WORK-PREM > +0
              MOVE 'CSO'               TO OUT-PAYEE
110612     ELSE
110612      IF WS-WORK-PREM = +0
110612         MOVE 'ZERO'             TO OUT-PAYEE
110612      ELSE
110612         MOVE 'ACCT'             TO OUT-PAYEE
110612      END-IF
           END-IF

110612     COMPUTE WS-ACCT-ORIG-LF-PORT = 
110612        EN-LF-ORIG-PRM-AMT * EN-LF-ORIG-COMM-PCT
110612     MOVE WS-ACCT-ORIG-LF-PORT TO OUT-ACCT-ORIG-LF-PORT
110612     COMPUTE WS-ACCT-ORIG-ALT-LF-PORT = 
110612        EN-LF-ORIG-ALT-PRM-AMT * EN-LF-ORIG-COMM-PCT
110612     MOVE WS-ACCT-ORIG-ALT-LF-PORT TO OUT-ACCT-ORIG-ALT-LF-PORT
110612     COMPUTE WS-ACCT-ORIG-CMB-LF-PORT = 
110612        (EN-LF-ORIG-PRM-AMT + EN-LF-ORIG-ALT-PRM-AMT) 
110612        * EN-LF-ORIG-COMM-PCT
110612     MOVE WS-ACCT-ORIG-CMB-LF-PORT TO OUT-ACCT-ORIG-CMB-LF-PORT
110612     COMPUTE WS-ACCT-ORIG-AH-PORT = 
110612        EN-AH-ORIG-PRM-AMT * EN-AH-ORIG-COMM-PCT
110612     MOVE WS-ACCT-ORIG-AH-PORT TO OUT-ACCT-ORIG-AH-PORT
110612     COMPUTE WS-ACCT-ORIG-PORTION =
110612        WS-ACCT-ORIG-CMB-LF-PORT + WS-ACCT-ORIG-AH-PORT
110612     MOVE WS-ACCT-ORIG-PORTION TO OUT-ACCT-ORIG-PORTION
110612
110612     COMPUTE WS-CSO-ORIG-LF-PORT = 
110612       EN-LF-ORIG-PRM-AMT - WS-ACCT-ORIG-LF-PORT
110612     MOVE WS-CSO-ORIG-LF-PORT TO OUT-CSO-ORIG-LF-PORT
110612     COMPUTE WS-CSO-ORIG-ALT-LF-PORT = 
110612       EN-LF-ORIG-ALT-PRM-AMT - WS-ACCT-ORIG-ALT-LF-PORT
110612     MOVE WS-CSO-ORIG-ALT-LF-PORT TO OUT-CSO-ORIG-ALT-LF-PORT
110612     COMPUTE WS-CSO-ORIG-CMB-LF-PORT = 
110612       (EN-LF-ORIG-PRM-AMT + EN-LF-ORIG-ALT-PRM-AMT) - 
110612        WS-ACCT-ORIG-CMB-LF-PORT
110612     MOVE WS-CSO-ORIG-CMB-LF-PORT TO OUT-CSO-ORIG-CMB-LF-PORT
110612     COMPUTE WS-CSO-ORIG-AH-PORT = 
110612        EN-AH-ORIG-PRM-AMT - WS-ACCT-ORIG-AH-PORT
110612     MOVE WS-CSO-ORIG-AH-PORT TO OUT-CSO-ORIG-AH-PORT
110612     COMPUTE WS-CSO-ORIG-PORTION = 
110612        WS-CSO-ORIG-CMB-LF-PORT + WS-CSO-ORIG-AH-PORT
110612     MOVE WS-CSO-ORIG-PORTION TO OUT-CSO-ORIG-PORTION

110612     COMPUTE WS-ACCT-NEW-LF-PORT = 
110612        EN-LF-NEW-PRM-AMT * EN-LF-NEW-COMM-PCT
110612     MOVE WS-ACCT-NEW-LF-PORT TO OUT-ACCT-NEW-LF-PORT
110612     COMPUTE WS-ACCT-NEW-ALT-LF-PORT = 
110612        EN-LF-NEW-ALT-PRM-AMT * EN-LF-NEW-COMM-PCT
110612     MOVE WS-ACCT-NEW-ALT-LF-PORT TO OUT-ACCT-NEW-ALT-LF-PORT
110612     COMPUTE WS-ACCT-NEW-CMB-LF-PORT = 
110612        (EN-LF-NEW-PRM-AMT + EN-LF-NEW-ALT-PRM-AMT) 
110612        * EN-LF-NEW-COMM-PCT
110612     MOVE WS-ACCT-NEW-CMB-LF-PORT TO OUT-ACCT-NEW-CMB-LF-PORT
110612     COMPUTE WS-ACCT-NEW-AH-PORT = 
110612        EN-AH-NEW-PRM-AMT * EN-AH-NEW-COMM-PCT
110612     MOVE WS-ACCT-NEW-AH-PORT TO OUT-ACCT-NEW-AH-PORT
110612     COMPUTE WS-ACCT-NEW-PORTION =
110612        WS-ACCT-NEW-CMB-LF-PORT + WS-ACCT-NEW-AH-PORT
110612     MOVE WS-ACCT-NEW-PORTION TO OUT-ACCT-NEW-PORTION
110612
110612     COMPUTE WS-CSO-NEW-LF-PORT = 
110612       EN-LF-NEW-PRM-AMT - WS-ACCT-NEW-LF-PORT
110612     MOVE WS-CSO-NEW-LF-PORT TO OUT-CSO-NEW-LF-PORT
110612     COMPUTE WS-CSO-NEW-ALT-LF-PORT = 
110612       EN-LF-NEW-ALT-PRM-AMT - WS-ACCT-NEW-ALT-LF-PORT
110612     MOVE WS-CSO-NEW-ALT-LF-PORT TO OUT-CSO-NEW-ALT-LF-PORT
110612     COMPUTE WS-CSO-NEW-CMB-LF-PORT = 
110612       (EN-LF-NEW-PRM-AMT + EN-LF-NEW-ALT-PRM-AMT) - 
110612        WS-ACCT-NEW-CMB-LF-PORT
110612     MOVE WS-CSO-NEW-CMB-LF-PORT TO OUT-CSO-NEW-CMB-LF-PORT
110612     COMPUTE WS-CSO-NEW-AH-PORT = 
110612        EN-AH-NEW-PRM-AMT - WS-ACCT-NEW-AH-PORT
110612     MOVE WS-CSO-NEW-AH-PORT TO OUT-CSO-NEW-AH-PORT
110612     COMPUTE WS-CSO-NEW-PORTION = 
110612        WS-CSO-NEW-CMB-LF-PORT + WS-CSO-NEW-AH-PORT
110612     MOVE WS-CSO-NEW-PORTION TO OUT-CSO-NEW-PORTION
110612
110612     COMPUTE WS-ACCT-LF-PORT-CHG =
110612        WS-ACCT-NEW-LF-PORT - WS-ACCT-ORIG-LF-PORT
110612     MOVE WS-ACCT-LF-PORT-CHG TO OUT-ACCT-LF-PORT-CHG
110612     COMPUTE WS-ACCT-ALT-LF-PORT-CHG =
110612        WS-ACCT-NEW-ALT-LF-PORT - WS-ACCT-ORIG-ALT-LF-PORT
110612     MOVE WS-ACCT-ALT-LF-PORT-CHG TO OUT-ACCT-ALT-LF-PORT-CHG
110612     COMPUTE WS-ACCT-CMB-LF-PORT-CHG =
110612        WS-ACCT-NEW-CMB-LF-PORT - WS-ACCT-ORIG-CMB-LF-PORT
110612     MOVE WS-ACCT-CMB-LF-PORT-CHG TO OUT-ACCT-CMB-LF-PORT-CHG
110612     COMPUTE WS-ACCT-AH-PORT-CHG =
110612        WS-ACCT-NEW-AH-PORT - WS-ACCT-ORIG-AH-PORT
110612     MOVE WS-ACCT-AH-PORT-CHG TO OUT-ACCT-AH-PORT-CHG
110612     COMPUTE WS-ACCT-PORTION-CHG =
110612        WS-ACCT-NEW-PORTION - WS-ACCT-ORIG-PORTION
110612     MOVE WS-ACCT-PORTION-CHG TO OUT-ACCT-PORTION-CHG
110612
110612     COMPUTE WS-CSO-LF-PORT-CHG =
110612        WS-CSO-NEW-LF-PORT - WS-CSO-ORIG-LF-PORT
110612     MOVE WS-CSO-LF-PORT-CHG TO OUT-CSO-LF-PORT-CHG
110612     COMPUTE WS-CSO-ALT-LF-PORT-CHG =
110612        WS-CSO-NEW-ALT-LF-PORT - WS-CSO-ORIG-ALT-LF-PORT
110612     MOVE WS-CSO-ALT-LF-PORT-CHG TO OUT-CSO-ALT-LF-PORT-CHG
110612     COMPUTE WS-CSO-CMB-LF-PORT-CHG =
110612        WS-CSO-NEW-CMB-LF-PORT - WS-CSO-ORIG-CMB-LF-PORT
110612     MOVE WS-CSO-CMB-LF-PORT-CHG TO OUT-CSO-CMB-LF-PORT-CHG
110612     COMPUTE WS-CSO-AH-PORT-CHG =
110612        WS-CSO-NEW-AH-PORT - WS-CSO-ORIG-AH-PORT
110612     MOVE WS-CSO-AH-PORT-CHG TO OUT-CSO-AH-PORT-CHG
110612     COMPUTE WS-CSO-PORTION-CHG =
110612        WS-CSO-NEW-PORTION - WS-CSO-ORIG-PORTION
110612     MOVE WS-CSO-PORTION-CHG TO OUT-CSO-PORTION-CHG
110612
110612     COMPUTE WS-WORK-PREM = 
110612         EN-LF-NEW-PRM-AMT - EN-LF-ORIG-PRM-AMT 
110612     MOVE WS-WORK-PREM           TO OUT-LF-PREM-CHG
110612     COMPUTE WS-WORK-PREM = 
110612         EN-LF-NEW-ALT-PRM-AMT - EN-LF-ORIG-ALT-PRM-AMT 
110612     MOVE WS-WORK-PREM           TO OUT-LF-ALT-PREM-CHG
110612     COMPUTE WS-WORK-PREM = 
110612        (EN-LF-NEW-PRM-AMT + EN-LF-NEW-ALT-PRM-AMT) - 
110612        (EN-LF-ORIG-PRM-AMT + EN-LF-ORIG-ALT-PRM-AMT)
110612     MOVE WS-WORK-PREM           TO OUT-LF-CMB-PREM-CHG
110612     COMPUTE WS-WORK-PREM = 
110612        (EN-AH-NEW-PRM-AMT - EN-AH-ORIG-PRM-AMT)
110612     MOVE WS-WORK-PREM           TO OUT-AH-PREM-CHG
110612


           move en-sig-sw              to out-sig-sw
           MOVE EN-HEALTH-APP          TO OUT-HEALTH-APP
072312     MOVE EN-COMM-CHGBK          TO OUT-CHGBACK

           GO TO 0620-CONTINUE

           .
       0620-PROCESS-CANCEL.

110612     IF EN-LF-ORIG-REF-DT NOT = LOW-VALUES AND SPACES
110612        MOVE EN-LF-ORIG-REF-DT   TO DC-BIN-DATE-1
110612        MOVE ' '                 TO DC-OPTION-CODE
110612        PERFORM 9700-DATE-LINK   THRU 9700-EXIT
110612        IF NO-CONVERSION-ERROR
110612           STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
110612              DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
110612                 INTO OUT-ORIG-LF-CAN-DT
110612           END-STRING
110612        END-IF
110612        MOVE OUT-ORIG-LF-CAN-DT  TO OUT-ORIG-CAN-DT
110612     END-IF
110612 
110612     IF EN-LF-NEW-REF-DT NOT = LOW-VALUES AND SPACES
110612        MOVE EN-LF-NEW-REF-DT    TO DC-BIN-DATE-1
110612        MOVE ' '                 TO DC-OPTION-CODE
110612        PERFORM 9700-DATE-LINK   THRU 9700-EXIT
110612        IF NO-CONVERSION-ERROR
110612           STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
110612              DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
110612                 INTO OUT-NEW-LF-CAN-DT
110612           END-STRING
110612        END-IF
110612        MOVE OUT-NEW-LF-CAN-DT   TO OUT-NEW-CAN-DT
110612     END-IF
110612 
110612     IF EN-AH-ORIG-REF-DT NOT = LOW-VALUES AND SPACES
110612        MOVE EN-AH-ORIG-REF-DT      TO DC-BIN-DATE-1
110612        MOVE ' '                    TO DC-OPTION-CODE
110612        PERFORM 9700-DATE-LINK      THRU 9700-EXIT
110612        IF NO-CONVERSION-ERROR
110612           STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
110612              DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
110612                 INTO OUT-ORIG-AH-CAN-DT
110612           END-STRING
110612        END-IF
110612        IF EN-AH-ORIG-REF-DT > EN-LF-ORIG-REF-DT
110612           MOVE OUT-ORIG-AH-CAN-DT  TO OUT-ORIG-CAN-DT
110612        END-IF
110612     END-IF
110612
110612     IF EN-AH-NEW-REF-DT NOT = LOW-VALUES AND SPACES
110612        MOVE EN-AH-NEW-REF-DT       TO DC-BIN-DATE-1
110612        MOVE ' '                    TO DC-OPTION-CODE
110612        PERFORM 9700-DATE-LINK      THRU 9700-EXIT
110612        IF NO-CONVERSION-ERROR
110612           STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
110612              DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
110612                 INTO OUT-NEW-AH-CAN-DT
110612           END-STRING
110612        END-IF
110612        IF EN-AH-NEW-REF-DT > EN-LF-NEW-REF-DT
110612           MOVE OUT-NEW-AH-CAN-DT  TO OUT-NEW-CAN-DT
110612        END-IF
110612     END-IF

           MOVE EN-LF-ORIG-REF-AMT     TO OUT-ORIG-LF-REF
           MOVE EN-AH-ORIG-REF-AMT     TO OUT-ORIG-AH-REF
           MOVE EN-LF-NEW-REF-AMT      TO OUT-NEW-LF-REF
           MOVE EN-AH-NEW-REF-AMT      TO OUT-NEW-AH-REF
           COMPUTE WS-WORK-PREM =
              EN-LF-ORIG-REF-AMT + EN-AH-ORIG-REF-AMT
           MOVE WS-WORK-PREM           TO OUT-ORIG-TOT-REF
072312     COMPUTE WS-WORK-PREM =
072312        EN-LF-NEW-REF-AMT + EN-AH-NEW-REF-AMT
072312     MOVE WS-WORK-PREM           TO OUT-NEW-TOT-REF

           COMPUTE WS-WORK-PREM =
              (EN-LF-NEW-REF-AMT + EN-AH-NEW-REF-AMT) -
              (EN-LF-ORIG-REF-AMT + EN-AH-ORIG-REF-AMT)
           MOVE WS-WORK-PREM           TO OUT-TOT-REF-CHG
110612
110612     COMPUTE WS-WORK-PREM = 
110612        (EN-LF-NEW-REF-AMT - EN-LF-ORIG-REF-AMT)
110612     MOVE WS-WORK-PREM           TO OUT-LF-REF-CHG
110612     COMPUTE WS-WORK-PREM = 
110612        (EN-AH-NEW-REF-AMT - EN-AH-ORIG-REF-AMT)
110612     MOVE WS-WORK-PREM           TO OUT-AH-REF-CHG
110612     
110612*     MOVE EN-CSO-PORTION         TO OUT-CSO-PORTION
110612*     MOVE EN-ACCT-PORTION        TO OUT-ACCT-PORTION
072312     MOVE EN-COMM-CHGBK          TO OUT-CHGBACK
110612
110612     IF EN-COMM-CHGBK = 'N'
116012         MOVE ZERO              TO OUT-CSO-ORIG-LF-PORT
110612                                   OUT-CSO-ORIG-ALT-LF-PORT
110612         MOVE EN-LF-ORIG-REF-AMT TO OUT-CSO-ORIG-CMB-LF-PORT
110612         MOVE EN-AH-ORIG-REF-AMT TO OUT-CSO-ORIG-AH-PORT
110612         COMPUTE WS-CSO-ORIG-PORTION = 
110612             EN-LF-ORIG-REF-AMT + EN-AH-ORIG-REF-AMT
110612         MOVE WS-CSO-ORIG-PORTION TO OUT-CSO-ORIG-PORTION
110612         MOVE ZERO              TO OUT-ACCT-ORIG-LF-PORT
110612                                   OUT-ACCT-ORIG-ALT-LF-PORT
110612                                   OUT-ACCT-ORIG-CMB-LF-PORT
110612                                   OUT-ACCT-ORIG-AH-PORT
110612                                   OUT-ACCT-ORIG-PORTION
110612
116012         MOVE ZERO              TO OUT-CSO-NEW-LF-PORT
110612                                   OUT-CSO-NEW-ALT-LF-PORT
110612         MOVE EN-LF-NEW-REF-AMT TO OUT-CSO-NEW-CMB-LF-PORT
110612         MOVE EN-AH-NEW-REF-AMT TO OUT-CSO-NEW-AH-PORT
110612         COMPUTE WS-CSO-NEW-PORTION = 
110612             EN-LF-NEW-REF-AMT + EN-AH-NEW-REF-AMT
110612         MOVE WS-CSO-NEW-PORTION TO OUT-CSO-NEW-PORTION
110612         MOVE ZERO              TO OUT-ACCT-NEW-LF-PORT
110612                                   OUT-ACCT-NEW-ALT-LF-PORT
110612                                   OUT-ACCT-NEW-CMB-LF-PORT
110612                                   OUT-ACCT-NEW-AH-PORT
110612                                   OUT-ACCT-NEW-PORTION
110612
116012         MOVE ZERO              TO OUT-CSO-LF-PORT-CHG 
110612                                   OUT-CSO-ALT-LF-PORT-CHG
110612         COMPUTE WS-CSO-CMB-LF-PORT-CHG =
110612           EN-LF-NEW-REF-AMT - EN-LF-ORIG-REF-AMT
116012         MOVE WS-CSO-CMB-LF-PORT-CHG TO OUT-CSO-CMB-LF-PORT-CHG 
110612         COMPUTE WS-CSO-AH-PORT-CHG =
110612           EN-AH-NEW-REF-AMT - EN-AH-ORIG-REF-AMT
116012         MOVE WS-CSO-AH-PORT-CHG TO OUT-CSO-AH-PORT-CHG 
110612         COMPUTE WS-CSO-PORTION-CHG =
110612           (EN-LF-NEW-REF-AMT + EN-AH-NEW-REF-AMT) -
110612           (EN-LF-ORIG-REF-AMT + EN-AH-ORIG-REF-AMT)
116012         MOVE WS-CSO-PORTION-CHG TO OUT-CSO-PORTION-CHG 
110612         MOVE ZERO              TO OUT-ACCT-LF-PORT-CHG
110612                                   OUT-ACCT-ALT-LF-PORT-CHG
110612                                   OUT-ACCT-CMB-LF-PORT-CHG
110612                                   OUT-ACCT-AH-PORT-CHG
110612                                   OUT-ACCT-PORTION-CHG
110612     ELSE
110612         MOVE ZERO              TO OUT-ACCT-ORIG-LF-PORT
110612                                   OUT-ACCT-ORIG-ALT-LF-PORT
110612         COMPUTE WS-ACCT-ORIG-CMB-LF-PORT = 
110612           (EN-LF-ORIG-REF-AMT * EN-LF-ORIG-REF-COMM-PCT)
110612         MOVE WS-ACCT-ORIG-CMB-LF-PORT TO 
110612                                  OUT-ACCT-ORIG-CMB-LF-PORT
110612         COMPUTE WS-ACCT-ORIG-AH-PORT = 
110612           (EN-AH-ORIG-REF-AMT * EN-AH-ORIG-REF-COMM-PCT)
110612         MOVE WS-ACCT-ORIG-AH-PORT TO OUT-ACCT-ORIG-AH-PORT
110612         COMPUTE WS-ACCT-ORIG-PORTION =
110612            WS-ACCT-ORIG-CMB-LF-PORT + WS-ACCT-ORIG-AH-PORT
110612         MOVE WS-ACCT-ORIG-PORTION TO OUT-ACCT-ORIG-PORTION
110612
116012         MOVE ZERO              TO OUT-CSO-ORIG-LF-PORT
110612                                   OUT-CSO-ORIG-ALT-LF-PORT
110612         COMPUTE WS-CSO-ORIG-CMB-LF-PORT = 
110612           (EN-LF-ORIG-REF-AMT - WS-ACCT-ORIG-CMB-LF-PORT)
110612         MOVE WS-CSO-ORIG-CMB-LF-PORT TO OUT-CSO-ORIG-CMB-LF-PORT
110612         COMPUTE WS-CSO-ORIG-AH-PORT = 
110612           (EN-AH-ORIG-REF-AMT - WS-ACCT-ORIG-AH-PORT)
110612         MOVE WS-CSO-AH-PORTION  TO OUT-CSO-ORIG-AH-PORT
110612         COMPUTE WS-CSO-ORIG-PORTION = 
110612            WS-CSO-ORIG-CMB-LF-PORT + WS-CSO-ORIG-AH-PORT
110612         MOVE WS-CSO-ORIG-PORTION TO OUT-CSO-ORIG-PORTION
110612
110612         MOVE ZERO              TO OUT-ACCT-NEW-LF-PORT
110612                                   OUT-ACCT-NEW-ALT-LF-PORT
110612         COMPUTE WS-ACCT-NEW-CMB-LF-PORT = 
110612           (EN-LF-NEW-REF-AMT * EN-LF-NEW-REF-COMM-PCT)
110612         MOVE WS-ACCT-NEW-CMB-LF-PORT TO OUT-ACCT-NEW-CMB-LF-PORT
110612         COMPUTE WS-ACCT-NEW-AH-PORT = 
110612           (EN-AH-NEW-REF-AMT * EN-AH-NEW-REF-COMM-PCT)
110612         MOVE WS-ACCT-NEW-AH-PORT TO OUT-ACCT-NEW-AH-PORT
110612         COMPUTE WS-ACCT-NEW-PORTION =
110612            WS-ACCT-NEW-CMB-LF-PORT + WS-ACCT-NEW-AH-PORT
110612         MOVE WS-ACCT-NEW-PORTION TO OUT-ACCT-NEW-PORTION
110612
116012         MOVE ZERO              TO OUT-CSO-NEW-LF-PORT
110612                                   OUT-CSO-NEW-ALT-LF-PORT
110612         COMPUTE WS-CSO-NEW-CMB-LF-PORT = 
110612           (EN-LF-NEW-REF-AMT - WS-ACCT-NEW-CMB-LF-PORT)
110612         MOVE WS-CSO-NEW-CMB-LF-PORT TO OUT-CSO-NEW-CMB-LF-PORT
110612         COMPUTE WS-CSO-NEW-AH-PORT = 
110612           (EN-AH-NEW-REF-AMT - WS-ACCT-NEW-AH-PORT)
110612         MOVE WS-CSO-NEW-AH-PORT TO OUT-CSO-NEW-AH-PORT
110612         COMPUTE WS-CSO-NEW-PORTION = 
110612            WS-CSO-NEW-CMB-LF-PORT + WS-CSO-NEW-AH-PORT
110612         MOVE WS-CSO-NEW-PORTION TO OUT-CSO-NEW-PORTION
110612
110612         MOVE ZERO               TO OUT-ACCT-LF-PORT-CHG
110612                                    OUT-ACCT-ALT-LF-PORT-CHG
110612         COMPUTE WS-ACCT-CMB-LF-PORT-CHG =
110612            WS-ACCT-NEW-CMB-LF-PORT - WS-ACCT-ORIG-CMB-LF-PORT
110612         MOVE WS-ACCT-CMB-LF-PORT-CHG TO OUT-ACCT-CMB-LF-PORT-CHG
110612         COMPUTE WS-ACCT-AH-PORT-CHG =
110612            WS-ACCT-NEW-AH-PORT - WS-ACCT-ORIG-AH-PORT
110612         MOVE WS-ACCT-AH-PORT-CHG TO OUT-ACCT-AH-PORT-CHG
110612         COMPUTE WS-ACCT-PORTION-CHG =
110612            WS-ACCT-NEW-PORTION - WS-ACCT-ORIG-PORTION
110612         MOVE WS-ACCT-PORTION-CHG TO OUT-ACCT-PORTION-CHG
110612
110612         MOVE ZERO               TO OUT-CSO-LF-PORT-CHG
110612                                    OUT-CSO-ALT-LF-PORT-CHG
110612         COMPUTE WS-CSO-CMB-LF-PORT-CHG =
110612            WS-CSO-NEW-CMB-LF-PORT - WS-CSO-ORIG-CMB-LF-PORT
110612         MOVE WS-CSO-CMB-LF-PORT-CHG TO OUT-CSO-CMB-LF-PORT-CHG
110612         COMPUTE WS-CSO-AH-PORT-CHG =
110612            WS-CSO-NEW-AH-PORT - WS-CSO-ORIG-AH-PORT
110612         MOVE WS-CSO-AH-PORT-CHG TO OUT-CSO-AH-PORT-CHG
110612         COMPUTE WS-CSO-PORTION-CHG =
110612            WS-CSO-NEW-PORTION - WS-CSO-ORIG-PORTION
110612         MOVE WS-CSO-PORTION-CHG TO OUT-CSO-PORTION-CHG
110612
110612     END-IF

           .
       0620-CONTINUE.

           move en-reason-code (1)     to out-rea-cd1
           move en-reason-code (2)     to out-rea-cd2
           move en-reason-code (3)     to out-rea-cd3
           move en-reason-code (4)     to out-rea-cd4
           move en-reason-code (5)     to out-rea-cd5
           move en-reason-code (6)     to out-rea-cd6
           move en-reason-code (7)     to out-rea-cd7
           move en-reason-code (8)     to out-rea-cd8
           move en-reason-code (9)     to out-rea-cd9
           move en-reason-code (10)    to out-rea-cd10
           move en-reason-code (11)    to out-rea-cd11
           move en-reason-code (12)    to out-rea-cd12

      *    PERFORM 1300-DETERMINE-COMM
      *                                THRU 1300-EXIT

           .
       0620-EXIT.
           EXIT.

       0650-PROCESS-NO-Change.

           IF EN-REC-TYPE = 'C'
              GO TO 0650-PROCESS-CANCEL
           END-IF


           if ((OUT-NEW-ILNAME = OUT-ORIG-ILNAME)
              AND (OUT-NEW-ILNAME NOT = SPACES))
                            AND
              ((OUT-NEW-IFNAME = OUT-ORIG-IFNAME)
              AND (OUT-NEW-IFNAME NOT = SPACES))
072312                      AND
072312        ((OUT-NEW-IMINIT = OUT-ORIG-MINIT)
072312        AND (OUT-NEW-IFNAME NOT = SPACES))
              move 'No Change'         to out-new-Ifname
072312        MOVE SPACES              TO OUT-NEW-IMINIT
              move spaces              to out-new-Ilname
           ELSE
               IF OUT-NEW-ILNAME = SPACES 
                 AND OUT-NEW-IFNAME = SPACES
072312           AND OUT-NEW-IMINIT = SPACES
                 AND OUT-ORIG-ILNAME NOT = SPACES
                  MOVE 'None'          TO OUT-NEW-IFNAME
072312            MOVE SPACES          TO OUT-NEW-IMINIT
                  MOVE SPACES          TO OUT-NEW-ILNAME
               END-IF
           END-IF

           IF ((OUT-NEW-JLNAME = OUT-ORIG-JLNAME)
              AND (OUT-NEW-JLNAME NOT = SPACES))
                          AND
              ((OUT-NEW-JFNAME = OUT-ORIG-JFNAME)
              AND (OUT-NEW-JFNAME NOT = SPACES))
072312                    AND
072312        ((OUT-NEW-JMINIT = OUT-ORIG-JMINIT)
091312        AND (OUT-NEW-JFNAME NOT = SPACES))
              MOVE 'No Change'         TO OUT-NEW-JFNAME
072312        MOVE SPACES              TO OUT-NEW-JMINIT
              MOVE SPACES              TO OUT-NEW-JLNAME
           ELSE
               IF OUT-NEW-JLNAME = SPACES 
                 AND OUT-NEW-JFNAME = SPACES
072312           AND OUT-NEW-JMINIT = SPACES
                 AND OUT-ORIG-JLNAME NOT = SPACES
                  MOVE 'None'          TO OUT-NEW-JFNAME
072312            MOVE SPACES          TO OUT-NEW-JMINIT
                  MOVE SPACES          TO OUT-NEW-JLNAME
               END-IF
           END-IF

           IF (EN-LF-NEW-BEN-AMT = EN-LF-ORIG-BEN-AMT)
              AND (EN-LF-NEW-BENCD NOT EQUAL '00' AND SPACES)
              MOVE 'No Change'         TO OUT-NEW-LF-BEN-A
           ELSE
               IF (EN-LF-ORIG-BENCD NOT = '00' AND SPACES)
                 AND (EN-LF-NEW-BENCD = '00' OR SPACES)
                   MOVE ZEROS          TO OUT-NEW-LF-BEN
               END-IF
           END-IF

           IF (EN-LF-NEW-PRM-AMT = EN-LF-ORIG-PRM-AMT)
              AND (EN-LF-NEW-BENCD NOT EQUAL '00' AND SPACES)
              MOVE 'No Change'         TO OUT-NEW-LF-PRM-A
           ELSE
               IF (EN-LF-ORIG-BENCD NOT = '00' AND SPACES)
                 AND (EN-LF-NEW-BENCD = '00' OR SPACES)
                   MOVE ZEROS          TO OUT-NEW-LF-PRM
               END-IF
           END-IF

           IF (EN-LF-NEW-ALT-BEN-AMT = EN-LF-ORIG-ALT-BEN-AMT)
              AND (EN-LF-NEW-BENCD NOT EQUAL '00' AND SPACES)
              MOVE 'No Change'         TO OUT-NEW-LF-ALT-BEN-A
           ELSE
               IF (EN-LF-ORIG-BENCD NOT = '00' AND SPACES)
                 AND (EN-LF-NEW-BENCD = '00' OR SPACES)
                   MOVE ZEROS          TO OUT-NEW-LF-ALT-BEN
               END-IF
           END-IF

           IF (EN-LF-NEW-ALT-PRM-AMT = EN-LF-ORIG-ALT-PRM-AMT)
              AND (EN-LF-NEW-BENCD NOT EQUAL '00' AND SPACES)
              MOVE 'No Change'         TO OUT-NEW-LF-ALT-PRM-A
           ELSE
               IF (EN-LF-ORIG-BENCD NOT = '00' AND SPACES)
                 AND (EN-LF-NEW-BENCD = '00' OR SPACES)
                   MOVE ZEROS          TO OUT-NEW-LF-ALT-PRM
               END-IF
           END-IF
110612
110612     COMPUTE WS-WRK-AMOUNT = EN-LF-ORIG-PRM-AMT +
110612                              EN-LF-ORIG-ALT-PRM-AMT
110612     COMPUTE WS-WRK-AMOUNT2 = EN-LF-NEW-PRM-AMT +
110612                               EN-LF-NEW-ALT-PRM-AMT
110612     IF (WS-WRK-AMOUNT2 = WS-WRK-AMOUNT)
110612        AND (EN-LF-NEW-BENCD NOT EQUAL '00' AND SPACES)
110612        MOVE 'No Change'         TO OUT-NEW-LF-CMB-PREM-A
110612     ELSE
110612         IF (EN-LF-ORIG-BENCD NOT = '00' AND SPACES)
110612           AND (EN-LF-NEW-BENCD = '00' OR SPACES)
110612            MOVE ZEROS          TO OUT-NEW-LF-CMB-PREM
110612         END-IF
110612     END-IF

           IF OUT-NEW-SCHED-EXP-DT = OUT-ORIG-SCHED-EXP-DT
              MOVE 'No Change'         TO OUT-NEW-SCHED-EXP-DT
           END-IF

           IF (OUT-NEW-LF-EXP-DT = OUT-ORIG-LF-EXP-DT)
              AND (OUT-NEW-LF-EXP-DT NOT = SPACES)
              MOVE 'No Change'         TO OUT-NEW-LF-EXP-DT
           END-IF

           IF (OUT-NEW-LF-DESC = OUT-ORIG-LF-DESC)
              AND (OUT-NEW-LF-DESC NOT = SPACES)
              MOVE 'No Change'         to OUT-NEW-LF-DESC
           ELSE
              IF (EN-LF-ORIG-BENCD NOT = '00' AND SPACES)
                AND (EN-LF-NEW-BENCD = '00' OR SPACES)
                  MOVE 'None'          TO OUT-NEW-LF-DESC
              END-IF
           end-if

           IF (OUT-NEW-AH-EXP-DT = OUT-ORIG-AH-EXP-DT)
             AND (OUT-NEW-AH-EXP-DT NOT = SPACES)
              MOVE 'No Change'          TO OUT-NEW-AH-EXP-DT
           END-IF

           IF (EN-AH-NEW-BEN-AMT = EN-AH-ORIG-BEN-AMT)
              AND (EN-AH-ORIG-BEN-AMT NOT = ZEROS)
              MOVE 'No Change'         TO OUT-NEW-AH-BEN-A
           ELSE
               IF (EN-AH-ORIG-BENCD NOT = '00' AND SPACES)
                 AND (EN-AH-NEW-BENCD = '00' OR SPACES)
                   MOVE ZEROS          TO OUT-NEW-AH-BEN
               END-IF
           END-IF

           IF (EN-AH-NEW-PRM-AMT = EN-AH-ORIG-PRM-AMT)
              AND (EN-AH-ORIG-PRM-AMT NOT = ZEROS)
              MOVE 'No Change'         TO OUT-NEW-AH-PRM-A
           ELSE
               IF (EN-AH-ORIG-BENCD NOT = '00' AND SPACES)
                 AND (EN-AH-NEW-BENCD = '00' OR SPACES)
                   MOVE ZEROS          TO OUT-NEW-AH-PRM
               END-IF
           END-IF

           IF (OUT-NEW-AH-DESC = OUT-ORIG-AH-DESC)
              AND (OUT-ORIG-AH-DESC NOT = SPACES)
              MOVE 'No Change'         TO OUT-NEW-AH-DESC
           ELSE
               IF (EN-AH-ORIG-BENCD NOT = '00' AND SPACES)
                 AND (EN-AH-NEW-BENCD = '00' OR SPACES)
                   MOVE 'None'         TO OUT-NEW-AH-DESC
               END-IF
           END-IF

011314     IF EN-INS-ORIG-AGE-DEF-FLAG <> 'Y'
011314         MOVE 'N' TO EN-INS-ORIG-AGE-DEF-FLAG
011314     END-IF
011314     IF EN-INS-NEW-AGE-DEF-FLAG <> 'Y'
011314         MOVE 'N' TO EN-INS-NEW-AGE-DEF-FLAG
011314     END-IF
011314     IF EN-JNT-ORIG-AGE-DEF-FLAG <> 'Y'
011314         MOVE 'N' TO EN-JNT-ORIG-AGE-DEF-FLAG
011314     END-IF
011314     IF EN-JNT-NEW-AGE-DEF-FLAG <> 'Y'
011314         MOVE 'N' TO EN-JNT-NEW-AGE-DEF-FLAG
011314     END-IF
011314
           IF EN-INS-NEW-AGE = EN-INS-ORIG-AGE
121712       AND EN-INS-NEW-AGE-DEF-FLAG = EN-INS-ORIG-AGE-DEF-FLAG
091213       AND EN-INS-NEW-AGE-DEF-FLAG <> 'Y'
091213       AND EN-INS-NEW-LAST-NAME = EN-INS-ORIG-LAST-NAME
091213       AND EN-INS-NEW-FIRST-NAME = EN-INS-ORIG-FIRST-NAME
091213       AND EN-INS-NEW-MIDDLE-INIT = EN-INS-ORIG-MIDDLE-INIT
              MOVE 'No Change'         TO OUT-NEW-IAGE-A
           END-IF

           IF (EN-JNT-NEW-AGE = EN-JNT-ORIG-AGE)
              AND (EN-JNT-ORIG-AGE NOT = ZEROS)
121712        AND EN-JNT-NEW-AGE-DEF-FLAG = EN-JNT-ORIG-AGE-DEF-FLAG
091213        AND EN-JNT-NEW-AGE-DEF-FLAG <> 'Y'
091213        AND EN-JNT-NEW-LAST-NAME = EN-JNT-ORIG-LAST-NAME
091213        AND EN-JNT-NEW-FIRST-NAME = EN-JNT-ORIG-FIRST-NAME
091213        AND EN-JNT-NEW-MIDDLE-INIT = EN-JNT-ORIG-MIDDLE-INIT
              MOVE 'No Change'         TO OUT-NEW-JAGE-A
           ELSE
091213         IF (EN-JNT-ORIG-AGE > ZEROS)
011314           AND (EN-JNT-ORIG-AGE-DEF-FLAG <> 'Y')
                 AND (EN-JNT-NEW-AGE = ZEROS OR SPACES)
                   MOVE 'None'         TO OUT-NEW-JAGE-A
               END-IF
           END-IF

           IF (EN-LF-NEW-TERM = EN-LF-ORIG-TERM)
              AND (EN-LF-ORIG-TERM NOT = ZEROS)
              MOVE 'No Change'         TO OUT-NEW-LF-TERM-A
           ELSE
               IF (EN-LF-ORIG-BENCD NOT = '00' AND SPACES)
                 AND (EN-LF-NEW-BENCD = '00' OR SPACES)
                   MOVE ZEROS          TO OUT-NEW-LF-TERM
               END-IF
           END-IF

           IF (EN-AH-NEW-TERM = EN-AH-ORIG-TERM)
              AND (EN-AH-ORIG-TERM NOT = ZEROS)
              MOVE 'No Change'         TO OUT-NEW-AH-TERM-A
           ELSE
               IF (EN-AH-ORIG-BENCD NOT = '00' AND SPACES)
                 AND (EN-AH-NEW-BENCD = '00' OR SPACES)
                   MOVE ZEROS          TO OUT-NEW-AH-TERM
               END-IF
           END-IF

           IF (EN-AH-NEW-CP = EN-AH-ORIG-CP)
              AND (EN-AH-NEW-BENCD NOT EQUAL '00' AND SPACES)
              MOVE 'No Change'         TO OUT-NEW-CRIT-PER-A
                                          OUT-NEW-MAX-PMTS-A
           ELSE
               IF (EN-AH-ORIG-BENCD NOT = '00' AND SPACES)
                 AND (EN-AH-NEW-BENCD = '00' OR SPACES)
                   MOVE 'None'         TO OUT-NEW-CRIT-PER-A
                                          OUT-NEW-MAX-PMTS-A
               END-IF
           END-IF

           IF OUT-NEW-SCHED-TERM = OUT-ORIG-SCHED-TERM
              MOVE 'No Change'         TO OUT-NEW-SCHED-TERM-A
           END-IF

           IF (OUT-NEW-WAIT-PER = OUT-ORIG-WAIT-PER)
             AND (OUT-ORIG-WAIT-PER NOT = SPACES)
               MOVE 'No Change'        TO OUT-NEW-WAIT-PER
           ELSE
               IF (EN-AH-ORIG-BENCD NOT = '00' AND SPACES)
                 AND (EN-AH-NEW-BENCD = '00' OR SPACES)
                   MOVE 'None'         TO OUT-NEW-WAIT-PER
               END-IF
           END-IF

           .
       0650-PROCESS-CANCEL.

           .
       0650-EXIT.
           EXIT.

       0700-WRITE-EXTR.

           PERFORM 1000-BUILD-ERARCH   THRU 1000-EXIT
           IF BL-FAIL
              MOVE ' COULDNT BUILD ERARCH ' TO BL-MESSAGE
              GO TO 0700-EXIT
           END-IF
           MOVE WS-COMPANY-CD          TO NSAS-COMPANY-CD
           MOVE BL-ARCHIVE-NO          TO NSAS-ARCHIVE-NO
           MOVE +0                     TO NSAS-SEQ-NO
           PERFORM 0710-BUILD-NSASEXTR THRU 0710-EXIT
           PERFORM 0720-WRITE-NSASEXTR THRU 0720-EXIT
           IF BL-FAIL
              MOVE ' COULDNT BUILD STD NSASEXTR ' TO BL-MESSAGE
              GO TO 0700-EXIT
           END-IF
           IF W-LETTER-TO-ACCT NOT = SPACES
              MOVE W-LETTER-TO-ACCT   TO NSAS-LETTER-VARIABLES (5:1)
              MOVE WS-COMPANY-CD       TO NSAS-COMPANY-CD
              MOVE BL-ARCHIVE-NO       TO NSAS-ARCHIVE-NO
              MOVE +1                  TO NSAS-SEQ-NO
              PERFORM 0720-WRITE-NSASEXTR
                                       THRU 0720-EXIT
              IF BL-FAIL
                 MOVE ' COULDNT BUILD ACCT NSASEXTR ' TO BL-MESSAGE
                 GO TO 0700-EXIT
              END-IF
           END-IF

           IF W-LETTER-TO-BENE NOT = SPACES
              MOVE W-LETTER-TO-BENE   TO NSAS-LETTER-VARIABLES (5:1)
              MOVE WS-COMPANY-CD       TO NSAS-COMPANY-CD
              MOVE BL-ARCHIVE-NO       TO NSAS-ARCHIVE-NO
              MOVE +2                  TO NSAS-SEQ-NO
              PERFORM 0720-WRITE-NSASEXTR
                                       THRU 0720-EXIT
              IF BL-FAIL
                 MOVE ' COULDNT BUILD BENE NSASEXTR ' TO BL-MESSAGE
              END-IF
           END-IF

122911     IF W-PRINT-CERTIFICATE = 'Y' AND
122911        BL-STATE = 'VA'
122911           MOVE OUT-ACCT-NAME       TO VD-ACCOUNT-NAME
122911           MOVE WS-COMPANY-CD       TO VD-COMPANY-CD
122911           MOVE BL-CARRIER          TO VD-CARRIER
122911           MOVE BL-GROUP            TO VD-GROUPING
122911           MOVE BL-STATE            TO VD-STATE
122911           MOVE BL-ACCOUNT          TO VD-ACCOUNT
122911           MOVE WS-ELCERT-EFF-DT    TO VD-CERT-EFF-DT
122911           MOVE BL-CERT-NO          TO VD-CERT-NO
122911           MOVE OUT-ILNAME          TO VD-INSURED-LAST-NAME
122911           MOVE OUT-IFNAME          TO VD-INSURED-FIRST-NAME
122911           MOVE OUT-IMINIT          TO VD-INSURED-MIDDLE-INIT
122911           MOVE OUT-JLNAME          TO VD-JOINT-LAST-NAME
122911           MOVE OUT-JFNAME          TO VD-JOINT-FIRST-NAME
122911           MOVE OUT-JMINIT          TO VD-JOINT-MIDDLE-INIT
122911           MOVE OUT-BENE-NAME       TO VD-BENEFICIARY-NAME
122911           MOVE OUT-ENTRY-BATCH     TO VD-ENTRY-BATCH
122911           MOVE OUT-CSR-CODE        TO VD-CSR-ID
122911           MOVE CM-LF-BENEFIT-CD    TO VD-LF-BENEFIT-CD
122911           MOVE CM-LF-PREMIUM-AMT   TO VD-LF-PREMIUM-AMT
122911           MOVE CM-LF-BENEFIT-AMT   TO VD-LF-BENEFIT-AMT
122911           MOVE CM-LF-ORIG-TERM     TO VD-LF-TERM
122911           MOVE CM-LF-PREMIUM-RATE  TO VD-LF-RATE
122911           MOVE CM-AH-BENEFIT-CD    TO VD-AH-BENEFIT-CD
122911           MOVE CM-AH-PREMIUM-AMT   TO VD-AH-PREMIUM-AMT
100412           MOVE CM-AH-BENEFIT-AMT   TO VD-AH-BENEFIT-AMT
122911           MOVE CM-AH-ORIG-TERM     TO VD-AH-TERM
122911           MOVE CM-AH-PREMIUM-RATE  TO VD-AH-RATE
122911           MOVE CM-LOAN-TERM        TO VD-LOAN-TERM
122911           MOVE CM-LOAN-APR         TO VD-LOAN-APR
122911           MOVE CM-LOAN-1ST-PMT-DT  TO VD-1ST-PMT-DT
122911           MOVE OUT-INS-ADDR1       TO VD-INSURED-ADDRESS-1
122911           MOVE OUT-INS-ADDR2       TO VD-INSURED-ADDRESS-2
122911           MOVE OUT-INS-CITY        TO VD-INSURED-CITY
122911           MOVE OUT-INS-STATE       TO VD-INSURED-STATE
122911           MOVE OUT-INS-ZIP         TO VD-INSURED-ZIP-CODE
122911           MOVE 'VADS'              TO VD-LETTER-ID
122911           MOVE OUT-PROC-ID         TO VD-PROC-ID
122911           MOVE BL-COMP-ID          TO VD-COMP-ID
122911           MOVE EIBTIME             TO VD-CURRENT-TIME         
122911           MOVE SAVE-BIN-DATE       TO VD-CURRENT-DATE
122911           MOVE BL-ARCHIVE-NO       TO VD-ARCHIVE-NO
122911           MOVE VIRGINIA-DISCLOSURE TO WS-PASS-VADS-REC
122911           EXEC CICS LINK
122911               PROGRAM    (LINK-ELVADS)
122911               COMMAREA   (WS-PASS-AREA)
122911               LENGTH     (WS-PASS-AREA-LENGTH)
122911           END-EXEC
122911     END-IF

           .
       0700-EXIT.
           EXIT.

        0710-BUILD-NSASEXTR.

      ***  i added the extra space in front of the 1st ~
      ***  only because there may be a letter to acct or bene

           STRING
               OUT-LETTER             ' ~'
               OUT-PROC-ID            '~'
               OUT-PROC-NAME          '~'
               OUT-PROC-TITLE         '~' 
               OUT-CSR-NAME           '~' 
               OUT-CSR-TITLE          '~' 
               OUT-CARRIER            '~' 
               OUT-GROUPING           '~' 
               OUT-STATE              '~' 
               OUT-ACCOUNT            '~' 
               OUT-CERT-EFF-DT        '~' 
               OUT-CERT-NO            '~' 
               OUT-CERT-SFX           '~' 
               OUT-ILNAME             '~' 
               OUT-IFNAME             '~' 
               OUT-IFINIT             '~' 
               OUT-IMINIT             '~' 
               OUT-IAGE               '~' 
               OUT-ISEX               '~' 
               OUT-INS-ADDR1          '~' 
               OUT-INS-ADDR2          '~' 
               OUT-INS-CITY           '~' 
               OUT-INS-STATE          '~' 
               OUT-INS-ZIP            '~' 
               OUT-SOC-SEC-NO         '~' 
               OUT-MEMBER-NO          '~' 
               OUT-JLNAME             '~' 
               OUT-JFNAME             '~' 
               OUT-JMINIT             '~' 
               OUT-JAGE               '~' 
               OUT-ACCT-NAME          '~' 
               OUT-ACCT-ADDR1         '~' 
               OUT-ACCT-ADDR2         '~' 
               OUT-ACCT-CITY          '~' 
               OUT-ACCT-STATE         '~' 
               OUT-ACCT-ZIP           '~' 
               OUT-ACCT-PHONE         '~' 
               OUT-ACCT-CNTRL-NAME    '~' 
               OUT-ACCT-BUS-TYPE      '~' 
               OUT-BENE-NAME          '~' 
               OUT-BENE-ADDR1         '~' 
               OUT-BENE-ADDR2         '~' 
               OUT-BENE-CITY          '~' 
               OUT-BENE-STATE         '~' 
               OUT-BENE-ZIP           '~' 
               OUT-CARR-NAME          '~' 
               OUT-RESP-NO            '~' 
               OUT-COMP-NAME          '~' 
               OUT-COMP-MAIL-TO       '~' 
               OUT-COMP-ADDR1         '~' 
               OUT-COMP-ADDR2         '~' 
               OUT-COMP-CITY          '~' 
               OUT-COMP-STATE         '~' 
               OUT-COMP-ZIP           '~' 
               OUT-COMP-PHONE         '~' 
               OUT-COMP-FAX           '~' 
               OUT-COMP-STATUS        '~' 
               OUT-BILL-SW            '~' 
               OUT-RPT-CD1            '~' 
               OUT-RPT-CD2            '~' 
               OUT-ENTRY-DT           '~' 
               OUT-ENTRY-BATCH        '~' 
               OUT-ENTRY-STATUS       '~' 
               OUT-1ST-PMT-DT         '~' 
               OUT-LOAN-APR           '~' 
               OUT-LOAN-TERM          '~' 
               OUT-RATE-CLASS         '~' 
               OUT-EXT-DAYS           '~' 
               OUT-CSR-CODE           '~' 
               OUT-UCODE              '~' 
               OUT-PREM-TYPE          '~' 
               OUT-IND-GRP            '~' 
               OUT-SKIP-CD            '~' 
               OUT-PMT-MODE           '~' 
               OUT-LOAN-OFF           '~' 
               OUT-REIN-TABLE         '~' 
               OUT-SPEC-REIN          '~' 
               OUT-LF-BENCD           '~' 
               OUT-LF-TERM            '~' 
               OUT-LF-DEV-CD          '~' 
               OUT-LF-DEV-PCT         '~' 
               OUT-LF-BEN             '~' 
               OUT-LF-PRM             '~' 
               OUT-LF-ALT-BEN         '~' 
               OUT-LF-ALT-PRM         '~' 
               OUT-LF-NSP             '~' 
               OUT-LF-REM-BEN         '~' 
               OUT-LF-REF             '~' 
               OUT-LF-DTH             '~' 
               OUT-LF-RATE            '~' 
               OUT-LF-ALT-RATE        '~' 
               OUT-LF-EXP-DT          '~' 
               OUT-LF-CUR-STATUS      '~' 
               OUT-LF-CAN-DT          '~' 
               OUT-LF-CAN-EXIT-DT     '~' 
               OUT-LF-DTH-DT          '~' 
               OUT-LF-DTH-EXIT-DT     '~' 
               OUT-LF-EXIT-BATCH      '~' 
               OUT-LF-COMM-PCT        '~' 
               OUT-LF-DESC            '~' 
               OUT-LF-ABBRV           '~' 
               OUT-AH-BENCD           '~' 
               OUT-AH-TERM            '~' 
               OUT-CRIT-PER           '~' 
               OUT-AH-DEV-CD          '~' 
               OUT-AH-DEV-PCT         '~' 
               OUT-AH-BEN             '~' 
               OUT-AH-PRM             '~' 
               OUT-AH-NSP             '~' 
               OUT-AH-REF             '~' 
               OUT-AH-CLM             '~' 
               OUT-AH-TOT-BEN         '~' 
               OUT-AH-PDTHRU-DT       '~' 
               OUT-AH-RATE            '~' 
               OUT-AH-EXP-DT          '~' 
               OUT-AH-CUR-STATUS      '~' 
               OUT-AH-CAN-DT          '~' 
               OUT-AH-CAN-EXIT-DT     '~' 
               OUT-AH-EXIT-BATCH      '~' 
               OUT-AH-COMM-PCT        '~' 
               OUT-AH-DESC            '~' 
               OUT-RET-ELIM           '~' 
               OUT-BEN-DAYS           '~' 
               OUT-WAIT-PER           '~' 
               OUT-MAX-PMTS           '~' 
               OUT-TOT-PRM            '~' 
               OUT-TOT-REF            '~' 
               OUT-SCHED-EXP-DT       '~' 
               OUT-SCHED-TERM         '~' 
               OUT-ORIG-ILNAME        '~' 
               OUT-ORIG-IFNAME        '~' 
               OUT-ORIG-MINIT         '~' 
               OUT-ORIG-IAGE          '~' 
               OUT-ORIG-JLNAME        '~' 
               OUT-ORIG-JFNAME        '~' 
               OUT-ORIG-JMINIT        '~' 
               OUT-ORIG-JAGE          '~' 
               OUT-ORIG-LF-BENCD      '~' 
               OUT-ORIG-LF-TERM       '~' 
               OUT-ORIG-LF-BEN        '~' 
               OUT-ORIG-LF-PRM        '~' 
               OUT-LF-CALC-PRM        '~' 
               OUT-ORIG-LF-REF        '~' 
               OUT-LF-CALC-REF        '~' 
               OUT-ORIG-LF-ALT-BEN    '~' 
               OUT-ORIG-LF-ALT-PRM    '~' 
               OUT-ORIG-LF-EXP-DT     '~' 
               OUT-ORIG-LF-DESC       '~' 
               OUT-ORIG-AH-BENCD      '~' 
               OUT-ORIG-AH-TERM       '~' 
               OUT-ORIG-CRIT-PER      '~' 
               OUT-ORIG-AH-BEN        '~' 
               OUT-ORIG-AH-PRM        '~' 
               OUT-AH-CALC-PRM        '~' 
               OUT-ORIG-AH-REF        '~' 
               OUT-AH-CALC-REF        '~' 
               OUT-ORIG-AH-EXP-DT     '~' 
               OUT-ORIG-AH-DESC       '~' 
               OUT-ORIG-RET-ELIM      '~' 
               OUT-ORIG-BEN-DAYS      '~' 
               OUT-ORIG-WAIT-PER      '~' 
               OUT-ORIG-MAX-PMTS      '~' 
               OUT-ORIG-SCHED-EXP-DT  '~' 
               OUT-ORIG-SCHED-TERM    '~' 
               OUT-NEW-ILNAME         '~' 
               OUT-NEW-IFNAME         '~' 
               OUT-NEW-IMINIT         '~' 
               OUT-NEW-IAGE           '~' 
               OUT-NEW-JLNAME         '~' 
               OUT-NEW-JFNAME         '~' 
               OUT-NEW-JMINIT         '~' 
               OUT-NEW-JAGE           '~' 
               OUT-NEW-LF-BENCD       '~' 
               OUT-NEW-LF-TERM        '~' 
               OUT-NEW-LF-BEN         '~' 
               OUT-NEW-LF-PRM         '~' 
               OUT-NEW-LF-REF         '~' 
               OUT-NEW-LF-ALT-BEN     '~' 
               OUT-NEW-LF-ALT-PRM     '~' 
               OUT-NEW-LF-EXP-DT      '~' 
               OUT-NEW-LF-DESC        '~' 
               OUT-NEW-AH-BENCD       '~' 
               OUT-NEW-AH-TERM        '~' 
               OUT-NEW-CRIT-PER       '~' 
               OUT-NEW-AH-BEN         '~' 
               OUT-NEW-AH-PRM         '~' 
               OUT-NEW-AH-REF         '~' 
               OUT-NEW-AH-EXP-DT      '~' 
               OUT-NEW-AH-DESC        '~' 
               OUT-NEW-RET-ELIM       '~' 
               OUT-NEW-BEN-DAYS       '~' 
               OUT-NEW-WAIT-PER       '~'
               OUT-NEW-MAX-PMTS       '~'
               OUT-NEW-SCHED-EXP-DT   '~'
               OUT-NEW-SCHED-TERM     '~'
               OUT-TOT-PRM-CHG        '~'
               OUT-TOT-REF-CHG        '~'
               OUT-PAYEE              '~'
               OUT-SIG-SW             '~'
               OUT-BALLOON-IND        '~'
               OUT-LEASE-IND          '~'
               OUT-RESCIND            '~'
               OUT-REA-CD1            '~'
               OUT-REA-CD2            '~'
               OUT-REA-CD3            '~'
               OUT-REA-CD4            '~'
               OUT-REA-CD5            '~'
               OUT-REA-CD6            '~'
               OUT-REA-CD7            '~'
               OUT-REA-CD8            '~'
               OUT-REA-CD9            '~'
               OUT-REA-CD10           '~'
               OUT-REA-CD11           '~'
               OUT-REA-CD12           '~'
               OUT-CYCLE-DT           '~'
               OUT-ARCH-NO            '~'
               OUT-FORM               '~'
               OUT-ENC-LINE           '~'
               OUT-ATTACH             '~'
               OUT-STACK              '~'
               OUT-STATE-NAME         '~'
               OUT-PRINT-NOW          '~'
072312         OUT-CHGBACK            '~'
               OUT-CSO-PORTION        '~'
               OUT-ACCT-PORTION       '~'
110612         OUT-LETTER-TYPE        '~'
110612         OUT-PRINT-CERTIFICATE  '~'
110612         OUT-INS-BIRTHDATE      '~'
110612         OUT-JNT-BIRTHDATE      '~'
110612         OUT-TOT-INTEREST       '~'
               OUT-ORIG-TOT-PREM      '~'
               OUT-ORIG-TOT-REF       '~'
               OUT-CANCEL-DT          '~'
               OUT-HEALTH-APP         '~'
110612         OUT-CERTIFICATE-ID     '~'
071212         OUT-UNDW-ID            '~'
072312         OUT-NEW-TOT-PREM       '~'
072312         OUT-NEW-TOT-REF        '~' 
110612         OUT-COVERAGE-IND       '~'
110612         OUT-ORIG-COVERAGE-IND  '~'
110612         OUT-NEW-COVERAGE-IND   '~'
110612         OUT-LF-CMB-PREM        '~'
110612         OUT-ORIG-LF-CMB-PREM   '~'
110612         OUT-NEW-LF-CMB-PREM    '~'
110612         OUT-LF-PREM-CHG        '~'
110612         OUT-LF-ALT-PREM-CHG    '~'
110612         OUT-LF-CMB-PREM-CHG    '~'
110612         OUT-AH-PREM-CHG        '~'
110612         OUT-LF-REF-CHG         '~'
110612         OUT-AH-REF-CHG         '~'
110612         OUT-ACCT-LF-PORTION    '~'
110612         OUT-ACCT-ALT-LF-PORT   '~'
110612         OUT-ACCT-CMB-LF-PORT   '~'
110612         OUT-ACCT-AH-PORTION    '~'
110612         OUT-CSO-LF-PORTION     '~'
110612         OUT-CSO-ALT-LF-PORT    '~'
110612         OUT-CSO-CMB-LF-PORT    '~'
110612         OUT-CSO-AH-PORTION     '~'
110612         OUT-ACCT-ORIG-LF-PORT  '~'
110612         OUT-ACCT-ORIG-ALT-LF-PORT '~'
110612         OUT-ACCT-ORIG-CMB-LF-PORT '~'
110612         OUT-ACCT-ORIG-AH-PORT  '~'
110612         OUT-ACCT-ORIG-PORTION  '~'
110612         OUT-CSO-ORIG-LF-PORT   '~'
110612         OUT-CSO-ORIG-ALT-LF-PORT '~'
110612         OUT-CSO-ORIG-CMB-LF-PORT '~'
110612         OUT-CSO-ORIG-AH-PORT   '~'
110612         OUT-CSO-ORIG-PORTION   '~'
110612         OUT-ACCT-NEW-LF-PORT   '~'
110612         OUT-ACCT-NEW-ALT-LF-PORT '~'
110612         OUT-ACCT-NEW-CMB-LF-PORT '~'
110612         OUT-ACCT-NEW-AH-PORT   '~'
110612         OUT-ACCT-NEW-PORTION   '~'
110612         OUT-CSO-NEW-LF-PORT    '~'
110612         OUT-CSO-NEW-ALT-LF-PORT '~'
110612         OUT-CSO-NEW-CMB-LF-PORT '~'
110612         OUT-CSO-NEW-AH-PORT    '~'
110612         OUT-CSO-NEW-PORTION    '~'
110612         OUT-ACCT-LF-PORT-CHG   '~'
110612         OUT-ACCT-ALT-LF-PORT-CHG '~'
110612         OUT-ACCT-CMB-LF-PORT-CHG '~'
110612         OUT-ACCT-AH-PORT-CHG   '~'
110612         OUT-ACCT-PORTION-CHG   '~'
110612         OUT-CSO-LF-PORT-CHG    '~'
110612         OUT-CSO-ALT-LF-PORT-CHG '~'
110612         OUT-CSO-CMB-LF-PORT-CHG '~'
110612         OUT-CSO-AH-PORT-CHG    '~'
110612         OUT-CSO-PORTION-CHG    '~'
110612         OUT-ORIG-LF-CAN-DT     '~'
110612         OUT-NEW-LF-CAN-DT      '~'
110612         OUT-ORIG-AH-CAN-DT     '~'
110612         OUT-NEW-AH-CAN-DT      '~'
110612         OUT-ORIG-CAN-DT        '~'
110612         OUT-NEW-CAN-DT         '~'
110612         OUT-SCREEN-ID          '~'
091213         OUT-NEXT-BUS-DT        '~'
121015         OUT-VIN-NUMBER         '~'
100112            DELIMITED BY '   ' INTO NSAS-LETTER-VARIABLES
           END-STRING

           .
       0710-exit.
           EXIT.

       0720-WRITE-NSASEXTR.

           EXEC CICS WRITE
              DATASET    ('NSASEXTR')
              FROM       (NSAS-EXTRACT-RECORD)
              RIDFLD     (NSAS-CONTROL-PRIMARY)
              RESP       (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              display ' bad write nsasextr ' ws-response
              SET BL-FAIL TO TRUE
           END-IF

           .
       0720-EXIT.
           EXIT.

       0800-init-out-data.

           MOVE ZEROS                  TO OUT-IAGE
                                          OUT-JAGE
                                          OUT-LOAN-APR
                                          OUT-LOAN-TERM
                                          OUT-EXT-DAYS
                                          OUT-LF-TERM
                                          OUT-AH-TERM
                                          OUT-LF-DEV-PCT
                                          OUT-AH-DEV-PCT
                                          OUT-LF-BEN
                                          OUT-AH-BEN
                                          OUT-CRIT-PER
                                          OUT-MAX-PMTS
                                          OUT-SCHED-TERM
                                          OUT-ORIG-IAGE
                                          OUT-ORIG-JAGE
                                          OUT-NEW-IAGE
                                          OUT-NEW-JAGE
                                          OUT-ORIG-LF-TERM
                                          OUT-ORIG-AH-TERM
                                          OUT-NEW-LF-TERM
                                          OUT-NEW-AH-TERM
                                          OUT-ORIG-CRIT-PER
                                          OUT-ORIG-MAX-PMTS
                                          OUT-ORIG-SCHED-TERM
                                          OUT-NEW-SCHED-TERM
                                          OUT-NEW-CRIT-PER
                                          OUT-NEW-MAX-PMTS
                                          OUT-CSO-PORTION
110612                                    OUT-CSO-LF-PORTION
110612                                    OUT-CSO-ALT-LF-PORT
110612                                    OUT-CSO-CMB-LF-PORT
110612                                    OUT-CSO-AH-PORTION
                                          OUT-ACCT-PORTION
110612                                    OUT-ACCT-LF-PORTION
110612                                    OUT-ACCT-ALT-LF-PORT
110612                                    OUT-ACCT-CMB-LF-PORT
110612                                    OUT-ACCT-AH-PORTION
110612                                    OUT-LF-PREM-CHG
110612                                    OUT-LF-ALT-PREM-CHG
110612                                    OUT-LF-CMB-PREM-CHG
110612                                    OUT-AH-PREM-CHG
110612                                    OUT-LF-REF-CHG
110612                                    OUT-AH-REF-CHG
080522                                    out-acct-orig-portion
080522                                    out-cso-orig-portion


           .
       0800-exit.
           exit.

       1000-BUILD-ERARCH.
      
      * DATA SOURCE MEANINGS  BL-DATA-SRCE
      *    1) FROM ACCT MAINT
      *    2) FROM CERT UPDATE
      *    3) FROM COMP MAINT
      *    4) FROM REVIEW AND CORRECTIONS
      ******************************************************************


           MOVE 'LA'                   TO LETTER-ARCHIVE

           MOVE BL-ARCHIVE-NO          TO LA-ARCHIVE-NO
                                          LA-ARCHIVE-NO-A2
                                          LA-ARCHIVE-NO-A3
                                          LA-ARCHIVE-NO-A4
                                          LA-ARCHIVE-NO-A5
                                          LA-ARCHIVE-NO-A6

           MOVE WS-COMPANY-CD          TO LA-COMPANY-CD
                                          LA-COMPANY-CD-A2
                                          LA-COMPANY-CD-A3
                                          LA-COMPANY-CD-A4
                                          LA-COMPANY-CD-A5
                                          LA-COMPANY-CD-A6
           MOVE BL-CARRIER             TO LA-CARRIER-A2
                                          LA-CARRIER-A3
                                          LA-CARRIER-A4
                                          LA-CARRIER-A5
           MOVE BL-GROUP               TO LA-GROUPING-A2
                                          LA-GROUPING-A3
                                          LA-GROUPING-A4
                                          LA-GROUPING-A5
           MOVE BL-ACCOUNT             TO LA-ACCOUNT-A2
                                          LA-ACCOUNT-A3
                                          LA-ACCOUNT-A4
                                          LA-ACCOUNT-A5
           MOVE BL-STATE               TO LA-STATE-A2
                                          LA-STATE-A3
                                          LA-STATE-A4
                                          LA-STATE-A5
           IF BL-DATA-SRCE = '2' OR '4'
              MOVE WS-ELCERT-EFF-DT    TO LA-EFFECT-DATE-A2
              MOVE BL-CERT-NO          TO LA-CERT-NO-A2
           ELSE
              MOVE LOW-VALUES          TO LA-EFFECT-DATE-A2
           END-IF
           IF BL-DATA-SRCE = '4'
              MOVE BL-BATCH-NO         TO LA-ENTRY-A6
           END-IF

           MOVE BL-PROC-ID             TO LA-PROCESSOR-CD
                                          LA-LAST-UPDATED-BY
      
           MOVE LOW-VALUES             TO LA-LAST-RESENT-PRINT-DATE
                                          LA-INITIAL-PRINT-DATE
                                          LA-SENT-DATE
                                          LA-REPLY-DATE
                                          LA-RESEND-DATE
                                          LA-FOLLOW-UP-DATE

           MOVE 'A'                    TO LA-STATUS
           MOVE W-NUMBER-OF-COPIES     TO LA-NO-OF-COPIES
           MOVE W-FORM-TO-RESEND       TO LA-RESEND-LETR
           IF W-DAYS-TO-RESEND NOT NUMERIC
              MOVE ZEROS               TO W-DAYS-TO-RESEND
           END-IF
           IF W-DAYS-TO-RESEND > ZEROS
123113        MOVE SAVE-BIN-NEXT-BUS-DT TO DC-BIN-DATE-1
              MOVE W-DAYS-TO-RESEND  TO DC-ELAPSED-DAYS
              MOVE ZEROS               TO DC-ELAPSED-MONTHS
              MOVE '6'                 TO DC-OPTION-CODE
              PERFORM 9700-DATE-LINK THRU 9700-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-2    TO LA-RESEND-DATE
              END-IF
           END-IF
090612     IF W-DAYS-TO-FOLLOW-UP >= ZEROS
090612        MOVE SAVE-BIN-DATE       TO DC-BIN-DATE-1
090612        MOVE W-DAYS-TO-FOLLOW-UP TO DC-ELAPSED-DAYS
090612        MOVE ZEROS               TO DC-ELAPSED-MONTHS
090612        MOVE '6'                 TO DC-OPTION-CODE
090612        PERFORM 9700-DATE-LINK THRU 9700-EXIT
090612        IF NO-CONVERSION-ERROR
090612           MOVE DC-BIN-DATE-2    TO LA-FOLLOW-UP-DATE
090612        END-IF
090612     END-IF
090612
           MOVE W-AUTO-CLOSE-IND       TO LA-FINAL-ACT-IND
           MOVE BL-LETTER-ID           TO LA-FORM-A3
           MOVE BL-DATA-SRCE           TO LA-DATA-SOURCE
           MOVE BL-WRITE-ERARCH        TO LA-ARCHIVE-STATUS
110612     IF BL-ENDT-ARCH-NO > ZEROS
110612        MOVE BL-ENDT-ARCH-NO     TO LA-ENDT-ARCH-NO
110612     ELSE
110612        MOVE BL-ARCHIVE-NO       TO LA-ENDT-ARCH-NO
110612     END-IF
           MOVE SAVE-BIN-DATE          TO LA-CREATION-DATE
                                          LA-LAST-MAINT-DATE

           MOVE ZEROS                  TO LA-NUMBER-LABEL-LINES
                                          LA-NO-OF-TEXT-RECORDS
           MOVE EIBTIME                TO LA-LAST-MAINT-TIME

           EXEC CICS WRITE
                DATASET   ('ERARCH')
                FROM      (LETTER-ARCHIVE)
                RIDFLD    (LA-CONTROL-PRIMARY)
                RESP      (WS-RESPONSE)
           END-EXEC
      
           IF NOT RESP-NORMAL
              DISPLAY ' BAD WRITE ON ERARCH ' WS-RESPONSE ' '
              la-archive-no
              SET BL-FAIL TO TRUE
           END-IF

           .
       1000-EXIT.
           EXIT.

       1025-GET-CSR-INFO.

           MOVE WS-CSR-ID              TO OUT-CSR-CODE
                                          OUT-CSR-TITLE
                                          OUT-PROC-TITLE
071212                                    OUT-UNDW-ID
           MOVE BL-COMP-ID             TO WS-ELCNTL-KEY
           MOVE '2'                    TO WS-ELCNTL-REC-TYPE
           MOVE WS-CSR-ID              TO WS-ELCNTL-GENL
           MOVE +0                     TO WS-ELCNTL-SEQ-NO
           EXEC CICS READ
              INTO    (CONTROL-FILE)
              DATASET ('ELCNTL')
              RIDFLD  (WS-ELCNTL-KEY)
              RESP    (WS-RESPONSE)
           END-EXEC
           IF RESP-NORMAL           
              MOVE CF-PROCESSOR-NAME   TO OUT-CSR-NAME
                                          OUT-PROC-NAME
      *       MOVE CF-PROCESSOR-TITLE  TO OUT-CSR-TITLE
      *                                   OUT-PROC-TITLE
           END-IF

           IF BL-PROC-ID NOT = PB-CSR-ID
              MOVE BL-COMP-ID          TO WS-ELCNTL-KEY
              MOVE '2'                 TO WS-ELCNTL-REC-TYPE
              MOVE BL-PROC-ID          TO WS-ELCNTL-GENL
                                          OUT-PROC-TITLE
              MOVE +0                  TO WS-ELCNTL-SEQ-NO
              EXEC CICS READ
                 INTO    (CONTROL-FILE)
                 DATASET ('ELCNTL')
                 RIDFLD  (WS-ELCNTL-KEY)
                 RESP    (WS-RESPONSE)
              END-EXEC
              IF RESP-NORMAL           
                 MOVE CF-PROCESSOR-NAME   TO OUT-PROC-NAME
      *          MOVE CF-PROCESSOR-TITLE  TO OUT-PROC-TITLE
              END-IF
           END-IF

           .
       1025-EXIT.
           EXIT.

       1050-GET-ERMAIL.

           MOVE CM-CONTROL-PRIMARY     TO WS-ERMAIL-KEY

           EXEC CICS READ                                               
                DATASET    ('ERMAIL')
                INTO       (MAILING-DATA)
                RIDFLD     (WS-ERMAIL-KEY)
                RESP       (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              MOVE MA-ADDRESS-LINE-1   TO OUT-INS-ADDR1
              MOVE MA-ADDRESS-LINE-2   TO OUT-INS-ADDR2
              MOVE MA-CITY             TO OUT-INS-CITY
              MOVE MA-ADDR-STATE       TO OUT-INS-STATE
              MOVE MA-ZIP              TO OUT-INS-ZIP
              MOVE MA-CRED-BENE-NAME   TO OUT-BENE-NAME
              MOVE MA-CRED-BENE-ADDR   TO OUT-BENE-ADDR1
              MOVE MA-CRED-BENE-ADDR2  TO OUT-BENE-ADDR2
              MOVE MA-CRED-BENE-CITY   TO OUT-BENE-CITY
              MOVE MA-CRED-BENE-STATE  TO OUT-BENE-STATE
              MOVE MA-CRED-BENE-ZIP    TO OUT-BENE-ZIP
              IF MA-INSURED-BIRTH-DT NOT = LOW-VALUES AND SPACES
                 MOVE MA-INSURED-BIRTH-DT
                                       TO DC-BIN-DATE-1
                 MOVE ' '              TO DC-OPTION-CODE
                 PERFORM 9700-DATE-LINK THRU 9700-EXIT
                 IF NO-CONVERSION-ERROR
                    STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
                      DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
110612                INTO OUT-INS-BIRTHDATE
                    END-STRING
                 END-IF
              END-IF
              IF MA-JOINT-BIRTH-DT NOT = LOW-VALUES AND SPACES
                 MOVE MA-JOINT-BIRTH-DT
                                       TO DC-BIN-DATE-1
                 MOVE ' '              TO DC-OPTION-CODE
                 PERFORM 9700-DATE-LINK THRU 9700-EXIT
                 IF NO-CONVERSION-ERROR
                    STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
                      DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
110612                INTO OUT-JNT-BIRTHDATE
                    END-STRING
                 END-IF
              END-IF
           END-IF

           .
       1050-EXIT.
           EXIT.

       1100-GET-ELCERT.

           EXEC CICS READ                                               
                DATASET    ('ELCERT') 
                INTO       (CERTIFICATE-MASTER)              
                RIDFLD     (WS-ELCERT-KEY)                                   
                RESP       (WS-RESPONSE)
           END-EXEC.                                                    

           IF RESP-NORMAL
              SET CERT-FOUND TO TRUE
              PERFORM 1110-BUILD-CERT  THRU 1110-EXIT
              PERFORM 1050-GET-ERMAIL  THRU 1050-EXIT
           END-IF

           .
       1100-EXIT.
           EXIT.

       1110-BUILD-CERT.

120412     PERFORM 1300-DETERMINE-COMM THRU 1300-EXIT

110612     MOVE SPACES                 TO OUT-COVERAGE-IND
110612     MOVE SPACES                 TO OUT-ORIG-COVERAGE-IND
110612     MOVE SPACES                 TO OUT-NEW-COVERAGE-IND
           MOVE CM-CARRIER             TO OUT-CARRIER
           MOVE CM-GROUPING            TO OUT-GROUPING
           MOVE CM-STATE               TO OUT-STATE
           MOVE CM-ACCOUNT             TO OUT-ACCOUNT
           MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           IF NO-CONVERSION-ERROR
              STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
                 DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
                    INTO OUT-CERT-EFF-DT
              END-STRING
           END-IF
      
           MOVE CM-CERT-NO             TO OUT-CERT-NO
           MOVE CM-INSURED-LAST-NAME   TO OUT-ILNAME
072312                                    OUT-ORIG-ILNAME
           MOVE CM-INSURED-INITIAL1    TO OUT-IFINIT
           MOVE CM-INSURED-INITIAL2    TO OUT-IMINIT
072312                                    OUT-ORIG-MINIT
           IF (CM-SSN-STATE = CM-STATE)
              AND (CM-SSN-ACCOUNT = CM-ACCOUNT (5:6))
              MOVE SPACES              TO OUT-SOC-SEC-NO
           ELSE
              MOVE CM-SOC-SEC-NO       TO OUT-SOC-SEC-NO
           END-IF

           IF (CM-MEMB-STATE = CM-STATE)
              AND (CM-MEMB-ACCOUNT = CM-ACCOUNT (5:6))
              MOVE SPACES              TO OUT-MEMBER-NO
           ELSE
              MOVE CM-MEMBER-NO        TO OUT-MEMBER-NO
           END-IF

           MOVE CM-INSURED-FIRST-NAME  TO OUT-IFNAME
072312                                    OUT-ORIG-IFNAME
           MOVE CM-INSURED-ISSUE-AGE   TO OUT-IAGE
072312                                    OUT-ORIG-IAGE
           MOVE CM-INSURED-SEX         TO OUT-ISEX
           MOVE CM-INSURED-JOINT-AGE   TO OUT-JAGE
072312                                    OUT-ORIG-JAGE
           MOVE CM-JT-LAST-NAME        TO OUT-JLNAME
072312                                    OUT-ORIG-JLNAME
           MOVE CM-JT-FIRST-NAME       TO OUT-JFNAME
072312                                    OUT-ORIG-JFNAME
           MOVE CM-JT-INITIAL          TO OUT-JMINIT
072312                                    OUT-ORIG-JMINIT
           MOVE CM-LF-BENEFIT-CD       TO OUT-LF-BENCD                                                              
072312                                    OUT-ORIG-LF-BENCD
           MOVE CM-LF-ORIG-TERM        TO OUT-LF-TERM
072312                                    OUT-ORIG-LF-TERM
           MOVE CM-LF-DEV-CODE         TO OUT-LF-DEV-CD
           IF CM-LF-DEV-PCT NOT NUMERIC
              MOVE ZEROS               TO CM-LF-DEV-PCT
           END-IF
           MOVE CM-LF-DEV-PCT          TO OUT-LF-DEV-PCT

121712     PERFORM 1160-READ-CERT-TRAILER THRU 1160-EXIT
121712     IF NOT CERT-TRL-REC-NOT-FOUND
121712         IF CS-INS-AGE-DEFAULT-FLAG = 'Y'
121712             MOVE ZERO           TO OUT-IAGE
121712                                    OUT-ORIG-IAGE
121712         END-IF
121712         IF CS-JNT-AGE-DEFAULT-FLAG = 'Y'
121712             MOVE ZERO           TO OUT-JAGE
121712                                    OUT-ORIG-JAGE
121712         END-IF
121015         MOVE CS-VIN-NUMBER      TO OUT-VIN-NUMBER
121712     END-IF
121712
072312     IF BL-DATA-SRCE = '4' AND PROCESS-CANCEL
072312        IF (PB-C-LF-CANCEL-DT = LOW-VALUES OR SPACES) AND
050213          ((CM-LF-CANCEL-DT NOT = LOW-VALUES AND SPACES) OR
050213           (CM-LF-DEATH-DT NOT = LOW-VALUES AND SPACES))
072312             MOVE SPACES          TO OUT-LF-BENCD
101412                                     WS-LF-BENCD
092412                                     OUT-LF-CAN-DT
072312             MOVE ZEROS           TO OUT-LF-TERM         
072312                                     OUT-LF-BEN      
072312                                     OUT-LF-PRM      
072312                                     OUT-LF-ALT-BEN  
072312                                     OUT-LF-ALT-PRM  
072312                                     OUT-LF-COMM-PCT 
072312                                     OUT-LF-NSP    
072312                                     OUT-LF-REM-BEN
072312                                     WS-WORK-LF-PREM
110612                                     WS-WORK-ALT-LF-PREM
110612                                     WS-WORK-CMB-LF-PREM
092412                                     OUT-LF-REF
092412                                     OUT-ORIG-LF-REF
092412                                     WS-WORK-LF-REF
072312             MOVE LOW-VALUES      TO OUT-LF-EXP-DT       
072312             GO TO 1100-LF-CONT
072312        END-IF
072312     END-IF

101412
101412     IF BL-DATA-SRCE = '2' AND BL-FUNC = 'CrtVerif'
101412        IF (CM-LF-CANCEL-DT NOT = LOW-VALUES AND SPACES)
101412             MOVE SPACES          TO OUT-LF-BENCD
101412                                     WS-LF-BENCD
101412                                     OUT-LF-CAN-DT
101412             MOVE ZEROS           TO OUT-LF-TERM         
101412                                     OUT-LF-BEN      
101412                                     OUT-LF-PRM      
101412                                     OUT-LF-ALT-BEN  
101412                                     OUT-LF-ALT-PRM  
101412                                     OUT-LF-COMM-PCT 
101412                                     OUT-LF-NSP    
101412                                     OUT-LF-REM-BEN
101412                                     WS-WORK-LF-PREM
110612                                     WS-WORK-ALT-LF-PREM
110612                                     WS-WORK-CMB-LF-PREM
101412                                     OUT-LF-REF
101412                                     OUT-ORIG-LF-REF
101412                                     WS-WORK-LF-REF
101412             MOVE LOW-VALUES      TO OUT-LF-EXP-DT       
101412             GO TO 1100-LF-CONT
101412        END-IF
101412     END-IF

110612     IF CM-LF-BENEFIT-CD NOT = '  ' AND '00'
110612         MOVE 'LIFE'             TO OUT-COVERAGE-IND
110612                                    OUT-ORIG-COVERAGE-IND
110612     END-IF
110612
101412     MOVE CM-LF-BENEFIT-CD       TO WS-LF-BENCD
           MOVE CM-LF-BENEFIT-AMT      TO OUT-LF-BEN
072312                                    OUT-ORIG-LF-BEN
091213     IF CM-LF-BENEFIT-AMT = 999999999.99
121112        MOVE SPACES              TO OUT-ORIG-LF-BEN-A
121112     END-IF
           MOVE CM-LF-PREMIUM-AMT      TO OUT-LF-PRM
072312                                    OUT-ORIG-LF-PRM
072312                                    WS-WORK-LF-PREM
091213     IF CM-LF-PREMIUM-AMT = .01
121112         MOVE SPACES             TO OUT-ORIG-LF-PRM-A
121112     END-IF
           IF CM-LF-ALT-BENEFIT-AMT NOT NUMERIC
              MOVE ZEROS               TO CM-LF-ALT-BENEFIT-AMT
           END-IF
           MOVE CM-LF-ALT-BENEFIT-AMT  TO OUT-LF-ALT-BEN
072312                                    OUT-ORIG-LF-ALT-BEN
091213     IF CM-LF-ALT-BENEFIT-AMT = 999999999.99
091213        MOVE SPACES              TO OUT-ORIG-LF-ALT-BEN-A
091213     END-IF
           IF CM-LF-ALT-PREMIUM-AMT NOT NUMERIC
              MOVE ZEROS               TO CM-LF-ALT-PREMIUM-AMT
           END-IF
           MOVE CM-LF-ALT-PREMIUM-AMT  TO OUT-LF-ALT-PRM
072312                                    OUT-ORIG-LF-ALT-PRM
110612                                    WS-WORK-ALT-LF-PREM
091213     IF CM-LF-ALT-PREMIUM-AMT = .01
091213         MOVE SPACES             TO OUT-ORIG-LF-ALT-PRM-A
091213     END-IF
110612     COMPUTE WS-WRK-AMOUNT = CM-LF-PREMIUM-AMT + 
110612                             CM-LF-ALT-PREMIUM-AMT
110612     MOVE WS-WRK-AMOUNT          TO OUT-LF-CMB-PREM
110612                                 OUT-ORIG-LF-CMB-PREM
110612                                 WS-WORK-CMB-LF-PREM
           IF CM-LF-NSP-PREMIUM-AMT NOT NUMERIC
              MOVE ZEROS               TO CM-LF-NSP-PREMIUM-AMT
           END-IF
           MOVE CM-LF-NSP-PREMIUM-AMT  TO OUT-LF-NSP
           MOVE CM-LF-REMAINING-AMT    TO OUT-LF-REM-BEN
092412     MOVE CM-LF-ITD-CANCEL-AMT   TO OUT-LF-REF
092412                                    OUT-ORIG-LF-REF
092412                                    WS-WORK-LF-REF
080522     if (ws-work-lf-ref = zeros)
080522        and (process-cancel)
080522        move pb-ci-lf-cancel-amt to ws-work-lf-ref
080522        if ws-work-lf-ref = zeros
080522           move pb-c-lf-cancel-amt to ws-work-lf-ref
080522        end-if
080522     end-if

092412     IF CM-LF-DEATH-CLAIM-APPLIED
092412        AND (PB-C-LF-CANCEL-DT NOT = LOW-VALUES OR SPACES)
092412            MOVE PB-C-LF-CANCEL-AMT TO OUT-LF-REF
092412                                    OUT-ORIG-LF-REF
092412                                    WS-WORK-LF-REF
092412     END-IF
092412     MOVE CM-LF-CANCEL-DT        TO DC-BIN-DATE-1
092412     MOVE ' '                    TO DC-OPTION-CODE
092412     PERFORM 9700-DATE-LINK      THRU 9700-EXIT
092412     IF NO-CONVERSION-ERROR
092412        STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
092412           DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
092412              INTO OUT-LF-CAN-DT
092412        END-STRING
092412     END-IF
110612
110612     IF CM-LF-CANCEL-DT NOT = LOW-VALUES AND SPACES
110612         MOVE ZEROS          TO WS-WORK-LF-PREM
110612                                WS-WORK-ALT-LF-PREM
110612                                WS-WORK-CMB-LF-PREM
110612     END-IF
110612
072312      .
072312 1100-LF-CONT.
072312
           MOVE CM-LF-ITD-DEATH-AMT    TO OUT-LF-DTH
           IF CM-LF-PREMIUM-RATE NOT NUMERIC
              MOVE ZEROS               TO CM-LF-PREMIUM-RATE
           END-IF
           MOVE CM-LF-PREMIUM-RATE     TO OUT-LF-RATE
           IF CM-LF-ALT-PREMIUM-RATE NOT NUMERIC
              MOVE ZEROS               TO CM-LF-ALT-PREMIUM-RATE
           END-IF
           MOVE CM-LF-ALT-PREMIUM-RATE TO OUT-LF-ALT-RATE

072312     IF BL-DATA-SRCE = '4' AND PROCESS-CANCEL
072312        IF (PB-C-AH-CANCEL-DT = LOW-VALUES OR SPACES) AND
072312          (CM-AH-CANCEL-DT NOT = LOW-VALUES AND SPACES)
072312              MOVE SPACES              TO OUT-AH-BENCD 
101412                                          WS-AH-BENCD
092412                                          OUT-AH-CAN-DT       
072312              MOVE ZEROS               TO OUT-AH-TERM         
072312                                          OUT-AH-BEN      
072312                                          OUT-AH-PRM
072312                                          OUT-AH-NSP
072312                                          OUT-AH-COMM-PCT     
072312                                          OUT-CRIT-PER
072312                                          WS-WORK-AH-PREM
092412                                          OUT-AH-REF
092412                                          OUT-ORIG-AH-REF
092412                                          WS-WORK-AH-REF
072312              MOVE SPACES              TO OUT-MAX-PMTS-A
072312                                          OUT-WAIT-PER      
072312                                          OUT-RET-ELIM      
072312                                          OUT-AH-DESC       
072312                                          OUT-BEN-DAYS      
072312              MOVE LOW-VALUES          TO OUT-AH-EXP-DT       
072312             GO TO 1100-AH-CONT
072312        END-IF
072312     END-IF
101412
101412     IF BL-DATA-SRCE = '2' AND BL-FUNC = 'CrtVerif'
101412        IF (CM-AH-CANCEL-DT NOT = LOW-VALUES AND SPACES)
101412              MOVE SPACES              TO OUT-AH-BENCD
101412                                          WS-AH-BENCD
101412                                          OUT-AH-CAN-DT       
101412              MOVE ZEROS               TO OUT-AH-TERM         
101412                                          OUT-AH-BEN      
101412                                          OUT-AH-PRM
101412                                          OUT-AH-NSP
101412                                          OUT-AH-COMM-PCT     
101412                                          OUT-CRIT-PER
101412                                          WS-WORK-AH-PREM
101412                                          OUT-AH-REF
101412                                          OUT-ORIG-AH-REF
101412                                          WS-WORK-AH-REF
101412              MOVE SPACES              TO OUT-MAX-PMTS-A
101412                                          OUT-WAIT-PER      
101412                                          OUT-RET-ELIM      
101412                                          OUT-AH-DESC       
101412                                          OUT-BEN-DAYS      
101412              MOVE LOW-VALUES          TO OUT-AH-EXP-DT       
101412             GO TO 1100-AH-CONT
101412        END-IF
101412     END-IF

101412     MOVE CM-AH-BENEFIT-CD       TO WS-AH-BENCD
           MOVE CM-AH-BENEFIT-CD       TO OUT-AH-BENCD
072312                                    OUT-ORIG-AH-BENCD

           IF CM-AH-BENEFIT-CD NOT = '  ' AND '00'
              COMPUTE OUT-AH-TOT-BEN = CM-AH-ORIG-TERM *
                 CM-AH-BENEFIT-AMT
110612        IF OUT-COVERAGE-IND = 'LIFE'
110612            MOVE 'BOTH'          TO OUT-COVERAGE-IND
110612                                    OUT-ORIG-COVERAGE-IND
110612        ELSE
110612            MOVE 'AH  '          TO OUT-COVERAGE-IND
110612                                    OUT-ORIG-COVERAGE-IND
110612        END-IF
           ELSE
              MOVE ZEROS               TO OUT-AH-TOT-BEN
           END-IF

           MOVE CM-AH-ORIG-TERM        TO OUT-AH-TERM
072312                                    OUT-ORIG-AH-TERM
           MOVE CM-AH-CRITICAL-PERIOD  TO OUT-CRIT-PER
072312                                    OUT-ORIG-CRIT-PER
                                          OUT-MAX-PMTS
072312                                    OUT-ORIG-MAX-PMTS
           IF CM-AH-BENEFIT-CD EQUAL '00' OR SPACES
               MOVE SPACES             TO OUT-MAX-PMTS-A
           END-IF
           MOVE CM-AH-DEV-CODE         TO OUT-AH-DEV-CD
           IF CM-AH-DEV-PCT NOT NUMERIC
              MOVE ZEROS               TO CM-AH-DEV-PCT
           END-IF
           MOVE CM-AH-DEV-PCT          TO OUT-AH-DEV-PCT
           MOVE CM-AH-BENEFIT-AMT      TO OUT-AH-BEN
072312                                    OUT-ORIG-AH-BEN
091213     IF CM-AH-BENEFIT-AMT = 9999999.99
121112        MOVE SPACES              TO OUT-ORIG-AH-BEN-A
121112     END-IF
           MOVE CM-AH-PREMIUM-AMT      TO OUT-AH-PRM
072312                                    OUT-ORIG-AH-PRM
072312                                    WS-WORK-AH-PREM
091213     IF CM-AH-PREMIUM-AMT = .01
121112         MOVE SPACES             TO OUT-ORIG-AH-PRM-A
121112     END-IF
           MOVE CM-AH-NSP-PREMIUM-AMT  TO OUT-AH-NSP
092412     MOVE CM-AH-ITD-CANCEL-AMT   TO OUT-AH-REF
092412                                    OUT-ORIG-AH-REF
092412                                    WS-WORK-AH-REF

080522     if (ws-work-ah-ref = zeros)
080522        and (process-cancel)
080522        move pb-ci-ah-cancel-amt to ws-work-ah-ref
080522        if ws-work-ah-ref = zeros
080522           move pb-c-ah-cancel-amt to ws-work-ah-ref
080522        end-if
080522     end-if

092412     IF CM-AH-DEATH-CLAIM-APPLIED
092412        AND (PB-C-AH-CANCEL-DT NOT = LOW-VALUES OR SPACES)
092412            MOVE PB-C-AH-CANCEL-AMT TO OUT-AH-REF
092412                                    OUT-ORIG-AH-REF
092412                                    WS-WORK-AH-REF
092412     END-IF
092412     MOVE CM-AH-CANCEL-DT        TO DC-BIN-DATE-1
092412     MOVE ' '                    TO DC-OPTION-CODE
092412     PERFORM 9700-DATE-LINK      THRU 9700-EXIT
092412     IF NO-CONVERSION-ERROR
092412        STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
092412           DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
092412              INTO OUT-AH-CAN-DT
092412        END-STRING
092412     END-IF
110612
110612     IF CM-AH-CANCEL-DT NOT = LOW-VALUES AND SPACES
110612         MOVE ZEROS          TO WS-WORK-AH-PREM
110612     END-IF
110612
072312      .
072312 1100-AH-CONT.
072312     
           MOVE CM-AH-ITD-AH-PMT       TO OUT-AH-CLM
      
           MOVE CM-AH-PAID-THRU-DT     TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           IF NO-CONVERSION-ERROR
              STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
                 DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
                    INTO OUT-AH-PDTHRU-DT
              END-STRING
           END-IF
      
           IF CM-AH-PREMIUM-RATE NOT NUMERIC
              MOVE ZEROS               TO CM-AH-PREMIUM-RATE
           END-IF
           MOVE CM-AH-PREMIUM-RATE     TO OUT-AH-RATE
           IF CM-LOAN-APR NOT NUMERIC
              MOVE ZEROS               TO CM-LOAN-APR
           END-IF
           MOVE CM-LOAN-APR            TO OUT-LOAN-APR
           MOVE CM-LOAN-TERM           TO OUT-LOAN-TERM
           MOVE CM-RATE-CLASS          TO OUT-RATE-CLASS
           MOVE CM-PMT-EXTENSION-DAYS  TO OUT-EXT-DAYS
           MOVE CM-UNDERWRITING-CODE   TO OUT-UCODE
           MOVE CM-PREMIUM-TYPE        TO OUT-PREM-TYPE
           MOVE CM-IND-GRP-TYPE        TO OUT-IND-GRP
           MOVE CM-LOAN-OFFICER        TO OUT-LOAN-OFF
           MOVE CM-REIN-TABLE          TO OUT-REIN-TABLE
           MOVE CM-SPECIAL-REIN-CODE   TO OUT-SPEC-REIN
      
           MOVE CM-LF-LOAN-EXPIRE-DT   TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           IF NO-CONVERSION-ERROR
              STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
                 DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
                    INTO OUT-LF-EXP-DT
              END-STRING
           END-IF
072312     MOVE OUT-LF-EXP-DT          TO OUT-ORIG-LF-EXP-DT
      
           MOVE CM-AH-LOAN-EXPIRE-DT   TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           IF NO-CONVERSION-ERROR
              STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
                 DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
                    INTO OUT-AH-EXP-DT
              END-STRING
           END-IF
072312     MOVE OUT-AH-EXP-DT          TO OUT-ORIG-AH-EXP-DT
      
           MOVE CM-LOAN-1ST-PMT-DT     TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           IF NO-CONVERSION-ERROR
              STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
                 DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
                    INTO OUT-1ST-PMT-DT
              END-STRING
           END-IF
      
           MOVE CM-ENTRY-STATUS        TO OUT-ENTRY-STATUS
      
           MOVE CM-ENTRY-DT            TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           IF NO-CONVERSION-ERROR
              STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
                 DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
                    INTO OUT-ENTRY-DT
              END-STRING
           END-IF
      

           MOVE CM-LF-CANCEL-EXIT-DT   TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           IF NO-CONVERSION-ERROR
              STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
                 DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
                    INTO OUT-LF-CAN-EXIT-DT
              END-STRING
           END-IF
      
           MOVE CM-LF-DEATH-DT         TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           IF NO-CONVERSION-ERROR
              STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
                 DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
                    INTO OUT-LF-DTH-DT
              END-STRING
           END-IF
      
           MOVE CM-LF-DEATH-EXIT-DT    TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           IF NO-CONVERSION-ERROR
              STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
                 DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
                    INTO OUT-LF-DTH-EXIT-DT
              END-STRING
           END-IF
      
           MOVE CM-LF-CURRENT-STATUS   TO OUT-LF-CUR-STATUS

           IF OUT-AH-CAN-DT NOT = SPACES
              MOVE OUT-AH-CAN-DT       TO OUT-CANCEL-DT
           ELSE
              MOVE OUT-LF-CAN-DT       TO OUT-CANCEL-DT
           END-IF
      
           MOVE CM-AH-CANCEL-EXIT-DT   TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           IF NO-CONVERSION-ERROR
              STRING DC-EDITA-CCYY '-' DC-EDITA-MONTH '-'
                 DC-EDITA-DAY ' 00:00:00.0' DELIMITED BY SIZE
                    INTO OUT-AH-CAN-EXIT-DT
              END-STRING
           END-IF
      
           MOVE CM-AH-CURRENT-STATUS   TO OUT-AH-CUR-STATUS
           IF CM-ENTRY-BATCH = LOW-VALUES
              MOVE SPACES              TO CM-ENTRY-BATCH
           END-IF
           MOVE CM-ENTRY-BATCH         TO OUT-ENTRY-BATCH
           IF CM-LF-EXIT-BATCH = LOW-VALUES
              MOVE SPACES              TO CM-LF-EXIT-BATCH
           END-IF
           MOVE CM-LF-EXIT-BATCH       TO OUT-LF-EXIT-BATCH
           IF CM-AH-EXIT-BATCH = LOW-VALUES
              MOVE SPACES              TO CM-AH-EXIT-BATCH
           END-IF
           MOVE CM-AH-EXIT-BATCH       TO OUT-AH-EXIT-BATCH
120412     MOVE WS-CHGBK-LIFE-PCT      TO OUT-LF-COMM-PCT
120412     MOVE WS-CHGBK-AH-PCT        TO OUT-AH-COMM-PCT
           MOVE CM-CERT-SFX            TO OUT-CERT-SFX
           MOVE CM-CONTROL-PRIMARY     TO MA-CONTROL-PRIMARY

           COMPUTE WS-WORK-PREM =
072312        (CM-LF-PREMIUM-AMT + CM-LF-ALT-PREMIUM-AMT
072312           + CM-AH-PREMIUM-AMT)
           MOVE WS-WORK-PREM           TO OUT-TOT-PRM
072312                                    OUT-ORIG-TOT-PREM

092412     COMPUTE WS-WORK-REF =
092412        (WS-WORK-LF-REF + WS-WORK-AH-REF)
092412     MOVE WS-WORK-REF            TO OUT-TOT-REF
072312                                    OUT-ORIG-TOT-REF

           IF CM-LF-LOAN-EXPIRE-DT > CM-AH-LOAN-EXPIRE-DT
              MOVE OUT-LF-EXP-DT       TO OUT-SCHED-EXP-DT
           ELSE
              MOVE OUT-AH-EXP-DT       TO OUT-SCHED-EXP-DT
           END-IF
072312     MOVE OUT-SCHED-EXP-DT       TO OUT-ORIG-SCHED-EXP-DT

           IF CM-LF-ORIG-TERM > CM-AH-ORIG-TERM
              MOVE CM-LF-ORIG-TERM     TO OUT-SCHED-TERM
           ELSE
              MOVE CM-AH-ORIG-TERM     TO OUT-SCHED-TERM
           END-IF
072312     MOVE OUT-SCHED-TERM         TO OUT-ORIG-SCHED-TERM

062017     IF CM-INT-ON-REFS NUMERIC
062017        IF CM-INT-ON-REFS NOT = ZEROS
062017           MOVE CM-INT-ON-REFS   TO OUT-TOT-INTEREST
              END-IF
           END-IF
110612
080522     IF (BL-DATA-SRCE = '4')
080522        AND (PROCESS-CANCEL)
110612        IF PB-C-LF-CANCEL-DT NOT = LOW-VALUES AND SPACES
110612            MOVE WS-WORK-LF-REF TO WS-WORK-CMB-LF-PREM
110612        END-IF
110612        IF PB-C-AH-CANCEL-DT NOT = LOW-VALUES AND SPACES
110612            MOVE WS-WORK-AH-REF TO WS-WORK-AH-PREM
110612        END-IF
110612     END-IF
120412*
120412*     IF BL-DATA-SRCE = '2' 
120412*        IF CM-LF-CANCEL-DT NOT = LOW-VALUES AND SPACES
120412*            MOVE WS-WORK-LF-REF TO WS-WORK-CMB-LF-PREM
120412*        END-IF
120412*        IF CM-AH-CANCEL-DT NOT = LOW-VALUES AND SPACES
120412*            MOVE WS-WORK-AH-REF TO WS-WORK-AH-PREM
120412*        END-IF
120412*     END-IF
110612
      *   display ' build cert ' bl-cert-no
      *   display ' ws work lf ref ' ws-work-lf-ref
      *   display ' ws-chgbk-life-pct ' ws-chgbk-life-pct
      *   display ' ws-work-lf-prem ' ws-work-lf-prem
      *   display ' ws-work-alt-lf-prem ' ws-work-alt-lf-prem
      *   display ' ws-work-cmb-lf-prem ' ws-work-cmb-lf-prem

      *   display ' ws-chgbk-ah-pct ' ws-chgbk-ah-pct
      *   display ' ws-work-ah-prem ' ws-work-ah-prem


110612     COMPUTE WS-ACCT-LF-PORTION = 
120412        WS-WORK-LF-PREM * WS-CHGBK-LIFE-PCT
110612     MOVE WS-ACCT-LF-PORTION TO OUT-ACCT-LF-PORTION
110612                                OUT-ACCT-ORIG-LF-PORT
110612     COMPUTE WS-ACCT-ALT-LF-PORTION = 
120412        WS-WORK-ALT-LF-PREM * WS-CHGBK-LIFE-PCT
110612     MOVE WS-ACCT-ALT-LF-PORTION TO OUT-ACCT-ALT-LF-PORT
110612                                OUT-ACCT-ORIG-ALT-LF-PORT
110612     COMPUTE WS-ACCT-CMB-LF-PORTION = 
120412        WS-WORK-CMB-LF-PREM * WS-CHGBK-LIFE-PCT
110612     MOVE WS-ACCT-CMB-LF-PORTION TO OUT-ACCT-CMB-LF-PORT
110612                                OUT-ACCT-ORIG-CMB-LF-PORT
110612     COMPUTE WS-ACCT-AH-PORTION = 
120412        WS-WORK-AH-PREM * WS-CHGBK-AH-PCT
110612     MOVE WS-ACCT-AH-PORTION TO OUT-ACCT-AH-PORTION
110612                                OUT-ACCT-ORIG-AH-PORT
110612     COMPUTE WS-ACCT-PORTION =
110612        WS-ACCT-CMB-LF-PORTION + WS-ACCT-AH-PORTION
110612     MOVE WS-ACCT-PORTION TO OUT-ACCT-PORTION
110612                                OUT-ACCT-ORIG-PORTION
110612
110612     COMPUTE WS-CSO-LF-PORTION = 
110612       WS-WORK-LF-PREM - WS-ACCT-LF-PORTION
110612     MOVE WS-CSO-LF-PORTION TO OUT-CSO-LF-PORTION
110612                               OUT-CSO-ORIG-LF-PORT
110612     COMPUTE WS-CSO-ALT-LF-PORTION = 
110612       WS-WORK-ALT-LF-PREM - WS-ACCT-ALT-LF-PORTION
110612     MOVE WS-CSO-ALT-LF-PORTION TO OUT-CSO-ALT-LF-PORT
110612                                   OUT-CSO-ORIG-ALT-LF-PORT
110612     COMPUTE WS-CSO-CMB-LF-PORTION = 
110612       WS-WORK-CMB-LF-PREM - WS-ACCT-CMB-LF-PORTION
110612     MOVE WS-CSO-CMB-LF-PORTION TO OUT-CSO-CMB-LF-PORT
110612                                   OUT-CSO-ORIG-CMB-LF-PORT
110612     COMPUTE WS-CSO-AH-PORTION = 
110612        WS-WORK-AH-PREM - WS-ACCT-AH-PORTION
110612     MOVE WS-CSO-AH-PORTION TO OUT-CSO-AH-PORTION
110612                               OUT-CSO-ORIG-AH-PORT
110612     COMPUTE WS-CSO-PORTION = 
110612        WS-CSO-CMB-LF-PORTION + WS-CSO-AH-PORTION
110612     MOVE WS-CSO-PORTION TO OUT-CSO-PORTION
110612                            OUT-CSO-ORIG-PORTION

           .
       1110-EXIT.
           EXIT.

       1115-MATCH-COMPANY.

           MOVE BL-COMP-ID             TO WS-ELCNTL-KEY
           MOVE '1'                    TO WS-ELCNTL-REC-TYPE
           MOVE +0                     TO WS-ELCNTL-SEQ-NO
           EXEC CICS READ
              INTO    (CONTROL-FILE)
              DATASET ('ELCNTL')
              RIDFLD  (WS-ELCNTL-KEY)
              RESP    (WS-RESPONSE)
           END-EXEC
           IF RESP-NORMAL           
              MOVE CF-COMPANY-CD       TO WS-COMPANY-CD
           END-IF

           .
       1115-EXIT.
           EXIT.

       1120-MATCH-CARRIER.

           MOVE BL-COMP-ID             TO WS-ELCNTL-KEY
           MOVE '6'                    TO WS-ELCNTL-REC-TYPE
           MOVE BL-CARRIER             TO WS-ELCNTL-CARR
           MOVE +0                     TO WS-ELCNTL-SEQ-NO
           EXEC CICS READ
              INTO    (CONTROL-FILE)
              DATASET ('ELCNTL')
              RIDFLD  (WS-ELCNTL-KEY)
              RESP    (WS-RESPONSE)
           END-EXEC
           IF RESP-NORMAL           
              MOVE CF-MAIL-TO-NAME     TO OUT-CARR-NAME
           ELSE
              MOVE 'UNKNOWN'           TO OUT-CARR-NAME
           END-IF

           .
       1120-EXIT.
           EXIT.

       1125-MATCH-STATE.

           MOVE BL-STATE               TO OUT-STATE
           MOVE BL-COMP-ID             TO WS-ELCNTL-KEY
           MOVE '3'                    TO WS-ELCNTL-REC-TYPE
           MOVE BL-STATE               TO WS-ELCNTL-STATE
           MOVE +0                     TO WS-ELCNTL-SEQ-NO
           EXEC CICS READ
              INTO    (CONTROL-FILE)
              DATASET ('ELCNTL')
              RIDFLD  (WS-ELCNTL-KEY)
              RESP    (WS-RESPONSE)
           END-EXEC
           IF RESP-NORMAL           
              MOVE CF-STATE-NAME       TO OUT-STATE-NAME
           ELSE
              MOVE 'UNKNOWN'           TO OUT-STATE-NAME
           END-IF

           .
       1125-EXIT.
           EXIT.

       1130-MATCH-BENCDS.

           MOVE SPACES                 TO WS-LF-ABBRV
                                          WS-LF-DESC
                                          WS-LF-EARN
                                          WS-WAIT-PER
                                          WS-RET-ELIM
                                          WS-AH-DESC
                                          WS-BEN-DAYS
           IF WS-LF-BENCD NOT = ZEROS AND SPACES
              PERFORM 1140-MATCH-LFBEN THRU 1140-EXIT
           END-IF

           IF WS-AH-BENCD NOT = ZEROS AND SPACES
              PERFORM 1150-MATCH-AHBEN THRU 1150-EXIT
           END-IF

           .
       1130-EXIT.
           EXIT.

       1140-MATCH-LFBEN.

           MOVE BL-COMP-ID             TO WS-ELCNTL-KEY
           MOVE '4'                    TO WS-ELCNTL-REC-TYPE
           MOVE WS-LF-BENCD            TO WS-ELCNTL-BEN-CD
           MOVE +0                     TO WS-ELCNTL-SEQ-NO
           EXEC CICS READ
              INTO    (CONTROL-FILE)
              DATASET ('ELCNTL')
              RIDFLD  (WS-ELCNTL-KEY)
              GTEQ
              RESP    (WS-RESPONSE)
           END-EXEC
           IF RESP-NORMAL           
              PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                 (WS-LF-BENCD = CF-BENEFIT-CODE (S1))
                 OR (S1 > +8)
              END-PERFORM
              IF S1 < + 9
                 MOVE CF-BENEFIT-ALPHA (S1)
                                       TO WS-LF-ABBRV
                 MOVE WS-LF-BENCD      TO WS-LF-DESC
                 MOVE CF-CO-EARNINGS-CALC (S1)
                                       TO WS-LF-EARN
              ELSE
                 MOVE 'XX'             TO WS-LF-DESC
                 MOVE 'XXX'            TO WS-LF-ABBRV
              END-IF
           ELSE
              DISPLAY ' NO LF BEN CD ' WS-LF-BENCD
           END-IF

           .
       1140-EXIT.
           EXIT.

       1150-MATCH-AHBEN.

           MOVE BL-COMP-ID             TO WS-ELCNTL-KEY
           MOVE '5'                    TO WS-ELCNTL-REC-TYPE
           MOVE WS-AH-BENCD            TO WS-ELCNTL-BEN-CD
           MOVE +0                     TO WS-ELCNTL-SEQ-NO
           EXEC CICS READ
              INTO    (CONTROL-FILE)
              DATASET ('ELCNTL')
              RIDFLD  (WS-ELCNTL-KEY)
              GTEQ
              RESP    (WS-RESPONSE)
           END-EXEC
           IF RESP-NORMAL           
              PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                 (WS-AH-BENCD = CF-BENEFIT-CODE (S1))
                 OR (S1 > +8)
              END-PERFORM
              IF S1 < + 9
                 MOVE CF-BENEFIT-ALPHA (S1)
                                       TO WS-WAIT-PER
                 MOVE CF-BENEFIT-ALPHA (S1) (3:1)
                                       TO WS-RET-ELIM
                 MOVE CF-BENEFIT-ALPHA (S1) (1:2)
                                       TO WS-BEN-DAYS
                 IF CF-SPECIAL-CALC-CD (S1) = 'C'
                    IF CF-JOINT-INDICATOR (S1) = 'J'
                       MOVE 'Joint Critical Period Disability'
                                       TO WS-AH-DESC
                    ELSE
072312                 MOVE 'Single Critical Period-Primary Only'
                                       TO WS-AH-DESC
                    END-IF
                 ELSE
                    IF CF-JOINT-INDICATOR (S1) = 'J'
                       MOVE 'Joint Disability'
                                       TO WS-AH-DESC
                    ELSE
072312                 MOVE 'Single Disability (Primary Only)'
                                       TO WS-AH-DESC
                    END-IF
                 END-IF
              ELSE
                 MOVE 'NOT FOUND'      TO WS-AH-DESC
              END-IF
           ELSE
              DISPLAY ' NO AH BEN CD ' WS-AH-BENCD
           END-IF

           .
       1150-EXIT.
           EXIT.
121712
121712 1160-READ-CERT-TRAILER.
121712
121712     MOVE +0                     TO  WS-CERT-TRL-REC-NOT-FOUND.
121712     MOVE WS-ELCERT-KEY          TO  WS-ELCRTT-PRIMARY.
121712     MOVE 'C'                    TO  WS-ELCRTT-REC-TYPE.
121712
121712     EXEC CICS HANDLE CONDITION
121712         NOTFND (1160-CERT-TRL-REC-NOTFND)
121712     END-EXEC.
121712
121712     EXEC CICS READ
121712         DATASET  ('ELCRTT')
121712         RIDFLD   (WS-ELCRTT-KEY)
121712         INTO     (CERTIFICATE-TRAILERS)
121712     END-EXEC.
121712
121712     GO TO 1160-EXIT.
121712
121712 1160-CERT-TRL-REC-NOTFND.
121712     MOVE +1                     TO WS-CERT-TRL-REC-NOT-FOUND.
121712
121712 1160-EXIT.
121712     EXIT.

       1200-GET-ERACCT.

           MOVE WS-ERACCT-KEY          TO WS-SAVE-ERACCT-KEY
           MOVE ' UNKNOWN '            TO OUT-ACCT-NAME
                                          OUT-ACCT-ADDR1
                                          OUT-ACCT-ADDR2
                                          OUT-ACCT-CITY
072312     MOVE ZEROS                  TO WS-CHGBACK

           EXEC CICS STARTBR                                            
               DATASET   ('ERACCT')
               RIDFLD    (WS-ERACCT-KEY)
               GTEQ
               RESP      (WS-RESPONSE)
           END-EXEC
           
           IF RESP-NORMAL
              EXEC CICS READNEXT
                 INTO    (ACCOUNT-MASTER)
                 DATASET ('ERACCT')
                 RIDFLD  (WS-ERACCT-KEY)
                 RESP    (WS-RESPONSE)
              END-EXEC
           END-IF

           IF RESP-NORMAL
              PERFORM UNTIL
                 NOT (RESP-NORMAL)
                 OR (WS-SAVE-ERACCT-KEY NOT =
                              AM-CONTROL-PRIMARY (1:20))
                 IF BL-DATA-SRCE = '4' OR '2'
                    IF (WS-CERT-EFF-DT < AM-EXPIRATION-DT
                       AND >= AM-EFFECTIVE-DT)
                       PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                          (AM-COM-TYP (S1) = 'C' OR 'D')
                          OR (S1 > +10)
                       END-PERFORM
                       IF S1 < +11
                          MOVE AM-COMM-CHARGEBACK (S1)
072312                                 TO WS-CHGBACK
                       END-IF
                    END-IF
                 END-IF
                 MOVE ACCOUNT-MASTER   TO WS-SAVE-ERACCT-REC
                 EXEC CICS READNEXT
                    INTO    (ACCOUNT-MASTER)
                    DATASET ('ERACCT')
                    RIDFLD  (WS-ERACCT-KEY)
                    RESP    (WS-RESPONSE)
                 END-EXEC
              END-PERFORM
              MOVE WS-SAVE-ERACCT-REC  TO ACCOUNT-MASTER
              IF WS-SAVE-ERACCT-KEY = AM-CONTROL-PRIMARY (1:20)
                 SET ACCT-FOUND TO TRUE
                 MOVE AM-NAME          TO OUT-ACCT-NAME
                 MOVE AM-ADDRS         TO OUT-ACCT-ADDR1
                 MOVE SPACES           TO OUT-ACCT-ADDR2
                 MOVE AM-ADDR-CITY     TO OUT-ACCT-CITY
                 MOVE AM-ADDR-STATE    TO OUT-ACCT-STATE
                 MOVE AM-ZIP-PRIME     TO OUT-ACCT-ZIP
091213           MOVE AM-CONTROL-NAME  TO OUT-ACCT-CNTRL-NAME
                 MOVE AM-REPORT-CODE-1 TO OUT-RPT-CD1
                 MOVE AM-REPORT-CODE-2 TO OUT-RPT-CD2
                 IF AM-ZIP-PLUS4 NOT = SPACES AND ZEROS
                    STRING '-' AM-ZIP-PLUS4 DELIMITED BY SIZE
                       INTO OUT-ACCT-ZIP (6:5)
                    END-STRING
                 END-IF
                 MOVE AM-TEL-NO        TO OUT-ACCT-PHONE
                 MOVE AM-GPCD          TO OUT-ACCT-BUS-TYPE
                 MOVE AM-STATE         TO OUT-FORM
                 IF AM-ACCOUNT (10:1) = 'L'
                    MOVE 'L'           TO OUT-FORM (3:1)
                                          OUT-LEASE-IND
                 END-IF
                 MOVE AM-CONTROL-PRIMARY (1:8)
                                       TO WS-ERCOMP-KEY
                 IF AM-REMIT-TO NOT NUMERIC
                    MOVE 01            TO AM-REMIT-TO
                 END-IF
                 MOVE AM-AGT (AM-REMIT-TO) TO WS-ERCOMP-RESP-NO
                 PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                    (AM-COM-TYP (S1) = 'C' OR 'D')
                    OR (S1 > +10)
                 END-PERFORM
                 IF S1 < +11
                    MOVE AM-AGT (S1)   TO WS-ERCOMP-ACCOUNT
                 END-IF
                 MOVE 'A'              TO WS-ERCOMP-REC-TYPE
                 PERFORM 1225-GET-ERCOMP THRU 1225-EXIT
              END-IF
           END-IF

072312     EXEC CICS ENDBR                                            
072312         DATASET   ('ERACCT')
072312     END-EXEC

          .
       1200-EXIT.
           EXIT.

       1225-GET-ERCOMP.

           EXEC CICS READ
              INTO    (COMPENSATION-MASTER)
              DATASET ('ERCOMP')
              RIDFLD  (WS-ERCOMP-KEY)
              RESP    (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              MOVE CO-RESP-NO          TO OUT-RESP-NO
              MOVE CO-ACCT-NAME        TO OUT-COMP-NAME
              MOVE CO-MAIL-NAME        TO OUT-COMP-MAIL-TO
              MOVE CO-ADDR-1           TO OUT-COMP-ADDR1
              MOVE CO-ADDR-2           TO OUT-COMP-ADDR2
              MOVE CO-ADDR-CITY        TO OUT-COMP-CITY
              MOVE CO-ADDR-STATE       TO OUT-COMP-STATE
              MOVE CO-ZIP              TO OUT-COMP-ZIP
              MOVE CO-TELEPHONE        TO OUT-COMP-PHONE
              MOVE CO-FAXNO            TO OUT-COMP-FAX
              MOVE CO-GA-STATUS-CODE   TO OUT-COMP-STATUS
              MOVE CO-BILL-SW          TO OUT-BILL-SW
           END-IF

           .
       1225-EXIT.
           EXIT.

       1250-GET-ELLETR.

           move spaces                 to W-Z-CONTROL-DATA
           MOVE WS-COMPANY-CD          TO WS-ELLETR-COMPANY-CD
           MOVE BL-LETTER-ID           TO WS-ELLETR-LETTER-ID
           MOVE +0                     TO WS-ELLETR-SEQ-NO

           EXEC CICS READ                                               
                DATASET    ('ELLETR') 
                INTO       (TEXT-FILES)
                RIDFLD     (WS-ELLETR-KEY)
                RESP       (WS-RESPONSE)
                GTEQ
           END-EXEC

           IF RESP-NORMAL
              IF (LETTER-FILE-TEXT)
                 AND (BL-LETTER-ID = TX-LETTER-NO)
                 AND (TX-LINE-SQUEEZE-CONTROL = 'Z')
                 display ' found good z record '
                 PERFORM 1280-PROCESS-Z-CONTROLS
                                       THRU 1280-EXIT
              END-IF
           END-IF

           .
       1250-EXIT.
           EXIT.


       1280-PROCESS-Z-CONTROLS.

           MOVE TX-TEXT-LINE           TO W-Z-CONTROL-DATA

      *    IF W-FORM-TO-RESEND > SPACES
      *       MOVE W-FORM-TO-RESEND    TO OUT-FORM
      *    ELSE
      *       MOVE SPACES              TO OUT-FORM
      *    END-IF
           
091213     IF BL-ENC-CD = SPACES OR LOW-VALUES
091213        MOVE W-ENCLOSURE-CD         TO BL-ENC-CD
091213     END-IF
110612     MOVE W-LETTER-TYPE          TO OUT-LETTER-TYPE
110612     MOVE W-PRINT-CERTIFICATE    TO OUT-PRINT-CERTIFICATE
           .
       1280-EXIT.
           EXIT.

       1300-DETERMINE-COMM.

      ******************************************************************
      * DATA SOURCE MEANINGS  BL-DATA-SRCE
      *    1) FROM ACCT MAINT
      *    2) FROM CERT UPDATE
      *    3) FROM COMP MAINT
      *    4) FROM REVIEW AND CORRECTIONS
      ******************************************************************

072312     MOVE 'Y'                    TO OUT-CHGBACK
           MOVE ZEROS                  TO WS-DIFF
120412                                    WS-CHGBK-LIFE-PCT
120412                                    WS-CHGBK-AH-PCT

           IF BL-DATA-SRCE = '4'
              IF PB-CANCELLATION
120412           MOVE PB-CI-LIFE-COMMISSION TO WS-CHGBK-LIFE-PCT
120412           MOVE PB-CI-AH-COMMISSION   TO WS-CHGBK-AH-PCT
                 MOVE PB-CERT-EFF-DT   TO DC-BIN-DATE-1
                 MOVE PB-C-AH-CANCEL-DT TO DC-BIN-DATE-2
                 IF PB-C-LF-CANCEL-DT > DC-BIN-DATE-2
                    MOVE PB-C-LF-CANCEL-DT
                                       TO DC-BIN-DATE-2
                 END-IF
                 MOVE '1'              TO DC-OPTION-CODE
                 PERFORM 9700-DATE-LINK THRU 9700-EXIT
                 IF NO-CONVERSION-ERROR
                    MOVE DC-ELAPSED-MONTHS TO WS-DIFF
                    IF DC-ODD-DAYS-OVER > +1
                       ADD +1          TO WS-DIFF
                    END-IF
072312              IF (WS-CHGBACK = 99)
072312                 OR (WS-DIFF > WS-CHGBACK AND WS-CHGBACK > 0)
120412                   MOVE +0   TO WS-CHGBK-LIFE-PCT
120412                                WS-CHGBK-AH-PCT
120412                   MOVE 'N'  TO OUT-CHGBACK                          
                    END-IF
                 END-IF
120412        ELSE
120412           MOVE PB-I-LIFE-COMMISSION TO WS-CHGBK-LIFE-PCT
120412           MOVE PB-I-AH-COMMISSION   TO WS-CHGBK-AH-PCT
              END-IF
           ELSE
120412        MOVE CM-LIFE-COMM-PCT TO WS-CHGBK-LIFE-PCT
120412        MOVE CM-AH-COMM-PCT   TO WS-CHGBK-AH-PCT
              IF (CM-LF-CANCEL-DT NOT = LOW-VALUES AND SPACES)
                 OR (CM-AH-CANCEL-DT NOT = LOW-VALUES AND SPACES)
                 MOVE CM-CERT-EFF-DT   TO DC-BIN-DATE-1
                 MOVE CM-AH-CANCEL-DT  TO DC-BIN-DATE-2
                 IF CM-LF-CANCEL-DT > DC-BIN-DATE-2
                    MOVE CM-LF-CANCEL-DT TO DC-BIN-DATE-2
                 END-IF
                 MOVE '1'              TO DC-OPTION-CODE
                 PERFORM 9700-DATE-LINK THRU 9700-EXIT
                 IF NO-CONVERSION-ERROR
                    MOVE DC-ELAPSED-MONTHS TO WS-DIFF
                    IF DC-ODD-DAYS-OVER > +1
                       ADD +1          TO WS-DIFF
                    END-IF
072312              IF (WS-CHGBACK = 99)
072312                 OR (WS-DIFF > WS-CHGBACK AND WS-CHGBACK > 0)
120412                   MOVE +0   TO WS-CHGBK-LIFE-PCT
120412                                WS-CHGBK-AH-PCT
120412                   MOVE 'N'  TO OUT-CHGBACK
                    END-IF
                 END-IF
              END-IF
           END-IF

           .
       1300-EXIT.
           EXIT.

       1500-GET-ARCH-NO.

           MOVE BL-COMP-ID             TO WS-ELCNTL-COMPANY-ID
           MOVE '1'                    TO WS-ELCNTL-REC-TYPE
           MOVE SPACES                 TO WS-ELCNTL-GENL
           MOVE +0                     TO WS-ELCNTL-SEQ-NO

           EXEC CICS READ
              INTO    (CONTROL-FILE)
              DATASET ('ELCNTL')
              RIDFLD  (WS-ELCNTL-KEY)
              UPDATE
              RESP    (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              AND (CF-COMPANY-ID  = BL-COMP-ID)
              AND (CF-RECORD-TYPE = '1')
              ADD +1                   TO CF-CREDIT-LAST-ARCH-NUM
              MOVE CF-CREDIT-LAST-ARCH-NUM
                                       TO WS-ARCHIVE-NO
              EXEC CICS REWRITE
                 FROM    (CONTROL-FILE)
                 DATASET ('ELCNTL')
              END-EXEC
           ELSE
              MOVE +0                  TO WS-ARCHIVE-NO
           END-IF

          .
       1500-EXIT.
           EXIT.

       1700-GET-ELENCC.
           MOVE WS-COMPANY-CD          TO WS-ELENCC-KEY
           MOVE '2'                    TO WS-ELENCC-REC-TYPE
           MOVE BL-ENC-CD              TO WS-ELENCC-ENC-CODE
           PERFORM VARYING E1 FROM +1 BY +1 UNTIL
              (WS-ELENCC-ENC-CODE (E1:1) = ' ')
              OR (E1 > +5)
           END-PERFORM

           IF E1 < +5
              MOVE BL-STATE            TO WS-ELENCC-ENC-CODE (E1:2)
           END-IF
           EXEC CICS READ
               DATASET   ('ELENCC')
               INTO      (ENCLOSURE-CODES)
               RIDFLD    (WS-ELENCC-KEY)
               RESP      (WS-RESPONSE)
           END-EXEC
           IF NOT RESP-NORMAL
              display ' elencc resp not normal ' ws-response
              MOVE BL-ENC-CD           TO WS-ELENCC-ENC-CODE
              EXEC CICS READ
                  DATASET   ('ELENCC')
                  INTO      (ENCLOSURE-CODES)
                  RIDFLD    (WS-ELENCC-KEY)
                  RESP      (WS-RESPONSE)
              END-EXEC
           END-IF
           IF RESP-NORMAL
              MOVE NC-OUTPUT-STACK     TO OUT-STACK
              MOVE NC-ENCLOSURE-LINE   TO OUT-ENC-LINE
              MOVE NC-ATTACHMENTS      TO OUT-ATTACH
           END-IF

           .
       1700-EXIT.
           EXIT.
070622 2000-GET-CANCEL-REFUND.
070622     IF NOT CONNECTED-TO-DB
070622        PERFORM 4150-CONNECT-TO-DB-CHECK THRU 4150-EXIT
070622     END-IF
070622
070622     MOVE WS-ELCERT-CARRIER           TO WS-CARRIER
070622     MOVE WS-ELCERT-GROUP             TO WS-GROUP
070622     MOVE WS-ELCERT-STATE             TO WS-STATE
070622     MOVE WS-ELCERT-ACCOUNT           TO WS-ACCOUNT
070622     MOVE OUT-CERT-EFF-DT             TO WS-EFF-DT
070622     MOVE WS-ELCERT-CERT-NO           TO WS-CERT-NO
070622     MOVE WS-ELCERT-CERT-NO(11:1)     TO WS-CERT-NO-SUF
070622
070622     EXEC SQL
070622        SELECT TOP 1
070622           Company,
070622           CertCarrier,
070622           CertGroup,
070622           CertState,
070622           CertAccount,
070622           CertEffDate,
070622           CertNumber,
070622           CertNumberSuf,
070622           CheckSeqNbr,
070622           CheckType,
070622           CheckAmount,
070622           PayeeName1,
070622           PayeeAddress1,
070622           PayeeCity,
070622           PayeeState,
070622           PayeeZIP,
070622           Preparer,
070622           CheckSubType,
070622           PayeeCode
070622        INTO
070622           :db-compid,
070622           :db-carrier,
070622           :db-grouping,
070622           :db-state,
070622           :db-account,
070622           :db-effdate,
070622           :db-certificate,
070622           :db-cert-sfx,
070622           :db-seq-no,
070622           :db-type,
070622           :db-amount-n,
070622           :db-payeename1,
070622           :db-payeeaddr1,
070622           :db-payeecity,
070622           :db-payeest,
070622           :db-payeezip,
070622           :db-preparer,
070622           :db-check-sub-type,
070622           :db-payeecode
070622        FROM
070622           ChkApp_Check
070622        WHERE
070622                  CertCarrier = :WS-CARRIER
070622             and  CertGroup   = :WS-GROUP
070622             and  CertState   = :WS-STATE
070622             and  CertAccount = :WS-ACCOUNT
070622*            and  CertEffDate = :WS-EFF-DT
070622             and  CertNumber  = :WS-CERT-NO
070622             AND  CertNumberSuf = :WS-CERT-NO-SUF
070622             AND CheckSubType = 1
070622        ORDER BY RELEASEDATE DESC
070622     end-exec
070622
070622     IF SQLCODE = 0
070622       OR SQLCODE = 1
070622        MOVE db-payeename1  TO OUT-REF-PAYEE
070622        MOVE db-payeeaddr1  TO OUT-REF-ADDRESS
070622        MOVE db-payeecity   TO OUT-REF-CITY
070622        MOVE db-payeest     TO OUT-REF-STATE
070622        MOVE db-payeezip    TO OUT-REF-ZIP
070622        MOVE db-amount-n    TO OUT-REF-CHK-AMOUNT
070622        MOVE db-payeecode   TO OUT-REF-PAYEE-CODE
070622     ELSE
070622        DISPLAY ' DIS SQL CODE ' SQLCODE
070622        DISPLAY "ERROR: CANNOT READ ROW ChkApp_Check "
070622        DISPLAY ' SQL RETURN CODE ' SQLCODE
070622        DISPLAY ' SQL ERR MESS    ' SQLERRMC
070622     END-IF
070622
070622     PERFORM 4300-DISCONNECT THRU 4300-EXIT
070622     MOVE SPACES TO WS-CONNECT-SW
070622
070622     .
070622
070622 2000-EXIT.
070622     EXIT.
011514           
011514           
011514 4100-CONNECT-TO-DB.
011514

      ****  The below code is for when the db has been
      ****  converted to sql server 2016
           evaluate ws-kix-myenv
             when 'cid1p'
                 move '//sdv-db01.cso.local:1433;'
                                       to p-sql-server
              when 'mdoff'
                 move '//hov-tstdb01.cso.local:55330;'
                                       to p-sql-server
              when other
                 move '//hov-tstdb01.cso.local:1433;'
                                       to p-sql-server
           end-evaluate


           move 'Logic'                to p-sql-database

           CALL 'SQLCONNECT' USING sqlconnect-parms
           display ' ret code ' p-connect-return-code
           move p-connect-return-code  to sqlcode
           move p-sql-return-message   to sqlerrmc

011514     IF SQLCODE NOT = 0
011514        DISPLAY "ERROR: CANNOT CONNECT "
011514        DISPLAY SQLCODE
011514        DISPLAY SQLERRMC
011514        GO TO 4100-EXIT
011514     END-IF
011514
011514     .
011514 4100-EXIT.
011514     EXIT.

011514 4200-GET-NEXT-BUS-DT.
011514
011514     MOVE WS-SAVE-EDIT-A-DATE    TO WS-CYCLE-DATE
011514     MOVE SPACES                 TO WS-NEXT-BUS-DT
011514
011514     IF WS-KIXHOST = 'logictest'
011514        EXEC SQL
011514          CALL NaperTestCalcNextBusDt 
                  (
011514              @cycledate = :WS-CYCLE-DATE,
PEMMOD              @nextbusdate = :WS-NEXT-BUS-DT
                  )
011514        END-EXEC
011514     ELSE
011514        EXEC SQL
011514          CALL NaperProdCalcNextBusDt 
                  (
011514              @cycledate = :WS-CYCLE-DATE,
PEMMOD              @nextbusdate = :WS-NEXT-BUS-DT
                  )
011514        END-EXEC
011514     END-IF
011514
011514     IF SQLCODE NOT = 0
011514        DISPLAY "ERROR: DID NOT RETURN NEXT BUS DT "
011514        DISPLAY ' SQL RETURN CODE ' SQLCODE
011514        DISPLAY ' SQL ERR MESS    ' SQLERRMC
011514        GO TO 4200-EXIT
011514     END-IF
011514
011514     .
011514 4200-EXIT.
011514     EXIT.
011514
011514
021214
021214 4300-DISCONNECT.
021214
021214     EXEC SQL
021214        DISCONNECT
021214     END-EXEC
021214     .
021214 4300-EXIT.
021214     EXIT.
021214
020916
020916 4500-CHECK-BALLOON-STATE.
020916
           move spaces                 to connect-string

           IF WS-KIXHOST = 'logictest'
              MOVE '1020'              TO WS-LOOKUPID
              move '//hov-tstdb01.cso.local:1433;'
                                       to p-sql-server
           else
              MOVE '1020'              TO WS-LOOKUPID
              move '//sdv-db01.cso.local:1433;'
                                       to p-sql-server
           end-if

           string
              'jdbc:sqlserver:' delimited size
              p-sql-server delimited space
              'databaseName=' delimited size
              into connect-string
           end-string
           display ' connection string = **' connect-string '**'

           SET ENVIRONMENT "jdbc.url" to connect-string

           move 'NapersoftRepository'  to p-sql-database

           move p-sql-database         to database
061421     MOVE 'naperadmin'           TO USR
061421     MOVE 'cCm8naper'            TO PASS

020916     MOVE SPACES                 TO WS-LOOKUP-VALUE
020916
020916*    PERFORM 4100-CONNECT-TO-DB  THRU 4100-EXIT
            exec sql
              connect to :database
               user      :usr
               using     :pass
            end-exec

061821     MOVE CM-STATE TO WS-LOOKUPNAME
061821     MOVE 'B'      TO WS-LOOKUPNAME (3:1)
020916
020916     EXEC SQL
020916           SELECT LOOKUPVALUE
020916             INTO :WS-LOOKUP-VALUE
020916             FROM LOOKUPVALUES
020916               WHERE LOOKUPID = :WS-LOOKUPID
061821                 AND LOOKUPNAME = :WS-LOOKUPNAME
020916     END-EXEC
020916
020916     IF SQLCODE = 0
020916       OR SQLCODE = 100
020916        CONTINUE
020916     ELSE
020916        DISPLAY "ERROR: INVALID BALLOON STATE SELECT "
020916        DISPLAY ' SQL RETURN CODE ' SQLCODE
020916        DISPLAY ' SQL ERR MESS    ' SQLERRMC
020916     END-IF
020916
020916     PERFORM 4300-DISCONNECT THRU 4300-EXIT.
020916
020916 4500-EXIT.
020916     EXIT.
       9700-DATE-LINK.                                                  

           EXEC CICS LINK                                               
               PROGRAM   ('ELDATCV')
               COMMAREA  (DATE-CONVERSION-DATA)                         
               LENGTH    (DC-COMM-LENGTH)                               
           END-EXEC.                                                    
                                                                        
                                                                        
       9700-EXIT.                                                       
            EXIT.
