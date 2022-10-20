       IDENTIFICATION DIVISION.                                         
      
       PROGRAM-ID.                 EL6318.
      *
      *AUTHOR.        PABLO
      *               OMAHA, NEBRASKA
      
      *DATE-COMPILED.
      
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CENTRAL STATES  *
      *            *   HEALTH AND LIFE                                 *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF CSO.        IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CENTRAL STATES. *
      *            *                                                   *
      *            *****************************************************
      *-----------------------------------------------------------------
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE  CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      *          2011022800001  PEMA  NEW PROGRAM
012412* 012412   2011022800001  AJRA  NAPERSOFT
060612* 060612   2011022800001  AJRA  NAPERSOFT
062712* 062712   2011022800001  AJRA  REDEFINE ORIG DATA
072312* 072312   2011022800001  AJRA  NAPERSOFT MISC
102212* 102212   2012101700002  AJRA  SCREEN ID
103012* 103012   2012101700002  AJRA  ADD CERT NOTES TO TOP OF LIST
110612* 110612   2012101700002  AJRA  ADD NEW FIELDS
112612* 112612   2012101700002  AJRA  CHECK FOR PROMPT LETTER
121012* 121012   2012101700002  AJRA  HIGHLIGHT REASON CODES
121112* 121112   2012101700002  AJRA  CHANGE PF1, PF2 TO PF3, PF4
121212* 121212   2012101700002  AJRA  CHG LAST 2 CERT NOTES TO BILLING NT
121712* 121712   2012101700002  AJRA  ADD DEFAULT AGE FLAG
122712* 122712   2012122700001  AJRA  FIX CERT PRINT FLAG
011413* 011413 IR2012122700003  AJRA  ADD CRTO ISSUE/CANCEL INDICATOR
091913* 091913   2013090300001  AJRA  ADD SIGNATURE FLAG DEFAULT
120313* 120313   2013090300001  AJRA  NAPERSOFT PHASE 2
121713* 121713   2013090300001  AJRA  ADD ENCLOSURE CODE TO SCREEN
091615* 091615 CR2015082000001  PEMA  ADD XFR TO EL677
020218* 020218 CR2017062000002  PEMA  AUDIT NB FOR PREV CLAIMS
041320* 041320 CR2020030500002  PEMA  Issue, cancel billing notes
      *-----------------------------------------------------------------
      
      *REMARKS.
      *        TRANSACTION - EXBE - GENERAL CHANGE ENDORSEMENT
       EJECT
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32)  VALUE '********************************'.
       77  FILLER  PIC X(32)  VALUE '*    EL6318 WORKING STORAGE    *'.
       77  FILLER  PIC X(32)  VALUE '******** VMOD=2.001 ************'.
       77  NOTE-SUB PIC S9(5) COMP-3 VALUE +0.
       77  WS-SEQ-NO                   PIC 9(4) BINARY.
       77  WS-SET-CODES-MDT            PIC X  VALUE SPACES.
       77  s1                          pic s999 comp-3 value +0.
       77  s2                          pic s999 comp-3 value +0.
       77  W-ARCH-NUMBER               PIC S9(08)      COMP value +0.
       77  WS-ERACCT-SW                PIC X  VALUE ' '.
           88  ACCT-FOUND                 VALUE 'Y'.
072312 77  WS-CHGBACK                 PIC 99  VALUE ZEROS.
       77  WS-CSO-PORTION              PIC S9(5)V99 COMP-3 VALUE +0.
       77  WS-ACCT-PORTION             PIC S9(5)V99 COMP-3 VALUE +0.
       77  WS-DIFF                     PIC 99 VALUE ZEROS.
       77  WS-SAVE-ERACCT-KEY          PIC X(20)  VALUE SPACES.
       77  WS-DUE-BORROWER             PIC S9(7)V99 COMP-3 VALUE +0.
060612 77  WS-REASON-SW                PIC X  VALUE ' '.
060612     88  REASONS-FOUND                  VALUE 'Y'.
060612     88  REASONS-NOT-FOUND              VALUE 'N'.
072312 77  WS-CERT-UPDATE-SW           PIC X  VALUE ' '.
072312     88  NO-CERT-RW                 VALUE 'N'.
072312     88  CERT-RW                    VALUE 'Y'.
091615 77  ws-el677-check-cut         pic x value 'N'.
091615 77  ws-el677-check-type        pic x value 'N'.
020218 77  ws-elcrtt-update-sw        pic x value ' '.
020218     88  update-elcrtt             value 'Y'.      
       01  P pointer.
       01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
       01  KIXHOST             pic x(9) value Z"HOSTNAME".
       01  var-ptr pointer.
       01  env-var-len                 pic 9(4)  binary.
       01  rc                          pic 9(9)  binary.

       01  WS-KIXHOST                  PIC X(10).
       01  WS-KIXSYS.
           05  WS-KIX-FIL1             PIC X(10).
           05  WS-KIX-APPS             PIC X(10).
           05  WS-KIX-ENV              PIC X(10).
           05  WS-KIX-MYENV            PIC X(10).
           05  WS-KIX-SYS              PIC X(10).

                                       COPY ELCSCTM.
                                       COPY ELCSCRTY.
       01 srch-commarea.
                                       copy ELCADLTRSPI.
012412
012412*** Z CONTROL LAYOUT MOVED TO COPYBOOK ELCZREC
012412                                 COPY ELCZREC.
     
      
       01  WS-PASSED-REASON-CODES      PIC X(60)  VALUE SPACES.
      
       01  STANDARD-AREAS.
           12  W-ARCH-SUPPRESS         PIC ZZZZZZZ9.
           12  W-ARCH-EDIT REDEFINES W-ARCH-SUPPRESS
                                       PIC  X(08).
           12  W-ARCH-LENGTH           PIC S9(04)  COMP  VALUE +250.
           12  QID.
               16  QID-TERM        PIC X(4)    VALUE SPACES.
               16  FILLER          PIC X(4)    VALUE '631K'.
           12  MAP-LENGTH          PIC 9(4)   VALUE 359 BINARY.
           12  TIME-MT                 PIC S9(7).
           12  TIME-MT-R  REDEFINES TIME-MT.
               16  FILLER              PIC X.
               16  TIME-LMT            PIC 99V99.
               16  FILLER              PIC X(2).
           12  GETMAIN-SPACE           PIC X VALUE SPACES.
           12  ERENDT-LENGTH           PIC S9(04) VALUE +579 COMP.
           12  ERPNDB-LENGTH           PIC 9(04)  VALUE 585 COMP.
072312     12  ERCNOT-LENGTH           PIC S9(04) VALUE +150 COMP.
072312     12  ERNOTE-LENGTH           PIC S9(04) VALUE +825 COMP.
072312     12  ELEOBC-LENGTH           PIC S9(04) VALUE +350 COMP.
121712     12  ELCRTT-LENGTH           PIC S9(4) COMP VALUE +552.
121713     12  ELENCC-LENGTH           PIC S9(04) VALUE +400 COMP.
      
      
           12  W-CNTL-KEY.
               16  W-CNTL-COMPANY-ID   PIC  X(03).
               16  W-CNTL-RECORD-TYPE  PIC  X(01)   VALUE '1'.
               16  W-CNTL-GENL.
                   20  W-CNTL-GEN1     PIC  X(02)   VALUE SPACES.
                   20  W-CNTL-GEN2.
                       24 W-CNTL-GEN3  PIC  X(01)   VALUE SPACES.
                       24 W-CNTL-GEN4  PIC  X(01)   VALUE SPACES.
               16  W-CNTL-SEQ-NO       PIC S9(04)   VALUE +0    COMP.
      
072312     12  WS-ELCERT-KEY.
072312         16  W-CERT-COMPANY-CD   PIC X.
072312         16  W-CERT-CARRIER      PIC X.
072312         16  W-CERT-GROUPING     PIC X(6).
072312         16  W-CERT-STATE        PIC XX.
072312         16  W-CERT-ACCOUNT      PIC X(10).
072312         16  W-CERT-CERT-EFF-DT  PIC XX.
072312         16  W-CERT-CERT-PRIME   PIC X(10).
072312         16  W-CERT-CERT-SFX     PIC X.
072312     12  WS-ELEOBC-KEY.
072312         16  WS-EOBC-COMPANY-CD  PIC X.
072312         16  WS-EOBC-REC-TYPE    PIC X.
072312         16  WS-EOBC-CODE        PIC X(4).
072312         16  FILLER              PIC X(9).
           12  WS-ERNOTE-KEY.
               16  W-NOTE-COMPANY-CD   PIC X.
               16  W-NOTE-CARRIER      PIC X.
               16  W-NOTE-GROUPING     PIC X(6).
               16  W-NOTE-STATE        PIC XX.
               16  W-NOTE-ACCOUNT      PIC X(10).
               16  W-NOTE-CERT-EFF-DT  PIC XX.
               16  W-NOTE-CERT-PRIME   PIC X(10).
               16  W-NOTE-CERT-SFX     PIC X.
041320         16  w-note-record-type  pic x.
           12  ELLETR-KEY.
112612         16  LETR-PART-KEY.
112612             20  LETR-COMPANY-CD PIC X.
112612             20  LETR-LETTER-ID  PIC X(4).
               16  LETR-FILLER         PIC X(8).
               16  LETR-SEQ-NO         PIC 9(4) BINARY.
112612     12  ELLETR-SAVE-PART-KEY    PIC X(5).               
           12  ERPNDB2-KEY.
               16  PNDB2-COMPANY-CD    PIC X.
               16  PNDB2-CARRIER       PIC X.
               16  PNDB2-GROUPING      PIC X(6).
               16  PNDB2-STATE         PIC XX.
               16  PNDB2-ACCOUNT       PIC X(10).
               16  PNDB2-CERT-EFF-DT   PIC XX.
               16  PNDB2-CERT-PRIME    PIC X(10).
               16  PNDB2-CERT-SFX      PIC X.
               16  PNDB2-CHG-SEQ-NO    PIC 9(4) BINARY.
               16  PNDB2-RECORD-TYPE   PIC X.
           12  ERPNDB-KEY.
               16  PNDB-COMPANY-CD     PIC X.
               16  PNDB-BATCH-NO       PIC X(6).
               16  PNDB-BATCH-SEQ-NO   PIC 9(4) BINARY.
               16  PNDB-CHG-SEQ-NO     PIC 9(4) BINARY.
           12  ERENDT-KEY.
               16  ENDT-COMPANY-CD       PIC X.
               16  ENDT-CARRIER          PIC X.
               16  ENDT-GROUPING         PIC X(6).
               16  ENDT-STATE            PIC XX.
               16  ENDT-ACCOUNT          PIC X(10).
               16  ENDT-CERT-EFF-DT      PIC XX.
               16  ENDT-CERT-PRIME       PIC X(10).
               16  ENDT-CERT-SFX         PIC X.
               16  ENDT-RECORD-TYPE      PIC X.
               16  ENDT-SEQ-NO           PIC 9(4) BINARY.
           12  ELCRTO-KEY.
               16  CRTO-COMPANY-CD     PIC X.
               16  CRTO-CARRIER        PIC X.
               16  CRTO-GROUPING       PIC X(6).
               16  CRTO-STATE          PIC XX.
               16  CRTO-ACCOUNT        PIC X(10).
               16  CRTO-CERT-EFF-DT    PIC XX.
               16  CRTO-CERT-PRIME     PIC X(10).
               16  CRTO-CERT-SFX       PIC X.
               16  CRTO-RECORD-TYPE    PIC X.
               16  CRTO-SEQ-NO         PIC 9(4) BINARY.
121712     12  WS-ELCRTT-KEY.
121712         16  WS-ELCRTT-PRIMARY   PIC X(33).
121712         16  WS-ELCRTT-REC-TYPE  PIC X(1).
           12  WS-ERMAIL-KEY.
               16  MAIL-COMPANY-CD     PIC X.
               16  MAIL-CARRIER        PIC X.
               16  MAIL-GROUPING       PIC X(6).
               16  MAIL-STATE          PIC XX.
               16  MAIL-ACCOUNT        PIC X(10).
               16  MAIL-CERT-EFF-DT    PIC XX.
               16  MAIL-CERT-PRIME     PIC X(10).
               16  MAIL-CERT-SFX       PIC X.
121713
121713     12  WS-ELENCC-KEY.
121713         16  WS-ENCC-COMPANY-CD  PIC X.
121713         16  WS-ENCC-REC-TYPE    PIC X.
121713         16  WS-ENCC-ENC-CODE    PIC X(5).
121713         16  FILLER              PIC X(09).
121713
           12  WS-LF-CANC-DT           PIC XX   VALUE LOW-VALUES.
           12  WS-AH-CANC-DT           PIC XX   VALUE LOW-VALUES.
           12  WS-CURRENT-DT.
               16  FILLER              PIC X(6)    VALUE SPACES.
               16  WS-CURRENT-YR       PIC XX      VALUE SPACES.
           12  WS-CURRENT-BIN-DT       PIC XX    VALUE LOW-VALUES.
           12  WS-ISS-CAN-SW           PIC X     VALUE LOW-VALUES.
               88  CANCEL-REC            VALUE 'C'.
               88  ISSUE-REC             VALUE 'I'.
           12  WS-WORK-AMT             PIC S9(9)V99 COMP-3 VALUE +0.
           12  WS-HOLD-ACCT-KEY        PIC X(26) VALUE LOW-VALUES.
           12  ELCNTL-KEY.
               16  CNTL-COMP-ID        PIC X(3)  VALUE SPACES.
               16  CNTL-REC-TYPE       PIC X     VALUE SPACES.
               16  CNTL-ACCESS.
                   20  CNTL-STATE      PIC XX    VALUE SPACES.
                   20  FILLER          PIC X     VALUE SPACES.
                   20  CNTL-CARRIER    PIC X     VALUE SPACES.
               16  CNTL-SEQ            PIC S9(4) VALUE +0 COMP.
           12  WS-ERACCT-KEY.
               16  ACCT-COMPANY-CD     PIC X.
               16  ACCT-CARRIER        PIC X.
               16  ACCT-GROUPING       PIC X(6).
               16  ACCT-STATE          PIC XX.
               16  ACCT-ACCOUNT        PIC X(10).
               16  ACCT-EXP-DATE       PIC XX    VALUE SPACES.
               16  FILLER              PIC X(4)  VALUE LOW-VALUES.
           12  ERACCT-SAVE-KEY         PIC X(20) VALUE SPACES.
           12  WS-ELCERT-FILE-ID       PIC X(8) VALUE 'ELCERT'.
           12  WS-ELCNTL-FILE-ID       PIC X(8) VALUE 'ELCNTL'.
           12  WS-ERNOTE-FILE-ID       PIC X(8) VALUE 'ERNOTE'.
           12  WS-ELCRTO-FILE-ID       PIC X(8) VALUE 'ELCRTO'.
           12  WS-ERENDT-FILE-ID       PIC X(8) VALUE 'ERENDT'.
           12  WS-ERPNDB2-FILE-ID      PIC X(8) VALUE 'ERPNDB2'.
           12  WS-ERPNDB-FILE-ID       PIC X(8) VALUE 'ERPNDB'.
           12  WS-ERACCT2-FILE-ID      PIC X(8) VALUE 'ERACCT2'.
072312     12  WS-ERCNOT-FILE-ID       PIC X(8) VALUE 'ERCNOT'.
072312     12  WS-ELEOBC-FILE-ID       PIC X(8) VALUE 'ELEOBC'.
121712     12  WS-ELCRTT-FILE-ID       PIC X(8) VALUE 'ELCRTT'.
121713     12  WS-ELENCC-FILE-ID       PIC X(8) VALUE 'ELENCC'.
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL                  VALUE +00.
               88  RESP-NOTFND                  VALUE +13.
061112         88  RESP-DUPREC                  VALUE +14.
061112         88  RESP-DUPKEY                  VALUE +15.
               88  RESP-NOTOPEN                 VALUE +19.
               88  RESP-ENDFILE                 VALUE +20.
           12  MAP-NAME            PIC X(8)    VALUE 'EL631K'.
           12  MAPSET-NAME         PIC X(8)    VALUE 'EL6318S'.
           12  SCREEN-NUMBER       PIC X(4)    VALUE '631K'.
           12  TRANS-ID            PIC X(4)    VALUE 'EXBE'.
           12  THIS-PGM            PIC X(8)    VALUE 'EL6318'.
           12  PGM-NAME            PIC X(8).
           12  TIME-IN             PIC S9(7).
           12  TIME-OUT-R  REDEFINES TIME-IN.
               16  FILLER          PIC X.
               16  TIME-OUT        PIC 99V99.
               16  FILLER          PIC X(2).
           12  XCTL-005            PIC X(8)    VALUE 'EL005'.
           12  XCTL-010            PIC X(8)    VALUE 'EL010'.
           12  XCTL-626            PIC X(8)    VALUE 'EL626'.
           12  XCTL-6311           PIC X(8)    VALUE 'EL6311'.
091615     12  XCTL-677            PIC X(8)    VALUE 'EL677'.
           12  LINK-001            PIC X(8)    VALUE 'EL001'.
           12  LINK-004            PIC X(8)    VALUE 'EL004'.
           12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.
           12  WS-PHONE.
               16  WS-PH1              PIC XXX.
               16  WS-PH2              PIC XXX.
               16  WS-PH3              PIC XXXX.
           12  WS-PHONE-NUM REDEFINES WS-PHONE PIC 9(10).
           12  WS-DEV-RT               PIC S9V9(6) COMP-3.
072312
072312     12  WS-BILLING-NOTE.
072312         16  WS-BN-NOTE          PIC X(25).
072312         16  WS-BN-LTRID         PIC X(4).
072312         16  FILLER              PIC X(3).
072312         16  WS-BN-DATE          PIC X(8).
072312         16  FILLER              PIC X(3).
072312         16  WS-BN-USERID        PIC X(4).
072312         16  FILLER              PIC X(30).
121212     12  WS-MANUAL-BILL-NOTE REDEFINES WS-BILLING-NOTE.
121212         16  WS-MAN-BN-NOTE      PIC X(63).
121212         16  FILLER              PIC X(14).
121212     12  WS-LEN                  PIC S9(5) COMP-3 VALUE +0.
121012
121012     12  WS-SAVE-REASON-CODE-IND PIC X(26).
061112
061112 01  NEW-ELCRTO-REC.
061112     12  NEW-ELCRTO-KEY.
061112         16  NCRTO-COMPANY-CD      PIC X.
061112         16  NCRTO-CARRIER         PIC X.
061112         16  NCRTO-GROUPING        PIC X(6).
061112         16  NCRTO-STATE           PIC XX.
061112         16  NCRTO-ACCOUNT         PIC X(10).
061112         16  NCRTO-CERT-EFF-DT     PIC XX.
061112         16  NCRTO-CERT-PRIME      PIC X(10).
061112         16  NCRTO-CERT-SFX        PIC X.
061112         16  NCRTO-RECORD-TYPE     PIC X.
061112         16  NCRTO-SEQ-NO          PIC 9(4) BINARY.
062712     12  NEW-ORIG-REC.
062712         16  NCRTO-INS-LAST-NAME   PIC X(15).
062712         16  NCRTO-INS-FIRST-NAME  PIC X(10).
062712         16  NCRTO-INS-MIDDLE-INIT PIC X.
062712         16  NCRTO-INS-AGE         PIC S999     COMP-3.
062712         16  NCRTO-JNT-LAST-NAME   PIC X(15).
062712         16  NCRTO-JNT-FIRST-NAME  PIC X(10).
062712         16  NCRTO-JNT-MIDDLE-INIT PIC X.
062712         16  NCRTO-JNT-AGE         PIC S999     COMP-3.
062712         16  NCRTO-LF-BENCD        PIC XX.
062712         16  NCRTO-LF-TERM         PIC S999      COMP-3.
062712         16  NCRTO-LF-BEN-AMT      PIC S9(9)V99  COMP-3.
062712         16  NCRTO-LF-PRM-AMT      PIC S9(7)V99  COMP-3.
062712         16  NCRTO-LF-ALT-BEN-AMT  PIC S9(9)V99  COMP-3.
062712         16  NCRTO-LF-ALT-PRM-AMT  PIC S9(7)V99  COMP-3.
062712         16  NCRTO-LF-EXP-DT       PIC XX.
062712         16  NCRTO-LF-COMM-PCT     PIC SV9(5)    COMP-3.
062712         16  NCRTO-LF-CANCEL-DT    PIC XX.
062712         16  NCRTO-LF-CANCEL-AMT   PIC S9(7)V99  COMP-3.
071712         16  NCRTO-LF-ITD-CANCEL-AMT PIC S9(7)V99 COMP-3.
062712         16  NCRTO-AH-BENCD        PIC XX.
062712         16  NCRTO-AH-TERM         PIC S999      COMP-3.
062712         16  NCRTO-AH-BEN-AMT      PIC S9(9)V99  COMP-3.
062712         16  NCRTO-AH-PRM-AMT      PIC S9(7)V99  COMP-3.
062712         16  NCRTO-AH-EXP-DT       PIC XX.
062712         16  NCRTO-AH-COMM-PCT     PIC SV9(5)    COMP-3.
062712         16  NCRTO-AH-CP           PIC 99.
062712         16  NCRTO-AH-CANCEL-DT    PIC XX.
062712         16  NCRTO-AH-CANCEL-AMT   PIC S9(7)V99  COMP-3.
071712         16  NCRTO-AH-ITD-CANCEL-AMT PIC S9(7)V99 COMP-3.
062712         16  NCRTO-CRED-BENE-NAME  PIC X(25).
062712         16  NCRTO-1ST-PMT-DT      PIC XX.
121712         16  NCRTO-INS-AGE-DEFAULT-FLAG PIC X.
121712         16  NCRTO-JNT-AGE-DEFAULT-FLAG PIC X.
011413         16  NCRTO-ISSUE-TRAN-IND  PIC X.
011413         16  NCRTO-CANCEL-TRAN-IND PIC X.
011413         16  FILLER                PIC X(235).
103012
103012 01  CERT-NOTE-ENTRIES.
103012     12  CERT-NT OCCURS 300 TIMES INDEXED BY TB-INDX TB-INDX1.
103012         16  CERT-NT-TEXT                PIC X(63).
103012         16  CERT-NT-LAST-MAINT-BY       PIC XXXX.
103012         16  CERT-NT-LAST-MAINT-DT       PIC XX.
103012         16  CERT-NT-LAST-MAINT-HHMMSS   PIC S9(7) COMP-3.
103012
103012 01  ERCNOT-AREA.
103012     12  ERCNOT-KEY-LENGTH       PIC S9(4)   COMP VALUE +36.
103012     12  ERCNOT-START-LENGTH     PIC S9(4)   COMP VALUE +34.
103012     12  ERCNOT-KEY.
103012         16  ERCNOT-PARTIAL-KEY.
103012             20 ERCNOT-COMPANY-CD    PIC X.
103012             20 ERCNOT-CARRIER       PIC X.
103012             20 ERCNOT-GROUPING      PIC X(06).
103012             20 ERCNOT-STATE         PIC XX.
103012             20 ERCNOT-ACCOUNT       PIC X(10).
103012             20 ERCNOT-EFF-DT        PIC XX.
103012             20 ERCNOT-CERT-NO.
103012                25 ERCNOT-CERT-PRIME PIC X(10).
103012                25 ERCNOT-CERT-SFX   PIC X.
103012             20 ERCNOT-REC-TYP       PIC X.
103012         16 ERCNOT-SEQ           PIC S9(4) COMP.
103012     12  SV-PARTIAL-KEY.
103012         20 SV-COMPANY-CD            PIC X.
103012         20 SV-CARRIER               PIC X.
103012         20 SV-GROUPING              PIC X(06).
103012         20 SV-STATE                 PIC XX.
103012         20 SV-ACCOUNT               PIC X(10).
103012         20 SV-EFF-DT                PIC XX.
103012         20 SV-CERT-NO.
103012            25 SV-CERT-PRIME         PIC X(10).
103012            25 SV-CERT-SFX           PIC X(1).
103012         20 SV-REC-TYP               PIC X.
      
       01  ERROR-MESSAGES.
           12  ER-0000                 PIC X(4)  VALUE '0000'.
           12  ER-0004                 PIC X(4)  VALUE '0004'.
           12  ER-0008                 PIC X(4)  VALUE '0008'.
           12  ER-0023                 PIC X(4)  VALUE '0023'.
           12  ER-0029                 PIC X(4)  VALUE '0029'.
           12  ER-0068                 PIC X(4)  VALUE '0068'.
           12  ER-0132                 PIC X(4)  VALUE '0132'.
           12  ER-0142                 PIC X(4)  VALUE '0142'.
           12  ER-0148                 PIC X(4)  VALUE '0148'.
           12  ER-0215                 PIC X(4)  VALUE '0215'.
           12  ER-0280                 PIC X(4)  VALUE '0280'.
           12  ER-0348                 PIC X(4)  VALUE '0348'.
           12  ER-0419                 PIC X(4)  VALUE '0419'.
           12  ER-0715                 PIC X(4)  VALUE '0715'.
112612     12  ER-0894                 PIC X(4)  VALUE '0894'.
           12  ER-1101                 PIC X(4)  VALUE '1101'.
           12  ER-1236                 PIC X(4)  VALUE '1236'.
121713     12  ER-1560                 PIC X(4)  VALUE '1560'.
091913     12  ER-1664                 PIC X(4)  VALUE '1664'.
           12  ER-1778                 PIC X(4)  VALUE '1778'.
           12  ER-1818                 PIC X(4)  VALUE '1818'.
           12  ER-1820                 PIC X(4)  VALUE '1820'.
           12  ER-1821                 PIC X(4)  VALUE '1821'.
           12  ER-2056                 PIC X(4)  VALUE '2056'.
           12  ER-2079                 PIC X(4)  VALUE '2079'.
           12  ER-2208                 PIC X(4)  VALUE '2208'.
           12  ER-2209                 PIC X(4)  VALUE '2209'.
           12  ER-2237                 PIC X(4)  VALUE '2237'.
           12  ER-2238                 PIC X(4)  VALUE '2238'.
           12  ER-2253                 PIC X(4)  VALUE '2253'.
           12  ER-2619                 PIC X(4)  VALUE '2619'.
           12  ER-2651                 PIC X(4)  VALUE '2651'.
           12  ER-2784                 PIC X(4)  VALUE '2784'.
           12  ER-2785                 PIC X(4)  VALUE '2785'.
           12  ER-2786                 PIC X(4)  VALUE '2786'.
           12  ER-2791                 PIC X(4)  VALUE '2791'.
           12  ER-2845                 PIC X(4)  VALUE '2845'.
           12  ER-2848                 PIC X(4)  VALUE '2848'.
           12  ER-2851                 PIC X(4)  VALUE '2851'.
061112     12  ER-3830                 PIC X(4)  VALUE '3830'.
121112     12  ER-3833                 PIC X(4)  VALUE '3833'.
091615     12  ER-3844                 PIC X(4)  VALUE '3844'.
           12  ER-3845                 PIC X(4)  VALUE '3845'.
           12  ER-7345                 PIC X(4)  VALUE '7345'.
           12  ER-7638                 PIC X(4)  VALUE '7638'.
           12  ER-8888                 PIC X(4)  VALUE '8888'.
           12  ER-9999                 PIC X(4)  VALUE '9999'.
      
           COPY ELCDATE.
      
           COPY ELCTEXT.
           COPY ELCLOGOF.
      
           COPY ELCATTR.
      
           COPY ELCEMIB.
      
           COPY ELCINTF.
           12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.
              16  PI-ISS-CAN-SW           PIC X.
              16  PI-ERENDT-KEY.  *>  36
                  20  PI-ENDT-COMPANY-CD  PIC X.
                  20  PI-ENDT-CARRIER     PIC X.
                  20  PI-ENDT-GROUPING    PIC X(6).
                  20  PI-ENDT-STATE       PIC XX.
                  20  PI-ENDT-ACCOUNT     PIC X(10).
                  20  PI-ENDT-CERT-EFF-DT PIC XX.
                  20  PI-ENDT-CERT-PRIME  PIC X(10).
                  20  PI-ENDT-CERT-SFX    PIC X.
                  20  PI-ENDT-RECORD-TYPE PIC X.
                  20  PI-ENDT-SEQ-NO      PIC 9(4) BINARY.
              16  PI-ELCRTO-KEY.  *> 36
                  20  PI-CRTO-COMPANY-CD  PIC X.
                  20  PI-CRTO-CARRIER     PIC X.
                  20  PI-CRTO-GROUPING    PIC X(6).
                  20  PI-CRTO-STATE       PIC XX.
                  20  PI-CRTO-ACCOUNT     PIC X(10).
                  20  PI-CRTO-CERT-EFF-DT PIC XX.
                  20  PI-CRTO-CERT-PRIME  PIC X(10).
                  20  PI-CRTO-CERT-SFX    PIC X.
                  20  PI-CRTO-RECORD-TYPE PIC X.
                  20  PI-CRTO-SEQ-NO      PIC 9(4) BINARY.
              16  PI-ERPNDB-KEY           PIC X(11).
              16  PI-DOCUMENT-PROCESSED   PIC X.
                  88  PI-VERI-PROCESSED      VALUE 'V'.
                  88  PI-ENDO-PROCESSED      VALUE 'G'.
              16  PI-PREV-ERENDT-KEY      PIC X(36).
062712        16  PI-RES-REF-CLM-TYPE     PIC X.
012412        16  PI-CLM-REFORM-IND       PIC X.
012412            88  PI-CLAIM-REFORMATION   VALUE 'Y'.
062712        16  PI-CLEAR-ERROR-SW       PIC X.
062712            88  PI-CLEAR-ERROR         VALUE 'Y'.
062712        16  PI-PRINT-CERTIFICATE    PIC X.
062712        16  PI-ENTERED.  *> 233
062712            20  PI-PRTSW-ENTERED    PIC X.
062712            20  PI-SIGREQ-ENTERED   PIC X.
062712            20  PI-LTRID-ENTERED    PIC X(4).
062712            20  PI-VOUCHER-ENTERED  PIC X.
062712            20  PI-HLTHAPP-ENTERED  PIC X.
062712            20  PI-PAYEE-ENTERED    PIC X(10).
062712            20  PI-CERTID-ENTERED   PIC X(5).
062712            20  PI-REASONS-ENTERED  PIC X(84).
072312            20  PI-CERTNT1-ENTERED  PIC X(63).
072312            20  PI-CERTNT2-ENTERED  PIC X(63).
072312        16  PI-FINALIZED-IND        PIC X.
072312        16  PI-CERTNT3-ENTERED      PIC X(63).
072312        16  PI-CERTNT4-ENTERED      PIC X(63).
121212        16  PI-BILLNT1-ENTERED      PIC X(63).
121212        16  PI-BILLNT2-ENTERED      PIC X(63).
121012        16  PI-REASON-CODE-IND      PIC X(26).
121713        16  PI-ENCCODE              PIC X(3).
091615    12  pi-el677-work-area redefines pi-program-work-area.
091615        16  filler                  pic x(318).
091615        16  pi-el677-prm-diff       pic s9(7)v99 comp-3.
091615        16  pi-el677-com-diff       pic s9(5)v99 comp-3.
091615        16  pi-el677-check-type     pic x.
091615        16  pi-el677-check-cut      pic x.
091615        16  filler                  pic x(311).
091615    12  pi-endt-prm-diff            pic s9(7)v99 comp-3.
091615    12  pi-endt-com-diff            pic s9(5)v99 comp-3.
091615    12  pi-check-cut                pic x.
091615    12  pi-check-type               pic x.
091615    12  FILLER                      PIC X(265). *> was 276
      
           COPY ELCAID.
      
       01  FILLER    REDEFINES DFHAID.
           12  FILLER              PIC X(8).
           12  PF-VALUES           PIC X       OCCURS 24 TIMES.
072312     12  FILLER              PIC X(3).
      
          COPY EL6318S.
      
       01  FILLER REDEFINES EL631KI.
121713     12  FILLER             PIC X(217).
062712     12  REASONCDS.
062712         16  FILLER OCCURS 12.
062712           20  REACDL       PIC S9(4) COMP.
062712           20  REACDA       PIC X.
062712           20  REACD-IN     PIC X(4).
121713     12  FILLER             PIC X(557).
      
       LINKAGE SECTION.
       01  DFHCOMMAREA                 PIC X(1300).

       01  var  pic x(30).
                                       COPY ERCACCT.
                                       COPY ELCCNTL.
                                       COPY ERCPNDB.
                                       COPY ELCCERT.
                                       COPY ELCCRTO.
                                       COPY ERCENDT.
                                       COPY ERCNOTE.
                                       COPY ERCMAIL.
                                       COPY ERCARCH.
072312                                 COPY ERCCNOT.
072312                                 COPY ELCEOBC.
121712                                 COPY ELCCRTT.
121713                                 COPY ELCENCC.
      
       PROCEDURE DIVISION.
           MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK

           MOVE EIBDATE                TO DC-JULIAN-YYDDD
           MOVE '5'                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK
           MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DT
           MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT
           MOVE EIBTRMID               TO QID-TERM
           MOVE 2                      TO EMI-NUMBER-OF-LINES
      
           IF EIBCALEN = 0
              GO TO 8800-UNAUTHORIZED-ACCESS
           END-IF
      
           MOVE FUNCTION LENGTH(EL631KI) TO MAP-LENGTH
      
           IF PI-CALLING-PROGRAM NOT = THIS-PGM
              IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
                 MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
                 MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
                 MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
                 MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
                 MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
                 MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
                 MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
                 MOVE THIS-PGM             TO PI-CALLING-PROGRAM
                 MOVE LOW-VALUES           TO EL631KI
062712           MOVE 'Y'                  TO PI-CLEAR-ERROR-SW
                 GO TO 0350-BUILD-INIT-MAP
              END-IF
           END-IF
      
           IF PI-CALLING-PROGRAM = 'EL614'
              MOVE PI-PROGRAM-WORK-AREA (1:60)
                                       TO WS-PASSED-REASON-CODES
              IF WS-PASSED-REASON-CODES NOT = SPACES
                 MOVE 'Y'                TO WS-SET-CODES-MDT
              ELSE
                 MOVE 'N'                TO WS-SET-CODES-MDT
              END-IF
              PERFORM 0600-RECOVER-TEMP-STORAGE
                                       THRU 0600-EXIT 
062712        GO TO 0350-BUILD-INIT-MAP
           END-IF

091615     IF PI-CALLING-PROGRAM = 'EL677'
091615        move pi-el677-check-cut  to ws-el677-check-cut
091615        move pi-el677-check-type to ws-el677-check-type
091615        PERFORM 0600-RECOVER-TEMP-STORAGE
091615                                 THRU 0600-EXIT 
091615        move ws-el677-check-cut  to pi-check-cut
091615        move ws-el677-check-type to pi-check-type
091615        GO TO 0350-BUILD-INIT-MAP
091615     END-IF

091615     if (eibaid = dfhclear or dfhpf23 or dfhpf24)
091615        and (pi-check-cut = 'Y')
091615        MOVE ER-3844             TO EMI-ERROR
091615        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
091615        GO TO 0350-BUILD-INIT-MAP
091615     end-if

           IF EIBAID = DFHCLEAR
062712        IF PI-CLEAR-ERROR
062712           MOVE SPACES            TO PI-CLEAR-ERROR-SW
121112           MOVE ER-3833           TO EMI-ERROR
062712           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
062712           GO TO 0350-BUILD-INIT-MAP
062712        ELSE
062712           GO TO 9400-CLEAR
062712        END-IF
           END-IF
      
           .
       0200-RECEIVE.

           IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
              MOVE ER-0008             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              MOVE -1                  TO PFENTRL
              GO TO 8100-SEND-INITIAL-MAP
           END-IF
      
           EXEC CICS RECEIVE
               MAP      (MAP-NAME)
               MAPSET   (MAPSET-NAME)
               INTO     (EL631KI)
           END-EXEC
      
           IF PFENTRL = 0
               GO TO 0300-CHECK-PFKEYS
           END-IF
           IF EIBAID NOT = DFHENTER
               MOVE ER-0004            TO EMI-ERROR
               GO TO 0320-INPUT-ERROR
           END-IF
      
           IF (PFENTRI NUMERIC) AND (PFENTRI > 0 AND < 25)
              MOVE PF-VALUES (PFENTRI) TO EIBAID
           ELSE
              MOVE ER-0029            TO EMI-ERROR
              GO TO 0320-INPUT-ERROR
           END-IF
      
           .
       0300-CHECK-PFKEYS.

121112     IF EIBAID = DFHPF3
062712        MOVE 'Y'                 TO PI-CLEAR-ERROR-SW
091615        PERFORM 0325-EDIT-SCREEN THRU 0325-EXIT
              PERFORM 0500-CREATE-TEMP-STORAGE THRU 0500-EXIT   
              MOVE 'EL614'             TO PGM-NAME
121012        MOVE PI-REASON-CODE-IND  TO WS-SAVE-REASON-CODE-IND
              MOVE SPACES              TO PI-PROGRAM-WORK-AREA
              move +1                  to s2
              perform varying s1 from +1 by +1 until s1 > +12
                 if reacd-in (s1) not = spaces and low-values
                    move reacd-in (s1)    to pi-program-work-area (s2:4)
                    add +5 to s2
                 end-if
              END-perform
121012        MOVE WS-SAVE-REASON-CODE-IND TO PI-REASON-CODE-IND
              GO TO 9300-XCTL
           END-IF

091615     IF (EIBAID = DFHPF4 OR DFHPF8)
091615        and (vouchi = 'Y')
091615        and (pi-check-cut not = 'Y')
091615        MOVE ER-3845             TO EMI-ERROR
091615        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
091615        GO TO 0350-BUILD-INIT-MAP
091615     end-if



121112     IF EIBAID = DFHPF4 OR DFHPF8
              GO TO 0360-FINALIZE
           END-IF

091615     IF (EIBAID = DFHPF5)
091615        and (pi-endt-prm-diff > zeros)
091615        PERFORM 0500-CREATE-TEMP-STORAGE
091615                                 THRU 0500-EXIT
091615        MOVE 'EL677'             TO PGM-NAME
091615        move pi-endt-prm-diff    to pi-el677-prm-diff
091615        move pi-endt-com-diff    to pi-el677-com-diff
091615        move pi-check-cut        to pi-el677-check-cut
091615        GO TO 9300-XCTL
091615     END-IF

           IF EIBAID = DFHPF23
062712        IF PI-CLEAR-ERROR
062712           MOVE SPACES            TO PI-CLEAR-ERROR-SW
121112           MOVE ER-3833           TO EMI-ERROR
062712           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
062712           GO TO 0350-BUILD-INIT-MAP
062712        ELSE
062712           GO TO 8810-PF23
062712        END-IF
           END-IF
      
           IF EIBAID = DFHPF24
062712        IF PI-CLEAR-ERROR
062712           MOVE SPACES            TO PI-CLEAR-ERROR-SW
121112           MOVE ER-3833           TO EMI-ERROR
062712           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
062712           GO TO 0350-BUILD-INIT-MAP
062712        ELSE
062712           GO TO 9200-RETURN-MAIN-MENU
062712        END-IF
           END-IF
      
           IF EIBAID = DFHPF12
              GO TO 9500-PF12
           END-IF
      
           IF EIBAID = DFHENTER
062712        MOVE 'Y'                 TO PI-CLEAR-ERROR-SW
121713        PERFORM 0325-EDIT-SCREEN THRU 0325-EXIT
121713        GO TO 0330-CHECK-ERRORS              
           END-IF
      
           MOVE ER-0029                TO EMI-ERROR
      
           .
       0320-INPUT-ERROR.
      
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
           MOVE AL-UNBON               TO PFENTRA
           MOVE -1                     TO PFENTRL
           GO TO 8200-SEND-DATAONLY
      
           .
       0325-EDIT-SCREEN.

           IF PRTSWL > ZEROS
072312       IF PRTSWI = 'Y' OR 'N' OR 'P' OR ' '
072312*          MOVE AL-UANON       TO PRTSWA
072312*          IF PRTSWI = ' '
072312*             MOVE 'N'         TO PRTSWI
072312              MOVE 'P'         TO PRTSWI
072312*          END-IF
062712*          MOVE PRTSWI         TO PI-PRTSW-ENTERED
              END-IF
           END-IF

           IF LTRIDL > ZEROS
              AND LTRIDI > SPACES
              MOVE PI-COMPANY-CD       TO LETR-COMPANY-CD
              MOVE LTRIDI              TO LETR-LETTER-ID
062712                                    PI-LTRID-ENTERED  
112612        MOVE LETR-PART-KEY       TO ELLETR-SAVE-PART-KEY
              MOVE SPACES              TO LETR-FILLER
112612        MOVE 0                   TO LETR-SEQ-NO
112612        PERFORM 0326-GET-Z-RECORD THRU 0326-EXIT
           ELSE
              MOVE ER-1236          TO EMI-ERROR
              MOVE -1               TO LTRIDL
              MOVE AL-UABON         TO LTRIDA
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF
091913
091913     IF W-SIG-FLAG-DEFAULT > ' '
091913        IF (SIGREQL > ZERO AND SIGREQI <> W-SIG-FLAG-DEFAULT)
091913             MOVE W-SIG-FLAG-DEFAULT TO SIGREQI
091913             MOVE +1 TO SIGREQL
091913             MOVE ER-1664     TO EMI-ERROR
091913             MOVE AL-UABON    TO SIGREQA
091913             PERFORM 9900-ERROR-FORMAT
091913                                 THRU 9900-EXIT
091913        ELSE
091913             MOVE W-SIG-FLAG-DEFAULT TO SIGREQI
091913             MOVE +1 TO SIGREQL
091913             MOVE AL-UANON    TO SIGREQA
091913        END-IF
091913     END-IF
091913
091913     IF (SIGREQL > ZEROS)
091913        AND (SIGREQI = 'Y' OR 'N' OR ' ')
091913        MOVE AL-UANON         TO SIGREQA
091913        IF SIGREQI = ' '
091913           MOVE 'N'           TO SIGREQI
091913        END-IF
091913        MOVE SIGREQI          TO PI-SIGREQ-ENTERED
091913     ELSE
091913        MOVE ER-2651          TO EMI-ERROR
091913        MOVE -1               TO SIGREQL
091913        MOVE AL-UABON         TO SIGREQA
091913        PERFORM 9900-ERROR-FORMAT
091913                                 THRU 9900-EXIT
091913     END-IF
112612
112612     IF W-PROMPT-LETTER EQUAL 'Y'
112612         MOVE ER-0894            TO EMI-ERROR
112612         MOVE -1                 TO LTRIDL
112612         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
112612     END-IF

           IF VOUCHL > ZEROS
              IF VOUCHI = 'Y' OR 'N'
                 MOVE AL-UANON         TO VOUCHA
062712           MOVE VOUCHI           TO PI-VOUCHER-ENTERED
              ELSE
                 MOVE ER-9999          TO EMI-ERROR
                 MOVE -1               TO VOUCHL
                 MOVE AL-UABON         TO VOUCHA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

           IF HLTHAPPL > ZEROS
              IF HLTHAPPI = 'Y' OR 'N'
                 MOVE AL-UANON         TO HLTHAPPA
062712           MOVE HLTHAPPI         TO PI-HLTHAPP-ENTERED
              ELSE
                 MOVE ER-9999          TO EMI-ERROR
                 MOVE -1               TO HLTHAPPL
                 MOVE AL-UABON         TO HLTHAPPA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF

           IF PAYEEL > ZEROS
              IF PAYEEI > SPACES
                 MOVE AL-UANON         TO PAYEEA
062712           MOVE PAYEEI           TO PI-PAYEE-ENTERED  
              ELSE
                 MOVE ER-9999          TO EMI-ERROR
                 MOVE -1               TO PAYEEL
                 MOVE AL-UABON         TO PAYEEA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF
           
062712     IF CERTIDL > ZEROS
062712         IF CERTIDI NUMERIC
062712            MOVE AL-UANON      TO CERTIDA
062712            MOVE CERTIDI       TO PI-CERTID-ENTERED   
062712         ELSE
062712            MOVE ER-1778       TO EMI-ERROR
062712            MOVE -1            TO CERTIDL
062712            MOVE AL-UABON      TO CERTIDA
062712            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
062712        END-IF
062712     ELSE
062712        IF PI-PRINT-CERTIFICATE = 'Y'
062712            MOVE ER-0715       TO EMI-ERROR
062712            MOVE -1            TO CERTIDL
062712            MOVE AL-UABON      TO CERTIDA
062712            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              
              END-IF
           END-IF
121713
121713     IF ENCCODEL > ZEROS
121713        MOVE FUNCTION UPPER-CASE(ENCCODEI) TO ENCCODEI
121713                                              PI-ENCCODE
121713        MOVE SPACES             TO WS-ELENCC-KEY
121713        MOVE PI-COMPANY-CD      TO WS-ENCC-COMPANY-CD
121713        MOVE '2'                TO WS-ENCC-REC-TYPE
121713        MOVE ENCCODEI           TO WS-ENCC-ENC-CODE
121713
121713        EXEC CICS READ
121713            DATASET    (WS-ELENCC-FILE-ID)
121713            SET        (ADDRESS OF ENCLOSURE-CODES)
121713            RIDFLD     (WS-ELENCC-KEY)
121713            RESP       (WS-RESPONSE)
121713        END-EXEC
121713
121713        IF RESP-NORMAL
121713           MOVE ENCCODEI         TO PI-ENCCODE
121713                                    W-ENCLOSURE-CD
121713           MOVE AL-UANON         TO ENCCODEA
121713        ELSE
121713           MOVE ER-1560          TO EMI-ERROR
121713           MOVE -1               TO ENCCODEL
121713           MOVE AL-UABON         TO ENCCODEA
121713           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
121713        END-IF
121713     END-IF
121713
060612     SET REASONS-NOT-FOUND TO TRUE
060612     PERFORM VARYING S1 FROM +1 BY +1 UNTIL S1 > +12
060612        IF REACDL (S1) = +4 OR
060612          (REACD-IN (S1) NOT = SPACES AND LOW-VALUES)
060612              MOVE AL-SANON     TO REACDA (S1)
060612              SET REASONS-FOUND TO TRUE
060612        END-IF
060612     END-PERFORM
060612     
060612     IF REASONS-NOT-FOUND
060612        MOVE ER-1820             TO EMI-ERROR
060612        MOVE -1                  TO PFENTRL
060612        PERFORM 9900-ERROR-FORMAT
060612                                 THRU 9900-EXIT
062712     ELSE
062712        MOVE REASONCDS           TO PI-REASONS-ENTERED
060612     END-IF
060612     
072312     IF CRTNT1L > ZEROS
072312        IF CRTNT1I > SPACES
072312           MOVE CRTNT1I          TO PI-CERTNT1-ENTERED
072312        END-IF
072312     END-IF
072312
072312     IF CRTNT2L > ZEROS
072312        IF CRTNT2I > SPACES
072312           MOVE CRTNT2I          TO PI-CERTNT2-ENTERED
072312        END-IF
072312     END-IF
072312
072312     IF CRTNT3L > ZEROS
072312        IF CRTNT3I > SPACES
072312           MOVE CRTNT3I          TO PI-CERTNT3-ENTERED
072312        END-IF
072312     END-IF
072312
072312     IF CRTNT4L > ZEROS
072312        IF CRTNT4I > SPACES
072312           MOVE CRTNT4I          TO PI-CERTNT4-ENTERED
072312        END-IF
072312     END-IF
072312
121212     IF BILNT1L > ZEROS
121212        IF BILNT1I > SPACES
121212           MOVE BILNT1I          TO PI-BILLNT1-ENTERED
072312        END-IF
072312     END-IF
072312
121212     IF BILNT2L > ZEROS
121212        IF BILNT2I > SPACES
121212           MOVE BILNT2I          TO PI-BILLNT2-ENTERED
072312        END-IF
072312     END-IF
072312
           .
       0325-EXIT.
           EXIT.
      
112612
112612 0326-GET-Z-RECORD.
112612
112612     MOVE SPACES  TO W-Z-CONTROL-DATA
112612     
112612     EXEC CICS STARTBR
112612          DATASET    ('ELLETR')
112612          RIDFLD     (ELLETR-KEY)
112612          GTEQ
112612          RESP      (WS-RESPONSE)
112612     END-EXEC.
112612     IF NOT RESP-NORMAL
112612         MOVE ER-1236          TO EMI-ERROR
112612         MOVE -1               TO LTRIDL
112612         MOVE AL-UABON         TO LTRIDA
112612         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
112612         GO TO 0326-ENDBR
112612     END-IF.
112612
112612 0326-READNEXT.
112612
112612     EXEC CICS READNEXT
112612         DATASET   ('ELLETR')
112612         INTO      (TEXT-FILES)
112612         RIDFLD    (ELLETR-KEY)
112612         RESP      (WS-RESPONSE)
112612     END-EXEC
112612     IF RESP-NORMAL
112612        IF TX-CONTROL-PRIMARY(1:5) NOT = ELLETR-SAVE-PART-KEY
112612           MOVE ER-1236          TO EMI-ERROR
112612           MOVE -1               TO LTRIDL
112612           MOVE AL-UABON         TO LTRIDA
112612           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
112612           GO TO 0326-ENDBR
112612        END-IF
112612
112612        MOVE AL-UANON         TO LTRIDA
112612        IF TX-LINE-SQUEEZE-CONTROL = 'Z'
112612           MOVE TX-TEXT-LINE  TO W-Z-CONTROL-DATA
112612        ELSE
112612           GO TO 0326-READNEXT
112612        END-IF
112612     ELSE
112612        MOVE ER-1236          TO EMI-ERROR
112612        MOVE -1               TO LTRIDL
112612        MOVE AL-UABON         TO LTRIDA
112612        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
112612        GO TO 0326-ENDBR
112612     END-IF.
112612
122712     MOVE W-PRINT-CERTIFICATE TO PI-PRINT-CERTIFICATE.
121713     IF PI-ENCCODE NOT GREATER THAN SPACES
121713         MOVE W-ENCLOSURE-CD            TO PI-ENCCODE
121713                                           ENCCODEO
121713         MOVE AL-UANON                  TO ENCCODEA
121713         MOVE +3                        TO ENCCODEL
121713     END-IF.
122712
112612 0326-ENDBR.
112612
112612     EXEC CICS ENDBR
112612         DATASET     ('ELLETR')
112612     END-EXEC.
112612
112612 0326-EXIT.
112612      EXIT.

       0330-CHECK-ERRORS.

           IF EMI-NO-ERRORS
062712        GO TO 0350-BUILD-INIT-MAP
           ELSE
              GO TO 8200-SEND-DATAONLY
           END-IF

           .
       0330-EXIT.
           EXIT.

       0350-BUILD-INIT-MAP.
120313     IF PI-RETURN-TO-PROGRAM = 'EL6314'
120313        MOVE PI-COMPANY-CD          TO CRTO-COMPANY-CD
120313        MOVE PI-CARRIER             TO CRTO-CARRIER
120313        MOVE PI-GROUPING            TO CRTO-GROUPING
120313        MOVE PI-STATE               TO CRTO-STATE
120313        MOVE PI-ACCOUNT             TO CRTO-ACCOUNT
120313        MOVE PI-CERT-PRIME          TO CRTO-CERT-PRIME
120313        MOVE PI-CERT-SFX            TO CRTO-CERT-SFX
120313        MOVE PI-CERT-EFF-DT         TO CRTO-CERT-EFF-DT
120313        MOVE 'I'                    TO CRTO-RECORD-TYPE
120313        MOVE ZEROS                  TO CRTO-SEQ-NO
120313        MOVE ELCRTO-KEY             TO PI-ELCRTO-KEY
120313                                       PI-ERENDT-KEY
120313     END-IF

           MOVE PI-CRTO-CARRIER        TO CARRO
           MOVE PI-CRTO-GROUPING       TO GROUPO
           MOVE PI-CRTO-STATE          TO STATEO
           MOVE PI-CRTO-ACCOUNT        TO ACCTO
           MOVE PI-CRTO-CERT-PRIME     TO CERTNOO
           MOVE PI-CRTO-CERT-SFX       TO CRTSFXO
           MOVE PI-CRTO-SEQ-NO         TO SEQNOO
      
           MOVE PI-CRTO-CERT-EFF-DT    TO DC-BIN-DATE-1
           MOVE ' '                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK
           IF NO-CONVERSION-ERROR
              MOVE DC-GREG-DATE-1-EDIT TO EFFDTO
           END-IF
           
072312*     IF PI-PRTSW-ENTERED > SPACES
072312*        MOVE PI-PRTSW-ENTERED    TO PRTSWI
072312*        MOVE AL-UANON            TO PRTSWA
072312*        MOVE +1                  TO PRTSWL
072312*     END-IF
062712
062712     IF PI-SIGREQ-ENTERED > SPACES
062712        MOVE PI-SIGREQ-ENTERED   TO SIGREQI
062712        MOVE AL-UANON            TO SIGREQA
062712        MOVE +1                  TO SIGREQL
062712     ELSE
062412        IF PI-CLAIM-REFORMATION
062412            MOVE 'Y'             TO SIGREQI
062712            MOVE AL-UANON        TO SIGREQA
062412            MOVE +1              TO SIGREQL
062712        END-IF
062712     END-IF
062712
062712     IF PI-LTRID-ENTERED > SPACES
062712        MOVE PI-LTRID-ENTERED    TO LTRIDI
062712        MOVE AL-UANON            TO LTRIDA
062712        MOVE +4                  TO LTRIDL
062712     ELSE
062412        IF PI-CLAIM-REFORMATION
062412            MOVE 'CLRF'          TO LTRIDI
062712            MOVE AL-UANON        TO LTRIDA
062412            MOVE +4              TO LTRIDL
062712        END-IF
062712     END-IF
062712
091615     if pi-endt-prm-diff > zeros
091615        move 'Y'                 to vouchi
091615        MOVE AL-UANON            TO VOUCHA
091615        MOVE +1                  TO VOUCHL
091615     END-IF

062712     IF PI-VOUCHER-ENTERED > SPACES
062712        MOVE PI-VOUCHER-ENTERED  TO VOUCHI
062712        MOVE AL-UANON            TO VOUCHA
062712        MOVE +1                  TO VOUCHL
062712     END-IF

062712     IF PI-HLTHAPP-ENTERED > SPACES
062712        MOVE PI-HLTHAPP-ENTERED  TO HLTHAPPI
062712        MOVE AL-UANON            TO HLTHAPPA
062712        MOVE +1                  TO HLTHAPPL
062712     END-IF
062712
062712     IF PI-PAYEE-ENTERED > SPACES
062712        MOVE PI-PAYEE-ENTERED    TO PAYEEI
062712        MOVE AL-UANON            TO PAYEEA
062712        MOVE +10                 TO PAYEEL
062712     END-IF
062712     
062712     IF PI-CERTID-ENTERED > SPACES
062712        MOVE PI-CERTID-ENTERED   TO CERTIDI
062712        MOVE AL-UANON            TO CERTIDA
062712        MOVE +5                  TO CERTIDL
062712     END-IF
121713
121713     IF PI-ENCCODE > SPACES
121713        MOVE PI-ENCCODE          TO ENCCODEI
121713        MOVE AL-UANON            TO ENCCODEA
121713        MOVE +3                  TO ENCCODEL
121713     END-IF
062712
062712     SET REASONS-NOT-FOUND TO TRUE
062712     MOVE PI-REASONS-ENTERED     TO REASONCDS
062712     PERFORM VARYING S1 FROM +1 BY +1 UNTIL S1 > +12
062712        IF REACDL (S1) = +4 OR
062712          (REACD-IN (S1) NOT = SPACES AND LOW-VALUES)
062712              MOVE AL-SANON      TO REACDA (S1)
062712              MOVE +4            TO REACDL (S1)
062712              SET REASONS-FOUND  TO TRUE
062712        END-IF
062712     END-PERFORM
062712
072312     IF REASONS-NOT-FOUND
072312         IF PI-CLAIM-REFORMATION
072312            MOVE 'CRNA'          TO REACD-IN (1)
072312            MOVE AL-SANON        TO REACDA (1)
072312            MOVE +4              TO REACDL (1)
072312            SET REASONS-FOUND    TO TRUE
072312        END-IF
072312     END-IF
072312
072312     IF PI-CERTNT1-ENTERED > SPACES
072312        MOVE PI-CERTNT1-ENTERED  TO CRTNT1I
072312        MOVE AL-UANON            TO CRTNT1A
072312        MOVE +63                 TO CRTNT1L
072312     END-IF
072312
072312     IF PI-CERTNT2-ENTERED > SPACES
072312        MOVE PI-CERTNT2-ENTERED  TO CRTNT2I
072312        MOVE AL-UANON            TO CRTNT2A
072312        MOVE +63                 TO CRTNT2L
072312     END-IF
072312
072312     IF PI-CERTNT3-ENTERED > SPACES
072312        MOVE PI-CERTNT3-ENTERED  TO CRTNT3I
072312        MOVE AL-UANON            TO CRTNT3A
072312        MOVE +63                 TO CRTNT3L
072312     END-IF
072312
072312     IF PI-CERTNT4-ENTERED > SPACES
072312        MOVE PI-CERTNT4-ENTERED  TO CRTNT4I
072312        MOVE AL-UANON            TO CRTNT4A
072312        MOVE +63                 TO CRTNT4L
072312     END-IF
072312
121212     IF PI-BILLNT1-ENTERED > SPACES
121212        MOVE PI-BILLNT1-ENTERED  TO BILNT1I
121212        MOVE AL-UANON            TO BILNT1A
121212        MOVE +63                 TO BILNT1L
121212     END-IF
121212
121212     IF PI-BILLNT2-ENTERED > SPACES
121212        MOVE PI-BILLNT2-ENTERED  TO BILNT2I
121212        MOVE AL-UANON            TO BILNT2A
121212        MOVE +63                 TO BILNT2L
121212     END-IF
072312
           GO TO 8100-SEND-INITIAL-MAP       
      
           .
       0360-FINALIZE.
      
      
           set P to address of KIXSYS
           CALL "getenv" using by value P returning var-ptr
           if var-ptr = null then
              display ' kixsys not set '
           else
              set address of var to var-ptr
              move 0 to env-var-len
              inspect var tallying env-var-len
                for characters before X'00' 
      *       DISPLAY '  KIXSYS = ' var (1:env-var-len)
              unstring var (1:env-var-len) delimited by '/'
                 into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
                    WS-KIX-SYS
              end-unstring
      *       DISPLAY ' WS KIX SYS ' WS-KIXSYS
      *       DISPLAY ' WS KIX MYENV ' WS-KIX-MYENV
           end-if

           set P to address of KIXHOST
           CALL "getenv" using by value P returning var-ptr
           if var-ptr = null then
              display ' kixhost not set '
           else
              set address of var to var-ptr
              move 0 to env-var-len
              inspect var tallying env-var-len
                for characters before X'00' 
      *       DISPLAY '  KIXHOST = ' var (1:env-var-len)
              MOVE var(1:env-var-len)  to ws-kixhost
              DISPLAY ' WS KIX HOST ' WS-KIXhost
           end-if

           PERFORM 0325-EDIT-SCREEN    THRU 0325-EXIT

           IF NOT EMI-NO-ERRORS
121112       AND EIBAID = DFHPF4
              GO TO 8200-SEND-DATAONLY
           END-IF

061112     MOVE LOW-VALUES             TO NEW-ELCRTO-REC
           MOVE PI-ELCRTO-KEY          TO ELCRTO-KEY
061112                                    NEW-ELCRTO-KEY
061112     SUBTRACT +1               FROM NCRTO-SEQ-NO           
      
           PERFORM 0362-READ-ELCRTO    THRU 0362-EXIT

           IF RESP-NORMAL
120313       AND (PI-ELCRTO-KEY (1:33) = OC-CONTROL-PRIMARY (1:33))
120313        MOVE OC-KEY-SEQ-NO       TO CRTO-SEQ-NO
120313                                    PI-CRTO-SEQ-NO
120313        MOVE LOW-VALUES          TO NEW-ELCRTO-REC
120313        MOVE PI-ELCRTO-KEY       TO NEW-ELCRTO-KEY
120313        SUBTRACT +1              FROM NCRTO-SEQ-NO
              PERFORM 0400-ADD-ERENDT  THRU 0400-EXIT
      *       PERFORM 1000-BUILD-ERARCH THRU 1000-EXIT
              PERFORM 1100-CALL-NS-BUS-LOGIC
                                       THRU 1100-EXIT
              IF BL-OK
                 CONTINUE
              ELSE
                 MOVE ER-8888          TO EMI-ERROR-NUMBER (1)
                 MOVE BL-MESSAGE       TO EMI-ERROR-TEXT (1)
060612           MOVE -1               TO SIGREQL
              END-IF

020218        IF EIBAID = DFHPF4
020218           move ' '              to ws-elcrtt-update-sw
020218           move pi-elcrto-key(1:33)
020218                                 to ws-elcrtt-primary
020218           move 'C'              to ws-elcrtt-rec-type
020218           EXEC CICS READ UPDATE
020218              DATASET  (WS-ELCRTT-FILE-ID)
020218              RIDFLD   (WS-ELCRTT-KEY)
020218              SET      (ADDRESS OF CERTIFICATE-TRAILERS)
020218              RESP     (WS-RESPONSE)
020218           END-EXEC
020218           if resp-normal
020218              if cs-claim-verification-status = 'B' or 'C'
020218                 or 'D' or 'E' or 'F' or 'G'
020218                 move 'H'        to cs-claim-verification-status
020218                 set update-elcrtt to true
020218              end-if
020218              if update-elcrtt
020218                 EXEC CICS REWRITE
020218                    DATASET    (ws-elcrtt-FILE-ID)
020218                    FROM       (CERTIFICATE-TRAILERS)
020218                 END-EXEC
020218              else
020218                 exec cics unlock
020218                    dataset    (ws-elcrtt-FILE-ID)
020218                 end-exec
020218              end-if
020218           end-if
020218        end-if
           ELSE
              MOVE ER-9999             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
060612        MOVE -1                  TO SIGREQL
              GO TO 8100-SEND-INITIAL-MAP
           END-IF

           MOVE FUNCTION UPPER-CASE(WS-KIX-MYENV)
                                       TO SYSO
           MOVE WS-KIXHOST             TO HOSTO
           MOVE PI-COMPANY-ID          TO COMPANYO
120313     IF PI-RETURN-TO-PROGRAM = 'EL6314'
120313         MOVE '2'                TO DATASORO
120313     ELSE
120313         MOVE '4'                TO DATASORO
120313     END-IF
           MOVE BL-BATCH-NO            TO BATCHNOO
           MOVE BL-BATCH-SEQ           TO BSEQNOO
           MOVE BL-ARCHIVE-NO          TO ARCHNOO
           MOVE PI-PROCESSOR-ID        TO PROCIDO
      *    MOVE 'N'                    TO PRTSWO
072312*    MOVE -1                     TO PRTSWL
072312*    MOVE AL-UANON               TO PRTSWA
072312     MOVE -1                     TO SIGREQL
072312     MOVE AL-UANON               TO SIGREQA
121112     IF EIBAID = DFHPF4
062712        MOVE SPACES              TO PI-CLEAR-ERROR-SW
              MOVE ER-0280             TO EMI-ERROR

              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              MOVE BL-ARCHIVE-NO       TO W-ARCH-SUPPRESS
              MOVE W-ARCH-EDIT         TO EMI-TEXT-VARIABLE (1)
072312        MOVE 'Y'                 TO PI-FINALIZED-IND
072312        GO TO 9400-CLEAR
           ELSE
              MOVE ER-1818             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF

           GO TO 0350-BUILD-INIT-MAP
      
           .
       0360-EXIT.
           EXIT.
      
       0362-READ-ELCRTO.
      
           EXEC CICS READ
               DATASET   (WS-ELCRTO-FILE-ID)
               SET       (ADDRESS OF ORIGINAL-CERTIFICATE)
               RIDFLD    (ELCRTO-KEY)
120313         GTEQ
               RESP      (WS-RESPONSE)
           END-EXEC
      
           .
       0362-EXIT.
           EXIT.
      
       0363-READ-ELCRTO-UPD.
      
           EXEC CICS READ
               DATASET   (WS-ELCRTO-FILE-ID)
               SET       (ADDRESS OF ORIGINAL-CERTIFICATE)
               RIDFLD    (ELCRTO-KEY)
               UPDATE
               RESP      (WS-RESPONSE)
           END-EXEC
      
           .
       0363-EXIT.
           EXIT.
      
       0365-READ-ERENDT.
      
           EXEC CICS READ
               DATASET   (WS-ERENDT-FILE-ID)
               SET       (ADDRESS OF ENDORSEMENT-RECORD)
               RIDFLD    (ERENDT-KEY)
               RESP      (WS-RESPONSE)
           END-EXEC
      
           .
       0365-EXIT.
           EXIT.
      
       0400-ADD-ERENDT.
      
           PERFORM 1500-GET-ARCH-NO    THRU 1500-EXIT

           EXEC CICS GETMAIN
                SET      (ADDRESS OF ENDORSEMENT-RECORD)
                LENGTH   (ERENDT-LENGTH)
                INITIMG  (GETMAIN-SPACE)
           END-EXEC
       
           PERFORM 0410-INIT-ERENDT    THRU 0410-EXIT
           MOVE PI-PROCESSOR-ID        TO EN-LAST-MAINT-BY
           MOVE EIBTIME                TO EN-LAST-MAINT-HHMMSS
           MOVE WS-CURRENT-BIN-DT      TO EN-LAST-MAINT-DT
                                          EN-INPUT-DT
                                          EN-PROCESS-DT
062712
062712     MOVE OC-INS-FIRST-NAME    TO EN-INS-ORIG-FIRST-NAME
062712     MOVE OC-INS-MIDDLE-INIT   TO EN-INS-ORIG-MIDDLE-INIT
062712     MOVE OC-INS-LAST-NAME     TO EN-INS-ORIG-LAST-NAME
062712     MOVE OC-INS-AGE           TO EN-INS-ORIG-AGE
121712     MOVE OC-INS-AGE-DEFAULT-FLAG TO EN-INS-ORIG-AGE-DEF-FLAG
062712     MOVE OC-JNT-FIRST-NAME    TO EN-JNT-ORIG-FIRST-NAME
062712     MOVE OC-JNT-MIDDLE-INIT   TO EN-JNT-ORIG-MIDDLE-INIT
062712     MOVE OC-JNT-LAST-NAME     TO EN-JNT-ORIG-LAST-NAME
062712     MOVE OC-JNT-AGE           TO EN-JNT-ORIG-AGE
121712     MOVE OC-JNT-AGE-DEFAULT-FLAG TO EN-JNT-ORIG-AGE-DEF-FLAG
062712     MOVE OC-LF-BENCD          TO EN-LF-ORIG-BENCD
062712     IF OC-LF-BENCD NOT = '00' AND '  '
062712        MOVE OC-LF-BEN-AMT       TO EN-LF-ORIG-BEN-AMT
062712        MOVE OC-LF-PRM-AMT       TO EN-LF-ORIG-PRM-AMT
062712        MOVE OC-LF-ALT-BEN-AMT   TO EN-LF-ORIG-ALT-BEN-AMT
062712        MOVE OC-LF-ALT-PRM-AMT   TO EN-LF-ORIG-ALT-PRM-AMT
062712        MOVE OC-LF-TERM          TO EN-LF-ORIG-TERM
062712        MOVE OC-LF-EXP-DT        TO EN-LF-ORIG-EXP-DT
062712        MOVE OC-LF-COMM-PCT      TO EN-LF-ORIG-COMM-PCT
062712     END-IF
062712     MOVE OC-AH-BENCD          TO EN-AH-ORIG-BENCD    
062712     IF OC-AH-BENCD NOT = '00' AND '  '
062712        MOVE OC-AH-BEN-AMT       TO EN-AH-ORIG-BEN-AMT  
062712        MOVE OC-AH-PRM-AMT       TO EN-AH-ORIG-PRM-AMT     
062712        MOVE OC-AH-TERM          TO EN-AH-ORIG-TERM     
062712        MOVE OC-AH-EXP-DT        TO EN-AH-ORIG-EXP-DT
062712        MOVE OC-AH-CP            TO EN-AH-ORIG-CP
062712        MOVE OC-AH-COMM-PCT      TO EN-AH-ORIG-COMM-PCT
062712     END-IF
062712     MOVE OC-CRED-BENE-NAME      TO EN-ORIG-CRED-BENE
120313
120313     IF PI-RETURN-TO-PROGRAM EQUAL 'EL6314'
120313
120313         MOVE PI-COMPANY-CD       TO  W-CERT-COMPANY-CD
120313         MOVE PI-CRTO-CARRIER     TO  W-CERT-CARRIER
120313         MOVE PI-CRTO-GROUPING    TO  W-CERT-GROUPING
120313         MOVE PI-CRTO-STATE       TO  W-CERT-STATE
120313         MOVE PI-CRTO-ACCOUNT     TO  W-CERT-ACCOUNT
120313         MOVE PI-CRTO-CERT-EFF-DT TO  W-CERT-CERT-EFF-DT
120313         MOVE PI-CRTO-CERT-PRIME  TO  W-CERT-CERT-PRIME
120313         MOVE PI-CRTO-CERT-SFX    TO  W-CERT-CERT-SFX
120313
120313         EXEC CICS READ
120313             DATASET  (WS-ELCERT-FILE-ID)
120313             RIDFLD   (WS-ELCERT-KEY)
120313             SET      (ADDRESS OF CERTIFICATE-MASTER)
120313             RESP     (WS-RESPONSE)
120313         END-EXEC
120313         IF NOT RESP-NORMAL
120313            GO TO 0400-EXIT
120313         END-IF
120313
120313         MOVE CM-INSURED-FIRST-NAME TO EN-INS-NEW-FIRST-NAME
120313                                       NCRTO-INS-FIRST-NAME
120313         MOVE CM-INSURED-INITIAL2   TO EN-INS-NEW-MIDDLE-INIT
120313                                       NCRTO-INS-MIDDLE-INIT
120313         MOVE CM-INSURED-LAST-NAME  TO EN-INS-NEW-LAST-NAME
120313                                       NCRTO-INS-LAST-NAME
120313         MOVE CM-INSURED-ISSUE-AGE  TO EN-INS-NEW-AGE
120313                                       NCRTO-INS-AGE
120313         MOVE CM-JT-FIRST-NAME      TO EN-JNT-NEW-FIRST-NAME
120313                                       NCRTO-JNT-FIRST-NAME
120313         MOVE CM-JT-INITIAL         TO EN-JNT-NEW-MIDDLE-INIT
120313                                       NCRTO-JNT-MIDDLE-INIT
120313         MOVE CM-JT-LAST-NAME       TO EN-JNT-NEW-LAST-NAME
120313                                       NCRTO-JNT-LAST-NAME
120313         MOVE CM-INSURED-JOINT-AGE  TO EN-JNT-NEW-AGE
120313                                       NCRTO-JNT-AGE
120313
120313         MOVE CM-LF-BENEFIT-CD      TO EN-LF-NEW-BENCD
120313                                       NCRTO-LF-BENCD
120313         IF CM-LF-BENEFIT-CD NOT = '00' AND '  '
120313            MOVE CM-LF-BENEFIT-AMT  TO EN-LF-NEW-BEN-AMT
120313                                       NCRTO-LF-BEN-AMT
120313            MOVE CM-LF-PREMIUM-AMT  TO EN-LF-NEW-PRM-AMT
120313                                       NCRTO-LF-PRM-AMT
120313            MOVE CM-LF-ALT-BENEFIT-AMT TO EN-LF-NEW-ALT-BEN-AMT
120313                                       NCRTO-LF-ALT-BEN-AMT
120313            MOVE CM-LF-ALT-PREMIUM-AMT TO EN-LF-NEW-ALT-PRM-AMT
120313                                       NCRTO-LF-ALT-PRM-AMT
120313            MOVE CM-LF-ORIG-TERM    TO EN-LF-NEW-TERM
120313                                       NCRTO-LF-TERM
120313            MOVE CM-LF-LOAN-EXPIRE-DT TO EN-LF-NEW-EXP-DT
120313                                       NCRTO-LF-EXP-DT
120313            MOVE CM-LIFE-COMM-PCT   TO EN-LF-NEW-COMM-PCT
120313                                       NCRTO-LF-COMM-PCT
120313         END-IF
120313
120313         MOVE CM-AH-BENEFIT-CD      TO EN-AH-NEW-BENCD    
120313                                       NCRTO-AH-BENCD
120313         IF CM-AH-BENEFIT-CD NOT = '00' AND '  '
120313            MOVE CM-AH-BENEFIT-AMT  TO EN-AH-NEW-BEN-AMT  
120313                                       NCRTO-AH-BEN-AMT
120313            MOVE CM-AH-PREMIUM-AMT  TO EN-AH-NEW-PRM-AMT     
120313                                       NCRTO-AH-PRM-AMT
120313            MOVE CM-AH-ORIG-TERM    TO EN-AH-NEW-TERM     
120313                                       NCRTO-AH-TERM
120313            MOVE CM-AH-LOAN-EXPIRE-DT TO EN-AH-NEW-EXP-DT   
120313                                       NCRTO-AH-EXP-DT
120313            MOVE CM-AH-CRITICAL-PERIOD TO EN-AH-NEW-CP
120313                                       NCRTO-AH-CP
120313            MOVE CM-AH-COMM-PCT     TO EN-AH-NEW-COMM-PCT
120313                                       NCRTO-AH-COMM-PCT
120313         END-IF
120313
120313         MOVE CM-LOAN-1ST-PMT-DT    TO NCRTO-1ST-PMT-DT
120313         MOVE CM-ENTRY-BATCH        TO EN-BATCH-NUMBER
120313
120313         GO TO 0400-CONTINUE
120313     END-IF.
120313
           EXEC CICS GETMAIN
                SET      (ADDRESS OF PENDING-BUSINESS)
                LENGTH   (ERPNDB-LENGTH)
                INITIMG  (GETMAIN-SPACE)
           END-EXEC
      
           MOVE PI-ERPNDB-KEY          TO ERPNDB-KEY
           EXEC CICS READ
               DATASET   (WS-ERPNDB-FILE-ID)
               INTO      (PENDING-BUSINESS)
               RIDFLD    (ERPNDB-KEY)
               RESP      (WS-RESPONSE)
           END-EXEC
      
           IF NOT RESP-NORMAL
              GO TO 0400-EXIT
           END-IF
      
      *    set found-pndb to true

           MOVE PB-I-INSURED-FIRST-NAME  TO EN-INS-NEW-FIRST-NAME
062712                                      NCRTO-INS-FIRST-NAME
           MOVE PB-I-INSURED-MIDDLE-INIT TO EN-INS-NEW-MIDDLE-INIT
062712                                      NCRTO-INS-MIDDLE-INIT
           MOVE PB-I-INSURED-LAST-NAME   TO EN-INS-NEW-LAST-NAME
062712                                      NCRTO-INS-LAST-NAME
           MOVE PB-I-AGE                 TO EN-INS-NEW-AGE
062712                                      NCRTO-INS-AGE
           MOVE PB-I-JOINT-FIRST-NAME    TO EN-JNT-NEW-FIRST-NAME
062712                                      NCRTO-JNT-FIRST-NAME
           MOVE PB-I-JOINT-MIDDLE-INIT   TO EN-JNT-NEW-MIDDLE-INIT
062712                                      NCRTO-JNT-MIDDLE-INIT
           MOVE PB-I-JOINT-LAST-NAME     TO EN-JNT-NEW-LAST-NAME
062712                                      NCRTO-JNT-LAST-NAME
           MOVE PB-I-JOINT-AGE           TO EN-JNT-NEW-AGE
062712                                      NCRTO-JNT-AGE
      
062712     MOVE PB-I-LF-BENEFIT-CD       TO EN-LF-NEW-BENCD
062712                                      NCRTO-LF-BENCD
           IF PB-I-LF-BENEFIT-CD NOT = '00' AND '  '
              MOVE PB-I-LF-BENEFIT-AMT   TO EN-LF-NEW-BEN-AMT
062712                                      NCRTO-LF-BEN-AMT
              MOVE PB-I-LF-PREMIUM-AMT   TO EN-LF-NEW-PRM-AMT
062712                                      NCRTO-LF-PRM-AMT
              MOVE PB-I-LF-ALT-BENEFIT-AMT TO EN-LF-NEW-ALT-BEN-AMT
062712                                      NCRTO-LF-ALT-BEN-AMT
              MOVE PB-I-LF-ALT-PREMIUM-AMT TO EN-LF-NEW-ALT-PRM-AMT
062712                                      NCRTO-LF-ALT-PRM-AMT
              MOVE PB-I-LF-TERM          TO EN-LF-NEW-TERM
062712                                      NCRTO-LF-TERM
              MOVE PB-I-LF-EXPIRE-DT     TO EN-LF-NEW-EXP-DT
062712                                      NCRTO-LF-EXP-DT
              IF PB-I-JOINT-COMMISSION > +0
                 MOVE PB-I-JOINT-COMMISSION
                                         TO EN-LF-NEW-COMM-PCT
062712                                      NCRTO-LF-COMM-PCT
              ELSE
                 MOVE PB-I-LIFE-COMMISSION
                                         TO EN-LF-NEW-COMM-PCT
062712                                      NCRTO-LF-COMM-PCT
              END-IF
           END-IF
      
062712     MOVE PB-I-AH-BENEFIT-CD       TO EN-AH-NEW-BENCD    
062712                                      NCRTO-AH-BENCD
           IF PB-I-AH-BENEFIT-CD NOT = '00' AND '  '
              MOVE PB-I-AH-BENEFIT-AMT   TO EN-AH-NEW-BEN-AMT  
062712                                      NCRTO-AH-BEN-AMT
              MOVE PB-I-AH-PREMIUM-AMT   TO EN-AH-NEW-PRM-AMT     
062712                                      NCRTO-AH-PRM-AMT
              MOVE PB-I-AH-TERM          TO EN-AH-NEW-TERM     
062712                                      NCRTO-AH-TERM
              MOVE PB-I-AH-EXPIRE-DT     TO EN-AH-NEW-EXP-DT   
062712                                      NCRTO-AH-EXP-DT
              MOVE PB-I-AH-CRIT-PER      TO EN-AH-NEW-CP
062712                                      NCRTO-AH-CP
              MOVE PB-I-AH-COMMISSION    TO EN-AH-NEW-COMM-PCT
062712                                      NCRTO-AH-COMM-PCT
           END-IF

062712     MOVE PB-I-1ST-PMT-DT          TO NCRTO-1ST-PMT-DT
072312     MOVE PB-ENTRY-BATCH           TO EN-BATCH-NUMBER
120313     .
120313 0400-CONTINUE.
120313
072312     MOVE W-ACCT-SUMM              TO EN-ACCT-SUMM
072312     MOVE W-CSO-SUMM               TO EN-CSO-SUMM

072312*     PERFORM 0450-GET-ERACCT     THRU 0450-EXIT

072312     MOVE 'Y'                    TO EN-COMM-CHGBK
072312
           COMPUTE WS-DUE-BORROWER = (EN-LF-ORIG-PRM-AMT
              + EN-LF-ORIG-ALT-PRM-AMT + EN-AH-ORIG-PRM-AMT) -
              (EN-LF-NEW-PRM-AMT + EN-LF-NEW-ALT-PRM-AMT +
              EN-AH-NEW-PRM-AMT)

           COMPUTE EN-ACCT-PORTION ROUNDED = 
              (((EN-LF-ORIG-PRM-AMT + EN-LF-ORIG-ALT-PRM-AMT) -
              (EN-LF-NEW-PRM-AMT + EN-LF-NEW-ALT-PRM-AMT))
              * EN-LF-NEW-COMM-PCT)
                             +
              ((EN-AH-ORIG-PRM-AMT - EN-AH-NEW-PRM-AMT)
              * EN-AH-NEW-COMM-PCT)

           COMPUTE EN-CSO-PORTION = WS-DUE-BORROWER -
              EN-ACCT-PORTION
      
120313     IF PI-RETURN-TO-PROGRAM = 'EL6314'
120313         MOVE CM-CONTROL-PRIMARY TO WS-ERMAIL-KEY
120313     ELSE
           move pb-control-by-account (1:33)
                                       to ws-ermail-key
120313     END-IF
120313
           EXEC CICS READ
               DATASET   ('ERMAIL')
               SET       (ADDRESS OF MAILING-DATA)
               RIDFLD    (WS-ERMAIL-KEY)
               RESP      (WS-RESPONSE)
           END-EXEC
      
           IF RESP-NORMAL
              MOVE MA-CRED-BENE-NAME   TO EN-NEW-CRED-BENE
062712                                    NCRTO-CRED-BENE-NAME
           ELSE
              DISPLAY ' ERMAIL READ ' WS-RESPONSE
           END-IF
121712
120313     IF PI-RETURN-TO-PROGRAM = 'EL6314'
120313         MOVE CM-CONTROL-PRIMARY TO WS-ELCRTT-PRIMARY
120313     ELSE
121712     MOVE PB-CONTROL-BY-ACCOUNT (1:33) TO WS-ELCRTT-PRIMARY
120313     END-IF
120313
121712     MOVE 'C'                   TO WS-ELCRTT-REC-TYPE
121712
121712     EXEC CICS READ
121712          DATASET  (WS-ELCRTT-FILE-ID)
121712          RIDFLD   (WS-ELCRTT-KEY)
121712          SET      (ADDRESS OF CERTIFICATE-TRAILERS)
121712          RESP     (WS-RESPONSE)
121712     END-EXEC
121712
121712     IF RESP-NORMAL
121712        MOVE CS-INS-AGE-DEFAULT-FLAG TO EN-INS-NEW-AGE-DEF-FLAG
121712                                     NCRTO-INS-AGE-DEFAULT-FLAG
121712        MOVE CS-JNT-AGE-DEFAULT-FLAG TO EN-JNT-NEW-AGE-DEF-FLAG
121712                                     NCRTO-JNT-AGE-DEFAULT-FLAG
121712     END-IF.
       
           PERFORM VARYING S1 FROM +1 BY +1 UNTIL S1 > +12
              IF REACD-IN (S1) NOT = LOW-VALUES
                 MOVE REACD-IN (S1)    TO EN-REASON-CODE (S1)
              END-IF
           END-PERFORM
           MOVE LTRIDI                 TO EN-TEMPLATE-USED
           MOVE 'G'                    TO EN-DOCU-TYPE
           MOVE 'Y'                    TO EN-MONEY-SW
           MOVE SIGREQI                TO EN-SIG-SW
           MOVE VOUCHI                 TO EN-VOUCHER-SW
           MOVE PAYEEI                 TO EN-PAYEE
           MOVE HLTHAPPI               TO EN-HEALTH-APP
091615     if pi-check-cut = 'Y' and pi-check-type = 'C'
091615        move 'C'                 to en-check-type
091615     end-if

           PERFORM 0430-WRITE-ERENDT   THRU 0430-EXIT

           .
       0400-EXIT.
           EXIT.
      
       0410-INIT-ERENDT.
      
           MOVE OC-CONTROL-PRIMARY     TO ERENDT-KEY
           MOVE ZEROS                  TO ENDT-SEQ-NO
      
           EXEC CICS READ
               DATASET    (WS-ERENDT-FILE-ID)
               RIDFLD     (ERENDT-KEY)
               INTO       (ENDORSEMENT-RECORD)
               RESP       (WS-RESPONSE)
               GTEQ
           END-EXEC
      
           IF RESP-NORMAL
              AND (EN-CONTROL-PRIMARY (1:34) =
                 OC-CONTROL-PRIMARY (1:34))
              COMPUTE WS-SEQ-NO = EN-SEQ-NO - +1
           ELSE
              MOVE +4096               TO WS-SEQ-NO
           END-IF
      
           MOVE 'EN'                   TO ENDORSEMENT-RECORD
           MOVE OC-CONTROL-PRIMARY     TO EN-CONTROL-PRIMARY
           MOVE EN-COMPANY-CD          TO EN-COMPANY-CD-A1
           MOVE WS-SEQ-NO              TO EN-SEQ-NO
      
           INITIALIZE EN-ISSUE-RECORD
      
           MOVE LOW-VALUES             TO EN-LF-ORIG-EXP-DT
                                          EN-AH-ORIG-EXP-DT
                                          EN-LF-NEW-EXP-DT
                                          EN-AH-NEW-EXP-DT
                                          EN-PROCESS-DT
      
           .
       0410-EXIT.
           EXIT.
      
       0420-UPDATE-ELCRTO.
      
           PERFORM 0363-READ-ELCRTO-UPD THRU 0363-EXIT
           IF RESP-NORMAL
              MOVE WS-CURRENT-BIN-DT   TO OC-ENDORSEMENT-PROCESSED-DT
061112                                    OC-LAST-MAINT-DT
061112        MOVE EIBTIME             TO OC-LAST-MAINT-HHMMSS
061112        MOVE PI-PROCESSOR-ID     TO OC-LAST-MAINT-BY

              EXEC CICS REWRITE
                 FROM     (ORIGINAL-CERTIFICATE)
                 DATASET  ('ELCRTO')
              END-EXEC
           END-IF
      
           .
       0420-EXIT.
           EXIT.
061112
061112 0425-ADD-NEW-ELCRTO.
061112     MOVE SPACES                 TO ORIGINAL-CERTIFICATE
061112     MOVE 'OC'                   TO OC-RECORD-ID
061112     MOVE NEW-ELCRTO-KEY         TO OC-CONTROL-PRIMARY
061112     MOVE PI-PROCESSOR-ID        TO OC-LAST-MAINT-BY
061112     MOVE EIBTIME                TO OC-LAST-MAINT-HHMMSS
061112     MOVE WS-CURRENT-BIN-DT      TO OC-LAST-MAINT-DT
062712     MOVE NCRTO-INS-FIRST-NAME   TO OC-INS-FIRST-NAME   
062712     MOVE NCRTO-INS-MIDDLE-INIT  TO OC-INS-MIDDLE-INIT  
062712     MOVE NCRTO-INS-LAST-NAME    TO OC-INS-LAST-NAME    
062712     MOVE NCRTO-INS-AGE          TO OC-INS-AGE          
062712     MOVE NCRTO-JNT-FIRST-NAME   TO OC-JNT-FIRST-NAME   
062712     MOVE NCRTO-JNT-MIDDLE-INIT  TO OC-JNT-MIDDLE-INIT  
062712     MOVE NCRTO-JNT-LAST-NAME    TO OC-JNT-LAST-NAME    
062712     MOVE NCRTO-JNT-AGE          TO OC-JNT-AGE          
062712     MOVE NCRTO-LF-BENCD         TO OC-LF-BENCD    
062712     MOVE NCRTO-LF-BEN-AMT       TO OC-LF-BEN-AMT   
062712     MOVE NCRTO-LF-PRM-AMT       TO OC-LF-PRM-AMT   
062712     MOVE NCRTO-LF-ALT-BEN-AMT   TO OC-LF-ALT-BEN-AMT
062712     MOVE NCRTO-LF-ALT-PRM-AMT   TO OC-LF-ALT-PRM-AMT
062712     MOVE NCRTO-LF-TERM          TO OC-LF-TERM          
062712     MOVE NCRTO-LF-EXP-DT        TO OC-LF-EXP-DT     
062712     MOVE NCRTO-LF-COMM-PCT      TO OC-LF-COMM-PCT
062712     MOVE LOW-VALUES             TO OC-LF-CANCEL-DT
062712     MOVE +0                     TO OC-LF-CANCEL-AMT
071712                                    OC-LF-ITD-CANCEL-AMT
062712     MOVE NCRTO-AH-BENCD         TO OC-AH-BENCD 
062712     MOVE NCRTO-AH-BEN-AMT       TO OC-AH-BEN-AMT
062712     MOVE NCRTO-AH-PRM-AMT       TO OC-AH-PRM-AMT
062712     MOVE NCRTO-AH-TERM          TO OC-AH-TERM       
062712     MOVE NCRTO-AH-EXP-DT        TO OC-AH-EXP-DT  
062712     MOVE NCRTO-AH-CP            TO OC-AH-CP   
062712     MOVE NCRTO-AH-COMM-PCT      TO OC-AH-COMM-PCT
062712     MOVE LOW-VALUES             TO OC-AH-CANCEL-DT    
062712     MOVE +0                     TO OC-AH-CANCEL-AMT
071712                                    OC-AH-ITD-CANCEL-AMT
061112     MOVE NCRTO-CRED-BENE-NAME   TO OC-CRED-BENE-NAME
062712     MOVE NCRTO-1ST-PMT-DT       TO OC-1ST-PMT-DT
121712     MOVE NCRTO-INS-AGE-DEFAULT-FLAG TO OC-INS-AGE-DEFAULT-FLAG
121712     MOVE NCRTO-JNT-AGE-DEFAULT-FLAG TO OC-JNT-AGE-DEFAULT-FLAG
011413     MOVE 'Y'                    TO OC-ISSUE-TRAN-IND
011413     MOVE 'N'                    TO OC-CANCEL-TRAN-IND
061112     MOVE LOW-VALUES             TO OC-ENDORSEMENT-PROCESSED-DT
061112     .
061112 0425-WRITE-ELCRTO.
061112
061112     EXEC CICS WRITE
061112        DATASET   ('ELCRTO')
061112        FROM      (ORIGINAL-CERTIFICATE)
061112        RIDFLD    (OC-CONTROL-PRIMARY)
061112        RESP      (WS-RESPONSE)
061112     END-EXEC
061112
061112     IF RESP-DUPKEY
061112        SUBTRACT +1              FROM OC-KEY-SEQ-NO
061112        GO TO 0425-WRITE-ELCRTO
061112     ELSE
061112        IF NOT RESP-NORMAL
061112           MOVE -1               TO SIGREQL
061112           MOVE ER-3830          TO EMI-ERROR
061112           PERFORM 9900-ERROR-FORMAT
061112                                 THRU 9900-EXIT
061112        ELSE
061112           MOVE OC-CONTROL-PRIMARY TO PI-ELCRTO-KEY
061112        END-IF
061112     END-IF
061112
061112     .
061112 0425-EXIT.
061112     EXIT.
061112
072312 0426-ADD-CERT-NOTES.
072312
103012     MOVE LOW-VALUES TO CERT-NOTE-ENTRIES
103012     SET TB-INDX TO 1
103012
072312     IF PI-CERTNT1-ENTERED > SPACES
103012         MOVE PI-CERTNT1-ENTERED TO CERT-NT-TEXT (TB-INDX)
103012         MOVE PI-PROCESSOR-ID    TO 
103012                      CERT-NT-LAST-MAINT-BY (TB-INDX)
103012         MOVE EIBTIME            TO 
103012                      CERT-NT-LAST-MAINT-HHMMSS (TB-INDX)
103012         MOVE WS-CURRENT-BIN-DT  TO 
103012                      CERT-NT-LAST-MAINT-DT (TB-INDX)
103012         SET TB-INDX UP BY +1
072312     END-IF
072312
072312     IF PI-CERTNT2-ENTERED > SPACES
103012         MOVE PI-CERTNT2-ENTERED TO CERT-NT-TEXT (TB-INDX)
103012         MOVE PI-PROCESSOR-ID    TO 
103012                      CERT-NT-LAST-MAINT-BY (TB-INDX)
103012         MOVE EIBTIME            TO 
103012                      CERT-NT-LAST-MAINT-HHMMSS (TB-INDX)
103012         MOVE WS-CURRENT-BIN-DT  TO 
103012                      CERT-NT-LAST-MAINT-DT (TB-INDX)
103012         SET TB-INDX UP BY +1
072312     END-IF
072312
072312     IF PI-CERTNT3-ENTERED > SPACES
103012         MOVE PI-CERTNT3-ENTERED TO CERT-NT-TEXT (TB-INDX)
103012         MOVE PI-PROCESSOR-ID    TO 
103012                      CERT-NT-LAST-MAINT-BY (TB-INDX)
103012         MOVE EIBTIME            TO 
103012                      CERT-NT-LAST-MAINT-HHMMSS (TB-INDX)
103012         MOVE WS-CURRENT-BIN-DT  TO 
103012                      CERT-NT-LAST-MAINT-DT (TB-INDX)
103012         SET TB-INDX UP BY +1
072312     END-IF
072312
072312     IF PI-CERTNT4-ENTERED > SPACES
103012         MOVE PI-CERTNT4-ENTERED TO CERT-NT-TEXT (TB-INDX)
103012         MOVE PI-PROCESSOR-ID    TO 
103012                      CERT-NT-LAST-MAINT-BY (TB-INDX)
103012         MOVE EIBTIME            TO 
103012                      CERT-NT-LAST-MAINT-HHMMSS (TB-INDX)
103012         MOVE WS-CURRENT-BIN-DT  TO 
103012                      CERT-NT-LAST-MAINT-DT (TB-INDX)
103012         SET TB-INDX UP BY +1
072312     END-IF
103012
103012     IF CERT-NOTE-ENTRIES > SPACES
103012         PERFORM 0427-LOAD-CURRENT-CERT-NOTES THRU 0427-EXIT
103012         PERFORM 1700-WRITE-CERT-NOTE THRU 1799-EXIT
103012     END-IF
121212
121212     IF PI-BILLNT1-ENTERED > SPACES
121212         MOVE SPACES             TO WS-MANUAL-BILL-NOTE
121212         MOVE PI-BILLNT1-ENTERED TO WS-MAN-BN-NOTE
121212         MOVE +63 TO WS-LEN
121212         PERFORM 0441-UPDATE-BILLING-NOTE THRU 0441-EXIT
072312     END-IF
072312
121212     IF PI-BILLNT2-ENTERED > SPACES
121212         MOVE SPACES             TO WS-MANUAL-BILL-NOTE
121212         MOVE PI-BILLNT2-ENTERED TO WS-MAN-BN-NOTE
121212         MOVE +63 TO WS-LEN
121212         PERFORM 0441-UPDATE-BILLING-NOTE THRU 0441-EXIT
072312     END-IF
072312
072312     .
072312 0426-EXIT.
072312     EXIT.
072312
103012 0427-LOAD-CURRENT-CERT-NOTES.
103012
103012     MOVE PI-CRTO-COMPANY-CD  TO ERCNOT-COMPANY-CD
103012     MOVE PI-CRTO-CARRIER     TO ERCNOT-CARRIER
103012     MOVE PI-CRTO-GROUPING    TO ERCNOT-GROUPING
103012     MOVE PI-CRTO-STATE       TO ERCNOT-STATE
103012     MOVE PI-CRTO-ACCOUNT     TO ERCNOT-ACCOUNT
103012     MOVE PI-CRTO-CERT-EFF-DT TO ERCNOT-EFF-DT
103012     MOVE PI-CRTO-CERT-PRIME  TO ERCNOT-CERT-PRIME
103012     MOVE PI-CRTO-CERT-SFX    TO ERCNOT-CERT-SFX
103012     MOVE '1'                 TO ERCNOT-REC-TYP
103012     MOVE +0                  TO ERCNOT-SEQ
103012     MOVE ERCNOT-PARTIAL-KEY  TO SV-PARTIAL-KEY
103012
103012     EXEC CICS STARTBR
103012          DATASET(WS-ERCNOT-FILE-ID)
103012          RIDFLD(ERCNOT-KEY)
103012          KEYLENGTH(ERCNOT-START-LENGTH)
103012          RESP      (WS-RESPONSE)
103012          GENERIC
103012          GTEQ
103012     END-EXEC.
103012
103012     IF NOT RESP-NORMAL
103012        GO TO 0427-ENDBR
103012     END-IF.
103012
103012 0427-LOOP.
103012     EXEC CICS READNEXT
103012          SET(ADDRESS OF CERT-NOTE-FILE)
103012          DATASET(WS-ERCNOT-FILE-ID)
103012          RIDFLD(ERCNOT-KEY)
103012     END-EXEC.
103012
103012     IF CZ-COMPANY-CD NOT = PI-CRTO-COMPANY-CD
103012         GO TO 0427-ENDBR
103012     END-IF.
103012
103012     IF (CZ-CARRIER = SV-CARRIER)
103012        AND (CZ-GROUPING = SV-GROUPING)
103012        AND (CZ-STATE = SV-STATE)
103012        AND (CZ-ACCOUNT = SV-ACCOUNT)
103012        AND (CZ-CERT-EFF-DT = SV-EFF-DT)
103012        AND (CZ-CERT-NO = SV-CERT-NO)
103012        AND (CZ-RECORD-TYPE = '1')
103012          MOVE CZ-NOTE TO CERT-NT-TEXT (TB-INDX)
103012          MOVE CZ-LAST-MAINT-USER TO 
103012                      CERT-NT-LAST-MAINT-BY (TB-INDX)
103012          MOVE CZ-LAST-MAINT-DT TO 
103012                      CERT-NT-LAST-MAINT-DT (TB-INDX)
103012          MOVE CZ-LAST-MAINT-HHMMSS TO
103012                      CERT-NT-LAST-MAINT-HHMMSS (TB-INDX)
103012          SET TB-INDX UP BY 1
103012          GO TO 0427-LOOP
103012     END-IF.
103012
103012 0427-ENDBR.
103012
103012     EXEC CICS ENDBR
103012          DATASET(WS-ERCNOT-FILE-ID)
103012     END-EXEC.
103012
103012     .
103012 0427-EXIT.
103012     EXIT.
103012
       0430-WRITE-ERENDT.

           MOVE EN-CONTROL-PRIMARY     TO PI-ERENDT-KEY
           MOVE W-ARCH-NUMBER          TO EN-ARCHIVE-NO
           EXEC CICS WRITE
                DATASET   (WS-ERENDT-FILE-ID)
                FROM      (ENDORSEMENT-RECORD)
                RIDFLD    (EN-CONTROL-PRIMARY)
                RESP      (WS-RESPONSE)
           END-EXEC
           IF RESP-NORMAL
              MOVE 'G'                 TO PI-DOCUMENT-PROCESSED
121112        IF EIBAID = DFHPF4            
                 PERFORM 0420-UPDATE-ELCRTO THRU 0420-EXIT
061112           PERFORM 0425-ADD-NEW-ELCRTO THRU 0425-EXIT
072312           PERFORM 0426-ADD-CERT-NOTES THRU 0426-EXIT
072312           PERFORM 0440-ADD-BILLING-NOTE THRU 0440-EXIT
062712        END-IF  
           ELSE
              DISPLAY ' NOT A GOOD WRITE ERENDT ' WS-RESPONSE
060612        MOVE -1                  TO SIGREQL
              MOVE ER-0132             TO EMI-ERROR
060612        MOVE AL-UABON            TO SIGREQA
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
           END-IF

           .
       0430-EXIT.
           EXIT.
072312
072312
072312 0440-ADD-BILLING-NOTE.
072312     EXEC CICS GETMAIN
072312          SET      (ADDRESS OF EOB-CODES)
072312          LENGTH   (ELEOBC-LENGTH)
072312          INITIMG  (GETMAIN-SPACE)
072312     END-EXEC           
072312
072312     MOVE LOW-VALUES             TO WS-ELEOBC-KEY
072312     MOVE PI-COMPANY-CD          TO WS-EOBC-COMPANY-CD
072312     MOVE '5'                    TO WS-EOBC-REC-TYPE
072312
072312     EXEC CICS STARTBR                                            
072312         DATASET   ('ELEOBC')
072312         RIDFLD    (WS-ELEOBC-KEY)
072312         GTEQ
072312         RESP      (WS-RESPONSE)
072312     END-EXEC
072312
072312     IF NOT RESP-NORMAL
072312        GO TO 0440-EXIT
072312     END-IF
072312      .
072312 0440-READNEXT-ELEOBC.
072312
072312     EXEC CICS READNEXT
072312        INTO    (EOB-CODES)
072312        DATASET ('ELEOBC')
072312        RIDFLD  (WS-ELEOBC-KEY)
072312        RESP    (WS-RESPONSE)
072312     END-EXEC
072312
072312     IF RESP-NORMAL
072312         IF EO-RECORD-TYPE NOT = '5'
072312             GO TO 0440-EXIT
072312         END-IF
072312     ELSE
072312         GO TO 0440-EXIT
072312     END-IF
072312     
072312     IF EO-RECORD-TYPE = '5' AND
072312        EO-EOB-CODE = PI-LTRID-ENTERED
072312           CONTINUE
072312     ELSE
072312         GO TO 0440-READNEXT-ELEOBC
072312     END-IF
072312     
072312     MOVE SPACES TO WS-BILLING-NOTE
072312     MOVE EO-DESCRIPTION TO WS-BN-NOTE
072312     MOVE PI-LTRID-ENTERED TO WS-BN-LTRID
072312     MOVE WS-CURRENT-DT TO WS-BN-DATE
072312     MOVE PI-PROCESSOR-ID TO WS-BN-USERID
121212     MOVE +25 TO WS-LEN
072312
121212     PERFORM 0441-UPDATE-BILLING-NOTE THRU 0441-EXIT
121212     .
121212 0440-EXIT.
121212     EXIT.
121212
121212 0441-UPDATE-BILLING-NOTE.
072312     EXEC CICS GETMAIN
072312          SET      (ADDRESS OF CERTIFICATE-NOTE)
072312          LENGTH   (ERNOTE-LENGTH)
072312          INITIMG  (GETMAIN-SPACE)
072312     END-EXEC           
072312     
072312     MOVE PI-ERENDT-KEY (1:33) TO WS-ERNOTE-KEY
041320     move '1'                    to w-note-record-type
072312
072312     EXEC CICS READ
072312        DATASET    (WS-ERNOTE-FILE-ID)
072312        RIDFLD     (WS-ERNOTE-KEY)
072312        INTO       (CERTIFICATE-NOTE)
072312        RESP       (WS-RESPONSE)
072312        UPDATE
072312     END-EXEC
072312
072312     IF RESP-NORMAL
072312       IF CN-BILLING-START-LINE-NO NOT NUMERIC
072312          MOVE ZEROS            TO CN-BILLING-START-LINE-NO
072312       END-IF
072312       IF CN-BILLING-END-LINE-NO NOT NUMERIC
072312          MOVE ZEROS            TO CN-BILLING-END-LINE-NO
072312       END-IF
072312       PERFORM VARYING NOTE-SUB FROM +1 BY +1 UNTIL
072312           (NOTE-SUB > +10) OR
121212           (CN-LINE (NOTE-SUB) (1:WS-LEN) = 
121212                             WS-BILLING-NOTE (1:WS-LEN))
072312       END-PERFORM
121212       IF CN-LINE (NOTE-SUB) (1:WS-LEN) = 
121212                              WS-BILLING-NOTE (1:WS-LEN)
072312         EXEC CICS UNLOCK
072312            DATASET    (WS-ERNOTE-FILE-ID)
072312         END-EXEC
072312       ELSE
072312         PERFORM VARYING NOTE-SUB FROM +1 BY +1 UNTIL
072312           (NOTE-SUB > +10) OR
072312           (CN-LINE (NOTE-SUB) = SPACES OR LOW-VALUES) 
072312         END-PERFORM
072312         IF (NOTE-SUB < +11)
072312           IF NOTE-SUB >= CN-BILLING-START-LINE-NO AND
072312              NOTE-SUB <= CN-BILLING-END-LINE-NO
072312                MOVE WS-BILLING-NOTE TO CN-LINE (NOTE-SUB)
072312           ELSE 
072312             IF (CN-BILLING-END-LINE-NO NOT = ZEROS) AND
072312              (NOTE-SUB = (CN-BILLING-END-LINE-NO + +1))
072312                MOVE WS-BILLING-NOTE   TO CN-LINE (NOTE-SUB)
072312                MOVE NOTE-SUB     TO CN-BILLING-END-LINE-NO
072312             ELSE
072312               IF (CN-BILLING-START-LINE-NO NOT = ZEROS) AND
072312                  (NOTE-SUB = (CN-BILLING-START-LINE-NO - +1))
072312                     MOVE WS-BILLING-NOTE TO CN-LINE (NOTE-SUB)
072312                     MOVE NOTE-SUB  TO CN-BILLING-START-LINE-NO
072312               ELSE
072312                 IF (CN-BILLING-END-LINE-NO = ZEROS)
072312                   MOVE WS-BILLING-NOTE  TO CN-LINE (NOTE-SUB)
072312                   MOVE NOTE-SUB    TO CN-BILLING-END-LINE-NO
072312                                       CN-BILLING-START-LINE-NO
072312                 ELSE
072312                    PERFORM 0442-SQUEEZE-IT-IN THRU 0442-EXIT
072312                 END-IF
072312               END-IF                          
072312             END-IF
072312           END-IF
072312           MOVE PI-PROCESSOR-ID     TO CN-LAST-MAINT-USER
072312           MOVE WS-CURRENT-BIN-DT   TO CN-LAST-MAINT-DT
072312           MOVE EIBTIME             TO CN-LAST-MAINT-HHMMSS
072312           EXEC CICS REWRITE
072312              DATASET    (WS-ERNOTE-FILE-ID)
072312              FROM       (CERTIFICATE-NOTE)
072312              RESP       (WS-RESPONSE)
072312           END-EXEC
072312           PERFORM 0445-CERTIFICATE-UPDATE THRU 0445-EXIT
072312         END-IF
072312       END-IF
072312     ELSE
072312        MOVE SPACES              TO CERTIFICATE-NOTE
072312        MOVE 'CN'                TO CN-RECORD-ID
072312        MOVE PI-ERENDT-KEY (1:33) TO CN-CONTROL-PRIMARY
072312                                     WS-ERNOTE-KEY
041320        move '1'                 to cn-record-type
041320                                    w-note-record-type
072312        MOVE 01                  TO CN-BILLING-START-LINE-NO
072312                                    CN-BILLING-END-LINE-NO
072312        MOVE WS-BILLING-NOTE     TO CN-LINE (01)
072312        MOVE PI-PROCESSOR-ID     TO CN-LAST-MAINT-USER
072312        MOVE WS-CURRENT-BIN-DT   TO CN-LAST-MAINT-DT
072312        MOVE EIBTIME             TO CN-LAST-MAINT-HHMMSS
072312        EXEC CICS WRITE
072312           DATASET    (WS-ERNOTE-FILE-ID)
072312           FROM       (CERTIFICATE-NOTE)
072312           RIDFLD     (WS-ERNOTE-KEY)
072312           RESP       (WS-RESPONSE)
072312        END-EXEC
072312
072312        PERFORM 0445-CERTIFICATE-UPDATE THRU 0445-EXIT
072312     END-IF              
072312
072312     .
121212 0441-EXIT.
072312     EXIT.
072312
072312
072312 0442-SQUEEZE-IT-IN.
072312
072312     IF NOTE-SUB < CN-BILLING-START-LINE-NO
072312        PERFORM VARYING NOTE-SUB FROM NOTE-SUB BY +1 UNTIL
072312           NOTE-SUB = +10
072312           MOVE CN-LINE (NOTE-SUB + 1) TO CN-LINE (NOTE-SUB)
072312           IF (NOTE-SUB + 1) = (CN-BILLING-START-LINE-NO - 1)
072312             MOVE WS-BILLING-NOTE TO CN-LINE (NOTE-SUB + 1)
072312             COMPUTE CN-BILLING-START-LINE-NO = NOTE-SUB + 1
072312             MOVE +9 TO NOTE-SUB
072312           END-IF
072312        END-PERFORM
072312     ELSE
072312        IF NOTE-SUB > CN-BILLING-END-LINE-NO
072312           PERFORM VARYING NOTE-SUB FROM NOTE-SUB BY -1 
072312             UNTIL NOTE-SUB = +1
072312             MOVE CN-LINE (NOTE-SUB - 1) TO CN-LINE (NOTE-SUB)
072312             IF (NOTE-SUB - 1) = (CN-BILLING-END-LINE-NO + 1)
072312                MOVE WS-BILLING-NOTE  TO CN-LINE (NOTE-SUB - 1)
072312                COMPUTE CN-BILLING-END-LINE-NO = NOTE-SUB - 1
072312                MOVE +2          TO NOTE-SUB
072312             END-IF
072312           END-PERFORM
072312        END-IF  
072312     END-IF
072312
072312     .
072312 0442-EXIT.
072312     EXIT.
072312
072312 0445-CERTIFICATE-UPDATE.

072312     PERFORM 1800-READ-ELCERT-UPDATE THRU 1800-EXIT
072312     IF RESP-NORMAL
072312        EVALUATE CM-NOTE-SW
072312           WHEN '2'
072312           WHEN '3'
072312           WHEN '6'
072312           WHEN '7'
072312              SET NO-CERT-RW     TO TRUE
072312           WHEN ' '
072312              MOVE '2'           TO CM-NOTE-SW
072312           WHEN '1'
072312              MOVE '3'           TO CM-NOTE-SW
072312           WHEN '4'
072312              MOVE '6'           TO CM-NOTE-SW
072312           WHEN '5'
072312              MOVE '7'           TO CM-NOTE-SW
072312        END-EVALUATE
072312     END-IF
072312     IF NOT NO-CERT-RW
072312        PERFORM 1810-REWRITE-ELCERT
072312                                 THRU 1810-EXIT
072312     ELSE
072312        EXEC CICS UNLOCK
072312           DATASET    (WS-ELCERT-FILE-ID)
072312        END-EXEC
072312     END-IF
072312
072312     .
072312 0445-EXIT.
072312     EXIT.
072312

       0450-GET-ERACCT.

           MOVE ' '                    TO WS-ERACCT-SW
072312     MOVE ZEROS                  TO WS-CHGBACK
                                          WS-CSO-PORTION
                                          WS-ACCT-PORTION
                                          WS-DIFF

           PERFORM 0460-STARTBR-ERACCT THRU 0460-EXIT

           IF RESP-NORMAL
              PERFORM 0470-READNEXT-ERACCT
                                       THRU 0470-EXIT
           END-IF

           IF RESP-NORMAL
072312        IF AM-CONTROL-PRIMARY (1:20) = EN-CONTROL-PRIMARY (1:20)
                 IF EN-CERT-EFF-DT < AM-EXPIRATION-DT
                    AND >= AM-EFFECTIVE-DT
                    SET ACCT-FOUND TO TRUE
                 ELSE
                    PERFORM 0470-READNEXT-ERACCT
                                       THRU 0470-EXIT
                    IF RESP-NORMAL
072312                 IF AM-CONTROL-PRIMARY (1:20)
072312                    = EN-CONTROL-PRIMARY (1:20)
                          IF EN-CERT-EFF-DT < AM-EXPIRATION-DT
                             AND >= AM-EFFECTIVE-DT
                             SET ACCT-FOUND TO TRUE
                          END-IF
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-IF
                    
           IF ACCT-FOUND
              PERFORM VARYING S1 FROM +1 BY +1 UNTIL
                 (AM-COM-TYP (S1) = 'C' OR 'D')
                 OR (S1 > +10)
              END-PERFORM
              IF S1 < +11
                 MOVE AM-COMM-CHARGEBACK (S1)
072312                                 TO WS-CHGBACK
              END-IF
           END-IF

          .
       0450-EXIT.
           EXIT.

       0460-STARTBR-ERACCT.

           MOVE LOW-VALUES             TO WS-ERACCT-KEY
           MOVE EN-CONTROL-PRIMARY     TO WS-ERACCT-KEY (1:22)
           MOVE WS-ERACCT-KEY          TO WS-SAVE-ERACCT-KEY

           EXEC CICS STARTBR                                            
               DATASET   ('ERACCT')
               RIDFLD    (WS-ERACCT-KEY)
               GTEQ
               RESP      (WS-RESPONSE)
           END-EXEC

           .
       0460-EXIT.
           EXIT.

       0470-READNEXT-ERACCT.

           EXEC CICS READNEXT
              INTO    (ACCOUNT-MASTER)
              DATASET ('ERACCT')
              RIDFLD  (WS-ERACCT-KEY)
              RESP    (WS-RESPONSE)
           END-EXEC

           .
       0470-EXIT.
           EXIT.

       0500-CREATE-TEMP-STORAGE.                          
                                                          
      *    display ' about to write pi ' program-interface-block
           EXEC CICS WRITEQ TS                            
               QUEUE (QID)                                
               FROM  (PROGRAM-INTERFACE-BLOCK)            
091615         LENGTH(1300)                     
           END-EXEC
                                                          
      *    display ' about to write map ' el631ki
           EXEC CICS WRITEQ TS                            
               QUEUE (QID)                                
               FROM  (EL631KI)                            
               LENGTH(MAP-LENGTH)                         
           END-EXEC.                                      
                                                          
       0500-EXIT.                                         
            EXIT.                                         
      
       0600-RECOVER-TEMP-STORAGE.

           EXEC CICS READQ TS
               QUEUE (QID)
               INTO  (PROGRAM-INTERFACE-BLOCK)
091615         LENGTH(1300)
           END-EXEC
                                                             
           EXEC CICS READQ TS                                
               QUEUE (QID)                                   
               INTO  (EL631KI)
               LENGTH(MAP-LENGTH)                            
           END-EXEC
                                                             
           PERFORM 0800-DELETE-TS THRU 0800-EXIT.            
                                                             
           MOVE AL-UANON            TO SIGREQA
      
           MOVE AL-UANON            TO LTRIDA
      
           MOVE AL-UANON            TO VOUCHA
      
           MOVE AL-UANON            TO HLTHAPPA
      
           MOVE AL-UANON            TO PAYEEA
      
           IF WS-SET-CODES-MDT = 'Y'
              move +1                  to s2
              perform varying s1 from +1 by +5 until s1 > +60
                 MOVE WS-PASSED-REASON-CODES (s1:4)
                                       TO reacd-in (s2)
012412           IF REACD-IN (S2) NOT EQUAL SPACES AND LOW-VALUES
012412               MOVE AL-SANON     TO REACDA (S2)
060612               MOVE +4           TO REACDL (S2)
012412           END-IF
                 add +1                to s2
              end-perform
062712        MOVE REASONCDS       TO PI-REASONS-ENTERED
           END-IF
      
060612     IF CERTIDI > SPACES
060612         MOVE AL-UANON       TO CERTIDA
060612     END-IF
121713
121713     IF ENCCODEI > SPACES
121713         MOVE AL-UANON       TO ENCCODEA
121713     END-IF
072312
072312     IF CRTNT1I > SPACES
072312         MOVE AL-UANON       TO CRTNT1A
072312     END-IF
072312
072312     IF CRTNT2I > SPACES
072312         MOVE AL-UANON       TO CRTNT2A
072312     END-IF
072312
072312     IF CRTNT3I > SPACES
072312         MOVE AL-UANON       TO CRTNT3A
072312     END-IF
072312
072312     IF CRTNT4I > SPACES
072312         MOVE AL-UANON       TO CRTNT4A
072312     END-IF
072312
121212     IF BILNT1I > SPACES
121212         MOVE AL-UANON       TO BILNT1A
072312     END-IF
072312
121212     IF BILNT2I > SPACES
121212         MOVE AL-UANON       TO BILNT2A
072312     END-IF
  
           .
       0600-EXIT.                                              
            EXIT.                                              
      
       0800-DELETE-TS.                    
                                          
           EXEC CICS DELETEQ TS           
               QUEUE(QID)                 
           END-EXEC
      
           .                                          
       0800-EXIT.                         
            EXIT.                         
      
       1000-BUILD-ERARCH.
      
           MOVE SPACES                 TO W-CNTL-KEY
           MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID
           MOVE '1'                    TO W-CNTL-RECORD-TYPE
           MOVE ZEROS                  TO W-CNTL-SEQ-NO
      
           EXEC CICS READ
                DATASET    (WS-ELCNTL-FILE-ID)
                SET        (ADDRESS OF CONTROL-FILE)
                RIDFLD     (W-CNTL-KEY)
                UPDATE
                RESP       (WS-RESPONSE)
           END-EXEC
      
           IF RESP-NORMAL
              IF CF-CREDIT-LAST-ARCH-NUM NOT NUMERIC
                 MOVE ZEROS            TO CF-CREDIT-LAST-ARCH-NUM
              END-IF
              ADD 1                    TO CF-CREDIT-LAST-ARCH-NUM
              MOVE CF-CREDIT-LAST-ARCH-NUM
                                       TO W-ARCH-NUMBER
              EXEC CICS REWRITE
                 FROM      (CONTROL-FILE)
                 DATASET   (WS-ELCNTL-FILE-ID)
              END-EXEC
           END-IF
      
           EXEC CICS GETMAIN
              SET      (ADDRESS OF LETTER-ARCHIVE)
              LENGTH   (W-ARCH-LENGTH)
           END-EXEC
      
           MOVE 'LA'                   TO LETTER-ARCHIVE
      
           MOVE W-ARCH-NUMBER          TO LA-ARCHIVE-NO
                                          LA-ARCHIVE-NO-A2
                                          LA-ARCHIVE-NO-A3
                                          LA-ARCHIVE-NO-A4
                                          LA-ARCHIVE-NO-A5
                                          LA-ARCHIVE-NO-A6
      
           MOVE PI-COMPANY-CD          TO LA-COMPANY-CD
                                          LA-COMPANY-CD-A2
                                          LA-COMPANY-CD-A3
                                          LA-COMPANY-CD-A4
                                          LA-COMPANY-CD-A5
                                          LA-COMPANY-CD-A6
           MOVE PB-CARRIER             TO LA-CARRIER-A2
                                          LA-CARRIER-A3
                                          LA-CARRIER-A4
                                          LA-CARRIER-A5
           MOVE Pb-cert-eff-dt         TO LA-EFFECT-DATE-A2
           MOVE PB-GROUPING            TO LA-GROUPING-A2
                                          LA-GROUPING-A3
                                          LA-GROUPING-A4
                                          LA-GROUPING-A5
           MOVE PB-ACCOUNT             TO LA-ACCOUNT-A2
                                          LA-ACCOUNT-A3
                                          LA-ACCOUNT-A4
                                          LA-ACCOUNT-A5
           MOVE PB-STATE               TO LA-STATE-A2
                                          LA-STATE-A3
                                          LA-STATE-A4
                                          LA-STATE-A5
           MOVE PB-CERT-PRIME          TO LA-CERT-PRIME-A2
           MOVE PB-CERT-SFX            TO LA-CERT-SUFFIX-A2
           MOVE PB-ENTRY-BATCH         TO LA-ENTRY-A6
      
           MOVE PI-PROCESSOR-ID        TO LA-PROCESSOR-CD
      
           MOVE LOW-VALUES             TO LA-LAST-RESENT-PRINT-DATE
                                          LA-INITIAL-PRINT-DATE
                                          LA-SENT-DATE
                                          LA-REPLY-DATE
                                          LA-RESEND-DATE
                                          LA-FOLLOW-UP-DATE
      
           MOVE 'A'                    TO LA-STATUS
           MOVE W-NUMBER-OF-COPIES     TO LA-NO-OF-COPIES
           MOVE W-AUTO-CLOSE-IND       TO LA-FINAL-ACT-IND
           MOVE LTRIDI                 TO LA-FORM-A3
           MOVE '4'                    TO LA-DATA-SOURCE
           MOVE WS-CURRENT-BIN-DT      TO LA-CREATION-DATE
      
           MOVE ZEROS                  TO LA-NUMBER-LABEL-LINES
                                          LA-NO-OF-TEXT-RECORDS
      
           EXEC CICS WRITE
                DATASET   ('ERARCH')
                FROM      (LETTER-ARCHIVE)
                RIDFLD    (LA-CONTROL-PRIMARY)
                RESP      (WS-RESPONSE)
           END-EXEC
      
           IF RESP-NORMAL
              MOVE ER-0280             TO EMI-ERROR
060612        MOVE -1                  TO SIGREQL
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              MOVE W-ARCH-NUMBER       TO W-ARCH-SUPPRESS
              MOVE W-ARCH-EDIT         TO EMI-TEXT-VARIABLE (1)
           ELSE
              MOVE ER-7345             TO EMI-ERROR
060612        MOVE -1                  TO SIGREQL
              PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
           END-IF
      
           .
       1000-EXIT.
           EXIT.
      
       1100-CALL-NS-BUS-LOGIC.

           MOVE SPACES                 TO BL-INPUT
           MOVE W-ARCH-NUMBER          TO BL-ARCHIVE-NO
120313     IF PI-RETURN-TO-PROGRAM EQUAL 'EL6314'
120313         MOVE '2'                TO BL-DATA-SRCE
120313         MOVE SPACES             TO BL-BATCH-NO
120313         MOVE ZEROS              TO BL-BATCH-SEQ
120313     ELSE
120313         MOVE '4'                TO BL-DATA-SRCE
120313         MOVE PB-ENTRY-BATCH     TO BL-BATCH-NO
120313         MOVE PB-BATCH-SEQ-NO    TO BL-BATCH-SEQ
120313     END-IF
120313     MOVE PI-CARRIER             TO BL-CARRIER
120313     MOVE PI-GROUPING            TO BL-GROUP
120313     MOVE PI-STATE               TO BL-STATE
120313     MOVE PI-ACCOUNT             TO BL-ACCOUNT
120313     IF PI-CERT-EFF-DT NOT = LOW-VALUES
120313        MOVE PI-CERT-EFF-DT      TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 9700-date-link
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-B-EDIT
                                       TO BL-EFF-DT
              END-IF
           END-IF
120313     MOVE PI-CERT-NO             TO BL-CERT-NO
           MOVE SPACES                 TO BL-RESP-NO
           MOVE LTRIDI                 TO BL-LETTER-ID
           MOVE W-NUMBER-OF-COPIES     TO BL-NO-OF-COPIES
           MOVE PI-PROCESSOR-ID        TO BL-PROC-ID
      *    MOVE 'ALWA'                 TO BL-PROC-ID
           MOVE PI-COMPANY-ID          TO BL-COMP-ID
      *    MOVE IFF-PRINT-NOW-SW       TO BL-PRINT-NOW-SW
      *    MOVE IFF-ENC-CD             TO BL-ENC-CD
121713     MOVE PI-ENCCODE             TO BL-ENC-CD
121112     IF EIBAID = DFHPF4
              MOVE 'B'                 TO BL-WRITE-ERARCH
           ELSE
              MOVE 'T'                 TO BL-WRITE-ERARCH
           END-IF
           MOVE CERTIDI                TO BL-CERT-FORM-ID
110612     MOVE W-ARCH-NUMBER          TO BL-ENDT-ARCH-NO           
102212     MOVE 'ISS ENDT'             TO BL-SOURCE-SCREEN

      
           PERFORM VARYING S1 FROM +1 BY +1 UNTIL S1 > +12
              IF REACD-IN (S1) NOT = LOW-VALUES
                 MOVE REACD-IN (S1)    TO BL-REASON-CODE (S1)
              END-IF
           END-PERFORM
      
      *****************************************
      * Invoke the LETTER business logic
      *****************************************
      
           exec cics link
              program('NSRASBL')
              commarea(srch-commarea)
           end-exec.

           .
       1100-EXIT.
           EXIT.
      
       1500-GET-ARCH-NO.

           MOVE SPACES                 TO ELCNTL-KEY
           MOVE PI-COMPANY-ID          TO CNTL-COMP-ID
           MOVE '1'                    TO CNTL-REC-TYPE
           MOVE ZEROS                  TO CNTL-SEQ
      
           EXEC CICS READ
                DATASET    (WS-ELCNTL-FILE-ID)
                SET        (ADDRESS OF CONTROL-FILE)
                RIDFLD     (ELCNTL-KEY)
                UPDATE
                RESP       (WS-RESPONSE)
           END-EXEC

           IF RESP-NORMAL
              IF CF-CREDIT-LAST-ARCH-NUM NOT NUMERIC
                 MOVE ZEROS            TO CF-CREDIT-LAST-ARCH-NUM
              END-IF
              ADD 1                    TO CF-CREDIT-LAST-ARCH-NUM
              MOVE CF-CREDIT-LAST-ARCH-NUM
                                       TO W-ARCH-NUMBER
              EXEC CICS REWRITE
                 FROM      (CONTROL-FILE)
                 DATASET   (WS-ELCNTL-FILE-ID)
              END-EXEC
           END-IF

           .
       1500-EXIT.
           EXIT.
072312           
072312     
072312 1700-WRITE-CERT-NOTE      SECTION.
103012
103012     MOVE SV-PARTIAL-KEY TO ERCNOT-PARTIAL-KEY.
103012     MOVE ZERO           TO ERCNOT-SEQ.
103012     SET TB-INDX         DOWN BY 1.
103012     SET TB-INDX1        TO TB-INDX.
103012
103012****DELETE CURRENT CERT NOTES
103012 1700-LOOP.
103012     EXEC CICS READ
103012         DATASET (WS-ERCNOT-FILE-ID)
103012         RIDFLD  (ERCNOT-KEY)
103012         SET     (ADDRESS OF CERT-NOTE-FILE)
103012         RESP    (WS-RESPONSE)
103012         GTEQ
103012     END-EXEC.
103012
103012     IF NOT RESP-NORMAL
103012        GO TO 1700-ENDDEL
103012     END-IF
103012
103012     MOVE CZ-CONTROL-PRIMARY     TO ERCNOT-KEY.
103012
103012     IF ERCNOT-PARTIAL-KEY NOT = SV-PARTIAL-KEY
103012         GO TO 1700-ENDDEL
103012     END-IF.
103012
103012     EXEC CICS DELETE
103012         DATASET (WS-ERCNOT-FILE-ID)
103012         RIDFLD  (ERCNOT-KEY)
103012     END-EXEC.
103012
103012     GO TO 1700-LOOP.
103012
103012 1700-ENDDEL.
103012
103012     EXEC CICS GETMAIN
103012          LENGTH(ERCNOT-LENGTH)
103012          SET(ADDRESS OF CERT-NOTE-FILE)
103012          INITIMG(GETMAIN-SPACE)
103012     END-EXEC.
103012
103012     SET TB-INDX TO 1.
103012     MOVE SV-PARTIAL-KEY TO ERCNOT-PARTIAL-KEY
103012     MOVE +0             TO ERCNOT-SEQ
103012
103012     PERFORM VARYING TB-INDX FROM 1 BY 1
103012             UNTIL TB-INDX > TB-INDX1
103012        MOVE SPACES                 TO  CERT-NOTE-FILE
103012        ADD 1                       TO  ERCNOT-SEQ
103012        MOVE ERCNOT-KEY             TO  CZ-CONTROL-PRIMARY
103012        MOVE  'CZ'                  TO  CZ-RECORD-ID
103012        MOVE CERT-NT-TEXT (TB-INDX) TO  CZ-NOTE
103012        MOVE CERT-NT-LAST-MAINT-BY (TB-INDX)
103012                                    TO  CZ-LAST-MAINT-USER
103012        MOVE CERT-NT-LAST-MAINT-HHMMSS (TB-INDX)
103012                                    TO  CZ-LAST-MAINT-HHMMSS
103012        MOVE CERT-NT-LAST-MAINT-DT (TB-INDX)
103012                                    TO  CZ-LAST-MAINT-DT
103012
103012        EXEC CICS WRITE 
103012             DATASET(WS-ERCNOT-FILE-ID)
103012             FROM(CERT-NOTE-FILE)
103012             RIDFLD(ERCNOT-KEY)
103012        END-EXEC
103012     END-PERFORM
072312
072312     PERFORM 1800-READ-ELCERT-UPDATE THRU 1800-EXIT
072312     IF RESP-NORMAL
072312        EVALUATE CM-NOTE-SW
072312           WHEN '1'
072312           WHEN '3'
072312           WHEN '5'
072312           WHEN '7'
072312              SET NO-CERT-RW     TO TRUE
072312           WHEN ' '
072312              MOVE '1'           TO CM-NOTE-SW
072312           WHEN '2'
072312              MOVE '3'           TO CM-NOTE-SW
072312           WHEN '4'
072312              MOVE '5'           TO CM-NOTE-SW
072312           WHEN '6'
072312              MOVE '7'           TO CM-NOTE-SW
072312        END-EVALUATE
072312     END-IF
072312     IF NOT NO-CERT-RW
072312        PERFORM 1810-REWRITE-ELCERT
072312                                 THRU 1810-EXIT
072312     ELSE
072312        EXEC CICS UNLOCK
072312           DATASET    (WS-ELCERT-FILE-ID)
072312        END-EXEC
072312     END-IF
072312
072312     .
072312 1799-EXIT.
072312     EXIT.
072312
072312 1800-READ-ELCERT-UPDATE.
072312
072312     MOVE PI-COMPANY-CD          TO  W-CERT-COMPANY-CD.
072312     MOVE PI-CRTO-CARRIER        TO  W-CERT-CARRIER.
072312     MOVE PI-CRTO-GROUPING       TO  W-CERT-GROUPING.
072312     MOVE PI-CRTO-STATE          TO  W-CERT-STATE.
072312     MOVE PI-CRTO-ACCOUNT        TO  W-CERT-ACCOUNT.
072312     MOVE PI-CRTO-CERT-EFF-DT    TO  W-CERT-CERT-EFF-DT.
072312     MOVE PI-CRTO-CERT-PRIME     TO  W-CERT-CERT-PRIME.
072312     MOVE PI-CRTO-CERT-SFX       TO  W-CERT-CERT-SFX.
072312
072312     EXEC CICS READ
072312         UPDATE
072312         DATASET  (WS-ELCERT-FILE-ID)
072312         RIDFLD   (WS-ELCERT-KEY)
072312         SET      (ADDRESS OF CERTIFICATE-MASTER)
072312         RESP     (WS-RESPONSE)
072312     END-EXEC
072312
072312     .
072312 1800-EXIT.
072312     EXIT.
072312
072312 1810-REWRITE-ELCERT.
072312
072312     EXEC CICS REWRITE
072312         FROM     (CERTIFICATE-MASTER)
072312         DATASET  (WS-ELCERT-FILE-ID)
072312         RESP     (WS-RESPONSE)
072312     END-EXEC.
072312
072312     .
072312 1810-EXIT.
072312     EXIT.
072312

       8100-SEND-INITIAL-MAP.
       
           MOVE EIBDATE                TO DC-JULIAN-YYDDD.
           MOVE '5'                    TO DC-OPTION-CODE.
           PERFORM 9700-DATE-LINK.
           MOVE DC-GREG-DATE-1-EDIT    TO DATEO.
           MOVE EIBTIME                TO TIME-IN.
           MOVE TIME-OUT               TO TIMEO.
060612     MOVE -1                     TO SIGREQL
      
012412     IF PI-PROCESSOR-IS-CSR-SUPER
              perform varying s1 from +1 by +1 until
                 s1 > +12
                 MOVE AL-UANON            TO reacda (s1)
              end-perform
012412     END-IF              
      
           MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.
           MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.
           EXEC CICS SEND
               MAP      (MAP-NAME)
               MAPSET   (MAPSET-NAME)
               FROM     (EL631KO)
               ERASE
               CURSOR
           END-EXEC.
      
           GO TO 9100-RETURN-TRAN.
      
       8200-SEND-DATAONLY.

           MOVE EIBDATE                TO DC-JULIAN-YYDDD.
           MOVE '5'                    TO DC-OPTION-CODE.
           PERFORM 9700-DATE-LINK.
           MOVE DC-GREG-DATE-1-EDIT    TO DATEO.
           MOVE EIBTIME                TO TIME-IN.
           MOVE TIME-OUT               TO TIMEO.
           MOVE -1                     TO PFENTRL
           MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O
           MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O
           EXEC CICS SEND
               MAP      (MAP-NAME)
               MAPSET   (MAPSET-NAME)
               FROM     (EL631KO)
               DATAONLY
               ERASEAUP
               CURSOR
           END-EXEC.
      
           GO TO 9100-RETURN-TRAN.
      
       8300-SEND-TEXT.
           EXEC CICS SEND TEXT
               FROM     (LOGOFF-TEXT)
               LENGTH   (LOGOFF-LENGTH)
               ERASE
               FREEKB
           END-EXEC.
      
           EXEC CICS RETURN
           END-EXEC.

       8800-UNAUTHORIZED-ACCESS.
           MOVE UNACCESS-MSG           TO LOGOFF-MSG.
           GO TO 8300-SEND-TEXT.
      
       8810-PF23.
           MOVE EIBAID                 TO PI-ENTRY-CD-1.
           MOVE XCTL-005               TO PGM-NAME.
           GO TO 9300-XCTL.
       9000-RETURN-CICS.
           EXEC CICS RETURN
           END-EXEC.
      
       9100-RETURN-TRAN.
           MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
           MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO.

           EXEC CICS RETURN
               TRANSID    (TRANS-ID)
               COMMAREA   (PROGRAM-INTERFACE-BLOCK)
091615         LENGTH     (1300)
      *        LENGTH     (PI-COMM-LENGTH)
           END-EXEC.
      
       9200-RETURN-MAIN-MENU.
           MOVE XCTL-626               TO PGM-NAME.
           GO TO 9300-XCTL.
      
       9300-XCTL.
           EXEC CICS XCTL
               PROGRAM    (PGM-NAME)
               COMMAREA   (PROGRAM-INTERFACE-BLOCK)
               LENGTH     (PI-COMM-LENGTH)
           END-EXEC.
      
       9400-CLEAR.

           MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.
      
           EXEC CICS XCTL
               PROGRAM    (PGM-NAME)
               COMMAREA   (PROGRAM-INTERFACE-BLOCK)
               LENGTH     (PI-COMM-LENGTH)
           END-EXEC.
      
       9500-PF12.
           MOVE XCTL-010               TO PGM-NAME.
           GO TO 9300-XCTL.
      
       9600-PGMID-ERROR.
           EXEC CICS HANDLE CONDITION
               PGMIDERR    (8300-SEND-TEXT)
           END-EXEC.
      
           MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
           MOVE ' '                    TO PI-ENTRY-CD-1.
           MOVE XCTL-005               TO PGM-NAME.
           MOVE PGM-NAME               TO LOGOFF-PGM.
           MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
           GO TO 9300-XCTL.
      
       9700-DATE-LINK.
           MOVE LINK-ELDATCV           TO PGM-NAME.
      
           EXEC CICS LINK
               PROGRAM    (PGM-NAME)
               COMMAREA   (DATE-CONVERSION-DATA)
               LENGTH     (DC-COMM-LENGTH)
           END-EXEC.
      
       9900-ERROR-FORMAT.
           IF NOT EMI-ERRORS-COMPLETE
              MOVE LINK-001            TO PGM-NAME
              EXEC CICS LINK
                  PROGRAM    (PGM-NAME)
                  COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
                  LENGTH     (EMI-COMM-LENGTH)
              END-EXEC
           END-IF
      
           .
       9900-EXIT.
           EXIT.
      
       9990-ABEND.
           MOVE LINK-004               TO PGM-NAME.
           MOVE DFHEIBLK               TO EMI-LINE1.
           EXEC CICS LINK
               PROGRAM   (PGM-NAME)
               COMMAREA  (EMI-LINE1)
               LENGTH    (72)
           END-EXEC.
      
           GO TO 8200-SEND-DATAONLY.
      
           GOBACK.
      
       9995-SECURITY-VIOLATION.
                                   COPY ELCSCTP.
      
       9995-EXIT.
           EXIT.
      
