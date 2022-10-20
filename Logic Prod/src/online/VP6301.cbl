       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 VP6301.
      *                            VMOD=2.075
      *
      *AUTHOR.     LOGIC,INC.
      *            DALLAS, TEXAS.
      *DATE-COMPILED.
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CSO.            *
      *            *                                                   *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF    CSO      IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
      *            *                                                   *
      *            *****************************************************
      *REMARKS.
      *         TRANSACTION - VPA6 - NEW BUSINESS - DATA ENTRY (ISSUES).
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
      * 100703    2003080800002  PEMA  ADD SUPER GAP PROCESSING
      * 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
      * 081606  CR2006080800002  PEMA  ADD VIN NUMBER
      * 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
      * 072308  CR2008040800002  PEMA  ADD CRED BENE INFORMATION
      * 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
      * 030309  CR2009021700001  PEMA  ADD EDIT FOR BENE AND INS ADDR
      * 020210  IR2010011100002  PEMA  CORRECT ATTRB ON BCZIPCD
      * 030310  IR2010022400001  PEMA  CORRECT PF5 LOGIC, ADD APR PROC
      * 030310  CR2009031200002  PEMA  CHECK LOAN OFF EDIT SWITCH
      * 060211  CR2011051600002  PEMA  OPEN CP FIELD
      * 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
      * 050713  CR2008042200001  PEMA  ADD ZERO PCT APR PROCESSING
      * 111913  CR2008042200001  PEMA  ADDITIONAL 0 % APR CHANGES
      * 020514  IR2014012400001  PEMA  DARK OUT AH ALT BEN FOR CID&AHL
      * 042114  CR2014032000001  PEMA  rearrange dob,jntdob&ssn
      * 071714    2013100100002  PEMA  FIX CRIT PERIOD EDITS
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
062017* 062017  CR2015091000001  PEMA  ADD PROCESSING FOR TN REF INTERES
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
       77  FILLER  PIC X(32)  VALUE '********************************'.
       77  FILLER  PIC X(32)  VALUE '*    VP6301 WORKING STORAGE    *'.
       77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.075 *********'.
       77  WS-BEG                      PIC S999 COMP-3 VALUE +0.
       77  WS-END                      PIC S999 COMP-3 VALUE +0.
      *    COPY ELCSCTM.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCSCTM                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY MESSAGE AREA     *
00007 *                                                                *
00008 ******************************************************************
00009  01  SECURITY-MESSAGE.
00010      12  FILLER                          PIC X(30)
00011             VALUE '** LOGIC SECURITY VIOLATION -'.
00012      12  SM-READ                         PIC X(6).
00013      12  FILLER                          PIC X(5)
00014             VALUE ' PGM='.
00015      12  SM-PGM                          PIC X(6).
00016      12  FILLER                          PIC X(5)
00017             VALUE ' OPR='.
00018      12  SM-PROCESSOR-ID                 PIC X(4).
00019      12  FILLER                          PIC X(6)
00020             VALUE ' TERM='.
00021      12  SM-TERMID                       PIC X(4).
00022      12  FILLER                          PIC XX   VALUE SPACE.
00023      12  SM-JUL-DATE                     PIC 9(5).
00024      12  FILLER                          PIC X    VALUE SPACE.
00025      12  SM-TIME                         PIC 99.99.
00026
      *    COPY ELCSCRTY.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCSCRTY                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY DATA AREA        *
00007 *        AREA ACQUIRED BY SIGN ON PROGRAM EL125 AND ADDRESS      *
00008 *        SAVED IN PI-SECURITY-ADDRESS.                           *
00009 *                                                                *
00010 ******************************************************************
00011  01  SECURITY-CONTROL.
00012      12  SC-COMM-LENGTH               PIC S9(4) VALUE +144 COMP.
00013      12  FILLER                       PIC XX    VALUE 'SC'.
00014      12  SC-CREDIT-CODES.
00015          16  SC-CREDIT-AUTHORIZATION OCCURS 40 TIMES.
00016              20  SC-CREDIT-DISPLAY    PIC X.
00017              20  SC-CREDIT-UPDATE     PIC X.
00018      12  SC-CLAIMS-CODES.
00019          16  SC-CLAIMS-AUTHORIZATION OCCURS 30 TIMES.
00020              20  SC-CLAIMS-DISPLAY    PIC X.
00021              20  SC-CLAIMS-UPDATE     PIC X.
       01  WS-NUM-TABLE                PIC X(26)  VALUE
           '12345678012345070923456789'.
       01  FILLER REDEFINES WS-NUM-TABLE.
           05  WS-NUM OCCURS 26        PIC 9.
       01  V1                          PIC S999 COMP-3.
       01  V2                          PIC S999 COMP-3.
       01  V3                          PIC S999 COMP-3.
       01  WS-WORK-VIN                 PIC X(17)  VALUE SPACES.
       01  FILLER REDEFINES WS-WORK-VIN.
           05  WS-WORK-VIN-N OCCURS 17        PIC 9.
       01  WS-VIN-TOTAL                PIC S9(9)  VALUE +0.
       01  WS-VIN-FINAL                PIC S9(7)  VALUE +0.
       01  WS-VIN-REMAINDER            PIC S999   VALUE +0.
       01  WS-HEX-WORK.
           05  FILLER                  PIC X  VALUE LOW-VALUES.
           05  WS-HEX-BYTE             PIC X.
       01  WS-CHARCD REDEFINES WS-HEX-WORK PIC S9(4)  COMP.
       01  WS-CHARCD-A                 PIC S9(4)   COMP VALUE +65.
       01  WS-COMM-LENGTH          PIC S9(4) COMP VALUE +1900.
       01  STANDARD-AREAS.
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL              VALUE +00.
               88  RESP-ERROR               VALUE +01.
               88  RESP-NOTFND              VALUE +13.
               88  RESP-NOTOPEN             VALUE +19.
               88  RESP-ENDFILE             VALUE +20.
           12  GETMAIN-SPACE       PIC X       VALUE SPACE.
           12  VP630B              PIC X(8)    VALUE 'VP630B'.
           12  VP630C              PIC X(8)    VALUE 'VP630C'.
           12  MAPSET-VP6301S      PIC X(8)    VALUE 'VP6301S'.
           12  TRANS-EXA6          PIC X(4)    VALUE 'VPA6'.
           12  THIS-PGM            PIC X(8)    VALUE 'VP6301'.
           12  PGM-NAME            PIC X(8).
           12  TIME-IN             PIC S9(7).
           12  TIME-OUT-R  REDEFINES TIME-IN.
               16  FILLER          PIC X.
               16  TIME-OUT        PIC 99V99.
               16  FILLER          PIC XX.
           12  LINK-EL001          PIC X(8)    VALUE 'EL001'.
           12  LINK-EL004          PIC X(8)    VALUE 'EL004'.
           12  XCTL-EL005          PIC X(8)    VALUE 'EL005'.
           12  XCTL-EL010          PIC X(8)    VALUE 'EL010'.
           12  XCTL-EL626          PIC X(8)    VALUE 'EL626'.
           12  XCTL-EL630          PIC X(8)    VALUE 'EL630'.
           12  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.
           12  FILE-ID-ERPNDB      PIC X(8)    VALUE 'ERPNDB'.
           12  FILE-ID-ERPNDM      PIC X(8)    VALUE 'ERPNDM'.
           12  FILE-ID-ELCERT      PIC X(8)    VALUE 'ELCERT'.
           12  FILE-ID-ELCNTL      PIC X(8)    VALUE 'ELCNTL'.
           12  WS-CURRENT-DT       PIC X(8)    VALUE SPACES.
           12  WS-CURRENT-BIN-DT   PIC XX      VALUE SPACES.
           12  WS-TERM-IN-DAYS-SW  PIC X.
               88  WS-TERM-IN-DAYS-FOUND       VALUE 'Y'.
           EJECT
       01  ERROR-MESSAGES.
           12  ER-0000                 PIC X(4)  VALUE '0000'.
           12  ER-0004                 PIC X(4)  VALUE '0004'.
           12  ER-0008                 PIC X(4)  VALUE '0008'.
           12  ER-0029                 PIC X(4)  VALUE '0029'.
           12  ER-0070                 PIC X(4)  VALUE '0070'.
           12  ER-0582                 PIC X(4)  VALUE '0582'.
           12  ER-1923                 PIC X(4)  VALUE '1923'.
           12  ER-2049                 PIC X(4)  VALUE '2049'.
           12  ER-2119                 PIC X(4)  VALUE '2119'.
           12  ER-2212                 PIC X(4)  VALUE '2212'.
           12  ER-2217                 PIC X(4)  VALUE '2217'.
           12  ER-2218                 PIC X(4)  VALUE '2218'.
           12  ER-2200                 PIC X(4)  VALUE '2200'.
           12  ER-2209                 PIC X(4)  VALUE '2209'.
           12  ER-2220                 PIC X(4)  VALUE '2220'.
           12  ER-2222                 PIC X(4)  VALUE '2222'.
           12  ER-2223                 PIC X(4)  VALUE '2223'.
           12  ER-2224                 PIC X(4)  VALUE '2224'.
           12  ER-2226                 PIC X(4)  VALUE '2226'.
           12  ER-2227                 PIC X(4)  VALUE '2227'.
           12  ER-2228                 PIC X(4)  VALUE '2228'.
           12  ER-2240                 PIC X(4)  VALUE '2240'.
           12  ER-2241                 PIC X(4)  VALUE '2241'.
           12  ER-2247                 PIC X(4)  VALUE '2247'.
           12  ER-2423                 PIC X(4)  VALUE '2423'.
           12  ER-2424                 PIC X(4)  VALUE '2424'.
           12  ER-2425                 PIC X(4)  VALUE '2425'.
           12  ER-2426                 PIC X(4)  VALUE '2426'.
           12  ER-2427                 PIC X(4)  VALUE '2427'.
           12  ER-2428                 PIC X(4)  VALUE '2428'.
           12  ER-2431                 PIC X(4)  VALUE '2431'.
           12  ER-2433                 PIC X(4)  VALUE '2433'.
           12  ER-2437                 PIC X(4)  VALUE '2437'.
           12  ER-2429                 PIC X(4)  VALUE '2429'.
           12  ER-2442                 PIC X(4)  VALUE '2442'.
           12  ER-2471                 PIC X(4)  VALUE '2471'.
           12  ER-2526                 PIC X(4)  VALUE '2526'.
           12  ER-2529                 PIC X(4)  VALUE '2529'.
           12  ER-2531                 PIC X(4)  VALUE '2531'.
           12  ER-2532                 PIC X(4)  VALUE '2532'.
           12  ER-2541                 PIC X(4)  VALUE '2541'.
           12  ER-2542                 PIC X(4)  VALUE '2542'.
           12  ER-2589                 PIC X(4)  VALUE '2589'.
           12  ER-2591                 PIC X(4)  VALUE '2591'.
           12  ER-2592                 PIC X(4)  VALUE '2592'.
           12  ER-2593                 PIC X(4)  VALUE '2593'.
           12  ER-2594                 PIC X(4)  VALUE '2594'.
           12  ER-2629                 PIC X(4)  VALUE '2629'.
           12  ER-2630                 PIC X(4)  VALUE '2630'.
           12  ER-2635                 PIC X(4)  VALUE '2635'.
           12  ER-2636                 PIC X(4)  VALUE '2636'.
           12  ER-2651                 PIC X(4)  VALUE '2651'.
           12  ER-2670                 PIC X(4)  VALUE '2670'.
           12  ER-2683                 PIC X(4)  VALUE '2683'.
           12  ER-2700                 PIC X(4)  VALUE '2700'.
           12  ER-2701                 PIC X(4)  VALUE '2701'.
           12  ER-2702                 PIC X(4)  VALUE '2702'.
           12  ER-2901                 PIC X(4)  VALUE '2901'.
           12  ER-2963                 PIC X(4)  VALUE '2963'.
           12  ER-2964                 PIC X(4)  VALUE '2964'.
           12  ER-3166                 PIC X(4)  VALUE '3166'.
           12  ER-3825                 PIC X(4)  VALUE '3825'.
           12  ER-3826                 PIC X(4)  VALUE '3826'.
           12  ER-7400                 PIC X(4)  VALUE '7400'.
           12  ER-7403                 PIC X(4)  VALUE '7403'.
           12  ER-7404                 PIC X(4)  VALUE '7404'.
           12  ER-7405                 PIC X(4)  VALUE '7405'.
           12  ER-7423                 PIC X(4)  VALUE '7423'.
           12  ER-7424                 PIC X(4)  VALUE '7424'.
           12  ER-7530                 PIC X(4)  VALUE '7530'.
           12  ER-7632                 PIC X(4)  VALUE '7632'.
           12  ER-7630                 PIC X(4)  VALUE '7630'.
           12  ER-7631                 PIC X(4)  VALUE '7631'.
           12  ER-7633                 PIC X(4)  VALUE '7633'.
           12  ER-7997                 PIC X(4)  VALUE '7997'.
           12  ER-7998                 PIC X(4)  VALUE '7998'.
           12  ER-9841                 PIC X(4)  VALUE '9841'.
           12  ER-9999                 PIC X(4)  VALUE '9999'.
           EJECT
       01  ACCESS-KEYS.
           12  ERPNDB-KEY.
               16  ERPNDB-COMP-CD          PIC X     VALUE SPACE.
               16  ERPNDB-ENTRY-BATCH      PIC X(6)  VALUE SPACES.
               16  ERPNDB-BATCH-SEQ        PIC S9(4) VALUE +1 COMP.
               16  ERPNDB-BATCH-CHG-SEQ    PIC S9(4) VALUE +0 COMP.
           12  ERPNDB-RECORD-LENGTH        PIC S9(4) COMP VALUE +585.
           12  ERPNDB-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +608.
           12  ELCNTL-KEY.
               16  ELCNTL-COMPANY-ID       PIC X(3)  VALUE SPACES.
               16  ELCNTL-REC-TYPE         PIC X     VALUE SPACES.
               16  ELCNTL-ACCESS.
                   20  FILLER              PIC XX.
                   20  ELCNTL-HI-BEN       PIC XX.
               16  ELCNTL-SEQ              PIC S9(4) VALUE +0 COMP.
           12  ELCNTL-RECORD-LENGTH        PIC S9(4) COMP VALUE +504.
           12  ELCNTL-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +527.
           12  ELCERT-KEY.
               16  ELCERT-COMPANY-CD       PIC X.
               16  ELCERT-CARRIER          PIC X.
               16  ELCERT-GROUPING         PIC X(6).
               16  ELCERT-STATE            PIC XX.
               16  ELCERT-ACCOUNT          PIC X(10).
               16  ELCERT-CERT-EFF-DT      PIC XX.
               16  ELCERT-CERT-NO.
                   20  ELCERT-CERT-PRIME   PIC X(10).
                   20  ELCERT-CERT-SFX     PIC X.
           12  ELCRTT-KEY.
               16  ELCRTT-COMPANY-CD       PIC X.
               16  ELCRTT-CARRIER          PIC X.
               16  ELCRTT-GROUPING         PIC X(6).
               16  ELCRTT-STATE            PIC XX.
               16  ELCRTT-ACCOUNT          PIC X(10).
               16  ELCRTT-CERT-EFF-DT      PIC XX.
               16  ELCRTT-CERT-NO.
                   20  ELCRTT-CERT-PRIME   PIC X(10).
                   20  ELCRTT-CERT-SFX     PIC X.
               16  ELCRTT-TRLR-TYPE        PIC X.
           12  ELCRTT-RECORD-LENGTH        PIC S9(4) COMP VALUE +552.
           12  ELCERT-RECORD-LENGTH        PIC S9(4) COMP VALUE +450.
           12  ELCERT-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +473.
           12  ERPNDM-KEY.
               16  ERPNDM-COMP-CD          PIC X     VALUE SPACE.
               16  ERPNDM-ENTRY-BATCH      PIC X(6)  VALUE SPACES.
               16  ERPNDM-BATCH-SEQ        PIC S9(4) VALUE +1 COMP.
               16  ERPNDM-BATCH-CHG-SEQ    PIC S9(4) VALUE +0 COMP.
      *    12  ERPNDM-RECORD-LENGTH        PIC S9(4) COMP VALUE +250.
           12  ERPNDM-RECORD-LENGTH        PIC S9(4) COMP VALUE +374.
      *    12  ERPNDM-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +273.
           12  ERPNDM-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +397.
           EJECT
       01  WORK-AREA.
           12  DEEDIT-FIELD            PIC X(15).
           12  FILLER REDEFINES DEEDIT-FIELD.
               16  FILLER              PIC X(4).
               16  DEEDIT-FIELD-X11    PIC X(11).
           12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD PIC S9(15).
           12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD PIC S9(13)V99.
           12  DEEDIT-FIELD-V3 REDEFINES DEEDIT-FIELD PIC S9(12)V9(3).
           12  DEEDIT-FIELD-V4 REDEFINES DEEDIT-FIELD PIC S9(11)V9(4).
           12  DEEDIT-FIELD-V5 REDEFINES DEEDIT-FIELD PIC S9(10)V9(5).
           12  WS-SUB                  PIC S9(4) VALUE +0  COMP.
           12  WS-SUB1                 PIC S9(4) VALUE +0  COMP.
           12  WS-SUB2                 PIC S9(4) VALUE +0  COMP.
           12  WS-SUB3                 PIC S9(4) VALUE +0  COMP.
           12  WS-ACCT-SUB             PIC S9(4) VALUE +0  COMP.
           12  WS-COV-SUB              PIC S9(4) VALUE +0  COMP.
           12  WS-EDIT-SUB             PIC S9(4) VALUE +0  COMP.
           12  CENTURY-ADJ             PIC S9(08) VALUE +38400 COMP.
           12  WS-WORK-BIN-RED         PIC S9(08) VALUE +0 COMP.
           12  FILLER REDEFINES WS-WORK-BIN-RED.
               16  FILLER              PIC X(02).
               16  WS-WORK-BIN-DT      PIC X(02).
           12  WS-CALC-TERM            PIC S999V9(5) VALUE ZEROS.
           12  WS-CALC-TERM-R REDEFINES WS-CALC-TERM.
               16  WS-CALC-TERM-WHOLE  PIC S999.
               16  WS-CALC-TERM-REMAIN PIC SV9(5).
           12  ERROR-SW                PIC X     VALUE SPACE.
               88  NO-ERROR                VALUE SPACE.
               88  ERRORS                  VALUE 'Y'.
               88  WS-COVERAGE-PRESENT     VALUE 'Y'.
           12  WS-DATA-KEYED-SW        PIC X     VALUE SPACE.
               88  WS-DATA-NOT-KEYED       VALUE SPACE.
               88  WS-DATA-KEYED           VALUE 'Y'.
           12  WS-EDITED-LF-CODE       PIC XX   VALUE SPACES.
           12  WS-LF-ABBR-DESC         PIC XXX  VALUE SPACES.
           12  ws-lf-earnings-calc     pic x    value spaces.
           12  WS-EDITED-AH-CODE       PIC XX   VALUE ZEROS.
           12  WS-AH-ABBR-DESC         PIC XXX  VALUE SPACES.
           12  WS-BEN-CD               PIC XX   VALUE SPACES.
           12  WS-ENTRY-CODE           PIC X     VALUE SPACE.
               88  WS-ENTRY-CODE-VALID   VALUE ' ' 'E' 'R' 'P'
                                               'M' 'D' 'V' 'U'.
           12  WS-FORCE-CODE           PIC X     VALUE SPACE.
               88  WS-FORCE-CODE-VALID   VALUE ' ' 'A' 'D'.
           12  WS-ALL-NINES            PIC S9(7)V99 VALUE +9999999.99.
           12  WS-MODE-CODE            PIC X     VALUE SPACE.
               88 WS-MODE-CODE-VALID     VALUE ' ' 'M' 'W' 'S' 'B' 'T'.
           12  WS-SKIP-CODE            PIC X     VALUE SPACE.
               88 WS-SKIP-CODE-VALID     VALUE ' ' 'A' 'X' '0' THRU '9'.
           12  WS-KIND                 PIC XX    VALUE SPACE.
               88 WS-KIND-LF             VALUE 'LF'.
               88 WS-KIND-AH             VALUE 'AH'.
               88 WS-KIND-PR             VALUE 'PR'.
               88 WS-KIND-UE             VALUE 'UE'.
               88 WS-KIND-DI             VALUE 'DI'.
               88 WS-KIND-MONTHLY        VALUE 'AH' 'UE'.
           12  WS-JOURNAL-RECORD-LENGTH   PIC S9(4) COMP VALUE +0000.
           12  WS-EDIT-CODE               PIC X(4)  VALUE SPACES.
           12  WS-SAVE-INPUT-FIELDS.
               16  WS-BAGE                 PIC 99       VALUE ZERO.
               16  WS-BJNT-AGE             PIC 99       VALUE ZERO.
               16  WS-BDAYS                PIC 999      VALUE ZERO.
               16  WS-BLN-TERM             PIC 999      VALUE ZERO.
               16  WS-BFREQ                PIC 99       VALUE ZERO.
               16  WS-BPHONE               PIC 9(12)    VALUE  0 COMP-3.
               16  WS-BAPR                 PIC 99V9(4) VALUE ZEROS.
               16  FILLER REDEFINES WS-BAPR.
                   20  WS-APR-WHOLE-NUM    PIC 99.
                   20  WS-APR-DEC          PIC 9999.
               16  WS-BPMT                 PIC S9(6)V99 VALUE +0 COMP-3.
               16  WS-BPMTS                PIC S999     VALUE +0 COMP-3.
               16  WS-BLIVES               PIC 9(7)      COMP-3.
               16  WS-B-COVERAGE.
                   20  WS-BTERM1            PIC 999       COMP-3.
                   20  WS-BBEN1             PIC S9(10)V99 COMP-3.
                   20  WS-BALT-BEN1     PIC S9(10)V99 COMP-3.
                   20  WS-BPREM1        PIC S9(10)V99 COMP-3.
                   20  WS-BALT-PREM1    PIC S9(7)V99 COMP-3.
                   20  WS-BTERM2            PIC 999       COMP-3.
                   20  WS-BCRIT-PERD2       PIC 99        COMP-3.
                   20  WS-BBEN2             PIC S9(10)V99 COMP-3.
                   20  WS-BPREM2        PIC S9(10)V99 COMP-3.
                   20  WS-BALT-BEN2     PIC S9(10)V99 COMP-3.
                   20  WS-BALT-PREM2    PIC S9(7)V99 COMP-3.
               16  WS-C-FIELDS   OCCURS 4 TIMES.
                   20  WS-CLIVES      PIC 9(3)          COMP-3.
                   20  WS-CREFUND1    PIC S9(7)V99      COMP-3.
                   20  WS-CREFUND2    PIC S9(7)V99      COMP-3.
                   20  WS-CAN-REA     PIC X.
           12  WS-CONVERTED-BIRTH      OCCURS 2 PIC XX.
           12  WS-CONVERTED-EFFDT      PIC XX    VALUE SPACES.
           12  WS-CONVERTED-1ST-PMT-DT PIC XX    VALUE SPACES.
           12  WS-CONVERTED-EXPIRDT      OCCURS 2 TIMES PIC XX.
           12  WS-CONVERTED-CANCEL-DATES OCCURS 4 TIMES.
               16  WS-CONVERTED-CANDT1 PIC XX.
               16  WS-CONVERTED-CANDT2 PIC XX.
           12  WS-CONVERTED-CAN-EFF-DATES OCCURS 4 TIMES.
               16  WS-CONVERTED-CAN-EFF-DT PIC XX.
           12  WS-FIRST-NAME.
               16  WS-1ST-INIT         PIC X.
               16  FILLER              PIC X(9).
           12  WS-INITIALS.
               16  WS-INITIAL-1        PIC X.
               16  WS-INITIAL-2        PIC X.
           12  WS-ZIP-CODE.
               16  WS-ZIP-1            PIC X.
                   88  WS-CANADIAN-ZIP    VALUE 'A' THRU 'Z'.
               16  WS-ZIP-2-3          PIC XX.
               16  WS-ZIP-4            PIC X.
               16  WS-ZIP-5            PIC X.
               16  WS-ZIP-6            PIC X.
               16  FILLER              PIC X(4).
           12  WS-ZIP-AM-1  REDEFINES  WS-ZIP-CODE.
               16  WS-ZIP-AM-1-CODE    PIC X(5).
               16  WS-ZIP-AM-1-PLUS4   PIC X(4).
               16  FILLER              PIC X.
           12  WS-ZIP-AM-2  REDEFINES  WS-ZIP-CODE.
               16  WS-ZIP-AM-2-CODE    PIC X(5).
               16  WS-ZIP-AM-2-DASH    PIC X.
               16  WS-ZIP-AM-2-PLUS4   PIC X(4).
           12  WS-ZIP-CAN-1  REDEFINES  WS-ZIP-CODE.
               16  WS-ZIP-CAN-1-POST1  PIC XXX.
               16  WS-ZIP-CAN-1-POST2  PIC XXX.
               16  FILLER              PIC X(4).
           12  WS-ZIP-CAN-2  REDEFINES  WS-ZIP-CODE.
               16  WS-ZIP-CAN-2-POST1  PIC XXX.
               16  FILLER              PIC X.
               16  WS-ZIP-CAN-2-POST2  PIC XXX.
               16  FILLER              PIC XXX.
           12  WS-MEMBER-NO            PIC X(12).
           12  FILLER  REDEFINES  WS-MEMBER-NO.
               16  WS-MEMBER-NO-1-8    PIC 9(8).
               16  FILLER              PIC X(4).
           12  WS-I-MICRO-NO           PIC S9(9)        COMP-3.
           EJECT
       01  WS-DATE-AREA.
           12  WS-COMPARE-CURRENT-DT.
               16  FILLER              PIC X(4)    VALUE SPACES.
               16  WS-COMPARE-CURR-YR  PIC X(2)    VALUE SPACES.
           12  WS-SAVE-BIRTH-DATE.
               16  FILLER              PIC X(4)   VALUE SPACES.
               16  WS-SAVE-BIRTH-YR    PIC X(2)   VALUE SPACES.
       01  CLASIC-WARNING.
           12  WARNING-LENGTH              PIC S9(4)  VALUE +124 COMP.
           12  WARNING-TEXT.
               16  FILLER                  PIC X(80)  VALUE
                   'THIS DATA MAY HAVE PREVIOUSLY BEEN PROCESSED'.
               16  FILLER                  PIC X(44)  VALUE
                   'CONTACT PAUL @ CSO FOR FURTHER INFORMATION'.
      *                                COPY ELCDATE.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCDATE.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003
00006 *                                                                *
00007 *                                                                *
00008 *   DESCRIPTION:  DATA PASSED TO DATE CONVERSION ROUTINE.        *
00009 *                 LENGTH = 200                                   *
00010 ******************************************************************
00011
00012  01  DATE-CONVERSION-DATA.
00013      12  DC-COMM-LENGTH                PIC S9(4) COMP VALUE +200.
00014      12  DC-OPTION-CODE                PIC X.
00015          88  BIN-TO-GREG                VALUE ' '.
00016          88  ELAPSED-BETWEEN-BIN        VALUE '1'.
00017          88  EDIT-GREG-TO-BIN           VALUE '2'.
00018          88  YMD-GREG-TO-BIN            VALUE '3'.
00019          88  MDY-GREG-TO-BIN            VALUE '4'.
00020          88  JULIAN-TO-BIN              VALUE '5'.
00021          88  BIN-PLUS-ELAPSED           VALUE '6'.
00022          88  FIND-CENTURY               VALUE '7'.
00023          88  ELAPSED-BETWEEN-BIN-3      VALUE '8'.
00024          88  EDIT-GREG-TO-BIN-3         VALUE '9'.
00025          88  YMD-GREG-TO-BIN-3          VALUE 'A'.
00026          88  MDY-GREG-TO-BIN-3          VALUE 'B'.
00027          88  JULIAN-TO-BIN-3            VALUE 'C'.
00028          88  BIN-PLUS-ELAPSED-3         VALUE 'D'.
00029          88  JULIAN-EXPANDED-TO-BIN     VALUE 'E'.
00030          88  JULIAN-EXPANDED-TO-BIN-3   VALUE 'F'.
00031          88  BIN-TO-JULIAN-EXPANDED     VALUE 'G'.
00032          88  JULIAN-EXPANDED            VALUE 'E', 'F', 'G'.
00033          88  CHECK-LEAP-YEAR            VALUE 'H'.
00034          88  BIN-3-TO-GREG              VALUE 'I'.
00035          88  CYMD-GREG-TO-BIN-3         VALUE 'J'.
00036          88  MDCY-GREG-TO-BIN-3         VALUE 'K'.
00037          88  CYMD-GREG-TO-BIN           VALUE 'L'.
00038          88  MDCY-GREG-TO-BIN           VALUE 'M'.
00039          88  MDY-GREG-TO-JULIAN         VALUE 'N'.
00040          88  MDCY-GREG-TO-JULIAN        VALUE 'O'.
00041          88  YMD-GREG-TO-JULIAN         VALUE 'P'.
00042          88  CYMD-GREG-TO-JULIAN        VALUE 'Q'.
00043          88  THREE-CHARACTER-BIN
00044                   VALUES  '8' '9' 'A' 'B' 'C' 'D' 'I' 'J' 'K'.
00045          88  GREGORIAN-TO-BIN
00046                   VALUES '2' '3' '4' '9' 'A' 'B' 'J' 'K' 'L' 'M'.
00047          88  BIN-TO-GREGORIAN
00048                   VALUES ' ' '1' 'I' '8' 'G'.
00049          88  JULIAN-TO-BINARY
00050                   VALUES '5' 'C' 'E' 'F'.
00051      12  DC-ERROR-CODE                 PIC X.
00052          88  NO-CONVERSION-ERROR        VALUE ' '.
00053          88  DATE-CONVERSION-ERROR
00054                   VALUES '1' '2' '3' '4' '5' '9' 'A' 'B' 'C'.
00055          88  DATE-IS-ZERO               VALUE '1'.
00056          88  DATE-IS-NON-NUMERIC        VALUE '2'.
00057          88  DATE-IS-INVALID            VALUE '3'.
00058          88  DATE1-GREATER-DATE2        VALUE '4'.
00059          88  ELAPSED-PLUS-NEGATIVE      VALUE '5'.
00060          88  DATE-INVALID-OPTION        VALUE '9'.
00061          88  INVALID-CENTURY            VALUE 'A'.
00062          88  ONLY-CENTURY               VALUE 'B'.
00063          88  ONLY-LEAP-YEAR             VALUE 'C'.
00064          88  VALID-CENTURY-LEAP-YEAR    VALUE 'B', 'C'.
00065      12  DC-END-OF-MONTH               PIC X.
00066          88  CALCULATE-END-OF-MONTH     VALUE '1'.
00067      12  DC-CENTURY-ADJUSTMENT         PIC X   VALUE SPACES.
00068          88  USE-NORMAL-PROCESS         VALUE ' '.
00069          88  ADJUST-DOWN-100-YRS        VALUE '1'.
00070          88  ADJUST-UP-100-YRS          VALUE '2'.
00071      12  FILLER                        PIC X.
00072      12  DC-CONVERSION-DATES.
00073          16  DC-BIN-DATE-1             PIC XX.
00074          16  DC-BIN-DATE-2             PIC XX.
00075          16  DC-GREG-DATE-1-EDIT       PIC X(08).
00076          16  DC-GREG-DATE-1-EDIT-R REDEFINES
00077                        DC-GREG-DATE-1-EDIT.
00078              20  DC-EDIT1-MONTH        PIC 99.
00079              20  SLASH1-1              PIC X.
00080              20  DC-EDIT1-DAY          PIC 99.
00081              20  SLASH1-2              PIC X.
00082              20  DC-EDIT1-YEAR         PIC 99.
00083          16  DC-GREG-DATE-2-EDIT       PIC X(08).
00084          16  DC-GREG-DATE-2-EDIT-R REDEFINES
00085                      DC-GREG-DATE-2-EDIT.
00086              20  DC-EDIT2-MONTH        PIC 99.
00087              20  SLASH2-1              PIC X.
00088              20  DC-EDIT2-DAY          PIC 99.
00089              20  SLASH2-2              PIC X.
00090              20  DC-EDIT2-YEAR         PIC 99.
00091          16  DC-GREG-DATE-1-YMD        PIC 9(06).
00092          16  DC-GREG-DATE-1-YMD-R  REDEFINES
00093                      DC-GREG-DATE-1-YMD.
00094              20  DC-YMD-YEAR           PIC 99.
00095              20  DC-YMD-MONTH          PIC 99.
00096              20  DC-YMD-DAY            PIC 99.
00097          16  DC-GREG-DATE-1-MDY        PIC 9(06).
00098          16  DC-GREG-DATE-1-MDY-R REDEFINES
00099                       DC-GREG-DATE-1-MDY.
00100              20  DC-MDY-MONTH          PIC 99.
00101              20  DC-MDY-DAY            PIC 99.
00102              20  DC-MDY-YEAR           PIC 99.
00103          16  DC-GREG-DATE-1-ALPHA.
00104              20  DC-ALPHA-MONTH        PIC X(10).
00105              20  DC-ALPHA-DAY          PIC 99.
00106              20  FILLER                PIC XX.
00107              20  DC-ALPHA-CENTURY.
00108                  24 DC-ALPHA-CEN-N     PIC 99.
00109              20  DC-ALPHA-YEAR         PIC 99.
00110          16  DC-ELAPSED-MONTHS         PIC S9(4)     COMP.
00111          16  DC-ODD-DAYS-OVER          PIC S9(4)     COMP.
00112          16  DC-ELAPSED-DAYS           PIC S9(4)     COMP.
00113          16  DC-JULIAN-DATE            PIC 9(05).
00114          16  DC-JULIAN-YYDDD REDEFINES DC-JULIAN-DATE
00115                                        PIC 9(05).
00116          16  DC-JULIAN-DT REDEFINES DC-JULIAN-DATE.
00117              20  DC-JULIAN-YEAR        PIC 99.
00118              20  DC-JULIAN-DAYS        PIC 999.
00119          16  DC-DAYS-IN-MONTH          PIC S9(3)       COMP-3.
00120          16  DC-DAY-OF-WEEK            PIC S9  VALUE ZERO COMP-3.
00121          16  DC-DAY-OF-WEEK2           PIC S9  VALUE ZERO COMP-3.
00122      12  DATE-CONVERSION-VARIBLES.
00123          16  HOLD-CENTURY-1            PIC 9(11) VALUE 0.
00124          16  HOLD-CENTURY-1-SPLIT REDEFINES HOLD-CENTURY-1.
00125              20  FILLER                PIC 9(3).
00126              20  HOLD-CEN-1-CCYY.
00127                  24  HOLD-CEN-1-CC     PIC 99.
00128                  24  HOLD-CEN-1-YY     PIC 99.
00129              20  HOLD-CEN-1-MO         PIC 99.
00130              20  HOLD-CEN-1-DA         PIC 99.
00131          16  HOLD-CENTURY-1-R   REDEFINES HOLD-CENTURY-1.
00132              20  HOLD-CEN-1-R-MO       PIC 99.
00133              20  HOLD-CEN-1-R-DA       PIC 99.
00134              20  HOLD-CEN-1-R-CCYY.
00135                  24  HOLD-CEN-1-R-CC   PIC 99.
00136                  24  HOLD-CEN-1-R-YY   PIC 99.
00137              20  FILLER                PIC 9(3).
00138          16  HOLD-CENTURY-1-X.
00139              20  FILLER                PIC X(3)  VALUE SPACES.
00140              20  HOLD-CEN-1-X-CCYY.
00141                  24  HOLD-CEN-1-X-CC   PIC XX VALUE SPACES.
00142                  24  HOLD-CEN-1-X-YY   PIC XX VALUE SPACES.
00143              20  HOLD-CEN-1-X-MO       PIC XX VALUE SPACES.
00144              20  HOLD-CEN-1-X-DA       PIC XX VALUE SPACES.
00145          16  HOLD-CENTURY-1-R-X REDEFINES HOLD-CENTURY-1-X.
00146              20  HOLD-CEN-1-R-X-MO     PIC XX.
00147              20  HOLD-CEN-1-R-X-DA     PIC XX.
00148              20  HOLD-CEN-1-R-X-CCYY.
00149                  24  HOLD-CEN-1-R-X-CC PIC XX.
00150                  24  HOLD-CEN-1-R-X-YY PIC XX.
00151              20  FILLER                PIC XXX.
00152          16  DC-BIN-DATE-EXPAND-1      PIC XXX.
00153          16  DC-BIN-DATE-EXPAND-2      PIC XXX.
00154          16  DC-JULIAN-DATE-1          PIC 9(07).
00155          16  DC-JULIAN-DATE-1-R REDEFINES DC-JULIAN-DATE-1.
00156              20  DC-JULIAN-1-CCYY.
00157                  24  DC-JULIAN-1-CC    PIC 99.
00158                  24  DC-JULIAN-1-YR    PIC 99.
00159              20  DC-JULIAN-DA-1        PIC 999.
00160          16  DC-JULIAN-DATE-2          PIC 9(07).
00161          16  DC-JULIAN-DATE-2-R REDEFINES DC-JULIAN-DATE-2.
00162              20  DC-JULIAN-2-CCYY.
00163                  24  DC-JULIAN-2-CC    PIC 99.
00164                  24  DC-JULIAN-2-YR    PIC 99.
00165              20  DC-JULIAN-DA-2        PIC 999.
00166          16  DC-GREG-DATE-A-EDIT.
00167              20  DC-EDITA-MONTH        PIC 99.
00168              20  SLASHA-1              PIC X VALUE '/'.
00169              20  DC-EDITA-DAY          PIC 99.
00170              20  SLASHA-2              PIC X VALUE '/'.
00171              20  DC-EDITA-CCYY.
00172                  24  DC-EDITA-CENT     PIC 99.
00173                  24  DC-EDITA-YEAR     PIC 99.
00174          16  DC-GREG-DATE-B-EDIT.
00175              20  DC-EDITB-MONTH        PIC 99.
00176              20  SLASHB-1              PIC X VALUE '/'.
00177              20  DC-EDITB-DAY          PIC 99.
00178              20  SLASHB-2              PIC X VALUE '/'.
00179              20  DC-EDITB-CCYY.
00180                  24  DC-EDITB-CENT     PIC 99.
00181                  24  DC-EDITB-YEAR     PIC 99.
00182          16  DC-GREG-DATE-CYMD         PIC 9(08).
00183          16  DC-GREG-DATE-CYMD-R REDEFINES
00184                               DC-GREG-DATE-CYMD.
00185              20  DC-CYMD-CEN           PIC 99.
00186              20  DC-CYMD-YEAR          PIC 99.
00187              20  DC-CYMD-MONTH         PIC 99.
00188              20  DC-CYMD-DAY           PIC 99.
00189          16  DC-GREG-DATE-MDCY         PIC 9(08).
00190          16  DC-GREG-DATE-MDCY-R REDEFINES
00191                               DC-GREG-DATE-MDCY.
00192              20  DC-MDCY-MONTH         PIC 99.
00193              20  DC-MDCY-DAY           PIC 99.
00194              20  DC-MDCY-CEN           PIC 99.
00195              20  DC-MDCY-YEAR          PIC 99.
CIDMOD    12  DC-FORCE-EL310-DATE-SW         PIC X    VALUE SPACE.
CIDMOD        88  DC-FORCE-EL310-DATE                 VALUE 'Y'.
CIDMOD    12  DC-EL310-DATE                  PIC X(21).
CIDMOD    12  FILLER                         PIC X(28).
      *                                COPY ELCLOGOF.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCLOGOF.                           *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *             STANDARD CLAS-IC LOGOFF TEXT AREA                  *
00007 *                                                                *
00008 ******************************************************************
00009  01  CLASIC-LOGOFF.
00010      12  LOGOFF-LENGTH       PIC S9(4)   VALUE +185   COMP.
00011      12  LOGOFF-TEXT.
00012          16  FILLER          PIC X(5)    VALUE SPACES.
00013          16  LOGOFF-MSG.
00014              20  LOGOFF-PGM  PIC X(8)    VALUE SPACES.
00015              20  FILLER      PIC X       VALUE SPACES.
00016              20  LOGOFF-FILL PIC X(66)   VALUE SPACES.
00017          16  FILLER          PIC X(80)
00018            VALUE '* YOU ARE NOW LOGGED OFF'.
00019          16  FILLER          PIC X(7)    VALUE '* LOGIC'.
00020          16  FILLER          PIC X       VALUE QUOTE.
00021          16  LOGOFF-SYS-MSG  PIC X(17)
00022            VALUE 'S CLAS-IC SYSTEM '.
00023      12  TEXT-MESSAGES.
00024          16  UNACCESS-MSG    PIC X(29)
00025              VALUE  'UNAUTHORIZED ACCESS ATTEMPTED'.
00026          16  PGMIDERR-MSG    PIC X(17)
00027              VALUE 'PROGRAM NOT FOUND'.
      *                                COPY ELCATTR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCATTR.                            *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *             LIST OF STANDARD ATTRIBUTE VALUES                  *
00007 *                                                                *
00008 *   THE DATA NAMES IN THIS COPY BOOK WERE ASSIGNED AS FOLLOWS:   *
00009 *                                                                *
00010 *                   POS 1   P=PROTECTED                          *
00011 *                           U=UNPROTECTED                        *
00012 *                           S=ASKIP                              *
00013 *                   POS 2   A=ALPHA/NUMERIC                      *
00014 *                           N=NUMERIC                            *
00015 *                   POS 3   N=NORMAL                             *
00016 *                           B=BRIGHT                             *
00017 *                           D=DARK                               *
00018 *                   POS 4-5 ON=MODIFIED DATA TAG ON              *
00019 *                           OF=MODIFIED DATA TAG OFF             *
00020 *                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCATTR                          *
00021 ******************************************************************
00022  01  ATTRIBUTE-LIST.
00023      12  AL-PABOF            PIC X       VALUE 'Y'.
00024      12  AL-PABON            PIC X       VALUE 'Z'.
00025      12  AL-PADOF            PIC X       VALUE '%'.
00026      12  AL-PADON            PIC X       VALUE '_'.
00027      12  AL-PANOF            PIC X       VALUE '-'.
00028      12  AL-PANON            PIC X       VALUE '/'.
00029      12  AL-SABOF            PIC X       VALUE '8'.
00030      12  AL-SABON            PIC X       VALUE '9'.
00031      12  AL-SADOF            PIC X       VALUE '@'.
00032      12  AL-SADON            PIC X       VALUE QUOTE.
00033      12  AL-SANOF            PIC X       VALUE '0'.
00034      12  AL-SANON            PIC X       VALUE '1'.
00035      12  AL-UABOF            PIC X       VALUE 'H'.
00036      12  AL-UABON            PIC X       VALUE 'I'.
00037      12  AL-UADOF            PIC X       VALUE '<'.
00038      12  AL-UADON            PIC X       VALUE '('.
00039      12  AL-UANOF            PIC X       VALUE ' '.
00040      12  AL-UANON            PIC X       VALUE 'A'.
00041      12  AL-UNBOF            PIC X       VALUE 'Q'.
00042      12  AL-UNBON            PIC X       VALUE 'R'.
00043      12  AL-UNDOF            PIC X       VALUE '*'.
00044      12  AL-UNDON            PIC X       VALUE ')'.
00045      12  AL-UNNOF            PIC X       VALUE '&'.
00046      12  AL-UNNON            PIC X       VALUE 'J'.
      *                                COPY ELCEMIB.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCEMIB.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.005                          *
00006 *                                                                *
00007 *    STANDARD CLAS-IC ERROR MESSAGE COMMUNICATIONS AREA          *
00008 *                                                                *
00009 ******************************************************************
00010  01  ERROR-MESSAGE-INTERFACE-BLOCK.
00011      12  EMI-COMM-LENGTH         PIC S9(4)    VALUE +400 COMP.
00012      12  EMI-NUMBER-OF-LINES     PIC 9        VALUE 1.
00013      12  EMI-ERROR               PIC 9(4)     VALUE ZEROS.
00014      12  EMI-SUB                 PIC 99       VALUE 1 COMP-3.
00015      12  EMI-NOTE-CTR            PIC 999      VALUE 0 COMP-3.
00016      12  EMI-WARNING-CTR         PIC 999      VALUE 0 COMP-3.
00017      12  EMI-FORCABLE-CTR        PIC 999      VALUE 0 COMP-3.
00018      12  EMI-FATAL-CTR           PIC 999      VALUE 0 COMP-3.
00019      12  EMI-SWITCH1             PIC X        VALUE '1'.
00020          88  EMI-NO-ERRORS                    VALUE '1'.
00021          88  EMI-ERRORS-NOT-COMPLETE          VALUE '2'.
00022          88  EMI-ERRORS-COMPLETE              VALUE '3'.
00023      12  EMI-SWITCH2             PIC X        VALUE '1'.
00024          88  EMI-FORMAT-CODES-ONLY            VALUE '2'.
00025      12  EMI-SWITCH-AREA-1       PIC X        VALUE '1'.
00026          88  EMI-AREA1-EMPTY                  VALUE '1'.
00027          88  EMI-AREA1-FULL                   VALUE '2'.
00028      12  EMI-SWITCH-AREA-2       PIC X        VALUE '1'.
00029          88  EMI-AREA2-EMPTY                  VALUE '1'.
00030          88  EMI-AREA2-FULL                   VALUE '2'.
00031      12  EMI-ACTION-SWITCH       PIC X        VALUE ' '.
00032          88  EMI-PROCESS-ALL-ERRORS           VALUE ' '.
00033          88  EMI-BYPASS-NOTES                 VALUE 'N'.
00034          88  EMI-BYPASS-WARNINGS              VALUE 'W'.
00035          88  EMI-BYPASS-FORCABLES             VALUE 'F'.
00036          88  EMI-BYPASS-FATALS                VALUE 'X'.
00037      12  EMI-ERROR-LINES.
00038          16  EMI-LINE1           PIC X(72)   VALUE SPACES.
00039          16  EMI-LINE2           PIC X(72)   VALUE SPACES.
00040          16  EMI-LINE3           PIC X(72)   VALUE SPACES.
00041          16  EMI-CODE-LINE REDEFINES EMI-LINE3.
00042              20  EMI-ERR-CODES OCCURS 10 TIMES.
00043                  24  EMI-ERR-NUM         PIC X(4).
00044                  24  EMI-FILLER          PIC X.
00045                  24  EMI-SEV             PIC X.
00046                  24  FILLER              PIC X.
00047              20  FILLER                  PIC X(02).
00048      12  EMI-ERR-LINES REDEFINES EMI-ERROR-LINES.
00049          16  EMI-MESSAGE-AREA OCCURS 3 TIMES INDEXED BY EMI-INDX.
00050              20  EMI-ERROR-NUMBER    PIC X(4).
00051              20  EMI-FILL            PIC X.
00052              20  EMI-SEVERITY        PIC X.
00053              20  FILLER              PIC X.
00054              20  EMI-ERROR-TEXT.
00055                  24  EMI-TEXT-VARIABLE   PIC X(10).
00056                  24  FILLER          PIC X(55).
00057      12  EMI-SEVERITY-SAVE           PIC X.
00058          88  EMI-NOTE                    VALUE 'N'.
00059          88  EMI-WARNING                 VALUE 'W'.
00060          88  EMI-FORCABLE                VALUE 'F'.
00061          88  EMI-FATAL                   VALUE 'X'.
00062      12  EMI-MESSAGE-FLAG            PIC X.
00063          88  EMI-MESSAGE-FORMATTED       VALUE 'Y'.
00064          88  EMI-NO-MESSAGE-FORMATTED    VALUE 'N'.
00065      12  EMI-ROLL-SWITCH             PIC X       VALUE SPACES.
00066      12  EMI-LANGUAGE-IND            PIC X       VALUE SPACES.
00067          88  EMI-LANGUAGE-IS-FR                  VALUE 'F'.
00068          88  EMI-LANGUAGE-IS-ENG                 VALUE 'E'.
00069          88  EMI-LANGUAGE-IS-SPAN                VALUE 'S'.
           12  emi-claim-no                pic x(7).
           12  emi-claim-type              pic x(6).
00070      12  FILLER                      PIC X(124)  VALUE SPACES.
00071      12  EMI-DATE-FIELD              PIC X(06)   VALUE SPACES.
00072      12  EMI-CLIENT-ID               PIC X(3)    VALUE SPACES.
00073      12  EMI-LIFE-OVERRIDE-L6        PIC X(6).
00074      12  EMI-AH-OVERRIDE-L6          PIC X(6).
      *                                COPY ELCINTF.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCINTF.                            *
00004 *                            VMOD=2.017                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON DATA AREA                 *
00007 *                                                                *
00008 *       LENGTH = 1024                                            *
00009 *                                                                *
00010 ******************************************************************
011812*                   C H A N G E   L O G
011812*
011812* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
011812*-----------------------------------------------------------------
011812*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
011812* EFFECTIVE    NUMBER
011812*-----------------------------------------------------------------
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
011812******************************************************************
00011  01  PROGRAM-INTERFACE-BLOCK.
00012      12  PI-COMM-LENGTH                PIC S9(4) COMP VALUE +1024.
00013      12  PI-CALLING-PROGRAM              PIC X(8).
00014      12  PI-SAVED-PROGRAM-1              PIC X(8).
00015      12  PI-SAVED-PROGRAM-2              PIC X(8).
00016      12  PI-SAVED-PROGRAM-3              PIC X(8).
00017      12  PI-SAVED-PROGRAM-4              PIC X(8).
00018      12  PI-SAVED-PROGRAM-5              PIC X(8).
00019      12  PI-SAVED-PROGRAM-6              PIC X(8).
00020      12  PI-RETURN-TO-PROGRAM            PIC X(8).
00021      12  PI-COMPANY-ID                   PIC XXX.
00022      12  PI-COMPANY-CD                   PIC X.
00023
00024      12  PI-COMPANY-PASSWORD             PIC X(8).
00025
00026      12  PI-JOURNAL-FILE-ID              PIC S9(4) COMP.
00027
00028      12  PI-CONTROL-IN-PROGRESS.
00029          16  PI-CARRIER                  PIC X.
00030          16  PI-GROUPING                 PIC X(6).
00031          16  PI-STATE                    PIC XX.
00032          16  PI-ACCOUNT                  PIC X(10).
00033          16  PI-PRODUCER REDEFINES PI-ACCOUNT
00034                                          PIC X(10).
00035          16  PI-CLAIM-CERT-GRP.
00036              20  PI-CLAIM-NO             PIC X(7).
00037              20  PI-CERT-NO.
00038                  25  PI-CERT-PRIME       PIC X(10).
00039                  25  PI-CERT-SFX         PIC X.
00040              20  PI-CERT-EFF-DT          PIC XX.
00041          16  PI-PLAN-DATA REDEFINES PI-CLAIM-CERT-GRP.
00042              20  PI-PLAN-CODE            PIC X(2).
00043              20  PI-REVISION-NUMBER      PIC X(3).
00044              20  PI-PLAN-EFF-DT          PIC X(2).
00045              20  PI-PLAN-EXP-DT          PIC X(2).
00046              20  FILLER                  PIC X(11).
00047          16  PI-OE-REFERENCE-1 REDEFINES PI-CLAIM-CERT-GRP.
00048              20  PI-OE-REFERENCE-1.
00049                  25  PI-OE-REF-1-PRIME   PIC X(18).
00050                  25  PI-OE-REF-1-SUFF    PIC XX.
00051
00052      12  PI-SESSION-IN-PROGRESS          PIC X.
00053          88  CLAIM-SESSION                   VALUE '1'.
00054          88  CREDIT-SESSION                  VALUE '2'.
00055          88  WARRANTY-SESSION                VALUE '3'.
00056          88  MORTGAGE-SESSION                VALUE '4'.
00057          88  GENERAL-LEDGER-SESSION          VALUE '5'.
00058
00059
00060 *THE FOLLOWING TWO FIELDS ARE USED ONLY WITH MULTI COMPANY CLIENTS
00061
00062      12  PI-ORIGINAL-COMPANY-ID          PIC X(3).
00063      12  PI-ORIGINAL-COMPANY-CD          PIC X.
00064
00065      12  PI-CREDIT-USER                  PIC X.
00066          88  PI-NOT-CREDIT-USER              VALUE 'N'.
00067          88  PI-HAS-CLAS-IC-CREDIT           VALUE 'Y'.
00068
00069      12  PI-CLAIM-USER                   PIC X.
00070          88  PI-NOT-CLAIM-USER               VALUE 'N'.
00071          88  PI-HAS-CLAS-IC-CLAIM            VALUE 'Y'.
00072
00073      12  PI-PROCESSOR-SYS-ACCESS         PIC X.
00074          88  PI-ACCESS-TO-BOTH-SYSTEMS       VALUE ' '.
00075          88  PI-ACCESS-TO-ALL-SYSTEMS        VALUE ' '.
00076          88  PI-ACCESS-TO-CLAIM-ONLY         VALUE '1'.
00077          88  PI-ACCESS-TO-CREDIT-ONLY        VALUE '2'.
00078          88  PI-ACCESS-TO-MORTGAGE-ONLY      VALUE '3'.
00079
00080      12  PI-PROCESSOR-ID                 PIC X(4).
00081
00082      12  PI-PROCESSOR-PASSWORD           PIC X(11).
00083
00084      12  PI-MEMBER-CAPTION               PIC X(10).
00085
00086      12  PI-PROCESSOR-USER-ALMIGHTY      PIC X.
00087          88  PI-USER-ALMIGHTY-YES            VALUE 'Y'.
00088
00089      12  PI-LIFE-OVERRIDE-L1             PIC X.
00090      12  PI-LIFE-OVERRIDE-L2             PIC XX.
00091      12  PI-LIFE-OVERRIDE-L6             PIC X(6).
00092      12  PI-LIFE-OVERRIDE-L12            PIC X(12).
00093
00094      12  PI-AH-OVERRIDE-L1               PIC X.
00095      12  PI-AH-OVERRIDE-L2               PIC XX.
00096      12  PI-AH-OVERRIDE-L6               PIC X(6).
00097      12  PI-AH-OVERRIDE-L12              PIC X(12).
00098
00099      12  PI-NEW-SYSTEM                   PIC X(2).
00100
00101      12  PI-PRIMARY-CERT-NO              PIC X(11).
00102      12  PI-CLAIM-PAID-THRU-TO           PIC X(01).
00103          88  PI-USES-PAID-TO                 VALUE '1'.
00104      12  PI-CRDTCRD-SYSTEM.
00105          16  PI-CRDTCRD-USER             PIC X.
00106              88  PI-NOT-CRDTCRD-USER         VALUE 'N'.
00107              88  PI-HAS-CLAS-IC-CRDTCRD      VALUE 'Y'.
00108          16  PI-CC-MONTH-END-DT          PIC XX.
00109      12  PI-PROCESSOR-PRINTER            PIC X(4).
00110
00111      12  PI-OE-REFERENCE-2.
00112          16  PI-OE-REF-2-PRIME           PIC X(10).
00113          16  PI-OE-REF-2-SUFF            PIC X.
00114
00115      12  PI-REM-TRM-CALC-OPTION          PIC X.
00116
00117      12  PI-LANGUAGE-TYPE                PIC X.
00118              88  PI-LANGUAGE-IS-ENG          VALUE 'E'.
00119              88  PI-LANGUAGE-IS-FR           VALUE 'F'.
00120              88  PI-LANGUAGE-IS-SPAN         VALUE 'S'.
00121
00122      12  PI-POLICY-LINKAGE-IND           PIC X.
00123          88  PI-USE-POLICY-LINKAGE           VALUE 'Y'.
00124          88  PI-POLICY-LINKAGE-NOT-USED      VALUE 'N'
00125                                                    LOW-VALUES.
00126
00127      12  PI-ALT-DMD-PRT-ID               PIC X(4).
00128      12  PI-CLAIM-PW-SESSION             PIC X(1).
00129          88  PI-CLAIM-CREDIT                 VALUE '1'.
00130          88  PI-CLAIM-CONVEN                 VALUE '2'.
011812
011812     12  PI-PROCESSOR-CSR-IND            PIC X.
011812         88  PI-PROCESSOR-IS-CSR             VALUE 'Y' 'S'.
011812         88  PI-PROCESSOR-IS-CSR-SUPER       VALUE 'S'.
011812
011812     12  FILLER                          PIC X(3).
00132
00133      12  PI-SYSTEM-LEVEL                 PIC X(145).
00134
00135      12  PI-CLAIMS-CREDIT-LEVEL          REDEFINES
00136          PI-SYSTEM-LEVEL.
00137
00138          16  PI-ENTRY-CODES.
00139              20  PI-ENTRY-CD-1           PIC X.
00140              20  PI-ENTRY-CD-2           PIC X.
00141
00142          16  PI-RETURN-CODES.
00143              20  PI-RETURN-CD-1          PIC X.
00144              20  PI-RETURN-CD-2          PIC X.
00145
00146          16  PI-UPDATE-STATUS-SAVE.
00147              20  PI-UPDATE-BY            PIC X(4).
00148              20  PI-UPDATE-HHMMSS        PIC S9(7)     COMP-3.
00149
00150          16  PI-LOWER-CASE-LETTERS       PIC X.
00151              88  LOWER-CASE-LETTERS-USED     VALUE 'Y'.
00152
00153 *        16  PI-CLAIM-ACCESS-CONTROL     PIC X.
00154 *            88  CLAIM-NO-UNIQUE             VALUE '1'.
00155 *            88  CARRIER-CLM-CNTL            VALUE '2'.
00156
00157          16  PI-CERT-ACCESS-CONTROL      PIC X.
00158              88  ST-ACCNT-CNTL               VALUE ' '.
00159              88  CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
00160              88  CARR-ST-ACCNT-CNTL          VALUE '2'.
00161              88  ACCNT-CNTL                  VALUE '3'.
00162              88  CARR-ACCNT-CNTL             VALUE '4'.
00163
00164          16  PI-PROCESSOR-CAP-LIST.
00165              20  PI-SYSTEM-CONTROLS.
00166                 24 PI-SYSTEM-DISPLAY     PIC X.
00167                  88  SYSTEM-DISPLAY-CAP      VALUE 'Y'.
00168                 24 PI-SYSTEM-MODIFY      PIC X.
00169                  88  SYSTEM-MODIFY-CAP       VALUE 'Y'.
00170              20  FILLER                  PIC XX.
00171              20  PI-DISPLAY-CAP          PIC X.
00172                  88  DISPLAY-CAP             VALUE 'Y'.
00173              20  PI-MODIFY-CAP           PIC X.
00174                  88  MODIFY-CAP              VALUE 'Y'.
00175              20  PI-MSG-AT-LOGON-CAP     PIC X.
00176                  88  MSG-AT-LOGON-CAP        VALUE 'Y'.
00177              20  PI-FORCE-CAP            PIC X.
00178                  88  FORCE-CAP               VALUE 'Y'.
00179
00180          16  PI-PROGRAM-CONTROLS.
00181              20  PI-PGM-PRINT-OPT        PIC X.
00182              20  PI-PGM-FORMAT-OPT       PIC X.
00183              20  PI-PGM-PROCESS-OPT      PIC X.
00184              20  PI-PGM-TOTALS-OPT       PIC X.
00185
00186          16  PI-HELP-INTERFACE.
00187              20  PI-LAST-ERROR-NO        PIC X(4).
00188              20  PI-CURRENT-SCREEN-NO    PIC X(4).
00189
00190          16  PI-CARRIER-CONTROL-LEVEL    PIC X.
00191              88  CONTROL-IS-ACTUAL-CARRIER   VALUE SPACE.
00192
00193          16  PI-CR-CONTROL-IN-PROGRESS.
00194              20  PI-CR-CARRIER           PIC X.
00195              20  PI-CR-GROUPING          PIC X(6).
00196              20  PI-CR-STATE             PIC XX.
00197              20  PI-CR-ACCOUNT           PIC X(10).
00198              20  PI-CR-FIN-RESP          PIC X(10).
00199              20  PI-CR-TYPE              PIC X.
00200
00201          16  PI-CR-BATCH-NUMBER          PIC X(6).
00202
00203          16  PI-CR-MONTH-END-DT          PIC XX.
00204
00205          16  PI-CAR-GROUP-ACCESS-CNTL    PIC X.
00206              88  PI-USE-ACTUAL-CARRIER       VALUE ' '.
00207              88  PI-ZERO-CARRIER             VALUE '1'.
00208              88  PI-ZERO-GROUPING            VALUE '2'.
00209              88  PI-ZERO-CAR-GROUP           VALUE '3'.
00210
00211          16  PI-CARRIER-SECURITY         PIC X.
00212              88  PI-NO-CARRIER-SECURITY      VALUE ' '.
00213
00214          16  PI-ACCOUNT-SECURITY         PIC X(10).
00215              88  PI-NO-ACCOUNT-SECURITY      VALUE SPACES.
00216              88  PI-NO-PRODUCER-SECURITY     VALUE SPACES.
00217
00218          16  PI-CODE-SECURITY REDEFINES PI-ACCOUNT-SECURITY.
00219              20  PI-ACCESS-CODE          OCCURS 10 TIMES
00220                                          INDEXED BY PI-ACCESS-NDX
00221                                          PIC X.
00222
00223          16  PI-GA-BILLING-CONTROL       PIC X.
00224              88  PI-GA-BILLING               VALUE '1'.
00225
00226          16  PI-MAIL-PROCESSING          PIC X.
00227              88  PI-MAIL-YES                 VALUE 'Y'.
00228
00229          16  PI-SECURITY-TEMP-STORE-ID   PIC X(8).
00230
00231          16  PI-AR-SYSTEM.
00232              20  PI-AR-PROCESSING-CNTL   PIC X.
00233                  88  PI-AR-PROCESSING        VALUE 'Y'.
00234              20  PI-AR-SUMMARY-CODE      PIC X(6).
00235              20  PI-AR-MONTH-END-DT      PIC XX.
00236
00237          16  PI-MP-SYSTEM.
00238              20  PI-MORTGAGE-USER            PIC X.
00239                  88  PI-NOT-MORTGAGE-USER            VALUE 'N'.
00240                  88  PI-HAS-CLAS-IC-MORTGAGE         VALUE 'Y'.
00241              20  PI-MORTGAGE-ACCESS-CONTROL  PIC X.
00242                  88  PI-MP-ST-PROD-CNTL              VALUE ' '.
00243                  88  PI-MP-CARR-GRP-ST-PROD-CNTL     VALUE '1'.
00244                  88  PI-MP-CARR-ST-PROD-CNTL         VALUE '2'.
00245                  88  PI-MP-PROD-CNTL                 VALUE '3'.
00246                  88  PI-MP-CARR-PROD-CNTL            VALUE '4'.
00247              20  PI-MP-MONTH-END-DT          PIC XX.
00248              20  PI-MP-REFERENCE-NO.
00249                  24  PI-MP-REFERENCE-PRIME   PIC X(18).
00250                  24  PI-MP-REFERENCE-SFX     PIC XX.
00251
00252          16  PI-LABEL-CONTROL            PIC X(01).
00253              88  PI-CREATE-LABELS                    VALUE 'Y'.
00254              88  PI-BYPASS-LABELS                    VALUE 'N'.
00255
00256          16  PI-BILL-GROUPING-CODE       PIC X(01).
00257              88  PI-CO-HAS-BILL-GROUPING             VALUE 'Y'.
00258
00259          16  PI-RATE-DEV-AUTHORIZATION   PIC X(01).
00260              88  PI-RATE-DEV-AUTHORIZED              VALUE 'Y'.
00261              88  PI-RATE-DEV-NOT-AUTHORIZED          VALUE 'N'.
00262
00263          16  FILLER                      PIC X(14).
00264
00265      12  PI-PROGRAM-WORK-AREA            PIC X(640).
00266 ******************************************************************
      *                                COPY ELC630PI.
00001 ******************************************************************
00004 *                                                                *
00002 *                            ELC630PI                            *
00003 *                            VMOD=2.014                          *
00004 *                                                                *
00005 * - PI-PROGRAM-WORK-AREA FOR THE DATA-ENTRY SUB-SYSTEM -         *
00006 *                                                                *
00007 *    THE FOLLOWING PROGRAMS USE THIS COPYBOOK.                   *
00008 *                                                                *
00009 *               EL630 - EL6301 - EL6302                          *
00010 ******************************************************************
072308*                   C H A N G E   L O G
072308*
072308* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
072308*-----------------------------------------------------------------
072308*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
072308* EFFECTIVE    NUMBER
072308*-----------------------------------------------------------------
072308* 072308  CR2008040800002  PEMA  ADD CRED BENE INFORMATION
030310* 030310  CR2009031200002  PEMA  OPEN LOAN OFFICER FIELD
071211* 071211  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
00017 ******************************************************************
00011
00012      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.
00013          16  PI-AM-NAME                  PIC X(30).
00014          16  PI-MAP-NAME                 PIC X(8).
00015          16  PI-BATCH-AMOUNTS    COMP-3.
00016              20  PI-LF-ISS-REMITTED      PIC S9(8)V99.
00017              20  PI-LF-ISS-ENTERED       PIC S9(8)V99.
00018              20  PI-LF-CAN-REMITTED      PIC S9(8)V99.
00019              20  PI-LF-CAN-ENTERED       PIC S9(8)V99.
00020              20  PI-AH-ISS-REMITTED      PIC S9(8)V99.
00021              20  PI-AH-ISS-ENTERED       PIC S9(8)V99.
00022              20  PI-AH-CAN-REMITTED      PIC S9(8)V99.
00023              20  PI-AH-CAN-ENTERED       PIC S9(8)V99.
00024              20  PI-ISS-CNT-REMITTED     PIC S9(5).
00025              20  PI-ISS-CNT-ENTERED      PIC S9(5).
00026              20  PI-CAN-CNT-REMITTED     PIC S9(5).
00027              20  PI-CAN-CNT-ENTERED      PIC S9(5).
00028          16  PI-MAINT-FUNC               PIC X.
00029          16  PI-ERROR-SW                 PIC X.
00030              88  PI-DATA-ERRORS              VALUE 'Y'.
00031          16  PI-UPDATE-SW                PIC X.
00032              88  PI-DATA-UPDATED             VALUE 'Y'.
00033          16  PI-DISPLAY-SW               PIC X.
00034              88  PI-LAST-FUNC-DISPLAY        VALUE 'Y'.
00035          16  PI-SAVE-CALLING-PGM         PIC X(8).
00036          16  PI-LAST-SEQ-NO-ADDED        PIC S9(4) COMP.
00037          16  PI-NEXT-DISPLAY-SEQ-NO      PIC S9(4) COMP.
00038          16  PI-SAV-CARRIER              PIC X.
00039          16  PI-SAV-GROUPING             PIC X(6).
00040          16  PI-SAV-STATE                PIC XX.
00041          16  PI-SAV-ACCOUNT              PIC X(10).
00042          16  PI-SAV-CERT-EFF-DT          PIC XX.
00043          16  PI-SAV-CERT-NO.
00044              20  PI-SAV-CERT-PRIME       PIC X(14).
00045              20  PI-SAV-CERT-SFX         PIC X.
00046          16  PI-PYAJ-REFERENCE REDEFINES PI-SAV-CERT-NO.
00047              20  PI-SAV-PYAJ-REFERENCE   PIC X(12).
00048              20  FILLER                  PIC X(3).
00049          16  PI-SAV-ENDING-ERPNDB-KEY.
00050              20  PI-SAV-COMP-CD          PIC X.
00051              20  PI-SAV-ENTRY-BATCH      PIC X(6).
00052              20  PI-SAV-BATCH-SEQ        PIC S9(4) COMP.
00053              20  PI-SAV-BATCH-CHG-SEQ    PIC S9(4) COMP.
00054          16  PI-SAV-REFERENCE            PIC X(12).
00055          16  PI-SAV-FULL-CONTROL.
00056              20  PI-SAV-FC-CARRIER       PIC X.
00057              20  PI-SAV-FC-GROUPING      PIC X(6).
00058              20  PI-SAV-FC-STATE         PIC XX.
00059          16  PI-VERIFY-DELETE-SW         PIC X.
00060              88  PI-DELETE-IS-OK             VALUE 'Y'.
00061          16  PI-EL630-FIRST-TIME-SW      PIC X.
00062              88  PI-EL630-FIRST-TIME         VALUE SPACE.
00063          16  PI-CREDIT-EDIT-CONTROLS.
00064              20  PI-MIN-PREMIUM          PIC S9(3)V99  COMP-3.
00065              20  PI-MIN-AGE              PIC 99.
00066              20  PI-DEFAULT-AGE          PIC 99.
00067              20  PI-MIN-TERM             PIC S9(3)     COMP-3.
00068              20  PI-MAX-TERM             PIC S9(3)     COMP-3.
00069              20  PI-DEFAULT-SEX          PIC X.
00070              20  PI-JOINT-AGE-INPUT      PIC X.
00071                  88 PI-JOINT-AGE-IS-INPUT       VALUE '1'.
00072              20  PI-BIRTH-DATE-INPUT     PIC X.
00073                  88 PI-BIRTH-DATE-IS-INPUT      VALUE '1'.
00074          16  PI-KEYED-SWITCHES.
00075              20  PI-ISS-SUFFIX-KEYED-SW  PIC X.
00076                  88  PI-ISS-SUFFIX-KEYED     VALUE 'Y'.
00077              20  PI-CAN-SUFFIX-KEYED-SW  PIC X.
00078                  88  PI-CAN-SUFFIX-KEYED     VALUE 'Y'.
00079              20  PI-IG-KEYED-SW          PIC X.
00080                  88  PI-IG-KEYED             VALUE 'Y'.
00081              20  PI-APR-KEYED-SW         PIC X.
00082                  88  PI-APR-KEYED            VALUE 'Y'.
00083 *            20  PI-FREQ-KEYED-SW        PIC X.
00084 *                88  PI-FREQ-KEYED           VALUE 'Y'.
00083              20  PI-VIN-KEYED-SW         PIC X.
00084                  88  PI-VIN-KEYED            VALUE 'Y'.
00085              20  PI-SIG-KEYED-SW         PIC X.
00086                  88  PI-SIG-KEYED            VALUE 'Y'.
00087              20  PI-LFRT-KEYED-SW        PIC X.
00088                  88  PI-LFRT-KEYED           VALUE 'Y'.
00089              20  PI-AHRT-KEYED-SW        PIC X.
00090                  88  PI-AHRT-KEYED           VALUE 'Y'.
00091              20  PI-SSNUM-KEYED-SW       PIC X.
00092                  88  PI-SSNUM-KEYED          VALUE 'Y'.
00093              20  PI-JNT-SSNUM-KEYED-SW   PIC X.
00094                  88  PI-JNT-SSNUM-KEYED      VALUE 'Y'.
00095              20  PI-MEMBER-KEYED-SW      PIC X.
00096                  88  PI-MEMBER-KEYED         VALUE 'Y'.
00097              20  PI-MODE-KEYED-SW        PIC X.
00098                  88  PI-MODE-KEYED           VALUE 'Y'.
00099              20  PI-PMTS-KEYED-SW        PIC X.
00100                  88  PI-PMTS-KEYED           VALUE 'Y'.
00101              20  PI-LN-OFFICER-KEYED-SW  PIC X.
00102                  88  PI-LN-OFFICER-KEYED     VALUE 'Y'.
00103              20  PI-ENTRY-KEYED-SW       PIC X.
00104                  88  PI-ENTRY-KEYED          VALUE 'Y'.
00105              20  PI-FORCE-KEYED-SW       PIC X.
00106                  88  PI-FORCE-KEYED          VALUE 'Y'.
00107              20  PI-RINCD-KEYED-SW       PIC X.
00108                  88  PI-RINCD-KEYED          VALUE 'Y'.
00109              20  PI-BILLCD-KEYED-SW      PIC X.
00110                  88  PI-BILLCD-KEYED         VALUE 'Y'.
00111              20  PI-RTCLS-KEYED-SW       PIC X.
00112                  88  PI-RTCLS-KEYED          VALUE 'Y'.
00113              20  PI-LNTRM-KEYED-SW       PIC X.
00114                  88  PI-LNTRM-KEYED          VALUE 'Y'.
00115              20  PI-EXPIR-KEYED-SW       PIC X.
00116                  88  PI-EXPIR-KEYED          VALUE 'Y'.
00117              20  PI-PMT-KEYED-SW         PIC X.
00118                  88  PI-PMT-KEYED            VALUE 'Y'.
00119              20  PI-1ST-PMT-KEYED-SW     PIC X.
00120                  88  PI-1ST-PMT-KEYED        VALUE 'Y'.
00121              20  PI-DAYS-KEYED-SW        PIC X.
00122                  88  PI-DAYS-KEYED           VALUE 'Y'.
00123              20  PI-SKPCD-KEYED-SW       PIC X.
00124                  88  PI-SKPCD-KEYED          VALUE 'Y'.
00125              20  PI-JNT-AGE-KEYED-SW     PIC X.
00126                  88  PI-JNT-AGE-KEYED        VALUE 'Y'.
00127              20  PI-JNT-NAME-KEYED-SW    PIC X.
00128                  88  PI-JNT-NAME-KEYED       VALUE 'Y'.
00129              20  PI-ISS-LIVES-KEYED-SW   PIC X.
00130                  88  PI-ISS-LIVES-KEYED      VALUE 'Y'.
00131              20  PI-CAN-LIVES-KEYED-SW   PIC X.
00132                  88  PI-CAN-LIVES-KEYED      VALUE 'Y'.
00133              20  PI-PAYEE-KEYED-SW       PIC X.
00134                  88  PI-PAYEE-KEYED          VALUE 'Y'.
00135              20  PI-CHK-REQ-KEYED-SW     PIC X.
00136                  88  PI-CHK-REQ-KEYED        VALUE 'Y'.
00137              20  PI-ZIP4-KEYED-SW        PIC X.
00138                  88  PI-ZIP4-KEYED           VALUE 'Y'.
00139              20  PI-POLICY-KEYED-SW      PIC X.
00140                  88  PI-POLICY-KEYED         VALUE 'Y'.
00141              20  PI-EXPIRE-KEYED-SW      PIC X.
00142                  88  PI-EXPIRE-KEYED         VALUE 'Y'.
00143              20  PI-CRIT-PERD-KEYED-SW    PIC X.
00144                  88  PI-CRIT-PERD-KEYED      VALUE 'Y'.
00145              20  PI-BENEFICIARY-KEYED-SW PIC X.
00146                  88  PI-BENEFICIARY-KEYED    VALUE 'Y'.
00147              20  PI-PHONE-KEYED-SW       PIC X.
00148                  88  PI-PHONE-KEYED          VALUE 'Y'.
00149              20  PI-ALT-BEN-KEYED-SW     PIC X.
00150                  88  PI-ALT-BEN-KEYED        VALUE 'Y'.
00151              20  PI-ALT-PREM-KEYED-SW    PIC X.
00152                  88  PI-ALT-PREM-KEYED       VALUE 'Y'.
00153              20  PI-REFUND-MTHD-KEYED-SW PIC X.
00154                  88  PI-REFUND-MTHD-KEYED    VALUE 'Y'.
00155          16  PI-ACCT-LOW-EFF-DT          PIC XX.
00156          16  PI-ACCT-HIGH-EXP-DT         PIC XX.
00157          16  PI-BATCH-EOF-SW             PIC X.
00158              88  PI-BATCH-EOF                VALUE 'Y'.
00159          16  PI-NB-MONTH-END-DT          PIC XX.
00160          16  PI-ISSUE-ADDED-SW           PIC X.
00161              88  PI-ISSUE-ADDED              VALUE 'Y'.
00162          16  PI-BROWSE-SW                PIC X.
00163              88  PI-BROWSE                   VALUE 'Y'.
00164          16  PI-ACCT-AGENT-ERROR-SW      PIC X.
00165              88  PI-ACCT-AGENT-ERROR         VALUE 'Y'.
00166          16  PI-FIN-RESP-ERROR-SW        PIC X.
00167              88  PI-FIN-RESP-ERROR           VALUE 'Y'.
071211         16  PI-CAN-REA-KEYED-SW         PIC X.
071211             88  PI-CAN-REA-KEYED            VALUE 'Y'.
030310         16  PI-AM-EDIT-LOAN-OFC         PIC X.
072308         16  PI-AM-ADDR1                 PIC X(30).
072308         16  PI-AM-ADDR2                 PIC X(30).
               16  PI-AM-CITYST.
072308             20  PI-AM-CITY              PIC X(28).
                   20  PI-AM-STATE             PIC XX.
072308         16  PI-AM-ZIP                   PIC X(9).
072308         16  FILLER                      PIC X(290).
072308*        16  FILLER                      PIC X(390).
00171      12  PI-MISC.
00172          16  PI-ACCT-DATE-RANGES OCCURS 32 TIMES.
00173              20  PI-ACCT-EFF-DT          PIC XX.
00174              20  PI-ACCT-EXP-DT          PIC XX.
00175              20  PI-REMIT-AGENT          PIC X(10).
00176              20  PI-ACCT-AGENT           PIC X(10).
00177          16  PI-ACCOUNT-AGENT            PIC X(10).
00178          16  PI-FIN-RESP                 PIC X(10).
00179          16  PI-SUMMARY-CODE             PIC X(6).
00180          16  PI-SUB                      PIC S9(4) COMP.
00181          16  PI-COMP-CARRIER             PIC X.
00182          16  PI-COMP-GROUPING            PIC X(6).
00183          16  PI-ACCT-AGENT-PROCESSED-SW  PIC X.
00184              88  PI-ACCT-AGENT-PROCESSED     VALUE 'Y'.
00185          16  PI-CLEAR-ERROR-SW           PIC X.
00186              88  PI-CLEAR-ERROR              VALUE 'Y'.
00187          16  PI-AGE-KEYED-SW             PIC X.
00188              88  PI-AGE-KEYED                VALUE 'Y'.
00189          16  PI-BIRTHDT-KEYED-SW         PIC X.
00190              88  PI-BIRTHDT-KEYED            VALUE 'Y'.
00191          16  PI-RECEIVED-DT              PIC XX.
00192          16  PI-CSR-ID                   PIC X(4).
00193
00194      EJECT
      *                                COPY ELCJPFX.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCJPFX.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *    USER DATA FOR SYSTEM JOURNAL RECORDS  JOURNAL I.D. = "EL"   *
00008 *                                                                *
00009 *     ALL RECORDS ARE JOURNALED FOR ERROR RECOVERY               *
00010 *     FILES JOURNALED FOR AUDIT TRAIL (BEFORE CHANGE) ARE -      *
00011 *        ELCNTL - CONTROL FILE                                   *
00012 *        ELMSTR - CLAIM MASTERS                                  *
00013 *        ELTRLR - ACTIVITY TRAILERS                              *
00014 *        ELCHKQ - CHECK QUE                                      *
00015 ******************************************************************
00016  01  JOURNAL-RECORD.
           12  jp-date                     pic s9(5) comp-3.
           12  jp-time                     pic s9(7) comp-3.
00017      12  JP-USER-ID                  PIC X(4).
00018      12  JP-FILE-ID                  PIC X(8).
00019      12  JP-PROGRAM-ID               PIC X(8).
00020      12  JP-RECORD-TYPE              PIC X.
00021          88 JP-ADD              VALUE 'A'.
00022          88 JP-BEFORE-CHANGE    VALUE 'B'.
00023          88 JP-AFTER-CHANGE     VALUE 'C'.
00024          88 JP-DELETE           VALUE 'D'.
00025          88 JP-GENERIC-DELETE   VALUE 'G'.
00026          88 JP-KEY-CHG-DELETE   VALUE 'K'.
00027          88 JP-KEY-CHG-ADD      VALUE 'N'.
00028      12  JP-GENERIC-KEY-LENGTH       PIC S9(4)   COMP.
00029      12  JP-RECORD-AREA
00030
00031
                                       PIC X(608).
      *                                COPY ELCAID.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCAID.                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   DESCRIPTION:  ATTENTION IDENTIFER CHARACTERS.                *
CIDMOD*                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCAID                           *
051007*  051007  2007041300002 Change PF22 from x'D5' to x'5B'
00007 ******************************************************************
00008
00009  01  DFHAID.
00010    02  DFHNULL   PIC  X  VALUE  ' '.
00011    02  DFHENTER  PIC  X  VALUE  QUOTE.
00012    02  DFHCLEAR  PIC  X  VALUE  '_'.
00013    02  DFHPEN    PIC  X  VALUE  '='.
00014    02  DFHOPID   PIC  X  VALUE  'W'.
00015    02  DFHPA1    PIC  X  VALUE  '%'.
00016    02  DFHPA2    PIC  X  VALUE  '>'.
00017    02  DFHPA3    PIC  X  VALUE  ','.
00018    02  DFHPF1    PIC  X  VALUE  '1'.
00019    02  DFHPF2    PIC  X  VALUE  '2'.
00020    02  DFHPF3    PIC  X  VALUE  '3'.
00021    02  DFHPF4    PIC  X  VALUE  '4'.
00022    02  DFHPF5    PIC  X  VALUE  '5'.
00023    02  DFHPF6    PIC  X  VALUE  '6'.
00024    02  DFHPF7    PIC  X  VALUE  '7'.
00025    02  DFHPF8    PIC  X  VALUE  '8'.
00026    02  DFHPF9    PIC  X  VALUE  '9'.
00027    02  DFHPF10   PIC  X  VALUE  ':'.
00028    02  DFHPF11   PIC  X  VALUE  '#'.
00029    02  DFHPF12   PIC  X  VALUE  '@'.
00030    02  DFHPF13   PIC  X  VALUE  'A'.
00031    02  DFHPF14   PIC  X  VALUE  'B'.
00032    02  DFHPF15   PIC  X  VALUE  'C'.
00033    02  DFHPF16   PIC  X  VALUE  'D'.
00034    02  DFHPF17   PIC  X  VALUE  'E'.
00035    02  DFHPF18   PIC  X  VALUE  'F'.
00036    02  DFHPF19   PIC  X  VALUE  'G'.
00037    02  DFHPF20   PIC  X  VALUE  'H'.
00038    02  DFHPF21   PIC  X  VALUE  'I'.
051007*00039    02  DFHPF22   PIC  X  VALUE  'Õ'.
051007   02  DFHPF22   PIC  X  VALUE  '['.
00040    02  DFHPF23   PIC  X  VALUE  '.'.
00041    02  DFHPF24   PIC  X  VALUE  '<'.
00042    02  DFHMSRE   PIC  X  VALUE  'X'.
00043    02  DFHSTRF   PIC  X  VALUE  'h'.
00044    02  DFHTRIG   PIC  X  VALUE  '"'.
       01  FILLER    REDEFINES DFHAID.
           12  FILLER              PIC X(8).
           12  PF-VALUES           PIC X       OCCURS 2.
      *                                COPY VP6301S.
       01  VP630BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  BDATEL PIC S9(0004) COMP.
           05  BDATEF PIC  X(0001).
           05  FILLER REDEFINES BDATEF.
               10  BDATEA PIC  X(0001).
           05  BDATEI PIC  X(0008).
      *    -------------------------------
           05  BTIMEL PIC S9(0004) COMP.
           05  BTIMEF PIC  X(0001).
           05  FILLER REDEFINES BTIMEF.
               10  BTIMEA PIC  X(0001).
           05  BTIMEI PIC  X(0005).
      *    -------------------------------
           05  BBATCHL PIC S9(0004) COMP.
           05  BBATCHF PIC  X(0001).
           05  FILLER REDEFINES BBATCHF.
               10  BBATCHA PIC  X(0001).
           05  BBATCHI PIC  X(0008).
      *    -------------------------------
           05  BSEQL PIC S9(0004) COMP.
           05  BSEQF PIC  X(0001).
           05  FILLER REDEFINES BSEQF.
               10  BSEQA PIC  X(0001).
           05  BSEQI PIC  X(0004).
      *    -------------------------------
           05  BMOENDL PIC S9(0004) COMP.
           05  BMOENDF PIC  X(0001).
           05  FILLER REDEFINES BMOENDF.
               10  BMOENDA PIC  X(0001).
           05  BMOENDI PIC  X(0008).
      *    -------------------------------
           05  BACCTNML PIC S9(0004) COMP.
           05  BACCTNMF PIC  X(0001).
           05  FILLER REDEFINES BACCTNMF.
               10  BACCTNMA PIC  X(0001).
           05  BACCTNMI PIC  X(0030).
      *    -------------------------------
           05  BEFFDTL PIC S9(0004) COMP.
           05  BEFFDTF PIC  X(0001).
           05  FILLER REDEFINES BEFFDTF.
               10  BEFFDTA PIC  X(0001).
           05  BEFFDTI PIC  X(0006).
      *    -------------------------------
           05  BCERTL PIC S9(0004) COMP.
           05  BCERTF PIC  X(0001).
           05  FILLER REDEFINES BCERTF.
               10  BCERTA PIC  X(0001).
           05  BCERTI PIC  X(0010).
      *    -------------------------------
           05  BSFXL PIC S9(0004) COMP.
           05  BSFXF PIC  X(0001).
           05  FILLER REDEFINES BSFXF.
               10  BSFXA PIC  X(0001).
           05  BSFXI PIC  X(0001).
      *    -------------------------------
           05  BLONOFCL PIC S9(0004) COMP.
           05  BLONOFCF PIC  X(0001).
           05  FILLER REDEFINES BLONOFCF.
               10  BLONOFCA PIC  X(0001).
           05  BLONOFCI PIC  X(0005).
      *    -------------------------------
           05  B1STNML PIC S9(0004) COMP.
           05  B1STNMF PIC  X(0001).
           05  FILLER REDEFINES B1STNMF.
               10  B1STNMA PIC  X(0001).
           05  B1STNMI PIC  X(0010).
      *    -------------------------------
           05  BINTL PIC S9(0004) COMP.
           05  BINTF PIC  X(0001).
           05  FILLER REDEFINES BINTF.
               10  BINTA PIC  X(0001).
           05  BINTI PIC  X(0001).
      *    -------------------------------
           05  BLASTNML PIC S9(0004) COMP.
           05  BLASTNMF PIC  X(0001).
           05  FILLER REDEFINES BLASTNMF.
               10  BLASTNMA PIC  X(0001).
           05  BLASTNMI PIC  X(0015).
      *    -------------------------------
           05  BAGEL PIC S9(0004) COMP.
           05  BAGEF PIC  X(0001).
           05  FILLER REDEFINES BAGEF.
               10  BAGEA PIC  X(0001).
           05  BAGEI PIC  X(0002).
      *    -------------------------------
           05  BSEXL PIC S9(0004) COMP.
           05  BSEXF PIC  X(0001).
           05  FILLER REDEFINES BSEXF.
               10  BSEXA PIC  X(0001).
           05  BSEXI PIC  X(0001).
      *    -------------------------------
           05  BJNT1STL PIC S9(0004) COMP.
           05  BJNT1STF PIC  X(0001).
           05  FILLER REDEFINES BJNT1STF.
               10  BJNT1STA PIC  X(0001).
           05  BJNT1STI PIC  X(0010).
      *    -------------------------------
           05  BJNTINTL PIC S9(0004) COMP.
           05  BJNTINTF PIC  X(0001).
           05  FILLER REDEFINES BJNTINTF.
               10  BJNTINTA PIC  X(0001).
           05  BJNTINTI PIC  X(0001).
      *    -------------------------------
           05  BJNTNAML PIC S9(0004) COMP.
           05  BJNTNAMF PIC  X(0001).
           05  FILLER REDEFINES BJNTNAMF.
               10  BJNTNAMA PIC  X(0001).
           05  BJNTNAMI PIC  X(0015).
      *    -------------------------------
           05  BJNTAGEL PIC S9(0004) COMP.
           05  BJNTAGEF PIC  X(0001).
           05  FILLER REDEFINES BJNTAGEF.
               10  BJNTAGEA PIC  X(0001).
           05  BJNTAGEI PIC  X(0002).
      *    -------------------------------
           05  BADDRS1L PIC S9(0004) COMP.
           05  BADDRS1F PIC  X(0001).
           05  FILLER REDEFINES BADDRS1F.
               10  BADDRS1A PIC  X(0001).
           05  BADDRS1I PIC  X(0030).
      *    -------------------------------
           05  BADDRS2L PIC S9(0004) COMP.
           05  BADDRS2F PIC  X(0001).
           05  FILLER REDEFINES BADDRS2F.
               10  BADDRS2A PIC  X(0001).
           05  BADDRS2I PIC  X(0030).
      *    -------------------------------
           05  BCITYL PIC S9(0004) COMP.
           05  BCITYF PIC  X(0001).
           05  FILLER REDEFINES BCITYF.
               10  BCITYA PIC  X(0001).
           05  BCITYI PIC  X(0028).
      *    -------------------------------
           05  BSTATEL PIC S9(0004) COMP.
           05  BSTATEF PIC  X(0001).
           05  FILLER REDEFINES BSTATEF.
               10  BSTATEA PIC  X(0001).
           05  BSTATEI PIC  X(0002).
      *    -------------------------------
           05  BZIPCDEL PIC S9(0004) COMP.
           05  BZIPCDEF PIC  X(0001).
           05  FILLER REDEFINES BZIPCDEF.
               10  BZIPCDEA PIC  X(0001).
           05  BZIPCDEI PIC  X(0010).
      *    -------------------------------
           05  BYEARL PIC S9(0004) COMP.
           05  BYEARF PIC  X(0001).
           05  FILLER REDEFINES BYEARF.
               10  BYEARA PIC  X(0001).
           05  BYEARI PIC  X(0004).
      *    -------------------------------
           05  BMAKEL PIC S9(0004) COMP.
           05  BMAKEF PIC  X(0001).
           05  FILLER REDEFINES BMAKEF.
               10  BMAKEA PIC  X(0001).
           05  BMAKEI PIC  X(0020).
      *    -------------------------------
           05  BMODELL PIC S9(0004) COMP.
           05  BMODELF PIC  X(0001).
           05  FILLER REDEFINES BMODELF.
               10  BMODELA PIC  X(0001).
           05  BMODELI PIC  X(0020).
      *    -------------------------------
           05  BFUTUREL PIC S9(0004) COMP.
           05  BFUTUREF PIC  X(0001).
           05  FILLER REDEFINES BFUTUREF.
               10  BFUTUREA PIC  X(0001).
           05  BFUTUREI PIC  X(0020).
      *    -------------------------------
           05  BVINHDL PIC S9(0004) COMP.
           05  BVINHDF PIC  X(0001).
           05  FILLER REDEFINES BVINHDF.
               10  BVINHDA PIC  X(0001).
           05  BVINHDI PIC  X(0005).
      *    -------------------------------
           05  BVINL PIC S9(0004) COMP.
           05  BVINF PIC  X(0001).
           05  FILLER REDEFINES BVINF.
               10  BVINA PIC  X(0001).
           05  BVINI PIC  X(0017).
      *    -------------------------------
           05  BOMETERL PIC S9(0004) COMP.
           05  BOMETERF PIC  X(0001).
           05  FILLER REDEFINES BOMETERF.
               10  BOMETERA PIC  X(0001).
           05  BOMETERI PIC  X(0007).
      *    -------------------------------
           05  BNFICRYL PIC S9(0004) COMP.
           05  BNFICRYF PIC  X(0001).
           05  FILLER REDEFINES BNFICRYF.
               10  BNFICRYA PIC  X(0001).
           05  BNFICRYI PIC  X(0025).
      *    -------------------------------
           05  BCADDR1L PIC S9(0004) COMP.
           05  BCADDR1F PIC  X(0001).
           05  FILLER REDEFINES BCADDR1F.
               10  BCADDR1A PIC  X(0001).
           05  BCADDR1I PIC  X(0030).
      *    -------------------------------
           05  BCADDR2L PIC S9(0004) COMP.
           05  BCADDR2F PIC  X(0001).
           05  FILLER REDEFINES BCADDR2F.
               10  BCADDR2A PIC  X(0001).
           05  BCADDR2I PIC  X(0030).
      *    -------------------------------
           05  BCCITYL PIC S9(0004) COMP.
           05  BCCITYF PIC  X(0001).
           05  FILLER REDEFINES BCCITYF.
               10  BCCITYA PIC  X(0001).
           05  BCCITYI PIC  X(0028).
      *    -------------------------------
           05  BCSTATEL PIC S9(0004) COMP.
           05  BCSTATEF PIC  X(0001).
           05  FILLER REDEFINES BCSTATEF.
               10  BCSTATEA PIC  X(0001).
           05  BCSTATEI PIC  X(0002).
      *    -------------------------------
           05  BCZIPCDL PIC S9(0004) COMP.
           05  BCZIPCDF PIC  X(0001).
           05  FILLER REDEFINES BCZIPCDF.
               10  BCZIPCDA PIC  X(0001).
           05  BCZIPCDI PIC  X(0010).
      *    -------------------------------
           05  BKIND1L PIC S9(0004) COMP.
           05  BKIND1F PIC  X(0001).
           05  FILLER REDEFINES BKIND1F.
               10  BKIND1A PIC  X(0001).
           05  BKIND1I PIC  X(0002).
      *    -------------------------------
           05  BTYPE1L PIC S9(0004) COMP.
           05  BTYPE1F PIC  X(0001).
           05  FILLER REDEFINES BTYPE1F.
               10  BTYPE1A PIC  X(0001).
           05  BTYPE1I PIC  X(0003).
      *    -------------------------------
           05  BTRM1L PIC S9(0004) COMP.
           05  BTRM1F PIC  X(0001).
           05  FILLER REDEFINES BTRM1F.
               10  BTRM1A PIC  X(0001).
           05  BTRM1I PIC  X(0003).
      *    -------------------------------
           05  BBEN1L PIC S9(0004) COMP.
           05  BBEN1F PIC  X(0001).
           05  FILLER REDEFINES BBEN1F.
               10  BBEN1A PIC  X(0001).
           05  BBEN1I PIC  X(0012).
      *    -------------------------------
           05  BPRM1L PIC S9(0004) COMP.
           05  BPRM1F PIC  X(0001).
           05  FILLER REDEFINES BPRM1F.
               10  BPRM1A PIC  X(0001).
           05  BPRM1I PIC  X(0011).
      *    -------------------------------
           05  BALTBN1L PIC S9(0004) COMP.
           05  BALTBN1F PIC  X(0001).
           05  FILLER REDEFINES BALTBN1F.
               10  BALTBN1A PIC  X(0001).
           05  BALTBN1I PIC  X(0012).
      *    -------------------------------
           05  BALTPM1L PIC S9(0004) COMP.
           05  BALTPM1F PIC  X(0001).
           05  FILLER REDEFINES BALTPM1F.
               10  BALTPM1A PIC  X(0001).
           05  BALTPM1I PIC  X(0009).
      *    -------------------------------
           05  BKIND2L PIC S9(0004) COMP.
           05  BKIND2F PIC  X(0001).
           05  FILLER REDEFINES BKIND2F.
               10  BKIND2A PIC  X(0001).
           05  BKIND2I PIC  X(0002).
      *    -------------------------------
           05  BTYPE2L PIC S9(0004) COMP.
           05  BTYPE2F PIC  X(0001).
           05  FILLER REDEFINES BTYPE2F.
               10  BTYPE2A PIC  X(0001).
           05  BTYPE2I PIC  X(0003).
      *    -------------------------------
           05  BTRM2L PIC S9(0004) COMP.
           05  BTRM2F PIC  X(0001).
           05  FILLER REDEFINES BTRM2F.
               10  BTRM2A PIC  X(0001).
           05  BTRM2I PIC  X(0003).
      *    -------------------------------
           05  BBEN2L PIC S9(0004) COMP.
           05  BBEN2F PIC  X(0001).
           05  FILLER REDEFINES BBEN2F.
               10  BBEN2A PIC  X(0001).
           05  BBEN2I PIC  X(0012).
      *    -------------------------------
           05  BPRM2L PIC S9(0004) COMP.
           05  BPRM2F PIC  X(0001).
           05  FILLER REDEFINES BPRM2F.
               10  BPRM2A PIC  X(0001).
           05  BPRM2I PIC  X(0011).
      *    -------------------------------
           05  BCP2L PIC S9(0004) COMP.
           05  BCP2F PIC  X(0001).
           05  FILLER REDEFINES BCP2F.
               10  BCP2A PIC  X(0001).
           05  BCP2I PIC  X(0002).
      *    -------------------------------
           05  BALTBN2L PIC S9(0004) COMP.
           05  BALTBN2F PIC  X(0001).
           05  FILLER REDEFINES BALTBN2F.
               10  BALTBN2A PIC  X(0001).
           05  BALTBN2I PIC  X(0012).
      *    -------------------------------
           05  BALTPM2L PIC S9(0004) COMP.
           05  BALTPM2F PIC  X(0001).
           05  FILLER REDEFINES BALTPM2F.
               10  BALTPM2A PIC  X(0001).
           05  BALTPM2I PIC  X(0009).
      *    -------------------------------
           05  BERMSG1L PIC S9(0004) COMP.
           05  BERMSG1F PIC  X(0001).
           05  FILLER REDEFINES BERMSG1F.
               10  BERMSG1A PIC  X(0001).
           05  BERMSG1I PIC  X(0079).
      *    -------------------------------
           05  BERMSG2L PIC S9(0004) COMP.
           05  BERMSG2F PIC  X(0001).
           05  FILLER REDEFINES BERMSG2F.
               10  BERMSG2A PIC  X(0001).
           05  BERMSG2I PIC  X(0079).
      *    -------------------------------
           05  BPFENTRL PIC S9(0004) COMP.
           05  BPFENTRF PIC  X(0001).
           05  FILLER REDEFINES BPFENTRF.
               10  BPFENTRA PIC  X(0001).
           05  BPFENTRI PIC  9(2).
      *    -------------------------------
           05  BDELHDGL PIC S9(0004) COMP.
           05  BDELHDGF PIC  X(0001).
           05  FILLER REDEFINES BDELHDGF.
               10  BDELHDGA PIC  X(0001).
           05  BDELHDGI PIC  X(0017).
       01  VP630BO REDEFINES VP630BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BBATCHO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEQO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BMOENDO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCTNMO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEFFDTO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCERTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSFXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BLONOFCO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  B1STNMO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BLASTNMO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BJNT1STO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BJNTINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BJNTNAMO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BJNTAGEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BADDRS1O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BADDRS2O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCITYO PIC  X(0028).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BZIPCDEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BYEARO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BMAKEO PIC  X(0020).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BMODELO PIC  X(0020).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BFUTUREO PIC  X(0020).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BVINHDO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BVINO PIC  X(0017).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BOMETERO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNFICRYO PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCADDR1O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCADDR2O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCCITYO PIC  X(0028).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCSTATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCZIPCDO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BKIND1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE1O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTRM1O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BBEN1O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPRM1O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BALTBN1O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BALTPM1O PIC  X(0009).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BKIND2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYPE2O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTRM2O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BBEN2O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPRM2O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCP2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BALTBN2O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BALTPM2O PIC  X(0009).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BERMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BERMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPFENTRO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDELHDGO PIC  X(0017).
      *    -------------------------------
       01  VP630CI REDEFINES VP630BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  CDATEL PIC S9(0004) COMP.
           05  CDATEF PIC  X(0001).
           05  FILLER REDEFINES CDATEF.
               10  CDATEA PIC  X(0001).
           05  CDATEI PIC  X(0008).
      *    -------------------------------
           05  CTIMEL PIC S9(0004) COMP.
           05  CTIMEF PIC  X(0001).
           05  FILLER REDEFINES CTIMEF.
               10  CTIMEA PIC  X(0001).
           05  CTIMEI PIC  X(0005).
      *    -------------------------------
           05  CBATCHL PIC S9(0004) COMP.
           05  CBATCHF PIC  X(0001).
           05  FILLER REDEFINES CBATCHF.
               10  CBATCHA PIC  X(0001).
           05  CBATCHI PIC  X(0008).
      *    -------------------------------
           05  CMOENDL PIC S9(0004) COMP.
           05  CMOENDF PIC  X(0001).
           05  FILLER REDEFINES CMOENDF.
               10  CMOENDA PIC  X(0001).
           05  CMOENDI PIC  X(0008).
      *    -------------------------------
           05  CACCTNML PIC S9(0004) COMP.
           05  CACCTNMF PIC  X(0001).
           05  FILLER REDEFINES CACCTNMF.
               10  CACCTNMA PIC  X(0001).
           05  CACCTNMI PIC  X(0030).
      *    -------------------------------
           05  CSEQ1L PIC S9(0004) COMP.
           05  CSEQ1F PIC  X(0001).
           05  FILLER REDEFINES CSEQ1F.
               10  CSEQ1A PIC  X(0001).
           05  CSEQ1I PIC  X(0004).
      *    -------------------------------
           05  CCERT1L PIC S9(0004) COMP.
           05  CCERT1F PIC  X(0001).
           05  FILLER REDEFINES CCERT1F.
               10  CCERT1A PIC  X(0001).
           05  CCERT1I PIC  X(0010).
      *    -------------------------------
           05  CSFX1L PIC S9(0004) COMP.
           05  CSFX1F PIC  X(0001).
           05  FILLER REDEFINES CSFX1F.
               10  CSFX1A PIC  X(0001).
           05  CSFX1I PIC  X(0001).
      *    -------------------------------
           05  CEFFDT1L PIC S9(0004) COMP.
           05  CEFFDT1F PIC  X(0001).
           05  FILLER REDEFINES CEFFDT1F.
               10  CEFFDT1A PIC  X(0001).
           05  CEFFDT1I PIC  9(6).
      *    -------------------------------
           05  CLSTNM1L PIC S9(0004) COMP.
           05  CLSTNM1F PIC  X(0001).
           05  FILLER REDEFINES CLSTNM1F.
               10  CLSTNM1A PIC  X(0001).
           05  CLSTNM1I PIC  X(0015).
      *    -------------------------------
           05  CKIND1L PIC S9(0004) COMP.
           05  CKIND1F PIC  X(0001).
           05  FILLER REDEFINES CKIND1F.
               10  CKIND1A PIC  X(0001).
           05  CKIND1I PIC  X(0002).
      *    -------------------------------
           05  CCANDT1L PIC S9(0004) COMP.
           05  CCANDT1F PIC  X(0001).
           05  FILLER REDEFINES CCANDT1F.
               10  CCANDT1A PIC  X(0001).
           05  CCANDT1I PIC  9(6).
      *    -------------------------------
           05  CRFUND1L PIC S9(0004) COMP.
           05  CRFUND1F PIC  X(0001).
           05  FILLER REDEFINES CRFUND1F.
               10  CRFUND1A PIC  X(0001).
           05  CRFUND1I PIC  S9(9)V9(2).
      *    -------------------------------
           05  CMTHD1L PIC S9(0004) COMP.
           05  CMTHD1F PIC  X(0001).
           05  FILLER REDEFINES CMTHD1F.
               10  CMTHD1A PIC  X(0001).
           05  CMTHD1I PIC  X(0001).
      *    -------------------------------
           05  CKIND2L PIC S9(0004) COMP.
           05  CKIND2F PIC  X(0001).
           05  FILLER REDEFINES CKIND2F.
               10  CKIND2A PIC  X(0001).
           05  CKIND2I PIC  X(0002).
      *    -------------------------------
           05  CCANDT2L PIC S9(0004) COMP.
           05  CCANDT2F PIC  X(0001).
           05  FILLER REDEFINES CCANDT2F.
               10  CCANDT2A PIC  X(0001).
           05  CCANDT2I PIC  9(6).
      *    -------------------------------
           05  CRFUND2L PIC S9(0004) COMP.
           05  CRFUND2F PIC  X(0001).
           05  FILLER REDEFINES CRFUND2F.
               10  CRFUND2A PIC  X(0001).
           05  CRFUND2I PIC  S9(9)V9(2).
      *    -------------------------------
           05  CMTHD2L PIC S9(0004) COMP.
           05  CMTHD2F PIC  X(0001).
           05  FILLER REDEFINES CMTHD2F.
               10  CMTHD2A PIC  X(0001).
           05  CMTHD2I PIC  X(0001).
      *    -------------------------------
           05  CCHK1L PIC S9(0004) COMP.
           05  CCHK1F PIC  X(0001).
           05  FILLER REDEFINES CCHK1F.
               10  CCHK1A PIC  X(0001).
           05  CCHK1I PIC  X(0001).
      *    -------------------------------
           05  CPAYEE1L PIC S9(0004) COMP.
           05  CPAYEE1F PIC  X(0001).
           05  FILLER REDEFINES CPAYEE1F.
               10  CPAYEE1A PIC  X(0001).
           05  CPAYEE1I PIC  X(0006).
      *    -------------------------------
           05  CLIVES1L PIC S9(0004) COMP.
           05  CLIVES1F PIC  X(0001).
           05  FILLER REDEFINES CLIVES1F.
               10  CLIVES1A PIC  X(0001).
           05  CLIVES1I PIC  9(3).
      *    -------------------------------
           05  CCANRN1L PIC S9(0004) COMP.
           05  CCANRN1F PIC  X(0001).
           05  FILLER REDEFINES CCANRN1F.
               10  CCANRN1A PIC  X(0001).
           05  CCANRN1I PIC  X(0001).
      *    -------------------------------
           05  CSEQ2L PIC S9(0004) COMP.
           05  CSEQ2F PIC  X(0001).
           05  FILLER REDEFINES CSEQ2F.
               10  CSEQ2A PIC  X(0001).
           05  CSEQ2I PIC  X(0004).
      *    -------------------------------
           05  CCERT2L PIC S9(0004) COMP.
           05  CCERT2F PIC  X(0001).
           05  FILLER REDEFINES CCERT2F.
               10  CCERT2A PIC  X(0001).
           05  CCERT2I PIC  X(0010).
      *    -------------------------------
           05  CSFX2L PIC S9(0004) COMP.
           05  CSFX2F PIC  X(0001).
           05  FILLER REDEFINES CSFX2F.
               10  CSFX2A PIC  X(0001).
           05  CSFX2I PIC  X(0001).
      *    -------------------------------
           05  CEFFDT2L PIC S9(0004) COMP.
           05  CEFFDT2F PIC  X(0001).
           05  FILLER REDEFINES CEFFDT2F.
               10  CEFFDT2A PIC  X(0001).
           05  CEFFDT2I PIC  9(6).
      *    -------------------------------
           05  CLSTNM2L PIC S9(0004) COMP.
           05  CLSTNM2F PIC  X(0001).
           05  FILLER REDEFINES CLSTNM2F.
               10  CLSTNM2A PIC  X(0001).
           05  CLSTNM2I PIC  X(0015).
      *    -------------------------------
           05  CKIND3L PIC S9(0004) COMP.
           05  CKIND3F PIC  X(0001).
           05  FILLER REDEFINES CKIND3F.
               10  CKIND3A PIC  X(0001).
           05  CKIND3I PIC  X(0002).
      *    -------------------------------
           05  CCANDT3L PIC S9(0004) COMP.
           05  CCANDT3F PIC  X(0001).
           05  FILLER REDEFINES CCANDT3F.
               10  CCANDT3A PIC  X(0001).
           05  CCANDT3I PIC  9(6).
      *    -------------------------------
           05  CRFUND3L PIC S9(0004) COMP.
           05  CRFUND3F PIC  X(0001).
           05  FILLER REDEFINES CRFUND3F.
               10  CRFUND3A PIC  X(0001).
           05  CRFUND3I PIC  S9(9)V9(2).
      *    -------------------------------
           05  CMTHD3L PIC S9(0004) COMP.
           05  CMTHD3F PIC  X(0001).
           05  FILLER REDEFINES CMTHD3F.
               10  CMTHD3A PIC  X(0001).
           05  CMTHD3I PIC  X(0001).
      *    -------------------------------
           05  CKIND4L PIC S9(0004) COMP.
           05  CKIND4F PIC  X(0001).
           05  FILLER REDEFINES CKIND4F.
               10  CKIND4A PIC  X(0001).
           05  CKIND4I PIC  X(0002).
      *    -------------------------------
           05  CCANDT4L PIC S9(0004) COMP.
           05  CCANDT4F PIC  X(0001).
           05  FILLER REDEFINES CCANDT4F.
               10  CCANDT4A PIC  X(0001).
           05  CCANDT4I PIC  9(6).
      *    -------------------------------
           05  CRFUND4L PIC S9(0004) COMP.
           05  CRFUND4F PIC  X(0001).
           05  FILLER REDEFINES CRFUND4F.
               10  CRFUND4A PIC  X(0001).
           05  CRFUND4I PIC  S9(9)V9(2).
      *    -------------------------------
           05  CMTHD4L PIC S9(0004) COMP.
           05  CMTHD4F PIC  X(0001).
           05  FILLER REDEFINES CMTHD4F.
               10  CMTHD4A PIC  X(0001).
           05  CMTHD4I PIC  X(0001).
      *    -------------------------------
           05  CCHK2L PIC S9(0004) COMP.
           05  CCHK2F PIC  X(0001).
           05  FILLER REDEFINES CCHK2F.
               10  CCHK2A PIC  X(0001).
           05  CCHK2I PIC  X(0001).
      *    -------------------------------
           05  CPAYEE2L PIC S9(0004) COMP.
           05  CPAYEE2F PIC  X(0001).
           05  FILLER REDEFINES CPAYEE2F.
               10  CPAYEE2A PIC  X(0001).
           05  CPAYEE2I PIC  X(0006).
      *    -------------------------------
           05  CLIVES2L PIC S9(0004) COMP.
           05  CLIVES2F PIC  X(0001).
           05  FILLER REDEFINES CLIVES2F.
               10  CLIVES2A PIC  X(0001).
           05  CLIVES2I PIC  9(3).
      *    -------------------------------
           05  CCANRN2L PIC S9(0004) COMP.
           05  CCANRN2F PIC  X(0001).
           05  FILLER REDEFINES CCANRN2F.
               10  CCANRN2A PIC  X(0001).
           05  CCANRN2I PIC  X(0001).
      *    -------------------------------
           05  CSEQ3L PIC S9(0004) COMP.
           05  CSEQ3F PIC  X(0001).
           05  FILLER REDEFINES CSEQ3F.
               10  CSEQ3A PIC  X(0001).
           05  CSEQ3I PIC  X(0004).
      *    -------------------------------
           05  CCERT3L PIC S9(0004) COMP.
           05  CCERT3F PIC  X(0001).
           05  FILLER REDEFINES CCERT3F.
               10  CCERT3A PIC  X(0001).
           05  CCERT3I PIC  X(0010).
      *    -------------------------------
           05  CSFX3L PIC S9(0004) COMP.
           05  CSFX3F PIC  X(0001).
           05  FILLER REDEFINES CSFX3F.
               10  CSFX3A PIC  X(0001).
           05  CSFX3I PIC  X(0001).
      *    -------------------------------
           05  CEFFDT3L PIC S9(0004) COMP.
           05  CEFFDT3F PIC  X(0001).
           05  FILLER REDEFINES CEFFDT3F.
               10  CEFFDT3A PIC  X(0001).
           05  CEFFDT3I PIC  9(6).
      *    -------------------------------
           05  CLSTNM3L PIC S9(0004) COMP.
           05  CLSTNM3F PIC  X(0001).
           05  FILLER REDEFINES CLSTNM3F.
               10  CLSTNM3A PIC  X(0001).
           05  CLSTNM3I PIC  X(0015).
      *    -------------------------------
           05  CKIND5L PIC S9(0004) COMP.
           05  CKIND5F PIC  X(0001).
           05  FILLER REDEFINES CKIND5F.
               10  CKIND5A PIC  X(0001).
           05  CKIND5I PIC  X(0002).
      *    -------------------------------
           05  CCANDT5L PIC S9(0004) COMP.
           05  CCANDT5F PIC  X(0001).
           05  FILLER REDEFINES CCANDT5F.
               10  CCANDT5A PIC  X(0001).
           05  CCANDT5I PIC  9(6).
      *    -------------------------------
           05  CRFUND5L PIC S9(0004) COMP.
           05  CRFUND5F PIC  X(0001).
           05  FILLER REDEFINES CRFUND5F.
               10  CRFUND5A PIC  X(0001).
           05  CRFUND5I PIC  S9(9)V9(2).
      *    -------------------------------
           05  CMTHD5L PIC S9(0004) COMP.
           05  CMTHD5F PIC  X(0001).
           05  FILLER REDEFINES CMTHD5F.
               10  CMTHD5A PIC  X(0001).
           05  CMTHD5I PIC  X(0001).
      *    -------------------------------
           05  CKIND6L PIC S9(0004) COMP.
           05  CKIND6F PIC  X(0001).
           05  FILLER REDEFINES CKIND6F.
               10  CKIND6A PIC  X(0001).
           05  CKIND6I PIC  X(0002).
      *    -------------------------------
           05  CCANDT6L PIC S9(0004) COMP.
           05  CCANDT6F PIC  X(0001).
           05  FILLER REDEFINES CCANDT6F.
               10  CCANDT6A PIC  X(0001).
           05  CCANDT6I PIC  9(6).
      *    -------------------------------
           05  CRFUND6L PIC S9(0004) COMP.
           05  CRFUND6F PIC  X(0001).
           05  FILLER REDEFINES CRFUND6F.
               10  CRFUND6A PIC  X(0001).
           05  CRFUND6I PIC  S9(9)V9(2).
      *    -------------------------------
           05  CMTHD6L PIC S9(0004) COMP.
           05  CMTHD6F PIC  X(0001).
           05  FILLER REDEFINES CMTHD6F.
               10  CMTHD6A PIC  X(0001).
           05  CMTHD6I PIC  X(0001).
      *    -------------------------------
           05  CCHK3L PIC S9(0004) COMP.
           05  CCHK3F PIC  X(0001).
           05  FILLER REDEFINES CCHK3F.
               10  CCHK3A PIC  X(0001).
           05  CCHK3I PIC  X(0001).
      *    -------------------------------
           05  CPAYEE3L PIC S9(0004) COMP.
           05  CPAYEE3F PIC  X(0001).
           05  FILLER REDEFINES CPAYEE3F.
               10  CPAYEE3A PIC  X(0001).
           05  CPAYEE3I PIC  X(0006).
      *    -------------------------------
           05  CLIVES3L PIC S9(0004) COMP.
           05  CLIVES3F PIC  X(0001).
           05  FILLER REDEFINES CLIVES3F.
               10  CLIVES3A PIC  X(0001).
           05  CLIVES3I PIC  9(3).
      *    -------------------------------
           05  CCANRN3L PIC S9(0004) COMP.
           05  CCANRN3F PIC  X(0001).
           05  FILLER REDEFINES CCANRN3F.
               10  CCANRN3A PIC  X(0001).
           05  CCANRN3I PIC  X(0001).
      *    -------------------------------
           05  CSEQ4L PIC S9(0004) COMP.
           05  CSEQ4F PIC  X(0001).
           05  FILLER REDEFINES CSEQ4F.
               10  CSEQ4A PIC  X(0001).
           05  CSEQ4I PIC  X(0004).
      *    -------------------------------
           05  CCERT4L PIC S9(0004) COMP.
           05  CCERT4F PIC  X(0001).
           05  FILLER REDEFINES CCERT4F.
               10  CCERT4A PIC  X(0001).
           05  CCERT4I PIC  X(0010).
      *    -------------------------------
           05  CSFX4L PIC S9(0004) COMP.
           05  CSFX4F PIC  X(0001).
           05  FILLER REDEFINES CSFX4F.
               10  CSFX4A PIC  X(0001).
           05  CSFX4I PIC  X(0001).
      *    -------------------------------
           05  CEFFDT4L PIC S9(0004) COMP.
           05  CEFFDT4F PIC  X(0001).
           05  FILLER REDEFINES CEFFDT4F.
               10  CEFFDT4A PIC  X(0001).
           05  CEFFDT4I PIC  9(6).
      *    -------------------------------
           05  CLSTNM4L PIC S9(0004) COMP.
           05  CLSTNM4F PIC  X(0001).
           05  FILLER REDEFINES CLSTNM4F.
               10  CLSTNM4A PIC  X(0001).
           05  CLSTNM4I PIC  X(0015).
      *    -------------------------------
           05  CKIND7L PIC S9(0004) COMP.
           05  CKIND7F PIC  X(0001).
           05  FILLER REDEFINES CKIND7F.
               10  CKIND7A PIC  X(0001).
           05  CKIND7I PIC  X(0002).
      *    -------------------------------
           05  CCANDT7L PIC S9(0004) COMP.
           05  CCANDT7F PIC  X(0001).
           05  FILLER REDEFINES CCANDT7F.
               10  CCANDT7A PIC  X(0001).
           05  CCANDT7I PIC  9(6).
      *    -------------------------------
           05  CRFUND7L PIC S9(0004) COMP.
           05  CRFUND7F PIC  X(0001).
           05  FILLER REDEFINES CRFUND7F.
               10  CRFUND7A PIC  X(0001).
           05  CRFUND7I PIC  S9(9)V9(2).
      *    -------------------------------
           05  CMTHD7L PIC S9(0004) COMP.
           05  CMTHD7F PIC  X(0001).
           05  FILLER REDEFINES CMTHD7F.
               10  CMTHD7A PIC  X(0001).
           05  CMTHD7I PIC  X(0001).
      *    -------------------------------
           05  CKIND8L PIC S9(0004) COMP.
           05  CKIND8F PIC  X(0001).
           05  FILLER REDEFINES CKIND8F.
               10  CKIND8A PIC  X(0001).
           05  CKIND8I PIC  X(0002).
      *    -------------------------------
           05  CCANDT8L PIC S9(0004) COMP.
           05  CCANDT8F PIC  X(0001).
           05  FILLER REDEFINES CCANDT8F.
               10  CCANDT8A PIC  X(0001).
           05  CCANDT8I PIC  9(6).
      *    -------------------------------
           05  CRFUND8L PIC S9(0004) COMP.
           05  CRFUND8F PIC  X(0001).
           05  FILLER REDEFINES CRFUND8F.
               10  CRFUND8A PIC  X(0001).
           05  CRFUND8I PIC  S9(9)V9(2).
      *    -------------------------------
           05  CMTHD8L PIC S9(0004) COMP.
           05  CMTHD8F PIC  X(0001).
           05  FILLER REDEFINES CMTHD8F.
               10  CMTHD8A PIC  X(0001).
           05  CMTHD8I PIC  X(0001).
      *    -------------------------------
           05  CCHK4L PIC S9(0004) COMP.
           05  CCHK4F PIC  X(0001).
           05  FILLER REDEFINES CCHK4F.
               10  CCHK4A PIC  X(0001).
           05  CCHK4I PIC  X(0001).
      *    -------------------------------
           05  CPAYEE4L PIC S9(0004) COMP.
           05  CPAYEE4F PIC  X(0001).
           05  FILLER REDEFINES CPAYEE4F.
               10  CPAYEE4A PIC  X(0001).
           05  CPAYEE4I PIC  X(0006).
      *    -------------------------------
           05  CLIVES4L PIC S9(0004) COMP.
           05  CLIVES4F PIC  X(0001).
           05  FILLER REDEFINES CLIVES4F.
               10  CLIVES4A PIC  X(0001).
           05  CLIVES4I PIC  9(3).
      *    -------------------------------
           05  CCANRN4L PIC S9(0004) COMP.
           05  CCANRN4F PIC  X(0001).
           05  FILLER REDEFINES CCANRN4F.
               10  CCANRN4A PIC  X(0001).
           05  CCANRN4I PIC  X(0001).
      *    -------------------------------
           05  CERMSG1L PIC S9(0004) COMP.
           05  CERMSG1F PIC  X(0001).
           05  FILLER REDEFINES CERMSG1F.
               10  CERMSG1A PIC  X(0001).
           05  CERMSG1I PIC  X(0079).
      *    -------------------------------
           05  CERMSG2L PIC S9(0004) COMP.
           05  CERMSG2F PIC  X(0001).
           05  FILLER REDEFINES CERMSG2F.
               10  CERMSG2A PIC  X(0001).
           05  CERMSG2I PIC  X(0079).
      *    -------------------------------
           05  CPFENTRL PIC S9(0004) COMP.
           05  CPFENTRF PIC  X(0001).
           05  FILLER REDEFINES CPFENTRF.
               10  CPFENTRA PIC  X(0001).
           05  CPFENTRI PIC  9(2).
      *    -------------------------------
           05  CDELHDGL PIC S9(0004) COMP.
           05  CDELHDGF PIC  X(0001).
           05  FILLER REDEFINES CDELHDGF.
               10  CDELHDGA PIC  X(0001).
           05  CDELHDGI PIC  X(0017).
       01  VP630CO REDEFINES VP630BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CBATCHO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMOENDO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CACCTNMO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSEQ1O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCERT1O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSFX1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CEFFDT1O PIC  999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLSTNM1O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CKIND1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCANDT1O PIC  999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRFUND1O PIC  9999999.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMTHD1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CKIND2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCANDT2O PIC  999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRFUND2O PIC  9999999.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMTHD2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCHK1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPAYEE1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLIVES1O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCANRN1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSEQ2O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCERT2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSFX2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CEFFDT2O PIC  999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLSTNM2O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CKIND3O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCANDT3O PIC  999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRFUND3O PIC  9999999.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMTHD3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CKIND4O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCANDT4O PIC  999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRFUND4O PIC  9999999.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMTHD4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCHK2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPAYEE2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLIVES2O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCANRN2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSEQ3O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCERT3O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSFX3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CEFFDT3O PIC  999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLSTNM3O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CKIND5O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCANDT5O PIC  999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRFUND5O PIC  9999999.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMTHD5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CKIND6O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCANDT6O PIC  999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRFUND6O PIC  9999999.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMTHD6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCHK3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPAYEE3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLIVES3O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCANRN3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSEQ4O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCERT4O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSFX4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CEFFDT4O PIC  999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLSTNM4O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CKIND7O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCANDT7O PIC  999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRFUND7O PIC  9999999.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMTHD7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CKIND8O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCANDT8O PIC  999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRFUND8O PIC  9999999.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMTHD8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCHK4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPAYEE4O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLIVES4O PIC  999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCANRN4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPFENTRO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CDELHDGO PIC  X(0017).
      *    -------------------------------
       01  MAP-B REDEFINES VP630BI.
           12  FILLER                      PIC X(42).
           12  DATA-AREA-B.
               16  BSEQ-LEN                PIC S9(4)  COMP.
               16  BSEQ-ATTRB              PIC X.
               16  BSEQ                    PIC 9(4).
               16  BMO-END-LEN             PIC S9(4)  COMP.
               16  BMO-END-ATTRB           PIC X.
               16  BMO-END                 PIC X(8).
               16  BACCT-NM-LEN            PIC S9(4)  COMP.
               16  BACCT-NM-ATTRB          PIC X.
               16  BACCT-NM                PIC X(30).
               16  BEFFDT-LEN              PIC S9(4)  COMP.
               16  BEFFDT-ATTRB            PIC X.
               16  BEFFDT                  PIC X(6).
               16  BCERT-LEN               PIC S9(4)  COMP.
               16  BCERT-ATTRB             PIC X.
               16  BCERT                   PIC X(10).
               16  BSFX-LEN                PIC S9(4)  COMP.
               16  BSFX-ATTRB              PIC X.
               16  BSFX                    PIC X.
               16  BLN-OFFICER-LEN         PIC S9(4)  COMP.
               16  BLN-OFFICER-ATTRB       PIC X.
               16  BLN-OFFICER             PIC X(5).
               16  B1ST-NAME-LEN           PIC S9(4)  COMP.
               16  B1ST-NAME-ATTRB         PIC X.
               16  B1ST-NAME               PIC X(10).
               16  BINIT-LEN               PIC S9(4)  COMP.
               16  BINIT-ATTRB             PIC X.
               16  BINIT                   PIC X.
               16  BLAST-NAME-LEN          PIC S9(4)  COMP.
               16  BLAST-NAME-ATTRB        PIC X.
               16  BLAST-NAME              PIC X(15).
               16  BAGE-LEN                PIC S9(4)  COMP.
               16  BAGE-ATTRB              PIC X.
               16  BAGE                    PIC 99.
               16  BSEX-LEN                PIC S9(4)  COMP.
               16  BSEX-ATTRB              PIC X.
               16  BSEX                    PIC X.
               16  BJNT-1ST-NAME-LEN       PIC S9(4)   COMP.
               16  BJNT-1ST-NAME-ATTRB     PIC X.
               16  BJNT-1ST-NAME           PIC X(10).
               16  BJNT-INIT-LEN           PIC S9(4)   COMP.
               16  BJNT-INIT-ATTRB         PIC X.
               16  BJNT-INIT               PIC X.
               16  BJNT-LST-NAME-LEN       PIC S9(4)   COMP.
               16  BJNT-LST-NAME-ATTRB     PIC X.
               16  BJNT-LST-NAME           PIC X(15).
               16  BJNT-AGE-LEN            PIC S9(4)   COMP.
               16  BJNT-AGE-ATTRB          PIC X.
               16  BJNT-AGE                PIC 99.
               16  BADDRS1-LEN             PIC S9(4)  COMP.
               16  BADDRS1-ATTRB           PIC X.
               16  BADDRS1                 PIC X(30).
               16  BADDRS2-LEN             PIC S9(4)  COMP.
               16  BADDRS2-ATTRB           PIC X.
               16  BADDRS2                 PIC X(30).
               16  BCITY-LEN               PIC S9(4)  COMP.
               16  BCITY-ATTRB             PIC X.
               16  BCITY                   PIC X(28).
               16  BSTATE-LEN              PIC S9(4)  COMP.
               16  BSTATE-ATTRB            PIC X.
               16  BSTATE                  PIC XX.
               16  BZIPCDE-LEN             PIC S9(4)  COMP.
               16  BZIPCDE-ATTRB           PIC X.
               16  BZIPCDE                 PIC X(10).
               16  BYEAR-LEN               PIC S9(4)  COMP.
               16  BYEAR-ATTRB             PIC X.
               16  BYEAR                   PIC X(4).
               16  BMAKE-LEN               PIC S9(4)  COMP.
               16  BMAKE-ATTRB             PIC X.
               16  BMAKE                   PIC X(20).
               16  BMODEL-LEN              PIC S9(4)  COMP.
               16  BMODEL-ATTRB            PIC X.
               16  BMODEL                  PIC X(20).
               16  BFUTURE-LEN             PIC S9(4)  COMP.
               16  BFUTURE-ATTRB           PIC X.
               16  BFUTURE                 PIC X(20).
               16  BVINHD-LEN              PIC S9(4)  COMP.
               16  BVINHD-ATTRB            PIC X.
               16  BVINNDI                 PIC X(5).
               16  BVIN-LEN                PIC S9(4)  COMP.
               16  BVIN-ATTRB              PIC X.
               16  BVIN-NOI                PIC X(17).
               16  BOMETER-LEN             PIC S9(4)  COMP.
               16  BOMETER-ATTRB           PIC X.
               16  BOMETER-in              PIC x(7).
               16  BOMETER-out REDEFINES BOMETER-in
                                           PIC 999,999.
               16  BBENEFICIARY-LEN        PIC S9(4)   COMP.
               16  BBENEFICIARY-ATTRB      PIC X.
               16  BBENEFICIARY            PIC X(25).
               16  BCADDR1-LEN             PIC S9(4)  COMP.
               16  BCADDR1-ATTRB           PIC X.
               16  BCADDR1                 PIC X(30).
               16  BCADDR2-LEN             PIC S9(4)  COMP.
               16  BCADDR2-ATTRB           PIC X.
               16  BCADDR2                 PIC X(30).
               16  BCCITY-LEN              PIC S9(4)  COMP.
               16  BCCITY-ATTRB            PIC X.
               16  BCCITY                  PIC X(28).
               16  BCSTATE-LEN             PIC S9(4)  COMP.
               16  BCSTATE-ATTRB           PIC X.
               16  BCSTATE                 PIC XX.
               16  BCZIPCD-LEN             PIC S9(4)  COMP.
               16  BCZIPCD-ATTRB           PIC X.
               16  BCZIPCD                 PIC X(10).
               16  BKIND1-LEN           PIC S9(4)  COMP.
               16  BKIND1-ATTRB         PIC X.
               16  BKIND1               PIC XX.
               16  BTYPE1-LEN           PIC S9(4)  COMP.
               16  BTYPE1-ATTRB         PIC X.
               16  BTYPE1               PIC X(3).
               16  BTERM1-LEN           PIC S9(4)  COMP.
               16  BTERM1-ATTRB         PIC X.
               16  BTERM1I              PIC 999.
               16  BTERM1O REDEFINES
                              BTERM1I   PIC ZZZ.
               16  BBENE1-LEN            PIC S9(4)  COMP.
               16  BBENE1-ATTRB          PIC X.
               16  BBENE1I               PIC 9(10)V99.
      *        16  BBENE1I               PIC 9(12).
               16  BBENE1O REDEFINES
                                 BBENE1I PIC Z(9).99.
               16  BPREM1-LEN           PIC S9(4)  COMP.
               16  BPREM1-ATTRB         PIC X.
               16  BPREM1I              PIC 9(9)V99.
      *        16  BPREM1I              PIC 9(11).
               16  BPREM1O REDEFINES
                                BPREM1I PIC Z(7).99-.
               16  BALT-BEN1-LEN        PIC S9(4)  COMP.
               16  BALT-BEN1-ATTRB      PIC X.
               16  BALT-BEN1I           PIC 9(10)V99.
      *        16  BALT-BEN1I           PIC 9(12).
               16  BALT-BEN1O REDEFINES
                                 BALT-BEN1I PIC Z(9).ZZ.
               16  BALT-PREM1-LEN       PIC S9(4)  COMP.
               16  BALT-PREM1-ATTRB     PIC X.
               16  BALT-PREM1I          PIC 9(7)V99.
      *        16  BALT-PREM1I          PIC 9(9).
               16  BALT-PREM1O REDEFINES
                                 BALT-PREM1I PIC Z(6).ZZ.
               16  BKIND2-LEN           PIC S9(4)  COMP.
               16  BKIND2-ATTRB         PIC X.
               16  BKIND2               PIC XX.
               16  BTYPE2-LEN           PIC S9(4)  COMP.
               16  BTYPE2-ATTRB         PIC X.
               16  BTYPE2               PIC X(3).
               16  BTERM2-LEN           PIC S9(4)  COMP.
               16  BTERM2-ATTRB         PIC X.
               16  BTERM2I              PIC 999.
               16  BTERM2O REDEFINES
                              BTERM2I   PIC ZZZ.
               16  BBENE2-LEN            PIC S9(4)  COMP.
               16  BBENE2-ATTRB          PIC X.
               16  BBENE2I               PIC 9(10)V99.
      *        16  BBENE2I               PIC 9(12).
               16  BBENE2O REDEFINES
                                 BBENE2I PIC Z(9).99.
               16  BPREM2-LEN           PIC S9(4)  COMP.
               16  BPREM2-ATTRB         PIC X.
               16  BPREM2I              PIC 9(9)V99.
      *        16  BPREM2I              PIC 9(11).
               16  BPREM2O REDEFINES
                                BPREM2I PIC Z(7).99-.
               16  BCRIT-PERD2-LEN      PIC S9(4)  COMP.
               16  BCRIT-PERD2-ATTRB    PIC X.
               16  BCRIT-PERD2I         PIC 99.
               16  BCRIT-PERD2O REDEFINES
                       BCRIT-PERD2I PIC ZZ.
               16  BALT-BEN2-LEN        PIC S9(4)  COMP.
               16  BALT-BEN2-ATTRB      PIC X.
               16  BALT-BEN2I           PIC 9(10)V99.
      *        16  BALT-BEN2I           PIC 9(12).
               16  BALT-BEN2O REDEFINES
                                 BALT-BEN2I PIC Z(9).ZZ.
               16  BALT-PREM2-LEN       PIC S9(4)  COMP.
               16  BALT-PREM2-ATTRB     PIC X.
               16  BALT-PREM2I          PIC 9(7)V99.
      *        16  BALT-PREM2I          PIC 9(9).
               16  BALT-PREM2O REDEFINES
                                 BALT-PREM2I PIC Z(6).ZZ.
       01  MAP-C REDEFINES VP630BI.
           12  FILLER                  PIC X(86).
           12  DATA-AREA-C             OCCURS 4 TIMES.
               16  CSEQ-LEN                PIC S9(4)  COMP.
               16  CSEQ-ATTRB              PIC X.
               16  CSEQ                    PIC 9(4).
               16  CCERT-LEN               PIC S9(4)  COMP.
               16  CCERT-ATTRB             PIC X.
               16  CCERT                   PIC X(10).
               16  CSFX-LEN                PIC S9(4)  COMP.
               16  CSFX-ATTRB              PIC X.
               16  CSFX                    PIC X.
               16  CEFFDT-LEN              PIC S9(4)  COMP.
               16  CEFFDT-ATTRB            PIC X.
               16  CEFFDT                  PIC 9(6).
               16  CLAST-NAME-LEN          PIC S9(4)  COMP.
               16  CLAST-NAME-ATTRB        PIC X.
               16  CLAST-NAME              PIC X(15).
               16  CANCEL-INFO.
                   20  CKIND1-LEN          PIC S9(4)  COMP.
                   20  CKIND1-ATTRB        PIC X.
                   20  CKIND1              PIC XX.
                   20  CCANDT1-LEN         PIC S9(4)  COMP.
                   20  CCANDT1-ATTRB       PIC X.
                   20  CCANDT1             PIC 9(6).
                   20  CREFUND1-LEN        PIC S9(4)  COMP.
                   20  CREFUND1-ATTRB      PIC X.
                   20  CREFUND1I           PIC S9(9)V99.
      *            20  CREFUND1I           PIC X(11).
                   20  CREFUND1O REDEFINES
                                 CREFUND1I PIC Z(7).99-.
                   20  CMTHD1-LEN          PIC S9(4)  COMP.
                   20  CMTHD1-ATTRB        PIC X.
                   20  CMTHD1              PIC X.
                   20  CKIND2-LEN          PIC S9(4)  COMP.
                   20  CKIND2-ATTRB        PIC X.
                   20  CKIND2              PIC XX.
                   20  CCANDT2-LEN         PIC S9(4)  COMP.
                   20  CCANDT2-ATTRB       PIC X.
                   20  CCANDT2             PIC 9(6).
                   20  CREFUND2-LEN        PIC S9(4)  COMP.
                   20  CREFUND2-ATTRB      PIC X.
                   20  CREFUND2I           PIC S9(9)V99.
      *            20  CREFUND2I           PIC X(11).
                   20  CREFUND2O REDEFINES
                                 CREFUND2I PIC Z(7).99-.
                   20  CMTHD2-LEN          PIC S9(4)  COMP.
                   20  CMTHD2-ATTRB        PIC X.
                   20  CMTHD2              PIC X.
                   20  CCHK-LEN            PIC S9(4)  COMP.
                   20  CCHK-ATTRB          PIC X.
                   20  CCHK                PIC X.
                   20  CPAYEE-LEN          PIC S9(4)  COMP.
                   20  CPAYEE-ATTRB        PIC X.
                   20  CPAYEE              PIC X(6).
                   20  CLIVES-LEN          PIC S9(4)  COMP.
                   20  CLIVES-ATTRB        PIC X.
                   20  CLIVESI             PIC 999.
                   20  CLIVESO REDEFINES
                                CLIVESI    PIC ZZZ.
                   20  CCANREA-LEN         PIC S9(4)  COMP.
                   20  CCANREA-ATTRB       PIC X.
                   20  CCANREA             PIC X.
           EJECT
      ****************************************************************
      *                                                               
      * Copyright (c) 2007-2013 Dell Inc.                             
      * All rights reserved.                                          
      *                                                               
      ****************************************************************
       01  DFHEIV.                                                    
         02  DFHEIV0               PIC X(35).                         
         02  DFHEIV1               PIC X(08).                         
         02  DFHEIV2               PIC X(08).                         
         02  DFHEIV3               PIC X(08).                         
         02  DFHEIV4               PIC X(06).                         
         02  DFHEIV5               PIC X(04).                         
         02  DFHEIV6               PIC X(04).                         
         02  DFHEIV7               PIC X(02).                         
         02  DFHEIV8               PIC X(02).                         
         02  DFHEIV9               PIC X(01).                         
         02  DFHEIV10              PIC S9(7) COMP-3.                  
         02  DFHEIV11              PIC S9(4) COMP SYNC.               
         02  DFHEIV12              PIC S9(4) COMP SYNC.               
         02  DFHEIV13              PIC S9(4) COMP SYNC.               
         02  DFHEIV14              PIC S9(4) COMP SYNC.               
         02  DFHEIV15              PIC S9(4) COMP SYNC.               
         02  DFHEIV16              PIC S9(9) COMP SYNC.               
         02  DFHEIV17              PIC X(04).                         
         02  DFHEIV18              PIC X(04).                         
         02  DFHEIV19              PIC X(04).                         
         02  DFHEIV20              USAGE IS POINTER.                  
         02  DFHEIV21              USAGE IS POINTER.                  
         02  DFHEIV22              USAGE IS POINTER.                  
         02  DFHEIV23              USAGE IS POINTER.                  
         02  DFHEIV24              USAGE IS POINTER.                  
         02  DFHEIV25              PIC S9(9) COMP SYNC.               
         02  DFHEIV26              PIC S9(9) COMP SYNC.               
         02  DFHEIV27              PIC S9(9) COMP SYNC.               
         02  DFHEIV28              PIC S9(9) COMP SYNC.               
         02  DFHEIV29              PIC S9(9) COMP SYNC.               
         02  DFHEIV30              PIC S9(9) COMP SYNC.               
         02  DFHEIV31              PIC S9(9) COMP SYNC.               
         02  DFHEIV32              PIC S9(4) COMP SYNC.               
         02  DFHEIV33              PIC S9(4) COMP SYNC.               
         02  DFHEIV34              PIC S9(4) COMP SYNC.               
         02  DFHEIV35              PIC S9(4) COMP SYNC.               
         02  DFHEIV97              PIC S9(7) COMP-3 VALUE ZERO.       
         02  DFHEIV98              PIC S9(4) COMP SYNC VALUE ZERO.    
         02  FILLER                PIC X(02).                         
         02  DFHEIV99              PIC X(08) VALUE SPACE.             
         02  DFHEIVL0              PIC X(48) VALUE SPACE.             
         02  DFHEIVL1              PIC X(48) VALUE SPACE.             
         02  DFHEIVL2              PIC X(48) VALUE SPACE.             
         02  DFHEIVL3              PIC X(48) VALUE SPACE.             
         02  DFHEIVL4              PIC X(255) VALUE SPACE.            
         02  DFHEIVL5              PIC X(255) VALUE SPACE.            
       LINKAGE  SECTION.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007-2013 Dell Inc.                             *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       01  dfheiblk.
           02  eibtime          pic s9(7) comp-3.
           02  eibdate          pic s9(7) comp-3.
           02  eibtrnid         pic x(4).
           02  eibtaskn         pic s9(7) comp-3.
           02  eibtrmid         pic x(4).
           02  dfheigdi         pic s9(4) comp.
           02  eibcposn         pic s9(4) comp.
           02  eibcalen         pic s9(4) comp.
           02  eibaid           pic x(1).
           02  eibfiller1       pic x(1).
           02  eibfn            pic x(2).
           02  eibfiller2       pic x(2).
           02  eibrcode         pic x(6).
           02  eibfiller3       pic x(2).
           02  eibds            pic x(8).
           02  eibreqid         pic x(8).
           02  eibrsrce         pic x(8).
           02  eibsync          pic x(1).
           02  eibfree          pic x(1).
           02  eibrecv          pic x(1).
           02  eibsend          pic x(1).
           02  eibatt           pic x(1).
           02  eibeoc           pic x(1).
           02  eibfmh           pic x(1).
           02  eibcompl         pic x(1).
           02  eibsig           pic x(1).
           02  eibconf          pic x(1).
           02  eiberr           pic x(1).
           02  eibrldbk         pic x(1).
           02  eiberrcd         pic x(4).
           02  eibsynrb         pic x(1).
           02  eibnodat         pic x(1).
           02  eibfiller5       pic x(2).
           02  eibresp          pic s9(8) comp.
           02  eibresp2         pic s9(8) comp.
           02  dfheigdj         pic s9(4) comp.
           02  dfheigdk         pic s9(4) comp.
       01  DFHCOMMAREA             PIC X(1900).
           EJECT
      *01 PARMLIST .
      *    02  FILLER              PIC S9(8)   COMP.
      *    02  ERPNDB-POINTER      PIC S9(8)   COMP.
      *    02  ELCNTL-POINTER      PIC S9(8)   COMP.
      *    02  ELCERT-POINTER      PIC S9(8)   COMP.
      *    02  ERPNDM-POINTER      PIC S9(8)   COMP.
      *                                COPY ERCPNDB.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCPNDB.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.025                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = PENDING NEW BUSINESS (ISSUES AND CANCELS) *
00008 *                                                                *
00009 ******************************************************************
00010 *   NOTE: IF THIS FORMAT IS CHANGED, THE SORT RECORD IN THE      *
00011 *         EL861 A/R SYSTEM MAY NEED TO BE CHANGED.               *
00012 ******************************************************************
00013 *                                                                *
00014 *                                                                *
00015 *   FILE TYPE = VSAM,KSDS                                        *
00016 *   RECORD SIZE = 585  RECFORM = FIXED                           *
00017 *                                                                *
00018 *   BASE CLUSTER = ERPNDB                         RKP=2,LEN=11   *
00019 *       ALTERNATE PATH1 = ERPNDB2  (BY CAR GRP STATE ACCOUNT     *
00020 *                                  EFF-DT CERT CHG-SEQ REC-TYPE) *
00021 *                                                 RKP=13,LEN=36  *
00022 *       ALTERNATE PATH2 = ERPNDB3  (BY CO, ORIGINAL BATCH, SEQ.  *
00023 *                                      AND CHG-SEQ.)             *
00024 *                                                RKP=49,LEN=11   *
00025 *       ALTERNATE PATH3 = ERPNDB4  (BY CO, CSR-ID, BATCH, SEQ.   *
00026 *                                      AND CHG-SEQ.)             *
00027 *                                                RKP=60,LEN=15   *
00028 *                                                                *
00029 *   LOG = NO                                                     *
00030 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00031 ******************************************************************
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
011904* 011904                   PEMA  ADD TOTAL FEE PROCESSING
040504* 040504    2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
020305* 020305    2005020000000  PEMA  ADD CLP STATE TO PNDB RECORD
110105* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
032306* 032306                   PEMA  ADD BOW LOAN NUMBER
081606* 081606    2006080800002  PEMA  ADD VIN, ISSUES ONLY
073107* 073107  CR2006051600002  PEMA  ADD OVERCHARGE PROCESSING
072308* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
090408* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
032109* 032109    2009021700002  PEMA  ADD CRED BENE TO PNDB REC
072209* 072209  CR2008101500003  AJRA  ADD BORROWER FIRST NAME TO ENDORS
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
071211* 071211  CR2010012700001  PEMA  ADD SPP DEALER DIRECT
073114* 073114  CR2014012300001  PEMA  ADD CU CARRIER 7 PROCESSING
010716* 010716    2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
010517* 010517  CR2016021600005  PEMA ADD NEW FORCE CODE FOR AGG
062017* 062017  CR2015091000001  PEMA RENAME INTEREST FIELD
122002******************************************************************
00032
00033  01  PENDING-BUSINESS.
00034      12  PB-RECORD-ID                     PIC XX.
00035          88  VALID-PB-ID                        VALUE 'PB'.
00036
00037      12  PB-CONTROL-PRIMARY.
00038          16  PB-COMPANY-CD                PIC X.
00039          16  PB-ENTRY-BATCH               PIC X(6).
00040          16  PB-BATCH-SEQ-NO              PIC S9(4)     COMP.
00041          16  PB-BATCH-CHG-SEQ-NO          PIC S9(4)     COMP.
00042
00043      12  PB-CONTROL-BY-ACCOUNT.
00044          16  PB-COMPANY-CD-A1             PIC X.
00045          16  PB-CARRIER                   PIC X.
00046          16  PB-GROUPING.
00047              20  PB-GROUPING-PREFIX       PIC XXX.
00048              20  PB-GROUPING-PRIME        PIC XXX.
00049          16  PB-STATE                     PIC XX.
00050          16  PB-ACCOUNT.
00051              20  PB-ACCOUNT-PREFIX        PIC X(4).
00052              20  PB-ACCOUNT-PRIME         PIC X(6).
00053          16  PB-CERT-EFF-DT               PIC XX.
00054          16  PB-CERT-NO.
00055              20  PB-CERT-PRIME            PIC X(10).
00056              20  PB-CERT-SFX              PIC X.
00057          16  PB-ALT-CHG-SEQ-NO            PIC S9(4)     COMP.
00058
00059          16  PB-RECORD-TYPE               PIC X.
00060              88  PB-MAILING-DATA                VALUE '0'.
00061              88  PB-ISSUE                       VALUE '1'.
00062              88  PB-CANCELLATION                VALUE '2'.
00063              88  PB-BATCH-TRAILER               VALUE '9'.
00064
00065      12  PB-CONTROL-BY-ORIG-BATCH.
00066          16  PB-ORIGINAL-COMPANY-CD       PIC X.
00067          16  PB-ORIGINAL-ENTRY-BATCH      PIC X(6).
00068          16  PB-ORIGINAL-SEQ-NO           PIC S9(4)     COMP.
00069          16  PB-ORIGINAL-CHG-SEQ-NO       PIC S9(4)     COMP.
00070
00071      12  PB-CONTROL-BY-CSR.
00072          16  PB-CSR-COMPANY-CD            PIC X.
00073          16  PB-CSR-ID                    PIC X(4).
00074          16  PB-CSR-ENTRY-BATCH           PIC X(6).
00075          16  PB-CSR-BATCH-SEQ-NO          PIC S9(4)     COMP.
00076          16  PB-CSR-BATCH-CHG-SEQ-NO      PIC S9(4)     COMP.
00077 ******************************************************************
00078 *    MAILING DATA IS PROCESSED IN ONLY PRGS. EL512 & EL513       *
00079 ******************************************************************
00080
00081      12  PB-LAST-MAINT-DT                 PIC XX.
00082      12  PB-LAST-MAINT-BY                 PIC X(4).
00083      12  PB-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.
00084
00085      12  PB-RECORD-BODY                   PIC X(375).
00086
00087      12  PB-ISSUE-RECORD   REDEFINES PB-RECORD-BODY.
00088          16  PB-CERT-ORIGIN               PIC X.
00089              88  CLASIC-CREATED-CERT         VALUE '1'.
00090          16  PB-I-NAME.
00091              20  PB-I-INSURED-LAST-NAME   PIC X(15).
00092              20  PB-I-INSURED-FIRST-NAME.
00093                  24  PB-I-INSURED-1ST-INIT PIC X.
00094                  24  FILLER                PIC X(9).
00095              20  PB-I-INSURED-MIDDLE-INIT PIC X.
00096          16  PB-I-AGE                     PIC S99   COMP-3.
00097          16  PB-I-JOINT-AGE               PIC S99   COMP-3.
00098          16  PB-I-BIRTHDAY                PIC XX.
00099          16  PB-I-INSURED-SEX             PIC X.
00100              88  PB-SEX-MALE     VALUE 'M'.
00101              88  PB-SEX-FEMALE   VALUE 'F'.
00102
00103          16  PB-I-LF-TERM                 PIC S999   COMP-3.
00104          16  PB-I-AH-TERM                 PIC S999   COMP-3.
00105          16  PB-I-LOAN-TERM               PIC S999   COMP-3.
00106          16  PB-I-PAY-FREQUENCY           PIC S99    COMP-3.
00107          16  PB-I-SKIP-CODE               PIC X.
00108              88  PB-NO-MONTHS-SKIPPED      VALUE ' ' '0'.
00109              88  PB-SKIP-JULY              VALUE '1'.
00110              88  PB-SKIP-AUGUST            VALUE '2'.
00111              88  PB-SKIP-SEPTEMBER         VALUE '3'.
00112              88  PB-SKIP-JULY-AUG          VALUE '4'.
00113              88  PB-SKIP-AUG-SEPT          VALUE '5'.
00114              88  PB-SKIP-JULY-AUG-SEPT     VALUE '6'.
00115              88  PB-SKIP-JUNE-JULY-AUG     VALUE '7'.
00116              88  PB-SKIP-JUNE              VALUE '8'.
00117              88  PB-SKIP-JUNE-JULY         VALUE '9'.
00118              88  PB-SKIP-AUG-SEPT-OCT      VALUE 'A'.
00119              88  PB-SKIP-BI-WKLY-3RD-PMT   VALUE 'X'.
00120          16  PB-I-TERM-TYPE               PIC X.
00121              88  PB-PAID-MONTHLY           VALUE ' ' 'M'.
00122              88  PB-PAID-WEEKLY            VALUE 'W'.
00123              88  PB-PAID-SEMI-MONTHLY      VALUE 'S'.
00124              88  PB-PAID-BI-WEEKLY         VALUE 'B'.
00125              88  PB-PAID-13-YEARLY         VALUE 'T'.
00126          16  PB-I-NO-OF-PAYMENTS          PIC S999   COMP-3.
00127          16  PB-I-POLICY-FORM-NO          PIC X(12).
00128          16  PB-I-DATA-ENTRY-SW           PIC X.
00129              88  PB-EFF-DT-PROCESSING      VALUE '1' ' '.
00130              88  PB-EXT-DAYS-PROCESSING    VALUE '2'.
00131              88  PB-EXPIRE-DT-PROCESSING   VALUE '3'.
00132              88  PB-1ST-PMT-DT-PROCESSING  VALUE '4'.
00133          16  PB-I-PAYMENT-AMOUNT          PIC S9(7)V99  COMP-3.
073107         16  PB-I-DCC-OVER-CHG-AMT        PIC S9(5)V99  COMP-3.
011410*        16  PB-I-MICROFILM-NO            PIC S9(9)      COMP-3.
011410         16  PB-I-AH-CLP                  PIC S9(5)V99 COMP-3.
011410         16  FILLER                       PIC X.
00136
00137          16  PB-I-LIFE-BENEFIT-CD         PIC XX.
00138              88  PB-VALID-LIFE               VALUE '01' THRU '89'.
00139              88  PB-INVALID-LIFE             VALUE '  ' '00'
00140                                                    '90' THRU '99'.
00141          16  PB-I-LF-BENEFIT-CD   REDEFINES PB-I-LIFE-BENEFIT-CD
00142                                           PIC XX.
00143          16  PB-I-LF-BENEFIT-AMT          PIC S9(9)V99   COMP-3.
100703         16  PB-I-AMOUNT-FINANCED REDEFINES
100703                  PB-I-LF-BENEFIT-AMT     PIC S9(9)V99   COMP-3.
00144          16  PB-I-LF-ALT-BENEFIT-AMT      PIC S9(9)V99   COMP-3.
100703         16  PB-I-UNPAID-CASH-PRICE REDEFINES
100703                  PB-I-LF-ALT-BENEFIT-AMT PIC S9(9)V99   COMP-3.
00145          16  PB-I-LF-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
00146          16  PB-I-LF-ALT-PREMIUM-AMT      PIC S9(7)V99   COMP-3.
100703         16  PB-I-CLP-AMOUNT REDEFINES
100703                  PB-I-LF-ALT-PREMIUM-AMT PIC S9(7)V99   COMP-3.
00147          16  PB-I-LF-CALC-FLAG            PIC X.
00148              88 PB-COMP-LF-PREM               VALUE '?'.
00149          16  PB-I-LF-PREM-CALC            PIC S9(7)V99   COMP-3.
00150          16  PB-I-LF-ALT-PREM-CALC        PIC S9(7)V99   COMP-3.
00151          16  PB-I-LF-RATE                 PIC S99V9(5)   COMP-3.
00152          16  PB-I-LF-ALT-RATE             PIC S99V9(5)   COMP-3.
00153          16  PB-I-LF-POLICY-FEE           PIC S9(3)V99   COMP-3.
00154          16  PB-I-LF-REI-RATE             PIC S99V9(5)   COMP-3.
00155          16  PB-I-LF-ALT-REI-RATE         PIC S99V9(5)   COMP-3.
00156          16  PB-I-LF-ABBR                 PIC XXX.
00157          16  PB-I-LF-INPUT-CD             PIC XX.
00158
00159          16  PB-I-AH-BENEFIT-CD           PIC XX.
00160              88  PB-VALID-AH                 VALUE '01' THRU '89'.
00161              88  PB-INVALID-AH               VALUE '  ' '00'
00162                                                    '90' THRU '99'.
00163          16  PB-I-AH-BENEFIT-AMT          PIC S9(7)V99   COMP-3.
00164          16  PB-I-AH-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
00165          16  PB-I-AH-CALC-FLAG            PIC X.
00166              88 PB-COMP-AH-PREM                  VALUE '?'.
00167          16  PB-I-AH-PREM-CALC            PIC S9(7)V99   COMP-3.
00168          16  PB-I-AH-RATE                 PIC S99V9(5)   COMP-3.
010716         16  PB-I-CANCEL-FEE              PIC S9(3)V99   COMP-3.
00170          16  PB-I-AH-REI-RATE             PIC S99V9(5)   COMP-3.
00171          16  PB-I-AH-RATE-TRM             PIC S999       COMP-3.
00172          16  PB-I-AH-ABBR                 PIC XXX.
00173          16  PB-I-AH-INPUT-CD             PIC XXX.
00174
00175          16  PB-I-SPECIAL-REIN-CODE       PIC X.
00176          16  PB-I-REIN-TABLE              PIC XXX.
00177          16  PB-I-BUSINESS-TYPE           PIC 99.
00178          16  PB-I-INDV-GRP-CD             PIC X.
00179          16  PB-I-MORT-CODE.
00180              20  PB-I-TABLE               PIC X.
00181              20  PB-I-INTEREST            PIC XX.
00182              20  PB-I-MORT-TYP            PIC X.
00183          16  PB-I-LF-CRIT-PER             PIC S9(3)      COMP-3.
00184          16  PB-I-AH-CRIT-PER             PIC S9(3)      COMP-3.
011410         16  PB-I-LF-CLP                  PIC S9(5)V99   COMP-3.
00186          16  PB-I-INDV-GRP-OVRD           PIC X.
00187          16  PB-I-RATE-CLASS-OVRD         PIC XX.
00188          16  PB-I-SIG-SW                  PIC X.
00189              88  PB-POLICY-SIGNED             VALUE 'Y'.
00190          16  PB-I-RATE-CLASS              PIC XX.
00191          16  PB-I-RATE-DEVIATION-LF       PIC XXX.
00192          16  PB-I-RATE-DEVIATION-AH       PIC XXX.
00193          16  PB-I-RATE-DEV-PCT-LF         PIC S9V9(6)    COMP-3.
00194          16  PB-I-RATE-DEV-PCT-AH         PIC S9V9(6)    COMP-3.
00195          16  PB-I-LIFE-COMMISSION         PIC SV9(5)     COMP-3.
00196          16  PB-I-JOINT-COMMISSION        PIC SV9(5)     COMP-3.
00197          16  PB-I-AH-COMMISSION           PIC SV9(5)     COMP-3.
00198          16  PB-I-BENEFIT-TYPE            PIC XXX.
00199          16  PB-I-OB-FLAG                 PIC X.
00200              88  PB-I-OB                      VALUE 'B'.
00201              88  PB-I-SUMMARY                 VALUE 'Z'.
00202          16  PB-I-ENTRY-STATUS            PIC X.
00203              88  PB-I-POLICY-IS-ACTIVE        VALUE '1' '3' '4'
122002                                              'M' '5' '9' '2'.
00205              88  PB-I-NORMAL-ENTRY            VALUE '1'.
00206              88  PB-I-POLICY-PENDING          VALUE '2'.
00207              88  PB-I-CONVERSION-ENTRY        VALUE '4'.
00208              88  PB-I-POLICY-IS-REISSUE       VALUE '5'.
                   88  PB-I-POLICY-IS-CASH          VALUE 'C'.
122002             88  PB-I-POLICY-IS-MONTHLY       VALUE 'M'.
00209              88  PB-I-REIN-ONLY               VALUE '9'.
00210              88  PB-I-POLICY-IS-DECLINED      VALUE 'D'.
00211              88  PB-I-POLICY-IS-VOIDED        VALUE 'V'.
00212              88  PB-I-PREM-ACCTNG-ONLY        VALUE 'P'.
00213              88  PB-I-UNDERWRITE-POLICY       VALUE 'U'.
00214          16  PB-I-INT-CODE                PIC X.
00215              88  PB-ADD-ON-INTEREST           VALUE 'A'.
00216              88  PB-SIMPLE-INTEREST           VALUE 'S'.
00217          16  PB-I-LOAN-APR                PIC 9(3)V9(4)   COMP-3.
00218          16  PB-I-SOC-SEC-NO              PIC X(11).
00219          16  PB-I-MEMBER-NO               PIC X(12).
00220          16  PB-I-CURR-SEQ                PIC S9(4)       COMP.
110105*        16  PB-I-LOAN-OFFICER            PIC XXX.
110105         16  PB-I-OLD-LOF                 PIC XXX.
00222          16  PB-I-LF-EXPIRE-DT            PIC XX.
00223          16  PB-I-AH-EXPIRE-DT            PIC XX.
00224          16  PB-I-EXTENTION-DAYS          PIC S999        COMP-3.
00225          16  PB-I-TERM-IN-DAYS            PIC S9(5)       COMP-3.
00226          16  PB-I-LIFE-INDICATOR          PIC X.
00227              88  PB-I-JOINT-COVERAGE         VALUE 'J'.
00228          16  PB-I-LIVES                   PIC S9(7)       COMP-3.
071211         16  PB-I-DDF-IU-RATE-UP REDEFINES PB-I-LIVES
071211                                          PIC S9(5)V99    COMP-3.
00229          16  PB-I-MAIL-ADDRS-SW           PIC X.
00230              88 PB-I-MAIL-ADDRS-NOT-PRESENT  VALUE ' '.
00231              88 PB-I-MAIL-ADDRS-PRESENT      VALUE '1'.
00232          16  PB-I-1ST-PMT-DT              PIC XX.
00233          16  PB-I-JOINT-INSURED.
00234              20 PB-I-JOINT-LAST-NAME      PIC X(15).
00235              20 PB-I-JOINT-FIRST-NAME.
00236                 24  PB-I-JOINT-FIRST-INIT PIC X.
00237                 24  FILLER                PIC X(9).
00238              20 PB-I-JOINT-MIDDLE-INIT    PIC X.
100703*        16  PB-I-BENEFICIARY-NAME        PIC X(25).
100703         16  PB-I-BENEFICIARY-NAME.
100703             20  PB-I-BANK-NUMBER         PIC X(10).
100703             20  FILLER                   PIC X(15).
00240          16  PB-I-LAST-ADD-ON-DT          PIC XX.
011904         16  PB-I-REFERENCE               PIC X(12).
011904         16  FILLER REDEFINES PB-I-REFERENCE.
011904             20  PB-I-TOT-FEES            PIC S9(7)V99 COMP-3.
011904             20  PB-I-TOT-FEES-CALC       PIC S9(7)V99 COMP-3.
020305             20  PB-I-CLP-STATE           PIC XX.
00242          16  PB-I-UNDERWRITING-STATUS     PIC X.
00243              88  PB-I-POLICY-ACCEPTED         VALUE 'A' 'N'.
00244              88  PB-I-POLICY-DECLINED         VALUE 'D'.
00245              88  PB-I-NEEDS-UNDERWRITING      VALUE 'U'.
00246          16  PB-I-STATE-TAX               PIC S9(7)V99 COMP-3.
00247          16  PB-I-MUNI-TAX                PIC S9(7)V99 COMP-3.
00248          16  PB-I-RESIDENT-STATE          PIC XX.
00249          16  PB-I-RATE-CODE               PIC X(4).
00250          16  PB-I-NUM-BILLED              PIC S9(7)    COMP-3.
PEMMOD         16  PB-I-LF-PREM-TAX             PIC S9V9(4)  COMP-3.
PEMMOD         16  PB-I-AH-PREM-TAX             PIC S9V9(4)  COMP-3.
100703         16  PB-I-BANK-FEE                PIC S999V99  COMP-3.
100703         16  PB-I-BANK-NOCHRGB            PIC 99.
040504         16  PB-I-ADDL-CLP                PIC S9(5)V99 COMP-3.
081108         16  PB-I-JOINT-BIRTHDAY          PIC XX.
00252
00253      12  PB-CANCEL-RECORD   REDEFINES PB-RECORD-BODY.
00254          16  PB-C-LF-CANCEL-VOID-SW       PIC X.
00255              88  PB-C-LF-CANCEL-VOIDED        VALUE '1'.
00256          16  PB-C-CANCEL-ORIGIN           PIC X.
00257              88  PB-C-CLAIM-CREATED-CANCEL   VALUE '1'.
00258          16  PB-C-LF-CANCEL-DT            PIC XX.
00259          16  PB-C-LF-CANCEL-AMT           PIC S9(7)V99    COMP-3.
00260          16  PB-C-LF-CALC-REQ             PIC X.
00261              88 PB-COMP-LF-CANCEL            VALUE '?'.
00262          16  PB-C-LF-REF-CALC             PIC S9(7)V99    COMP-3.
00263          16  PB-C-LF-REM-TERM             PIC S9(3)       COMP-3.
00264          16  PB-C-AH-CANCEL-VOID-SW       PIC X.
00265              88  PB-C-AH-CANCEL-VOIDED        VALUE '1'.
00266          16  PB-C-AH-CANCEL-DT            PIC XX.
00267          16  PB-C-AH-CANCEL-AMT           PIC S9(7)V99    COMP-3.
00268          16  PB-C-AH-CALC-REQ             PIC X.
00269              88 PB-COMP-AH-CANCEL            VALUE '?'.
00270          16  PB-C-AH-REF-CALC             PIC S9(7)V99    COMP-3.
00271          16  PB-C-AH-REM-TERM             PIC S9(3)       COMP-3.
00272          16  PB-C-LAST-NAME               PIC X(15).
00273          16  PB-C-REFUND-SW               PIC X.
00274              88  PB-C-REFUND-CREATED          VALUE 'Y'.
00275              88  PB-C-REFUND-REQUESTED        VALUE 'R'.
00276          16  PB-C-LIVES                   PIC S9(3)       COMP-3.
00277          16  PB-C-PAYEE-CODE              PIC X(6).
00278          16  PB-C-LF-REFUND-OVERRIDE      PIC X.
00279          16  PB-C-AH-REFUND-OVERRIDE      PIC X.
00280          16  PB-C-LF-COMM-CHARGEBACK      PIC X.
00281          16  PB-C-AH-COMM-CHARGEBACK      PIC X.
00282          16  PB-C-REFERENCE               PIC X(12).
PEMMOD         16  PB-C-LF-PREM-TAX             PIC S9V9(4)  COMP-3.
PEMMOD         16  PB-C-AH-PREM-TAX             PIC S9V9(4)  COMP-3.
081606         16  PB-C-POST-CARD-IND           PIC X.
081606         16  PB-C-CANCEL-REASON           PIC X.
072308         16  PB-C-REF-INTERFACE-SW        PIC X.
071211         16  PB-C-LF-RFND-CLP             PIC S9(5)V99 COMP-3.
071211         16  PB-C-AH-RFND-CLP             PIC S9(5)V99 COMP-3.
00283          16  FILLER                       PIC X(01).
PEMMOD*        16  FILLER                       PIC X(18).
00284          16  PB-C-POLICY-FORM-NO          PIC X(12).
072308*        16  PB-C-MICROFILM-NO            PIC S9(9)      COMP-3.
062017         16  PB-C-INT-ON-REFS             PIC S9(7)V99   COMP-3.
00286          16  PB-CANCELED-CERT-DATA.
00287              20  PB-CI-INSURED-NAME.
00288                  24  PB-CI-LAST-NAME      PIC X(15).
00289                  24  PB-CI-INITIALS       PIC XX.
00290              20  PB-CI-INSURED-AGE        PIC S99         COMP-3.
00291              20  PB-CI-INSURED-SEX        PIC X.
00292              20  PB-CI-LF-TERM            PIC S999        COMP-3.
00293              20  PB-CI-LF-BENEFIT-CD      PIC XX.
00294              20  PB-CI-LF-BENEFIT-AMT     PIC S9(9)V99    COMP-3.
00295              20  PB-CI-LF-ALT-BENEFIT-AMT PIC S9(9)V99    COMP-3.
00296              20  PB-CI-LF-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
00297              20  PB-CI-LF-ALT-PREMIUM-AMT PIC S9(7)V99    COMP-3.
00298              20  PB-CI-AH-TERM            PIC S999        COMP-3.
00299              20  PB-CI-AH-BENEFIT-CD      PIC XX.
00300              20  PB-CI-AH-BENEFIT-AMT     PIC S9(7)V99    COMP-3.
00301              20  PB-CI-AH-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
00302              20  PB-CI-RATE-CLASS         PIC XX.
00303              20  PB-CI-RATE-DEV-LF        PIC XXX.
00304              20  PB-CI-RATE-DEV-AH        PIC XXX.
00305              20  PB-CI-RATE-DEV-PCT-LF    PIC S9V9(6)     COMP-3.
00306              20  PB-CI-RATE-DEV-PCT-AH    PIC S9V9(6)     COMP-3.
00307              20  PB-CI-LIFE-COMMISSION    PIC SV9(5)      COMP-3.
00308              20  PB-CI-AH-COMMISSION      PIC SV9(5)      COMP-3.
00309              20  PB-CI-LF-ABBR            PIC X(3).
00310              20  PB-CI-AH-ABBR            PIC X(3).
00311              20  PB-CI-OB-FLAG            PIC X.
00312                  88  PB-CI-OB                VALUE 'B'.
00313              20  PB-CI-LF-POLICY-STATUS   PIC X.
00314                  88  PB-CI-LF-POLICY-IS-ACTIVE       VALUE '1' '3'
122002                                           'M' '4' '5' '9' '2'.
00316                  88  PB-CI-LF-NORMAL-ENTRY           VALUE '1'.
00317                  88  PB-CI-LF-POLICY-PENDING         VALUE '2'.
00318                  88  PB-CI-LF-POLICY-IS-RESTORE      VALUE '3'.
00319                  88  PB-CI-LF-CONVERSION-ENTRY       VALUE '4'.
00320                  88  PB-CI-LF-POLICY-IS-REISSUE      VALUE '5'.
                       88  PB-CI-LF-POLICY-IS-CASH         VALUE 'C'.
122002                 88  PB-CI-LF-POLICY-IS-MONTHLY      VALUE 'M'.
00321                  88  PB-CI-LF-LUMP-SUM-DISAB         VALUE '6'.
00322                  88  PB-CI-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
00323                  88  PB-CI-LF-CANCEL-APPLIED         VALUE '8'.
00324                  88  PB-CI-LF-REIN-ONLY              VALUE '9'.
00325                  88  PB-CI-LF-POLICY-IS-DECLINED     VALUE 'D'.
00326                  88  PB-CI-LF-POLICY-IS-VOID         VALUE 'V'.
00327              20  PB-CI-AH-POLICY-STATUS   PIC X.
00328                  88  PB-CI-AH-POLICY-IS-ACTIVE       VALUE '1' '3'
122002                                           'M' '4' '5' '9' '2'.
00330                  88  PB-CI-AH-NORMAL-ENTRY           VALUE '1'.
00331                  88  PB-CI-AH-POLICY-PENDING         VALUE '2'.
00332                  88  PB-CI-AH-POLICY-IS-RESTORE      VALUE '3'.
00333                  88  PB-CI-AH-CONVERSION-ENTRY       VALUE '4'.
00334                  88  PB-CI-AH-POLICY-IS-REISSUE      VALUE '5'.
                       88  PB-CI-AH-POLICY-IS-CASH         VALUE 'C'.
122002                 88  PB-CI-AH-POLICY-IS-MONTHLY      VALUE 'M'.
00335                  88  PB-CI-AH-LUMP-SUM-DISAB         VALUE '6'.
00336                  88  PB-CI-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
00337                  88  PB-CI-AH-CANCEL-APPLIED         VALUE '8'.
00338                  88  PB-CI-AH-REIN-ONLY              VALUE '9'.
00339                  88  PB-CI-AH-POLICY-IS-DECLINED     VALUE 'D'.
00340                  88  PB-CI-AH-POLICY-IS-VOID         VALUE 'V'.
00341              20  PB-CI-PAY-FREQUENCY      PIC 99.
00342              20  PB-CI-LOAN-APR           PIC 9(3)V9(4)   COMP-3.
00343              20  PB-CI-SOC-SEC-NO         PIC X(11).
00344              20  PB-CI-MEMBER-NO          PIC X(12).
00345              20  PB-CI-INT-CODE           PIC X.
00346                  88  PB-CI-ADD-ON                  VALUE 'A'.
00347                  88  PB-CI-SIMPLE                  VALUE 'S'.
00348              20  PB-CI-LOAN-TERM          PIC S999        COMP-3.
00349              20  PB-CI-LOAN-1ST-PMT-DT    PIC X(2).
00350              20  PB-CI-COMP-EXCP-SW       PIC X.
00351                  88  PB-CI-NO-COMP-EXCP            VALUE ' '.
00352                  88  PB-CI-CERT-HAS-ERCOMM-ENTRY   VALUE '1'.
00353              20  PB-CI-ENTRY-STATUS       PIC X.
00354              20  PB-CI-CURR-SEQ           PIC S9(4)       COMP.
00355              20  PB-CI-AH-PAID-THRU-DT    PIC XX.
00356              20  PB-CI-AH-SETTLEMENT-DT   PIC XX.
00357              20  PB-CI-DEATH-DT           PIC XX.
00358              20  PB-CI-LF-PRIOR-CANCEL-DT PIC XX.
00359              20  PB-CI-AH-PRIOR-CANCEL-DT PIC XX.
00360              20  PB-CI-AH-CANCEL-AMT      PIC S9(7)V99    COMP-3.
00361              20  PB-CI-LF-CANCEL-AMT      PIC S9(7)V99    COMP-3.
00362              20  PB-CI-CREDIT-INTERFACE-SW-1 PIC X.
00363              20  PB-CI-CREDIT-INTERFACE-SW-2 PIC X.
00364              20  PB-CI-ENTRY-DT              PIC XX.
00365              20  PB-CI-ENTRY-BATCH           PIC X(6).
00366              20  PB-CI-LF-EXPIRE-DT          PIC XX.
00367              20  PB-CI-AH-EXPIRE-DT          PIC XX.
00368              20  PB-CI-EXTENTION-DAYS        PIC S999     COMP-3.
00369              20  PB-CI-TERM-IN-DAYS          PIC S9(5)    COMP-3.
110105             20  PB-CI-OLD-LOF               PIC XXX.
110105*            20  PB-CI-LOAN-OFFICER          PIC XXX.
00371              20  PB-CI-LIVES                 PIC S9(3)    COMP-3.
00372              20  PB-CI-LF-CRIT-PER           PIC S9(3)    COMP-3.
00373              20  PB-CI-AH-CRIT-PER           PIC S9(3)    COMP-3.
00374              20  PB-CI-INDV-GRP-CD           PIC X.
100703             20  PB-CI-BENEFICIARY-NAME.
100703                 24  PB-CI-BANK-NUMBER       PIC X(10).
100703                 24  FILLER                  PIC X(15).
00376              20  PB-CI-NOTE-SW               PIC X.
00377              20  PB-CI-CANCEL-FEE            PIC S9(3)V99 COMP-3.
00378              20  PB-CI-MUNI-TAX              PIC S9(7)V99 COMP-3.
00379              20  PB-CI-STATE-TAX             PIC S9(7)V99 COMP-3.
040504             20  PB-CI-ADDL-CLP              PIC S9(5)V99 COMP-3.
110105             20  PB-CI-LOAN-OFFICER          PIC X(5).
032306             20  PB-CI-BOW-LOAN-NUMBER       PIC X(14).
072209             20  PB-CI-FIRST-NAME            PIC X(10).
071211             20  PB-CI-DDF-IU-RATE-UP        PIC S9(5)V99 COMP-3.
00380
072209         16  FILLER                       PIC X(13).
072209*032306  16  FILLER                       PIC X(27).
040504*        16  FILLER                       PIC X(46).
00382
00383      12  PB-MAIL-RECORD    REDEFINES PB-RECORD-BODY.
00384          16  FILLER                       PIC X(10).
00385          16  PB-M-INSURED-LAST-NAME       PIC X(15).
00386          16  PB-M-INSURED-FIRST-NAME      PIC X(10).
00387          16  PB-M-INSURED-MID-INIT        PIC X.
00388          16  PB-M-INSURED-AGE             PIC 99.
00389          16  PB-M-INSURED-BIRTHDAY        PIC XX.
00390          16  PB-M-INSURED-SEX             PIC X.
00391          16  PB-M-INSURED-SOC-SEC-NO      PIC X(11).
00392          16  PB-M-INSURED-ADDRESS-1       PIC X(30).
00393          16  PB-M-INSURED-ADDRESS-2       PIC X(30).
00394          16  PB-M-INSURED-CITY-STATE.
051810             20  PB-M-INSURED-CITY        PIC X(28).
051810             20  PB-M-INSURED-STATE       PIC XX.
00395          16  PB-M-INSURED-ZIP-CODE.
00396              20  PB-M-INSURED-ZIP-PRIME.
00397                  24  PB-M-INSURED-ZIP-1   PIC X.
00398                      88  PB-M-CANADIAN-POST-CODE
00399                                              VALUE 'A' THRU 'Z'.
00400                  24  FILLER               PIC X(4).
00401              20  PB-M-INSURED-ZIP-PLUS4   PIC X(4).
00402          16  PB-M-INSURED-CANADIAN-ZIP  REDEFINES
00403                                         PB-M-INSURED-ZIP-CODE.
00404              20  PM-M-INS-CAN-POST1       PIC XXX.
00405              20  PM-M-INS-CAN-POST2       PIC XXX.
00406              20  FILLER                   PIC XXX.
00407          16  PB-M-INSURED-PHONE-NO        PIC 9(10).
081108         16  PB-M-JOINT-BIRTHDAY          PIC XX.
               16  PB-M-CRED-BENE-NAME          PIC X(30).
               16  PB-M-CRED-BENE-ADDR1         PIC X(30).
               16  PB-M-CRED-BENE-ADDR2         PIC X(30).
               16  PB-M-CRED-BENE-CITYST.
                   20  PB-M-CRED-BENE-CITY      PIC X(28).
                   20  PB-M-CRED-BENE-STATE     PIC XX.
081108         16  FILLER                       PIC X(92).
00409
00410      12  PB-BATCH-RECORD   REDEFINES PB-RECORD-BODY.
00411          16  FILLER                       PIC X(10).
00412          16  PB-B-LF-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00413          16  PB-B-LF-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00414          16  PB-B-LF-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00415          16  PB-B-LF-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00416          16  PB-B-LF-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00417          16  PB-B-LF-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00418          16  PB-B-AH-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00419          16  PB-B-AH-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00420          16  PB-B-AH-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00421          16  PB-B-AH-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00422          16  PB-B-AH-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00423          16  PB-B-AH-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00424          16  PB-B-ISSUE-CNT-REMITTED      PIC S9(5)    COMP-3.
00425          16  PB-B-ISSUE-CNT-ENTERED       PIC S9(5)    COMP-3.
00426          16  PB-B-CANCEL-CNT-REMITTED     PIC S9(5)    COMP-3.
00427          16  PB-B-CANCEL-CNT-ENTERED      PIC S9(5)    COMP-3.
00428          16  PB-B-HIGHEST-SEQ-NO          PIC S9(4)    COMP.
00429          16  PB-ACCOUNT-NAME              PIC X(30).
00430          16  PB-PREM-REF-RPT-FLAG         PIC X.
00431          16  PB-REFERENCE                 PIC X(12).
00432          16  PB-B-RECEIVED-DT             PIC XX.
00433          16  FILLER                       PIC X(234).
00434
00435      12  PB-RECORD-STATUS.
00436          16  PB-CREDIT-SELECT-DT          PIC XX.
00437          16  PB-CREDIT-ACCEPT-DT          PIC XX.
00438          16  PB-BILLED-DT                 PIC XX.
00439          16  PB-BILLING-STATUS            PIC X.
00440              88  PB-ENTRY-REVERSED            VALUE 'R'.
00441              88  PB-EL860-INTERNAL-ERROR      VALUE 'E'.
00442              88  PB-EL860-INTERNAL-PROCESS    VALUE 'P'.
00443          16  PB-RECORD-BILL               PIC X.
00444              88  PB-RECORD-ON-HOLD            VALUE 'H'.
00445              88  PB-RECORD-RETURNED           VALUE 'R'.
00446              88  PB-RECORD-ENDORSED           VALUE 'E'.
00447              88  PB-OVERRIDE-LIFE             VALUE 'L'.
00448              88  PB-OVERRIDE-AH               VALUE 'A'.
00449              88  PB-OVERRIDE-BOTH             VALUE 'B'.
00450          16  PB-BATCH-ENTRY               PIC X.
00451              88  PB-POLICY-IS-DECLINED        VALUE 'D'.
00452              88  PB-REIN-ONLY-CERT            VALUE 'R'.
00453              88  PB-REISSUED-CERT             VALUE 'E'.
                   88  PB-CASH-CERT                 VALUE 'C'.
122002             88  PB-MONTHLY-CERT              VALUE 'M'.
00454              88  PB-PREM-ACCTNG-ONLY          VALUE 'P'.
00455              88  PB-NEEDS-UNDERWRITING        VALUE 'U'.
00456              88  PB-POLICY-IS-VOIDED          VALUE 'V'.
00457          16  PB-FORCE-CODE                PIC X.
00458              88  PB-FORCE-OFF                 VALUE ' ' '0'.
00459              88  PB-ISSUE-FORCE               VALUE 'A' 'O'.
00460              88  PB-CANCEL-FORCE              VALUE '8'.
00461              88  PB-ALL-ISSUE-FORCED          VALUE 'A' 'O'.
00462              88  PB-ALL-CANCEL-FORCED         VALUE '8'.
00463              88  PB-ALL-CANCEL-FORCED-NO-FEE  VALUE '9'.
00464              88  PB-CANCEL-DATE-FORCED        VALUE 'D'.
00465              88  PB-CANCEL-DATE-FORCED-NO-FEE VALUE 'E'.
00466              88  PB-ISSUE-DATE-FORCED         VALUE 'D'.
010517             88  PB-EXCEEDED-LIMIT-FORCED     VALUE 'L'.
073107             88  PB-OVERCHARGE-FORCE          VALUE 'O'.
00467          16  PB-FATAL-FLAG                PIC X.
00468              88  PB-FATAL-ERRORS              VALUE 'X'.
00469          16  PB-FORCE-ER-CD               PIC X.
00470              88  PB-FORCE-ERRORS              VALUE 'F'.
00471              88  PB-UNFORCED-ERRORS           VALUE 'X'.
00472          16  PB-WARN-ER-CD                PIC X.
00473              88  PB-WARNING-ERRORS            VALUE 'W'.
00474          16  FILLER                       PIC X.
00475          16  PB-OUT-BAL-CD                PIC X.
00476              88  PB-OUT-OF-BAL                VALUE 'O'.
00477          16  PB-LIFE-OVERRIDE-L1          PIC X.
00478          16  PB-AH-OVERRIDE-L1            PIC X.
00479          16  PB-INPUT-DT                  PIC XX.
00480          16  PB-INPUT-BY                  PIC X(4).
00481          16  PB-CHG-COUNT                 PIC 9(3)        COMP-3.
00482          16  PB-CALC-TOLERANCE            PIC 9(3)V99     COMP-3.
00483          16  PB-TOLERANCE-REJECT-SW       PIC X.
00484          16  PB-LF-EARNING-METHOD         PIC X.
00485          16  PB-AH-EARNING-METHOD         PIC X.
00486          16  PB-LF-TERM-CALC-METHOD       PIC X.
00487          16  PB-AH-TERM-CALC-METHOD       PIC X.
00488          16  PB-REIN-CD                   PIC XXX.
00489          16  PB-LF-REFUND-TYPE            PIC X.
00490          16  PB-AH-REFUND-TYPE            PIC X.
00491          16  PB-ACCT-EFF-DT               PIC XX.
00492          16  PB-ACCT-EXP-DT               PIC XX.
00493          16  PB-COMPANY-ID                PIC X(3).
00494          16  PB-LF-BILLED-AMTS            PIC S9(7)V99  COMP-3.
00495          16  PB-AH-BILLED-AMTS            PIC S9(7)V99  COMP-3.
00496          16  PB-SV-CARRIER                PIC X.
00497          16  PB-SV-GROUPING               PIC X(6).
00498          16  PB-SV-STATE                  PIC XX.
00499          16  PB-CONFIRMATION-REPT-DT      PIC XX.
00500          16  PB-GA-BILLING-INFO.
00501              20  PB-GA-BILL-DT OCCURS 5 TIMES
00502                                           PIC XX.
00503          16  PB-SV-REMIT-TO  REDEFINES
00504              PB-GA-BILLING-INFO           PIC X(10).
00505          16  PB-NO-OF-ERRORS              PIC S9(3) COMP-3.
110105         16  PB-I-LOAN-OFFICER            PIC X(5).
081606         16  PB-I-VIN                     PIC X(17).
00506
110105         16  FILLER                       PIC X(04).
110105         16  IMNET-BYPASS-SW              PIC X.
00508
00509 ******************************************************************
00510 *                COMMON EDIT ERRORS                              *
00511 ******************************************************************
00512
00513      12  PB-COMMON-ERRORS.
00514          16  PB-COMMON-ERROR    OCCURS 10 TIMES
00515                                            PIC S9(4)     COMP.
00516
00517 ******************************************************************
      *                                COPY ELCCNTL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCCNTL.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.059                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = SYSTEM CONTROL FILE                       *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 750  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCNTL                        RKP=2,LEN=10    *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
082503*                   C H A N G E   L O G
082503*
082503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
082503*-----------------------------------------------------------------
082503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
082503* EFFECTIVE    NUMBER
082503*-----------------------------------------------------------------
082503* 082503                   PEMA  ADD BENEFIT GROUP
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
033104* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
092705* 092705    2005050300006  PEMA  ADD SPP LEASES
031808* 031808    2006032200004  AJRA  ADD APPROVAL LEVEL 4
071508* 071508  CR2007110500003  PEMA  ADD NH INTEREST REFUND PROCESSING
091808* 091808    2008022800002  AJRA  ADD CHECK NUMBER TO STATE CNTL FO
011410* 011410    2009061500002  AJRA  ADD REFUND IND FOR AH AND DEATH C
061511* 061511    2011042000002  AJRA  ADD IND TO VERIFY 2ND BENEFICIARY
011812* 011812    2011022800001  AJRA  ADD CSR IND TO USER SECURITY
012913* 012913    2012092400007  AJRA  ADD CAUSAL STATE IND
032813* 032813    2011013100001  AJRA  ADD CLAIM REAUDIT FIELDS
091813* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
102717* 102717  CR2017062000003  PEMA  COMM CAP CHANGES
082503******************************************************************
00018 *
00019  01  CONTROL-FILE.
00020      12  CF-RECORD-ID                       PIC XX.
00021          88  VALID-CF-ID                        VALUE 'CF'.
00022
00023      12  CF-CONTROL-PRIMARY.
00024          16  CF-COMPANY-ID                  PIC XXX.
00025          16  CF-RECORD-TYPE                 PIC X.
00026              88  CF-COMPANY-MASTER              VALUE '1'.
00027              88  CF-PROCESSOR-MASTER            VALUE '2'.
00028              88  CF-STATE-MASTER                VALUE '3'.
00029              88  CF-LF-BENEFIT-MASTER           VALUE '4'.
00030              88  CF-AH-BENEFIT-MASTER           VALUE '5'.
00031              88  CF-CARRIER-MASTER              VALUE '6'.
00032              88  CF-MORTALITY-MASTER            VALUE '7'.
00033              88  CF-BUSINESS-TYPE-MASTER        VALUE '8'.
00034              88  CF-TERMINAL-MASTER             VALUE '9'.
00035              88  CF-AH-EDIT-MASTER              VALUE 'A'.
00036              88  CF-CREDIBILITY-FACTOR-MASTER   VALUE 'B'.
00037              88  CF-CUSTOM-REPORT-MASTER        VALUE 'C'.
00038              88  CF-MORTGAGE-HT-WT-CHART        VALUE 'H'.
00039              88  CF-LIFE-EDIT-MASTER            VALUE 'L'.
00040              88  CF-MORTGAGE-PLAN-MASTER        VALUE 'M'.
00041              88  CF-MORTGAGE-COMPANY-MASTER     VALUE 'N'.
00042              88  CF-REMINDERS-MASTER            VALUE 'R'.
00043              88  CF-AUTO-ACTIVITY-MASTER        VALUE 'T'.
00044          16  CF-ACCESS-CD-GENL              PIC X(4).
00045          16  CF-ACCESS-OF-PROCESSOR  REDEFINES CF-ACCESS-CD-GENL.
00046              20  CF-PROCESSOR               PIC X(4).
00047          16  CF-ACCESS-OF-STATE  REDEFINES  CF-ACCESS-CD-GENL.
00048              20  CF-STATE-CODE              PIC XX.
00049              20  FILLER                     PIC XX.
00050          16  CF-ACCESS-OF-BENEFIT  REDEFINES  CF-ACCESS-CD-GENL.
00051              20  FILLER                     PIC XX.
00052              20  CF-HI-BEN-IN-REC           PIC XX.
00053          16  CF-ACCESS-OF-CARRIER  REDEFINES  CF-ACCESS-CD-GENL.
00054              20  FILLER                     PIC XXX.
00055              20  CF-CARRIER-CNTL            PIC X.
00056          16  CF-ACCESS-OF-BUS-TYPE REDEFINES  CF-ACCESS-CD-GENL.
00057              20  FILLER                     PIC XX.
00058              20  CF-HI-TYPE-IN-REC          PIC 99.
00059          16  CF-ACCESS-OF-CRDB-TBL REDEFINES  CF-ACCESS-CD-GENL.
00060              20  CF-CRDB-TABLE-INDICATOR    PIC X.
00061                  88  CF-CRDB-NAIC-TABLE         VALUE '9'.
00062              20  CF-CRDB-BENEFIT-TYPE       PIC X.
00063              20  CF-CRDB-WAITING-PERIOD     PIC XX.
00064          16  CF-ACCESS-OF-CUST-RPT REDEFINES  CF-ACCESS-CD-GENL.
00065              20  FILLER                     PIC X.
00066              20  CF-CUSTOM-REPORT-NO        PIC 999.
00067          16  CF-ACCESS-OF-PLAN   REDEFINES  CF-ACCESS-CD-GENL.
00068              20  FILLER                     PIC XX.
00069              20  CF-MORTGAGE-PLAN           PIC XX.
00070          16  CF-SEQUENCE-NO                 PIC S9(4)   COMP.
00071
00072      12  CF-LAST-MAINT-DT                   PIC XX.
00073      12  CF-LAST-MAINT-BY                   PIC X(4).
00074      12  CF-LAST-MAINT-HHMMSS               PIC S9(6)   COMP-3.
00075
00076      12  CF-RECORD-BODY                     PIC X(728).
00077
00078
00079 ****************************************************************
00080 *             COMPANY MASTER RECORD                            *
00081 ****************************************************************
00082
00083      12  CF-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00084          16  CF-COMPANY-ADDRESS.
00085              20  CF-CL-MAIL-TO-NAME         PIC X(30).
00086              20  CF-CL-IN-CARE-OF           PIC X(30).
00087              20  CF-CL-ADDR-LINE-1          PIC X(30).
00088              20  CF-CL-ADDR-LINE-2          PIC X(30).
00089              20  CF-CL-CITY-STATE           PIC X(30).
00090              20  CF-CL-ZIP-CODE-NUM         PIC 9(9)    COMP-3.
00091              20  CF-CL-PHONE-NO             PIC 9(11)   COMP-3.
00092          16  CF-COMPANY-CD                  PIC X.
00093          16  CF-COMPANY-PASSWORD            PIC X(8).
00094          16  CF-SECURITY-OPTION             PIC X.
00095              88  ALL-SECURITY                   VALUE '1'.
00096              88  COMPANY-VERIFY                 VALUE '2'.
00097              88  PROCESSOR-VERIFY               VALUE '3'.
00098              88  NO-SECURITY                    VALUE '4'.
00099              88  ALL-BUT-TERM                   VALUE '5'.
00100          16  CF-CARRIER-CONTROL-LEVEL       PIC X.
00101              88  USE-ACTUAL-CARRIER             VALUE SPACE.
00102          16  CF-LGX-INTERFACE-CNTL          PIC X.
00103              88  LGX-TIME-SHR-COMPANY           VALUE '1'.
00104          16  CF-INFORCE-LOCATION            PIC X.
00105              88  CERTS-ARE-ONLINE               VALUE '1'.
00106              88  CERTS-ARE-OFFLINE              VALUE '2'.
00107              88  NO-CERTS-AVAILABLE             VALUE '3'.
00108          16  CF-LOWER-CASE-LETTERS          PIC X.
00109          16  CF-CERT-ACCESS-CONTROL         PIC X.
00110              88  CF-ST-ACCNT-CNTL               VALUE ' '.
00111              88  CF-CARR-GROUP-ST-ACCNT-CNTL    VALUE '1'.
00112              88  CF-CARR-ST-ACCNT-CNTL          VALUE '2'.
00113              88  CF-ACCNT-CNTL                  VALUE '3'.
00114              88  CF-CARR-ACCNT-CNTL             VALUE '4'.
00115
00116          16  CF-FORMS-PRINTER-ID            PIC X(4).
00117          16  CF-CHECK-PRINTER-ID            PIC X(4).
00118
00119          16  CF-LGX-CREDIT-USER             PIC X.
00120              88  CO-IS-NOT-USER                 VALUE 'N'.
00121              88  CO-HAS-CLAS-IC-CREDIT          VALUE 'Y'.
00122
00123          16 CF-CREDIT-CALC-CODES.
00124              20  CF-CR-REM-TERM-CALC PIC X.
00125                88  CR-EARN-AFTER-15TH           VALUE '1'.
00126                88  CR-EARN-ON-HALF-MO           VALUE '2'.
00127                88  CR-EARN-ON-1ST-DAY           VALUE '3'.
00128                88  CR-EARN-ON-FULL-MO           VALUE '4'.
00129                88  CR-EARN-WITH-NO-DAYS         VALUE '5'.
00130                88  CR-EARN-AFTER-14TH           VALUE '6'.
00131                88  CR-EARN-AFTER-16TH           VALUE '7'.
00132              20  CF-CR-R78-METHOD           PIC X.
00133                88  USE-TERM-PLUS-ONE            VALUE SPACE.
00134                88  DONT-USE-PLUS-ONE            VALUE '1'.
00135
00136          16  CF-CLAIM-CONTROL-COUNTS.
00137              20  CF-CO-CLAIM-COUNTER        PIC S9(8)   COMP.
00138                  88  CO-CLM-COUNT-RESET         VALUE +99999.
00139
00140              20  CF-CO-ARCHIVE-COUNTER      PIC S9(8)   COMP.
00141                  88  CO-ARCHIVE-COUNT-RESET     VALUE +999999.
00142
00143              20  CF-CO-CHECK-COUNTER        PIC S9(8)   COMP.
00144                  88  CO-CHECK-COUNT-RESET       VALUE +9999999.
00145
00146              20  CF-CO-CHECK-QUE-COUNTER    PIC S9(8)   COMP.
00147                  88  CO-QUE-COUNT-RESET         VALUE +9999999.
00148
00149          16  CF-CURRENT-MONTH-END           PIC XX.
00150
00151          16  CF-CO-CALC-QUOTE-TOLERANCE.
00152              20  CF-CO-TOL-CLAIM            PIC S999V99   COMP-3.
00153              20  CF-CO-TOL-PREM             PIC S999V99   COMP-3.
00154              20  CF-CO-TOL-REFUND           PIC S999V99   COMP-3.
00155              20  CF-CO-CLAIM-REJECT-SW      PIC X.
00156                  88 CO-WARN-IF-CLAIM-OUT        VALUE SPACE.
00157                  88 CO-FORCE-IF-CLAIM-OUT       VALUE '1'.
00158              20  CF-CO-PREM-REJECT-SW       PIC X.
00159                  88 CO-WARN-IF-PREM-OUT         VALUE SPACE.
00160                  88 CO-FORCE-IF-PREM-OUT        VALUE '1'.
00161              20  CF-CO-REF-REJECT-SW        PIC X.
00162                  88 CO-WARN-IF-REF-OUT          VALUE SPACE.
00163                  88 CO-FORCE-IF-REF-OUT         VALUE '1'.
00164
00165          16  CF-CO-REPORTING-DT             PIC XX.
00166          16  CF-CO-REPORTING-MONTH-DT       PIC XX.
00167          16  CF-CO-REPORTING-MONTH-END-SW   PIC X.
00168            88  CF-CO-NOT-MONTH-END              VALUE SPACES.
00169            88  CF-CO-MONTH-END                  VALUE '1'.
00170
00171          16  CF-LGX-CLAIM-USER              PIC X.
00172              88  CO-IS-NOT-CLAIM-USER           VALUE 'N'.
00173              88  CO-HAS-CLAS-IC-CLAIM           VALUE 'Y'.
00174
00175          16  CF-CREDIT-EDIT-CONTROLS.
00176              20  CF-MIN-PREMIUM             PIC S999V99   COMP-3.
00177              20  CF-MIN-AGE                 PIC 99.
00178              20  CF-DEFAULT-AGE             PIC 99.
00179              20  CF-MIN-TERM                PIC S999      COMP-3.
00180              20  CF-MAX-TERM                PIC S999      COMP-3.
00181              20  CF-DEFAULT-SEX             PIC X.
00182              20  CF-JOINT-AGE-INPUT         PIC X.
00183                  88 CF-JOINT-AGE-IS-INPUT       VALUE '1'.
00184              20  CF-BIRTH-DATE-INPUT        PIC X.
00185                  88 CF-BIRTH-DATE-IS-INPUT      VALUE '1'.
00186              20  CF-CAR-GROUP-ACCESS-CNTL   PIC X.
00187                  88  CF-USE-ACTUAL-CARRIER      VALUE ' '.
00188                  88  CF-ZERO-CARRIER            VALUE '1'.
00189                  88  CF-ZERO-GROUPING           VALUE '2'.
00190                  88  CF-ZERO-CAR-GROUP          VALUE '3'.
00191              20  CF-EDIT-SW                 PIC X.
00192                  88  CF-START-EDIT-TONIGHT      VALUE '1'.
00193              20  CF-EDIT-RESTART-BATCH      PIC X(6).
00194              20  CF-CR-PR-METHOD            PIC X.
00195                88  USE-NORMAL-PR-METHOD         VALUE SPACE.
00196                88  ADJUST-ORIG-TERM-BY-5        VALUE '1'.
00197              20  FILLER                     PIC X.
00198
00199          16  CF-CREDIT-MISC-CONTROLS.
00200              20  CF-REIN-TABLE-SW           PIC X.
00201                  88 REIN-TABLES-ARE-USED        VALUE '1'.
00202              20  CF-COMP-TABLE-SW           PIC X.
00203                  88 COMP-TABLES-ARE-USED        VALUE '1'.
00204              20  CF-EXPERIENCE-RETENTION-AGE
00205                                             PIC S9        COMP-3.
00206              20  CF-CONVERSION-DT           PIC XX.
00207              20  CF-COMP-WRITE-OFF-AMT      PIC S999V99   COMP-3.
00208              20  CF-RUN-FREQUENCY-SW        PIC X.
00209                  88 CO-IS-PROCESSED-MONTHLY     VALUE SPACE.
00210                  88 CO-IS-PROCESSED-ON-QTR      VALUE '1'.
00211
00212              20  CF-CR-CHECK-NO-CONTROL.
00213                  24  CF-CR-CHECK-NO-METHOD    PIC X.
00214                      88  CR-CHECK-NO-MANUAL       VALUE '1'.
00215                      88  CR-CHECK-NO-AUTO-SEQ     VALUE '2'.
00216                      88  CR-CHECK-NO-AT-PRINT     VALUE '4'.
00217                  24  CF-CR-CHECK-COUNTER      PIC S9(8)   COMP.
00218                      88  CR-CHECK-CNT-RESET-VALUE VALUE +999999.
00219
00220                  24  CF-CR-CHECK-COUNT       REDEFINES
00221                      CF-CR-CHECK-COUNTER      PIC X(4).
00222
00223                  24  CF-CR-CHECK-QUE-COUNTER  PIC S9(8)  COMP.
00224                      88  CR-QUE-COUNT-RESET      VALUE +9999999.
00225
00226                  24  CF-CR-CHECK-QUE-COUNT   REDEFINES
00227                      CF-CR-CHECK-QUE-COUNTER  PIC X(4).
00228                  24  CF-MAIL-PROCESSING       PIC X.
00229                      88  MAIL-PROCESSING          VALUE 'Y'.
00230
00231          16  CF-MISC-SYSTEM-CONTROL.
00232              20  CF-SYSTEM-C                 PIC X.
00233                  88  CONFIRMATION-SYS-USED       VALUE '1'.
00234              20  CF-SYSTEM-D                 PIC X.
00235                  88  DAILY-BILL-SYS-USED         VALUE '1'.
00236              20  CF-SOC-SEC-NO-SW            PIC X.
00237                  88  SOC-SEC-NO-USED             VALUE '1'.
00238              20  CF-MEMBER-NO-SW             PIC X.
00239                  88  MEMBER-NO-USED              VALUE '1'.
00240              20  CF-TAX-ID-NUMBER            PIC X(11).
00241              20  CF-JOURNAL-FILE-ID          PIC S9(4) COMP.
00242              20  CF-PAYMENT-APPROVAL-SW      PIC X.
00243                  88  CF-PMT-APPROVAL-USED        VALUE 'Y' 'G'.
00244                  88  CF-NO-APPROVAL              VALUE ' ' 'N'.
00245                  88  CF-ALL-APPROVED             VALUE 'Y'.
00246                  88  CF-GRADUATED-APPROVAL       VALUE 'G'.
00247              20  CF-SYSTEM-E                 PIC X.
00248                  88  CF-AR-SYSTEM-USED           VALUE 'Y'.
00249
00250          16  CF-LGX-LIFE-USER               PIC X.
00251              88  CO-IS-NOT-LIFE-USER            VALUE 'N'.
00252              88  CO-HAS-CLAS-IC-LIFE            VALUE 'Y'.
00253
00254          16  CF-CR-MONTH-END-DT             PIC XX.
00255
00256          16  CF-FILE-MAINT-DATES.
00257              20  CF-LAST-BATCH-NO           PIC S9(8)   COMP.
00258                  88  CF-LAST-BATCH-RESET        VALUE +999999.
00259              20  CF-LAST-BATCH       REDEFINES
00260                  CF-LAST-BATCH-NO               PIC X(4).
00261              20  CF-RATES-FILE-MAINT-DT         PIC XX.
00262              20  CF-RATES-FILE-CREATE-DT        PIC XX.
00263              20  CF-COMMISSION-TAB-MAINT-DT     PIC XX.
00264              20  CF-COMMISSION-TAB-CREATE-DT    PIC XX.
00265              20  CF-ACCOUNT-MSTR-MAINT-DT       PIC XX.
00266              20  CF-ACCOUNT-MSTR-CREATE-DT      PIC XX.
00267              20  CF-REINSURANCE-TAB-MAINT-DT    PIC XX.
00268              20  CF-REINSURANCE-TAB-CREATE-DT   PIC XX.
00269              20  CF-COMPENSATION-MSTR-MAINT-DT  PIC XX.
00270              20  CF-COMPENSATION-MSTR-CREATE-DT PIC XX.
00271
00272          16  CF-NEXT-COMPANY-ID             PIC XXX.
00273          16  FILLER                         PIC X.
00274
00275          16  CF-ALT-MORT-CODE               PIC X(4).
00276          16  CF-MEMBER-CAPTION              PIC X(10).
00277
00278          16  CF-LIFE-ACCESS-CONTROL         PIC X.
00279              88  CF-LIFE-ST-ACCNT-CNTL          VALUE ' '.
00280              88  CF-LIFE-CARR-GRP-ST-ACCNT-CNTL VALUE '1'.
00281              88  CF-LIFE-CARR-ST-ACCNT-CNTL     VALUE '2'.
00282              88  CF-LIFE-ACCNT-CNTL             VALUE '3'.
00283              88  CF-LIFE-CARR-ACCNT-CNTL        VALUE '4'.
00284
00285          16  CF-STARTING-ARCH-NO            PIC S9(8) COMP.
00286
00287          16  CF-LIFE-OVERRIDE-L1            PIC X.
00288          16  CF-LIFE-OVERRIDE-L2            PIC XX.
00289          16  CF-LIFE-OVERRIDE-L6            PIC X(6).
00290          16  CF-LIFE-OVERRIDE-L12           PIC X(12).
00291
00292          16  CF-AH-OVERRIDE-L1              PIC X.
00293          16  CF-AH-OVERRIDE-L2              PIC XX.
00294          16  CF-AH-OVERRIDE-L6              PIC X(6).
00295          16  CF-AH-OVERRIDE-L12             PIC X(12).
00296
00297          16  CF-REPORT-CD1-CAPTION          PIC X(10).
00298          16  CF-REPORT-CD2-CAPTION          PIC X(10).
00299
00300          16  CF-CLAIM-CUTOFF-DATE           PIC XX.
00301          16  CF-AR-LAST-EL860-DT            PIC XX.
00302          16  CF-MP-MONTH-END-DT             PIC XX.
00303
00304          16  CF-MAX-NUM-PMTS-CHECK          PIC 99.
00305          16  CF-CLAIM-PAID-THRU-TO          PIC X.
00306              88  CF-CLAIM-PAID-TO               VALUE '1'.
00307
00308          16  CF-AR-MONTH-END-DT             PIC XX.
00309
00310          16  CF-CRDTCRD-USER                PIC X.
00311              88  CO-IS-NOT-CRDTCRD-USER         VALUE 'N'.
00312              88  CO-HAS-CLAS-IC-CRDTCRD         VALUE 'Y'.
00313
00314          16  CF-CC-MONTH-END-DT             PIC XX.
00315
00316          16  CF-PRINT-ADDRESS-LABELS        PIC X.
00317
00318          16  CF-MORTALITY-AGE-CALC-METHOD   PIC X.
00319              88  CF-USE-TABLE-ASSIGNED-METHOD   VALUE '1' ' '.
00320              88  CF-USE-ALL-AGE-LAST            VALUE '2'.
00321              88  CF-USE-ALL-AGE-NEAR            VALUE '3'.
00322          16  CF-CO-TOL-PREM-PCT             PIC S9V9(4)   COMP-3.
00323          16  CF-CO-TOL-REFUND-PCT           PIC S9V9(4)   COMP-3.
00324          16  CF-CO-TOL-CAP                  PIC S9(3)V99  COMP-3.
00325          16  CF-CO-RESERVE-OPTION-SWITCH    PIC  X.
00326              88  OPTIONAL-RESERVE-METHOD-AUTH    VALUE 'Y'.
00327              88  OPTIONAL-RESERVE-MTHD-NOT-AUTH  VALUE ' ' 'N'.
00328          16  CF-CO-IBNR-LAG-MONTHS          PIC S9(3)     COMP-3.
00329          16  CF-CO-CIDA-TABLE-DISCOUNT-PCT  PIC S9V9(4)   COMP-3.
00330          16  CF-CO-CRDB-TABLE-SELECTION     PIC  X.
00331              88  NIAC-CREDIBILITY-TABLE          VALUE '9'.
00332          16  CF-CO-ST-CALL-RPT-CNTL         PIC  X.
00333
00334          16  CF-CL-ZIP-CODE.
00335              20  CF-CL-ZIP-PRIME.
00336                  24  CF-CL-ZIP-1ST          PIC X.
00337                      88  CF-CL-CAN-POST-CODE  VALUE 'A' THRU 'Z'.
00338                  24  FILLER                 PIC X(4).
00339              20  CF-CL-ZIP-PLUS4            PIC X(4).
00340          16  CF-CL-CANADIAN-POSTAL-CODE REDEFINES CF-CL-ZIP-CODE.
00341              20  CF-CL-CAN-POSTAL-1         PIC XXX.
00342              20  CF-CL-CAN-POSTAL-2         PIC XXX.
00343              20  FILLER                     PIC XXX.
00344
00345          16  CF-CO-CALCULATION-INTEREST     PIC S9V9(4)  COMP-3.
00346          16  CF-CO-IBNR-AH-FACTOR           PIC S9V9(4)  COMP-3.
00347          16  CF-CO-IBNR-LIFE-FACTOR         PIC S9V9(4)  COMP-3.
00348          16  CF-CO-OPTION-START-DATE        PIC XX.
00349          16  CF-REM-TRM-CALC-OPTION         PIC X.
00350            88  CF-VALID-REM-TRM-OPTION          VALUE '1' '2'
00351                                                       '3' '4'.
00352            88  CF-CONSIDER-EXTENSION            VALUE '3' '4'.
00353            88  CF-30-DAY-MONTH                  VALUE '1' '3'.
00354            88  CF-NO-EXT-30-DAY-MONTH           VALUE '1'.
00355            88  CF-NO-EXT-ACTUAL-DAYS            VALUE '2'.
00356            88  CF-EXT-30-DAY-MONTH              VALUE '3'.
00357            88  CF-EXT-ACTUAL-DAYS               VALUE '4'.
00358
00359          16  CF-DEFAULT-APR                 PIC S999V9(4) COMP-3.
00360
00361          16  CF-PAYMENT-APPROVAL-LEVELS.
00362              20  CF-LIFE-PAY-APP-LEVEL-1    PIC S9(7)   COMP-3.
00363              20  CF-LIFE-PAY-APP-LEVEL-2    PIC S9(7)   COMP-3.
00364              20  CF-LIFE-PAY-APP-LEVEL-3    PIC S9(7)   COMP-3.
00365              20  CF-AH-PAY-APP-LEVEL-1      PIC S9(7)   COMP-3.
00366              20  CF-AH-PAY-APP-LEVEL-2      PIC S9(7)   COMP-3.
00367              20  CF-AH-PAY-APP-LEVEL-3      PIC S9(7)   COMP-3.
00368
00369          16  CF-END-USER-REPORTING-USER     PIC X.
00370              88  CO-NO-END-USER-REPORTING       VALUE 'N'.
00371              88  CO-USES-END-USER-REPORTING     VALUE 'Y'.
00372
00373          16  CF-CLAIMS-CHECK-RECON-USER     PIC X.
00374              88  CO-NO-USE-CLAIMS-RECON         VALUE 'N'.
00375              88  CO-USES-CLAIMS-RECON           VALUE 'Y'.
00376
00377          16  CF-CLAIMS-LAST-PROCESS-DT      PIC XX.
00378
071508         16  CF-CREDIT-REF-SSN-CNT          PIC S9(5)  COMP-3.
00379          16  FILLER                         PIC X.
00380
00381          16  CF-CREDIT-ARCHIVE-CNTL.
00382              20  CF-CREDIT-LAST-ARCH-NUM    PIC S9(9)  COMP-3.
00383              20  CF-CREDIT-START-ARCH-NUM   PIC S9(9)  COMP-3.
00384              20  CF-CREDIT-ARCH-PURGE-YR    PIC S9     COMP-3.
00385
00386          16  CF-CR-PRINT-ADDRESS-LABELS     PIC X.
00387
00388          16  CF-CLAIMS-AUDIT-CHANGES        PIC X.
00389              88  CO-NO-USE-AUDIT-CHANGES        VALUE 'N'.
00390              88  CO-USES-AUDIT-CHANGES          VALUE 'Y'.
00391
00392          16  CF-CLAIMS-CREDIT-CARD-INDEX    PIC X.
00393              88  CO-NO-USE-CREDIT-CARD-INDEX    VALUE 'N'.
00394              88  CO-USES-CREDIT-CARD-INDEX      VALUE 'Y'.
00395
00396          16  CF-CLAIMS-LETTER-MAINT-DAYS    PIC 99.
00397
00398          16  CF-CO-ACH-ID-CODE              PIC  X.
00399              88  CF-CO-ACH-ICD-IRS-EIN          VALUE '1'.
00400              88  CF-CO-ACH-ICD-DUNS             VALUE '2'.
00401              88  CF-CO-ACH-ICD-USER-NO          VALUE '3'.
00402          16  CF-CO-ACH-CLAIM-SEND-NAME      PIC X(23).
00403          16  CF-CO-ACH-CLAIM-BK-NO          PIC X(09).
00404          16  CF-CO-ACH-ADMIN-SEND-NAME      PIC X(23).
00405          16  CF-CO-ACH-ADMIN-NO             PIC X(09).
00406          16  CF-CO-ACH-RECV-NAME            PIC X(23).
00407          16  CF-CO-ACH-RECV-NO              PIC X(08).
00408          16  CF-CO-ACH-ORIGINATOR-NO        PIC X(08).
00409          16  CF-CO-ACH-COMPANY-ID           PIC X(09).
00410          16  CF-CO-ACH-TRACE-NO             PIC 9(07) COMP.
00411                  88  CO-ACH-TRACE-NO-RESET      VALUE 9999999.
00412          16  CF-CO-ACH-TRACE-SPACE REDEFINES
00413                  CF-CO-ACH-TRACE-NO         PIC X(4).
00414
00415          16  CF-CO-OVER-SHORT.
00416              20 CF-CO-OVR-SHT-AMT           PIC S999V99   COMP-3.
00417              20 CF-CO-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
00418
031808*         16  FILLER                         PIC X(102).
031808         16  CF-PAYMENT-APPROVAL-LEVELS-2.
031808             20  CF-LIFE-PAY-APP-LEVEL-4    PIC S9(7)   COMP-3.
031808             20  CF-AH-PAY-APP-LEVEL-4      PIC S9(7)   COMP-3.
031808
031808         16  CF-AH-APPROVAL-DAYS.
031808             20  CF-AH-APP-DAY-LEVEL-1     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-2     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-3     PIC S9(5)   COMP-3.
031808             20  CF-AH-APP-DAY-LEVEL-4     PIC S9(5)   COMP-3.
032813
032813         16  CF-CO-REAUDIT-INTERVAL        PIC S9(5)   COMP-3.
031808
091813         16  CF-APPROV-LEV-5.
091813             20  CF-LIFE-PAY-APP-LEVEL-5    PIC S9(7)   COMP-3.
091813             20  CF-AH-PAY-APP-LEVEL-5      PIC S9(7)   COMP-3.
091813             20  CF-AH-APP-DAY-LEVEL-5      PIC S9(5)   COMP-3.
091813
091813         16  FILLER                         PIC X(68).
00421 ****************************************************************
00422 *             PROCESSOR/USER RECORD                            *
00423 ****************************************************************
00424
00425      12  CF-PROCESSOR-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00426          16  CF-PROCESSOR-NAME              PIC X(30).
00427          16  CF-PROCESSOR-PASSWORD          PIC X(11).
00428          16  CF-PROCESSOR-TITLE             PIC X(26).
00429          16  CF-MESSAGE-AT-LOGON-CAP        PIC X.
00430                  88  MESSAGE-YES                VALUE 'Y'.
00431                  88  MESSAGE-NO                 VALUE ' ' 'N'.
00432
00433 *****************************************************
00434 ****  OCCURRENCE (1) CREDIT APPLICATIONS         ****
00435 ****  OCCURRENCE (2) CLAIMS APPLICATIONS         ****
00436 ****  OCCURRENCE (3) CREDIT CARD APPLICATIONS    ****
00437 ****  OCCURRENCE (4) ACCT RECV APPLICATIONS      ****
00438 *****************************************************
00439
00440          16  CF-SYSTEM-SECURITY  OCCURS  4 TIMES.
00441              20  CF-ADMINISTRATION-CONTROLS PIC XX.
00442              20  CF-APPLICATION-FORCE       PIC X.
00443              20  CF-INDIVIDUAL-APP.
00444                  24  CF-APP-SWITCHES  OCCURS  44 TIMES.
00445                      28  CF-BROWSE-APP      PIC X.
00446                      28  CF-UPDATE-APP      PIC X.
00447
00448          16  CF-CURRENT-TERM-ON             PIC X(4).
00449          16  CF-PROCESSOR-LIMITS-CLAIMS.
00450              20  CF-PROC-CALC-AMT-TOL       PIC S999V99   COMP-3.
00451              20  CF-PROC-MAX-REG-PMT        PIC S9(7)V99  COMP-3.
00452              20  CF-PROC-MAX-REG-DAYS       PIC S999      COMP-3.
00453              20  CF-PROC-MAX-AUTO-PMT       PIC S9(7)V99  COMP-3.
00454              20  CF-PROC-MAX-AUTO-MOS       PIC S999      COMP-3.
00455              20  CF-PROC-CALC-DAYS-TOL      PIC S999      COMP-3.
00456              20  CF-PROC-MAX-LF-PMT         PIC S9(7)V99  COMP-3.
00457          16  CF-PROCESSOR-CARRIER           PIC X.
00458              88  NO-CARRIER-SECURITY            VALUE ' '.
00459          16  CF-PROCESSOR-ACCOUNT           PIC X(10).
00460              88  NO-ACCOUNT-SECURITY            VALUE SPACES.
00461          16  CF-PROCESSOR-LIFE-ACCESS       PIC X.
00462              88  PROCESSOR-HAS-LIFE-ACCESS      VALUE 'Y'.
00463          16  CF-PROCESSOR-USER-ALMIGHTY     PIC X.
00464              88  PROCESSOR-USER-IS-ALMIGHTY     VALUE 'Y'.
00465
00466          16  CF-PROC-SYS-ACCESS-SW.
00467              20  CF-PROC-CREDIT-CLAIMS-SW.
00468                  24  CF-PROC-SYS-ACCESS-CREDIT  PIC X.
00469                      88  ACCESS-TO-CREDIT           VALUE 'Y'.
00470                  24  CF-PROC-SYS-ACCESS-CLAIMS  PIC X.
00471                      88  ACCESS-TO-CLAIMS           VALUE 'Y'.
00472              20  CF-PROC-CREDIT-CLAIMS   REDEFINES
00473                  CF-PROC-CREDIT-CLAIMS-SW       PIC XX.
00474                  88  ACCESS-TO-CLAIM-CREDIT         VALUE 'YY'.
00475              20  CF-PROC-LIFE-GNRLDGR-SW.
00476                  24  CF-PROC-SYS-ACCESS-LIFE    PIC X.
00477                      88  ACCESS-TO-LIFE             VALUE 'Y'.
00478                  24  CF-PROC-SYS-ACCESS-GNRLDGR PIC X.
00479                      88  ACCESS-TO-GNRLDGR          VALUE 'Y'.
00480              20  CF-PROC-LIFE-GNRLDGR    REDEFINES
00481                  CF-PROC-LIFE-GNRLDGR-SW        PIC XX.
00482                  88  ACCESS-TO-LIFE-GNRLDGR         VALUE 'YY'.
00483          16  CF-PROC-SYS-ACCESS-ALL      REDEFINES
00484              CF-PROC-SYS-ACCESS-SW              PIC X(4).
00485              88  ACCESS-TO-ALL-SYSTEMS              VALUE 'YYYY'.
00486          16  CF-PROCESSOR-PRINTER               PIC X(4).
00487
00488          16  CF-APPROVAL-LEVEL                  PIC X.
00489              88  APPROVAL-LEVEL-1                   VALUE '1'.
00490              88  APPROVAL-LEVEL-2                   VALUE '2'.
00491              88  APPROVAL-LEVEL-3                   VALUE '3'.
031808             88  APPROVAL-LEVEL-4                   VALUE '4'.
091813             88  APPROVAL-LEVEL-5                   VALUE '5'.
00492
00493          16  CF-PROC-MAX-EXP-PMT            PIC S9(7)V99  COMP-3.
00494
00495          16  CF-LANGUAGE-TYPE                   PIC X.
00496              88  CF-LANG-IS-ENG                     VALUE 'E'.
00497              88  CF-LANG-IS-FR                      VALUE 'F'.
011812
011812         16  CF-CSR-IND                         PIC X.
011812         16  FILLER                             PIC X(239).
00499
00500 ****************************************************************
00501 *             PROCESSOR/REMINDERS RECORD                       *
00502 ****************************************************************
00503
00504      12  CF-PROCESSOR-REMINDER-REC  REDEFINES  CF-RECORD-BODY.
00505          16  CF-PROCESSOR-REMINDERS  OCCURS 8 TIMES.
00506              20  CF-START-REMIND-DT         PIC XX.
00507              20  CF-END-REMIND-DT           PIC XX.
00508              20  CF-REMINDER-TEXT           PIC X(50).
00509          16  FILLER                         PIC X(296).
00510
00511
00512 ****************************************************************
00513 *             STATE MASTER RECORD                              *
00514 ****************************************************************
00515
00516      12  CF-STATE-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00517          16  CF-STATE-ABBREVIATION          PIC XX.
00518          16  CF-STATE-NAME                  PIC X(25).
00519          16  CF-ST-CALC-INTEREST            PIC S9V9(4)   COMP-3.
00520          16  CF-ST-CALC-QUOTE-TOLERANCE.
00521              20  CF-ST-TOL-CLAIM            PIC S999V99   COMP-3.
00522              20  CF-ST-TOL-PREM             PIC S999V99   COMP-3.
00523              20  CF-ST-TOL-REFUND           PIC S999V99   COMP-3.
00524              20  CF-ST-CLAIM-REJECT-SW      PIC X.
00525                  88 ST-WARN-IF-CLAIM-OUT        VALUE SPACE.
00526                  88 ST-FORCE-IF-CLAIM-OUT       VALUE '1'.
00527              20  CF-ST-PREM-REJECT-SW       PIC X.
00528                  88 ST-WARN-IF-PREM-OUT         VALUE SPACE.
00529                  88 ST-FORCE-IF-PREM-OUT        VALUE '1'.
00530              20  CF-ST-REF-REJECT-SW        PIC X.
00531                  88 ST-WARN-IF-REF-OUT          VALUE SPACE.
00532                  88 ST-FORCE-IF-REF-OUT         VALUE '1'.
00533          16  CF-ST-LF-EXP-PCT               PIC S999V9(4) COMP-3.
00534          16  CF-ST-AH-EXP-PCT               PIC S999V9(4) COMP-3.
00535          16  CF-ST-REFUND-RULES.
00536              20  CF-ST-REFUND-MIN           PIC S999V99    COMP-3.
00537              20  CF-ST-REFUND-DAYS-FIRST    PIC 99.
00538              20  CF-ST-REFUND-DAYS-SUBSEQ   PIC 99.
00539          16  CF-ST-FST-PMT-EXTENSION.
00540              20  CF-ST-FST-PMT-DAYS-MAX     PIC 999.
00541              20  CF-ST-FST-PMT-DAYS-CHG     PIC X.
00542                  88  CF-ST-EXT-NO-CHG           VALUE ' '.
00543                  88  CF-ST-EXT-CHG-LF           VALUE '1'.
00544                  88  CF-ST-EXT-CHG-AH           VALUE '2'.
00545                  88  CF-ST-EXT-CHG-LF-AH        VALUE '3'.
00546          16  CF-ST-STATE-CALL.
00547              20  CF-ST-CALL-UNEARNED        PIC X.
00548              20  CF-ST-CALL-RPT-CNTL        PIC X.
00549              20  CF-ST-CALL-RATE-DEV        PIC XXX.
00550          16  CF-REPLACEMENT-LAW-SW          PIC X.
00551              88  CF-REPLACEMENT-LAW-APPLIES     VALUE 'Y'.
00552              88  CF-REPL-LAW-NOT-APPLICABLE     VALUE 'N'.
00553          16  CF-REPLACEMENT-LETTER          PIC X(4).
00554          16  CF-ST-TOL-PREM-PCT             PIC S9V9999 COMP-3.
00555          16  CF-ST-TOL-REF-PCT              PIC S9V9999 COMP-3.
00556          16  CF-ST-TARGET-LOSS-RATIO        PIC S9V9(4) COMP-3.
00557          16  CF-ST-SPLIT-PAYMENT            PIC X.
00558          16  FILLER                         PIC X.
00559          16  CF-STATE-BENEFIT-CNTL  OCCURS 50 TIMES.
00560              20  CF-ST-BENEFIT-CD           PIC XX.
00561              20  CF-ST-BENEFIT-KIND         PIC X.
00562                  88  CF-ST-LIFE-KIND            VALUE 'L'.
00563                  88  CF-ST-AH-KIND              VALUE 'A'.
00564              20  CF-ST-REM-TERM-CALC        PIC X.
00565                  88  ST-REM-TERM-NOT-USED       VALUE SPACE.
00566                  88  ST-EARN-AFTER-15TH         VALUE '1'.
00567                  88  ST-EARN-ON-HALF-MO         VALUE '2'.
00568                  88  ST-EARN-ON-1ST-DAY         VALUE '3'.
00569                  88  ST-EARN-ON-FULL-MO         VALUE '4'.
00570                  88  ST-EARN-WITH-NO-DAYS       VALUE '5'.
00571                  88  ST-EARN-AFTER-14TH         VALUE '6'.
00572                  88  ST-EARN-AFTER-16TH         VALUE '7'.
00573
00574              20  CF-ST-REFUND-CALC          PIC X.
00575                  88  ST-REFUND-NOT-USED         VALUE SPACE.
00576                  88  ST-REFD-BY-R78             VALUE '1'.
00577                  88  ST-REFD-BY-PRO-RATA        VALUE '2'.
00578                  88  ST-REFD-AS-CALIF           VALUE '3'.
00579                  88  ST-REFD-AS-TEXAS           VALUE '4'.
00580                  88  ST-REFD-IS-NET-PAY         VALUE '5'.
00581                  88  ST-REFD-ANTICIPATION       VALUE '6'.
00582                  88  ST-REFD-UTAH               VALUE '7'.
00583                  88  ST-REFD-SUM-OF-DIGITS      VALUE '9'.
00584                  88  ST-REFD-REG-BALLOON        VALUE 'B'.
033104                 88  ST-REFD-GAP-NON-REFUND     VALUE 'G'.
00585
00586              20  CF-ST-EARNING-CALC         PIC X.
00587                  88  ST-EARNING-NOT-USED        VALUE SPACE.
00588                  88  ST-EARN-BY-R78             VALUE '1'.
00589                  88  ST-EARN-BY-PRO-RATA        VALUE '2'.
00590                  88  ST-EARN-AS-CALIF           VALUE '3'.
00591                  88  ST-EARN-AS-TEXAS           VALUE '4'.
00592                  88  ST-EARN-IS-NET-PAY         VALUE '5'.
00593                  88  ST-EARN-ANTICIPATION       VALUE '6'.
00594                  88  ST-EARN-MEAN               VALUE '8'.
00595                  88  ST-EARN-REG-BALLOON        VALUE 'B'.
00596
00597              20  CF-ST-OVRD-EARNINGS-CALC   PIC X.
00598                  88  ST-OVERRIDE-NOT-USED       VALUE SPACE.
00599                  88  ST-OVRD-BY-R78             VALUE '1'.
00600                  88  ST-OVRD-BY-PRO-RATA        VALUE '2'.
00601                  88  ST-OVRD-AS-CALIF           VALUE '3'.
00602                  88  ST-OVRD-AS-TEXAS           VALUE '4'.
00603                  88  ST-OVRD-IS-NET-PAY         VALUE '5'.
00604                  88  ST-OVRD-ANTICIPATION       VALUE '6'.
00605                  88  ST-OVRD-MEAN               VALUE '8'.
00606                  88  ST-OVRD-REG-BALLOON        VALUE 'B'.
                   20  cf-st-extra-periods        pic 9.
00607 *            20  FILLER                     PIC X.
00608
00609          16  CF-ST-COMMISSION-CAPS.
00610              20  CF-ST-COMM-CAP-SL          PIC S9V9(4) COMP-3.
00611              20  CF-ST-COMM-CAP-JL          PIC S9V9(4) COMP-3.
00612              20  CF-ST-COMM-CAP-SA          PIC S9V9(4) COMP-3.
00613              20  CF-ST-COMM-CAP-JA          PIC S9V9(4) COMP-3.
00614          16  CF-COMM-CAP-LIMIT-TO           PIC X.
00615                  88  ST-LIMIT-TO-ACCOUNT        VALUE 'A'.
102717                 88  ST-LIMIT-TO-GA             VALUE 'G'.
102717                 88  ST-LIMIT-TO-BOTH           VALUE 'B'.
00616
00617          16  CF-ST-RES-TAX-PCT              PIC S9V9(4) COMP-3.
00618
00619          16  CF-ST-STATUTORY-INTEREST.
00620              20  CF-ST-STAT-DATE-FROM       PIC X.
00621                  88  ST-STAT-FROM-INCURRED      VALUE 'I'.
00622                  88  ST-STAT-FROM-REPORTED      VALUE 'R'.
00623              20  CF-ST-NO-DAYS-ELAPSED      PIC 99.
00624              20  CF-ST-STAT-INTEREST        PIC S9V9(4) COMP-3.
00625              20  CF-ST-STAT-INTEREST-1      PIC S9V9(4) COMP-3.
00626              20  CF-ST-STAT-INTEREST-2      PIC S9V9(4) COMP-3.
00627              20  CF-ST-STAT-INTEREST-3      PIC S9V9(4) COMP-3.
00628
00629          16  CF-ST-OVER-SHORT.
00630              20 CF-ST-OVR-SHT-AMT           PIC S999V99 COMP-3.
00631              20 CF-ST-OVR-SHT-PCT           PIC S9V9(4) COMP-3.
00632
00633          16  CF-ST-FREE-LOOK-PERIOD         PIC S9(3)   COMP-3.
00634
CIDMOD         16  CF-ST-RT-CALC                  PIC X.
CIDMOD
PEMMOD         16  CF-ST-LF-PREM-TAX              PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-AH-PREM-TAX-I            PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-AH-PREM-TAX-G            PIC S9V9(4) COMP-3.
PEMMOD         16  CF-ST-RF-LR-CALC               PIC X.
PEMMOD         16  CF-ST-RF-LL-CALC               PIC X.
PEMMOD         16  CF-ST-RF-LN-CALC               PIC X.
PEMMOD         16  CF-ST-RF-AH-CALC               PIC X.
PEMMOD         16  CF-ST-RF-CP-CALC               PIC X.
PEMMOD*        16  FILLER                         PIC X(206).
091808*CIDMOD         16  FILLER                         PIC X(192).
091808         16  CF-ST-CHECK-COUNTER            PIC S9(8)   COMP.
091808             88  CF-ST-CHECK-CNT-RESET      VALUE +9999999.
011410         16  CF-ST-REF-AH-DEATH-IND         PIC X.
061511         16  CF-ST-VFY-2ND-BENE             PIC X.
012913         16  CF-ST-CAUSAL-STATE             PIC X.
022415         16  CF-ST-EXTRA-INTEREST-PERIODS   PIC 9.
022415         16  CF-ST-EXTRA-PAYMENTS           PIC 9.
040915         16  CF-ST-AGENT-SIG-EDIT           PIC X.
040915             88  CF-ST-EDIT-FOR-SIG           VALUE 'Y'.
070115         16  CF-ST-NET-ONLY-STATE           PIC X.
070115             88  CF-ST-IS-NET-ONLY            VALUE 'Y'.
102717         16  cf-commission-cap-required     pic x.
102717         16  CF-ST-GA-COMMISSION-CAPS.
102717             20  CF-ST-GA-COMM-CAP-SL       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-JL       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-SA       PIC S9V9(4) COMP-3.
102717             20  CF-ST-GA-COMM-CAP-JA       PIC S9V9(4) COMP-3.
102717         16  CF-ST-TOT-COMMISSION-CAPS.
102717             20  CF-ST-TOT-COMM-CAP-SL      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-JL      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-SA      PIC S9V9(4) COMP-3.
102717             20  CF-ST-TOT-COMM-CAP-JA      PIC S9V9(4) COMP-3.
102717         16  FILLER                         PIC X(156).
00636
00637 ****************************************************************
00638 *             BENEFIT MASTER RECORD                            *
00639 ****************************************************************
00640
00641      12  CF-BENEFIT-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00642          16  CF-BENEFIT-CONTROLS  OCCURS 8 TIMES.
00643              20  CF-BENEFIT-CODE            PIC XX.
00644              20  CF-BENEFIT-NUMERIC  REDEFINES
00645                  CF-BENEFIT-CODE            PIC XX.
00646              20  CF-BENEFIT-ALPHA           PIC XXX.
00647              20  CF-BENEFIT-DESCRIP         PIC X(10).
00648              20  CF-BENEFIT-COMMENT         PIC X(10).
00649
00650              20  CF-LF-COVERAGE-TYPE        PIC X.
00651                  88  CF-REDUCING                VALUE 'R'.
00652                  88  CF-LEVEL                   VALUE 'L' 'P'.
00653
00654              20  CF-SPECIAL-CALC-CD         PIC X.
00655                  88  CF-ALTERNATE-NET-PAY       VALUE 'A'.
00656                  88  CF-NP-0-MO-INT             VALUE 'A'.
00657                  88  CF-OB-OFFLINE-RESERVED     VALUE 'B'.
00658                  88  CF-CRITICAL-PERIOD         VALUE 'C'.
00659                  88  CF-TERM-IN-DAYS            VALUE 'D'.
00660                  88  CF-USE-PREMIUM-AS-ENTERED  VALUE 'E'.
00661                  88  CF-FARM-PLAN               VALUE 'F'.
00662                  88  CF-RATE-AS-STANDARD        VALUE 'G'.
00663                  88  CF-2-MTH-INTEREST          VALUE 'I'.
00664                  88  CF-3-MTH-INTEREST          VALUE 'J'.
00665                  88  CF-4-MTH-INTEREST          VALUE 'K'.
00666                  88  CF-BALLOON-LAST-PMT        VALUE 'L'.
00667                  88  CF-MORTGAGE-PROCESSING     VALUE 'M'.
00668                  88  CF-PRUDENTIAL              VALUE 'P'.
00669                  88  CF-OUTSTANDING-BAL         VALUE 'O'.
00670                  88  CF-TRUNCATED-LIFE          VALUE 'T'.
00671                  88  CF-TRUNCATED-LIFE-ONE      VALUE 'U'.
00672                  88  CF-TRUNCATED-LIFE-TWO      VALUE 'V'.
00673                  88  CF-NET-PAY-SIMPLE          VALUE 'S'.
00674                  88  CF-SUMMARY-PROCESSING      VALUE 'Z'.
00675
00676              20  CF-JOINT-INDICATOR         PIC X.
00677                  88  CF-JOINT-COVERAGE          VALUE 'J'.
00678
082603*            20  FILLER                     PIC X(12).
                   20  cf-maximum-benefits        pic s999 comp-3.
                   20  FILLER                     PIC X(09).
082503             20  CF-BENEFIT-CATEGORY        PIC X.
00680              20  CF-LOAN-TYPE               PIC X(8).
00681
00682              20  CF-CO-REM-TERM-CALC        PIC X.
00683                  88  CO-EARN-AFTER-15TH         VALUE '1'.
00684                  88  CO-EARN-ON-HALF-MO         VALUE '2'.
00685                  88  CO-EARN-ON-1ST-DAY         VALUE '3'.
00686                  88  CO-EARN-ON-FULL-MO         VALUE '4'.
00687                  88  CO-EARN-WITH-NO-DAYS       VALUE '5'.
00688
00689              20  CF-CO-EARNINGS-CALC        PIC X.
00690                  88  CO-EARN-BY-R78             VALUE '1'.
00691                  88  CO-EARN-BY-PRO-RATA        VALUE '2'.
00692                  88  CO-EARN-AS-CALIF           VALUE '3'.
00693                  88  CO-EARN-AS-TEXAS           VALUE '4'.
00694                  88  CO-EARN-IS-NET-PAY         VALUE '5'.
00695                  88  CO-EARN-ANTICIPATION       VALUE '6'.
00696                  88  CO-EARN-AS-MEAN            VALUE '8'.
00697                  88  CO-EARN-AS-REG-BALLOON     VALUE 'B'.
00698
00699              20  CF-CO-REFUND-CALC          PIC X.
00700                  88  CO-REFUND-NOT-USED         VALUE SPACE.
00701                  88  CO-REFD-BY-R78             VALUE '1'.
00702                  88  CO-REFD-BY-PRO-RATA        VALUE '2'.
00703                  88  CO-REFD-AS-CALIF           VALUE '3'.
00704                  88  CO-REFD-AS-TEXAS           VALUE '4'.
00705                  88  CO-REFD-IS-NET-PAY         VALUE '5'.
00706                  88  CO-REFD-ANTICIPATION       VALUE '6'.
00707                  88  CO-REFD-MEAN               VALUE '8'.
00708                  88  CO-REFD-SUM-OF-DIGITS      VALUE '9'.
00709                  88  CO-REFD-AS-REG-BALLOON     VALUE 'B'.
033104                 88  CO-REFD-GAP-NON-REFUND     VALUE 'G'.
00710
00711              20  CF-CO-OVRD-EARNINGS-CALC   PIC X.
00712                  88  CO-OVERRIDE-NOT-USED       VALUE SPACE.
00713                  88  CO-OVRD-BY-R78             VALUE '1'.
00714                  88  CO-OVRD-BY-PRO-RATA        VALUE '2'.
00715                  88  CO-OVRD-AS-CALIF           VALUE '3'.
00716                  88  CO-OVRD-AS-TEXAS           VALUE '4'.
00717                  88  CO-OVRD-IS-NET-PAY         VALUE '5'.
00718                  88  CO-OVRD-ANTICIPATION       VALUE '6'.
00719                  88  CO-OVRD-MEAN               VALUE '8'.
00720                  88  CO-OVRD-AS-REG-BALLOON     VALUE 'B'.
00721
00722              20  CF-CO-BEN-I-G-CD           PIC X.
00723                  88  CO-BEN-I-G-NOT-USED        VALUE SPACE.
00724                  88  CO-BEN-I-G-IS-INDV         VALUE 'I'.
00725                  88  CO-BEN-I-G-IS-GRP          VALUE 'G'.
00726
00727          16  FILLER                         PIC X(304).
00728
00729
00730 ****************************************************************
00731 *             CARRIER MASTER RECORD                            *
00732 ****************************************************************
00733
00734      12  CF-CARRIER-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00735          16  CF-ADDRESS-DATA.
00736              20  CF-MAIL-TO-NAME            PIC X(30).
00737              20  CF-IN-CARE-OF              PIC X(30).
00738              20  CF-ADDRESS-LINE-1          PIC X(30).
00739              20  CF-ADDRESS-LINE-2          PIC X(30).
00740              20  CF-CITY-STATE              PIC X(30).
00741              20  CF-ZIP-CODE-NUM            PIC 9(9)      COMP-3.
00742              20  CF-PHONE-NO                PIC 9(11)     COMP-3.
00743
00744          16  CF-CLAIM-NO-CONTROL.
00745              20  CF-CLAIM-NO-METHOD         PIC X.
00746                  88  CLAIM-NO-MANUAL            VALUE '1'.
00747                  88  CLAIM-NO-Y-M-SEQ           VALUE '2'.
00748                  88  CLAIM-NO-SEQ               VALUE '3'.
00749                  88  CLAIM-NO-ALPHA-SEQ         VALUE '5'.
00750              20  CF-CLAIM-COUNTER           PIC S9(8)   COMP.
00751                  88  CLAIM-CNT-RESET-IF-SEQ     VALUE +9999999.
00752                  88  CLAIM-CNT-RESET-IF-YRMO    VALUE +99999.
00753                  88  CLAIM-CNT-RESET-IF-YRALPHA VALUE +9999.
00754
00755          16  CF-CHECK-NO-CONTROL.
00756              20  CF-CHECK-NO-METHOD         PIC X.
00757                  88  CHECK-NO-MANUAL            VALUE '1'.
00758                  88  CHECK-NO-AUTO-SEQ          VALUE '2'.
00759                  88  CHECK-NO-CARR-SEQ          VALUE '3'.
00760                  88  CHECK-NO-AT-PRINT          VALUE '4'.
00761              20  CF-CHECK-COUNTER           PIC S9(8)   COMP.
00762                  88  CHECK-CNT-RESET-VALUE      VALUE +999999.
00763
00764          16  CF-DOMICILE-STATE              PIC XX.
00765
00766          16  CF-EXPENSE-CONTROLS.
00767              20  CF-EXPENSE-METHOD          PIC X.
00768                  88  EXPENSE-CALC-MANUAL        VALUE '1'.
00769                  88  DOLLARS-PER-PMT            VALUE '2'.
00770                  88  PERCENT-OF-PAYMENT         VALUE '3'.
00771                  88  DOLLARS-PER-MONTH          VALUE '4'.
00772              20  CF-EXPENSE-PERCENT         PIC S999V99   COMP-3.
00773              20  CF-EXPENSE-DOLLAR          PIC S999V99   COMP-3.
00774
00775          16  CF-CORRESPONDENCE-CONTROL.
00776              20  CF-LETTER-RESEND-OPT       PIC X.
00777                  88  LETTERS-NOT-ARCHIVED       VALUE SPACE.
00778                  88  LETTERS-ARE-ARCHIVED       VALUE '1'.
00779              20  FILLER                     PIC X(4).
00780
00781          16  CF-RESERVE-CONTROLS.
00782              20  CF-MANUAL-SW               PIC X.
00783                  88  CF-MANUAL-RESERVES-USED    VALUE '1'.
00784              20  CF-FUTURE-SW               PIC X.
00785                  88  CF-FUTURE-RESERVES-USED    VALUE '1'.
00786              20  CF-PTC-SW                  PIC X.
00787                  88  CF-PAY-TO-CURRENT-USED     VALUE '1'.
00788              20  CF-IBNR-SW                 PIC X.
00789                  88  CF-IBNR-RESERVES-USED      VALUE '1'.
00790              20  CF-PTC-LF-SW               PIC X.
00791                  88  CF-LF-PTC-USED             VALUE '1'.
00792              20  CF-CDT-ACCESS-METHOD       PIC X.
00793                  88  CF-CDT-ROUND-NEAR          VALUE '1'.
00794                  88  CF-CDT-ROUND-HIGH          VALUE '2'.
00795                  88  CF-CDT-INTERPOLATED        VALUE '3'.
00796              20  CF-PERCENT-OF-CDT          PIC S999V99   COMP-3.
00797
00798          16  CF-CLAIM-CALC-METHOD           PIC X.
00799              88  360-PLUS-MONTHS                VALUE '1'.
00800              88  365-PLUS-MONTHS                VALUE '2'.
00801              88  FULL-MONTHS-ACTUAL-DAY         VALUE '3'.
00802              88  360-DAILY                      VALUE '4'.
00803              88  365-DAILY                      VALUE '5'.
00804
00805          16  CF-LAST-ALPHA-CHARACTER        PIC X.
00806          16  FILLER                         PIC X(11).
00807
00808          16  CF-LIMIT-AMOUNTS.
00809              20  CF-CALC-AMT-TOL            PIC S999V99   COMP-3.
00810              20  CF-MAX-REG-PMT             PIC S9(7)V99  COMP-3.
00811              20  CF-MAX-REG-DAYS            PIC S999      COMP-3.
00812              20  CF-MAX-AUTO-PMT            PIC S9(7)V99  COMP-3.
00813              20  CF-MAX-AUTO-MOS            PIC S999      COMP-3.
00814              20  CF-CALC-DAYS-TOL           PIC S999      COMP-3.
00815              20  CF-CR-TOL-PREM             PIC S999V99   COMP-3.
00816              20  CF-CR-TOL-REFUND           PIC S999V99   COMP-3.
00817              20  CF-CR-TOL-PREM-PCT         PIC S9V9(4)   COMP-3.
00818              20  CF-CR-TOL-REFUND-PCT       PIC S9V9(4)   COMP-3.
00819
00820          16  CF-DAYS-BEFORE-CLOSED          PIC S999      COMP-3.
00821          16  CF-MONTHS-BEFORE-PURGED        PIC S999      COMP-3.
00822          16  CF-IBNR-PERCENT                PIC S9V9(4)   COMP-3.
00823
00824          16  CF-ZIP-CODE.
00825              20  CF-ZIP-PRIME.
00826                  24  CF-ZIP-1ST             PIC X.
00827                      88  CF-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00828                  24  FILLER                 PIC X(4).
00829              20  CF-ZIP-PLUS4               PIC X(4).
00830          16  CF-CANADIAN-POSTAL-CODE REDEFINES CF-ZIP-CODE.
00831              20  CF-CAN-POSTAL-1            PIC XXX.
00832              20  CF-CAN-POSTAL-2            PIC XXX.
00833              20  FILLER                     PIC XXX.
00834
00835          16  CF-IBNR-UEPRM-PERCENT          PIC S9V9(4) COMP-3.
00836          16  CF-IBNR-R78-PERCENT            PIC S9V9(4) COMP-3.
00837          16  CF-IBNR-PRO-PERCENT            PIC S9V9(4) COMP-3.
00838
00839          16  CF-RATING-SWITCH               PIC X.
00840              88  CF-PERFORM-RATING              VALUE ' ' 'Y'.
00841              88  CF-NO-RATING                   VALUE 'N'.
00842
00843          16  CF-BUILD-RETRIEVE-AFTER-MONTHS PIC 99.
00844
00845          16  CF-CARRIER-OVER-SHORT.
00846              20 CF-CR-OVR-SHT-AMT           PIC S999V99   COMP-3.
00847              20 CF-CR-OVR-SHT-PCT           PIC S9V9(4)   COMP-3.
00848
100703         16  CF-CARRIER-CLP-TOL-PCT         PIC S9V9(4)   COMP-3.
100703         16  CF-SECPAY-SWITCH               PIC X.
100703             88  CF-SECURE-PAY-CARRIER          VALUE 'Y'.
100703             88  CF-NO-SECURE-PAY               VALUE ' ' 'N'.
092705         16  CF-CARRIER-LEASE-COMM          PIC S9(5)V99  COMP-3.
032813         16  CF-CARRIER-NEXT-AUDIT-CHK-NO   PIC S9(8)     COMP.
032813         16  FILLER                         PIC X(444).
100703*        16  FILLER                         PIC X(452).
00850
00851
00852 ****************************************************************
00853 *             MORTALITY MASTER RECORD                          *
00854 ****************************************************************
00855
00856      12  CF-MORTALITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
00857          16  CF-MORT-TABLE-LINE OCCURS  9  TIMES
00858                                 INDEXED BY CF-MORT-NDX.
00859              20  CF-MORT-TABLE              PIC X(5).
00860              20  CF-MORT-TABLE-TYPE         PIC X.
00861                  88  CF-MORT-JOINT              VALUE 'J'.
00862                  88  CF-MORT-SINGLE             VALUE 'S'.
00863                  88  CF-MORT-COMBINED           VALUE 'C'.
00864                  88  CF-MORT-TYPE-VALID-C       VALUE 'J' 'S'.
00865                  88  CF-MORT-TYPE-VALID-M       VALUE 'J' 'S' 'C'.
00866              20  CF-MORT-INTEREST           PIC SV9(4)  COMP-3.
00867              20  CF-MORT-AGE-METHOD         PIC XX.
00868                  88  CF-AGE-LAST                VALUE 'AL'.
00869                  88  CF-AGE-NEAR                VALUE 'AN'.
00870              20  CF-MORT-RESERVE-ADJUSTMENT PIC S9V9(4) COMP-3.
00871              20  CF-MORT-ADJUSTMENT-DIRECTION
00872                                             PIC X.
00873                  88  CF-MINUS                   VALUE '-'.
00874                  88  CF-PLUS                    VALUE '+'.
00875              20  CF-MORT-JOINT-FACTOR       PIC S9V9(4) COMP-3.
00876              20  CF-MORT-JOINT-CODE         PIC X.
00877                  88  CF-VALID-JOINT-CODE        VALUE 'A' 'V'.
00878              20  CF-MORT-PC-Q               PIC X.
00879                  88  CF-VALID-PC-Q              VALUE 'Y' 'N' ' '.
00880              20  CF-MORT-TABLE-CODE         PIC X(4).
00881              20  CF-MORT-COMMENTS           PIC X(15).
00882              20  FILLER                     PIC X(14).
00883
00884          16  FILLER                         PIC X(251).
00885
00886
00887 ****************************************************************
00888 *             BUSSINESS TYPE MASTER RECORD                     *
00889 ****************************************************************
00890
00891      12  CF-BUSINESS-TYPE-MASTER-REC REDEFINES  CF-RECORD-BODY.
00892 * FIRST ENTRY IS TYPE 01.. LAST IS TYPE 20
00893 * RECORD 02 IS TYPES 21-40..RECORD 03 IS 41-60..04 IS 61-80
00894 * AND RECORD 05 IS TYPES 81-99
00895          16  CF-TYPE-DESCRIPTIONS   OCCURS  20  TIMES.
00896              20  CF-BUSINESS-TITLE          PIC  X(19).
00897              20  CF-BUS-MOD-ST-TRGT-LOSS-RATIO
00898                                             PIC S9V9(4) COMP-3.
00899              20  CF-BUS-EXCL-ST-CALL        PIC  X.
00900              20  FILLER                     PIC  X.
00901          16  FILLER                         PIC  X(248).
00902
00903
00904 ****************************************************************
00905 *             TERMINAL MASTER RECORD                           *
00906 ****************************************************************
00907
00908      12  CF-TERMINAL-MASTER-REC  REDEFINES  CF-RECORD-BODY.
00909
00910          16  CF-COMPANY-TERMINALS.
00911              20  CF-TERMINAL-ID  OCCURS 120 TIMES
00912                                   PIC X(4).
00913          16  FILLER               PIC X(248).
00914
00915
00916 ****************************************************************
00917 *             LIFE EDIT MASTER RECORD                          *
00918 ****************************************************************
00919
00920      12  CF-LIFE-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
00921          16  CF-LIFE-EDIT-ENTRIES   OCCURS 120  TIMES.
00922              20  CF-LIFE-CODE-IN            PIC XX.
00923              20  CF-LIFE-CODE-OUT           PIC XX.
00924          16  FILLER                         PIC X(248).
00925
00926
00927 ****************************************************************
00928 *             AH EDIT MASTER RECORD                            *
00929 ****************************************************************
00930
00931      12  CF-AH-EDIT-MASTER-REC REDEFINES  CF-RECORD-BODY.
00932          16  CF-AH-EDIT-ENTRIES   OCCURS  96  TIMES.
00933              20  CF-AH-CODE-IN              PIC XXX.
00934              20  CF-AH-CODE-OUT             PIC XX.
00935          16  FILLER                         PIC X(248).
00936
00937
00938 ****************************************************************
00939 *             CREDIBILITY TABLES                               *
00940 ****************************************************************
00941
00942      12  CF-CREDIBILITY-MASTER-REC REDEFINES  CF-RECORD-BODY.
00943          16  CF-CRDB-ENTRY   OCCURS 36 TIMES
00944                              INDEXED BY CF-CRDB-NDX.
00945              20  CF-CRDB-FROM               PIC S9(7)   COMP-3.
00946              20  CF-CRDB-TO                 PIC S9(7)   COMP-3.
00947              20  CF-CREDIBILITY-FACTOR      PIC S9V9(4) COMP-3.
00948          16  FILLER                         PIC  X(332).
00949
00950
00951 ****************************************************************
00952 *             REPORT CUSTOMIZATION RECORD                      *
00953 ****************************************************************
00954
00955      12  CF-CUSTOM-REPORT-REC  REDEFINES  CF-RECORD-BODY.
00956          16  CF-ACCOUNT-MASTER-STATUS       PIC X.
00957              88  CF-ACTIVE-ACCOUNTS             VALUE 'A'.
00958              88  CF-INACTIVE-ACCOUNTS           VALUE 'I'.
121307             88  CF-CANCELLED-ACCOUNTS          VALUE 'C'.
00959 **** NOTE: INACTIVE WILL INCLUDE ACCOUNT MASTER CODED WITH ****
00960 ****       A T-TRANSFER.                                   ****
00961              88  CF-ALL-ACCOUNTS                VALUE 'B'.
00962
00963          16  FILLER                         PIC XX.
00964
00965          16  CF-CARRIER-CNTL-OPT.
00966              20  CF-CARRIER-OPT-SEQ         PIC 9.
00967                  88  CF-CARRIER-OPT-USED        VALUE 1 THRU 6.
00968                  88  CF-CARRIER-OPT-NOT-USED    VALUE 0.
00969              20  CF-CARRIER-SELECT OCCURS 3 TIMES
00970                                             PIC X.
00971          16  CF-GROUP-CNTL-OPT.
00972              20  CF-GROUP-OPT-SEQ           PIC 9.
00973                  88  CF-GROUP-OPT-USED          VALUE 1 THRU 6.
00974                  88  CF-GROUP-OPT-NOT-USED      VALUE 0.
00975              20  CF-GROUP-SELECT OCCURS 3 TIMES
00976                                             PIC X(6).
00977          16  CF-STATE-CNTL-OPT.
00978              20  CF-STATE-OPT-SEQ           PIC 9.
00979                  88  CF-STATE-OPT-USED          VALUE 1 THRU 6.
00980                  88  CF-STATE-OPT-NOT-USED      VALUE 0.
00981              20  CF-STATE-SELECT OCCURS 3 TIMES
00982                                             PIC XX.
00983          16  CF-ACCOUNT-CNTL-OPT.
00984              20  CF-ACCOUNT-OPT-SEQ         PIC 9.
00985                  88  CF-ACCOUNT-OPT-USED        VALUE 1 THRU 6.
00986                  88  CF-ACCOUNT-OPT-NOT-USED    VALUE 0.
00987              20  CF-ACCOUNT-SELECT OCCURS 3 TIMES
00988                                             PIC X(10).
00989          16  CF-BUS-TYP-CNTL-OPT.
00990              20  CF-BUS-TYP-OPT-SEQ         PIC 9.
00991                  88  CF-BUS-TYP-OPT-USED        VALUE 1 THRU 6.
00992                  88  CF-BUS-TYP-OPT-NOT-USED    VALUE 0.
00993              20  CF-BUS-TYP-SELECT OCCURS 3 TIMES
00994                                             PIC XX.
00995          16  CF-LF-TYP-CNTL-OPT.
00996              20  CF-LF-TYP-OPT-SEQ          PIC 9.
00997                  88  CF-LF-TYP-OPT-USED         VALUE 1 THRU 6.
00998                  88  CF-LF-TYP-OPT-NOT-USED     VALUE 0.
00999              20  CF-BUS-LF-SELECT OCCURS 3 TIMES
01000                                             PIC XX.
01001          16  CF-AH-TYP-CNTL-OPT.
01002              20  CF-AH-TYP-OPT-SEQ          PIC 9.
01003                  88  CF-AH-TYP-OPT-USED         VALUE 1 THRU 6.
01004                  88  CF-AH-TYP-OPT-NOT-USED     VALUE 0.
01005              20  CF-BUS-AH-SELECT OCCURS 3 TIMES
01006                                             PIC XX.
01007          16  CF-REPTCD1-CNTL-OPT.
01008              20  CF-REPTCD1-OPT-SEQ         PIC 9.
01009                  88  CF-REPTCD1-OPT-USED        VALUE 1 THRU 6.
01010                  88  CF-REPTCD1-OPT-NOT-USED    VALUE 0.
01011              20  CF-REPTCD1-SELECT OCCURS 3 TIMES
01012                                             PIC X(10).
01013          16  CF-REPTCD2-CNTL-OPT.
01014              20  CF-REPTCD2-OPT-SEQ         PIC 9.
01015                  88  CF-REPTCD2-OPT-USED        VALUE 1 THRU 6.
01016                  88  CF-REPTCD2-OPT-NOT-USED    VALUE 0.
01017              20  CF-REPTCD2-SELECT OCCURS 3 TIMES
01018                                             PIC X(10).
01019          16  CF-USER1-CNTL-OPT.
01020              20  CF-USER1-OPT-SEQ           PIC 9.
01021                  88  CF-USER1-OPT-USED          VALUE 1 THRU 6.
01022                  88  CF-USER1-OPT-NOT-USED      VALUE 0.
01023              20  CF-USER1-SELECT OCCURS 3 TIMES
01024                                             PIC X(10).
01025          16  CF-USER2-CNTL-OPT.
01026              20  CF-USER2-OPT-SEQ           PIC 9.
01027                  88  CF-USER2-OPT-USED          VALUE 1 THRU 6.
01028                  88  CF-USER2-OPT-NOT-USED      VALUE 0.
01029              20  CF-USER2-SELECT OCCURS 3 TIMES
01030                                             PIC X(10).
01031          16  CF-USER3-CNTL-OPT.
01032              20  CF-USER3-OPT-SEQ           PIC 9.
01033                  88  CF-USER3-OPT-USED          VALUE 1 THRU 6.
01034                  88  CF-USER3-OPT-NOT-USED      VALUE 0.
01035              20  CF-USER3-SELECT OCCURS 3 TIMES
01036                                             PIC X(10).
01037          16  CF-USER4-CNTL-OPT.
01038              20  CF-USER4-OPT-SEQ           PIC 9.
01039                  88  CF-USER4-OPT-USED          VALUE 1 THRU 6.
01040                  88  CF-USER4-OPT-NOT-USED      VALUE 0.
01041              20  CF-USER4-SELECT OCCURS 3 TIMES
01042                                             PIC X(10).
01043          16  CF-USER5-CNTL-OPT.
01044              20  CF-USER5-OPT-SEQ           PIC 9.
01045                  88  CF-USER5-OPT-USED          VALUE 1 THRU 6.
01046                  88  CF-USER5-OPT-NOT-USED      VALUE 0.
01047              20  CF-USER5-SELECT OCCURS 3 TIMES
01048                                             PIC X(10).
01049          16  CF-REINS-CNTL-OPT.
01050              20  CF-REINS-OPT-SEQ           PIC 9.
01051                  88  CF-REINS-OPT-USED          VALUE 1 THRU 6.
01052                  88  CF-REINS-OPT-NOT-USED      VALUE 0.
01053              20  CF-REINS-SELECT OCCURS 3 TIMES.
01054                  24  CF-REINS-PRIME         PIC XXX.
01055                  24  CF-REINS-SUB           PIC XXX.
01056
01057          16  CF-AGENT-CNTL-OPT.
01058              20  CF-AGENT-OPT-SEQ           PIC 9.
01059                  88  CF-AGENT-OPT-USED          VALUE 1 THRU 6.
01060                  88  CF-AGENT-OPT-NOT-USED      VALUE 0.
01061              20  CF-AGENT-SELECT OCCURS 3 TIMES
01062                                             PIC X(10).
01063
01064          16  FILLER                         PIC X(43).
01065
01066          16  CF-LOSS-RATIO-SELECT.
01067              20  CF-SEL-LO-LOSS-RATIO       PIC S999V99  COMP-3.
01068              20  CF-SEL-HI-LOSS-RATIO       PIC S999V99  COMP-3.
01069          16  CF-ENTRY-DATE-SELECT.
01070              20  CF-SEL-LO-ENTRY-DATE       PIC XX.
01071              20  CF-SEL-HI-ENTRY-DATE       PIC XX.
01072          16  CF-EFFECTIVE-DATE-SELECT.
01073              20  CF-SEL-LO-EFFECTIVE-DATE   PIC XX.
01074              20  CF-SEL-HI-EFFECTIVE-DATE   PIC XX.
01075
01076          16  CF-EXCEPTION-LIST-IND          PIC X.
01077              88  CF-EXCEPTION-LIST-REQUESTED VALUE 'Y'.
01078
01079          16  FILLER                         PIC X(318).
01080
01081 ****************************************************************
01082 *                  EXCEPTION REPORTING RECORD                  *
01083 ****************************************************************
01084
01085      12  CF-EXCEPTION-REPORT-REC REDEFINES   CF-RECORD-BODY.
01086          16  CF-ACCOUNTS-LT-ONE-YEAR        PIC X.
01087              88  CF-EXCEPTION-ACCTS-WITHIN-ONE  VALUE 'Y'.
01088
01089          16  CF-COMBINED-LIFE-AH-OPT.
01090              20  CF-ISS-COUNT-DIFF          PIC S9(05)     COMP-3.
01091              20  CF-SINGLE-MO-PREM-PCT      PIC S9(02).
01092              20  CF-EARN-PREM-DECR-PCT      PIC S9(02).
01093              20  CF-CANCELLATION-RATIO      PIC S9(02).
01094
01095          16  CF-LIFE-OPT.
01096              20  CF-LF-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
01097              20  CF-LF-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
01098              20  CF-LF-PERIOD-PROFIT        PIC S9(03)     COMP-3.
01099              20  CF-LF-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
01100              20  CF-LF-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
01101              20  CF-LF-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
01102              20  CF-LF-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
01103              20  CF-LF-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
01104              20  CF-LF-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
01105              20  CF-LF-AVG-AGE-MAX          PIC S9(02).
01106
01107          16  CF-AH-OPT.
01108              20  CF-AH-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.
01109              20  CF-AH-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.
01110              20  CF-AH-PERIOD-PROFIT        PIC S9(03)     COMP-3.
01111              20  CF-AH-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.
01112              20  CF-AH-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.
01113              20  CF-AH-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.
01114              20  CF-AH-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.
01115              20  CF-AH-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.
01116              20  CF-AH-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.
01117              20  CF-AH-AVG-AGE-MAX          PIC S9(02).
01118
01119          16  CF-ACCT-ZERO-MONTH-PRODUCTION PIC X.
01120              88  CF-ACCT-CURRENT-MONTH-ACT      VALUE 'A'.
01121              88  CF-ACCT-WITH-NO-PRODUCTION     VALUE 'B'.
01122              88  CF-ACCT-WITH-ISSUE-ACTIVITY    VALUE 'C'.
01123
01124          16  CF-RETENTION-LIMIT             PIC S9(7)      COMP-3.
01125
01126          16  FILLER                         PIC X(673).
01127
01128
01129 ****************************************************************
01130 *             MORTGAGE SYSTEM PLAN RECORD                      *
01131 ****************************************************************
01132
01133      12  CF-MORTGAGE-PLAN-MASTER  REDEFINES  CF-RECORD-BODY.
01134          16  CF-PLAN-TYPE                   PIC X.
01135              88  CF-LIFE-MORT-PLAN             VALUE 'L'.
01136              88  CF-DISAB-MORT-PLAN            VALUE 'D'.
01137              88  CF-AD-D-MORT-PLAN             VALUE 'A'.
01138          16  CF-PLAN-ABBREV                 PIC XXX.
01139          16  CF-PLAN-DESCRIPT               PIC X(10).
01140          16  CF-PLAN-NOTES                  PIC X(20).
01141          16  CF-PLAN-ESTABLISH-DATE         PIC XX.
01142          16  CF-PLAN-UNDERWRITING.
01143              20  CF-PLAN-TERM-DATA.
01144                  24  CF-MINIMUM-TERM        PIC S999      COMP-3.
01145                  24  CF-MAXIMUM-TERM        PIC S999      COMP-3.
01146              20  CF-PLAN-AGE-DATA.
01147                  24  CF-MINIMUM-AGE         PIC S999      COMP-3.
01148                  24  CF-MAXIMUM-AGE         PIC S999      COMP-3.
01149                  24  CF-MAXIMUM-ATTAIN-AGE  PIC S999      COMP-3.
01150              20  CF-PLAN-BENEFIT-DATA.
01151                  24  CF-MINIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
01152                  24  CF-MAXIMUM-BENEFIT     PIC S9(7)V99  COMP-3.
01153                  24  CF-MAXIMUM-MONTHLY-BENEFIT
01154                                             PIC S9(7)V99  COMP-3.
01155          16  CF-PLAN-POLICY-FORMS.
01156              20  CF-POLICY-FORM             PIC X(12).
01157              20  CF-MASTER-APPLICATION      PIC X(12).
01158              20  CF-MASTER-POLICY           PIC X(12).
01159          16  CF-PLAN-RATING.
01160              20  CF-RATE-CODE               PIC X(5).
01161              20  CF-SEX-RATING              PIC X.
01162                  88  CF-PLAN-NOT-SEX-RATED     VALUE '1'.
01163                  88  CF-PLAN-SEX-RATED         VALUE '2'.
01164              20  CF-SUB-STD-PCT             PIC S9V9999   COMP-3.
01165              20  CF-SUB-STD-TYPE            PIC X.
01166                  88  CF-PCT-OF-PREM            VALUE '1'.
01167                  88  CF-PCT-OF-BENE            VALUE '2'.
01168          16  CF-PLAN-PREM-TOLERANCES.
01169              20  CF-PREM-TOLERANCE          PIC S999      COMP-3.
01170              20  CF-PREM-TOLERANCE-PCT      PIC SV999     COMP-3.
01171          16  CF-PLAN-PYMT-TOLERANCES.
01172              20  CF-PYMT-TOLERANCE          PIC S999      COMP-3.
01173              20  CF-PYMT-TOLERANCE-PCT      PIC SV999     COMP-3.
01174          16  CF-PLAN-MISC-DATA.
01175              20  FILLER                     PIC X.
01176              20  CF-FREE-EXAM-DAYS          PIC S999      COMP-3.
01177              20  CF-RETRO-RETENTION         PIC S9V9999   COMP-3.
01178          16  CF-MORT-PLAN-USE-CTR           PIC S999      COMP-3.
01179          16  CF-PLAN-IND-GRP                PIC X.
01180              88  CF-MORT-INDIV-PLAN            VALUE 'I'
01181                                                      '1'.
01182              88  CF-MORT-GROUP-PLAN            VALUE 'G'
01183                                                      '2'.
01184          16  CF-MIB-SEARCH-SW               PIC X.
01185              88  CF-MIB-SEARCH-ALL             VALUE '1'.
01186              88  CF-MIB-SEARCH-NONE            VALUE '2'.
01187              88  CF-MIB-SEARCH-EXCEEDED        VALUE '3'.
01188              88  CF-MIB-SEARCH-VALID      VALUES ARE '1' '2' '3'.
01189          16  CF-ALPHA-SEARCH-SW             PIC X.
01190              88  CF-MIB-ALPHA-ALL              VALUE '1'.
01191              88  CF-MIB-ALPHA-NONE             VALUE '2'.
01192              88  CF-MIB-APLHA-EXCEEDED         VALUE '3'.
01193              88  CF-CLIENT-ALPHA-ALL           VALUE 'A'.
01194              88  CF-CLIENT-ALPHA-NONE          VALUE 'B'.
01195              88  CF-CLIENT-APLHA-EXCEEDED      VALUE 'C'.
01196              88  CF-BOTH-ALPHA-ALL             VALUE 'X'.
01197              88  CF-BOTH-ALPHA-NONE            VALUE 'Y'.
01198              88  CF-BOTH-APLHA-EXCEEDED        VALUE 'Z'.
01199              88  CF-ALPHA-SEARCH-VALID    VALUES ARE '1' '2' '3'
01200                                                      'A' 'B' 'C'
01201                                                      'X' 'Y' 'Z'.
01202          16  CF-EFF-DT-RULE-SW              PIC X.
01203              88  CF-EFF-DT-ENTER               VALUE 'E'.
01204              88  CF-EFF-DT-MONTH               VALUE 'M'.
01205              88  CF-EFF-DT-QTR                 VALUE 'Q'.
01206              88  CF-EFF-DT-SEMI                VALUE 'S'.
01207              88  CF-EFF-DT-ANN                 VALUE 'A'.
01208          16  FILLER                         PIC X(4).
01209          16  CF-HEALTH-QUESTIONS            PIC X.
01210              88  CF-VALID-QUESTIONS-CNT VALUES ARE '0' THRU '9'.
01211          16  CF-GRACE-PERIOD                PIC S999      COMP-3.
01212          16  CF-NUMBER-LAPSE-NOTICES        PIC S999      COMP-3.
01213          16  CF-PLAN-SNGL-JNT               PIC X.
01214              88  CF-COMBINED-PLAN              VALUE 'C'.
01215              88  CF-JNT-PLAN                   VALUE 'J'.
01216              88  CF-SNGL-PLAN                  VALUE 'S'.
01217          16  CF-DAYS-TO-1ST-NOTICE          PIC  99.
01218          16  CF-DAYS-TO-2ND-NOTICE          PIC  99.
01219          16  CF-DAYS-TO-3RD-NOTICE          PIC  99.
01220          16  CF-DAYS-TO-4TH-NOTICE          PIC  99.
01221          16  CF-RERATE-CNTL                 PIC  X.
01222              88  CF-RERATE-WITH-ISSUE-AGE       VALUE '1'.
01223              88  CF-RERATE-WITH-CURRENT-AGE     VALUE '2'.
01224              88  CF-DO-NOT-RERATE               VALUE '3' ' '.
01225              88  CF-AUTO-RECALC                 VALUE '4'.
01226          16  CF-BENEFIT-TYPE                PIC  X.
01227              88  CF-BENEFIT-IS-LEVEL            VALUE '1'.
01228              88  CF-BENEFIT-REDUCES             VALUE '2'.
01229          16  CF-POLICY-FEE                  PIC S999V99
01230                                                     COMP-3.
01231          16  CF-1ST-NOTICE-FORM             PIC  X(04).
01232          16  CF-2ND-NOTICE-FORM             PIC  X(04).
01233          16  CF-3RD-NOTICE-FORM             PIC  X(04).
01234          16  CF-4TH-NOTICE-FORM             PIC  X(04).
01235          16  FILLER                         PIC  X(32).
01236          16  CF-TERMINATION-FORM            PIC  X(04).
01237          16  FILLER                         PIC  X(08).
01238          16  CF-CLAIM-CAP                   PIC S9(7)V99
01239                                                        COMP-3.
01240          16  CF-REOCCURRING-DISABILITY-PRD  PIC S999   COMP-3.
01241          16  CF-ISSUE-LETTER                PIC  X(4).
01242          16  CF-YEARS-TO-NEXT-RERATE        PIC  99.
01243          16  CF-DEPENDENT-COVERAGE          PIC  X.
01244              88  CF-YES-DEP-COV                 VALUE 'Y'.
01245              88  CF-NO-DEP-COV             VALUES ARE 'N' ' '.
01246          16  CF-MP-REFUND-CALC              PIC X.
01247              88  CF-MP-REFUND-NOT-USED          VALUE SPACE.
01248              88  CF-MP-REFD-BY-R78              VALUE '1'.
01249              88  CF-MP-REFD-BY-PRO-RATA         VALUE '2'.
01250              88  CF-MP-REFD-AS-CALIF            VALUE '3'.
01251              88  CF-MP-REFD-AS-TEXAS            VALUE '4'.
01252              88  CF-MP-REFD-IS-NET-PAY          VALUE '5'.
01253              88  CF-MP-REFD-ANTICIPATION        VALUE '6'.
01254              88  CF-MP-REFD-MEAN                VALUE '8'.
01255          16  CF-ALT-RATE-CODE               PIC  X(5).
01256
01257
01258          16  FILLER                         PIC X(498).
01259 ****************************************************************
01260 *             MORTGAGE COMPANY MASTER RECORD                   *
01261 ****************************************************************
01262
01263      12  CF-MORTG-COMPANY-MASTER-REC  REDEFINES  CF-RECORD-BODY.
01264          16  CF-MORTG-ALT-MORT-CODE         PIC X(4).
01265          16  CF-MORTG-ACCESS-CONTROL        PIC X.
01266              88  CF-MORT-ST-PROD-CNTL                VALUE ' '.
01267              88  CF-MORT-CARR-GRP-ST-PROD-CNTL       VALUE '1'.
01268              88  CF-MORT-CARR-ST-PROD-CNTL           VALUE '2'.
01269              88  CF-MORT-PROD-CNTL                   VALUE '3'.
01270              88  CF-MORT-CARR-PROD-CNTL              VALUE '4'.
01271
01272          16  CF-MORTG-CONVERSION-DATE       PIC XX.
01273          16  CF-MORTG-RATE-FILE-MAINT-DATE  PIC XX.
01274          16  CF-MORTG-RATE-FILE-CREAT-DATE  PIC XX.
01275          16  CF-MORTG-PROD-FILE-MAINT-DATE  PIC XX.
01276          16  CF-MORTG-PROD-FILE-CREAT-DATE  PIC XX.
01277
01278          16  CF-MP-POLICY-LINKAGE-IND       PIC X(1).
01279              88  CF-MP-PLCY-LINKAGE-USED     VALUE 'Y'.
01280          16  CF-MP-RECON-USE-IND            PIC X(1).
01281              88  CF-MP-USE-RECON             VALUE 'Y'.
01282          16  CF-MORTG-CHECK-NO-COUNTER      PIC 9(6).
01283              88  CF-MP-CHECK-CNT-RESET-VALUE VALUE 999999.
01284          16  CF-MP-REPORT-LANGUAGE-IND      PIC X(1).
01285              88  CF-MP-LANGUAGE-IS-ENG       VALUE 'E'.
01286              88  CF-MP-LANGUAGE-IS-FR        VALUE 'F'.
01287          16  FILLER                         PIC X(1).
01288          16  CF-MORTG-CHECK-QUEUE-COUNTER   PIC 9(6).
01289              88  CF-MP-CHKQ-CNT-RESET-VALUE  VALUE 999999.
01290          16  CF-MORTG-MIB-VERSION           PIC X.
01291              88  CF-MORTG-MIB-BATCH         VALUE '1'.
01292              88  CF-MORTG-MIB-ONLINE        VALUE '2'.
01293              88  CF-MORTG-MIB-BOTH          VALUE '3'.
01294          16  CF-MORTG-ALT-MIB-SEARCH-CNTL.
01295              20  CF-MORTG-MIB-LNAME-SEARCH  PIC X.
01296                  88  CF-MIB-LAST-NAME-SEARCH     VALUE 'Y'.
01297              20  CF-MORTG-MIB-FNAME-SEARCH  PIC X.
01298                  88  CF-MIB-FIRST-NAME-SEARCH    VALUE 'Y'.
01299              20  CF-MORTG-MIB-MNAME-SEARCH  PIC X.
01300                  88  CF-MIB-MIDDLE-NAME-SEARCH   VALUE 'Y'.
01301              20  CF-MORTG-MIB-BDATE-SEARCH  PIC X.
01302                  88  CF-MIB-BIRTH-DATE-SEARCH    VALUE 'Y'.
01303              20  CF-MORTG-MIB-BSTATE-SEARCH PIC X.
01304                  88  CF-MIB-BIRTH-STATE-SEARCH   VALUE 'Y'.
01305              20  CF-MORTG-MIB-RSTATE-SEARCH PIC X.
01306                  88  CF-MIB-RESIDNT-STATE-SEARCH VALUE 'Y'.
01307          16  CF-MORTG-MIB-COMPANY-SYMBOL    PIC XXX.
01308          16  FILLER                         PIC X(7).
01309          16  CF-MORTG-DESTINATION-SYMBOL.
01310              20  CF-MORTG-MIB-COMM          PIC X(5).
01311              20  CF-MORTG-MIB-TERM          PIC X(5).
01312          16  CF-ASSIGN-POLICY-NO-SW         PIC X(01).
01313              88  CF-ASSIGN-POLICY-NO             VALUE 'Y'.
01314          16  FILLER                         PIC X(03).
01315          16  CF-MP-CHECK-NO-CONTROL.
01316              20  CF-MP-CHECK-NO-METHOD      PIC X(01).
01317                  88  CF-MP-CHECK-NO-MANUAL     VALUE '1'.
01318                  88  CF-MP-CHECK-NO-AUTO-SEQ   VALUE '2'
01319                                                 ' ' LOW-VALUES.
01320                  88  CF-MP-CHECK-NO-PRE-PRINTED
01321                                                VALUE '3'.
01322          16  CF-MORTG-LOAN-SHIFT-IND        PIC X(01).
01323          16  CF-MORTG-SOLICITATION-NUM      PIC S9(17) COMP-3.
01324          16  CF-MORTG-ALT-ALPHA-SEARCH-CNTL.
01325              20  CF-MORTG-ALP-LNAME-SEARCH  PIC X.
01326                  88  CF-ALPHA-LAST-NAME-SEARCH      VALUE 'Y'.
01327              20  CF-MORTG-ALP-FNAME-SEARCH  PIC X.
01328                  88  CF-ALPHA-FIRST-NAME-SEARCH     VALUE 'Y'.
01329              20  CF-MORTG-ALP-MNAME-SEARCH  PIC X.
01330                  88  CF-ALPHA-MIDDLE-NAME-SEARCH    VALUE 'Y'.
01331              20  CF-MORTG-ALP-BDATE-SEARCH  PIC X.
01332                  88  CF-ALPHA-BIRTH-DATE-SEARCH     VALUE 'Y'.
01333              20  CF-MORTG-ALP-BSTATE-SEARCH PIC X.
01334                  88  CF-ALPHA-BIRTH-STATE-SEARCH    VALUE 'Y'.
01335              20  CF-MORTG-ALP-RSTATE-SEARCH PIC X.
01336                  88  CF-ALPHA-RESIDNT-STATE-SEARCH  VALUE 'Y'.
01337          16  CF-MORTG-BILLING-AREA.
01338              20  CF-MORTG-BILL-CYCLE   OCCURS  5  TIMES
01339                                             PIC X.
01340          16  CF-MORTG-MONTH-END-DT          PIC XX.
01341          16  CF-MORTG-CURRENT-ARCH-NUM      PIC S9(8)  COMP.
01342          16  CF-MORTG-START-ARCH-NUM        PIC S9(8)  COMP.
01343          16  CF-MORTG-MIB-DEST-SW           PIC X.
01344              88 CF-MORTG-MIB-COMM-DEST              VALUE '1'.
01345              88 CF-MORTG-MIB-TERM-DEST              VALUE '2'.
01346          16  FILLER                         PIC X.
01347          16  CF-MORTG-LABEL-CONTROL         PIC X.
01348              88 CF-MORTG-CREATE-LABELS              VALUE 'Y'.
01349              88 CF-MORTG-BYPASS-LABELS              VALUE 'N'.
01350          16  CF-ACH-ORIGINATING-DFI-ID      PIC X(8).
01351          16  FILLER                         PIC X(8).
01352          16  CF-ACH-SENDING-DFI-NAME        PIC X(23).
01353          16  CF-ACH-RECVING-DFI-ROUTING-NO  PIC X(8).
01354          16  CF-ACH-RECVING-DFI-NAME        PIC X(23).
01355          16  CF-ACH-COMPANY-ID.
01356              20  CF-ACH-ID-CODE-DESIGNATOR  PIC X.
01357                  88  CF-ACH-ICD-IRS-EIN             VALUE '1'.
01358                  88  CF-ACH-ICD-DUNS                VALUE '3'.
01359                  88  CF-ACH-ICD-USER-ASSIGNED-NO    VALUE '9'.
01360              20  CF-ACH-COMPANY-ID-NO       PIC X(9).
01361          16  CF-MORTG-BILL-GROUPING-CODE    PIC X.
01362              88  CF-MORTG-CO-HAS-GROUPING           VALUE 'Y'.
01363          16  CF-RATE-DEV-AUTHORIZATION      PIC X.
01364              88  CF-RATE-DEV-AUTHORIZED             VALUE 'Y'.
01365              88  CF-RATE-DEV-NOT-AUTHORIZED         VALUE 'N'.
01366          16  CF-ACH-SENDING-DFI-ROUTING-NO  PIC X(9).
01367          16  CF-CBA-FILE-CREATE-NUM         PIC 9(4).
01368          16  FILLER                         PIC X(536).
01369
01370 ****************************************************************
01371 *             MORTGAGE HEIGHT - WEIGHT CHARTS                  *
01372 ****************************************************************
01373
01374      12  CF-FEMALE-HT-WT-REC  REDEFINES CF-RECORD-BODY.
01375          16  CF-FEMALE-HT-WT-INFO OCCURS 30 TIMES.
01376              20  CF-FEMALE-HEIGHT.
01377                  24  CF-FEMALE-FT           PIC 99.
01378                  24  CF-FEMALE-IN           PIC 99.
01379              20  CF-FEMALE-MIN-WT           PIC 999.
01380              20  CF-FEMALE-MAX-WT           PIC 999.
01381          16  FILLER                         PIC X(428).
01382
01383      12  CF-MALE-HT-WT-REC    REDEFINES CF-RECORD-BODY.
01384          16  CF-MALE-HT-WT-INFO   OCCURS 30 TIMES.
01385              20  CF-MALE-HEIGHT.
01386                  24  CF-MALE-FT             PIC 99.
01387                  24  CF-MALE-IN             PIC 99.
01388              20  CF-MALE-MIN-WT             PIC 999.
01389              20  CF-MALE-MAX-WT             PIC 999.
01390          16  FILLER                         PIC X(428).
01391 ******************************************************************
01392 *             AUTOMATIC ACTIVITY RECORD                          *
01393 ******************************************************************
01394      12  CF-AUTO-ACTIVITY-REC REDEFINES CF-RECORD-BODY.
01395          16  CF-SYSTEM-DEFINED-ACTIVITY OCCURS 09 TIMES.
01396              20  CF-SYS-ACTIVE-SW           PIC X(01).
01397              20  CF-SYS-LETTER-ID           PIC X(04).
01398              20  CF-SYS-RESEND-DAYS         PIC 9(03).
01399              20  CF-SYS-FOLLOW-UP-DAYS      PIC 9(03).
01400              20  CF-SYS-RESET-SW            PIC X(01).
01401              20  CF-SYS-REPORT-DAYS         PIC 9(03).
01402              20  CF-SYS-EACH-DAY-AFTER-SW   PIC X(01).
01403
01404          16  FILLER                         PIC X(50).
01405
01406          16  CF-USER-DEFINED-ACTIVITY  OCCURS 08 TIMES.
01407              20  CF-USER-ACTIVE-SW          PIC X(01).
01408              20  CF-USER-LETTER-ID          PIC X(04).
01409              20  CF-USER-RESEND-DAYS        PIC 9(03).
01410              20  CF-USER-FOLLOW-UP-DAYS     PIC 9(03).
01411              20  CF-USER-RESET-SW           PIC X(01).
01412              20  CF-USER-REPORT-DAYS        PIC 9(03).
01413              20  CF-USER-EACH-DAY-AFTER-SW  PIC X(01).
01414              20  CF-USER-ACTIVITY-DESC      PIC X(20).
01415
01416          16  FILLER                         PIC X(246).
      *                                COPY ELCCERT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCCERT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.013                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CERTIFICATE MASTER                        *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 450  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCERT                         RKP=2,LEN=33   *
00013 *       ALTERNATE PATH1 = ELCERT2 (BY NAME)       RKP=35,LEN=18  *
00014 *       ALTERNATE PATH2 = ELCERT3 (BY SOC SEC NO) RKP=53,LEN=12  *
00015 *       ALTERNATE PATH3 = ELCERT5 (BY CERT NO.)   RKP=65,LEN=12  *
00016 *       ALTERNATE PATH4 = ELCERT6 (BY MEMBER NO.) RKP=77,LEN=13  *
00017 *                                                                *
00018 *   LOG = YES                                                    *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
040504* 040504  CR2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
061405* 061405  CR2005060300001  PEMA  ADD CLP STATE PROCESS FOR DCC
110105* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
072308* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
102109* 102109  CR2008100900003  AJRA  ADD CLAIM CERT NOTE IND
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
032612* 032612  CR2011110200001  PEMA  AHL CHANGES
090314* 090314  CR2014081300001  PEMA  LOAD CERTS INVOLVED IN THAO
010716* 010716  CR2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
062017* 062017  CR2015091000001  PEMA RENAME INTEREST FIELD
122002******************************************************************
00021
00022  01  CERTIFICATE-MASTER.
00023      12  CM-RECORD-ID                      PIC XX.
00024          88  VALID-CM-ID                      VALUE 'CM'.
00025
00026      12  CM-CONTROL-PRIMARY.
00027          16  CM-COMPANY-CD                 PIC X.
00028          16  CM-CARRIER                    PIC X.
00029          16  CM-GROUPING.
00030              20  CM-GROUPING-PREFIX        PIC X(3).
00031              20  CM-GROUPING-PRIME         PIC X(3).
00032          16  CM-STATE                      PIC XX.
00033          16  CM-ACCOUNT.
00034              20  CM-ACCOUNT-PREFIX         PIC X(4).
00035              20  CM-ACCOUNT-PRIME          PIC X(6).
00036          16  CM-CERT-EFF-DT                PIC XX.
00037          16  CM-CERT-NO.
00038              20  CM-CERT-PRIME             PIC X(10).
00039              20  CM-CERT-SFX               PIC X.
00040
00041      12  CM-CONTROL-BY-NAME.
00042          16  CM-COMPANY-CD-A1              PIC X.
00043          16  CM-INSURED-LAST-NAME          PIC X(15).
00044          16  CM-INSURED-INITIALS.
00045              20  CM-INSURED-INITIAL1       PIC X.
00046              20  CM-INSURED-INITIAL2       PIC X.
00047
00048      12  CM-CONTROL-BY-SSN.
00049          16  CM-COMPANY-CD-A2              PIC X.
00050          16  CM-SOC-SEC-NO.
00051              20  CM-SSN-STATE              PIC XX.
00052              20  CM-SSN-ACCOUNT            PIC X(6).
00053              20  CM-SSN-LN3.
00054                  25  CM-INSURED-INITIALS-A2.
00055                      30 CM-INSURED-INITIAL1-A2   PIC X.
00056                      30 CM-INSURED-INITIAL2-A2   PIC X.
00057                  25 CM-PART-LAST-NAME-A2         PIC X.
00058
00059      12  CM-CONTROL-BY-CERT-NO.
00060          16  CM-COMPANY-CD-A4              PIC X.
00061          16  CM-CERT-NO-A4                 PIC X(11).
00062
00063      12  CM-CONTROL-BY-MEMB.
00064          16  CM-COMPANY-CD-A5              PIC X.
00065          16  CM-MEMBER-NO.
00066              20  CM-MEMB-STATE             PIC XX.
00067              20  CM-MEMB-ACCOUNT           PIC X(6).
00068              20  CM-MEMB-LN4.
00069                  25  CM-INSURED-INITIALS-A5.
00070                      30 CM-INSURED-INITIAL1-A5   PIC X.
00071                      30 CM-INSURED-INITIAL2-A5   PIC X.
00072                  25 CM-PART-LAST-NAME-A5         PIC XX.
00073
00074      12  CM-INSURED-PROFILE-DATA.
00075          16  CM-INSURED-FIRST-NAME.
00076              20  CM-INSURED-1ST-INIT       PIC X.
00077              20  FILLER                    PIC X(9).
00078          16  CM-INSURED-ISSUE-AGE          PIC 99.
00079          16  CM-INSURED-SEX                PIC X.
00080              88  CM-SEX-MALE                  VALUE 'M'.
00081              88  CM-SEX-FEMAL                 VALUE 'F'.
00082          16  CM-INSURED-JOINT-AGE          PIC 99.
00083          16  CM-JOINT-INSURED-NAME.
00084              20  CM-JT-LAST-NAME           PIC X(15).
00085              20  CM-JT-FIRST-NAME.
00086                  24  CM-JT-1ST-INIT        PIC X.
00087                  24  FILLER                PIC X(9).
00088              20  CM-JT-INITIAL             PIC X.
00089
00090      12  CM-LIFE-DATA.
00091          16  CM-LF-BENEFIT-CD              PIC XX.
00092          16  CM-LF-ORIG-TERM               PIC S999      COMP-3.
00093          16  CM-LF-CRITICAL-PERIOD         PIC S999      COMP-3.
00094          16  CM-LF-TERM-IN-DAYS            PIC S9(5)     COMP-3.
00095          16  CM-LF-DEV-CODE                PIC XXX.
00096          16  CM-LF-DEV-PCT                 PIC S9V9(6)   COMP-3.
00097          16  CM-LF-BENEFIT-AMT             PIC S9(9)V99  COMP-3.
00098          16  CM-LF-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
00099          16  CM-LF-ALT-BENEFIT-AMT         PIC S9(9)V99  COMP-3.
00100          16  CM-LF-ALT-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00101          16  CM-LF-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00102          16  CM-LF-REMAINING-AMT           PIC S9(9)V99  COMP-3.
00103          16  CM-LF-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
00104          16  CM-LF-ITD-DEATH-AMT           PIC S9(9)V99  COMP-3.
00105          16  CM-LF-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
00106          16  CM-LF-POLICY-FEE              PIC S9(3)V99  COMP-3.
00107          16  CM-LF-ALT-PREMIUM-RATE        PIC S99V9(5)  COMP-3.
090314         16  cm-temp-epiq                  pic xx.
090314             88  EPIQ-CLASS                  value 'EQ'.
090314*        16  FILLER                        PIC XX.
00109
00110      12  CM-AH-DATA.
00111          16  CM-AH-BENEFIT-CD              PIC XX.
00112          16  CM-AH-ORIG-TERM               PIC S999      COMP-3.
00113          16  CM-AH-CRITICAL-PERIOD         PIC S999      COMP-3.
00114          16  CM-AH-DEV-CODE                PIC XXX.
00115          16  CM-AH-DEV-PCT                 PIC S9V9(6)   COMP-3.
00116          16  CM-AH-BENEFIT-AMT             PIC S9(7)V99  COMP-3.
00117          16  CM-AH-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
00118          16  CM-AH-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00119          16  CM-AH-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
00120          16  CM-AH-ITD-LUMP-PMT            PIC S9(7)V99  COMP-3.
00121          16  CM-AH-ITD-AH-PMT              PIC S9(9)V99  COMP-3.
00122          16  CM-AH-PAID-THRU-DT            PIC XX.
00123              88  NO-AH-CLAIMS-PAID            VALUE LOW-VALUE.
00124          16  CM-AH-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
010716         16  CM-CANCEL-FEE                 PIC S9(3)V99  COMP-3.
00126          16  CM-AH-CEDED-BENEFIT           PIC S9(7)V99  COMP-3.
00127          16  FILLER                        PIC X.
00128
00129      12  CM-LOAN-INFORMATION.
00130          16  CM-LIVES                      PIC S9(7)     COMP-3.
011410         16  CM-DDF-IU-RATE-UP REDEFINES CM-LIVES
011410                                           PIC S9(5)V99  COMP-3.
00131          16  CM-BILLED                     PIC S9(7)     COMP-3.
00132          16  CM-LOAN-APR                   PIC S999V9(4) COMP-3.
00133          16  CM-PAY-FREQUENCY              PIC S99.
00134          16  CM-LOAN-TERM                  PIC S999      COMP-3.
00135          16  CM-RATE-CLASS                 PIC XX.
00136          16  CM-BENEFICIARY                PIC X(25).
00137          16  CM-POLICY-FORM-NO             PIC X(12).
00138          16  CM-PMT-EXTENSION-DAYS         PIC S999      COMP-3.
00139          16  CM-LAST-ADD-ON-DT             PIC XX.
00140          16  CM-DEDUCTIBLE-AMOUNTS.
00141              20  CM-CLAIM-DEDUCT-WITHHELD  PIC S9(5)V99  COMP-3.
00142              20  CM-CANCEL-DEDUCT-WITHHELD PIC S9(5)V99  COMP-3.
00143          16  CM-RESIDENT-RATE REDEFINES CM-DEDUCTIBLE-AMOUNTS.
00144              20  CM-RESIDENT-STATE         PIC XX.
00145              20  CM-RATE-CODE              PIC X(4).
00146              20  FILLER                    PIC XX.
110105         16  FILLER REDEFINES CM-DEDUCTIBLE-AMOUNTS.
110105             20  CM-LOAN-OFFICER           PIC X(5).
110105             20  FILLER                    PIC XXX.
00147          16  CM-CSR-CODE                   PIC XXX.
00148          16  CM-UNDERWRITING-CODE          PIC X.
00149              88  CM-POLICY-UNDERWRITTEN       VALUE 'Y'.
081606         16  CM-POST-CARD-IND              PIC X.
062017         16  CM-REF-INTERFACE-SW           PIC X.
00151          16  CM-PREMIUM-TYPE               PIC X.
00152              88  CM-SING-PRM                  VALUE '1'.
00153              88  CM-O-B-COVERAGE              VALUE '2'.
00154              88  CM-OPEN-END                  VALUE '3'.
00155          16  CM-IND-GRP-TYPE               PIC X.
00156              88  CM-INDIVIDUAL                VALUE 'I'.
00157              88  CM-GROUP                     VALUE 'G'.
00158          16  CM-SKIP-CODE                  PIC X.
00159              88  NO-MONTHS-SKIPPED            VALUE SPACE.
00160              88  SKIP-JULY                    VALUE '1'.
00161              88  SKIP-AUGUST                  VALUE '2'.
00162              88  SKIP-SEPTEMBER               VALUE '3'.
00163              88  SKIP-JULY-AUG                VALUE '4'.
00164              88  SKIP-AUG-SEPT                VALUE '5'.
00165              88  SKIP-JULY-AUG-SEPT           VALUE '6'.
00166              88  SKIP-JUNE-JULY-AUG           VALUE '7'.
00167              88  SKIP-JUNE                    VALUE '8'.
00168              88  SKIP-JUNE-JULY               VALUE '9'.
00169              88  SKIP-AUG-SEPT-OCT            VALUE 'A'.
00170              88  SKIP-BI-WEEKLY-3RD-PMT       VALUE 'X'.
00171          16  CM-PAYMENT-MODE               PIC X.
00172              88  PAY-MONTHLY                  VALUE SPACE.
00173              88  PAY-WEEKLY                   VALUE '1'.
00174              88  PAY-SEMI-MONTHLY             VALUE '2'.
00175              88  PAY-BI-WEEKLY                VALUE '3'.
00176              88  PAY-SEMI-ANUALLY             VALUE '4'.
00177          16  CM-LOAN-NUMBER                PIC X(8).
00178          16  CM-LOAN-BALANCE               PIC S9(7)V99  COMP-3.
110105         16  CM-OLD-LOF                    PIC XXX.
00179 *        16  CM-LOAN-OFFICER               PIC XXX.
00180          16  CM-REIN-TABLE                 PIC XXX.
00181          16  CM-SPECIAL-REIN-CODE          PIC X.
00182          16  CM-LF-LOAN-EXPIRE-DT          PIC XX.
00183          16  CM-AH-LOAN-EXPIRE-DT          PIC XX.
00184          16  CM-LOAN-1ST-PMT-DT            PIC XX.
00185
00186      12  CM-STATUS-DATA.
00187          16  CM-ENTRY-STATUS               PIC X.
00188          16  CM-ENTRY-DT                   PIC XX.
00189
00190          16  CM-LF-STATUS-AT-CANCEL        PIC X.
00191          16  CM-LF-CANCEL-DT               PIC XX.
00192          16  CM-LF-CANCEL-EXIT-DT          PIC XX.
00193
00194          16  CM-LF-STATUS-AT-DEATH         PIC X.
00195          16  CM-LF-DEATH-DT                PIC XX.
00196          16  CM-LF-DEATH-EXIT-DT           PIC XX.
00197
00198          16  CM-LF-CURRENT-STATUS          PIC X.
00199              88  CM-LF-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
00200                                                 'M' '4' '5' '9'.
00201              88  CM-LF-NORMAL-ENTRY           VALUE '1'.
00202              88  CM-LF-POLICY-PENDING         VALUE '2'.
00203              88  CM-LF-POLICY-IS-RESTORE      VALUE '3'.
00204              88  CM-LF-CONVERSION-ENTRY       VALUE '4'.
00205              88  CM-LF-POLICY-IS-REISSUE      VALUE '5'.
                   88  CM-LF-POLICY-IS-CASH         VALUE 'C'.
122002             88  CM-LF-POLICY-IS-MONTHLY      VALUE 'M'.
00206              88  CM-LF-LUMP-SUM-DISAB         VALUE '6'.
00207              88  CM-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
00208              88  CM-LF-CANCEL-APPLIED         VALUE '8'.
00209              88  CM-LF-IS-REIN-ONLY           VALUE '9'.
00210              88  CM-LF-DECLINED               VALUE 'D'.
00211              88  CM-LF-VOIDED                 VALUE 'V'.
00212
00213          16  CM-AH-STATUS-AT-CANCEL        PIC X.
00214          16  CM-AH-CANCEL-DT               PIC XX.
00215          16  CM-AH-CANCEL-EXIT-DT          PIC XX.
00216
00217          16  CM-AH-STATUS-AT-SETTLEMENT    PIC X.
00218          16  CM-AH-SETTLEMENT-DT           PIC XX.
00219          16  CM-AH-SETTLEMENT-EXIT-DT      PIC XX.
00220
00221          16  CM-AH-CURRENT-STATUS          PIC X.
00222              88  CM-AH-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
00223                                                 'M' '4' '5' '9'.
00224              88  CM-AH-NORMAL-ENTRY           VALUE '1'.
00225              88  CM-AH-POLICY-PENDING         VALUE '2'.
00226              88  CM-AH-POLICY-IS-RESTORE      VALUE '3'.
00227              88  CM-AH-CONVERSION-ENTRY       VALUE '4'.
00228              88  CM-AH-POLICY-IS-REISSUE      VALUE '5'.
                   88  CM-AH-POLICY-IS-CASH         VALUE 'C'.
122002             88  CM-AH-POLICY-IS-MONTHLY      VALUE 'M'.
00229              88  CM-AH-LUMP-SUM-DISAB         VALUE '6'.
00230              88  CM-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
00231              88  CM-AH-CANCEL-APPLIED         VALUE '8'.
00232              88  CM-AH-IS-REIN-ONLY           VALUE '9'.
00233              88  CM-AH-DECLINED               VALUE 'D'.
00234              88  CM-AH-VOIDED                 VALUE 'V'.
00235
00236          16  CM-CLAIM-INTERFACE-SW         PIC X.
00237              88  NO-CLAIM-ATTACHED            VALUE SPACE.
00238              88  CERT-AND-CLAIM-ONLINE        VALUE '1'.
00239              88  CERT-WAS-CREATED-FOR-CLAIM   VALUE '2'.
00240          16  CM-CLAIM-ATTACHED-COUNT       PIC S9(4)     COMP.
00241
00242          16  CM-ENTRY-BATCH                PIC X(6).
00243          16  CM-LF-EXIT-BATCH              PIC X(6).
00244          16  CM-AH-EXIT-BATCH              PIC X(6).
00245          16  CM-LAST-MONTH-END             PIC XX.
00246
00247      12  CM-NOTE-SW                        PIC X.
00248          88  CERT-NOTES-ARE-NOT-PRESENT       VALUE ' '.
00249          88  CERT-NOTES-PRESENT               VALUE '1'.
00250          88  BILLING-NOTES-PRESENT            VALUE '2'.
00251          88  CERT-BILLING-NOTES-PRESENT       VALUE '3'.
102109         88  CLAIM-NOTES-PRESENT              VALUE '4'.
102109         88  CLAIM-CERT-NOTES-PRESENT         VALUE '5'.
102109         88  CLAIM-BILLING-NOTES-PRESENT      VALUE '6'.
102109         88  CLAIM-CERT-BILL-NOTES-PRESENT    VALUE '7'.
00252      12  CM-COMP-EXCP-SW                   PIC X.
00253          88  COMPENSATION-SAME-AS-ACCT        VALUE ' '.
00254          88  THIS-CERT-HAS-ERCOMM-ENTRY       VALUE '1'.
00255      12  CM-INSURED-ADDRESS-SW             PIC X.
00256          88  INSURED-ADDR-NOT-PRESENT         VALUE ' '.
00257          88  INSURED-ADDR-PRESENT             VALUE '1'.
00258
011410*    12  CM-LF-CEDED-BENEFIT               PIC S9(7)V99   COMP-3.
011410     12  CM-LF-CLP                         PIC S9(5)V99   COMP-3.
011410     12  FILLER                            PIC X.
00260
011410*    12  CM-ISS-MICROFILM-NO               PIC S9(9)      COMP-3.
011410     12  CM-AH-CLP                         PIC S9(5)V99   COMP-3.
011410     12  FILLER                            PIC X.
072308*    12  CM-CAN-MICROFILM-NO               PIC S9(9)      COMP-3.
062017     12  CM-INT-ON-REFS                    PIC S9(7)V99   COMP-3.
00263
00264      12  CM-CREDIT-INTERFACE-SW-1          PIC X.
00265          88  CERT-ADDED-BATCH                 VALUE ' '.
00266          88  CERT-ADDED-ONLINE                VALUE '1'.
00267          88  CERT-PEND-ISSUE-ERROR            VALUE '2'.
00268          88  CERT-PURGED-OFFLINE              VALUE '3'.
00269          88  CERT-PEND-ISSUE-RETURNED         VALUE '4'.
00270      12  CM-CREDIT-INTERFACE-SW-2          PIC X.
00271          88  CERT-AS-LOADED                   VALUE ' '.
00272          88  CERT-CANCELLED-ONLINE            VALUE '1'.
00273          88  CERT-CLAIM-ONLINE                VALUE '2'.
00274          88  CERT-CLAIM-CANCEL-ONLINE         VALUE '3'.
00275          88  CERT-PEND-CANCEL-ERROR           VALUE '4'.
00276          88  CERT-PEND-CANCEL-VOID            VALUE '5'.
00277          88  CERT-PEND-CAN-VOID-ERROR         VALUE '6'.
00278          88  CERT-PEND-CANCEL-RETURNED        VALUE '7'.
00279
00280      12  CM-ACCOUNT-COMM-PCTS.
00281          16  CM-LIFE-COMM-PCT              PIC SV9(5)    COMP-3.
00282          16  CM-AH-COMM-PCT                PIC SV9(5)    COMP-3.
00283
00284      12  CM-USER-FIELD                     PIC X.
040504     12  CM-ADDL-CLP                       PIC S9(5)V99  COMP-3.
061405     12  CM-CLP-STATE                      PIC XX.
032612     12  CM-LF-CLASS-CD REDEFINES CM-CLP-STATE PIC XX.
061405     12  CM-USER-RESERVED                  PIC XXX.
032612     12  FILLER REDEFINES CM-USER-RESERVED.
032612         16  CM-AH-CLASS-CD                PIC XX.
032612         16  F                             PIC X.
00286 ******************************************************************
      *                                COPY ELCCRTT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCCRTT.                            *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CERTIFICATE TRAILERS                      *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 552  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCRTT                         RKP=2,LEN=34   *
00017 *                                                                *
00018 *   LOG = YES                                                    *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
111204******************************************************************
111204*                   C H A N G E   L O G
111204*
111204* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
111204*-----------------------------------------------------------------
111204*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
111204* EFFECTIVE    NUMBER
111204*-----------------------------------------------------------------
111204* 111204                   PEMA  NEW FILE TO SPLIT BANK COMM
040109* 040109  2009031600001    AJRA  ADD NEW TRAILER TYPE AND REDEFINE
012010* 012010  2009061500002    AJRA  ADD FLAG FOR REFUND WITH OPEN CLA
121712* 121712  CR2012101700002  AJRA  ADD DEFAULT AGE FLAG
061013* 061013  CR2012113000002  PEMA  SPP CLAIM RELATED CHANGES
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
022715* 022715  CR2015010800003  PEMA  AGENT SIGNATURE
020816* 020816  CR2015082500001  PEMA  ADD NEW VPP COMPANY
111204******************************************************************
00021
00022  01  CERTIFICATE-TRAILERS.
00023      12  CS-RECORD-ID                      PIC XX.
00024          88  VALID-CS-ID                      VALUE 'CS'.
00025
00026      12  CS-CONTROL-PRIMARY.
00027          16  CS-COMPANY-CD                 PIC X.
00028          16  CS-CARRIER                    PIC X.
00029          16  CS-GROUPING                   PIC X(6).
00032          16  CS-STATE                      PIC XX.
00033          16  CS-ACCOUNT                    PIC X(10).
00036          16  CS-CERT-EFF-DT                PIC XX.
00037          16  CS-CERT-NO.
00038              20  CS-CERT-PRIME             PIC X(10).
00039              20  CS-CERT-SFX               PIC X.
               16  CS-TRAILER-TYPE               PIC X.
                   88  COMM-TRLR           VALUE 'A'.
061013             88  CLAIM-HISTORY-TRLR  VALUE 'B'.
040109             88  CERT-DATA-TRLR      VALUE 'C'.
00040
040109     12  CS-DATA-AREA                      PIC X(516).
040109
040109     12  CS-BANK-COMMISSIONS REDEFINES CS-DATA-AREA.
040109         16  CS-BANK-COMMISSION-AREA.
040109             20  CS-BANK-COMMS       OCCURS 10.
040109                 24  CS-AGT                PIC X(10).
040109                 24  CS-COM-TYP            PIC X.
040109                 24  CS-SPP-FEES           PIC S9(5)V99   COMP-3.
040109                 24  CS-RECALC-LV-INDIC    PIC X.
040109                 24  FILLER                PIC X(10).
040109         16  FILLER                        PIC X(256).
040109
061013     12  CS-CLAIM-HISTORY-TRAILER REDEFINES CS-DATA-AREA.
061013****  TO CALC NO OF BENEFITS PAID = (CS-DAYS-PAID / 30)
               16  CS-MB-CLAIM-DATA OCCURS 24.
                   20  CS-CLAIM-NO               PIC X(7).
                   20  CS-CLAIM-TYPE             PIC X.
                       88  CS-AH-CLM               VALUE 'A'.
                       88  CS-IU-CLM               VALUE 'I'.
                       88  CS-GP-CLM               VALUE 'G'.
                       88  CS-LF-CLM               VALUE 'L'.
                       88  CS-PR-CLM               VALUE 'P'.
052614                 88  CS-FL-CLM               VALUE 'F'.
                   20  CS-INSURED-TYPE           PIC X.
                       88  CS-PRIM-INSURED          VALUE 'P'.
                       88  CS-CO-BORROWER           VALUE 'C'.
                   20  CS-BENEFIT-PERIOD         PIC 99.
                   20  CS-DAYS-PAID              PIC S9(5) COMP-3.
                   20  CS-TOTAL-PAID             PIC S9(7)V99 COMP-3.
                   20  CS-REMAINING-BENS         PIC S999 COMP-3.
               16  FILLER                        PIC X(12).
040109     12  CS-CERT-DATA REDEFINES CS-DATA-AREA.
040109         16  CS-VIN-NUMBER                 PIC X(17).
012010         16  CS-REFUND-CLAIM-FLAG          PIC X(01).
121712         16  CS-INS-AGE-DEFAULT-FLAG       PIC X(01).
121712         16  CS-JNT-AGE-DEFAULT-FLAG       PIC X(01).
022715         16  cs-agent-name.
022715             20  cs-agent-fname            pic x(20).
022715             20  cs-agent-mi               pic x.
022715             20  cs-agent-lname            pic x(25).
022715         16  cs-license-no                 pic x(15).
022715         16  cs-npn-number                 pic x(10).
022715         16  cs-agent-edit-status          pic x.
022715             88  cs-ae-refer-to-manager      value 'M'.
022715             88  cs-ae-cover-sheet           value 'C'.
022715             88  cs-ae-sig-form              value 'S'.
022715             88  cs-ae-verified              value 'V'.
022715             88  cs-unidentified-signature   value 'U'.
022715             88  cs-cert-returned            value 'R'.
022715             88  cs-accept-no-commission     value 'N'.
020816         16  cs-year                       pic 9999.
020816         16  cs-make                       pic x(20).
020816         16  cs-model                      pic x(20).
020816         16  cs-future                     pic x(20).
020816         16  cs-vehicle-odometer           pic s9(7) comp-3.
020816         16  FILLER                        PIC X(356). *> was 420
121712*        16  FILLER                        PIC X(496).
      *                                COPY ERCPNDM.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCPNDM                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = PENDING MAILING DATA                      *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
CIDMOD*   RECORD SIZE = 374   RECFORM = FIX                            *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ERPNDM                 RKP=2,LEN=11      *
00013 *   ALTERNATE PATH    = NOT USED                                 *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
080406******************************************************************
080406*                   C H A N G E   L O G
080406*
080406* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
080406*-----------------------------------------------------------------
080406*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
080406* EFFECTIVE    NUMBER
080406*-----------------------------------------------------------------
080406* 080406    2006051800002  PEMA  ADD POST CARD MAIL INFO
071108* 071108  CR2008040800002  PEMA  ADD CRED BENE INFORMATION
090408* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
100217* 100217  CR2016091600001  PEMA  ADD EDIT FOR ZIP CODE
00017 ******************************************************************
00018
00019  01  PENDING-MAILING-DATA.
00020      12  PM-RECORD-ID                      PIC XX.
00021          88  VALID-MA-ID                       VALUE 'PM'.
00022
00023      12  PM-CONTROL-PRIMARY.
00024          16  PM-COMPANY-CD                 PIC X.
00025          16  PM-ENTRY-BATCH                PIC X(6).
00026          16  PM-BATCH-SEQ-NO               PIC S9(4)     COMP.
00027          16  PM-BATCH-CHG-SEQ-NO           PIC S9(4)     COMP.
00028
00029      12  FILLER                            PIC X(14).
00030
00031      12  PM-ACCESS-CONTROL.
00032          16  PM-SOURCE-SYSTEM              PIC XX.
00033              88  PM-FROM-CREDIT                VALUE 'CR'.
00034              88  PM-FROM-VSI                   VALUE 'VS'.
00035              88  PM-FROM-WARRANTY              VALUE 'WA'.
00036              88  PM-FROM-OTHER                 VALUE 'OT'.
00037          16  PM-RECORD-ADD-DT              PIC XX.
00038          16  PM-RECORD-ADDED-BY            PIC XXXX.
00039          16  PM-LAST-MAINT-DT              PIC XX.
00040          16  PM-LAST-MAINT-BY              PIC XXXX.
00041          16  PM-LAST-MAINT-HHMMSS          PIC S9(6)       COMP-3.
00042
00043      12  PM-PROFILE-INFO.
00044          16  PM-QUALIFY-CODE-1             PIC XX.
00045          16  PM-QUALIFY-CODE-2             PIC XX.
00046          16  PM-QUALIFY-CODE-3             PIC XX.
00047          16  PM-QUALIFY-CODE-4             PIC XX.
00048          16  PM-QUALIFY-CODE-5             PIC XX.
00049
00050          16  PM-INSURED-LAST-NAME          PIC X(15).
00051          16  PM-INSURED-FIRST-NAME         PIC X(10).
00052          16  PM-INSURED-MIDDLE-INIT        PIC X.
00053          16  PM-INSURED-ISSUE-AGE          PIC 99.
00054          16  PM-INSURED-BIRTH-DT           PIC XX.
00055          16  PM-INSURED-SEX                PIC X.
00056              88  PM-SEX-MALE                   VALUE 'M'.
00057              88  PM-SEX-FEMALE                 VALUE 'F'.
00058          16  PM-INSURED-SOC-SEC-NO         PIC X(11).
00059
080406         16  PM-ADDRESS-CORRECTED          PIC X.
081108         16  PM-JOINT-BIRTH-DT             PIC XX.
00060 *        16  FILLER                        PIC X(12).
00061
00062          16  PM-ADDRESS-LINE-1             PIC X(30).
00063          16  PM-ADDRESS-LINE-2             PIC X(30).
00064          16  PM-CITY-STATE.
                   20  PM-CITY                   PIC X(28).
                   20  PM-STATE                  PIC XX.
00065          16  PM-ZIP.
00066              20  PM-ZIP-CODE.
00067                  24  PM-ZIP-1              PIC X.
00068                      88  PM-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00069                  24  FILLER                PIC X(4).
00070              20  PM-ZIP-PLUS4              PIC X(4).
00071          16  PM-CANADIAN-ZIP  REDEFINES  PM-ZIP.
00072              20  PM-CAN-POST1              PIC XXX.
00073              20  PM-CAN-POST2              PIC XXX.
00074              20  FILLER                    PIC XXX.
00075
00076          16  PM-PHONE-NO                   PIC 9(11)       COMP-3.
100217         16  pm-city-st-zip-verified       pic x.
100217         16  FILLER                        PIC XX.
00079
           12  PM-CRED-BENE-INFO.
CIDMOD         16  PM-CRED-BENE-NAME             PIC X(25).
CIDMOD         16  PM-CRED-BENE-ADDR             PIC X(30).
071108         16  PM-CRED-BENE-ADDR2            PIC X(30).
CIDMOD         16  PM-CRED-BENE-CTYST.
                   20  PM-CRED-BENE-CITY         PIC X(28).
                   20  PM-CRED-BENE-STATE        PIC XX.
CIDMOD         16  PM-CRED-BENE-ZIP.
CIDMOD             20  PM-CB-ZIP-CODE.
CIDMOD                 24  PM-CB-ZIP-1           PIC X.
CIDMOD                     88  PM-CB-CANADIAN-POST-CODE
                                        VALUE 'A' THRU 'Z'.
CIDMOD                 24  FILLER                PIC X(4).
CIDMOD             20  PM-CB-ZIP-PLUS4           PIC X(4).
CIDMOD         16  PM-CB-CANADIAN-ZIP  REDEFINES  PM-CRED-BENE-ZIP.
CIDMOD             20  PM-CB-CAN-POST1           PIC XXX.
CIDMOD             20  PM-CB-CAN-POST2           PIC XXX.
CIDMOD             20  FILLER                    PIC XXX.
080406     12  PM-POST-CARD-MAIL-DATA.
080406         16  PM-MAIL-DATA OCCURS 7.
080406             20  PM-MAIL-TYPE              PIC X.
080406                 88  PM-12MO-MAILING           VALUE '1'.
080406                 88  PM-EXP-MAILING            VALUE '2'.
080406             20  PM-MAIL-STATUS            PIC X.
080406                 88  PM-MAIL-ST-MAILED         VALUE '1'.
080406                 88  PM-MAIL-ST-RETURNED       VALUE '2'.
080406                 88  PM-MAIL-ST-NOT-MAILED     VALUE '3'.
080406             20  PM-MAIL-DATE              PIC XX.
080406     12  FILLER                            PIC XX.
           12  FILLER                            PIC X(12).
080406*    12  FILLER                            PIC X(30).
00075
00081 ******************************************************************
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA PENDING-BUSINESS
                                CONTROL-FILE CERTIFICATE-MASTER
                                CERTIFICATE-TRAILERS
                                PENDING-MAILING-DATA.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'VP6301' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
           MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
           MOVE PI-COMPANY-ID          TO ELCNTL-COMPANY-ID.
           MOVE +2                     TO EMI-NUMBER-OF-LINES.
           IF EIBCALEN = 0
               GO TO 8800-UNAUTHORIZED-ACCESS.
           MOVE EIBDATE                TO DC-JULIAN-YYDDD.
           MOVE '5'                    TO DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
           MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.
           MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DT.
           MOVE DC-GREG-DATE-1-MDY     TO WS-COMPARE-CURRENT-DT.
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
               ELSE
                   MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
                   MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
                   MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
                   MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
                   MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
                   MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
                   MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
                   MOVE SPACES               TO PI-SAVED-PROGRAM-6.
           MOVE LOW-VALUES             TO VP630BI.
           IF EIBTRNID NOT = TRANS-EXA6
               MOVE ZEROS              TO PI-LF-ISS-ENTERED
                                          PI-LF-CAN-ENTERED
                                          PI-AH-ISS-ENTERED
                                          PI-AH-CAN-ENTERED
                                          PI-ISS-CNT-ENTERED
                                          PI-CAN-CNT-ENTERED
               IF PI-MAINT-FUNC = 'N'
                  MOVE +0              TO PI-LAST-SEQ-NO-ADDED
                  MOVE +1              TO PI-NEXT-DISPLAY-SEQ-NO
                  IF PI-MAP-NAME = VP630B
                     PERFORM 8550-SET-MAP-SEQ-NOS
                     GO TO 8100-SEND-INITIAL-MAP
                  ELSE
                     PERFORM 8550-SET-MAP-SEQ-NOS
                             VARYING WS-SUB2 FROM 1 BY 1
                             UNTIL WS-SUB2   GREATER 4
                       GO TO 8100-SEND-INITIAL-MAP
               ELSE
                   GO TO 3000-CONTINUE-ENTRY.
           
      * EXEC CICS HANDLE CONDITION
      *        PGMIDERR  (9600-PGMID-ERROR)
      *        ERROR     (9990-ABEND)
      *    END-EXEC.
      *    MOVE '"$L.                  ! " #00005697' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303035363937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           IF EIBAID = DFHCLEAR
               MOVE SPACE TO PI-DISPLAY-SW
                             PI-BROWSE-SW
               GO TO 9400-CLEAR.
           EJECT
       0200-RECEIVE.
           IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
               MOVE ER-0008            TO EMI-ERROR
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
               IF PI-MAP-NAME = VP630B
                   MOVE -1             TO BPFENTRL
                   GO TO 8200-SEND-DATAONLY
               ELSE
                   MOVE -1             TO CPFENTRL
                   GO TO 8200-SEND-DATAONLY.
           
      * EXEC CICS RECEIVE
      *        MAP      (PI-MAP-NAME)
      *        MAPSET   (MAPSET-VP6301S)
      *        INTO     (VP630BI)
      *    END-EXEC.
           MOVE LENGTH OF
            VP630BI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00005716' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 VP630BI, 
                 DFHEIV11, 
                 MAPSET-VP6301S, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           INSPECT VP630BI CONVERTING '_' TO ' '.
           IF PI-MAP-NAME = VP630B
               IF BPFENTRL GREATER ZERO
                   IF EIBAID NOT = DFHENTER
                       MOVE ER-0004    TO EMI-ERROR
                       MOVE AL-UNBOF   TO BPFENTRA
                       MOVE -1         TO BPFENTRL
                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                       GO TO 8200-SEND-DATAONLY
                   ELSE
                       IF BPFENTRI NUMERIC AND
                          BPFENTRI GREATER 0 AND LESS 23
                           MOVE PF-VALUES (BPFENTRI) TO EIBAID
                       ELSE
                           MOVE ER-0029  TO EMI-ERROR
                           MOVE AL-UNBOF TO BPFENTRA
                           MOVE -1       TO BPFENTRL
                           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                           GO TO 8200-SEND-DATAONLY
               ELSE
                   NEXT SENTENCE
           ELSE
           IF PI-MAP-NAME = VP630C
               IF CPFENTRL GREATER ZERO
                   IF EIBAID NOT = DFHENTER
                       MOVE ER-0004    TO EMI-ERROR
                       MOVE AL-UNBOF   TO CPFENTRA
                       MOVE -1         TO CPFENTRL
                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                       GO TO 8200-SEND-DATAONLY
                   ELSE
                       IF CPFENTRI NUMERIC AND
                          CPFENTRI GREATER 0 AND LESS 23
                           MOVE PF-VALUES (CPFENTRI) TO EIBAID
                       ELSE
                           MOVE ER-0029  TO EMI-ERROR
                           MOVE AL-UNBOF TO BPFENTRA
                           MOVE -1       TO BPFENTRL
                           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                           GO TO 8200-SEND-DATAONLY.
           EJECT
      ******************************************************************
      *   PF KEY FUNCTIONS:                                            *
      *                                                                *
      *   PF1 = BROWSE FOWARD                                          *
      *   PF2 = BROWSE BACKWARD                                        *
      *   PF3 = ADD ISSUE RECORD                                       *
      *   PF4 = ADD CANCEL RECORD                                      *
      *   PF5 = RESET TABS (OPEN PROTECTED FIELDS)                     *
      *   PF6 = DELETE ENTRY                                           *
      ******************************************************************
       0300-CHECK-PFKEYS.
           IF EIBAID = DFHPF12
               GO TO 9500-PF12.
           IF EIBAID NOT = DFHPF5
              MOVE SPACE               TO PI-BROWSE-SW.
           IF EIBAID = DFHENTER
               GO TO 1000-EDIT-MAPB.
           IF EIBAID = DFHPF1
               MOVE 'Y'                TO PI-BROWSE-SW
               GO TO 2000-BROWSE-FWD.
           IF EIBAID = DFHPF2
               MOVE 'Y'                TO PI-BROWSE-SW
               GO TO 2100-BROWSE-BKWD.
           IF EIBAID = DFHPF3
               MOVE SPACE              TO PI-DISPLAY-SW
               MOVE LOW-VALUES         TO VP630BI
               ADD +1                     PI-LAST-SEQ-NO-ADDED
                     GIVING PI-NEXT-DISPLAY-SEQ-NO
               MOVE PI-NEXT-DISPLAY-SEQ-NO TO PI-SAV-BATCH-SEQ
               MOVE VP630B             TO PI-MAP-NAME
               PERFORM 8550-SET-MAP-SEQ-NOS
               GO TO 8100-SEND-INITIAL-MAP.
           IF EIBAID = DFHPF4
               MOVE SPACE              TO PI-DISPLAY-SW
               MOVE LOW-VALUES         TO MAP-C
               ADD +1                     PI-LAST-SEQ-NO-ADDED
                     GIVING PI-NEXT-DISPLAY-SEQ-NO
               MOVE PI-NEXT-DISPLAY-SEQ-NO TO PI-SAV-BATCH-SEQ
               MOVE VP630C             TO PI-MAP-NAME
               PERFORM 8550-SET-MAP-SEQ-NOS
                      VARYING WS-SUB2 FROM 1 BY 1
                      UNTIL WS-SUB2 GREATER +4
               GO TO 8100-SEND-INITIAL-MAP.
           IF EIBAID = DFHPF5
              IF PI-BROWSE
                 IF PI-MAP-NAME = VP630B
                    MOVE -1          TO BEFFDTL
                    GO TO 8200-SEND-DATAONLY
                 ELSE
                    MOVE -1          TO CCERT-LEN (1)
                    GO TO 8200-SEND-DATAONLY.
           IF EIBAID = DFHPF5
              IF PI-MAP-NAME = VP630B
                 PERFORM 0610-UNPROTECT-FIELDS THRU 0610-EXIT
                 MOVE -1             TO BEFFDTL
                 GO TO 8200-SEND-DATAONLY
              ELSE
                 PERFORM 0710-UNPROTECT-FIELDS THRU 0710-EXIT
                 ADD +1    PI-LAST-SEQ-NO-ADDED
                        GIVING PI-NEXT-DISPLAY-SEQ-NO
                MOVE -1             TO CCERT-LEN  (1)
                GO TO 8200-SEND-DATAONLY.
           IF EIBAID = DFHPF6
               IF PI-LAST-FUNC-DISPLAY
                   GO TO 6000-DELETE-PEND-BUS-RECORD
               ELSE
                   MOVE ER-2594        TO EMI-ERROR
                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                   IF PI-MAP-NAME = VP630B
                       MOVE -1         TO BPFENTRL
                       GO TO 8200-SEND-DATAONLY
                   ELSE
                       MOVE -1         TO CPFENTRL
                       GO TO 8200-SEND-DATAONLY.
           MOVE ER-0008 TO EMI-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           IF PI-MAP-NAME = VP630B
               MOVE -1                 TO BPFENTRL
           ELSE
               MOVE -1                 TO CPFENTRL.
           GO TO 8200-SEND-DATAONLY.
           EJECT
       0600-PROTECT-FIELDS.
           IF PI-COMPANY-ID = 'MON'
               MOVE AL-UANOF         TO BSFX-ATTRB
           ELSE
           IF PI-COMPANY-ID = 'PEM' OR
                              'CGL' OR
                              'TIH' OR
                              'TII' OR
                              'FGL' OR
                              'OFL'
              NEXT SENTENCE
           ELSE
              IF NOT PI-ISS-SUFFIX-KEYED
                 MOVE AL-SANOF         TO BSFX-ATTRB.
           IF PI-PROCESSOR-ID = 'LGXX'
              IF NOT PI-ISS-SUFFIX-KEYED
                 MOVE AL-SANOF         TO BSFX-ATTRB.
           IF PI-COMPANY-ID = 'DCC' or 'VPP' or 'CID'
              CONTINUE
           ELSE
              IF NOT PI-VIN-KEYED
                 MOVE AL-SANOF         TO BVIN-ATTRB
              END-IF
           END-IF
           IF PI-COMPANY-ID = 'CID' OR 'DCC' or 'AHL' or 'VPP'
              CONTINUE
           ELSE
              IF NOT PI-LN-OFFICER-KEYED
                 MOVE AL-SANOF         TO BLN-OFFICER-ATTRB
              END-IF
           END-IF
           IF PI-COMPANY-ID = 'CRI' OR 'LGX'
               IF PI-BIRTHDT-KEYED  AND  NOT PI-AGE-KEYED
                   MOVE AL-SANOF       TO BAGE-ATTRB.
           IF PI-COMPANY-ID = 'PEM' OR 'CGL' OR 'TIH' OR 'TII' OR
              'FGL' OR 'OFL'
              CONTINUE
           ELSE
              IF NOT PI-JNT-AGE-KEYED
                 MOVE AL-SANOF         TO BJNT-AGE-ATTRB
              END-IF
           END-IF
           IF PI-COMPANY-ID = 'PEM' OR
                              'CGL' OR
                              'TIH' OR
                              'TII' OR
                              'FGL' OR
                              'OFL'
              NEXT SENTENCE
           ELSE
              IF NOT PI-JNT-NAME-KEYED
                 MOVE AL-SANOF           TO BJNT-INIT-ATTRB
                                            BJNT-LST-NAME-ATTRB
                                            BJNT-1ST-NAME-ATTRB.
           IF PI-COMPANY-ID = 'TMS' OR 'CID' OR 'DCC' or 'AHL' or 'VPP'
              NEXT SENTENCE
           ELSE
              IF NOT PI-BENEFICIARY-KEYED
                  MOVE AL-SANOF           TO BBENEFICIARY-ATTRB.
           IF NOT PI-ALT-BEN-KEYED
               MOVE AL-SANOF           TO BALT-BEN1-ATTRB
           END-IF
           IF NOT PI-ALT-PREM-KEYED
               MOVE AL-SANOF           TO BALT-PREM1-ATTRB
           END-IF
           IF NOT PI-ALT-BEN-KEYED
               MOVE AL-SANOF           TO BALT-BEN2-ATTRB
           END-IF
           IF NOT PI-ALT-PREM-KEYED
               MOVE AL-SANOF           TO BALT-PREM2-ATTRB
           END-IF
           IF PI-PROCESSOR-ID = 'LGXX'
              CONTINUE
           ELSE
              IF PI-COMPANY-ID = 'PEM' OR
                                 'CGL' OR
                                 'TIH' OR
                                 'TII' OR
                                 'FGL' OR
                                 'OFL'
                 MOVE AL-SANOF            TO BADDRS2-ATTRB
      *                                      BPHONE-ATTRB
              ELSE
                 IF PI-COMPANY-ID ='CID' or 'AHL'
                    MOVE AL-SANOF         TO BADDRS2-ATTRB
                 END-IF
              END-IF
           END-IF
           IF PI-COMPANY-ID = 'TMS'
      *        MOVE AL-UANOF              TO BSIG-ATTRB
               MOVE AL-UANOF              TO BJNT-AGE-ATTRB
                                             BJNT-LST-NAME-ATTRB
                                             BJNT-1ST-NAME-ATTRB
                                             BJNT-INIT-ATTRB.
       0600-EXIT.
           EXIT.
           EJECT
       0610-UNPROTECT-FIELDS.
           IF PI-COMPANY-ID = 'PEM' OR
                              'CGL' OR
                              'TIH' OR
                              'TII' OR
                              'FGL' OR
                              'OFL'
              NEXT SENTENCE
           ELSE
              IF NOT PI-ISS-SUFFIX-KEYED
                 MOVE AL-UANOF         TO BSFX-ATTRB.
           IF PI-PROCESSOR-ID = 'LGXX'
              IF NOT PI-ISS-SUFFIX-KEYED
                 MOVE AL-UANOF         TO BSFX-ATTRB.
           IF PI-COMPANY-ID = 'CID' OR 'DCC' or 'AHL' or 'VPP'
              CONTINUE
           ELSE
              IF NOT PI-LN-OFFICER-KEYED
                 MOVE AL-UANOF         TO BLN-OFFICER-ATTRB
              END-IF
           END-IF
           IF PI-COMPANY-ID = 'FLA'
              IF BAGE-LEN NOT GREATER THAN +0
                 MOVE AL-UANOF       TO BAGE-ATTRB.
           IF PI-COMPANY-ID = 'CRI' OR
                              'LGX'
              IF NOT PI-AGE-KEYED
                 IF BAGE-LEN NOT GREATER THAN +0
                    MOVE AL-UANOF     TO BAGE-ATTRB.
           IF PI-COMPANY-ID = 'PEM' OR
                              'CGL' OR
                              'TIH' OR
                              'TII' OR
                              'FGL' OR
                              'OFL'
              NEXT SENTENCE
           ELSE
              IF NOT PI-JNT-AGE-KEYED
                 MOVE AL-UANOF           TO BJNT-AGE-ATTRB.
           IF PI-COMPANY-ID = 'PEM' OR
                              'CGL' OR
                              'TIH' OR
                              'TII' OR
                              'FGL' OR
                              'OFL'
              NEXT SENTENCE
           ELSE
              IF NOT PI-JNT-NAME-KEYED
                 MOVE AL-UANOF           TO BJNT-INIT-ATTRB
                                            BJNT-LST-NAME-ATTRB
                                            BJNT-1ST-NAME-ATTRB.
           IF PI-COMPANY-ID = 'TMS' OR 'CID' OR 'DCC' or 'AHL' or 'VPP'
              NEXT SENTENCE
           ELSE
              IF NOT PI-BENEFICIARY-KEYED
                  MOVE AL-UANOF           TO BBENEFICIARY-ATTRB.
           IF NOT PI-ALT-BEN-KEYED
               MOVE AL-UANOF           TO BALT-BEN1-ATTRB
           END-IF
           IF NOT PI-ALT-PREM-KEYED
               MOVE AL-UANOF           TO BALT-PREM1-ATTRB
           END-IF
           if pi-company-id = 'DCC' or 'VPP'
              IF NOT PI-ALT-BEN-KEYED
                 MOVE AL-UANOF         TO BALT-BEN2-ATTRB
              END-IF
              IF NOT PI-ALT-PREM-KEYED
                 MOVE AL-UANOF         TO BALT-PREM2-ATTRB
              END-IF
           end-if
           IF PI-PROCESSOR-ID = 'LGXX'
              CONTINUE
           ELSE
              IF PI-COMPANY-ID = 'PEM' OR
                                 'CGL' OR
                                 'TIH' OR
                                 'TII' OR
                                 'FGL' OR
                                 'OFL'
                 MOVE AL-UANOF            TO BADDRS2-ATTRB
      *                                      BPHONE-ATTRB
              ELSE
                 IF (PI-COMPANY-ID = 'CID' or 'AHL') AND
                    (PI-MAIL-YES)
                    MOVE AL-UANOF         TO BADDRS2-ATTRB
                 END-IF
              END-IF
           END-IF
           .
       0610-EXIT.
           EXIT.
           EJECT
       0700-PROTECT-FIELDS.
           IF PI-COMPANY-ID = 'PEM' OR
                              'CGL' OR
                              'TIH' OR
                              'TII' OR
                              'FGL' OR
                              'OFL'
              NEXT SENTENCE
           ELSE
              IF NOT PI-CAN-SUFFIX-KEYED
                 MOVE AL-SANOF         TO CSFX-ATTRB (1)
                                          CSFX-ATTRB (2)
                                          CSFX-ATTRB (3)
                                          CSFX-ATTRB (4).
           IF PI-PROCESSOR-ID = 'LGXX'
              IF NOT PI-CAN-SUFFIX-KEYED
                 MOVE AL-SANOF         TO CSFX-ATTRB (1)
                                          CSFX-ATTRB (2)
                                          CSFX-ATTRB (3)
                                          CSFX-ATTRB (4).
           IF PI-COMPANY-ID = 'CSO' OR 'CID' or 'AHL'
              MOVE AL-SANOF            TO CLAST-NAME-ATTRB (1)
                                          CLAST-NAME-ATTRB (2)
                                          CLAST-NAME-ATTRB (3)
                                          CLAST-NAME-ATTRB (4).
           IF NOT PI-CAN-LIVES-KEYED
              MOVE AL-SANOF            TO CLIVES-ATTRB (1)
                                          CLIVES-ATTRB (2)
                                          CLIVES-ATTRB (3)
                                          CLIVES-ATTRB (4).
           IF NOT PI-CAN-REA-KEYED
              MOVE AL-SANOF            TO CCANREA-ATTRB (1)
                                          CCANREA-ATTRB (2)
                                          CCANREA-ATTRB (3)
                                          CCANREA-ATTRB (4)
           END-IF
      *    IF PI-COMPANY-ID = 'HER'
      *        NEXT SENTENCE
      *    ELSE
      *        IF NOT PI-MICRO-NO-KEYED
      *            MOVE AL-SANOF       TO CMICRO-NO-ATTRB (1)
      *                                   CMICRO-NO-ATTRB (2)
      *                                   CMICRO-NO-ATTRB (3)
      *                                   CMICRO-NO-ATTRB (4).
           IF NOT PI-PAYEE-KEYED
               MOVE AL-SANOF           TO CPAYEE-ATTRB (1)
                                          CPAYEE-ATTRB (2)
                                          CPAYEE-ATTRB (3)
                                          CPAYEE-ATTRB (4).
           IF NOT PI-CHK-REQ-KEYED
               MOVE AL-SANOF           TO CCHK-ATTRB   (1)
                                          CCHK-ATTRB   (2)
                                          CCHK-ATTRB   (3)
                                          CCHK-ATTRB   (4).
           IF NOT PI-REFUND-MTHD-KEYED
               MOVE AL-SANOF           TO CMTHD1-ATTRB (1)
                                          CMTHD2-ATTRB (1)
                                          CMTHD1-ATTRB (2)
                                          CMTHD2-ATTRB (2)
                                          CMTHD1-ATTRB (3)
                                          CMTHD2-ATTRB (3)
                                          CMTHD1-ATTRB (4)
                                          CMTHD2-ATTRB (4).
       0700-EXIT.
           EXIT.
           EJECT
       0710-UNPROTECT-FIELDS.
           IF PI-COMPANY-ID = 'PEM' OR
                              'CGL' OR
                              'TIH' OR
                              'TII' OR
                              'FGL' OR
                              'OFL'
              NEXT SENTENCE
           ELSE
              IF NOT PI-CAN-SUFFIX-KEYED
               MOVE AL-UANOF           TO CSFX-ATTRB (1)
                                          CSFX-ATTRB (2)
                                          CSFX-ATTRB (3)
                                          CSFX-ATTRB (4).
           IF PI-PROCESSOR-ID = 'LGXX'
              IF NOT PI-CAN-SUFFIX-KEYED
                 MOVE AL-UANOF         TO CSFX-ATTRB (1)
                                          CSFX-ATTRB (2)
                                          CSFX-ATTRB (3)
                                          CSFX-ATTRB (4).
           IF PI-COMPANY-ID = 'CSO' OR 'CID' or 'AHL'
              MOVE AL-UANOF            TO CLAST-NAME-ATTRB (1)
                                          CLAST-NAME-ATTRB (2)
                                          CLAST-NAME-ATTRB (3)
                                          CLAST-NAME-ATTRB (4).
           IF NOT PI-CAN-LIVES-KEYED
               MOVE AL-UANOF           TO CLIVES-ATTRB (1)
                                          CLIVES-ATTRB (2)
                                          CLIVES-ATTRB (3)
                                          CLIVES-ATTRB (4).
           IF NOT PI-CAN-REA-KEYED
              MOVE AL-UANOF            TO CCANREA-ATTRB (1)
                                          CCANREA-ATTRB (2)
                                          CCANREA-ATTRB (3)
                                          CCANREA-ATTRB (4)
           END-IF
      *    IF PI-COMPANY-ID = 'HER'
      *        NEXT SENTENCE
      *    ELSE
      *        IF NOT PI-MICRO-NO-KEYED
      *            MOVE AL-UNNOF       TO CMICRO-NO-ATTRB (1)
      *                                   CMICRO-NO-ATTRB (2)
      *                                   CMICRO-NO-ATTRB (3)
      *                                   CMICRO-NO-ATTRB (4).
           IF NOT PI-PAYEE-KEYED
               MOVE AL-UANOF           TO CPAYEE-ATTRB (1)
                                          CPAYEE-ATTRB (2)
                                          CPAYEE-ATTRB (3)
                                          CPAYEE-ATTRB (4).
           IF NOT PI-CHK-REQ-KEYED
               MOVE AL-UANOF           TO CCHK-ATTRB   (1)
                                          CCHK-ATTRB   (2)
                                          CCHK-ATTRB   (3)
                                          CCHK-ATTRB   (4).
           IF NOT PI-REFUND-MTHD-KEYED
               MOVE AL-UANOF           TO CMTHD1-ATTRB (1)
                                          CMTHD2-ATTRB (1)
                                          CMTHD1-ATTRB (2)
                                          CMTHD2-ATTRB (2)
                                          CMTHD1-ATTRB (3)
                                          CMTHD2-ATTRB (3)
                                          CMTHD1-ATTRB (4)
                                          CMTHD2-ATTRB (4).
       0710-EXIT.
           EXIT.
           EJECT
       1000-EDIT-MAPB.
           IF PI-MAP-NAME NOT = VP630B
               GO TO 1100-EDIT-MAPC.
           IF PI-LAST-FUNC-DISPLAY
             AND BSFX-LEN           = ZEROS
             AND B1ST-NAME-LEN      = ZEROS
             AND BLAST-NAME-LEN     = ZEROS
             AND BINIT-LEN          = ZEROS
             AND BJNT-1ST-NAME-LEN  = ZEROS
             AND BJNT-INIT-LEN      = ZEROS
             AND BJNT-LST-NAME-LEN  = ZEROS
             AND BSEX-LEN           = ZEROS
             AND BAGE-LEN           = ZEROS
             AND BTERM1-LEN         = ZEROS
             AND BTERM2-LEN         = ZEROS
             AND BTYPE1-LEN         = ZEROS
             AND BTYPE2-LEN         = ZEROS
             AND BBENE1-LEN         = ZEROS
             AND BBENE2-LEN         = ZEROS
             AND BALT-BEN1-LEN      = ZEROS
             AND BPREM1-LEN         = ZEROS
             AND BPREM2-LEN         = ZEROS
             AND BALT-PREM1-LEN     = ZEROS
             AND BVIN-LEN           = ZEROS
             AND BJNT-AGE-LEN       = ZEROS
             AND BBENEFICIARY-LEN   = ZEROS
             AND BCADDR1-LEN        = ZEROS
             AND BCADDR2-LEN        = ZEROS
             AND BCCITY-LEN         = ZEROS
             AND BCSTATE-LEN        = ZEROS
             AND BCZIPCD-LEN        = ZEROS
             AND BLN-OFFICER-LEN    = ZEROS
             AND BADDRS1-LEN        = ZEROS
             AND BADDRS2-LEN        = ZEROS
             AND BCITY-LEN          = ZEROS
             AND BSTATE-LEN         = ZEROS
             AND BZIPCDE-LEN        = ZEROS
             AND BAGE-LEN           = ZEROS
             and byear-len          = zeros
             and bmake-len          = zeros
             and bmodel-len         = zeros
             and bometer-len        = zeros
               MOVE SPACE              TO PI-DISPLAY-SW
               GO TO 1030-NOTHING-TO-EDIT.
       1010-EDIT-MAPB.
           IF BCERT-LEN             = ZEROS
             AND BLAST-NAME-LEN     = ZEROS
             AND BEFFDT-LEN         = ZEROS
             AND NOT PI-LAST-FUNC-DISPLAY
               GO TO 1030-NOTHING-TO-EDIT.
           MOVE AL-SABON               TO BSEQ-ATTRB.
           IF BCERT-LEN  GREATER ZEROS
             AND PI-LAST-FUNC-DISPLAY
               NEXT SENTENCE
           ELSE
               IF BCERT-LEN  GREATER ZEROS
                   MOVE AL-UANON       TO BCERT-ATTRB
               ELSE
                   MOVE -1             TO BEFFDT-LEN
                   MOVE ER-2218        TO EMI-ERROR
                   MOVE AL-UABON       TO BCERT-ATTRB
                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           IF BSFX-LEN  NOT = ZEROS
               MOVE 'Y'                TO PI-ISS-SUFFIX-KEYED-SW
               MOVE AL-UANON           TO BSFX-ATTRB.
           IF BEFFDT-LEN  = ZEROS
             AND PI-LAST-FUNC-DISPLAY
               NEXT SENTENCE
           ELSE
               IF BEFFDT-LEN   GREATER ZEROS
                   MOVE AL-UNNON           TO BEFFDT-ATTRB
                   IF BEFFDT   NUMERIC
                       MOVE 4              TO DC-OPTION-CODE
                       MOVE BEFFDT    TO DC-GREG-DATE-1-MDY
                       PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
                       MOVE DC-BIN-DATE-1  TO WS-CONVERTED-EFFDT
                       IF NO-CONVERSION-ERROR
                           IF WS-CONVERTED-EFFDT NOT LESS
                             PI-ACCT-LOW-EFF-DT  AND LESS
                             PI-ACCT-HIGH-EXP-DT
                               PERFORM 1500-EDIT-ACCT-DT-RANGES THRU
                                       1590-EXIT
                           ELSE
                               MOVE 'Y' TO PI-FIN-RESP-ERROR-SW
      *                        MOVE -1       TO BEFFDT-LEN
      *                        MOVE ER-2589  TO EMI-ERROR
      *                        MOVE AL-UNBON TO BEFFDT-ATTRB
      *                        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                       ELSE
                           MOVE -1         TO BEFFDT-LEN
                           MOVE ER-2226    TO EMI-ERROR
                           MOVE AL-UNBON   TO BEFFDT-ATTRB
                           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                   ELSE
                       MOVE -1             TO BEFFDT-LEN
                       MOVE ER-2223        TO EMI-ERROR
                       MOVE AL-UNBON       TO BEFFDT-ATTRB
                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
               ELSE
                   MOVE -1                 TO BEFFDT-LEN
                   MOVE ER-2220            TO EMI-ERROR
                   MOVE AL-UNBON           TO BEFFDT-ATTRB
                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
      *    IF PI-COMPANY-ID = ('PEM' OR 'CRI')
      *      AND NOT PI-LAST-FUNC-DISPLAY
      *       IF BCERT      = BCERTV   AND
      *          BSFX       = BSFXV    AND
      *          BEFFDT     = BEFFDTV  AND
      *          BLAST-NAME = BLAST-NAMEV
      *          NEXT SENTENCE
      *       ELSE
      *          MOVE -1               TO BCERT-LEN
      *          MOVE -1               TO BLAST-NAME-LEN
      *          MOVE ER-3166          TO EMI-ERROR
      *          MOVE AL-UNBON         TO BCERT-ATTRB
      *                                   BCERTV-ATTRB
      *                                   BSFX-ATTRB
      *                                   BSFXV-ATTRB
      *                                   BEFFDT-ATTRB
      *                                   BEFFDTV-ATTRB
      *                                   BLAST-NAME-ATTRB
      *                                   BLAST-NAMEV-ATTRB
      *          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           IF BLAST-NAME-LEN   GREATER ZEROS
               MOVE AL-UANON           TO BLAST-NAME-ATTRB.
           IF B1ST-NAME-LEN    GREATER ZEROS
               MOVE AL-UANON           TO B1ST-NAME-ATTRB.
           IF BINIT-LEN        GREATER ZEROS
               MOVE AL-UANON           TO BINIT-ATTRB.
           IF BSEX-LEN         GREATER ZEROS
               IF BSEX   = 'M' OR 'F'
                   MOVE AL-UANON       TO BSEX-ATTRB
               ELSE
                   MOVE -1             TO BSEX-LEN
                   MOVE ER-2629        TO EMI-ERROR
                   MOVE AL-UABON       TO BSEX-ATTRB
                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           IF BAGE-LEN > 0
              MOVE 'Y'                 TO PI-AGE-KEYED-SW
              IF BAGE NUMERIC
                 MOVE BAGE             TO WS-BAGE
                 MOVE AL-UNNON         TO BAGE-ATTRB
              ELSE
                 MOVE -1             TO BAGE-LEN
                 MOVE ER-2223        TO EMI-ERROR
                 MOVE AL-UNBON       TO BAGE-ATTRB
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           MOVE +0                     TO WS-SUB1
           .
       1020-EDIT-COVERAGES.
           IF NOT MODIFY-CAP
                MOVE 'UPDATE'       TO SM-READ
                PERFORM 9995-SECURITY-VIOLATION
                MOVE ER-0070        TO EMI-ERROR
                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                GO TO 8100-SEND-INITIAL-MAP.
           ADD +1                      TO WS-SUB1.
           IF BTYPE1-LEN       > ZEROS OR
              BTERM1-LEN       > ZEROS OR
              BBENE1-LEN       > ZEROS OR
              BPREM1-LEN       > ZEROS OR
              BALT-PREM1-LEN   > ZEROS OR
              BALT-BEN1-LEN    > ZEROS
              MOVE 'Y'                 TO WS-DATA-KEYED-SW
           ELSE
              GO TO 1020-EDIT-BENEFIT-2
           END-IF
           MOVE +1                     TO WS-SUB1
           IF NOT PI-LAST-FUNC-DISPLAY
              IF BTYPE1-LEN  > ZEROS
                 MOVE AL-UANON         TO BTYPE1-ATTRB
                 PERFORM 1040-EDIT-INPUT-CODE
                                       THRU 1059-EXIT
              END-IF
           ELSE
              IF BTYPE1-LEN > ZEROS
                 IF BTYPE1 = SPACES OR ZEROS
                    MOVE AL-UANON      TO BTYPE1-ATTRB
                 ELSE
                    MOVE AL-UANON      TO BTYPE1-ATTRB
                    PERFORM 1040-EDIT-INPUT-CODE
                                       THRU 1059-EXIT
                 END-IF
              END-IF
           END-IF
           IF PI-LAST-FUNC-DISPLAY
              IF BTERM1-LEN  = ZEROS
                 CONTINUE
              ELSE
                 MOVE BTERM1I          TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT
                 IF DEEDIT-FIELD-V0 NUMERIC
                    MOVE DEEDIT-FIELD-V0
                                       TO WS-BTERM1
                    IF WS-BTERM1 > ZERO
                       MOVE AL-UNNON   TO BTERM1-ATTRB
                    ELSE
                       MOVE ER-2241    TO EMI-ERROR
                       MOVE -1         TO BTERM1-LEN
                       MOVE AL-UNBOF   TO BTERM1-ATTRB
                       PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                    END-IF
                 ELSE
                    MOVE ER-2223       TO EMI-ERROR
                    MOVE -1            TO BTERM1-LEN
                    MOVE AL-UNBON      TO BTERM1-ATTRB
                    PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 END-IF
              END-IF
           ELSE
              IF BTERM1-LEN > ZEROS
                 MOVE BTERM1I          TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT
                 IF DEEDIT-FIELD-V0      NUMERIC
                    IF DEEDIT-FIELD-V0 > ZERO
                       MOVE DEEDIT-FIELD-V0 TO WS-BTERM1
                       MOVE AL-UNNON        TO BTERM1-ATTRB
                    ELSE
                       MOVE ER-2241         TO EMI-ERROR
                       MOVE -1              TO BTERM1-LEN
                       MOVE AL-UNBOF        TO BTERM1-ATTRB
                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                    END-IF
                 ELSE
                    MOVE ER-2223             TO EMI-ERROR
                    MOVE -1                  TO BTERM1-LEN
                    MOVE AL-UNBON            TO BTERM1-ATTRB
                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 END-IF
              ELSE
                 IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
                    MOVE ER-2240             TO EMI-ERROR
                    MOVE -1                  TO BTERM1-LEN
                    MOVE AL-UNBOF            TO BTERM1-ATTRB
                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 END-IF
              END-IF
           END-IF
           IF BBENE1-LEN = ZEROS
              AND PI-LAST-FUNC-DISPLAY
              CONTINUE
           ELSE
              IF BBENE1-LEN > ZEROS
                 MOVE AL-UNNON           TO BBENE1-ATTRB
                 
      * EXEC CICS BIF DEEDIT
      *              FIELD  (BBENE1I)
      *              LENGTH (12)
      *          END-EXEC
           MOVE 12
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006411' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BBENE1I, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                 IF BBENE1I NUMERIC
                    IF BBENE1I > ZEROS
                       MOVE BBENE1I TO WS-BBEN1
      *          MOVE BBENE1I           TO DEEDIT-FIELD
      *          PERFORM 8600-DEEDIT
      *          IF DEEDIT-FIELD-V2  NUMERIC
      *             IF DEEDIT-FIELD-V2 > ZEROS
      *                MOVE DEEDIT-FIELD-V2 TO WS-BBEN1
                    ELSE
                       MOVE ER-7632    TO EMI-ERROR
                       MOVE -1         TO BBENE1-LEN
                       MOVE AL-UNBOF   TO BBENE1-ATTRB
                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                    END-IF
                 ELSE
                    MOVE ER-2223         TO EMI-ERROR
                    MOVE AL-UNBON        TO BBENE1-ATTRB
                    MOVE -1              TO BBENE1-LEN
                 END-IF
              ELSE
                 IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
                   MOVE ER-7632    TO EMI-ERROR
                   MOVE -1         TO BBENE1-LEN
                   MOVE AL-UNBOF   TO BBENE1-ATTRB
                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 END-IF
              END-IF
           END-IF
           IF BPREM1-LEN = ZEROS
              AND PI-LAST-FUNC-DISPLAY
              CONTINUE
           ELSE
              IF BPREM1-LEN > ZEROS
                 MOVE AL-UNNON           TO BPREM1-ATTRB
      *          MOVE BPREM1I            TO DEEDIT-FIELD
      *          PERFORM 8600-DEEDIT
      *          IF DEEDIT-FIELD-V2  NUMERIC
      *             IF DEEDIT-FIELD-V2 GREATER ZEROS
      *                MOVE DEEDIT-FIELD-V2 TO WS-BPREM1
                 
      * EXEC CICS BIF DEEDIT
      *              FIELD  (BPREM1I)
      *              LENGTH (11)
      *          END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006454' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BPREM1I, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                 IF BPREM1I NUMERIC
                    IF BPREM1I > ZEROS
                       MOVE BPREM1I TO WS-BPREM1
                    ELSE
                       MOVE ER-7633    TO EMI-ERROR
                       MOVE -1         TO BPREM1-LEN
                       MOVE AL-UNBOF   TO BPREM1-ATTRB
                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                    END-IF
                 ELSE
                    MOVE AL-UNBON   TO BPREM1-ATTRB
                    MOVE ER-2223         TO EMI-ERROR
                    MOVE -1              TO BPREM1-LEN
                 END-IF
              ELSE
                 IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
                    MOVE ER-7633    TO EMI-ERROR
                    MOVE -1         TO BPREM1-LEN
                    MOVE AL-UNBOF   TO BPREM1-ATTRB
                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 END-IF
              END-IF
           END-IF
           IF BALT-BEN1-LEN = ZEROS
              AND PI-LAST-FUNC-DISPLAY
              CONTINUE
           ELSE
              IF BALT-BEN1-LEN > ZEROS
                 MOVE 'Y'              TO PI-ALT-BEN-KEYED-SW
                 MOVE AL-UNNON         TO BALT-BEN1-ATTRB
      *          MOVE BALT-BEN1I       TO DEEDIT-FIELD
      *          PERFORM 8600-DEEDIT
      *          IF DEEDIT-FIELD-V2  NUMERIC
      *             MOVE DEEDIT-FIELD-V2 TO WS-BALT-BEN1
                 
      * EXEC CICS BIF DEEDIT
      *              FIELD  (BALT-BEN1I)
      *              LENGTH (12)
      *          END-EXEC
           MOVE 12
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006492' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BALT-BEN1I, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                 IF BALT-BEN1I NUMERIC
                    MOVE BALT-BEN1I TO WS-BALT-BEN1
                 ELSE
                    MOVE ER-2223       TO EMI-ERROR
                    MOVE AL-UNBON      TO BALT-BEN1-ATTRB
                    MOVE -1            TO BALT-BEN1-LEN
                 END-IF
              END-IF
           END-IF
           IF BALT-PREM1-LEN = ZEROS
              AND PI-LAST-FUNC-DISPLAY
              NEXT SENTENCE
           ELSE
              IF BALT-PREM1-LEN > ZEROS
                 MOVE 'Y'              TO PI-ALT-PREM-KEYED-SW
                 MOVE AL-UNNON         TO BALT-PREM1-ATTRB
      *          MOVE BALT-PREM1I      TO DEEDIT-FIELD
      *          PERFORM 8600-DEEDIT
      *          IF DEEDIT-FIELD-V2  NUMERIC
      *             MOVE DEEDIT-FIELD-V2 TO WS-BALT-PREM1
                 
      * EXEC CICS BIF DEEDIT
      *              FIELD  (BALT-PREM1I)
      *              LENGTH (9)
      *          END-EXEC
           MOVE 9
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006516' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BALT-PREM1I, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                 IF BALT-PREM1I NUMERIC
                    MOVE BALT-PREM1I TO WS-BALT-PREM1
                 ELSE
                    MOVE AL-UNBON      TO BALT-PREM1-ATTRB
                    MOVE ER-2223       TO EMI-ERROR
                    MOVE -1            TO BALT-PREM1-LEN
                 END-IF
              END-IF
           END-IF
           .
       1020-EDIT-BENEFIT-2.
           IF BTYPE2-LEN       > ZEROS OR
              BTERM2-LEN       > ZEROS OR
              BBENE2-LEN       > ZEROS OR
              BPREM2-LEN       > ZEROS OR
              BCRIT-PERD2-LEN  > ZEROS OR
              BALT-PREM2-LEN   > ZEROS OR
              BALT-BEN2-LEN    > ZEROS
              MOVE 'Y'                 TO WS-DATA-KEYED-SW
           ELSE
              GO TO 1025-CONT-EDIT
           END-IF
           MOVE +2                     TO WS-SUB1
           IF NOT PI-LAST-FUNC-DISPLAY
              IF BTYPE2-LEN  > ZEROS
                 MOVE AL-UANON         TO BTYPE2-ATTRB
                 PERFORM 1040-EDIT-INPUT-CODE
                                       THRU 1059-EXIT
              END-IF
           ELSE
              IF BTYPE2-LEN > ZEROS
                 IF BTYPE2 = SPACES OR ZEROS
                    MOVE AL-UANON      TO BTYPE2-ATTRB
                 ELSE
                    MOVE AL-UANON      TO BTYPE2-ATTRB
                    PERFORM 1040-EDIT-INPUT-CODE
                                       THRU 1059-EXIT
                 END-IF
              END-IF
           END-IF
           IF PI-LAST-FUNC-DISPLAY
              IF BTERM2-LEN  = ZEROS
                 CONTINUE
              ELSE
                 MOVE BTERM2I          TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT
                 IF DEEDIT-FIELD-V0 NUMERIC
                    MOVE DEEDIT-FIELD-V0
                                       TO WS-BTERM2
                    IF WS-BTERM2 > ZERO
                       MOVE AL-UNNON   TO BTERM2-ATTRB
                    ELSE
                       MOVE ER-2241    TO EMI-ERROR
                       MOVE -1         TO BTERM2-LEN
                       MOVE AL-UNBOF   TO BTERM2-ATTRB
                       PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                    END-IF
                 ELSE
                    MOVE ER-2223       TO EMI-ERROR
                    MOVE -1            TO BTERM2-LEN
                    MOVE AL-UNBON      TO BTERM2-ATTRB
                    PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 END-IF
              END-IF
           ELSE
              IF BTERM2-LEN > ZEROS
                 MOVE BTERM2I          TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT
                 IF DEEDIT-FIELD-V0      NUMERIC
                    IF DEEDIT-FIELD-V0 > ZERO
                       MOVE DEEDIT-FIELD-V0 TO WS-BTERM2
                       MOVE AL-UNNON        TO BTERM2-ATTRB
                    ELSE
                       MOVE ER-2241         TO EMI-ERROR
                       MOVE -1              TO BTERM2-LEN
                       MOVE AL-UNBOF        TO BTERM2-ATTRB
                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                    END-IF
                 ELSE
                    MOVE ER-2223             TO EMI-ERROR
                    MOVE -1                  TO BTERM2-LEN
                    MOVE AL-UNBON            TO BTERM2-ATTRB
                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 END-IF
              ELSE
                 IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
                    MOVE ER-2240             TO EMI-ERROR
                    MOVE -1                  TO BTERM2-LEN
                    MOVE AL-UNBOF            TO BTERM2-ATTRB
                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 END-IF
              END-IF
           END-IF
           IF BBENE2-LEN = ZEROS
              AND PI-LAST-FUNC-DISPLAY
              CONTINUE
           ELSE
              IF BBENE2-LEN > ZEROS
                 MOVE AL-UNNON           TO BBENE2-ATTRB
                 
      * EXEC CICS BIF DEEDIT
      *              FIELD  (BBENE2I)
      *              LENGTH (12)
      *          END-EXEC
           MOVE 12
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006621' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BBENE2I, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                 IF BBENE2I NUMERIC
                    IF BBENE2I > ZEROS
                       MOVE BBENE2I TO WS-BBEN2
      *          MOVE BBENE2I           TO DEEDIT-FIELD
      *          PERFORM 8600-DEEDIT
      *          IF DEEDIT-FIELD-V2  NUMERIC
      *             IF DEEDIT-FIELD-V2 > ZEROS
      *                MOVE DEEDIT-FIELD-V2 TO WS-BBEN2
                    ELSE
                       MOVE ER-7632    TO EMI-ERROR
                       MOVE -1         TO BBENE2-LEN
                       MOVE AL-UNBOF   TO BBENE2-ATTRB
                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                    END-IF
                 ELSE
                    MOVE ER-2223         TO EMI-ERROR
                    MOVE AL-UNBON        TO BBENE2-ATTRB
                    MOVE -1              TO BBENE2-LEN
                 END-IF
              ELSE
                 IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
                   MOVE ER-7632    TO EMI-ERROR
                   MOVE -1         TO BBENE2-LEN
                   MOVE AL-UNBOF   TO BBENE2-ATTRB
                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 END-IF
              END-IF
           END-IF
           IF BPREM2-LEN = ZEROS
              AND PI-LAST-FUNC-DISPLAY
              CONTINUE
           ELSE
              IF BPREM2-LEN > ZEROS
                 MOVE AL-UNNON           TO BPREM2-ATTRB
      *          MOVE BPREM2I            TO DEEDIT-FIELD
      *          PERFORM 8600-DEEDIT
      *          IF DEEDIT-FIELD-V2  NUMERIC
      *             IF DEEDIT-FIELD-V2 GREATER ZEROS
      *                MOVE DEEDIT-FIELD-V2 TO WS-BPREM2
                 
      * EXEC CICS BIF DEEDIT
      *              FIELD  (BPREM2I)
      *              LENGTH (11)
      *          END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006664' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BPREM2I, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                 IF BPREM2I NUMERIC
                    IF BPREM2I > ZEROS
                       MOVE BPREM2I TO WS-BPREM2
                    ELSE
                       MOVE ER-7633    TO EMI-ERROR
                       MOVE -1         TO BPREM2-LEN
                       MOVE AL-UNBOF   TO BPREM2-ATTRB
                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                    END-IF
                 ELSE
                    MOVE AL-UNBON   TO BPREM2-ATTRB
                    MOVE ER-2223         TO EMI-ERROR
                    MOVE -1              TO BPREM2-LEN
                 END-IF
              ELSE
                 IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
                    MOVE ER-7633    TO EMI-ERROR
                    MOVE -1         TO BPREM2-LEN
                    MOVE AL-UNBOF   TO BPREM2-ATTRB
                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 END-IF
              END-IF
           END-IF
      *    IF BEXPIRE-LEN (WS-SUB1)   GREATER ZEROS
      *       MOVE 'Y'                 TO PI-EXPIRE-KEYED-SW
      *        IF BEXPIRE (WS-SUB1)    NUMERIC
      *            MOVE AL-UNNON       TO BEXPIRE-ATTRB (WS-SUB1)
      *            MOVE 4              TO DC-OPTION-CODE
      *            MOVE BEXPIRE (WS-SUB1)   TO DC-GREG-DATE-1-MDY
      *            PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
      *            MOVE DC-BIN-DATE-1  TO WS-CONVERTED-EXPIRDT (WS-SUB1)
      *            IF NO-CONVERSION-ERROR
      *                NEXT SENTENCE
      *            ELSE
      *                MOVE -1         TO BEXPIRE-LEN   (WS-SUB1)
      *                MOVE ER-2531    TO EMI-ERROR
      *                MOVE AL-UNBON   TO BEXPIRE-ATTRB (WS-SUB1)
      *                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
      *        ELSE
      *            MOVE -1             TO BEXPIRE-LEN   (WS-SUB1)
      *            MOVE ER-2532        TO EMI-ERROR
      *            MOVE AL-UNBON       TO BEXPIRE-ATTRB (WS-SUB1)
      *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           IF BCRIT-PERD2-LEN > ZEROS
              MOVE 'Y'                     TO PI-CRIT-PERD-KEYED-SW
              MOVE BCRIT-PERD2I            TO DEEDIT-FIELD
              PERFORM 8600-DEEDIT
              IF DEEDIT-FIELD-V0 NUMERIC
                 MOVE DEEDIT-FIELD-V0      TO WS-BCRIT-PERD2
                 MOVE AL-UNNON             TO BCRIT-PERD2-ATTRB
              ELSE
                 MOVE -1                   TO BCRIT-PERD2-LEN
                 MOVE AL-UNBON             TO BCRIT-PERD2-ATTRB
                 MOVE ER-2223              TO EMI-ERROR
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              END-IF
           END-IF
           IF BALT-BEN2-LEN = ZEROS
              AND PI-LAST-FUNC-DISPLAY
              CONTINUE
           ELSE
              IF BALT-BEN2-LEN > ZEROS
                 MOVE 'Y'              TO PI-ALT-BEN-KEYED-SW
                 MOVE AL-UNNON         TO BALT-BEN2-ATTRB
      *          MOVE BALT-BEN2I       TO DEEDIT-FIELD
      *          PERFORM 8600-DEEDIT
      *          IF DEEDIT-FIELD-V2  NUMERIC
      *             MOVE DEEDIT-FIELD-V2 TO WS-BALT-BEN2
                 
      * EXEC CICS BIF DEEDIT
      *              FIELD  (BALT-BEN2I)
      *              LENGTH (12)
      *          END-EXEC
           MOVE 12
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006736' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BALT-BEN2I, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                 IF BALT-BEN2I NUMERIC
                    MOVE BALT-BEN2I TO WS-BALT-BEN2
                 ELSE
                    MOVE ER-2223       TO EMI-ERROR
                    MOVE AL-UNBON      TO BALT-BEN2-ATTRB
                    MOVE -1            TO BALT-BEN2-LEN
                 END-IF
              END-IF
           END-IF
           IF BALT-PREM2-LEN = ZEROS
              AND PI-LAST-FUNC-DISPLAY
              NEXT SENTENCE
           ELSE
              IF BALT-PREM2-LEN > ZEROS
                 MOVE 'Y'              TO PI-ALT-PREM-KEYED-SW
                 MOVE AL-UNNON         TO BALT-PREM2-ATTRB
      *          MOVE BALT-PREM2I      TO DEEDIT-FIELD
      *          PERFORM 8600-DEEDIT
      *          IF DEEDIT-FIELD-V2  NUMERIC
      *             MOVE DEEDIT-FIELD-V2 TO WS-BALT-PREM2
                 
      * EXEC CICS BIF DEEDIT
      *              FIELD  (BALT-PREM2I)
      *              LENGTH (9)
      *          END-EXEC
           MOVE 9
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006760' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BALT-PREM2I, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                 IF BALT-PREM2I NUMERIC
                    MOVE BALT-PREM2I TO WS-BALT-PREM2
                 ELSE
                    MOVE AL-UNBON      TO BALT-PREM2-ATTRB
                    MOVE ER-2223       TO EMI-ERROR
                    MOVE -1            TO BALT-PREM2-LEN
                 END-IF
              END-IF
           END-IF
      *    GO TO 1020-EDIT-COVERAGES.
           .
       1025-CONT-EDIT.
      *    IF BLIVES-LEN               GREATER ZEROS
      *       MOVE 'Y'                   TO PI-ISS-LIVES-KEYED-SW
      *       MOVE BLIVESI               TO DEEDIT-FIELD
      *       PERFORM 8600-DEEDIT
      *       IF DEEDIT-FIELD-V0 NUMERIC
      *          MOVE DEEDIT-FIELD-V0    TO WS-BLIVES
      *          MOVE AL-UNNON           TO BLIVES-ATTRB
      *       ELSE
      *          MOVE -1                 TO BLIVES-LEN
      *          MOVE AL-UNBON           TO BLIVES-ATTRB
      *          MOVE ER-2223            TO EMI-ERROR
      *          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           IF (BSTATE-LEN > 0)
              AND (BSTATE NOT = '  ' AND '00')
              MOVE AL-UANON            TO BSTATE-ATTRB
              MOVE SPACES              TO ELCNTL-KEY
              MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
              MOVE '3'                 TO ELCNTL-REC-TYPE
              MOVE BSTATE              TO ELCNTL-ACCESS
              MOVE +0                  TO ELCNTL-SEQ
              
      * EXEC CICS READ
      *          DATASET   (FILE-ID-ELCNTL)
      *          SET       (ADDRESS OF CONTROL-FILE)
      *          RIDFLD    (ELCNTL-KEY)
      *          RESP      (WS-RESPONSE)
      *       END-EXEC
      *    MOVE '&"S        E          (  N#00006796' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303036373936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ELCNTL, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF RESP-NORMAL
                 CONTINUE
              ELSE
                 MOVE ER-2963          TO EMI-ERROR
      *          MOVE -1               TO BSTATE-LEN
                 MOVE -1               TO BPFENTRL
                 MOVE AL-UABON         TO BSTATE-ATTRB
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 SUBTRACT +1 FROM EMI-FATAL-CTR
              END-IF
      *    ELSE
      *       MOVE ER-2209          TO EMI-ERROR
      *       MOVE -1               TO BSTATE-LEN
      *       MOVE AL-UABON         TO BSTATE-ATTRB
      *       PERFORM 9900-ERROR-FORMAT
      *                             THRU 9900-EXIT
      *       SUBTRACT +1 FROM EMI-FATAL-CTR
           END-IF
           IF BCSTATE-LEN > 0
              MOVE SPACES              TO ELCNTL-KEY
              MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
              MOVE '3'                 TO ELCNTL-REC-TYPE
              MOVE BCSTATE             TO ELCNTL-ACCESS
              MOVE +0                  TO ELCNTL-SEQ
              
      * EXEC CICS READ
      *          DATASET   (FILE-ID-ELCNTL)
      *          SET       (ADDRESS OF CONTROL-FILE)
      *          RIDFLD    (ELCNTL-KEY)
      *          RESP      (WS-RESPONSE)
      *       END-EXEC
      *    MOVE '&"S        E          (  N#00006827' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303036383237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ELCNTL, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF RESP-NORMAL
                 CONTINUE
              ELSE
                 MOVE ER-2964          TO EMI-ERROR
      *          MOVE -1               TO BCSTATE-LEN
                 MOVE -1               TO BPFENTRL
                 MOVE AL-UABON         TO BCSTATE-ATTRB
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
                 SUBTRACT +1 FROM EMI-FATAL-CTR
              END-IF
           END-IF
           IF BJNT-1ST-NAME-LEN     GREATER ZEROS OR
              BJNT-INIT-LEN         GREATER ZEROS OR
              BJNT-LST-NAME-LEN     GREATER ZEROS
               MOVE 'Y'                TO PI-JNT-NAME-KEYED-SW
               MOVE AL-UANON           TO BJNT-1ST-NAME-ATTRB
                                          BJNT-INIT-ATTRB
                                          BJNT-LST-NAME-ATTRB.
           IF BJNT-AGE-LEN GREATER ZEROS
              MOVE 'Y'                 TO PI-JNT-AGE-KEYED-SW
              IF BJNT-AGE NUMERIC
                 MOVE BJNT-AGE         TO WS-BJNT-AGE
                 MOVE AL-UNNON         TO BJNT-AGE-ATTRB
              ELSE
                 MOVE -1             TO BJNT-AGE-LEN
                 MOVE ER-2223        TO EMI-ERROR
                 MOVE AL-UNBON       TO BJNT-AGE-ATTRB
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
      *    IF BMICROFILM-NO-LEN  GREATER  ZEROS
      *        MOVE 'Y'                TO  PI-MICRO-NO-KEYED-SW
      *        MOVE BMICROFILM-NOI     TO  DEEDIT-FIELD
      *        PERFORM 8600-DEEDIT
      *        IF DEEDIT-FIELD-V0  NUMERIC
      *            MOVE DEEDIT-FIELD-V0
      *                                TO  WS-I-MICRO-NO
      *            MOVE AL-UNNON       TO  BMICROFILM-NO-ATTRB
      *        ELSE
      *            MOVE -1             TO  BMICROFILM-NO-LEN
      *            MOVE AL-UNBON       TO  BMICROFILM-NO-ATTRB
      *            MOVE ER-2701        TO  EMI-ERROR
      *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           IF BBENEFICIARY-LEN GREATER ZEROS
               MOVE 'Y'                TO PI-BENEFICIARY-KEYED-SW
               MOVE AL-UANON           TO BBENEFICIARY-ATTRB.
           IF BLN-OFFICER-LEN GREATER ZEROS
               MOVE 'Y'                TO PI-LN-OFFICER-KEYED-SW
               MOVE AL-UANON           TO BLN-OFFICER-ATTRB.
           .
       1027-CHECK-FUNCTION.
           .
       1028-CHECK-MEMNO.
      *    IF BMEM-NO-LEN GREATER ZEROS
      *        MOVE 'Y'                TO PI-MEMBER-KEYED-SW
      *        MOVE AL-UANON           TO BMEM-NO-ATTRB.
      *    IF  BPHONE-LEN GREATER ZEROS
      *        MOVE BPHONE             TO DEEDIT-FIELD
      *        PERFORM 8600-DEEDIT
      *        MOVE DEEDIT-FIELD-V0    TO WS-BPHONE
      *        MOVE AL-UANON           TO BPHONE-ATTRB.
      *    IF BENTRYI = 'D' OR 'V'
      *       GO TO 1029-CHECK-ERRORS.
           IF NOT PI-LAST-FUNC-DISPLAY
              AND WS-DATA-NOT-KEYED
              MOVE ER-7400             TO EMI-ERROR
              MOVE -1                  TO BTYPE1-LEN
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE 'Y'                 TO PI-ERROR-SW.
       1029-CHECK-ERRORS.
           IF EMI-ERROR = ZEROS
               MOVE 'Y'                TO PI-UPDATE-SW
               MOVE SPACE              TO PI-DISPLAY-SW
               PERFORM 4000-BUILD-ISSUE-RECORD THRU 4900-EXIT
           ELSE
              IF (EMI-FATAL-CTR = ZEROS)
                 AND (EMI-FORCABLE-CTR = ZEROS)
                 MOVE 'Y'              TO PI-UPDATE-SW
                 MOVE SPACE            TO PI-DISPLAY-SW
                 PERFORM 4000-BUILD-ISSUE-RECORD THRU 4900-EXIT
                 MOVE 'Y'              TO PI-ERROR-SW
              ELSE
                 MOVE ZEROS              TO EMI-ERROR
                 MOVE 'Y'                TO PI-ERROR-SW
              END-IF
           END-IF
           .
       1030-NOTHING-TO-EDIT.
           IF PI-DATA-ERRORS
               MOVE AL-SABON           TO BSEQ-ATTRB
               GO TO 8200-SEND-DATAONLY.
           IF PI-SAV-BATCH-SEQ LESS PI-LAST-SEQ-NO-ADDED
               SUBTRACT 1 FROM PI-SAV-BATCH-SEQ
               MOVE ER-0000        TO EMI-ERROR
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
               GO TO 2000-BROWSE-FWD.
           MOVE LOW-VALUES     TO VP630BI.
           ADD +1                 PI-LAST-SEQ-NO-ADDED
                                  GIVING PI-NEXT-DISPLAY-SEQ-NO.
           MOVE PI-NEXT-DISPLAY-SEQ-NO TO PI-SAV-BATCH-SEQ.
           PERFORM 8550-SET-MAP-SEQ-NOS.
           MOVE ER-0000        TO EMI-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           GO TO 8100-SEND-INITIAL-MAP.
           EJECT
       1040-EDIT-INPUT-CODE.
           IF WS-SUB1 = +2
              GO TO 1050-EDIT-INPUT-AH-CODE.
           MOVE SPACES                 TO ELCNTL-ACCESS.
           MOVE 'L'                    TO ELCNTL-REC-TYPE.
           PERFORM 1070-ELCNTL-READ THRU 1079-EXIT.
           IF EMI-ERROR = 9999
               GO TO 1048-NO-RECORD.
           MOVE +1 TO WS-EDIT-SUB.
       1041-SEARCH-LOOP.
           IF CF-LIFE-CODE-OUT (WS-EDIT-SUB) = ZEROS
               GO TO 1047-NO-MATCH-FOUND.
           IF BTYPE1  = CF-LIFE-CODE-IN (WS-EDIT-SUB)
               MOVE CF-LIFE-CODE-OUT (WS-EDIT-SUB) TO WS-EDITED-LF-CODE
               PERFORM 1060-BENEFIT-MASTER-READ THRU 1069-EXIT
               GO TO 1059-EXIT.
           ADD 1   TO WS-EDIT-SUB.
           IF WS-EDIT-SUB GREATER 120
               GO TO 1047-NO-MATCH-FOUND.
           GO TO 1041-SEARCH-LOOP.
       1047-NO-MATCH-FOUND.
      *    MOVE ER-2424                TO EMI-ERROR.
      *    MOVE AL-UABON               TO BTYPE1-ATTRB
      *    MOVE -1                     TO BTYPE1-LEN
      *    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
      *    MOVE 'Y'                    TO ERROR-SW.
           MOVE BTYPE1                 TO WS-EDITED-LF-CODE.
           PERFORM 1060-BENEFIT-MASTER-READ THRU 1069-EXIT.
           GO TO 1059-EXIT.
       1048-NO-RECORD.
      *    MOVE ER-2423                TO EMI-ERROR.
      *    MOVE AL-UABON               TO BTYPE1-ATTRB
      *    MOVE -1                     TO BTYPE1-LEN
      *    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
      *    MOVE 'Y'                    TO ERROR-SW.
           MOVE BTYPE1                 TO WS-EDITED-LF-CODE.
           PERFORM 1060-BENEFIT-MASTER-READ THRU 1069-EXIT.
           GO TO 1059-EXIT.
       1050-EDIT-INPUT-AH-CODE.
           MOVE SPACES                 TO ELCNTL-ACCESS.
           MOVE 'A'                    TO ELCNTL-REC-TYPE.
           PERFORM 1070-ELCNTL-READ THRU 1079-EXIT.
           IF EMI-ERROR = 9999
               GO TO 1058-NO-RECORD.
           MOVE +1 TO WS-EDIT-SUB.
       1051-SEARCH-LOOP.
           IF CF-AH-CODE-OUT (WS-EDIT-SUB) = ZEROS
               GO TO 1057-NO-MATCH-FOUND.
           IF BTYPE2    = CF-AH-CODE-IN (WS-EDIT-SUB)
               MOVE CF-AH-CODE-OUT (WS-EDIT-SUB) TO WS-EDITED-AH-CODE
               PERFORM 1060-BENEFIT-MASTER-READ THRU 1069-EXIT
               GO TO 1059-EXIT.
           ADD +1  TO WS-EDIT-SUB.
           IF WS-EDIT-SUB GREATER +96
               GO TO 1057-NO-MATCH-FOUND.
           GO TO 1051-SEARCH-LOOP.
       1057-NO-MATCH-FOUND.
      *    MOVE ER-2428                TO EMI-ERROR.
      *    MOVE AL-UABON               TO BTYPE2-ATTRB
      *    MOVE -1                     TO BTYPE2-LEN
      *    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
      *    MOVE 'Y'                    TO ERROR-SW.
           MOVE BTYPE2                 TO WS-EDITED-AH-CODE.
           PERFORM 1060-BENEFIT-MASTER-READ THRU 1069-EXIT.
           GO TO 1059-EXIT.
       1058-NO-RECORD.
      *    MOVE ER-2427                TO EMI-ERROR.
      *    MOVE AL-UABON               TO BTYPE2-ATTRB
      *    MOVE -1                     TO BTYPE2-LEN
      *    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
      *    MOVE 'Y'                    TO ERROR-SW.
           MOVE BTYPE2                 TO WS-EDITED-AH-CODE.
           PERFORM 1060-BENEFIT-MASTER-READ THRU 1069-EXIT.
       1059-EXIT.
           EXIT.
           EJECT
       1060-BENEFIT-MASTER-READ.
           MOVE SPACES                 TO ELCNTL-ACCESS.
           IF ELCNTL-REC-TYPE = 'L'
               MOVE WS-EDITED-LF-CODE  TO WS-BEN-CD
                                          ELCNTL-HI-BEN
               MOVE '4'                TO ELCNTL-REC-TYPE
           ELSE
               MOVE WS-EDITED-AH-CODE  TO WS-BEN-CD
                                          ELCNTL-HI-BEN
               MOVE '5'                TO ELCNTL-REC-TYPE.
           PERFORM 1070-ELCNTL-READ THRU 1079-EXIT.
           IF EMI-ERROR = 9999
               GO TO 1062-NO-RECORD.
           IF ELCNTL-COMPANY-ID NOT = CF-COMPANY-ID  OR
              ELCNTL-REC-TYPE   NOT = CF-RECORD-TYPE
                 GO TO 1062-NO-RECORD.
           perform varying ws-sub from +1 by +1 until
              (ws-sub > +8)
              or (cf-benefit-code (ws-sub) = ws-ben-cd)
           end-perform
           IF WS-SUB NOT = +9
               IF ELCNTL-REC-TYPE = '4'
                   MOVE CF-BENEFIT-ALPHA (WS-SUB) TO WS-LF-ABBR-DESC
                   move cf-co-earnings-calc (ws-sub)
                                       to ws-lf-earnings-calc
               ELSE
                   MOVE CF-BENEFIT-ALPHA (WS-SUB) TO WS-AH-ABBR-DESC
           ELSE
               GO TO 1063-NO-MATCH-FOUND.
           IF  CF-TERM-IN-DAYS (WS-SUB)
               MOVE 'Y'                TO WS-TERM-IN-DAYS-SW.
           GO TO 1069-EXIT.
       1062-NO-RECORD.
           MOVE ER-2426                TO EMI-ERROR.
           IF ELCNTL-REC-TYPE = '4'
               MOVE AL-UABON           TO BTYPE1-ATTRB
               MOVE -1                 TO BTYPE1-LEN
           ELSE
               MOVE AL-UABON           TO BTYPE2-ATTRB
               MOVE -1                 TO BTYPE2-LEN
           END-IF
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           MOVE 'Y'                    TO ERROR-SW.
           GO TO 1069-EXIT.
       1063-NO-MATCH-FOUND.
           IF ELCNTL-REC-TYPE = '4'
               MOVE ER-2425            TO EMI-ERROR
               MOVE AL-UABON           TO BTYPE1-ATTRB
               MOVE -1                 TO BTYPE1-LEN
           ELSE
               MOVE ER-2429            TO EMI-ERROR
               MOVE AL-UABON           TO BTYPE2-ATTRB
               MOVE -1                 TO BTYPE2-LEN
           END-IF
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           MOVE 'Y'                    TO ERROR-SW.
           GO TO 1069-EXIT.
       1069-EXIT.
           EXIT.
           EJECT
       1070-ELCNTL-READ.
           
      * EXEC CICS HANDLE CONDITION
      *        NOTFND  (1078-NO-RECORD)
      *        ENDFILE (1078-NO-RECORD)
      *    END-EXEC.
      *    MOVE '"$I''                  ! # #00007074' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303037303734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           IF ELCNTL-REC-TYPE = '1' OR '4' OR '5'
               
      * EXEC CICS READ
      *            DATASET (FILE-ID-ELCNTL)
      *            SET     (ADDRESS OF CONTROL-FILE)
      *            RIDFLD  (ELCNTL-KEY)
      *            GTEQ
      *        END-EXEC
      *    MOVE '&"S        G          (   #00007079' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ELCNTL, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           ELSE
               
      * EXEC CICS READ
      *            DATASET (FILE-ID-ELCNTL)
      *            SET     (ADDRESS OF CONTROL-FILE)
      *            RIDFLD  (ELCNTL-KEY)
      *        END-EXEC.
      *    MOVE '&"S        E          (   #00007086' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ELCNTL, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           GO TO 1079-EXIT.
       1078-NO-RECORD.
           MOVE ER-9999                TO EMI-ERROR.
       1079-EXIT.
           EXIT.
           EJECT
      *1080-TERM-CONVERSION.
      *    IF BMODE   = ' ' OR 'M'
      *        MOVE WS-BPMTS      TO WS-CALC-TERM-WHOLE
      *        GO TO 1085-ROUND-TERM.
      *    IF BMODE   = 'S'
      *        COMPUTE WS-CALC-TERM = WS-BPMTS / 2
      *        GO TO 1085-ROUND-TERM.
      *    IF BMODE   = 'W'
      *        COMPUTE WS-CALC-TERM = WS-BPMTS / 4.33333
      *        GO TO 1085-ROUND-TERM.
      *    IF BMODE   = 'B'
      *        COMPUTE WS-CALC-TERM = WS-BPMTS / 2.16667
      *        GO TO 1085-ROUND-TERM.
      *    IF BMODE   = 'T'
      *        COMPUTE WS-CALC-TERM = WS-BPMTS / 1.08334.
      *1085-ROUND-TERM.
      *    IF WS-CALC-TERM-REMAIN GREATER .00000
      *       ADD +1 TO WS-CALC-TERM.
      *    MOVE ZEROS                  TO WS-CALC-TERM-REMAIN.
      *    IF  BTYPE-LEN       (WS-SUB1)  GREATER ZEROS
      *        IF  WS-KIND-MONTHLY
      *            IF BTERM-LEN    (WS-SUB1)  = ZEROS
      *               IF BPREM-LEN (WS-SUB1)  GREATER ZEROS
      *                  IF  BPMT-LEN   GREATER ZEROS
      *                      NEXT SENTENCE
      *                  ELSE
      *                      GO TO 1087-EDIT-TERM.
      *    IF PI-COMPANY-ID IS EQUAL TO 'HAN' OR 'JHL'
      *        IF BBEN-LEN (WS-SUB1) IS GREATER THAN ZEROS
      *            GO TO 1087-EDIT-TERM.
      *        MOVE WS-BPMT            TO  WS-BBEN    (WS-SUB1).
      *    IF  BMODE   = 'W'
      *        COMPUTE WS-BBEN (WS-SUB1) ROUNDED = WS-BPMT * 4.33333.
      *    IF  BMODE   = 'S'
      *        COMPUTE WS-BBEN (WS-SUB1)  ROUNDED = WS-BPMT * 2.
      *    IF  BMODE   = 'B'
      *        COMPUTE WS-BBEN (WS-SUB1) ROUNDED = WS-BPMT * 2.16667.
      *    IF  BMODE   = 'T'
      *        COMPUTE WS-BBEN (WS-SUB1) ROUNDED = WS-BPMT * 1.08334.
      *    IF WS-SUB1 = +1
      *        COMPUTE WS-BBEN  (WS-SUB1) = WS-BPMT * WS-BPMTS.
      *    IF PI-COMPANY-ID = 'CRI'       OR  'LGX'
      *        IF WS-SUB1 = +1  AND
      *           BBEN-LEN (WS-SUB1) GREATER ZEROS
      *             GO TO 1087-EDIT-TERM.
      *    MOVE  WS-BBEN (WS-SUB1)     TO BBENO       (WS-SUB1).
      *    MOVE +12                    TO BBEN-LEN    (WS-SUB1).
      *    MOVE AL-UNNON               TO BBEN-ATTRB  (WS-SUB1).
      *1087-EDIT-TERM.
      *    IF BTERM-LEN (WS-SUB1)  = ZEROS
      *        MOVE WS-CALC-TERM-WHOLE   TO BTERMI      (WS-SUB1)
      *                                     WS-BTERM    (WS-SUB1)
      *        MOVE +3                   TO BTERM-LEN   (WS-SUB1)
      *        GO TO 1089-EXIT.
      *    MOVE BTERMI    (WS-SUB1)   TO DEEDIT-FIELD.
      *    PERFORM 8600-DEEDIT.
      *    IF DEEDIT-FIELD-V0 NUMERIC
      *       MOVE DEEDIT-FIELD-V0    TO WS-BTERM    (WS-SUB1)
      *       IF WS-BTERM (WS-SUB1)  GREATER ZERO
      *          MOVE AL-UNNON        TO BTERM-ATTRB (WS-SUB1)
      *       ELSE
      *          MOVE ER-2241         TO EMI-ERROR
      *          MOVE -1              TO BTERM-LEN   (WS-SUB1)
      *          MOVE AL-UNBOF        TO BTERM-ATTRB (WS-SUB1)
      *          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
      *          GO TO 1089-EXIT.
      *    IF WS-BTERM (WS-SUB1)   NOT = WS-CALC-TERM-WHOLE
      *        MOVE -1                   TO BTERM-LEN   (WS-SUB1)
      *        MOVE ER-2593              TO EMI-ERROR
      *        MOVE AL-UNBON             TO BTERM-ATTRB (WS-SUB1)
      *        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
      *1089-EXIT.
      *    EXIT.
      *1090-CALCULATE-MONTHLY-TERM.
      *    IF  BTYPE-LEN  (WS-SUB1)  GREATER ZEROS
      *        IF BTERM-LEN (WS-SUB1)  = ZEROS
      *           IF BPREM-LEN ( WS-SUB1) GREATER ZEROS
      *              IF  BPMT-LEN   GREATER ZEROS
      *                  NEXT SENTENCE
      *              ELSE
      *                  GO TO 1094-EXIT.
      *    COMPUTE  WS-CALC-TERM = WS-BDAYS / 31.
      *    IF WS-CALC-TERM-REMAIN GREATER .00000
      *       ADD +1 TO WS-CALC-TERM.
      *    MOVE ZEROS                  TO  WS-CALC-TERM-REMAIN.
      *    MOVE WS-CALC-TERM           TO  BTERMI    (WS-SUB1).
      *    MOVE +3                     TO  BTERM-LEN (WS-SUB1).
      *        MOVE WS-BPMT            TO  WS-BBEN    (WS-SUB1).
      *    IF  BMODE   = 'W'
      *        COMPUTE WS-BBEN (WS-SUB1) ROUNDED = WS-BPMT * 4.33333.
      *    IF  BMODE   = 'S'
      *        COMPUTE WS-BBEN (WS-SUB1)  ROUNDED = WS-BPMT * 2.
      *    IF  BMODE   = 'B'
      *        COMPUTE WS-BBEN (WS-SUB1) ROUNDED = WS-BPMT * 2.16667.
      *    IF  BMODE   = 'T'
      *        COMPUTE WS-BBEN (WS-SUB1) ROUNDED = WS-BPMT * 1.08334.
      *    IF WS-SUB1 = +1
      *       COMPUTE WS-BBEN  (WS-SUB1) =
      *               WS-BBEN (WS-SUB1) * WS-CALC-TERM.
      *    MOVE  WS-BBEN (WS-SUB1)     TO BBENO       (WS-SUB1).
      *    MOVE +12                    TO BBEN-LEN    (WS-SUB1).
      *    MOVE AL-UNNON               TO BBEN-ATTRB  (WS-SUB1).
      *1094-EXIT.
      *    EXIT.
       1095-CALC-AGE.
           MOVE WS-CONVERTED-BIRTH (1) TO DC-BIN-DATE-1
           MOVE WS-CURRENT-BIN-DT  TO DC-BIN-DATE-2.
           MOVE 1 TO DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
           IF NO-CONVERSION-ERROR
               COMPUTE WS-BAGE = DC-ELAPSED-MONTHS / 12
               MOVE WS-BAGE            TO BAGE
               MOVE +2                 TO BAGE-LEN
               MOVE AL-UNNON TO BAGE-ATTRB.
       1099-EXIT.
           EXIT.
           EJECT
       1100-EDIT-MAPC.
           MOVE +1                     TO WS-SUB2.
           IF PI-LAST-FUNC-DISPLAY
              AND CLAST-NAME-LEN (1)   = ZEROS
              AND CLAST-NAME-LEN (2)   = ZEROS
              AND CLAST-NAME-LEN (3)   = ZEROS
              AND CLAST-NAME-LEN (4)   = ZEROS
              AND CCANDT1-LEN     (1) = ZEROS
              AND CCANDT2-LEN     (1) = ZEROS
              AND CCANDT1-LEN     (2) = ZEROS
              AND CCANDT2-LEN     (2) = ZEROS
              AND CCANDT1-LEN     (3) = ZEROS
              AND CCANDT2-LEN     (3) = ZEROS
              AND CCANDT1-LEN     (4) = ZEROS
              AND CCANDT2-LEN     (4) = ZEROS
              AND CMTHD1-LEN      (1) = ZEROS
              AND CMTHD2-LEN      (1) = ZEROS
              AND CMTHD1-LEN      (2) = ZEROS
              AND CMTHD2-LEN      (2) = ZEROS
              AND CMTHD1-LEN      (3) = ZEROS
              AND CMTHD2-LEN      (3) = ZEROS
              AND CMTHD1-LEN      (4) = ZEROS
              AND CMTHD2-LEN      (4) = ZEROS
              AND CREFUND1-LEN    (1) = ZEROS
              AND CREFUND2-LEN    (1) = ZEROS
              AND CREFUND1-LEN    (2) = ZEROS
              AND CREFUND2-LEN    (2) = ZEROS
              AND CREFUND1-LEN    (3) = ZEROS
              AND CREFUND2-LEN    (3) = ZEROS
              AND CREFUND1-LEN    (4) = ZEROS
              AND CREFUND2-LEN    (4) = ZEROS
              AND CLIVES-LEN     (1) = ZEROS
              AND CLIVES-LEN     (2) = ZEROS
              AND CLIVES-LEN     (3) = ZEROS
              AND CLIVES-LEN     (4) = ZEROS
              AND CCANREA-LEN    (1) = ZEROS
              AND CCANREA-LEN    (2) = ZEROS
              AND CCANREA-LEN    (3) = ZEROS
              AND CCANREA-LEN    (4) = ZEROS
              AND CPAYEE-LEN     (1) = ZEROS
              AND CPAYEE-LEN     (2) = ZEROS
              AND CPAYEE-LEN     (3) = ZEROS
              AND CPAYEE-LEN     (4) = ZEROS
              AND CCHK-LEN       (1) = ZEROS
              AND CCHK-LEN       (2) = ZEROS
              AND CCHK-LEN       (3) = ZEROS
              AND CCHK-LEN       (4) = ZEROS
               GO TO 1130-NOTHING-TO-EDIT.
       1110-EDIT-MAPC-LOOP.
           IF CCERT-LEN       (WS-SUB2) = ZEROS
             AND CEFFDT-LEN   (WS-SUB2) = ZEROS
             AND CCANDT1-LEN  (WS-SUB2) = ZEROS
             AND CCANDT2-LEN  (WS-SUB2) = ZEROS
             AND CMTHD1-LEN   (WS-SUB2) = ZEROS
             AND CMTHD2-LEN   (WS-SUB2) = ZEROS
             AND CREFUND1-LEN (WS-SUB2) = ZEROS
             AND CREFUND2-LEN (WS-SUB2) = ZEROS
             AND NOT PI-LAST-FUNC-DISPLAY
               GO TO 1120-INCREMENT-OCCURRENCE.
           MOVE 'Y'                    TO WS-DATA-KEYED-SW.
           MOVE AL-SABON               TO CSEQ-ATTRB (WS-SUB2).
           IF CCERT-LEN (WS-SUB2) = ZEROS
             AND PI-LAST-FUNC-DISPLAY
               NEXT SENTENCE
           ELSE
               IF CCERT-LEN (WS-SUB2)  NOT = ZEROS
                   MOVE AL-UANON       TO CCERT-ATTRB (WS-SUB2)
               ELSE
                   MOVE -1             TO CCERT-LEN   (WS-SUB2)
                   MOVE ER-2218        TO EMI-ERROR
                   MOVE AL-UABON       TO CCERT-ATTRB (WS-SUB2)
                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           IF CSFX-LEN (WS-SUB2)      GREATER ZEROS
               MOVE 'Y'                TO PI-CAN-SUFFIX-KEYED-SW
               MOVE AL-UANON           TO CSFX-ATTRB (WS-SUB2).
           IF CEFFDT-LEN (WS-SUB2) = ZEROS
             AND PI-LAST-FUNC-DISPLAY
               NEXT SENTENCE
           ELSE
               IF CEFFDT-LEN (WS-SUB2) GREATER ZEROS
                   MOVE AL-UNNON           TO CEFFDT-ATTRB (WS-SUB2)
                   IF CEFFDT (WS-SUB2) NUMERIC
                       MOVE 4              TO DC-OPTION-CODE
                       MOVE CEFFDT (WS-SUB2)  TO DC-GREG-DATE-1-MDY
                       PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
                       MOVE DC-BIN-DATE-1  TO
                                   WS-CONVERTED-CAN-EFF-DT (WS-SUB2)
                                   WS-CONVERTED-EFFDT
                       IF NO-CONVERSION-ERROR
                           IF WS-CONVERTED-CAN-EFF-DT (WS-SUB2)
                             NOT LESS PI-ACCT-LOW-EFF-DT
                             AND LESS PI-ACCT-HIGH-EXP-DT
                               PERFORM 1500-EDIT-ACCT-DT-RANGES THRU
                                       1590-EXIT
                           ELSE
                               NEXT SENTENCE
      *                        MOVE -1       TO CEFFDT-LEN (WS-SUB2)
      *                        MOVE ER-2589  TO EMI-ERROR
      *                        MOVE AL-UNBON TO CEFFDT-ATTRB (WS-SUB2)
      *                        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                       ELSE
                           MOVE -1         TO CEFFDT-LEN (WS-SUB2)
                           MOVE ER-2226    TO EMI-ERROR
                           MOVE AL-UNBON   TO CEFFDT-ATTRB (WS-SUB2)
                           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                   ELSE
                       MOVE -1             TO CEFFDT-LEN (WS-SUB2)
                       MOVE ER-2223        TO EMI-ERROR
                       MOVE AL-UNBON       TO CEFFDT-ATTRB (WS-SUB2)
                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
               ELSE
                   MOVE -1                 TO CEFFDT-LEN (WS-SUB2)
                   MOVE ER-2220            TO EMI-ERROR
                   MOVE AL-UNBON           TO CEFFDT-ATTRB (WS-SUB2)
                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           IF CLAST-NAME-LEN (WS-SUB2) GREATER ZEROS
               MOVE AL-UANON           TO CLAST-NAME-ATTRB (WS-SUB2).
           EJECT
       1115-EDIT-COVERAGES.
           IF CCANDT1-LEN (WS-SUB2) = ZEROS
             AND PI-LAST-FUNC-DISPLAY
               NEXT SENTENCE
           ELSE
               IF CCANDT1-LEN (WS-SUB2) GREATER ZEROS
                   MOVE AL-UNNON       TO CCANDT1-ATTRB (WS-SUB2)
                   IF PI-LAST-FUNC-DISPLAY AND
                      CCANDT1 (WS-SUB2) = SPACES
                       MOVE LOW-VALUES TO WS-CONVERTED-CANDT1 (WS-SUB2)
                   ELSE
                      IF CCANDT1 (WS-SUB2) NUMERIC
                         MOVE 4              TO DC-OPTION-CODE
                         MOVE CCANDT1 (WS-SUB2) TO
                                            DC-GREG-DATE-1-MDY
                         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
                         MOVE DC-BIN-DATE-1  TO WS-CONVERTED-CANDT1
                                                               (WS-SUB2)
                         IF NO-CONVERSION-ERROR
                            NEXT SENTENCE
                         ELSE
                            MOVE -1       TO CCANDT1-LEN   (WS-SUB2)
                            MOVE ER-2227  TO EMI-ERROR
                            MOVE AL-UNBON TO CCANDT1-ATTRB (WS-SUB2)
                            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                      ELSE
                         MOVE -1         TO CCANDT1-LEN   (WS-SUB2)
                         MOVE ER-2223    TO EMI-ERROR
                         MOVE AL-UNBON   TO CCANDT1-ATTRB (WS-SUB2)
                         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
               ELSE
                   IF CREFUND1-LEN (WS-SUB2) GREATER ZEROS
                      MOVE -1             TO CCANDT1-LEN   (WS-SUB2)
                      MOVE ER-2222        TO EMI-ERROR
                      MOVE AL-UNBOF       TO CCANDT1-ATTRB (WS-SUB2)
                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           IF CREFUND1-LEN (WS-SUB2) = ZEROS
             AND PI-LAST-FUNC-DISPLAY
               NEXT SENTENCE
           ELSE
               IF CREFUND1-LEN (WS-SUB2) NOT = ZEROS
                  MOVE AL-UNNON       TO CREFUND1-ATTRB (WS-SUB2)
      *           MOVE CREFUND1I (WS-SUB2) TO DEEDIT-FIELD-X11
      *           PERFORM 8600-DEEDIT
      *           MOVE DEEDIT-FIELD-V2  TO WS-CREFUND1 (WS-SUB2).
                  
      * EXEC CICS BIF DEEDIT
      *               FIELD  (CREFUND1I (WS-SUB2))
      *               LENGTH (11)
      *           END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00007379' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CREFUND1I(WS-SUB2), 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                  MOVE CREFUND1I (WS-SUB2) TO WS-CREFUND1 (WS-SUB2).
      ******************************************************************
      *********** REFUND METHODS CORRESPOND TO EARNING METHODS *********
      *********** METHOD 'R' INDICATES A REPOSSESSION FOR 'FLC'*********
      ******************************************************************
           IF CMTHD1-LEN (WS-SUB2) = ZEROS
             AND PI-LAST-FUNC-DISPLAY
               NEXT SENTENCE
           ELSE
               IF CMTHD1-LEN (WS-SUB2) NOT = ZEROS
                  MOVE 'Y'            TO PI-REFUND-MTHD-KEYED-SW
                  MOVE AL-UNNON       TO CMTHD1-ATTRB (WS-SUB2)
                  IF CMTHD1 (WS-SUB2) EQUAL '1' OR '2' OR '3' OR '4'
                                         OR '5' OR '6' OR '8' OR ' '
      *                                  OR 'R'
                     NEXT SENTENCE
                  ELSE
                      MOVE -1         TO CMTHD1-LEN   (WS-SUB2)
                      MOVE ER-0582    TO EMI-ERROR
                      MOVE AL-UNBON   TO CMTHD1-ATTRB (WS-SUB2)
                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           MOVE SPACES                  TO WS-CONVERTED-CANDT2 (WS-SUB2)
           IF CCANDT2-LEN (WS-SUB2) = ZEROS
             AND PI-LAST-FUNC-DISPLAY
               NEXT SENTENCE
           ELSE
               IF CCANDT2-LEN (WS-SUB2) GREATER ZEROS
                   MOVE AL-UNNON       TO CCANDT2-ATTRB (WS-SUB2)
                   IF PI-LAST-FUNC-DISPLAY AND
                      CCANDT2 (WS-SUB2) = SPACES
                       MOVE LOW-VALUES TO WS-CONVERTED-CANDT2 (WS-SUB2)
                   ELSE
                      IF CCANDT2 (WS-SUB2) NUMERIC
                         MOVE 4              TO DC-OPTION-CODE
                         MOVE CCANDT2 (WS-SUB2) TO
                                            DC-GREG-DATE-1-MDY
                         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
                         MOVE DC-BIN-DATE-1  TO WS-CONVERTED-CANDT2
                                                               (WS-SUB2)
                         IF NO-CONVERSION-ERROR
                            NEXT SENTENCE
                         ELSE
                            MOVE -1       TO CCANDT2-LEN   (WS-SUB2)
                            MOVE ER-2227  TO EMI-ERROR
                            MOVE AL-UNBON TO CCANDT2-ATTRB (WS-SUB2)
                            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                      ELSE
                         MOVE -1         TO CCANDT2-LEN   (WS-SUB2)
                         MOVE ER-2223    TO EMI-ERROR
                         MOVE AL-UNBON   TO CCANDT2-ATTRB (WS-SUB2)
                         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
      ****     ELSE
      ****         IF CREFUND2-LEN (WS-SUB2) GREATER ZEROS
      ****            MOVE -1             TO CCANDT2-LEN   (WS-SUB2)
      ****            MOVE ER-2222        TO EMI-ERROR
      ****            MOVE AL-UNBOF       TO CCANDT2-ATTRB (WS-SUB2)
      ****            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           IF CREFUND2-LEN (WS-SUB2) = ZEROS
             AND PI-LAST-FUNC-DISPLAY
               NEXT SENTENCE
           ELSE
               IF CREFUND2-LEN (WS-SUB2) NOT = ZEROS
                  MOVE AL-UNNON       TO CREFUND2-ATTRB (WS-SUB2)
      *           MOVE CREFUND2I (WS-SUB2) TO DEEDIT-FIELD-X11
      *           PERFORM 8600-DEEDIT
      *           MOVE DEEDIT-FIELD-V2  TO WS-CREFUND2 (WS-SUB2).
                  
      * EXEC CICS BIF DEEDIT
      *               FIELD  (CREFUND2I (WS-SUB2))
      *               LENGTH (11)
      *           END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00007449' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CREFUND2I(WS-SUB2), 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                  MOVE CREFUND2I (WS-SUB2) TO WS-CREFUND2 (WS-SUB2).
           IF CMTHD2-LEN (WS-SUB2) = ZEROS
             AND PI-LAST-FUNC-DISPLAY
               NEXT SENTENCE
           ELSE
               IF CMTHD2-LEN (WS-SUB2) NOT = ZEROS
                  MOVE 'Y'            TO PI-REFUND-MTHD-KEYED-SW
                  MOVE AL-UNNON       TO CMTHD2-ATTRB (WS-SUB2)
                  IF CMTHD2 (WS-SUB2) EQUAL '1' OR '2' OR '3' OR '4'
                                         OR '5' OR '6' OR '8' OR ' '
      *                                  OR 'R'
                     NEXT SENTENCE
                  ELSE
                      MOVE -1         TO CMTHD2-LEN   (WS-SUB2)
                      MOVE ER-0582    TO EMI-ERROR
                      MOVE AL-UNBON   TO CMTHD2-ATTRB (WS-SUB2)
                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           IF CLIVES-LEN  (WS-SUB2)  GREATER ZEROS
               MOVE 'Y'                TO PI-CAN-LIVES-KEYED-SW
               MOVE CLIVESI (WS-SUB2 ) TO DEEDIT-FIELD
               PERFORM 8600-DEEDIT
               IF DEEDIT-FIELD-V0 NUMERIC
                   MOVE DEEDIT-FIELD-V0 TO WS-CLIVES   (WS-SUB2)
                   MOVE AL-UNNON       TO CLIVES-ATTRB (WS-SUB2)
               ELSE
                   MOVE -1             TO CLIVES-LEN   (WS-SUB2)
                   MOVE AL-UNBON       TO CLIVES-ATTRB (WS-SUB2)
                   MOVE ER-2223        TO EMI-ERROR
                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           IF CCANREA-LEN (WS-SUB2)  GREATER  ZEROS
               MOVE 'Y'                TO  PI-CAN-REA-KEYED-SW
               IF CCANREA (WS-SUB2) = 'R' OR ' '
                  MOVE CCANREA (WS-SUB2)
                                       TO  WS-CAN-REA (WS-SUB2)
                   MOVE AL-UANON       TO  CCANREA-ATTRB (WS-SUB2)
               ELSE
                   MOVE -1             TO  CCANREA-LEN (WS-SUB2)
                   MOVE AL-UABON       TO  CCANREA-ATTRB (WS-SUB2)
                   MOVE ER-9841        TO  EMI-ERROR
                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           IF CPAYEE-LEN  (WS-SUB2)  GREATER ZEROS
              MOVE 'Y'                 TO PI-PAYEE-KEYED-SW.
           IF CCHK-LEN    (WS-SUB2)  GREATER ZEROS
              MOVE 'Y'                 TO PI-CHK-REQ-KEYED-SW
              IF  CCHK    (WS-SUB2)  = 'R' OR ' '
                  NEXT SENTENCE
              ELSE
                  MOVE ER-7405         TO EMI-ERROR
                  MOVE -1              TO CCHK-LEN      (WS-SUB2)
                  MOVE AL-UABON        TO CCHK-ATTRB    (WS-SUB2)
                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           IF PI-LAST-FUNC-DISPLAY
              NEXT SENTENCE
           ELSE
              IF CCANDT1-LEN (WS-SUB2) GREATER ZEROS OR
                 CCANDT2-LEN (WS-SUB2) GREATER ZEROS
                   NEXT SENTENCE
                 ELSE
                   MOVE ER-2222          TO EMI-ERROR
                   MOVE -1               TO CCANDT1-LEN   (WS-SUB2)
                   MOVE AL-UNBOF         TO CCANDT1-ATTRB (WS-SUB2)
                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           IF PI-LAST-FUNC-DISPLAY
              NEXT SENTENCE
           ELSE
              IF WS-CONVERTED-CANDT1 (WS-SUB2) NOT = LOW-VALUES AND
                 WS-CONVERTED-CANDT2 (WS-SUB2) = SPACES AND
                 CREFUND2-LEN (WS-SUB2) GREATER ZEROS
                   MOVE WS-CONVERTED-CANDT1 (WS-SUB2) TO
                        WS-CONVERTED-CANDT2 (WS-SUB2)
                   MOVE CCANDT1     (WS-SUB2) TO CCANDT2     (WS-SUB2)
                   MOVE CCANDT1-LEN (WS-SUB2) TO CCANDT2-LEN (WS-SUB2).
           IF  EMI-ERROR = ZEROS
               MOVE 'Y'                TO PI-UPDATE-SW
           ELSE
               MOVE 'Y'                TO PI-ERROR-SW.
       1120-INCREMENT-OCCURRENCE.
           ADD +1                      TO WS-SUB2.
           IF WS-SUB2 GREATER +4 OR PI-LAST-FUNC-DISPLAY
               NEXT SENTENCE
           ELSE
               GO TO 1110-EDIT-MAPC-LOOP.
           IF PI-DATA-ERRORS
               MOVE AL-SABON           TO CSEQ-ATTRB (1) CSEQ-ATTRB (2)
                                          CSEQ-ATTRB (3) CSEQ-ATTRB (4)
              GO TO 8200-SEND-DATAONLY
           ELSE
              PERFORM 5000-BUILD-CANCEL-RECORD THRU 5900-EXIT.
       1130-NOTHING-TO-EDIT.
           IF NOT PI-LAST-FUNC-DISPLAY
              AND WS-DATA-NOT-KEYED
                MOVE ER-7400             TO EMI-ERROR
                MOVE -1                  TO CCANDT1-LEN (1)
                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                MOVE 'Y'                 TO PI-ERROR-SW
                GO TO 8200-SEND-DATAONLY.
           IF PI-SAV-BATCH-SEQ LESS PI-LAST-SEQ-NO-ADDED
              SUBTRACT 1 FROM PI-SAV-BATCH-SEQ
              MOVE ER-0000            TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              GO TO 2000-BROWSE-FWD.
           MOVE LOW-VALUES     TO VP630BI.
           MOVE SPACE          TO PI-DISPLAY-SW.
           ADD +1                 PI-LAST-SEQ-NO-ADDED
                                  GIVING PI-NEXT-DISPLAY-SEQ-NO.
           MOVE PI-NEXT-DISPLAY-SEQ-NO TO PI-SAV-BATCH-SEQ.
           PERFORM 8550-SET-MAP-SEQ-NOS
                        VARYING WS-SUB2 FROM +1 BY +1
                        UNTIL WS-SUB2 GREATER +4.
           MOVE ER-0000            TO EMI-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           GO TO 8100-SEND-INITIAL-MAP.
           EJECT
       1500-EDIT-ACCT-DT-RANGES.
      ******************************************************************
      *                                                                *
      *         E D I T   A C C O U N T   D A T E   R A N G E S        *
      *                                                                *
      *                                                                *
      *    NOTE:  IT IS ONLY NECESSARY TO EDIT THE DATE RANGES         *
      *           FOR COMPANYS THAT USE THE ACCOUNTS RECEIVABLE        *
      *           SYSTEM.                                              *
      *                                                                *
      *    1.  DETERMINE THE DATE RANGE FOR THE EFFECTIVE DATE.        *
      *                                                                *
      *    2.  VERIFY THE ACCOUNT AGENT.  THE ACCOUNT AGENT SHOULD BE  *
      *        THE SAME FOR THE ENTIRE BATCH.  IF IT CHANGES, IT IS    *
      *        AN ERROR.                                               *
      *                                                                *
      *    3.  VERIFY THAT THE FINANCIAL RESPONSIBLITY.  THE FINANCIAL *
      *        RESPONSIBILITY SHOULD BE THE SAME FOR THE ENTIRE BATCH. *
      *        IF IT CHANGES, IT IS AN ERROR.                          *
      *                                                                *
      ******************************************************************
           IF PI-AR-PROCESSING
              NEXT SENTENCE
           ELSE
              GO TO 1590-EXIT.
           MOVE +0                     TO WS-ACCT-SUB.
       1525-FIND-ACCT-DT-RANGE.
           ADD  +1                     TO WS-ACCT-SUB.
           IF WS-ACCT-SUB GREATER +32
              MOVE  ER-2119            TO EMI-ERROR
              IF PI-MAP-NAME = VP630B
                 MOVE -1               TO BPFENTRL
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 GO TO 8200-SEND-DATAONLY
              ELSE
                 MOVE -1               TO CEFFDT-LEN (WS-SUB2)
                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                 GO TO 1590-EXIT.
           IF WS-CONVERTED-EFFDT NOT LESS PI-ACCT-EFF-DT
                                                       (WS-ACCT-SUB)
              IF WS-CONVERTED-EFFDT  LESS PI-ACCT-EXP-DT
                                                       (WS-ACCT-SUB)
                 NEXT SENTENCE
              ELSE
                 GO TO 1525-FIND-ACCT-DT-RANGE.
           IF PI-ACCOUNT-AGENT = SPACES
              MOVE PI-ACCT-AGENT  (WS-ACCT-SUB) TO PI-ACCOUNT-AGENT
              MOVE PI-REMIT-AGENT (WS-ACCT-SUB) TO PI-FIN-RESP.
           IF PI-ACCT-AGENT  (WS-ACCT-SUB) = PI-ACCOUNT-AGENT
              NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO PI-ACCT-AGENT-ERROR-SW.
           IF PI-REMIT-AGENT (WS-ACCT-SUB) = PI-FIN-RESP
              NEXT SENTENCE
           ELSE
              MOVE 'Y'                 TO PI-FIN-RESP-ERROR-SW.
       1590-EXIT.
            EXIT.
           EJECT
       2000-BROWSE-FWD.
           MOVE LOW-VALUES             TO VP630BI.
           ADD +1                      TO PI-SAV-BATCH-SEQ.
           
      * EXEC CICS HANDLE CONDITION
      *        NOTFND (2020-END-FILE)
      *    END-EXEC.
      *    MOVE '"$I                   ! $ #00007628' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303037363238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * EXEC CICS READ
      *        SET     (ADDRESS OF PENDING-BUSINESS)
      *        DATASET (FILE-ID-ERPNDB)
      *        RIDFLD  (PI-SAV-ENDING-ERPNDB-KEY)
      *        GTEQ
      *    END-EXEC.
      *    MOVE '&"S        G          (   #00007631' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037363331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-SAV-ENDING-ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           IF PB-COMPANY-CD  = PI-SAV-COMP-CD  AND
              PB-ENTRY-BATCH = PI-SAV-ENTRY-BATCH
               NEXT SENTENCE
           ELSE
               GO TO 2020-END-FILE.
           IF PB-BATCH-TRAILER
               GO TO 2020-END-FILE.
           MOVE PB-BATCH-SEQ-NO        TO PI-SAV-BATCH-SEQ.
           IF PB-ISSUE
               MOVE VP630B             TO PI-MAP-NAME
               MOVE AL-SANOF           TO BDELHDGA
               PERFORM 7000-FORMAT-ISSUE-SCREEN THRU 7090-EXIT
           ELSE
               MOVE VP630C             TO PI-MAP-NAME
               MOVE AL-SANOF           TO CDELHDGA
               PERFORM 7100-FORMAT-CANCEL-SCREEN THRU 7190-EXIT.
       2010-SEND-MAP.
           GO TO 8100-SEND-INITIAL-MAP.
       2020-END-FILE.
           MOVE SPACE                  TO PI-DISPLAY-SW.
           IF PI-MAP-NAME = VP630B
               MOVE LOW-VALUES         TO VP630BI
               MOVE -1                 TO BPFENTRL
               ADD +1                     PI-LAST-SEQ-NO-ADDED
                     GIVING PI-NEXT-DISPLAY-SEQ-NO
               PERFORM 8550-SET-MAP-SEQ-NOS
           ELSE
               MOVE LOW-VALUES         TO VP630BI
               MOVE -1                 TO CPFENTRL
               ADD +1                     PI-LAST-SEQ-NO-ADDED
                     GIVING PI-NEXT-DISPLAY-SEQ-NO
               PERFORM 8550-SET-MAP-SEQ-NOS
                       VARYING WS-SUB2 FROM +1 BY +1
                       UNTIL WS-SUB2 GREATER +4.
           MOVE ER-2217                TO EMI-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           GO TO 2010-SEND-MAP.
           EJECT
       2100-BROWSE-BKWD.
           MOVE LOW-VALUES             TO VP630BI.
           SUBTRACT +1             FROM PI-SAV-BATCH-SEQ.
           IF PI-SAV-BATCH-SEQ NOT GREATER +0
               GO TO 2120-END-FILE.
           
      * EXEC CICS HANDLE CONDITION
      *        NOTFND (2100-BROWSE-BKWD)
      *    END-EXEC.
      *    MOVE '"$I                   ! % #00007680' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303037363830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * EXEC CICS READ
      *        SET     (ADDRESS OF PENDING-BUSINESS)
      *        DATASET (FILE-ID-ERPNDB)
      *        RIDFLD  (PI-SAV-ENDING-ERPNDB-KEY)
      *    END-EXEC.
      *    MOVE '&"S        E          (   #00007683' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037363833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-SAV-ENDING-ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           IF PB-COMPANY-CD  = PI-SAV-COMP-CD  AND
              PB-ENTRY-BATCH = PI-SAV-ENTRY-BATCH
               NEXT SENTENCE
           ELSE
               GO TO 2120-END-FILE.
           IF PB-BATCH-TRAILER
               GO TO 2120-END-FILE.
           IF PB-ISSUE
               MOVE VP630B             TO PI-MAP-NAME
               MOVE AL-SANOF           TO BDELHDGA
               PERFORM 7000-FORMAT-ISSUE-SCREEN THRU 7090-EXIT
           ELSE
               MOVE VP630C             TO PI-MAP-NAME
               MOVE AL-SANOF           TO CDELHDGA
               PERFORM 7100-FORMAT-CANCEL-SCREEN THRU 7190-EXIT.
       2110-SEND-MAP.
           GO TO 8100-SEND-INITIAL-MAP.
       2120-END-FILE.
           MOVE ER-2431                TO EMI-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           MOVE ER-0000                TO EMI-ERROR.
           MOVE ZEROS                  TO PI-SAV-BATCH-SEQ.
           GO TO 2000-BROWSE-FWD.
           EJECT
       3000-CONTINUE-ENTRY.
           MOVE PI-SAV-ENDING-ERPNDB-KEY TO ERPNDB-KEY.
           
      * EXEC CICS HANDLE CONDITION
      *        NOTFND (3300-REC-NOT-FND)
      *    END-EXEC.
      *    MOVE '"$I                   ! & #00007714' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303037373134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * EXEC CICS STARTBR
      *        DATASET (FILE-ID-ERPNDB)
      *        RIDFLD  (ERPNDB-KEY)
      *        GTEQ
      *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00007717' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       3100-READ-LOOP.
           
      * EXEC CICS HANDLE CONDITION
      *        ENDFILE (3200-END-BROWSE)
      *    END-EXEC.
      *    MOVE '"$''                   ! '' #00007723' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303037373233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * EXEC CICS READNEXT
      *        SET     (ADDRESS OF PENDING-BUSINESS)
      *        DATASET (FILE-ID-ERPNDB)
      *        RIDFLD  (ERPNDB-KEY)
      *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00007726' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           IF PB-COMPANY-CD  = PI-SAV-COMP-CD   AND
              PB-ENTRY-BATCH = PI-SAV-ENTRY-BATCH
               NEXT SENTENCE
           ELSE
               GO TO 3200-END-BROWSE.
           IF NOT PB-BATCH-TRAILER
               GO TO 3110-NOT-BATCH-TRAILER.
       3105-PRIME-PI-COUNTS.
           IF PI-LF-ISS-REMITTED = ZEROS
               MOVE PB-B-LF-ISS-PRM-REMITTED  TO PI-LF-ISS-REMITTED.
           IF PI-AH-ISS-REMITTED = ZEROS
               MOVE PB-B-AH-ISS-PRM-REMITTED  TO PI-AH-ISS-REMITTED.
           IF PI-ISS-CNT-REMITTED = ZEROS
               MOVE PB-B-ISSUE-CNT-REMITTED   TO PI-ISS-CNT-REMITTED.
           IF PI-CAN-CNT-REMITTED = ZEROS
               MOVE PB-B-CANCEL-CNT-REMITTED  TO PI-CAN-CNT-REMITTED.
           IF PI-LF-CAN-REMITTED = ZEROS
               MOVE PB-B-LF-CAN-PRM-REMITTED  TO PI-LF-CAN-REMITTED.
           IF PI-AH-CAN-REMITTED = ZEROS
               MOVE PB-B-AH-CAN-PRM-REMITTED  TO PI-AH-CAN-REMITTED.
           GO TO 3200-END-BROWSE.
       3110-NOT-BATCH-TRAILER.
           IF PB-ISSUE
               ADD PB-I-LF-PREMIUM-AMT     TO PI-LF-ISS-ENTERED
               ADD PB-I-LF-ALT-PREMIUM-AMT TO PI-LF-ISS-ENTERED
               ADD PB-I-AH-PREMIUM-AMT     TO PI-AH-ISS-ENTERED
               ADD +1                      TO PI-ISS-CNT-ENTERED
           ELSE
               ADD PB-C-LF-CANCEL-AMT      TO PI-LF-CAN-ENTERED
               ADD PB-C-AH-CANCEL-AMT      TO PI-AH-CAN-ENTERED
               ADD +1                      TO PI-CAN-CNT-ENTERED.
           MOVE PB-BATCH-SEQ-NO            TO PI-LAST-SEQ-NO-ADDED
                                              PI-SAV-BATCH-SEQ.
           GO TO 3100-READ-LOOP.
       3200-END-BROWSE.
           
      * EXEC CICS ENDBR
      *        DATASET (FILE-ID-ERPNDB)
      *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007766' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           ADD +1                         PI-LAST-SEQ-NO-ADDED
                                  GIVING PI-NEXT-DISPLAY-SEQ-NO.
           IF PI-MAINT-FUNC = 'B'
               MOVE ZEROS              TO PI-SAV-BATCH-SEQ
               GO TO 2000-BROWSE-FWD
           ELSE
               ADD +1                  TO PI-SAV-BATCH-SEQ.
           IF PI-LF-ISS-REMITTED  = ZEROS  AND
              PI-AH-ISS-REMITTED  = ZEROS  AND
              PI-LF-CAN-REMITTED  = ZEROS  AND
              PI-AH-CAN-REMITTED  = ZEROS  AND
              PI-ISS-CNT-REMITTED = ZEROS  AND
              PI-CAN-CNT-REMITTED = ZEROS
               MOVE  VP630B            TO PI-MAP-NAME
           ELSE
               IF PI-LF-ISS-REMITTED  = ZEROS AND
                  PI-AH-ISS-REMITTED  = ZEROS AND
                  PI-ISS-CNT-REMITTED = ZEROS
                   MOVE  VP630C        TO PI-MAP-NAME
               ELSE
                   MOVE  VP630B        TO PI-MAP-NAME.
           IF PI-MAP-NAME = VP630B
               PERFORM 8550-SET-MAP-SEQ-NOS
           ELSE
               PERFORM 8550-SET-MAP-SEQ-NOS
                       VARYING WS-SUB2 FROM +1 BY +1
                       UNTIL WS-SUB2 GREATER +4.
           GO TO 8100-SEND-INITIAL-MAP.
       3300-REC-NOT-FND.
           MOVE ER-2212                TO EMI-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           GO TO 8100-SEND-INITIAL-MAP.
           EJECT
       4000-BUILD-ISSUE-RECORD.
           IF BSEQ  GREATER PI-LAST-SEQ-NO-ADDED
               GO TO 4100-ADD-ISSUE-RECORD.
      ******************************************************************
      *   THE DATA ENTRY SYSTEM ALLOWS BROWSING OF THE CURRENT BUS.    *
      *   FILE. THE DATA ENTRY SYS. DOES NOT HAVE A MAINT. FUNCTION.   *
      *   THE PROGRAM ASSUMES THAT IF A MATCH ON THE READ FOR UPDATE   *
      *   IS SUCCESSFUL, THE RECORD HAS PREVIOUSLY BEEN DISPLAYED      *
      *   THRUOUGH A BROWSE.  CHANGES ARE APPLIED AND THE PB-RECORD IS *
      *   REWRITTEN, ELSE A NEW PENDING BUS. RECORD IS ADDED.          *
      ******************************************************************
           MOVE PI-COMPANY-CD          TO ERPNDB-COMP-CD.
           MOVE PI-SAV-ENTRY-BATCH     TO ERPNDB-ENTRY-BATCH.
           MOVE BSEQ                   TO ERPNDB-BATCH-SEQ.
           
      * EXEC CICS HANDLE CONDITION
      *        NOTFND (4100-ADD-ISSUE-RECORD)
      *    END-EXEC.
      *    MOVE '"$I                   ! ( #00007816' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303037383136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * EXEC CICS READ
      *        SET     (ADDRESS OF PENDING-BUSINESS)
      *        DATASET (FILE-ID-ERPNDB)
      *        RIDFLD  (ERPNDB-KEY)
      *        UPDATE
      *    END-EXEC.
      *    MOVE '&"S        EU         (   #00007819' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE PB-CONTROL-PRIMARY     TO ERPNDM-KEY.
           MOVE 'B'                    TO JP-RECORD-TYPE
           MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.
           MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.
           MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.
           PERFORM 8400-LOG-JOURNAL-RECORD.
           MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY.
           MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.
           MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT.
           IF BLAST-NAME-LEN GREATER ZEROS
               MOVE BLAST-NAME         TO PB-I-INSURED-LAST-NAME.
           IF B1ST-NAME-LEN GREATER ZEROS
               MOVE B1ST-NAME          TO PB-I-INSURED-FIRST-NAME.
           IF BINIT-LEN     GREATER ZEROS
               MOVE BINIT              TO PB-I-INSURED-MIDDLE-INIT.
           IF BAGE-LEN      GREATER ZEROS
               MOVE WS-BAGE            TO PB-I-AGE.
           IF BJNT-AGE-LEN  GREATER ZEROS
               MOVE WS-BJNT-AGE        TO PB-I-JOINT-AGE.
           IF BSEX-LEN      GREATER ZEROS
               MOVE BSEX               TO PB-I-INSURED-SEX.
           IF BTERM1-LEN > ZEROS
              MOVE WS-BTERM1           TO PB-I-LF-TERM.
           IF BTERM2-LEN > ZEROS
              MOVE WS-BTERM2             TO PB-I-AH-TERM.
      ******************************************************************
      *          IF BTYPE = ZEROS DELETE LIFE COVERAGE.                *
      *                                                                *
      *          IF BTYPE = SPACES ZERO OUT BENEFIT CODE.              *
      ******************************************************************
           IF BTYPE1-LEN > ZEROS
              IF BTYPE1           NOT = SPACES AND ZEROS
                 MOVE BTYPE1            TO PB-I-LF-INPUT-CD
                 MOVE WS-EDITED-LF-CODE TO PB-I-LIFE-BENEFIT-CD
                 MOVE WS-LF-ABBR-DESC   TO PB-I-LF-ABBR
              ELSE
                 IF BTYPE1      = SPACES
                    MOVE SPACES         TO PB-I-LF-INPUT-CD
                                           PB-I-LF-ABBR
                    MOVE ZEROS          TO PB-I-LIFE-BENEFIT-CD
                 ELSE
                    SUBTRACT PB-I-LF-PREMIUM-AMT FROM PI-LF-ISS-ENTERED
                    SUBTRACT PB-I-LF-ALT-PREMIUM-AMT FROM
                             PI-LF-ISS-ENTERED
                    MOVE SPACES         TO PB-I-LF-INPUT-CD
                                           PB-I-LF-ABBR
                    MOVE ZEROS          TO PB-I-LF-TERM
                                           PB-I-LF-BENEFIT-AMT
                                           PB-I-LF-PREMIUM-AMT
                                           PB-I-LF-BENEFIT-CD
                                           PB-I-LF-PREM-CALC
                                           PB-I-LF-ALT-BENEFIT-AMT
                                           PB-I-LF-ALT-PREMIUM-AMT
                                           PB-I-LF-CRIT-PER
                    MOVE LOW-VALUES     TO PB-I-LF-EXPIRE-DT.
           IF  BBENE1-LEN > ZEROS
               MOVE WS-BBEN1           TO PB-I-LF-BENEFIT-AMT.
           IF  BALT-BEN1-LEN > ZEROS
               MOVE WS-BALT-BEN1          TO PB-I-LF-ALT-BENEFIT-AMT.
           IF  BPREM1-LEN > ZEROS
               IF WS-BPREM1       = WS-ALL-NINES OR
                  WS-BPREM1       GREATER WS-ALL-NINES
                  SUBTRACT PB-I-LF-PREMIUM-AMT FROM PI-LF-ISS-ENTERED
                  MOVE ZEROS           TO PB-I-LF-PREMIUM-AMT
                  MOVE '?'             TO PB-I-LF-CALC-FLAG
               ELSE
                  SUBTRACT PB-I-LF-PREMIUM-AMT FROM PI-LF-ISS-ENTERED
                  MOVE WS-BPREM1       TO PB-I-LF-PREMIUM-AMT
                  ADD  WS-BPREM1       TO PI-LF-ISS-ENTERED
                  MOVE SPACE           TO PB-I-LF-CALC-FLAG.
           IF  BALT-PREM1-LEN > ZEROS
               SUBTRACT PB-I-LF-ALT-PREMIUM-AMT FROM PI-LF-ISS-ENTERED
               MOVE WS-BALT-PREM1      TO PB-I-LF-ALT-PREMIUM-AMT
               ADD  WS-BALT-PREM1      TO PI-LF-ISS-ENTERED.
           IF  BALT-PREM2-LEN > ZEROS
               MOVE WS-BALT-PREM2      TO PB-I-TOT-FEES
           END-IF
      ******************************************************************
      *          IF BTYPE = ZEROS DELETE A&H COVERAGE.                 *
      *                                                                *
      *          IF BTYPE = SPACES ZERO OUT BENEFIT CODE.              *
      ******************************************************************
           IF BTYPE2-LEN > ZEROS
              IF BTYPE2           NOT = SPACES AND ZEROS
                 MOVE BTYPE2            TO PB-I-AH-INPUT-CD
                 MOVE WS-EDITED-LF-CODE TO PB-I-AH-BENEFIT-CD
                 MOVE WS-LF-ABBR-DESC   TO PB-I-AH-ABBR
              ELSE
                 IF BTYPE2      = SPACES
                    MOVE SPACES         TO PB-I-AH-INPUT-CD
                                           PB-I-AH-ABBR
                    MOVE ZEROS          TO PB-I-AH-BENEFIT-CD
                 ELSE
                    SUBTRACT PB-I-AH-PREMIUM-AMT FROM PI-AH-ISS-ENTERED
                    MOVE SPACES         TO PB-I-AH-INPUT-CD
                                           PB-I-AH-ABBR
                    MOVE ZEROS          TO PB-I-AH-TERM
                                           PB-I-AH-BENEFIT-AMT
                                           PB-I-AH-PREMIUM-AMT
                                           PB-I-AH-BENEFIT-CD
                                           PB-I-AH-PREM-CALC
                                           PB-I-AH-CRIT-PER
                                           PB-I-TOT-FEES
                                           PB-I-TOT-FEES-CALC
                    MOVE LOW-VALUES     TO PB-I-AH-EXPIRE-DT.
           IF  BBENE2-LEN > ZEROS
               MOVE WS-BBEN2           TO PB-I-AH-BENEFIT-AMT.
           IF  BPREM2-LEN > ZEROS
               IF WS-BPREM2       = WS-ALL-NINES OR
                  WS-BPREM2       GREATER WS-ALL-NINES
                  SUBTRACT PB-I-AH-PREMIUM-AMT FROM PI-AH-ISS-ENTERED
                  MOVE ZEROS              TO PB-I-AH-PREMIUM-AMT
                  MOVE '?'                TO PB-I-AH-CALC-FLAG
               ELSE
                  SUBTRACT PB-I-AH-PREMIUM-AMT FROM PI-AH-ISS-ENTERED
                  MOVE WS-BPREM2       TO PB-I-AH-PREMIUM-AMT
                  ADD  WS-BPREM2       TO PI-AH-ISS-ENTERED
                  MOVE SPACE           TO PB-I-AH-CALC-FLAG.
           MOVE ZEROS                  TO PB-I-LF-CRIT-PER
           IF BCRIT-PERD2-LEN > ZEROS
              MOVE WS-BCRIT-PERD2       TO PB-I-AH-CRIT-PER.
           IF BLN-OFFICER-LEN  GREATER ZEROS
               MOVE BLN-OFFICER        TO PB-I-LOAN-OFFICER.
           IF PB-1ST-PMT-DT-PROCESSING
              IF PB-I-1ST-PMT-DT = LOW-VALUES
                 MOVE '1'              TO PB-I-DATA-ENTRY-SW.
           IF BVIN-LEN > ZEROS
              MOVE BVIN-NOI            TO PB-I-VIN
           END-IF
           IF BJNT-1ST-NAME-LEN   GREATER ZEROS
               MOVE BJNT-1ST-NAME      TO PB-I-JOINT-FIRST-NAME.
           IF BJNT-INIT-LEN       GREATER ZEROS
               MOVE BJNT-INIT          TO PB-I-JOINT-MIDDLE-INIT.
           IF BJNT-LST-NAME-LEN   GREATER ZEROS
               MOVE BJNT-LST-NAME      TO PB-I-JOINT-LAST-NAME.
           IF BBENEFICIARY-LEN    GREATER ZEROS
               MOVE BBENEFICIARY       TO PB-I-BENEFICIARY-NAME.
           MOVE PI-LIFE-OVERRIDE-L1    TO PB-LIFE-OVERRIDE-L1.
           MOVE PI-AH-OVERRIDE-L1      TO PB-AH-OVERRIDE-L1.
           IF PI-MAIL-YES
              MOVE '1'                 TO PB-I-MAIL-ADDRS-SW.
           MOVE 'C'                    TO JP-RECORD-TYPE.
           MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.
           MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.
           move pb-control-by-account  to elcrtt-key
           
      * EXEC CICS REWRITE
      *        DATASET (FILE-ID-ERPNDB)
      *        FROM    (PENDING-BUSINESS)
      *    END-EXEC.
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00007970' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE ERPNDB-KEY             TO PI-SAV-ENDING-ERPNDB-KEY.
           PERFORM 8400-LOG-JOURNAL-RECORD.
           IF EIBAID = DFHENTER
               MOVE PI-NEXT-DISPLAY-SEQ-NO TO BSEQ
               ADD +1                      TO PI-NEXT-DISPLAY-SEQ-NO
               MOVE AL-SABON               TO BSEQ-ATTRB.
           if (byear-len      <> zeros)
              or (bmake-len   <> zeros)
              or (bmodel-len  <> zeros)
              or (bometer-len <> zeros)
              move 'C'                 to elcrtt-trlr-type
              perform 4910-read-elcrtt-update
                                       thru 4910-exit
              if resp-normal
                 perform 4920-rewrite-elcrtt
                                       thru 4920-exit
              else
                 if resp-notfnd
                    perform 4930-write-elcrtt
                                       thru 4930-exit
                 end-if
              end-if
           end-if
           IF PI-MAIL-YES
              NEXT SENTENCE
           ELSE
              GO TO 4900-EXIT.
           IF BLAST-NAME-LEN = ZEROS AND
              B1ST-NAME-LEN  = ZEROS AND
              BINIT-LEN      = ZEROS AND
              BADDRS1-LEN    = ZEROS AND
              BADDRS2-LEN    = ZEROS AND
              BCITY-LEN      = ZEROS AND
              BSTATE-LEN     = ZEROS AND
              BZIPCDE-LEN    = ZEROS AND
              BBENEFICIARY-LEN = ZEROS AND
              BCADDR1-LEN    = ZEROS AND
              BCADDR2-LEN    = ZEROS AND
              BCCITY-LEN     = ZEROS AND
              BCSTATE-LEN    = ZEROS AND
              BCZIPCD-LEN    = ZEROS
              GO TO 4900-EXIT.
           
      * EXEC CICS HANDLE CONDITION
      *        NOTFND (4185-ADD-MAILING-RECORD)
      *    END-EXEC.
      *    MOVE '"$I                   ! ) #00008016' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303038303136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * EXEC CICS READ
      *        SET     (ADDRESS OF PENDING-MAILING-DATA)
      *        DATASET (FILE-ID-ERPNDM)
      *        RIDFLD  (ERPNDM-KEY)
      *        UPDATE
      *    END-EXEC.
      *    MOVE '&"S        EU         (   #00008019' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038303139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDM, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-MAILING-DATA TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE 'B'                    TO JP-RECORD-TYPE.
           MOVE PENDING-MAILING-DATA   TO JP-RECORD-AREA.
           MOVE ERPNDM-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.
           MOVE FILE-ID-ERPNDM         TO JP-FILE-ID.
           PERFORM 8400-LOG-JOURNAL-RECORD.
           MOVE PI-PROCESSOR-ID        TO PM-LAST-MAINT-BY.
           MOVE EIBTIME                TO PM-LAST-MAINT-HHMMSS.
           MOVE WS-CURRENT-BIN-DT      TO PM-LAST-MAINT-DT.
           IF BLAST-NAME-LEN      GREATER ZEROS
               MOVE BLAST-NAME         TO PM-INSURED-LAST-NAME.
           IF B1ST-NAME-LEN       GREATER ZEROS
               MOVE B1ST-NAME          TO PM-INSURED-FIRST-NAME.
           IF BINIT-LEN           GREATER ZEROS
               MOVE BINIT              TO PM-INSURED-MIDDLE-INIT.
           IF BAGE-LEN            GREATER ZEROS
               MOVE WS-BAGE            TO PM-INSURED-ISSUE-AGE.
           IF BLAST-NAME-LEN      GREATER ZEROS
               MOVE BLAST-NAME         TO PM-INSURED-LAST-NAME.
           IF BINIT-LEN           GREATER ZEROS
               MOVE BINIT              TO PM-INSURED-MIDDLE-INIT.
           IF BADDRS1-LEN         GREATER ZERO
               MOVE BADDRS1            TO PM-ADDRESS-LINE-1.
           IF BADDRS2-LEN         GREATER ZERO
               MOVE BADDRS2            TO PM-ADDRESS-LINE-2.
           IF BCITY-LEN > 0
              MOVE BCITY               TO PM-CITY
           END-IF
           IF BSTATE-LEN > 0
              MOVE BSTATE              TO PM-STATE
           END-IF
           IF BZIPCDE-LEN GREATER ZEROS
              MOVE BZIPCDE             TO WS-ZIP-CODE
           ELSE
              GO TO 4010-CRED-BENE
           END-IF
           IF WS-CANADIAN-ZIP
              IF WS-ZIP-4 = SPACE  OR  '-'
                 MOVE WS-ZIP-CAN-2-POST1   TO PM-CAN-POST1
                 MOVE WS-ZIP-CAN-2-POST2   TO PM-CAN-POST2
              ELSE
                 MOVE WS-ZIP-CAN-1-POST1   TO PM-CAN-POST1
                 MOVE WS-ZIP-CAN-1-POST2   TO PM-CAN-POST2
              END-IF
           ELSE
              IF WS-ZIP-6 = SPACE  OR  '-'
                 MOVE WS-ZIP-AM-2-CODE     TO PM-ZIP-CODE
                 MOVE WS-ZIP-AM-2-PLUS4    TO PM-ZIP-PLUS4
              ELSE
                 MOVE WS-ZIP-AM-1-CODE     TO PM-ZIP-CODE
                 MOVE WS-ZIP-AM-1-PLUS4    TO PM-ZIP-PLUS4
              END-IF
           END-IF
           .
       4010-CRED-BENE.
           IF BBENEFICIARY-LEN > ZEROS
              MOVE BBENEFICIARY        TO PM-CRED-BENE-NAME
           END-IF
           IF BCADDR1-LEN > ZEROS
              MOVE BCADDR1             TO PM-CRED-BENE-ADDR
           END-IF
           IF BCADDR2-LEN > ZEROS
              MOVE BCADDR2             TO PM-CRED-BENE-ADDR2
           END-IF
           IF BCCITY-LEN > ZEROS
              MOVE BCCITY              TO PM-CRED-BENE-CITY
           END-IF
           IF BCSTATE-LEN > ZEROS
              MOVE BCSTATE             TO PM-CRED-BENE-STATE
           END-IF
           IF BCZIPCD-LEN > ZEROS
              MOVE BCZIPCD             TO WS-ZIP-CODE
           ELSE
              GO TO 4010-CONTINUE
           END-IF
           IF WS-CANADIAN-ZIP
              IF WS-ZIP-4 = SPACE  OR  '-'
                 MOVE WS-ZIP-CAN-2-POST1   TO PM-CB-CAN-POST1
                 MOVE WS-ZIP-CAN-2-POST2   TO PM-CB-CAN-POST2
              ELSE
                 MOVE WS-ZIP-CAN-1-POST1   TO PM-CB-CAN-POST1
                 MOVE WS-ZIP-CAN-1-POST2   TO PM-CB-CAN-POST2
              END-IF
           ELSE
              IF WS-ZIP-6 = SPACE  OR  '-'
                 MOVE WS-ZIP-AM-2-CODE     TO PM-CB-ZIP-CODE
                 MOVE WS-ZIP-AM-2-PLUS4    TO PM-CB-ZIP-PLUS4
              ELSE
                 MOVE WS-ZIP-AM-1-CODE     TO PM-CB-ZIP-CODE
                 MOVE WS-ZIP-AM-1-PLUS4    TO PM-CB-ZIP-PLUS4
              END-IF
           END-IF
           .
       4010-CONTINUE.
           MOVE 'C'                    TO JP-RECORD-TYPE.
           MOVE PENDING-MAILING-DATA   TO JP-RECORD-AREA.
           MOVE ERPNDM-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.
           MOVE FILE-ID-ERPNDM         TO JP-FILE-ID.
           
      * EXEC CICS REWRITE
      *        DATASET (FILE-ID-ERPNDM)
      *        FROM    (PENDING-MAILING-DATA)
      *    END-EXEC.
           MOVE LENGTH OF
            PENDING-MAILING-DATA
             TO DFHEIV11
      *    MOVE '&& L                  %   #00008122' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303038313232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDM, 
                 PENDING-MAILING-DATA, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           PERFORM 8400-LOG-JOURNAL-RECORD.
           GO TO 4900-EXIT.
           EJECT
       4100-ADD-ISSUE-RECORD.
           
      * EXEC CICS GETMAIN
      *        SET     (ADDRESS OF CERTIFICATE-TRAILERS)
      *        LENGTH  (ELCRTT-RECORD-LENGTH)
      *        INITIMG (GETMAIN-SPACE)
      *    END-EXEC
      *    MOVE ',"IL                  $   #00008130' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038313330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ELCRTT-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF CERTIFICATE-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           
      * EXEC CICS GETMAIN
      *        SET     (ADDRESS OF PENDING-BUSINESS)
      *        LENGTH  (ERPNDB-RECORD-LENGTH)
      *        INITIMG (GETMAIN-SPACE)
      *    END-EXEC.
      *    MOVE ',"IL                  $   #00008135' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038313335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERPNDB-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE 'PB'                   TO PB-RECORD-ID.
           MOVE PI-COMPANY-CD          TO PB-COMPANY-CD
                                          PB-COMPANY-CD-A1.
           MOVE PI-COMPANY-ID          TO PB-COMPANY-ID.
           MOVE PI-SAV-ENTRY-BATCH     TO PB-ENTRY-BATCH.
           MOVE BSEQ                   TO PB-BATCH-SEQ-NO.
           IF BSEQ   GREATER PI-LAST-SEQ-NO-ADDED
               MOVE BSEQ          TO PI-LAST-SEQ-NO-ADDED.
           MOVE PI-SAV-CARRIER         TO PB-CARRIER.
           MOVE PI-SAV-GROUPING        TO PB-GROUPING.
           MOVE PI-SAV-STATE           TO PB-STATE.
           MOVE PI-SAV-ACCOUNT         TO PB-ACCOUNT.
           MOVE '1'                    TO PB-RECORD-TYPE.
           MOVE BCERT                  TO PB-CERT-PRIME.
           MOVE WS-CONVERTED-EFFDT     TO PB-CERT-EFF-DT.
           MOVE ZEROS                  TO PB-BATCH-CHG-SEQ-NO
                                          PB-ALT-CHG-SEQ-NO.
           MOVE +0                     TO PB-NO-OF-ERRORS.
           MOVE LOW-VALUES             TO PB-COMMON-ERRORS.
           MOVE ZEROS                  TO PB-I-LOAN-TERM
                                          PB-I-LF-POLICY-FEE
                                          PB-I-LF-PREM-CALC
                                          PB-I-LF-ALT-PREM-CALC
                                          PB-I-LF-RATE
                                          PB-I-LF-ALT-RATE
                                          PB-I-LF-REI-RATE
                                          PB-I-LF-ALT-REI-RATE
                                          PB-I-RATE-DEV-PCT-LF
                                          PB-I-cancel-fee
                                          PB-I-AH-PREM-CALC
                                          PB-I-TOT-FEES
                                          PB-I-TOT-FEES-CALC
                                          PB-I-AH-RATE
                                          PB-I-AH-REI-RATE
                                          PB-I-AH-RATE-TRM
                                          PB-I-RATE-DEV-PCT-AH
                                          PB-I-BUSINESS-TYPE
                                          PB-I-LIFE-COMMISSION
                                          PB-I-JOINT-COMMISSION
                                          PB-I-AH-COMMISSION
                                          PB-I-CURR-SEQ
                                          PB-CHG-COUNT
                                          PB-LF-BILLED-AMTS
                                          PB-AH-BILLED-AMTS
                                          PB-CALC-TOLERANCE
                                          PB-I-EXTENTION-DAYS
      *                                   PB-I-MICROFILM-NO
                                          PB-I-TERM-IN-DAYS
                                          PB-I-STATE-TAX
                                          PB-I-MUNI-TAX
                                          PB-I-NUM-BILLED
                                          pb-i-loan-apr.
           MOVE LOW-VALUES             TO PB-CREDIT-ACCEPT-DT
                                          PB-I-LF-EXPIRE-DT
                                          PB-I-AH-EXPIRE-DT
                                          PB-I-1ST-PMT-DT
                                          PB-BILLED-DT
                                          PB-ACCT-EFF-DT
                                          PB-ACCT-EXP-DT.
           MOVE 'X'                    TO PB-FATAL-FLAG.
           IF PI-MAIL-YES
              MOVE '1'                 TO PB-I-MAIL-ADDRS-SW.
           IF PI-NB-MONTH-END-DT NOT = SPACES
              MOVE PI-NB-MONTH-END-DT  TO PB-CREDIT-SELECT-DT
             ELSE
              MOVE PI-CR-MONTH-END-DT     TO PB-CREDIT-SELECT-DT.
           IF BSFX-LEN            GREATER ZEROS
               MOVE BSFX               TO PB-CERT-SFX.
           IF BLAST-NAME-LEN      GREATER ZEROS
               MOVE BLAST-NAME         TO PB-I-INSURED-LAST-NAME.
           IF B1ST-NAME-LEN       GREATER ZEROS
               MOVE B1ST-NAME          TO PB-I-INSURED-FIRST-NAME.
           IF BINIT-LEN           GREATER ZEROS
               MOVE BINIT              TO PB-I-INSURED-MIDDLE-INIT.
           IF BAGE-LEN            GREATER ZEROS
               MOVE WS-BAGE            TO PB-I-AGE
           ELSE
               MOVE ZEROS              TO PB-I-AGE.
           IF BJNT-AGE-LEN        GREATER ZEROS
               MOVE WS-BJNT-AGE        TO PB-I-JOINT-AGE
           ELSE
               MOVE ZEROS              TO PB-I-JOINT-AGE.
               MOVE LOW-VALUES          TO PB-I-BIRTHDAY.
              MOVE LOW-VALUES          TO PB-I-JOINT-BIRTHDAY
           IF BSEX-LEN            GREATER ZEROS
               MOVE BSEX               TO PB-I-INSURED-SEX.
           IF BTERM1-LEN > ZEROS
              MOVE WS-BTERM1              TO PB-I-LF-TERM
           ELSE
              MOVE ZEROS               TO PB-I-LF-TERM.
           IF BTERM2-LEN > ZEROS
              MOVE WS-BTERM2              TO PB-I-AH-TERM
           ELSE
              MOVE ZEROS              TO PB-I-AH-TERM.
           MOVE ZEROS              TO PB-I-LOAN-TERM.
               MOVE ZEROS              TO PB-I-PAY-FREQUENCY.
               MOVE ZEROS              TO PB-I-NO-OF-PAYMENTS.
               MOVE ZEROS              TO PB-I-PAYMENT-AMOUNT.
           IF BTYPE1-LEN > ZEROS
              IF BTYPE1           NOT = ZEROS OR SPACES
                 MOVE BTYPE1            TO PB-I-LF-INPUT-CD
                 MOVE WS-EDITED-LF-CODE TO PB-I-LIFE-BENEFIT-CD
                 MOVE WS-LF-ABBR-DESC   TO PB-I-LF-ABBR
              ELSE
                 MOVE ZEROS             TO PB-I-LIFE-BENEFIT-CD
           ELSE
                 MOVE ZEROS             TO PB-I-LIFE-BENEFIT-CD.
           IF  BBENE1-LEN > ZEROS
               MOVE WS-BBEN1           TO PB-I-LF-BENEFIT-AMT
           ELSE
               MOVE ZEROS              TO PB-I-LF-BENEFIT-AMT.
           IF  BALT-BEN1-LEN > ZEROS
               MOVE WS-BALT-BEN1       TO PB-I-LF-ALT-BENEFIT-AMT
           ELSE
               MOVE ZEROS              TO PB-I-LF-ALT-BENEFIT-AMT.
           IF  BPREM1-LEN > ZEROS
               IF WS-BPREM1      = WS-ALL-NINES OR
                  WS-BPREM1      GREATER WS-ALL-NINES
                  MOVE ZEROS           TO PB-I-LF-PREMIUM-AMT
                  MOVE '?'             TO PB-I-LF-CALC-FLAG
               ELSE
                  ADD  WS-BPREM1       TO PI-LF-ISS-ENTERED
                  MOVE WS-BPREM1       TO PB-I-LF-PREMIUM-AMT
           ELSE
               MOVE ZEROS              TO PB-I-LF-PREMIUM-AMT.
           IF  BALT-PREM1-LEN > ZEROS
               MOVE WS-BALT-PREM1      TO PB-I-LF-ALT-PREMIUM-AMT
               ADD  WS-BALT-PREM1      TO PI-LF-ISS-ENTERED
           ELSE
               MOVE ZEROS              TO PB-I-LF-ALT-PREMIUM-AMT.
           IF  BALT-PREM2-LEN > ZEROS
               MOVE WS-BALT-PREM2      TO PB-I-TOT-FEES
           ELSE
               MOVE ZEROS              TO PB-I-TOT-FEES
           END-IF
           IF BTYPE2-LEN > ZEROS
              IF BTYPE2           NOT = ZEROS
                 MOVE BTYPE2            TO PB-I-AH-INPUT-CD
                 MOVE WS-EDITED-AH-CODE TO PB-I-AH-BENEFIT-CD
                 MOVE WS-AH-ABBR-DESC   TO PB-I-AH-ABBR
              ELSE
                 MOVE ZEROS             TO PB-I-AH-BENEFIT-CD
           ELSE
                 MOVE ZEROS             TO PB-I-AH-BENEFIT-CD.
           IF  BBENE2-LEN > ZEROS
               MOVE WS-BBEN2           TO PB-I-AH-BENEFIT-AMT
           ELSE
               MOVE ZEROS              TO PB-I-AH-BENEFIT-AMT.
           IF  BPREM2-LEN > ZEROS
               IF WS-BPREM2      = WS-ALL-NINES OR
                  WS-BPREM2      GREATER WS-ALL-NINES
                  MOVE ZEROS            TO PB-I-AH-PREMIUM-AMT
                  MOVE '?'              TO PB-I-AH-CALC-FLAG
               ELSE
                  ADD  WS-BPREM2        TO PI-AH-ISS-ENTERED
                  MOVE WS-BPREM2        TO PB-I-AH-PREMIUM-AMT
           ELSE
               MOVE ZEROS               TO PB-I-AH-PREMIUM-AMT.
           IF PB-COMPANY-ID = 'NSL'
           IF PB-I-AGE       GREATER 49   OR
              PB-I-JOINT-AGE GREATER 49
               MOVE 'H'                TO PB-RECORD-BILL
             ELSE
           IF PB-I-LF-BENEFIT-AMT GREATER +14999.99 OR
              PB-I-AH-BENEFIT-AMT GREATER +14999.99
                MOVE 'H'                TO PB-RECORD-BILL.
      *    IF BCRIT-PERD-LEN      (1)   GREATER ZEROS
      *       MOVE WS-BCRIT-PERD  (1)   TO PB-I-LF-CRIT-PER
      *    ELSE
              MOVE ZEROS                TO PB-I-LF-CRIT-PER.
           IF BCRIT-PERD2-LEN > ZEROS
              MOVE WS-BCRIT-PERD2       TO PB-I-AH-CRIT-PER
           ELSE
              MOVE ZEROS                TO PB-I-AH-CRIT-PER.
      *    IF BIND-GRP-LEN        GREATER ZEROS
      *        MOVE BIND-GRP           TO PB-I-INDV-GRP-OVRD.
      *    IF BRTCLS-LEN          GREATER ZEROS
      *        MOVE BRTCLS             TO PB-I-RATE-CLASS-OVRD.
      *    IF BSIG-LEN            GREATER ZEROS
      *        MOVE BSIG               TO PB-I-SIG-SW.
           IF BLN-OFFICER-LEN     GREATER ZEROS
               MOVE BLN-OFFICER        TO PB-I-LOAN-OFFICER.
           MOVE LOW-VALUES               TO PB-I-LF-EXPIRE-DT.
           MOVE LOW-VALUES               TO PB-I-AH-EXPIRE-DT.
              MOVE ZEROS               TO PB-I-TERM-IN-DAYS
                                          PB-I-EXTENTION-DAYS.
           IF PB-1ST-PMT-DT-PROCESSING
              IF PB-I-1ST-PMT-DT = LOW-VALUES
                 MOVE '1'              TO PB-I-DATA-ENTRY-SW.
           IF BVIN-LEN > ZEROS
              MOVE BVIN-NOI            TO PB-I-VIN
           ELSE
              MOVE SPACES              TO PB-I-VIN
           END-IF
           MOVE ZEROS               TO PB-I-LIVES.
           IF BJNT-1ST-NAME-LEN   GREATER ZEROS
               MOVE BJNT-1ST-NAME      TO PB-I-JOINT-FIRST-NAME.
           IF BJNT-INIT-LEN       GREATER ZEROS
               MOVE BJNT-INIT          TO PB-I-JOINT-MIDDLE-INIT.
           IF BJNT-LST-NAME-LEN   GREATER ZEROS
               MOVE BJNT-LST-NAME      TO PB-I-JOINT-LAST-NAME.
           IF BBENEFICIARY-LEN    GREATER ZEROS
               MOVE BBENEFICIARY       TO PB-I-BENEFICIARY-NAME.
           MOVE PI-LIFE-OVERRIDE-L1    TO PB-LIFE-OVERRIDE-L1.
           MOVE PI-AH-OVERRIDE-L1      TO PB-AH-OVERRIDE-L1.
       4175-WRITE-PB-RECORD.
           MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY
                                          PB-INPUT-BY.
           MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.
           MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT
                                          PB-INPUT-DT.
           MOVE PB-CONTROL-PRIMARY     TO PB-CONTROL-BY-ORIG-BATCH.
           MOVE PB-COMPANY-CD          TO PB-CSR-COMPANY-CD.
           MOVE PI-CSR-ID              TO PB-CSR-ID.
           MOVE PB-ENTRY-BATCH         TO PB-CSR-ENTRY-BATCH.
           MOVE PB-BATCH-SEQ-NO        TO PB-CSR-BATCH-SEQ-NO.
           MOVE PB-BATCH-CHG-SEQ-NO    TO PB-CSR-BATCH-CHG-SEQ-NO.
           MOVE 'A'                    TO JP-RECORD-TYPE.
           MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.
           MOVE PB-CONTROL-PRIMARY     TO PI-SAV-ENDING-ERPNDB-KEY
                                          ERPNDM-KEY.
      *    MOVE PI-SAV-REFERENCE       TO PB-I-REFERENCE.
           ADD +1                      TO PI-SAV-BATCH-SEQ.
           move pb-control-by-account  to elcrtt-key
           
      * EXEC CICS HANDLE CONDITION
      *        DUPREC (4200-DUPLICATE-ALT-INDEX)
      *    END-EXEC.
      *    MOVE '"$%                   ! * #00008364' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303038333634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * EXEC CICS WRITE
      *        DATASET (FILE-ID-ERPNDB)
      *        FROM    (PENDING-BUSINESS)
      *        RIDFLD  (PB-CONTROL-PRIMARY)
      *    END-EXEC.
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00008367' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038333637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 PB-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           ADD +1                      TO PI-ISS-CNT-ENTERED.
           MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.
           MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.
           PERFORM 8400-LOG-JOURNAL-RECORD.
           MOVE PI-NEXT-DISPLAY-SEQ-NO TO BSEQ.
           MOVE AL-SABON               TO BSEQ-ATTRB.
           ADD +1                      TO PI-NEXT-DISPLAY-SEQ-NO.
           if (byear-len      <> zeros)
              or (bmake-len   <> zeros)
              or (bmodel-len  <> zeros)
              or (bometer-len <> zeros)
              move pb-control-by-account
                                       to elcrtt-key
              move 'C'                 to elcrtt-trlr-type
              perform 4910-read-elcrtt-update
                                       thru 4910-exit
              if resp-normal
                 perform 4920-rewrite-elcrtt
                                       thru 4920-exit
              else
                 if resp-notfnd
                    perform 4930-write-elcrtt
                                       thru 4930-exit
                 end-if
              end-if
           end-if
      ******************************************************************
      *    CHECK THE FIRST ISSUE RECORD IN EVERY NEW BATCH.  VERIFY    *
      *    THAT THE CERTIFICATE DOES NOT EXIST ON THE CERT. MASTER     *
      *    FILE.  IF IT DOES DISPLAY WARNING MESSAGE ON BLANK SCREEN.  *
      ******************************************************************
           IF  PI-MAINT-FUNC = 'N' NEXT SENTENCE
              ELSE
               GO TO 4185-ADD-MAILING-RECORD.
           IF  PI-ISSUE-ADDED
               GO TO 4185-ADD-MAILING-RECORD.
           MOVE 'Y'                    TO  PI-ISSUE-ADDED-SW.
           
      * EXEC CICS HANDLE CONDITION
      *        NOTFND (4185-ADD-MAILING-RECORD)
      *    END-EXEC.
      *    MOVE '"$I                   ! + #00008409' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303038343039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE PB-CONTROL-BY-ACCOUNT  TO  ELCERT-KEY.
           MOVE PI-SAV-FC-CARRIER      TO  ELCERT-CARRIER.
           MOVE PI-SAV-FC-GROUPING     TO  ELCERT-GROUPING.
           MOVE PI-SAV-FC-STATE        TO  ELCERT-STATE.
           
      * EXEC CICS READ
      *        SET     (ADDRESS OF CERTIFICATE-MASTER)
      *        DATASET (FILE-ID-ELCERT)
      *        RIDFLD  (ELCERT-KEY)
      *        LENGTH  (ELCERT-RECORD-LENGTH)
      *        UPDATE
      *    END-EXEC.
      *    MOVE '&"SL       EU         (   #00008416' TO DFHEIV0
           MOVE X'2622534C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038343136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ELCERT, 
                 DFHEIV20, 
                 ELCERT-RECORD-LENGTH, 
                 ELCERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           IF  CERT-WAS-CREATED-FOR-CLAIM
               GO TO 4185-ADD-MAILING-RECORD.
           go to 4900-exit
           .
       4185-ADD-MAILING-RECORD.
           IF  PI-MAIL-YES
               IF  BADDRS1-LEN > ZERO
                       OR
                   BBENEFICIARY-LEN > ZERO
                        OR
                   BADDRS2-LEN > ZERO
                       OR
                   BCITY-LEN > ZERO
                       OR
                   BSTATE-LEN > ZERO
                       OR
                   BCADDR1-LEN > ZERO
                       OR
                   BCADDR2-LEN > ZERO
                       OR
                   BCCITY-LEN > ZERO
                       OR
                   BCSTATE-LEN > ZERO
                   NEXT SENTENCE
               ELSE
                   GO TO 4900-EXIT
           ELSE
               GO TO 4900-EXIT.
           
      * EXEC CICS GETMAIN
      *        SET     (ADDRESS OF PENDING-MAILING-DATA)
      *        LENGTH  (ERPNDM-RECORD-LENGTH)
      *        INITIMG (GETMAIN-SPACE)
      *    END-EXEC.
      *    MOVE ',"IL                  $   #00008451' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038343531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERPNDM-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF PENDING-MAILING-DATA TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE 'PM'                   TO PM-RECORD-ID.
           MOVE 'ER'                   TO PM-SOURCE-SYSTEM.
           MOVE PI-PROCESSOR-ID        TO PM-LAST-MAINT-BY
                                          PM-RECORD-ADDED-BY.
           MOVE EIBTIME                TO PM-LAST-MAINT-HHMMSS.
           MOVE WS-CURRENT-BIN-DT      TO PM-LAST-MAINT-DT
                                          PM-RECORD-ADD-DT.
           MOVE ERPNDM-KEY             TO PM-CONTROL-PRIMARY.
           IF BLAST-NAME-LEN      GREATER ZEROS
               MOVE BLAST-NAME         TO PM-INSURED-LAST-NAME.
           IF B1ST-NAME-LEN       GREATER ZEROS
               MOVE B1ST-NAME          TO PM-INSURED-FIRST-NAME.
           IF BINIT-LEN           GREATER ZEROS
               MOVE BINIT              TO PM-INSURED-MIDDLE-INIT.
           IF BAGE-LEN            GREATER ZEROS
               MOVE WS-BAGE            TO PM-INSURED-ISSUE-AGE
           ELSE
               MOVE ZEROS              TO PM-INSURED-ISSUE-AGE.
              MOVE LOW-VALUES          TO PM-INSURED-BIRTH-DT.
              MOVE LOW-VALUES          TO PM-JOINT-BIRTH-DT
           IF BLAST-NAME-LEN      GREATER ZEROS
               MOVE BLAST-NAME         TO PM-INSURED-LAST-NAME.
           IF BINIT-LEN           GREATER ZEROS
               MOVE BINIT              TO PM-INSURED-MIDDLE-INIT.
           IF BADDRS1-LEN         GREATER ZERO
               MOVE BADDRS1            TO PM-ADDRESS-LINE-1.
           IF BADDRS2-LEN         GREATER ZERO
               MOVE BADDRS2            TO PM-ADDRESS-LINE-2.
           IF BCITY-LEN > 0
              MOVE BCITY               TO PM-CITY
           END-IF
           IF BSTATE-LEN > 0
              MOVE BSTATE              TO PM-STATE
           END-IF
           IF BZIPCDE-LEN GREATER ZEROS
               MOVE BZIPCDE            TO  WS-ZIP-CODE
           ELSE
               GO TO 4188-CRED-BENE.
           IF WS-CANADIAN-ZIP
               IF WS-ZIP-4 = SPACE  OR  '-'
                   MOVE WS-ZIP-CAN-2-POST1   TO PM-CAN-POST1
                   MOVE WS-ZIP-CAN-2-POST2   TO PM-CAN-POST2
               ELSE
                   MOVE WS-ZIP-CAN-1-POST1   TO PM-CAN-POST1
                   MOVE WS-ZIP-CAN-1-POST2   TO PM-CAN-POST2
           ELSE
               IF WS-ZIP-6 = SPACE  OR  '-'
                   MOVE WS-ZIP-AM-2-CODE     TO PM-ZIP-CODE
                   MOVE WS-ZIP-AM-2-PLUS4    TO PM-ZIP-PLUS4
               ELSE
                   MOVE WS-ZIP-AM-1-CODE     TO PM-ZIP-CODE
                   MOVE WS-ZIP-AM-1-PLUS4    TO PM-ZIP-PLUS4.
           .
       4188-CRED-BENE.
           IF BBENEFICIARY-LEN > ZEROS
              MOVE BBENEFICIARY        TO PM-CRED-BENE-NAME
           END-IF
           IF BCADDR1-LEN > ZEROS
              MOVE BCADDR1             TO PM-CRED-BENE-ADDR
           END-IF
           IF BCADDR2-LEN > ZEROS
              MOVE BCADDR2             TO PM-CRED-BENE-ADDR2
           END-IF
           IF BCCITY-LEN > ZEROS
              MOVE BCCITY              TO PM-CRED-BENE-CITY
           END-IF
           IF BCSTATE-LEN > ZEROS
              MOVE BCSTATE             TO PM-CRED-BENE-STATE
           END-IF
           IF BCZIPCD-LEN > ZEROS
              MOVE BCZIPCD             TO WS-ZIP-CODE
           ELSE
              GO TO 4188-CONTINUE
           END-IF
           IF WS-CANADIAN-ZIP
              IF WS-ZIP-4 = SPACE  OR  '-'
                 MOVE WS-ZIP-CAN-2-POST1   TO PM-CB-CAN-POST1
                 MOVE WS-ZIP-CAN-2-POST2   TO PM-CB-CAN-POST2
              ELSE
                 MOVE WS-ZIP-CAN-1-POST1   TO PM-CB-CAN-POST1
                 MOVE WS-ZIP-CAN-1-POST2   TO PM-CB-CAN-POST2
              END-IF
           ELSE
              IF WS-ZIP-6 = SPACE  OR  '-'
                 MOVE WS-ZIP-AM-2-CODE     TO PM-CB-ZIP-CODE
                 MOVE WS-ZIP-AM-2-PLUS4    TO PM-CB-ZIP-PLUS4
              ELSE
                 MOVE WS-ZIP-AM-1-CODE     TO PM-CB-ZIP-CODE
                 MOVE WS-ZIP-AM-1-PLUS4    TO PM-CB-ZIP-PLUS4
              END-IF
           END-IF
           .
       4188-CONTINUE.
      *    IF BPHONE-LEN          GREATER ZERO
      *        MOVE WS-BPHONE          TO PM-PHONE-NO
      *    ELSE
               MOVE ZEROS              TO PM-PHONE-NO.
           MOVE 'A'                    TO JP-RECORD-TYPE.
           MOVE PENDING-MAILING-DATA   TO JP-RECORD-AREA.
           MOVE ERPNDM-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.
           MOVE FILE-ID-ERPNDM         TO JP-FILE-ID.
           
      * EXEC CICS WRITE
      *        DATASET (FILE-ID-ERPNDM)
      *        FROM    (PENDING-MAILING-DATA)
      *        RIDFLD  (PM-CONTROL-PRIMARY)
      *    END-EXEC.
           MOVE LENGTH OF
            PENDING-MAILING-DATA
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00008557' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDM, 
                 PENDING-MAILING-DATA, 
                 DFHEIV11, 
                 PM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           PERFORM 8400-LOG-JOURNAL-RECORD.
           MOVE LOW-VALUES             TO MAP-B.
           GO TO 4900-EXIT.
       4200-DUPLICATE-ALT-INDEX.
           MOVE ER-2247                TO EMI-ERROR.
           MOVE -1                     TO BCERT-LEN.
           MOVE AL-UABON               TO BCERT-ATTRB.
           MOVE AL-UNBON               TO BEFFDT-ATTRB.
           MOVE 'Y'                    TO PI-ERROR-SW.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
           IF BPREM1-LEN > ZEROS
               SUBTRACT WS-BPREM1
                                       FROM PI-LF-ISS-ENTERED.
           IF BALT-PREM1-LEN > ZEROS
               SUBTRACT WS-BALT-PREM1
                                       FROM PI-LF-ISS-ENTERED.
           IF BPREM2-LEN > ZEROS
               SUBTRACT WS-BPREM2
                                       FROM PI-AH-ISS-ENTERED.
           SUBTRACT +1                 FROM PI-LAST-SEQ-NO-ADDED
                                            PI-SAV-BATCH-SEQ.
       4900-EXIT.
           EXIT.
       4905-read-elcrtt.
           
      * EXEC CICS READ
      *        SET     (ADDRESS OF certificate-trailers)
      *        DATASET ('ELCRTT')
      *        RIDFLD  (ELCRTT-KEY)
      *        resp    (ws-response)
      *    END-EXEC
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00008586' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303038353836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCRTT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF certificate-trailers TO
               DFHEIV20
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       4905-exit.
           exit.
       4910-read-elcrtt-update.
           
      * EXEC CICS READ
      *        SET     (ADDRESS OF certificate-trailers)
      *        DATASET ('ELCRTT')
      *        RIDFLD  (ELCRTT-KEY)
      *        UPDATE
      *        resp    (ws-response)
      *    END-EXEC
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&"S        EU         (  N#00008596' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303038353936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCRTT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF certificate-trailers TO
               DFHEIV20
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       4910-exit.
           exit.
       4920-rewrite-elcrtt.
           perform 4940-update-elcrtt  thru 4940-exit
           
      * exec cics rewrite
      *       dataset     ('ELCRTT')
      *       from        (certificate-trailers)
      *       resp        (ws-response)
      *    end-exec
           MOVE LENGTH OF
            certificate-trailers
             TO DFHEIV11
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&& L                  %  N#00008608' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'204E233030303038363038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 certificate-trailers, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       4920-exit.
           exit.
       4930-write-elcrtt.
           move 'CS'                   to certificate-trailers
           move elcrtt-key             to cs-control-primary
           move 'C'                    to cs-trailer-type
           perform 4940-update-elcrtt  thru 4940-exit
           
      * EXEC CICS WRITE
      *        DATASET ('ELCRTT')
      *        FROM    (CERTIFICATE-TRAILERS)
      *        RIDFLD  (CS-CONTROL-PRIMARY)
      *    END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-TRAILERS
             TO DFHEIV11
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00008621' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CERTIFICATE-TRAILERS, 
                 DFHEIV11, 
                 CS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       4930-exit.
           exit.
       4940-update-elcrtt.
           if byear-len <> zeros
              move byear               to cs-year
           end-if
           if bmake-len <> zeros
              move bmake               to cs-make
           end-if
           if bmodel-len <> zeros
              move bmodel              to cs-model
           end-if
           if bometer-len <> zeros
              MOVE Bometer-in          TO DEEDIT-FIELD
              PERFORM 8600-DEEDIT
              IF DEEDIT-FIELD-V0 NUMERIC
                 MOVE DEEDIT-FIELD-V0  TO cs-vehicle-odometer
              end-if
           end-if
           .
       4940-exit.
           exit.
       5000-BUILD-CANCEL-RECORD.
           MOVE +0                     TO WS-SUB2.
       5025-PROCESS-CANCEL.
           ADD +1                      TO WS-SUB2.
           IF PI-LAST-FUNC-DISPLAY
              IF WS-SUB2 GREATER +1
                 GO TO 5900-EXIT.
           IF WS-SUB2 GREATER +4
              GO TO 5900-EXIT.
           IF CCERT-LEN    (WS-SUB2) = ZEROS AND
              CEFFDT-LEN   (WS-SUB2) = ZEROS AND
              CCANDT1-LEN  (WS-SUB2) = ZEROS AND
              CCANDT2-LEN  (WS-SUB2) = ZEROS AND
              CMTHD1-LEN   (WS-SUB2) = ZEROS AND
              CMTHD2-LEN   (WS-SUB2) = ZEROS AND
              CREFUND1-LEN (WS-SUB2) = ZEROS AND
              CREFUND2-LEN (WS-SUB2) = ZEROS AND
              CLIVES-LEN   (WS-SUB2) = ZEROS AND
              CCHK-LEN     (WS-SUB2) = ZEROS
                 GO TO 5025-PROCESS-CANCEL.
           IF CSEQ (WS-SUB2) GREATER PI-LAST-SEQ-NO-ADDED
               GO TO 5100-ADD-CANCEL-RECORD.
      ******************************************************************
      *   THE DATA ENTRY SYSTEM ALLOWS BROWSING OF THE CURRENT BUS.    *
      *   FILE. THE DATA ENTRY SYS. DOES NOT HAVE A MAINT. FUNCTION.   *
      *   THE PROGRAM ASSUMES THAT IF A MATCH ON THE READ FOR UPDATE   *
      *   IS SUCCESSFUL, THE RECORD HAS PREVIOUSLY BEEN DISPLAYED      *
      *   THROUGH A BROWSE.  CHANGES ARE APPLIED AND THE PB-RECORD IS  *
      *   REWRITTEN, ELSE A NEW PB-RECORD IS ADDED.                    *
      ******************************************************************
           MOVE PI-COMPANY-CD          TO ERPNDB-COMP-CD.
           MOVE PI-SAV-ENTRY-BATCH     TO ERPNDB-ENTRY-BATCH.
           MOVE CSEQ (WS-SUB2)         TO ERPNDB-BATCH-SEQ.
           
      * EXEC CICS HANDLE CONDITION
      *        NOTFND (5100-ADD-CANCEL-RECORD)
      *    END-EXEC.
      *    MOVE '"$I                   ! , #00008682' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303038363832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * EXEC CICS READ
      *        SET     (ADDRESS OF PENDING-BUSINESS)
      *        DATASET (FILE-ID-ERPNDB)
      *        RIDFLD  (ERPNDB-KEY)
      *        UPDATE
      *    END-EXEC.
      *    MOVE '&"S        EU         (   #00008685' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE 'B'                    TO JP-RECORD-TYPE
           MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.
           MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.
           MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.
           PERFORM 8400-LOG-JOURNAL-RECORD.
           MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY.
           MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.
           MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT.
           IF CSFX-LEN  (WS-SUB2) GREATER ZEROS
              MOVE CSFX (WS-SUB2)      TO PB-CERT-SFX.
           IF CLAST-NAME-LEN (WS-SUB2) GREATER ZEROS
               MOVE CLAST-NAME (WS-SUB2)  TO PB-C-LAST-NAME.
           IF CREFUND1-LEN   (WS-SUB2) GREATER ZEROS
               IF WS-CREFUND1 (WS-SUB2) = WS-ALL-NINES OR
                  WS-CREFUND1 (WS-SUB2) GREATER WS-ALL-NINES
                  SUBTRACT PB-C-LF-CANCEL-AMT FROM PI-LF-CAN-ENTERED
                  MOVE ZEROS            TO PB-C-LF-CANCEL-AMT
                  MOVE '?'              TO PB-C-LF-CALC-REQ
               ELSE
                  SUBTRACT PB-C-LF-CANCEL-AMT FROM PI-LF-CAN-ENTERED
                  ADD WS-CREFUND1  (WS-SUB2) TO PI-LF-CAN-ENTERED
                  MOVE WS-CREFUND1 (WS-SUB2) TO PB-C-LF-CANCEL-AMT
                  MOVE SPACE                 TO PB-C-LF-CALC-REQ.
           IF CREFUND2-LEN   (WS-SUB2) GREATER ZEROS
               IF WS-CREFUND2 (WS-SUB2) = WS-ALL-NINES OR
                  WS-CREFUND2 (WS-SUB2) GREATER WS-ALL-NINES
                  SUBTRACT PB-C-AH-CANCEL-AMT FROM PI-AH-CAN-ENTERED
                  MOVE ZEROS            TO PB-C-AH-CANCEL-AMT
                  MOVE '?'              TO PB-C-AH-CALC-REQ
               ELSE
                  SUBTRACT PB-C-AH-CANCEL-AMT FROM PI-AH-CAN-ENTERED
                  ADD WS-CREFUND2  (WS-SUB2) TO PI-AH-CAN-ENTERED
                  MOVE WS-CREFUND2 (WS-SUB2) TO PB-C-AH-CANCEL-AMT
                  MOVE SPACE                 TO PB-C-AH-CALC-REQ.
      ******************************************************************
      *      IF CANCEL DATE = SPACES (LOW-VALUES) DELETE COVERAGE.     *
      ******************************************************************
           IF CCANDT1-LEN (WS-SUB2) GREATER ZEROS
              MOVE WS-CONVERTED-CANDT1 (WS-SUB2) TO PB-C-LF-CANCEL-DT
              IF   WS-CONVERTED-CANDT1 (WS-SUB2) = LOW-VALUES
                   SUBTRACT PB-C-LF-CANCEL-AMT FROM PI-LF-CAN-ENTERED
                   MOVE ZEROS          TO PB-C-LF-REF-CALC
                                          PB-C-LF-CANCEL-AMT.
           IF CCANDT2-LEN (WS-SUB2) GREATER ZEROS
              MOVE WS-CONVERTED-CANDT2 (WS-SUB2) TO PB-C-AH-CANCEL-DT
              IF   WS-CONVERTED-CANDT2 (WS-SUB2) = LOW-VALUES
                   SUBTRACT PB-C-AH-CANCEL-AMT FROM PI-AH-CAN-ENTERED
                   MOVE ZEROS          TO PB-C-AH-REF-CALC
                                          PB-C-AH-CANCEL-AMT.
           IF CMTHD1-LEN (WS-SUB2) GREATER THAN +0
              MOVE CMTHD1 (WS-SUB2)    TO PB-C-LF-REFUND-OVERRIDE.
           IF CMTHD2-LEN (WS-SUB2) GREATER THAN +0
              MOVE CMTHD2 (WS-SUB2)    TO PB-C-AH-REFUND-OVERRIDE.
           IF CLIVES-LEN  (WS-SUB2) GREATER ZEROS
              MOVE WS-CLIVES (WS-SUB2) TO PB-C-LIVES.
           IF CCANREA-LEN (WS-SUB2) > ZEROS
              MOVE WS-CAN-REA (WS-SUB2) TO PB-C-CANCEL-REASON
           END-IF
      *    IF CMICRO-NO-LEN (WS-SUB2)  GREATER  ZEROS
      *        MOVE WS-MICRO-NO (WS-SUB2)
      *                                TO  PB-C-MICROFILM-NO.
           IF CPAYEE-LEN  (WS-SUB2) GREATER ZEROS
              MOVE CPAYEE (WS-SUB2)    TO PB-C-PAYEE-CODE.
           IF CCHK-LEN    (WS-SUB2) GREATER ZEROS
              MOVE CCHK   (WS-SUB2)    TO PB-C-REFUND-SW.
           MOVE PI-LIFE-OVERRIDE-L1    TO PB-LIFE-OVERRIDE-L1.
           MOVE PI-AH-OVERRIDE-L1      TO PB-AH-OVERRIDE-L1.
           MOVE 'C'                    TO JP-RECORD-TYPE.
           MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.
           MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.
           MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.
           
      * EXEC CICS REWRITE
      *        DATASET (FILE-ID-ERPNDB)
      *        FROM    (PENDING-BUSINESS)
      *    END-EXEC.
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00008762' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303038373632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE ERPNDB-KEY             TO PI-SAV-ENDING-ERPNDB-KEY.
           PERFORM 8400-LOG-JOURNAL-RECORD.
           MOVE LOW-VALUES             TO DATA-AREA-C (WS-SUB2).
           IF EIBAID = DFHENTER
               MOVE PI-NEXT-DISPLAY-SEQ-NO TO CSEQ (WS-SUB2)
               ADD +1 TO PI-NEXT-DISPLAY-SEQ-NO
               MOVE AL-SABON               TO CSEQ-ATTRB (WS-SUB2).
           GO TO 5900-EXIT.
           EJECT
       5100-ADD-CANCEL-RECORD.
           
      * EXEC CICS GETMAIN
      *        SET     (ADDRESS OF PENDING-BUSINESS)
      *        LENGTH  (ERPNDB-RECORD-LENGTH)
      *        INITIMG (GETMAIN-SPACE)
      *    END-EXEC.
      *    MOVE ',"IL                  $   #00008776' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038373736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERPNDB-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE 'PB'                   TO PB-RECORD-ID.
           MOVE PI-COMPANY-CD          TO PB-COMPANY-CD
                                          PB-COMPANY-CD-A1.
           MOVE PI-COMPANY-ID          TO PB-COMPANY-ID.
           MOVE PI-SAV-ENTRY-BATCH     TO PB-ENTRY-BATCH.
           MOVE CSEQ (WS-SUB2)         TO PB-BATCH-SEQ-NO.
           IF CSEQ (WS-SUB2) GREATER PI-LAST-SEQ-NO-ADDED
              MOVE CSEQ (WS-SUB2)      TO PI-LAST-SEQ-NO-ADDED.
           MOVE PI-SAV-CARRIER         TO PB-CARRIER.
           MOVE PI-SAV-GROUPING        TO PB-GROUPING.
           MOVE PI-SAV-STATE           TO PB-STATE.
           MOVE PI-SAV-ACCOUNT         TO PB-ACCOUNT.
           MOVE '2'                    TO PB-RECORD-TYPE.
           MOVE CCERT (WS-SUB2)        TO PB-CERT-PRIME.
           MOVE WS-CONVERTED-CAN-EFF-DT (WS-SUB2)
                                       TO PB-CERT-EFF-DT.
      *    MOVE PI-SAV-REFERENCE       TO PB-C-REFERENCE.
           MOVE ZEROS                  TO PB-BATCH-CHG-SEQ-NO
                                          PB-ALT-CHG-SEQ-NO.
           MOVE +0                     TO PB-NO-OF-ERRORS.
           MOVE LOW-VALUES             TO PB-COMMON-ERRORS.
           MOVE ZEROS                  TO PB-C-LF-REF-CALC
                                          PB-C-AH-REF-CALC
                                          PB-C-LF-RFND-CLP
                                          PB-C-AH-RFND-CLP
                                          PB-CI-INSURED-AGE
                                          PB-CI-LF-TERM
                                          PB-CI-AH-TERM
                                          PB-CI-LF-BENEFIT-CD
                                          PB-CI-LF-BENEFIT-AMT
                                          PB-CI-LF-ALT-BENEFIT-AMT
                                          PB-CI-LF-PREMIUM-AMT
                                          PB-CI-LF-ALT-PREMIUM-AMT
                                          PB-CI-AH-BENEFIT-CD
                                          PB-CI-AH-BENEFIT-AMT
                                          PB-CI-AH-PREMIUM-AMT
                                          PB-CI-PAY-FREQUENCY
                                          PB-CI-LOAN-APR
                                          PB-CI-LOAN-TERM
                                          PB-CI-LIFE-COMMISSION
                                          PB-CI-AH-COMMISSION
                                          PB-CI-CURR-SEQ
                                          PB-CI-AH-CANCEL-AMT
                                          PB-CI-LF-CANCEL-AMT
                                          PB-CI-RATE-DEV-PCT-LF
                                          PB-CI-RATE-DEV-PCT-AH
                                          PB-CI-EXTENTION-DAYS
                                          PB-CI-TERM-IN-DAYS
                                          PB-CI-LIVES
                                          PB-CI-LF-CRIT-PER
                                          PB-CI-AH-CRIT-PER
                                          PB-C-LF-REM-TERM
                                          PB-C-AH-REM-TERM
                                          PB-CHG-COUNT
                                          PB-LF-BILLED-AMTS
                                          PB-AH-BILLED-AMTS
      *                                   PB-C-MICROFILM-NO
062017                                    PB-C-INT-ON-REFS
                                          PB-CALC-TOLERANCE.
           MOVE LOW-VALUES             TO PB-CI-AH-PAID-THRU-DT
                                          PB-CI-AH-SETTLEMENT-DT
                                          PB-CI-DEATH-DT
                                          PB-CI-LF-PRIOR-CANCEL-DT
                                          PB-CI-AH-PRIOR-CANCEL-DT
                                          PB-CI-ENTRY-DT
                                          PB-CI-LF-EXPIRE-DT
                                          PB-CI-AH-EXPIRE-DT
                                          PB-CI-LOAN-1ST-PMT-DT
                                          PB-C-LF-CANCEL-DT
                                          PB-C-AH-CANCEL-DT
                                          PB-CREDIT-ACCEPT-DT
                                          PB-BILLED-DT
                                          PB-ACCT-EFF-DT
                                          PB-ACCT-EXP-DT.
           IF PI-NB-MONTH-END-DT NOT = SPACES
              MOVE PI-NB-MONTH-END-DT  TO PB-CREDIT-SELECT-DT
             ELSE
              MOVE PI-CR-MONTH-END-DT  TO PB-CREDIT-SELECT-DT.
           MOVE 'X'                    TO PB-FATAL-FLAG.
           IF CSFX-LEN  (WS-SUB2) GREATER ZEROS
              MOVE CSFX (WS-SUB2)      TO PB-CERT-SFX.
           IF CLAST-NAME-LEN (WS-SUB2) GREATER ZEROS
               MOVE CLAST-NAME (WS-SUB2)  TO PB-C-LAST-NAME.
           IF CCANDT1-LEN (WS-SUB2) GREATER ZEROS
              MOVE WS-CONVERTED-CANDT1 (WS-SUB2) TO PB-C-LF-CANCEL-DT.
           IF CCANDT2-LEN (WS-SUB2) GREATER ZEROS
              MOVE WS-CONVERTED-CANDT2 (WS-SUB2) TO PB-C-AH-CANCEL-DT.
           IF CMTHD1-LEN (WS-SUB2) GREATER ZEROS
              MOVE CMTHD1 (WS-SUB2) TO PB-C-LF-REFUND-OVERRIDE.
           IF CMTHD2-LEN (WS-SUB2) GREATER ZEROS
              MOVE CMTHD2 (WS-SUB2) TO PB-C-AH-REFUND-OVERRIDE.
           IF CREFUND1-LEN   (WS-SUB2) GREATER ZEROS
               IF WS-CREFUND1 (WS-SUB2) = WS-ALL-NINES OR
                  WS-CREFUND1 (WS-SUB2) GREATER WS-ALL-NINES
                  MOVE ZEROS            TO PB-C-LF-CANCEL-AMT
                  MOVE '?'              TO PB-C-LF-CALC-REQ
               ELSE
                  ADD  WS-CREFUND1  (WS-SUB2) TO PI-LF-CAN-ENTERED
                  MOVE WS-CREFUND1  (WS-SUB2) TO PB-C-LF-CANCEL-AMT
           ELSE
               MOVE ZEROS            TO PB-C-LF-CANCEL-AMT.
           IF CREFUND2-LEN   (WS-SUB2) GREATER ZEROS
               IF WS-CREFUND2 (WS-SUB2) = WS-ALL-NINES OR
                  WS-CREFUND2 (WS-SUB2) GREATER WS-ALL-NINES
                  MOVE ZEROS            TO PB-C-AH-CANCEL-AMT
                  MOVE '?'              TO PB-C-AH-CALC-REQ
               ELSE
                  ADD  WS-CREFUND2  (WS-SUB2) TO PI-AH-CAN-ENTERED
                  MOVE WS-CREFUND2  (WS-SUB2) TO PB-C-AH-CANCEL-AMT
           ELSE
               MOVE ZEROS              TO PB-C-AH-CANCEL-AMT.
           IF CLIVES-LEN  (WS-SUB2) GREATER ZEROS
              MOVE WS-CLIVES (WS-SUB2) TO PB-C-LIVES
           ELSE
              MOVE ZEROS               TO PB-C-LIVES.
           IF CCANREA-LEN (WS-SUB2) > ZEROS
              MOVE WS-CAN-REA (WS-SUB2) TO PB-C-CANCEL-REASON
           END-IF
      *    IF CMICRO-NO-LEN (WS-SUB2)  GREATER  ZEROS
      *        MOVE WS-MICRO-NO (WS-SUB2)
      *                                TO  PB-C-MICROFILM-NO
      *    ELSE
      *        MOVE ZEROS              TO  PB-C-MICROFILM-NO.
           IF CPAYEE-LEN  (WS-SUB2) GREATER ZEROS
              MOVE CPAYEE (WS-SUB2)    TO PB-C-PAYEE-CODE.
           IF CCHK-LEN    (WS-SUB2) GREATER ZEROS
              MOVE CCHK   (WS-SUB2)    TO PB-C-REFUND-SW.
           IF PB-COMPANY-ID = 'LAP'  OR  'RMC'
               MOVE 'H'                TO PB-RECORD-BILL.
           MOVE PI-LIFE-OVERRIDE-L1    TO PB-LIFE-OVERRIDE-L1.
           MOVE PI-AH-OVERRIDE-L1      TO PB-AH-OVERRIDE-L1.
           MOVE PI-PROCESSOR-ID        TO PB-LAST-MAINT-BY
                                          PB-INPUT-BY.
           MOVE EIBTIME                TO PB-LAST-MAINT-HHMMSS.
           MOVE WS-CURRENT-BIN-DT      TO PB-LAST-MAINT-DT
                                          PB-INPUT-DT.
           MOVE PB-CONTROL-PRIMARY     TO PB-CONTROL-BY-ORIG-BATCH.
           MOVE PB-COMPANY-CD          TO PB-CSR-COMPANY-CD.
           MOVE PI-CSR-ID              TO PB-CSR-ID.
           MOVE PB-ENTRY-BATCH         TO PB-CSR-ENTRY-BATCH.
           MOVE PB-BATCH-SEQ-NO        TO PB-CSR-BATCH-SEQ-NO.
           MOVE PB-BATCH-CHG-SEQ-NO    TO PB-CSR-BATCH-CHG-SEQ-NO.
           MOVE 'A'                    TO JP-RECORD-TYPE.
           MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.
           MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.
           MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.
           MOVE PB-CONTROL-PRIMARY     TO PI-SAV-ENDING-ERPNDB-KEY.
           ADD +1                      TO PI-SAV-BATCH-SEQ.
           
      * EXEC CICS HANDLE CONDITION
      *        DUPREC (5200-DUPLICATE-ALT-INDEX)
      *    END-EXEC.
      *    MOVE '"$%                   ! - #00008929' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303038393239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * EXEC CICS WRITE
      *        DATASET (FILE-ID-ERPNDB)
      *        FROM    (PENDING-BUSINESS)
      *        RIDFLD  (PB-CONTROL-PRIMARY)
      *    END-EXEC.
           MOVE LENGTH OF
            PENDING-BUSINESS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00008932' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038393332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 PENDING-BUSINESS, 
                 DFHEIV11, 
                 PB-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           ADD +1                      TO PI-CAN-CNT-ENTERED.
           PERFORM 8400-LOG-JOURNAL-RECORD.
           MOVE LOW-VALUES             TO DATA-AREA-C (WS-SUB2).
           MOVE PI-NEXT-DISPLAY-SEQ-NO TO CSEQ        (WS-SUB2).
           MOVE AL-SABON               TO CSEQ-ATTRB  (WS-SUB2).
           ADD +1 TO                   PI-NEXT-DISPLAY-SEQ-NO.
           GO TO 5025-PROCESS-CANCEL.
       5200-DUPLICATE-ALT-INDEX.
           MOVE ER-2247                TO EMI-ERROR.
           MOVE -1                     TO CCERT-LEN    (WS-SUB2).
           MOVE AL-UABON               TO CCERT-ATTRB  (WS-SUB2).
           MOVE AL-UNBON               TO CEFFDT-ATTRB (WS-SUB2).
           MOVE 'Y'                    TO PI-ERROR-SW.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           IF CREFUND1-LEN (WS-SUB2) GREATER ZEROS
               SUBTRACT WS-CREFUND1 (WS-SUB2) FROM PI-LF-CAN-ENTERED.
           IF CREFUND2-LEN (WS-SUB2) GREATER ZEROS
               SUBTRACT WS-CREFUND2 (WS-SUB2) FROM PI-AH-CAN-ENTERED.
           GO TO 8200-SEND-DATAONLY.
       5900-EXIT.
           EXIT.
           EJECT
       6000-DELETE-PEND-BUS-RECORD.
           MOVE PI-COMPANY-CD          TO ERPNDB-COMP-CD.
           MOVE PI-SAV-ENTRY-BATCH     TO ERPNDB-ENTRY-BATCH.
           IF PI-MAP-NAME = VP630B
               MOVE BSEQ               TO ERPNDB-BATCH-SEQ
           ELSE
               MOVE CSEQ (1)           TO ERPNDB-BATCH-SEQ.
           
      * EXEC CICS HANDLE CONDITION
      *        NOTFND (6990-REC-NOTFND)
      *    END-EXEC.
      *    MOVE '"$I                   ! . #00008966' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303038393636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * EXEC CICS READ
      *        SET     (ADDRESS OF PENDING-BUSINESS)
      *        DATASET (FILE-ID-ERPNDB)
      *        RIDFLD  (ERPNDB-KEY)
      *        UPDATE
      *    END-EXEC.
      *    MOVE '&"S        EU         (   #00008969' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038393639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
      ******************************************************************
      *    PENDING BUSINESS RECORD CAN NOT BE DELETED THROUGH DATA     *
      *    ENTRY IF THE RECORD HAS BEEN EDITED.  IF THE RECORD HAS     *
      *    BEEN EDITED, THE CURRENT BUSINESS RECORD CAN ONLY BE DELETED*
      *    THROUGH REVIEW AND CORRECTION.                              *
      ******************************************************************
           IF  PB-ACCT-EFF-DT = LOW-VALUES NEXT SENTENCE
              ELSE
               GO TO 6880-DELETE-ERROR.
           IF PB-ISSUE
               SUBTRACT PB-I-LF-PREMIUM-AMT     FROM PI-LF-ISS-ENTERED
               SUBTRACT PB-I-LF-ALT-PREMIUM-AMT FROM PI-LF-ISS-ENTERED
               SUBTRACT PB-I-AH-PREMIUM-AMT     FROM PI-AH-ISS-ENTERED
               SUBTRACT +1 FROM PI-ISS-CNT-ENTERED
           ELSE
               SUBTRACT PB-C-LF-CANCEL-AMT FROM PI-LF-CAN-ENTERED
               SUBTRACT PB-C-AH-CANCEL-AMT FROM PI-AH-CAN-ENTERED
               SUBTRACT +1 FROM PI-CAN-CNT-ENTERED.
       6300-DELETE-PB-RECORD.
           MOVE 'D'                    TO JP-RECORD-TYPE.
           MOVE PENDING-BUSINESS       TO JP-RECORD-AREA.
           MOVE ERPNDB-JOURNAL-LENGTH  TO WS-JOURNAL-RECORD-LENGTH.
           MOVE FILE-ID-ERPNDB         TO JP-FILE-ID.
           
      * EXEC CICS DELETE
      *        DATASET (FILE-ID-ERPNDB)
      *    END-EXEC.
      *    MOVE '&(                    &   #00008998' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303038393938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           PERFORM 8400-LOG-JOURNAL-RECORD.
           MOVE 'Y'                    TO PI-UPDATE-SW.
           MOVE ER-0000                TO EMI-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           ADD +1             PI-LAST-SEQ-NO-ADDED
                           GIVING PI-NEXT-DISPLAY-SEQ-NO.
           IF PI-MAP-NAME = VP630B
               MOVE LOW-VALUES         TO MAP-B
               PERFORM 8550-SET-MAP-SEQ-NOS
           ELSE
               MOVE SPACE              TO PI-DISPLAY-SW
               MOVE LOW-VALUES         TO MAP-C
               PERFORM 8550-SET-MAP-SEQ-NOS
                       VARYING WS-SUB2 FROM +1 BY +1
                       UNTIL WS-SUB2 GREATER +5.
           GO TO 8100-SEND-INITIAL-MAP.
       6880-DELETE-ERROR.
           
      * EXEC CICS UNLOCK
      *         DATASET (FILE-ID-ERPNDB)
      *    END-EXEC.
      *    MOVE '&*                    #   #00009018' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303039303138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE ER-2901        TO EMI-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           IF PI-MAP-NAME = VP630B
               MOVE -1                 TO BPFENTRL
           ELSE
               MOVE -1                 TO CPFENTRL.
           GO TO 8200-SEND-DATAONLY.
       6990-REC-NOTFND.
           MOVE ER-2433                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           IF PI-MAP-NAME = VP630B
               MOVE -1                 TO BPFENTRL
           ELSE
               MOVE -1                 TO CPFENTRL.
           GO TO 8200-SEND-DATAONLY.
           EJECT
       7000-FORMAT-ISSUE-SCREEN.
           MOVE 'Y'                        TO PI-DISPLAY-SW.
           MOVE LOW-VALUES                 TO DATA-AREA-B.
           MOVE -1                         TO BPFENTRL.
           MOVE PB-BATCH-SEQ-NO            TO BSEQ.
           MOVE AL-SABON                   TO BSEQ-ATTRB.
           MOVE PB-CERT-PRIME              TO BCERT.
           MOVE AL-SANON                   TO BCERT-ATTRB.
           MOVE PB-CERT-SFX                TO BSFX.
           MOVE AL-SANOF                   TO BSFX-ATTRB.
           MOVE PB-CERT-EFF-DT             TO DC-BIN-DATE-1.
           MOVE SPACE                      TO DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
           MOVE DC-GREG-DATE-1-MDY         TO BEFFDT.
           MOVE AL-SANON                   TO BEFFDT-ATTRB.
           MOVE PI-LIFE-OVERRIDE-L2        TO BKIND1
           MOVE PI-AH-OVERRIDE-L2          TO BKIND2
           IF PB-I-INSURED-LAST-NAME GREATER SPACES
              MOVE PB-I-INSURED-LAST-NAME  TO BLAST-NAME.
           IF PB-I-INSURED-FIRST-NAME GREATER SPACES
              MOVE PB-I-INSURED-FIRST-NAME TO B1ST-NAME.
           IF PB-I-INSURED-MIDDLE-INIT GREATER SPACES
              MOVE PB-I-INSURED-MIDDLE-INIT TO BINIT.
           IF PB-I-AGE GREATER ZEROS
              MOVE PB-I-AGE                TO BAGE.
           IF PB-I-JOINT-AGE GREATER ZEROS
              MOVE PB-I-JOINT-AGE          TO BJNT-AGE.
           IF PB-I-INSURED-SEX GREATER SPACES
              MOVE PB-I-INSURED-SEX        TO BSEX.
           IF PB-I-LF-TERM GREATER ZEROS
              MOVE PB-I-LF-TERM            TO BTERM1O.
           IF PB-I-AH-TERM GREATER ZEROS
              MOVE PB-I-AH-TERM            TO BTERM2O.
           IF PB-I-LF-INPUT-CD GREATER SPACES
              MOVE PB-I-LF-INPUT-CD        TO BTYPE1.
           IF PB-I-LF-BENEFIT-AMT GREATER ZEROS
              MOVE PB-I-LF-BENEFIT-AMT     TO BBENE1O.
           IF PB-I-LF-ALT-BENEFIT-AMT GREATER ZEROS
              MOVE PB-I-LF-ALT-BENEFIT-AMT TO BALT-BEN1O.
           IF PB-I-LF-PREMIUM-AMT GREATER ZEROS
              MOVE PB-I-LF-PREMIUM-AMT     TO BPREM1O.
           IF PB-I-LF-ALT-PREMIUM-AMT GREATER ZEROS
              MOVE PB-I-LF-ALT-PREMIUM-AMT TO BALT-PREM1O.
           IF PB-I-AH-INPUT-CD GREATER SPACES
              MOVE PB-I-AH-INPUT-CD        TO BTYPE2.
           IF PB-I-AH-BENEFIT-AMT GREATER ZEROS
              MOVE PB-I-AH-BENEFIT-AMT     TO BBENE2O.
           IF PB-I-AH-PREMIUM-AMT GREATER ZEROS
              MOVE PB-I-AH-PREMIUM-AMT     TO BPREM2O.
           IF PB-I-AH-CRIT-PER GREATER ZEROS
              MOVE PB-I-AH-CRIT-PER           TO BCRIT-PERD2O.
           IF PB-I-LOAN-OFFICER GREATER SPACES
              MOVE PB-I-LOAN-OFFICER       TO BLN-OFFICER.
           IF PB-I-VIN > ZEROS
              MOVE PB-I-VIN            TO BVIN-NOI
           END-IF
           IF PB-I-JOINT-FIRST-NAME GREATER SPACES
              MOVE PB-I-JOINT-FIRST-NAME   TO BJNT-1ST-NAME.
           IF PB-I-JOINT-MIDDLE-INIT GREATER SPACES
              MOVE PB-I-JOINT-MIDDLE-INIT  TO BJNT-INIT.
           IF PB-I-JOINT-LAST-NAME GREATER SPACES
              MOVE PB-I-JOINT-LAST-NAME    TO BJNT-LST-NAME.
           IF PB-I-BENEFICIARY-NAME GREATER SPACES
              MOVE PB-I-BENEFICIARY-NAME   TO BBENEFICIARY.
           move pb-control-by-account  to elcrtt-key
           move 'C'                    to elcrtt-trlr-type
           perform 4905-read-elcrtt    thru 4905-exit
           if resp-normal
              if cs-year numeric
                 move cs-year          to byearo
              end-if
              move cs-make             to bmakeo
              move cs-model            to bmodelo
              if cs-vehicle-odometer numeric
                 move cs-vehicle-odometer
                                       to bometer-out
              end-if
           end-if
           MOVE PB-CONTROL-PRIMARY         TO ERPNDM-KEY.
           
      * EXEC CICS HANDLE CONDITION
      *        NOTFND (7090-EXIT)
      *    END-EXEC.
      *    MOVE '"$I                   ! / #00009116' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2F20233030303039313136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * EXEC CICS READ
      *        SET     (ADDRESS OF PENDING-MAILING-DATA)
      *        DATASET (FILE-ID-ERPNDM)
      *        RIDFLD  (ERPNDM-KEY)
      *        UPDATE
      *    END-EXEC.
      *    MOVE '&"S        EU         (   #00009119' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039313139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDM, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-MAILING-DATA TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           IF PM-ADDRESS-LINE-1 GREATER SPACES
              MOVE PM-ADDRESS-LINE-1       TO BADDRS1.
           IF PM-ADDRESS-LINE-2 GREATER SPACES
              MOVE PM-ADDRESS-LINE-2       TO BADDRS2.
           IF PM-CITY > SPACES
              MOVE PM-CITY                 TO BCITY
           END-IF
           IF PM-STATE > SPACES
              MOVE PM-STATE                TO BSTATE
           END-IF
           IF PM-ZIP            GREATER SPACES
               MOVE SPACES               TO WS-ZIP-CODE
               IF PM-CANADIAN-POST-CODE
                   MOVE PM-CAN-POST1     TO WS-ZIP-CAN-2-POST1
                   MOVE PM-CAN-POST2     TO WS-ZIP-CAN-2-POST2
                   MOVE WS-ZIP-CODE      TO BZIPCDE
               ELSE
                   MOVE PM-ZIP-CODE      TO WS-ZIP-AM-2-CODE
                   MOVE WS-ZIP-CODE      TO BZIPCDE
                   IF PM-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
                       MOVE '-'          TO WS-ZIP-AM-2-DASH
                       MOVE PM-ZIP-PLUS4 TO WS-ZIP-AM-2-PLUS4
                       MOVE WS-ZIP-CODE  TO BZIPCDE.
      *    IF PM-PHONE-NO NUMERIC
      *       IF PM-PHONE-NO GREATER ZEROS
      *          MOVE PM-PHONE-NO          TO  BPHONE-NO
      *          INSPECT BPHONE-NO CONVERTING ' ' TO '-'.
           IF PM-CRED-BENE-ADDR > SPACES
              MOVE PM-CRED-BENE-ADDR       TO BCADDR1
           END-IF
           IF PM-CRED-BENE-ADDR2 > SPACES
              MOVE PM-CRED-BENE-ADDR2     TO BCADDR2
           END-IF
           IF PM-CRED-BENE-CITY > SPACES
              MOVE PM-CRED-BENE-CITY       TO BCCITY
           END-IF
           IF PM-CRED-BENE-STATE > SPACES
              MOVE PM-CRED-BENE-STATE      TO BCSTATE
           END-IF
           IF PM-CRED-BENE-ZIP > SPACES
              MOVE SPACES               TO WS-ZIP-CODE
              IF PM-CB-CANADIAN-POST-CODE
                 MOVE PM-CB-CAN-POST1     TO WS-ZIP-CAN-2-POST1
                 MOVE PM-CB-CAN-POST2     TO WS-ZIP-CAN-2-POST2
                 MOVE WS-ZIP-CODE      TO BZIPCDE
              ELSE
                 MOVE PM-CB-ZIP-CODE      TO WS-ZIP-AM-2-CODE
                 MOVE WS-ZIP-CODE      TO BCZIPCD
                 IF PM-CB-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
                    MOVE '-'          TO WS-ZIP-AM-2-DASH
                    MOVE PM-CB-ZIP-PLUS4 TO WS-ZIP-AM-2-PLUS4
                    MOVE WS-ZIP-CODE  TO BCZIPCD
                 END-IF
              END-IF
           END-IF
           .
       7090-EXIT.
           EXIT.
           EJECT
       7100-FORMAT-CANCEL-SCREEN.
           MOVE 'Y'                    TO PI-DISPLAY-SW.
           MOVE LOW-VALUES             TO DATA-AREA-C (2)
                                          DATA-AREA-C (3).
           MOVE -1                     TO CPFENTRL.
           MOVE PB-BATCH-SEQ-NO        TO CSEQ        (1).
           MOVE AL-SABON               TO CSEQ-ATTRB  (1).
           MOVE PB-CERT-PRIME          TO CCERT       (1).
           MOVE AL-SANON               TO CCERT-ATTRB (1).
           MOVE PB-CERT-SFX            TO CSFX        (1).
           MOVE AL-SANON               TO CSFX-ATTRB  (1).
           MOVE PB-CERT-EFF-DT         TO DC-BIN-DATE-1.
           MOVE SPACE                  TO DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
           MOVE DC-GREG-DATE-1-MDY     TO CEFFDT       (1).
           MOVE AL-SANON               TO CEFFDT-ATTRB (1).
           MOVE PB-C-LAST-NAME         TO CLAST-NAME   (1).
           IF PB-C-LF-CANCEL-DT NOT = LOW-VALUES
              MOVE PB-C-LF-CANCEL-DT   TO DC-BIN-DATE-1
              MOVE SPACE               TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
              MOVE DC-GREG-DATE-1-MDY  TO CCANDT1 (1)
              MOVE AL-UANON            TO CCANDT1-ATTRB (1).
           IF PB-C-AH-CANCEL-DT NOT = LOW-VALUES
              MOVE PB-C-AH-CANCEL-DT   TO DC-BIN-DATE-1
              MOVE SPACE               TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
              MOVE DC-GREG-DATE-1-MDY  TO CCANDT2 (1)
              MOVE AL-UANON            TO CCANDT2-ATTRB (1).
           IF PB-C-LF-REFUND-OVERRIDE EQUAL SPACES
              NEXT SENTENCE
           ELSE
              MOVE PB-C-LF-REFUND-OVERRIDE
                                       TO CMTHD1 (1)
              MOVE AL-UANON            TO CMTHD1-ATTRB (1).
           IF PB-C-AH-REFUND-OVERRIDE EQUAL SPACES
              NEXT SENTENCE
           ELSE
              MOVE PB-C-AH-REFUND-OVERRIDE
                                       TO CMTHD2 (1)
              MOVE AL-UANON            TO CMTHD2-ATTRB (1).
           IF PB-C-LF-CANCEL-AMT NOT =  ZEROS
              MOVE PB-C-LF-CANCEL-AMT  TO CREFUND1O (1)
              MOVE AL-UNNON            TO CREFUND1-ATTRB (1).
           IF PB-C-AH-CANCEL-AMT NOT =  ZEROS
              MOVE PB-C-AH-CANCEL-AMT  TO CREFUND2O      (1)
              MOVE AL-UNNON            TO CREFUND2-ATTRB (1).
           IF PB-C-LIVES GREATER ZEROS
              MOVE PB-C-LIVES          TO CLIVESO      (1)
              MOVE AL-UNNON            TO CLIVES-ATTRB (1).
           IF PB-C-CANCEL-REASON GREATER SPACES
              MOVE PB-C-CANCEL-REASON  TO CCANREA (1)
              MOVE AL-UANON            TO CCANREA-ATTRB (1)
           END-IF
      *    IF PB-C-MICROFILM-NO  IS NUMERIC
      *        IF PB-C-MICROFILM-NO  NOT =  ZEROS
      *            MOVE PB-C-MICROFILM-NO
      *                                TO  CMICRO-NOO (1)
      *            MOVE AL-UNNON       TO  CMICRO-NO-ATTRB (1).
           IF PB-C-PAYEE-CODE GREATER SPACES
              MOVE PB-C-PAYEE-CODE     TO CPAYEE       (1)
              MOVE AL-UANON            TO CPAYEE-ATTRB (1).
           IF PB-C-REFUND-SW  GREATER SPACES
              MOVE PB-C-REFUND-SW      TO CCHK         (1)
              MOVE AL-UANON            TO CCHK-ATTRB   (1).
           IF PB-C-LAST-NAME GREATER SPACES
              MOVE PB-C-LAST-NAME      TO CLAST-NAME       (1)
              MOVE AL-UANON            TO CLAST-NAME-ATTRB (1).
           PERFORM 7180-PROTECT-FIELDS VARYING WS-SUB2 FROM +2 BY +1
                                       UNTIL WS-SUB2 GREATER +4.
           GO TO 7190-EXIT.
       7180-PROTECT-FIELDS.
           MOVE AL-SANOF               TO CCERT-ATTRB      (WS-SUB2)
                                          CSFX-ATTRB       (WS-SUB2)
                                          CEFFDT-ATTRB     (WS-SUB2)
                                          CLAST-NAME-ATTRB (WS-SUB2)
                                          CCANDT1-ATTRB    (WS-SUB2)
                                          CCANDT2-ATTRB    (WS-SUB2)
                                          CMTHD1-ATTRB     (WS-SUB2)
                                          CMTHD2-ATTRB     (WS-SUB2)
                                          CREFUND1-ATTRB   (WS-SUB2)
                                          CREFUND2-ATTRB   (WS-SUB2)
                                          CCHK-ATTRB       (WS-SUB2)
                                          CPAYEE-ATTRB     (WS-SUB2)
                                          CLIVES-ATTRB     (WS-SUB2)
                                          CCANREA-ATTRB    (WS-SUB2).
       7190-EXIT.
           EJECT
       8100-SEND-INITIAL-MAP.
           IF PI-MAP-NAME = VP630B
               NEXT SENTENCE
           ELSE
               GO TO 8110-SEND-INITIAL-CANCEL-MAP.
      *    MOVE PI-MEMBER-CAPTION        TO BCAPTNO.
           IF EIBAID NOT = DFHPF1   AND
              EIBAID NOT = DFHPF2   AND
              EIBAID NOT = DFHPF5   AND
              PI-MAINT-FUNC NOT = 'B'
                PERFORM 0600-PROTECT-FIELDS THRU 0600-EXIT.
           MOVE PI-SAV-ENTRY-BATCH     TO BBATCHO.
           MOVE PI-AM-NAME             TO BACCTNMO
           IF ((PI-AM-ADDR1 NOT = SPACES)
              OR (PI-AM-ADDR2 NOT = SPACES))
              IF BNFICRYO (1:5) = LOW-VALUES OR '_____'
                 MOVE PI-AM-NAME       TO BNFICRYO
                 MOVE AL-UANON         TO BBENEFICIARY-ATTRB
              END-IF
              IF BCADDR1 (1:5) = LOW-VALUES OR '_____'
                 MOVE PI-AM-ADDR2      TO BCADDR1
                 MOVE AL-UANON         TO BCADDR1-ATTRB
              END-IF
              IF BCADDR2 = SPACES OR LOW-VALUES
      *          MOVE PI-AM-ADDR2      TO BCADDR2
                 MOVE AL-UANON         TO BCADDR2-ATTRB
              END-IF
              IF BCCITY = SPACES OR LOW-VALUES
                 MOVE PI-AM-CITY       TO BCCITY
                 MOVE AL-UANON         TO BCCITY-ATTRB
              END-IF
              IF BCSTATE = SPACES OR LOW-VALUES
                 MOVE PI-AM-STATE      TO BCSTATE
                 MOVE AL-UANON         TO BCSTATE-ATTRB
              END-IF
              IF BCZIPCD = SPACES OR LOW-VALUES
                 MOVE PI-AM-ZIP        TO BCZIPCD
                 MOVE AL-UANON         TO BCZIPCD-ATTRB
              END-IF
           END-IF
           IF PI-AM-EDIT-LOAN-OFC = 'N'
              MOVE AL-SANOF            TO BLN-OFFICER-ATTRB
           END-IF
           IF PI-MAIL-YES
              continue
           ELSE
              MOVE AL-SANOF            TO BADDRS1-ATTRB
                                          BADDRS2-ATTRB
                                          BCITY-ATTRB
                                          BSTATE-ATTRB
                                          BZIPCDE-ATTRB
      *                                   BZIP4-ATTRB
      *                                   BPHONE-ATTRB
           END-IF
           IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
              MOVE AL-SADOF            TO balt-ben2-attrb
                                          balt-prem2-attrb
           END-IF
           IF PI-COMPANY-ID NOT = 'DCC' and 'CID' and 'VPP'
              MOVE AL-SADOF            TO BVINHD-ATTRB
                                          BVIN-ATTRB
           END-IF
      *    IF PI-LAST-FUNC-DISPLAY
      *          MOVE AL-SADOF            TO BCERTV-ATTRB
      *                                      BSFXV-ATTRB
      *                                      BEFFDTV-ATTRB
      *                                      BLAST-NAMEV-ATTRB
      *    ELSE
      *       IF PI-COMPANY-ID = ('PEM' OR 'CRI')
      *          MOVE AL-UANOF            TO BCERTV-ATTRB
      *                                      BSFXV-ATTRB
      *                                      BEFFDTV-ATTRB
      *                                      BLAST-NAMEV-ATTRB
      *       ELSE
      *          MOVE AL-SADOF            TO BCERTV-ATTRB
      *                                      BSFXV-ATTRB
      *                                      BEFFDTV-ATTRB
      *                                      BLAST-NAMEV-ATTRB.
           IF PI-LAST-FUNC-DISPLAY
              NEXT SENTENCE
           ELSE
              IF PI-COMPANY-ID = 'PEM' OR 'CRI'
                 MOVE AL-UADOF            TO BCERT-ATTRB
                                             BSFX-ATTRB
                                             BEFFDT-ATTRB
                                             BLAST-NAME-ATTRB
              ELSE
                 MOVE AL-UANOF            TO BCERT-ATTRB
                                             BEFFDT-ATTRB.
           IF PI-NB-MONTH-END-DT NOT = SPACES
              MOVE PI-NB-MONTH-END-DT  TO DC-BIN-DATE-1
             ELSE
              MOVE PI-CR-MONTH-END-DT  TO DC-BIN-DATE-1.
           MOVE SPACE                  TO DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
           MOVE DC-GREG-DATE-1-EDIT    TO BMOENDO.
           MOVE WS-CURRENT-DT          TO BDATEO.
           MOVE EIBTIME                TO TIME-IN.
           MOVE TIME-OUT               TO BTIMEO.
           MOVE PI-LIFE-OVERRIDE-L2    TO BKIND1
           MOVE AL-SABOF               TO BKIND1-ATTRB
           MOVE PI-AH-OVERRIDE-L2      TO BKIND2
           MOVE AL-SABOF               TO BKIND2-ATTRB
           IF PI-DATA-ERRORS
              MOVE SPACE               TO PI-ERROR-SW
           ELSE
              IF EIBAID = DFHPF1 OR DFHPF2
                 CONTINUE
              ELSE
                 MOVE -1               TO BEFFDTL
               END-IF
           END-IF
           MOVE EMI-MESSAGE-AREA (1)   TO BERMSG1O.
           MOVE EMI-MESSAGE-AREA (2)   TO BERMSG2O.
           
      * EXEC CICS SEND
      *        MAP      (PI-MAP-NAME)
      *        MAPSET   (MAPSET-VP6301S)
      *        FROM     (VP630BI)
      *        ERASE
      *        CURSOR
      *    END-EXEC.
           MOVE LENGTH OF
            VP630BI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00009386' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303039333836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 VP630BI, 
                 DFHEIV12, 
                 MAPSET-VP6301S, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           GO TO 9100-RETURN-TRAN.
           EJECT
       8110-SEND-INITIAL-CANCEL-MAP.
           IF EIBAID NOT = DFHPF5  AND
              EIBAID NOT = DFHPF1  AND
              EIBAID NOT = DFHPF2  AND
              PI-MAINT-FUNC NOT = 'B'
                PERFORM 0700-PROTECT-FIELDS THRU 0700-EXIT.
           MOVE PI-SAV-ENTRY-BATCH     TO CBATCHO.
           MOVE PI-AM-NAME             TO CACCTNMO.
           IF PI-NB-MONTH-END-DT NOT = SPACES
              MOVE PI-NB-MONTH-END-DT  TO DC-BIN-DATE-1
             ELSE
              MOVE PI-CR-MONTH-END-DT  TO DC-BIN-DATE-1.
           MOVE SPACE                  TO DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
           MOVE DC-GREG-DATE-1-EDIT    TO CMOENDO.
           MOVE WS-CURRENT-DT          TO CDATEO.
           MOVE EIBTIME                TO TIME-IN.
           MOVE TIME-OUT               TO CTIMEO.
           MOVE PI-LIFE-OVERRIDE-L2    TO CKIND1 (1)
                                          CKIND1 (2)
                                          CKIND1 (3)
                                          CKIND1 (4).
           MOVE PI-AH-OVERRIDE-L2      TO CKIND2 (1)
                                          CKIND2 (2)
                                          CKIND2 (3)
                                          CKIND2 (4).
           MOVE AL-SABOF               TO CKIND1-ATTRB (1)
                                          CKIND1-ATTRB (2)
                                          CKIND1-ATTRB (3)
                                          CKIND1-ATTRB (4)
                                          CKIND2-ATTRB (1)
                                          CKIND2-ATTRB (2)
                                          CKIND2-ATTRB (3)
                                          CKIND2-ATTRB (4).
           IF PI-DATA-ERRORS
               MOVE SPACE              TO PI-ERROR-SW
           ELSE
               IF EIBAID = DFHPF1 OR DFHPF2
                   NEXT SENTENCE
               ELSE
                   MOVE -1             TO CCERT1L.
           MOVE EMI-MESSAGE-AREA (1)   TO CERMSG1O.
           MOVE EMI-MESSAGE-AREA (2)   TO CERMSG2O.
           
      * EXEC CICS SEND
      *        MAP      (PI-MAP-NAME)
      *        MAPSET   (MAPSET-VP6301S)
      *        FROM     (VP630BI)
      *        ERASE
      *        CURSOR
      *    END-EXEC.
           MOVE LENGTH OF
            VP630BI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00009438' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 VP630BI, 
                 DFHEIV12, 
                 MAPSET-VP6301S, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           GO TO 9100-RETURN-TRAN.
           EJECT
       8200-SEND-DATAONLY.
           MOVE SPACE              TO PI-ERROR-SW.
           IF PI-MAP-NAME = VP630B
      *        MOVE PI-MEMBER-CAPTION      TO BCAPTNO
               MOVE WS-CURRENT-DT          TO BDATEO
               MOVE EIBTIME                TO TIME-IN
               MOVE TIME-OUT               TO BTIMEO
               MOVE EMI-MESSAGE-AREA (1)   TO BERMSG1O
               MOVE EMI-MESSAGE-AREA (2)   TO BERMSG2O
               
      * EXEC CICS SEND
      *            MAP      (PI-MAP-NAME)
      *            MAPSET   (MAPSET-VP6301S)
      *            FROM     (VP630BI)
      *            DATAONLY
      *            CURSOR
      *        END-EXEC
           MOVE LENGTH OF
            VP630BI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00009456' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 VP630BI, 
                 DFHEIV12, 
                 MAPSET-VP6301S, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           ELSE
               MOVE WS-CURRENT-DT          TO CDATEO
               MOVE EIBTIME                TO TIME-IN
               MOVE TIME-OUT               TO CTIMEO
               MOVE EMI-MESSAGE-AREA (1)   TO CERMSG1O
               MOVE EMI-MESSAGE-AREA (2)   TO CERMSG2O
               
      * EXEC CICS SEND
      *            MAP      (PI-MAP-NAME)
      *            MAPSET   (MAPSET-VP6301S)
      *            FROM     (VP630BI)
      *            DATAONLY
      *            CURSOR
      *        END-EXEC.
           MOVE LENGTH OF
            VP630BI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00009469' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 VP630BI, 
                 DFHEIV12, 
                 MAPSET-VP6301S, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           GO TO 9100-RETURN-TRAN.
           EJECT
       8300-SEND-TEXT.
           
      * EXEC CICS SEND TEXT
      *        FROM     (LOGOFF-TEXT)
      *        LENGTH   (LOGOFF-LENGTH)
      *        ERASE
      *        FREEKB
      *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00009479' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LOGOFF-TEXT, 
                 LOGOFF-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * EXEC CICS RETURN
      *    END-EXEC.
      *    MOVE '.(                    ''   #00009485' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       8350-SEND-WARNING.
           
      * EXEC CICS SEND TEXT
      *        FROM     (WARNING-TEXT)
      *        LENGTH   (WARNING-LENGTH)
      *        ERASE
      *        FREEKB
      *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00009488' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WARNING-TEXT, 
                 WARNING-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           GO TO 9100-RETURN-TRAN.
       8400-LOG-JOURNAL-RECORD.
           MOVE PI-PROCESSOR-ID        TO JP-USER-ID.
           MOVE THIS-PGM                TO JP-PROGRAM-ID.
      *    EXEC CICS JOURNAL
      *        JFILEID     (PI-JOURNAL-FILE-ID)
      *        JTYPEID     ('EL')
      *        FROM        (JOURNAL-RECORD)
      *        LENGTH      (WS-JOURNAL-RECORD-LENGTH)
      *        END-EXEC.
       8500-DATE-CONVERT.
           
      * EXEC CICS LINK
      *        PROGRAM  (LINK-ELDATCV)
      *        COMMAREA (DATE-CONVERSION-DATA)
      *        LENGTH   (DC-COMM-LENGTH)
      *    END-EXEC.
      *    MOVE '."C                   (   #00009505' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LINK-ELDATCV, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       8500-EXIT.
           EXIT.
           EJECT
       8550-SET-MAP-SEQ-NOS.
           IF PI-MAP-NAME = VP630B
               MOVE PI-NEXT-DISPLAY-SEQ-NO TO BSEQ
               MOVE AL-SABON               TO BSEQ-ATTRB
           ELSE
               MOVE PI-NEXT-DISPLAY-SEQ-NO TO CSEQ (WS-SUB2)
               MOVE AL-SABON               TO CSEQ-ATTRB (WS-SUB2).
           ADD +1  TO PI-NEXT-DISPLAY-SEQ-NO.
       8555-EXIT.
           EXIT.
       8600-DEEDIT.
           
      * EXEC CICS BIF DEEDIT
      *        FIELD   (DEEDIT-FIELD)
      *        LENGTH  (15)
      *    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00009524' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       8600-EXIT.
           EXIT.
       8800-UNAUTHORIZED-ACCESS.
           MOVE UNACCESS-MSG           TO LOGOFF-MSG.
           GO TO 8300-SEND-TEXT.
       9000-RETURN-CICS.
           
      * EXEC CICS RETURN
      *    END-EXEC.
      *    MOVE '.(                    ''   #00009534' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9100-RETURN-TRAN.
           MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
           IF  PI-MAP-NAME = VP630B
               MOVE '630B'             TO PI-CURRENT-SCREEN-NO.
           IF  PI-MAP-NAME = VP630C
               MOVE '630C'             TO PI-CURRENT-SCREEN-NO.
           
      * EXEC CICS RETURN
      *        TRANSID    (TRANS-EXA6)
      *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
      *        LENGTH     (WS-COMM-LENGTH)
      *    END-EXEC.
      *    MOVE '.(CT                  ''   #00009542' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-EXA6, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9300-XCTL.
           
      * EXEC CICS XCTL
      *        PROGRAM    (PGM-NAME)
      *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
      *        LENGTH     (WS-COMM-LENGTH)
      *    END-EXEC.
      *    MOVE '.$C                   %   #00009548' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9400-CLEAR.
           MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME
           GO TO 9300-XCTL.
       9500-PF12.
           MOVE XCTL-EL010             TO PGM-NAME.
           GO TO 9300-XCTL.
       9600-PGMID-ERROR.
           
      * EXEC CICS HANDLE CONDITION
      *        PGMIDERR    (8300-SEND-TEXT)
      *    END-EXEC.
      *    MOVE '"$L                   ! 0 #00009560' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3020233030303039353630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
           MOVE ' '                    TO PI-ENTRY-CD-1.
           MOVE XCTL-EL005            TO PGM-NAME.
           MOVE PGM-NAME               TO LOGOFF-PGM.
           MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
           GO TO 9300-XCTL.
       9900-ERROR-FORMAT.
           IF PI-MAP-NAME = VP630B
              MOVE 2                   TO EMI-NUMBER-OF-LINES
             ELSE
              MOVE 1                   TO EMI-NUMBER-OF-LINES.
           IF NOT EMI-ERRORS-COMPLETE
               MOVE LINK-EL001         TO PGM-NAME
               
      * EXEC CICS LINK
      *            PROGRAM    (PGM-NAME)
      *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
      *            LENGTH     (EMI-COMM-LENGTH)
      *        END-EXEC.
      *    MOVE '."C                   (   #00009576' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9900-EXIT.
           EXIT.
       9990-ABEND.
           MOVE LINK-EL004             TO PGM-NAME.
           MOVE DFHEIBLK               TO EMI-LINE1.
           
      * EXEC CICS LINK
      *        PROGRAM   (PGM-NAME)
      *        COMMAREA  (EMI-LINE1)
      *        LENGTH    (72)
      *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00009586' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           IF PI-MAP-NAME = VP630B
               MOVE -1 TO BPFENTRL
           ELSE
               MOVE -1 TO CPFENTRL.
           GO TO 8200-SEND-DATAONLY.
           
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'VP6301' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9995-SECURITY-VIOLATION.
      *    COPY ELCSCTP.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCSCTP                             *
00004 *                            VMOD=2.001                          *
00005 *                                                                *
00006 *   DESCRIPTION = C.I.C.S. COMMON SECURITY-MESSAGE LINK          *
00007 ******************************************************************
00008
00008
00009      MOVE EIBDATE          TO SM-JUL-DATE.
00010      MOVE EIBTRMID         TO SM-TERMID.
00011      MOVE THIS-PGM         TO SM-PGM.
00012      MOVE EIBTIME          TO TIME-IN.
00013      MOVE TIME-OUT         TO SM-TIME.
00014      MOVE PI-PROCESSOR-ID  TO SM-PROCESSOR-ID.
00015
00016      
      * EXEC CICS LINK
00017 *         PROGRAM  ('EL003')
00018 *         COMMAREA (SECURITY-MESSAGE)
00019 *         LENGTH   (80)
00020 *    END-EXEC.
           MOVE 'EL003' TO DFHEIV1
           MOVE 80
             TO DFHEIV11
      *    MOVE '."C                   (   #00009615' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039363135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 SECURITY-MESSAGE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00021
00022 ******************************************************************
00023
       9995-EXIT.
           EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'VP6301' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMID-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 1078-NO-RECORD,
                     1078-NO-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 2020-END-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 2100-BROWSE-BKWD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 3300-REC-NOT-FND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 3200-END-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 4100-ADD-ISSUE-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 4185-ADD-MAILING-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 4200-DUPLICATE-ALT-INDEX
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 4185-ADD-MAILING-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 5100-ADD-CANCEL-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 5200-DUPLICATE-ALT-INDEX
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 6990-REC-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 7090-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'VP6301' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
