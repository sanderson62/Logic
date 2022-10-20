       IDENTIFICATION DIVISION.
       PROGRAM-ID.                 ELVADS.
      *AUTHOR.  PABLO.
      *         OMAHA, NEBRASKA.
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CENTRAL STATES  *
      *            *   HEALTH AND LIFE CO. OF OMAHA                    *
      *            *                                                   *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF CSO         IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
      *            *                                                   *
      *            *****************************************************
      *REMARKS. TRANSACTION VADS - VA DISCLOSURE PRINT TEST
      *                        C H A N G E   L O G
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST  PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 103107    2007092500001   PEMA  NEW PROGRAM
      * 100111    2011022800001   AJRA  NAPERSOFT
033015* 033015  IR2015032400002   PEMA  CORRECT CODE WHEN APR IS ZERO
      ******************************************************************
      *
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
       77  FILLER  PIC  X(32) VALUE '********************************'.
       77  FILLER  PIC  X(32) VALUE '*   ELVADS WORKING STORAGE     *'.
       77  FILLER  PIC  X(32) VALUE '********* V/M 2.001 ************'.
       77  VA-RATE-SW                  PIC X  VALUE ' '.
           88  FOUND-VA-RATE               VALUE 'F'.
           88  NO-VA-RATE-FOUND            VALUE 'N'.
100111 77  SAVE-BIN-DATE               PIC XX      VALUE SPACES.
100111 77  E1                          PIC S999 COMP-3 VALUE +0.
100111 77  ARCH-STATUS                 PIC X.
100111     88  ARCH-OK                        VALUE "P".
100111     88  ARCH-FAIL                      VALUE "F".
       01  WS-CALC-SWITCHES.
           05  WS-TYPE-OF-CERT         PIC X.
               88  GROSS-CERT              VALUE 'G'.
               88  NET-CERT                VALUE 'N'.
               88  LEVEL-CERT              VALUE 'L'.
               88  AH-ONLY                 VALUE 'A'.
           05  WS-CALC-LF-GROSS        PIC X.
               88  CALC-LF-GROSS           VALUE 'Y'.
           05  WS-CALC-LF-NET          PIC X.
               88  CALC-LF-NET             VALUE 'Y'.
           05  WS-CALC-LF-LEVEL        PIC X.
               88  CALC-LF-LEVEL           VALUE 'Y'.
           05  WS-CALC-AH              PIC X.
               88  CALC-AH                 VALUE 'Y'.
       01  WS-CALC-AREA.
           05  WS-NET-PMT-1            PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-NET-PMT-2            PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-NET-PMT-3            PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-NET-PMT-4            PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-GROSS-PMT-1          PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-GROSS-PMT-2          PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-GROSS-PMT-3          PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-GROSS-PMT-4          PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-LEVEL-PMT-1          PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-LEVEL-PMT-2          PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-LEVEL-PMT-3          PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-LEVEL-PMT-4          PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-NET-PREM-1           PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-NET-PREM-2           PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-NET-PREM-3           PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-NET-PREM-4           PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-GROSS-PREM-1         PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-GROSS-PREM-2         PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-GROSS-PREM-3         PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-GROSS-PREM-4         PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-LEVEL-PREM-1         PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-LEVEL-PREM-2         PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-LEVEL-PREM-3         PIC S9(7)V99 COMP-3 VALUE +0.
           05  WS-LEVEL-PREM-4         PIC S9(7)V99 COMP-3 VALUE +0.
           05  ANNUAL-DISCOUNT         PIC S99V999  COMP-3 VALUE +0.
           05  CUR-PMT                 PIC S9(7)V99 COMP-3 VALUE +0.
           05  AMT-FINANCED            PIC S9(9)V99 COMP-3 VALUE +0.
           05  PRINC-AMT               PIC S9(9)V99 COMP-3 VALUE +0.
           05  LFPREM                  PIC S9(7)V99 COMP-3 VALUE +0.
           05  AHPREM                  PIC S9(7)V99 COMP-3 VALUE +0.
           05  LFAMT                   PIC S9(9)V99 COMP-3 VALUE +0.
           05  AHRATE                  PIC S99V9(5) COMP-3 VALUE +0.
           05  LF-RATE                 PIC S99V9(5) COMP-3 VALUE +0.
           05  OBRATE                  PIC S99V9(5) COMP-3 VALUE +0.
033015     05  I                       PIC SV9(10)   COMP-3 VALUE +0.
           05  Z                       PIC S999     COMP-3 VALUE +0.
           05  D                       PIC S9(5)    COMP-3 VALUE +0.
           05  T                       PIC S9(5)    COMP-3 VALUE +0.
           05  DISC                    PIC SV9(9)   COMP-3 VALUE +0.
           05  V-M                     PIC S99V9(9) COMP-3 VALUE +0.
           05  VM-N                    PIC S99V9(9) COMP-3 VALUE +0.
           05  VM                      PIC S99V9(9) COMP-3 VALUE +0.
           05  V                       PIC S99V9(9) COMP-3 VALUE +0.
           05  X                       PIC S99V9(9) COMP-3 VALUE +0.
           05  Y                       PIC S99V9(9) COMP-3 VALUE +0.
           05  N                       PIC S999     COMP-3 VALUE +0.
           05  M                       PIC S999     COMP-3 VALUE +0.
           05  R                       PIC S9(7)V99 COMP-3 VALUE +0.
           05  FILLER PIC X(12) VALUE ' SP IS NEXT '.
           05  SP                      PIC S99V9(9) COMP-3 VALUE +0.
033015     05  ANGLEN                  PIC S9(5)V9(13) COMP-3 VALUE +0.
           05  ANGLENY                 PIC S999V9(9) COMP-3 VALUE +0.
           05  ANGLEN-LEVEL            PIC S999V9(9) COMP-3 VALUE +0.
           05  ANGLENY-LEVEL           PIC S999V9(9) COMP-3 VALUE +0.
           05  LDPN                    PIC S9(5)V99  COMP-3 VALUE +0.
           05  AHPN                    PIC S9(5)V99  COMP-3 VALUE +0.
           05  FILLER PIC X(12) VALUE ' LF IS NEXT '.
           05  LF                      PIC S99V9(11) COMP-3 VALUE +0.
           05  PPY                     PIC S999V9(7) COMP-3 VALUE +0.
       01  WS-HOLD-RATE-EXP.
           05  WS-HOLD-RATE-EXP-AL     PIC X(11).
           05  WS-HOLD-RATE-EXP-DT REDEFINES
               WS-HOLD-RATE-EXP-AL     PIC 9(11).
       01  ERRATE-KEY.
           05  RATE-COMPANY-CD         PIC X       VALUE SPACE.
           05  RATE-STATE-CODE.
               10  RATE-ST-CODE        PIC XX      VALUE SPACES.
               10  RATE-ST-CLASS       PIC XX      VALUE SPACES.
               10  RATE-ST-DEV         PIC XXX     VALUE SPACES.
           05  RATE-L-AH-CODE.
               10  RATE-L-AH           PIC X       VALUE SPACE.
               10  RATE-LAH-NUM        PIC XX      VALUE ZEROS.
           05  RATE-LIMITS.
               10  RATE-HIGH-AGE       PIC 99      VALUE ZEROS.
               10  RATE-HIGH-AMT       PIC 9(6)    VALUE ZEROS.
               10  RATE-FUTURE         PIC XX      VALUE SPACES.
               10  RATE-SEX            PIC X       VALUE '9'.
           05  RATE-EXPIRY-DATE        PIC 9(11)   COMP-3.
       01  SAVE-ERRATE-KEY.
           05  SVRT-COMPANY-CD         PIC X       VALUE SPACE.
           05  SVRT-STATE-CODE.
               10  SVRT-ST-CODE        PIC XX      VALUE SPACES.
               10  SVRT-ST-CLASS       PIC XX      VALUE SPACES.
               10  SVRT-ST-DEV         PIC XXX     VALUE SPACES.
           05  SVRT-L-AH-CODE.
               10  SVRT-L-AH           PIC X       VALUE SPACE.
               10  SVRT-LAH-NUM        PIC XX      VALUE ZEROS.
           05  SVRT-LIMITS.
               10  SVRT-HIGH-AGE       PIC 99      VALUE ZEROS.
               10  SVRT-HIGH-AMT       PIC 9(6)    VALUE ZEROS.
               10  SVRT-FUTURE         PIC XX      VALUE SPACES.
               10  SVRT-SEX            PIC X       VALUE '9'.
           05  SVRT-EXPIRY-DATE        PIC 9(11)   COMP-3.
100111*** Z CONTROL LAYOUT MOVED TO COPYBOOK ELCZREC
100111*                         COPY ELCZREC.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            ELCZREC.                            *
      *                                                                *
      *   FILE DESCRIPTION = Z CONTROL RECORD LAYOUT                   *
      *                                                                *
      ******************************************************************
      *-----------------------------------------------------------------
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 122011    2011022800001  AJRA  NEW FILE
073112* 073112    2011022800001  AJRA  ADD ACCT SUMM, CSO SUMM
122712* 122712    2012101700002  AJRA  ADD REASON CODE REQUIRED FLAG
072313* 072313    2013062000003  AJRA  ADD IND FOR INSERTION BAR CODE
091913* 091913    2013090300001  AJRA  ADD SIGNATURE FLAG DEFAULT
      *-----------------------------------------------------------------
       01  W-Z-CONTROL-DATA.
           05  W-NUMBER-OF-COPIES      PIC  9.
           05  FILLER                  PIC  X.
           05  W-DAYS-TO-FOLLOW-UP     PIC  999.
           05  FILLER                  PIC  X.
           05  W-DAYS-TO-RESEND        PIC  999.
           05  FILLER                  PIC  X.
           05  W-FORM-TO-RESEND        PIC  X(4).
072313     05  W-ADD-BAR-CODE          PIC  X.
           05  W-PROMPT-LETTER         PIC  X.
072313     05  W-HAS-RETURN-ENV        PIC  X.
           05  W-ENCLOSURE-CD          PIC  XXX.
091913     05  W-SIG-FLAG-DEFAULT      PIC  X.
           05  W-AUTO-CLOSE-IND        PIC  X.
           05  FILLER                  PIC  X.
           05  W-LETTER-TO-BENE        PIC  X.
           05  FILLER                  PIC  X.
           05  W-LETTER-TO-ACCT        PIC  X.
           05  FILLER                  PIC  X.
           05  W-LETTER-TYPE           PIC  X.
           05  FILLER                  PIC  X.
           05  W-PRINT-CERTIFICATE     PIC  X.
           05  FILLER                  PIC  X.
           05  W-REFUND-REQUIRED       PIC  X.
           05  FILLER                  PIC  X.
           05  W-ONBASE-CODE           PIC  XX.
073112     05  FILLER                  PIC  X.
073112     05  W-ACCT-SUMM             PIC  X.
073112     05  FILLER                  PIC  X.
073112     05  W-CSO-SUMM              PIC  X.
122712     05  FILLER                  PIC  X.
122712     05  W-REASONS-REQUIRED      PIC  X.
122712     05  FILLER                  PIC  X(29).
100111
100111 01  WS-ELLETR-KEY.
100111     05  WS-ELLETR-COMPANY-CD    PIC X.
100111     05  WS-ELLETR-LETTER-ID     PIC X(12).
100111     05  WS-ELLETR-SEQ-NO        PIC S9(4) COMP VALUE +0.
100111 01  WS-ELCNTL-KEY.
100111     05  WS-ELCNTL-COMPANY-ID    PIC XXX.
100111     05  WS-ELCNTL-REC-TYPE      PIC X.
100111     05  WS-ELCNTL-GENL.
100111         10  WS-ELCNTL-STATE     PIC XX  VALUE SPACES.
100111         10  WS-ELCNTL-BEN-CD.
100111             15  F               PIC X.
100111             15  WS-ELCNTL-CARR  PIC X.
100111     05  WS-ELCNTL-SEQ-NO        PIC S9(4) COMP.
100111 01  WS-ELENCC-KEY.
100111     05  WS-ELENCC-COMPANY-CD    PIC X.
100111     05  WS-ELENCC-REC-TYPE      PIC X.
100111     05  WS-ELENCC-ENC-CODE      PIC X(5).
100111     05  F                       PIC X(9).
100111
       01  WORK-AREA.
           05  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.
           05  PGM-NAME                PIC X(8).
           05  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL                  VALUE +00.
               88  RESP-NOTFND                  VALUE +13.
               88  RESP-DUPREC                  VALUE +14.
               88  RESP-DUPKEY                  VALUE +15.
               88  RESP-NOTOPEN                 VALUE +19.
               88  RESP-ENDFILE                 VALUE +20.
100111     05  WS-COMPANY-CD           PIC X  VALUE LOW-VALUES.
100111     05  WS-ARCHIVE-NO           PIC S9(8)  COMP VALUE +0.
100111     05  WS-FOLLOW-UP-DT         PIC XX  VALUE LOW-VALUES.
100111     05  WS-RESEND-DT            PIC XX  VALUE LOW-VALUES.
100111     05  WS-AUTO-LAST-SCHED-DT   PIC XX  VALUE LOW-VALUES.
100111     05  WS-LAST-ACT-DT          PIC XX  VALUE LOW-VALUES.
100111     05  WS-ACTIVITY-DT          PIC XX  VALUE LOW-VALUES.
100111     05  WS-FORM                 PIC XXXX  VALUE SPACES.
100111     05  WS-MESSAGE              PIC X(50) VALUE SPACES.
100111
100111 01  VAPRINT-LINE.
100111     05  VA-LETTER               PIC X(06) VALUE SPACES.
100111     05  VA-PROC-ID              PIC X(04) VALUE SPACES.
100111     05  VA-PROC-NAME            PIC X(30) VALUE SPACES.
100111     05  VA-PROC-TITLE           PIC X(30) VALUE SPACES.
100111     05  VA-CSR-CODE             PIC X(4) VALUE SPACES.
100111     05  VA-CSR-NAME             PIC X(30) VALUE SPACES.
100111     05  VA-CSR-TITLE            PIC X(30) VALUE SPACES.
100111     05  VA-CARRIER              PIC X(01) VALUE SPACES.
100111     05  VA-GROUPING             PIC X(06) VALUE SPACES.
100111     05  VA-STATE                PIC X(02) VALUE SPACES.
100111     05  VA-ACCOUNT              PIC X(10) VALUE SPACES.
100111     05  VA-EFFDATE              PIC X(10) VALUE SPACES.
100111     05  VA-CERT-NO              PIC X(11) VALUE SPACES.
100111     05  VA-FIRSTNAME            PIC X(10) VALUE SPACES.
100111     05  VA-MIDINIT              PIC X(1) VALUE SPACES.
100111     05  VA-LASTNAME             PIC X(15) VALUE SPACES.
100111     05  VA-ADDR1                PIC X(30) VALUE SPACES.
100111     05  VA-ADDR2                PIC X(30) VALUE SPACES.
100111     05  VA-ADDR-CITY            PIC X(28) VALUE SPACES.
100111     05  VA-ADDR-ST              PIC X(2) VALUE SPACES.
100111     05  VA-ADDR-ZIP             PIC X(9) VALUE SPACES.
100111     05  VA-JNT-FIRSTNAME        PIC X(10) VALUE SPACES.
100111     05  VA-JNT-MIDINIT          PIC X(1) VALUE SPACES.
100111     05  VA-JNT-LASTNAME         PIC X(15) VALUE SPACES.
100111     05  VA-ARCH-NO              PIC 9(9) VALUE ZEROS.
100111     05  VA-ENC-LINE             PIC X(50) VALUE SPACES.
100111     05  VA-ATTACH               PIC X(50) VALUE SPACES.
100111     05  VA-STACK                PIC X(10) VALUE SPACES.
100111     05  VA-PRINT-NOW            PIC X(01) VALUE SPACES.
100111     05  VA-BORROWER             PIC X(30) VALUE SPACES.
           05  VA-BENE                 PIC X(30) VALUE SPACES.
           05  VA-ACCT-NAME            PIC X(30) VALUE SPACES.
           05  VA-SL                   PIC X VALUE SPACES.
           05  VA-JL                   PIC X VALUE SPACES.
           05  VA-SA                   PIC X VALUE SPACES.
           05  VA-JA                   PIC X VALUE SPACES.
           05  VA-GD                   PIC X VALUE SPACES.
           05  VA-NP                   PIC X VALUE SPACES.
           05  VA-7R                   PIC X VALUE SPACES.
           05  VA-LL                   PIC X VALUE SPACES.
           05  VA-TN                   PIC X VALUE SPACES.
           05  VA-14R                  PIC X VALUE SPACES.
           05  VA-14E                  PIC X VALUE SPACES.
           05  VA-30R                  PIC X VALUE SPACES.
           05  VA-30E                  PIC X VALUE SPACES.
           05  VA-CL-NET-AMT           PIC 9(7).99 BLANK WHEN ZERO.
           05  VA-CL-GRS-AMT           PIC 9(7).99 BLANK WHEN ZERO.
           05  VA-CL-LEV-AMT           PIC 9(7).99 BLANK WHEN ZERO.
           05  VA-CL-NET-PMT           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CL-GRS-PMT           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CL-LEV-PMT           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CL-NET-PRM           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CL-GRS-PRM           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CL-LEV-PRM           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CD-NET-AMT           PIC 9(7).99 BLANK WHEN ZERO.
           05  VA-CD-GRS-AMT           PIC 9(7).99 BLANK WHEN ZERO.
           05  VA-CD-LEV-AMT           PIC 9(7).99 BLANK WHEN ZERO.
           05  VA-CD-NET-PMT           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CD-GRS-PMT           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CD-LEV-PMT           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CD-NET-PRM           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CD-GRS-PRM           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CD-LEV-PRM           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CLD-NET-AMT          PIC 9(7).99 BLANK WHEN ZERO.
           05  VA-CLD-GRS-AMT          PIC 9(7).99 BLANK WHEN ZERO.
           05  VA-CLD-LEV-AMT          PIC 9(7).99 BLANK WHEN ZERO.
           05  VA-CLD-NET-PMT          PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CLD-GRS-PMT          PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CLD-LEV-PMT          PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CLD-NET-PRM          PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CLD-GRS-PRM          PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-CLD-LEV-PRM          PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-NO-NET-AMT           PIC 9(7).99 BLANK WHEN ZERO.
           05  VA-NO-GRS-AMT           PIC 9(7).99 BLANK WHEN ZERO.
           05  VA-NO-LEV-AMT           PIC 9(7).99 BLANK WHEN ZERO.
           05  VA-NO-NET-PMT           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-NO-GRS-PMT           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-NO-LEV-PMT           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-NO-NET-PRM           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-NO-GRS-PRM           PIC 9(5).99  BLANK WHEN ZERO.
           05  VA-NO-LEV-PRM           PIC 9(5).99  BLANK WHEN ZERO.
      *                                COPY ERCRATE.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCRATE                             *
00003 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00004 *                            VMOD=2.008                          *
00005 *                                                                *
00006 *   ONLINE CREDIT SYSTEM                                         *
00007 *                                                                *
00008 *   FILE DESCRIPTION = RATES MASTER FILE                         *
00009 *                                                                *
00010 *   FILE TYPE = VSAM,KSDS                                        *
00011 *   RECORD SIZE = 1765  RECFORM = FIXED                          *
00012 *                                                                *
00013 *   BASE CLUSTER NAME = ERRATE                   RKP=2,LEN=28    *
00014 *       ALTERNATE PATH = NONE                                    *
00015 *                                                                *
00016 *   LOG = NO                                                     *
00017 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00018 *                                                                *
00019 ******************************************************************
010716******************************************************************
010716*                   C H A N G E   L O G
010716*
010716* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
010716*-----------------------------------------------------------------
010716*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
010716* EFFECTIVE    NUMBER
010716*-----------------------------------------------------------------
010716* 010716    2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
010716******************************************************************
00020
00021  01  RATE-RECORD.
00022      12  RT-RECORD-ID                      PIC XX.
00023          88  VALID-RT-ID                      VALUE 'RT'.
00024
00025      12  RT-CONTROL-PRIMARY.
00026          16  RT-COMPANY-CD                 PIC X.
00027          16  RT-STATE-CODE.
00028              20  RT-ST-CODE                PIC XX.
00029              20  RT-ST-CLASS               PIC XX.
00030              20  RT-ST-DEV                 PIC XXX.
00031          16  RT-L-AH-CODE.
00032              20  RT-L-AH                   PIC X.
00033              20  RT-LAH-NUM                PIC XX.
00034          16  RT-LIMITS.
00035              20  RT-HIGH-AGE               PIC 99.
00036              20  RT-HIGH-AMT               PIC 9(6).
00037              20  RT-FUTURE                 PIC XX.
00038              20  RT-SEX                    PIC X.
00039          16  RT-EXPIRY-DATE                PIC 9(11)  COMP-3.
00043
00044      12  RT-MAINT-INFORMATION.
00045          16  RT-LAST-MAINT-DT              PIC XX.
00046          16  RT-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
00047          16  RT-LAST-MAINT-USER            PIC X(4).
00048          16  FILLER                        PIC X(10).
00049
00050      12  RT-STRUCTURE-COMMENT              PIC X(50).
00051      12  RT-RATE-COMMENT                   PIC X(50).
00052
00053      12  CSL-RESERVED                      PIC X(10).
00054      12  FILLER                            PIC X(12).
00055
00056      12  RT-MAX-AGE                        PIC 99.
00057
00058      12  RT-LIFE-LIMS-FLDS.
00059          16  RT-LIFE-MORT-CODE             PIC X(4).
00060          16  RT-LIFE-EXCEPTIONS   OCCURS 8 TIMES.
00061              20  RT-L-EX-AGE               PIC 99.
00062              20  RT-L-EX-TERM              PIC S999       COMP-3.
00063              20  RT-L-EX-FACE              PIC S9(7)      COMP-3.
00064          16  FILLER                        PIC X(20).
00065
00066      12  RT-AH-LIMS-FLDS   REDEFINES   RT-LIFE-LIMS-FLDS.
00067          16  RT-AH-EXCEPTIONS   OCCURS 8 TIMES.
00068              20  RT-AH-AGE                 PIC 99.
00069              20  RT-AH-TERM                PIC S999       COMP-3.
00070              20  RT-AH-BEN-M               PIC S9(5)      COMP-3.
00071              20  RT-AH-BEN-F               PIC S9(7)      COMP-3.
00072
00073      12  RT-LIFE-RATES.
00074          16  RT-L-RATE  OCCURS 360 TIMES   PIC S99V9(5)   COMP-3.
00075
00076      12  RT-AH-RATES   REDEFINES   RT-LIFE-RATES.
00077          16  RT-AH-RATE  OCCURS 360 TIMES  PIC S99V9(5)   COMP-3.
00078
00079      12  RT-DAILY-RATE                     PIC S99V9(5)   COMP-3.
00080
00081      12  RT-DISCOUNT-OPTION                PIC X.
00082          88  RT-DO-NOT-USE                     VALUE ' '.
00083          88  RT-USE-DISCOUNT-FACTOR            VALUE '1'.
00084          88  RT-USE-APR-AS-DISCOUNT            VALUE '2'.
00085
00086      12  RT-DISCOUNT-RATE                  PIC S99V9(5)   COMP-3.
00087      12  RT-DISCOUNT-OB-RATE               PIC S99V9(5)   COMP-3.
00088
00089      12  RT-COMPOSITE-OPTION               PIC X.
00090          88  RT-NO-COMPOSITE                   VALUE ' '.
00091          88  RT-USE-COMPOSITE-RATE             VALUE '1'.
00092
00093      12  RT-COMPOSITE-RATE                 PIC S99V9(5)   COMP-3.
00094
010716     12  RT-CANCEL-FEE                     PIC S9(3)V99   COMP-3.
00096      12  FILLER                            PIC X(13).
00097
00098      12  RT-TYPE-RATE                      PIC X.
00099          88  RT-IS-STND                        VALUE ' ' 'S'.
00100          88  RT-IS-OB                          VALUE 'O'.
00101
00102      12  RT-SRT-ALPHA                      PIC X.
00103
00104      12  RT-CONTROL-2.
00105          16  RTC-1                         PIC X(7).
00106          16  RTC-3                         PIC X(11).
00107          16  RTC-4                         PIC 9(11) COMP-3.
00108          16  RTC-2                         PIC X(3).
00109 ******************************************************************
100111*                                COPY ELCVADS.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCVADS.                            *
00006 *                                                                *
00007 *   FILE DESCRIPTION = VIRGINIA DISLCOSURE FIELDS PASSED TO      *
00008 *   ELVADS                  LENGTH = 350                         *
00009 ******************************************************************
100111******************************************************************
100111*                   C H A N G E   L O G
100111*
100111* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
100111*-----------------------------------------------------------------
100111*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
100111* EFFECTIVE    NUMBER
100111*-----------------------------------------------------------------
100111* 100111    2011022800001  AJRA  NAPERSOFT
100111******************************************************************
00030
00040  01  VIRGINIA-DISCLOSURE.
00050      12  VD-COMPANY-CD              PIC X.
00060      12  VD-CARRIER                 PIC X.
00070      12  VD-GROUPING.
00080          20  VD-GROUPING-PREFIX     PIC XXX.
00090          20  VD-GROUPING-PRIME      PIC XXX.
00100      12  VD-STATE                   PIC XX.
00110      12  VD-ACCOUNT.
00120          20  VD-ACCOUNT-PREFIX      PIC X(4).
00130          20  VD-ACCOUNT-PRIME       PIC X(6).
00140      12  VD-CERT-EFF-DT             PIC XX.
00150      12  VD-CERT-NO.
00160          20  VD-CERT-PRIME          PIC X(10).
00170          20  VD-CERT-SFX            PIC X.
00180      12  VD-ENTRY-BATCH             PIC X(6).
00190      12  VD-CSR-ID                  PIC X(4).
00200      12  VD-NAME.
00210          20  VD-INSURED-LAST-NAME   PIC X(15).
00220          20  VD-INSURED-FIRST-NAME.
00230              24  VD-INSURED-1ST-INIT PIC X.
00240              24  FILLER             PIC X(9).
00250          20  VD-INSURED-MIDDLE-INIT PIC X.
00260      12  VD-JOINT-INSURED.
00270          20 VD-JOINT-LAST-NAME      PIC X(15).
00280          20 VD-JOINT-FIRST-NAME.
00290             24  VD-JOINT-FIRST-INIT PIC X.
00300             24  FILLER              PIC X(9).
00310          20 VD-JOINT-MIDDLE-INIT    PIC X.
00320      12  VD-INSURED-ADDRESS-1       PIC X(30).
00330      12  VD-INSURED-ADDRESS-2       PIC X(30).
00340      12  VD-INSURED-CITY-STATE.
00350          20  VD-INSURED-CITY        PIC X(28).
00360          20  VD-INSURED-STATE       PIC XX.
00370      12  VD-INSURED-ZIP-CODE.
00380          20  VD-INSURED-ZIP-PRIME.
00390              24  VD-INSURED-ZIP-1   PIC X.
00400                  88  VD-CANADIAN-POST-CODE
00410                                        VALUE 'A' THRU 'Z'.
00420              24  FILLER             PIC X(4).
00430          20  VD-INSURED-ZIP-PLUS4   PIC X(4).
00440      12  VD-BENEFICIARY-NAME        PIC X(30).
00450      12  VD-ACCOUNT-NAME            PIC X(30).
00480      12  VD-LOAN-TERM               PIC S999   COMP-3.
00650      12  VD-LOAN-APR                PIC 9(3)V9(4)   COMP-3.
00660      12  VD-1ST-PMT-DT              PIC XX.
00490      12  VD-LIFE-BENEFIT-CD         PIC XX.
00500          88  VD-VALID-LIFE             VALUE '01' THRU '89'.
00510          88  VD-INVALID-LIFE           VALUE '  ' '00'
00520                                              '90' THRU '99'.
00530      12  VD-LF-BENEFIT-CD   REDEFINES VD-LIFE-BENEFIT-CD
00540                                     PIC XX.
00550      12  VD-LF-BENEFIT-AMT          PIC S9(9)V99   COMP-3.
00560      12  VD-LF-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
00570      12  VD-LF-RATE                 PIC S99V9(5)   COMP-3.
00460      12  VD-LF-TERM                 PIC S999   COMP-3.
00580      12  VD-AH-BENEFIT-CD           PIC XX.
00590          88  VD-VALID-AH               VALUE '01' THRU '89'.
00600          88  VD-INVALID-AH             VALUE '  ' '00'
00610                                              '90' THRU '99'.
00620      12  VD-AH-BENEFIT-AMT          PIC S9(7)V99   COMP-3.
00630      12  VD-AH-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
00640      12  VD-AH-RATE                 PIC S99V9(5)   COMP-3.
00470      12  VD-AH-TERM                 PIC S999   COMP-3.
00670      12  VD-LETTER-ID               PIC X(4).
00680      12  VD-PROC-ID                 PIC X(4).
00690      12  VD-COMP-ID                 PIC X(3).
00700      12  VD-CURRENT-DATE            PIC XX.
00710      12  VD-CURRENT-TIME            PIC S9(6) COMP-3.
00720      12  VD-ARCHIVE-NO              PIC S9(08) COMP VALUE +0.
01000      12  FILLER                     PIC X(32).
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
100111*                                COPY ELCTEXT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCTEXT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.008                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = TEXT FILES FOR HELP DISPLAY,              *
00008 *                                     FORM LETTERS,              *
00009 *                                     CERT FORM DISPLAY.
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 100   RECFORM = FIXED                          *
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ELLETR (LETTERS)   RKP=2,LEN=15          *
00015 *       ALTERNATE INDEX = NONE                                   *
00016 *                                                                *
00017 *   BASE CLUSTER NAME = ELFORM (FORMS)     RKP=2,LEN=15          *
00018 *       ALTERNATE INDEX = NONE                                   *
00019 *                                                                *
00020 *   BASE CLUSTER NAME = ELHELP (HELP)      RKP=2,LEN=15          *
00021 *       ALTERNATE INDEX = NONE                                   *
00022 *                                                                *
00023 *   LOG = NO                                                     *
00024 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00025 ******************************************************************
00026  01  TEXT-FILES.
00027      12  TEXT-FILE-ID                PIC XX.
00028          88  FORMS-FILE-TEXT            VALUE 'TF'.
00029          88  LETTER-FILE-TEXT           VALUE 'TL'.
00030          88  HELP-FILE-TEXT             VALUE 'TH'.
00031
00032      12  TX-CONTROL-PRIMARY.
00033          16  TX-COMPANY-CD           PIC X.
00034              88  TX-SYSTEM-WIDE-FILE    VALUE LOW-VALUE.
00035          16  TX-ACCESS-CD-GENL       PIC X(12).
00036
00037          16  TX-LETTER-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
00038              20  TX-LETTER-NO        PIC X(4).
00039              20  FILLER              PIC X(8).
00040
00041          16  TX-FORM-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
00042              20  TX-FORM-NO          PIC X(12).
00043
00044          16  TX-HELP-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
00045              20  TX-HELP-TYPE        PIC X.
00046                  88  HELP-FOR-GENERAL   VALUE ' '.
00047                  88  HELP-BY-SCREEN     VALUE 'S'.
00048                  88  HELP-BY-ERROR      VALUE 'E'.
00049              20  TX-SCREEN-OR-ERROR  PIC X(4).
00050                  88  GENERAL-INFO-HELP  VALUE '0000'.
00051              20  TX-HELP-FOR-COMPANY  PIC XXX.
00052                  88  NOT-COMPANY-SPECIFIC VALUE '   '.
00053              20  FILLER              PIC X(4).
00054
00055          16  TX-LINE-SEQUENCE        PIC S9(4)     COMP.
00056
00057      12  TX-PROCESS-CONTROL          PIC XX.
00058          88  LETTER-LINE-SKIPS          VALUE '01' THRU '99'.
00059
00060      12  TX-TEXT-LINE                PIC X(70).
00061
00062      12  TX-FORM-SQUEEZE-CONTROL     PIC X.
00063          88  TX-FORM-SQUEEZE-ON         VALUE 'Y'.
00064          88  TX-FORM-SQUEEZE-OFF        VALUE SPACES.
00065          88  TX-VALID-FORM-SQUEEZE-VALUE
00066                                         VALUE 'Y' ' '.
00067
00068      12  TX-LINE-SQUEEZE-CONTROL     PIC X.
00069          88  TX-ADJUST-TO-LINE-LENGTH   VALUE 'A'.
00070          88  TX-CONTINUE-PARAGRAPH      VALUE 'C'.
00071          88  TX-DO-NOT-ADJUST           VALUE 'N'.
00072          88  TX-FORM-CONTROL-LINE       VALUE 'K'.
00073          88  TX-NEW-PARAGRAPH           VALUE 'P'.
00074          88  TX-NO-SPECIAL-INSTRUCTION  VALUE ' '.
00075          88  TX-VALID-LINE-SQ-VALUE     VALUE 'A' 'C' 'P'
00076                                               'K' 'N' ' '
00077                                               'Z'.
00078
00079      12  TX-ARCHIVE-SW               PIC X.
00080          88  TX-ARCHIVE-THIS-LETTER     VALUE 'Y'.
00081          88  TX-DO-NOT-ARCHIVE          VALUE SPACES.
00082          88  TX-VALID-ARCHIVE-VALUE     VALUE 'Y' ' '.
00083
00084      12  TX-LAST-MAINTENANCED-BY     PIC X(4).
00085      12  TX-LAST-MAINTENANCED-DT     PIC X(2).
00086
00087      12  TX-BSR-CODE                 PIC X.
00088          88  TX-BSR-LETTER              VALUE 'B'.
00089          88  TX-NON-BSR-LETTER          VALUE ' '.
00090
00091      12  FILLER                      PIC X.
00092 *****************************************************************
100111*                                COPY ELCCNTL.
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
012913         16  FILLER                         PIC X(181).
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
100111*                                COPY ERCARCH.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ERCARCH.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = LETTERS SENT TO ARCHIVE FILE              *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 250  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ERARCH                        RKP=2,LEN=5     *
00013 *     ALTERNATE PATH1 = ERARCH2 (CERT/RESP)      RKP=07,LEN=35   *
00014 *     ALTERNATE PATH2 = ERARCH3 (FORM NUMBER)    RKP=44,LEN=28   *
00015 *     ALTERNATE PATH3 = ERARCH4 (PROCCESSOR ID)  RKP=73,LEN=28   *
00016 *     ALTERNATE PATH4 = ERARCH5 (ACCOUNT KEY)    RKP=100,LEN=24  *
00017 *     ALTERNATE PATH5 = ERARCH6 (BTCH/CHK KEY)   RKP=124,LEN=11  *
00018 *                                                                *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00020 ******************************************************************
031011*                   C H A N G E   L O G
031011*
031011* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
031011*-----------------------------------------------------------------
031011*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
031011* EFFECTIVE    NUMBER
031011*-----------------------------------------------------------------
031011* 031011  CR2007070900001  PEMA  ADD FOLLOW-UP LETTER PROCESSING
070711* 070711  CR2011022800001  AJRA  NAPERSOFT CHANGES
110612* 110612    2012101700002  AJRA  ADD NEW FIELDS
031011******************************************************************
00021  01  LETTER-ARCHIVE.
00022      12  LA-RECORD-ID                PIC  X(02).
00023          88  LA-VALID-ID                VALUE 'LA'.
00024
00025      12  LA-CONTROL-PRIMARY.
00026          16  LA-COMPANY-CD           PIC  X(01).
00027          16  LA-ARCHIVE-NO           PIC S9(08)    COMP.
00028
00029      12  LA-CONTROL-BY-CERT-RESP.
00030          16  LA-COMPANY-CD-A2        PIC  X(01).
00031          16  LA-CERT-NO-A2.
00032              20  LA-CERT-PRIME-A2    PIC  X(10).
00033              20  LA-CERT-SUFFIX-A2   PIC  X(01).
00034          16  LA-RSP-PERSON-A2 REDEFINES LA-CERT-NO-A2.
00035              20  LA-RESP-PERSON-A2   PIC  X(10).
00036              20  LA-TYPE-A2          PIC  X(01).
00037          16  LA-CARRIER-A2           PIC  X(01).
00038          16  LA-GROUPING-A2          PIC  X(06).
00039          16  LA-STATE-A2             PIC  X(02).
00040          16  LA-ACCOUNT-A2           PIC  X(10).
00041          16  LA-EFFECT-DATE-A2       PIC  X(02).
00042          16  LA-ARCHIVE-NO-A2        PIC S9(08)    COMP.
00043
00044      12  LA-CONTROL-BY-FORM.
00045          16  LA-COMPANY-CD-A3        PIC  X(01).
00046          16  LA-FORM-A3              PIC  X(04).
00047          16  LA-CARRIER-A3           PIC  X(01).
00048          16  LA-GROUPING-A3          PIC  X(06).
00049          16  LA-STATE-A3             PIC  X(02).
00050          16  LA-ACCOUNT-A3           PIC  X(10).
00051          16  LA-ARCHIVE-NO-A3        PIC S9(08)    COMP.
00052
00053      12  LA-CONTROL-BY-PROCESSOR.
00054          16  LA-COMPANY-CD-A4        PIC  X(01).
00055          16  LA-PROCESSOR-CD         PIC  X(04).
00056          16  LA-CARRIER-A4           PIC  X(01).
00057          16  LA-GROUPING-A4          PIC  X(06).
00058          16  LA-STATE-A4             PIC  X(02).
00059          16  LA-ACCOUNT-A4           PIC  X(10).
00060          16  LA-ARCHIVE-NO-A4        PIC S9(08)    COMP.
00061
00062      12  LA-CONTROL-BY-KEY-FIELDS.
00063          16  LA-COMPANY-CD-A5        PIC  X(01).
00064          16  LA-CARRIER-A5           PIC  X(01).
00065          16  LA-GROUPING-A5          PIC  X(06).
00066          16  LA-STATE-A5             PIC  X(02).
00067          16  LA-ACCOUNT-A5           PIC  X(10).
00068          16  LA-ARCHIVE-NO-A5        PIC S9(08)    COMP.
00069
00070      12  LA-CONTROL-BY-GROUP-CODE.
00071          16  LA-COMPANY-CD-A6        PIC  X(01).
00072          16  LA-ENTRY-A6.
00073              20  LA-FILLER           PIC  X(02).
00074              20  LA-QUE-CONTROL-A6   PIC S9(08)    COMP.
00075          16  LA-ARCHIVE-NO-A6        PIC S9(08)    COMP.
00076
00077      12  FILLER                      PIC  X(09).
00078
00079      12  LA-HEADER-RECORD.
00080          16  LA-NUMBER-LABEL-LINES   PIC S9(04)    COMP.
00081          16  LA-CREATION-DATE        PIC  X(02).
00082          16  LA-FOLLOW-UP-DATE       PIC  X(02).
070711         16  LA-FINAL-ACT-DATE       REDEFINES
070711               LA-FOLLOW-UP-DATE     PIC  X(02).
00083          16  LA-INITIAL-PRINT-DATE   PIC  X(02).
00084          16  LA-NO-OF-COPIES         PIC S9(01).
00085          16  LA-NO-OF-TEXT-RECORDS   PIC S9(04)    COMP.
00086          16  LA-REPLY-DATE           PIC  X(02).
00087          16  LA-RESEND-DATES.
00090              20  LA-RESEND-DATE      PIC  X(02).
00091              20  LA-SENT-DATE        PIC  X(02).
                   20  FILLER              PIC  X(08).
00099          16  LA-SOURCE-INFORMATION.
00100              20  LA-DATA-SOURCE      PIC  X(01).
00101              20  LA-ADDR-SOURCE      PIC  X(01).
00102          16  LA-STATUS               PIC  X(01).
00103              88  LA-STATUS-ACTIVE         VALUE 'A'.
00104              88  LA-STATUS-COMPLETED      VALUE 'C'.
00105              88  LA-STATUS-ON-HOLD        VALUE 'H'.
00106              88  LA-STATUS-TO-BE-PURGED   VALUE 'X'.
00107              88  LA-STATUS-PURGED         VALUE 'P'.
00108              88  LA-STATUS-VOIDED         VALUE 'V'.
00109          16  LA-LAST-RESENT-PRINT-DATE
00110                                      PIC  X(02).
00111          16  LA-PRINT-RESTRICTION    PIC  X(01).
00112              88  LA-PRINT-ONLY-WHEN-CNTL-GIVEN
00113                                           VALUE 'C'.
00114              88  LA-PRINT-ONLY-WHEN-FORM-GIVEN
00115                                           VALUE 'F'.
00116              88  LA-PRINT-ONLY-WHEN-PROC-GIVEN
00117                                           VALUE 'P'.
00118          16  LA-PURGED-DATE          PIC  X(02).
00119          16  LA-VOIDED-DATE          PIC  X(02).
101705         16  LA-RESEND-LETR          PIC  X(4).
               16  FILLER                  PIC  X(08).
101705*        16  LA-RESEND-LETR-2        PIC  X(4).
101705*        16  LA-RESEND-LETR-3        PIC  X(4).
070711*        16  FILLER                  PIC  X(59).
               16  LA-ARCHIVE-STATUS       PIC  X.
                   88  LA-TEMP                VALUE 'T'.
                   88  LA-QWS                 VALUE 'Q'.
                   88  LA-BATCH               VALUE 'B'.
070711         16  LA-FINAL-ACT-IND        PIC  X(1).
070711         16  LA-VA-DISCLOSURE-IND    PIC  X(1).
110612         16  LA-ENDT-ARCH-NO         PIC S9(8) COMP.
110612         16  LA-ENDT-ARCH-NO-X REDEFINES LA-ENDT-ARCH-NO
110612                                     PIC X(4).
110612         16  FILLER                  PIC  X(42).
00120 *        16  FILLER                  PIC  X(71).
070711         16  LA-LAST-MAINT-DATE      PIC  X(2).
070711         16  LA-LAST-MAINT-TIME      PIC S9(6) COMP-3.
070711         16  LA-LAST-MAINT-TIMEX  REDEFINES LA-LAST-MAINT-TIME
070711                                     PIC  X(4).
070711         16  LA-LAST-UPDATED-BY      PIC  X(4).
00121
00122 ******************************************************************
100111*                                COPY ELCENCC.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            ELCENCC                             *
      *                            VMOD=2.001                          *
      *                                                                *
      *   CLAIM SYSTEM ENCLOSURE CODE TABLE                            *
      *                                                                *
      *   THIS COPYBOOK IS USED FOR THE ONLINE PROCESS OF CREATING     *
      *   A NAPERSOFT DOCUMENT                                         *
      *                                                                *
      *   FILE DESCRIPTION = ENCLOSURE CODE TABLE                      *
      *                                                                *
      *   FILE TYPE = VSAM,KSDS                                        *
      *   RECORD SIZE = 400   RECFORM = FIX                            *
      *                                                                *
      *   BASE CLUSTER NAME = ELENCC                    RKP=2,LEN=16   *
      *                                                                *
      *   LOG = NO                                                     *
      *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      *                                                                *
      *                                                                *
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 082010    2008100900001  PEMA  NEW COPYBOOK/FILE
      * 061217    2017060900001  TANA  INCREASE ATTACHMENTS FIELD SIZE
      ******************************************************************
       01  ENCLOSURE-CODES.
           12  NC-RECORD-ID                      PIC XX.
               88  VALID-NC-ID                      VALUE 'NC'.
           12  NC-CONTROL-PRIMARY.
               16  NC-COMPANY-CD                 PIC X.
               16  NC-REC-TYPE                   PIC X.
                   88  NC-CLAIMS                   VALUE '1'.
                   88  NC-ADMIN                    VALUE '2'.
               16  NC-ENC-CODE                   PIC X(5).
               16  FILLER                        PIC X(09).
           12  NC-MAINT-INFORMATION.
               16  NC-LAST-MAINT-DT              PIC XX.
               16  NC-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
               16  NC-LAST-MAINT-USER            PIC X(4).
               16  FILLER                        PIC XX.
           12  NC-OUTPUT-STACK                   PIC XXX.
           12  NC-ENCLOSURE-LINE                 PIC X(100).
           12  NC-ATTACHMENTS                    PIC X(255).
           12  NC-FUTURE                         PIC X(12).
      ******************************************************************
100111*                                COPY NSCASEXTR.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            NSCASEXTR.                          *
      *                            VMOD=2.001                          *
      *                                                                *
      *   FILE DESCRIPTION = NAPERSOFT ADMIN SERVICES EXTRACT FILE     *
      *                                                                *
      *   FILE TYPE = VSAM,KSDS                                        *
      *   RECORD SIZE = 4500 RECFORM = FIXED                           *
      *                                                                *
      *   BASE CLUSTER = NSASEXTR                       RKP=0,LEN=07   *
      *                                                                *
      *   LOG = NO                                                     *
      *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      ******************************************************************
      *-----------------------------------------------------------------
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 072211    2011022800001  PEMA  NEW FILE
      *-----------------------------------------------------------------
       01 NSAS-EXTRACT-RECORD.
          05  NSAS-CONTROL-PRIMARY.
              10  NSAS-COMPANY-CD      PIC X.
              10  NSAS-ARCHIVE-NO      PIC 9(8) BINARY.
              10  NSAS-SEQ-NO          PIC 9(4) BINARY.
          05  NSAS-LETTER-VARIABLES    PIC X(4350).
          05  FILLER                   PIC X(143).
           01  WS-PASS-AREA.
100111         05  WS-PASS-AREA-LENGTH PIC S9(4) COMP VALUE +352.
100111         05  WS-COMMAREA.
100111             10  WS-PASS-VADS-REC    PIC X(350).
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
100111
100111 01  DFHCOMMAREA                     PIC X(1000).
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'ELVADS' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
100111
100111     MOVE DFHCOMMAREA          TO  WS-PASS-AREA.
100111
100111     MOVE WS-PASS-VADS-REC       TO VIRGINIA-DISCLOSURE
           MOVE SPACES                 TO WS-CALC-SWITCHES
100111     MOVE VD-COMPANY-CD          TO WS-COMPANY-CD
100111     MOVE VD-LETTER-ID           TO VA-LETTER
100111     MOVE VD-CARRIER             TO VA-CARRIER
100111     MOVE VD-GROUPING            TO VA-GROUPING
100111     MOVE VD-STATE               TO VA-STATE
100111     MOVE VD-ACCOUNT             TO VA-ACCOUNT
100111     MOVE VD-INSURED-LAST-NAME   TO VA-LASTNAME
100111     MOVE VD-INSURED-FIRST-NAME  TO VA-FIRSTNAME
100111     MOVE VD-INSURED-MIDDLE-INIT TO VA-MIDINIT
100111     MOVE VD-INSURED-ADDRESS-1   TO VA-ADDR1
100111     MOVE VD-INSURED-ADDRESS-2   TO VA-ADDR2
100111     MOVE VD-INSURED-CITY        TO VA-ADDR-CITY
100111     MOVE VD-INSURED-STATE       TO VA-ADDR-ST
100111     MOVE VD-INSURED-ZIP-CODE    TO VA-ADDR-ZIP
100111     MOVE VD-JOINT-LAST-NAME     TO VA-JNT-LASTNAME
100111     MOVE VD-JOINT-FIRST-NAME    TO VA-JNT-FIRSTNAME
100111     MOVE VD-JOINT-MIDDLE-INIT   TO VA-JNT-MIDINIT
100111     IF VD-LF-BENEFIT-CD = '01' OR '03'
              SET GROSS-CERT           TO TRUE
              SET CALC-LF-GROSS        TO TRUE
              SET CALC-LF-NET          TO TRUE
           END-IF
100111     IF VD-LF-BENEFIT-CD = '05' OR '06'
              SET NET-CERT             TO TRUE
              SET CALC-LF-NET          TO TRUE
           END-IF
100111     IF VD-LF-BENEFIT-CD = '02' OR '04'
              SET CALC-LF-LEVEL        TO TRUE
              SET LEVEL-CERT           TO TRUE
           END-IF
100111     IF VD-AH-BENEFIT-CD NOT = '00' AND '  '
              SET CALC-AH              TO TRUE
           END-IF
100111     IF (VD-LF-BENEFIT-CD = '00' OR '  ')
100111        AND (VD-AH-BENEFIT-CD NOT = '00' AND '  ')
              SET AH-ONLY              TO TRUE
           END-IF
100111     MOVE VD-LF-PREMIUM-AMT      TO LFPREM
100111     MOVE VD-AH-PREMIUM-AMT      TO AHPREM
100111     MOVE VD-LF-BENEFIT-AMT      TO LFAMT
100111     IF VD-LF-BENEFIT-CD NOT = '00' AND '  '
100111        MOVE VD-LF-TERM          TO N
           ELSE
100111        MOVE VD-AH-TERM          TO N
           END-IF
100111     MOVE VD-LOAN-TERM           TO M
           IF M = 0
              MOVE N                   TO M
           END-IF
100111     COMPUTE I = VD-LOAN-APR / +1200
033015     if i = zeros
033015        move .0000000001         to i
033015     end-if
           MOVE ZEROS                  TO Z
           MOVE +30                    TO T
           MOVE +3.63                  TO ANNUAL-DISCOUNT
100111     MOVE VD-CERT-EFF-DT         TO DC-BIN-DATE-1
100111     MOVE VD-1ST-PMT-DT          TO DC-BIN-DATE-2
           MOVE '1'                    TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           IF NO-CONVERSION-ERROR
              COMPUTE D = (DC-ELAPSED-MONTHS * 30) + DC-ODD-DAYS-OVER
           END-IF
100111     IF VD-AH-BENEFIT-CD NOT = '  ' AND '00'
100111        MOVE VD-AH-RATE          TO AHRATE
           END-IF
           IF GROSS-CERT OR LEVEL-CERT
100111        COMPUTE LF-RATE = VD-LF-RATE / (N / 12)
           END-IF
           IF NET-CERT
100111        MOVE VD-LF-RATE          TO OBRATE
           END-IF
           IF GROSS-CERT
              PERFORM 1000-GET-ERRATE  THRU 1000-EXIT
           END-IF
           COMPUTE ANGLEN = (1 - (1 / (1 + I)) ** N) / I
           COMPUTE DISC = ANNUAL-DISCOUNT / 1200
           COMPUTE V-M = (1 / (1 + I)) ** N
           COMPUTE VM = (1 / (1 + I)) ** M
           COMPUTE VM-N = (1 / (1 + I)) ** (M - N)
           COMPUTE V = 1 / (1 + I)
           COMPUTE X = 1 / (1 + DISC)
           COMPUTE Y = (1 + (D * I / T)) / (1 + I)
           COMPUTE ANGLENY = ANGLEN / Y
           COMPUTE LF = ((1 + (Z * I)) / I)
              * (((X ** N - 1) / (X - 1))
              - ((X ** N * VM-N - VM) / (X * (1 + I) - 1)))
              * (OBRATE / 1000)
      *    COMPUTE LFA = (1 + (Z * I)) / I
      *    COMPUTE LFB = X ** N - 1
      *    COMPUTE LFC = X - 1
      *    COMPUTE LFD = LFB / LFC
      *    COMPUTE LFE = (((X ** N) * (V ** (M - N))) - (V ** M)) /
      *       ((X * (1 + I)) - 1)
      *    COMPUTE LF = LFA * (LFD - LFE) * (OBRATE / 1000)
           COMPUTE R = LFAMT / (ANGLENY - LF - N * AHRATE / 100)
           COMPUTE SP = (LF * 100) / (ANGLENY * (1 + (Z * I)))
           COMPUTE LDPN = SP * R * ANGLENY * (1 + (Z * I)) / 100
           COMPUTE AHPN = R * N * AHRATE / 100
           COMPUTE Y = (1 + (D * I / T)) / (1 + I)
           COMPUTE ANGLENY = ANGLEN / Y
           IF GROSS-CERT
100111        COMPUTE CUR-PMT = LFAMT / VD-LF-TERM
              COMPUTE AMT-FINANCED = CUR-PMT * ANGLEN
              COMPUTE PRINC-AMT = AMT-FINANCED - LFPREM - AHPREM
           ELSE
              IF NET-CERT
                 COMPUTE PRINC-AMT = LFAMT - LFPREM - AHPREM
                 MOVE LFAMT TO AMT-FINANCED
              END-IF
           END-IF
           MOVE 000000.00              TO VA-CL-NET-AMT
                                          VA-CL-GRS-AMT
                                          VA-CL-LEV-AMT
                                          VA-CD-NET-AMT
                                          VA-CD-GRS-AMT
                                          VA-CD-LEV-AMT
                                          VA-CLD-NET-AMT
                                          VA-CLD-GRS-AMT
                                          VA-CLD-LEV-AMT
                                          VA-NO-NET-AMT
                                          VA-NO-GRS-AMT
                                          VA-NO-LEV-AMT
           MOVE 00000.00               TO VA-CL-NET-PMT
                                          VA-CL-GRS-PMT
                                          VA-CL-LEV-PMT
                                          VA-CD-NET-PMT
                                          VA-CD-GRS-PMT
                                          VA-CD-LEV-PMT
                                          VA-CLD-NET-PMT
                                          VA-CLD-GRS-PMT
                                          VA-CLD-LEV-PMT
                                          VA-NO-NET-PMT
                                          VA-NO-GRS-PMT
                                          VA-NO-LEV-PMT
           MOVE 00000.00               TO VA-CL-NET-PRM
                                          VA-CL-GRS-PRM
                                          VA-CL-LEV-PRM
                                          VA-CD-NET-PRM
                                          VA-CD-GRS-PRM
                                          VA-CD-LEV-PRM
                                          VA-CLD-NET-PRM
                                          VA-CLD-GRS-PRM
                                          VA-CLD-LEV-PRM
                                          VA-NO-NET-PRM
                                          VA-NO-GRS-PRM
                                          VA-NO-LEV-PRM
           IF NET-CERT
              PERFORM 2000-BUILD-NET-AMTS
                                       THRU 2000-EXIT
           END-IF
           IF GROSS-CERT
              PERFORM 2000-BUILD-NET-AMTS
                                       THRU 2000-EXIT
              PERFORM 3000-BUILD-GROSS-AMTS
                                       THRU 3000-EXIT
           END-IF
           IF LEVEL-CERT
              PERFORM 4000-BUILD-LEVEL-AMTS
                                       THRU 4000-EXIT
           END-IF
           IF AH-ONLY
              PERFORM 3500-BUILD-AH-AMTS
                                       THRU 3500-EXIT
           END-IF
100111     STRING VD-INSURED-FIRST-NAME ' '
100111        VD-INSURED-LAST-NAME DELIMITED BY '  '
              INTO VA-BORROWER
           END-STRING
100111     MOVE VD-CERT-NO             TO VA-CERT-NO
100111     MOVE VD-ACCOUNT-NAME        TO VA-ACCT-NAME
100111     MOVE VD-BENEFICIARY-NAME    TO VA-BENE
100111     IF VD-LF-BENEFIT-CD = '03' OR '04' OR '06'
              MOVE 'X'                 TO VA-JL
           ELSE
100111        IF VD-LF-BENEFIT-CD NOT = '00' AND '  '
                 MOVE 'X'              TO VA-SL
              END-IF
           END-IF
           IF GROSS-CERT
              MOVE 'X'                 TO VA-GD
           ELSE
              IF LEVEL-CERT
                 MOVE 'X'              TO VA-LL
              ELSE
                 IF NET-CERT
                    MOVE 'X'           TO VA-NP
                 END-IF
              END-IF
           END-IF
           IF CALC-AH
              MOVE 'X'                 TO VA-SA
              EVALUATE TRUE
100111         WHEN VD-AH-BENEFIT-CD = '53' OR '54' OR
                 '55' OR '56' OR '57'
                 MOVE 'X'              TO VA-JA
                 MOVE ' '              TO VA-SA
100111         WHEN VD-AH-BENEFIT-CD = '03' OR '53'
                 MOVE 'X'              TO VA-7R
100111         WHEN VD-AH-BENEFIT-CD = '01' OR '54'
                 MOVE 'X'              TO VA-14R
100111         WHEN VD-AH-BENEFIT-CD = '05' OR '56'
                 MOVE 'X'              TO VA-14E
100111         WHEN VD-AH-BENEFIT-CD = '04' OR '55'
                 MOVE 'X'              TO VA-30R
100111         WHEN VD-AH-BENEFIT-CD = '02' OR '57'
                 MOVE 'X'              TO VA-30E
              END-EVALUATE
           END-IF
      *    MOVE 'X'                    TO VA-SL VA-JL VA-SA VA-JA VA-GD
      *       VA-NP VA-7R VA-LL VA-TN VA-14R VA-14E VA-30R VA-30E
100111     MOVE VD-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE SPACE                  TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           IF NO-CONVERSION-ERROR
              STRING DC-GREG-DATE-CYMD-R (5:2) '/'
                     DC-GREG-DATE-CYMD-R (7:2) '/'
                     DC-GREG-DATE-CYMD-R (1:4) DELIMITED BY SIZE
100111           INTO VA-EFFDATE
              END-STRING
           ELSE
100111        MOVE '00/00/0000'        TO VA-EFFDATE
           END-IF
100111     PERFORM 4100-CREATE-ARCHIVE THRU 4100-EXIT.
           GO TO 8000-RETURN
           .
       1000-GET-ERRATE.
           MOVE ALL '9'                TO ERRATE-KEY
100111     MOVE VD-COMPANY-CD          TO RATE-COMPANY-CD
           MOVE ZEROS                  TO RATE-STATE-CODE
100111     MOVE VD-STATE               TO RATE-ST-CODE
           MOVE 'L'                    TO RATE-L-AH
           IF GROSS-CERT
100111        IF VD-LF-BENEFIT-CD = '01'
                 MOVE '05'             TO RATE-LAH-NUM
              ELSE
                 MOVE '06'             TO RATE-LAH-NUM
              END-IF
           END-IF
           IF NET-CERT
100111        IF VD-LF-BENEFIT-CD = '05'
                 MOVE '01'             TO RATE-LAH-NUM
              ELSE
                 MOVE '03'             TO RATE-LAH-NUM
              END-IF
           END-IF
           MOVE 99                     TO RATE-HIGH-AGE
100111     MOVE VD-LF-BENEFIT-AMT    TO RATE-HIGH-AMT
100111     MOVE VD-CERT-EFF-DT         TO DC-BIN-DATE-1
           MOVE SPACE                  TO DC-OPTION-CODE
           PERFORM 9700-DATE-LINK      THRU 9700-EXIT
           IF NO-CONVERSION-ERROR
              MOVE ZEROS               TO RATE-EXPIRY-DATE
                                          WS-HOLD-RATE-EXP-DT
              MOVE DC-GREG-DATE-CYMD   TO WS-HOLD-RATE-EXP-AL(4:8)
              MOVE WS-HOLD-RATE-EXP-DT TO RATE-EXPIRY-DATE
              MOVE ERRATE-KEY          TO SAVE-ERRATE-KEY
           ELSE
              MOVE 00099999999         TO RATE-EXPIRY-DATE
           END-IF
           PERFORM 1010-STARTBR-ERRATE THRU 1010-EXIT
           IF RESP-NORMAL
             PERFORM UNTIL
              (FOUND-VA-RATE)
              OR (NO-VA-RATE-FOUND)
              OR (NOT RESP-NORMAL)
              PERFORM 1020-READNEXT-ERRATE
                                       THRU 1020-EXIT
              IF (RT-STATE-CODE = SVRT-STATE-CODE)
                 AND (RT-L-AH-CODE = SVRT-L-AH-CODE)
                 IF (RT-HIGH-AMT > SVRT-HIGH-AMT)
                    AND (RT-EXPIRY-DATE > SVRT-EXPIRY-DATE)
                    SET FOUND-VA-RATE     TO TRUE
                    IF SVRT-LAH-NUM = '05' OR '06'
                       MOVE RT-DISCOUNT-OB-RATE
                                       TO OBRATE
                    ELSE
                       COMPUTE LF-RATE = RT-L-RATE (N) / (N / 12)
                    END-IF
                 END-IF
              ELSE
                 SET NO-VA-RATE-FOUND  TO TRUE
              END-IF
             END-PERFORM
           END-IF
           .
       1000-EXIT.
           EXIT.
       1010-STARTBR-ERRATE.
           
      * EXEC CICS STARTBR
      *       DATASET    ('ERRATE')
      *       RIDFLD     (ERRATE-KEY)
      *       RESP       (WS-RESPONSE)
      *    END-EXEC
           MOVE 'ERRATE' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00002966' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303032393636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERRATE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       1010-EXIT.
           EXIT.
       1020-READNEXT-ERRATE.
           
      * EXEC CICS READNEXT
      *       DATASET    ('ERRATE')
      *       RIDFLD     (ERRATE-KEY)
      *       INTO       (RATE-RECORD)
      *       RESP       (WS-RESPONSE)
      *    END-EXEC
           MOVE LENGTH OF
            RATE-RECORD
             TO DFHEIV12
           MOVE 'ERRATE' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00002975' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303032393735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 RATE-RECORD, 
                 DFHEIV12, 
                 ERRATE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       1020-EXIT.
           EXIT.
       2000-BUILD-NET-AMTS.
           COMPUTE WS-NET-PMT-1 = PRINC-AMT / (ANGLEN - LF)
           IF AHRATE NOT = ZEROS
              COMPUTE WS-NET-PMT-2 = PRINC-AMT /
               (ANGLEN - (N * AHRATE / 100))
           END-IF
           COMPUTE WS-NET-PMT-3 = PRINC-AMT /
             (ANGLEN - LF - (N * AHRATE / 100))
           COMPUTE WS-NET-PMT-4 = PRINC-AMT / ANGLEN
           COMPUTE WS-NET-PREM-1 = SP * WS-NET-PMT-1 * ANGLEN *
             (1 + (Z * I)) / 100
           COMPUTE WS-NET-PREM-2 = WS-NET-PMT-2 * N * AHRATE / 100
           COMPUTE WS-NET-PREM-3 = (SP * WS-NET-PMT-3 * ANGLEN *
             (1 + (Z * I)) / 100) + (WS-NET-PMT-3 * N * AHRATE / 100)
100111     IF (VD-LF-BENEFIT-CD NOT = '00' AND '  ')
100111        AND (VD-AH-BENEFIT-CD = '00' OR '  ')
              COMPUTE VA-CL-NET-AMT = PRINC-AMT + WS-NET-PREM-1
              MOVE WS-NET-PMT-1        TO VA-CL-NET-PMT
              MOVE WS-NET-PREM-1       TO VA-CL-NET-PRM
           END-IF
100111     IF (VD-LF-BENEFIT-CD = '00' OR '  ')
100111        AND (VD-AH-BENEFIT-CD NOT = '00' AND '  ')
              IF WS-NET-PREM-2 NOT = ZEROS
                 COMPUTE VA-CD-NET-AMT = PRINC-AMT + WS-NET-PREM-2
              END-IF
              MOVE WS-NET-PMT-2        TO VA-CD-NET-PMT
              MOVE WS-NET-PREM-2       TO VA-CD-NET-PRM
           END-IF
100111     IF (VD-LF-BENEFIT-CD NOT = '00' AND '  ')
100111        AND (VD-AH-BENEFIT-CD NOT = '00' AND '  ')
              COMPUTE VA-CLD-NET-AMT = PRINC-AMT + WS-NET-PREM-3
              MOVE WS-NET-PMT-3        TO VA-CLD-NET-PMT
              MOVE WS-NET-PREM-3       TO VA-CLD-NET-PRM
           END-IF
           MOVE PRINC-AMT              TO VA-NO-NET-AMT
           MOVE WS-NET-PMT-4           TO VA-NO-NET-PMT
           MOVE WS-NET-PREM-4           TO VA-NO-NET-PRM
           .
       2000-EXIT.
           EXIT.
       3000-BUILD-GROSS-AMTS.
      *    COMPUTE WS-GROSS-PMT-1 = PRINC-AMT /
      *      (ANGLENY - (N * (N + ((D - T) / T)) / 12 * LF-RATE / 100))
      *    COMPUTE WS-GROSS-PMT-1 = AMT-FINANCED / ANGLEN
           COMPUTE WS-GROSS-PMT-1 = PRINC-AMT / (ANGLEN -
             (N * N / 12 * LF-RATE / 100))
      *    COMPUTE WS-GROSS-PMT-2 = PRINC-AMT /
      *      (ANGLENY - (N * AHRATE / 100))
           COMPUTE WS-GROSS-PMT-2 = PRINC-AMT /
             (ANGLEN - (N * AHRATE / 100))
      *    COMPUTE WS-GROSS-PMT-3 = PRINC-AMT /
      *      (ANGLENY - ((N * (N + ((D - T) / T)) /
      *      12 * LF-RATE / 100) + (N * AHRATE / 100)))
      *    COMPUTE WS-GROSS-PMT-3 = (AMT-FINANCED / ANGLEN)
      *      + (N * AHRATE / 100)
           COMPUTE WS-GROSS-PMT-3 = PRINC-AMT /
              (ANGLEN - ((N * N / 12 * LF-RATE / 100)
             + (N * AHRATE / 100)))
      *    COMPUTE WS-GROSS-PMT-4 = PRINC-AMT / ANGLENY
           COMPUTE WS-GROSS-PMT-4 = PRINC-AMT / ANGLEN
      *    COMPUTE WS-GROSS-PREM-1 = WS-GROSS-PMT-1 *
      *      N * (N + ((D - T) / T)) / 12 * LF-RATE / 100
           COMPUTE WS-GROSS-PREM-1 = WS-GROSS-PMT-1 *
             N * N / 12 * LF-RATE / 100
           COMPUTE WS-GROSS-PREM-2 = WS-GROSS-PMT-2 * N * AHRATE / 100
      *    COMPUTE WS-GROSS-PREM-3 = (WS-GROSS-PMT-3 * N *
      *      (N + ((D - T) / T)) / 12 * LF-RATE / 100) +
      *      (WS-GROSS-PMT-3 * N * AHRATE / 100)
           COMPUTE WS-GROSS-PREM-3 = (WS-GROSS-PMT-3 * N *
             N / 12 * LF-RATE / 100) +
             (WS-GROSS-PMT-3 * N * AHRATE / 100)
           MOVE ZEROS                  TO WS-GROSS-PREM-4
100111     IF (VD-LF-BENEFIT-CD NOT = '00' AND '  ')
100111        AND (VD-AH-BENEFIT-CD = '00' OR '  ')
              COMPUTE VA-CL-GRS-AMT = PRINC-AMT + WS-GROSS-PREM-1
              MOVE WS-GROSS-PMT-1      TO VA-CL-GRS-PMT
              MOVE WS-GROSS-PREM-1     TO VA-CL-GRS-PRM
           END-IF
100111     IF (VD-LF-BENEFIT-CD = '00' OR '  ')
100111        AND (VD-AH-BENEFIT-CD NOT = '00' AND '  ')
              COMPUTE VA-CD-GRS-AMT = PRINC-AMT + WS-GROSS-PREM-2
              MOVE WS-GROSS-PMT-2      TO VA-CD-GRS-PMT
              MOVE WS-GROSS-PREM-2     TO VA-CD-GRS-PRM
           END-IF
100111     IF (VD-LF-BENEFIT-CD NOT = '00' AND '  ')
100111        AND (VD-AH-BENEFIT-CD NOT = '00' AND '  ')
              COMPUTE VA-CLD-GRS-AMT = PRINC-AMT + WS-GROSS-PREM-3
              MOVE WS-GROSS-PMT-3      TO VA-CLD-GRS-PMT
              MOVE WS-GROSS-PREM-3     TO VA-CLD-GRS-PRM
           END-IF
           COMPUTE VA-NO-GRS-AMT = PRINC-AMT
           MOVE WS-GROSS-PMT-4           TO VA-NO-GRS-PMT
           MOVE WS-GROSS-PREM-4           TO VA-NO-GRS-PRM
           .
       3000-EXIT.
           EXIT.
       3500-BUILD-AH-AMTS.
100111     MOVE VD-AH-BENEFIT-AMT    TO CUR-PMT
           COMPUTE AMT-FINANCED = CUR-PMT * ANGLEN
           COMPUTE PRINC-AMT = AMT-FINANCED - LFPREM - AHPREM
           COMPUTE WS-GROSS-PMT-2 = PRINC-AMT /
             (ANGLEN - (N * AHRATE / 100))
           COMPUTE WS-GROSS-PMT-4 = PRINC-AMT / ANGLEN
           COMPUTE WS-GROSS-PREM-2 = WS-GROSS-PMT-2 * N * AHRATE / 100
           MOVE ZEROS                  TO WS-GROSS-PREM-4
           COMPUTE VA-CD-GRS-AMT = PRINC-AMT + WS-GROSS-PREM-2
           COMPUTE VA-NO-GRS-AMT = PRINC-AMT
      *    MOVE WS-GROSS-PMT-1           TO VA-CL-GRS-PMT
           MOVE WS-GROSS-PMT-2           TO VA-CD-GRS-PMT
      *    MOVE WS-GROSS-PMT-3           TO VA-CLD-GRS-PMT
           MOVE WS-GROSS-PMT-4           TO VA-NO-GRS-PMT
      *    MOVE WS-GROSS-PREM-1           TO VA-CL-GRS-PRM
           MOVE WS-GROSS-PREM-2           TO VA-CD-GRS-PRM
      *    MOVE WS-GROSS-PREM-3           TO VA-CLD-GRS-PRM
           MOVE WS-GROSS-PREM-4           TO VA-NO-GRS-PRM
           .
       3500-EXIT.
           EXIT.
       4000-BUILD-LEVEL-AMTS.
           MOVE 1                      TO N
           COMPUTE PPY = 1 / M * 12
           COMPUTE T = 360 * M / 12
           COMPUTE D = D + T
100111     COMPUTE I = VD-LOAN-APR / PPY / 100
           COMPUTE Y = (1 + (D * I / T)) / (1 + I)
           COMPUTE ANGLEN = (1 - (1 / (1 + I)) ** 1) / I
           COMPUTE ANGLENY = ANGLEN / Y
           COMPUTE PRINC-AMT = (LFAMT * ANGLENY) - LFPREM
           COMPUTE WS-LEVEL-PMT-1 = PRINC-AMT / (ANGLENY -
              (1 * ((1 + (D - T) / T) / PPY) * (LF-RATE / 100)))
           COMPUTE WS-LEVEL-PMT-4 = PRINC-AMT / ANGLENY
           COMPUTE WS-LEVEL-PREM-1 = WS-LEVEL-PMT-1 *
              (1 * ((1 + (D - T) / T) / PPY) * (LF-RATE / 100))
           MOVE ZEROS                  TO WS-LEVEL-PREM-4
           MOVE WS-LEVEL-PMT-1           TO VA-CL-LEV-PMT
           MOVE WS-LEVEL-PMT-4           TO VA-NO-LEV-PMT
           MOVE WS-LEVEL-PREM-1           TO VA-CL-LEV-PRM
           MOVE WS-LEVEL-PREM-4           TO VA-NO-LEV-PRM
           COMPUTE VA-CL-LEV-AMT = PRINC-AMT + WS-LEVEL-PREM-1
           MOVE PRINC-AMT              TO VA-NO-LEV-AMT
           .
       4000-EXIT.
           EXIT.
100111
100111 4100-CREATE-ARCHIVE.
100111
100111     PERFORM 4125-GET-CSR-INFO THRU 4125-EXIT.
100111     PERFORM 4250-GET-ELLETR   THRU 4250-EXIT.
100111     PERFORM 4500-GET-ARCH-NO  THRU 4500-EXIT.
100111     PERFORM 4700-GET-ELENCC   THRU 4700-EXIT.
100111     PERFORM 5700-WRITE-EXTR THRU 5700-EXIT.
100111
100111 4100-EXIT.
100111     EXIT.
100111
100111 4125-GET-CSR-INFO.
100111
100111     MOVE VD-PROC-ID             TO VA-PROC-ID
100111     MOVE VD-CSR-ID              TO VA-CSR-CODE
100111     MOVE VD-COMP-ID             TO WS-ELCNTL-COMPANY-ID
100111     MOVE '2'                    TO WS-ELCNTL-REC-TYPE
100111     MOVE VD-CSR-ID              TO WS-ELCNTL-GENL
100111     MOVE +0                     TO WS-ELCNTL-SEQ-NO
100111     
      * EXEC CICS READ
100111*       INTO    (CONTROL-FILE)
100111*       DATASET ('ELCNTL')
100111*       RIDFLD  (WS-ELCNTL-KEY)
100111*       RESP    (WS-RESPONSE)
100111*    END-EXEC
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00003147' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303033313437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 WS-ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
100111     IF RESP-NORMAL
100111        MOVE CF-PROCESSOR-NAME   TO VA-CSR-NAME
100111                                    VA-PROC-NAME
100111        MOVE VD-CSR-ID           TO VA-CSR-TITLE
100111                                    VA-PROC-TITLE
100111     END-IF
100111
100111     IF VD-PROC-ID NOT = VD-CSR-ID
100111        MOVE VD-COMP-ID          TO WS-ELCNTL-COMPANY-ID
100111        MOVE '2'                 TO WS-ELCNTL-REC-TYPE
100111        MOVE VD-PROC-ID          TO WS-ELCNTL-GENL
100111        MOVE +0                  TO WS-ELCNTL-SEQ-NO
100111        
      * EXEC CICS READ
100111*          INTO    (CONTROL-FILE)
100111*          DATASET ('ELCNTL')
100111*          RIDFLD  (WS-ELCNTL-KEY)
100111*          RESP    (WS-RESPONSE)
100111*       END-EXEC
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00003165' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303033313635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 WS-ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
100111        IF RESP-NORMAL
100111           MOVE CF-PROCESSOR-NAME   TO VA-PROC-NAME
100111           MOVE VD-PROC-ID          TO VA-PROC-TITLE
100111        END-IF
100111     END-IF
100111
100111     .
100111 4125-EXIT.
100111     EXIT.
100111
100111 4250-GET-ELLETR.
100111
100111     MOVE WS-COMPANY-CD          TO WS-ELLETR-COMPANY-CD
100111     MOVE VD-LETTER-ID           TO WS-ELLETR-LETTER-ID
100111     MOVE +0                     TO WS-ELLETR-SEQ-NO
100111     MOVE SPACES                 TO W-Z-CONTROL-DATA
100111
100111     
      * EXEC CICS READ
100111*         DATASET    ('ELLETR')
100111*         INTO       (TEXT-FILES)
100111*         RIDFLD     (WS-ELLETR-KEY)
100111*         RESP       (WS-RESPONSE)
100111*         GTEQ
100111*    END-EXEC
           MOVE LENGTH OF
            TEXT-FILES
             TO DFHEIV11
           MOVE 'ELLETR' TO DFHEIV1
      *    MOVE '&"IL       G          (  N#00003188' TO DFHEIV0
           MOVE X'2622494C2020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303033313838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 TEXT-FILES, 
                 DFHEIV11, 
                 WS-ELLETR-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
100111
100111     IF RESP-NORMAL
100111        IF (LETTER-FILE-TEXT)
100111           AND (VD-LETTER-ID = TX-LETTER-NO)
100111           AND (TX-LINE-SQUEEZE-CONTROL = 'Z')
100111           PERFORM 4280-PROCESS-Z-CONTROLS
100111                                 THRU 4280-EXIT
100111        END-IF
100111     END-IF
100111
100111     .
100111 4250-EXIT.
100111     EXIT.
100111
100111
100111 4280-PROCESS-Z-CONTROLS.
100111
100111     MOVE TX-TEXT-LINE           TO W-Z-CONTROL-DATA
100111
100111     .
100111 4280-EXIT.
100111     EXIT.
100111
100111
100111 4500-GET-ARCH-NO.
100111
100111     MOVE VD-COMP-ID             TO WS-ELCNTL-COMPANY-ID
100111     MOVE '1'                    TO WS-ELCNTL-REC-TYPE
100111     MOVE SPACES                 TO WS-ELCNTL-GENL
100111     MOVE +0                     TO WS-ELCNTL-SEQ-NO
100111
100111     
      * EXEC CICS READ
100111*       INTO    (CONTROL-FILE)
100111*       DATASET ('ELCNTL')
100111*       RIDFLD  (WS-ELCNTL-KEY)
100111*       UPDATE
100111*       RESP    (WS-RESPONSE)
100111*    END-EXEC
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"IL       EU         (  N#00003226' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303033323236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 WS-ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
100111
100111     IF RESP-NORMAL
100111        AND (CF-COMPANY-ID  = VD-COMP-ID)
100111        AND (CF-RECORD-TYPE = '1')
100111        ADD +1                   TO CF-CREDIT-LAST-ARCH-NUM
100111        MOVE CF-CREDIT-LAST-ARCH-NUM
100111                                 TO WS-ARCHIVE-NO
100111        
      * EXEC CICS REWRITE
100111*          FROM    (CONTROL-FILE)
100111*          DATASET ('ELCNTL')
100111*       END-EXEC
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&& L                  %   #00003240' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303033323430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
100111     ELSE
100111        MOVE +0                  TO WS-ARCHIVE-NO
100111     END-IF
100111
100111*** IF AN ARCHIVE NUMBER IS PASSED TO THIS PGM THEN
100111*** IT IS SAVED IN VA-ARCH-NO.  THE VA DISC LETTER
100111*** WILL CREATE A DIFFERENT ARCH NO.
100111     IF VD-ARCHIVE-NO GREATER THAN ZERO
100111         MOVE VD-ARCHIVE-NO      TO VA-ARCH-NO
100111     ELSE
100111         MOVE WS-ARCHIVE-NO      TO VA-ARCH-NO
100111     END-IF
100111    .
100111 4500-EXIT.
100111     EXIT.
100111
100111
100111 4700-GET-ELENCC.
100111
072312     MOVE SPACES                 TO WS-ELENCC-KEY
100111     MOVE WS-COMPANY-CD          TO WS-ELENCC-COMPANY-CD
100111     MOVE '2'                    TO WS-ELENCC-REC-TYPE
100111     MOVE W-ENCLOSURE-CD         TO WS-ELENCC-ENC-CODE
100111     PERFORM VARYING E1 FROM +1 BY +1 UNTIL
100111        (WS-ELENCC-ENC-CODE (E1:1) = ' ')
100111        OR (E1 > +5)
100111     END-PERFORM
100111
100111     IF E1 < +5
100111        MOVE VD-STATE            TO WS-ELENCC-ENC-CODE (E1:2)
100111     END-IF
100111
100111     
      * EXEC CICS READ
100111*        DATASET   ('ELENCC')
100111*        INTO      (ENCLOSURE-CODES)
100111*        RIDFLD    (WS-ELENCC-KEY)
100111*        RESP      (WS-RESPONSE)
100111*    END-EXEC
           MOVE LENGTH OF
            ENCLOSURE-CODES
             TO DFHEIV11
           MOVE 'ELENCC' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00003276' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303033323736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ENCLOSURE-CODES, 
                 DFHEIV11, 
                 WS-ELENCC-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
100111
100111     IF NOT RESP-NORMAL
100111        MOVE W-ENCLOSURE-CD    TO WS-ELENCC-ENC-CODE
100111        
      * EXEC CICS READ
100111*           DATASET   ('ELENCC')
100111*           INTO      (ENCLOSURE-CODES)
100111*           RIDFLD    (WS-ELENCC-KEY)
100111*           RESP      (WS-RESPONSE)
100111*       END-EXEC
           MOVE LENGTH OF
            ENCLOSURE-CODES
             TO DFHEIV11
           MOVE 'ELENCC' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00003285' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303033323835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ENCLOSURE-CODES, 
                 DFHEIV11, 
                 WS-ELENCC-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
100111     END-IF
100111
100111     IF RESP-NORMAL
100111        MOVE NC-OUTPUT-STACK     TO VA-STACK
100111        MOVE NC-ENCLOSURE-LINE   TO VA-ENC-LINE
100111        MOVE NC-ATTACHMENTS      TO VA-ATTACH
100111        MOVE 'N'                 TO VA-PRINT-NOW
100111     END-IF
100111
100111     .
100111 4700-EXIT.
100111     EXIT.
100111
100111
100111 5000-BUILD-ERARCH.
100111
100111* DATA SOURCE MEANINGS  BL-DATA-SRCE
100111*    1) FROM ACCT MAINT
100111*    2) FROM CERT UPDATE
100111*    3) FROM COMP MAINT
100111*    4) FROM REVIEW AND CORRECTIONS
100111******************************************************************
100111
100111
100111     MOVE 'LA'                   TO LETTER-ARCHIVE
100111
100111     MOVE WS-ARCHIVE-NO          TO LA-ARCHIVE-NO
100111                                    LA-ARCHIVE-NO-A2
100111                                    LA-ARCHIVE-NO-A3
100111                                    LA-ARCHIVE-NO-A4
100111                                    LA-ARCHIVE-NO-A5
100111                                    LA-ARCHIVE-NO-A6
100111
100111     MOVE VD-COMPANY-CD          TO LA-COMPANY-CD
100111                                    LA-COMPANY-CD-A2
100111                                    LA-COMPANY-CD-A3
100111                                    LA-COMPANY-CD-A4
100111                                    LA-COMPANY-CD-A5
100111                                    LA-COMPANY-CD-A6
100111     MOVE VD-CARRIER             TO LA-CARRIER-A2
100111                                    LA-CARRIER-A3
100111                                    LA-CARRIER-A4
100111                                    LA-CARRIER-A5
100111     MOVE VD-GROUPING            TO LA-GROUPING-A2
100111                                    LA-GROUPING-A3
100111                                    LA-GROUPING-A4
100111                                    LA-GROUPING-A5
100111     MOVE VD-ACCOUNT             TO LA-ACCOUNT-A2
100111                                    LA-ACCOUNT-A3
100111                                    LA-ACCOUNT-A4
100111                                    LA-ACCOUNT-A5
100111     MOVE VD-STATE               TO LA-STATE-A2
100111                                    LA-STATE-A3
100111                                    LA-STATE-A4
100111                                    LA-STATE-A5
100111     MOVE VD-CERT-EFF-DT         TO LA-EFFECT-DATE-A2
100111     MOVE VD-CERT-NO             TO LA-CERT-NO-A2
100111
100111     MOVE VD-ENTRY-BATCH         TO LA-ENTRY-A6
100111
100111     MOVE VD-PROC-ID             TO LA-PROCESSOR-CD
100111
100111     MOVE LOW-VALUES             TO LA-LAST-RESENT-PRINT-DATE
100111                                    LA-INITIAL-PRINT-DATE
100111                                    LA-SENT-DATE
100111                                    LA-REPLY-DATE
100111                                    LA-RESEND-DATE
100111                                    LA-FOLLOW-UP-DATE
100111
100111     MOVE 'A'                    TO LA-STATUS
100111     MOVE W-NUMBER-OF-COPIES     TO LA-NO-OF-COPIES
100111     MOVE W-AUTO-CLOSE-IND       TO LA-FINAL-ACT-IND
100111     MOVE VD-LETTER-ID           TO LA-FORM-A3
100111     MOVE SPACES                 TO LA-DATA-SOURCE
100111     MOVE 'B'                    TO LA-ARCHIVE-STATUS
100111     MOVE VD-CURRENT-DATE        TO LA-CREATION-DATE
100111     MOVE 'Y'                    TO LA-VA-DISCLOSURE-IND
100111     MOVE VD-CURRENT-DATE        TO LA-LAST-MAINT-DATE
100111     MOVE VD-CURRENT-TIME        TO LA-LAST-MAINT-TIME
100111     MOVE VD-PROC-ID             TO LA-LAST-UPDATED-BY
100111
100111     MOVE ZEROS                  TO LA-NUMBER-LABEL-LINES
100111                                    LA-NO-OF-TEXT-RECORDS
100111
100111     
      * EXEC CICS WRITE
100111*         DATASET   ('ERARCH')
100111*         FROM      (LETTER-ARCHIVE)
100111*         RIDFLD    (LA-CONTROL-PRIMARY)
100111*         RESP      (WS-RESPONSE)
100111*    END-EXEC
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
           MOVE 'ERARCH' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00003375' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303033333735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 LA-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
100111
100111     IF NOT RESP-NORMAL
100111        SET ARCH-FAIL TO TRUE
100111     END-IF
100111
100111     .
100111 5000-EXIT.
100111     EXIT.
100111
100111
100111 5700-WRITE-EXTR.
100111
100111     PERFORM 5000-BUILD-ERARCH   THRU 5000-EXIT
100111     IF ARCH-FAIL
100111        MOVE ' COULDNT BUILD ERARCH ' TO WS-MESSAGE
100111        GO TO 5700-EXIT
100111     END-IF
100111     MOVE WS-COMPANY-CD          TO NSAS-COMPANY-CD
100111     MOVE WS-ARCHIVE-NO          TO NSAS-ARCHIVE-NO
100111     MOVE +0                     TO NSAS-SEQ-NO
100111     PERFORM 5710-BUILD-NSASEXTR THRU 5710-EXIT
100111     PERFORM 5720-WRITE-NSASEXTR THRU 5720-EXIT
100111     IF ARCH-FAIL
100111        MOVE ' COULDNT BUILD STD NSASEXTR ' TO WS-MESSAGE
100111        GO TO 5700-EXIT
100111     END-IF
100111     IF W-LETTER-TO-ACCT NOT = SPACES
100111        MOVE W-LETTER-TO-ACCT   TO NSAS-LETTER-VARIABLES (5:1)
100111        MOVE WS-COMPANY-CD       TO NSAS-COMPANY-CD
100111        MOVE WS-ARCHIVE-NO       TO NSAS-ARCHIVE-NO
100111        MOVE +1                  TO NSAS-SEQ-NO
100111        PERFORM 5720-WRITE-NSASEXTR
100111                                 THRU 5720-EXIT
100111        IF ARCH-FAIL
100111           MOVE ' COULDNT BUILD ACCT NSASEXTR ' TO WS-MESSAGE
100111           GO TO 5700-EXIT
100111        END-IF
100111     END-IF
100111
100111     IF W-LETTER-TO-BENE NOT = SPACES
100111        MOVE W-LETTER-TO-BENE   TO NSAS-LETTER-VARIABLES (5:1)
100111        MOVE WS-COMPANY-CD       TO NSAS-COMPANY-CD
100111        MOVE WS-ARCHIVE-NO       TO NSAS-ARCHIVE-NO
100111        MOVE +2                  TO NSAS-SEQ-NO
100111        PERFORM 5720-WRITE-NSASEXTR
100111                                 THRU 5720-EXIT
100111        IF ARCH-FAIL
100111           MOVE ' COULDNT BUILD BENE NSASEXTR ' TO WS-MESSAGE
100111        END-IF
100111     END-IF
100111
100111     .
100111 5700-EXIT.
100111     EXIT.
100111
100111 5710-BUILD-NSASEXTR.
100111
100111***  i added the extra space in front of the 1st ~
100111***  only because there may be a letter to acct or bene
100111
100111     STRING
100111         VA-LETTER                ' ~'
100111         VA-PROC-ID               '~'
100111         VA-PROC-NAME             '~'
100111         VA-PROC-TITLE            '~'
100111         VA-CSR-CODE              '~'
100111         VA-CSR-NAME              '~'
100111         VA-CSR-TITLE             '~'
100111         VA-CARRIER               '~'
100111         VA-GROUPING              '~'
100111         VA-STATE                 '~'
100111         VA-ACCOUNT               '~'
100111         VA-EFFDATE               '~'
100111         VA-CERT-NO               '~'
100111         VA-FIRSTNAME             '~'
100111         VA-MIDINIT               '~'
100111         VA-LASTNAME              '~'
100111         VA-ADDR1                 '~'
100111         VA-ADDR2                 '~'
100111         VA-ADDR-CITY             '~'
100111         VA-ADDR-ST               '~'
100111         VA-ADDR-ZIP              '~'
100111         VA-JNT-FIRSTNAME         '~'
100111         VA-JNT-MIDINIT           '~'
100111         VA-JNT-LASTNAME          '~'
100111         VA-ARCH-NO               '~'
100111         VA-ENC-LINE              '~'
100111         VA-ATTACH                '~'
100111         VA-STACK                 '~'
100111         VA-PRINT-NOW             '~'
100111         VA-BORROWER              '~'
100111         VA-BENE                  '~'
100111         VA-ACCT-NAME             '~'
100111         VA-SL                    '~'
100111         VA-JL                    '~'
100111         VA-SA                    '~'
100111         VA-JA                    '~'
100111         VA-GD                    '~'
100111         VA-NP                    '~'
100111         VA-7R                    '~'
100111         VA-LL                    '~'
100111         VA-TN                    '~'
100111         VA-14R                   '~'
100111         VA-14E                   '~'
100111         VA-30R                   '~'
100111         VA-30E                   '~'
100111         VA-CL-NET-AMT            '~'
100111         VA-CL-GRS-AMT            '~'
100111         VA-CL-LEV-AMT            '~'
100111         VA-CL-NET-PMT            '~'
100111         VA-CL-GRS-PMT            '~'
100111         VA-CL-LEV-PMT            '~'
100111         VA-CL-NET-PRM            '~'
100111         VA-CL-GRS-PRM            '~'
100111         VA-CL-LEV-PRM            '~'
100111         VA-CD-NET-AMT            '~'
100111         VA-CD-GRS-AMT            '~'
100111         VA-CD-LEV-AMT            '~'
100111         VA-CD-NET-PMT            '~'
100111         VA-CD-GRS-PMT            '~'
100111         VA-CD-LEV-PMT            '~'
100111         VA-CD-NET-PRM            '~'
100111         VA-CD-GRS-PRM            '~'
100111         VA-CD-LEV-PRM            '~'
100111         VA-CLD-NET-AMT           '~'
100111         VA-CLD-GRS-AMT           '~'
100111         VA-CLD-LEV-AMT           '~'
100111         VA-CLD-NET-PMT           '~'
100111         VA-CLD-GRS-PMT           '~'
100111         VA-CLD-LEV-PMT           '~'
100111         VA-CLD-NET-PRM           '~'
100111         VA-CLD-GRS-PRM           '~'
100111         VA-CLD-LEV-PRM           '~'
100111         VA-NO-NET-AMT            '~'
100111         VA-NO-GRS-AMT            '~'
100111         VA-NO-LEV-AMT            '~'
100111         VA-NO-NET-PMT            '~'
100111         VA-NO-GRS-PMT            '~'
100111         VA-NO-LEV-PMT            '~'
100111         VA-NO-NET-PRM            '~'
100111         VA-NO-GRS-PRM            '~'
100111         VA-NO-LEV-PRM            '~'
100111            DELIMITED BY '  ' INTO NSAS-LETTER-VARIABLES
100111     END-STRING
100111
100111     .
100111 5710-EXIT.
100111     EXIT.
100111
100111 5720-WRITE-NSASEXTR.
100111
100111     
      * EXEC CICS WRITE
100111*       DATASET    ('NSASEXTR')
100111*       FROM       (NSAS-EXTRACT-RECORD)
100111*       RIDFLD     (NSAS-CONTROL-PRIMARY)
100111*       RESP       (WS-RESPONSE)
100111*    END-EXEC
           MOVE LENGTH OF
            NSAS-EXTRACT-RECORD
             TO DFHEIV11
           MOVE 'NSASEXTR' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00003532' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303033353332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 NSAS-EXTRACT-RECORD, 
                 DFHEIV11, 
                 NSAS-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
100111
100111     IF NOT RESP-NORMAL
100111        SET ARCH-FAIL TO TRUE
100111     END-IF
100111
100111     .
100111 5720-EXIT.
100111     EXIT.
100111
       8000-RETURN.
           
      * EXEC CICS RETURN
      *    END-EXEC
      *    MOVE '.(                    ''   #00003548' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           
      * GOBACK

           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELVADS' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK
           .
       9700-DATE-LINK.
           MOVE LINK-ELDATCV           TO PGM-NAME
           
      * EXEC CICS LINK
      *       PROGRAM    (PGM-NAME)
      *       COMMAREA   (DATE-CONVERSION-DATA)
      *       LENGTH     (DC-COMM-LENGTH)
      *    END-EXEC
      *    MOVE '."C                   (   #00003554' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       9700-EXIT.
           EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELVADS' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELVADS' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
