      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL1278.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 04/21/94 14:09:26.
00007 *                            VMOD=2.030.
00008 *
00008 *
00009 *AUTHOR.        LOGIC,INC.
00010 *               DALLAS, TEXAS.
00011
00012 *DATE-COMPILED.
00013
00014 *SECURITY.   *****************************************************
00015 *            *                                                   *
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *
00021 *            *                                                   *
00022 *            *****************************************************
00023 *
00024 *REMARKS.
00025 *        TRANSACTION - EXXA - CANCELLATION QUOTE.
00023 *
101201******************************************************************
101201*                   C H A N G E   L O G
101201*
101201* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101201*-----------------------------------------------------------------
101201*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101201* EFFECTIVE    NUMBER
101201*-----------------------------------------------------------------
101201* 101201    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
070105* 070105    2005063000003  PEMA  COMMENT OUT CODE THAT ADDS 1
070104*         TO THE TERM AND REMAINING TERM
092705* 092705  CR2005050300006  PEMA  ADD SPP LEASES
081606* 081606  CR2006051800002  PEMA  ADD POST CARD PROCESSING
111907* 111907  CR2004020600011  PEMA  FORCE FP TO REFUND AS FP
022608* 022608  CR2008010200006  PEMA  CHANGE MO RTERM MTHOD
050508* 050508  CR2008010200004  PEMA  FIX MO REFUNDING
090408* 090408  CR2008081900002  PEMA  UPDATE MO REFUNDING
021710* 021710  IR2010021000002  PEMA  ADD CANCEL DATE EDIT
101110* 101110  CR2010012700001  PEMA ADD DDF REFUND/UEP PROCESSING
042011* 042011  CR2010030900001  PEMA ADD CLM PROCESSING ON QUOTES
112911* 112911  CR2011083000003  PEMA  ADD SPPDDF TRUNCATED
010412* 010412  CR2011022800001  AJRA  NAPERSOFT CANCEL PROCESSING
032912* 032912  CR2011110200001  PEMA  AHL CHANGES
062712* 062712  CR2011022800001  AJRA NAPERSOFT
071712* 071712  CR2011022800001  AJRA  NAPERSOFT CANCELS
102212* 102212  CR2012013100001  PEMA  ALLOW MN OVERRIDE RF
112612* 112612  CR2012101700002  AJRA  FIX CANCEL WHEN DEATH CLAIM
121712* 121712  CR2012101700002  AJRA  ADD DEFAULT AGE FLAG
011413* 011413  IR2012122700003  AJRA  ADD CRTO ISSUE/CANCEL INDICATOR
050713* 050713  CR2011062300001  PEMA  REM CHG FOR EXT DYS FOR ROA REF
041514* 041514  CR2012110200003  AJRA  REMOVE PF11 FROM SCREEN,
041514*                                CANCEL QUOTE SCRIPT WILL DO PF11
010816* 010816  IR2015092900001  PEMA  USE CLP STATE WHERE NEEDED
020816* 020816  CR2015082500001  PEMA  ADD NEW VPP COMPANY
072616* 072616  CR2016020400001  PEMA  ADD PF10 CONFIRMATION
032117* 032117  CR2017030300002  PEMA  ADD COMM % AND COMM AMT
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
041320* 041320  CR2020030500002  PEMA  Distinguish between iss and canc
070622* 070622  CR2020061200002  TANA  Add cancel reason logic
091322* 091322  IR2022081700001  SJAA  Set fld len of can rea upon error
101201******************************************************************
00026  EJECT
00027  ENVIRONMENT DIVISION.
00028  DATA DIVISION.
00029  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00030  77  FILLER  PIC  X(32) VALUE '********************************'.
00031  77  FILLER  PIC  X(32) VALUE '*    EL1278 WORKING STORAGE    *'.
00032  77  FILLER  PIC  X(32) VALUE '*********** VMOD=2.030 *********'.
081606 77  S1                          PIC S999 COMP-3 VALUE +0.
       77  P1                          PIC S999 COMP-3 VALUE +0.
       77  P2                          PIC S999 COMP-3 VALUE +0.
       77  C0                          PIC S999 COMP-3 VALUE +0.
       77  C1                          PIC S999 COMP-3 VALUE +0.
       77  C2                          PIC S999 COMP-3 VALUE +0.
       77  C3                          PIC S999 COMP-3 VALUE +0.
       77  WS-STOP-SW                  PIC X  VALUE ' '.
           88  I-SAY-TO-STOP      VALUE 'Y'.
       77  WS-OPEN-LF-CLAIM            PIC X  VALUE ' '.
           88  OPEN-LF-CLAIM              VALUE 'Y'.
       77  WS-CLOSED-LF-CLAIM          PIC X  VALUE ' '.
           88  CLOSED-LF-CLAIM            VALUE 'Y'.
       77  WS-OPEN-AH-CLAIM            PIC X  VALUE ' '.
           88  OPEN-AH-CLAIM              VALUE 'Y'.
       77  WS-CLOSED-AH-CLAIM          PIC X  VALUE ' '.
           88  CLOSED-AH-CLAIM            VALUE 'Y'.
       77  WS-FOUND-A-CLAIM            PIC X  VALUE ' '.
           88  FOUND-A-CLAIM              VALUE 'Y'.
100518 77  WS-CLAIM-TYPE               PIC X  VALUE ' '.
100518     88  LIFE-CLAIM                 VALUE 'L'.
100518     88  OTHER-CLAIM                VALUE 'O'.
100518 77  WS-CLAIM-LIT                PIC X(06)  VALUE SPACES.
       77  WS-ST-REF-IND               PIC X.
       77  WS-LF-INCUR-DT              PIC XX  VALUE LOW-VALUES.
       77  WS-AH-INCUR-DT              PIC XX  VALUE LOW-VALUES.
       77  WS-AH-PAID-THRU-DT          PIC XX  VALUE LOW-VALUES.
       77  WS-LF-LAST-CLOSE-DT         PIC XX  VALUE LOW-VALUES.
       77  WS-AH-LAST-CLOSE-DT         PIC XX  VALUE LOW-VALUES.
       77  DD-IU-SW                    PIC X   VALUE ' '.
           88  DD-IU-PRESENT                 VALUE 'Y'.
       77  WS-DDF-COMM-AND-MFEE        PIC S9(5)V99 VALUE +0 COMP-3.
       77  WS-DDF-ADMIN-FEES           PIC S9(5)V99 VALUE +0 COMP-3.
       77  WS-DDF-CSO-ADMIN-FEE        PIC S9(5)V99 VALUE +0 COMP-3.
       77  WS-DDF-1ST-YR-TOT-EXP       PIC S9(5)V99 VALUE +0 COMP-3.
       77  WS-STATE-EXT-DAYS-CHG       PIC X  VALUE ' '.
       77  TEX-FACT-8                  PIC S9V9(6)     COMP-3.
       77  WS-COMM-PCT                 PIC S9(5)V9(5)  COMP-3 VALUE +0.
       77  WS-TERM                     PIC S999 COMP-3 VALUE +0.
       77  WS-CERT-UPDATE-SW           PIC X  VALUE ' '.
           88  NO-CERT-RW                 VALUE 'N'.
           88  CERT-RW                    VALUE 'Y'.
062712 77  WS-ERMAIL-SW                PIC X  VALUE ' '.
062712     88  ERMAIL-FOUND                 VALUE 'Y'.
121712 77  WS-CERT-TRL-REC-NOT-FOUND   PIC S9       VALUE ZERO.
070622 77  WS-ATT-AGE                  PIC S9(3)V99    COMP-3 VALUE +0.
070622 77  WS-EDIT-AGE                 PIC S999       COMP-3 VALUE ZERO.
070622 EXEC SQL
070622    INCLUDE SQLDA
070622 END-EXEC
070622
070622 EXEC SQL
070622    INCLUDE SQLCA
070622 END-EXEC
070622
070622 EXEC SQL
070622    BEGIN DECLARE SECTION
070622 END-EXEC
070622
070622 01  SQLCMD                      PIC X(1024).
070622 01  SVR                         PIC X(32).
070622 01  USR                         PIC X(32).
070622 01  PASS                        PIC X(32).
070622 01  USR-PASS                    PIC X(64).
070622
070622 01  WS-SQL-DATA.
070622     05  WS-CYCLE-DATE           PIC X(10).
070622     05  WS-NEXT-BUS-DT          PIC X(10).
070622     05  WS-LOOKUPID             PIC X(4).
070622     05  WS-LOOKUPNAME           PIC X(4).
070622     05  WS-LOOKUP-VALUE         PIC X(100).
070622     05  WS-CARRIER              PIC X.
070622     05  WS-GROUP                PIC X(6).
070622     05  WS-STATE                PIC XX.
070622     05  WS-ACCOUNT              PIC X(10).
070622     05  WS-EFF-DT               PIC XX.
070622     05  WS-CERT-NO              PIC X(10).
070622     05  WS-CERT-NO-SUF          PIC X(01).
070622
070622 EXEC SQL
070622    END DECLARE SECTION
070622 END-EXEC
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
00033
00034 *                            COPY ELCSCTM.
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
00035
00036 *                            COPY ELCSCRTY.
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
00037
00038  01  FILLER          COMP-3.
00039      12  WS-BALLOON-RTRM         PIC S9(03)      VALUE ZERO.
00039      12  WS-ELAPSED-MONTHS       PIC S9(03)      VALUE ZERO.
00040      12  WS-TOT-AH-PREM          PIC S9(9)V99    VALUE ZERO.
00041      12  WS-TOT-LF-PREM          PIC S9(9)V99    VALUE ZERO.
00042      12  WS-TOT-AH-RFND          PIC S9(9)V99    VALUE ZERO.
00043      12  WS-TOT-LF-RFND          PIC S9(9)V99    VALUE ZERO.
00044      12  WS-TOT-PREM             PIC S9(9)V99    VALUE ZERO.
00045      12  WS-TOT-RFND             PIC S9(9)V99    VALUE ZERO.
00046      12  WS-TOT-ITDR             PIC S9(9)V99    VALUE ZERO.
00047
00048  01  WS-DATE-AREA.
00049      12  SAVE-DATE               PIC  X(08)      VALUE SPACES.
00050      12  SAVE-BIN-DATE           PIC  X(02)      VALUE SPACES.
00051      12  WS-LF-CANCEL-DATE       PIC  X(02)      VALUE SPACES.
00052      12  WS-AH-CANCEL-DATE       PIC  X(02)      VALUE SPACES.
00053      12  WS-LF-CANCEL-DATE-ED    PIC  X(08)      VALUE SPACES.
00054      12  WS-AH-CANCEL-DATE-ED    PIC  X(08)      VALUE SPACES.
00055
       01  ERPDEF-KEY-SAVE             PIC X(18).
       01  ERPDEF-KEY.
           12  ERPDEF-COMPANY-CD       PIC X.
           12  ERPDEF-STATE            PIC XX.
           12  ERPDEF-PROD-CD          PIC XXX.
           12  F                       PIC X(7).
           12  ERPDEF-BEN-TYPE         PIC X.
           12  ERPDEF-BEN-CODE         PIC XX.
           12  ERPDEF-EXP-DT           PIC XX.
       01  ELMSTR-KEY.
           12  ELMSTR-COMP-CD          PIC X.
           12  ELMSTR-CERT-NO          PIC X(11).
121712 01  ELCRTT-KEY.
121712     12  ELCRTT-PRIMARY          PIC X(33).
121712     12  ELCRTT-REC-TYPE         PIC X(1).
121712
062712 01  ELCRTO-KEY.
062712     05  ELCRTO-COMPANY-CD       PIC X.
062712     05  ELCRTO-CARRIER          PIC X.
062712     05  ELCRTO-GROUPING         PIC X(6).
062712     05  ELCRTO-STATE            PIC XX.
062712     05  ELCRTO-ACCOUNT          PIC X(10).
062712     05  ELCRTO-CERT-EFF-DT      PIC XX.
062712     05  ELCRTO-CERT-NO.
062712         10  ELCRTO-CERT-PRIME   PIC X(10).
062712         10  ELCRTO-CERT-SFX     PIC X.
062712     05  ELCRTO-RECORD-TYPE      PIC X.
062712     05  ELCRTO-SEQ-NO           PIC 9(4)  BINARY.
062712
00056  01  STANDARD-AREAS.
00057      12  SC-ITEM                 PIC S9(04) COMP VALUE +0001.
00058      12  SUB3                    PIC S9(04) COMP VALUE +0.
00059      12  SUB4                    PIC S9(04) COMP VALUE +0.
00060      12  GETMAIN-SPACE           PIC  X(01)      VALUE SPACE.
00061      12  MAP-NAME                PIC  X(08)      VALUE 'EL127H'.
00062      12  MAPSET-NAME             PIC  X(08)      VALUE 'EL1278S'.
00063      12  SCREEN-NUMBER           PIC  X(04)      VALUE '127H'.
00064      12  TRANS-ID                PIC  X(04)      VALUE 'EXXA'.
00065      12  THIS-PGM                PIC  X(08)      VALUE 'EL1278'.
00066      12  PGM-NAME                PIC  X(08).
00067      12  TIME-IN                 PIC S9(07).
00068      12  TIME-OUT-R  REDEFINES  TIME-IN.
00069          16  FILLER              PIC  X(01).
00070          16  TIME-OUT            PIC  9(02)V99.
00071          16  FILLER              PIC  X(02).
00072      12  XCTL-005                PIC  X(08)      VALUE 'EL005'.
00073      12  XCTL-010                PIC  X(08)      VALUE 'EL010'.
00074      12  XCTL-126                PIC  X(08)      VALUE 'EL126'.
00075      12  LINK-001                PIC  X(08)      VALUE 'EL001'.
00076      12  LINK-004                PIC  X(08)      VALUE 'EL004'.
00077      12  LINK-ELDATCV            PIC  X(08)      VALUE 'ELDATCV'.
00078      12  ELRTRM-ID               PIC  X(08)      VALUE 'ELRTRM'.
00079      12  ELRAMT-ID               PIC  X(08)      VALUE 'ELRAMT'.
00080      12  ELRFND-ID               PIC  X(08)      VALUE 'ELRFND'.
00081      12  ELCNTL-ID               PIC  X(08)      VALUE 'ELCNTL'.
00082      12  ERACCT-ID               PIC  X(08)      VALUE 'ERACCT'.
00083      12  ELCERT-ID               PIC  X(08)      VALUE 'ELCERT'.
081606     12  ERMAIL-ID               PIC  X(08)      VALUE 'ERMAIL'.
00084      12  ERFORM-ID               PIC  X(08)      VALUE 'ERFORM'.
00085      12  ERNOTE-ID               PIC  X(08)      VALUE 'ERCNOT'.
121712     12  ELCRTT-ID               PIC  X(08)      VALUE 'ELCRTT'.
00086      12  DATE-RANGE-SW           PIC  X(01)      VALUE SPACE.
00087      12  WS-REFUND-SEARCH-SW     PIC  X(01)      VALUE ' '.
00088          88  LIFE-REFUND-SEARCH                  VALUE 'L'.
00089      12  WS-BROWSE-STARTED-SW    PIC  X(01)      VALUE SPACE.
00090          88  BROWSE-STARTED                      VALUE 'Y'.
00091      12  WS-FORM-RECORD-SW       PIC  X(01)      VALUE SPACE.
00092          88  FORM-FOUND                          VALUE 'Y'.
00093      12  BEN-SEARCH-SW           PIC  X(01)      VALUE SPACE.
00094          88  NO-BENEFIT-FOUND                    VALUE 'N'.
00095      12  WS-CF-LF-COVERAGE-TYPE  PIC  X(01)      VALUE SPACE.
00096          88  WS-REDUCING                         VALUE 'R'.
00097          88  WS-LEVEL                            VALUE 'L'  'P'.
00098      12  WS-NEW-LF-CANCEL-SW     PIC  X(01)      VALUE 'N'.
00099          88  WS-NEW-LF-CANCEL                    VALUE 'Y'.
00100      12  WS-NEW-AH-CANCEL-SW     PIC  X(01)      VALUE 'N'.
00101          88  WS-NEW-AH-CANCEL                    VALUE 'Y'.
00102      12  WS-BENEFIT-DESCRIP      PIC  X(10)      VALUE SPACES.
00103      12  WS-PMT-DESCRIP          PIC  X(10)      VALUE
00104          'CLAIM PMTS'.
00105      12  WS-REFUND-DESCRIP       PIC  X(10)      VALUE
00106          'ITD REFUND'.
00107      12  WS-ACCESS.
00108          16  FILLER              PIC  X(02).
00109          16  WS-BEN-CD           PIC  X(02).
00110      12  WS-KIND                 PIC  X(03)      VALUE SPACES.
           12  WS-LF-KIND              PIC  X(03)      VALUE SPACES.
           12  WS-AH-KIND              PIC  X(03)      VALUE SPACES.
00111      12  WS-LOOKUP-TYPE          PIC  X(01).
00112      12  WS-BENEFIT-NO           PIC  X(02)      VALUE ZERO.
00113      12  WS-STATE-ABBREVIATION   PIC  X(02)      VALUE SPACES.
00114      12  WS-CALC-CD              PIC  X(01).
00115      12  DEEDIT-FIELD            PIC  X(15).
00116      12  DEEDIT-FIELD-V0  REDEFINES
00117          DEEDIT-FIELD            PIC S9(15).
00118      12  WS-CURRENT-DATE         PIC  X(02)  VALUE LOW-VALUES.
00119      12  RETURN-FROM             PIC  X(08).
00120      12  QID.
00121          16  QID-TERM            PIC  X(04).
00122          16  FILLER              PIC  X(04)      VALUE '127H'.
00123      12  WS-FIRST-ENTRY-SW       PIC  X(01)      VALUE SPACE.
00124          88  FIRST-ENTRY                         VALUE 'Y'.
           12  WS-PDEF-RECORD-SW       PIC X           VALUE ' '.
               88  PDEF-FOUND                          VALUE 'Y'.
00125      12  WS-CERT-RECORD-SW       PIC  X(01)      VALUE SPACE.
00126          88  CERT-FOUND                          VALUE 'Y'.
00127          88  CERT-NOT-FOUND                      VALUE 'N'.
00128      12  WS-CNTL-RECORD-SW       PIC  X(01)      VALUE SPACE.
00129          88  CNTL-FOUND                          VALUE 'Y'.
00130          88  CNTL-NOT-FOUND                      VALUE 'N'.
00131      12  WS-BEN-RECORD-SW        PIC  X(01)      VALUE SPACE.
00132          88  BEN-FOUND                           VALUE 'Y'.
00133      12  WS-STATE-RECORD-SW      PIC  X(01)      VALUE SPACE.
00134          88  STATE-FOUND                         VALUE 'Y'.
00135      12  WS-ST-BEN-RECORD-SW     PIC  X(01)      VALUE SPACE.
00136          88  ST-BEN-FOUND                        VALUE 'Y'.
00137      12  WS-ACCT-RECORD-SW       PIC  X(01)      VALUE SPACE.
00138          88  ACCT-FOUND                          VALUE 'Y'.
00139      12  WS-DUPREC-SW            PIC  X(01)      VALUE SPACE.
00140          88  DUPLICATE-RECORD-FOUND              VALUE 'Y'.
00141      12  WS-NOT-FOUND            PIC S9(01) COMP-3
00142                                                  VALUE ZERO.
00143          88  BENEFIT-FOUND                       VALUE +1.
00144      12  WS-CALC-REFUND          PIC S9(07)V99 COMP-3
00145                                                  VALUE ZERO.
00146      12  WS-LIFE-PREMIUM         PIC S9(07)V99 COMP-3
00147                                                  VALUE ZERO.
00148      12  WS-INDEX                PIC S9(04) COMP VALUE ZERO.
00149      12  WS-SUB1                 PIC S99    COMP.
00150      12  WS-ACCT-USER-FLD-5      PIC  X(02).
00151      12  WS-CF-DEFAULT-APR       PIC  S9(03)V9(04) COMP-3.
00152      12  WS-CF-CR-R78-METHOD     PIC  X(01)      VALUE SPACE.
00153      12  WS-CF-CR-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
00154      12  WS-AH-SPECIAL-CALC-CD   PIC  X(01)      VALUE SPACE.
           12  WS-AH-BEN-CATEGORY      PIC  X          VALUE SPACE.
00155      12  WS-LF-SPECIAL-CALC-CD   PIC  X(01)      VALUE SPACE.
00156      12  WS-AH-CO-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
00157      12  WS-LF-CO-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
00158      12  WS-AH-CO-EARNINGS-CALC  PIC  X(01)      VALUE SPACE.
00159      12  WS-LF-CO-EARNINGS-CALC  PIC  X(01)      VALUE SPACE.
00160      12  WS-AH-CO-REFUND-CALC    PIC  X(01)      VALUE SPACE.
00161      12  WS-LF-CO-REFUND-CALC    PIC  X(01)      VALUE SPACE.
00162      12  WS-AH-ST-REFUND-CALC    PIC  X(01)      VALUE SPACE.
00163      12  WS-LF-ST-REFUND-CALC    PIC  X(01)      VALUE SPACE.
00164      12  WS-AH-FO-REFUND-CALC    PIC  X(01)      VALUE SPACE.
00165      12  WS-LF-FO-REFUND-CALC    PIC  X(01)      VALUE SPACE.
00166      12  WS-AH-ST-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
00167      12  WS-LF-ST-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
00168      12  WS-AH-AM-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
00169      12  WS-LF-AM-REM-TERM-CALC  PIC  X(01)      VALUE SPACE.
00170      12  WS-AM-EARN-METHOD-A     PIC  X(01)      VALUE SPACE.
00171      12  WS-AM-EARN-METHOD-L     PIC  X(01)      VALUE SPACE.
00172      12  WS-AM-EARN-METHOD-R     PIC  X(01)      VALUE SPACE.
00173      12  ERROR-MESSAGES.
00174          16  ER-0000             PIC  X(04)      VALUE '0000'.
00175          16  ER-0004             PIC  X(04)      VALUE '0004'.
00176          16  ER-0008             PIC  X(04)      VALUE '0008'.
00177          16  ER-0028             PIC  X(04)      VALUE '0028'.
00178          16  ER-0029             PIC  X(04)      VALUE '0029'.
00179          16  ER-0070             PIC  X(04)      VALUE '0070'.
00180          16  ER-0142             PIC  X(04)      VALUE '0142'.
00180          16  ER-0214             PIC  X(04)      VALUE '0214'.
00181          16  ER-0681             PIC  X(04)      VALUE '0681'.
00182          16  ER-0682             PIC  X(04)      VALUE '0682'.
00183          16  ER-0683             PIC  X(04)      VALUE '0683'.
00184          16  ER-0684             PIC  X(04)      VALUE '0684'.
070622         16  ER-1590             PIC  X(04)      VALUE '1590'.
00185          16  ER-2227             PIC  X(04)      VALUE '2227'.
00186          16  ER-2601             PIC  X(04)      VALUE '2601'.
00187          16  ER-2616             PIC  X(04)      VALUE '2616'.
00188          16  ER-2617             PIC  X(04)      VALUE '2617'.
00189          16  ER-2619             PIC  X(04)      VALUE '2619'.
00190          16  ER-2740             PIC  X(04)      VALUE '2740'.
               16  ER-2756             PIC  X(04)      VALUE '2756'.
               16  ER-2768             PIC  X(04)      VALUE '2768'.
               16  ER-2774             PIC  X(04)      VALUE '2774'.
               16  ER-2775             PIC  X(04)      VALUE '2775'.
               16  ER-2965             PIC  X(04)      VALUE '2965'.
               16  ER-2966             PIC  X(04)      VALUE '2966'.
               16  ER-2967             PIC  X(04)      VALUE '2967'.
               16  ER-2968             PIC  X(04)      VALUE '2968'.
               16  ER-2969             PIC  X(04)      VALUE '2969'.
               16  ER-2998             PIC  X(04)      VALUE '2998'.
               16  ER-3037             PIC  X(04)      VALUE '3037'.
               16  ER-3038             PIC  X(04)      VALUE '3038'.
               16  ER-3039             PIC  X(04)      VALUE '3039'.
00191          16  ER-3780             PIC  X(04)      VALUE '3780'.
00192          16  ER-3781             PIC  X(04)      VALUE '3781'.
062712         16  ER-3830             PIC  X(04)      VALUE '3830'.
072616         16  er-3847             pic  x(04)      value '3847'.
00193          16  ER-8160             PIC  X(04)      VALUE '8160'.
00194          16  ER-8161             PIC  X(04)      VALUE '8161'.
               16  ER-9999             PIC  X(04)      VALUE '9999'.
00195      12  WS-CF-LIFE-OVERRIDE-L1  PIC  X(01).
00196      12  WS-CF-LIFE-OVERRIDE-L2  PIC  X(02).
00197      12  WS-CF-AH-OVERRIDE-L1    PIC  X(01).
00198      12  WS-CF-AH-OVERRIDE-L2    PIC  X(02).
00199
00200      12  WS-RESPONSE             PIC S9(8)       COMP.
00201          88  WS-RESP-NORMAL                      VALUE +00.
00202          88  WS-RESP-NOTFND                      VALUE +13.
               88  WS-RESP-DUPREC                      VALUE +14.
               88  WS-RESP-DUPKEY                      VALUE +15.
00203          88  WS-RESP-NOTOPEN                     VALUE +19.
               88  WS-RESP-ENDFILE                     VALUE +20.
010412
010412     12  WS-01-CANCEL-ERR        PIC X(25)
010412         VALUE 'CANCEL DATE ERROR'.
010412     12  WS-02-CANCEL-ERR        PIC X(25)
010412         VALUE 'CERT NOT FOUND'.
010412     12  WS-04-CANCEL-ERR        PIC X(25)
010412         VALUE 'CANCEL AMOUNT ERROR'.
010412     12  WS-05-CANCEL-ERR        PIC X(25)
010412         VALUE 'CANCEL OPTION ERROR'.
010412     12  WS-06-CANCEL-ERR        PIC X(25)
010412         VALUE 'PREVIOUSLY CANCELLED'.
010412     12  WS-07-CANCEL-ERR        PIC X(25)
010412         VALUE 'INVALID DATA'.
010412     12  WS-08-CANCEL-ERR        PIC X(25)
010412         VALUE 'NO ACCOUNT MASTER'.
010412     12  WS-09-CANCEL-ERR        PIC X(25)
010412         VALUE 'SUFFIX ALREADY EXISTS'.
010412     12  WS-99-CANCEL-ERR        PIC X(25)
010412         VALUE 'MISC CANCEL ERROR'.
00204
00205  01  WS-SAVE-ALLOWABLE-BENEFIT.
00206      16  WS-SAVE-BENEFIT-CODE      PIC XX.
00207      16  WS-SAVE-BENEFIT-TYPE      PIC X.
00208      16  WS-SAVE-BENEFIT-REVISION  PIC XXX.
00209      16  WS-SAVE-BENEFIT-REM-TERM  PIC X.
00210      16  WS-SAVE-BENEFIT-RETRO-Y-N PIC X.
00211      16  FILLER                    PIC XX.
       01  CTBL-KEY-SAVE               PIC X(5).
       01  CTBL-KEY.
           05  CTBL-COMPANY-CD         PIC X.
           05  CTBL-TABLE              PIC XXX.
           05  CTBL-BEN-TYPE           PIC X.
           05  CTBL-BEN-CODE           PIC XX.
       01  CANCEL-GEN-PASS-AREA.
           05  CG-OPTION-CODE          PIC X.
               88  CG-VALID-OPTION       VALUE '1' '2' '3'.
               88  CG-FLAT-CANCEL        VALUE '1'.
               88  CG-CANCEL             VALUE '2'.
               88  CG-CANCEL-REISSUE     VALUE '3'.
           05  CG-ERROR-CODE           PIC 99.
               88  CG-SUCCESS            VALUE 00.
               88  CG-DATE-ERROR         VALUE 01.
               88  CG-CERT-NOT-FOUND     VALUE 02.
               88  CG-AMOUNT-ERROR       VALUE 04.
               88  CG-OPTION-ERROR       VALUE 05.
               88  CG-PREV-CAN           VALUE 06.
               88  CG-INVALID-DATA       VALUE 07.
               88  CG-NO-ACCT-MSTR       VALUE 08.
               88  CG-SFX-A-EXIST        VALUE 09.
               88  CG-MISC-ERROR         VALUE 99.
           05  CG-COMPANY-ID           PIC XXX.
           05  CG-PROC-ID              PIC XXXX.
           05  CG-CURRENT-DT           PIC XX.
           05  CG-MONTH-END-DT         PIC XX.
           05  CG-CERT-KEY.
               10  CG-CERT-COMPANY-CD  PIC X.
               10  CG-CERT-CARRIER     PIC X.
               10  CG-CERT-GROUP       PIC X(6).
               10  CG-CERT-STATE       PIC XX.
               10  CG-CERT-ACCOUNT     PIC X(10).
               10  CG-CERT-EFF-DT      PIC XX.
               10  CG-CERT-CERT-NO     PIC X(11).
           05  CG-LF-CAN-DATA.
               10  CG-LF-CAN-DT        PIC XX.
               10  CG-LF-CAN-AMT       PIC S9(7)V99 COMP-3.
           05  CG-AH-CAN-DATA.
               10  CG-AH-CAN-DT        PIC XX.
               10  CG-AH-CAN-AMT       PIC S9(7)V99 COMP-3.
           05  CG-CERT-PROFILE-DATA.
               10  CG-INS-LNAME        PIC X(15).
               10  CG-INS-FNAME        PIC X(10).
               10  CG-INS-MID-INIT     PIC X.
               10  CG-INS-AGE          PIC 99.
               10  CG-JNT-LNAME        PIC X(15).
               10  CG-JNT-FNAME        PIC X(10).
               10  CG-JNT-MID-INIT     PIC X.
               10  CG-JNT-AGE          PIC 99.
           05  CG-LF-ISS-DATA.
               10  CG-LF-BENCD         PIC XX.
               10  CG-LF-PREM-AMT      PIC S9(7)V99 COMP-3.
072312         10  CG-LF-ALT-PREM-AMT  PIC S9(7)V99 COMP-3.
           05  CG-AH-ISS-DATA.
               10  CG-AH-BENCD         PIC XX.
               10  CG-AH-PREM-AMT      PIC S9(7)V99 COMP-3.
           05  CG-BATCH-NO             PIC X(6).
010412     05  CG-BATCH-SEQ-NO         PIC 9(4)  COMP.
072312     05  CG-DCC-REASON-CD        PIC X.
122712     05  CG-COMM-PCT-ZERO        PIC X.
100917     05  cg-vin                  pic x(17).
101918     05  cg-from-where           pic x(6).
100917     05  FILLER                  PIC X(281).
00213  01  WS-CAN-QUOTE-NOTE.
041514     12  WS-CN-CANCEL-DT         PIC  X(09).
00215      12  FILLER                  PIC  X(10)      VALUE
00216          'RF QUOTE: '.
00217      12  WS-CN-LIFE-AMT-RF       PIC  Z,ZZ9.99.
00218      12  FILLER                  PIC  X(4)       VALUE
00219          ' LF '.
00220      12  WS-CN-LF-REF-METH       PIC  X.
00221      12  FILLER                  PIC  X          VALUE ','.
00222      12  WS-CN-AH-AMT-RF         PIC  Z,ZZ9.99.
00223      12  FILLER                  PIC  X(5)       VALUE
00224          ' AH '.
00225      12  WS-CN-AH-REF-METH       PIC  X.
00226      12  FILLER                  PIC  XX         VALUE ','.
00227      12  FILLER                  PIC  X(6)       VALUE
00228          'TOTAL '.
00229      12  WS-CN-TOTAL-RF          PIC  Z,ZZ9.99.
00230      12  FILLER                  PIC  X(01)      VALUE ' '.
041514     12  WS-CN-DT-QUOTED         PIC  X(08).
00232      12  FILLER                  PIC  X(01)      VALUE ';'.
00233      12  WS-CN-PROCESSOR-ID      PIC  X(04).
00234
00235  01  WS-CM-CONTROL-PRIMARY.
00236      12  WS-CM-COMPANY-CD        PIC  X(01).
00237      12  WS-CM-CARRIER           PIC  X(01).
00238      12  WS-CM-GROUPING          PIC  X(06).
00239      12  WS-CM-STATE             PIC  X(02).
00240      12  WS-CM-ACCOUNT           PIC  X(10).
00241      12  WS-CM-CERT-EFF-DT       PIC  X(02).
00242      12  WS-CM-CERT-NO.
00243          16  WS-CM-CERT-PRIME    PIC  X(10).
00244          16  WS-CM-CERT-SFX      PIC  X(01).
00245
081606 01  WS-MA-CONTROL-PRIMARY.
081606     12  WS-MA-COMPANY-CD        PIC  X(01).
081606     12  WS-MA-CARRIER           PIC  X(01).
081606     12  WS-MA-GROUPING          PIC  X(06).
081606     12  WS-MA-STATE             PIC  X(02).
081606     12  WS-MA-ACCOUNT           PIC  X(10).
081606     12  WS-MA-CERT-EFF-DT       PIC  X(02).
081606     12  WS-MA-CERT-NO.
081606         16  WS-MA-CERT-PRIME    PIC  X(10).
081606         16  WS-MA-CERT-SFX      PIC  X(01).
00246  01  WS-CF-CONTROL-PRIMARY.
00247      12  WS-CF-COMPANY-ID        PIC  X(03)      VALUE SPACES.
00248      12  WS-CF-RECORD-TYPE       PIC  X(01)      VALUE ZERO.
00249 *        88  COMPANY-MASTER                      VALUE '1'.
00250 *        88  STATE-MASTER                        VALUE '3'.
00251 *        88  LF-BENEFIT-MASTER                   VALUE '4'.
00252 *        88  AH-BENEFIT-MASTER                   VALUE '5'.
00253      12  WS-CF-ACCESS.
00254          16  WS-CF-STATE         PIC  X(02)      VALUE SPACES.
00255          16  WS-CF-BENEFIT-NO                    VALUE SPACES.
00256              20  FILLER          PIC  X(01).
00257              20  WS-CF-CARRIER   PIC  X(01).
00258      12  WS-CF-SEQUENCE-NO       PIC S9(04) COMP VALUE ZERO.
00259
00260  01  WS-AM-CONTROL-PRIMARY.
00261      12  WS-AM-COMPANY-CD        PIC  X(01).
00262      12  WS-AM-CARRIER           PIC  X(01).
00263      12  WS-AM-GROUPING          PIC  X(06).
00264      12  WS-AM-STATE             PIC  X(02).
00265      12  WS-AM-ACCOUNT           PIC  X(10).
00266      12  WS-AM-EXPIRATION-DT     PIC  X(02).
00267      12  WS-AM-FILLER            PIC  X(04).
00268
00269  01  WS-CN-CONTROL-PRIMARY.
00270      12  WS-CN-COMPANY-CD        PIC  X(01).
00271      12  WS-CN-CARRIER           PIC  X(01).
00272      12  WS-CN-GROUPING          PIC  X(06).
00273      12  WS-CN-STATE             PIC  X(02).
00274      12  WS-CN-ACCOUNT           PIC  X(10).
00275      12  WS-CN-CERT-EFF-DT       PIC  X(02).
00276      12  WS-CN-CERT-NO.
00277          16  WS-CN-CERT-PRIME    PIC  X(10).
00278          16  WS-CN-CERT-SFX      PIC  X.
           12  WS-CN-REC-TYPE          PIC X.
           12  WS-CN-SEQ-NO            PIC 9(4) BINARY.
00279
00280  01  WS-FO-CONTROL-PRIMARY.
00281      12  WS-FO-COMPANY-CD        PIC  X(01).
00282      12  WS-FO-STATE             PIC  X(02).
00283      12  WS-FO-FORM-ID           PIC  X(12).
00284      12  WS-FO-EXP-DT            PIC  X(02).
00285  EJECT
      *                            COPY ERCCNOT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ERCCNOT                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *        FILE DESCRIPTION = CERTIFICATE NOTES                    *
00008 *                                                                *
00009 *        FILE TYPE= VSAM,KSDS                                    *
00010 *        RECORD SIZE = 150    RECFORM = FIXED                    *
00011 *                                                                *
00012 *        BASE CLUSTER = ERCNOT        RKP=2,LEN=36               *
00013 *                                                                *
00014 *        LOG = YES                                               *
00015 *        SERVREQ = DELETE,UPDATE,NEWREC                          *
00016 *                                                                *
091509******************************************************************
091509*                   C H A N G E   L O G
091509*
091509* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
091509*-----------------------------------------------------------------
091509*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
091509* EFFECTIVE    NUMBER
091509*-----------------------------------------------------------------
091509* 091509  CR2008100900003  AJRA  NEW FILE FOR CERT NOTES.
00017 ******************************************************************
00018
00019  01  CERT-NOTE-FILE.
00020      12  CZ-RECORD-ID                PIC  XX.
00021          88  VALID-CZ-ID                  VALUE 'CZ'.
00022
00023      12  CZ-CONTROL-PRIMARY.
00024          16  CZ-COMPANY-CD           PIC X.
00025          16  CZ-CARRIER              PIC X.
00026          16  CZ-GROUPING.
00027              20 CZ-GROUPING-PREFIX   PIC XXX.
00028              20 CZ-GROUPING-PRIME    PIC XXX.
00029          16  CZ-STATE                PIC XX.
00030          16  CZ-ACCOUNT.
00031              20 CZ-ACCOUNT-PREFIX    PIC X(4).
00032              20 CZ-ACCOUNT-PRIME     PIC X(6).
00033          16  CZ-CERT-EFF-DT          PIC XX.
00034          16  CZ-CERT-NO.
00035              20  CZ-CERT-PRIME       PIC X(10).
00036              20  CZ-CERT-SFX         PIC X.
00037          16  CZ-RECORD-TYPE          PIC X.
00038              88  CERT-NOTE           VALUE '1'.
                   88  CLAIM-CERT-NOTE     VALUE '2'.
00039          16  CZ-NOTE-SEQUENCE        PIC S9(4)     COMP.
00040
00041      12  CZ-LAST-MAINT-DT            PIC XX.
00042      12  CZ-LAST-MAINT-HHMMSS        PIC S9(7)   COMP-3.
00043      12  CZ-LAST-MAINT-USER          PIC X(4).
00044
00045      12  CZ-NOTE-INFORMATION.
00046          16  CZ-NOTE                 PIC X(63).
00047          16  FILLER                  PIC X(39).
00048 ******************************************************************
00286 *                            COPY ELCDATE.
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
00287  EJECT
00288 *                            COPY ELCLOGOF.
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
00289  EJECT
00290 *                            COPY ELCATTR.
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
00291  EJECT
00292 *                            COPY ELCEMIB.
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
00293  EJECT
010412 01  WS-PASS-631.
010412     12  WS-PASS-WORK-AREA         PIC X(384).
010412     12  WS-PASS-PROGRAM-WORK-AREA PIC X(640).
010412     12  FILLER REDEFINES WS-PASS-PROGRAM-WORK-AREA.
010412*        COPY ELC631PI.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELC631PI                            *
00004 *                            VMOD=2.012                          *
00005 *                                                                *
00006 *    THIS IS THE PI-PROGRAM-WORK-AREA THAT IS USED FOR THE       *
00007 *    REVIEW AND CORRRECTION SUB-SYSTEM.  ANY CHANGES WILL        *
00008 *    WILL EFFECT THE PROGRAMS OF THAT SUB-SYSTEM.                *
00009 *                                                                *
00010 *    IF THE LENGTH OF THIS PI-AREA CHANGES THE LENGTH MUST       *
00011 *    BE CHANGED FOR THE COMM-AREA WHEN PASSING THIS PI-AREA      *
00012 *    BETWEEN PROGRAMS.                                           *
00013 *                                                                *
00014 *    THE FOLLOWING PROGRAMS USE THIS COPYBOOK:                   *
00015 *                                                                *
00016 *               EL631 - EL6311 - EL6312 - EL6313                 *
00017 *                                                                *
00018 ******************************************************************
021414*                   C H A N G E   L O G
021414*
021414* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
021414*-----------------------------------------------------------------
021414*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
021414* EFFECTIVE    NUMBER
021414*-----------------------------------------------------------------
021414* 021414    2003053000001  PEMA  changes for auto chk request
070622*         CR2020061200002  TANA  ADD Cancel Reason Code
021414******************************************************************
00019
00020          16  PI-631-DATA.
00021              20  PI-ERPNDB-KEY.
00022                  24  PI-PB-COMPANY-CD     PIC X.
00023                  24  PI-PB-ENTRY-BATCH    PIC X(6).
00024                  24  PI-PB-BATCH-SEQ-NO   PIC S9(4) COMP.
00025                  24  PI-PB-BATCH-CHG-SEQ-NO PIC S9(4) COMP.
00026
00027              20  PI-ERPNDB-ALT-KEY.
00028                  24  PI-PB-COMPANY-CD-A1  PIC X.
00029                  24  PI-PB-CARRIER        PIC X.
00030                  24  PI-PB-GROUPING       PIC X(6).
00031                  24  PI-PB-STATE          PIC XX.
00032                  24  PI-PB-ACCOUNT        PIC X(10).
00033                  24  PI-PB-CERT-EFF-DT    PIC XX.
00034                  24  PI-PB-CERT-NO.
00035                      28  PI-PB-CERT-PRIME PIC X(10).
00036                      28  PI-PB-CERT-SFX   PIC X.
00037                  24  PI-PB-ALT-CHG-SEQ-NO PIC S9(4) COMP.
00038                  24  PI-PB-RECORD-TYPE    PIC X.
00039
00040              20  PI-ERPNDB-CSR-KEY.
00041                  24  PI-PB-CSR-COMPANY-CD-A2  PIC X.
00042                  24  PI-PB-CSR-ID             PIC X(4).
00043                  24  PI-PB-CSR-ENTRY-BATCH    PIC X(6).
00044                  24  PI-PB-CSR-BTCH-SEQ-NO    PIC S9(4) COMP.
00045                  24  PI-PB-CSR-BTCH-CHG-SEQ-NO PIC S9(4) COMP.
00046
00047              20  PI-BROWSE-TYPE               PIC X.
00048                  88  PI-FILE-BROWSE             VALUE ' '.
00049                  88  PI-PRIMARY-BROWSE          VALUE '1'.
00050                  88  PI-ALTERNATE-BROWSE        VALUE '2'.
00051                  88  PI-PRIMARY-WITH-SELECT     VALUE '3'.
00052                  88  PI-CSR-BROWSE              VALUE '4'.
00053
00054              20  PI-MAINT-FUNCTION            PIC X.
00055                  88  PI-ADD-FUNCTION            VALUE 'A'.
00056                  88  PI-BROWSE-FUNCTION         VALUE 'B'.
00057                  88  PI-CHANGE-FUNCTION         VALUE 'C'.
00058                  88  PI-DELETE-FUNCTION         VALUE 'D'.
00059                  88  PI-SHOW-FUNCTION           VALUE 'S'.
00060                  88  PI-PF5-FUNCTION            VALUE '5'.
00061                  88  PI-PF6-FUNCTION            VALUE '6'.
00062
00063              20  PI-FILE-SWITCHES.
00064                  24  PI-ALL-ISSUES-SW         PIC X.
00065                      88  ALL-ISSUES             VALUE 'Y'.
00066                  24  PI-ALL-CANCELS-SW        PIC X.
00067                      88  ALL-CANCELS            VALUE 'Y'.
00068                  24  PI-ISSUES-IN-ERROR-SW    PIC X.
00069                      88  ISSUES-IN-ERROR        VALUE 'Y'.
00070                  24  PI-CANCELS-IN-ERROR-SW   PIC X.
00071                      88  CANCEL-IN-ERROR        VALUE 'Y'.
00072                  24  PI-ONLY-BATCH-HEADERS-SW PIC X.
00073                      88  ONLY-BATCH-HEADERS     VALUE 'Y'.
00074                  24  PI-ALL-OUT-OF-BAL-SW     PIC X.
00075                      88  ALL-OUT-OF-BAL         VALUE 'Y'.
00076                  24  PI-HOLD-REC-SW           PIC X.
00077                      88  DISPLAY-HOLD-RECORDS   VALUE 'Y'.
00078                  24  PI-CHANGE-REC-SW         PIC X.
00079                      88  DISPLAY-CHANGE-RECORDS VALUE 'Y'.
00080                  24  PI-CHK-REQ-REC-SW        PIC X.
00081                      88  DISPLAY-CHK-REQ-RECORDS VALUE 'Y'.
00082                  24  PI-ISSUE-WARNING-SW      PIC X.
00083                      88  ISSUE-WITH-WARNING     VALUE 'Y'.
00084                  24  PI-CANCEL-WARNING-SW     PIC X.
00085                      88  CANCEL-WITH-WARNING    VALUE 'Y'.
00086              20  PI-DISPLAY-SCREEN-SW         PIC X.
00087                      88  PI-DISPLAY-SCREEN      VALUE 'Y'.
00088              20  PI-ORIGINAL-BATCH-SW         PIC X.
00089                      88  PI-DISPLAY-ORIGINAL-BATCH VALUE 'Y'.
00090
00091              20  PI-MAP-NAME                  PIC X(8).
00092
00093              20  PI-CURSOR                    PIC S9(4) COMP.
00094
00095              20  PI-PREV-ALT-KEY              PIC X(36).
00096              20  PI-PREV-CSR-KEY              PIC X(15).
00097              20  PI-PREV-KEY.
00098                  24  PI-PREV-COMPANY-CD       PIC X.
00099                  24  PI-PREV-BATCH            PIC X(6).
00100                  24  PI-PREV-SEQ-NO           PIC S9(4) COMP.
00101                  24  PI-PREV-CHG-SEQ-NO       PIC S9(4) COMP.
00102              20  PI-PREV-CONTROL-PRIMARY      PIC X(11).
00103              20  PI-BROWSE-SW                 PIC X.
00104                  88  PI-GOOD-BROWSE             VALUE 'Y'.
00105                  88  PI-NO-PB-RECS-FOUND        VALUE '9'.
00106              20  PI-SV-CARRIER                PIC X.
00107              20  PI-SV-GROUPING               PIC X(6).
00108              20  PI-SV-STATE                  PIC XX.
00109              20  PI-EDIT-SW                   PIC X.
00110              20  PI-DISPLAY-SW                PIC XX.
00111                  88 PI-DISPLAY-LIFE        VALUE 'LF'.
00112                  88 PI-DISPLAY-AH          VALUE 'AH'.
00113              20  PI-CRITERIA-DATA             PIC X(350).
00114              20  PI-BMODE                     PIC X.
00115              20  PI-BPMTAMT                   PIC S9(7)V99 COMP-3.
00116              20  PI-BPMTS                     PIC S999     COMP-3.
00117              20  PI-BTYPE                     PIC XXX OCCURS 2.
00118              20  PI-HIGH-SEQ-NO               PIC S9(4) COMP.
                   20  PI-CSR-SESSION-SW            PIC X.
                       88  CSR-EDIT-SESSION           VALUE 'Y'.
                   20  PI-ERRORS-SW                 PIC X.
062712                 88  FATAL-ERRORS               VALUE 'X'.
062712*                88  FATAL-OR-UNFORCED          VALUE 'X'.
021414             20  pi-unforced-sw               pic x.
021414                 88  unforced-errors            value 'X'.
070622             20  PI-CANCEL-REASON-631         PIC X.
070622             20  FILLER                       PIC X(3).
00120
010412         16  FILLER                PIC X(94).
010412
010412
00294 *                            COPY ELCINTF.
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
00295      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.
00296          16  FILLER                PIC  X(500).
00297          16  PI-PEND-SW            PIC  X(01).
00298          16  PI-LF-CANCEL-DATE     PIC  X(02).
00299          16  PI-AH-CANCEL-DATE     PIC  X(02).
00300          16  PI-LF-CANCEL-DATE-ED  PIC  X(08).
00301          16  PI-AH-CANCEL-DATE-ED  PIC  X(08).
00302          16  PI-EARNING-METHOD-LF  PIC  X.
00303          16  PI-EARNING-METHOD-AH  PIC  X.
00304          16  PI-LF-REFUND-AMT      PIC  S9(7)V99 COMP-3.
00305          16  PI-LF-REFUND-METH     PIC  X.
00306          16  PI-AH-REFUND-AMT      PIC  S9(7)V99 COMP-3.
00307          16  PI-AH-REFUND-METH     PIC  X.
00308          16  PI-TOTAL-REFUND-AMT   PIC  S9(7)V99 COMP-3.
00309          16  PI-MODIFY-CERT-CAP    PIC  X.
00310              88 MODIFY-CERT-CAP    VALUE 'Y'.
               16  PI-CLP-YN             PIC X.
               16  PI-CANCEL-REASON      PIC X.
072616         16  PI-PF10-OK            PIC X.
072616         16  FILLER                PIC  X(96).
00312  EJECT
00313 *                            COPY ELCAID.
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
051007*00039    02  DFHPF22   PIC  X  VALUE  '�'.
051007   02  DFHPF22   PIC  X  VALUE  '['.
00040    02  DFHPF23   PIC  X  VALUE  '.'.
00041    02  DFHPF24   PIC  X  VALUE  '<'.
00042    02  DFHMSRE   PIC  X  VALUE  'X'.
00043    02  DFHSTRF   PIC  X  VALUE  'h'.
00044    02  DFHTRIG   PIC  X  VALUE  '"'.
00314
00315  01  FILLER  REDEFINES  DFHAID.
00316      12  FILLER                  PIC  X(08).
00317      12  PF-VALUES               PIC  X(01)      OCCURS 24 TIMES.
00318  EJECT
00319 *                            COPY EL1278S.
       01  EL127HI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  HDATEL PIC S9(0004) COMP.
           05  HDATEF PIC  X(0001).
           05  FILLER REDEFINES HDATEF.
               10  HDATEA PIC  X(0001).
           05  HDATEI PIC  X(0008).
      *    -------------------------------
           05  HTIMEL PIC S9(0004) COMP.
           05  HTIMEF PIC  X(0001).
           05  FILLER REDEFINES HTIMEF.
               10  HTIMEA PIC  X(0001).
           05  HTIMEI PIC  999V99.
      *    -------------------------------
           05  CMPNYIDL PIC S9(0004) COMP.
           05  CMPNYIDF PIC  X(0001).
           05  FILLER REDEFINES CMPNYIDF.
               10  CMPNYIDA PIC  X(0001).
           05  CMPNYIDI PIC  X(0003).
      *    -------------------------------
           05  USERIDL PIC S9(0004) COMP.
           05  USERIDF PIC  X(0001).
           05  FILLER REDEFINES USERIDF.
               10  USERIDA PIC  X(0001).
           05  USERIDI PIC  X(0004).
      *    -------------------------------
           05  HCARIERL PIC S9(0004) COMP.
           05  HCARIERF PIC  X(0001).
           05  FILLER REDEFINES HCARIERF.
               10  HCARIERA PIC  X(0001).
           05  HCARIERI PIC  X(0001).
      *    -------------------------------
           05  HGROUPL PIC S9(0004) COMP.
           05  HGROUPF PIC  X(0001).
           05  FILLER REDEFINES HGROUPF.
               10  HGROUPA PIC  X(0001).
           05  HGROUPI PIC  X(0006).
      *    -------------------------------
           05  HSTATEL PIC S9(0004) COMP.
           05  HSTATEF PIC  X(0001).
           05  FILLER REDEFINES HSTATEF.
               10  HSTATEA PIC  X(0001).
           05  HSTATEI PIC  X(0002).
      *    -------------------------------
           05  HACCTNOL PIC S9(0004) COMP.
           05  HACCTNOF PIC  X(0001).
           05  FILLER REDEFINES HACCTNOF.
               10  HACCTNOA PIC  X(0001).
           05  HACCTNOI PIC  X(0010).
      *    -------------------------------
           05  HEFFDTL PIC S9(0004) COMP.
           05  HEFFDTF PIC  X(0001).
           05  FILLER REDEFINES HEFFDTF.
               10  HEFFDTA PIC  X(0001).
           05  HEFFDTI PIC  X(0008).
      *    -------------------------------
           05  HCERTNOL PIC S9(0004) COMP.
           05  HCERTNOF PIC  X(0001).
           05  FILLER REDEFINES HCERTNOF.
               10  HCERTNOA PIC  X(0001).
           05  HCERTNOI PIC  X(0010).
      *    -------------------------------
           05  HCRTSFXL PIC S9(0004) COMP.
           05  HCRTSFXF PIC  X(0001).
           05  FILLER REDEFINES HCRTSFXF.
               10  HCRTSFXA PIC  X(0001).
           05  HCRTSFXI PIC  X(0001).
      *    -------------------------------
           05  HLNAMEL PIC S9(0004) COMP.
           05  HLNAMEF PIC  X(0001).
           05  FILLER REDEFINES HLNAMEF.
               10  HLNAMEA PIC  X(0001).
           05  HLNAMEI PIC  X(0015).
      *    -------------------------------
           05  HFNAMEL PIC S9(0004) COMP.
           05  HFNAMEF PIC  X(0001).
           05  FILLER REDEFINES HFNAMEF.
               10  HFNAMEA PIC  X(0001).
           05  HFNAMEI PIC  X(0010).
      *    -------------------------------
           05  HINITL PIC S9(0004) COMP.
           05  HINITF PIC  X(0001).
           05  FILLER REDEFINES HINITF.
               10  HINITA PIC  X(0001).
           05  HINITI PIC  X(0001).
      *    -------------------------------
           05  LCLMDTHL PIC S9(0004) COMP.
           05  LCLMDTHF PIC  X(0001).
           05  FILLER REDEFINES LCLMDTHF.
               10  LCLMDTHA PIC  X(0001).
           05  LCLMDTHI PIC  X(0022).
      *    -------------------------------
           05  LCLMDTL PIC S9(0004) COMP.
           05  LCLMDTF PIC  X(0001).
           05  FILLER REDEFINES LCLMDTF.
               10  LCLMDTA PIC  X(0001).
           05  LCLMDTI PIC  X(0008).
      *    -------------------------------
           05  HPCYNL PIC S9(0004) COMP.
           05  HPCYNF PIC  X(0001).
           05  FILLER REDEFINES HPCYNF.
               10  HPCYNA PIC  X(0001).
           05  HPCYNI PIC  X(0001).
      *    -------------------------------
           05  ACLMDTHL PIC S9(0004) COMP.
           05  ACLMDTHF PIC  X(0001).
           05  FILLER REDEFINES ACLMDTHF.
               10  ACLMDTHA PIC  X(0001).
           05  ACLMDTHI PIC  X(0023).
      *    -------------------------------
           05  ACLMDTL PIC S9(0004) COMP.
           05  ACLMDTF PIC  X(0001).
           05  FILLER REDEFINES ACLMDTF.
               10  ACLMDTA PIC  X(0001).
           05  ACLMDTI PIC  X(0008).
      *    -------------------------------
           05  PMTHDL PIC S9(0004) COMP.
           05  PMTHDF PIC  X(0001).
           05  FILLER REDEFINES PMTHDF.
               10  PMTHDA PIC  X(0001).
           05  PMTHDI PIC  X(0010).
      *    -------------------------------
           05  CNCDTHDL PIC S9(0004) COMP.
           05  CNCDTHDF PIC  X(0001).
           05  FILLER REDEFINES CNCDTHDF.
               10  CNCDTHDA PIC  X(0001).
           05  CNCDTHDI PIC  X(0007).
      *    -------------------------------
           05  HLKINDL PIC S9(0004) COMP.
           05  HLKINDF PIC  X(0001).
           05  FILLER REDEFINES HLKINDF.
               10  HLKINDA PIC  X(0001).
           05  HLKINDI PIC  X(0002).
      *    -------------------------------
           05  HLCDL PIC S9(0004) COMP.
           05  HLCDF PIC  X(0001).
           05  FILLER REDEFINES HLCDF.
               10  HLCDA PIC  X(0001).
           05  HLCDI PIC  X(0002).
      *    -------------------------------
           05  HLEDESCL PIC S9(0004) COMP.
           05  HLEDESCF PIC  X(0001).
           05  FILLER REDEFINES HLEDESCF.
               10  HLEDESCA PIC  X(0001).
           05  HLEDESCI PIC  X(0003).
      *    -------------------------------
           05  HLTERML PIC S9(0004) COMP.
           05  HLTERMF PIC  X(0001).
           05  FILLER REDEFINES HLTERMF.
               10  HLTERMA PIC  X(0001).
           05  HLTERMI PIC  9(3).
      *    -------------------------------
           05  HLREML PIC S9(0004) COMP.
           05  HLREMF PIC  X(0001).
           05  FILLER REDEFINES HLREMF.
               10  HLREMA PIC  X(0001).
           05  HLREMI PIC  X(0003).
      *    -------------------------------
           05  HLPREML PIC S9(0004) COMP.
           05  HLPREMF PIC  X(0001).
           05  FILLER REDEFINES HLPREMF.
               10  HLPREMA PIC  X(0001).
           05  HLPREMI PIC  X(0011).
      *    -------------------------------
           05  HLITDRL PIC S9(0004) COMP.
           05  HLITDRF PIC  X(0001).
           05  FILLER REDEFINES HLITDRF.
               10  HLITDRA PIC  X(0001).
           05  HLITDRI PIC  X(0011).
      *    -------------------------------
           05  HLREFNDL PIC S9(0004) COMP.
           05  HLREFNDF PIC  X(0001).
           05  FILLER REDEFINES HLREFNDF.
               10  HLREFNDA PIC  X(0001).
           05  HLREFNDI PIC  X(0011).
      *    -------------------------------
           05  HLCANCL PIC S9(0004) COMP.
           05  HLCANCF PIC  X(0001).
           05  FILLER REDEFINES HLCANCF.
               10  HLCANCA PIC  X(0001).
           05  HLCANCI PIC  X(0008).
      *    -------------------------------
           05  HLCAL1L PIC S9(0004) COMP.
           05  HLCAL1F PIC  X(0001).
           05  FILLER REDEFINES HLCAL1F.
               10  HLCAL1A PIC  X(0001).
           05  HLCAL1I PIC  X(0001).
      *    -------------------------------
           05  HLCALCL PIC S9(0004) COMP.
           05  HLCALCF PIC  X(0001).
           05  FILLER REDEFINES HLCALCF.
               10  HLCALCA PIC  X(0001).
           05  HLCALCI PIC  X(0012).
      *    -------------------------------
           05  HLCPCTL PIC S9(0004) COMP.
           05  HLCPCTF PIC  X(0001).
           05  FILLER REDEFINES HLCPCTF.
               10  HLCPCTA PIC  X(0001).
           05  HLCPCTI PIC  X(0006).
      *    -------------------------------
           05  HLCAMTL PIC S9(0004) COMP.
           05  HLCAMTF PIC  X(0001).
           05  FILLER REDEFINES HLCAMTF.
               10  HLCAMTA PIC  X(0001).
           05  HLCAMTI PIC  X(0009).
      *    -------------------------------
           05  HAKINDL PIC S9(0004) COMP.
           05  HAKINDF PIC  X(0001).
           05  FILLER REDEFINES HAKINDF.
               10  HAKINDA PIC  X(0001).
           05  HAKINDI PIC  X(0002).
      *    -------------------------------
           05  HACDL PIC S9(0004) COMP.
           05  HACDF PIC  X(0001).
           05  FILLER REDEFINES HACDF.
               10  HACDA PIC  X(0001).
           05  HACDI PIC  X(0002).
      *    -------------------------------
           05  HAEDESCL PIC S9(0004) COMP.
           05  HAEDESCF PIC  X(0001).
           05  FILLER REDEFINES HAEDESCF.
               10  HAEDESCA PIC  X(0001).
           05  HAEDESCI PIC  X(0003).
      *    -------------------------------
           05  HATERML PIC S9(0004) COMP.
           05  HATERMF PIC  X(0001).
           05  FILLER REDEFINES HATERMF.
               10  HATERMA PIC  X(0001).
           05  HATERMI PIC  9(3).
      *    -------------------------------
           05  HAREML PIC S9(0004) COMP.
           05  HAREMF PIC  X(0001).
           05  FILLER REDEFINES HAREMF.
               10  HAREMA PIC  X(0001).
           05  HAREMI PIC  X(0003).
      *    -------------------------------
           05  HAPREML PIC S9(0004) COMP.
           05  HAPREMF PIC  X(0001).
           05  FILLER REDEFINES HAPREMF.
               10  HAPREMA PIC  X(0001).
           05  HAPREMI PIC  X(0011).
      *    -------------------------------
           05  HAITDRL PIC S9(0004) COMP.
           05  HAITDRF PIC  X(0001).
           05  FILLER REDEFINES HAITDRF.
               10  HAITDRA PIC  X(0001).
           05  HAITDRI PIC  X(0011).
      *    -------------------------------
           05  HAREFNDL PIC S9(0004) COMP.
           05  HAREFNDF PIC  X(0001).
           05  FILLER REDEFINES HAREFNDF.
               10  HAREFNDA PIC  X(0001).
           05  HAREFNDI PIC  X(0011).
      *    -------------------------------
           05  HACANCL PIC S9(0004) COMP.
           05  HACANCF PIC  X(0001).
           05  FILLER REDEFINES HACANCF.
               10  HACANCA PIC  X(0001).
           05  HACANCI PIC  X(0008).
      *    -------------------------------
           05  HACAL1L PIC S9(0004) COMP.
           05  HACAL1F PIC  X(0001).
           05  FILLER REDEFINES HACAL1F.
               10  HACAL1A PIC  X(0001).
           05  HACAL1I PIC  X(0001).
      *    -------------------------------
           05  HACALCL PIC S9(0004) COMP.
           05  HACALCF PIC  X(0001).
           05  FILLER REDEFINES HACALCF.
               10  HACALCA PIC  X(0001).
           05  HACALCI PIC  X(0012).
      *    -------------------------------
           05  HACPCTL PIC S9(0004) COMP.
           05  HACPCTF PIC  X(0001).
           05  FILLER REDEFINES HACPCTF.
               10  HACPCTA PIC  X(0001).
           05  HACPCTI PIC  X(0006).
      *    -------------------------------
           05  HACAMTL PIC S9(0004) COMP.
           05  HACAMTF PIC  X(0001).
           05  FILLER REDEFINES HACAMTF.
               10  HACAMTA PIC  X(0001).
           05  HACAMTI PIC  X(0009).
      *    -------------------------------
           05  CANFEEHL PIC S9(0004) COMP.
           05  CANFEEHF PIC  X(0001).
           05  FILLER REDEFINES CANFEEHF.
               10  CANFEEHA PIC  X(0001).
           05  CANFEEHI PIC  X(0012).
      *    -------------------------------
           05  CANFEEL PIC S9(0004) COMP.
           05  CANFEEF PIC  X(0001).
           05  FILLER REDEFINES CANFEEF.
               10  CANFEEA PIC  X(0001).
           05  CANFEEI PIC  X(0006).
      *    -------------------------------
           05  TOPREML PIC S9(0004) COMP.
           05  TOPREMF PIC  X(0001).
           05  FILLER REDEFINES TOPREMF.
               10  TOPREMA PIC  X(0001).
           05  TOPREMI PIC  X(0011).
      *    -------------------------------
           05  TOITDRL PIC S9(0004) COMP.
           05  TOITDRF PIC  X(0001).
           05  FILLER REDEFINES TOITDRF.
               10  TOITDRA PIC  X(0001).
           05  TOITDRI PIC  X(0011).
      *    -------------------------------
           05  TORFNDL PIC S9(0004) COMP.
           05  TORFNDF PIC  X(0001).
           05  FILLER REDEFINES TORFNDF.
               10  TORFNDA PIC  X(0001).
           05  TORFNDI PIC  X(0011).
      *    -------------------------------
           05  CANREAHL PIC S9(0004) COMP.
           05  CANREAHF PIC  X(0001).
           05  FILLER REDEFINES CANREAHF.
               10  CANREAHA PIC  X(0001).
           05  CANREAHI PIC  X(0015).
      *    -------------------------------
           05  CANREAL PIC S9(0004) COMP.
           05  CANREAF PIC  X(0001).
           05  FILLER REDEFINES CANREAF.
               10  CANREAA PIC  X(0001).
           05  CANREAI PIC  X(0001).
      *    -------------------------------
           05  CLPYNHL PIC S9(0004) COMP.
           05  CLPYNHF PIC  X(0001).
           05  FILLER REDEFINES CLPYNHF.
               10  CLPYNHA PIC  X(0001).
           05  CLPYNHI PIC  X(0005).
      *    -------------------------------
           05  CLPYNL PIC S9(0004) COMP.
           05  CLPYNF PIC  X(0001).
           05  FILLER REDEFINES CLPYNF.
               10  CLPYNA PIC  X(0001).
           05  CLPYNI PIC  X(0001).
      *    -------------------------------
           05  REFDUEHL PIC S9(0004) COMP.
           05  REFDUEHF PIC  X(0001).
           05  FILLER REDEFINES REFDUEHF.
               10  REFDUEHA PIC  X(0001).
           05  REFDUEHI PIC  X(0018).
      *    -------------------------------
           05  REFDUEL PIC S9(0004) COMP.
           05  REFDUEF PIC  X(0001).
           05  FILLER REDEFINES REFDUEF.
               10  REFDUEA PIC  X(0001).
           05  REFDUEI PIC  X(0010).
      *    -------------------------------
           05  HLREDL PIC S9(0004) COMP.
           05  HLREDF PIC  X(0001).
           05  FILLER REDEFINES HLREDF.
               10  HLREDA PIC  X(0001).
           05  HLREDI PIC  X(0014).
      *    -------------------------------
           05  FREMTRML PIC S9(0004) COMP.
           05  FREMTRMF PIC  X(0001).
           05  FILLER REDEFINES FREMTRMF.
               10  FREMTRMA PIC  X(0001).
           05  FREMTRMI PIC  X(0003).
      *    -------------------------------
           05  HERMSG1L PIC S9(0004) COMP.
           05  HERMSG1F PIC  X(0001).
           05  FILLER REDEFINES HERMSG1F.
               10  HERMSG1A PIC  X(0001).
           05  HERMSG1I PIC  X(0079).
      *    -------------------------------
           05  HERMSG2L PIC S9(0004) COMP.
           05  HERMSG2F PIC  X(0001).
           05  FILLER REDEFINES HERMSG2F.
               10  HERMSG2A PIC  X(0001).
           05  HERMSG2I PIC  X(0079).
      *    -------------------------------
           05  HPFKEYL PIC S9(0004) COMP.
           05  HPFKEYF PIC  X(0001).
           05  FILLER REDEFINES HPFKEYF.
               10  HPFKEYA PIC  X(0001).
           05  HPFKEYI PIC  99.
      *    -------------------------------
           05  PFKEY11L PIC S9(0004) COMP.
           05  PFKEY11F PIC  X(0001).
           05  FILLER REDEFINES PFKEY11F.
               10  PFKEY11A PIC  X(0001).
           05  PFKEY11I PIC  X(0014).
       01  EL127HO REDEFINES EL127HI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMPNYIDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HCARIERO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HGROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HSTATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HACCTNOO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HEFFDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HCERTNOO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HCRTSFXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HLNAMEO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HFNAMEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HINITO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCLMDTHO PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCLMDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HPCYNO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLMDTHO PIC  X(0023).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLMDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PMTHDO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CNCDTHDO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HLKINDO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HLCDO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HLEDESCO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HLTERMO PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HLREMO PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HLPREMO PIC  ZZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HLITDRO PIC  ZZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HLREFNDO PIC  ZZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HLCANCO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HLCAL1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HLCALCO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HLCPCTO PIC  .99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HLCAMTO PIC  ZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HAKINDO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HACDO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HAEDESCO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HATERMO PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HAREMO PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HAPREMO PIC  ZZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HAITDRO PIC  ZZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HAREFNDO PIC  ZZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HACANCO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HACAL1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HACALCO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HACPCTO PIC  .99999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HACAMTO PIC  ZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CANFEEHO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CANFEEO PIC  ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TOPREMO PIC  ZZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TOITDRO PIC  ZZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TORFNDO PIC  ZZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CANREAHO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CANREAO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLPYNHO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLPYNO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFDUEHO PIC  X(0018).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFDUEO PIC  ZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HLREDO PIC  ZZZ,ZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FREMTRMO PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HERMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HERMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HPFKEYO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFKEY11O PIC  X(0014).
      *    -------------------------------
00320  EJECT
00321 *                            COPY ELCCALC.
00001 ******************************************************************
00002 *                                                                *
00003 *                           ELCCALC.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.025                          *
00006 *                                                                *
00007 *   DESCRIPTION:  DATA TO BE PASSED TO REMAINING TERM ROUTINE    *
00008 *                 REMAINING AMOUNT ROUTINE, LOSS RESERVE ROUTINE *
00009 *                 REFUND CALCULATIONS ROUTINE, EARNINGS CALCU -  *
00010 *                 LATIONS ROUTINE, AND THE RATING ROUTINE.       *
00011 *                                                                *
00012 *  PASSED TO ELRTRM                                              *
00013 *  -----------------                                             *
00014 *  METHOD CODE (I.E. FULL MONTH, HALF ADJ, ETC)                  *
00015 *  ORIGINAL TERM                                                 *
00016 *  BEGINNING DATE                                                *
00017 *  ENDING DATE                                                   *
00018 *  COMPANY I.D.                                                  *
00019 *  ACCOUNT MASTER USER FIELD                                     *
00020 *  PROCESS SWITCH (CANCEL, CLAIM)                                *
00021 *  FREE LOOK DAYS                                                *
00022 *                                                                *
00023 *  RETURNED FROM ELRTRM                                          *
00024 *  ---------------------                                         *
00025 *  REMAINING TERM 1 - USED FOR EARNINGS                          *
00026 *  REMAINING TERM 2 - USED FOR BENEFIT CALCULATIONS              *
00027 *  REMAINING TERM 3 - USED FOR CLAIM BENEFITS                    *
00028 *  ODD DAYS - REMAINING DAYS PAST FULL MONTHS                    *
00029 *----------------------------------------------------------------*
00030 *  PASSED TO ELRAMT                                              *
00031 *  ----------------                                              *
00032 *  REMAINING TERM 1 OR 2 OR 3 (FROM ELRTRM)                      *
00033 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00034 *  ORIGINAL AMOUNT                                               *
00035 *  ALTERNATE BENEFIT (BALLON)                                    *
00036 *  A.P.R. - NET PAY ONLY                                         *
00037 *  METHOD
00038 *  PAYMENT FREQUENCY - FOR FARM PLAN                             *
00039 *  COMPANY I.D.                                                  *
00040 *  BENEFIT TYPE                                                  *
00041 *                                                                *
00042 *  RETURNED FROM ELRAMT                                          *
00043 *  --------------------                                          *
00044 *  REMAINING AMOUNT 1 - CURRENT                                  *
00045 *  REMAINING AMOUNT 2 - PREVIOUS MONTH                           *
00046 *  REMAINING AMOUNT FACTOR
00047 *----------------------------------------------------------------*
00048 *  PASSED TO ELRESV                                              *
00049 *  -----------------                                             *
00050 *  CERTIFICATE EFFECTIVE DATE                                    *
00051 *  VALUATION DATE                                                *
00052 *  PAID THRU DATE                                                *
00053 *  BENEFIT                                                       *
00054 *  INCURRED DATE                                                 *
00055 *  REPORTED DATE                                                 *
00056 *  ISSUE AGE                                                     *
00057 *  TERM                                                          *
00058 *  CDT PERCENT                                                   *
00059 *  CDT METHOD (I.E. INTERPOLATED, AVERAGE, ETC)                  *
00060 * *CLAIM TYPE (LIFE, A/H)                                        *
00061 * *REMAINING BENEFIT (FROM ELRAMT)                               *
00062 * *ONLY FIELDS REQUIRED FOR LIFE CLAIMS                          *
00063 *                                                                *
00064 *  RETURNED FROM ELRESV                                          *
00065 *  --------------------                                          *
00066 *  CDT TABLE USED                                                *
00067 *  CDT FACTOR USED                                               *
00068 *  PAY TO CURRENT RESERVE                                        *
00069 *  I.B.N.R. - A/H ONLY                                           *
00070 *  FUTURE (ACCRUED) AH ONLY                                      *
00071 *----------------------------------------------------------------*
00072 *  PASSED TO ELRATE                                              *
00073 *  ----------------                                              *
00074 *  CERT ISSUE DATE                                               *
00075 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00076 *  TERM OR EXT DAYS  (DAY TERM FOR SP CALC = 'D', ELSE EXT DAYS) *
00077 *  CAPPED TERM   (ONLY FOR TRUNCATED LIFE)                       *
00078 *  STATE CODE (CLIENT DEFINED)                                   *
00079 *  STATE CODE (STANDARD P.O. ABBRV)                              *
00080 *  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
00081 *  DEVIATION CODE                                                *
00082 *  ISSUE AGE                                                     *
00083 *  ORIGINAL BENEFIT AMOUNT                                       *
00084 *  RATING BENEFIT AMT (TOTAL BENEFIT AMT FOR BALLOONS)           *
00085 *  PROCESS TYPE (ISSUE OR CANCEL)                                *
00086 *  BENEFIT KIND (LIFE OR A/H)                                    *
00087 *  A.P.R.                                                        *
00088 *  METHOD
00089 *  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
00090 *  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
00091 *  COMPANY I.D. (3 CHARACTER)                                    *
00092 *  BENEFIT CODE                                                  *
00093 *  BENEFIT OVERRIDE CODE                                         *
00094 *  MAXIMUM MONTHLY BENEFIT (FROM ACCT MASTER - CSL ONLY)         *
00095 *  MAXIMUM TOTAL BENEFIT (FROM ACCT MASTER - CSL ONLY)           *
00096 *  JOINT INDICATOR (CSL ONLY)                                    *
00097 *  FIRST PAYMENT DATE (CSL ONLY)                                 *
00098 *  PERIODIC PAYMENT AMOUNT (IN CP-REMAINING-TERM - CSL ONLY)     *
00099 *                                                                *
00100 *  RETURNED FROM ELRATE                                          *
00101 *  --------------------                                          *
00102 *  CALCULATED PREMIUM                                            *
00103 *  PREMIUM RATE                                                  *
00104 *  MORTALITY CODE                                                *
00105 *  MAX ATTAINED AGE                                              *
00106 *  MAX AGE                                                       *
00107 *  MAX TERM                                                      *
00108 *  MAX MONTHLY BENEFIT                                           *
00109 *  MAX TOTAL BENIFIT                                             *
00110 *  COMPOSITE RATE (OPEN-END ONLY)                                *
00111 *----------------------------------------------------------------*
00112 *  PASSED TO ELRFND                                              *
00113 *  ----------------                                              *
00114 *  CERT ISSUE DATE                                               *
00115 *  REFUND DATE                                                   *
00116 *  RULE OF 78 OPTION (FROM CONTROL RECORD)                       *
00117 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00118 *  TERM OR EXT DAYS  (DAY TERM FOR SP CALC = 'D', ELSE EXT DAYS) *
00119 *  REMAINING TERM (REMAINING TERM 1 FROM ELTERM)                 *
00120 *  STATE CODE (CLIENT DEFINED)                                   *
00121 *  STATE CODE (STANDARD P.O. ABBRV)                              *
00122 *  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
00123 *  DEVIATION CODE                                                *
00124 *  ISSUE AGE                                                     *
00125 *  ORIGINAL BENEFIT AMOUNT                                       *
00126 *  RATING BENEFIT AMT (TOTAL BENEFIT AMT FOR BALLOONS)           *
00127 *  PROCESS TYPE (CANCEL)                                         *
00128 *  BENEFIT KIND (LIFE OR A/H)                                    *
00129 *  A.P.R.                                                        *
00130 *  EARNING METHOD - (CODE FROM BENEFIT, STATE OR ACCOUNT RECORD) *
00131 *  RATING METHOD -  (CODE FROM BENEFIT)                          *
00132 *  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
00133 *  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
00134 *  COMPANY I.D. (3 CHARACTER)                                    *
00135 *  BENEFIT CODE                                                  *
00136 *  BENEFIT OVERRIDE CODE                                         *
00137 *                                                                *
00138 *  RETURNED FROM ELRFND                                          *
00139 *  --------------------                                          *
00140 *  CALCULATED REFUND                                             *
00141 *----------------------------------------------------------------*
00142 *  PASSED TO ELEARN                                              *
00143 *  ----------------                                              *
00144 *  CERT ISSUE DATE                                               *
00145 *  ORIGINAL TERM (ADJUSTED IF SKIP MONTHS ARE USED)              *
00146 *  REMAINING TERM (REMAINING TERM 1 FROM ELTERM)                 *
00147 *  RULE OF 78 OPTION (FROM CONTROL RECORD)                       *
00148 *  STATE CODE (CLIENT DEFINED)                                   *
00149 *  STATE CODE (STANDARD P.O. ABBRV)                              *
00150 *  CLASS CODE (FROM CERT OR ACCOUNT IF CERT ZERO OR SPACES)      *
00151 *  DEVIATION CODE                                                *
00152 *  ISSUE AGE                                                     *
00153 *  ORIGINAL BENEFIT AMOUNT                                       *
00154 *  BENEFIT KIND (LIFE OR A/H)                                    *
00155 *  A.P.R.                                                        *
00156 *  METHOD - (EARNING CODE FROM BENEFIT RECORD)                   *
00157 *  SPECIAL METHOD - (SPECIAL CODE FROM BENEFIT RECORD)           *
00158 *  PAYMENT FREQUENCY  (FOR TEXAS IRREGULAR)                      *
00159 *  COMPANY I.D. (3 CHARACTER)                                    *
00160 *  BENEFIT CODE                                                  *
00161 *  BENEFIT OVERRIDE CODE                                         *
00162 *                                                                *
00163 *  RETURNED FROM ELEARN                                          *
00164 *  --------------------                                          *
00165 *  INDICATED  EARNINGS                                           *
00166 *----------------------------------------------------------------*
00167 *                 LENGTH = 450                                   *
00168 *                                                                *
00169 ******************************************************************
010303******************************************************************
010303*                   C H A N G E   L O G
010303*
010303* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
010303*-----------------------------------------------------------------
010303*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
010303* EFFECTIVE    NUMBER
010303*-----------------------------------------------------------------
010303* 010303    2001061800003  PEMA  ADD DCC/MONTHLY PROCESSING
033104* 033104    2003080800002  PEMA  ADD GAP NON REFUNDABLE OPTION
101807* 101807    2007100100007  PEMA  EXPAND CLM RESERVE FIELDS
010410* 010410    2008021200005  PEMA  ADD FIELDS FOR MN NET PAY BALLOON
010410* 010410    2009050700003  PEMA  ADD FIELDS FOR SPP-DD
041310* 041310  CR2008021200005  PEMA  ADD CODE FOR MN LEVEL
041710* 041710    2007111300001  AJRA  ADD CLAIM CALC SW FOR SC NP+6
101110* 101110  CR2010012700001  PEMA ADD DDF REFUND/UEP PROCESSING
071211* 071211  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
040615* 040615  CR2013072200002  PEMA  ADD EXTRA PERIODS
010716* 010716    2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
012820* 012820  CR2020012800001  PEMA ADD MIN LOAN TERM FOR EXT TERM.
010303******************************************************************
00170
00171  01  CALCULATION-PASS-AREA.
00172      12  CP-COMM-LENGTH            PIC S9(4)         VALUE +450
00173                                      COMP.
00174
00175      12  CP-RETURN-CODE            PIC X             VALUE ZERO.
00176        88  NO-CP-ERROR                             VALUE ZERO.
00177        88  CP-ERROR-OCCURED VALUE '1' '2' '3' '4' '5' '6' '7' '8'
012820                                  '9' 'A' 'B' 'C' 'D' 'E' 'H' 'I'.
00179        88  CP-ERROR-IN-AMOUNTS                     VALUE '1'.
00180        88  CP-ERROR-IN-DATES                       VALUE '2'.
00181        88  CP-ERROR-IN-OPTIONS                     VALUE '3'.
00182        88  CP-ERROR-IN-TERMS                       VALUE '4'.
00183        88  CP-ERROR-IN-FREQUENCY                   VALUE '5'.
00184        88  CP-ERROR-RATE-NOT-FOUND                 VALUE '6'.
00185        88  CP-ERROR-RATE-IS-ZERO                   VALUE '7'.
00186        88  CP-ERROR-AMT-OUTSIDE-LIMIT              VALUE '8'.
00187        88  CP-ERROR-TERM-OUTSIDE-LIMIT             VALUE '9'.
00188        88  CP-ERROR-AGE-OUTSIDE-LIMIT              VALUE 'A'.
00189        88  CP-ERROR-ATT-OUTSIDE-LIMIT              VALUE 'B'.
00190        88  CP-ERROR-TOT-OUTSIDE-LIMIT              VALUE 'C'.
00191        88  CP-ERROR-RATE-FILE-NOTOPEN              VALUE 'D'.
00192        88  CP-ERROR-ISSUE-AGE-ZERO                 VALUE 'E'.
00193        88  CP-ERROR-NO-LIMITS-CRI                  VALUE 'F'.
00194        88  CP-ERROR-DIV-BY-ZERO                    VALUE 'G'.
00195        88  CP-ERROR-LOAN-TERM                      VALUE 'H'.
012820       88  CP-ERROR-TERM-BELOW-MINIMUM             VALUE 'I'.
00196
00197      12  CP-RETURN-CODE-2          PIC X             VALUE ZERO.
00198        88  NO-CP-ERROR-2                           VALUE ZERO.
00199 ***********************  INPUT AREAS ****************************
00200
00201      12  CP-CALCULATION-AREA.
00202          16  CP-ACCOUNT-NUMBER     PIC X(10)       VALUE SPACES.
00203          16  CP-CERT-EFF-DT        PIC XX.
00204          16  CP-VALUATION-DT       PIC XX.
00205          16  CP-PAID-THRU-DT       PIC XX.
00206          16  CP-BENEFIT-TYPE       PIC X.
00207            88  CP-AH                               VALUE 'A' 'D'
00208                                                    'I' 'U'.
00209            88  CP-REDUCING-LIFE                    VALUE 'R'.
00210            88  CP-LEVEL-LIFE                       VALUE 'L' 'P'.
00211          16  CP-INCURRED-DT        PIC XX.
00212          16  CP-REPORTED-DT        PIC XX.
00213          16  CP-ACCT-FLD-5         PIC XX            VALUE SPACE.
00214          16  CP-COMPANY-ID         PIC XXX           VALUE SPACE.
00215          16  CP-ISSUE-AGE          PIC S9(3)         VALUE ZERO
00216                                      COMP-3.
00217          16  CP-CDT-PERCENT        PIC S9(3)V99      VALUE ZERO
00218                                      COMP-3.
00219          16  CP-CDT-METHOD         PIC X.
00220            88  CP-CDT-ROUND-NEAR                   VALUE '1'.
00221            88  CP-CDT-ROUND-HIGH                   VALUE '2'.
00222            88  CP-CDT-INTERPOLATED                 VALUE '3'.
00223          16  CP-CLAIM-TYPE         PIC X.
00224            88  CP-AH-CLAIM                         VALUE 'A'.
00225            88  CP-LIFE-CLAIM                       VALUE 'L'.
00226          16  CP-ORIGINAL-TERM      PIC S9(3)         VALUE ZERO
00227                                      COMP-3.
00228          16  CP-ORIGINAL-BENEFIT   PIC S9(9)V99      VALUE ZERO
00229                                      COMP-3.
00230          16  CP-ORIGINAL-PREMIUM   PIC S9(7)V99      VALUE ZERO
00231                                      COMP-3.
00232          16  CP-REMAINING-TERM     PIC S9(3)V99      VALUE ZERO
00233                                      COMP-3.
00234          16  CP-REMAINING-BENEFIT  PIC S9(9)V99      VALUE ZERO
00235                                      COMP-3.
00236          16  CP-LOAN-APR           PIC S9(3)V9(4)    VALUE ZERO
00237                                      COMP-3.
00238          16  CP-PAY-FREQUENCY      PIC S9(3)         VALUE ZERO
00239                                      COMP-3.
00240          16  CP-REM-TERM-METHOD    PIC X.
00241            88  CP-EARN-AFTER-15TH                  VALUE '1'.
00242            88  CP-EARN-ON-HALF-MONTH               VALUE '2'.
00243            88  CP-EARN-ON-1ST-DAY                  VALUE '3'.
00244            88  CP-EARN-ON-FULL-MONTH               VALUE '4'.
00245            88  CP-EARN-WITH-NO-DAYS                VALUE '5'.
00246            88  CP-EARN-AFTER-14TH                  VALUE '6'.
00247            88  CP-EARN-AFTER-16TH                  VALUE '7'.
00248          16  CP-EARNING-METHOD     PIC X.
00249            88  CP-EARN-BY-R78                      VALUE '1' 'R'.
00250            88  CP-EARN-BY-PRORATA                  VALUE '2' 'P'.
00251            88  CP-EARN-AS-CALIF                    VALUE '3' 'C'.
00252            88  CP-EARN-AS-TEXAS                    VALUE '4' 'T'.
00253            88  CP-EARN-AS-FARM-PLAN                VALUE '4' 'T'.
00254            88  CP-EARN-AS-NET-PAY                  VALUE '5' 'N'.
00255            88  CP-EARN-ANTICIPATION                VALUE '6' 'A'.
00256            88  CP-EARN-AS-MEAN                     VALUE '8' 'M'.
00257            88  CP-EARN-AS-SUM-OF-DIGITS            VALUE '9'.
00258            88  CP-EARN-AS-REG-BALLOON              VALUE 'B'.
033104           88  CP-GAP-NON-REFUNDABLE               VALUE 'G'.
033104           88  CP-GAP-ACTUARIAL                    VALUE 'S'.
092310           88  CP-DCC-SPP-DDF                      VALUE 'D' 'I'.
                 88  CP-DCC-SPP-DDF-IU                   VALUE 'I'.
00259          16  CP-PROCESS-TYPE       PIC X.
00260            88  CP-CLAIM                            VALUE '1'.
00261            88  CP-CANCEL                           VALUE '2'.
00262            88  CP-ISSUE                            VALUE '3'.
00263          16  CP-SPECIAL-CALC-CD    PIC X.
00264            88  CP-OUTSTANDING-BAL              VALUE 'O'.
00265            88  CP-1-MTH-INTEREST               VALUE ' '.
00266            88  CP-0-MTH-INTEREST               VALUE 'A'.
00267            88  CP-OB-OFFLINE-RESERVED          VALUE 'B'.
00268            88  CP-CRITICAL-PERIOD              VALUE 'C'.
00269            88  CP-TERM-IS-DAYS                 VALUE 'D'.
00270            88  CP-USE-PREM-AS-ENTERED          VALUE 'E'.
00271            88  CP-FARM-PLAN                    VALUE 'F'.
00272            88  CP-RATE-AS-STANDARD             VALUE 'G'.
00273            88  CP-2-MTH-INTEREST               VALUE 'I'.
00274            88  CP-3-MTH-INTEREST               VALUE 'J'.
00275            88  CP-4-MTH-INTEREST               VALUE 'K'.
00276            88  CP-BALLOON-LAST-PMT             VALUE 'L'.
00277            88  CP-MORTGAGE-REC                 VALUE 'M'.
00278            88  CP-OUTSTANDING-BALANCE          VALUE 'O'.
00279            88  CP-NET-PAY-PRUDENTIAL           VALUE 'P'.
00280            88  CP-NET-PAY-SIMPLE               VALUE 'S'.
00281            88  CP-TRUNCATED-LIFE               VALUE 'T' 'U' 'V'
00282                                                      'W' 'X'.
00283            88  CP-TRUNCATE-0-MTH               VALUE 'T'.
00284            88  CP-TRUNCATE-1-MTH               VALUE 'U'.
00285            88  CP-TRUNCATE-2-MTH               VALUE 'V'.
00286            88  CP-TRUNCATE-3-MTH               VALUE 'W'.
00287            88  CP-TRUNCATE-4-MTH               VALUE 'X'.
00288            88  CP-SUMMARY-REC                  VALUE 'Z'.
00289            88  CP-PROPERTY-BENEFIT             VALUE '2'.
00290            88  CP-UNEMPLOYMENT-BENEFIT         VALUE '3'.
00291            88  CP-AD-D-BENEFIT                 VALUE '4'.
00292            88  CP-CSL-METH-1                   VALUE '5'.
00293            88  CP-CSL-METH-2                   VALUE '6'.
00294            88  CP-CSL-METH-3                   VALUE '7'.
00295            88  CP-CSL-METH-4                   VALUE '8'.
00296
00297          16  CP-LOAN-TERM          PIC S9(3)       VALUE ZERO
00298                                      COMP-3.
00299          16  CP-CLASS-CODE         PIC XX          VALUE ZERO.
00300          16  CP-DEVIATION-CODE     PIC XXX         VALUE ZERO.
00301          16  CP-STATE              PIC XX          VALUE SPACE.
00302          16  CP-STATE-STD-ABBRV    PIC XX          VALUE SPACE.
00303          16  CP-BENEFIT-CD         PIC XX          VALUE ZERO.
00304            88  CP-CSL-VALID-NP-BENEFIT-CD VALUES '12' '13'
00305                '34' '35' '36' '37' '44' '45' '46' '47' '72' '73'.
00306          16  CP-R78-OPTION         PIC X.
00307            88  CP-TERM-TIMES-TERM-PLUS-1           VALUE ' '.
00308            88  CP-TERM-TIMES-TERM                  VALUE '1'.
00309
00310          16  CP-COMPANY-CD         PIC X             VALUE SPACE.
00311          16  CP-IBNR-RESERVE-SW    PIC X.
00312          16  CP-CLAIM-STATUS       PIC X.
00313          16  CP-RATE-FILE          PIC X.
00314          16  CP-TERM-OR-EXT-DAYS   PIC S9(05)        VALUE ZERO
00315                                      COMP-3.
00316
00317          16  CP-LIFE-OVERRIDE-CODE PIC X.
00318          16  CP-AH-OVERRIDE-CODE   PIC X.
00319
00320          16  CP-RATE-DEV-PCT       PIC S9V9(6)       VALUE ZERO
00321                                      COMP-3.
               16  CP-CLP-RATE-UP        REDEFINES CP-RATE-DEV-PCT
                                         PIC S9(5)V99 COMP-3.
00322          16  CP-CRITICAL-MONTHS    PIC S9(3)         VALUE ZERO
00323                                      COMP-3.
00324          16  CP-ALTERNATE-BENEFIT  PIC S9(9)V99      VALUE ZERO
00325                                      COMP-3.
00326          16  CP-ALTERNATE-PREMIUM  PIC S9(7)V99      VALUE ZERO
00327                                      COMP-3.
               16  CP-DDF-CSO-ADMIN-FEE REDEFINES CP-ALTERNATE-PREMIUM
                                        PIC S9(7)V99 COMP-3.
00328
00329          16  CP-PAID-FROM-DATE     PIC X(02).
00330          16  CP-CLAIM-CALC-METHOD  PIC X(01).
00331          16  CP-EXT-DAYS-CALC      PIC X.
00332            88  CP-EXT-NO-CHG                   VALUE ' '.
00333            88  CP-EXT-CHG-LF                   VALUE '1'.
00334            88  CP-EXT-CHG-AH                   VALUE '2'.
00335            88  CP-EXT-CHG-LF-AH                VALUE '3'.
00336          16  CP-DOMICILE-STATE     PIC XX.
00337          16  CP-CARRIER            PIC X.
00338          16  CP-REIN-FLAG          PIC X.
00339          16  CP-REM-TRM-CALC-OPTION PIC X.
00340            88  VALID-REM-TRM-CALC-OPTION    VALUE '1'
00341                       '2' '3' '4' '5'.
00342            88  CP-CALC-OPTION-DEFAULT       VALUE '4'.
00343            88  CP-CONSIDER-EXTENSION        VALUE '3' '4' '5'.
00344            88  CP-30-DAY-MONTH              VALUE '1' '3' '5'.
00345            88  CP-NO-EXT-30-DAY-MONTH       VALUE '1'.
00346            88  CP-NO-EXT-ACTUAL-DAYS        VALUE '2'.
00347            88  CP-EXT-30-DAY-MONTH          VALUE '3'.
00348            88  CP-EXT-ACTUAL-DAYS           VALUE '4'.
                 88  CP-USE-EXP-AND-1ST-PMT       VALUE '5'.
00349          16  CP-SIG-SWITCH         PIC X.
00350          16  CP-RATING-METHOD      PIC X.
00351            88  CP-RATE-AS-R78                      VALUE '1' 'R'.
00352            88  CP-RATE-AS-PRORATA                  VALUE '2' 'P'.
00353            88  CP-RATE-AS-CALIF                    VALUE '3' 'C'.
00354            88  CP-RATE-AS-TEXAS                    VALUE '4' 'T'.
00355            88  CP-RATE-AS-FARM-PLAN                VALUE '4' 'T'.
00356            88  CP-RATE-AS-NET-PAY                  VALUE '5' 'N'.
00357            88  CP-RATE-AS-ANTICIPATION             VALUE '6' 'A'.
00358            88  CP-RATE-AS-MEAN                     VALUE '8' 'M'.
00359            88  CP-RATE-AS-REG-BALLOON              VALUE 'B'.
00360          16  CP-SALES-TAX          PIC S9V9999     VALUE  ZEROS
00361                                      COMP-3.
090803         16  CP-BEN-CATEGORY       PIC X.
011904         16  CP-DCC-LF-RATE        PIC S99V9(5) COMP-3 VALUE +0.
               16  CP-DCC-ACT-COMM REDEFINES CP-DCC-LF-RATE
                                         PIC S99V9(5) COMP-3.
011904         16  CP-DCC-AH-RATE        PIC S99V9(5) COMP-3 VALUE +0.
               16  CP-DCC-PMF-COMM REDEFINES CP-DCC-AH-RATE
                                         PIC S99V9(5) COMP-3.
080305         16  CP-DAYS-TO-1ST-PMT    PIC S999     COMP-3 VALUE +0.
               16  CP-AH-BALLOON-SW      PIC X  VALUE ' '.
041310         16  CP-EXPIRE-DT          PIC XX.
041710         16  CP-LF-CLAIM-CALC-SW   PIC X  VALUE ' '.
               16  CP-DDF-HI-FACT        PIC S9V999   COMP-3 VALUE +0.
               16  CP-DDF-LO-FACT        PIC S9V999   COMP-3 VALUE +0.
               16  CP-DDF-CLP            PIC S9(5)V99 COMP-3 VALUE +0.
               16  CP-DDF-SPEC-CALC      PIC X.
                   88  CP-CALC-GROSS-FEE        VALUE 'G'.
                   88  CP-CALC-CLP              VALUE 'C'.
               16  CP-IU-RATE-UP         PIC S9(5)V99   COMP-3 VALUE +0.
               16  CP-CANCEL-REASON      PIC X.
               16  CP-DDF-ADMIN-FEES     PIC S9(5)V99 COMP-3 VALUE +0.
               16  CP-PMT-MODE           PIC X.
               16  CP-NO-OF-PMTS         PIC S999 COMP-3 VALUE +0.
071211         16  CP-1ST-YR-ALLOW       PIC S999V99 COMP-3 VALUE +0.
               16  CP-DDF-COMM-AND-MFEE  PIC S9(5)V99 COMP-3 VALUE +0.
               16  CP-DDF-YR1AF          PIC S9(5)V99 COMP-3 VALUE +0.
071211         16  FILLER                PIC X.
00363
00364 ***************    OUTPUT FROM ELRESV   ************************
00365
00366          16  CP-CDT-TABLE          PIC 9             VALUE ZERO.
00367
00368          16  CP-CDT-FACTOR         PIC S9(5)V9(6)    VALUE ZERO
00369                                      COMP-3.
101807         16  CP-PTC-RESERVE        PIC S9(7)V99   VALUE ZERO
101807                                     COMP-3.
101807         16  CP-IBNR-RESERVE       PIC S9(7)V99   VALUE ZERO
101807                                     COMP-3.
101807         16  CP-FUTURE-RESERVE     PIC S9(7)V99   VALUE ZERO
101807                                     COMP-3.
101807         16  FILLER                PIC X(09).
00377 ***************    OUTPUT FROM ELRTRM   *************************
00378
00379          16  CP-REMAINING-TERM-1   PIC S9(4)V9    VALUE ZERO
00380                                      COMP-3.
00381          16  CP-REMAINING-TERM-2   PIC S9(4)V9    VALUE ZERO
00382                                      COMP-3.
00383          16  CP-REMAINING-TERM-3   PIC S9(4)V9    VALUE ZERO
00384                                      COMP-3.
00385          16  CP-ODD-DAYS           PIC S9(3)      VALUE ZERO
00386                                      COMP-3.
00387          16  FILLER                PIC X(12).
00388
00389 ***************    OUTPUT FROM ELRAMT   *************************
00390
00391          16  CP-REMAINING-AMT      PIC S9(9)V99   VALUE ZERO
00392                                      COMP-3.
00393          16  CP-REMAINING-AMT-PRV  PIC S9(9)V99   VALUE ZERO
00394                                      COMP-3.
00395          16  FILLER                PIC X(12).
00396
00397 ***************    OUTPUT FROM ELRATE   *************************
00398
00399          16  CP-CALC-PREMIUM       PIC S9(7)V99   VALUE ZERO
00400                                      COMP-3.
00401          16  CP-PREMIUM-RATE       PIC S9(2)V9(5) VALUE ZERO
00402                                      COMP-3.
00403          16  CP-MORTALITY-CODE     PIC X(4).
00404          16  CP-RATE-EDIT-FLAG     PIC X.
00405              88  CP-RATES-NEED-APR                  VALUE '1'.
00406          16  CP-COMPOSITE-RATE     PIC S99V999    VALUE ZERO
00407                                      COMP-3.
010716         16  CP-CANCEL-FEE         PIC S9(3)V99 VALUE +0 COMP-3.
032905         16  CP-LF-PREM            PIC S9(7)V99 VALUE +0 COMP-3.
               16  CP-LF-BALLOON-PREM REDEFINES CP-LF-PREM
                                         PIC S9(7)V99 COMP-3.
00409          16  FILLER                PIC X(07).
00410
00411 ***************    OUTPUT FROM ELRFND   *************************
00412
00413          16  CP-CALC-REFUND        PIC S9(7)V99   VALUE ZERO
00414                                      COMP-3.
00415          16  CP-REFUND-TYPE-USED   PIC X.
00416            88  CP-R-AS-R78                         VALUE '1'.
00417            88  CP-R-AS-PRORATA                     VALUE '2'.
00418            88  CP-R-AS-CALIF                       VALUE '3'.
00419            88  CP-R-AS-TEXAS                       VALUE '4'.
00420            88  CP-R-AS-FARM-PLAN                   VALUE '4'.
00421            88  CP-R-AS-NET-PAY                     VALUE '5'.
00422            88  CP-R-AS-ANTICIPATION                VALUE '6'.
00423            88  CP-R-AS-MEAN                        VALUE '8'.
00424            88  CP-R-AS-SUM-OF-DIGITS               VALUE '9'.
033104           88  CP-R-AS-GAP-NON-REFUND              VALUE 'G'.
033104           88  CP-R-AS-GAP-ACTUARIAL               VALUE 'S'.
092310           88  CP-R-AS-SPP-DDF                     VALUE 'D'.
092310           88  CP-R-AS-SPP-DDF-IU                  VALUE 'I'.
                 88  CP-R-AS-REPOSSESSION                VALUE 'R'.
00425          16  FILLER                PIC X(12).
00426
00427 ***************    OUTPUT FROM ELEARN   *************************
00428
00429          16  CP-R78-U-PRM          PIC S9(7)V99   VALUE ZERO
00430                                      COMP-3.
00431          16  CP-R78-U-PRM-ALT      PIC S9(7)V99   VALUE ZERO
00432                                      COMP-3.
00433          16  CP-PRORATA-U-PRM      PIC S9(7)V99   VALUE ZERO
00434                                      COMP-3.
00435          16  CP-PRORATA-U-PRM-ALT  PIC S9(7)V99   VALUE ZERO
00436                                      COMP-3.
00437          16  CP-STATE-U-PRM        PIC S9(7)V99   VALUE ZERO
00438                                      COMP-3.
00439          16  CP-DOMICILE-U-PRM     PIC S9(7)V99   VALUE ZERO
00440                                      COMP-3.
00441          16  CP-EARNING-TYPE-USED  PIC X.
00442            88  CP-E-AS-SPECIAL                     VALUE 'S'.
00443            88  CP-E-AS-R78                         VALUE '1'.
00444            88  CP-E-AS-PRORATA                     VALUE '2'.
00445            88  CP-E-AS-TEXAS                       VALUE '4'.
00446            88  CP-E-AS-FARM-PLAN                   VALUE '4'.
00447            88  CP-E-AS-NET-PAY                     VALUE '5'.
00448            88  CP-E-AS-ANTICIPATION                VALUE '6'.
00449            88  CP-E-AS-MEAN                        VALUE '8'.
00450            88  CP-E-AS-SUM-OF-DIGITS               VALUE '9'.
00451          16  FILLER                PIC X(12).
00452
00453 ***************    OUTPUT FROM ELPMNT   *************************
00454
00455          16  CP-ACTUAL-DAYS        PIC S9(05)     VALUE ZERO
00456                                      COMP-3.
00457          16  CP-CLAIM-PAYMENT      PIC S9(7)V99   VALUE ZERO
00458                                      COMP-3.
00459          16  FILLER                PIC X(12).
00460
00461 ***************   MISC WORK AREAS    *****************************
00462          16  CP-TOTAL-PAID         PIC S9(7)V99   VALUE ZERO
00463                                      COMP-3.
00464          16  CP-R-MAX-ATT-AGE      PIC S9(3)      VALUE ZERO
00465                                      COMP-3.
00466          16  CP-R-MAX-AGE          PIC S9(3)      VALUE ZERO
00467                                      COMP-3.
00468          16  CP-R-MAX-TERM         PIC S9(5)      VALUE ZERO
00469                                      COMP-3.
00470          16  CP-R-MAX-TOT-BEN      PIC S9(7)V99   VALUE ZERO
00471                                      COMP-3.
00472          16  CP-R-MAX-MON-BEN      PIC S9(7)V99   VALUE ZERO
00473                                      COMP-3.
00474          16  CP-IO-FUNCTION        PIC X          VALUE SPACE.
00475              88  OPEN-RATE-FILE                   VALUE 'O'.
00476              88  CLOSE-RATE-FILE                  VALUE 'C'.
00477              88  IO-ERROR                         VALUE 'E'.
00478
00479          16  CP-FIRST-PAY-DATE     PIC XX.
00480
00481          16  CP-JOINT-INDICATOR    PIC X.
00482
00483          16  CP-RESERVE-REMAINING-TERM
00484                                    PIC S9(4)V9    VALUE ZERO
00485                                      COMP-3.
00486
00487          16  CP-INSURED-BIRTH-DT   PIC XX.
00488
00489          16  CP-INCURRED-AGE       PIC S9(3)      VALUE ZERO
00490                                      COMP-3.
00491
00492          16  CP-MONTHLY-PAYMENT    PIC S9(5)V99   VALUE ZERO
00493                                      COMP-3.
00494
00495          16  CP-RATING-BENEFIT-AMT PIC S9(9)V99   VALUE ZERO
00496                                      COMP-3.
00497
00498          16  CP-ODD-DAYS-TO-PMT    PIC S9(3)      VALUE ZERO
00499                                      COMP-3.
00500
00501          16  CP-MNTHS-TO-FIRST-PMT PIC S9(3)      VALUE ZERO
00502                                      COMP-3.
00503
00504          16  CP-REMAMT-FACTOR      PIC S9(4)V9(9) VALUE ZEROS
00505                                      COMP-3.
00506
00507          16  CP-FREE-LOOK          PIC S9(3)      VALUE ZERO
00508                                      COMP-3.
00509
00510          16  CP-ROA-REFUND         PIC X          VALUE 'N'.
00511              88  CP-ROA-PREM-AT-REFUND            VALUE 'Y'.
00512
010303         16  CP-NET-BENEFIT-AMT    PIC S9(9)V99   VALUE ZERO
010303                                     COMP-3.
041710         16  CP-SCNP-6MO-AMT       PIC S9(9)V99   VALUE ZERO
041710                                     COMP-3.
               16  CP-MONTH              PIC S999     COMP-3 VALUE +0.
040615         16  cp-extra-periods      pic 9 value zeros.
070115         16  cp-net-only-state     pic x value spaces.
041710         16  FILLER                PIC X(13).
00514 ******************************************************************
00322  EJECT
      *                            COPY ERCCTBL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCTBL                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003
00006 *                                                                *
00007 *   ONLINE CREDIT SYSTEM                                         *
00008 *                                                                *
00009 *   FILE DESCRIPTION = COMPENSATION TABLE                        *
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 200   RECFORM = FIXED                          *
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ERCTBL                   RKP=2,LEN=7     *
00015 *       ALTERNATE PATH = NONE                                    *
00016 *                                                                *
00017 *   LOG = NO                                                     *
00018 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00019 *                                                                *
00020 *                                                                *
00021 ******************************************************************
00022
00023  01  COMM-TABLE-RECORD.
00024      12  CT-RECORD-ID                      PIC XX.
00025          88  VALID-CT-ID                      VALUE 'CT'.
00026
00027      12  CT-CONTROL-PRIMARY.
00028          16  CT-COMPANY-CD                 PIC X.
00029          16  CT-TABLE                      PIC XXX.
00030          16  CT-CNTRL-2.
00031              20  CT-BEN-TYPE               PIC X.
00032              20  CT-BEN-CODE               PIC XX.
00033
00034      12  CT-MAINT-INFORMATION.
00035          16  CT-LAST-MAINT-DT              PIC XX.
00036          16  CT-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
00037          16  CT-LAST-MAINT-USER            PIC X(4).
00038          16  FILLER                        PIC X(31).
00039
00040      12  CT-LIMITS.
00041          16  CT-TBF OCCURS 3 TIMES         PIC S9(7)V99   COMP-3.
00042
00043          16  CT-AGE OCCURS 3 TIMES         PIC S99        COMP-3.
00044
00045          16  CT-TRM OCCURS 3 TIMES         PIC S999       COMP-3.
00046
00047      12  CT-RATES.
00048          16  CT-RTX          OCCURS 27 TIMES.
00049              20  CT-RT                     PIC SV9(5)     COMP-3.
00050              20  CT-RT-R   REDEFINES
00051                  CT-RT                     PIC XXX.
00052
00053      12  FILLER                            PIC  X(42).
00054
00055 ******************************************************************
00324
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
00326
00327  01  DFHCOMMAREA                 PIC  X(1024).
00328
00329 *01 PARMLIST .
00330 *    12  FILLER                  PIC S9(08)      COMP.
00331 *    12  ELCNTL-POINTER          PIC S9(08)      COMP.
00332 *    12  ELACCT-POINTER          PIC S9(08)      COMP.
00333 *    12  ELCERT-POINTER          PIC S9(08)      COMP.
00335 *                            COPY ELCCNTL.
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
00337 *                            COPY ERCACCT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCACCT                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.031                          *
00006 *                                                                *
00007 *   CREDIT SYSTEM ACCOUNT MASTER FILE                            *
00008 *                                                                *
00009 *   THIS COPYBOOK IS USED FOR BOTH THE ONLINE AND BATCH          *
00010 *   VSAM ACCOUNT MASTER FILES.                                   *
00011 *                                                                *
00012 *   FILE DESCRIPTION = ACCOUNT OR PRODUCER FILES                 *
00013 *                                                                *
00014 *   FILE TYPE = VSAM,KSDS                                        *
00015 *   RECORD SIZE = 2000  RECFORM = FIX                            *
00016 *                                                                *
00017 *   BASE CLUSTER NAME = ERACCT                    RKP=2,LEN=26   *
00018 *       ALTERNATE PATH1 = ERACCT2 (ALT GROUPING) RKP=28,LEN=26   *
00019 *                                                                *
00020 *   LOG = NO                                                     *
00021 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00022 *                                                                *
00023 *                                                                *
00024 ******************************************************************
102004*                   C H A N G E   L O G
102004*
102004* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
102004*-----------------------------------------------------------------
102004*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
102004* EFFECTIVE    NUMBER
102004*-----------------------------------------------------------------
102004* 102004    2003031400002  PEMA  ADD NEW STATUS CODE
092705* 092705    2005050300006  PEMA  ADD SPP LEASES
022808* 022808    2007083100002  PEMA  ADD FREEZE STATUS
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
030211* 030211  CR2010012100001  PEMA  ADD EMAILS FROM RDS
031811* 031811  CR2011012700001  PEMA  ADD ACCT STATUS S - SUSPENDED
101711* 101711  CR2011092000001  PEMA  ADD UNEARNED FACTOR STATE FOR DCC
021916* 021916  CR2014010900001  TANA  ADD NEW STATUS CODE VALUES
102004******************************************************************
00025
00026  01  ACCOUNT-MASTER.
00027      12  AM-RECORD-ID                      PIC XX.
00028          88  VALID-AM-ID                      VALUE 'AM'.
00029
00030      12  AM-CONTROL-PRIMARY.
00031          16  AM-COMPANY-CD                 PIC X.
00032          16  AM-MSTR-CNTRL.
00033              20  AM-CONTROL-A.
00034                  24  AM-CARRIER            PIC X.
00035                  24  AM-GROUPING.
00036                      28 AM-GROUPING-PREFIX PIC XXX.
00037                      28 AM-GROUPING-PRIME  PIC XXX.
00038                  24  AM-STATE              PIC XX.
00039                  24  AM-ACCOUNT.
00040                      28  AM-ACCOUNT-PREFIX PIC X(4).
00041                      28  AM-ACCOUNT-PRIME  PIC X(6).
00042              20  AM-CNTRL-1   REDEFINES   AM-CONTROL-A
00043                                            PIC X(19).
00044              20  AM-CNTRL-B.
00045                  24  AM-EXPIRATION-DT      PIC XX.
00046                  24  FILLER                PIC X(4).
00047              20  AM-CNTRL-2 REDEFINES AM-CNTRL-B.
00048                  24  AM-EXPIRE-DT          PIC 9(11)  COMP-3.
00049
00050      12  AM-CONTROL-BY-VAR-GRP.
00051          16  AM-COMPANY-CD-A1              PIC X.
00052          16  AM-VG-CARRIER                 PIC X.
00053          16  AM-VG-GROUPING                PIC X(6).
00054          16  AM-VG-STATE                   PIC XX.
00055          16  AM-VG-ACCOUNT                 PIC X(10).
00056          16  AM-VG-DATE.
00057              20  AM-VG-EXPIRATION-DT       PIC XX.
00058              20  FILLER                    PIC X(4).
00059          16  AM-VG-EXP-DATE REDEFINES AM-VG-DATE
00060                                            PIC 9(11)      COMP-3.
030211     12  FILLER REDEFINES AM-CONTROL-BY-VAR-GRP.
030211         16  FILLER                        PIC X(10).
030211         16  AM-VG-KEY3.
030211             20  AM-VG3-ACCOUNT            PIC X(10).
030211             20  AM-VG3-EXP-DT             PIC XX.
030211         16  FILLER                        PIC X(4).
00061      12  AM-MAINT-INFORMATION.
00062          16  AM-LAST-MAINT-DT              PIC XX.
00063          16  AM-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
00064          16  AM-LAST-MAINT-USER            PIC X(4).
00065          16  FILLER                        PIC XX.
00066
00067      12  AM-EFFECTIVE-DT                   PIC XX.
00068      12  AM-EFFECT-DT                      PIC 9(11)      COMP-3.
00069
00070      12  AM-PREV-DATES  COMP-3.
00071          16  AM-PREV-EXP-DT                PIC 9(11).
00072          16  AM-PREV-EFF-DT                PIC 9(11).
00073
00074      12  AM-REPORT-CODE-1                  PIC X(10).
00075      12  AM-REPORT-CODE-2                  PIC X(10).
00076
00077      12  AM-CITY-CODE                      PIC X(4).
00078      12  AM-COUNTY-PARISH                  PIC X(6).
00079
00080      12  AM-NAME                           PIC X(30).
00081      12  AM-PERSON                         PIC X(30).
00082      12  AM-ADDRS                          PIC X(30).
00083      12  AM-CITY.
               16  AM-ADDR-CITY                  PIC X(28).
               16  AM-ADDR-STATE                 PIC XX.
00084      12  AM-ZIP.
00085          16  AM-ZIP-PRIME.
00086              20  AM-ZIP-PRI-1ST            PIC X.
00087                  88  AM-CANADIAN-POST-CODE    VALUE 'A' THRU 'Z'.
00088              20  FILLER                    PIC X(4).
00089          16  AM-ZIP-PLUS4                  PIC X(4).
00090      12  AM-CANADIAN-POSTAL-CODE  REDEFINES  AM-ZIP.
00091          16  AM-CAN-POSTAL-1               PIC XXX.
00092          16  AM-CAN-POSTAL-2               PIC XXX.
00093          16  FILLER                        PIC XXX.
00094      12  AM-TEL-NO.
00095          16  AM-AREA-CODE                  PIC 999.
00096          16  AM-TEL-PRE                    PIC 999.
00097          16  AM-TEL-NBR                    PIC 9(4).
00098      12  AM-TEL-LOC                        PIC X.
00099          88  AM-TEL-AT-HOME                   VALUE 'H'.
00100          88  AM-TEL-AT-BUSINESS               VALUE 'B'.
00101
00102      12  AM-COMM-STRUCTURE.
00103          16  AM-DEFN-1.
00104              20  AM-AGT-COMMS       OCCURS 10 TIMES.
00105                  24  AM-AGT.
00106                      28  AM-AGT-PREFIX     PIC X(4).
00107                      28  AM-AGT-PRIME      PIC X(6).
00108                  24  AM-COM-TYP            PIC X.
00109                  24  AM-L-COM              PIC SV9(5)     COMP-3.
00110                  24  AM-J-COM              PIC SV9(5)     COMP-3.
00111                  24  AM-A-COM              PIC SV9(5)     COMP-3.
00112                  24  AM-RECALC-LV-INDIC    PIC X.
00113                  24  AM-RETRO-LV-INDIC     PIC X.
00114                  24  AM-GL-CODES           PIC X.
00115                  24  AM-COMM-CHARGEBACK    PIC 9(02).
00116                  24  FILLER                PIC X(01).
00117          16  AM-DEFN-2   REDEFINES   AM-DEFN-1.
00118              20  AM-COM-TBLS        OCCURS 10 TIMES.
00119                  24  FILLER                PIC X(11).
00120                  24  AM-L-COMA             PIC XXX.
00121                  24  AM-J-COMA             PIC XXX.
00122                  24  AM-A-COMA             PIC XXX.
00123                  24  FILLER                PIC X(6).
00124
00125      12  AM-COMM-CHANGE-STATUS             PIC X.
00126          88  AM-COMMISSIONS-CHANGED           VALUE '*'.
00127
00128      12  AM-CSR-CODE                       PIC X(4).
00129
00130      12  AM-BILLING-STATUS                 PIC X.
00131          88  AM-ACCOUNT-BILLED                VALUE 'B'.
00132          88  AM-ACCOUNT-NOT-BILLED            VALUE ' '.
00133      12  AM-AUTO-REFUND-SW                 PIC X.
00134          88  AUTO-REFUNDS-USED                VALUE 'Y'.
00135          88  AUTO-REFUNDS-NOT-USED            VALUE 'N' ' '.
00136      12  AM-GPCD                           PIC 99.
00137      12  AM-IG                             PIC X.
00138          88  AM-HAS-INDIVIDUAL                VALUE '1'.
00139          88  AM-HAS-GROUP                     VALUE '2'.
00140      12  AM-STATUS                         PIC X.
00141          88  AM-ACCOUNT-ACTIVE                VALUE '0'.
00142          88  AM-ACCOUNT-INACTIVE              VALUE '1'.
00143          88  AM-ACCOUNT-TRANSFERRED           VALUE '2'.
102004         88  AM-ACCOUNT-CANCELLED             VALUE '3'.
022808         88  AM-ACCOUNT-FROZEN                VALUE '4'.
031811         88  AM-ACCOUNT-SUSPENDED             VALUE '5'.
021916         88  AM-ACCOUNT-DROPPED               VALUE '6'.
021916         88  AM-ACCOUNT-LAPSED                VALUE '7'.
021916         88  AM-ACCOUNT-RUN-OFF               VALUE '8'.
021916         88  AM-ACCOUNT-PENDING               VALUE '9'.
00144      12  AM-REMIT-TO                       PIC 99.
00145      12  AM-ID-NO                          PIC X(11).
00146
00147      12  AM-CAL-TABLE                      PIC XX.
00148      12  AM-LF-DEVIATION                   PIC XXX.
00149      12  AM-AH-DEVIATION                   PIC XXX.
00150      12  AM-LF-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
00151      12  AM-AH-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
00152      12  AM-LF-OB-RATE                     PIC S99V9(5)   COMP-3.
00153      12  AM-AH-OB-RATE                     PIC S99V9(5)   COMP-3.
00154      12  AM-LF-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
00155      12  AM-AH-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
00156
00157      12  AM-USER-FIELDS.
00158          16  AM-FLD-1                      PIC XX.
00159          16  AM-FLD-2                      PIC XX.
00160          16  AM-FLD-3                      PIC XX.
00161          16  AM-FLD-4                      PIC XX.
00162          16  AM-FLD-5                      PIC XX.
00163
00164      12  AM-1ST-PROD-DATE.
00165          16  AM-1ST-PROD-YR                PIC XX.
00166          16  AM-1ST-PROD-MO                PIC XX.
00167          16  AM-1ST-PROD-DA                PIC XX.
00168      12  AM-ANNIVERSARY-DATE               PIC 9(11)  COMP-3.
00169      12  AM-CERTS-PURGED-DATE.
00170          16  AM-PUR-YR                     PIC XX.
00171          16  AM-PUR-MO                     PIC XX.
00172          16  AM-PUR-DA                     PIC XX.
00173      12  AM-HI-CERT-DATE                   PIC 9(11)  COMP-3.
00174      12  AM-LO-CERT-DATE                   PIC 9(11)  COMP-3.
00175      12  AM-ENTRY-DATE                     PIC 9(11)  COMP-3.
00176      12  AM-INACTIVE-DATE.
00177          16  AM-INA-MO                     PIC 99.
00178          16  AM-INA-DA                     PIC 99.
00179          16  AM-INA-YR                     PIC 99.
00180      12  AM-AR-HI-CERT-DATE                PIC XX.
00181
00182      12  AM-LF-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
00183      12  AM-AH-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
00184
00185      12  AM-OB-PAYMENT-MODE                PIC X.
00186          88  AM-OB-PAID-MONTHLY               VALUE 'M' ' '.
00187          88  AM-OB-PAID-QUARTERLY             VALUE 'Q'.
00188          88  AM-OB-PAID-SEMI-ANNUALLY         VALUE 'S'.
00189          88  AM-OB-PAID-ANNUALLY              VALUE 'A'.
00190
00191      12  AM-AH-ONLY-INDICATOR              PIC X.
00192          88  AM-AH-ONLY-ALLOWED               VALUE 'Y' ' '.
00193          88  AM-NO-AH-ONLY                    VALUE 'N'.
00194
00195      12  AM-EDIT-LOAN-OFC                  PIC X(01).
00196
00197      12  AM-OVER-SHORT.
00198          16 AM-OVR-SHT-AMT                 PIC S999V99    COMP-3.
00199          16 AM-OVR-SHT-PCT                 PIC S9V9(4)    COMP-3.
00200
011410     12  AM-DCC-PRODUCT-CODE               PIC XXX.
041910     12  AM-DCC-CLP-STATE                  PIC XX.
00202
00203      12  AM-RECALC-COMM                    PIC X.
00204      12  AM-RECALC-REIN                    PIC X.
00205
00206      12  AM-REI-TABLE                      PIC XXX.
00207      12  AM-REI-ET-LF                      PIC X.
00208      12  AM-REI-ET-AH                      PIC X.
00209      12  AM-REI-PE-LF                      PIC X.
00210      12  AM-REI-PE-AH                      PIC X.
00211      12  AM-REI-PRT-ST                     PIC X.
00212      12  AM-REI-FEE-LF                     PIC S9V9999    COMP-3.
00213      12  AM-REI-FEE-AH                     PIC S9V9999    COMP-3.
00214      12  AM-REI-LF-TAX                     PIC S9V9999    COMP-3.
00215      12  AM-REI-GROUP-A                    PIC X(6).
00216      12  AM-REI-MORT                       PIC X(4).
00217      12  AM-REI-PRT-OW                     PIC X.
00218      12  AM-REI-PR-PCT                     PIC S9V9999    COMP-3.
00219      12  AM-REI-78-PCT                     PIC S9V9999    COMP-3.
00220      12  AM-REI-AH-TAX                     PIC S9V9999    COMP-3.
00221      12  AM-REI-GROUP-B                    PIC X(6).
00222
00223      12  AM-TRUST-TYPE                     PIC X(2).
00224
00225      12  AM-EMPLOYER-STMT-USED             PIC X.
00226      12  AM-GROUPED-CHECKS-Y-N             PIC X.
00227
00228      12  AM-STD-AH-TYPE                    PIC XX.
00229      12  AM-EARN-METHODS.
00230          16  AM-EARN-METHOD-R              PIC X.
00231              88 AM-REF-RL-R78                 VALUE 'R'.
00232              88 AM-REF-RL-PR                  VALUE 'P'.
00233              88 AM-REF-RL-MEAN                VALUE 'M'.
00234              88 AM-REF-RL-ANTICIPATION        VALUE 'A'.
00235          16  AM-EARN-METHOD-L              PIC X.
00236              88 AM-REF-LL-R78                 VALUE 'R'.
00237              88 AM-REF-LL-PR                  VALUE 'P'.
00238              88 AM-REF-LL-MEAN                VALUE 'M'.
00239              88 AM-REF-LL-ANTICIPATION        VALUE 'A'.
00240          16  AM-EARN-METHOD-A              PIC X.
00241              88 AM-REF-AH-R78                 VALUE 'R'.
00242              88 AM-REF-AH-PR                  VALUE 'P'.
00243              88 AM-REF-AH-MEAN                VALUE 'M'.
00244              88 AM-REF-AH-ANTICIPATION        VALUE 'A'.
00245              88 AM-REF-AH-CALIF-SPEC          VALUE 'C'.
00246              88 AM-REF-AH-NET                 VALUE 'N'.
00247
00248      12  AM-TOL-PREM                       PIC S999V99    COMP-3.
00249      12  AM-TOL-REF                        PIC S999V99    COMP-3.
00250      12  AM-TOL-CLM                        PIC S999V99    COMP-3.
00251
00252      12  AM-RET-Y-N                        PIC X.
00253      12  AM-RET-P-E                        PIC X.
00254      12  AM-LF-RET                         PIC S9V9999    COMP-3.
00255      12  AM-AH-RET                         PIC S9V9999    COMP-3.
00256      12  AM-RET-GRP                        PIC X(6).
00257      12  AM-RETRO-POOL  REDEFINES  AM-RET-GRP.
00258          16  AM-POOL-PRIME                 PIC XXX.
00259          16  AM-POOL-SUB                   PIC XXX.
00260      12  AM-RETRO-EARNINGS.
00261          16  AM-RET-EARN-R                 PIC X.
00262          16  AM-RET-EARN-L                 PIC X.
00263          16  AM-RET-EARN-A                 PIC X.
00264      12  AM-RET-ST-TAX-USE                 PIC X.
00265          88  CHARGE-ST-TAXES-ON-RETRO         VALUE 'Y' 'E' 'P'.
00266          88  TAXES-NOT-IN-RETRO               VALUE 'N' ' '.
00267      12  AM-RETRO-BEG-EARNINGS.
00268          16  AM-RET-BEG-EARN-R             PIC X.
00269          16  AM-RET-BEG-EARN-L             PIC X.
00270          16  AM-RET-BEG-EARN-A             PIC X.
00271      12  AM-RET-MIN-LOSS-L                 PIC SV999      COMP-3.
00272      12  AM-RET-MIN-LOSS-A                 PIC SV999      COMP-3.
00273
00274      12  AM-USER-SELECT-OPTIONS.
00275          16  AM-USER-SELECT-1              PIC X(10).
00276          16  AM-USER-SELECT-2              PIC X(10).
00277          16  AM-USER-SELECT-3              PIC X(10).
00278          16  AM-USER-SELECT-4              PIC X(10).
00279          16  AM-USER-SELECT-5              PIC X(10).
00280
00281      12  AM-LF-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
00282
00283      12  AM-AH-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
00284
00285      12  AM-RPT045A-SWITCH                 PIC X.
00286          88  RPT045A-OFF                   VALUE 'N'.
00287
00288      12  AM-INSURANCE-LIMITS.
00289          16  AM-MAX-MON-BEN                PIC S9(7)      COMP-3.
00290          16  AM-MAX-TOT-BEN                PIC S9(7)      COMP-3.
00291
00292      12  AM-PROFILE-CHANGE-SWITCH          PIC X.
00293          88  AM-PROFILE-DATA-CHANGED          VALUE '*'.
00294
00295      12  AM-DISMBR-COVERAGE-SW             PIC X.
00296          88  AM-DISMBR-COVERAGE               VALUE 'Y'.
00297          88  AM-NO-DISMBR-COVERAGE            VALUE 'N'.
00298
00299      12  AM-CANCEL-FEE                     PIC S9(3)V9(2) COMP-3.
00300
00301      12  AM-TOL-REF-PCT                    PIC S9V9(4)    COMP-3.
090803     12  AM-CLP-TOL-PCT                    PIC S9V9(4)    COMP-3.
092705     12  AM-SPP-LEASE-COMM                 PIC S9(5)V99   COMP-3.
           12  AM-DCC-MAX-MARKETING-FEE          PIC S9(5)      COMP-3.
           12  AM-DCC-UEF-STATE                  PIC XX.
           12  FILLER                            PIC XXX.
120406     12  AM-REPORT-CODE-3                  PIC X(10).
090803*    12  FILLER                            PIC X(22).
00303
00304      12  AM-RESERVE-DATE.
00305          16  AM-TARGET-LOSS-RATIO          PIC S9V9(4) COMP-3.
00306          16  AM-LIFE-IBNR-PCT              PIC S9V9(4) COMP-3.
00307          16  AM-CRDT-MODIFICATION-PCT      PIC S9V9(4) COMP-3.
00308
00309      12  AM-3RD-PARTY-NOTIF-LEVEL          PIC 99.
00310      12  AM-NOTIFICATION-TYPES.
00311          16  AM-NOTIF-OF-LETTERS           PIC X.
00312          16  AM-NOTIF-OF-PAYMENTS          PIC X.
00313          16  AM-NOTIF-OF-REPORTS           PIC X.
00314          16  AM-NOTIF-OF-STATUS            PIC X.
00315
00316      12  AM-BENEFIT-TABLE-USAGE            PIC X.
00317          88  AM-BENEFIT-TABLE-USED            VALUE 'Y'.
00318          88  AM-USE-DEVIATIONS-ONLY           VALUE 'D'.
00319          88  AM-EDIT-BENEFITS-ONLY            VALUE 'E'.
00320          88  AM-EDITS-NOT-USED                VALUE ' '  'N'.
00321
00322      12  AM-BENEFIT-CONTROLS.
00323          16  AM-ALLOWABLE-BENEFITS  OCCURS  20  TIMES.
00324              20  AM-BENEFIT-CODE           PIC XX.
00325              20  AM-BENEFIT-TYPE           PIC X.
00326              20  AM-BENEFIT-REVISION       PIC XXX.
00327              20  AM-BENEFIT-REM-TERM       PIC X.
00328              20  AM-BENEFIT-RETRO-Y-N      PIC X.
00329              20  FILLER                    PIC XX.
00330          16  FILLER                        PIC X(80).
00331
00332      12  AM-TRANSFER-DATA.
00333          16  AM-TRANSFERRED-FROM.
00334              20  AM-TRNFROM-CARRIER        PIC X.
00335              20  AM-TRNFROM-GROUPING.
00336                  24  AM-TRNFROM-GRP-PREFIX PIC XXX.
00337                  24  AM-TRNFROM-GRP-PRIME  PIC XXX.
00338              20  AM-TRNFROM-STATE          PIC XX.
00339              20  AM-TRNFROM-ACCOUNT.
00340                  24  AM-TRNFROM-ACCT-PREFIX PIC X(4).
00341                  24  AM-TRNFROM-ACCT-PRIME PIC X(6).
00342              20  AM-TRNFROM-DTE            PIC XX.
00343          16  AM-TRANSFERRED-TO.
00344              20  AM-TRNTO-CARRIER          PIC X.
00345              20  AM-TRNTO-GROUPING.
00346                  24  AM-TRNTO-GRP-PREFIX   PIC XXX.
00347                  24  AM-TRNTO-GRP-PRIME    PIC XXX.
00348              20  AM-TRNTO-STATE            PIC XX.
00349              20  AM-TRNTO-ACCOUNT.
00350                  24  AM-TRNTO-ACCT-PREFIX  PIC X(4).
00351                  24  AM-TRNTO-ACCT-PRIME   PIC X(6).
00352              20  AM-TRNTO-DTE              PIC XX.
00353          16  FILLER                        PIC X(10).
00354
00355      12  AM-SAVED-REMIT-TO                 PIC 99.
00356
00357      12  AM-COMM-STRUCTURE-SAVED.
00358          16  AM-DEFN-1-SAVED.
00359              20  AM-AGT-COMMS-SAVED    OCCURS 10 TIMES.
00360                  24  AM-AGT-SV             PIC X(10).
00361                  24  AM-COM-TYP-SV         PIC X.
00362                  24  AM-L-COM-SV           PIC SV9(5)     COMP-3.
00363                  24  AM-J-COM-SV           PIC SV9(5)     COMP-3.
00364                  24  AM-A-COM-SV           PIC SV9(5)     COMP-3.
00365                  24  AM-RECALC-LV-INDIC-SV PIC X.
00366                  24  FILLER                PIC X.
00367                  24  AM-GL-CODES-SV        PIC X.
00368                  24  AM-COM-CHARGEBACK-SV  PIC 99.
00369                  24  FILLER                PIC X.
00370          16  AM-DEFN-2-SAVED   REDEFINES   AM-DEFN-1-SAVED.
00371              20  AM-COM-TBLS-SAVED    OCCURS 10 TIMES.
00372                  24  FILLER                PIC X(11).
00373                  24  AM-L-COMA-SV          PIC XXX.
00374                  24  AM-J-COMA-SV          PIC XXX.
00375                  24  AM-A-COMA-SV          PIC XXX.
00376                  24  FILLER                PIC X(6).
00377
00378      12  AM-FLC-NET-PREMIUM-ALLOWANCE.
00379          16 AM-ACCOUNT-ALLOWANCE OCCURS  5 TIMES.
00380             20  AM-ALLOW-BEGIN-RANGE       PIC S9(5)      COMP-3.
00381             20  AM-ALLOW-END-RANGE         PIC S9(5)      COMP-3.
00382             20  AM-ALLOWANCE-AMT           PIC S9(5)V99   COMP-3.
00383
122806     12  AM-ORIG-DEALER-NO                 PIC X(10).
122806     12  FILLER                            PIC X(120).
00385
00386      12  AM-ACCOUNT-EXECUTIVE-DATA.
00387          16  AM-CONTROL-NAME               PIC X(30).
00388          16  AM-EXECUTIVE-ONE.
00389              20  AM-EXEC1-NAME             PIC X(15).
00390              20  AM-EXEC1-DIS-PERCENT      PIC S9(01)V9(04)
00391                                                           COMP-3.
00392              20  AM-EXEC1-LIFE-PERCENT     PIC S9(01)V9(04)
00393                                                           COMP-3.
00394          16  AM-EXECUTIVE-TWO.
00395              20  AM-EXEC2-NAME             PIC X(15).
00396              20  AM-EXEC2-DIS-PERCENT      PIC S9(01)V9(04)
00397                                                           COMP-3.
00398              20  AM-EXEC2-LIFE-PERCENT     PIC S9(01)V9(04)
00399                                                           COMP-3.
00400
00401      12  AM-RETRO-ADDITIONAL-DATA.
00402          16  AM-RETRO-QUALIFY-LIMIT        PIC S9(7)      COMP-3.
00403          16  AM-RETRO-PREM-P-E             PIC X.
00404          16  AM-RETRO-CLMS-P-I             PIC X.
00405          16  AM-RETRO-RET-BRACKET-LF.
00406              20  AM-RETRO-RET-METHOD-LF    PIC X.
00407                  88  AM-RETRO-USE-PCT-LF      VALUE 'P' ' '.
00408                  88  AM-RETRO-USE-SCALE-LF    VALUE 'S'.
00409              20  AM-RETRO-RET-BASIS-LF     PIC X.
00410                  88  AM-RETRO-EARN-BASIS-LF   VALUE 'E' ' '.
00411                  88  AM-RETRO-PAID-BASIS-LF   VALUE 'P'.
00412              20  AM-RETRO-BRACKETS-LF  OCCURS  3 TIMES.
00413                  24  AM-RETRO-RET-PCT-LF   PIC S9V9999    COMP-3.
00414                  24  AM-RETRO-RET-THRU-LF  PIC S9(7)      COMP-3.
00415          16  AM-RETRO-RET-BRACKET-AH.
00416              20  AM-RETRO-RET-METHOD-AH    PIC X.
00417                  88  AM-RETRO-USE-PCT-AH      VALUE 'P' ' '.
00418                  88  AM-RETRO-USE-SCALE-AH    VALUE 'S'.
00419                  88  AM-RETRO-USE-LIFE-METHOD VALUE 'L'.
00420              20  AM-RETRO-RET-BASIS-AH     PIC X.
00421                  88  AM-RETRO-EARN-BASIS-AH   VALUE 'E' ' '.
00422                  88  AM-RETRO-PAID-BASIS-AH   VALUE 'P'.
00423              20  AM-RETRO-BRACKETS-AH  OCCURS  3 TIMES.
00424                  24  AM-RETRO-RET-PCT-AH   PIC S9V9999    COMP-3.
00425                  24  AM-RETRO-RET-THRU-AH  PIC S9(7)      COMP-3.
00426
00427      12  AM-COMMENTS.
00428          16  AM-COMMENT-LINE           PIC X(50)   OCCURS 5 TIMES.
00429
00430      12  AM-CLIENT-OVERLAY-FLI   REDEFINES   AM-COMMENTS.
00431          16  AM-FLI-RETRO-SHARE-CODE       PIC X.
00432          16  AM-FLI-BILLING-CODE           PIC X.
00433          16  AM-FLI-ALT-STATE-CODE         PIC XX.
00434          16  AM-FLI-UNITED-IDENT           PIC X.
00435          16  AM-FLI-INTEREST-LOST-DATA.
00436              20  AM-FLI-BANK-NO            PIC X(5).
00437              20  AM-FLI-BANK-BALANCE       PIC S9(9)V99   COMP-3.
00438              20  AM-FLI-BANK-1ST-6-PREM    PIC S9(9)V99   COMP-3.
00439              20  AM-FLI-BANK-CAP-AMT       PIC S9(9)V99   COMP-3.
00440          16  AM-FLI-ALT-AGENT-CODES   OCCURS 10 TIMES.
00441              20  AM-FLI-AGT                PIC X(9).
00442              20  AM-FLI-AGT-COMM-ACC       PIC X.
00443              20  AM-FLI-AGT-SHARE-PCT      PIC S9V99      COMP-3.
00444          16  FILLER                        PIC X(102).
00445
00446      12  AM-CLIENT-OVERLAY-DMD   REDEFINES   AM-COMMENTS.
00447          16  AM-ALLOWABLE-DMD-BENEFITS  OCCURS 30 TIMES.
00448              20  AM-BENEFIT-DMD-CODE         PIC XX.
00449              20  AM-BENEFIT-DMD-TYPE         PIC X.
00450              20  AM-BENEFIT-DMD-REVISION     PIC XXX.
00451              20  AM-BENEFIT-DMD-REM-TERM     PIC X.
00452              20  AM-BENEFIT-DMD-RETRO-Y-N    PIC X.
00453          16  FILLER                          PIC X(10).
00454 ******************************************************************
00339 *                            COPY ELCCERT.
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
081606*                            COPY ERCMAIL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCMAIL                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = MAILING DATA CAPTURE RECORDS              *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
CIDMOD*   RECORD SIZE = 374   RECFORM = FIX                            *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ERMAIL                 RKP=2,LEN=33      *
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
090408* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
111108* 111108                   PEMA  ADD CRED BENE ADDR2
00017 ******************************************************************
00018
00019  01  MAILING-DATA.
00020      12  MA-RECORD-ID                      PIC XX.
00021          88  VALID-MA-ID                       VALUE 'MA'.
00022
00023      12  MA-CONTROL-PRIMARY.
00024          16  MA-COMPANY-CD                 PIC X.
00025          16  MA-CARRIER                    PIC X.
00026          16  MA-GROUPING.
00027              20  MA-GROUPING-PREFIX        PIC XXX.
00028              20  MA-GROUPING-PRIME         PIC XXX.
00029          16  MA-STATE                      PIC XX.
00030          16  MA-ACCOUNT.
00031              20  MA-ACCOUNT-PREFIX         PIC X(4).
00032              20  MA-ACCOUNT-PRIME          PIC X(6).
00033          16  MA-CERT-EFF-DT                PIC XX.
00034          16  MA-CERT-NO.
00035              20  MA-CERT-PRIME             PIC X(10).
00036              20  MA-CERT-SFX               PIC X.
00037
00038      12  FILLER                            PIC XX.
00039
00040      12  MA-ACCESS-CONTROL.
00041          16  MA-SOURCE-SYSTEM              PIC XX.
00042              88  MA-FROM-CREDIT                VALUE 'CR'.
00043              88  MA-FROM-VSI                   VALUE 'VS'.
00044              88  MA-FROM-WARRANTY              VALUE 'WA'.
00045              88  MA-FROM-OTHER                 VALUE 'OT'.
00046          16  MA-RECORD-ADD-DT              PIC XX.
00047          16  MA-RECORD-ADDED-BY            PIC XXXX.
00048          16  MA-LAST-MAINT-DT              PIC XX.
00049          16  MA-LAST-MAINT-BY              PIC XXXX.
00050          16  MA-LAST-MAINT-HHMMSS          PIC S9(6)       COMP-3.
00051
00052      12  MA-PROFILE-INFO.
00053          16  MA-QUALIFY-CODE-1             PIC XX.
00054          16  MA-QUALIFY-CODE-2             PIC XX.
00055          16  MA-QUALIFY-CODE-3             PIC XX.
00056          16  MA-QUALIFY-CODE-4             PIC XX.
00057          16  MA-QUALIFY-CODE-5             PIC XX.
00058
00059          16  MA-INSURED-LAST-NAME          PIC X(15).
00060          16  MA-INSURED-FIRST-NAME         PIC X(10).
00061          16  MA-INSURED-MIDDLE-INIT        PIC X.
00062          16  MA-INSURED-ISSUE-AGE          PIC 99.
00063          16  MA-INSURED-BIRTH-DT           PIC XX.
00064          16  MA-INSURED-SEX                PIC X.
00065              88  MA-SEX-MALE                   VALUE 'M'.
00066              88  MA-SEX-FEMALE                 VALUE 'F'.
00067          16  MA-INSURED-SOC-SEC-NO         PIC X(11).
00068
080406         16  MA-ADDRESS-CORRECTED          PIC X.
081108         16  MA-JOINT-BIRTH-DT             PIC XX.
00069 *        16  FILLER                        PIC X(12).
00070
00071          16  MA-ADDRESS-LINE-1             PIC X(30).
00072          16  MA-ADDRESS-LINE-2             PIC X(30).
00073          16  MA-CITY-STATE.
                   20  MA-CITY                   PIC X(28).
                   20  MA-ADDR-STATE             PIC XX.
00074          16  MA-ZIP.
00075              20  MA-ZIP-CODE.
00076                  24  MA-ZIP-CODE-1ST       PIC X(1).
00077                      88  MA-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00078                  24  FILLER                PIC X(4).
00079              20  MA-ZIP-PLUS4              PIC X(4).
00080          16  MA-CANADIAN-POSTAL-CODE REDEFINES MA-ZIP.
00081              20  MA-CAN-POSTAL-CODE-1      PIC X(3).
00082              20  MA-CAN-POSTAL-CODE-2      PIC X(3).
00083              20  FILLER                    PIC X(3).
00084
00085          16  MA-PHONE-NO                   PIC 9(11)       COMP-3.
00086
               16  FILLER                        PIC XXX.
00087 *        16  FILLER                        PIC X(10).
00088
           12  MA-CRED-BENE-INFO.
CIDMOD         16  MA-CRED-BENE-NAME                 PIC X(25).
CIDMOD         16  MA-CRED-BENE-ADDR                 PIC X(30).
               16  MA-CRED-BENE-ADDR2                PIC X(30).
CIDMOD         16  MA-CRED-BENE-CTYST.
                   20  MA-CRED-BENE-CITY             PIC X(28).
                   20  MA-CRED-BENE-STATE            PIC XX.
CIDMOD         16  MA-CRED-BENE-ZIP.
CIDMOD             20  MA-CB-ZIP-CODE.
CIDMOD                 24  MA-CB-ZIP-CODE-1ST        PIC X(1).
CIDMOD                     88  MA-CB-CANADIAN-POST-CODE
                                                 VALUE 'A' THRU 'Z'.
CIDMOD                 24  FILLER                    PIC X(4).
CIDMOD             20  MA-CB-ZIP-PLUS4               PIC X(4).
CIDMOD         16  MA-CB-CANADIAN-POSTAL-CODE
                                  REDEFINES MA-CRED-BENE-ZIP.
CIDMOD             20  MA-CB-CAN-POSTAL-CODE-1       PIC X(3).
CIDMOD             20  MA-CB-CAN-POSTAL-CODE-2       PIC X(3).
CIDMOD             20  FILLER                        PIC X(3).
080406     12  MA-POST-CARD-MAIL-DATA.
080406         16  MA-MAIL-DATA OCCURS 7.
080406             20  MA-MAIL-TYPE              PIC X.
080406                 88  MA-12MO-MAILING           VALUE '1'.
080406                 88  MA-EXP-MAILING            VALUE '2'.
080406             20  MA-MAIL-STATUS            PIC X.
080406                 88  MA-MAIL-ST-MAILED         VALUE '1'.
080406                 88  MA-MAIL-ST-RETURNED       VALUE '2'.
080406                 88  MA-MAIL-ST-NOT-MAILED     VALUE '3'.
080406             20  MA-MAIL-DATE              PIC XX.
080406     12  FILLER                            PIC XX.
           12  FILLER                            PIC XX.
080406*    12  FILLER                            PIC X(30).
00090 ******************************************************************
062712*                            COPY ELCCRTO.
      ******************************************************************
      *                                                                *
      *                            ELCCRTO.                            *
      *                                                                *
      *   FILE DESCRIPTION = ORIGINAL CERTIFICATE INFORMATION          *
      *                                                                *
      *   FILE TYPE = VSAM,KSDS                                        *
      *   RECORD SIZE = 524  RECFORM = FIXED                           *
      *                                                                *
      *   BASE CLUSTER = ELCRTO                         RKP=2,LEN=36   *
      *                                                                *
      *   LOG = YES                                                    *
      *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
061011* 061011  2011022800001    PEMA  NEW FILE TO SAVE ORIG CERT INFO
062712* 062712  2011022800001    AJRA  REDEFINE ORIG DATA
071712* 071712  CR2011022800001  AJRA  NAPERSOFT CANCELS
121812* 121812  CR2012101700002  AJRA  ADD DEFAULT AGE FLAG
011413* 011413  IR2012122700003  AJRA  ADD CRTO ISSUE/CANCEL INDICATOR
      ******************************************************************
       01  ORIGINAL-CERTIFICATE.
           12  OC-RECORD-ID                      PIC XX.
               88  VALID-OC-ID                      VALUE 'OC'.
           12  OC-CONTROL-PRIMARY.
               16  OC-COMPANY-CD                 PIC X.
               16  OC-CARRIER                    PIC X.
               16  OC-GROUPING                   PIC X(6).
               16  OC-STATE                      PIC XX.
               16  OC-ACCOUNT                    PIC X(10).
               16  OC-CERT-EFF-DT                PIC XX.
               16  OC-CERT-NO.
                   20  OC-CERT-PRIME             PIC X(10).
                   20  OC-CERT-SFX               PIC X.
               16  OC-RECORD-TYPE                PIC X.
               16  OC-KEY-SEQ-NO                 PIC 9(4) BINARY.
           12  OC-LAST-MAINT-DT                  PIC XX.
           12  OC-LAST-MAINT-BY                  PIC X(4).
           12  OC-LAST-MAINT-HHMMSS              PIC S9(6)   COMP-3.
           12  OC-ENDORSEMENT-PROCESSED-DT       PIC XX.
           12  FILLER                            PIC X(49).
062712     12  OC-ORIG-REC.
062712         16  OC-INS-LAST-NAME              PIC X(15).
062712         16  OC-INS-FIRST-NAME             PIC X(10).
062712         16  OC-INS-MIDDLE-INIT            PIC X.
062712         16  OC-INS-AGE                    PIC S999     COMP-3.
062712         16  OC-JNT-LAST-NAME              PIC X(15).
062712         16  OC-JNT-FIRST-NAME             PIC X(10).
062712         16  OC-JNT-MIDDLE-INIT            PIC X.
062712         16  OC-JNT-AGE                    PIC S999     COMP-3.
062712         16  OC-LF-BENCD                   PIC XX.
062712         16  OC-LF-TERM                    PIC S999      COMP-3.
062712         16  OC-LF-BEN-AMT                 PIC S9(9)V99  COMP-3.
062712         16  OC-LF-PRM-AMT                 PIC S9(7)V99  COMP-3.
062712         16  OC-LF-ALT-BEN-AMT             PIC S9(9)V99  COMP-3.
062712         16  OC-LF-ALT-PRM-AMT             PIC S9(7)V99  COMP-3.
062712         16  OC-LF-EXP-DT                  PIC XX.
062712         16  OC-LF-COMM-PCT                PIC SV9(5)    COMP-3.
062712         16  OC-LF-CANCEL-DT               PIC XX.
062712         16  OC-LF-CANCEL-AMT              PIC S9(7)V99  COMP-3.
071712         16  OC-LF-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
062712         16  OC-AH-BENCD                   PIC XX.
062712         16  OC-AH-TERM                    PIC S999      COMP-3.
062712         16  OC-AH-BEN-AMT                 PIC S9(9)V99  COMP-3.
062712         16  OC-AH-PRM-AMT                 PIC S9(7)V99  COMP-3.
062712         16  OC-AH-EXP-DT                  PIC XX.
062712         16  OC-AH-COMM-PCT                PIC SV9(5)    COMP-3.
062712         16  OC-AH-CP                      PIC 99.
062712         16  OC-AH-CANCEL-DT               PIC XX.
062712         16  OC-AH-CANCEL-AMT              PIC S9(7)V99  COMP-3.
071712         16  OC-AH-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
062712         16  OC-CRED-BENE-NAME             PIC X(25).
062712         16  OC-1ST-PMT-DT                 PIC XX.
121812         16  OC-INS-AGE-DEFAULT-FLAG       PIC X.
121812         16  OC-JNT-AGE-DEFAULT-FLAG       PIC X.
011413         16  OC-ISSUE-TRAN-IND             PIC X.
011413         16  OC-CANCEL-TRAN-IND            PIC X.
011413         16  FILLER                        PIC X(211).
062712
062712     12  FILLER                            PIC X(50).
      ******************************************************************
00341 *                            COPY ERCFORM.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCFORM.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *    FILE DESCRIPTION = POLICY FORM MASTER FILE                  *
00008 *                                                                *
00009 *    FILE TYPE = VSAM,KSDS                                       *
00010 *    RECORD SIZE = 500  RECFORM = FIXED                          *
00011 *                                                                *
00012 *    BASE CLUSTER = ERFORM                      RKP=02,LEN=20    *
00013 *                                                                *
00014 *    LOG = YES                                                   *
00015 *    SEVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00016 ******************************************************************
00017  01  FORM-MASTER.
00018     12  FO-RECORD-ID                 PIC X(02).
00019         88  VALID-FO-ID                  VALUE 'FO'.
00020
00021     12  FO-CONTROL-PRIMARY.
00022         16  FO-COMPANY-CD            PIC X(01).
00023         16  FO-STATE                 PIC X(02).
00024         16  FO-FORM-ID               PIC X(12).
00025         16  FO-FORM-EXP-DT           PIC X(02).
00026
00027     12  FO-POLICY-FORM-DATA.
00028         16  FO-IND-GRP-CD            PIC X(01).
00029         16  FO-MAX-ATT-AGE           PIC S9(03)      COMP-3.
00030         16  FO-LF-MIN-ISSUE-AGE      PIC S9(03)      COMP-3.
00031         16  FO-AH-MIN-ISSUE-AGE      PIC S9(03)      COMP-3.
00032         16  FO-LF-MAX-ISSUE-AGE      PIC S9(03)      COMP-3.
00033         16  FO-AH-MAX-ISSUE-AGE      PIC S9(03)      COMP-3.
00034         16  FO-LF-MAX-TERM           PIC S9(03)      COMP-3.
00035         16  FO-AH-MAX-TERM           PIC S9(03)      COMP-3.
00036         16  FO-MAX-LF-AMT            PIC S9(07)V99   COMP-3.
00037         16  FO-MAX-AH-AMT            PIC S9(05)V99   COMP-3.
00038         16  FO-SUICIDE-EXCL-TYPE     PIC 9(02).
00039         16  FO-LF-PRE-EXIST-EXCL-TYPE    PIC 9(02).
00040         16  FO-AH-PRE-EXIST-EXCL-TYPE    PIC 9(02).
00041         16  FO-DIS-DEF-TYPE          PIC 9(02).
00042         16  FO-DISMEMBERMENT-CD      PIC X(01).
00043         16  FO-APP-CERT-USE-CD       PIC X(01).
00044
00045     12  FILLER                       PIC X(29).
00046
00047     12  FO-FORM-PLAN-TABLE.
00048         16  FO-FORM-PLANS     OCCURS 40 TIMES.
00049             20  FO-PLAN-TYPE         PIC X(01).
00050             20  FO-PLAN-ID           PIC X(02).
00051             20  FO-PLAN-TERM         PIC 9(03).
00052             20  FO-PLAN-REFUND-METHOD
00053                                      PIC X.
00054
00055     12  FILLER                       PIC X(30).
00056
00057     12  FO-COMMENTS-AREA.
00058         16  FO-COMMENT-LINE-1        PIC X(78).
00059
00060     12  FILLER                       PIC X(20).
00061
00062     12  FO-MAINT-INFORMATION.
00063         16  FO-LAST-MAINT-DT         PIC X(02).
00064         16  FO-LAST-MAINT-HHMMSS     PIC S9(07)      COMP-3.
00065         16  FO-LAST-MAINT-BY         PIC X(04).
      *                            COPY ELCMSTR.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCMSTR.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.012                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CLAIM MASTER FILE                         *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 350  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELMSTR                         RKP=2,LEN=20   *
00013 *       ALTERNATE PATH1 = ELMSTR2 (BY NAME)       RKP=22,LEN=29  *
00014 *       ALTERNATE PATH2 = ELMSTR3 (BY SOC SEC NO) RKP=51,LEN=12  *
00015 *       ALTERNATE PATH3 = ELMSTR5 (BY CERT NO)    RKP=63,LEN=12  *
00016 *       ALTERNATE PATH4 = ELMSTR6 (BY CREDIT CARD NO)            *
00017 *                                                 RKP=75,LEN=21  *
00018 *                                                                *
00019 *   **** NOTE ****                                               *
00020 *             ANY CHANGES TO THIS COPYBOOK MUST ALSO BE          *
00021 *             IMPLEMENTED IN COPYBOOK ELCRETR (RETRIEVE MASTER)  *
00022 *                                                                *
00023 *   LOG = YES                                                    *
00024 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
120503******************************************************************
120503*                   C H A N G E   L O G
120503*
120503* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
120503*-----------------------------------------------------------------
120503*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
120503* EFFECTIVE    NUMBER
120503*-----------------------------------------------------------------
120503* 120503    2003080800002  SMVA  INITIAL SECURE PAY CHANGES
080307* 080307    2007032100001  PEMA  ADD TOTAL INTEREST PAID FIELD
031213* 031213    2012113000002  PEMA  ADD ACCIDENT INDICATOR
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
081817* 081817    2016100700001  TANA  ADD NBR OF EXTENSIONS
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
022122* 022122  CR2021100800003  PEMA  Add B and H claim types
00025 ******************************************************************
00026  01  CLAIM-MASTER.
00027      12  CL-RECORD-ID                PIC XX.
00028          88  VALID-CL-ID         VALUE 'CL'.
00029
00030      12  CL-CONTROL-PRIMARY.
00031          16  CL-COMPANY-CD           PIC X.
00032          16  CL-CARRIER              PIC X.
00033          16  CL-CLAIM-NO             PIC X(7).
00034          16  CL-CERT-NO.
00035              20  CL-CERT-PRIME       PIC X(10).
00036              20  CL-CERT-SFX         PIC X.
00037
00038      12  CL-CONTROL-BY-NAME.
00039          16  CL-COMPANY-CD-A1        PIC X.
00040          16  CL-INSURED-LAST-NAME    PIC X(15).
00041          16  CL-INSURED-NAME.
00042              20  CL-INSURED-1ST-NAME PIC X(12).
00043              20  CL-INSURED-MID-INIT PIC X.
00044
00045      12  CL-CONTROL-BY-SSN.
00046          16  CL-COMPANY-CD-A2        PIC X.
00047          16  CL-SOC-SEC-NO.
00048              20  CL-SSN-STATE        PIC XX.
00049              20  CL-SSN-ACCOUNT      PIC X(6).
00050              20  CL-SSN-LN3          PIC X(3).
00051
00052      12  CL-CONTROL-BY-CERT-NO.
00053          16  CL-COMPANY-CD-A4        PIC X.
00054          16  CL-CERT-NO-A4.
00055              20  CL-CERT-A4-PRIME    PIC X(10).
00056              20  CL-CERT-A4-SFX      PIC X.
00057
00058      12  CL-CONTROL-BY-CCN.
00059          16  CL-COMPANY-CD-A5        PIC X.
00060          16  CL-CCN-A5.
00061              20  CL-CCN.
00062                  24  CL-CCN-PREFIX-A5 PIC X(4).
00063                  24  CL-CCN-PRIME-A5 PIC X(12).
00064              20  CL-CCN-FILLER-A5    PIC X(4).
00065
00066      12  CL-INSURED-PROFILE-DATA.
00067          16  CL-INSURED-BIRTH-DT     PIC XX.
00068          16  CL-INSURED-SEX-CD       PIC X.
00069              88  INSURED-IS-MALE        VALUE 'M'.
00070              88  INSURED-IS-FEMALE      VALUE 'F'.
00071              88  INSURED-SEX-UNKNOWN    VALUE ' '.
00072          16  CL-INSURED-OCC-CD       PIC X(6).
00073          16  FILLER                  PIC X(5).
00074
00075      12  CL-PROCESSING-INFO.
00076          16  CL-PROCESSOR-ID         PIC X(4).
00077          16  CL-CLAIM-STATUS         PIC X.
00078              88  CLAIM-IS-OPEN          VALUE 'O'.
00079              88  CLAIM-IS-CLOSED        VALUE 'C'.
00080          16  CL-CLAIM-TYPE           PIC X.
00081 *            88  AH-CLAIM               VALUE 'A'.
00082 *            88  LIFE-CLAIM             VALUE 'L'.
00083 *            88  PROPERTY-CLAIM         VALUE 'P'.
00084 *            88  IUI-CLAIM              VALUE 'I'.
120503*            88  GAP-CLAIM              VALUE 'G'.
052614*            88  FAMILY-LEAVE-CLAIM     VALUE 'F'.
100518*            88  OTHER-CLAIM            VALUE 'O'.
022122*            88  hospital-claim         value 'H'.
022122*            88  bereavement-claim      value 'B'.
00085          16  CL-CLAIM-PREM-TYPE      PIC X.
00086              88  SINGLE-PREMIUM         VALUE '1'.
00087              88  O-B-COVERAGE           VALUE '2'.
00088              88  OPEN-END-COVERAGE      VALUE '3'.
00089          16  CL-INCURRED-DT          PIC XX.
00090          16  CL-REPORTED-DT          PIC XX.
00091          16  CL-FILE-ESTABLISH-DT    PIC XX.
00092          16  CL-EST-END-OF-DISAB-DT  PIC XX.
00093          16  CL-LAST-PMT-DT          PIC XX.
00094          16  CL-LAST-PMT-AMT         PIC S9(7)V99  COMP-3.
00095          16  CL-PAID-THRU-DT         PIC XX.
00096          16  CL-TOTAL-PAID-AMT       PIC S9(7)V99  COMP-3.
00097          16  CL-NO-OF-PMTS-MADE      PIC S9(3)     COMP-3.
00098          16  CL-NO-OF-DAYS-PAID      PIC S9(4)     COMP.
00099          16  CL-PMT-CALC-METHOD      PIC X.
00100              88  CL-360-DAY-YR          VALUE '1'.
00101              88  CL-365-DAY-YR          VALUE '2'.
00102              88  CL-FULL-MONTHS         VALUE '3'.
00103          16  CL-CAUSE-CD             PIC X(6).
00104
00105          16  CL-PRIME-CERT-NO.
00106              20  CL-PRIME-CERT-PRIME PIC X(10).
00107              20  CL-PRIME-CERT-SFX   PIC X.
00108
00109          16  CL-SYSTEM-IDENTIFIER    PIC XX.
00110              88  CL-CREDIT-CLAIM        VALUE 'CR'.
00111              88  CL-CONVENIENCE-CLAIM   VALUE 'CV'.
00112
00113          16  CL-MICROFILM-NO         PIC X(10).
051414         16  FILLER REDEFINES CL-MICROFILM-NO.
051414             20  CL-BENEFIT-PERIOD   PIC 99.
051414             20  FILLER              PIC X(8).
00114          16  CL-PROG-FORM-TYPE       PIC X.
00115          16  CL-LAST-ADD-ON-DT       PIC XX.
00116
00117          16  CL-LAST-REOPEN-DT       PIC XX.
00118          16  CL-LAST-CLOSE-DT        PIC XX.
00119          16  CL-LAST-CLOSE-REASON    PIC X(01).
00120              88  FINAL-PAID             VALUE '1'.
00121              88  CLAIM-DENIED           VALUE '2'.
00122              88  AUTO-CLOSE             VALUE '3'.
00123              88  MANUAL-CLOSE           VALUE '4'.
00124              88  BENEFITS-CHANGED       VALUE 'C'.
00125              88  SETUP-ERRORS           VALUE 'E'.
00126          16  CL-ASSOC-CERT-SEQU      PIC S99.
00127          16  CL-ASSOC-CERT-TOTAL     PIC S99.
00128          16  CL-CLAIM-PAYMENT-STATUS PIC 9.
00129              88  PAYMENT-IN-PREP        VALUE 1 THRU 9.
080307         16  CL-TOTAL-INT-PAID       PIC S9(5)V99 COMP-3.
080307         16  FILLER                  PIC X.
00131
00132      12  CL-CERTIFICATE-DATA.
00133          16  CL-CERT-ORIGIN          PIC X.
00134              88  CERT-WAS-ONLINE        VALUE '1'.
00135              88  CERT-WAS-CREATED       VALUE '2'.
00136              88  COVERAGE-WAS-ADDED     VALUE '3'.
00137          16  CL-CERT-KEY-DATA.
00138              20  CL-CERT-CARRIER     PIC X.
00139              20  CL-CERT-GROUPING    PIC X(6).
00140              20  CL-CERT-STATE       PIC XX.
00141              20  CL-CERT-ACCOUNT.
00142                  24  CL-CERT-ACCOUNT-PREFIX PIC X(4).
00143                  24  CL-CERT-ACCOUNT-PRIME  PIC X(6).
00144              20  CL-CERT-EFF-DT      PIC XX.
00145
00146      12  CL-STATUS-CONTROLS.
00147          16  CL-PRIORITY-CD          PIC X.
00148              88  CONFIDENTIAL-DATA      VALUE '8'.
00149              88  HIGHEST-PRIORITY       VALUE '9'.
00150          16  CL-SUPV-ATTN-CD         PIC X.
00151              88  SUPV-NOT-REQUIRED      VALUE ' ' 'N'.
00152              88  SUPV-IS-REQUIRED       VALUE 'Y'.
00153          16  CL-PURGED-DT            PIC XX.
00154          16  CL-RESTORED-DT          PIC XX.
00155          16  CL-NEXT-AUTO-PAY-DT     PIC XX.
00156          16  CL-NEXT-RESEND-DT       PIC XX.
00157          16  CL-NEXT-FOLLOWUP-DT     PIC XX.
031213         16  CL-CRITICAL-PERIOD      PIC 99.
031213*        16  FILLER                  PIC XX.
00159          16  CL-LAST-MAINT-DT        PIC XX.
00160          16  CL-LAST-MAINT-USER      PIC X(4).
00161          16  CL-LAST-MAINT-HHMMSS    PIC S9(6)     COMP-3.
00162          16  CL-LAST-MAINT-TYPE      PIC X.
00163              88  CLAIM-SET-UP           VALUE ' '.
00164              88  PAYMENT-MADE           VALUE '1'.
00165              88  LETTER-SENT            VALUE '2'.
00166              88  MASTER-WAS-ALTERED     VALUE '3'.
00167              88  MASTER-WAS-RESTORED    VALUE '4'.
00168              88  INCURRED-DATE-CHANGED  VALUE '5'.
00169              88  FILE-CONVERTED         VALUE '6'.
00170              88  CHANGE-OF-BENEFITS     VALUE 'C'.
00171              88  ERROR-CORRECTION       VALUE 'E'.
00172          16  CL-RELATED-CLAIM-NO     PIC X(7).
00173          16  CL-HISTORY-ARCHIVE-DT   PIC XX.
00174          16  CL-BENEFICIARY          PIC X(10).
00175          16  CL-FILE-ESTABLISHED-BY  PIC X(4).
120808         16  CL-DENIAL-TYPE          PIC X.
                   88  CL-TYPE-DENIAL          VALUE '1'.
                   88  CL-TYPE-RESCISSION      VALUE '2'.
                   88  CL-TYPE-REFORMATION     VALUE '3'.
                   88  CL-TYPE-REF-TO-RES      VALUE '4'.
                   88  CL-TYPE-RECONSIDERED    VALUE '5'.
081817         16  CL-NO-OF-EXTENSIONS     PIC 99.
081817         16  filler                  pic x(3).
      *        16  CL-CRIT-PER-RECURRENT   PIC X.
      *        16  CL-CRIT-PER-RTW-MOS     PIC 99.
      *        16  CL-RTW-DT               PIC XX.
00177
00178      12  CL-TRAILER-CONTROLS.
00179          16  CL-TRAILER-SEQ-CNT      PIC S9(4)     COMP.
00180              88  CL-1ST-TRL-AVAIL       VALUE +4095.
00181              88  CL-LAST-TRL-AVAIL      VALUE +100.
00182              88  CL-RESV-EXP-HIST-TRLR  VALUE +0.
00183          16  CL-LAST-INC-DT-CHANGE   PIC S9(4)     COMP.
00184          16  FILLER                  PIC XX.
00185          16  CL-AUTO-PAY-SEQ         PIC S9(4)     COMP.
00186          16  CL-ADDRESS-TRAILER-CNT.
00187              20  CL-INSURED-ADDR-CNT  PIC S9(1).
00188                  88  NO-INSURED-AVAILABLE    VALUE ZERO.
00189              20  CL-ACCOUNT-ADDR-CNT  PIC S9(1).
00190                  88  ACCOUNT-IS-ONLINE       VALUE ZERO.
00191              20  CL-BENIF-ADDR-CNT    PIC S9(1).
00192                  88  BENEFICIARY-IS-ONLINE   VALUE ZERO.
00193              20  CL-EMPLOYER-ADDR-CNT PIC S9(1).
00194                  88  NO-EMPLOY-AVAILABLE     VALUE ZERO.
00195              20  CL-DOCTOR-ADDR-CNT   PIC S9(1).
00196                  88  NO-DOCTOR-AVAILABLE     VALUE ZERO.
00197              20  CL-OTHER-1-ADDR-CNT  PIC S9(1).
00198                  88  NO-OTHER-1-ADDRESSES    VALUE ZERO.
00199              20  CL-OTHER-2-ADDR-CNT  PIC S9(1).
00200                  88  NO-OTHER-2-ADDRESSES    VALUE ZERO.
00201
00202      12  CL-CV-REFERENCE-NO.
00203          16  CL-CV-REFNO-PRIME       PIC X(18).
00204          16  CL-CV-REFNO-SFX         PIC XX.
00205
00206      12  CL-FILE-LOCATION            PIC X(4).
00207
00208      12  CL-PROCESS-ERRORS.
00209          16  CL-FATAL-ERROR-CNT      PIC S9(4)     COMP.
00210              88  NO-FATAL-ERRORS        VALUE ZERO.
00211          16  CL-FORCEABLE-ERROR-CNT  PIC S9(4)     COMP.
00212              88  NO-FORCABLE-ERRORS     VALUE ZERO.
00213
00214      12  CL-PRODUCT-CD               PIC X.
00215
00216      12  CL-CURRENT-KEY-DATA.
00217          16  CL-CURRENT-CARRIER      PIC X.
00218          16  CL-CURRENT-GROUPING     PIC X(6).
00219          16  CL-CURRENT-STATE        PIC XX.
00220          16  CL-CURRENT-ACCOUNT      PIC X(10).
00221
00222      12  CL-ASSOCIATES               PIC X.
00223          88  CL-ASSOC-NO-INTERFACE      VALUE 'A'.
00224          88  CL-ASSOC-INTERFACE         VALUE 'I'.
00225          88  CL-NON-ASSOC-NO-INTERFACE  VALUE 'N'.
00226          88  CL-NON-ASSOC-INTERFACE     VALUE 'M'.
00227
00228      12  CL-ACTIVITY-CODE            PIC 99.
00229      12  CL-ACTIVITY-MAINT-DT        PIC XX.
00230      12  CL-ACTIVITY-MAINT-TYPE      PIC X(4).
00231
00232      12  CL-LAPSE-REPORT-CODE        PIC 9.
00233      12  CL-LAG-REPORT-CODE          PIC 9.
00234      12  CL-LOAN-TYPE                PIC XX.
00235      12  CL-LEGAL-STATE              PIC XX.
00236
CIDMOD     12  CL-YESNOSW                  PIC X.
031213     12  CL-ACCIDENT-CLAIM-SW        PIC X.
031213         88  CL-ACCIDENT-NOT-SET           VALUE ' '.
031213         88  CL-CLAIM-DUE-TO-ACCIDENT      VALUE 'Y'.
031213         88  CL-CLAIM-NOT-DUE-TO-ACCIDENT  VALUE 'N'.
051414     12  cl-insured-type             pic x.
051414         88  cl-claim-on-primary         value 'P'.
051414         88  cl-claim-on-co-borrower     value 'C'.
031213     12  cl-benefit-expiration-dt    PIC XX.
      *                            COPY ERCPDEF.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            ERCPDEF.                            *
      *                                                                *
      *    FILE DESCRIPTION = PRODUCT DEFINITION MASTER                *
      *                                                                *
      *    FILE TYPE = VSAM,KSDS                                       *
      *    RECORD SIZE = 1319 RECFORM = FIXED                          *
      *                                                                *
      *    BASE CLUSTER = ERPDEF                      RKP=02,LEN=18    *
      *                                                                *
      *    LOG = YES                                                   *
      *    SEVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      ******************************************************************
051414*                   C H A N G E   L O G
051414*
051414* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
051414*-----------------------------------------------------------------
051414*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
051414* EFFECTIVE    NUMBER
051414*-----------------------------------------------------------------
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
100314* 100314  CR2014061900001  PEMA  ADD PCT OF BENEFIT
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
022122* 022122  CR2021100800003  TANA  Add B and H claim types
      ******************************************************************
       01  PRODUCT-MASTER.
          12  PD-RECORD-ID                 PIC X(02).
              88  VALID-PD-ID                  VALUE 'PD'.
          12  PD-CONTROL-PRIMARY.
              16  PD-COMPANY-CD            PIC X.
              16  PD-STATE                 PIC XX.
              16  PD-PRODUCT-CD            PIC XXX.
              16  PD-FILLER                PIC X(7).
              16  PD-BEN-TYPE              PIC X.
              16  PD-BEN-CODE              PIC XX.
              16  PD-PROD-EXP-DT           PIC XX.
          12  FILLER                       PIC X(6).
          12  PD-PRODUCT-DATA OCCURS 11.
              16  PD-PROD-CODE             PIC X.
                  88  PD-PROD-LIFE           VALUE 'L'.
                  88  PD-PROD-PROP           VALUE 'P'.
                  88  PD-PROD-AH             VALUE 'A'.
                  88  PD-PROD-IU             VALUE 'I'.
                  88  PD-PROD-GAP            VALUE 'G'.
052614            88  PD-PROD-FAML           VALUE 'F'.
100518            88  PD-PROD-OTH            VALUE 'O'.
022122            88  PD-PROD-BRV            VALUE 'B'.
022122            88  PD-PROD-HOSP           VALUE 'H'.
              16  PD-MAX-ATT-AGE           PIC S999        COMP-3.
022122        16  PD-WAIT-PERIOD.
                  20  pd-wait-days         pic 99.
022122            20  PD-RET-ELIM          PIC X.
022122        16  FILLER                   PIC X.
021222*       16  PD-MIN-ISSUE-AGE         PIC S999        COMP-3.
021222*       16  PD-MAX-ISSUE-AGE         PIC S999        COMP-3.
              16  PD-MAX-TERM              PIC S999        COMP-3.
              16  PD-MAX-AMT               PIC S9(07)      COMP-3.
              16  FILLER                   PIC X.
              16  PD-PRE-EXIST-EXCL-TYPE   PIC 99.
              16  PD-EXCLUSION-PERIOD-DAYS PIC S999        COMP-3.
              16  PD-COVERAGE-ENDS-MOS     PIC S999        COMP-3.
              16  PD-ACCIDENT-ONLY-MOS     PIC S999        COMP-3.
              16  PD-CRIT-PERIOD           PIC S999        COMP-3.
              16  PD-REC-CRIT-PERIOD       PIC 99.
              16  PD-REC-CP-ALPHA  REDEFINES PD-REC-CRIT-PERIOD.
                  20  PD-RECURRING-YN      PIC X.
                  20  FILLER               PIC X.
              16  PD-RTW-MOS               PIC 99.
051414        16  PD-MAX-EXTENSION         PIC 99.
100314        16  pd-ben-pct               pic sv999 comp-3.
100314*       16  FILLER                   PIC XX.
          12  PD-1ST-YR-ADMIN-ALLOW        PIC S9(3)V99    COMP-3.
          12  PD-TERM-LIMITS OCCURS 15.
              16  PD-LOW-TERM              PIC S999        COMP-3.
              16  PD-HI-TERM               PIC S999        COMP-3.
      *  THE LOAN AMT LIMITS CORRESPOND TO THE TERM LIMITS ABOVE
          12  PD-LOAN-AMT-LIMITS OCCURS 15.
              16  PD-LOW-AMT               PIC S9(5)       COMP-3.
              16  PD-HI-AMT                PIC S9(7)       COMP-3.
          12  PD-EARN-FACTORS.
              16  FILLER OCCURS 15.
                  20  FILLER OCCURS 15.
                      24  PD-UEP-FACTOR    PIC S9V9(3)     COMP-3.
          12  PD-PRODUCT-DESC              PIC X(80).
          12  PD-TRUNCATED                 PIC X.
          12  FILLER                       PIC X(7).
          12  PD-MAINT-INFORMATION.
              16  PD-LAST-MAINT-DT         PIC X(02).
              16  PD-LAST-MAINT-HHMMSS     PIC S9(07)      COMP-3.
              16  PD-LAST-MAINT-BY         PIC X(04).
121712*                            COPY ELCCRTT.
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
012918* 012918  CR2017062000002  PEMA  AUDIT NB FOR PREV CLAIMS
091318* 091318  CR2018073000001  PEMA  ADD Refund methods
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
080322* 080322  CR2021100800003  TANA  Add B and H claim types
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
100518                 88  CS-OT-CLM               VALUE 'O'.
080322                 88  CS-BR-CLM               VALUE 'B'.
080322                 88  CS-HS-CLM               VALUE 'H'.
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
012918         16  cs-claim-verification-status  pic x.
012918             88  cs-clm-ver-eligible         value 'A'.
012918             88  cs-clm-ver-partial-elig     value 'B'.
012918             88  cs-clm-ver-not-eligible     value 'C'.
012918             88  cs-clm-ver-not-elig-opn-clm value 'D'.
012918             88  cs-clm-ver-not-part-elig-rw value 'E'.
012918             88  cs-clm-ver-ND-CERT          value 'F'.
012918             88  cs-clm-ver-spec-other       value 'G'.
012918             88  cs-clam-ver-pratial-corrected
012918                                             value 'H'.
012918             88  cs-clm-ver-no-matches       value 'I'.
012918             88  cs-clm-ver-not-elig-corrected
012918                                             value 'J'.
012918             88  cs-clm-ver-needs-review     value 'R'.
012918             88  cs-clm-ver-sent-to-claims   value 'W'.
091318         16  CS-LF-REFUND-METHOD           PIC X.
091318         16  CS-AH-REFUND-METHOD           PIC X.
020816         16  FILLER                        PIC X(353). *> was 420
121712*        16  FILLER                        PIC X(496).
011514 01  var  pic x(30).
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CONTROL-FILE
                                ACCOUNT-MASTER CERTIFICATE-MASTER
                                MAILING-DATA ORIGINAL-CERTIFICATE
                                FORM-MASTER CLAIM-MASTER
                                PRODUCT-MASTER CERTIFICATE-TRAILERS
                                VAR.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL1278' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00344 *    SERVICE RELOAD PARMLIST.
00345
00346      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00347      MOVE '5'                    TO  DC-OPTION-CODE.
00348
00349      PERFORM 9600-DATE-LINK.
00350
00351      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00352      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00353      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00354      MOVE 2                      TO  EMI-NUMBER-OF-LINES.
           MOVE PI-LIFE-OVERRIDE-L6    TO EMI-LIFE-OVERRIDE-L6
           MOVE PI-AH-OVERRIDE-L6      TO EMI-AH-OVERRIDE-L6
00355      MOVE EIBTRMID               TO  QID-TERM.
00356
00357      IF EIBCALEN  IS EQUAL TO  ZERO
00358          GO TO 8700-UNAUTHORIZED-ACCESS.
00359
00360      IF PI-CALLING-PROGRAM  IS NOT EQUAL TO  THIS-PGM
00361          IF PI-RETURN-TO-PROGRAM  IS NOT EQUAL TO  THIS-PGM
00362              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6
00363              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5
00364              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4
00365              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3
00366              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2
00367              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1
00368              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM
00369              MOVE THIS-PGM              TO  PI-CALLING-PROGRAM
00370          ELSE
00371              MOVE PI-CALLING-PROGRAM    TO  RETURN-FROM
00372              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM
00373              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM
00374              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1
00375              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2
00376              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3
00377              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4
00378              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5
00379              MOVE SPACES                TO  PI-SAVED-PROGRAM-6.
00380
00381      IF EIBAID  IS EQUAL TO  DFHCLEAR
00382          GO TO 9300-CLEAR.
00383
00384      IF PI-PROCESSOR-ID  IS EQUAL TO  'LGXX'
00385          NEXT SENTENCE
00386      ELSE
00387          
      * EXEC CICS READQ TS
00388 *            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00389 *            INTO    (SECURITY-CONTROL)
00390 *            LENGTH  (SC-COMM-LENGTH)
00391 *            ITEM    (SC-ITEM)
00392 *        END-EXEC
      *    MOVE '*$II   L              ''   #00006082' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00393          MOVE SC-CREDIT-DISPLAY (33)
00394                                  TO  PI-DISPLAY-CAP
00395          MOVE SC-CREDIT-UPDATE  (33)
00396                                  TO  PI-MODIFY-CAP
00397          MOVE SC-CREDIT-UPDATE  (32)
00398                                  TO  PI-MODIFY-CERT-CAP
00399          IF NOT  DISPLAY-CAP
00400              MOVE 'READ'         TO  SM-READ
00401              PERFORM 9800-SECURITY-VIOLATION  THRU  9899-EXIT
00402              MOVE ER-0070        TO  EMI-ERROR
00403              PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
00404              GO TO 8100-SEND-INITIAL-MAP.
00405
00406      IF EIBTRNID  IS NOT EQUAL TO  TRANS-ID
00407          MOVE 'Y' TO WS-FIRST-ENTRY-SW
00408          MOVE LOW-VALUES         TO  EL127HI
00409          MOVE SAVE-BIN-DATE      TO  WS-LF-CANCEL-DATE
00410                                      PI-LF-CANCEL-DATE
00411                                      WS-AH-CANCEL-DATE
00412                                      PI-AH-CANCEL-DATE
00413          MOVE SAVE-DATE          TO  WS-LF-CANCEL-DATE-ED
00414                                      PI-LF-CANCEL-DATE-ED
00415                                      WS-AH-CANCEL-DATE-ED
00416                                      PI-AH-CANCEL-DATE-ED
00417          MOVE SPACES             TO  PI-EARNING-METHOD-LF
00418                                      PI-EARNING-METHOD-AH
00419          PERFORM 0500-FORMAT-SCREEN
00420          GO TO 8100-SEND-INITIAL-MAP.
00421
00422      IF NOT  MODIFY-CAP
00423          MOVE 'UPDATE'           TO  SM-READ
00424          PERFORM 9800-SECURITY-VIOLATION  THRU  9899-EXIT
00425          MOVE ER-0070            TO  EMI-ERROR
00426          PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
00427          GO TO 8100-SEND-INITIAL-MAP.
00428
00429      
      * EXEC CICS HANDLE CONDITION
00430 *        PGMIDERR  (9500-PGMID-ERROR)
00431 *        ERROR     (9900-ABEND)
00432 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00006124' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303036313234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
070622
070622     set P to address of KIXSYS
070622     CALL "getenv" using by value P returning var-ptr
070622     if var-ptr = null then
070622        display ' kixsys not set '
070622     else
070622        set address of var to var-ptr
070622        move 0 to env-var-len
070622        inspect var tallying env-var-len
070622          for characters before X'00'
070622        unstring var (1:env-var-len) delimited by '/'
070622        into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
070622              WS-KIX-SYS
070622        end-unstring
070622     end-if
070622
070622     set P to address of KIXHOST
070622     CALL "getenv" using by value P returning var-ptr
070622     if var-ptr = null then
070622        display ' kixhost not set '
070622     else
070622        set address of var to var-ptr
070622        move 0 to env-var-len
070622        inspect var tallying env-var-len
070622          for characters before X'00'
070622        MOVE var(1:env-var-len)  to ws-kixhost
070622        DISPLAY ' WS KIX HOST ' WS-KIXHOST
070622     end-if.
00433  EJECT
00434  0200-RECEIVE.
00435      IF EIBAID  IS EQUAL TO  DFHPA1  OR  DFHPA2  OR  DFHPA3
00436          MOVE ER-0008            TO  EMI-ERROR
00437          PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
00438          MOVE -1                 TO  HLCANCL
00439          GO TO 8200-SEND-DATAONLY.
00440
00441      
      * EXEC CICS RECEIVE
00442 *        MAP     (MAP-NAME)
00443 *        MAPSET  (MAPSET-NAME)
00444 *        INTO    (EL127HI)
00445 *    END-EXEC.
           MOVE LENGTH OF
            EL127HI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00006164' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036313634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL127HI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00446
00447      IF HPFKEYL  IS EQUAL TO  ZERO
00448          GO TO 0300-CHECK-PFKEYS.
00449
00450      IF EIBAID  IS NOT EQUAL TO  DFHENTER
00451          MOVE ER-0004            TO  EMI-ERROR
00452          GO TO 0320-INPUT-ERROR.
00453
00454      IF HPFKEYI NUMERIC
00455          IF HPFKEYI  IS EQUAL TO  '12'  OR  '23'  OR  '24' OR '11'
00456              MOVE PF-VALUES (HPFKEYI)
00457                                  TO  EIBAID
00458          ELSE
00459              MOVE ER-0029        TO  EMI-ERROR
00460              GO TO 0320-INPUT-ERROR.
00461
00462  0300-CHECK-PFKEYS.
00463      IF EIBAID  IS EQUAL TO  DFHPF23
00464          GO TO 8800-PF23.
00465
00466      IF EIBAID  IS EQUAL TO  DFHPF24
00467          GO TO 9100-RETURN-MAIN-MENU.
072616     if eibaid not = dfhpf10
072616        move 'N'                 to pi-pf10-ok
072616     end-if
00469      IF EIBAID = DFHPF11
00471         GO TO 1700-WRITE-CERT-NOTE
           END-IF
00472
00473      IF EIBAID  IS EQUAL TO  DFHPF12
00474          GO TO 9400-PF12.
00475
00476      IF EIBAID = DFHENTER OR DFHPF10
00477         GO TO 0400-EDIT-INPUT-DATA
           END-IF
00478
00479      MOVE ER-0029                TO  EMI-ERROR.
00480
00481  0320-INPUT-ERROR.
00482      PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
00483
00484      MOVE AL-UNBON               TO  HPFKEYA.
00485
00486      IF HPFKEYL  IS EQUAL TO  ZERO
00487          MOVE -1                 TO  HLCANCL
00488      ELSE
00489          MOVE -1                 TO  HPFKEYL.
00490
00491      GO TO 8200-SEND-DATAONLY.
00492  EJECT
00493  0400-EDIT-INPUT-DATA.
00494      MOVE 'N'                    TO  WS-NEW-AH-CANCEL-SW
00495                                      WS-NEW-LF-CANCEL-SW.
00496
081606     IF (HPCYNL > 0)
081606        AND (HPCYNI = 'Y' OR 'N')
081606        PERFORM 1800-READ-ELCERT-UPDATE
081606                                 THRU 1800-EXIT
              IF WS-RESP-NORMAL
081606           MOVE HPCYNI           TO CM-POST-CARD-IND
081606           PERFORM 1810-REWRITE-ELCERT
081606                                 THRU 1810-EXIT
              END-IF
081606     END-IF
00497      IF HLCAL1L GREATER ZERO
00498          MOVE HLCAL1I              TO  PI-EARNING-METHOD-LF.
           IF CLPYNI = 'Y'
              MOVE 'Y'                   TO PI-CLP-YN
              MOVE AL-UANON              TO CLPYNA
           ELSE
              MOVE ' '                   TO PI-CLP-YN
              MOVE AL-UANON              TO CLPYNA
           END-IF
           IF CANREAI = 'R'
              MOVE 'R'                 TO PI-CANCEL-REASON
              MOVE AL-UANON            TO CANREAA
           ELSE
070622        IF CANREAI > SPACES
070622           MOVE CANREAI             TO PI-CANCEL-REASON
                 MOVE AL-UANON            TO CANREAA
              END-IF
           END-IF
070622     PERFORM 0450-CHECK-CANCEL-REASON THRU 0450-EXIT.
00499
00500      IF HLCANCL  IS EQUAL TO  ZERO
00501        AND HACANCL  IS EQUAL TO  ZERO
00502          MOVE PI-LF-CANCEL-DATE    TO  WS-LF-CANCEL-DATE
00503          MOVE PI-AH-CANCEL-DATE    TO  WS-AH-CANCEL-DATE
00504          MOVE PI-LF-CANCEL-DATE-ED TO  WS-LF-CANCEL-DATE-ED
00505          MOVE PI-AH-CANCEL-DATE-ED TO  WS-AH-CANCEL-DATE-ED
00506          GO TO 0420-CHECK-ERRORS.
00507
00508      IF HLCANCL  IS EQUAL TO  ZERO
00509          MOVE PI-LF-CANCEL-DATE    TO  WS-LF-CANCEL-DATE
00510          MOVE PI-LF-CANCEL-DATE-ED TO  WS-LF-CANCEL-DATE-ED
00511          GO TO 0410-CHECK-AH-DATE.
00512
00508      IF HLCANCI = SPACES
              MOVE LOW-VALUES            TO PI-LF-CANCEL-DATE
                                            WS-LF-CANCEL-DATE
              MOVE SPACES                TO PI-LF-CANCEL-DATE-ED
                                            WS-LF-CANCEL-DATE-ED
                                            PI-LF-REFUND-METH
              MOVE +0                    TO PI-LF-REFUND-AMT
00511         GO TO 0410-CHECK-AH-DATE
           END-IF
00512
00513      MOVE AL-UNNON               TO  HLCANCA.
00514      MOVE HLCANCI                TO  DEEDIT-FIELD.
00515      MOVE 'Y'                    TO  WS-NEW-LF-CANCEL-SW.
00516
00517      PERFORM 8600-DEEDIT.
00518
00519      MOVE DEEDIT-FIELD           TO  DC-GREG-DATE-1-MDY.
00520      MOVE '4'                    TO  DC-OPTION-CODE.
00521
00522      PERFORM 9600-DATE-LINK.
00523
00524      IF DATE-CONVERSION-ERROR
00525          MOVE -1                 TO  HLCANCL
00526          MOVE AL-UABON           TO  HLCANCA
00527          MOVE ER-2227            TO  EMI-ERROR
00528          PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
00529      ELSE
              IF PI-CERT-EFF-DT > DC-BIN-DATE-1
                 MOVE -1               TO  HACANCL
                 MOVE AL-UABON         TO  HACANCA
                 MOVE ER-2774          TO  EMI-ERROR
                 PERFORM 9700-ERROR-FORMAT
                                       THRU  9799-EXIT
              ELSE
00530            MOVE DC-BIN-DATE-1    TO WS-LF-CANCEL-DATE
00531                                     PI-LF-CANCEL-DATE
00532            MOVE DC-GREG-DATE-1-EDIT
00533                                  TO HLCANCO
00534                                     WS-LF-CANCEL-DATE-ED
00535                                     PI-LF-CANCEL-DATE-ED
              END-IF
           END-IF
           .
00537  0410-CHECK-AH-DATE.
00538      IF HACAL1L GREATER ZERO
00539          MOVE HACAL1I              TO  PI-EARNING-METHOD-AH.
00540
00541      IF HACANCL  IS EQUAL TO  ZERO
00542          MOVE PI-AH-CANCEL-DATE    TO  WS-AH-CANCEL-DATE
00543          MOVE PI-AH-CANCEL-DATE-ED TO  WS-AH-CANCEL-DATE-ED
00544          GO TO 0420-CHECK-ERRORS.
00541      IF HACANCI = SPACES
00542         MOVE LOW-VALUES            TO PI-AH-CANCEL-DATE
                                            WS-AH-CANCEL-DATE
00543         MOVE SPACES                TO PI-AH-CANCEL-DATE-ED
                                            WS-AH-CANCEL-DATE-ED
                                            PI-AH-REFUND-METH
              MOVE +0                    TO PI-AH-REFUND-AMT
00544         GO TO 0420-CHECK-ERRORS
           END-IF
00545
00546      MOVE AL-UNNON               TO  HACANCA.
00547      MOVE HACANCI                TO  DEEDIT-FIELD.
00548      MOVE 'Y'                    TO  WS-NEW-AH-CANCEL-SW.
00549
00550      PERFORM 8600-DEEDIT.
00551
00552      MOVE DEEDIT-FIELD           TO  DC-GREG-DATE-1-MDY.
00553      MOVE '4'                    TO  DC-OPTION-CODE.
00554
00555      PERFORM 9600-DATE-LINK.
00556
00557      IF DATE-CONVERSION-ERROR
00558          MOVE -1                 TO  HACANCL
00559          MOVE AL-UABON           TO  HACANCA
00560          MOVE ER-2227            TO  EMI-ERROR
00561          PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
00562      ELSE
              IF PI-CERT-EFF-DT > DC-BIN-DATE-1
                 MOVE -1               TO  HACANCL
                 MOVE AL-UABON         TO  HACANCA
                 MOVE ER-2775          TO  EMI-ERROR
                 PERFORM 9700-ERROR-FORMAT
                                       THRU  9799-EXIT
              ELSE
00563            MOVE DC-BIN-DATE-1    TO WS-AH-CANCEL-DATE
00564                                     PI-AH-CANCEL-DATE
00565            MOVE DC-GREG-DATE-1-EDIT
00566                                  TO HACANCO
00567                                     WS-AH-CANCEL-DATE-ED
00568                                     PI-AH-CANCEL-DATE-ED
              END-IF
           END-IF
           .
00570  0420-CHECK-ERRORS.
00571      IF EMI-ERROR  IS EQUAL TO  ZEROS
00572          NEXT SENTENCE
00573      ELSE
00574          GO TO 8200-SEND-DATAONLY.
00575
00576      PERFORM 0500-FORMAT-SCREEN.
072616     if eibaid = dfhpf10
072616        if pi-pf10-ok not = 'Y'
072616           move 'Y'              to pi-pf10-ok
072616           move er-3847          to emi-error
072616           move -1               to hlcancl
072616           perform 9700-error-format
072616                                 thru 9799-exit
072616           go to 8200-send-dataonly
072616        end-if
072616     end-if
           IF EIBAID = DFHPF10
              perform 4000-GEN-CANCEL-TRANS
                                       thru 4000-exit
010412        MOVE PROGRAM-INTERFACE-BLOCK TO WS-PASS-631
010412        MOVE LOW-VALUES     TO WS-PASS-PROGRAM-WORK-AREA
010412        MOVE PI-COMPANY-CD  TO PI-PB-COMPANY-CD
010412        MOVE CG-BATCH-NO    TO PI-PB-ENTRY-BATCH
010412        MOVE 1              TO PI-PB-BATCH-SEQ-NO
010412        MOVE 'Y'            TO PI-ALL-ISSUES-SW
010412                               PI-ALL-CANCELS-SW
062712        IF PI-PROCESSOR-IS-CSR
062712            MOVE 'Y'        TO PI-CSR-SESSION-SW
062712        ELSE
062712            MOVE ' '        TO PI-CSR-SESSION-SW
062712        END-IF
010412        MOVE 'N'            TO PI-ISSUES-IN-ERROR-SW
010412                               PI-CANCELS-IN-ERROR-SW
010412                               PI-ONLY-BATCH-HEADERS-SW
010412                               PI-ALL-OUT-OF-BAL-SW
010412                               PI-HOLD-REC-SW
010412                               PI-CHANGE-REC-SW
010412                               PI-CHK-REQ-REC-SW
010412                               PI-ISSUE-WARNING-SW
010412                               PI-CANCEL-WARNING-SW
010412        MOVE '1'            TO PI-BROWSE-TYPE
010412        MOVE DFHENTER       TO  EIBAID
070622        MOVE PI-CANCEL-REASON TO PI-CANCEL-REASON-631
010412
010412        
      * EXEC CICS XCTL
010412*           PROGRAM    ('EL6311')
010412*           COMMAREA   (WS-PASS-631)
010412*           LENGTH     (1300)
010412*       END-EXEC
           MOVE 'EL6311' TO DFHEIV1
           MOVE 1300
             TO DFHEIV11
      *    MOVE '.$C                   %   #00006405' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-PASS-631, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
010412
010412*        PERFORM 0500-FORMAT-SCREEN
           END-IF
00578      GO TO 8100-SEND-INITIAL-MAP.
070622 0450-CHECK-CANCEL-REASON.
070622
070622*      Read Napersoft CancelReasons Lookup Table
070622     MOVE 'naperadmin'           TO USR
070622     MOVE 'cCm8naper'            TO PASS
070622     IF WS-KIXHOST = 'logictest'
070622        MOVE 'HOVTSTDB01_NaperRepo'
070622                                 TO SVR
070622        MOVE '1029'              TO WS-LOOKUPID
070622     ELSE
070622        MOVE 'SDVDB01_NaperRepo' TO SVR
070622        MOVE '1029'              TO WS-LOOKUPID
070622     END-IF
070622
070622     MOVE SPACES                 TO WS-LOOKUP-VALUE
070622
070622     PERFORM 4100-CONNECT-TO-DB  THRU 4100-EXIT
070622     MOVE PI-CANCEL-REASON TO WS-LOOKUPNAME
070622
070622     EXEC SQL
070622           SELECT LOOKUPVALUE
070622             INTO :WS-LOOKUP-VALUE
070622             FROM LOOKUPVALUES
070622               WHERE LOOKUPID = :WS-LOOKUPID
070622                 AND LOOKUPNAME = :WS-LOOKUPNAME
070622     END-EXEC
070622
070622     IF SQLCODE = 0
070622        CONTINUE
070622     ELSE
070622     IF SQLCODE = 100
070622         MOVE -1                    TO CANREAL
070622         MOVE AL-UANON              TO CANREAA
070622         MOVE ER-1590               TO EMI-ERROR
070622         PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
070622     ELSE
070622        DISPLAY "ERROR: INVALID CancelReasons Lookup Table SELECT"
070622        DISPLAY ' SQL RETURN CODE ' SQLCODE
070622        DISPLAY ' SQL ERR MESS    ' SQLERRMC
070622     END-IF.
070622
070622     PERFORM 4300-DISCONNECT THRU 4300-EXIT.
070622
070622 0450-EXIT.
070622     EXIT.
00579  EJECT
00580  0500-FORMAT-SCREEN  SECTION.
00581      MOVE LOW-VALUES             TO  EL127HO.
00582      MOVE PI-CARRIER             TO  HCARIERO.
00583      MOVE PI-GROUPING            TO  HGROUPO.
00584      MOVE PI-STATE               TO  HSTATEO.
00585      MOVE PI-ACCOUNT             TO  HACCTNOO.
00586      MOVE ' '                    TO  DC-OPTION-CODE.
00587      MOVE PI-CERT-EFF-DT         TO  DC-BIN-DATE-1.
00588
00589      PERFORM 9600-DATE-LINK.
00590
00591      MOVE DC-GREG-DATE-1-EDIT    TO  HEFFDTO.
00592      MOVE PI-CERT-PRIME          TO  HCERTNOO.
00593      MOVE PI-CERT-SFX            TO  HCRTSFXO.
           MOVE PI-CLP-YN              TO  CLPYNO
           .
00595  0510-GET-CERT.
00596      MOVE PI-COMPANY-CD          TO  WS-CM-COMPANY-CD.
00597      MOVE PI-CARRIER             TO  WS-CM-CARRIER.
00598      MOVE PI-GROUPING            TO  WS-CM-GROUPING.
00599      MOVE PI-STATE               TO  WS-CM-STATE.
00600      MOVE PI-ACCOUNT             TO  WS-CM-ACCOUNT.
00601      MOVE PI-CERT-EFF-DT         TO  WS-CM-CERT-EFF-DT.
00602      MOVE PI-CERT-PRIME          TO  WS-CM-CERT-PRIME.
00603      MOVE PI-CERT-SFX            TO  WS-CM-CERT-SFX.
00604
00605      PERFORM 0900-READ-CERT-FILE.
00606
00607      IF CERT-NOT-FOUND
00608          MOVE -1                 TO  HLCANCL
00609          MOVE AL-SANON           TO  HLCANCA
00610          MOVE ER-0214            TO  EMI-ERROR
00611          PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
00612          GO TO 8100-SEND-INITIAL-MAP.
00613
           IF CM-CLAIM-ATTACHED-COUNT = +0
              GO TO 0510-SKIP-CLAIM-CHECK
           END-IF
           MOVE CM-COMPANY-CD          TO ELMSTR-COMP-CD
           MOVE CM-CERT-NO             TO ELMSTR-CERT-NO
           
      * EXEC CICS STARTBR
      *       DATASET     ('ELMSTR5')
      *       RIDFLD      (ELMSTR-KEY)
      *       RESP        (WS-RESPONSE)
      *    END-EXEC
           MOVE 'ELMSTR5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00006501' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303036353031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF WS-RESP-NORMAL
           PERFORM WITH TEST AFTER UNTIL I-SAY-TO-STOP
              
      * EXEC CICS READNEXT
      *          DATASET   ('ELMSTR5')
      *          RIDFLD    (ELMSTR-KEY)
      *          SET       (ADDRESS OF CLAIM-MASTER)
      *          RESP      (WS-RESPONSE)
      *       END-EXEC
           MOVE 'ELMSTR5' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )  N#00006508' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303036353038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELMSTR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF (WS-RESP-NORMAL OR WS-RESP-DUPKEY)
                 AND (CM-COMPANY-CD = CL-COMPANY-CD)
                 AND (CM-CERT-NO = CL-CERT-NO)
                 IF (CM-ACCOUNT = CL-CERT-ACCOUNT)
                    AND (CM-CERT-EFF-DT = CL-CERT-EFF-DT)
                    SET FOUND-A-CLAIM  TO TRUE
100518              MOVE CL-CLAIM-TYPE TO WS-CLAIM-TYPE
100518              IF CL-CLAIM-TYPE = 'L' OR 'O'
                       IF CL-INCURRED-DT > WS-LF-INCUR-DT
                          MOVE CL-INCURRED-DT TO WS-LF-INCUR-DT
                       END-IF
                       IF CLAIM-IS-OPEN
                          SET OPEN-LF-CLAIM TO TRUE
                       ELSE
                          IF CL-LAST-CLOSE-DT > WS-LF-LAST-CLOSE-DT
                             MOVE CL-LAST-CLOSE-DT
                                       TO WS-LF-LAST-CLOSE-DT
                          END-IF
                          SET CLOSED-LF-CLAIM TO TRUE
                       END-IF
                    ELSE
                       IF CLAIM-IS-OPEN
                          MOVE CL-PAID-THRU-DT
                                       TO WS-AH-PAID-THRU-DT
                          SET OPEN-AH-CLAIM TO TRUE
                       ELSE
                          IF CL-PAID-THRU-DT > WS-AH-PAID-THRU-DT
                             MOVE CL-PAID-THRU-DT
                                       TO WS-AH-PAID-THRU-DT
                          END-IF
                          IF CL-LAST-CLOSE-DT > WS-AH-LAST-CLOSE-DT
                             MOVE CL-LAST-CLOSE-DT
                                       TO WS-AH-LAST-CLOSE-DT
                          END-IF
                          SET CLOSED-AH-CLAIM TO TRUE
                       END-IF
                    END-IF
                 END-IF
              ELSE
                 SET I-SAY-TO-STOP TO TRUE
              END-IF
           END-PERFORM
           END-IF
           IF FOUND-A-CLAIM
              IF WS-LF-INCUR-DT NOT = LOW-VALUES
                 MOVE WS-LF-INCUR-DT   TO DC-BIN-DATE-1
                 MOVE ' '              TO DC-OPTION-CODE
                 PERFORM 9600-DATE-LINK
                 IF NO-CONVERSION-ERROR
                    MOVE DC-GREG-DATE-1-EDIT TO LCLMDTO
                    MOVE AL-SANOF      TO LCLMDTHA
                 END-IF
              END-IF
100518        IF WS-CLAIM-TYPE = 'O'
100518           MOVE 'CLAIM TYPE O INCURRED' TO LCLMDTHO
100518           MOVE 'OTHER' TO WS-CLAIM-LIT
100518        ELSE
100518           MOVE 'LIFE ' TO WS-CLAIM-LIT
100518        END-IF
              IF WS-AH-PAID-THRU-DT NOT = LOW-VALUES
                 MOVE WS-AH-PAID-THRU-DT TO DC-BIN-DATE-1
                 MOVE ' '              TO DC-OPTION-CODE
                 PERFORM 9600-DATE-LINK
                 IF NO-CONVERSION-ERROR
                    MOVE DC-GREG-DATE-1-EDIT TO ACLMDTO
                    MOVE AL-SANOF      TO ACLMDTHA
                 END-IF
              END-IF
              MOVE PI-COMPANY-ID       TO WS-CF-COMPANY-ID
              MOVE '3'                 TO WS-CF-RECORD-TYPE
              MOVE CM-STATE            TO WS-CF-ACCESS
              MOVE +0                  TO WS-CF-SEQUENCE-NO
              
      * EXEC CICS READ
      *          DATASET  (ELCNTL-ID)
      *          SET      (ADDRESS OF CONTROL-FILE)
      *          RIDFLD   (WS-CF-CONTROL-PRIMARY)
      *          RESP     (WS-RESPONSE)
      *       END-EXEC
      *    MOVE '&"S        E          (  N#00006586' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303036353836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF WS-RESP-NORMAL
                 MOVE CF-ST-REF-AH-DEATH-IND
                                       TO WS-ST-REF-IND
              END-IF
           END-IF
           .
       0510-SKIP-CLAIM-CHECK.
00614      MOVE CM-INSURED-LAST-NAME   TO  HLNAMEO.
00615      MOVE CM-INSURED-FIRST-NAME  TO  HFNAMEO.
00616      MOVE CM-INSURED-INITIAL2    TO  HINITO.
081606     MOVE WS-CM-CONTROL-PRIMARY  TO WS-MA-CONTROL-PRIMARY
081606     
      * EXEC CICS READ
081606*        DATASET  (ERMAIL-ID)
081606*        RIDFLD   (WS-MA-CONTROL-PRIMARY)
081606*        SET      (ADDRESS OF MAILING-DATA)
081606*        RESP     (WS-RESPONSE)
081606*    END-EXEC.
      *    MOVE '&"S        E          (  N#00006603' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303036363033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERMAIL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-MA-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF MAILING-DATA TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
081606     IF WS-RESP-NORMAL
081606        PERFORM VARYING S1 FROM +1 BY +1 UNTIL
081606           (S1 > +7)
081606           OR (MA-MAIL-STATUS (S1) = '1')
081606        END-PERFORM
081606        IF S1 > +7
081606           MOVE AL-SANOF         TO HPCYNA
081606        END-IF
081606     END-IF
           IF CM-POST-CARD-IND = 'Y'
              MOVE 'Y'                 TO HPCYNO
           ELSE
              MOVE 'N'                 TO HPCYNO
           END-IF
081606     IF ('8' NOT = CM-LF-CURRENT-STATUS AND CM-AH-CURRENT-STATUS)
081606        OR (NOT CERT-AS-LOADED)
081606        MOVE AL-SANOF            TO HPCYNA
081606     END-IF
081606*    IF (CERT-ADDED-BATCH)
081606*       CONTINUE
      *    ELSE
      *       MOVE AL-SANOF            TO HPCYNA
      *    END-IF
00617      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00618      MOVE '5'                    TO  DC-OPTION-CODE.
00619
00620      PERFORM 9600-DATE-LINK.
00621
00622      MOVE DC-BIN-DATE-1          TO  DC-BIN-DATE-2.
00623      MOVE CM-CERT-EFF-DT         TO  DC-BIN-DATE-1.
00624      MOVE '1'                    TO  DC-OPTION-CODE.
00625
00626      PERFORM 9600-DATE-LINK.
00627
00628      MOVE DC-ELAPSED-MONTHS      TO  WS-ELAPSED-MONTHS.
00629
00630      MOVE ZEROS TO WS-TOT-ITDR.
00631      MOVE WS-REFUND-DESCRIP TO PMTHDO.
00632
00633  0510-CHECK-LF-STATUS.
00634
00635      IF (CM-LF-BENEFIT-CD IS EQUAL TO '00' OR '  ')
00636          MOVE LOW-VALUES         TO WS-LF-CANCEL-DATE
00637                                     PI-LF-CANCEL-DATE
00638          MOVE SPACES             TO WS-LF-CANCEL-DATE-ED
00639                                     PI-LF-CANCEL-DATE-ED
00640                                     PI-LF-REFUND-METH
00641          MOVE ZEROS              TO PI-LF-REFUND-AMT
00642          GO TO 0510-CHECK-AH-STATUS.
00643
00644      IF (CM-LF-CURRENT-STATUS  IS EQUAL TO  '8') AND
00645         (CM-LF-CANCEL-DT  IS NOT EQUAL TO  LOW-VALUES)
00646           MOVE CM-LF-ITD-CANCEL-AMT TO HLITDRO
00647           ADD  CM-LF-ITD-CANCEL-AMT TO WS-TOT-ITDR
00648           IF FIRST-ENTRY
00649               MOVE CM-LF-CANCEL-DT      TO DC-BIN-DATE-1
00650               MOVE SPACES               TO DC-OPTION-CODE
00651               PERFORM 9600-DATE-LINK
00652               IF NOT DATE-CONVERSION-ERROR
00653                    MOVE DC-GREG-DATE-1-EDIT TO HLCANCO
00654                                             PI-LF-CANCEL-DATE-ED
00655                                             WS-LF-CANCEL-DATE-ED
00656                    MOVE CM-LF-CANCEL-DT TO  PI-LF-CANCEL-DATE
00657                                             WS-LF-CANCEL-DATE.
00658
00659      IF (CM-LF-CURRENT-STATUS  IS EQUAL TO  '7')  AND
00660         (CM-LF-DEATH-DT  IS NOT EQUAL TO  LOW-VALUES)
00661           MOVE WS-PMT-DESCRIP       TO PMTHDO
00662           MOVE CM-LF-ITD-DEATH-AMT  TO HLITDRO
00663           ADD  CM-LF-ITD-DEATH-AMT TO WS-TOT-ITDR
00664         IF FIRST-ENTRY
00665             MOVE CM-LF-DEATH-DT       TO DC-BIN-DATE-1
00666             MOVE SPACES               TO DC-OPTION-CODE
00667             PERFORM 9600-DATE-LINK
00668             IF NOT DATE-CONVERSION-ERROR
00669                  MOVE DC-GREG-DATE-1-EDIT TO HLCANCO
00670                                           PI-LF-CANCEL-DATE-ED
00671                                           WS-LF-CANCEL-DATE-ED
00672                  MOVE CM-LF-DEATH-DT  TO  PI-LF-CANCEL-DATE
00673                                           WS-LF-CANCEL-DATE.
00674
00675  0510-CHECK-AH-STATUS.
00676
00677      IF (CM-AH-BENEFIT-CD IS EQUAL TO '00' OR '  ')
00678          MOVE LOW-VALUES         TO WS-AH-CANCEL-DATE
00679                                     PI-AH-CANCEL-DATE
00680          MOVE SPACES             TO WS-AH-CANCEL-DATE-ED
00681                                     PI-AH-CANCEL-DATE-ED
00682                                     PI-AH-REFUND-METH
00683          MOVE ZEROS              TO PI-AH-REFUND-AMT
00684          GO TO 0510-CONT.
00685
00686      IF (CM-AH-CURRENT-STATUS  IS EQUAL TO  '8')  AND
00687         (CM-AH-CANCEL-DT  IS NOT EQUAL TO  LOW-VALUES)
00688           MOVE CM-AH-ITD-CANCEL-AMT TO HAITDRO
00689           ADD  CM-AH-ITD-CANCEL-AMT TO WS-TOT-ITDR
00690         IF FIRST-ENTRY
00691             MOVE CM-AH-CANCEL-DT      TO DC-BIN-DATE-1
00692             MOVE SPACES               TO DC-OPTION-CODE
00693             PERFORM 9600-DATE-LINK
00694             IF NOT DATE-CONVERSION-ERROR
00695                 MOVE DC-GREG-DATE-1-EDIT TO HACANCO
00696                                             PI-AH-CANCEL-DATE-ED
00697                                             WS-AH-CANCEL-DATE-ED
00698                  MOVE CM-AH-CANCEL-DT  TO   PI-AH-CANCEL-DATE
00699                                             WS-AH-CANCEL-DATE.
00700
00701      IF (CM-AH-CURRENT-STATUS  IS EQUAL TO  '6' OR '7')   AND
00702         (CM-AH-SETTLEMENT-DT  IS NOT EQUAL TO  LOW-VALUES)
00703           MOVE WS-PMT-DESCRIP       TO PMTHDO
00704           MOVE CM-AH-ITD-LUMP-PMT   TO HAITDRO
00705           ADD  CM-AH-ITD-LUMP-PMT   TO WS-TOT-ITDR
00706          IF FIRST-ENTRY
00707              MOVE CM-AH-SETTLEMENT-DT  TO DC-BIN-DATE-1
00708              MOVE SPACES               TO DC-OPTION-CODE
00709              PERFORM 9600-DATE-LINK
00710              IF NOT DATE-CONVERSION-ERROR
00711                 MOVE DC-GREG-DATE-1-EDIT TO HACANCO
00712                                             PI-AH-CANCEL-DATE-ED
00713                                             WS-AH-CANCEL-DATE-ED
00714                 MOVE CM-AH-SETTLEMENT-DT TO PI-AH-CANCEL-DATE
00715                                             WS-AH-CANCEL-DATE.
00716
00717      IF CM-AH-CURRENT-STATUS IS EQUAL TO '8'
00718          MOVE 'CANC DT'              TO  CNCDTHDO
00719          GO TO 0510-CONT.
00720
00721      IF CM-LF-CURRENT-STATUS IS EQUAL TO '7'
00722          MOVE 'DTH DT '              TO  CNCDTHDO
00723          IF FIRST-ENTRY
00724              MOVE CM-LF-DEATH-DT     TO  DC-BIN-DATE-1
00725              MOVE ' '                TO  DC-OPTION-CODE
00726              PERFORM 9600-DATE-LINK
00727              IF NOT DATE-CONVERSION-ERROR
00728                  MOVE DC-GREG-DATE-1-EDIT TO HACANCO
00729                                              PI-AH-CANCEL-DATE-ED
00730                                              WS-AH-CANCEL-DATE-ED
00731                  MOVE CM-LF-DEATH-DT      TO PI-AH-CANCEL-DATE
00732                                              WS-AH-CANCEL-DATE.
00733
00734  0510-CONT.
00735
00736      IF WS-TOT-ITDR GREATER THAN ZEROS
00737          MOVE WS-TOT-ITDR TO TOITDRO.
00738
00739  0520-GET-COMPANY-RECORD.
00740      MOVE PI-COMPANY-ID          TO  WS-CF-COMPANY-ID.
00741      MOVE '1'                    TO  WS-CF-RECORD-TYPE.
00742      MOVE SPACES                 TO  WS-CF-ACCESS.
00743      MOVE +0                     TO  WS-CF-SEQUENCE-NO.
00744
00745      PERFORM 1000-READ-CONTROL  THRU  1099-EXIT.
00746
00747      IF CNTL-NOT-FOUND
00748          MOVE -1                 TO  HLCANCL
00749          MOVE AL-SANON           TO  HLCANCA
00750          MOVE ER-2616            TO  EMI-ERROR
00751          PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
00752          GO TO 8100-SEND-INITIAL-MAP.
00753
00754      IF WS-CF-COMPANY-ID  IS EQUAL TO  CF-COMPANY-ID
00755        AND WS-CF-RECORD-TYPE  IS EQUAL TO  CF-RECORD-TYPE
00756          MOVE CF-LIFE-OVERRIDE-L1
00757                                  TO  WS-CF-LIFE-OVERRIDE-L1
00758          MOVE CF-LIFE-OVERRIDE-L2
00759                                  TO  WS-CF-LIFE-OVERRIDE-L2
00760          MOVE CF-AH-OVERRIDE-L1  TO  WS-CF-AH-OVERRIDE-L1
00761          MOVE CF-AH-OVERRIDE-L2  TO  WS-CF-AH-OVERRIDE-L2
00762          MOVE CF-CR-REM-TERM-CALC
00763                                  TO  WS-CF-CR-REM-TERM-CALC
00764          MOVE CF-CR-R78-METHOD   TO  WS-CF-CR-R78-METHOD
00765          IF CF-DEFAULT-APR NUMERIC
00766              MOVE CF-DEFAULT-APR     TO  WS-CF-DEFAULT-APR
00767              GO TO 0525-GET-LF-BENEFIT
00768          ELSE
00769              MOVE ZEROS              TO  WS-CF-DEFAULT-APR
00770              GO TO 0525-GET-LF-BENEFIT.
00771
00772      MOVE -1                     TO  HLCANCL.
00773      MOVE AL-SANON               TO  HLCANCA.
00774      MOVE ER-2616                TO  EMI-ERROR.
00775
00776      PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
00777
00778      GO TO 8100-SEND-INITIAL-MAP.
00779
PEMMOD 0525-GET-LF-BENEFIT.
PEMMOD
PEMMOD     IF CM-LF-BENEFIT-CD = '  ' OR '00'
PEMMOD        GO TO 0527-GET-AH-BENEFIT
PEMMOD     END-IF
PEMMOD
00995      MOVE '4'                    TO  WS-CF-RECORD-TYPE.
00996      MOVE CM-LF-BENEFIT-CD       TO  WS-BENEFIT-NO
00997                                      HLCDO.
00998      MOVE WS-CF-LIFE-OVERRIDE-L2
00999                                  TO  HLKINDO.
01000
01001      PERFORM 1600-LOCATE-BENEFIT THRU 1699-EXIT.
01002
01003      IF BENEFIT-FOUND
01004          NEXT SENTENCE
01005      ELSE
01006          MOVE -1                 TO  HLCDL
01007          MOVE AL-SANON           TO  HLCDA
01008          MOVE ER-0028            TO  EMI-ERROR
01009          PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
01010          GO TO 8100-SEND-INITIAL-MAP.
01011
01016      IF CF-SUMMARY-PROCESSING (WS-INDEX)
01017          MOVE AL-UNNOF           TO  HLTERMA.
01018
01019 *****MOVE WS-BENEFIT-DESCRIP     TO  HLDESCO.
           MOVE WS-KIND                TO WS-LF-KIND
01020      MOVE CF-LF-COVERAGE-TYPE (WS-INDEX)
01021                                  TO  WS-CF-LF-COVERAGE-TYPE.
01022
01023      IF CF-CO-REM-TERM-CALC (WS-INDEX)  IS GREATER THAN  '0'
01024          MOVE CF-CO-REM-TERM-CALC (WS-INDEX)
01025                                  TO  WS-LF-CO-REM-TERM-CALC.
01026
01027      IF CF-CO-EARNINGS-CALC (WS-INDEX)  IS GREATER THAN  ' '
01028          MOVE CF-CO-EARNINGS-CALC (WS-INDEX)
01029                                  TO  WS-LF-CO-EARNINGS-CALC
01030                                      WS-LF-CO-REFUND-CALC.
01031
01032      IF CF-SPECIAL-CALC-CD (WS-INDEX)  IS GREATER THAN  ' '
01033          MOVE CF-SPECIAL-CALC-CD (WS-INDEX)
01034                                  TO  WS-LF-SPECIAL-CALC-CD.
01035
01036      IF CF-CO-REFUND-CALC (WS-INDEX)  IS GREATER THAN  '0'
01037          MOVE CF-CO-REFUND-CALC (WS-INDEX)
01038                                  TO  WS-LF-CO-REFUND-CALC.
01039
PEMMOD 0527-GET-AH-BENEFIT.
PEMMOD
PEMMOD     IF CM-AH-BENEFIT-CD = '00' OR '  '
PEMMOD        GO TO 0530-GET-STATE-RECORD
PEMMOD     END-IF
PEMMOD
01265      MOVE '5'                    TO  WS-CF-RECORD-TYPE.
01266      MOVE CM-AH-BENEFIT-CD       TO  WS-BENEFIT-NO
01267                                      HACDO.
01268      MOVE WS-CF-AH-OVERRIDE-L2   TO  HAKINDO.
01269
01270      PERFORM 1600-LOCATE-BENEFIT THRU 1699-EXIT.
01271
01272      IF BENEFIT-FOUND
01273          NEXT SENTENCE
01274      ELSE
01275          MOVE -1                 TO  HACDL
01276          MOVE AL-SANON           TO  HACDA
01277          MOVE ER-0028            TO  EMI-ERROR
01278          PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
01279          GO TO 8100-SEND-INITIAL-MAP.
01280
01285      IF CF-SUMMARY-PROCESSING (WS-INDEX)
01286          MOVE AL-UNNOF           TO  HATERMA.
01287
01288 *****MOVE WS-BENEFIT-DESCRIP     TO  HADESCO.
           MOVE WS-KIND                TO  WS-AH-KIND
01289
01290      IF CF-CO-REM-TERM-CALC (WS-INDEX)  IS GREATER THAN  '0'
01291          MOVE CF-CO-REM-TERM-CALC (WS-INDEX)
01292                                  TO  WS-AH-CO-REM-TERM-CALC.
01293
01294      IF CF-CO-EARNINGS-CALC (WS-INDEX)  IS GREATER THAN  ' '
01295          MOVE CF-CO-EARNINGS-CALC (WS-INDEX)
01296                                  TO  WS-AH-CO-EARNINGS-CALC
01297                                      WS-AH-CO-REFUND-CALC.
01298
01299      IF CF-SPECIAL-CALC-CD (WS-INDEX)  IS GREATER THAN  ' '
01300          MOVE CF-SPECIAL-CALC-CD (WS-INDEX)
01301                                  TO  WS-AH-SPECIAL-CALC-CD.
01302
01303      IF CF-CO-REFUND-CALC (WS-INDEX)  IS GREATER THAN  '0'
01304          MOVE CF-CO-REFUND-CALC (WS-INDEX)
01305                                  TO  WS-AH-CO-REFUND-CALC.
           IF CF-BENEFIT-CATEGORY (WS-INDEX) > ' '
              MOVE CF-BENEFIT-CATEGORY (WS-INDEX)
                                       TO WS-AH-BEN-CATEGORY
           END-IF
01306      .
00780  0530-GET-STATE-RECORD.
00781      MOVE PI-COMPANY-ID          TO  WS-CF-COMPANY-ID.
00782      MOVE '3'                    TO  WS-CF-RECORD-TYPE.
00783      MOVE CM-STATE               TO  WS-CF-ACCESS.
00784      MOVE +0                     TO  WS-CF-SEQUENCE-NO.
00785
00786      PERFORM 1000-READ-CONTROL  THRU  1099-EXIT.
00787
00788      IF CNTL-NOT-FOUND
00789          MOVE '0'                TO  WS-LF-ST-REFUND-CALC
00790                                      WS-LF-ST-REM-TERM-CALC
00791          MOVE 'N'                TO  WS-STATE-RECORD-SW
00792          GO TO 0560-FIND-ACCOUNT.
00793
00794      IF WS-CF-COMPANY-ID  IS EQUAL TO  CF-COMPANY-ID
00795        AND WS-CF-RECORD-TYPE  IS EQUAL TO  CF-RECORD-TYPE
00796        AND CM-STATE  IS EQUAL TO  CF-STATE-CODE
00797          MOVE 'Y'                TO  WS-STATE-RECORD-SW
00798      ELSE
00799          MOVE '0'                TO  WS-LF-ST-REFUND-CALC
00800                                      WS-LF-ST-REM-TERM-CALC
00801          MOVE 'N'                TO  WS-STATE-RECORD-SW
00802          GO TO 0560-FIND-ACCOUNT.
00803
00804      MOVE CF-STATE-ABBREVIATION  TO  WS-STATE-ABBREVIATION.
           MOVE CF-ST-FST-PMT-DAYS-CHG TO  WS-STATE-EXT-DAYS-CHG
00805
00806      IF CM-LF-BENEFIT-CD  = '00'
00807          GO TO 0550-GET-AH-BEN-IN-STATE.
00808
00809  0540-GET-LF-BEN-IN-STATE.
00810      MOVE CM-LF-BENEFIT-CD       TO  WS-BEN-CD.
00811      MOVE PI-LIFE-OVERRIDE-L1    TO  WS-LOOKUP-TYPE.
00812
CIDMOD     MOVE '0'                    TO WS-LF-ST-REM-TERM-CALC
CIDMOD     IF CF-ST-RT-CALC NOT = SPACES
CIDMOD        MOVE CF-ST-RT-CALC       TO WS-LF-ST-REM-TERM-CALC
CIDMOD     END-IF
CIDMOD
PEMMOD     IF WS-CF-LF-COVERAGE-TYPE = 'R'
PEMMOD        IF CF-ST-RF-LR-CALC > '0'
PEMMOD           MOVE CF-ST-RF-LR-CALC TO WS-LF-ST-REFUND-CALC
PEMMOD        END-IF
PEMMOD        IF WS-LF-CO-EARNINGS-CALC = 'N' OR '5'
PEMMOD           IF CF-ST-RF-LN-CALC > '0'
PEMMOD              MOVE CF-ST-RF-LN-CALC
PEMMOD                                 TO WS-LF-ST-REFUND-CALC
PEMMOD           END-IF
PEMMOD        END-IF
PEMMOD     ELSE
PEMMOD        IF CF-ST-RF-LL-CALC > '0'
PEMMOD           MOVE CF-ST-RF-LL-CALC TO WS-LF-ST-REFUND-CALC
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD
00813      PERFORM 1100-FIND-BENEFIT-IN-STATE  THRU  1199-EXIT.
00814
00815      IF NO-BENEFIT-FOUND
PEMMOD*        MOVE '0'                TO  WS-LF-ST-REFUND-CALC
00818          GO TO 0550-GET-AH-BEN-IN-STATE.
00819
00820      IF CF-ST-REM-TERM-CALC (SUB3)  IS GREATER THAN  '0'
00821          MOVE CF-ST-REM-TERM-CALC (SUB3)
00822                                  TO  WS-LF-ST-REM-TERM-CALC.
00823
00824      IF CF-ST-REFUND-CALC (SUB3)  IS GREATER THAN  '0'
00825          MOVE CF-ST-REFUND-CALC (SUB3)
00826                                  TO  WS-LF-ST-REFUND-CALC.
00827
00828  0550-GET-AH-BEN-IN-STATE.
00829      IF CM-AH-BENEFIT-CD  = '00'
00830          GO TO 0560-FIND-ACCOUNT.
00831
CIDMOD     MOVE '0'                    TO WS-AH-ST-REM-TERM-CALC
CIDMOD     IF CF-ST-RT-CALC NOT = SPACES
CIDMOD        MOVE CF-ST-RT-CALC       TO WS-AH-ST-REM-TERM-CALC
CIDMOD     END-IF
CIDMOD
PEMMOD     IF CF-ST-RF-AH-CALC > '0'
PEMMOD        MOVE CF-ST-RF-AH-CALC    TO WS-AH-ST-REFUND-CALC
PEMMOD     END-IF
PEMMOD     IF WS-AH-SPECIAL-CALC-CD = 'C'
PEMMOD        IF CF-ST-RF-CP-CALC > '0'
PEMMOD           MOVE CF-ST-RF-CP-CALC TO WS-AH-ST-REFUND-CALC
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD
00832      MOVE CM-AH-BENEFIT-CD       TO  WS-BEN-CD.
00833      MOVE PI-AH-OVERRIDE-L1      TO  WS-LOOKUP-TYPE.
00834
00835      PERFORM 1100-FIND-BENEFIT-IN-STATE  THRU  1199-EXIT.
00836
00837      IF NO-BENEFIT-FOUND
PEMMOD*        MOVE '0'                TO  WS-AH-ST-REFUND-CALC
00840          GO TO 0560-FIND-ACCOUNT.
00841
00842      IF CF-ST-REM-TERM-CALC (SUB3)  IS GREATER THAN  '0'
00843          MOVE CF-ST-REM-TERM-CALC (SUB3)
00844                                  TO  WS-AH-ST-REM-TERM-CALC.
00845
00846      IF CF-ST-REFUND-CALC (SUB3)  IS GREATER THAN  '0'
00847          MOVE CF-ST-REFUND-CALC (SUB3)
00848                                  TO  WS-AH-ST-REFUND-CALC.
00849
00850  0560-FIND-ACCOUNT.
00851      MOVE CM-COMPANY-CD          TO  WS-AM-COMPANY-CD.
00852      MOVE CM-CARRIER             TO  WS-AM-CARRIER.
00853      MOVE CM-GROUPING            TO  WS-AM-GROUPING.
00854      MOVE CM-STATE               TO  WS-AM-STATE.
00855      MOVE CM-ACCOUNT             TO  WS-AM-ACCOUNT.
00856      MOVE CM-CERT-EFF-DT         TO  WS-AM-EXPIRATION-DT.
00857      MOVE LOW-VALUES             TO  WS-AM-FILLER.
00858
00859      PERFORM 1200-START-ACCOUNT-MASTER  THRU  1299-EXIT.
00860
00861      IF ACCT-FOUND
00862          NEXT SENTENCE
00863      ELSE
00864          GO TO 0585-NO-ACCT.
00865
00866  0570-READ-NEXT-RANGE.
00867      PERFORM 1300-READ-ACCOUNT-MASTER  THRU  1399-EXIT.
00868
00869      IF ACCT-FOUND
00870          NEXT SENTENCE
00871      ELSE
00872          GO TO 0580-ACCT-NOT-FOUND.
00873
00874      IF CM-COMPANY-CD  IS EQUAL TO  AM-COMPANY-CD
00875        AND CM-CARRIER  IS EQUAL TO  AM-CARRIER
00876        AND CM-GROUPING  IS EQUAL TO  AM-GROUPING
00877        AND CM-STATE  IS EQUAL TO  AM-STATE
00878        AND CM-ACCOUNT  IS EQUAL TO  AM-ACCOUNT
00879          NEXT SENTENCE
00880      ELSE
00881          GO TO 0580-ACCT-NOT-FOUND.
00882
00883      MOVE 'X'                    TO  DATE-RANGE-SW.
00884
00885      IF CM-CERT-EFF-DT  IS LESS THAN  AM-EFFECTIVE-DT
00886          MOVE 'X'                TO  DATE-RANGE-SW
00887          GO TO 0580-ACCT-NOT-FOUND.
00888
00889      IF CM-CERT-EFF-DT  IS NOT LESS THAN  AM-EXPIRATION-DT
00890          GO TO 0570-READ-NEXT-RANGE.
00891
           IF WS-BROWSE-STARTED-SW = 'Y'
              
      * EXEC CICS ENDBR
      *          DATASET    ('ERACCT')
      *       END-EXEC
           MOVE 'ERACCT' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007039' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              MOVE SPACES             TO  WS-BROWSE-STARTED-SW
           END-IF
00892      MOVE ' '                    TO  DATE-RANGE-SW.
00893      MOVE 'Y'                    TO  WS-ACCT-RECORD-SW.
00894
00895      GO TO 0590-SET-ACCOUNT.
00896
00897  0580-ACCT-NOT-FOUND.
00898      IF DATE-RANGE-SW  IS NOT EQUAL TO  SPACES
00899          MOVE ER-2601            TO  EMI-ERROR
00900      ELSE
00901          MOVE ER-2619            TO  EMI-ERROR.
00902
00903      MOVE 'N'                    TO  WS-ACCT-RECORD-SW.
00904
00905      PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
00906
00907  0585-NO-ACCT.
00908      MOVE SPACES                 TO  WS-ACCT-USER-FLD-5.
00909      MOVE '0'                    TO  WS-AM-EARN-METHOD-L
00910                                      WS-AM-EARN-METHOD-R
00911                                      WS-AM-EARN-METHOD-A
00912                                      WS-LF-AM-REM-TERM-CALC
00913                                      WS-AH-AM-REM-TERM-CALC.
00914
00915      GO TO 0600-CHECK-FOR-LIFE.
00916
00917  0590-SET-ACCOUNT.
00918 ******************************************************************
00919 ******* THIS CODE ALLOWS THE FORM MASTER BENEFIT CODES TERM
00920 ******* AND REFUND METHOD TO OVERRIDE COMPANY AND STATE REFUND
00921 ******* METHODS.
00922 ******************************************************************
00923      MOVE SPACES              TO WS-LF-FO-REFUND-CALC.
00924      MOVE SPACES              TO WS-AH-FO-REFUND-CALC.
00925      IF PI-COMPANY-ID EQUAL TO 'NCL'
00926           IF ((CM-LF-BENEFIT-CD NOT = ZEROS AND SPACES)
00927                  OR (CM-AH-BENEFIT-CD NOT = ZEROS AND SPACES))
00928                   PERFORM 7000-GET-ERFORM THRU 7000-EXIT
00929                   IF FORM-FOUND
00930                       PERFORM 7100-BENEFIT-SEARCH THRU 7100-EXIT.
00931 ******************************************************************
00932
00933      MOVE AM-FLD-5               TO  WS-ACCT-USER-FLD-5.
00934      MOVE '0'                    TO  WS-AM-EARN-METHOD-L
00935                                      WS-AM-EARN-METHOD-R
00936                                      WS-AM-EARN-METHOD-A.
070622     IF AM-GPCD > 1
070622       AND AM-GPCD < 6
070622       AND PI-CANCEL-REASON NOT > SPACES
070622        MOVE 'Y'                 TO PI-CANCEL-REASON
070622     END-IF.
           MOVE PI-CANCEL-REASON       TO  CANREAO
00937
PEMMOD*    EVALUATE AM-EARN-METHOD-R
PEMMOD*       WHEN 'R'
PEMMOD*           MOVE '1'                TO  WS-AM-EARN-METHOD-R
PEMMOD*       WHEN 'P'
PEMMOD*           MOVE '2'                TO  WS-AM-EARN-METHOD-R
PEMMOD*       WHEN 'C'
PEMMOD*           MOVE '3'                TO  WS-AM-EARN-METHOD-R
PEMMOD*       WHEN 'A'
PEMMOD*           MOVE '6'                TO  WS-AM-EARN-METHOD-R
PEMMOD*       WHEN 'M'
PEMMOD*           MOVE '8'                TO  WS-AM-EARN-METHOD-R
PEMMOD*       WHEN 'S'
PEMMOD*           MOVE '9'                TO  WS-AM-EARN-METHOD-R
PEMMOD*    END-EVALUATE.
PEMMOD
PEMMOD*    EVALUATE AM-EARN-METHOD-L
PEMMOD*       WHEN 'R'
PEMMOD*           MOVE '1'                TO  WS-AM-EARN-METHOD-L
PEMMOD*       WHEN 'P'
PEMMOD*           MOVE '2'                TO  WS-AM-EARN-METHOD-L
PEMMOD*       WHEN 'C'
PEMMOD*           MOVE '3'                TO  WS-AM-EARN-METHOD-L
PEMMOD*       WHEN 'A'
PEMMOD*           MOVE '6'                TO  WS-AM-EARN-METHOD-L
PEMMOD*       WHEN 'M'
PEMMOD*           MOVE '8'                 TO  WS-AM-EARN-METHOD-L
PEMMOD*       WHEN 'S'
PEMMOD*           MOVE '9'                 TO  WS-AM-EARN-METHOD-L
PEMMOD*    END-EVALUATE.
PEMMOD
PEMMOD*    EVALUATE AM-EARN-METHOD-A
PEMMOD*       WHEN 'R'
PEMMOD*           MOVE '1'                TO  WS-AM-EARN-METHOD-A
PEMMOD*       WHEN 'P'
PEMMOD*           MOVE '2'                TO  WS-AM-EARN-METHOD-A
PEMMOD*       WHEN 'C'
PEMMOD*           MOVE '3'                TO  WS-AM-EARN-METHOD-A
PEMMOD*       WHEN 'A'
PEMMOD*           MOVE '6'                TO  WS-AM-EARN-METHOD-A
PEMMOD*       WHEN 'M'
PEMMOD*           MOVE '8'                TO  WS-AM-EARN-METHOD-A
PEMMOD*       WHEN 'N'
PEMMOD*           MOVE '5'                TO  WS-AM-EARN-METHOD-A
PEMMOD*       WHEN 'S'
PEMMOD*           MOVE '9'                TO  WS-AM-EARN-METHOD-A
PEMMOD*    END-EVALUATE.
00984
00985      MOVE ZEROS                  TO  WS-TOT-AH-PREM
00986                                      WS-TOT-LF-PREM
00987                                      WS-TOT-AH-RFND
00988                                      WS-TOT-LF-RFND.
00989
00990  0600-CHECK-FOR-LIFE.
00991      IF CM-LF-BENEFIT-CD  = '00'
00992          GO TO 0640-AH-BENEFIT.
00993
00994  0610-GET-LIFE-BENEFIT.
00995
01012      IF ACCT-FOUND
01013          MOVE +1                 TO SUB3
01014          PERFORM 5000-GET-LF-AM-REM-TERM THRU 5900-EXIT.
01046      MOVE WS-LF-CANCEL-DATE-ED   TO  HLCANCO.
01051      MOVE WS-LF-KIND             TO  HLEDESCO.
01075      MOVE CM-LF-ORIG-TERM        TO  HLTERMO
           IF WS-LF-CO-EARNINGS-CALC = 'B'
01146         COMPUTE WS-LIFE-PREMIUM = CM-LF-PREMIUM-AMT
01147                              + CM-LF-ALT-PREMIUM-AMT
           ELSE
              MOVE CM-LF-PREMIUM-AMT TO WS-LIFE-PREMIUM
           END-IF
01148
01149      MOVE WS-LIFE-PREMIUM        TO  HLPREMO
           EVALUATE TRUE
              WHEN ((OPEN-LF-CLAIM)
                 OR (CLOSED-LF-CLAIM))
                 AND (WS-LF-CANCEL-DATE NOT = WS-LF-INCUR-DT)
                 MOVE ZEROS            TO HLREFNDO  WS-CALC-REFUND
                                          WS-TOT-LF-RFND
                                          PI-LF-REFUND-AMT
                 IF CM-AH-BENEFIT-CD NOT = '00'
                    MOVE ZEROS         TO HAREFNDO
                                          WS-TOT-AH-RFND
                                          PI-AH-REFUND-AMT
                    MOVE SPACES        TO PI-AH-REFUND-METH
                 END-IF
100518           IF OTHER-CLAIM
100518              MOVE ER-2998          TO EMI-ERROR
100518           ELSE
                    MOVE ER-2965          TO EMI-ERROR
100518           END-IF
                 PERFORM 9700-ERROR-FORMAT
                                       THRU  9799-EXIT
                 GO TO 0640-AH-BENEFIT
              WHEN OPEN-LF-CLAIM
                 AND WS-LF-CANCEL-DATE = WS-LF-INCUR-DT
                 AND WS-ST-REF-IND = '4'
                 MOVE ER-2966          TO EMI-ERROR
100518           MOVE WS-CLAIM-LIT     TO EMI-CLAIM-TYPE
                 PERFORM 9700-ERROR-FORMAT
                                       THRU  9799-EXIT
              WHEN OPEN-LF-CLAIM
                 AND WS-LF-CANCEL-DATE = WS-LF-INCUR-DT
                 AND WS-ST-REF-IND NOT = '4'
                 MOVE ER-2967          TO EMI-ERROR
100518           MOVE WS-CLAIM-LIT     TO EMI-CLAIM-TYPE
                 PERFORM 9700-ERROR-FORMAT
                                       THRU  9799-EXIT
                 GO TO 0640-AH-BENEFIT
              WHEN CLOSED-LF-CLAIM
                 AND WS-LF-CANCEL-DATE = WS-LF-INCUR-DT
                 AND WS-ST-REF-IND = '4'
                 MOVE ER-2968          TO EMI-ERROR
100518           MOVE WS-CLAIM-LIT     TO EMI-CLAIM-TYPE
                 PERFORM 9700-ERROR-FORMAT
                                       THRU  9799-EXIT
              WHEN CLOSED-LF-CLAIM
                 AND WS-LF-CANCEL-DATE = WS-LF-INCUR-DT
                 AND WS-ST-REF-IND NOT = '4'
                 MOVE ER-2969          TO EMI-ERROR
100518           MOVE WS-CLAIM-LIT     TO EMI-CLAIM-TYPE
                 PERFORM 9700-ERROR-FORMAT
                                       THRU  9799-EXIT
                 GO TO 0640-AH-BENEFIT
              WHEN OTHER
                 CONTINUE
           END-EVALUATE
           .
01040  0620-GET-REM-TERM.
01041      MOVE WS-CF-LIFE-OVERRIDE-L1
01042                                  TO  CP-LIFE-OVERRIDE-CODE.
01043      MOVE CM-CERT-EFF-DT         TO  CP-CERT-EFF-DT.
01044      MOVE CM-LOAN-1ST-PMT-DT     TO  CP-FIRST-PAY-DATE.
01045      MOVE WS-LF-CANCEL-DATE      TO  CP-VALUATION-DT.
01046      MOVE WS-LF-CANCEL-DATE-ED   TO  HLCANCO.
01047      MOVE CM-STATE               TO  CP-STATE.
01048      MOVE WS-STATE-ABBREVIATION  TO  CP-STATE-STD-ABBRV.
01049      MOVE WS-CF-LF-COVERAGE-TYPE
01050                                  TO  CP-BENEFIT-TYPE.
01051      MOVE WS-LF-KIND             TO  HLEDESCO.
01052      MOVE WS-LF-SPECIAL-CALC-CD  TO  CP-SPECIAL-CALC-CD.
01053      MOVE CM-PAY-FREQUENCY       TO  CP-PAY-FREQUENCY.
01054      MOVE CM-LOAN-APR            TO  CP-LOAN-APR.
01055      MOVE '2'                    TO  CP-PROCESS-TYPE.
01056      MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.
01057      MOVE CM-COMPANY-CD          TO  CP-COMPANY-CD.
01058      MOVE WS-ACCT-USER-FLD-5     TO  CP-ACCT-FLD-5.
01059      MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.
01060      MOVE WS-CF-CR-REM-TERM-CALC
01061                                  TO  CP-REM-TERM-METHOD.
01062
01063      IF WS-LF-CO-REM-TERM-CALC  IS GREATER THAN  '0'
01064          MOVE WS-LF-CO-REM-TERM-CALC
01065                                  TO  CP-REM-TERM-METHOD.
01066
01067      IF WS-LF-ST-REM-TERM-CALC  IS GREATER THAN  '0'
01068          MOVE WS-LF-ST-REM-TERM-CALC
01069                                  TO  CP-REM-TERM-METHOD.
01070
01071      IF WS-LF-AM-REM-TERM-CALC  IS GREATER THAN  '0'
01072          MOVE WS-LF-AM-REM-TERM-CALC
01073                                  TO  CP-REM-TERM-METHOD.
01074
083005     IF (PI-COMPANY-ID = 'CID')
022608        AND (CP-STATE-STD-ABBRV = 'WI')
083005        MOVE '7'                 TO CP-REM-TERM-METHOD
022608        MOVE '5'                 TO CP-REM-TRM-CALC-OPTION
083005     END-IF
           IF (PI-COMPANY-ID = 'CID')
              AND (CP-STATE-STD-ABBRV = 'MO')
              AND (CM-CERT-EFF-DT >= X'9B41')
              AND (CM-CERT-EFF-DT <= X'A2FB')
              MOVE '7'                 TO CP-REM-TERM-METHOD
              MOVE '5'                 TO CP-REM-TRM-CALC-OPTION
           END-IF
01075      MOVE CM-LF-ORIG-TERM        TO  HLTERMO
01076                                      CP-ORIGINAL-TERM
01077                                      CP-LOAN-TERM.
01078
01079      IF CP-TRUNCATED-LIFE
01080          MOVE CM-LOAN-TERM       TO  CP-LOAN-TERM.
01081
01082      IF CP-TERM-IS-DAYS
01083          IF CM-LF-TERM-IN-DAYS  IS NUMERIC
01084              MOVE CM-LF-TERM-IN-DAYS
01085                                  TO  CP-TERM-OR-EXT-DAYS
01086          ELSE
01087              MOVE ZEROS          TO  CP-TERM-OR-EXT-DAYS
01088      ELSE
01089          IF CM-PMT-EXTENSION-DAYS  IS NUMERIC
01090              MOVE CM-PMT-EXTENSION-DAYS
01091                                  TO  CP-TERM-OR-EXT-DAYS
01092          ELSE
01093              MOVE ZEROS          TO  CP-TERM-OR-EXT-DAYS.
01094
01095 *** READ STATE MASTER RECORD FOR FREE LOOK PERIOD ***
01096      MOVE PI-COMPANY-ID          TO  WS-CF-COMPANY-ID.
01097      MOVE '3'                    TO  WS-CF-RECORD-TYPE.
01098      MOVE CM-STATE               TO  WS-CF-ACCESS.
01099      MOVE +0                     TO  WS-CF-SEQUENCE-NO.
01100
01101      
      * EXEC CICS READ
01102 *        DATASET  (ELCNTL-ID)
01103 *        SET      (ADDRESS OF CONTROL-FILE)
01104 *        RIDFLD   (WS-CF-CONTROL-PRIMARY)
01105 *        RESP     (WS-RESPONSE)
01106 *    END-EXEC.
      *    MOVE '&"S        E          (  N#00007296' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303037323936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01107
01108      IF WS-RESP-NOTOPEN
01109          MOVE ER-2617            TO  EMI-ERROR
01110          GO TO 0320-INPUT-ERROR.
01111
01112      IF WS-RESP-NOTFND
01113          MOVE ZERO               TO  CP-FREE-LOOK
01114      ELSE
01115          MOVE CF-ST-FREE-LOOK-PERIOD
01116                                  TO CP-FREE-LOOK.
01117
CIDMOD     IF (WS-LF-CO-EARNINGS-CALC = 'B') AND
CIDMOD        (WS-LF-SPECIAL-CALC-CD NOT = 'L')
CIDMOD        ADD +1   TO CP-ORIGINAL-TERM CP-LOAN-TERM
CIDMOD     END-IF
CIDMOD
01118      PERFORM 0700-LINK-REM-TERM  THRU  0700-EXIT.
01119
CIDMOD     IF (WS-LF-CO-EARNINGS-CALC = 'B') AND
CIDMOD        (WS-LF-SPECIAL-CALC-CD NOT = 'L')
CIDMOD        MOVE CP-REMAINING-TERM-1   TO WS-BALLOON-RTRM
CIDMOD        COMPUTE CP-REMAINING-TERM-1 =
CIDMOD                CP-REMAINING-TERM-1 - +1
CIDMOD        COMPUTE CP-REMAINING-TERM-2 =
CIDMOD                CP-REMAINING-TERM-2 - +1
CIDMOD     END-IF
CIDMOD
CIDMOD     IF CP-REMAINING-TERM-1 NEGATIVE
CIDMOD        MOVE ZEROS               TO CP-REMAINING-TERM-1
CIDMOD     END-IF
CIDMOD     IF CP-REMAINING-TERM-2 NEGATIVE
CIDMOD        MOVE ZEROS               TO CP-REMAINING-TERM-2
CIDMOD     END-IF
01120      MOVE CP-REMAINING-TERM-1    TO  HLREMO.
01121
00991      IF PI-LF-CANCEL-DATE = LOW-VALUES
00992          GO TO 0640-AH-BENEFIT.
01122 *    IF CP-REMAINING-TERM-1  IS GREATER THAN  CM-LF-ORIG-TERM
01123 *        MOVE CM-LF-ORIG-TERM    TO  HLREMO.
01124
01125      IF CM-LF-CURRENT-STATUS  IS EQUAL TO  '8'
01126          IF CM-LF-CANCEL-DT  IS NOT EQUAL TO  LOW-VALUES
01127              MOVE ER-0681        TO  EMI-ERROR
01128              PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
01129
01130      IF CM-LF-CURRENT-STATUS  IS EQUAL TO  '7'
01131          IF CM-LF-DEATH-DT  IS NOT EQUAL TO  LOW-VALUES
01132              MOVE ER-0682        TO  EMI-ERROR
01133              PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
01134
01135  0630-CONTINUE-LIFE.
01136
CIDMOD     IF (WS-LF-CO-EARNINGS-CALC = 'B') AND
CIDMOD        (WS-LF-SPECIAL-CALC-CD NOT = 'L')
CIDMOD        MOVE WS-BALLOON-RTRM     TO CP-REMAINING-TERM
                                          FREMTRMO
CIDMOD     ELSE
              MOVE CP-REMAINING-TERM-2 TO  CP-REMAINING-TERM
                                           FREMTRMO
CIDMOD     END-IF
01139      MOVE WS-LF-CO-EARNINGS-CALC TO  CP-EARNING-METHOD.
01140      MOVE CM-LF-BENEFIT-AMT      TO  CP-ORIGINAL-BENEFIT.
032912     if pi-company-id = 'AHL'
032912        MOVE CM-LF-CLASS-CD      TO CP-CLASS-CODE
032912        if cp-class-code = spaces
032912           move zeros            to cp-class-code
032912        end-if
032912     ELSE
CIDMOD        MOVE CM-RATE-CLASS       TO  CP-CLASS-CODE
032912     END-IF
           IF (PI-COMPANY-ID = 'DCC')
              AND (AM-DCC-PRODUCT-CODE = 'DDF')
              MOVE ' '                 TO WS-PDEF-RECORD-SW
              PERFORM 0730-GET-DDF-FACTORS
                                       THRU 0730-EXIT
              IF PDEF-FOUND
070622           IF CM-INSURED-JOINT-AGE LESS CM-INSURED-ISSUE-AGE
070622               MOVE CM-INSURED-JOINT-AGE TO WS-EDIT-AGE
070622           ELSE
070622               MOVE CM-INSURED-ISSUE-AGE TO WS-EDIT-AGE
070622           END-IF
070622           COMPUTE WS-ATT-AGE = WS-EDIT-AGE
070622              + ((CM-LF-ORIG-TERM - CP-REMAINING-TERM-3) / 12)
                 PERFORM VARYING P1 FROM +1 BY +1 UNTIL
PEMTST              (P1 > +11)
                    OR ((PD-PROD-CODE (P1) = 'L' OR 'O')
070622                AND (PD-MAX-ATT-AGE (P1) >= WS-ATT-AGE))
                 END-PERFORM
PEMTST           IF P1 < +12
                    MOVE PD-MAX-AMT (P1) TO CP-R-MAX-TOT-BEN
                 END-IF
              END-IF
           END-IF
01142      PERFORM 0710-LINK-REM-AMOUNT THRU 0710-EXIT.
01143
01144      MOVE CP-REMAINING-AMT       TO CP-REMAINING-BENEFIT.
01145
           IF WS-LF-CO-EARNINGS-CALC = 'B'
01146         COMPUTE WS-LIFE-PREMIUM = CM-LF-PREMIUM-AMT
01147                              + CM-LF-ALT-PREMIUM-AMT
           ELSE
              MOVE CM-LF-PREMIUM-AMT TO WS-LIFE-PREMIUM
           END-IF
01148
01149      MOVE WS-LIFE-PREMIUM        TO  HLPREMO
01150                                      WS-TOT-LF-PREM.
01151
CIDMOD     IF (WS-LF-CO-EARNINGS-CALC = 'B') AND
CIDMOD        (WS-LF-SPECIAL-CALC-CD NOT = 'L')
CIDMOD        MOVE WS-BALLOON-RTRM     TO CP-REMAINING-TERM
CIDMOD     ELSE
CIDMOD        MOVE CP-REMAINING-TERM-1 TO  CP-REMAINING-TERM
CIDMOD     END-IF
01153
01154      MOVE WS-LF-CO-REFUND-CALC   TO  CP-EARNING-METHOD.
01155      MOVE WS-LF-CO-EARNINGS-CALC TO  CP-RATING-METHOD.
01156
01157      IF WS-LF-ST-REFUND-CALC  IS GREATER THAN  ZERO
01158          MOVE WS-LF-ST-REFUND-CALC
01159                                  TO  CP-EARNING-METHOD.
01160
01161      IF WS-LF-FO-REFUND-CALC  IS GREATER THAN  ZERO
01162          MOVE WS-LF-FO-REFUND-CALC
01163                                  TO  CP-EARNING-METHOD.
01164
102212     if (pi-company-id = 'CID')
102212        and (cp-state-std-abbrv = 'MN' OR 'WA')
102212        continue
102212     else
111907        IF CP-RATING-METHOD = '4'
111907           MOVE '4'              TO CP-EARNING-METHOD
111907        END-IF
102212     end-if
PEMMOD*    IF WS-CF-LF-COVERAGE-TYPE = 'R'
PEMMOD*        IF WS-AM-EARN-METHOD-R  IS GREATER THAN  ZERO
PEMMOD*            MOVE WS-AM-EARN-METHOD-R
PEMMOD*                                TO  CP-EARNING-METHOD
PEMMOD*        END-IF
PEMMOD*    ELSE
PEMMOD*        IF WS-AM-EARN-METHOD-L  IS GREATER THAN  ZERO
PEMMOD*            MOVE WS-AM-EARN-METHOD-L
PEMMOD*                                TO  CP-EARNING-METHOD.
01174
032912     if pi-company-id = 'AHL'
032912        MOVE CM-LF-CLASS-CD      TO CP-CLASS-CODE
032912        if cp-class-code = spaces
032912           move zeros            to cp-class-code
032912        end-if
032912     ELSE
CIDMOD        MOVE CM-RATE-CLASS       TO  CP-CLASS-CODE
032912     END-IF
01176      MOVE CM-LF-BENEFIT-CD       TO  CP-BENEFIT-CD.
01177      MOVE CM-LF-BENEFIT-AMT      TO  CP-ORIGINAL-BENEFIT
01178                                      CP-RATING-BENEFIT-AMT.
01179      IF CP-STATE-STD-ABBRV = 'OR'
01180          COMPUTE CP-RATING-BENEFIT-AMT = CM-LF-BENEFIT-AMT +
01181                                          CM-LF-ALT-BENEFIT-AMT.
CIDMOD****   N O T E   ****
CIDMOD*      CID DOES NOT WANT THE REFUND METHOD TO OVERRIDE
CIDMOD*      THE OH HARD CODING IN ELCRFNDP
CIDMOD
CIDMOD     IF PI-COMPANY-ID = 'CID'
CIDMOD        IF CP-STATE-STD-ABBRV = 'OH'
CIDMOD           MOVE WS-LF-CO-EARNINGS-CALC
CIDMOD                                 TO CP-EARNING-METHOD
CIDMOD        END-IF
CIDMOD     END-IF
CIDMOD
CIDMOD****   N O T E   ****
CIDMOD
      ****   MN   NOTE  ****  CID HAS THE STATE REFUND METHOD SET AS
      ****      R78 BUT THEY WANT THE BALLOONS REFUNDED WITH A
      ****      CUSTOM FORMULA THAT IS IN ELRFND SO I AM SETTING
      ****     THE APPROPRIATE OPTIONS HERE
      *    IF PI-COMPANY-ID = 'CID'
      *       IF (CP-STATE-STD-ABBRV = 'MN')
      *          AND (CM-CERT-EFF-DT > X'A4FF')
      *          AND (WS-LF-CO-EARNINGS-CALC = 'B')
      *          MOVE '5'              TO CP-EARNING-METHOD
      *       END-IF
      *    END-IF
01182      MOVE CM-LF-PREMIUM-AMT      TO  CP-ORIGINAL-PREMIUM.
01183      MOVE CM-INSURED-ISSUE-AGE   TO  CP-ISSUE-AGE.
01184      MOVE CM-LF-DEV-CODE         TO  CP-DEVIATION-CODE.
01185      MOVE CM-LF-DEV-PCT          TO  CP-RATE-DEV-PCT.
01186      MOVE WS-CF-CR-R78-METHOD    TO  CP-R78-OPTION.
           MOVE PI-CANCEL-REASON       TO CP-CANCEL-REASON
01187
102808*    IF CP-EARN-AS-NET-PAY
01189 *        IF CP-LOAN-APR NOT GREATER THAN ZEROS
01190 *           IF WS-CF-DEFAULT-APR GREATER THAN ZEROS
01191 *               MOVE WS-CF-DEFAULT-APR
01192 *                                TO  CP-LOAN-APR
01193 *               MOVE ER-3780     TO  EMI-ERROR
01194 *               PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
01195 *           ELSE
01196 *               MOVE ER-3781     TO  EMI-ERROR
01197 *               PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
01198
      *    DISPLAY ' 1278 REM AMT      ' CP-REMAINING-BENEFIT
      *    DISPLAY ' 1278 REM TRM      ' CP-REMAINING-TERM
      *    DISPLAY ' 1278 EARN METH    ' CP-EARNING-METHOD
      *    DISPLAY ' 1278 RATE METH    ' CP-RATING-METHOD
      *    DISPLAY ' 1278 CLASS        ' CP-CLASS-CODE
      *    DISPLAY ' 1278 BENE CODE    ' CP-BENEFIT-CD
      *    DISPLAY ' 1278 ORIG BENE    ' CP-ORIGINAL-BENEFIT
      *    DISPLAY ' 1278 RATE BENE    ' CP-RATING-BENEFIT-AMT
      *    DISPLAY ' 1278 ORIG PREM    ' CP-ORIGINAL-PREMIUM
      *    DISPLAY ' 1278 ISS AGE      ' CP-ISSUE-AGE
      *    DISPLAY ' 1278 DEV CODE     ' CP-DEVIATION-CODE
      *    DISPLAY ' 1278 DEV PCT      ' CP-RATE-DEV-PCT
      *    DISPLAY ' 1278 APR          ' CP-LOAN-APR
      *    DISPLAY ' 1278 EXT DAYS     ' CP-TERM-OR-EXT-DAYS
      *    DISPLAY ' 1278 LOAN TERM    ' CP-LOAN-TERM
      *    DISPLAY ' 1278 SPEC CALC    ' CP-SPECIAL-CALC-CD
      *    DISPLAY ' 1278 ST STD ABB   ' CP-STATE-STD-ABBRV
050713*    MOVE WS-STATE-EXT-DAYS-CHG  TO CP-EXT-DAYS-CALC
01199      PERFORM 0800-LINK-REFUND  THRU  0899-EXIT.
01200
01201      MOVE 'L'                    TO  WS-REFUND-SEARCH-SW.
01202
01203      PERFORM 1400-GET-REFUND-TYPE  THRU  1499-EXIT.
01204
01205      IF CP-ERROR-RATE-IS-ZERO
01206        OR CP-ERROR-RATE-NOT-FOUND
01207          MOVE ER-2740            TO  EMI-ERROR
01208          PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
01209
01210      IF CP-ERROR-RATE-FILE-NOTOPEN
01211          MOVE ER-2617            TO  EMI-ERROR
01212          PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
01213
01214      MOVE ZEROS                  TO  WS-CALC-REFUND.
01215      MOVE CP-CALC-REFUND         TO  HLREFNDO  WS-CALC-REFUND
01216                                      WS-TOT-LF-RFND
01217                                      PI-LF-REFUND-AMT.
01218      MOVE CP-REMAINING-AMT       TO  HLREDO.
01219
01220      MOVE CP-REFUND-TYPE-USED    TO  PI-LF-REFUND-METH.
032117     move cm-life-comm-pct       to hlcpcto
032117     compute hlcamto rounded =
032117        cp-calc-refund * cm-life-comm-pct
01222      IF WS-LF-CO-EARNINGS-CALC  IS NOT EQUAL TO  'B'
01223          GO TO 0640-AH-BENEFIT.
01224
070105*    IF WS-LF-SPECIAL-CALC-CD = 'L'
070105*        ADD +1                  TO  CP-REMAINING-TERM
070105*                                    CP-ORIGINAL-TERM.
01228
01229      MOVE 'L'                    TO  CP-BENEFIT-TYPE.
01230      MOVE '2'                    TO  CP-EARNING-METHOD
01231                                      CP-RATING-METHOD.
           IF PI-COMPANY-ID = 'CID'
              IF (CP-STATE-STD-ABBRV = 'MN')
                 AND (CM-CERT-EFF-DT > X'A4FF')
                 AND (WS-LF-CO-EARNINGS-CALC = 'B')
                 MOVE '5'              TO CP-EARNING-METHOD
                 MOVE WS-LF-CO-EARNINGS-CALC TO CP-RATING-METHOD
              END-IF
           END-IF
01232      MOVE CM-LF-ALT-BENEFIT-AMT  TO  CP-ORIGINAL-BENEFIT
01233                                      CP-REMAINING-BENEFIT
01234                                      CP-RATING-BENEFIT-AMT.
01235      IF CP-STATE-STD-ABBRV = 'OR'
01236          COMPUTE CP-RATING-BENEFIT-AMT = CM-LF-BENEFIT-AMT +
01237                                          CM-LF-ALT-BENEFIT-AMT.
01238      MOVE CM-LF-ALT-PREMIUM-AMT  TO  CP-ORIGINAL-PREMIUM.
01239      MOVE 'LEV'                  TO  CP-DEVIATION-CODE.
01240
01241      PERFORM 0800-LINK-REFUND  THRU  0899-EXIT.
01242
01243      IF CP-ERROR-RATE-IS-ZERO
01244        OR CP-ERROR-RATE-NOT-FOUND
01245          MOVE ER-2740            TO  EMI-ERROR
01246          PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
01247
01248      IF CP-ERROR-RATE-FILE-NOTOPEN
01249          MOVE ER-2617            TO  EMI-ERROR
01250          PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
01251
01252      ADD CP-CALC-REFUND          TO  WS-CALC-REFUND.
01253
01254      MOVE WS-CALC-REFUND         TO  HLREFNDO
01255                                      WS-TOT-LF-RFND
01256                                      PI-LF-REFUND-AMT.
01257
01258      MOVE CP-REFUND-TYPE-USED    TO  PI-LF-REFUND-METH.
01259
01260  0640-AH-BENEFIT.
01261      IF CM-AH-BENEFIT-CD = '00'
01262          GO TO 0680-CONTINUE.
01263
01264  0650-GET-AH-BENEFIT.
01281      IF ACCT-FOUND
01282          MOVE +1                 TO SUB3
01283          PERFORM 6000-GET-AH-AM-REM-TERM THRU 6900-EXIT.
01388      MOVE CM-AH-PREMIUM-AMT      TO  HAPREMO
01312      MOVE WS-AH-CANCEL-DATE-ED   TO  HACANCO.
01339      MOVE CM-AH-ORIG-TERM        TO  HATERMO
01314      MOVE WS-AH-KIND             TO  HAEDESCO.
           EVALUATE TRUE
              WHEN ((OPEN-LF-CLAIM)
                 OR (CLOSED-LF-CLAIM))
                 AND (WS-AH-CANCEL-DATE NOT = WS-LF-INCUR-DT)
                 MOVE ZEROS            TO HAREFNDO  WS-CALC-REFUND
                                          WS-TOT-AH-RFND
                                          PI-AH-REFUND-AMT
                 MOVE SPACES           TO PI-AH-REFUND-METH
100518           IF OTHER-CLAIM
100518              MOVE ER-2998          TO EMI-ERROR
100518           ELSE
                    MOVE ER-2965          TO EMI-ERROR
100518           END-IF
                 PERFORM 9700-ERROR-FORMAT
                                       THRU  9799-EXIT
                 GO TO 0680-CONTINUE
              WHEN (CM-LF-BENEFIT-CD NOT = '00')
                 AND ((OPEN-LF-CLAIM) OR (CLOSED-LF-CLAIM))
100518           AND ((EMI-ERROR = ER-2965) OR (EMI-ERROR = ER-2998))
                 AND (WS-LF-CANCEL-DATE NOT = WS-LF-INCUR-DT)
                 MOVE ZEROS            TO HAREFNDO
                                          WS-TOT-AH-RFND
                                          PI-AH-REFUND-AMT
                 MOVE SPACES           TO PI-AH-REFUND-METH
                 GO TO 0680-CONTINUE
              WHEN (OPEN-AH-CLAIM)
                 AND (WS-ST-REF-IND = '1')
                 MOVE ZEROS            TO HAREFNDO
                                          WS-TOT-AH-RFND
                                          PI-AH-REFUND-AMT
                 MOVE ER-3039          TO EMI-ERROR
                 PERFORM 9700-ERROR-FORMAT
                                       THRU  9799-EXIT
                 GO TO 0680-CONTINUE
              WHEN (OPEN-AH-CLAIM)
                 AND (WS-ST-REF-IND = '2' OR '3' OR '4')
                 MOVE ER-3037          TO EMI-ERROR
                 PERFORM 9700-ERROR-FORMAT
                                       THRU  9799-EXIT
      *          CONTINUE
              WHEN (OPEN-AH-CLAIM)
                 AND (WS-ST-REF-IND = '5')
                 MOVE ER-3038          TO EMI-ERROR
                 PERFORM 9700-ERROR-FORMAT
                                       THRU  9799-EXIT
      *          CONTINUE
              WHEN (OPEN-AH-CLAIM)
                 AND (WS-ST-REF-IND = ' ')
                 MOVE ER-2768          TO EMI-ERROR
                 PERFORM 9700-ERROR-FORMAT
                                       THRU  9799-EXIT
              WHEN (CLOSED-AH-CLAIM)
                 AND (WS-AH-CANCEL-DATE <= WS-AH-PAID-THRU-DT)
                 MOVE ZEROS            TO HAREFNDO
                                          WS-TOT-AH-RFND
                                          PI-AH-REFUND-AMT
                 MOVE ER-2756          TO EMI-ERROR
                 PERFORM 9700-ERROR-FORMAT
                                       THRU  9799-EXIT
                 IF WS-TOT-LF-RFND > 0
                    MOVE ZEROS         TO HLREFNDO
                                          WS-TOT-LF-RFND
                                          PI-LF-REFUND-AMT
                    MOVE SPACES        TO PI-LF-REFUND-METH
                 END-IF
                 GO TO 0680-CONTINUE
      *       WHEN OTHER
      *          CONTINUE
           END-EVALUATE
           .
01307  0660-GET-REM-TERM.
01308      MOVE WS-CF-AH-OVERRIDE-L1   TO  CP-AH-OVERRIDE-CODE.
01309      MOVE CM-CERT-EFF-DT         TO  CP-CERT-EFF-DT.
01310      MOVE CM-LOAN-1ST-PMT-DT     TO  CP-FIRST-PAY-DATE.
01311      MOVE WS-AH-CANCEL-DATE      TO  CP-VALUATION-DT.
01312      MOVE WS-AH-CANCEL-DATE-ED   TO  HACANCO.
01313 *****MOVE WS-BENEFIT-DESCRIP     TO  HADESCO.
01314      MOVE WS-AH-KIND             TO  HAEDESCO.
01315      MOVE CM-STATE               TO  CP-STATE.
01316      MOVE WS-STATE-ABBREVIATION  TO  CP-STATE-STD-ABBRV.
01317      MOVE 'A'                    TO  CP-BENEFIT-TYPE.
01318      MOVE WS-AH-SPECIAL-CALC-CD  TO  CP-SPECIAL-CALC-CD.
01319      MOVE '2'                    TO  CP-PROCESS-TYPE.
01320      MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.
01321      MOVE CM-COMPANY-CD          TO  CP-COMPANY-CD.
01322      MOVE WS-ACCT-USER-FLD-5     TO  CP-ACCT-FLD-5.
01323      MOVE PI-REM-TRM-CALC-OPTION TO CP-REM-TRM-CALC-OPTION.
01324      MOVE WS-CF-CR-REM-TERM-CALC
01325                                  TO  CP-REM-TERM-METHOD.
01326
01327      IF WS-AH-CO-REM-TERM-CALC  IS GREATER THAN  '0'
01328          MOVE WS-AH-CO-REM-TERM-CALC
01329                                  TO  CP-REM-TERM-METHOD.
01330
01331      IF WS-AH-ST-REM-TERM-CALC  IS GREATER THAN  '0'
01332          MOVE WS-AH-ST-REM-TERM-CALC
01333                                  TO  CP-REM-TERM-METHOD.
01334
01335      IF WS-AH-AM-REM-TERM-CALC  IS GREATER THAN  '0'
01336          MOVE WS-AH-AM-REM-TERM-CALC
01337                                  TO  CP-REM-TERM-METHOD.
083005     IF (PI-COMPANY-ID = 'CID')
022608        AND (CP-STATE-STD-ABBRV = 'WI')
083005        MOVE '7'                 TO CP-REM-TERM-METHOD
022608        MOVE '5'                 TO CP-REM-TRM-CALC-OPTION
083005     END-IF
           IF (PI-COMPANY-ID = 'CID')
              AND (CP-STATE-STD-ABBRV = 'MO')
              AND (CM-CERT-EFF-DT >= X'9B41')
              AND (CM-CERT-EFF-DT <= X'A2FB')
              MOVE '7'                 TO CP-REM-TERM-METHOD
              MOVE '5'                 TO CP-REM-TRM-CALC-OPTION
           END-IF
01338
01339      MOVE CM-AH-ORIG-TERM        TO  HATERMO
01340                                      CP-ORIGINAL-TERM
01341                                      CP-LOAN-TERM.
01342
01343      IF  NOT  CP-TERM-IS-DAYS
01344          IF CM-PMT-EXTENSION-DAYS  IS NUMERIC
01345              MOVE CM-PMT-EXTENSION-DAYS
01346                                  TO  CP-TERM-OR-EXT-DAYS
01347          ELSE
01348              MOVE ZEROS          TO  CP-TERM-OR-EXT-DAYS.
01349
01350 *** READ STATE MASTER RECORD FOR FREE LOOK PERIOD ***
01351      MOVE PI-COMPANY-ID          TO  WS-CF-COMPANY-ID.
01352      MOVE '3'                    TO  WS-CF-RECORD-TYPE.
01353      MOVE CM-STATE               TO  WS-CF-ACCESS.
01354      MOVE +0                     TO  WS-CF-SEQUENCE-NO.
01355
01356      
      * EXEC CICS READ
01357 *        DATASET  (ELCNTL-ID)
01358 *        SET      (ADDRESS OF CONTROL-FILE)
01359 *        RIDFLD   (WS-CF-CONTROL-PRIMARY)
01360 *        RESP     (WS-RESPONSE)
01361 *    END-EXEC.
      *    MOVE '&"S        E          (  N#00007733' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303037373333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01362
01363      IF WS-RESP-NOTOPEN
01364          MOVE ER-2617            TO  EMI-ERROR
01365          GO TO 0320-INPUT-ERROR.
01366
01367      IF WS-RESP-NOTFND
01368          MOVE ZERO               TO CP-FREE-LOOK
01369      ELSE
01370          MOVE CF-ST-FREE-LOOK-PERIOD
01371                                  TO CP-FREE-LOOK.
01372
01373      PERFORM 0700-LINK-REM-TERM  THRU  0700-EXIT.
01374
01375      MOVE CP-REMAINING-TERM-1    TO  HAREMO.
01376
01377      IF CM-AH-CURRENT-STATUS  IS EQUAL TO  '8'
01378          IF CM-AH-CANCEL-DT  IS NOT EQUAL TO  LOW-VALUES
01379              MOVE ER-0683        TO  EMI-ERROR
01380              PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
01381
01382      IF CM-AH-CURRENT-STATUS  IS EQUAL TO  '6'  OR  '7'
01383          IF CM-AH-SETTLEMENT-DT  IS NOT EQUAL TO  LOW-VALUES
01384              MOVE ER-0684        TO  EMI-ERROR
01385              PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
01386
01387  0670-CONTINUE-AH.
01388      MOVE CM-AH-PREMIUM-AMT      TO  HAPREMO
01389                                      WS-TOT-AH-PREM
01390      MOVE CP-REMAINING-TERM-1    TO  HAREMO.
01391
01392      IF CP-REMAINING-TERM-1  IS GREATER THAN  CM-AH-ORIG-TERM
01393          MOVE CM-AH-ORIG-TERM    TO  HAREMO.
01394
01261      IF PI-AH-CANCEL-DATE = LOW-VALUES
01262         GO TO 0680-CONTINUE.
01395      MOVE CP-REMAINING-TERM-1    TO  CP-REMAINING-TERM.
01396      MOVE WS-AH-CO-REFUND-CALC   TO  CP-EARNING-METHOD.
01397      MOVE WS-AH-CO-EARNINGS-CALC TO  CP-RATING-METHOD.
01398
01399      IF WS-AH-ST-REFUND-CALC  IS GREATER THAN  ZERO
01400          MOVE WS-AH-ST-REFUND-CALC
01401                                  TO  CP-EARNING-METHOD.
01402
01403      IF WS-AH-FO-REFUND-CALC  IS GREATER THAN  ZERO
01404          MOVE WS-AH-FO-REFUND-CALC
01405                                  TO  CP-EARNING-METHOD.
01406
PEMMOD*    IF WS-AM-EARN-METHOD-A  IS GREATER THAN  ZERO
PEMMOD*        MOVE WS-AM-EARN-METHOD-A
PEMMOD*                                TO  CP-EARNING-METHOD.
01410
032912     if pi-company-id = 'AHL'
032912        MOVE CM-AH-CLASS-CD      TO CP-CLASS-CODE
032912        if cp-class-code = spaces
032912           move zeros            to cp-class-code
032912        end-if
032912     ELSE
CIDMOD        MOVE CM-RATE-CLASS       TO  CP-CLASS-CODE
032912     END-IF
01412      MOVE CM-AH-BENEFIT-CD       TO  CP-BENEFIT-CD.
01413      MOVE CM-AH-BENEFIT-AMT      TO  CP-ORIGINAL-BENEFIT
01414                                      CP-RATING-BENEFIT-AMT.
01415      IF CP-STATE-STD-ABBRV = 'OR'
01416          COMPUTE CP-RATING-BENEFIT-AMT = CM-AH-BENEFIT-AMT *
01417                                          CM-AH-ORIG-TERM.
01418      MOVE CM-AH-PREMIUM-AMT      TO  CP-ORIGINAL-PREMIUM.
01419      MOVE CM-PAY-FREQUENCY       TO  CP-PAY-FREQUENCY.
01420      MOVE CM-INSURED-ISSUE-AGE   TO  CP-ISSUE-AGE.
01421      MOVE CM-AH-DEV-CODE         TO  CP-DEVIATION-CODE.
01422      MOVE CM-AH-DEV-PCT          TO  CP-RATE-DEV-PCT.
01423      MOVE CM-LOAN-APR            TO  CP-LOAN-APR.
01424
01425      MOVE WS-CF-CR-R78-METHOD    TO  CP-R78-OPTION.
01426      MOVE CM-CERT-EFF-DT         TO  DC-BIN-DATE-1.
01427      MOVE ' '                    TO  DC-OPTION-CODE.
01428
01429      PERFORM 9600-DATE-LINK.
01430
01431      IF CP-STATE-STD-ABBRV = 'OH'
CIDMOD        IF PI-COMPANY-ID NOT EQUAL 'NCL' AND 'CID'
01433          IF (CP-ORIGINAL-TERM  IS GREATER THAN  60)
01434            AND (DC-GREG-DATE-1-YMD  IS GREATER THAN  '831101')
01435            AND (CM-LF-BENEFIT-CD  IS NOT EQUAL TO  ZERO)
CIDMOD             MOVE '6'            TO  CP-EARNING-METHOD
CIDMOD         END-IF
CIDMOD        END-IF
CIDMOD        IF PI-COMPANY-ID = 'CID'
CIDMOD           IF CM-LF-BENEFIT-CD = (SPACES OR ZEROS OR
CIDMOD                           LOW-VALUES)
CIDMOD              IF CP-CRITICAL-PERIOD
CIDMOD                 MOVE '2'       TO CP-EARNING-METHOD
CIDMOD              ELSE
CIDMOD                 MOVE '6'       TO CP-EARNING-METHOD
CIDMOD              END-IF
CIDMOD           ELSE
CIDMOD              IF WS-CF-LF-COVERAGE-TYPE = 'L'
CIDMOD                 MOVE '2'       TO CP-EARNING-METHOD
CIDMOD              ELSE
CIDMOD                 IF ((CM-LF-ORIG-TERM > 60) AND
CIDMOD                    (CM-RATE-CLASS NOT = 'L '))
CIDMOD                               OR
CIDMOD                    (WS-LF-CO-EARNINGS-CALC = '5')
CIDMOD                    IF CP-CRITICAL-PERIOD
CIDMOD                       MOVE '2'  TO CP-EARNING-METHOD
CIDMOD                    ELSE
CIDMOD                       MOVE '6'  TO CP-EARNING-METHOD
CIDMOD                    END-IF
CIDMOD                 ELSE
CIDMOD                    MOVE '1'     TO CP-EARNING-METHOD
CIDMOD                 END-IF
CIDMOD              END-IF
CIDMOD           END-IF
CIDMOD        END-IF
CIDMOD     END-IF
01437
01438      IF CP-STATE-STD-ABBRV = 'VA'
01439        IF PI-COMPANY-ID NOT EQUAL 'NCL' AND 'CID'
01440          IF DC-GREG-DATE-1-YMD  IS GREATER THAN  '921231'
01441             IF CP-ORIGINAL-TERM  IS GREATER THAN  61
01442                 MOVE '6'            TO  CP-EARNING-METHOD
01443             ELSE
01444                 MOVE '1'            TO  CP-EARNING-METHOD.
01445
01446      IF CP-EARN-AS-NET-PAY
01447          IF CP-LOAN-APR NOT GREATER THAN ZEROS
01448             IF WS-CF-DEFAULT-APR GREATER THAN ZEROS
01449                 MOVE WS-CF-DEFAULT-APR
01450                                  TO  CP-LOAN-APR
01451                 MOVE ER-3780     TO  EMI-ERROR
01452                 PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
01453             ELSE
01454                 MOVE ER-3781     TO  EMI-ERROR
01455                 PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
           IF PI-COMPANY-ID = 'DCC'
092705        IF (WS-AH-BEN-CATEGORY = 'G' OR 'L')
                 AND (CP-EARNING-METHOD NOT = 'G' AND 'D')
                 MOVE 'S'              TO CP-EARNING-METHOD
              END-IF
           END-IF
           MOVE PI-CANCEL-REASON       TO CP-CANCEL-REASON
           IF (PI-COMPANY-ID = 'DCC')
              AND (CP-EARNING-METHOD = 'D')
              MOVE +0                  TO WS-DDF-ADMIN-FEES
                                          WS-DDF-CSO-ADMIN-FEE
                                          WS-DDF-1ST-YR-TOT-EXP
                                          WS-DDF-COMM-AND-MFEE
              PERFORM VARYING S1 FROM +2 BY +1 UNTIL
                 S1 > +10
                 IF AM-COM-TYP (S1) = 'L' OR 'N' OR 'J' OR 'I'
                    IF (AM-A-COM (S1) NUMERIC)
                       AND (AM-A-COMA (S1) (3:1) NOT = 'L' AND 'M')
                       COMPUTE WS-COMM-PCT = (AM-A-COM (S1) * +1000)
                    ELSE
                       MOVE +0         TO WS-COMM-PCT C0
                       PERFORM 0740-GET-ERCTBL THRU 0740-EXIT
                       COMPUTE WS-COMM-PCT = WS-COMM-PCT * +1000
                    END-IF
                    IF AM-COM-TYP (S1) = 'L' OR 'N'
                       COMPUTE WS-DDF-ADMIN-FEES = WS-DDF-ADMIN-FEES
                          + WS-COMM-PCT
                    END-IF
                    IF AM-COM-TYP (S1) = 'N'
                       COMPUTE WS-DDF-CSO-ADMIN-FEE =
                          WS-DDF-CSO-ADMIN-FEE + WS-COMM-PCT
                    END-IF
                    IF AM-COM-TYP (S1) = 'J' OR 'L'
                       COMPUTE WS-DDF-1ST-YR-TOT-EXP
                          = WS-DDF-1ST-YR-TOT-EXP + WS-COMM-PCT
                    END-IF
                    IF AM-COM-TYP (S1) = 'I'
                       COMPUTE WS-DDF-COMM-AND-MFEE
                          = WS-DDF-COMM-AND-MFEE + WS-COMM-PCT
                    END-IF
                 END-IF
              END-PERFORM
              MOVE WS-DDF-CSO-ADMIN-FEE TO CP-DDF-CSO-ADMIN-FEE
              MOVE WS-DDF-ADMIN-FEES   TO CP-DDF-ADMIN-FEES
              COMPUTE WS-DDF-COMM-AND-MFEE = WS-DDF-COMM-AND-MFEE +
                 (CM-AH-PREMIUM-AMT - CM-AH-CLP - CM-ADDL-CLP)
           END-IF
           IF (PI-COMPANY-ID = 'DCC')
              AND (CP-EARNING-METHOD = 'D')
              AND (PI-CANCEL-REASON NOT = 'R')
              PERFORM 0730-GET-DDF-FACTORS
                                       THRU 0730-EXIT
              IF NOT PDEF-FOUND
                 MOVE ER-9999          TO EMI-ERROR
                 PERFORM 9700-ERROR-FORMAT
                                       THRU 9799-EXIT
                 GO TO 8200-SEND-DATAONLY
              END-IF
              MOVE PD-UEP-FACTOR (P1 P2 + 1)
                                       TO CP-DDF-LO-FACT
              MOVE PD-UEP-FACTOR (P1 P2)
                                       TO CP-DDF-HI-FACT
              MOVE WS-DDF-COMM-AND-MFEE TO CP-DDF-COMM-AND-MFEE
              MOVE CM-AH-CLP           TO CP-DDF-CLP
              MOVE PD-1ST-YR-ADMIN-ALLOW TO CP-DDF-YR1AF
              COMPUTE CP-1ST-YR-ALLOW = WS-DDF-1ST-YR-TOT-EXP
                 + PD-1ST-YR-ADMIN-ALLOW
              MOVE 'G'                 TO CP-DDF-SPEC-CALC
              IF PI-CLP-YN = 'Y'
PEMTST           MOVE 'C'              TO CP-DDF-SPEC-CALC
PEMTST           MOVE CM-AH-CLP        TO CP-ORIGINAL-PREMIUM
                                          CP-DDF-CLP
PEMTST           MOVE ZEROS            TO CP-1ST-YR-ALLOW
              END-IF
              IF DD-IU-PRESENT
                 MOVE 'I'              TO CP-EARNING-METHOD
              END-IF
              MOVE CM-DDF-IU-RATE-UP   TO CP-IU-RATE-UP
                                          CP-CLP-RATE-UP
              IF (CP-CALC-GROSS-FEE)
                 AND (CP-IU-RATE-UP NOT = ZEROS)
                 COMPUTE TEX-FACT-8 = 1 - ((CM-ADDL-CLP + CM-AH-CLP)
                    / CP-ORIGINAL-PREMIUM)
                 COMPUTE CP-IU-RATE-UP ROUNDED = CP-IU-RATE-UP
                    / (1 - TEX-FACT-8)
              END-IF
           END-IF
020816     if pi-company-id = 'VPP'
020816        IF PI-CLP-YN = 'Y'
020816           MOVE CM-AH-CLP        TO CP-ORIGINAL-PREMIUM
020816                                    hapremo
020816                                    CP-DDF-CLP
020816        END-IF
020816     end-if
01457      PERFORM 0800-LINK-REFUND  THRU  0899-EXIT
01458
01459      MOVE ' '                    TO  WS-REFUND-SEARCH-SW.
01460
01461      PERFORM 1400-GET-REFUND-TYPE  THRU  1499-EXIT.
01462
01463      IF CP-ERROR-RATE-IS-ZERO
01464        OR CP-ERROR-RATE-NOT-FOUND
01465          MOVE ER-2740            TO  EMI-ERROR
01466          PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
01467
01468      IF CP-ERROR-RATE-FILE-NOTOPEN
01469          MOVE ER-2617            TO  EMI-ERROR
01470          PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
01471
01472      MOVE ZEROS                  TO  WS-CALC-REFUND.
01473      MOVE CP-CALC-REFUND         TO  HAREFNDO  WS-CALC-REFUND
01474                                      WS-TOT-AH-RFND
01475                                      PI-AH-REFUND-AMT.
032117     move cm-ah-comm-pct         to hacpcto
032117     compute hacamto rounded =
032117        cp-calc-refund * cm-ah-comm-pct
020816     if pi-company-id = 'VPP'
020816        if cp-calc-refund < cm-cancel-fee
020816           move cp-calc-refund   to cm-cancel-fee
020816        end-if
020816        if cp-calc-refund = cm-ah-premium-amt
020816           move zeros            to cm-cancel-fee
020816        end-if
020816        move cm-cancel-fee       to canfeeo
020816        compute refdueo =
020816           cp-calc-refund - cm-cancel-fee
020816     end-if
01476
01477      MOVE CP-REFUND-TYPE-USED    TO  PI-AH-REFUND-METH.
01478
01479      MOVE AL-UANON               TO  HACANCA.
01480
01481  0680-CONTINUE.
01482      IF BROWSE-STARTED
01483          PERFORM 1500-END-BROWSE  THRU  1599-EXIT
01484          MOVE SPACES             TO  WS-BROWSE-STARTED-SW.
01485
01486      IF CM-LF-BENEFIT-CD = '00'
01487          MOVE -1                 TO  HACANCL
01488      ELSE
01489          MOVE -1                 TO  HLCANCL.
01490
01491      ADD WS-TOT-LF-PREM    WS-TOT-AH-PREM GIVING
01492                            WS-TOT-PREM.
01493      ADD WS-TOT-LF-RFND    WS-TOT-AH-RFND GIVING
01494                            WS-TOT-RFND.
01495
01496      MOVE WS-TOT-PREM            TO  TOPREMO.
01497      MOVE WS-TOT-RFND            TO  TORFNDO.
01498
01499  0680-EXIT.
01500      EXIT.
01501  EJECT
01502  0700-LINK-REM-TERM SECTION.
01503      
      * EXEC CICS LINK
01504 *        PROGRAM   (ELRTRM-ID)
01505 *        COMMAREA  (CALCULATION-PASS-AREA)
01506 *        LENGTH    (CP-COMM-LENGTH)
01507 *    END-EXEC.
      *    MOVE '."C                   (   #00008026' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038303236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELRTRM-ID, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01508
01509  0700-EXIT.
01510      EXIT.
01511  EJECT
01512  0710-LINK-REM-AMOUNT SECTION.
01513      
      * EXEC CICS LINK
01514 *        PROGRAM   (ELRAMT-ID)
01515 *        COMMAREA  (CALCULATION-PASS-AREA)
01516 *        LENGTH    (CP-COMM-LENGTH)
01517 *    END-EXEC.
      *    MOVE '."C                   (   #00008036' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038303336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELRAMT-ID, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01518
01519  0710-EXIT.
01520      EXIT.
       0730-GET-DDF-FACTORS.
           MOVE ' '                    TO WS-PDEF-RECORD-SW
           MOVE PI-COMPANY-CD          TO ERPDEF-KEY
           MOVE CM-STATE               TO ERPDEF-STATE
010816     if cm-clp-state <> cm-state and spaces
010816                  and low-values and zeros
010816        move cm-clp-state        to erpdef-state
010816     end-if
           MOVE AM-DCC-PRODUCT-CODE    TO ERPDEF-PROD-CD
           MOVE 'A'                    TO ERPDEF-BEN-TYPE
           MOVE CM-AH-BENEFIT-CD       TO ERPDEF-BEN-CODE
           MOVE CM-CERT-EFF-DT         TO ERPDEF-EXP-DT
           MOVE ERPDEF-KEY             TO ERPDEF-KEY-SAVE
           
      * EXEC CICS STARTBR
      *        DATASET  ('ERPDEF')
      *        RIDFLD   (ERPDEF-KEY)
      *        GTEQ
      *        RESP     (WS-RESPONSE)
      *    END-EXEC
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00008057' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303038303537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF NOT WS-RESP-NORMAL
              GO TO 0730-EXIT
           END-IF
           .
       0730-READNEXT.
           
      * EXEC CICS READNEXT
      *       DATASET  ('ERPDEF')
      *       SET      (ADDRESS OF PRODUCT-MASTER)
      *       RIDFLD   (ERPDEF-KEY)
      *       RESP     (WS-RESPONSE)
      *    END-EXEC
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )  N#00008068' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303038303638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCT-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF NOT WS-RESP-NORMAL
              GO TO 0730-ENDBR
           END-IF
           IF (ERPDEF-KEY-SAVE (1:16) = PD-CONTROL-PRIMARY (1:16))
              IF (CM-CERT-EFF-DT < PD-PROD-EXP-DT)
                 MOVE 'Y'              TO WS-PDEF-RECORD-SW
              ELSE
                 GO TO 0730-READNEXT
              END-IF
           ELSE
              GO TO 0730-ENDBR
           END-IF
           PERFORM VARYING P1 FROM +1 BY +1 UNTIL
              (P1 > +8)
              OR (PD-PROD-CODE (P1) = 'I')
           END-PERFORM
           IF P1 < +9
              SET DD-IU-PRESENT        TO TRUE
           END-IF
           IF CM-LOAN-TERM = ZEROS
              MOVE CP-ORIGINAL-TERM    TO CP-LOAN-TERM
           END-IF
           IF PD-TRUNCATED = 'Y'
              MOVE CM-LOAN-TERM        TO WS-TERM
           ELSE
              MOVE CP-ORIGINAL-TERM    TO WS-TERM
           END-IF
           EVALUATE TRUE
              WHEN WS-TERM > +168
                 MOVE 15               TO P1
              WHEN WS-TERM > +156
                 MOVE 14               TO P1
              WHEN WS-TERM > +144
                 MOVE 13               TO P1
              WHEN WS-TERM > +132
                 MOVE 12               TO P1
              WHEN WS-TERM > +120
                 MOVE 11               TO P1
              WHEN WS-TERM > +108
                 MOVE 10               TO P1
              WHEN WS-TERM > +96
                 MOVE 9                TO P1
              WHEN WS-TERM > +84
                 MOVE 8                TO P1
              WHEN WS-TERM > +72
                 MOVE 7                TO P1
              WHEN WS-TERM > +60
                 MOVE 6                TO P1
              WHEN WS-TERM > +48
                 MOVE 5                TO P1
              WHEN WS-TERM > +36
                 MOVE 4                TO P1
              WHEN WS-TERM > +24
                 MOVE 3                TO P1
              WHEN WS-TERM > +12
                 MOVE 2                TO P1
              WHEN OTHER
                 MOVE 1                TO P1
           END-EVALUATE
           EVALUATE TRUE
      *       WHEN ((CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +13)
      *          AND (DD-IU-PRESENT)
      *          MOVE 2                TO P2
      *       WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +13
      *          MOVE 1                TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +25
                 MOVE 2                TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +37
                 MOVE 3                TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +49
                 MOVE 4                TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +61
                 MOVE 5                TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +73
                 MOVE 6                TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +85
                 MOVE 7                TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +97
                 MOVE 8                TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +109
                 MOVE 9                TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +121
                 MOVE 10               TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +133
                 MOVE 11               TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +145
                 MOVE 12               TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +157
                 MOVE 13               TO P2
              WHEN (CP-ORIGINAL-TERM - CP-REMAINING-TERM) < +169
                 MOVE 14               TO P2
              WHEN OTHER
                 MOVE 15               TO P2
           END-EVALUATE
           .
       0730-ENDBR.
           
      * EXEC CICS ENDBR
      *       DATASET  ('ERPDEF')
      *    END-EXEC
           MOVE 'ERPDEF' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00008170' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038313730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0730-EXIT.
           EXIT.
       0740-GET-ERCTBL.
           MOVE AM-COMPANY-CD          TO CTBL-COMPANY-CD
           MOVE AM-A-COMA (S1)         TO CTBL-TABLE
           MOVE 'A'                    TO CTBL-BEN-TYPE
           MOVE CM-AH-BENEFIT-CD       TO CTBL-BEN-CODE
           MOVE CTBL-KEY               TO CTBL-KEY-SAVE
           PERFORM 0750-READ-ERCTBL    THRU 0750-EXIT
           IF WS-RESP-NORMAL
              PERFORM 0760-FIND-COMM   THRU 0760-EXIT
           ELSE
              MOVE AM-COMPANY-CD          TO CTBL-COMPANY-CD
              MOVE AM-A-COMA (S1)         TO CTBL-TABLE
              MOVE 'A'                    TO CTBL-BEN-TYPE
              MOVE 'AA'                   TO CTBL-BEN-CODE
              MOVE CTBL-KEY               TO CTBL-KEY-SAVE
              PERFORM 0750-READ-ERCTBL    THRU 0750-EXIT
              IF WS-RESP-NORMAL
                 PERFORM 0760-FIND-COMM   THRU 0760-EXIT
              END-IF
           END-IF
           .
       0740-EXIT.
           EXIT.
       0750-READ-ERCTBL.
           
      * EXEC CICS READ
      *         INTO    (COMM-TABLE-RECORD)
      *         DATASET ('ERCTBL')
      *         RIDFLD  (CTBL-KEY)
      *         RESP    (WS-RESPONSE)
      *    END-EXEC
           MOVE LENGTH OF
            COMM-TABLE-RECORD
             TO DFHEIV11
           MOVE 'ERCTBL' TO DFHEIV1
      *    MOVE '&"IL       E          (  N#00008200' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303038323030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 COMM-TABLE-RECORD, 
                 DFHEIV11, 
                 CTBL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
       0750-EXIT.
           EXIT.
       0760-FIND-COMM.
           PERFORM VARYING C1 FROM +1 BY +1 UNTIL
              ((CM-AH-BENEFIT-AMT * CM-AH-ORIG-TERM) <= CT-TBF (C1))
              OR (C1 > +3)
           END-PERFORM
           PERFORM VARYING C2 FROM +1 BY +1 UNTIL
              (CM-INSURED-ISSUE-AGE <= CT-AGE (C2))
              OR (C2 > +3)
           END-PERFORM
           PERFORM VARYING C3 FROM +1 BY +1 UNTIL
              (CM-AH-ORIG-TERM <= CT-TRM (C3))
              OR (C3 > +3)
           END-PERFORM
           IF C1 > +3
              MOVE +1                  TO C1
           END-IF
           IF C2 > +3
              MOVE +1                  TO C2
           END-IF
           IF C3 > +3
              MOVE +1                  TO C3
           END-IF
           IF C1 = +3
              MOVE +18                 TO C0
           ELSE
              IF C1 = +2
                 MOVE +9               TO C0
              END-IF
           END-IF
           IF C2 = +3
              ADD +6                   TO C0
           ELSE
              IF C2 = +2
                 ADD +3                TO C0
              END-IF
           END-IF
           ADD C3                      TO C0
           MOVE CT-RT (C0)             TO WS-COMM-PCT
           .
       0760-EXIT.
           EXIT.
01522  0800-LINK-REFUND SECTION.
01523      IF CP-AH
01524        IF PI-EARNING-METHOD-AH NOT = SPACES
01525          MOVE PI-EARNING-METHOD-AH   TO CP-EARNING-METHOD
01526         ELSE
01527          NEXT SENTENCE
01528       ELSE
01529        IF PI-EARNING-METHOD-LF NOT = SPACES
01530          MOVE PI-EARNING-METHOD-LF   TO CP-EARNING-METHOD.
01531
01532      
      * EXEC CICS LINK
01533 *        PROGRAM   (ELRFND-ID)
01534 *        COMMAREA  (CALCULATION-PASS-AREA)
01535 *        LENGTH    (CP-COMM-LENGTH)
01536 *    END-EXEC.
      *    MOVE '."C                   (   #00008260' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038323630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELRFND-ID, 
                 CALCULATION-PASS-AREA, 
                 CP-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01537
01538  0899-EXIT.
01539      EXIT.
01540  EJECT
01541  0900-READ-CERT-FILE  SECTION.
01542      
      * EXEC CICS HANDLE CONDITION
01543 *        NOTFND  (0910-NOT-FOUND)
01544 *    END-EXEC.
      *    MOVE '"$I                   ! # #00008270' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303038323730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01545
01546      
      * EXEC CICS READ
01547 *        DATASET  (ELCERT-ID)
01548 *        RIDFLD   (WS-CM-CONTROL-PRIMARY)
01549 *        SET      (ADDRESS OF CERTIFICATE-MASTER)
01550 *    END-EXEC.
      *    MOVE '&"S        E          (   #00008274' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038323734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01551
01552 *    SERVICE RELOAD CERTIFICATE-MASTER.
01553
01554      GO TO 0999-EXIT.
01555
01556  0910-NOT-FOUND.
01557      MOVE 'N'                    TO  WS-CERT-RECORD-SW.
01558
01559  0999-EXIT.
01560      EXIT.
01561  EJECT
01562  1000-READ-CONTROL  SECTION.
01563      
      * EXEC CICS HANDLE CONDITION
01564 *        NOTFND   (1010-NOTFND)
01565 *        NOTOPEN  (1020-NOTOPEN)
01566 *    END-EXEC.
      *    MOVE '"$IJ                  ! $ #00008291' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303038323931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01567
01568      
      * EXEC CICS READ
01569 *        DATASET  (ELCNTL-ID)
01570 *        SET      (ADDRESS OF CONTROL-FILE)
01571 *        RIDFLD   (WS-CF-CONTROL-PRIMARY)
01572 *        GTEQ
01573 *    END-EXEC.
      *    MOVE '&"S        G          (   #00008296' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038323936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01574
01575 *    SERVICE RELOAD CONTROL-FILE.
01576
01577      GO TO 1099-EXIT.
01578
01579  1010-NOTFND.
01580      MOVE 'N'                    TO  WS-CNTL-RECORD-SW.
01581
01582      GO TO 1099-EXIT.
01583
01584  1020-NOTOPEN.
01585      MOVE ER-2617                TO  EMI-ERROR.
01586
01587      GO TO 0320-INPUT-ERROR.
01588
01589  1099-EXIT.
01590      EXIT.
01591  EJECT
01592  1100-FIND-BENEFIT-IN-STATE  SECTION.
01593      MOVE 'N'                    TO  BEN-SEARCH-SW.
01594
01595      PERFORM 1110-BENEFIT-DUMMY  THRU  1119-EXIT
01596          VARYING  SUB3  FROM  1  BY  1
01597              UNTIL  ((SUB3  IS GREATER THAN  50)
01598                OR ((CF-ST-BENEFIT-CD (SUB3)
01599                       IS EQUAL TO  WS-BEN-CD)
01600                AND (WS-LOOKUP-TYPE
01601                       IS EQUAL TO  CF-ST-BENEFIT-KIND (SUB3)))).
01602
01603      IF SUB3  IS NOT EQUAL TO  51
01604          MOVE 'Y'                TO  BEN-SEARCH-SW.
01605
01606      GO TO 1199-EXIT.
01607
01608  1110-BENEFIT-DUMMY.
01609
01610  1119-EXIT.
01611      EXIT.
01612
01613  1199-EXIT.
01614      EXIT.
01615  EJECT
01616  1200-START-ACCOUNT-MASTER  SECTION.
01617      
      * EXEC CICS HANDLE CONDITION
01618 *        NOTFND   (1290-ACCT-NOT-FOUND)
01619 *        ENDFILE  (1290-ACCT-NOT-FOUND)
01620 *    END-EXEC.
      *    MOVE '"$I''                  ! % #00008345' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303038333435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01621
01622      MOVE ' '                    TO  WS-ACCT-RECORD-SW.
01623
01624      
      * EXEC CICS STARTBR
01625 *        DATASET  (ERACCT-ID)
01626 *        RIDFLD   (WS-AM-CONTROL-PRIMARY)
01627 *        GTEQ
01628 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00008352' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303038333532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-ID, 
                 WS-AM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01629
01630      MOVE 'Y'                    TO  WS-BROWSE-STARTED-SW.
01631      MOVE 'Y'                    TO  WS-ACCT-RECORD-SW.
01632
01633      GO TO 1299-EXIT.
01634
01635  1290-ACCT-NOT-FOUND.
01636      MOVE ' '                    TO  WS-ACCT-RECORD-SW.
01637
01638  1299-EXIT.
01639      EXIT.
01640  EJECT
01641  1300-READ-ACCOUNT-MASTER  SECTION.
01642      
      * EXEC CICS HANDLE CONDITION
01643 *        NOTFND   (0580-ACCT-NOT-FOUND)
01644 *        ENDFILE  (0580-ACCT-NOT-FOUND)
01645 *    END-EXEC.
      *    MOVE '"$I''                  ! & #00008370' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303038333730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01646
01647      
      * EXEC CICS READNEXT
01648 *        DATASET  (ERACCT-ID)
01649 *        SET      (ADDRESS OF ACCOUNT-MASTER)
01650 *        RIDFLD   (WS-AM-CONTROL-PRIMARY)
01651 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00008375' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303038333735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-AM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01652
01653 *    SERVICE RELOAD ACCOUNT-MASTER.
01654
01655  1399-EXIT.
01656      EXIT.
01657  EJECT
01658  1400-GET-REFUND-TYPE  SECTION.
01659      IF LIFE-REFUND-SEARCH
01660          NEXT SENTENCE
01661      ELSE
01662          GO TO 1410-AH-REFUND-TYPE.
01663
01664      IF CP-REFUND-TYPE-USED  IS EQUAL TO  SPACE
01665          MOVE SPACES             TO  HLCALCO
01666                                      HLCAL1O
01667          GO TO 1499-EXIT.
01668
01669      MOVE CP-REFUND-TYPE-USED    TO  HLCAL1O.
01670
01671      EVALUATE TRUE
              WHEN CP-R-AS-REPOSSESSION
                 MOVE 'REPOSSESSION'     TO  HLCALCO
01672         WHEN CP-R-AS-R78
01673            MOVE 'RULE 78'          TO  HLCALCO
01674         WHEN  CP-R-AS-PRORATA
01675            MOVE 'PRO RATA'         TO  HLCALCO
01676         WHEN CP-R-AS-CALIF
01677            MOVE 'CALIF'            TO  HLCALCO
01678         WHEN CP-R-AS-TEXAS
01679            MOVE 'IRREG'            TO  HLCALCO
01680         WHEN CP-REFUND-TYPE-USED IS EQUAL TO 'S'
01681            MOVE 'UTAH'             TO  HLCALCO
01682         WHEN CP-R-AS-FARM-PLAN
01683            MOVE 'FARM PLAN'        TO  HLCALCO
01684         WHEN CP-R-AS-NET-PAY
01685            MOVE 'NET PAY'          TO  HLCALCO
01686         WHEN CP-R-AS-ANTICIPATION
01687            MOVE 'ANTICIPATION'     TO  HLCALCO
01688         WHEN CP-R-AS-MEAN
01689            MOVE 'MEAN'             TO  HLCALCO
01690         WHEN CP-R-AS-SUM-OF-DIGITS
01691            MOVE 'SUM OF DIGIT'     TO  HLCALCO
              WHEN OTHER
                 MOVE 'UNDEFINED'        TO  HLCALCO
01692      END-EVALUATE.
01693
01694      GO TO 1499-EXIT.
01695
01696  1410-AH-REFUND-TYPE.
01697      IF CP-REFUND-TYPE-USED  IS EQUAL TO  SPACE
01698          MOVE SPACES             TO  HACALCO
01699                                      HACAL1O
01700          GO TO 1499-EXIT.
01701
01702      MOVE CP-REFUND-TYPE-USED    TO  HACAL1O.
01703
01704      EVALUATE TRUE
              WHEN CP-R-AS-REPOSSESSION
                 MOVE 'REPOSSESSION'     TO HACALCO
01705         WHEN CP-R-AS-R78
01706            MOVE 'RULE 78'          TO HACALCO
01707         WHEN CP-R-AS-PRORATA
01708            MOVE 'PRO RATA'         TO HACALCO
01709         WHEN CP-REFUND-TYPE-USED = '3'
01710            MOVE 'CALIF'            TO HACALCO
01711         WHEN CP-R-AS-TEXAS
01712            MOVE 'IRREG'            TO HACALCO
01713         WHEN CP-R-AS-FARM-PLAN
01714            MOVE 'FARM PLAN'        TO HACALCO
01715         WHEN CP-R-AS-NET-PAY
01716            MOVE 'NET PAY'          TO HACALCO
01717         WHEN CP-R-AS-ANTICIPATION
01718            MOVE 'ANTICIPATION'     TO HACALCO
01719         WHEN CP-R-AS-MEAN
01720            MOVE 'MEAN'             TO HACALCO
01721         WHEN CP-R-AS-SUM-OF-DIGITS
01722            MOVE 'SUM OF DIGIT'     TO HACALCO
              WHEN CP-GAP-ACTUARIAL
                 MOVE 'SP ACTUARIAL'     TO HACALCO
              WHEN CP-R-AS-SPP-DDF
                 MOVE 'SPEC DDF'         TO HACALCO
              WHEN CP-R-AS-SPP-DDF-IU
                 MOVE 'SPEC DDF IU'      TO HACALCO
              WHEN OTHER
                 MOVE 'UNKNOWN'          TO HLCALCO
01723      END-EVALUATE
           .
01726  1499-EXIT.
01727      EXIT.
01728  EJECT
01729  1500-END-BROWSE  SECTION.
01730      IF BROWSE-STARTED
01731          
      * EXEC CICS ENDBR
01732 *            DATASET  (ERACCT-ID)
01733 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00008472' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038343732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01734
01735  1599-EXIT.
01736      EXIT.
01737  EJECT
01738  1600-LOCATE-BENEFIT  SECTION.
01739      
      * EXEC CICS HANDLE CONDITION
01740 *        NOTFND  (1699-EXIT)
01741 *    END-EXEC.
      *    MOVE '"$I                   ! '' #00008480' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303038343830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01742
01743      MOVE SPACES                 TO  WS-KIND.
01744      MOVE ZERO                   TO  WS-NOT-FOUND.
01745      MOVE PI-COMPANY-ID          TO  WS-CF-COMPANY-ID.
01746      MOVE SPACES                 TO  WS-CF-STATE.
01747      MOVE WS-BENEFIT-NO          TO  WS-CF-BENEFIT-NO.
01748
01749      
      * EXEC CICS READ
01750 *        DATASET  (ELCNTL-ID)
01751 *        RIDFLD   (WS-CF-CONTROL-PRIMARY)
01752 *        SET      (ADDRESS OF CONTROL-FILE)
01753 *        GTEQ
01754 *    END-EXEC.
      *    MOVE '&"S        G          (   #00008490' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038343930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01755
01756 *    SERVICE RELOAD CONTROL-FILE.
01757
01758      IF WS-CF-COMPANY-ID  IS NOT EQUAL TO  CF-COMPANY-ID
01759        OR WS-CF-RECORD-TYPE  IS NOT EQUAL TO  CF-RECORD-TYPE
01760          GO TO 1699-EXIT.
01761
01762      MOVE +1                     TO  WS-INDEX.
01763
01764  1610-LOOKUP-BENEFIT.
01765      IF WS-BENEFIT-NO  IS EQUAL TO  CF-BENEFIT-CODE (WS-INDEX)
01766          MOVE CF-BENEFIT-ALPHA (WS-INDEX)
01767                                  TO  WS-KIND
01768          MOVE CF-SPECIAL-CALC-CD (WS-INDEX)
01769                                  TO  WS-CALC-CD
01770          MOVE CF-BENEFIT-DESCRIP (WS-INDEX)
01771                                  TO  WS-BENEFIT-DESCRIP
01772          MOVE +1                 TO  WS-NOT-FOUND
01773          GO TO 1699-EXIT.
01774
01775      IF CF-BENEFIT-CODE (WS-INDEX)
01776              IS NOT LESS THAN  CF-HI-BEN-IN-REC
01777          GO TO 1699-EXIT.
01778
01779      IF WS-INDEX  IS LESS THAN  +8
01780          ADD +1                  TO  WS-INDEX
01781          GO TO 1610-LOOKUP-BENEFIT.
01782
01783  1699-EXIT.
01784      EXIT.
01785  EJECT
01786  1700-WRITE-CERT-NOTE      SECTION.
01788      IF NOT MODIFY-CERT-CAP
01789          MOVE 'UPDATE'           TO  SM-READ
01790          PERFORM 9800-SECURITY-VIOLATION  THRU  9899-EXIT
01791          MOVE ER-0070            TO  EMI-ERROR
01792          PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT
01793          GO TO 8200-SEND-DATAONLY.
01794
01795 **MOVE CANCEL QUOTE NOTE INFO
01796      IF PI-LF-REFUND-AMT NUMERIC
01797         MOVE PI-LF-REFUND-AMT    TO WS-CN-LIFE-AMT-RF
01798         MOVE PI-LF-REFUND-METH   TO WS-CN-LF-REF-METH
01799      ELSE
01800         MOVE ZERO                TO WS-CN-LIFE-AMT-RF
01801                                     PI-LF-REFUND-AMT
01802         MOVE SPACE               TO WS-CN-LF-REF-METH
01803                                     PI-LF-REFUND-METH.
01804
01805      IF PI-AH-REFUND-AMT NUMERIC
01806         MOVE PI-AH-REFUND-AMT    TO WS-CN-AH-AMT-RF
01807         MOVE PI-AH-REFUND-METH   TO WS-CN-AH-REF-METH
01808      ELSE
01809         MOVE ZERO                TO WS-CN-AH-AMT-RF
01810                                     PI-AH-REFUND-AMT
01811         MOVE SPACE               TO WS-CN-AH-REF-METH
01812                                     PI-AH-REFUND-METH.
01813
01814      COMPUTE PI-TOTAL-REFUND-AMT = PI-LF-REFUND-AMT +
01815                                    PI-AH-REFUND-AMT.
01816
01817      MOVE PI-TOTAL-REFUND-AMT    TO WS-CN-TOTAL-RF.
01818
01819      MOVE SAVE-DATE              TO WS-CN-DT-QUOTED.
01820      MOVE PI-PROCESSOR-ID        TO WS-CN-PROCESSOR-ID.
01821      IF PI-LF-CANCEL-DATE-ED NOT EQUAL SPACE
01822         MOVE PI-LF-CANCEL-DATE-ED
01823                                  TO WS-CN-CANCEL-DT
01824      ELSE
01825         MOVE PI-AH-CANCEL-DATE-ED
01826                                  TO WS-CN-CANCEL-DT
           END-IF
           MOVE 'CZ'                   TO CERT-NOTE-FILE
           MOVE PI-COMPANY-CD          TO CZ-COMPANY-CD
           MOVE PI-CARRIER             TO CZ-CARRIER
           MOVE PI-GROUPING            TO CZ-GROUPING
           MOVE PI-STATE               TO CZ-STATE
           MOVE PI-ACCOUNT             TO CZ-ACCOUNT
           MOVE PI-CERT-EFF-DT         TO CZ-CERT-EFF-DT
           MOVE PI-CERT-PRIME          TO CZ-CERT-PRIME
           MOVE PI-CERT-SFX            TO CZ-CERT-SFX
           MOVE '1'                    TO CZ-RECORD-TYPE
           MOVE +1                     TO CZ-NOTE-SEQUENCE
           MOVE PI-PROCESSOR-ID        TO CZ-LAST-MAINT-USER
           MOVE EIBTIME                TO CZ-LAST-MAINT-HHMMSS
           MOVE SAVE-BIN-DATE          TO CZ-LAST-MAINT-DT
           MOVE WS-CAN-QUOTE-NOTE      TO CZ-NOTE-INFORMATION
           MOVE ' '                    TO WS-STOP-SW
           PERFORM UNTIL I-SAY-TO-STOP
              
      * EXEC CICS WRITE
      *           FROM      (CERT-NOTE-FILE)
      *           DATASET   (ERNOTE-ID)
      *           RIDFLD    (CZ-CONTROL-PRIMARY)
      *           RESP      (WS-RESPONSE)
      *       END-EXEC
           MOVE LENGTH OF
            CERT-NOTE-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''  N#00008585' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303038353835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERNOTE-ID, 
                 CERT-NOTE-FILE, 
                 DFHEIV11, 
                 CZ-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF WS-RESP-DUPREC
                 ADD +1                TO CZ-NOTE-SEQUENCE
              ELSE
                 SET I-SAY-TO-STOP     TO TRUE
              END-IF
           END-PERFORM
           PERFORM 1800-READ-ELCERT-UPDATE THRU 1800-EXIT
           IF WS-RESP-NORMAL
              EVALUATE CM-NOTE-SW
                 WHEN '1'
                 WHEN '3'
                 WHEN '5'
                 WHEN '7'
                    SET NO-CERT-RW     TO TRUE
                 WHEN ' '
                    MOVE '1'           TO CM-NOTE-SW
                 WHEN '2'
                    MOVE '3'           TO CM-NOTE-SW
                 WHEN '4'
                    MOVE '5'           TO CM-NOTE-SW
                 WHEN '6'
                    MOVE '7'           TO CM-NOTE-SW
              END-EVALUATE
           END-IF
           IF NOT NO-CERT-RW
              PERFORM 1810-REWRITE-ELCERT
                                       THRU 1810-EXIT
           ELSE
              
      * EXEC CICS UNLOCK
      *          DATASET    (ELCERT-ID)
      *       END-EXEC
      *    MOVE '&*                    #   #00008619' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           END-IF
01919      MOVE ER-8160                TO EMI-ERROR.
01920      PERFORM 9700-ERROR-FORMAT  THRU  9799-EXIT.
01921      GO TO 8200-SEND-DATAONLY.
01922
01923  1799-EXIT.
01924      EXIT.
081606 1800-READ-ELCERT-UPDATE.
081606     MOVE PI-COMPANY-CD          TO  WS-CM-COMPANY-CD.
081606     MOVE PI-CARRIER             TO  WS-CM-CARRIER.
081606     MOVE PI-GROUPING            TO  WS-CM-GROUPING.
081606     MOVE PI-STATE               TO  WS-CM-STATE.
081606     MOVE PI-ACCOUNT             TO  WS-CM-ACCOUNT.
081606     MOVE PI-CERT-EFF-DT         TO  WS-CM-CERT-EFF-DT.
081606     MOVE PI-CERT-PRIME          TO  WS-CM-CERT-PRIME.
081606     MOVE PI-CERT-SFX            TO  WS-CM-CERT-SFX.
081606     
      * EXEC CICS READ
081606*        UPDATE
081606*        DATASET  (ELCERT-ID)
081606*        RIDFLD   (WS-CM-CONTROL-PRIMARY)
081606*        SET      (ADDRESS OF CERTIFICATE-MASTER)
081606*        RESP     (WS-RESPONSE)
081606*    END-EXEC
      *    MOVE '&"S        EU         (  N#00008638' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303038363338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
081606     .
081606 1800-EXIT.
081606     EXIT.
081606 1810-REWRITE-ELCERT.
081606     
      * EXEC CICS REWRITE
081606*        FROM      (CERTIFICATE-MASTER)
081606*        DATASET   (ELCERT-ID)
081606*        RESP     (WS-RESPONSE)
081606*    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %  N#00008649' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'204E233030303038363439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
081606     .
081606 1810-EXIT.
081606     EXIT.
       4000-GEN-CANCEL-TRANS.
112612     IF PI-LF-REFUND-AMT NOT NUMERIC
112612         MOVE +0 TO PI-LF-REFUND-AMT
112612     END-IF
112612     IF PI-AH-REFUND-AMT NOT NUMERIC
112612         MOVE +0 TO PI-AH-REFUND-AMT
112612     END-IF
112612
062712     PERFORM 4400-ADD-ORIG-REC THRU 4400-EXIT
062712
           MOVE '2'                    TO CG-OPTION-CODE
           MOVE PI-COMPANY-ID          TO CG-COMPANY-ID
           MOVE PI-PROCESSOR-ID        TO CG-PROC-ID
           MOVE PI-CR-MONTH-END-DT     TO CG-MONTH-END-DT
           MOVE PI-COMPANY-CD          TO CG-CERT-COMPANY-CD
           MOVE PI-CARRIER             TO CG-CERT-CARRIER
           MOVE PI-GROUPING            TO CG-CERT-GROUP
           MOVE PI-STATE               TO CG-CERT-STATE
           MOVE PI-ACCOUNT             TO CG-CERT-ACCOUNT
           MOVE PI-CERT-EFF-DT         TO CG-CERT-EFF-DT
           MOVE PI-CERT-NO             TO CG-CERT-CERT-NO
           MOVE SAVE-BIN-DATE          TO CG-CURRENT-DT
           MOVE ZEROS                  TO CG-LF-BENCD
                                          CG-AH-BENCD
                                          CG-LF-CAN-AMT
                                          CG-AH-CAN-AMT
           MOVE LOW-VALUES             TO CG-LF-CAN-DT
                                          CG-AH-CAN-DT
           MOVE SPACES                 TO CG-BATCH-NO
070622     MOVE PI-CANCEL-REASON       TO CG-DCC-REASON-CD
062712     IF CM-LF-BENEFIT-CD NOT = '00' AND '  ' AND 'DD'
062712       AND (CM-LF-CANCEL-DT = LOW-VALUES OR SPACES)
062712        IF (PI-LF-CANCEL-DATE NOT = LOW-VALUES AND SPACES)
062712           MOVE CM-LF-BENEFIT-CD  TO CG-LF-BENCD
062712           MOVE PI-LF-REFUND-AMT  TO CG-LF-CAN-AMT
062712           MOVE PI-LF-CANCEL-DATE TO CG-LF-CAN-DT
062712        END-IF
062712     END-IF
062712     IF CM-AH-BENEFIT-CD NOT = '00' AND '  '
062712       AND (CM-AH-CANCEL-DT = LOW-VALUES OR SPACES)
062712        IF (PI-AH-CANCEL-DATE NOT = LOW-VALUES AND SPACES)
062712           MOVE CM-AH-BENEFIT-CD  TO CG-AH-BENCD
062712           MOVE PI-AH-REFUND-AMT  TO CG-AH-CAN-AMT
062712           MOVE PI-AH-CANCEL-DATE TO CG-AH-CAN-DT
062712        END-IF
062712     END-IF
           
      * EXEC CICS LINK
      *        PROGRAM  ('ELCANC')
      *        COMMAREA (CANCEL-GEN-PASS-AREA)
      *    END-EXEC
           MOVE LENGTH OF
            CANCEL-GEN-PASS-AREA
             TO DFHEIV11
           MOVE 'ELCANC' TO DFHEIV1
      *    MOVE '."C                   (   #00008703' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038373033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CANCEL-GEN-PASS-AREA, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF CG-SUCCESS
              CONTINUE
           ELSE
010412        MOVE SPACES              TO EMI-MESSAGE-AREA (1)
              move '3'                 to emi-switch1
              MOVE CG-ERROR-CODE       TO EMI-TEXT-VARIABLE (1)
010412        EVALUATE TRUE
010412           WHEN CG-DATE-ERROR
010412             MOVE WS-01-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
010412           WHEN CG-CERT-NOT-FOUND
010412             MOVE WS-02-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
010412           WHEN CG-AMOUNT-ERROR
010412             MOVE WS-04-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
010412           WHEN CG-OPTION-ERROR
010412             MOVE WS-05-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
010412           WHEN CG-PREV-CAN
010412             MOVE WS-06-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
010412           WHEN CG-INVALID-DATA
010412             MOVE WS-07-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
010412           WHEN CG-NO-ACCT-MSTR
010412             MOVE WS-08-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
010412           WHEN CG-SFX-A-EXIST
010412             MOVE WS-09-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
010412           WHEN CG-MISC-ERROR
010412             MOVE WS-99-CANCEL-ERR TO EMI-ERROR-TEXT (1) (12:25)
010412           WHEN OTHER
010412             move ' error - elcanc - return '
                                       to emi-error-text (1) (12:25)
010412        END-EVALUATE
              go to 8200-send-dataonly
           end-if
           .
       4000-exit.
           exit.
070622 4100-CONNECT-TO-DB.
070622
070622     IF SVR > SPACES
070622        CONTINUE
070622     ELSE
070622        MOVE 'NTCSO2_LOGIC'         TO SVR
070622        MOVE 'sa'                   TO USR
070622        MOVE 'ntcso2'               TO PASS
070622     END-IF
070622
070622     STRING
070622         USR DELIMITED SPACE
070622         "." DELIMITED SIZE
070622         PASS DELIMITED SPACE INTO USR-PASS
070622     END-STRING
070622
070622     EXEC SQL
070622        CONNECT TO :SVR USER :USR-PASS
070622     END-EXEC
070622
070622     IF SQLCODE NOT = 0
070622        DISPLAY "ERROR: CANNOT CONNECT "
070622        DISPLAY SQLCODE
070622        DISPLAY SQLERRMC
070622        GO TO 4100-EXIT
070622     END-IF
070622
070622     .
070622 4100-EXIT.
070622     EXIT.
070622 4300-DISCONNECT.
070622
070622     EXEC SQL
070622        DISCONNECT
070622     END-EXEC
070622     .
070622 4300-EXIT.
070622     EXIT.
062712
062712 4400-ADD-ORIG-REC.
062712
062712     MOVE PI-COMPANY-CD          TO  WS-CM-COMPANY-CD.
062712     MOVE PI-CARRIER             TO  WS-CM-CARRIER.
062712     MOVE PI-GROUPING            TO  WS-CM-GROUPING.
062712     MOVE PI-STATE               TO  WS-CM-STATE.
062712     MOVE PI-ACCOUNT             TO  WS-CM-ACCOUNT.
062712     MOVE PI-CERT-EFF-DT         TO  WS-CM-CERT-EFF-DT.
062712     MOVE PI-CERT-PRIME          TO  WS-CM-CERT-PRIME.
062712     MOVE PI-CERT-SFX            TO  WS-CM-CERT-SFX.
062712
062712     
      * EXEC CICS READ
062712*        DATASET  (ELCERT-ID)
062712*        RIDFLD   (WS-CM-CONTROL-PRIMARY)
062712*        SET      (ADDRESS OF CERTIFICATE-MASTER)
062712*        RESP     (WS-RESPONSE)
062712*    END-EXEC
      *    MOVE '&"S        E          (  N#00008791' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303038373931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CM-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
062712     IF WS-RESP-NORMAL
062712         CONTINUE
062712     ELSE
062712        MOVE ER-0142                TO EMI-ERROR
062712        PERFORM 9700-ERROR-FORMAT THRU 9799-EXIT
062712        GO TO 8100-SEND-INITIAL-MAP
062712     END-IF
062712
062712******************************************************************
062712*            A D D   O R I G   C E R T   I N F O                 *
062712******************************************************************
062712
062712     display ' made it to add orig cert ' WS-CM-CONTROL-PRIMARY
062712
062712     MOVE WS-CM-CONTROL-PRIMARY  TO WS-MA-CONTROL-PRIMARY
062712     MOVE ' '                    TO WS-ERMAIL-SW
062712
062712     
      * EXEC CICS READ
062712*       DATASET   ('ERMAIL')
062712*       SET       (ADDRESS OF MAILING-DATA)
062712*       RIDFLD    (WS-MA-CONTROL-PRIMARY)
062712*       RESP      (WS-RESPONSE)
062712*    END-EXEC
           MOVE 'ERMAIL' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00008814' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303038383134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-MA-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF MAILING-DATA TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
062712
062712     IF WS-RESP-NORMAL
062712        SET ERMAIL-FOUND TO TRUE
062712     END-IF
121712
121712     MOVE WS-CM-CONTROL-PRIMARY TO ELCRTT-PRIMARY
121712     MOVE 'C'                   TO ELCRTT-REC-TYPE
121712     MOVE +0                    TO WS-CERT-TRL-REC-NOT-FOUND
121712
121712     
      * EXEC CICS READ
121712*         DATASET  (ELCRTT-ID)
121712*         RIDFLD   (ELCRTT-KEY)
121712*         SET      (ADDRESS OF CERTIFICATE-TRAILERS)
121712*         RESP     (WS-RESPONSE)
121712*    END-EXEC
      *    MOVE '&"S        E          (  N#00008829' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303038383239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCRTT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCRTT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
121712
121712     IF NOT WS-RESP-NORMAL
121712        MOVE +1               TO WS-CERT-TRL-REC-NOT-FOUND
121712     END-IF
062712
062712     MOVE WS-CM-CONTROL-PRIMARY TO ELCRTO-KEY (1:33)
062712     MOVE 'I'                 TO ELCRTO-RECORD-TYPE
062712     MOVE +0                  TO ELCRTO-SEQ-NO
062712
062712     
      * EXEC CICS READ
062712*       DATASET   ('ELCRTO')
062712*       SET       (ADDRESS OF ORIGINAL-CERTIFICATE)
062712*       RIDFLD    (ELCRTO-KEY)
062712*       GTEQ
062712*       RESP      (WS-RESPONSE)
062712*    END-EXEC
           MOVE 'ELCRTO' TO DFHEIV1
      *    MOVE '&"S        G          (  N#00008844' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303038383434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCRTO-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ORIGINAL-CERTIFICATE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
062712
062712     IF WS-RESP-NORMAL
062712        AND (OC-CONTROL-PRIMARY (1:33) =
062712                 WS-CM-CONTROL-PRIMARY)
062712        AND (OC-RECORD-TYPE = 'I')
062712        IF (OC-ENDORSEMENT-PROCESSED-DT = LOW-VALUES)
062712           
      * EXEC CICS READ
062712*             DATASET   ('ELCRTO')
062712*             SET       (ADDRESS OF ORIGINAL-CERTIFICATE)
062712*             RIDFLD    (OC-CONTROL-PRIMARY)
062712*             UPDATE
062712*             RESP      (WS-RESPONSE)
062712*          END-EXEC
           MOVE 'ELCRTO' TO DFHEIV1
      *    MOVE '&"S        EU         (  N#00008857' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303038383537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 OC-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ORIGINAL-CERTIFICATE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
062712           MOVE CM-INSURED-LAST-NAME   TO OC-INS-LAST-NAME
062712           MOVE CM-INSURED-FIRST-NAME  TO OC-INS-FIRST-NAME
062712           MOVE CM-INSURED-INITIAL2    TO OC-INS-MIDDLE-INIT
062712           MOVE CM-INSURED-ISSUE-AGE   TO OC-INS-AGE
062712           MOVE CM-JT-LAST-NAME        TO OC-JNT-LAST-NAME
062712           MOVE CM-JT-FIRST-NAME       TO OC-JNT-FIRST-NAME
062712           MOVE CM-JT-INITIAL          TO OC-JNT-MIDDLE-INIT
062712           MOVE CM-INSURED-JOINT-AGE   TO OC-JNT-AGE
072312           IF ((PI-LF-CANCEL-DATE = SPACES OR LOW-VALUES) AND
072312            (CM-LF-CANCEL-DT NOT = SPACES AND LOW-VALUES))
072312               MOVE SPACES             TO OC-LF-BENCD
072312               MOVE ZEROS              TO OC-LF-TERM
072312                                          OC-LF-BEN-AMT
072312                                          OC-LF-PRM-AMT
072312                                          OC-LF-ALT-BEN-AMT
072312                                          OC-LF-ALT-PRM-AMT
072312                                          OC-LF-COMM-PCT
072312                                          OC-LF-CANCEL-AMT
072312               MOVE LOW-VALUES         TO OC-LF-EXP-DT
072312               MOVE CM-LF-CANCEL-DT    TO OC-LF-CANCEL-DT
072312           ELSE
062712               MOVE CM-LF-BENEFIT-CD       TO OC-LF-BENCD
062712               MOVE CM-LF-ORIG-TERM        TO OC-LF-TERM
062712               MOVE CM-LF-BENEFIT-AMT      TO OC-LF-BEN-AMT
062712               MOVE CM-LF-PREMIUM-AMT      TO OC-LF-PRM-AMT
062712               MOVE CM-LF-ALT-BENEFIT-AMT  TO OC-LF-ALT-BEN-AMT
062712               MOVE CM-LF-ALT-PREMIUM-AMT  TO OC-LF-ALT-PRM-AMT
062712               MOVE CM-LF-LOAN-EXPIRE-DT   TO OC-LF-EXP-DT
062712               MOVE CM-LIFE-COMM-PCT       TO OC-LF-COMM-PCT
071712               MOVE PI-LF-REFUND-AMT       TO OC-LF-CANCEL-AMT
071712               MOVE PI-LF-CANCEL-DATE      TO OC-LF-CANCEL-DT
072312           END-IF
071712           MOVE CM-LF-ITD-CANCEL-AMT   TO OC-LF-ITD-CANCEL-AMT
072312           IF ((PI-AH-CANCEL-DATE = SPACES OR LOW-VALUES) AND
072312            (CM-AH-CANCEL-DT NOT = SPACES AND LOW-VALUES))
072312               MOVE SPACES             TO OC-AH-BENCD
072312               MOVE ZEROS              TO OC-AH-TERM
072312                                          OC-AH-BEN-AMT
072312                                          OC-AH-PRM-AMT
072312                                          OC-AH-COMM-PCT
072312                                          OC-AH-CANCEL-AMT
072312                                          OC-AH-CP
072312               MOVE LOW-VALUES         TO OC-AH-EXP-DT
072312               MOVE CM-AH-CANCEL-DT    TO OC-AH-CANCEL-DT
072312           ELSE
062712               MOVE CM-AH-BENEFIT-CD   TO OC-AH-BENCD
062712               MOVE CM-AH-ORIG-TERM    TO OC-AH-TERM
062712               MOVE CM-AH-BENEFIT-AMT  TO OC-AH-BEN-AMT
062712               MOVE CM-AH-PREMIUM-AMT  TO OC-AH-PRM-AMT
062712               MOVE CM-AH-LOAN-EXPIRE-DT TO OC-AH-EXP-DT
062712               MOVE CM-AH-COMM-PCT     TO OC-AH-COMM-PCT
062712               MOVE CM-AH-CRITICAL-PERIOD TO OC-AH-CP
071712               MOVE PI-AH-REFUND-AMT   TO OC-AH-CANCEL-AMT
071712               MOVE PI-AH-CANCEL-DATE  TO OC-AH-CANCEL-DT
072312           END-IF
071712           MOVE CM-AH-ITD-CANCEL-AMT   TO OC-AH-ITD-CANCEL-AMT
062712           MOVE CM-LOAN-1ST-PMT-DT     TO OC-1ST-PMT-DT
011413           MOVE 'Y'                    TO OC-CANCEL-TRAN-IND
062712           MOVE PI-PROCESSOR-ID        TO OC-LAST-MAINT-BY
062712           MOVE EIBTIME                TO OC-LAST-MAINT-HHMMSS
062712           MOVE EIBDATE                TO DC-JULIAN-YYDDD
062712           MOVE '5'                    TO DC-OPTION-CODE
062712           PERFORM 9600-DATE-LINK
062712           MOVE DC-BIN-DATE-1          TO OC-LAST-MAINT-DT
062712           IF ERMAIL-FOUND
062712               MOVE MA-CRED-BENE-NAME
062712                           TO OC-CRED-BENE-NAME
062712           END-IF
121712           IF WS-CERT-TRL-REC-NOT-FOUND = ZERO
121712               MOVE CS-INS-AGE-DEFAULT-FLAG TO
121712                                  OC-INS-AGE-DEFAULT-FLAG
121712               MOVE CS-JNT-AGE-DEFAULT-FLAG TO
121712                                  OC-JNT-AGE-DEFAULT-FLAG
121712           END-IF
121712
062712           
      * EXEC CICS REWRITE
062712*             DATASET   ('ELCRTO')
062712*             FROM      (ORIGINAL-CERTIFICATE)
062712*             RESP      (WS-RESPONSE)
062712*          END-EXEC
           MOVE LENGTH OF
            ORIGINAL-CERTIFICATE
             TO DFHEIV11
           MOVE 'ELCRTO' TO DFHEIV1
      *    MOVE '&& L                  %  N#00008939' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'204E233030303038393339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ORIGINAL-CERTIFICATE, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
062712           IF NOT WS-RESP-NORMAL
062712              MOVE ER-3830          TO EMI-ERROR
062712              PERFORM 9700-ERROR-FORMAT
062712                                 THRU 9799-EXIT
062712              GO TO 8200-SEND-DATAONLY
062712           END-IF
062712
062712           GO TO 4400-EXIT
062712
062712        ELSE
062712           SUBTRACT +1 FROM OC-KEY-SEQ-NO
062712        END-IF
062712     ELSE
062712        MOVE SPACES              TO ORIGINAL-CERTIFICATE
062712        MOVE 'OC'                TO OC-RECORD-ID
062712        MOVE WS-CM-CONTROL-PRIMARY TO OC-CONTROL-PRIMARY (1:33)
062712        MOVE 'I'                 TO OC-RECORD-TYPE
062712        MOVE +4096               TO OC-KEY-SEQ-NO
062712     END-IF
062712
062712     MOVE CM-INSURED-LAST-NAME   TO OC-INS-LAST-NAME
062712     MOVE CM-INSURED-FIRST-NAME  TO OC-INS-FIRST-NAME
062712     MOVE CM-INSURED-INITIAL2    TO OC-INS-MIDDLE-INIT
062712     MOVE CM-INSURED-ISSUE-AGE   TO OC-INS-AGE
062712     MOVE CM-JT-LAST-NAME        TO OC-JNT-LAST-NAME
062712     MOVE CM-JT-FIRST-NAME       TO OC-JNT-FIRST-NAME
062712     MOVE CM-JT-INITIAL          TO OC-JNT-MIDDLE-INIT
062712     MOVE CM-INSURED-JOINT-AGE   TO OC-JNT-AGE
072312     IF ((PI-LF-CANCEL-DATE = SPACES OR LOW-VALUES) AND
072312      (CM-LF-CANCEL-DT NOT = SPACES AND LOW-VALUES))
072312         MOVE SPACES             TO OC-LF-BENCD
072312         MOVE ZEROS              TO OC-LF-TERM
072312                                    OC-LF-BEN-AMT
072312                                    OC-LF-PRM-AMT
072312                                    OC-LF-ALT-BEN-AMT
072312                                    OC-LF-ALT-PRM-AMT
072312                                    OC-LF-COMM-PCT
072312                                    OC-LF-CANCEL-AMT
072312         MOVE LOW-VALUES         TO OC-LF-EXP-DT
072312         MOVE CM-LF-CANCEL-DT    TO OC-LF-CANCEL-DT
072312     ELSE
062712         MOVE CM-LF-BENEFIT-CD   TO OC-LF-BENCD
062712         MOVE CM-LF-ORIG-TERM    TO OC-LF-TERM
062712         MOVE CM-LF-BENEFIT-AMT  TO OC-LF-BEN-AMT
062712         MOVE CM-LF-PREMIUM-AMT  TO OC-LF-PRM-AMT
062712         MOVE CM-LF-ALT-BENEFIT-AMT TO OC-LF-ALT-BEN-AMT
062712         MOVE CM-LF-ALT-PREMIUM-AMT TO OC-LF-ALT-PRM-AMT
062712         MOVE CM-LF-LOAN-EXPIRE-DT TO OC-LF-EXP-DT
062712         MOVE CM-LIFE-COMM-PCT   TO OC-LF-COMM-PCT
071712         MOVE PI-LF-REFUND-AMT   TO OC-LF-CANCEL-AMT
071712         MOVE PI-LF-CANCEL-DATE  TO OC-LF-CANCEL-DT
072312     END-IF
071712     MOVE CM-LF-ITD-CANCEL-AMT   TO OC-LF-ITD-CANCEL-AMT
072312     IF ((PI-AH-CANCEL-DATE = SPACES OR LOW-VALUES) AND
072312      (CM-AH-CANCEL-DT NOT = SPACES AND LOW-VALUES))
072312         MOVE SPACES             TO OC-AH-BENCD
072312         MOVE ZEROS              TO OC-AH-TERM
072312                                    OC-AH-BEN-AMT
072312                                    OC-AH-PRM-AMT
072312                                    OC-AH-COMM-PCT
072312                                    OC-AH-CANCEL-AMT
072312                                    OC-AH-CP
072312         MOVE LOW-VALUES         TO OC-AH-EXP-DT
072312         MOVE CM-AH-CANCEL-DT    TO OC-AH-CANCEL-DT
072312     ELSE
062712         MOVE CM-AH-BENEFIT-CD   TO OC-AH-BENCD
062712         MOVE CM-AH-ORIG-TERM    TO OC-AH-TERM
062712         MOVE CM-AH-BENEFIT-AMT  TO OC-AH-BEN-AMT
062712         MOVE CM-AH-PREMIUM-AMT  TO OC-AH-PRM-AMT
062712         MOVE CM-AH-LOAN-EXPIRE-DT TO OC-AH-EXP-DT
062712         MOVE CM-AH-COMM-PCT     TO OC-AH-COMM-PCT
062712         MOVE CM-AH-CRITICAL-PERIOD TO OC-AH-CP
062712         MOVE CM-AH-CANCEL-DT    TO OC-AH-CANCEL-DT
071712         MOVE PI-AH-REFUND-AMT   TO OC-AH-CANCEL-AMT
071712         MOVE PI-AH-CANCEL-DATE  TO OC-AH-CANCEL-DT
072312     END-IF
071712     MOVE CM-AH-ITD-CANCEL-AMT   TO OC-AH-ITD-CANCEL-AMT
062712     MOVE CM-LOAN-1ST-PMT-DT     TO OC-1ST-PMT-DT
011413     MOVE 'N'                    TO OC-ISSUE-TRAN-IND
011413     MOVE 'Y'                    TO OC-CANCEL-TRAN-IND
062712     MOVE PI-PROCESSOR-ID        TO OC-LAST-MAINT-BY
062712     MOVE EIBTIME                TO OC-LAST-MAINT-HHMMSS
062712     MOVE EIBDATE                TO DC-JULIAN-YYDDD
062712     MOVE '5'                    TO DC-OPTION-CODE
062712     PERFORM 9600-DATE-LINK
062712     MOVE DC-BIN-DATE-1          TO OC-LAST-MAINT-DT
062712     IF ERMAIL-FOUND
062712         MOVE MA-CRED-BENE-NAME
062712                     TO OC-CRED-BENE-NAME
062712     END-IF
121712     IF WS-CERT-TRL-REC-NOT-FOUND = ZERO
121712         MOVE CS-INS-AGE-DEFAULT-FLAG TO
121712                            OC-INS-AGE-DEFAULT-FLAG
121712         MOVE CS-JNT-AGE-DEFAULT-FLAG TO
121712                            OC-JNT-AGE-DEFAULT-FLAG
121712     END-IF
121712
062712     MOVE LOW-VALUES       TO OC-ENDORSEMENT-PROCESSED-DT
062712     .
062712 4400-WRITE-ELCRTO.
062712
062712     
      * EXEC CICS WRITE
062712*       DATASET   ('ELCRTO')
062712*       FROM      (ORIGINAL-CERTIFICATE)
062712*       RIDFLD    (OC-CONTROL-PRIMARY)
062712*       RESP      (WS-RESPONSE)
062712*    END-EXEC
           MOVE LENGTH OF
            ORIGINAL-CERTIFICATE
             TO DFHEIV11
           MOVE 'ELCRTO' TO DFHEIV1
      *    MOVE '&$ L                  ''  N#00009045' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303039303435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ORIGINAL-CERTIFICATE, 
                 DFHEIV11, 
                 OC-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
062712
062712    IF WS-RESP-DUPKEY OR WS-RESP-DUPREC
062712        SUBTRACT +1    FROM OC-KEY-SEQ-NO
062712        GO TO 4400-WRITE-ELCRTO
062712    ELSE
062712        IF NOT WS-RESP-NORMAL
062712           MOVE ER-3830          TO EMI-ERROR
062712           PERFORM 9700-ERROR-FORMAT
062712                                 THRU 9799-EXIT
062712           GO TO 8200-SEND-DATAONLY
062712        END-IF
062712    END-IF
062712
062712     .
062712 4400-EXIT.
062712    EXIT.
062712
062712
01926  5000-GET-LF-AM-REM-TERM.
01927      MOVE +1                     TO SUB3.
01928
01929  5100-GET-LF-AM-REM-TERM.
01930      IF AM-BENEFIT-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1
01931         IF CM-LF-BENEFIT-CD = AM-BENEFIT-CODE (SUB3)
01932            MOVE AM-ALLOWABLE-BENEFITS (SUB3)
01933                          TO WS-SAVE-ALLOWABLE-BENEFIT
01934            GO TO 5300-MOVE-LF-REM-TERM.
01935
01936      IF SUB3 LESS THAN +20
01937         ADD +1                   TO SUB3
01938         GO TO 5100-GET-LF-AM-REM-TERM.
01939
01940      MOVE +1                     TO SUB3.
01941
01942      IF PI-COMPANY-ID NOT EQUAL 'DMD'
01943          GO TO 5200-EDIT-FOR-NINETY-LF.
01944
01945  5100-GET-LF-DMD-AM-REM-TERM.
01946
01947      IF AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1
01948         IF CM-LF-BENEFIT-CD = AM-BENEFIT-DMD-CODE (SUB3)
01949            MOVE AM-ALLOWABLE-DMD-BENEFITS (SUB3)
01950                          TO WS-SAVE-ALLOWABLE-BENEFIT
01951            GO TO 5300-MOVE-LF-REM-TERM.
01952
01953      IF SUB3 LESS THAN +30
01954         ADD +1                   TO SUB3
01955         GO TO 5100-GET-LF-DMD-AM-REM-TERM.
01956
01957      MOVE +1                     TO SUB3.
01958
01959  5200-EDIT-FOR-NINETY-LF.
01960
01961 ***************************************************************
01962 *                                                             *
01963 *    NINETY BENEFIT PLAN TYPES:                               *
01964 *                                                             *
01965 *      91 = ALL INDIVIDUAL REDUCING BENEFITS.                 *
01966 *      92 = ALL GROUP REDUCING BENEFITS                       *
01967 *      93 = ALL INDIVIDUAL LEVEL BENEFITS                     *
01968 *      94 = ALL GROUP LEVEL BENEFITS                          *
01969 *      98 = ALL BENEFITS EXCEPT THOSE CODED IN ACCOUNT        *
01970 *           BENEFIT CONTROLS.                                 *
01971 *      99 = ALL BENEFITS                                      *
01972 *                                                             *
01973 ***************************************************************
01974
01975      MOVE AM-ALLOWABLE-BENEFITS (SUB3)
01976                          TO WS-SAVE-ALLOWABLE-BENEFIT.
01977
01978      IF (AM-BENEFIT-CODE (SUB3) = '99')
01979        AND
01980         (AM-BENEFIT-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
01981            GO TO 5300-MOVE-LF-REM-TERM.
01982
01983      IF (AM-BENEFIT-CODE (SUB3) = '91')
01984        AND
01985         (AM-BENEFIT-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
01986         IF CF-REDUCING (WS-INDEX)
01987            IF CM-INDIVIDUAL
01988                GO TO 5300-MOVE-LF-REM-TERM.
01989
01990      IF (AM-BENEFIT-CODE (SUB3) = '92')
01991        AND
01992         (AM-BENEFIT-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
01993         IF CF-REDUCING (WS-INDEX)
01994            IF CM-GROUP
01995                GO TO 5300-MOVE-LF-REM-TERM.
01996
01997      IF (AM-BENEFIT-CODE (SUB3) = '93')
01998        AND
01999         (AM-BENEFIT-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
02000         IF CF-LEVEL (WS-INDEX)
02001            IF CM-INDIVIDUAL
02002                GO TO 5300-MOVE-LF-REM-TERM.
02003
02004      IF (AM-BENEFIT-CODE (SUB3) = '94')
02005        AND
02006         (AM-BENEFIT-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
02007         IF CF-LEVEL (WS-INDEX)
02008            IF CM-GROUP
02009                GO TO 5300-MOVE-LF-REM-TERM.
02010
02011      IF (AM-BENEFIT-CODE (SUB3) = '98')
02012        AND
02013         (AM-BENEFIT-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
02014             GO TO 5300-MOVE-LF-REM-TERM.
02015
02016      IF SUB3 LESS THAN +20
02017         ADD +1           TO SUB3
02018           GO TO 5200-EDIT-FOR-NINETY-LF.
02019
02020      MOVE SPACES         TO WS-SAVE-ALLOWABLE-BENEFIT.
02021
02022      IF PI-COMPANY-ID NOT EQUAL 'DMD'
02023          MOVE '0'                 TO WS-LF-AM-REM-TERM-CALC
02024          GO TO 5900-EXIT.
02025
02026      MOVE 1                       TO SUB3.
02027
02028  5200-EDIT-FOR-DMD-NINETY-LF.
02029
02030      MOVE AM-ALLOWABLE-DMD-BENEFITS (SUB3)
02031                          TO WS-SAVE-ALLOWABLE-BENEFIT.
02032
02033      IF (AM-BENEFIT-DMD-CODE (SUB3) = '99')
02034        AND
02035         (AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
02036            GO TO 5300-MOVE-LF-REM-TERM.
02037
02038      IF (AM-BENEFIT-DMD-CODE (SUB3) = '91')
02039        AND
02040         (AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
02041         IF CF-REDUCING (WS-INDEX)
02042            IF CM-INDIVIDUAL
02043                GO TO 5300-MOVE-LF-REM-TERM.
02044
02045      IF (AM-BENEFIT-DMD-CODE (SUB3) = '92')
02046        AND
02047         (AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
02048         IF CF-REDUCING (WS-INDEX)
02049            IF CM-GROUP
02050                GO TO 5300-MOVE-LF-REM-TERM.
02051
02052      IF (AM-BENEFIT-DMD-CODE (SUB3) = '93')
02053        AND
02054         (AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
02055         IF CF-LEVEL (WS-INDEX)
02056            IF CM-INDIVIDUAL
02057                GO TO 5300-MOVE-LF-REM-TERM.
02058
02059      IF (AM-BENEFIT-DMD-CODE (SUB3) = '94')
02060        AND
02061         (AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
02062         IF CF-LEVEL (WS-INDEX)
02063            IF CM-GROUP
02064                GO TO 5300-MOVE-LF-REM-TERM.
02065
02066      IF (AM-BENEFIT-DMD-CODE (SUB3) = '98')
02067        AND
02068         (AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-LIFE-OVERRIDE-L1)
02069             GO TO 5300-MOVE-LF-REM-TERM.
02070
02071      IF SUB3 LESS THAN +30
02072         ADD +1                   TO SUB3
02073           GO TO 5200-EDIT-FOR-DMD-NINETY-LF.
02074
02075      MOVE '0'                    TO WS-LF-AM-REM-TERM-CALC.
02076
02077      GO TO 5900-EXIT.
02078
02079  5300-MOVE-LF-REM-TERM.
02080      MOVE WS-SAVE-BENEFIT-REM-TERM
02081                                  TO WS-LF-AM-REM-TERM-CALC.
02082
02083  5900-EXIT.
02084       EXIT.
02085
02086  EJECT
02087
02088  6000-GET-AH-AM-REM-TERM.
02089      MOVE +1                     TO SUB3.
02090
02091  6100-GET-AH-AM-REM-TERM.
02092
02093      IF AM-BENEFIT-TYPE (SUB3) = WS-CF-AH-OVERRIDE-L1
02094         IF WS-BEN-CD = AM-BENEFIT-CODE (SUB3)
02095            MOVE AM-ALLOWABLE-BENEFITS (SUB3)
02096                          TO WS-SAVE-ALLOWABLE-BENEFIT
02097            GO TO 6300-MOVE-AH-REM-TERM.
02098
02099      IF SUB3 LESS THAN +20
02100         ADD +1                   TO SUB3
02101         GO TO 6100-GET-AH-AM-REM-TERM.
02102
02103      MOVE +1                     TO SUB3.
02104
02105      IF PI-COMPANY-ID NOT EQUAL 'DMD'
02106          GO TO 6200-EDIT-FOR-NINETY-AH.
02107
02108  6100-GET-AH-AM-DMD-REM-TERM.
02109      IF AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-AH-OVERRIDE-L1
02110         IF WS-BEN-CD = AM-BENEFIT-DMD-CODE (SUB3)
02111            MOVE AM-ALLOWABLE-DMD-BENEFITS (SUB3)
02112                          TO WS-SAVE-ALLOWABLE-BENEFIT
02113            GO TO 6300-MOVE-AH-REM-TERM.
02114
02115      IF SUB3 LESS THAN +30
02116         ADD +1                   TO SUB3
02117         GO TO 6100-GET-AH-AM-DMD-REM-TERM.
02118
02119      MOVE +1                     TO SUB3.
02120
02121  6200-EDIT-FOR-NINETY-AH.
02122
02123 ***************************************************************
02124 *                                                             *
02125 *    NINETY BENEFIT PLAN TYPES:                               *
02126 *                                                             *
02127 *      91 = ALL INDIVIDUAL REDUCING BENEFITS.                 *
02128 *      92 = ALL GROUP REDUCING BENEFITS.                      *
02129 *      98 = ALL BENEFITS EXCEPT THOSE CODED IN ACCOUNT        *
02130 *           BENEFIT CONTROLS.                                 *
02131 *      99 = ALL BENEFITS.                                     *
02132 *                                                             *
02133 ***************************************************************
02134
02135      MOVE AM-ALLOWABLE-BENEFITS (SUB3)
02136                          TO WS-SAVE-ALLOWABLE-BENEFIT.
02137
02138      IF AM-BENEFIT-CODE (SUB3) = '99'
02139        AND
02140         AM-BENEFIT-TYPE (SUB3) = WS-CF-AH-OVERRIDE-L1
02141            GO TO 6300-MOVE-AH-REM-TERM.
02142
02143      IF AM-BENEFIT-CODE (SUB3) = '91'
02144        AND
02145         AM-BENEFIT-TYPE (SUB3) = WS-CF-AH-OVERRIDE-L1
02146            IF CM-INDIVIDUAL
02147                GO TO 6300-MOVE-AH-REM-TERM.
02148
02149      IF AM-BENEFIT-CODE (SUB3) = '92'
02150        AND
02151         AM-BENEFIT-TYPE (SUB3) = WS-CF-AH-OVERRIDE-L1
02152            IF CM-GROUP
02153                GO TO 6300-MOVE-AH-REM-TERM.
02154
02155      IF AM-BENEFIT-CODE (SUB3) = '98'
02156        AND
02157         AM-BENEFIT-TYPE (SUB3) = WS-CF-AH-OVERRIDE-L1
02158            GO TO 6300-MOVE-AH-REM-TERM.
02159
02160      IF SUB3 LESS THAN +20
02161         ADD +1                    TO SUB3
02162         GO TO 6200-EDIT-FOR-NINETY-AH.
02163
02164      MOVE +1                      TO SUB3.
02165
02166      IF PI-COMPANY-ID NOT EQUAL 'DMD'
02167          MOVE '0'                 TO WS-AH-AM-REM-TERM-CALC
02168          GO TO 6900-EXIT.
02169
02170  6200-EDIT-FOR-DMD-NINETY-AH.
02171
02172      MOVE AM-ALLOWABLE-DMD-BENEFITS (SUB3)
02173                          TO WS-SAVE-ALLOWABLE-BENEFIT.
02174
02175      IF AM-BENEFIT-DMD-CODE (SUB3) = '99'
02176        AND
02177         AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-AH-OVERRIDE-L1
02178            GO TO 6300-MOVE-AH-REM-TERM.
02179
02180      IF AM-BENEFIT-DMD-CODE (SUB3) = '91'
02181        AND
02182         AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-AH-OVERRIDE-L1
02183            IF CM-INDIVIDUAL
02184                GO TO 6300-MOVE-AH-REM-TERM.
02185
02186      IF AM-BENEFIT-DMD-CODE (SUB3) = '92'
02187        AND
02188         AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-AH-OVERRIDE-L1
02189            IF CM-GROUP
02190                GO TO 6300-MOVE-AH-REM-TERM.
02191
02192      IF AM-BENEFIT-DMD-CODE (SUB3) = '98'
02193        AND
02194         AM-BENEFIT-DMD-TYPE (SUB3) = WS-CF-AH-OVERRIDE-L1
02195            GO TO 6300-MOVE-AH-REM-TERM.
02196
02197      IF SUB3 LESS THAN +30
02198         ADD +1                   TO SUB3
02199         GO TO 6200-EDIT-FOR-DMD-NINETY-AH.
02200
02201      MOVE '0'                    TO WS-AH-AM-REM-TERM-CALC.
02202
02203      GO TO 6900-EXIT.
02204
02205  6300-MOVE-AH-REM-TERM.
02206
02207      MOVE WS-SAVE-BENEFIT-REM-TERM
02208                                  TO WS-AH-AM-REM-TERM-CALC.
02209
02210  6900-EXIT.
02211       EXIT.
02212
02213  EJECT
02214  7000-GET-ERFORM     SECTION.
02215      
      * EXEC CICS HANDLE CONDITION
02216 *        NOTFND  (7000-EXIT)
02217 *    END-EXEC.
      *    MOVE '"$I                   ! ( #00009358' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303039333538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02218
02219      MOVE ' '                    TO  WS-FORM-RECORD-SW.
02220      MOVE PI-COMPANY-CD          TO  WS-FO-COMPANY-CD.
02221      MOVE PI-STATE               TO  WS-FO-STATE.
02222      MOVE CM-POLICY-FORM-NO      TO  WS-FO-FORM-ID.
02223      MOVE CM-CERT-EFF-DT         TO  WS-FO-EXP-DT.
02224
02225      
      * EXEC CICS READ
02226 *        DATASET  (ERFORM-ID)
02227 *        RIDFLD   (WS-FO-CONTROL-PRIMARY)
02228 *        SET      (ADDRESS OF FORM-MASTER)
02229 *        GTEQ
02230 *    END-EXEC.
      *    MOVE '&"S        G          (   #00009368' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039333638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERFORM-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-FO-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF FORM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02231
02232      IF PI-COMPANY-CD     EQUAL FO-COMPANY-CD AND
02233         PI-STATE          EQUAL FO-STATE      AND
02234         CM-POLICY-FORM-NO EQUAL FO-FORM-ID
02235          MOVE 'Y'  TO WS-FORM-RECORD-SW
02236          GO TO 7000-EXIT.
02237  7000-EXIT.
02238      EXIT.
02239  7100-BENEFIT-SEARCH SECTION.
02240
02241      IF CM-LF-BENEFIT-CD NOT = ZEROS AND SPACES
02242         MOVE +1                  TO  SUB4
02243         PERFORM 7150-BENEFIT-LIFE-SEARCH THRU 7150-EXIT.
02244
02245      IF CM-AH-BENEFIT-CD NOT = ZEROS AND SPACES
02246         MOVE +1                  TO  SUB4
02247         PERFORM 7250-BENEFIT-AH-SEARCH THRU 7250-EXIT.
02248  7100-EXIT.
02249      EXIT.
02250  7150-BENEFIT-LIFE-SEARCH.
02251      IF PI-LIFE-OVERRIDE-L1 EQUAL TO FO-PLAN-TYPE (SUB4)
02252        IF CM-LF-BENEFIT-CD EQUAL TO FO-PLAN-ID (SUB4)
02253          IF (CM-LF-ORIG-TERM IS LESS THAN FO-PLAN-TERM (SUB4)
02254           OR CM-LF-ORIG-TERM IS EQUAL TO FO-PLAN-TERM (SUB4))
02255             IF FO-PLAN-REFUND-METHOD (SUB4)
02256                     IS GREATER THAN SPACES
02257                MOVE FO-PLAN-REFUND-METHOD (SUB4) TO
02258                                     WS-LF-FO-REFUND-CALC
02259                GO TO 7150-EXIT.
02260
02261      IF SUB4 LESS THAN +40
02262          ADD +1              TO SUB4
02263          GO TO 7150-BENEFIT-LIFE-SEARCH.
02264
02265  7150-EXIT.
02266      EXIT.
02267  7250-BENEFIT-AH-SEARCH.
02268      IF PI-AH-OVERRIDE-L1 EQUAL TO FO-PLAN-TYPE (SUB4)
02269        IF CM-AH-BENEFIT-CD EQUAL TO FO-PLAN-ID (SUB4)
02270          IF (CM-AH-ORIG-TERM IS LESS THAN FO-PLAN-TERM (SUB4)
02271           OR CM-AH-ORIG-TERM IS EQUAL TO FO-PLAN-TERM (SUB4))
02272             IF FO-PLAN-REFUND-METHOD (SUB4)
02273                     IS GREATER THAN SPACES
02274                MOVE FO-PLAN-REFUND-METHOD (SUB4) TO
02275                                     WS-AH-FO-REFUND-CALC
02276                GO TO 7250-EXIT.
02277
02278      IF SUB4 LESS THAN +40
02279          ADD +1              TO SUB4
02280          GO TO 7250-BENEFIT-AH-SEARCH.
02281
02282  7250-EXIT.
02283      EXIT.
02284  EJECT
02285
02286  8100-SEND-INITIAL-MAP  SECTION.
02287      MOVE SAVE-DATE              TO  HDATEO.
02288      MOVE EIBTIME                TO  TIME-IN.
02289      MOVE TIME-OUT               TO  HTIMEO.
101201     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101201     MOVE PI-PROCESSOR-ID        TO  USERIDO.
02290      MOVE EMI-MESSAGE-AREA (1)   TO  HERMSG1O.
02291      MOVE EMI-MESSAGE-AREA (2)   TO  HERMSG2O.
02292
02293      IF CM-LF-BENEFIT-CD = '00'
02294          MOVE -1                 TO  HACANCL
02295      ELSE
02296          MOVE -1                 TO  HLCANCL.
02297
020816     if pi-company-id = 'VPP'
020816        move al-sanof            to refdueha
020816                                    canfeeha
020816        move al-uanof            to refduea
020816                                    canfeea
020816     end-if
020816     IF PI-COMPANY-ID NOT = 'DCC' and 'VPP'
070622*       MOVE AL-SADOF            TO CANREAHA
020816        MOVE AL-SADOF            TO CLPYNHA
070622*                                   CANREAA
020816                                    CLPYNA
020816     END-IF
02300
02301      
      * EXEC CICS SEND
02302 *        FROM    (EL127HO)
02303 *        MAPSET  (MAPSET-NAME)
02304 *        MAP     (MAP-NAME)
02305 *        CURSOR
02306 *        ERASE
02307 *    END-EXEC.
           MOVE LENGTH OF
            EL127HO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00009456' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL127HO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02308
02309      GO TO 9000-RETURN-TRAN.
02310
02311  8200-SEND-DATAONLY  SECTION.
02312      IF FIRST-ENTRY
02313          GO TO 8100-SEND-INITIAL-MAP.
02314
02315      MOVE SAVE-DATE              TO  HDATEO.
02316      MOVE EIBTIME                TO  TIME-IN.
02317      MOVE TIME-OUT               TO  HTIMEO.
101201     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101201     MOVE PI-PROCESSOR-ID        TO  USERIDO.
02318      MOVE EMI-MESSAGE-AREA (1)   TO  HERMSG1O.
02319      MOVE EMI-MESSAGE-AREA (2)   TO  HERMSG2O.
091322*    MOVE -1                     TO  HLCANCL.
02321
02322      
      * EXEC CICS SEND DATAONLY
02323 *        FROM    (EL127HO)
02324 *        MAPSET  (MAPSET-NAME)
02325 *        MAP     (MAP-NAME)
02326 *        CURSOR
02327 *    END-EXEC.
           MOVE LENGTH OF
            EL127HO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00009479' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL127HO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02328
02329      GO TO 9000-RETURN-TRAN.
02330  EJECT
02331  8300-SEND-TEXT  SECTION.
02332      
      * EXEC CICS SEND TEXT
02333 *        FROM    (LOGOFF-TEXT)
02334 *        LENGTH  (LOGOFF-LENGTH)
02335 *        ERASE
02336 *        FREEKB
02337 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00009489' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343839' TO DFHEIV0(25:11)
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
           
02338
02339      
      * EXEC CICS RETURN
02340 *    END-EXEC.
      *    MOVE '.(                    ''   #00009496' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02341  EJECT
02342  8600-DEEDIT  SECTION.
02343      
      * EXEC CICS BIF DEEDIT
02344 *        FIELD   (DEEDIT-FIELD)
02345 *        LENGTH  (15)
02346 *    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00009500' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02347  EJECT
02348  8700-UNAUTHORIZED-ACCESS  SECTION.
02349      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
02350
02351      GO TO 8300-SEND-TEXT.
02352
02353  8800-PF23  SECTION.
02354      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
02355      MOVE XCTL-005               TO  PGM-NAME.
02356
02357      GO TO 9200-XCTL.
02358
02359  8900-RETURN-CICS  SECTION.
02360      
      * EXEC CICS RETURN
02361 *    END-EXEC.
      *    MOVE '.(                    ''   #00009517' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02362
02363  9000-RETURN-TRAN  SECTION.
02364      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
02365      MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.
02366
02367      
      * EXEC CICS RETURN
02368 *        TRANSID   (TRANS-ID)
02369 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
02370 *        LENGTH    (PI-COMM-LENGTH)
02371 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00009524' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02372
02373  9100-RETURN-MAIN-MENU  SECTION.
02374      MOVE XCTL-126               TO  PGM-NAME.
02375
02376      GO TO 9200-XCTL.
02377
02378  9200-XCTL  SECTION.
02379      
      * EXEC CICS XCTL
02380 *        PROGRAM   (PGM-NAME)
02381 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
02382 *        LENGTH    (PI-COMM-LENGTH)
02383 *    END-EXEC.
      *    MOVE '.$C                   %   #00009536' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02384
02385  9300-CLEAR  SECTION.
02386      MOVE ' '                    TO  PI-PEND-SW.
02387      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.
02388
02389      GO TO 9200-XCTL.
02390
02391  9400-PF12  SECTION.
02392      MOVE XCTL-010               TO  PGM-NAME.
02393
02394      GO TO 9200-XCTL.
02395
02396  9500-PGMID-ERROR  SECTION.
02397      
      * EXEC CICS HANDLE CONDITION
02398 *        PGMIDERR  (8300-SEND-TEXT)
02399 *    END-EXEC.
      *    MOVE '"$L                   ! ) #00009554' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303039353534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02400
02401      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.
02402      MOVE ' '                    TO  PI-ENTRY-CD-1.
02403      MOVE XCTL-005               TO  PGM-NAME.
02404      MOVE PGM-NAME               TO  LOGOFF-PGM.
02405      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
02406
02407      GO TO 9200-XCTL.
02408
02409  9600-DATE-LINK  SECTION.
02410      MOVE LINK-ELDATCV           TO  PGM-NAME
02411
02412      
      * EXEC CICS LINK
02413 *        PROGRAM   (PGM-NAME)
02414 *        COMMAREA  (DATE-CONVERSION-DATA)
02415 *        LENGTH    (DC-COMM-LENGTH)
02416 *    END-EXEC.
      *    MOVE '."C                   (   #00009569' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02417
02418
02419  9700-ERROR-FORMAT  SECTION.
02420      IF NOT EMI-ERRORS-COMPLETE
02421          MOVE LINK-001               TO  PGM-NAME
02422          
      * EXEC CICS LINK
02423 *            PROGRAM   (PGM-NAME)
02424 *            COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
02425 *            LENGTH    (EMI-COMM-LENGTH)
02426 *        END-EXEC.
      *    MOVE '."C                   (   #00009579' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02427
02428  9799-EXIT.
02429      EXIT.
02430
02431  9800-SECURITY-VIOLATION  SECTION.
02432 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00009606' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039363036' TO DFHEIV0(25:11)
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
02433
02434  9899-EXIT.
02435      EXIT.
02436
02437  9900-ABEND  SECTION.
02438      MOVE LINK-004               TO  PGM-NAME.
02439      MOVE DFHEIBLK               TO  EMI-LINE1.
02440
02441      
      * EXEC CICS LINK
02442 *        PROGRAM   (PGM-NAME)
02443 *        COMMAREA  (EMI-LINE1)
02444 *        LENGTH    (72)
02445 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00009622' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039363232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02446
02447      MOVE -1                     TO  HLCANCL.
02448
02449      GO TO 8200-SEND-DATAONLY.
02450
02451  9999-LAST-PARAGRAPH.
02452      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1278' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1278' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9500-PGMID-ERROR,
                     9900-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 0910-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 1010-NOTFND,
                     1020-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 1290-ACCT-NOT-FOUND,
                     1290-ACCT-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 0580-ACCT-NOT-FOUND,
                     0580-ACCT-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 1699-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 7000-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1278' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
