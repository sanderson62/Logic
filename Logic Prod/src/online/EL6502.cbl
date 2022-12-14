00001  ID DIVISION.
00002
00003  PROGRAM-ID.                 EL6502.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 09/06/94 09:23:05.
00007 *                            VMOD=2.018
00008 *
00009 *AUTHOR.     LOGIC,INC.
00010 *            DALLAS, TEXAS.
00011
00012 *DATE-COMPILED.
00013 *SECURITY.   *****************************************************
00014 *            *                                                   *
00015 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00016 *            *                                                   *
00017 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00018 *                                                                *
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00021 *            *                                                   *
00022 *            *****************************************************
00023 *
00024 *REMARKS.    TRANSACTION - EXG6 - ACCOUNT MAINT (MISC. ACCT DATA).
00025 *
101101******************************************************************
101101*                   C H A N G E   L O G
101101*
101101* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101101*-----------------------------------------------------------------
101101*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101101* EFFECTIVE    NUMBER
101101*-----------------------------------------------------------------
101101* 101101    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
101101*                              ADJUSTED REDEFINES EL6502AI FILLER
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
092711* 092711  CR2011092100002  AJRA  ADD ORIG DEALER NO
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
101101******************************************************************
00026  ENVIRONMENT DIVISION.
00027
00028      EJECT
00029  DATA DIVISION.
00030  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00031  77  FILLER  PIC X(32)  VALUE '********************************'.
00032  77  FILLER  PIC X(32)  VALUE '*    EL6502 WORKING STORAGE    *'.
00033  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.018 *********'.
00034
00035  01  WS-DATE-AREA.
00036      05  SAVE-DATE                   PIC X(8)    VALUE SPACES.
00037      05  SAVE-BIN-DATE               PIC XX      VALUE SPACES.
00038
00039  01  STANDARD-AREAS.
00040      12  GETMAIN-SPACE               PIC X       VALUE SPACE.
00041      12  WS-COMM-LENGTH              PIC S9(4) COMP VALUE +1500.
00042      12  COMP-REC-LEN                PIC S9(4) COMP VALUE +700.
00043      12  MAP-NAME                    PIC X(8)    VALUE 'EL6502A'.
00044      12  MAPSET-NAME                 PIC X(8)    VALUE 'EL6502S'.
00045      12  SCREEN-NUMBER               PIC X(4)    VALUE '650C'.
00046      12  TRANS-ID                    PIC X(4)    VALUE 'EXC6'.
00047      12  THIS-PGM                    PIC X(8)    VALUE 'EL6502'.
00048      12  PGM-NAME                    PIC X(8).
00049      12  TIME-IN                     PIC S9(7).
00050      12  TIME-OUT-R  REDEFINES TIME-IN.
00051          16  FILLER                  PIC X.
00052          16  TIME-OUT                PIC 99V99.
00053          16  FILLER                  PIC XX.
00054      12  XCTL-005                    PIC X(8)    VALUE 'EL005'.
00055      12  XCTL-010                    PIC X(8)    VALUE 'EL010'.
00056      12  XCTL-626                    PIC X(8)    VALUE 'EL626'.
00057      12  XCTL-650                    PIC X(8)    VALUE 'EL650'.
00058      12  XCTL-6501                   PIC X(8)    VALUE 'EL6501'.
00059      12  XCTL-6503                   PIC X(8)    VALUE 'EL6503'.
00060      12  XCTL-6504                   PIC X(8)    VALUE 'EL6504'.
00061      12  XCTL-6505                   PIC X(8)    VALUE 'EL6505'.
00062      12  XCTL-6506                   PIC X(8)    VALUE 'EL6506'.
00063      12  XCTL-6507                   PIC X(8)    VALUE 'EL6507'.
00064      12  XCTL-6508                   PIC X(8)    VALUE 'EL6508'.
00065      12  LINK-001                    PIC X(8)    VALUE 'EL001'.
00066      12  LINK-004                    PIC X(8)    VALUE 'EL004'.
00067      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'.
00068      12  FILE-ID                     PIC X(8)    VALUE SPACES.
00069      12  ERACCT-FILE                 PIC X(8)    VALUE 'ERACCT'.
00070      12  ERRATE-FILE                 PIC X(8)    VALUE 'ERRATE'.
00071      12  ELCNTL-FILE                 PIC X(8)    VALUE 'ELCNTL'.
00072      12  ERCOMP-FILE                 PIC X(8)    VALUE 'ERCOMP'.
00073      12  BIN-CURRENT-SAVE            PIC XX      VALUE SPACES.
00074      12  YMD-CURRENT-SAVE            PIC X(6)    VALUE SPACES.
00075
00076      12  ERACCT-LENGTH               PIC S9(4)   VALUE +2023 COMP.
00077      12  ELCNTL-LENGTH               PIC S9(4)   VALUE +773  COMP.
00078      12  ERCOMP-LENGTH               PIC S9(4)   VALUE +773  COMP.
00079      12  SC-ITEM                     PIC S9(4)   VALUE +1    COMP.
00080      12  WS-JOURNAL-FILE-LENGTH      PIC S9(4)   VALUE +0    COMP.
00081      12  SUB1                        PIC S9(4)   VALUE +0    COMP.
00082      12  SUB2                        PIC S9(4)   VALUE +0    COMP.
00083
00084      12  DEEDIT-FIELD                PIC X(15).
00085      12  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD PIC S9(15).
00086      12  DEEDIT-FIELD-V1  REDEFINES DEEDIT-FIELD PIC S9(13)V99.
00087      12  DEEDIT-FIELD-V4  REDEFINES DEEDIT-FIELD PIC S9(11)V9(4).
00088      12  DEEDIT-FIELD-V5  REDEFINES DEEDIT-FIELD PIC S9(10)V9(5).
00089      12  DEEDIT-FIELD-V6  REDEFINES DEEDIT-FIELD PIC S9(9)V9(6).
00090
00091      12  WS-EDIT-FIELD-CONV          PIC S9(4)   VALUE +0.
00092      12  WS-TOT-PERCENT              PIC S9V9999 VALUE +0.
00093
00094      12  RATE-KEY.
00095          16  RATE-COMP-CD            PIC X.
00096          16  RATE-STATE              PIC XX.
00097          16  RATE-CLASS              PIC XX.
00098          16  RATE-DEV                PIC XXX.
00099          16  FILLER                  PIC X(20).
00100
00101      12  W-CANCEL-FEE-LONG           PIC S9(3)V99 VALUE ZEROS.
00102      12  W-CANCEL-FEE                PIC S99      VALUE ZEROS.
00103
00104      12  W-EXEC1-DIS-PRCNT           PIC S9V9(4)  VALUE ZEROS.
00105      12  W-EXEC1-LIF-PRCNT           PIC S9V9(4)  VALUE ZEROS.
00106      12  W-EXEC2-DIS-PRCNT           PIC S9V9(4)  VALUE ZEROS.
00107      12  W-EXEC2-LIF-PRCNT           PIC S9V9(4)  VALUE ZEROS.
00108
00109      12  W-CRDT-MOD-PCT              PIC S9V9(4)  VALUE ZEROS.
00110      12  W-LIFE-IBNR-PCT             PIC S9V9(4)  VALUE ZEROS.
00111      12  W-TARGET-LOSS-RATIO         PIC S9V9(4)  VALUE ZEROS.
00112
00113      12  WS-LF-DEV-PERCENT           PIC S9V9(6)  VALUE ZEROS.
00114      12  WS-AH-DEV-PERCENT           PIC S9V9(6)  VALUE ZEROS.
00115
00116      12  WS-OB-LF-RATE               PIC S99V9(5) VALUE ZEROS.
00117      12  WS-OB-JNTLF-RATE            PIC S99V9(5) VALUE ZEROS.
00118      12  WS-OB-AH-RATE               PIC S99V9(5) VALUE ZEROS.
00119
00120      12  WS-TOL-PREM                 PIC S999V99    VALUE ZEROS.
00121      12  WS-TOL-REF                  PIC S999V99    VALUE ZEROS.
00122      12  WS-TOL-CLM                  PIC S999V99    VALUE ZEROS.
00123      12  WS-LF-EXP-PERCENT           PIC S9(3)V9(4) VALUE ZEROS.
00124      12  WS-AH-EXP-PERCENT           PIC S9(3)V9(4) VALUE ZEROS.
00125      12  SV-MAX-MON-BEN              PIC S9(7)      VALUE ZEROS.
00126      12  SV-MAX-TOT-BEN              PIC S9(7)      VALUE ZEROS.
00127      12  WS-MAX-MON-BEN              PIC S9(7)      VALUE ZEROS.
00128      12  WS-MAX-MON-BEN-Z  REDEFINES WS-MAX-MON-BEN
00129                                      PIC ZZZ,ZZ9.
00130      12  WS-MAX-TOT-BEN              PIC S9(7)      VALUE ZEROS.
00131      12  WS-MAX-TOT-BEN-Z  REDEFINES WS-MAX-TOT-BEN
00132                                      PIC ZZZ,ZZ9.
00133      EJECT
00134      12  ERROR-MESSAGES.
00135          16  ER-0000                 PIC X(4)    VALUE '0000'.
00136          16  ER-0002                 PIC X(4)    VALUE '0002'.
00137          16  ER-0004                 PIC X(4)    VALUE '0004'.
00138          16  ER-0008                 PIC X(4)    VALUE '0008'.
00139          16  ER-0029                 PIC X(4)    VALUE '0029'.
00140          16  ER-0068                 PIC X(4)    VALUE '0068'.
00141          16  ER-0070                 PIC X(4)    VALUE '0070'.
00142          16  ER-0627                 PIC X(4)    VALUE '0627'.
00143          16  ER-0906                 PIC X(4)    VALUE '0906'.
00144          16  ER-2039                 PIC X(4)    VALUE '2039'.
00145          16  ER-2114                 PIC X(4)    VALUE '2114'.
00146          16  ER-2154                 PIC X(4)    VALUE '2154'.
00147          16  ER-2165                 PIC X(4)    VALUE '2165'.
00148          16  ER-2168                 PIC X(4)    VALUE '2168'.
00149          16  ER-2169                 PIC X(4)    VALUE '2169'.
00150          16  ER-2170                 PIC X(4)    VALUE '2170'.
00151          16  ER-2572                 PIC X(4)    VALUE '2572'.
00152          16  ER-3124                 PIC X(4)    VALUE '3124'.
00153          16  ER-3125                 PIC X(4)    VALUE '3125'.
00154          16  ER-3126                 PIC X(4)    VALUE '3126'.
00155          16  ER-3127                 PIC X(4)    VALUE '3127'.
00156          16  ER-3128                 PIC X(4)    VALUE '3128'.
00157          16  ER-3257                 PIC X(4)    VALUE '3257'.
00158          16  ER-3258                 PIC X(4)    VALUE '3258'.
00159          16  ER-3267                 PIC X(4)    VALUE '3267'.
00160          16  ER-4011                 PIC X(4)    VALUE '4011'.
00161          16  ER-7320                 PIC X(4)    VALUE '7320'.
00162          16  ER-7531                 PIC X(4)    VALUE '7531'.
00163          16  ER-7717                 PIC X(4)    VALUE '7717'.
00164          16  ER-7718                 PIC X(4)    VALUE '7718'.
00165          16  ER-7719                 PIC X(4)    VALUE '7719'.
00166
00167      12  ELCNTL-KEY.
00168          16  CNTL-COMP-ID            PIC X(3)    VALUE SPACES.
00169          16  CNTL-REC-TYPE           PIC X       VALUE SPACES.
00170          16  CNTL-ACCESS             PIC X(4)    VALUE SPACES.
00171          16  CNTL-SEQ-NO             PIC S9(4)   VALUE +0  COMP.
00172
00173      12  ERCOMP-KEY.
00174          16  COMP-CO-ID              PIC X(1)    VALUE SPACES.
00175          16  COMP-CARRIER            PIC X       VALUE SPACES.
00176          16  COMP-GROUPING           PIC X(6)    VALUE SPACES.
00177          16  COMP-FIN-RESP           PIC X(10)   VALUE SPACES.
00178          16  COMP-ACCT-AGT           PIC X(10)   VALUE SPACES.
00179          16  COMP-REC-TYPE           PIC X       VALUE SPACES.
00180
00181      12  WS-SAVE-REPORT-CODE1        PIC X(10)   VALUE SPACES.
00182      12  WS-SAVE-REPORT-CODE2        PIC X(10)   VALUE SPACES.
120706     12  WS-SAVE-REPORT-CODE3        PIC X(10)   VALUE SPACES.
00183      12  WS-REPORT-CODE-CAPTION.
00184          16  WS-REPORT-CD-CAPTION    PIC X(14)   VALUE SPACES.
00185          16  FILLER                  PIC X       VALUE ':'.
00186
00187      12  WS-FLI-PFK-DESC             PIC X(25)   VALUE
00188          '    PF10=CLIENT ADDL DATA'.
00189      12  WS-CANCEL-TEXT              PIC X(17)   VALUE
00190          '  CANCEL FEE    :'.
PEMMOD     12  WS-YYYYMMDD             PIC X(8).
PEMMOD     12  WS-COMP-DATE REDEFINES WS-YYYYMMDD
PEMMOD                                 PIC 9(8).
00191      EJECT
00192 *                          COPY ELCSCTM.
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
00193      EJECT
00194 *                          COPY ELCSCRTY.
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
00195      EJECT
00196 *                          COPY ELCLOGOF.
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
00197      EJECT
00198 *                          COPY ELCDATE.
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
00199      EJECT
00200 *                          COPY ELCATTR.
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
00201      EJECT
00202 *                          COPY ELCEMIB.
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
00203      EJECT
00204 *                          COPY ELCINTF.
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
00205 *                          COPY ELC650PI.
00001 ******************************************************************
00002 *
00003 *                            ELC650PI.
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.007
00006 *  ***  NOTE  ***   IF ANY CHANGES ARE MADE TO THIS COPYBOOK
00007 *   YOU MUST CONSIDER ALL PROGRAMS THAT USE THIS COPYBOOK AND
00008 * PROGRAM EL6565.  ALSO, CONSIDER EL106 AND EL1061
00009 *
00010 ******************************************************************
101916******************************************************************
101916*                   C H A N G E   L O G
101916*
101916* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101916*-----------------------------------------------------------------
101916*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101916* EFFECTIVE    NUMBER
101916*-----------------------------------------------------------------
101916* 101916  IR2016101900001  PEMA  Inc tot line to 3 bytes
00011
00012      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.
00013          16  PI-MAINT                   PIC X.
00014          16  PI-PREV-ACCOUNT            PIC X(20).
00015          16  PI-PREV-VG-ACCOUNT         PIC X(20).
00016          16  PI-ACCT-KEY.
00017              20  PI-ACCT-CCGSA-KEY.
00018                  24  PI-ACCT-CO             PIC X.
00019                  24  PI-ACCT-CARRIER        PIC X.
00020                  24  PI-ACCT-GROUPING       PIC X(6).
00021                  24  PI-ACCT-STATE          PIC XX.
00022                  24  PI-ACCT-ACCOUNT        PIC X(10).
00023              20  PI-ACCT-EXP-DT           PIC XX.
00024              20  PI-ACCT-REST-OF-EXP      PIC X(4).
00025          16  PI-ACCT-ID                 PIC X(8).
00026          16  PI-PLAN-KEY.
00027              20  PI-PLAN-ACCT-KEY.
00028                  24  PI-PLAN-COMPANY-CD PIC X.
00029                  24  PI-PLAN-CARRIER    PIC X.
00030                  24  PI-PLAN-GROUP      PIC X(6).
00031                  24  PI-PLAN-STATE      PIC X(2).
00032                  24  PI-PLAN-ACCOUNT    PIC X(10).
00033              20  PI-PLAN-BEN-TYPE       PIC X.
00034              20  PI-PLAN-BEN            PIC XX.
00035              20  PI-PLAN-REVISION       PIC X(3).
00036          16  PI-WS-STATE                PIC XX.
00037          16  PI-WS-CLASS                PIC XX.
00038          16  PI-WS-DEV                  PIC X(3).
00039          16  PI-WS-TYPE                 PIC X.
00040          16  PI-WS-PLAN                 PIC XX.
00041
00042          16  PI-ERPNDB-ALT-KEY.
00043              20  PI-PB-COMPANY-CD-A1    PIC X.
00044              20  PI-PB-CARRIER          PIC X.
00045              20  PI-PB-GROUPING         PIC X(6).
00046              20  PI-PB-STATE            PIC XX.
00047              20  PI-PB-ACCOUNT          PIC X(10).
00048              20  PI-PB-CERT-EFF-DT      PIC XX.
00049              20  PI-PB-CERT-NO          PIC X(10).
00050              20  PI-PB-ALT-CHG-SEQ-NO   PIC S9(4)      COMP.
00051              20  PI-PB-RECORD-TYPE      PIC X.
00052
00053          16  PI-DATE-RANGE-TABLE.
00054              20  PI-TABLE-ENT OCCURS 32 TIMES
00055                             INDEXED BY T-INDEX.
00056                  24  PI-BIN-EFF-DT          PIC XX.
00057                  24  PI-BIN-EXP-DT          PIC XX.
00058                  24  PI-BIN-MAINT-DT        PIC XX.
00059                  24  PI-BIN-LO-CERT         PIC XX.
00060                  24  PI-BIN-AR-HI-CERT      PIC XX.
00061                  24  PI-BIN-HI-CERT         PIC XX.
00062          16  PI-PAGE-NUMBER             PIC S9.
00063              88  PI-FST-PAGE               VALUE +1.
00064              88  PI-2ND-PAGE               VALUE +2.
00065              88  PI-3RD-PAGE               VALUE +3.
00066              88  PI-LST-PAGE               VALUE +4.
101916         16  PI-TOTAL-LINES             PIC S999.
00068          16  PI-LINE-SELECTED    PIC S9.
00069 ***  Y2K PROJ 7744
00070          16  EFFCHG-SAVE         PIC 9(11)   COMP-3.
00071          16  BIN-EFFCHG-SAVE     PIC XX.
00072          16  EXPCHG-SAVE         PIC 9(11)   COMP-3.
00073 ***  Y2K PROJ 7744
00074          16  BIN-EXPCHG-SAVE     PIC XX.
00075          16  PI-RECORD-ADDED-SW  PIC X.
00076              88  PI-RECORD-ADDED            VALUE '1'.
00077              88  PI-RECORD-NOT-CREATED      VALUE SPACE.
00078          16  PI-ACCNAME          PIC X(30).
00079          16  PI-COMM-POINTER     PIC S9(8)   COMP.
00080          16  PI-SV-MAINT         PIC X.
00081          16  PI-CURRENT-LINE     PIC S9(3)   COMP-3.
00082          16  PI-TEMP-STOR-ITEMS  PIC S9(4)   COMP.
00083          16  PI-UPDATE-SW        PIC X.
00084              88  PI-CHANGES-MADE             VALUE '1'.
00085          16  PI-NOTE-TYPE        PIC X.
00086              88  PI-ACCT-NOTE                VALUE '1'.
00087          16  PI-DMD-FILE-SW      PIC X.
00088              88  END-OF-FILE                 VALUE 'E'.
00089              88  INTO-NEXT-BENEFITS          VALUE 'I'.
00090              88  FIRST-OCCURS                VALUE 'F'.
00091          16  PI-DMD-OCCURS       PIC S999.
00092          16  PI-DMD-SCREEN       PIC X.
00093              88  SCREEN-1-DISPLAYED  VALUE '1'.
00094              88  SCREEN-2-DISPLAYED  VALUE '2'.
00095              88  SCREEN-3-DISPLAYED  VALUE '3'.
00096          16  PI-NAMEFLG          PIC X.
PEMTST         16  PI-EL650-DEL-SW     PIC X.
               16  PI-MAX-MFEE         PIC S9(5) COMP-3.
               16  PI-DCC-PROD-CODE    PIC XXX.
101916         16  FILLER              PIC X(34).
00098      EJECT
00206
00207      EJECT
00208 *                          COPY ELCAID.
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
051007*00039    02  DFHPF22   PIC  X  VALUE  '?'.
051007   02  DFHPF22   PIC  X  VALUE  '['.
00040    02  DFHPF23   PIC  X  VALUE  '.'.
00041    02  DFHPF24   PIC  X  VALUE  '<'.
00042    02  DFHMSRE   PIC  X  VALUE  'X'.
00043    02  DFHSTRF   PIC  X  VALUE  'h'.
00044    02  DFHTRIG   PIC  X  VALUE  '"'.
00209  01  FILLER    REDEFINES DFHAID.
00210      12  FILLER                      PIC X(8).
00211      12  PF-VALUES                   PIC X       OCCURS 2.
00212
00213      EJECT
00214 *                          COPY EL6502S.
       01  EL6502AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  DATEL PIC S9(0004) COMP.
           05  DATEF PIC  X(0001).
           05  FILLER REDEFINES DATEF.
               10  DATEA PIC  X(0001).
           05  DATEI PIC  X(0008).
      *    -------------------------------
           05  TIMEL PIC S9(0004) COMP.
           05  TIMEF PIC  X(0001).
           05  FILLER REDEFINES TIMEF.
               10  TIMEA PIC  X(0001).
           05  TIMEI PIC  X(0005).
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
           05  MAINTYPL PIC S9(0004) COMP.
           05  MAINTYPF PIC  X(0001).
           05  FILLER REDEFINES MAINTYPF.
               10  MAINTYPA PIC  X(0001).
           05  MAINTYPI PIC  X(0001).
      *    -------------------------------
           05  CARRL PIC S9(0004) COMP.
           05  CARRF PIC  X(0001).
           05  FILLER REDEFINES CARRF.
               10  CARRA PIC  X(0001).
           05  CARRI PIC  X(0001).
      *    -------------------------------
           05  GROUPINL PIC S9(0004) COMP.
           05  GROUPINF PIC  X(0001).
           05  FILLER REDEFINES GROUPINF.
               10  GROUPINA PIC  X(0001).
           05  GROUPINI PIC  X(0006).
      *    -------------------------------
           05  STATEL PIC S9(0004) COMP.
           05  STATEF PIC  X(0001).
           05  FILLER REDEFINES STATEF.
               10  STATEA PIC  X(0001).
           05  STATEI PIC  X(0002).
      *    -------------------------------
           05  ACCOUNTL PIC S9(0004) COMP.
           05  ACCOUNTF PIC  X(0001).
           05  FILLER REDEFINES ACCOUNTF.
               10  ACCOUNTA PIC  X(0001).
           05  ACCOUNTI PIC  X(0010).
      *    -------------------------------
           05  EFFDTEL PIC S9(0004) COMP.
           05  EFFDTEF PIC  X(0001).
           05  FILLER REDEFINES EFFDTEF.
               10  EFFDTEA PIC  X(0001).
           05  EFFDTEI PIC  X(0008).
      *    -------------------------------
           05  EXPDTEL PIC S9(0004) COMP.
           05  EXPDTEF PIC  X(0001).
           05  FILLER REDEFINES EXPDTEF.
               10  EXPDTEA PIC  X(0001).
           05  EXPDTEI PIC  X(0008).
      *    -------------------------------
           05  CANTXTL PIC S9(0004) COMP.
           05  CANTXTF PIC  X(0001).
           05  FILLER REDEFINES CANTXTF.
               10  CANTXTA PIC  X(0001).
           05  CANTXTI PIC  X(0017).
      *    -------------------------------
           05  CANFEEL PIC S9(0004) COMP.
           05  CANFEEF PIC  X(0001).
           05  FILLER REDEFINES CANFEEF.
               10  CANFEEA PIC  X(0001).
           05  CANFEEI PIC  99.
      *    -------------------------------
           05  ODEALERL PIC S9(0004) COMP.
           05  ODEALERF PIC  X(0001).
           05  FILLER REDEFINES ODEALERF.
               10  ODEALERA PIC  X(0001).
           05  ODEALERI PIC  X(0010).
      *    -------------------------------
           05  RCAP1L PIC S9(0004) COMP.
           05  RCAP1F PIC  X(0001).
           05  FILLER REDEFINES RCAP1F.
               10  RCAP1A PIC  X(0001).
           05  RCAP1I PIC  X(0015).
      *    -------------------------------
           05  RPTCD1L PIC S9(0004) COMP.
           05  RPTCD1F PIC  X(0001).
           05  FILLER REDEFINES RPTCD1F.
               10  RPTCD1A PIC  X(0001).
           05  RPTCD1I PIC  X(0010).
      *    -------------------------------
           05  CTYCDL PIC S9(0004) COMP.
           05  CTYCDF PIC  X(0001).
           05  FILLER REDEFINES CTYCDF.
               10  CTYCDA PIC  X(0001).
           05  CTYCDI PIC  X(0004).
      *    -------------------------------
           05  AUTORFDL PIC S9(0004) COMP.
           05  AUTORFDF PIC  X(0001).
           05  FILLER REDEFINES AUTORFDF.
               10  AUTORFDA PIC  X(0001).
           05  AUTORFDI PIC  X(0001).
      *    -------------------------------
           05  RCAP2L PIC S9(0004) COMP.
           05  RCAP2F PIC  X(0001).
           05  FILLER REDEFINES RCAP2F.
               10  RCAP2A PIC  X(0001).
           05  RCAP2I PIC  X(0015).
      *    -------------------------------
           05  RPTCD2L PIC S9(0004) COMP.
           05  RPTCD2F PIC  X(0001).
           05  FILLER REDEFINES RPTCD2F.
               10  RPTCD2A PIC  X(0001).
           05  RPTCD2I PIC  X(0010).
      *    -------------------------------
           05  CNTYCDL PIC S9(0004) COMP.
           05  CNTYCDF PIC  X(0001).
           05  FILLER REDEFINES CNTYCDF.
               10  CNTYCDA PIC  X(0001).
           05  CNTYCDI PIC  X(0006).
      *    -------------------------------
           05  GRPCHKL PIC S9(0004) COMP.
           05  GRPCHKF PIC  X(0001).
           05  FILLER REDEFINES GRPCHKF.
               10  GRPCHKA PIC  X(0001).
           05  GRPCHKI PIC  X(0001).
      *    -------------------------------
           05  RCAP3L PIC S9(0004) COMP.
           05  RCAP3F PIC  X(0001).
           05  FILLER REDEFINES RCAP3F.
               10  RCAP3A PIC  X(0001).
           05  RCAP3I PIC  X(0015).
      *    -------------------------------
           05  RPTCD3L PIC S9(0004) COMP.
           05  RPTCD3F PIC  X(0001).
           05  FILLER REDEFINES RPTCD3F.
               10  RPTCD3A PIC  X(0001).
           05  RPTCD3I PIC  X(0010).
      *    -------------------------------
           05  TRUSTTYL PIC S9(0004) COMP.
           05  TRUSTTYF PIC  X(0001).
           05  FILLER REDEFINES TRUSTTYF.
               10  TRUSTTYA PIC  X(0001).
           05  TRUSTTYI PIC  X(0002).
      *    -------------------------------
           05  AHONLYL PIC S9(0004) COMP.
           05  AHONLYF PIC  X(0001).
           05  FILLER REDEFINES AHONLYF.
               10  AHONLYA PIC  X(0001).
           05  AHONLYI PIC  X(0001).
      *    -------------------------------
           05  USERL PIC S9(0004) COMP.
           05  USERF PIC  X(0001).
           05  FILLER REDEFINES USERF.
               10  USERA PIC  X(0001).
           05  USERI PIC  X(0010).
      *    -------------------------------
           05  LOANOFCL PIC S9(0004) COMP.
           05  LOANOFCF PIC  X(0001).
           05  FILLER REDEFINES LOANOFCF.
               10  LOANOFCA PIC  X(0001).
           05  LOANOFCI PIC  X(0001).
      *    -------------------------------
           05  DISMBRL PIC S9(0004) COMP.
           05  DISMBRF PIC  X(0001).
           05  FILLER REDEFINES DISMBRF.
               10  DISMBRA PIC  X(0001).
           05  DISMBRI PIC  X(0001).
      *    -------------------------------
           05  NLEVELL PIC S9(0004) COMP.
           05  NLEVELF PIC  X(0001).
           05  FILLER REDEFINES NLEVELF.
               10  NLEVELA PIC  X(0001).
           05  NLEVELI PIC  99.
      *    -------------------------------
           05  NCORRL PIC S9(0004) COMP.
           05  NCORRF PIC  X(0001).
           05  FILLER REDEFINES NCORRF.
               10  NCORRA PIC  X(0001).
           05  NCORRI PIC  X(0001).
      *    -------------------------------
           05  NPMTSL PIC S9(0004) COMP.
           05  NPMTSF PIC  X(0001).
           05  FILLER REDEFINES NPMTSF.
               10  NPMTSA PIC  X(0001).
           05  NPMTSI PIC  X(0001).
      *    -------------------------------
           05  NRPTSL PIC S9(0004) COMP.
           05  NRPTSF PIC  X(0001).
           05  FILLER REDEFINES NRPTSF.
               10  NRPTSA PIC  X(0001).
           05  NRPTSI PIC  X(0001).
      *    -------------------------------
           05  NSTATL PIC S9(0004) COMP.
           05  NSTATF PIC  X(0001).
           05  FILLER REDEFINES NSTATF.
               10  NSTATA PIC  X(0001).
           05  NSTATI PIC  X(0001).
      *    -------------------------------
           05  EMPCAPL PIC S9(0004) COMP.
           05  EMPCAPF PIC  X(0001).
           05  FILLER REDEFINES EMPCAPF.
               10  EMPCAPA PIC  X(0001).
           05  EMPCAPI PIC  X(0020).
      *    -------------------------------
           05  EMPSTMTL PIC S9(0004) COMP.
           05  EMPSTMTF PIC  X(0001).
           05  FILLER REDEFINES EMPSTMTF.
               10  EMPSTMTA PIC  X(0001).
           05  EMPSTMTI PIC  X(0001).
      *    -------------------------------
           05  USER1L PIC S9(0004) COMP.
           05  USER1F PIC  X(0001).
           05  FILLER REDEFINES USER1F.
               10  USER1A PIC  X(0001).
           05  USER1I PIC  X(0010).
      *    -------------------------------
           05  USER2L PIC S9(0004) COMP.
           05  USER2F PIC  X(0001).
           05  FILLER REDEFINES USER2F.
               10  USER2A PIC  X(0001).
           05  USER2I PIC  X(0010).
      *    -------------------------------
           05  USER3L PIC S9(0004) COMP.
           05  USER3F PIC  X(0001).
           05  FILLER REDEFINES USER3F.
               10  USER3A PIC  X(0001).
           05  USER3I PIC  X(0010).
      *    -------------------------------
           05  USER4L PIC S9(0004) COMP.
           05  USER4F PIC  X(0001).
           05  FILLER REDEFINES USER4F.
               10  USER4A PIC  X(0001).
           05  USER4I PIC  X(0010).
      *    -------------------------------
           05  USER5L PIC S9(0004) COMP.
           05  USER5F PIC  X(0001).
           05  FILLER REDEFINES USER5F.
               10  USER5A PIC  X(0001).
           05  USER5I PIC  X(0010).
      *    -------------------------------
           05  TARRATL PIC S9(0004) COMP.
           05  TARRATF PIC  X(0001).
           05  FILLER REDEFINES TARRATF.
               10  TARRATA PIC  X(0001).
           05  TARRATI PIC  99V9999.
      *    -------------------------------
           05  LIFBNRL PIC S9(0004) COMP.
           05  LIFBNRF PIC  X(0001).
           05  FILLER REDEFINES LIFBNRF.
               10  LIFBNRA PIC  X(0001).
           05  LIFBNRI PIC  99V9999.
      *    -------------------------------
           05  CDTMODL PIC S9(0004) COMP.
           05  CDTMODF PIC  X(0001).
           05  FILLER REDEFINES CDTMODF.
               10  CDTMODA PIC  X(0001).
           05  CDTMODI PIC  99V9999.
      *    -------------------------------
           05  ACCEXTL PIC S9(0004) COMP.
           05  ACCEXTF PIC  X(0001).
           05  FILLER REDEFINES ACCEXTF.
               10  ACCEXTA PIC  X(0001).
           05  ACCEXTI PIC  X(0022).
      *    -------------------------------
           05  CNTLTL PIC S9(0004) COMP.
           05  CNTLTF PIC  X(0001).
           05  FILLER REDEFINES CNTLTF.
               10  CNTLTA PIC  X(0001).
           05  CNTLTI PIC  X(0014).
      *    -------------------------------
           05  CNTLTITL PIC S9(0004) COMP.
           05  CNTLTITF PIC  X(0001).
           05  FILLER REDEFINES CNTLTITF.
               10  CNTLTITA PIC  X(0001).
           05  CNTLTITI PIC  X(0030).
      *    -------------------------------
           05  ACCEXT1L PIC S9(0004) COMP.
           05  ACCEXT1F PIC  X(0001).
           05  FILLER REDEFINES ACCEXT1F.
               10  ACCEXT1A PIC  X(0001).
           05  ACCEXT1I PIC  X(0008).
      *    -------------------------------
           05  AXNAME1L PIC S9(0004) COMP.
           05  AXNAME1F PIC  X(0001).
           05  FILLER REDEFINES AXNAME1F.
               10  AXNAME1A PIC  X(0001).
           05  AXNAME1I PIC  X(0015).
      *    -------------------------------
           05  AXDIST1L PIC S9(0004) COMP.
           05  AXDIST1F PIC  X(0001).
           05  FILLER REDEFINES AXDIST1F.
               10  AXDIST1A PIC  X(0001).
           05  AXDIST1I PIC  X(0004).
      *    -------------------------------
           05  AXDISP1L PIC S9(0004) COMP.
           05  AXDISP1F PIC  X(0001).
           05  FILLER REDEFINES AXDISP1F.
               10  AXDISP1A PIC  X(0001).
           05  AXDISP1I PIC  99V9999.
      *    -------------------------------
           05  AXLIFT1L PIC S9(0004) COMP.
           05  AXLIFT1F PIC  X(0001).
           05  FILLER REDEFINES AXLIFT1F.
               10  AXLIFT1A PIC  X(0001).
           05  AXLIFT1I PIC  X(0005).
      *    -------------------------------
           05  AXLIFP1L PIC S9(0004) COMP.
           05  AXLIFP1F PIC  X(0001).
           05  FILLER REDEFINES AXLIFP1F.
               10  AXLIFP1A PIC  X(0001).
           05  AXLIFP1I PIC  99V9999.
      *    -------------------------------
           05  ACCEXT2L PIC S9(0004) COMP.
           05  ACCEXT2F PIC  X(0001).
           05  FILLER REDEFINES ACCEXT2F.
               10  ACCEXT2A PIC  X(0001).
           05  ACCEXT2I PIC  X(0008).
      *    -------------------------------
           05  AXNAME2L PIC S9(0004) COMP.
           05  AXNAME2F PIC  X(0001).
           05  FILLER REDEFINES AXNAME2F.
               10  AXNAME2A PIC  X(0001).
           05  AXNAME2I PIC  X(0015).
      *    -------------------------------
           05  AXDIST2L PIC S9(0004) COMP.
           05  AXDIST2F PIC  X(0001).
           05  FILLER REDEFINES AXDIST2F.
               10  AXDIST2A PIC  X(0001).
           05  AXDIST2I PIC  X(0004).
      *    -------------------------------
           05  AXDISP2L PIC S9(0004) COMP.
           05  AXDISP2F PIC  X(0001).
           05  FILLER REDEFINES AXDISP2F.
               10  AXDISP2A PIC  X(0001).
           05  AXDISP2I PIC  99V9999.
      *    -------------------------------
           05  AXLIFT2L PIC S9(0004) COMP.
           05  AXLIFT2F PIC  X(0001).
           05  FILLER REDEFINES AXLIFT2F.
               10  AXLIFT2A PIC  X(0001).
           05  AXLIFT2I PIC  X(0005).
      *    -------------------------------
           05  AXLIFP2L PIC S9(0004) COMP.
           05  AXLIFP2F PIC  X(0001).
           05  FILLER REDEFINES AXLIFP2F.
               10  AXLIFP2A PIC  X(0001).
           05  AXLIFP2I PIC  99V9999.
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0070).
      *    -------------------------------
           05  ERRMSG2L PIC S9(0004) COMP.
           05  ERRMSG2F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG2F.
               10  ERRMSG2A PIC  X(0001).
           05  ERRMSG2I PIC  X(0070).
      *    -------------------------------
           05  PFENTERL PIC S9(0004) COMP.
           05  PFENTERF PIC  X(0001).
           05  FILLER REDEFINES PFENTERF.
               10  PFENTERA PIC  X(0001).
           05  PFENTERI PIC  99.
      *    -------------------------------
           05  FLIPFKL PIC S9(0004) COMP.
           05  FLIPFKF PIC  X(0001).
           05  FILLER REDEFINES FLIPFKF.
               10  FLIPFKA PIC  X(0001).
           05  FLIPFKI PIC  X(0025).
       01  EL6502AO REDEFINES EL6502AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMPNYIDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GROUPINO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCOUNTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXPDTEO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CANTXTO PIC  X(0017).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CANFEEO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ODEALERO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RCAP1O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RPTCD1O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CTYCDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AUTORFDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RCAP2O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RPTCD2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CNTYCDO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRPCHKO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RCAP3O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RPTCD3O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TRUSTTYO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHONLYO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOANOFCO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DISMBRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NLEVELO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NCORRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NPMTSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NRPTSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NSTATO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EMPCAPO PIC  X(0020).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EMPSTMTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USER1O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USER2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USER3O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USER4O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USER5O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TARRATO PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LIFBNRO PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CDTMODO PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCEXTO PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CNTLTO PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CNTLTITO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCEXT1O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AXNAME1O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AXDIST1O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AXDISP1O PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AXLIFT1O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AXLIFP1O PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCEXT2O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AXNAME2O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AXDIST2O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AXDISP2O PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AXLIFT2O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AXLIFP2O PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG2O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FLIPFKO PIC  X(0025).
      *    -------------------------------
00215  01  FILLER REDEFINES EL6502AI.
101101     12  FILLER                      PIC X(306).
00217      12  W-MAP-REST                  PIC X(199).
00218
00219      EJECT
      ****************************************************************
      *                                                               
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 
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
         02  DFHEIV97              PIC S9(7) COMP-3 VALUE ZERO.       
         02  DFHEIV98              PIC S9(4) COMP SYNC VALUE ZERO.    
         02  FILLER                PIC X(02).                         
         02  DFHEIV99              PIC X(08) VALUE SPACE.             
         02  DFHEIVL0              PIC X(48) VALUE SPACE.             
         02  DFHEIVL1              PIC X(48) VALUE SPACE.             
         02  DFHEIVL2              PIC X(48) VALUE SPACE.             
         02  DFHEIVL3              PIC X(48) VALUE SPACE.             
       LINKAGE  SECTION.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 *
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
           02  eibresp          pic 9(09) comp.
           02  eibresp2         pic 9(09) comp.
           02  dfheigdj         pic s9(4) comp.
           02  dfheigdk         pic s9(4) comp.
00221  01  DFHCOMMAREA                     PIC X(1500).
00222
00223      EJECT
00224 *01 PARMLIST .
00225 *    02  FILLER                      PIC S9(8)   COMP.
00226 *    02  ERACCT-POINTER              PIC S9(8)   COMP.
00227 *    02  ERRATE-POINTER              PIC S9(8)   COMP.
00228 *    02  ELCNTL-POINTER              PIC S9(8)   COMP.
00229 *    02  ERCOMP-POINTER              PIC S9(8)   COMP.
00230
00231 *                          COPY ERCACCT.
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
00232      EJECT
00233 *                          COPY ERCRATE.
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
00234      EJECT
00235 *                          COPY ELCCNTL.
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
00236      EJECT
00237 *                          COPY ERCCOMP.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCOMP                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.019                          *
00006 *                                                                *
00007 *   ONLINE CREDIT SYSTEM                                         *
00008 *                                                                *
00009 *   FILE DESCRIPTION = COMPENSATION MASTER                       *
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 700   RECFORM = FIXED                          *
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ERCOMP                   RKP=2,LEN=29    *
00015 *       ALTERNATE PATH = NONE                                    *
00016 *                                                                *
00017 *   LOG = NO                                                     *
00018 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00019 *                                                                *
00020 ******************************************************************
100703*                   C H A N G E   L O G
100703*
100703* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
100703*-----------------------------------------------------------------
100703*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
100703* EFFECTIVE    NUMBER
100703*-----------------------------------------------------------------
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
041105* 041105    2005031100003  PEMA  ADD TYPE CODE FOR BANKS
092205* 092205    2005050300006  PEMA  ADD LEASE FEE
032406* 032406    2006022800001  AJRA  ADD FIRST WRITTEN DATE
072406* 072406    2006022400001  PEMA  ADD REF EDIT FLD ON B RECS
062907* 062907    2004020600003  PEMA  ADD WITHOLDING PERCENT
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
020310* 020310  CR2008100900004  PEMA  ADD REF4 EXTRACT PROCESSING
071712* 071712  CR2012042700005  PEMA  ADD OVER 120 FOR AHL ONLY
100703******************************************************************
00021
00022  01  COMPENSATION-MASTER.
00023      12  CO-RECORD-ID                          PIC XX.
00024          88  VALID-CO-ID                          VALUE 'CO'.
00025
00026      12  CO-CONTROL-PRIMARY.
00027          16  CO-COMPANY-CD                     PIC X.
00028          16  CO-CONTROL.
00029              20  CO-CTL-1.
00030                  24  CO-CARR-GROUP.
00031                      28  CO-CARRIER            PIC X.
00032                      28  CO-GROUPING.
00033                          32  CO-GROUP-PREFIX   PIC XXX.
00034                          32  CO-GROUP-PRIME    PIC XXX.
00035                  24  CO-RESP-NO.
00036                      28  CO-RESP-PREFIX        PIC X(4).
00037                      28  CO-RESP-PRIME         PIC X(6).
00038              20  CO-CTL-2.
00039                  24  CO-ACCOUNT.
00040                      28  CO-ACCT-PREFIX        PIC X(4).
00041                      28  CO-ACCT-PRIME         PIC X(6).
00042          16  CO-TYPE                           PIC X.
00043              88  CO-COMPANY-TYPE                  VALUE 'C'.
041105             88  CO-GEN-AGENT-TYPE     VALUE 'G' 'B'.
00045              88  CO-ACCOUNT-TYPE                  VALUE 'A'.
00046
00047      12  CO-MAINT-INFORMATION.
00048          16  CO-LAST-MAINT-DT                  PIC XX.
00049          16  CO-LAST-MAINT-HHMMSS              PIC S9(7)  COMP-3.
00050          16  CO-LAST-MAINT-USER                PIC X(4).
011410     12  FILLER                                PIC XX.
020210     12  CO-STMT-TYPE                          PIC XXX.
011410     12  CO-COMP-TYPE                          PIC X.
011410         88  CO-COMP-IS-SPPDD                    VALUE '1'.
           12  CO-STMT-OWNER                         PIC X(4).
00053      12  CO-BALANCE-CONTROL                    PIC X.
00054          88  CO-CARRY-BALANCE                     VALUE 'Y'.
00055          88  CO-NO-BALANCE                        VALUE 'N'.
00056
00057      12  CO-INTERNAL-CONTROL-1                 PIC X.
00058          88  CO-AUTO-GENERATED-THIS-RUN           VALUE 'X'.
00059          88  CO-AUTO-GENERATED                    VALUE 'Y'.
00060          88  CO-NOT-AUTO-GENERATED                VALUE 'N'.
00061
00062      12  CO-INTERNAL-CONTROL-2                 PIC X.
00063          88  CO-STATEMENT-THIS-RUN                VALUE 'Y'.
00064          88  CO-NO-STATEMENT-THIS-RUN             VALUE 'N'.
00065
062907     12  CO-GA-WITHOLD-PCT                     PIC S9V9999 COMP-3.
062907     12  CO-GA-DIRECT-DEP                      PIC X.
062907     12  CO-FUTURE-SPACE                       PIC X.
062907         88  CO-FUTURE-NOT-USED                   VALUE ' '.
00068
00069      12  CO-ACCT-NAME                          PIC X(30).
00070      12  CO-MAIL-NAME                          PIC X(30).
00071      12  CO-ADDR-1                             PIC X(30).
00072      12  CO-ADDR-2                             PIC X(30).
CIDMOD     12  CO-ADDR-3.
               16  CO-ADDR-CITY                      PIC X(27).
               16  CO-ADDR-STATE                     PIC XX.
CIDMOD     12  CO-CSO-1099                           PIC X.
00074      12  CO-ZIP.
00075          16  CO-ZIP-PRIME.
00076              20  CO-ZIP-PRI-1ST                PIC X.
00077                  88  CO-CANADIAN-POST-CODE  VALUE 'A' THRU 'Z'.
00078              20  FILLER                        PIC X(4).
00079          16  CO-ZIP-PLUS4                      PIC X(4).
00080      12  CO-CANADIAN-POSTAL-CODE  REDEFINES  CO-ZIP.
00081          16  CO-CAN-POSTAL-1                   PIC XXX.
00082          16  CO-CAN-POSTAL-2                   PIC XXX.
00083          16  FILLER                            PIC XXX.
00084      12  CO-SOC-SEC                            PIC X(13).
00085      12  CO-TELEPHONE.
00086          16  CO-AREA-CODE                      PIC XXX.
00087          16  CO-PREFIX                         PIC XXX.
00088          16  CO-PHONE                          PIC X(4).
00089
00090      12  CO-ROLADEX-PRINT-DT                   PIC XX.
00091
00092      12  CO-AR-BAL-LEVEL                       PIC X.
00093          88  CO-AR-REF-LVL                        VALUE '1'.
00094          88  CO-AR-BILL-REF-LVL                   VALUE '1'.
00095          88  CO-AR-BILL-LVL                       VALUE '2'.
00096          88  CO-AR-AGT-LVL                        VALUE '3'.
00097          88  CO-AR-FR-LVL                         VALUE '4'.
00098
00099      12  CO-AR-NORMAL-PRINT                    PIC X.
00100          88  CO-AR-BILL-IS-PRINTED                VALUE 'Y'.
00101          88  CO-AR-BILL-NOT-PRINTED               VALUE 'N'.
00102
00103      12  CO-AR-SUMMARY-CODE                    PIC X(6).
00104
00105      12  CO-AR-REPORTING                       PIC X.
00106          88  CO-AR-NET-REPORT                     VALUE 'N'.
00107          88  CO-AR-GROSS-REPORT                   VALUE 'G'.
00108
00109      12  CO-AR-PULL-CHECK                      PIC X.
00110          88  CO-AR-CHECKS-PULLED                  VALUE 'Y'.
00111          88  CO-AR-CHECKS-NOT-PULLED              VALUE 'N'.
00112
00113      12  CO-AR-BALANCE-PRINT                   PIC X.
00114          88  CO-AR-PRINT-NO-BALANCE               VALUE 'N'.
00115
00116      12  CO-AR-LAST-RUN-CODE                   PIC X.
00117          88  CO-AR-LAST-RUN-ANNUAL                VALUE 'A'.
00118          88  CO-AR-LAST-RUN-CYCLE                 VALUE 'C'.
00119          88  CO-AR-LAST-RUN-EOM                   VALUE 'M'.
00120
00121      12  CO-LAST-EOM-STMT-DT                   PIC XX.
00122
00123      12  CO-USER-CODE                          PIC X.
00124      12  CO-REPORT-GROUP-ID                    PIC X(12).
00125
00126 ******************************************************************
00127 *    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN THE TOTALS AS OF
00128 *    THE LAST MONTH END RUN.
00129 ******************************************************************
00130
00131      12  CO-LAST-ACTIVITY-DATE.
00132          16  CO-ACT-YEAR                       PIC 99.
00133          16  CO-ACT-MONTH                      PIC 99.
00134          16  CO-ACT-DAY                        PIC 99.
00135
00136      12  CO-LAST-STMT-DT.
00137          16  CO-LAST-STMT-YEAR                 PIC 99.
00138          16  CO-LAST-STMT-MONTH                PIC 99.
00139          16  CO-LAST-STMT-DAY                  PIC 99.
00140
00141      12  CO-MO-END-TOTALS.
00142          16  CO-MONTHLY-TOTALS.
00143              20  CO-BAL-FWD                PIC S9(7)V99   COMP-3.
00144              20  CO-CUR-COM                PIC S9(7)V99   COMP-3.
00145              20  CO-CUR-CHG                PIC S9(7)V99   COMP-3.
00146              20  CO-CUR-PMT                PIC S9(7)V99   COMP-3.
00147              20  CO-END-BAL                PIC S9(7)V99   COMP-3.
00148
00149          16  CO-AGING-TOTALS.
00150              20  CO-CUR                    PIC S9(7)V99   COMP-3.
00151              20  CO-OV30                   PIC S9(7)V99   COMP-3.
00152              20  CO-OV60                   PIC S9(7)V99   COMP-3.
00153              20  CO-OV90                   PIC S9(7)V99   COMP-3.
00154
00155          16  CO-YTD-TOTALS.
00156              20  CO-YTD-COM                PIC S9(7)V99   COMP-3.
00157              20  CO-YTD-OV                 PIC S9(7)V99   COMP-3.
00158
00159          16  CO-OVER-UNDER-TOTALS.
00160              20  CO-CUR-OVR-UNDR           PIC S9(7)V99   COMP-3.
00161              20  CO-YTD-OVR-UNDR           PIC S9(7)V99   COMP-3.
00162
00163      12  CO-MISCELLANEOUS-TOTALS.
00164          16  CO-FICA-TOTALS.
00165              20  CO-CUR-FICA               PIC S9(7)V99   COMP-3.
00166              20  CO-YTD-FICA               PIC S9(7)V99   COMP-3.
00167
00168          16  CO-CLAIM-TOTALS.
00169              20  CO-LF-CLM-AMT             PIC S9(9)V99   COMP-3.
00170              20  CO-AH-CLM-AMT             PIC S9(9)V99   COMP-3.
00171
00172 ******************************************************************
00173 *    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN TOTALS THAT
00174 *    REPRESENT CURRENT MONTH (TOTALS OF CYCLES).
00175 ******************************************************************
00176
00177      12  CO-CURRENT-TOTALS.
00178          16  CO-CURRENT-LAST-STMT-DT.
00179              20  CO-CURRENT-LAST-STMT-YEAR     PIC 99.
00180              20  CO-CURRENT-LAST-STMT-MONTH    PIC 99.
00181              20  CO-CURRENT-LAST-STMT-DAY      PIC 99.
00182
00183          16  CO-CURRENT-MONTHLY-TOTALS.
00184              20  CO-CURRENT-BAL-FWD        PIC S9(7)V99   COMP-3.
00185              20  CO-CURRENT-CUR-COM        PIC S9(7)V99   COMP-3.
00186              20  CO-CURRENT-CUR-CHG        PIC S9(7)V99   COMP-3.
00187              20  CO-CURRENT-CUR-PMT        PIC S9(7)V99   COMP-3.
00188              20  CO-CURRENT-END-BAL        PIC S9(7)V99   COMP-3.
00189
00190          16  CO-CURRENT-AGING-TOTALS.
00191              20  CO-CURRENT-CUR            PIC S9(7)V99   COMP-3.
00192              20  CO-CURRENT-OV30           PIC S9(7)V99   COMP-3.
00193              20  CO-CURRENT-OV60           PIC S9(7)V99   COMP-3.
00194              20  CO-CURRENT-OV90           PIC S9(7)V99   COMP-3.
00195
00196          16  CO-CURRENT-YTD-TOTALS.
00197              20  CO-CURRENT-YTD-COM        PIC S9(7)V99   COMP-3.
00198              20  CO-CURRENT-YTD-OV         PIC S9(7)V99   COMP-3.
00199
00200      12  CO-PAID-COMM-TOTALS.
00201          16  CO-YTD-PAID-COMMS.
00202              20  CO-YTD-PAID-COM           PIC S9(7)V99   COMP-3.
00203              20  CO-YTD-PAID-OV            PIC S9(7)V99   COMP-3.
00204
00205      12  CO-CURRENT-MONTH-ACTIVITY         PIC X.
00206          88  CO-HAS-CURR-MONTH-ACTIVITY       VALUE 'Y'.
00207          88  CO-NO-CURR-MONTH-ACTIVITY        VALUE 'N'.
00208
00209      12  CO-DELINQUENT-LETTER-CODE         PIC X.
00210          88  CO-ACCOUNT-1ST-LETTER            VALUE 'A'.
00211          88  CO-ACCOUNT-2ND-LETTER            VALUE 'B'.
00212          88  CO-AGENT-1ST-LETTER              VALUE 'B'.
00213          88  CO-AGENT-2ND-LETTER              VALUE 'G'.
00214          88  CO-OVERWRITE-LETTER              VALUE 'O'.
00215          88  CO-MEMO-TO-REGION-MGR            VALUE 'M'.
00216          88  CO-FINAL-LETTER                  VALUE 'F'.
00217          88  CO-RECONCILING                   VALUE 'R'.
00218          88  CO-PHONE-CALL                    VALUE 'P'.
00219          88  CO-LEGAL                         VALUE 'L'.
00220          88  CO-COLLECTION-AGENCY             VALUE 'C'.
00221          88  CO-WRITE-OFF                     VALUE 'W'.
00222          88  CO-NO-ACTION                     VALUE 'N' ' '.
00223
00224      12  CO-CSR-CODE                       PIC X(4).
00225
00226      12  CO-GA-STATUS-INFO.
00227          16  CO-GA-EFFECTIVE-DT            PIC XX.
00228          16  CO-GA-TERMINATION-DT          PIC XX.
00229          16  CO-GA-STATUS-CODE             PIC X.
00230              88  CO-GA-ACTIVE                 VALUE 'A'.
00231              88  CO-GA-INACTIVE               VALUE 'I'.
00232              88  CO-GA-PENDING                VALUE 'P'.
00233          16  CO-GA-COMMENTS.
00234              20  CO-GA-COMMENT-1           PIC X(40).
00235              20  CO-GA-COMMENT-2           PIC X(40).
00236              20  CO-GA-COMMENT-3           PIC X(40).
00237              20  CO-GA-COMMENT-4           PIC X(40).
00238
00239      12  CO-RPTCD2                         PIC X(10).
071712     12  CO-AHL-OVER120-DATA REDEFINES CO-RPTCD2.
071712         16  CO-OV120                      PIC S9(7)V99   COMP-3.
071712         16  CO-CURRENT-OV120              PIC S9(7)V99   COMP-3.
00240
00241      12  CO-TYPE-AGENT                     PIC X(01).
00242          88  CO-CORPORATION                   VALUE 'C'.
00243          88  CO-PARTNERSHIP                   VALUE 'P'.
00244          88  CO-SOLE-PROPRIETOR               VALUE 'S'.
00245          88  CO-TRUST                         VALUE 'T'.
00246          88  CO-UNKNOWN                       VALUE ' ' 'X'.
00247
00248      12  CO-FAXNO.
00249          16  CO-FAX-AREA-CODE                  PIC XXX.
00250          16  CO-FAX-PREFIX                     PIC XXX.
00251          16  CO-FAX-PHONE                      PIC X(4).
00252
00253      12  CO-BANK-INFORMATION.
00254          16  CO-BANK-TRANSIT-NO                PIC X(8).
00255          16  CO-BANK-TRANSIT-NON REDEFINES
00256              CO-BANK-TRANSIT-NO                PIC 9(8).
00257
00258          16  CO-BANK-ACCOUNT-NUMBER            PIC X(17).
           12  CO-MISC-DEDUCT-INFO REDEFINES
                        CO-BANK-INFORMATION.
               16  CO-MD-GL-ACCT                     PIC X(10).
               16  CO-MD-DIV                         PIC XX.
               16  CO-MD-CENTER                      PIC X(4).
               16  CO-MD-AMT                        PIC S9(5)V99 COMP-3.
092707         16  CO-CREATE-AP-CHECK                PIC X.
092707         16  CO-DELIVER-CK-TO-MEL              PIC X.
092707         16  FILLER                            PIC XXX.
00259      12  CO-ACH-STATUS                         PIC X.
00260          88  CO-ACH-ACTIVE                         VALUE 'A'.
00261          88  CO-ACH-PENDING                        VALUE 'P'.
00262
CIDMOD     12  CO-BILL-SW                            PIC X.
CIDMOD     12  CO-CONTROL-NAME                       PIC X(30).
092205     12  CO-MAX-BANK-FEE-LEASE                 PIC S999V99 COMP-3.
111504     12  CO-MAX-BANK-FEE                       PIC S999V99 COMP-3.
100703     12  CO-CLP-STATE                          PIC XX.
032406     12  CO-FIRST-WRITTEN-DT                   PIC XX.
072406     12  CO-SPP-REFUND-EDIT                    PIC X.
00264
00265 ******************************************************************
00238      EJECT
00239
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA ACCOUNT-MASTER
                                RATE-RECORD CONTROL-FILE
                                COMPENSATION-MASTER.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL6502' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00241
00242      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00243      MOVE '5'                    TO  DC-OPTION-CODE.
00244      PERFORM 9700-LINK-DATE-CONVERT
                                       THRU 9700-EXIT
00245      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00246      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00247      MOVE DC-GREG-DATE-1-YMD     TO  YMD-CURRENT-SAVE.
00248
00249      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00250      MOVE +2                     TO  EMI-NUMBER-OF-LINES.
00251
00252      IF EIBCALEN = 0
00253          GO TO 8800-UNAUTHORIZED-ACCESS.
00254
00255      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00256          IF PI-CALLING-PROGRAM = XCTL-6501
00257              MOVE PI-CALLING-PROGRAM
00258                                  TO  PI-RETURN-TO-PROGRAM
00259              MOVE THIS-PGM       TO  PI-CALLING-PROGRAM
00260          ELSE
00261              MOVE THIS-PGM       TO  PI-CALLING-PROGRAM.
00262
00263      PERFORM 7800-COMPANY-REC-READ THRU 7899-EXIT.
00264
00265      MOVE LOW-VALUES             TO  EL6502AI.
00266
00267      IF EIBTRNID NOT = TRANS-ID
00268          MOVE PI-MAINT           TO  MAINTYPO
00269          MOVE AL-UANON           TO  MAINTYPA
00270          MOVE -1                 TO  MAINTYPL
00271          IF PI-MAINT = 'S' OR 'C'
00272              GO TO 4000-SHOW
00273          ELSE
00274              IF PI-MAINT = 'A'
00275                  MOVE 'C'            TO  PI-MAINT
00276                  GO TO 4000-SHOW
00277              ELSE
00278                  GO TO 8100-SEND-INITIAL-MAP.
00279
00280      
      * EXEC CICS HANDLE CONDITION
00281 *        PGMIDERR  (9600-PGMID-ERROR)
00282 *        ERROR     (9990-ABEND)
00283 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00004221' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303034323231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00284
00285      IF EIBAID = DFHCLEAR
00286          GO TO 9400-CLEAR.
00287
00288      EJECT
00289  0200-RECEIVE.
00290      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00291          MOVE ER-0008            TO  EMI-ERROR
00292          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00293          MOVE -1                 TO  PFENTERL
00294          GO TO 8200-SEND-DATAONLY.
00295
00296      
      * EXEC CICS RECEIVE
00297 *        MAP      (MAP-NAME)
00298 *        MAPSET   (MAPSET-NAME)
00299 *        INTO     (EL6502AI)
00300 *    END-EXEC.
           MOVE LENGTH OF
            EL6502AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00004237' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6502AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00301
00302      IF PFENTERL = 0
00303          GO TO 0300-CHECK-PFKEYS.
00304      IF EIBAID NOT = DFHENTER
00305          MOVE ER-0004            TO  EMI-ERROR
00306          GO TO 0320-INPUT-ERROR.
00307      IF (PFENTERI NUMERIC) AND (PFENTERI GREATER 0 AND LESS 25)
00308          MOVE PF-VALUES (PFENTERI) TO  EIBAID
00309      ELSE
00310          MOVE ER-0029            TO  EMI-ERROR
00311          GO TO 0320-INPUT-ERROR.
00312
00313      EJECT
00314  0300-CHECK-PFKEYS.
00315      IF EIBAID = DFHPF23
00316          GO TO 8810-PF23.
00317      IF EIBAID = DFHPF24
00318          GO TO 9200-RETURN-MAIN-MENU.
00319      IF EIBAID = DFHPF12
00320          GO TO 9500-PF12.
00321      IF EIBAID = DFHPF5
00322          MOVE XCTL-6501          TO  PGM-NAME
00323          GO TO 9300-XCTL.
00324      IF EIBAID = DFHPF7
00325          MOVE XCTL-6504          TO  PGM-NAME
00326          GO TO 9300-XCTL.
00327      IF EIBAID = DFHPF8
00328          MOVE XCTL-6506          TO  PGM-NAME
00329          GO TO 9300-XCTL.
00330      IF EIBAID = DFHPF9
00331          MOVE XCTL-6505          TO  PGM-NAME
00332          GO TO 9300-XCTL.
00333      IF (EIBAID = DFHPF10)  AND
00334         (PI-COMPANY-ID = 'FLI' OR 'FLU' OR 'LGX')
00335          MOVE XCTL-6508          TO  PGM-NAME
00336          GO TO 9300-XCTL.
00337
00338      IF EIBAID = DFHENTER
00339          GO TO 0330-CHECK-MAINTYP.
00340
00341      MOVE ER-0029                TO  EMI-ERROR.
00342  0320-INPUT-ERROR.
00343      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00344      MOVE AL-UNBON               TO  PFENTERA.
00345      MOVE -1                     TO  PFENTERL.
00346      GO TO 8200-SEND-DATAONLY.
00347
00348  0330-CHECK-MAINTYP.
00349      IF MAINTYPL GREATER ZERO
00350          IF MAINTYPI = 'S' OR 'C' OR 'A'
00351              MOVE AL-UANON       TO  MAINTYPA
00352              MOVE MAINTYPI       TO  PI-MAINT
00353          ELSE
00354              MOVE -1             TO  MAINTYPL
00355              MOVE AL-UABON       TO  MAINTYPA
00356              MOVE ER-2039        TO  EMI-ERROR
00357              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00358              GO TO 8200-SEND-DATAONLY
00359      ELSE
00360          MOVE -1                 TO  MAINTYPL
00361          MOVE AL-UABON           TO  MAINTYPA
00362          MOVE ER-2039            TO  EMI-ERROR
00363          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00364          GO TO 8200-SEND-DATAONLY.
00365
00366      IF PI-MAINT = 'S'
00367          GO TO 4000-SHOW.
00368
CIDMOD     PERFORM 7800-COMPANY-REC-READ THRU 7899-EXIT.
CIDMOD
00369      IF EMI-ERROR NOT = ZEROS
00370          MOVE -1                 TO  MAINTYPL
00371          GO TO 8200-SEND-DATAONLY.
00372
00373      GO TO 4200-MAINT.
00374
00375      EJECT
00376
00377  4000-SHOW.
CIDMOD     PERFORM 7800-COMPANY-REC-READ THRU 7899-EXIT.
CIDMOD
00379      PERFORM 7100-READ-ERACCT THRU 7100-EXIT.
00380      MOVE LOW-VALUES             TO  EL6502AO.
00381      GO TO 5000-BUILD-INITIAL-SCREEN.
00382
00383      EJECT
00384  4200-MAINT.
00385      IF NOT MODIFY-CAP
00386          MOVE 'UPDATE'       TO SM-READ
00387          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00388          MOVE ER-0070             TO  EMI-ERROR
00389          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00390          GO TO 8100-SEND-INITIAL-MAP.
00391
00392      PERFORM 7000-EDIT THRU 7099-EXIT.
00393
00394      IF EMI-NO-ERRORS
00395          NEXT SENTENCE
00396      ELSE
00397          IF EMI-FORCABLE OR EMI-FATAL
00398             GO TO 8200-SEND-DATAONLY.
00399
00400      PERFORM 7300-READ-ERACCT-UPDATE THRU 7300-EXIT.
00401
00402      PERFORM 6000-CHECK-FOR-UPDATE   THRU 6049-EXIT.
00403
00404      IF AM-LAST-MAINT-USER   = PI-UPDATE-BY OR
00405         AM-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS
00406          NEXT SENTENCE
00407      ELSE
00408          
      * EXEC CICS UNLOCK
00409 *             DATASET  (ERACCT-FILE)
00410 *        END-EXEC
      *    MOVE '&*                    #   #00004352' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-FILE, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00411          MOVE ER-0068            TO  EMI-ERROR
00412          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00413          PERFORM 7100-READ-ERACCT  THRU 7100-EXIT
00414          MOVE LOW-VALUES         TO  EL6502AO
00415          MOVE -1                 TO  MAINTYPL
00416          MOVE 'S'                TO  PI-MAINT
00417          GO TO 5000-BUILD-INITIAL-SCREEN.
00418
00419      MOVE PI-PROCESSOR-ID        TO  AM-LAST-MAINT-USER.
00420      MOVE EIBTIME                TO  AM-LAST-MAINT-HHMMSS.
00421      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00422      MOVE '5'                    TO  DC-OPTION-CODE.
00423      MOVE LINK-ELDATCV           TO  PGM-NAME.
00424
00425      
      * EXEC CICS LINK
00426 *        PROGRAM (PGM-NAME)
00427 *        COMMAREA(DATE-CONVERSION-DATA)
00428 *        LENGTH  (DC-COMM-LENGTH)
00429 *    END-EXEC.
      *    MOVE '."C                   (   #00004369' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00430
00431      MOVE DC-BIN-DATE-1          TO  AM-LAST-MAINT-DT
00432                                      BIN-CURRENT-SAVE.
00433
00434      
      * EXEC CICS REWRITE
00435 *        DATASET  (ERACCT-FILE)
00436 *        FROM     (ACCOUNT-MASTER)
00437 *    END-EXEC.
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004378' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-FILE, 
                 ACCOUNT-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00438
00439      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.
00440      MOVE ER-0000                TO  EMI-ERROR.
00441      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00442
00443      PERFORM 7100-READ-ERACCT THRU 7100-EXIT.
00444      MOVE LOW-VALUES             TO  EL6502AO.
00445      MOVE 'C'                    TO  PI-MAINT.
00446
00447      EJECT
00448
00449  5000-BUILD-INITIAL-SCREEN.
092711     MOVE AM-ORIG-DEALER-NO      TO ODEALERO.
00450      MOVE AM-REPORT-CODE-1       TO RPTCD1O.
00451      MOVE AM-CITY-CODE           TO CTYCDO.
00452      MOVE AM-AUTO-REFUND-SW      TO AUTORFDO.
00453
00454      MOVE AM-REPORT-CODE-2       TO RPTCD2O.
120706     MOVE AM-REPORT-CODE-3       TO RPTCD3O
00455      MOVE AM-COUNTY-PARISH       TO CNTYCDO.
00456
00457      MOVE AM-USER-FIELDS         TO USERO.
00458      MOVE AM-TRUST-TYPE          TO TRUSTTYO.
00459
00460      IF AM-AH-ONLY-INDICATOR = 'N'
00461         MOVE 'N'                 TO AHONLYO
00462      ELSE
00463         MOVE 'Y'                 TO AHONLYO.
00464
00465      IF AM-EDIT-LOAN-OFC = 'Y'
00466          MOVE 'Y'                TO LOANOFCO
00467      ELSE
00468          MOVE 'N'                TO LOANOFCO.
00469
00470      IF AM-DISMBR-COVERAGE-SW = 'Y'
00471          MOVE 'Y'                TO DISMBRO
00472      ELSE
00473          MOVE 'N'                TO DISMBRO.
00474
00475      IF AM-3RD-PARTY-NOTIF-LEVEL NOT NUMERIC
00476         MOVE ZEROS TO AM-3RD-PARTY-NOTIF-LEVEL.
00477
00478      MOVE AM-3RD-PARTY-NOTIF-LEVEL
00479                                  TO NLEVELO.
00480
00481      IF AM-NOTIF-OF-LETTERS = 'Y'
00482         MOVE 'Y'                 TO NCORRO
00483      ELSE
00484         MOVE 'N'                 TO NCORRO.
00485
00486      IF AM-NOTIF-OF-PAYMENTS = 'Y'
00487         MOVE 'Y'                 TO NPMTSO
00488      ELSE
00489         MOVE 'N'                 TO NPMTSO.
00490
00491      IF AM-NOTIF-OF-REPORTS = 'Y'
00492         MOVE 'Y'                 TO NRPTSO
00493      ELSE
00494         MOVE 'N'                 TO NRPTSO.
00495
00496      IF AM-NOTIF-OF-STATUS = 'Y'
00497         MOVE 'Y'                 TO NSTATO
00498      ELSE
00499         MOVE 'N'                 TO NSTATO.
00500
00501      IF AM-GROUPED-CHECKS-Y-N = 'Y'
00502         MOVE 'Y'                 TO GRPCHKO
00503      ELSE
00504         MOVE 'N'                 TO GRPCHKO.
00505
00506      IF AM-EMPLOYER-STMT-USED = 'Y' OR '1' OR '2' OR '3'
00507         MOVE AM-EMPLOYER-STMT-USED  TO EMPSTMTO
00508      ELSE
00509         MOVE 'N'                 TO EMPSTMTO.
00510
00511      IF  AM-TARGET-LOSS-RATIO NUMERIC
00512          MOVE AM-TARGET-LOSS-RATIO
00513                                  TO TARRATO
00514      ELSE
00515          MOVE ZEROS              TO TARRATO.
00516
00517      IF  AM-LIFE-IBNR-PCT NUMERIC
00518          MOVE AM-LIFE-IBNR-PCT
00519                                  TO LIFBNRO
00520      ELSE
00521          MOVE ZEROS              TO LIFBNRO.
00522
00523      IF  AM-CRDT-MODIFICATION-PCT NUMERIC
00524          MOVE AM-CRDT-MODIFICATION-PCT
00525                                  TO CDTMODO
00526      ELSE
00527          MOVE ZEROS              TO CDTMODO.
00528
070109*  THE FOLLOWING IS FOR JHL ONLY AND IS NOT USED IN THE BATCH SYST
00529      IF  AM-EXEC1-DIS-PERCENT NUMERIC
00530          MOVE AM-EXEC1-DIS-PERCENT
00531                                  TO AXDISP1O.
00532
00533      IF  AM-EXEC1-LIFE-PERCENT NUMERIC
00534          MOVE AM-EXEC1-LIFE-PERCENT
00535                                  TO AXLIFP1O.
00536
00537      IF  AM-EXEC2-DIS-PERCENT NUMERIC
00538          MOVE AM-EXEC2-DIS-PERCENT
00539                                  TO AXDISP2O.
00540
00541      IF  AM-EXEC2-LIFE-PERCENT NUMERIC
00542          MOVE AM-EXEC2-LIFE-PERCENT
00543                                  TO AXLIFP2O.
00544
00545      MOVE AM-EXEC1-NAME          TO AXNAME1O.
00546      MOVE AM-EXEC2-NAME          TO AXNAME2O.
00547      MOVE AM-CONTROL-NAME        TO CNTLTITO.
00548
00549      MOVE AM-USER-SELECT-1       TO USER1O.
00550      MOVE AM-USER-SELECT-2       TO USER2O.
00551      MOVE AM-USER-SELECT-3       TO USER3O.
00552      MOVE AM-USER-SELECT-4       TO USER4O.
00553      MOVE AM-USER-SELECT-5       TO USER5O.
00554
00555      MOVE PI-MAINT               TO  MAINTYPO.
00556      MOVE -1                     TO  MAINTYPL.
00557      MOVE AL-UANON               TO  MAINTYPA.
00558
CIDMOD     IF WS-SAVE-REPORT-CODE1 NOT = SPACES AND LOW-VALUES
CIDMOD         MOVE WS-SAVE-REPORT-CODE1  TO WS-REPORT-CD-CAPTION
CIDMOD         MOVE WS-REPORT-CODE-CAPTION TO RCAP1O.
CIDMOD
CIDMOD     IF WS-SAVE-REPORT-CODE2 NOT = SPACES AND LOW-VALUES
CIDMOD         MOVE WS-SAVE-REPORT-CODE2  TO WS-REPORT-CD-CAPTION
CIDMOD         MOVE WS-REPORT-CODE-CAPTION TO RCAP2O.
CIDMOD
120706     IF WS-SAVE-REPORT-CODE3 NOT = SPACES AND LOW-VALUES
120706         MOVE WS-SAVE-REPORT-CODE3
120706                                 TO WS-REPORT-CD-CAPTION
120706         MOVE WS-REPORT-CODE-CAPTION
120706                                 TO RCAP3O
120706     END-IF
CIDMOD
00559      GO TO 8100-SEND-INITIAL-MAP.
00560
00561  5099-EXIT.
00562      EXIT.
00563      EJECT
00564  6000-CHECK-FOR-UPDATE.
00565
020816     IF PI-COMPANY-ID = 'ACE' OR 'LGX' or 'DCC' or 'VPP'
00567         IF CANFEEL NOT EQUAL ZEROS
00568            MOVE W-CANCEL-FEE     TO AM-CANCEL-FEE.
092711
092711     IF ODEALERL GREATER ZEROS
092711        MOVE ODEALERI            TO AM-ORIG-DEALER-NO
092711     END-IF.
00569
00570      IF RPTCD1L GREATER ZEROS
00571         MOVE RPTCD1I             TO AM-REPORT-CODE-1.
00572
00573      IF CTYCDL GREATER ZEROS
00574         MOVE CTYCDI              TO AM-CITY-CODE.
00575
00576      IF AUTORFDL GREATER ZEROS
00577          MOVE AUTORFDI           TO AM-AUTO-REFUND-SW.
00578
00579      IF RPTCD2L GREATER ZEROS
00580         MOVE RPTCD2I             TO AM-REPORT-CODE-2
00581         IF PI-COMPANY-ID = 'NCL'
00582             PERFORM 7400-UPDATE-COMP-MAST THRU 7400-EXIT
00583         ELSE
00584             NEXT SENTENCE
00585      ELSE
00586         IF PI-COMPANY-ID = 'NCL' AND
00587            PI-SV-MAINT   = 'A'
00588                 MOVE ER-4011         TO EMI-ERROR
00589                 MOVE -1              TO RPTCD2L
00590                 MOVE AL-UABON        TO RPTCD2A
00591                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00592
120706     IF RPTCD3L > ZEROS
120706        MOVE RPTCD3I             TO AM-REPORT-CODE-3
120706     END-IF
00593      IF CNTYCDL GREATER ZEROS
00594         MOVE CNTYCDI             TO AM-COUNTY-PARISH.
00595
00596      IF USERL GREATER ZEROS
00597         MOVE USERI               TO AM-USER-FIELDS.
00598
00599      IF TRUSTTYL GREATER ZEROS
00600         MOVE TRUSTTYI            TO AM-TRUST-TYPE.
00601
00602      IF AHONLYL GREATER ZEROS
00603         MOVE AHONLYI             TO AM-AH-ONLY-INDICATOR.
00604
00605      IF PI-COMPANY-ID = 'NCL'
00606         IF AM-AH-ONLY-INDICATOR NOT = 'Y'
00607            MOVE 'N'              TO AM-AH-ONLY-INDICATOR.
00608
00609      IF LOANOFCL GREATER ZEROS
00610         MOVE LOANOFCI            TO AM-EDIT-LOAN-OFC.
00611
00612      IF DISMBRL  GREATER ZEROS
00613         MOVE DISMBRI             TO AM-DISMBR-COVERAGE-SW.
00614
00615      IF NLEVELL GREATER ZEROS
00616         IF NLEVELI NUMERIC
00617            MOVE NLEVELI          TO AM-3RD-PARTY-NOTIF-LEVEL.
00618
00619      IF NCORRL GREATER ZEROS
00620         IF NCORRI = ' ' OR 'Y' OR 'N'
00621            MOVE NCORRI           TO AM-NOTIF-OF-LETTERS.
00622
00623      IF NPMTSL GREATER ZEROS
00624         IF NPMTSI = ' ' OR 'Y' OR 'N'
00625            MOVE NPMTSI           TO AM-NOTIF-OF-PAYMENTS.
00626
00627      IF NRPTSL GREATER ZEROS
00628         IF NRPTSI = ' ' OR 'Y' OR 'N'
00629            MOVE NRPTSI           TO AM-NOTIF-OF-REPORTS.
00630
00631      IF NSTATL GREATER ZEROS
00632         IF NSTATI = ' ' OR 'Y' OR 'N'
00633            MOVE NSTATI           TO AM-NOTIF-OF-STATUS.
00634
00635      IF GRPCHKL GREATER ZEROS
00636         MOVE GRPCHKI             TO AM-GROUPED-CHECKS-Y-N.
00637
00638      IF EMPSTMTL GREATER ZEROS
00639         MOVE EMPSTMTI            TO AM-EMPLOYER-STMT-USED.
00640
00641      IF TARRATL GREATER ZEROS
00642         MOVE W-TARGET-LOSS-RATIO TO AM-TARGET-LOSS-RATIO.
00643
00644      IF LIFBNRL GREATER ZEROS
00645         MOVE W-LIFE-IBNR-PCT     TO AM-LIFE-IBNR-PCT.
00646
00647      IF CDTMODL GREATER ZEROS
00648         MOVE W-CRDT-MOD-PCT      TO AM-CRDT-MODIFICATION-PCT.
00649
00650      IF CNTLTITL GREATER ZEROS
00651         MOVE CNTLTITI            TO AM-CONTROL-NAME.
00652
00653      IF AXNAME1L NOT EQUAL ZEROS
00654         MOVE AXNAME1I            TO AM-EXEC1-NAME.
00655
00656      IF AXDISP1L NOT EQUAL ZEROS
00657         MOVE W-EXEC1-DIS-PRCNT   TO AM-EXEC1-DIS-PERCENT
00658
00659      ELSE
00660         IF  AM-EXEC1-DIS-PERCENT NOT NUMERIC
00661             MOVE ZEROS           TO AM-EXEC1-DIS-PERCENT.
00662
00663      IF AXLIFP1L NOT EQUAL ZEROS
00664         MOVE W-EXEC1-LIF-PRCNT   TO AM-EXEC1-LIFE-PERCENT
00665
00666      ELSE
00667         IF  AM-EXEC1-LIFE-PERCENT NOT NUMERIC
00668             MOVE ZEROS           TO AM-EXEC1-LIFE-PERCENT.
00669
00670      IF AXNAME2L NOT EQUAL ZEROS
00671         MOVE AXNAME2I            TO AM-EXEC2-NAME.
00672
00673      IF AXDISP2L NOT EQUAL ZEROS
00674         MOVE W-EXEC2-DIS-PRCNT   TO AM-EXEC2-DIS-PERCENT
00675
00676      ELSE
00677         IF  AM-EXEC2-DIS-PERCENT NOT NUMERIC
00678             MOVE ZEROS           TO AM-EXEC2-DIS-PERCENT.
00679
00680      IF AXLIFP2L NOT EQUAL ZEROS
00681         MOVE W-EXEC2-LIF-PRCNT   TO AM-EXEC2-LIFE-PERCENT
00682
00683      ELSE
00684         IF  AM-EXEC2-LIFE-PERCENT NOT NUMERIC
00685             MOVE ZEROS           TO AM-EXEC2-LIFE-PERCENT.
00686
00687      IF USER1L GREATER ZEROS
00688         MOVE USER1I              TO AM-USER-SELECT-1.
00689
00690      IF USER2L GREATER ZEROS
00691         MOVE USER2I              TO AM-USER-SELECT-2.
00692
00693      IF USER3L GREATER ZEROS
00694         MOVE USER3I              TO AM-USER-SELECT-3.
00695
00696      IF USER4L GREATER ZEROS
00697         MOVE USER4I              TO AM-USER-SELECT-4.
00698
00699      IF USER5L GREATER ZEROS
00700         MOVE USER5I              TO AM-USER-SELECT-5.
00701
00702  6049-EXIT.
00703      EXIT.
00704      EJECT
00705
00706  7000-EDIT.
00707
00708      IF GRPCHKL GREATER ZEROS
00709         IF GRPCHKI = ' ' OR 'Y' OR 'N'
00710            NEXT SENTENCE
00711          ELSE
00712             MOVE -1                      TO GRPCHKL
00713             MOVE AL-UABON                TO GRPCHKA
00714             MOVE ER-0627                 TO EMI-ERROR
00715             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00716
00717      IF AHONLYL GREATER ZEROS
00718         IF AHONLYI = ' ' OR 'Y' OR 'N'
00719            NEXT SENTENCE
00720          ELSE
00721             MOVE -1                      TO AHONLYL
00722             MOVE AL-UABON                TO AHONLYA
00723             MOVE ER-0627                 TO EMI-ERROR
00724             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00725
00726      IF LOANOFCL GREATER ZEROS
00727         IF LOANOFCI = ' ' OR 'Y' OR 'N'
00728             NEXT SENTENCE
00729         ELSE
00730             MOVE -1                      TO LOANOFCL
00731             MOVE AL-UABON                TO LOANOFCA
00732             MOVE ER-0627                 TO EMI-ERROR
00733             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00734
00735      IF DISMBRL GREATER ZEROS
00736         IF DISMBRI = ' ' OR 'Y' OR 'N'
00737             NEXT SENTENCE
00738         ELSE
00739             MOVE -1                      TO DISMBRL
00740             MOVE AL-UABON                TO DISMBRA
00741             MOVE ER-0906                 TO EMI-ERROR
00742             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00743
00744      IF EMPSTMTL GREATER ZEROS
00745         IF EMPSTMTI = ' ' OR 'Y' OR 'N' OR '1' OR '2' OR '3'
00746            NEXT SENTENCE
00747          ELSE
00748             MOVE -1                      TO EMPSTMTL
00749             MOVE AL-UABON                TO EMPSTMTA
00750             MOVE ER-0627                 TO EMI-ERROR
00751             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00752
00753      IF AUTORFDL GREATER ZEROS
00754          IF AUTORFDI = ' '  OR  'Y'  OR  'N'
00755              NEXT SENTENCE
00756          ELSE
00757              MOVE -1                     TO AUTORFDL
00758              MOVE AL-UABON               TO AUTORFDA
00759              MOVE ER-3124                TO EMI-ERROR
00760              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00761
00762      IF  TARRATL GREATER ZEROS
00763          
      * EXEC CICS BIF DEEDIT
00764 *            FIELD   (TARRATI)
00765 *            LENGTH  (6)
00766 *            END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004732' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TARRATI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00767          IF  TARRATI NUMERIC
00768              MOVE TARRATI        TO W-TARGET-LOSS-RATIO
00769                                     TARRATO
00770          ELSE
00771              MOVE -1             TO TARRATL
00772              MOVE AL-UABON       TO TARRATA
00773              MOVE ER-7717        TO EMI-ERROR
00774              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00775
00776      IF  LIFBNRL GREATER ZEROS
00777          
      * EXEC CICS BIF DEEDIT
00778 *            FIELD   (LIFBNRI)
00779 *            LENGTH  (6)
00780 *            END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004746' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LIFBNRI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00781          IF  LIFBNRI NUMERIC
00782              MOVE LIFBNRI        TO W-LIFE-IBNR-PCT
00783                                     LIFBNRO
00784          ELSE
00785              MOVE -1             TO LIFBNRL
00786              MOVE AL-UABON       TO LIFBNRA
00787              MOVE ER-7718        TO EMI-ERROR
00788              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00789
00790      IF  CDTMODL GREATER ZEROS
00791          
      * EXEC CICS BIF DEEDIT
00792 *            FIELD   (CDTMODI)
00793 *            LENGTH  (6)
00794 *            END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004760' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CDTMODI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00795          IF  CDTMODI NUMERIC
00796              MOVE CDTMODI        TO W-CRDT-MOD-PCT
00797                                     CDTMODO
00798          ELSE
00799              MOVE -1             TO CDTMODL
00800              MOVE AL-UABON       TO CDTMODA
00801              MOVE ER-7719        TO EMI-ERROR
00802              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00803
020816     IF PI-COMPANY-ID = 'ACE' OR 'LGX' or 'DCC' or 'VPP'
00805          IF  CANFEEL GREATER ZEROS
00806
00807              IF  CANFEEI NUMERIC
00808                  MOVE CANFEEI    TO W-CANCEL-FEE
00809                                     CANFEEO
00810
00811              ELSE
00812                  MOVE -1         TO CANFEEL
00813                  MOVE AL-UABON   TO CANFEEA
00814                  MOVE ER-3267    TO EMI-ERROR
00815                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00816
00817
00818      IF  PI-COMPANY-ID NOT EQUAL 'HAN'
00819              AND
00820          PI-COMPANY-ID NOT EQUAL 'JHL'
00821          GO TO 7099-EXIT.
00822
00823      IF  AXDISP1L GREATER ZEROS
00824
00825          
      * EXEC CICS BIF DEEDIT
00826 *            FIELD   (AXDISP1I)
00827 *            LENGTH  (6)
00828 *            END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004794' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AXDISP1I, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00829
00830          IF  AXDISP1I NUMERIC
00831              MOVE AXDISP1I       TO W-EXEC1-DIS-PRCNT
00832                                     AXDISP1O
00833
00834          ELSE
00835              MOVE -1             TO AXDISP1L
00836              MOVE AL-UABON       TO AXDISP1A
00837              MOVE ER-3257        TO EMI-ERROR
00838              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00839
00840      IF  AXLIFP1L GREATER ZEROS
00841
00842          
      * EXEC CICS BIF DEEDIT
00843 *            FIELD   (AXLIFP1I)
00844 *            LENGTH  (6)
00845 *            END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004811' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AXLIFP1I, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00846
00847          IF  AXLIFP1I NUMERIC
00848              MOVE AXLIFP1I       TO W-EXEC1-LIF-PRCNT
00849                                     AXLIFP1O
00850
00851          ELSE
00852              MOVE -1             TO AXLIFP1L
00853              MOVE AL-UABON       TO AXLIFP1A
00854              MOVE ER-3258        TO EMI-ERROR
00855              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00856
00857
00858      IF  AXDISP2L GREATER ZEROS
00859
00860          
      * EXEC CICS BIF DEEDIT
00861 *            FIELD   (AXDISP2I)
00862 *            LENGTH  (6)
00863 *            END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004829' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AXDISP2I, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00864
00865          IF  AXDISP2I NUMERIC
00866              MOVE AXDISP2I       TO W-EXEC2-DIS-PRCNT
00867                                     AXDISP2O
00868
00869          ELSE
00870              MOVE -1             TO AXDISP2L
00871              MOVE AL-UABON       TO AXDISP2A
00872              MOVE ER-3257        TO EMI-ERROR
00873              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00874
00875      IF  AXLIFP2L GREATER ZEROS
00876
00877          
      * EXEC CICS BIF DEEDIT
00878 *            FIELD   (AXLIFP2I)
00879 *            LENGTH  (6)
00880 *            END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004846' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AXLIFP2I, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00881
00882          IF  AXLIFP2I NUMERIC
00883              MOVE AXLIFP2I       TO W-EXEC2-LIF-PRCNT
00884                                     AXLIFP2O
00885
00886          ELSE
00887              MOVE -1             TO AXLIFP2L
00888              MOVE AL-UABON       TO AXLIFP2A
00889              MOVE ER-3258        TO EMI-ERROR
00890              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00891
00892  7099-EXIT.
00893      EXIT.
00894      EJECT
00895
00896  7100-READ-ERACCT.
00897      
      * EXEC CICS READ
00898 *         DATASET  (ERACCT-FILE)
00899 *         SET      (ADDRESS OF ACCOUNT-MASTER)
00900 *         RIDFLD   (PI-ACCT-KEY)
00901 *    END-EXEC.
      *    MOVE '&"S        E          (   #00004866' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-FILE, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00902
00903      MOVE AM-LAST-MAINT-USER     TO  PI-UPDATE-BY.
00904      MOVE AM-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.
00905
00906  7100-EXIT.
00907      EXIT.
00908      EJECT
00909
00910  7300-READ-ERACCT-UPDATE.
00911      
      * EXEC CICS READ
00912 *         DATASET  (ERACCT-FILE)
00913 *         SET      (ADDRESS OF ACCOUNT-MASTER)
00914 *         RIDFLD   (PI-ACCT-KEY)
00915 *         UPDATE
00916 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00004880' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-FILE, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00917
00918  7300-EXIT.
00919      EXIT.
00920      EJECT
00921  7400-UPDATE-COMP-MAST.
00922
00923      
      * EXEC CICS GETMAIN
00924 *         LENGTH   (COMP-REC-LEN)
00925 *         SET      (ADDRESS OF COMPENSATION-MASTER)
00926 *         INITIMG  (GETMAIN-SPACE)
00927 *    END-EXEC.
      *    MOVE ',"IL                  $   #00004892' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 COMP-REC-LEN, 
                 GETMAIN-SPACE
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00928
00929      MOVE AM-COMPANY-CD          TO  COMP-CO-ID.
00930
00931      IF PI-CAR-GROUP-ACCESS-CNTL = '1' OR '3'
00932          MOVE ZEROS              TO  COMP-CARRIER
00933      ELSE
00934          MOVE AM-CARRIER         TO  COMP-CARRIER.
00935
00936      IF PI-CAR-GROUP-ACCESS-CNTL = '2' OR '3'
00937          MOVE ZEROS              TO  COMP-GROUPING
00938      ELSE
00939          MOVE AM-GROUPING        TO  COMP-GROUPING.
00940
00941      MOVE 'A'                    TO  COMP-REC-TYPE.
00942      MOVE AM-AGT (AM-REMIT-TO)   TO  COMP-FIN-RESP.
00943      MOVE AM-AGT (1)             TO  COMP-ACCT-AGT.
00944
00945
00946      
      * EXEC CICS HANDLE CONDITION
00947 *        NOTFND   (7400-COMP-NOT-FOUND)
00948 *    END-EXEC.
      *    MOVE '"$I                   ! # #00004915' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303034393135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00949
00950      
      * EXEC CICS READ
00951 *         DATASET  (ERCOMP-FILE)
00952 *         SET      (ADDRESS OF COMPENSATION-MASTER)
00953 *         RIDFLD   (ERCOMP-KEY)
00954 *         UPDATE
00955 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00004919' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCOMP-FILE, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00956
00957      MOVE RPTCD2I                TO  CO-RPTCD2.
00958
00959      
      * EXEC CICS REWRITE
00960 *         DATASET  (ERCOMP-FILE)
00961 *         FROM     (COMPENSATION-MASTER)
00962 *    END-EXEC.
           MOVE LENGTH OF
            COMPENSATION-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004928' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERCOMP-FILE, 
                 COMPENSATION-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00963
00964      GO TO 7400-EXIT.
00965
00966  7400-COMP-NOT-FOUND.
00967      MOVE ER-2114                TO  EMI-ERROR.
00968      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00969
00970  7400-EXIT.
00971      EXIT.
00972      EJECT
00973  7800-COMPANY-REC-READ.
00974      MOVE SPACES                 TO  ELCNTL-KEY.
00975      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
00976      MOVE '1'                    TO  CNTL-REC-TYPE.
00977      MOVE +0                     TO  CNTL-SEQ-NO.
00978      
      * EXEC CICS HANDLE CONDITION
00979 *        NOTFND   (7880-NO-COMP)
00980 *    END-EXEC.
      *    MOVE '"$I                   ! $ #00004947' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303034393437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00981
00982      
      * EXEC CICS READ
00983 *        DATASET   (ELCNTL-FILE)
00984 *        SET       (ADDRESS OF CONTROL-FILE)
00985 *        RIDFLD    (ELCNTL-KEY)
00986 *    END-EXEC.
      *    MOVE '&"S        E          (   #00004951' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00987
00988      IF CF-ACCOUNT-MSTR-MAINT-DT = LOW-VALUES
00989          MOVE ER-2572            TO  EMI-ERROR
00990          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00991
00992      MOVE CF-REPORT-CD1-CAPTION  TO WS-SAVE-REPORT-CODE1.
00993      MOVE CF-REPORT-CD2-CAPTION  TO WS-SAVE-REPORT-CODE2
120706     MOVE 'RPT CODE 3'           TO WS-SAVE-REPORT-CODE3
00995      GO TO 7899-EXIT.
00996
00997  7880-NO-COMP.
00998      MOVE ER-0002                TO  EMI-ERROR.
00999      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01000
01001  7899-EXIT.
01002      EXIT.
01003      EJECT
01004  8000-UPDATE-MAINT-DATE.
01005      MOVE SPACES                 TO  ELCNTL-KEY.
01006
01007      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
01008      MOVE '1'                    TO  CNTL-REC-TYPE.
01009      MOVE +0                     TO  CNTL-SEQ-NO.
01010
01011      
      * EXEC CICS HANDLE CONDITION
01012 *        NOTFND   (8000-EXIT)
01013 *    END-EXEC.
      *    MOVE '"$I                   ! % #00004980' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303034393830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01014
01015      
      * EXEC CICS READ
01016 *        UPDATE
01017 *        DATASET   (ELCNTL-FILE)
01018 *        SET       (ADDRESS OF CONTROL-FILE)
01019 *        RIDFLD    (ELCNTL-KEY)
01020 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00004984' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01021
01022      MOVE BIN-CURRENT-SAVE       TO  CF-ACCOUNT-MSTR-MAINT-DT.
01023
01024      
      * EXEC CICS REWRITE
01025 *        DATASET   (ELCNTL-FILE)
01026 *        FROM      (CONTROL-FILE)
01027 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004993' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01028
01029  8000-EXIT.
01030       EXIT.
01031      EJECT
01032
01033  8100-SEND-INITIAL-MAP.
01034      MOVE SAVE-DATE              TO  DATEO.
01035      MOVE EIBTIME                TO  TIME-IN.
01036      MOVE TIME-OUT               TO  TIMEO.
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01037      MOVE -1                     TO  PFENTERL.
01038      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
01039
01040      IF WS-SAVE-REPORT-CODE1 NOT = SPACES AND LOW-VALUES
01041          MOVE WS-SAVE-REPORT-CODE1  TO WS-REPORT-CD-CAPTION
01042          MOVE WS-REPORT-CODE-CAPTION TO RCAP1O.
01043
01044      IF WS-SAVE-REPORT-CODE2 NOT = SPACES AND LOW-VALUES
01045          MOVE WS-SAVE-REPORT-CODE2  TO WS-REPORT-CD-CAPTION
01046          MOVE WS-REPORT-CODE-CAPTION TO RCAP2O.
01047
120706     IF WS-SAVE-REPORT-CODE3 NOT = SPACES AND LOW-VALUES
120706         MOVE WS-SAVE-REPORT-CODE3
120706                                 TO WS-REPORT-CD-CAPTION
120706         MOVE WS-REPORT-CODE-CAPTION
120706                                 TO RCAP3O
120706     END-IF
01048      IF  PI-COMPANY-ID NOT EQUAL 'JHL'
01049              AND
01050          PI-COMPANY-ID NOT EQUAL 'HAN'
01051          MOVE AL-SADOF           TO ACCEXTA
01052                                     CNTLTA
01053                                     CNTLTITA
01054                                     ACCEXT1A
01055                                     AXNAME1A
01056                                     AXDIST1A
01057                                     AXDISP1A
01058                                     AXLIFT1A
01059                                     AXLIFP1A
01060                                     ACCEXT2A
01061                                     AXNAME2A
01062                                     AXDIST2A
01063                                     AXDISP2A
01064                                     AXLIFT2A
01065                                     AXLIFP2A.
01066
01067 *****     CANCELATION FEE IS CUSTOMIZED FOR CLIENT ACE ONLY      *
020816     IF PI-COMPANY-ID = 'ACE' OR 'LGX' or 'DCC' or 'VPP'
01069         MOVE WS-CANCEL-TEXT      TO CANTXTO
01070         IF AM-CANCEL-FEE NUMERIC
01071            MOVE AM-CANCEL-FEE    TO W-CANCEL-FEE-LONG
01072            MOVE W-CANCEL-FEE-LONG  TO W-CANCEL-FEE
01073         ELSE
01074            MOVE ZEROES           TO W-CANCEL-FEE
01075         END-IF
01076         MOVE W-CANCEL-FEE        TO CANFEEO
01077      ELSE
01078         MOVE AL-SADOF            TO CANTXTA
01079         MOVE AL-SANOF            TO CANFEEA.
01080
01081      IF PI-COMPANY-ID = 'LGX' OR 'CRI'
01082         MOVE AL-SANOF            TO  EMPCAPA
01083         MOVE AL-UANON            TO  EMPSTMTA.
01084
01085      IF PI-COMPANY-ID = 'FLI' OR 'FLU' OR 'LGX'
01086         MOVE WS-FLI-PFK-DESC     TO  FLIPFKO.
01087
PEMMOD     SET T-INDEX                 TO PI-LINE-SELECTED
PEMMOD
PEMMOD     IF PI-2ND-PAGE
PEMMOD        SET T-INDEX UP BY 8
PEMMOD     ELSE
PEMMOD        IF PI-3RD-PAGE
PEMMOD           SET T-INDEX UP BY 16
PEMMOD        ELSE
PEMMOD           IF PI-LST-PAGE
PEMMOD              SET T-INDEX UP BY 24
PEMMOD           END-IF
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD
PEMMOD     MOVE PI-ACCT-CARRIER        TO CARRI
PEMMOD     MOVE PI-ACCT-GROUPING       TO GROUPINI
PEMMOD     MOVE PI-ACCT-STATE          TO STATEI
PEMMOD     MOVE PI-ACCT-ACCOUNT        TO ACCOUNTI
PEMMOD
PEMMOD     IF PI-ACCT-EXP-DT = LOW-VALUES
PEMMOD        MOVE SPACES              TO EXPDTEI
PEMMOD     ELSE
PEMMOD        IF PI-ACCT-EXP-DT NOT = HIGH-VALUES
PEMMOD           MOVE PI-ACCT-EXP-DT   TO DC-BIN-DATE-1
PEMMOD           MOVE SPACE            TO DC-OPTION-CODE
PEMMOD           PERFORM 9700-LINK-DATE-CONVERT
PEMMOD                                 THRU 9700-EXIT
PEMMOD           MOVE DC-GREG-DATE-1-EDIT
PEMMOD                                 TO EXPDTEI
PEMMOD        ELSE
PEMMOD           MOVE 999999           TO EXPDTEO
PEMMOD           INSPECT EXPDTEI CONVERTING SPACES TO '/'
PEMMOD        END-IF
PEMMOD     END-IF
PEMMOD
PEMMOD     MOVE EXPDTEI (1:2)          TO WS-YYYYMMDD (5:2)
PEMMOD     MOVE EXPDTEI (4:2)          TO WS-YYYYMMDD (7:2)
PEMMOD     MOVE EXPDTEI (6:2)          TO WS-YYYYMMDD (3:2)
PEMMOD     IF EXPDTEI (7:1) NOT = '9'
PEMMOD        MOVE '20'                TO WS-YYYYMMDD (1:2)
PEMMOD     ELSE
PEMMOD        MOVE '19'                TO WS-YYYYMMDD (1:2)
PEMMOD     END-IF
PEMMOD
PEMMOD     MOVE PI-BIN-EFF-DT (T-INDEX)
PEMMOD                                 TO DC-BIN-DATE-1
PEMMOD     MOVE SPACE                  TO DC-OPTION-CODE
PEMMOD     PERFORM 9700-LINK-DATE-CONVERT
PEMMOD                                 THRU 9700-EXIT
PEMMOD     MOVE DC-GREG-DATE-1-EDIT    TO EFFDTEI
PEMMOD
01088      
      * EXEC CICS SEND
01089 *        MAP      (MAP-NAME)
01090 *        MAPSET   (MAPSET-NAME)
01091 *        FROM     (EL6502AO)
01092 *        ERASE
01093 *        CURSOR
01094 *    END-EXEC.
           MOVE LENGTH OF
            EL6502AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00005116' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6502AO, 
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
           
01095
01096      GO TO 9100-RETURN-TRAN.
01097
01098  8200-SEND-DATAONLY.
01099      MOVE SAVE-DATE              TO  DATEO.
01100      MOVE EIBTIME                TO  TIME-IN.
01101      MOVE TIME-OUT               TO  TIMEO.
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01102      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
01103
01104      IF WS-SAVE-REPORT-CODE1 NOT = SPACES AND LOW-VALUES
01105          MOVE WS-SAVE-REPORT-CODE1  TO WS-REPORT-CD-CAPTION
01106          MOVE WS-REPORT-CODE-CAPTION TO RCAP1O.
01107
01108      IF WS-SAVE-REPORT-CODE2 NOT = SPACES AND LOW-VALUES
01109          MOVE WS-SAVE-REPORT-CODE2  TO WS-REPORT-CD-CAPTION
01110          MOVE WS-REPORT-CODE-CAPTION TO RCAP2O.
01111
120706     IF WS-SAVE-REPORT-CODE3 NOT = SPACES AND LOW-VALUES
120706         MOVE WS-SAVE-REPORT-CODE3
120706                                 TO WS-REPORT-CD-CAPTION
120706         MOVE WS-REPORT-CODE-CAPTION
120706                                 TO RCAP3O
120706     END-IF
01112      IF PI-COMPANY-ID = 'FLI' OR 'FLU' OR 'LGX'
01113         MOVE WS-FLI-PFK-DESC     TO  FLIPFKO.
01114
01115      IF  PI-COMPANY-ID NOT EQUAL 'JHL'
01116              AND
01117          PI-COMPANY-ID NOT EQUAL 'HAN'
01118          MOVE AL-SADOF           TO ACCEXTA
01119                                     CNTLTITA
01120                                     CNTLTA
01121                                     ACCEXT1A
01122                                     AXNAME1A
01123                                     AXDIST1A
01124                                     AXDISP1A
01125                                     AXLIFT1A
01126                                     AXLIFP1A
01127                                     ACCEXT2A
01128                                     AXNAME2A
01129                                     AXDIST2A
01130                                     AXDISP2A
01131                                     AXLIFT2A
01132                                     AXLIFP2A.
01133
01134 *****     CANCELATION FEE IS CUSTOMIZED FOR CLIENT ACE ONLY      *
020816     IF PI-COMPANY-ID = 'ACE' OR 'LGX' or 'DCC' or 'VPP'
01136         MOVE WS-CANCEL-TEXT      TO CANTXTO
01137         IF AM-CANCEL-FEE  NUMERIC
01138            MOVE AM-CANCEL-FEE    TO W-CANCEL-FEE-LONG
01139            MOVE W-CANCEL-FEE-LONG   TO W-CANCEL-FEE
01140         ELSE
01141            MOVE ZEROES           TO W-CANCEL-FEE
01142         END-IF
01143         MOVE W-CANCEL-FEE        TO CANFEEO
01144      ELSE
01145         MOVE AL-SADOF            TO CANTXTA
01146         MOVE AL-SANOF            TO CANFEEA.
01147
01148      
      * EXEC CICS SEND
01149 *        MAP      (MAP-NAME)
01150 *        MAPSET   (MAPSET-NAME)
01151 *        FROM     (EL6502AO)
01152 *        DATAONLY
01153 *        CURSOR
01154 *    END-EXEC.
           MOVE LENGTH OF
            EL6502AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00005184' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6502AO, 
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
           
01155
01156      GO TO 9100-RETURN-TRAN.
01157
01158  8300-SEND-TEXT.
01159      
      * EXEC CICS SEND TEXT
01160 *        FROM     (LOGOFF-TEXT)
01161 *        LENGTH   (LOGOFF-LENGTH)
01162 *        ERASE
01163 *        FREEKB
01164 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00005195' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313935' TO DFHEIV0(25:11)
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
           
01165
01166      
      * EXEC CICS RETURN
01167 *    END-EXEC.
      *    MOVE '.(                    ''   #00005202' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01168
01169  8600-DEEDIT.
01170      
      * EXEC CICS BIF DEEDIT
01171 *         FIELD(DEEDIT-FIELD)
01172 *         LENGTH(15)
01173 *    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005206' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01174
01175  8800-UNAUTHORIZED-ACCESS.
01176      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
01177      GO TO 8300-SEND-TEXT.
01178
01179  8810-PF23.
01180      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
01181      MOVE XCTL-005               TO  PGM-NAME.
01182      GO TO 9300-XCTL.
01183
01184  9100-RETURN-TRAN.
01185      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
01186      MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.
01187      
      * EXEC CICS RETURN
01188 *        TRANSID    (TRANS-ID)
01189 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
01190 *        LENGTH     (WS-COMM-LENGTH)
01191 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00005223' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01192
01193  9200-RETURN-MAIN-MENU.
01194      MOVE XCTL-626               TO  PGM-NAME.
01195      GO TO 9300-XCTL.
01196
01197  9300-XCTL.
01198      
      * EXEC CICS XCTL
01199 *        PROGRAM    (PGM-NAME)
01200 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
01201 *        LENGTH     (WS-COMM-LENGTH)
01202 *    END-EXEC.
      *    MOVE '.$C                   %   #00005234' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01203
01204  9400-CLEAR.
01205      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.
01206      GO TO 9300-XCTL.
01207
01208  9500-PF12.
01209      MOVE XCTL-010               TO  PGM-NAME.
01210      GO TO 9300-XCTL.
01211
01212  9600-PGMID-ERROR.
01213      
      * EXEC CICS HANDLE CONDITION
01214 *        PGMIDERR    (8300-SEND-TEXT)
01215 *    END-EXEC.
      *    MOVE '"$L                   ! & #00005249' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303035323439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01216
01217      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.
01218      MOVE ' '                    TO  PI-ENTRY-CD-1.
01219      MOVE XCTL-005               TO  PGM-NAME.
01220      MOVE PGM-NAME               TO  LOGOFF-PGM.
01221      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
01222      GO TO 9300-XCTL.
01223
01224  9700-LINK-DATE-CONVERT.
01225      
      * EXEC CICS LINK
01226 *        PROGRAM    ('ELDATCV')
01227 *        COMMAREA   (DATE-CONVERSION-DATA)
01228 *        LENGTH     (DC-COMM-LENGTH)
01229 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00005261' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01230
01231  9700-EXIT.
01232      EXIT.
01233
01234  9900-ERROR-FORMAT.
01235      IF NOT EMI-ERRORS-COMPLETE
01236          MOVE LINK-001           TO  PGM-NAME
01237          
      * EXEC CICS LINK
01238 *            PROGRAM    (PGM-NAME)
01239 *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
01240 *            LENGTH     (EMI-COMM-LENGTH)
01241 *        END-EXEC.
      *    MOVE '."C                   (   #00005273' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01242
01243  9900-EXIT.
01244      EXIT.
01245
01246  9990-ABEND.
01247      MOVE LINK-004               TO  PGM-NAME.
01248      MOVE DFHEIBLK               TO  EMI-LINE1.
01249      
      * EXEC CICS LINK
01250 *        PROGRAM   (PGM-NAME)
01251 *        COMMAREA  (EMI-LINE1)
01252 *        LENGTH    (72)
01253 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00005285' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01254
01255      GO TO 8200-SEND-DATAONLY.
01256
01257  9995-SECURITY-VIOLATION.
01258 *           COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00005311' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333131' TO DFHEIV0(25:11)
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
01259  9995-EXIT.
01260       EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6502' TO DFHEIV1
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
               GO TO 7400-COMP-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 7880-NO-COMP
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 8000-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6502' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
