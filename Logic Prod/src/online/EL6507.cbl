00001  ID DIVISION.
00002
00003  PROGRAM-ID.                 EL6507.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 11/27/95 09:07:19.
00007 *                            VMOD=2.009
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
00018 *            *                                                   *
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00021 *            *                                                   *
00022 *            *****************************************************
00023 *
00024 *REMARKS.    TRANSACTION - EXD3 - ACCOUNT PLAN MASTER MAINT.
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
101101*                              ADJUSTED REDEFINES EL6507AI FILLER
101101******************************************************************
00026  ENVIRONMENT DIVISION.
00027
00028      EJECT
00029  DATA DIVISION.
00030  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00031  77  FILLER  PIC X(32)  VALUE '********************************'.
00032  77  FILLER  PIC X(32)  VALUE '*    EL6507 WORKING STORAGE    *'.
00033  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.009 *********'.
00034
00035  01  WS-DATE-AREA.
00036      05  SAVE-DATE                   PIC X(8)    VALUE SPACES.
00037      05  SAVE-BIN-DATE               PIC XX      VALUE SPACES.
00038
00039  01  STANDARD-AREAS.
00040      12  RETURNED-FROM               PIC X(08).
00041      12  WS-PLAN-KEY.
00042          16  WS-PLAN-ACCT-KEY.
00043              20  WS-PLAN-COMPANY-CD  PIC X.
00044              20  WS-PLAN-CARRIER     PIC X.
00045              20  WS-PLAN-GROUP       PIC X(6).
00046              20  WS-PLAN-STATE       PIC X(2).
00047              20  WS-PLAN-ACCOUNT     PIC X(10).
00048          16  WS-PLAN-BEN-TYPE        PIC X.
00049          16  WS-PLAN-BEN             PIC XX.
00050          16  WS-PLAN-REVISION        PIC X(3).
00051      12  GETMAIN-SPACE               PIC X(01) VALUE SPACES.
00052      12  SUB                         PIC S9(04) COMP VALUE +0.
00053      12  SUB1                        PIC S9(4)   VALUE +0    COMP.
00054      12  SUB2                        PIC S9(4)   VALUE +0    COMP.
00055      12  WS-COMM-LENGTH              PIC S9(4) COMP VALUE +1500.
00056      12  QID1.
00057          16  QID1-TERM               PIC X(4).
00058          16  FILLER                  PIC X(4)    VALUE '650H'.
00059      12  QID2.
00060          16  QID2-TERM               PIC X(4).
00061          16  FILLER                  PIC X(4)    VALUE '650I'.
00062      12  WS-MAP-LENGTH               PIC S9(4) COMP VALUE +656.
00063      12  MAP-NAME                    PIC X(8)    VALUE 'EL6507A'.
00064      12  MAPSET-NAME                 PIC X(8)    VALUE 'EL6507S'.
00065      12  SCREEN-NUMBER               PIC X(4)    VALUE '650H'.
00066      12  TRANS-ID                    PIC X(4)    VALUE 'EXD3'.
00067      12  THIS-PGM                    PIC X(8)    VALUE 'EL6507'.
00068      12  PGM-NAME                    PIC X(8).
00069      12  TIME-IN                     PIC S9(7).
00070      12  TIME-OUT-R  REDEFINES TIME-IN.
00071          16  FILLER                  PIC X.
00072          16  TIME-OUT                PIC 99V99.
00073          16  FILLER                  PIC XX.
00074      12  XCTL-005                    PIC X(8)    VALUE 'EL005'.
00075      12  XCTL-010                    PIC X(8)    VALUE 'EL010'.
00076      12  XCTL-106                    PIC X(8)    VALUE 'EL106'.
00077      12  XCTL-626                    PIC X(8)    VALUE 'EL626'.
00078      12  XCTL-650                    PIC X(8)    VALUE 'EL650'.
00079      12  XCTL-6501                   PIC X(8)    VALUE 'EL6501'.
00080      12  XCTL-6502                   PIC X(8)    VALUE 'EL6502'.
00081      12  XCTL-6503                   PIC X(8)    VALUE 'EL6503'.
00082      12  XCTL-6504                   PIC X(8)    VALUE 'EL6504'.
00083      12  XCTL-6505                   PIC X(8)    VALUE 'EL6505'.
00084      12  XCTL-6506                   PIC X(8)    VALUE 'EL6505'.
00085      12  XCTL-656                    PIC X(8)    VALUE 'EL656'.
00086      12  XCTL-6565                   PIC X(8)    VALUE 'EL6565'.
00087      12  LINK-001                    PIC X(8)    VALUE 'EL001'.
00088      12  LINK-004                    PIC X(8)    VALUE 'EL004'.
00089      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'.
00090      12  FILE-ID                     PIC X(8)    VALUE SPACES.
00091      12  ERACCT-FILE                 PIC X(8)    VALUE 'ERACCT'.
00092      12  ERPLAN-FILE                 PIC X(8)    VALUE 'ERPLAN'.
00093      12  ERRATE-FILE                 PIC X(8)    VALUE 'ERRATE'.
00094      12  ELCNTL-FILE                 PIC X(8)    VALUE 'ELCNTL'.
00095      12  BIN-CURRENT-SAVE            PIC XX      VALUE SPACES.
00096      12  YMD-CURRENT-SAVE            PIC X(6)    VALUE SPACES.
00097
00098      12  JOURNAL-LENGTH              PIC S9(4)   VALUE +23   COMP.
00099      12  ERPLAN-LENGTH               PIC S9(4)   VALUE +420  COMP.
00100      12  ELCNTL-LENGTH               PIC S9(4)   VALUE +750  COMP.
00101      12  SC-ITEM                     PIC S9(4)   VALUE +1    COMP.
00102      12  WS-JOURNAL-FILE-LENGTH      PIC S9(4)   VALUE +0    COMP.
00103
00104      12  DEEDIT-FIELD                PIC X(15).
00105      12  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD PIC S9(15).
00106      12  DEEDIT-FIELD-V1  REDEFINES DEEDIT-FIELD PIC S9(14)V9.
00107      12  DEEDIT-FIELD-V2  REDEFINES DEEDIT-FIELD PIC S9(13)V99.
00108      12  DEEDIT-FIELD-V3  REDEFINES DEEDIT-FIELD PIC S9(12)V999.
00109      12  DEEDIT-FIELD-V4  REDEFINES DEEDIT-FIELD PIC S9(11)V9(4).
00110      12  DEEDIT-FIELD-V5  REDEFINES DEEDIT-FIELD PIC S9(10)V9(5).
00111      12  DEEDIT-FIELD-V6  REDEFINES DEEDIT-FIELD PIC S9(9)V9(6).
00112
00113      12  WS-EDIT-FIELD-CONV          PIC S9(4)   VALUE +0.
00114
00115      12  RATE-KEY.
00116          16  RATE-COMP-CD            PIC X.
00117          16  RATE-STATE              PIC XX.
00118          16  RATE-CLASS              PIC XX.
00119          16  RATE-DEV                PIC XXX.
00120          16  FILLER                  PIC X(20).
00121
00122      12  FILLER OCCURS 8 TIMES.
00123          16  WS-MOBENI               PIC S9(04).
00124          16  WS-TOTBENI              PIC S9(06).
00125      12  WS-PRMPCTI                  PIC S9V9(04)   VALUE ZEROS.
00126      12  WS-REFPCTI                  PIC S9V9(04)   VALUE ZEROS.
00127      12  WS-PRMAMTI                  PIC S9(03)V99  VALUE ZEROS.
00128      12  WS-REFAMTI                  PIC S9(03)V99  VALUE ZEROS.
00129      12  WS-CLMAMTI                  PIC S9(03)V99  VALUE ZEROS.
00130      12  WS-STTAXI                   PIC S9V9(04)   VALUE ZEROS.
00131      12  WS-POLFEEI                  PIC S99V999    VALUE ZEROS.
00132      12  WS-CORETI                   PIC S9V9(04)   VALUE ZEROS.
00133      12  WS-DEVPCTI                  PIC S9V9(06)   VALUE ZEROS.
00134      12  WS-SALTAXI                  PIC S9V9(04)   VALUE ZEROS.
00135      12  WS-OVSAMTI                  PIC S9(03)V99  VALUE ZEROS.
00136      12  WS-OVSPCTI                  PIC S9V9(4)    VALUE ZEROS.
00137      EJECT
00138      12  ERROR-MESSAGES.
00139          16  ER-0000                 PIC X(4)    VALUE '0000'.
00140          16  ER-0002                 PIC X(4)    VALUE '0002'.
00141          16  ER-0004                 PIC X(4)    VALUE '0004'.
00142          16  ER-0008                 PIC X(4)    VALUE '0008'.
00143          16  ER-0029                 PIC X(4)    VALUE '0029'.
00144          16  ER-0066                 PIC X(4)    VALUE '0066'.
00145          16  ER-0067                 PIC X(4)    VALUE '0067'.
00146          16  ER-0068                 PIC X(4)    VALUE '0068'.
00147          16  ER-0070                 PIC X(4)    VALUE '0070'.
00148          16  ER-0132                 PIC X(4)    VALUE '0132'.
00149          16  ER-0627                 PIC X(4)    VALUE '0627'.
00150          16  ER-2039                 PIC X(4)    VALUE '2039'.
00151          16  ER-2056                 PIC X(4)    VALUE '2056'.
00152          16  ER-2106                 PIC X(4)    VALUE '2106'.
00153          16  ER-2107                 PIC X(4)    VALUE '2107'.
00154          16  ER-2152                 PIC X(4)    VALUE '2152'.
00155          16  ER-2154                 PIC X(4)    VALUE '2154'.
00156          16  ER-2168                 PIC X(4)    VALUE '2168'.
00157          16  ER-2169                 PIC X(4)    VALUE '2169'.
00158          16  ER-2170                 PIC X(4)    VALUE '2170'.
00159          16  ER-2273                 PIC X(4)    VALUE '2273'.
00160          16  ER-2274                 PIC X(4)    VALUE '2274'.
00161          16  ER-2275                 PIC X(4)    VALUE '2275'.
00162          16  ER-2276                 PIC X(4)    VALUE '2276'.
00163          16  ER-2572                 PIC X(4)    VALUE '2572'.
00164          16  ER-2937                 PIC X(4)    VALUE '2937'.
00165          16  ER-2938                 PIC X(4)    VALUE '2938'.
00166          16  ER-2939                 PIC X(4)    VALUE '2939'.
00167          16  ER-2948                 PIC X(4)    VALUE '2948'.
00168          16  ER-3126                 PIC X(4)    VALUE '3126'.
00169          16  ER-3127                 PIC X(4)    VALUE '3127'.
00170          16  ER-3128                 PIC X(4)    VALUE '3128'.
00171          16  ER-7208                 PIC X(4)    VALUE '7208'.
00172          16  ER-9055                 PIC X(4)    VALUE '9055'.
00173          16  ER-9407                 PIC X(4)    VALUE '9407'.
00174
00175      12  ELCNTL-KEY.
00176          16  CNTL-COMP-ID            PIC X(3)    VALUE SPACES.
00177          16  CNTL-REC-TYPE           PIC X       VALUE SPACES.
00178          16  CNTL-ACCESS             PIC X(4)    VALUE SPACES.
00179          16  CNTL-SEQ-NO             PIC S9(4)   VALUE +0  COMP.
00180
00181      EJECT
00182 *                          COPY ELCSCTM.
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
00183      EJECT
00184 *                          COPY ELCSCRTY.
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
00185      EJECT
00186 *                          COPY ELCLOGOF.
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
00187      EJECT
00188 *                          COPY ELCDATE.
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
00189      EJECT
00190 *                          COPY ELCATTR.
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
00191      EJECT
00192 *                          COPY ELCEMIB.
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
00193      EJECT
00194 *                          COPY ELCINTF.
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
00195      EJECT
00196 *                          COPY ELC650PI.
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
00197      EJECT
00198 *                          COPY ELCJPFX.
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
00199                                          PIC X(2000).
00200
00201      EJECT
00202 *                                    COPY ELCAID.
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
00203  01  FILLER    REDEFINES DFHAID.
00204      12  FILLER                      PIC X(8).
00205      12  PF-VALUES                   PIC X       OCCURS 2.
00206
00207      EJECT
00208 *                                    COPY EL6507S.
       01  EL6507AI.
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
           05  MAINTL PIC S9(0004) COMP.
           05  MAINTF PIC  X(0001).
           05  FILLER REDEFINES MAINTF.
               10  MAINTA PIC  X(0001).
           05  MAINTI PIC  X(0001).
      *    -------------------------------
           05  CARRL PIC S9(0004) COMP.
           05  CARRF PIC  X(0001).
           05  FILLER REDEFINES CARRF.
               10  CARRA PIC  X(0001).
           05  CARRI PIC  X(0001).
      *    -------------------------------
           05  GROUPL PIC S9(0004) COMP.
           05  GROUPF PIC  X(0001).
           05  FILLER REDEFINES GROUPF.
               10  GROUPA PIC  X(0001).
           05  GROUPI PIC  X(0006).
      *    -------------------------------
           05  STATEL PIC S9(0004) COMP.
           05  STATEF PIC  X(0001).
           05  FILLER REDEFINES STATEF.
               10  STATEA PIC  X(0001).
           05  STATEI PIC  X(0002).
      *    -------------------------------
           05  ACCTL PIC S9(0004) COMP.
           05  ACCTF PIC  X(0001).
           05  FILLER REDEFINES ACCTF.
               10  ACCTA PIC  X(0001).
           05  ACCTI PIC  X(0010).
      *    -------------------------------
           05  TYPEL PIC S9(0004) COMP.
           05  TYPEF PIC  X(0001).
           05  FILLER REDEFINES TYPEF.
               10  TYPEA PIC  X(0001).
           05  TYPEI PIC  X(0001).
      *    -------------------------------
           05  BENEL PIC S9(0004) COMP.
           05  BENEF PIC  X(0001).
           05  FILLER REDEFINES BENEF.
               10  BENEA PIC  X(0001).
           05  BENEI PIC  X(0002).
      *    -------------------------------
           05  REVISEL PIC S9(0004) COMP.
           05  REVISEF PIC  X(0001).
           05  FILLER REDEFINES REVISEF.
               10  REVISEA PIC  X(0001).
           05  REVISEI PIC  X(0003).
      *    -------------------------------
           05  CALCTPL PIC S9(0004) COMP.
           05  CALCTPF PIC  X(0001).
           05  FILLER REDEFINES CALCTPF.
               10  CALCTPA PIC  X(0001).
           05  CALCTPI PIC  X(0001).
      *    -------------------------------
           05  ATTAGEL PIC S9(0004) COMP.
           05  ATTAGEF PIC  X(0001).
           05  FILLER REDEFINES ATTAGEF.
               10  ATTAGEA PIC  X(0001).
           05  ATTAGEI PIC  X(0002).
      *    -------------------------------
           05  BENEC1L PIC S9(0004) COMP.
           05  BENEC1F PIC  X(0001).
           05  FILLER REDEFINES BENEC1F.
               10  BENEC1A PIC  X(0001).
           05  BENEC1I PIC  X(0007).
      *    -------------------------------
           05  BENEC2L PIC S9(0004) COMP.
           05  BENEC2F PIC  X(0001).
           05  FILLER REDEFINES BENEC2F.
               10  BENEC2A PIC  X(0001).
           05  BENEC2I PIC  X(0008).
      *    -------------------------------
           05  AGE1L PIC S9(0004) COMP.
           05  AGE1F PIC  X(0001).
           05  FILLER REDEFINES AGE1F.
               10  AGE1A PIC  X(0001).
           05  AGE1I PIC  X(0002).
      *    -------------------------------
           05  TERM1L PIC S9(0004) COMP.
           05  TERM1F PIC  X(0001).
           05  FILLER REDEFINES TERM1F.
               10  TERM1A PIC  X(0001).
           05  TERM1I PIC  X(0003).
      *    -------------------------------
           05  TOTBEN1L PIC S9(0004) COMP.
           05  TOTBEN1F PIC  X(0001).
           05  FILLER REDEFINES TOTBEN1F.
               10  TOTBEN1A PIC  X(0001).
           05  TOTBEN1I PIC  X(0007).
      *    -------------------------------
           05  MOBEN1L PIC S9(0004) COMP.
           05  MOBEN1F PIC  X(0001).
           05  FILLER REDEFINES MOBEN1F.
               10  MOBEN1A PIC  X(0001).
           05  MOBEN1I PIC  X(0005).
      *    -------------------------------
           05  AGE2L PIC S9(0004) COMP.
           05  AGE2F PIC  X(0001).
           05  FILLER REDEFINES AGE2F.
               10  AGE2A PIC  X(0001).
           05  AGE2I PIC  X(0002).
      *    -------------------------------
           05  TERM2L PIC S9(0004) COMP.
           05  TERM2F PIC  X(0001).
           05  FILLER REDEFINES TERM2F.
               10  TERM2A PIC  X(0001).
           05  TERM2I PIC  X(0003).
      *    -------------------------------
           05  TOTBEN2L PIC S9(0004) COMP.
           05  TOTBEN2F PIC  X(0001).
           05  FILLER REDEFINES TOTBEN2F.
               10  TOTBEN2A PIC  X(0001).
           05  TOTBEN2I PIC  X(0007).
      *    -------------------------------
           05  MOBEN2L PIC S9(0004) COMP.
           05  MOBEN2F PIC  X(0001).
           05  FILLER REDEFINES MOBEN2F.
               10  MOBEN2A PIC  X(0001).
           05  MOBEN2I PIC  X(0005).
      *    -------------------------------
           05  AGE3L PIC S9(0004) COMP.
           05  AGE3F PIC  X(0001).
           05  FILLER REDEFINES AGE3F.
               10  AGE3A PIC  X(0001).
           05  AGE3I PIC  X(0002).
      *    -------------------------------
           05  TERM3L PIC S9(0004) COMP.
           05  TERM3F PIC  X(0001).
           05  FILLER REDEFINES TERM3F.
               10  TERM3A PIC  X(0001).
           05  TERM3I PIC  X(0003).
      *    -------------------------------
           05  TOTBEN3L PIC S9(0004) COMP.
           05  TOTBEN3F PIC  X(0001).
           05  FILLER REDEFINES TOTBEN3F.
               10  TOTBEN3A PIC  X(0001).
           05  TOTBEN3I PIC  X(0007).
      *    -------------------------------
           05  MOBEN3L PIC S9(0004) COMP.
           05  MOBEN3F PIC  X(0001).
           05  FILLER REDEFINES MOBEN3F.
               10  MOBEN3A PIC  X(0001).
           05  MOBEN3I PIC  X(0005).
      *    -------------------------------
           05  AGE4L PIC S9(0004) COMP.
           05  AGE4F PIC  X(0001).
           05  FILLER REDEFINES AGE4F.
               10  AGE4A PIC  X(0001).
           05  AGE4I PIC  X(0002).
      *    -------------------------------
           05  TERM4L PIC S9(0004) COMP.
           05  TERM4F PIC  X(0001).
           05  FILLER REDEFINES TERM4F.
               10  TERM4A PIC  X(0001).
           05  TERM4I PIC  X(0003).
      *    -------------------------------
           05  TOTBEN4L PIC S9(0004) COMP.
           05  TOTBEN4F PIC  X(0001).
           05  FILLER REDEFINES TOTBEN4F.
               10  TOTBEN4A PIC  X(0001).
           05  TOTBEN4I PIC  X(0007).
      *    -------------------------------
           05  MOBEN4L PIC S9(0004) COMP.
           05  MOBEN4F PIC  X(0001).
           05  FILLER REDEFINES MOBEN4F.
               10  MOBEN4A PIC  X(0001).
           05  MOBEN4I PIC  X(0005).
      *    -------------------------------
           05  AGE5L PIC S9(0004) COMP.
           05  AGE5F PIC  X(0001).
           05  FILLER REDEFINES AGE5F.
               10  AGE5A PIC  X(0001).
           05  AGE5I PIC  X(0002).
      *    -------------------------------
           05  TERM5L PIC S9(0004) COMP.
           05  TERM5F PIC  X(0001).
           05  FILLER REDEFINES TERM5F.
               10  TERM5A PIC  X(0001).
           05  TERM5I PIC  X(0003).
      *    -------------------------------
           05  TOTBEN5L PIC S9(0004) COMP.
           05  TOTBEN5F PIC  X(0001).
           05  FILLER REDEFINES TOTBEN5F.
               10  TOTBEN5A PIC  X(0001).
           05  TOTBEN5I PIC  X(0007).
      *    -------------------------------
           05  MOBEN5L PIC S9(0004) COMP.
           05  MOBEN5F PIC  X(0001).
           05  FILLER REDEFINES MOBEN5F.
               10  MOBEN5A PIC  X(0001).
           05  MOBEN5I PIC  X(0005).
      *    -------------------------------
           05  AGE6L PIC S9(0004) COMP.
           05  AGE6F PIC  X(0001).
           05  FILLER REDEFINES AGE6F.
               10  AGE6A PIC  X(0001).
           05  AGE6I PIC  X(0002).
      *    -------------------------------
           05  TERM6L PIC S9(0004) COMP.
           05  TERM6F PIC  X(0001).
           05  FILLER REDEFINES TERM6F.
               10  TERM6A PIC  X(0001).
           05  TERM6I PIC  X(0003).
      *    -------------------------------
           05  TOTBEN6L PIC S9(0004) COMP.
           05  TOTBEN6F PIC  X(0001).
           05  FILLER REDEFINES TOTBEN6F.
               10  TOTBEN6A PIC  X(0001).
           05  TOTBEN6I PIC  X(0007).
      *    -------------------------------
           05  MOBEN6L PIC S9(0004) COMP.
           05  MOBEN6F PIC  X(0001).
           05  FILLER REDEFINES MOBEN6F.
               10  MOBEN6A PIC  X(0001).
           05  MOBEN6I PIC  X(0005).
      *    -------------------------------
           05  AGE7L PIC S9(0004) COMP.
           05  AGE7F PIC  X(0001).
           05  FILLER REDEFINES AGE7F.
               10  AGE7A PIC  X(0001).
           05  AGE7I PIC  X(0002).
      *    -------------------------------
           05  TERM7L PIC S9(0004) COMP.
           05  TERM7F PIC  X(0001).
           05  FILLER REDEFINES TERM7F.
               10  TERM7A PIC  X(0001).
           05  TERM7I PIC  X(0003).
      *    -------------------------------
           05  TOTBEN7L PIC S9(0004) COMP.
           05  TOTBEN7F PIC  X(0001).
           05  FILLER REDEFINES TOTBEN7F.
               10  TOTBEN7A PIC  X(0001).
           05  TOTBEN7I PIC  X(0007).
      *    -------------------------------
           05  MOBEN7L PIC S9(0004) COMP.
           05  MOBEN7F PIC  X(0001).
           05  FILLER REDEFINES MOBEN7F.
               10  MOBEN7A PIC  X(0001).
           05  MOBEN7I PIC  X(0005).
      *    -------------------------------
           05  AGE8L PIC S9(0004) COMP.
           05  AGE8F PIC  X(0001).
           05  FILLER REDEFINES AGE8F.
               10  AGE8A PIC  X(0001).
           05  AGE8I PIC  X(0002).
      *    -------------------------------
           05  TERM8L PIC S9(0004) COMP.
           05  TERM8F PIC  X(0001).
           05  FILLER REDEFINES TERM8F.
               10  TERM8A PIC  X(0001).
           05  TERM8I PIC  X(0003).
      *    -------------------------------
           05  TOTBEN8L PIC S9(0004) COMP.
           05  TOTBEN8F PIC  X(0001).
           05  FILLER REDEFINES TOTBEN8F.
               10  TOTBEN8A PIC  X(0001).
           05  TOTBEN8I PIC  X(0007).
      *    -------------------------------
           05  MOBEN8L PIC S9(0004) COMP.
           05  MOBEN8F PIC  X(0001).
           05  FILLER REDEFINES MOBEN8F.
               10  MOBEN8A PIC  X(0001).
           05  MOBEN8I PIC  X(0005).
      *    -------------------------------
           05  PRMACTL PIC S9(0004) COMP.
           05  PRMACTF PIC  X(0001).
           05  FILLER REDEFINES PRMACTF.
               10  PRMACTA PIC  X(0001).
           05  PRMACTI PIC  X(0008).
      *    -------------------------------
           05  COMACTL PIC S9(0004) COMP.
           05  COMACTF PIC  X(0001).
           05  FILLER REDEFINES COMACTF.
               10  COMACTA PIC  X(0001).
           05  COMACTI PIC  X(0008).
      *    -------------------------------
           05  CLMACTL PIC S9(0004) COMP.
           05  CLMACTF PIC  X(0001).
           05  FILLER REDEFINES CLMACTF.
               10  CLMACTA PIC  X(0001).
           05  CLMACTI PIC  X(0008).
      *    -------------------------------
           05  PRMPCTL PIC S9(0004) COMP.
           05  PRMPCTF PIC  X(0001).
           05  FILLER REDEFINES PRMPCTF.
               10  PRMPCTA PIC  X(0001).
           05  PRMPCTI PIC  X(0006).
      *    -------------------------------
           05  PRMAMTL PIC S9(0004) COMP.
           05  PRMAMTF PIC  X(0001).
           05  FILLER REDEFINES PRMAMTF.
               10  PRMAMTA PIC  X(0001).
           05  PRMAMTI PIC  X(0006).
      *    -------------------------------
           05  STTAXL PIC S9(0004) COMP.
           05  STTAXF PIC  X(0001).
           05  FILLER REDEFINES STTAXF.
               10  STTAXA PIC  X(0001).
           05  STTAXI PIC  X(0006).
      *    -------------------------------
           05  REFPCTL PIC S9(0004) COMP.
           05  REFPCTF PIC  X(0001).
           05  FILLER REDEFINES REFPCTF.
               10  REFPCTA PIC  X(0001).
           05  REFPCTI PIC  X(0006).
      *    -------------------------------
           05  REFAMTL PIC S9(0004) COMP.
           05  REFAMTF PIC  X(0001).
           05  FILLER REDEFINES REFAMTF.
               10  REFAMTA PIC  X(0001).
           05  REFAMTI PIC  X(0006).
      *    -------------------------------
           05  POLFEEL PIC S9(0004) COMP.
           05  POLFEEF PIC  X(0001).
           05  FILLER REDEFINES POLFEEF.
               10  POLFEEA PIC  X(0001).
           05  POLFEEI PIC  X(0006).
      *    -------------------------------
           05  INDGRPL PIC S9(0004) COMP.
           05  INDGRPF PIC  X(0001).
           05  FILLER REDEFINES INDGRPF.
               10  INDGRPA PIC  X(0001).
           05  INDGRPI PIC  X(0001).
      *    -------------------------------
           05  OVSPCTL PIC S9(0004) COMP.
           05  OVSPCTF PIC  X(0001).
           05  FILLER REDEFINES OVSPCTF.
               10  OVSPCTA PIC  X(0001).
           05  OVSPCTI PIC  X(0006).
      *    -------------------------------
           05  OVSAMTL PIC S9(0004) COMP.
           05  OVSAMTF PIC  X(0001).
           05  FILLER REDEFINES OVSAMTF.
               10  OVSAMTA PIC  X(0001).
           05  OVSAMTI PIC  X(0006).
      *    -------------------------------
           05  POLFORML PIC S9(0004) COMP.
           05  POLFORMF PIC  X(0001).
           05  FILLER REDEFINES POLFORMF.
               10  POLFORMA PIC  X(0001).
           05  POLFORMI PIC  X(0012).
      *    -------------------------------
           05  POLEDTL PIC S9(0004) COMP.
           05  POLEDTF PIC  X(0001).
           05  FILLER REDEFINES POLEDTF.
               10  POLEDTA PIC  X(0001).
           05  POLEDTI PIC  X(0001).
      *    -------------------------------
           05  CLMAMTL PIC S9(0004) COMP.
           05  CLMAMTF PIC  X(0001).
           05  FILLER REDEFINES CLMAMTF.
               10  CLMAMTA PIC  X(0001).
           05  CLMAMTI PIC  X(0006).
      *    -------------------------------
           05  CORETL PIC S9(0004) COMP.
           05  CORETF PIC  X(0001).
           05  FILLER REDEFINES CORETF.
               10  CORETA PIC  X(0001).
           05  CORETI PIC  X(0006).
      *    -------------------------------
           05  DEVCDEL PIC S9(0004) COMP.
           05  DEVCDEF PIC  X(0001).
           05  FILLER REDEFINES DEVCDEF.
               10  DEVCDEA PIC  X(0001).
           05  DEVCDEI PIC  X(0003).
      *    -------------------------------
           05  BENGRPL PIC S9(0004) COMP.
           05  BENGRPF PIC  X(0001).
           05  FILLER REDEFINES BENGRPF.
               10  BENGRPA PIC  X(0001).
           05  BENGRPI PIC  X(0005).
      *    -------------------------------
           05  SALTAXL PIC S9(0004) COMP.
           05  SALTAXF PIC  X(0001).
           05  FILLER REDEFINES SALTAXF.
               10  SALTAXA PIC  X(0001).
           05  SALTAXI PIC  X(0006).
      *    -------------------------------
           05  DEVPCTL PIC S9(0004) COMP.
           05  DEVPCTF PIC  X(0001).
           05  FILLER REDEFINES DEVPCTF.
               10  DEVPCTA PIC  X(0001).
           05  DEVPCTI PIC  X(0008).
      *    -------------------------------
           05  ERRMSGL PIC S9(0004) COMP.
           05  ERRMSGF PIC  X(0001).
           05  FILLER REDEFINES ERRMSGF.
               10  ERRMSGA PIC  X(0001).
           05  ERRMSGI PIC  X(0079).
      *    -------------------------------
           05  PFKEYL PIC S9(0004) COMP.
           05  PFKEYF PIC  X(0001).
           05  FILLER REDEFINES PFKEYF.
               10  PFKEYA PIC  X(0001).
           05  PFKEYI PIC  9(2).
       01  EL6507AO REDEFINES EL6507AI.
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
           05  MAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REVISEO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CALCTPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATTAGEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENEC1O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENEC2O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGE1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERM1O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TOTBEN1O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MOBEN1O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGE2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERM2O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TOTBEN2O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MOBEN2O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGE3O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERM3O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TOTBEN3O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MOBEN3O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGE4O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERM4O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TOTBEN4O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MOBEN4O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGE5O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERM5O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TOTBEN5O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MOBEN5O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGE6O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERM6O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TOTBEN6O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MOBEN6O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGE7O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERM7O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TOTBEN7O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MOBEN7O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGE8O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TERM8O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TOTBEN8O PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MOBEN8O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRMACTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMACTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLMACTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRMPCTO PIC  Z.ZZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRMAMTO PIC  ZZZ.ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STTAXO PIC  Z.ZZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFPCTO PIC  Z.ZZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REFAMTO PIC  ZZZ.ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  POLFEEO PIC  ZZ.ZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INDGRPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OVSPCTO PIC  Z.ZZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OVSAMTO PIC  ZZZ.ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  POLFORMO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  POLEDTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLMAMTO PIC  ZZZ.ZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CORETO PIC  Z.ZZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DEVCDEO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENGRPO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SALTAXO PIC  Z.ZZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DEVPCTO PIC  Z.ZZZZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSGO PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFKEYO PIC  99.
      *    -------------------------------
00209  01  FILLER REDEFINES EL6507AI.
101101     12  FILLER                      PIC X(124).
00211      12  WS-MAP-LIMITS OCCURS 8 TIMES.
00212          16  EL6507H-AGEL            PIC S9(04) COMP.
00213          16  EL6507H-AGEA            PIC X(01).
00214          16  EL6507H-AGEI            PIC 9(02).
00215 *        16  EL6507H-AGEO REDEFINES
00216 *            EL6507H-AGEI            PIC ZZ.
00217          16  EL6507H-TERML           PIC S9(04) COMP.
00218          16  EL6507H-TERMA           PIC X(01).
00219          16  EL6507H-TERMI           PIC 9(03).
00220 *        16  EL6507H-TERMO REDEFINES
00221 *            EL6507H-TERMI           PIC ZZZ.
00222          16  EL6507H-TOTBENL         PIC S9(04) COMP.
00223          16  EL6507H-TOTBENA         PIC X(01).
00224          16  EL6507H-TOTBENI         PIC X(07).
00225          16  EL6507H-TOTBENO REDEFINES
00226              EL6507H-TOTBENI         PIC ZZZ,ZZZ.
00227          16  EL6507H-MOBENL          PIC S9(04) COMP.
00228          16  EL6507H-MOBENA          PIC X(01).
00229          16  EL6507H-MOBENI          PIC X(05).
00230          16  EL6507H-MOBENO REDEFINES
00231              EL6507H-MOBENI          PIC Z,ZZZ.
00232
00233      EJECT
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
00235  01  DFHCOMMAREA                     PIC X(1500).
00236
00237      EJECT
00238 *01 PARMLIST .
00239 *    02  FILLER                      PIC S9(8)   COMP.
00240 *    02  ERPLAN-POINTER              PIC S9(8)   COMP.
00241 *    02  ERRATE-POINTER              PIC S9(8)   COMP.
00242 *    02  ELCNTL-POINTER              PIC S9(8)   COMP.
00243 *    02  ERACCT-POINTER              PIC S9(8)   COMP.
00244
00245 *                                    COPY ERCPLAN.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCPLAN                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *   CREDIT SYSTEM ACCOUNT PLAN MASTER FILE                       *
00008 *                                                                *
00009 *   THIS COPYBOOK IS USED FOR THE ONLINE VSAM ACCOUNT            *
00010 *   PLAN MASTER                                                  *
00011 *                                                                *
00012 *   FILE DESCRIPTION = ACCOUNT OR PRODUCER PLAN MASTER           *
00013 *                                                                *
00014 *   FILE TYPE = VSAM,KSDS                                        *
00015 *   RECORD SIZE = 420   RECFORM = FIX                            *
00016 *                                                                *
00017 *   BASE CLUSTER NAME = ERPLAN                    RKP=2,LEN=26   *
00018 *       ALTERNATE PATH1 = N/A                                    *
00019 *                                                                *
00020 *   LOG = NO                                                     *
00021 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00022 *                                                                *
00023 *                                                                *
00024 ******************************************************************
00025
00026  01  PLAN-MASTER.
00027      12  PL-RECORD-ID                      PIC XX.
00028          88  VALID-PL-ID                      VALUE 'PL'.
00029
00030      12  PL-CONTROL-PRIMARY.
00031          16  PL-COMPANY-CD                 PIC X.
00032          16  PL-MSTR-CNTRL.
00033              20  PL-CONTROL-A.
00034                  24  PL-CARRIER            PIC X.
00035                  24  PL-GROUPING           PIC X(6).
00036                  24  PL-STATE              PIC XX.
00037                  24  PL-ACCOUNT            PIC X(10).
00038              20  PL-BENEFIT-TYPE           PIC X.
00039              20  PL-BENEFIT-CODE           PIC XX.
00040              20  PL-REVISION-NO            PIC X(3).
00041
00042      12  PL-CONTROL-BY-VAR-GRP.
00043          16  PL-COMPANY-CD-A1              PIC X.
00044          16  PL-VG-CARRIER                 PIC X.
00045          16  PL-VG-GROUPING                PIC X(6).
00046          16  PL-VG-STATE                   PIC XX.
00047          16  PL-VG-ACCOUNT                 PIC X(10).
00048          16  PL-BENEFIT-TYPE-A1            PIC X.
00049          16  PL-BENEFIT-CODE-A1            PIC XX.
00050          16  PL-REVISION-NO-A1             PIC 999.
00051
00052      12  PL-MAINT-INFORMATION.
00053          16  PL-LAST-MAINT-DT              PIC XX.
00054          16  PL-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
00055          16  PL-LAST-MAINT-USER            PIC X(4).
00056          16  FILLER                        PIC XX.
00057
00058      12  PL-LIABILITY-LIMITS.
00059          16  PL-ATT-AGE                    PIC S99        COMP-3.
00060          16  PL-L-LIMITS        OCCURS 8 TIMES.
00061              20  PL-LM-AGE                 PIC S99        COMP-3.
00062              20  PL-LM-DUR                 PIC S999       COMP-3.
00063              20  PL-LM-MOA                 PIC S9(4)      COMP-3.
00064              20  PL-LM-AMT                 PIC S9(6)      COMP-3.
00065
00066      12  FILLER                            PIC X(30).
00067
00068      12  PL-GL-ACCOUNT-NOS.
00069          16  PL-PREMIUM-GL                 PIC X(8).
00070          16  PL-COMM-GL                    PIC X(8).
00071          16  PL-CLAIM-GL                   PIC X(8).
00072      12  FILLER                            PIC X(24).
00073      12  PL-TOLERANCES.
00074          16  PL-TOL-PREM-AMT               PIC S999V99    COMP-3.
00075          16  PL-TOL-REF-AMT                PIC S999V99    COMP-3.
00076          16  PL-TOL-CLM-AMT                PIC S999V99    COMP-3.
00077          16  PL-TOL-PREM-PCT               PIC S9V9999    COMP-3.
00078          16  PL-TOL-REF-PCT                PIC S9V9999    COMP-3.
00079          16  PL-TOL-CLM-PCT                PIC S9V9999    COMP-3.
00080      12  PL-OVER-SHORT.
00081          16  PL-OVER-SHORT-AMT             PIC S999V99    COMP-3.
00082          16  PL-OVER-SHORT-PCT             PIC S9V9(4)    COMP-3.
00083      12  FILLER                            PIC X(24).
00084      12  PLAN-MISC.
00085          16  PL-POLICY-FEE                 PIC S9(3)V99   COMP-3.
00086          16  PL-STATE-TAX                  PIC S9V9999    COMP-3.
00087          16  PL-IG                         PIC X.
00088          16  PL-RETRO-RET                  PIC S9V9999    COMP-3.
00089          16  PL-POLICY-FORM                PIC X(12).
00090          16  PL-EDIT-FOR-FORM              PIC X(01).
00091          16  PL-DEV-CODE                   PIC XXX.
00092          16  PL-DEV-PCT                    PIC S9V9(6)    COMP-3.
00093          16  PL-CALC-METHOD                PIC X.
00094          16  PL-BENEFIT-GROUP              PIC X(05).
00095          16  PL-SALES-TAX                  PIC S9V9999    COMP-3.
00096
00097          16  FILLER                        PIC X(99).
00098 ******************************************************************
00246      EJECT
00247 *                                    COPY ERCRATE.
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
00248      EJECT
00249 *                                    COPY ELCCNTL.
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
00250      EJECT
00251 *                                    COPY ERCACCT.
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
00252      EJECT
00253
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA PLAN-MASTER
                                RATE-RECORD CONTROL-FILE
                                ACCOUNT-MASTER.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL6507' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00255
00256      MOVE EIBDATE                TO DC-JULIAN-YYDDD
00257      MOVE '5'                    TO DC-OPTION-CODE
00258      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
00259      MOVE DC-GREG-DATE-1-EDIT    TO SAVE-DATE
00260      MOVE DC-BIN-DATE-1          TO SAVE-BIN-DATE
00261      MOVE DC-GREG-DATE-1-YMD     TO YMD-CURRENT-SAVE
00262
00263      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK
00264
00265      MOVE EIBTRMID               TO QID1-TERM
00266                                      QID2-TERM.
00267
00268      IF EIBCALEN = 0
00269          GO TO 8800-UNAUTHORIZED-ACCESS.
00270
00271      IF PI-RETURN-TO-PROGRAM EQUAL THIS-PGM
00272         MOVE PI-CALLING-PROGRAM  TO RETURNED-FROM.
00273
00274      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00275         MOVE THIS-PGM            TO PI-CALLING-PROGRAM.
00276
00277      MOVE LOW-VALUES             TO EL6507AI.
00278
00279      IF EIBTRNID NOT = TRANS-ID
00280         IF RETURNED-FROM = XCTL-6565 OR XCTL-106
00281            MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00282            MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00283            MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00284            MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00285            MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00286            MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00287            MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00288            MOVE SPACES               TO PI-SAVED-PROGRAM-6
00289            PERFORM 6200-RECOVER-TEMP-STORAGE THRU 6200-EXIT.
00290
00291      IF EIBTRNID NOT = TRANS-ID
00292         MOVE PI-PLAN-KEY         TO WS-PLAN-KEY
00293         GO TO 4000-SHOW.
00294
00295      
      * EXEC CICS HANDLE CONDITION
00296 *         PGMIDERR  (9600-PGMID-ERROR)
00297 *         ERROR     (9990-ABEND)
00298 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00004115' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303034313135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00299
00300      IF EIBAID = DFHCLEAR
00301          GO TO 9400-CLEAR.
00302
00303      EJECT
00304  0200-RECEIVE.
00305
00306      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00307          MOVE ER-0008            TO EMI-ERROR
00308          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00309          MOVE -1                 TO PFKEYL
00310          GO TO 8200-SEND-DATAONLY.
00311
00312      
      * EXEC CICS RECEIVE
00313 *        MAP      (MAP-NAME)
00314 *        MAPSET   (MAPSET-NAME)
00315 *        INTO     (EL6507AI)
00316 *    END-EXEC.
           MOVE LENGTH OF
            EL6507AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00004132' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6507AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00317
00318      IF PFKEYL EQUAL +0
00319          GO TO 0300-CHECK-PFKEYS.
00320
00321      IF EIBAID NOT = DFHENTER
00322          MOVE ER-0004            TO EMI-ERROR
00323          GO TO 0320-INPUT-ERROR.
00324
00325      IF (PFKEYI NUMERIC) AND (PFKEYI GREATER 0 AND LESS 25)
00326          MOVE PF-VALUES (PFKEYI) TO EIBAID
00327      ELSE
00328          MOVE ER-0029            TO EMI-ERROR
00329          GO TO 0320-INPUT-ERROR.
00330
00331      EJECT
00332  0300-CHECK-PFKEYS.
00333
00334      IF EIBAID = DFHPF23
00335          GO TO 8810-PF23.
00336
00337      IF EIBAID = DFHPF24
00338          GO TO 9200-RETURN-MAIN-MENU.
00339
00340      IF EIBAID = DFHPF12
00341          GO TO 9500-PF12.
00342
00343      IF EIBAID = DFHPF1
00344          GO TO 0400-BROWSE-FORWARD.
00345
00346      IF EIBAID = DFHPF2
00347          GO TO 0500-BROWSE-BACKWARD.
00348
00349      IF EIBAID = DFHPF4
00350         MOVE PI-ACCT-STATE      TO PI-WS-STATE
00351         PERFORM 6100-CREATE-TEMP-STORAGE THRU 6100-EXIT
00352         MOVE XCTL-106           TO PGM-NAME
00353         GO TO 9300-XCTL.
00354
00355      IF EIBAID = DFHPF3
00356         MOVE PI-ACCT-STATE      TO PI-WS-STATE
00357         MOVE PI-PLAN-BEN        TO PI-WS-PLAN
00358         MOVE PI-PLAN-BEN-TYPE   TO PI-WS-TYPE
00359         MOVE '00'               TO PI-WS-CLASS
00360         PERFORM 6100-CREATE-TEMP-STORAGE THRU 6100-EXIT
00361         MOVE XCTL-6565          TO PGM-NAME
00362         GO TO 9300-XCTL.
00363
00364      IF EIBAID = DFHENTER
00365         GO TO 0330-CHECK-MAINTYP.
00366
00367      MOVE ER-0029                TO EMI-ERROR.
00368
00369  0320-INPUT-ERROR.
00370
00371      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00372      MOVE AL-UNBON               TO PFKEYA
00373      MOVE -1                     TO PFKEYL
00374      GO TO 8200-SEND-DATAONLY.
00375
00376  0330-CHECK-MAINTYP.
00377
00378      IF MAINTL GREATER ZERO
00379         IF MAINTI = 'S' OR 'C' OR 'A' OR 'D'
00380            MOVE AL-UANON         TO MAINTA
00381         ELSE
00382            MOVE -1               TO MAINTL
00383            MOVE AL-UABON         TO MAINTA
00384            MOVE ER-2039          TO EMI-ERROR
00385            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00386            GO TO 8200-SEND-DATAONLY
00387      ELSE
00388         MOVE -1                  TO MAINTL
00389         MOVE AL-UABON            TO MAINTA
00390         MOVE ER-2039             TO EMI-ERROR
00391         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00392         GO TO 8200-SEND-DATAONLY.
00393
00394      PERFORM 7400-BUILD-KEY THRU 7499-EXIT
00395
00396      IF MAINTI EQUAL 'S'
00397         GO TO 4000-SHOW.
00398
00399      PERFORM 7800-COMPANY-REC-READ THRU 7899-EXIT
00400
00401      IF EMI-ERROR NOT = ZEROS
00402         MOVE -1                  TO MAINTL
00403         GO TO 8200-SEND-DATAONLY.
00404
00405      IF MAINTI EQUAL 'A'
00406         GO TO 4100-ADD.
00407
00408      IF MAINTI EQUAL 'D'
00409         GO TO 4500-DELETE.
00410
00411      GO TO 4200-CHANGE.
00412
00413      EJECT
00414
00415  0400-BROWSE-FORWARD.
00416
00417      PERFORM 7400-BUILD-KEY THRU 7499-EXIT
00418
00419      MOVE WS-PLAN-KEY            TO PI-PLAN-KEY
00420
00421      
      * EXEC CICS HANDLE CONDITION
00422 *        NOTFND   (0420-END-FILE)
00423 *        ENDFILE  (0420-END-FILE)
00424 *    END-EXEC
      *    MOVE '"$I''                  ! # #00004241' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303034323431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00425
00426      
      * EXEC CICS STARTBR
00427 *         DATASET  (ERPLAN-FILE)
00428 *         RIDFLD   (WS-PLAN-KEY)
00429 *         GTEQ
00430 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00004246' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPLAN-FILE, 
                 WS-PLAN-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00431
00432  0410-READ-ERPLAN-NEXT.
00433
00434      
      * EXEC CICS READNEXT
00435 *         DATASET  (ERPLAN-FILE)
00436 *         SET      (ADDRESS OF PLAN-MASTER)
00437 *         RIDFLD   (WS-PLAN-KEY)
00438 *    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00004254' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPLAN-FILE, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-PLAN-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PLAN-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00439
00440      IF PI-PLAN-KEY EQUAL WS-PLAN-KEY
00441         GO TO 0410-READ-ERPLAN-NEXT.
00442
00443      IF WS-PLAN-ACCT-KEY EQUAL PI-PLAN-ACCT-KEY
00444         MOVE WS-PLAN-KEY         TO PI-PLAN-KEY
00445         MOVE LOW-VALUES          TO EL6507AO
00446         GO TO 5000-BUILD-INITIAL-SCREEN.
00447
00448  0420-END-FILE.
00449
00450      MOVE -1                     TO MAINTL
00451      MOVE ER-0066                TO EMI-ERROR
00452      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00453      GO TO 8200-SEND-DATAONLY.
00454
00455  0500-BROWSE-BACKWARD.
00456
00457      PERFORM 7400-BUILD-KEY THRU 7499-EXIT
00458
00459      MOVE WS-PLAN-KEY            TO PI-PLAN-KEY
00460
00461      
      * EXEC CICS HANDLE CONDITION
00462 *        NOTFND   (0520-END-FILE)
00463 *        ENDFILE  (0520-END-FILE)
00464 *    END-EXEC
      *    MOVE '"$I''                  ! $ #00004281' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303034323831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00465
00466      
      * EXEC CICS STARTBR
00467 *         DATASET  (ERPLAN-FILE)
00468 *         RIDFLD   (WS-PLAN-KEY)
00469 *         GTEQ
00470 *    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00004286' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPLAN-FILE, 
                 WS-PLAN-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00471
00472      
      * EXEC CICS READNEXT
00473 *         DATASET  (ERPLAN-FILE)
00474 *         SET      (ADDRESS OF PLAN-MASTER)
00475 *         RIDFLD   (WS-PLAN-KEY)
00476 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00004292' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPLAN-FILE, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-PLAN-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PLAN-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00477
00478  0510-READ-ERPLAN-PREV.
00479
00480      
      * EXEC CICS READPREV
00481 *         DATASET  (ERPLAN-FILE)
00482 *         SET      (ADDRESS OF PLAN-MASTER)
00483 *         RIDFLD   (WS-PLAN-KEY)
00484 *    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00004300' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPLAN-FILE, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-PLAN-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PLAN-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00485
00486      IF PI-PLAN-KEY EQUAL WS-PLAN-KEY
00487         GO TO 0510-READ-ERPLAN-PREV.
00488
00489      IF WS-PLAN-ACCT-KEY EQUAL PI-PLAN-ACCT-KEY
00490         MOVE WS-PLAN-KEY         TO PI-PLAN-KEY
00491         MOVE LOW-VALUES          TO EL6507AO
00492         GO TO 5000-BUILD-INITIAL-SCREEN.
00493
00494  0520-END-FILE.
00495
00496      MOVE -1                     TO MAINTL
00497      MOVE ER-0067                TO EMI-ERROR
00498      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00499      GO TO 8200-SEND-DATAONLY.
00500
00501  4000-SHOW.
00502
00503      MOVE WS-PLAN-KEY            TO PI-PLAN-KEY
00504      PERFORM 7150-READ-ERPLAN THRU 7199-EXIT
00505
00506      IF EMI-ERROR EQUAL ER-2938
00507         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00508
00509      IF EMI-NO-ERRORS
00510         NEXT SENTENCE
00511      ELSE
00512         GO TO 8100-SEND-INITIAL-MAP.
00513
00514      MOVE PI-PLAN-KEY            TO WS-PLAN-KEY
00515      MOVE LOW-VALUES             TO EL6507AO
00516      GO TO 5000-BUILD-INITIAL-SCREEN.
00517
00518      EJECT
00519  4100-ADD.
00520
00521      IF NOT MODIFY-CAP
00522         MOVE 'UPDATE'            TO SM-READ
00523         PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00524         MOVE ER-0070             TO EMI-ERROR
00525         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00526         GO TO 8100-SEND-INITIAL-MAP.
00527
00528      MOVE WS-PLAN-KEY            TO PI-PLAN-KEY
00529      PERFORM 7150-READ-ERPLAN THRU 7199-EXIT
00530
00531      IF EMI-ERROR EQUAL ER-2938
00532         MOVE ZEROS               TO EMI-ERROR
00533      ELSE
00534         MOVE ER-0132             TO EMI-ERROR
00535         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00536         GO TO 8100-SEND-INITIAL-MAP.
00537
00538      MOVE WS-PLAN-KEY            TO PI-PLAN-KEY
00539      PERFORM 7000-EDIT THRU 7099-EXIT
00540
00541      IF EMI-NO-ERRORS
00542         NEXT SENTENCE
00543      ELSE
00544         IF EMI-FORCABLE OR EMI-FATAL
00545            GO TO 8200-SEND-DATAONLY.
00546
00547      
      * EXEC CICS GETMAIN
00548 *         LENGTH    (ERPLAN-LENGTH)
00549 *         SET       (ADDRESS OF PLAN-MASTER)
00550 *         INITIMG   (GETMAIN-SPACE)
00551 *    END-EXEC.
      *    MOVE ',"IL                  $   #00004367' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERPLAN-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF PLAN-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00552
00553      MOVE +0                 TO PL-ATT-AGE
00554                                 PL-LM-AGE (1)
00555                                 PL-LM-DUR (1)
00556                                 PL-LM-MOA (1)
00557                                 PL-LM-AMT (1)
00558                                 PL-TOL-PREM-AMT
00559                                 PL-TOL-REF-AMT
00560                                 PL-TOL-CLM-AMT
00561                                 PL-TOL-PREM-PCT
00562                                 PL-TOL-REF-PCT
00563                                 PL-TOL-CLM-PCT
00564                                 PL-POLICY-FEE
00565                                 PL-STATE-TAX
00566                                 PL-RETRO-RET
00567                                 PL-SALES-TAX
00568                                 PL-DEV-PCT.
00569
00570      MOVE PL-L-LIMITS (1)    TO PL-L-LIMITS (2)
00571                                 PL-L-LIMITS (3)
00572                                 PL-L-LIMITS (4)
00573                                 PL-L-LIMITS (5)
00574                                 PL-L-LIMITS (6)
00575                                 PL-L-LIMITS (7)
00576                                 PL-L-LIMITS (8).
00577
00578      MOVE 'PL'               TO PL-RECORD-ID
00579      MOVE PI-PLAN-KEY        TO PL-CONTROL-PRIMARY
00580      MOVE SAVE-BIN-DATE      TO PL-LAST-MAINT-DT
00581      MOVE EIBTIME            TO PL-LAST-MAINT-HHMMSS
00582      MOVE PI-PROCESSOR-ID    TO PL-LAST-MAINT-USER
00583
00584      PERFORM 6000-CHECK-FOR-UPDATE   THRU 6049-EXIT
00585      PERFORM 5100-CHECK-REASONABILITY THRU 5599-EXIT
00586      PERFORM 7500-READ-ERACCT THRU 7699-EXIT
00587
00588      IF EMI-NO-ERRORS
00589         NEXT SENTENCE
00590      ELSE
00591         
      * EXEC CICS FREEMAIN
00592 *            DATA      (PLAN-MASTER)
00593 *       END-EXEC
      *    MOVE ',$D                   "   #00004411' TO DFHEIV0
           MOVE X'2C2444202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PLAN-MASTER
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00594         GO TO 8200-SEND-DATAONLY.
00595
00596      MOVE PLAN-MASTER            TO JP-RECORD-AREA
00597
00598      
      * EXEC CICS WRITE
00599 *        DATASET  (ERPLAN-FILE)
00600 *        RIDFLD   (PI-PLAN-KEY)
00601 *        FROM     (PLAN-MASTER)
00602 *    END-EXEC.
           MOVE LENGTH OF
            PLAN-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004418' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPLAN-FILE, 
                 PLAN-MASTER, 
                 DFHEIV11, 
                 PI-PLAN-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00603
00604      MOVE 'A'                    TO JP-RECORD-TYPE
00605      MOVE ERPLAN-FILE            TO FILE-ID
00606      COMPUTE WS-JOURNAL-FILE-LENGTH EQUAL
00607         ERPLAN-LENGTH + JOURNAL-LENGTH.
00608      PERFORM 8400-LOG-JOURNAL-RECORD
00609
00610      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT
00611      MOVE ER-0000                TO EMI-ERROR
00612      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00613
00614      GO TO 4000-SHOW.
00615
00616  4200-CHANGE.
00617
00618      IF NOT MODIFY-CAP
00619         MOVE 'UPDATE'            TO SM-READ
00620         PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00621         MOVE ER-0070             TO EMI-ERROR
00622         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00623         GO TO 8100-SEND-INITIAL-MAP.
00624
00625      IF PI-PLAN-KEY NOT EQUAL WS-PLAN-KEY
00626         MOVE ER-2056             TO EMI-ERROR
00627         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00628         MOVE 'S'                 TO MAINTO
00629         MOVE -1                  TO MAINTL
00630         MOVE WS-PLAN-KEY         TO PI-PLAN-KEY
00631         GO TO 8200-SEND-DATAONLY.
00632
00633      MOVE WS-PLAN-KEY            TO PI-PLAN-KEY
00634      PERFORM 7000-EDIT THRU 7099-EXIT
00635
00636      IF EMI-NO-ERRORS
00637         NEXT SENTENCE
00638      ELSE
00639         IF EMI-FORCABLE OR EMI-FATAL
00640            GO TO 8200-SEND-DATAONLY.
00641
00642      PERFORM 7300-READ-ERPLAN-UPDATE THRU 7399-EXIT
00643      MOVE PLAN-MASTER            TO JP-RECORD-AREA
00644      MOVE ERPLAN-FILE            TO FILE-ID
00645      MOVE 'B'                    TO JP-RECORD-TYPE
00646      COMPUTE WS-JOURNAL-FILE-LENGTH EQUAL
00647         ERPLAN-LENGTH + JOURNAL-LENGTH
00648
00649      PERFORM 6000-CHECK-FOR-UPDATE   THRU 6049-EXIT
00650      PERFORM 5100-CHECK-REASONABILITY THRU 5599-EXIT
00651
00652      IF EMI-NO-ERRORS
00653         NEXT SENTENCE
00654      ELSE
00655         
      * EXEC CICS UNLOCK
00656 *            DATASET  (ERPLAN-FILE)
00657 *       END-EXEC
      *    MOVE '&*                    #   #00004475' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPLAN-FILE, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00658         GO TO 8200-SEND-DATAONLY.
00659
00660      IF PL-LAST-MAINT-USER   = PI-UPDATE-BY AND
00661         PL-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS
00662         NEXT SENTENCE
00663      ELSE
00664         
      * EXEC CICS UNLOCK
00665 *            DATASET  (ERPLAN-FILE)
00666 *       END-EXEC
      *    MOVE '&*                    #   #00004484' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPLAN-FILE, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00667         MOVE ER-0068            TO EMI-ERROR
00668         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00669         PERFORM 7150-READ-ERPLAN  THRU 7199-EXIT
00670         MOVE LOW-VALUES         TO EL6507AO
00671         GO TO 5000-BUILD-INITIAL-SCREEN.
00672
00673      MOVE PI-PROCESSOR-ID        TO PL-LAST-MAINT-USER
00674      MOVE EIBTIME                TO PL-LAST-MAINT-HHMMSS
00675      MOVE EIBDATE                TO DC-JULIAN-YYDDD
00676      MOVE '5'                    TO DC-OPTION-CODE
00677      MOVE LINK-ELDATCV           TO PGM-NAME
00678
00679      
      * EXEC CICS LINK
00680 *        PROGRAM (PGM-NAME)
00681 *        COMMAREA(DATE-CONVERSION-DATA)
00682 *        LENGTH  (DC-COMM-LENGTH)
00683 *    END-EXEC.
      *    MOVE '."C                   (   #00004499' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00684
00685      MOVE DC-BIN-DATE-1          TO PL-LAST-MAINT-DT
00686                                      BIN-CURRENT-SAVE
00687      PERFORM 8400-LOG-JOURNAL-RECORD
00688      MOVE PLAN-MASTER            TO JP-RECORD-AREA
00689
00690      
      * EXEC CICS REWRITE
00691 *        DATASET  (ERPLAN-FILE)
00692 *        FROM     (PLAN-MASTER)
00693 *    END-EXEC.
           MOVE LENGTH OF
            PLAN-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004510' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPLAN-FILE, 
                 PLAN-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00694
00695      MOVE 'C'                    TO JP-RECORD-TYPE
00696      MOVE ERPLAN-FILE            TO FILE-ID
00697      COMPUTE WS-JOURNAL-FILE-LENGTH EQUAL
00698         ERPLAN-LENGTH + JOURNAL-LENGTH
00699      PERFORM 8400-LOG-JOURNAL-RECORD
00700
00701      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT
00702      MOVE ER-0000                TO EMI-ERROR
00703      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00704
00705      PERFORM 7150-READ-ERPLAN THRU 7199-EXIT
00706      MOVE LOW-VALUES             TO EL6507AO
00707
00708      GO TO 5000-BUILD-INITIAL-SCREEN.
00709
00710      EJECT
00711
00712  4500-DELETE.
00713
00714      IF NOT MODIFY-CAP
00715         MOVE 'UPDATE'            TO SM-READ
00716         PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00717         MOVE ER-0070             TO EMI-ERROR
00718         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00719         GO TO 8100-SEND-INITIAL-MAP.
00720
00721      IF PI-PLAN-KEY NOT EQUAL WS-PLAN-KEY
00722         MOVE ER-2056             TO EMI-ERROR
00723         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00724         MOVE 'S'                 TO MAINTO
00725         MOVE -1                  TO MAINTL
00726         MOVE WS-PLAN-KEY         TO PI-PLAN-KEY
00727         GO TO 8200-SEND-DATAONLY.
00728
00729      PERFORM 7300-READ-ERPLAN-UPDATE THRU 7399-EXIT
00730
00731      IF PL-LAST-MAINT-USER   = PI-UPDATE-BY AND
00732         PL-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS
00733         NEXT SENTENCE
00734      ELSE
00735         
      * EXEC CICS UNLOCK
00736 *            DATASET  (ERPLAN-FILE)
00737 *       END-EXEC
      *    MOVE '&*                    #   #00004555' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPLAN-FILE, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00738         MOVE ER-0068            TO EMI-ERROR
00739         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00740         PERFORM 7150-READ-ERPLAN  THRU 7199-EXIT
00741         MOVE LOW-VALUES         TO EL6507AO
00742         GO TO 5000-BUILD-INITIAL-SCREEN.
00743
00744      MOVE PLAN-MASTER            TO JP-RECORD-AREA
00745      MOVE ERPLAN-FILE            TO FILE-ID
00746      MOVE 'D'                    TO JP-RECORD-TYPE
00747      COMPUTE WS-JOURNAL-FILE-LENGTH EQUAL
00748         ERPLAN-LENGTH + JOURNAL-LENGTH
00749
00750      
      * EXEC CICS DELETE
00751 *        DATASET  (ERPLAN-FILE)
00752 *    END-EXEC.
      *    MOVE '&(                    &   #00004570' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPLAN-FILE, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00753
00754      PERFORM 8400-LOG-JOURNAL-RECORD
00755      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT
00756      MOVE ER-0000                TO EMI-ERROR
00757      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00758
00759      MOVE LOW-VALUES             TO EL6507AO
00760      MOVE 'S'                    TO MAINTO
00761      GO TO 8100-SEND-INITIAL-MAP.
00762
00763  5000-BUILD-INITIAL-SCREEN.
00764
00765      MOVE 'S'                    TO MAINTO
00766      MOVE AL-UANON               TO MAINTA
00767      MOVE -1                     TO MAINTL
00768      MOVE PI-PLAN-CARRIER        TO CARRO
00769      MOVE PI-PLAN-GROUP          TO GROUPO
00770      MOVE PI-PLAN-STATE          TO STATEO
00771      MOVE PI-PLAN-ACCOUNT        TO ACCTO
00772      MOVE PI-PLAN-BEN-TYPE       TO TYPEO
00773      MOVE PI-PLAN-BEN            TO BENEO
00774      MOVE PI-PLAN-REVISION       TO REVISEO
00775
00776      MOVE PL-LAST-MAINT-USER     TO PI-UPDATE-BY
00777      MOVE PL-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS
00778
00779      IF PL-ATT-AGE NUMERIC AND
00780         PL-ATT-AGE GREATER THAN ZEROS
00781         MOVE PL-ATT-AGE          TO ATTAGEO
00782         MOVE AL-UNNON            TO ATTAGEA
00783         MOVE +2                  TO ATTAGEL.
00784
00785      IF PL-BENEFIT-TYPE EQUAL PI-LIFE-OVERRIDE-L1
00786         MOVE AL-SADOF            TO BENEC1A
00787                                     BENEC2A
00788                                     EL6507H-MOBENA (1)
00789                                     EL6507H-MOBENA (2)
00790                                     EL6507H-MOBENA (3)
00791                                     EL6507H-MOBENA (4)
00792                                     EL6507H-MOBENA (5)
00793                                     EL6507H-MOBENA (6)
00794                                     EL6507H-MOBENA (7)
00795                                     EL6507H-MOBENA (8).
00796
00797      MOVE PL-CALC-METHOD         TO CALCTPO
00798      MOVE +1 TO SUB.
00799
00800  5010-BUILD-LIMITS.
00801
00802      IF SUB GREATER THAN +8
00803         GO TO 5020-END-LIMITS.
00804
00805      IF (PL-LM-AGE (SUB) NUMERIC) AND
00806         (PL-LM-AGE (SUB) GREATER THAN ZEROS)
00807         MOVE PL-LM-AGE (SUB)     TO EL6507H-AGEI    (SUB)
00808         MOVE AL-UNNON            TO EL6507H-AGEA    (SUB)
00809         MOVE +2                  TO EL6507H-AGEL    (SUB)
00810         MOVE PL-LM-DUR (SUB)     TO EL6507H-TERMI   (SUB)
00811         MOVE AL-UNNON            TO EL6507H-TERMA   (SUB)
00812         MOVE +3                  TO EL6507H-TERML   (SUB)
00813         MOVE PL-LM-AMT (SUB)     TO EL6507H-TOTBENO (SUB)
00814         MOVE AL-UANON            TO EL6507H-TOTBENA (SUB)
00815         MOVE +7                  TO EL6507H-TOTBENL (SUB).
00816
00817      IF PL-BENEFIT-TYPE EQUAL PI-AH-OVERRIDE-L1
00818         MOVE PL-LM-MOA (SUB)     TO EL6507H-MOBENO  (SUB)
00819         MOVE AL-UNNON            TO EL6507H-MOBENA  (SUB)
00820         MOVE +6                  TO EL6507H-MOBENL  (SUB).
00821
00822      ADD +1 TO SUB
00823      GO TO 5010-BUILD-LIMITS.
00824
00825  5020-END-LIMITS.
00826
00827      MOVE PL-PREMIUM-GL          TO PRMACTO
00828      MOVE AL-UANON               TO PRMACTA
00829      MOVE +8                     TO PRMACTL
00830
00831      MOVE PL-COMM-GL             TO COMACTO
00832      MOVE AL-UANON               TO COMACTA
00833      MOVE +8                     TO COMACTL
00834
00835      MOVE PL-CLAIM-GL            TO CLMACTO
00836      MOVE AL-UANON               TO CLMACTA
00837      MOVE +8                     TO CLMACTL
00838
00839      MOVE PL-TOL-PREM-AMT        TO PRMAMTO
00840      MOVE AL-UANON               TO PRMAMTA
00841      MOVE +6                     TO PRMAMTL
00842
00843      MOVE PL-TOL-REF-AMT         TO REFAMTO
00844      MOVE AL-UANON               TO REFAMTA
00845      MOVE +6                     TO REFAMTL
00846
00847      MOVE PL-TOL-CLM-AMT         TO CLMAMTO
00848      MOVE AL-UANON               TO CLMAMTA
00849      MOVE +8                     TO CLMAMTL
00850
00851      MOVE PL-TOL-PREM-PCT        TO PRMPCTO
00852      MOVE AL-UANON               TO PRMPCTA
00853      MOVE +6                     TO PRMPCTL
00854
00855      MOVE PL-TOL-REF-PCT         TO REFPCTO
00856      MOVE AL-UANON               TO REFPCTA
00857      MOVE +6                     TO REFPCTL
00858
00859      MOVE PL-STATE-TAX           TO STTAXO
00860      MOVE AL-UANON               TO STTAXA
00861      MOVE +6                     TO STTAXL
00862
00863      MOVE PL-POLICY-FEE          TO POLFEEO
00864      MOVE AL-UANON               TO POLFEEA
00865      MOVE +6                     TO POLFEEL
00866
00867      IF PL-OVER-SHORT-AMT  NUMERIC
00868         MOVE PL-OVER-SHORT-AMT   TO OVSAMTO
00869         MOVE +6                     TO OVSAMTL
00870         MOVE AL-UANON               TO OVSAMTA
00871      END-IF.
00872
00873      IF PL-OVER-SHORT-PCT  NUMERIC
00874         MOVE PL-OVER-SHORT-PCT TO OVSPCTO
00875         MOVE +6                     TO OVSPCTL
00876         MOVE AL-UANON  TO OVSPCTA
00877      END-IF.
00878
00879      IF PL-POLICY-FORM GREATER THAN SPACES
00880          MOVE PL-POLICY-FORM     TO POLFORMO
00881          MOVE AL-UANON           TO POLFORMA
00882          MOVE +6                 TO POLFORML.
00883
00884      MOVE PL-EDIT-FOR-FORM       TO POLEDTO
00885      MOVE AL-UANON               TO POLEDTA
00886      MOVE +1                     TO POLEDTL
00887
00888      MOVE PL-RETRO-RET           TO CORETO
00889      MOVE AL-UANON               TO CORETA
00890      MOVE +6                     TO CORETL
00891
00892      MOVE PL-DEV-CODE            TO DEVCDEO
00893      IF PL-DEV-CODE NOT = SPACES
00894          MOVE AL-UANON           TO DEVCDEA
00895          MOVE +3                 TO DEVCDEL.
00896
00897      MOVE PL-DEV-PCT             TO DEVPCTO
00898      MOVE AL-UANON               TO DEVPCTA
00899      MOVE +8                     TO DEVPCTL
00900
00901      MOVE PL-BENEFIT-GROUP       TO BENGRPO
00902      MOVE AL-UANON               TO BENGRPA
00903      MOVE +5                     TO BENGRPL
00904
00905      MOVE PL-IG                  TO INDGRPO
00906
00907      IF INDGRPI = 'I' OR 'G'
00908          MOVE AL-UANON           TO INDGRPA
00909          MOVE +1                 TO INDGRPL.
00910
00911      MOVE PL-DEV-CODE            TO PI-WS-DEV
00912
00913      IF PI-WS-DEV EQUAL SPACES
00914         MOVE '000'               TO PI-WS-DEV.
00915
00916      MOVE PL-SALES-TAX           TO SALTAXO
00917      MOVE AL-UANON               TO SALTAXA
00918      MOVE +6                     TO SALTAXL.
00919
00920      GO TO 8100-SEND-INITIAL-MAP.
00921
00922  5099-EXIT.
00923      EXIT.
00924
00925      EJECT
00926  5100-CHECK-REASONABILITY.
00927
00928      MOVE +1                      TO SUB.
00929
00930  5200-CHECK-NUMERIC.
00931
00932      IF SUB GREATER THAN +8
00933         MOVE +1                   TO SUB
00934         GO TO 5300-LIMITS-NUMERIC.
00935
00936      IF PL-LM-AGE (SUB) NOT NUMERIC
00937         MOVE +0                   TO PL-LM-AGE (SUB).
00938
00939      IF PL-LM-DUR (SUB) NOT NUMERIC
00940         MOVE +0                   TO PL-LM-DUR (SUB).
00941
00942      IF PL-LM-MOA (SUB) NOT NUMERIC
00943         MOVE +0                   TO PL-LM-MOA (SUB).
00944
00945      IF PL-LM-AMT (SUB) NOT NUMERIC
00946         MOVE +0                   TO PL-LM-AMT (SUB).
00947
00948      ADD +1 TO SUB
00949      GO TO 5200-CHECK-NUMERIC.
00950
00951  5300-LIMITS-NUMERIC.
00952
00953      IF SUB GREATER THAN +8
00954         MOVE +1 TO SUB
00955         MOVE +2 TO SUB1
00956         GO TO 5500-CONTINUE.
00957
00958      IF PL-BENEFIT-TYPE EQUAL PI-AH-OVERRIDE-L1
00959         GO TO 5400-AH-LIMITS.
00960
00961      IF (+0 LESS THAN PL-LM-AGE (SUB) OR
00962                       PL-LM-DUR (SUB) OR
00963                       PL-LM-AMT (SUB))
00964         IF (PL-LM-AGE (SUB) GREATER THAN +0) AND
00965            (PL-LM-DUR (SUB) GREATER THAN +0) AND
00966            (PL-LM-AMT (SUB) GREATER THAN +0)
00967            NEXT SENTENCE
00968         ELSE
00969            MOVE -1              TO EL6507H-AGEL (SUB)
00970            MOVE AL-UABON        TO EL6507H-AGEA (SUB)
00971            MOVE ER-2106         TO EMI-ERROR
00972            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00973            MOVE +1 TO SUB
00974            MOVE +2 TO SUB1
00975            GO TO 5500-CONTINUE.
00976
00977      ADD +1 TO SUB
00978      GO TO 5300-LIMITS-NUMERIC.
00979
00980  5400-AH-LIMITS.
00981
00982      IF SUB GREATER THAN +8
00983         MOVE +1 TO SUB
00984         MOVE +2 TO SUB1
00985         GO TO 5500-CONTINUE.
00986
00987      IF (+0 LESS THAN PL-LM-AGE (SUB) OR
00988                       PL-LM-DUR (SUB) OR
00989                       PL-LM-AMT (SUB) OR
00990                       PL-LM-MOA (SUB))
00991         IF (PL-LM-AGE (SUB) GREATER THAN +0) AND
00992            (PL-LM-DUR (SUB) GREATER THAN +0) AND
00993            (PL-LM-AMT (SUB) GREATER THAN +0) AND
00994            (PL-LM-MOA (SUB) GREATER THAN +0)
00995            NEXT SENTENCE
00996         ELSE
00997            MOVE -1              TO EL6507H-AGEL (SUB)
00998            MOVE AL-UABON        TO EL6507H-AGEA (SUB)
00999            MOVE ER-2107         TO EMI-ERROR
01000            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01001            MOVE +1 TO SUB
01002            MOVE +2 TO SUB1
01003            GO TO 5500-CONTINUE.
01004
01005      ADD +1 TO SUB
01006      GO TO 5400-AH-LIMITS.
01007
01008  5500-CONTINUE.
01009
01010      IF EMI-NO-ERRORS
01011         NEXT SENTENCE
01012      ELSE
01013         GO TO 5599-EXIT.
01014
01015      IF SUB1 GREATER THAN +8
01016         GO TO 5599-EXIT.
01017
01018      IF (PL-LM-AGE (SUB) GREATER THAN +0) AND
01019         (PL-LM-AGE (SUB1) GREATER THAN +0)
01020         IF PL-LM-AGE (SUB1) LESS THAN PL-LM-AGE (SUB)
01021            MOVE -1              TO EL6507H-AGEL (SUB1)
01022            MOVE AL-UABON        TO EL6507H-AGEA (SUB1)
01023            MOVE ER-2274         TO EMI-ERROR
01024            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01025
01026      ADD +1 TO SUB SUB1
01027      GO TO 5500-CONTINUE.
01028
01029  5599-EXIT.
01030      EXIT.
01031
01032      EJECT
01033  6000-CHECK-FOR-UPDATE.
01034
01035      IF ATTAGEL GREATER THAN +0
01036         MOVE ATTAGEI              TO PL-ATT-AGE.
01037
01038      MOVE +1 TO SUB.
01039
01040  6010-CHECK-LIMITS.
01041
01042      IF SUB GREATER THAN +8
01043         GO TO 6020-END-CHECK-LIMITS.
01044
01045      IF EL6507H-AGEL (SUB) GREATER +0
01046         MOVE EL6507H-AGEI (SUB)   TO PL-LM-AGE (SUB).
01047
01048      IF EL6507H-TERML (SUB) GREATER +0
01049         MOVE EL6507H-TERMI (SUB)  TO PL-LM-DUR (SUB).
01050
01051      IF EL6507H-MOBENL (SUB) GREATER +0
01052         MOVE WS-MOBENI (SUB)      TO PL-LM-MOA (SUB).
01053
01054      IF EL6507H-TOTBENL (SUB) GREATER +0
01055         MOVE WS-TOTBENI (SUB)     TO PL-LM-AMT (SUB).
01056
01057      ADD +1 TO SUB
01058      GO TO 6010-CHECK-LIMITS.
01059
01060  6020-END-CHECK-LIMITS.
01061
01062      IF PRMACTL GREATER THAN +0
01063         MOVE PRMACTI              TO PL-PREMIUM-GL.
01064
01065      IF COMACTL GREATER THAN +0
01066         MOVE COMACTI              TO PL-COMM-GL.
01067
01068      IF CLMACTL GREATER THAN +0
01069         MOVE CLMACTI              TO PL-CLAIM-GL.
01070
01071      IF PRMAMTL GREATER THAN +0
01072         MOVE WS-PRMAMTI           TO PL-TOL-PREM-AMT.
01073
01074      IF REFAMTL GREATER THAN +0
01075         MOVE WS-REFAMTI           TO PL-TOL-REF-AMT.
01076
01077      IF OVSAMTL > +0
01078         MOVE WS-OVSAMTI TO PL-OVER-SHORT-AMT
01079      END-IF.
01080
01081      IF CLMAMTL GREATER THAN +0
01082         MOVE WS-CLMAMTI           TO PL-TOL-CLM-AMT.
01083
01084      IF PRMPCTL GREATER THAN +0
01085         MOVE WS-PRMPCTI           TO PL-TOL-PREM-PCT.
01086
01087      IF REFPCTL GREATER THAN +0
01088         MOVE WS-REFPCTI           TO PL-TOL-REF-PCT.
01089
01090      IF OVSPCTL > +0
01091         MOVE WS-OVSPCTI TO PL-OVER-SHORT-PCT
01092      END-IF.
01093
01094      IF POLFEEL GREATER THAN +0
01095         MOVE WS-POLFEEI           TO PL-POLICY-FEE.
01096
01097      IF STTAXL GREATER THAN +0
01098         MOVE WS-STTAXI            TO PL-STATE-TAX.
01099
01100      IF INDGRPL GREATER THAN +0
01101         MOVE INDGRPI              TO PL-IG.
01102
01103      IF CORETL GREATER THAN +0
01104         MOVE WS-CORETI            TO PL-RETRO-RET.
01105
01106      IF POLFORML GREATER THAN +0
01107         MOVE POLFORMI             TO PL-POLICY-FORM.
01108
01109      IF POLEDTL GREATER THAN +0
01110         MOVE POLEDTI              TO PL-EDIT-FOR-FORM.
01111
01112      IF DEVCDEL GREATER THAN +0
01113         MOVE DEVCDEI              TO PL-DEV-CODE.
01114
01115      IF DEVPCTL GREATER THAN +0
01116         MOVE WS-DEVPCTI           TO PL-DEV-PCT.
01117
01118      IF CALCTPL GREATER THAN +0
01119         MOVE CALCTPI              TO PL-CALC-METHOD.
01120
01121      IF BENGRPL GREATER THAN +0
01122         MOVE BENGRPI              TO PL-BENEFIT-GROUP.
01123
01124      IF SALTAXL GREATER THAN +0
01125         MOVE WS-SALTAXI           TO PL-SALES-TAX.
01126
01127  6049-EXIT.
01128      EXIT.
01129      EJECT
01130
01131  6100-CREATE-TEMP-STORAGE.
01132
01133      PERFORM 6300-DELETE-TEMP-STORAGE THRU 6300-EXIT
01134
01135      
      * EXEC CICS WRITEQ TS
01136 *        QUEUE   (QID1)
01137 *        FROM    (EL6507AO)
01138 *        LENGTH  (WS-MAP-LENGTH)
01139 *    END-EXEC.
      *    MOVE '*"                    ''   #00004955' TO DFHEIV0
           MOVE X'2A2220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID1, 
                 EL6507AO, 
                 WS-MAP-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01140
01141      
      * EXEC CICS WRITEQ TS
01142 *        QUEUE   (QID2)
01143 *        FROM    (PROGRAM-INTERFACE-BLOCK)
01144 *        LENGTH  (WS-COMM-LENGTH)
01145 *    END-EXEC.
      *    MOVE '*"                    ''   #00004961' TO DFHEIV0
           MOVE X'2A2220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID2, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01146
01147  6100-EXIT.
01148       EXIT.
01149
01150  6200-RECOVER-TEMP-STORAGE.
01151      
      * EXEC CICS READQ TS
01152 *        QUEUE   (QID1)
01153 *        INTO    (EL6507AO)
01154 *        LENGTH  (WS-MAP-LENGTH)
01155 *    END-EXEC.
      *    MOVE '*$I    L              ''   #00004971' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID1, 
                 EL6507AO, 
                 WS-MAP-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01156
01157      
      * EXEC CICS READQ TS
01158 *        QUEUE   (QID2)
01159 *        INTO    (PROGRAM-INTERFACE-BLOCK)
01160 *        LENGTH  (WS-COMM-LENGTH)
01161 *    END-EXEC.
      *    MOVE '*$I    L              ''   #00004977' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID2, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01162
01163      PERFORM 6300-DELETE-TEMP-STORAGE THRU 6300-EXIT.
01164
01165  6200-EXIT.
01166       EXIT.
01167
01168  6300-DELETE-TEMP-STORAGE.
01169
01170      
      * EXEC CICS HANDLE CONDITION
01171 *        QIDERR  (6300-EXIT)
01172 *    END-EXEC.
      *    MOVE '"$N                   ! % #00004990' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303034393930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01173
01174      
      * EXEC CICS DELETEQ TS
01175 *        QUEUE  (QID1)
01176 *    END-EXEC.
      *    MOVE '*&                    #   #00004994' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID1, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01177
01178      
      * EXEC CICS DELETEQ TS
01179 *        QUEUE  (QID2)
01180 *    END-EXEC.
      *    MOVE '*&                    #   #00004998' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID2, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01181
01182  6300-EXIT.
01183       EXIT.
01184
01185      EJECT
01186  7000-EDIT.
01187
01188      IF ATTAGEL GREATER THAN +0
01189         IF ATTAGEI NUMERIC
01190            MOVE AL-UNNON          TO ATTAGEA
01191         ELSE
01192            MOVE -1                TO ATTAGEL
01193            MOVE AL-UNBON          TO ATTAGEA
01194            MOVE ER-2273           TO EMI-ERROR
01195            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01196
01197      MOVE +1 TO SUB.
01198
01199  7010-EDIT-LIMITS.
01200
01201      IF SUB GREATER THAN +8
01202         GO TO 7020-END-EDIT-LIMITS.
01203
01204      IF (EL6507H-AGEL (SUB) EQUAL +0) AND
01205         (EL6507H-TERML (SUB) EQUAL +0) AND
01206         (EL6507H-MOBENL (SUB) EQUAL +0) AND
01207         (EL6507H-TOTBENL (SUB) EQUAL +0)
01208         ADD +1 TO SUB
01209         GO TO 7010-EDIT-LIMITS.
01210
01211      IF EL6507H-AGEL (SUB) GREATER +0
01212         IF EL6507H-AGEI (SUB) NUMERIC
01213            MOVE AL-UNNON          TO EL6507H-AGEA (SUB)
01214         ELSE
01215            MOVE -1                TO EL6507H-AGEL (SUB)
01216            MOVE AL-UNBON          TO EL6507H-AGEA (SUB)
01217            MOVE ER-2275           TO EMI-ERROR
01218            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01219
01220      IF EL6507H-TERML (SUB) GREATER +0
01221         IF EL6507H-TERMI (SUB) NUMERIC
01222            MOVE AL-UNNON          TO EL6507H-TERMA (SUB)
01223         ELSE
01224            MOVE -1                TO EL6507H-TERML (SUB)
01225            MOVE AL-UNBON          TO EL6507H-TERMA (SUB)
01226            MOVE ER-2276           TO EMI-ERROR
01227            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01228
01229      IF EL6507H-MOBENL (SUB) GREATER +0
01230         MOVE EL6507H-MOBENI (SUB) TO DEEDIT-FIELD
01231         PERFORM 8600-DEEDIT
01232         IF DEEDIT-FIELD-V0 NUMERIC
01233            MOVE DEEDIT-FIELD-V0   TO WS-MOBENI (SUB)
01234                                      EL6507H-MOBENO (SUB)
01235            MOVE AL-UANON          TO EL6507H-MOBENA (SUB)
01236         ELSE
01237            MOVE -1                TO EL6507H-MOBENL (SUB)
01238            MOVE AL-UABON          TO EL6507H-MOBENA (SUB)
01239            MOVE ER-3127           TO EMI-ERROR
01240            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01241
01242      IF EL6507H-TOTBENL (SUB) GREATER +0
01243         MOVE EL6507H-TOTBENI (SUB) TO DEEDIT-FIELD
01244         PERFORM 8600-DEEDIT
01245         IF DEEDIT-FIELD-V0 NUMERIC
01246            MOVE DEEDIT-FIELD-V0   TO WS-TOTBENI (SUB)
01247                                      EL6507H-TOTBENO (SUB)
01248            MOVE AL-UANON          TO EL6507H-TOTBENA (SUB)
01249         ELSE
01250            MOVE -1                TO EL6507H-TOTBENL (SUB)
01251            MOVE AL-UABON          TO EL6507H-TOTBENA (SUB)
01252            MOVE ER-3128           TO EMI-ERROR
01253            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01254
01255      ADD +1 TO SUB
01256      GO TO 7010-EDIT-LIMITS.
01257
01258  7020-END-EDIT-LIMITS.
01259
01260      IF PRMPCTL GREATER THAN +0
01261         MOVE PRMPCTI              TO DEEDIT-FIELD
01262         PERFORM 8600-DEEDIT
01263         IF DEEDIT-FIELD-V4 NUMERIC
01264            MOVE DEEDIT-FIELD-V4   TO WS-PRMPCTI
01265                                      PRMPCTO
01266            MOVE AL-UANON          TO PRMPCTA
01267         ELSE
01268            MOVE -1                TO PRMPCTL
01269            MOVE AL-UABON          TO PRMPCTA
01270            MOVE ER-2168           TO EMI-ERROR
01271            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01272
01273      IF PRMAMTL GREATER THAN +0
01274         MOVE PRMAMTI              TO DEEDIT-FIELD
01275         PERFORM 8600-DEEDIT
01276         IF DEEDIT-FIELD-V2 NUMERIC
01277            MOVE DEEDIT-FIELD-V2   TO WS-PRMAMTI
01278                                      PRMAMTO
01279            MOVE AL-UANON          TO PRMAMTA
01280         ELSE
01281            MOVE -1                TO PRMAMTL
01282            MOVE AL-UABON          TO PRMAMTA
01283            MOVE ER-2168           TO EMI-ERROR
01284            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01285
01286      IF REFPCTL GREATER THAN +0
01287         MOVE REFPCTI              TO DEEDIT-FIELD
01288         PERFORM 8600-DEEDIT
01289         IF DEEDIT-FIELD-V4 NUMERIC
01290            MOVE DEEDIT-FIELD-V4   TO WS-REFPCTI
01291                                      REFPCTO
01292            MOVE AL-UANON          TO REFPCTA
01293         ELSE
01294            MOVE -1                TO REFPCTL
01295            MOVE AL-UABON          TO REFPCTA
01296            MOVE ER-2169           TO EMI-ERROR
01297            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01298
01299      IF REFAMTL GREATER THAN +0
01300         MOVE REFAMTI              TO DEEDIT-FIELD
01301         PERFORM 8600-DEEDIT
01302         IF DEEDIT-FIELD-V2 NUMERIC
01303            MOVE DEEDIT-FIELD-V2   TO WS-REFAMTI
01304                                      REFAMTO
01305            MOVE AL-UANON          TO REFAMTA
01306         ELSE
01307            MOVE -1                TO REFAMTL
01308            MOVE AL-UABON          TO REFAMTA
01309            MOVE ER-2169           TO EMI-ERROR
01310            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01311
01312      IF OVSPCTL GREATER THAN +0
01313         MOVE OVSPCTI              TO  DEEDIT-FIELD
01314         PERFORM 8600-DEEDIT
01315         IF DEEDIT-FIELD-V4 NUMERIC
01316            MOVE DEEDIT-FIELD-V4   TO WS-OVSPCTI
01317                                      OVSPCTO
01318            MOVE AL-UANON          TO OVSPCTA
01319         ELSE
01320            MOVE -1                TO OVSPCTL
01321            MOVE AL-UABON          TO OVSPCTA
01322            MOVE ER-2169           TO EMI-ERROR
01323            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01324
01325      IF OVSAMTL GREATER THAN +0
01326         MOVE OVSAMTI              TO  DEEDIT-FIELD
01327         PERFORM 8600-DEEDIT
01328         IF DEEDIT-FIELD-V2 NUMERIC
01329            MOVE DEEDIT-FIELD-V2   TO WS-OVSAMTI
01330                                      OVSAMTO
01331            MOVE AL-UANON          TO OVSAMTA
01332         ELSE
01333            MOVE -1                TO OVSAMTL
01334            MOVE AL-UABON          TO OVSAMTA
01335            MOVE ER-2169           TO EMI-ERROR
01336            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01337
01338      IF DEVPCTI GREATER THAN +0
01339         MOVE DEVPCTI              TO DEEDIT-FIELD
01340         PERFORM 8600-DEEDIT
01341         IF DEEDIT-FIELD-V6 NUMERIC
01342            MOVE DEEDIT-FIELD-V5   TO WS-DEVPCTI
01343                                      DEVPCTO
01344            MOVE AL-UANON          TO DEVPCTA
01345         ELSE
01346            MOVE -1                TO DEVPCTL
01347            MOVE AL-UABON          TO DEVPCTA
01348            MOVE ER-3126           TO EMI-ERROR
01349            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01350
01351      IF CLMAMTL GREATER THAN +0
01352         MOVE CLMAMTI              TO DEEDIT-FIELD
01353         PERFORM 8600-DEEDIT
01354         IF DEEDIT-FIELD-V2 NUMERIC
01355            MOVE DEEDIT-FIELD-V2   TO WS-CLMAMTI
01356                                      CLMAMTO
01357            MOVE AL-UANON          TO CLMAMTA
01358         ELSE
01359            MOVE -1                TO CLMAMTL
01360            MOVE AL-UABON          TO CLMAMTA
01361            MOVE ER-2170           TO EMI-ERROR
01362            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01363
01364      IF STTAXL GREATER THAN +0
01365         MOVE STTAXI               TO DEEDIT-FIELD
01366         PERFORM 8600-DEEDIT
01367         IF DEEDIT-FIELD-V4 NUMERIC
01368            MOVE DEEDIT-FIELD-V4   TO WS-STTAXI
01369                                      STTAXO
01370            MOVE AL-UANON          TO STTAXA
01371         ELSE
01372            MOVE -1                TO STTAXL
01373            MOVE AL-UABON          TO STTAXA
01374            MOVE ER-7208           TO EMI-ERROR
01375            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01376
01377      IF POLFEEL GREATER THAN +0
01378         MOVE POLFEEI              TO DEEDIT-FIELD
01379         PERFORM 8600-DEEDIT
01380         IF DEEDIT-FIELD-V3 NUMERIC
01381            MOVE DEEDIT-FIELD-V3   TO WS-POLFEEI
01382                                      POLFEEO
01383            MOVE AL-UANON          TO POLFEEA
01384         ELSE
01385            MOVE -1                TO POLFEEL
01386            MOVE AL-UABON          TO POLFEEA
01387            MOVE ER-9407           TO EMI-ERROR
01388            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01389
01390      IF INDGRPL GREATER THAN +0
01391         IF INDGRPI EQUAL 'I' OR 'G'
01392            MOVE AL-UANON          TO INDGRPA
01393         ELSE
01394            MOVE -1                TO INDGRPL
01395            MOVE AL-UABON          TO INDGRPA
01396            MOVE ER-2152           TO EMI-ERROR
01397            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01398
01399      IF POLEDTL GREATER +0
01400         IF POLEDTI EQUAL 'Y' OR 'N' OR ' '
01401            MOVE AL-UANON          TO POLEDTA
01402         ELSE
01403            MOVE -1                TO POLEDTL
01404            MOVE AL-UABON          TO POLEDTA
01405            MOVE ER-0627           TO EMI-ERROR
01406            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01407
01408      IF (POLEDTL GREATER THAN +0) AND
01409         (POLEDTI EQUAL 'Y')
01410         IF POLFORML EQUAL +0
01411            MOVE -1                TO POLFORML
01412            MOVE AL-UNBON          TO POLFORMA
01413            MOVE ER-2937           TO EMI-ERROR
01414            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01415
01416      IF CORETL GREATER THAN +0
01417         MOVE CORETI               TO DEEDIT-FIELD
01418         PERFORM 8600-DEEDIT
01419         IF DEEDIT-FIELD-V4 NUMERIC
01420            MOVE DEEDIT-FIELD-V4   TO WS-CORETI
01421                                      CORETO
01422            MOVE AL-UANON          TO CORETA
01423         ELSE
01424            MOVE -1                TO CORETL
01425            MOVE AL-UABON          TO CORETA
01426            MOVE ER-9055           TO EMI-ERROR
01427            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01428
01429      IF SALTAXL GREATER THAN +0
01430         MOVE SALTAXI              TO DEEDIT-FIELD
01431         PERFORM 8600-DEEDIT
01432         IF DEEDIT-FIELD-V4 NUMERIC
01433            MOVE DEEDIT-FIELD-V4   TO WS-SALTAXI
01434                                      SALTAXO
01435            MOVE AL-UANON          TO SALTAXA
01436         ELSE
01437            MOVE -1                TO SALTAXL
01438            MOVE AL-UABON          TO SALTAXA
01439            MOVE ER-7208           TO EMI-ERROR
01440            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01441
01442  7030-CHECK-LF-DEVIATION.
01443
01444      IF (DEVCDEL GREATER THAN +0) AND
01445         (TYPEI EQUAL PI-LIFE-OVERRIDE-L1)
01446         MOVE AL-UANON             TO DEVCDEA
01447      ELSE
01448         GO TO 7040-CHECK-AH-DEVIATION.
01449
01450      MOVE LOW-VALUES             TO RATE-KEY
01451
01452      MOVE PI-COMPANY-CD          TO RATE-COMP-CD
01453      MOVE PI-ACCT-STATE          TO RATE-STATE
01454      MOVE ZEROS                  TO RATE-CLASS
01455      MOVE DEVCDEI                TO RATE-DEV
01456
01457      
      * EXEC CICS HANDLE CONDITION
01458 *        NOTFND   (7039-LIFE-DEV-ERROR)
01459 *    END-EXEC.
      *    MOVE '"$I                   ! & #00005277' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303035323737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01460
01461      
      * EXEC CICS READ
01462 *        GTEQ
01463 *        DATASET   (ERRATE-FILE)
01464 *        SET       (ADDRESS OF RATE-RECORD)
01465 *        RIDFLD    (RATE-KEY)
01466 *    END-EXEC.
      *    MOVE '&"S        G          (   #00005281' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERRATE-FILE, 
                 DFHEIV20, 
                 DFHEIV99, 
                 RATE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF RATE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01467
01468      IF RT-COMPANY-CD = RATE-COMP-CD AND
01469         RT-ST-CODE    = RATE-STATE   AND
01470         RT-ST-CLASS   = RATE-CLASS   AND
01471         RT-ST-DEV     = RATE-DEV
01472            GO TO 7040-CHECK-AH-DEVIATION.
01473
01474  7039-LIFE-DEV-ERROR.
01475
01476      MOVE -1                     TO DEVCDEL
01477      MOVE AL-UABON               TO DEVCDEA
01478      MOVE ER-2154                TO EMI-ERROR
01479      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01480
01481  7040-CHECK-AH-DEVIATION.
01482
01483      IF (DEVCDEL GREATER THAN +0) AND
01484         (TYPEI EQUAL PI-AH-OVERRIDE-L1)
01485         MOVE AL-UANON             TO DEVCDEA
01486      ELSE
01487         GO TO 7099-EXIT.
01488
01489      MOVE LOW-VALUES             TO RATE-KEY
01490
01491      MOVE PI-COMPANY-CD          TO RATE-COMP-CD
01492      MOVE PI-ACCT-STATE          TO RATE-STATE
01493      MOVE ZEROS                  TO RATE-CLASS
01494      MOVE DEVCDEI                TO RATE-DEV
01495
01496      
      * EXEC CICS HANDLE CONDITION
01497 *        NOTFND   (7049-AH-DEV-ERROR)
01498 *    END-EXEC.
      *    MOVE '"$I                   ! '' #00005316' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303035333136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01499
01500      
      * EXEC CICS READ
01501 *        GTEQ
01502 *        DATASET   (ERRATE-FILE)
01503 *        SET       (ADDRESS OF RATE-RECORD)
01504 *        RIDFLD    (RATE-KEY)
01505 *    END-EXEC.
      *    MOVE '&"S        G          (   #00005320' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERRATE-FILE, 
                 DFHEIV20, 
                 DFHEIV99, 
                 RATE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF RATE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01506
01507      IF RT-COMPANY-CD = RATE-COMP-CD AND
01508         RT-ST-CODE    = RATE-STATE   AND
01509         RT-ST-CLASS   = RATE-CLASS   AND
01510         RT-ST-DEV     = RATE-DEV
01511            GO TO 7099-EXIT.
01512
01513  7049-AH-DEV-ERROR.
01514
01515      MOVE -1                     TO DEVCDEL
01516      MOVE AL-UABON               TO DEVCDEA
01517      MOVE ER-2154                TO EMI-ERROR
01518      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01519
01520  7099-EXIT.
01521      EXIT.
01522      EJECT
01523
01524  7150-READ-ERPLAN.
01525
01526      
      * EXEC CICS HANDLE CONDITION
01527 *        NOTFND   (7120-NOTFND)
01528 *    END-EXEC.
      *    MOVE '"$I                   ! ( #00005346' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303035333436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01529
01530      
      * EXEC CICS READ
01531 *         DATASET  (ERPLAN-FILE)
01532 *         SET      (ADDRESS OF PLAN-MASTER)
01533 *         RIDFLD   (PI-PLAN-KEY)
01534 *    END-EXEC.
      *    MOVE '&"S        E          (   #00005350' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPLAN-FILE, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-PLAN-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PLAN-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01535
01536      MOVE PL-LAST-MAINT-USER     TO PI-UPDATE-BY
01537      MOVE PL-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.
01538
01539      IF PL-SALES-TAX NOT NUMERIC
01540          MOVE ZEROS              TO PL-SALES-TAX.
01541
01542      GO TO 7199-EXIT.
01543
01544  7120-NOTFND.
01545
01546      MOVE -1                     TO MAINTL
01547      MOVE ER-2938                TO EMI-ERROR.
01548 *PEM PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01549
01550  7199-EXIT.
01551      EXIT.
01552      EJECT
01553
01554  7200-DEEDIT.
01555
01556      
      * EXEC CICS BIF
01557 *         DEEDIT
01558 *         FIELD  (DEEDIT-FIELD)
01559 *         LENGTH (15)
01560 *    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005376' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01561
01562  7200-EXIT.
01563      EXIT.
01564      EJECT
01565  7300-READ-ERPLAN-UPDATE.
01566
01567      
      * EXEC CICS HANDLE CONDITION
01568 *        NOTFND   (7320-NOTFND)
01569 *    END-EXEC.
      *    MOVE '"$I                   ! ) #00005387' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303035333837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01570
01571      
      * EXEC CICS READ
01572 *         DATASET  (ERPLAN-FILE)
01573 *         SET      (ADDRESS OF PLAN-MASTER)
01574 *         RIDFLD   (PI-PLAN-KEY)
01575 *         UPDATE
01576 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005391' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPLAN-FILE, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-PLAN-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PLAN-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01577
01578      GO TO 7399-EXIT.
01579
01580  7320-NOTFND.
01581
01582      MOVE -1                     TO MAINTL
01583      MOVE ER-2938                TO EMI-ERROR.
01584      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01585      GO TO 8200-SEND-DATAONLY.
01586
01587  7399-EXIT.
01588      EXIT.
01589      EJECT
01590  7400-BUILD-KEY.
01591
01592      MOVE PI-COMPANY-CD          TO WS-PLAN-COMPANY-CD
01593
01594      IF CARRL GREATER THAN +0
01595         MOVE CARRI               TO WS-PLAN-CARRIER.
01596
01597      IF GROUPL GREATER THAN +0
01598         MOVE GROUPI              TO WS-PLAN-GROUP.
01599
01600      IF STATEL GREATER THAN +0
01601         MOVE STATEI              TO WS-PLAN-STATE.
01602
01603      IF ACCTL GREATER THAN +0
01604         MOVE ACCTI               TO WS-PLAN-ACCOUNT.
01605
01606      IF TYPEL GREATER THAN +0
01607         MOVE TYPEI               TO WS-PLAN-BEN-TYPE.
01608
01609      IF BENEL GREATER THAN +0
01610         MOVE BENEI               TO WS-PLAN-BEN.
01611
01612      IF REVISEL GREATER THAN +0
01613         MOVE REVISEI             TO WS-PLAN-REVISION.
01614
01615      IF (REVISEL GREATER THAN +0) AND
01616         (REVISEI EQUAL SPACES)
01617         MOVE ER-2948             TO EMI-ERROR
01618         MOVE -1                  TO REVISEL
01619         MOVE AL-UABON            TO REVISEA
01620         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01621         GO TO 8200-SEND-DATAONLY.
01622
01623  7499-EXIT.
01624      EXIT.
01625      EJECT
01626  7500-READ-ERACCT.
01627
01628      
      * EXEC CICS HANDLE CONDITION
01629 *        NOTFND   (7555-NOTFND)
01630 *    END-EXEC.
      *    MOVE '"$I                   ! * #00005448' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303035343438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01631
01632      
      * EXEC CICS READ
01633 *         DATASET  (ERACCT-FILE)
01634 *         SET      (ADDRESS OF ACCOUNT-MASTER)
01635 *         RIDFLD   (PI-ACCT-KEY)
01636 *    END-EXEC.
      *    MOVE '&"S        E          (   #00005452' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343532' TO DFHEIV0(25:11)
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
           
01637
01638      MOVE +0                    TO SUB2.
01639
01640  7520-CHECK-IF-EQUAL.
01641
01642      ADD +1                     TO SUB2
01643
01644      IF SUB2 GREATER THAN +20
01645         IF PI-COMPANY-ID EQUAL 'DMD'
01646             MOVE +0                 TO SUB2
01647             GO TO 7525-CHECK-EXTRA
01648         ELSE
01649             GO TO 7555-NOTFND.
01650
01651      IF PL-BENEFIT-TYPE EQUAL AM-BENEFIT-TYPE (SUB2) AND
01652         PL-BENEFIT-CODE EQUAL AM-BENEFIT-CODE (SUB2) AND
01653         PL-REVISION-NO  EQUAL AM-BENEFIT-REVISION  (SUB2)
01654         GO TO 7699-EXIT
01655      ELSE
01656         GO TO 7520-CHECK-IF-EQUAL.
01657
01658  7525-CHECK-EXTRA.
01659
01660      ADD +1                     TO SUB2
01661
01662      IF SUB2 GREATER THAN +30
01663         GO TO 7555-NOTFND.
01664
01665      IF PL-BENEFIT-TYPE EQUAL AM-BENEFIT-DMD-TYPE (SUB2) AND
01666         PL-BENEFIT-CODE EQUAL AM-BENEFIT-DMD-CODE (SUB2) AND
01667         PL-REVISION-NO  EQUAL AM-BENEFIT-DMD-REVISION  (SUB2)
01668         GO TO 7699-EXIT
01669      ELSE
01670         GO TO 7525-CHECK-EXTRA.
01671
01672  7555-NOTFND.
01673
01674      MOVE -1                     TO MAINTL
01675      MOVE ER-2939                TO EMI-ERROR
01676      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01677
01678  7699-EXIT.
01679      EXIT.
01680      EJECT
01681
01682  7800-COMPANY-REC-READ.
01683
01684      MOVE SPACES                 TO ELCNTL-KEY
01685      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID
01686      MOVE '1'                    TO CNTL-REC-TYPE
01687      MOVE +0                     TO CNTL-SEQ-NO
01688
01689      
      * EXEC CICS HANDLE CONDITION
01690 *        NOTFND   (7880-NO-COMP)
01691 *    END-EXEC.
      *    MOVE '"$I                   ! + #00005509' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303035353039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01692
01693      
      * EXEC CICS READ
01694 *        DATASET   (ELCNTL-FILE)
01695 *        SET       (ADDRESS OF CONTROL-FILE)
01696 *        RIDFLD    (ELCNTL-KEY)
01697 *    END-EXEC.
      *    MOVE '&"S        E          (   #00005513' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353133' TO DFHEIV0(25:11)
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
           
01698
01699      IF CF-ACCOUNT-MSTR-MAINT-DT = LOW-VALUES
01700          MOVE ER-2572            TO EMI-ERROR
01701          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01702
01703      GO TO 7899-EXIT.
01704
01705  7880-NO-COMP.
01706
01707      MOVE ER-0002                TO EMI-ERROR
01708      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01709
01710  7899-EXIT.
01711      EXIT.
01712      EJECT
01713  8000-UPDATE-MAINT-DATE.
01714
01715      MOVE SPACES                 TO ELCNTL-KEY
01716
01717      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID
01718      MOVE '1'                    TO CNTL-REC-TYPE
01719      MOVE +0                     TO CNTL-SEQ-NO
01720
01721      
      * EXEC CICS HANDLE CONDITION
01722 *        NOTFND   (8000-EXIT)
01723 *    END-EXEC.
      *    MOVE '"$I                   ! , #00005541' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303035353431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01724
01725      
      * EXEC CICS READ
01726 *        UPDATE
01727 *        DATASET   (ELCNTL-FILE)
01728 *        SET       (ADDRESS OF CONTROL-FILE)
01729 *        RIDFLD    (ELCNTL-KEY)
01730 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005545' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353435' TO DFHEIV0(25:11)
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
           
01731
01732      MOVE CONTROL-FILE           TO JP-RECORD-AREA
01733      MOVE 'B'                    TO JP-RECORD-TYPE
01734      COMPUTE WS-JOURNAL-FILE-LENGTH EQUAL
01735           ELCNTL-LENGTH + JOURNAL-LENGTH
01736      MOVE ELCNTL-FILE            TO FILE-ID
01737      PERFORM 8400-LOG-JOURNAL-RECORD
01738
01739      MOVE BIN-CURRENT-SAVE       TO CF-ACCOUNT-MSTR-MAINT-DT
01740
01741      MOVE CONTROL-FILE           TO JP-RECORD-AREA
01742      MOVE 'C'                    TO JP-RECORD-TYPE
01743      MOVE ELCNTL-FILE            TO FILE-ID
01744
01745      
      * EXEC CICS REWRITE
01746 *        DATASET   (ELCNTL-FILE)
01747 *        FROM      (CONTROL-FILE)
01748 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005565' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01749
01750      COMPUTE WS-JOURNAL-FILE-LENGTH EQUAL
01751           ELCNTL-LENGTH + JOURNAL-LENGTH
01752      PERFORM 8400-LOG-JOURNAL-RECORD.
01753
01754  8000-EXIT.
01755       EXIT.
01756      EJECT
01757
01758  8100-SEND-INITIAL-MAP.
01759
01760      MOVE SAVE-DATE              TO DATEO
01761      MOVE EIBTIME                TO TIME-IN
01762      MOVE TIME-OUT               TO TIMEO
101101     MOVE PI-COMPANY-ID          TO CMPNYIDO
101101     MOVE PI-PROCESSOR-ID        TO USERIDO
01763      MOVE -1                     TO MAINTL
01764      MOVE PI-PLAN-CARRIER        TO CARRO
01765      MOVE PI-PLAN-GROUP          TO GROUPO
01766      MOVE PI-PLAN-STATE          TO STATEO
01767      MOVE PI-PLAN-ACCOUNT        TO ACCTO
01768      MOVE PI-PLAN-BEN-TYPE       TO TYPEO
01769      MOVE PI-PLAN-BEN            TO BENEO
01770      MOVE PI-PLAN-REVISION       TO REVISEO
01771      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO
01772      MOVE AL-SABOF               TO ERRMSGA
01773
01774      
      * EXEC CICS SEND
01775 *        MAP      (MAP-NAME)
01776 *        MAPSET   (MAPSET-NAME)
01777 *        FROM     (EL6507AO)
01778 *        ERASE
01779 *        CURSOR
01780 *    END-EXEC.
           MOVE LENGTH OF
            EL6507AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00005596' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6507AO, 
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
           
01781
01782      GO TO 9100-RETURN-TRAN.
01783
01784  8200-SEND-DATAONLY.
01785
01786      MOVE SAVE-DATE              TO DATEO
01787      MOVE EIBTIME                TO TIME-IN
01788      MOVE TIME-OUT               TO TIMEO
101101     MOVE PI-COMPANY-ID          TO CMPNYIDO
101101     MOVE PI-PROCESSOR-ID        TO USERIDO
01789      MOVE PI-PLAN-CARRIER        TO CARRO
01790      MOVE PI-PLAN-GROUP          TO GROUPO
01791      MOVE PI-PLAN-STATE          TO STATEO
01792      MOVE PI-PLAN-ACCOUNT        TO ACCTO
01793      MOVE PI-PLAN-BEN-TYPE       TO TYPEO
01794      MOVE PI-PLAN-BEN            TO BENEO
01795      MOVE PI-PLAN-REVISION       TO REVISEO
01796      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO
01797      MOVE AL-SABOF               TO ERRMSGA
01798
01799      
      * EXEC CICS SEND
01800 *        MAP      (MAP-NAME)
01801 *        MAPSET   (MAPSET-NAME)
01802 *        FROM     (EL6507AO)
01803 *        DATAONLY
01804 *        CURSOR
01805 *    END-EXEC.
           MOVE LENGTH OF
            EL6507AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00005623' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6507AO, 
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
           
01806
01807      GO TO 9100-RETURN-TRAN.
01808
01809  8300-SEND-TEXT.
01810
01811      
      * EXEC CICS SEND TEXT
01812 *        FROM     (LOGOFF-TEXT)
01813 *        LENGTH   (LOGOFF-LENGTH)
01814 *        ERASE
01815 *        FREEKB
01816 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00005635' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363335' TO DFHEIV0(25:11)
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
           
01817
01818      
      * EXEC CICS RETURN
01819 *    END-EXEC.
      *    MOVE '.(                    ''   #00005642' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01820
01821  8400-LOG-JOURNAL-RECORD.
01822
01823      MOVE PI-PROCESSOR-ID        TO JP-USER-ID
01824      MOVE FILE-ID                TO JP-FILE-ID
01825      MOVE THIS-PGM               TO JP-PROGRAM-ID.
01826
pemuni*    IF PI-JOURNAL-FILE-ID NOT = ZERO
pemuni*        EXEC CICS JOURNAL
pemuni*            JFILEID     (PI-JOURNAL-FILE-ID)
pemuni*            JTYPEID     ('ER')
pemuni*            FROM        (JOURNAL-RECORD)
pemuni*            LENGTH      (WS-JOURNAL-FILE-LENGTH)
pemuni*        END-EXEC.
01834
01835  8600-DEEDIT.
01836
01837      
      * EXEC CICS BIF DEEDIT
01838 *         FIELD(DEEDIT-FIELD)
01839 *         LENGTH(15)
01840 *    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005661' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01841
01842  8800-UNAUTHORIZED-ACCESS.
01843
01844      MOVE UNACCESS-MSG           TO LOGOFF-MSG
01845      GO TO 8300-SEND-TEXT.
01846
01847  8810-PF23.
01848
01849      MOVE EIBAID                 TO PI-ENTRY-CD-1
01850      MOVE XCTL-005               TO PGM-NAME
01851      GO TO 9300-XCTL.
01852
01853  9000-RETURN-CICS.
01854
01855      
      * EXEC CICS RETURN
01856 *    END-EXEC.
      *    MOVE '.(                    ''   #00005679' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01857
01858  9100-RETURN-TRAN.
01859
01860      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO
01861      MOVE SCREEN-NUMBER          TO PI-CURRENT-SCREEN-NO
01862
01863      
      * EXEC CICS RETURN
01864 *        TRANSID    (TRANS-ID)
01865 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
01866 *        LENGTH     (WS-COMM-LENGTH)
01867 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00005687' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01868
01869  9200-RETURN-MAIN-MENU.
01870
01871      MOVE XCTL-626               TO PGM-NAME
01872      GO TO 9300-XCTL.
01873
01874  9300-XCTL.
01875
01876      
      * EXEC CICS XCTL
01877 *        PROGRAM    (PGM-NAME)
01878 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
01879 *        LENGTH     (WS-COMM-LENGTH)
01880 *    END-EXEC.
      *    MOVE '.$C                   %   #00005700' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01881
01882  9400-CLEAR.
01883
01884      MOVE XCTL-6505              TO PGM-NAME
01885      GO TO 9300-XCTL.
01886
01887  9500-PF12.
01888
01889      MOVE XCTL-010               TO PGM-NAME
01890      GO TO 9300-XCTL.
01891
01892  9600-PGMID-ERROR.
01893
01894      
      * EXEC CICS HANDLE CONDITION
01895 *        PGMIDERR    (8300-SEND-TEXT)
01896 *    END-EXEC.
      *    MOVE '"$L                   ! - #00005718' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303035373138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01897
01898      MOVE PGM-NAME               TO PI-CALLING-PROGRAM
01899      MOVE ' '                    TO PI-ENTRY-CD-1
01900      MOVE XCTL-005               TO PGM-NAME
01901      MOVE PGM-NAME               TO LOGOFF-PGM
01902      MOVE PGMIDERR-MSG           TO LOGOFF-FILL
01903      GO TO 9300-XCTL.
01904
01905  9700-LINK-DATE-CONVERT.
01906
01907      
      * EXEC CICS LINK
01908 *        PROGRAM    ('ELDATCV')
01909 *        COMMAREA   (DATE-CONVERSION-DATA)
01910 *        LENGTH     (DC-COMM-LENGTH)
01911 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00005731' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01912
01913  9700-EXIT.
01914      EXIT.
01915
01916  9900-ERROR-FORMAT.
01917
01918      IF NOT EMI-ERRORS-COMPLETE
01919          MOVE LINK-001           TO PGM-NAME
01920          
      * EXEC CICS LINK
01921 *            PROGRAM    (PGM-NAME)
01922 *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
01923 *            LENGTH     (EMI-COMM-LENGTH)
01924 *        END-EXEC.
      *    MOVE '."C                   (   #00005744' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01925
01926  9900-EXIT.
01927      EXIT.
01928
01929  9990-ABEND.
01930
01931      MOVE LINK-004               TO PGM-NAME
01932      MOVE DFHEIBLK               TO EMI-LINE1
01933      
      * EXEC CICS LINK
01934 *        PROGRAM   (PGM-NAME)
01935 *        COMMAREA  (EMI-LINE1)
01936 *        LENGTH    (72)
01937 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00005757' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01938
01939      MOVE -1                     TO MAINTL
01940      GO TO 8200-SEND-DATAONLY
01941      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6507' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
01942
01943  9995-SECURITY-VIOLATION.
01944 *           COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00005785' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373835' TO DFHEIV0(25:11)
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
01945  9995-EXIT.
01946       EXIT.


       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6507' TO DFHEIV1
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
               GO TO 0420-END-FILE,
                     0420-END-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 0520-END-FILE,
                     0520-END-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 6300-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 7039-LIFE-DEV-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 7049-AH-DEV-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 7120-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 7320-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 7555-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 7880-NO-COMP
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 8000-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6507' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
