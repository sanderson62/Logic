00001  ID DIVISION.
00002
00003  PROGRAM-ID.                 EL604.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 16:59:34.
00007 *                            VMOD=2.015.
00008 *
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
00023
00024 *REMARKS.    TRANSACTION - EXA9 - REPORT CUSTOMIZATION.
00025
121307******************************************************************
121307*                   C H A N G E   L O G
121307*
121307* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
121307*-----------------------------------------------------------------
121307*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
121307* EFFECTIVE    NUMBER
121307*-----------------------------------------------------------------
121307* 121307  CR2007090700001  PEMA  ADD ACCT STATUS OF "C"
121307******************************************************************
00026  ENVIRONMENT DIVISION.
00027
00028      EJECT
00029  DATA DIVISION.
00030  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00031  77  FILLER  PIC X(32)  VALUE '********************************'.
00032  77  FILLER  PIC X(32)  VALUE '*    EL604  WORKING STORAGE    *'.
00033  77  FILLER  PIC X(32)  VALUE '************ V/M 2.015 *********'.
00035 *                                COPY ELCSCTM.
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
00037 *                                COPY ELCSCRTY.
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
092308*                                COPY MPCSCRT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            MPCSCRT                             *
00004 *                            VMOD=1.001                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = C.I.C.S. COMMON SECURITY DATA AREA        *
00007 *        ACQUIRED BY SIGN-ON PROGRAM EL125.                      *
00008 *                                      (MP MORTGAGE PROTECTION)  *
00009 *                                                                *
00010 ******************************************************************
00011 *
00012  01  SECURITY-CONTROL-E.
00013      12  SC-COMM-LENGTH-E             PIC S9(04) VALUE +144 COMP.
00014      12  FILLER                       PIC  X(02) VALUE 'SC'.
00015      12  SC-QUID-KEY.
00016          16  SC-QUID-TERMINAL         PIC  X(04).
00017          16  SC-QUID-SYSTEM           PIC  X(04).
00018      12  SC-ITEM                      PIC S9(04) VALUE +1   COMP.
00019      12  SC-SECURITY-ACCESS-CODE      PIC  X(01).
00020      12  SC-PRODUCER-AUTHORIZED-SW    PIC  X(01).
00021          88 SC-PRODUCER-AUTHORIZED               VALUE ' '.
00022          88 SC-PRODUCER-NOT-AUTHORIZED           VALUE 'N'.
00023      12  SC-MP-CODES.
00024          16  SC-MP-AUTHORIZATION OCCURS 44 TIMES.
00025              20  SC-MP-DISPLAY        PIC  X(01).
00026              20  SC-MP-UPDATE         PIC  X(01).
00027      12  FILLER                       PIC  X(40).
00040  01  WS-DATE-AREA.
00041      05  SAVE-DATE                   PIC X(8)    VALUE SPACES.
00042      05  SAVE-BIN-DATE               PIC XX      VALUE SPACES.
00043
00044  01  STANDARD-AREAS.
092308     12  W-APPL-SCRTY-NDX    PIC S9(04) COMP   VALUE +30.
092308     12  SC-ITEM-CL-CR       PIC S9(4)         VALUE +1   COMP.
00046      12  WS-TEST-CARRIER             PIC X       VALUE SPACES.
00047      12  WS-TEST-STATE               PIC XX      VALUE SPACES.
00048      12  WS-TEST-BUSTYP              PIC 99      VALUE ZEROS.
00049      12  WS-TEST-BEN                 PIC XX      VALUE SPACES.
00050          88  INVALID-BENEFIT-CODE       VALUE '00'
00051                                               '90' THRU '99'.
00052      12  WS-TEST-SEQ                 PIC X       VALUE SPACES.
00053          88  VALID-ACCT-SEQ           VALUES  '1' '2' '3'
00054                                               '4' '5' '6'.
00055      12  WS-TEST-LEN                 PIC S9(4)   VALUE +0.
00056      12  WS-TEST-ATTRB               PIC X       VALUE SPACES.
00057      12  WS-SUB                      PIC S9(4)   VALUE +0 COMP.
00058      12  WS-BROWSE-SW                PIC X       VALUE SPACES.
00059      12  WS-SEQ-AREA.
00060          16  WS-SEQ-NO OCCURS 6 TIMES PIC X.
00061      12  WS-LO-EFF                   PIC XX      VALUE LOW-VALUES.
00062      12  WS-HI-EFF                   PIC XX      VALUE LOW-VALUES.
00063      12  WS-LO-ENT                   PIC XX      VALUE LOW-VALUES.
00064      12  WS-HI-ENT                   PIC XX      VALUE LOW-VALUES.
00065      12  WS-LO-LOSS                  PIC S9(3)V99 VALUE +0.
00066      12  WS-HI-LOSS                  PIC S9(3)V99 VALUE +0.
00067      12  GETMAIN-SPACE               PIC X       VALUE SPACE.
00068      12  MAP-NAME                    PIC X(8)    VALUE 'EL604A'.
00069      12  MAPSET-NAME                 PIC X(8)    VALUE 'EL604S'.
00070      12  SCREEN-NUMBER               PIC X(4)    VALUE '604A'.
00071      12  TRANS-ID                    PIC X(4)    VALUE 'EXA9'.
00072      12  THIS-PGM                    PIC X(8)    VALUE 'EL604'.
00073      12  PGM-NAME                    PIC X(8).
00074      12  TIME-IN                     PIC S9(7).
00075      12  TIME-OUT-R  REDEFINES TIME-IN.
00076          16  FILLER                  PIC X.
00077          16  TIME-OUT                PIC 99V99.
00078          16  FILLER                  PIC XX.
00079      12  XCTL-005                    PIC X(8)    VALUE 'EL005'.
00080      12  XCTL-010                    PIC X(8)    VALUE 'EL010'.
00081      12  XCTL-EL126                  PIC X(8)    VALUE 'EL126'.
00082      12  XCTL-EL6041                 PIC X(8)    VALUE 'EL6041'.
00083      12  XCTL-EL626                  PIC X(8)    VALUE 'EL626'.
00084      12  XCTL-EM626                  PIC X(8)    VALUE 'EM626'.
00085      12  XCTL-GL800                  PIC X(8)    VALUE 'GL800'.
00086      12  LINK-001                    PIC X(8)    VALUE 'EL001'.
00087      12  LINK-004                    PIC X(8)    VALUE 'EL004'.
00088      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'.
00089      12  ELCNTL-FILE-ID              PIC X(8)    VALUE 'ELCNTL'.
00090      12  WS-JOURNAL-RECORD-LENGTH PIC S9(4)   COMP VALUE +750.
00091
00092      12  DEEDIT-FIELD                PIC X(15).
00093      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD   PIC S9(15).
00094      12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD   PIC S9(13)V99.
00095
00096      12  RETURN-FROM                 PIC X(8).
00097      12  WS-REPORT-FOUND-SW          PIC X       VALUE 'N'.
00098          88  REPORT-WAS-FOUND                    VALUE 'Y'.
00099          88  REPORT-WAS-NOT-FOUND                VALUE 'N'.
00100
00101      EJECT
00102  01   ERROR-MESSAGES.
00103      12  ER-0000                     PIC  X(4)   VALUE '0000'.
00104      12  ER-0004                     PIC  X(4)   VALUE '0004'.
00105      12  ER-0021                     PIC  X(4)   VALUE '0021'.
00106      12  ER-0023                     PIC  X(4)   VALUE '0023'.
00107      12  ER-0028                     PIC  X(4)   VALUE '0028'.
00108      12  ER-0029                     PIC  X(4)   VALUE '0029'.
00109      12  ER-0050                     PIC  X(4)   VALUE '0050'.
00110      12  ER-0068                     PIC  X(4)   VALUE '0068'.
00111      12  ER-0070                     PIC  X(4)   VALUE '0070'.
00112      12  ER-0132                     PIC  X(4)   VALUE '0132'.
00113      12  ER-0138                     PIC  X(4)   VALUE '0138'.
00114      12  ER-0139                     PIC  X(4)   VALUE '0139'.
00115      12  ER-0142                     PIC  X(4)   VALUE '0142'.
00116      12  ER-2178                     PIC  X(4)   VALUE '2178'.
00117      12  ER-2237                     PIC  X(4)   VALUE '2237'.
00118      12  ER-2238                     PIC  X(4)   VALUE '2238'.
00119      12  ER-2845                     PIC  X(4)   VALUE '2845'.
00120      12  ER-2848                     PIC  X(4)   VALUE '2848'.
00121      12  ER-7008                     PIC  X(4)   VALUE '7008'.
00122      12  ER-7123                     PIC  X(4)   VALUE '7123'.
00123      12  ER-7125                     PIC  X(4)   VALUE '7125'.
00124      12  ER-7680                     PIC  X(4)   VALUE '7680'.
00125      12  ER-7681                     PIC  X(4)   VALUE '7681'.
00126      12  ER-7682                     PIC  X(4)   VALUE '7682'.
00127      12  ER-7683                     PIC  X(4)   VALUE '7683'.
00128      12  ER-7684                     PIC  X(4)   VALUE '7684'.
00129      12  ER-7685                     PIC  X(4)   VALUE '7685'.
00130      12  ER-7745                     PIC  X(4)   VALUE '7745'.
00131      12  ER-7746                     PIC  X(4)   VALUE '7746'.
00132      12  ER-9096                     PIC  X(4)   VALUE '9096'.
00133      12  ER-9097                     PIC  X(4)   VALUE '9097'.
00134
00135      EJECT
00136
00137  01  ACCESS-KEYS.
00138      12  ELCNTL-KEY.
00139          16  ELCNTL-COMPANY-ID          PIC  XXX.
00140          16  ELCNTL-RECORD-TYPE         PIC  X.
00141          16  FILLER                     PIC  X.
00142          16  ELCNTL-REPORT              PIC  999.
00143          16  ELCNTL-SEQ-NO              PIC  S9(4)   COMP.
00144
00145      12  ELCNTL-TEST-KEY.
00146          16  ELCNTL-TEST-COMP-ID        PIC  XXX.
00147          16  ELCNTL-TEST-REC-TYPE       PIC  X.
00148          16  ELCNTL-ACCESS-KEY.
00149              20  FILLER                 PIC  XXX.
00150              20  ELCNTL-TEST-CAR        PIC  X.
00151          16  ELCNTL-ACCESS-KEY1 REDEFINES ELCNTL-ACCESS-KEY.
00152              20  ELCNTL-TEST-STATE      PIC  XX.
00153              20  FILLER                 PIC  XX.
00154          16  ELCNTL-ACCESS-KEY2 REDEFINES ELCNTL-ACCESS-KEY.
00155              20  FILLER                 PIC  XX.
00156              20  ELCNTL-TEST-BEN        PIC  XX.
00157          16  ELCNTL-ACCESS-KEY3 REDEFINES ELCNTL-ACCESS-KEY.
00158              20  FILLER                 PIC  XX.
00159              20  ELCNTL-TEST-BUSTYP     PIC  99.
00160          16  ELCNTL-TEST-SEQ-NO         PIC  S9(4)   COMP.
00161
00162      12  ELCNTL-LENGTH           PIC S9(4) COMP VALUE +750.
00163
00164      EJECT
00165
00166 *                                COPY ELCDATE.
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
00167      EJECT
00168 *                                COPY ELCLOGOF.
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
00169      EJECT
00170 *                                COPY ELCATTR.
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
00171      EJECT
00172 *                                COPY ELCEMIB.
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
00070      12  FILLER                      PIC X(137)  VALUE SPACES.
00071      12  EMI-DATE-FIELD              PIC X(06)   VALUE SPACES.
00072      12  EMI-CLIENT-ID               PIC X(3)    VALUE SPACES.
00073      12  EMI-LIFE-OVERRIDE-L6        PIC X(6).
00074      12  EMI-AH-OVERRIDE-L6          PIC X(6).
00173      EJECT
00174 *                                COPY ELCINTF.
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
00131      12  FILLER                          PIC X(4).
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
00175
00176      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.
00177          16  PI-LAST-REPORT        PIC 9(3).
00178          16  FILLER                PIC X(637).
00179      EJECT
00180 *                            COPY ELCJPFX.
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
00181                              PIC X(750).
00182      EJECT
00183 *                            COPY ELCAID.
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
00184
00185  01  FILLER    REDEFINES DFHAID.
00186      12  FILLER              PIC X(8).
00187      12  PF-VALUES           PIC X       OCCURS 24 TIMES.
00188
00189      EJECT
00190 *                                COPY EL604S.
       01  EL604AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  RUNDTEL PIC S9(0004) COMP.
           05  RUNDTEF PIC  X(0001).
           05  FILLER REDEFINES RUNDTEF.
               10  RUNDTEA PIC  X(0001).
           05  RUNDTEI PIC  X(0008).
      *    -------------------------------
           05  RUNTIMEL PIC S9(0004) COMP.
           05  RUNTIMEF PIC  X(0001).
           05  FILLER REDEFINES RUNTIMEF.
               10  RUNTIMEA PIC  X(0001).
           05  RUNTIMEI PIC  X(0005).
      *    -------------------------------
           05  MAINTL PIC S9(0004) COMP.
           05  MAINTF PIC  X(0001).
           05  FILLER REDEFINES MAINTF.
               10  MAINTA PIC  X(0001).
           05  MAINTI PIC  X(0001).
      *    -------------------------------
           05  REPORTL PIC S9(0004) COMP.
           05  REPORTF PIC  X(0001).
           05  FILLER REDEFINES REPORTF.
               10  REPORTA PIC  X(0001).
           05  REPORTI PIC  X(0003).
      *    -------------------------------
           05  STATUSL PIC S9(0004) COMP.
           05  STATUSF PIC  X(0001).
           05  FILLER REDEFINES STATUSF.
               10  STATUSA PIC  X(0001).
           05  STATUSI PIC  X(0001).
      *    -------------------------------
           05  CPYRPTL PIC S9(0004) COMP.
           05  CPYRPTF PIC  X(0001).
           05  FILLER REDEFINES CPYRPTF.
               10  CPYRPTA PIC  X(0001).
           05  CPYRPTI PIC  X(0003).
      *    -------------------------------
           05  CARSEQL PIC S9(0004) COMP.
           05  CARSEQF PIC  X(0001).
           05  FILLER REDEFINES CARSEQF.
               10  CARSEQA PIC  X(0001).
           05  CARSEQI PIC  9.
      *    -------------------------------
           05  CAR1L PIC S9(0004) COMP.
           05  CAR1F PIC  X(0001).
           05  FILLER REDEFINES CAR1F.
               10  CAR1A PIC  X(0001).
           05  CAR1I PIC  X(0001).
      *    -------------------------------
           05  CAR2L PIC S9(0004) COMP.
           05  CAR2F PIC  X(0001).
           05  FILLER REDEFINES CAR2F.
               10  CAR2A PIC  X(0001).
           05  CAR2I PIC  X(0001).
      *    -------------------------------
           05  CAR3L PIC S9(0004) COMP.
           05  CAR3F PIC  X(0001).
           05  FILLER REDEFINES CAR3F.
               10  CAR3A PIC  X(0001).
           05  CAR3I PIC  X(0001).
      *    -------------------------------
           05  GRPSEQL PIC S9(0004) COMP.
           05  GRPSEQF PIC  X(0001).
           05  FILLER REDEFINES GRPSEQF.
               10  GRPSEQA PIC  X(0001).
           05  GRPSEQI PIC  9.
      *    -------------------------------
           05  GRP1L PIC S9(0004) COMP.
           05  GRP1F PIC  X(0001).
           05  FILLER REDEFINES GRP1F.
               10  GRP1A PIC  X(0001).
           05  GRP1I PIC  X(0006).
      *    -------------------------------
           05  GRP2L PIC S9(0004) COMP.
           05  GRP2F PIC  X(0001).
           05  FILLER REDEFINES GRP2F.
               10  GRP2A PIC  X(0001).
           05  GRP2I PIC  X(0006).
      *    -------------------------------
           05  GRP3L PIC S9(0004) COMP.
           05  GRP3F PIC  X(0001).
           05  FILLER REDEFINES GRP3F.
               10  GRP3A PIC  X(0001).
           05  GRP3I PIC  X(0006).
      *    -------------------------------
           05  STSEQL PIC S9(0004) COMP.
           05  STSEQF PIC  X(0001).
           05  FILLER REDEFINES STSEQF.
               10  STSEQA PIC  X(0001).
           05  STSEQI PIC  9.
      *    -------------------------------
           05  ST1L PIC S9(0004) COMP.
           05  ST1F PIC  X(0001).
           05  FILLER REDEFINES ST1F.
               10  ST1A PIC  X(0001).
           05  ST1I PIC  X(0002).
      *    -------------------------------
           05  ST2L PIC S9(0004) COMP.
           05  ST2F PIC  X(0001).
           05  FILLER REDEFINES ST2F.
               10  ST2A PIC  X(0001).
           05  ST2I PIC  X(0002).
      *    -------------------------------
           05  ST3L PIC S9(0004) COMP.
           05  ST3F PIC  X(0001).
           05  FILLER REDEFINES ST3F.
               10  ST3A PIC  X(0001).
           05  ST3I PIC  X(0002).
      *    -------------------------------
           05  BUSSEQL PIC S9(0004) COMP.
           05  BUSSEQF PIC  X(0001).
           05  FILLER REDEFINES BUSSEQF.
               10  BUSSEQA PIC  X(0001).
           05  BUSSEQI PIC  9.
      *    -------------------------------
           05  BUS1L PIC S9(0004) COMP.
           05  BUS1F PIC  X(0001).
           05  FILLER REDEFINES BUS1F.
               10  BUS1A PIC  X(0001).
           05  BUS1I PIC  X(0002).
      *    -------------------------------
           05  BUS2L PIC S9(0004) COMP.
           05  BUS2F PIC  X(0001).
           05  FILLER REDEFINES BUS2F.
               10  BUS2A PIC  X(0001).
           05  BUS2I PIC  X(0002).
      *    -------------------------------
           05  BUS3L PIC S9(0004) COMP.
           05  BUS3F PIC  X(0001).
           05  FILLER REDEFINES BUS3F.
               10  BUS3A PIC  X(0001).
           05  BUS3I PIC  X(0002).
      *    -------------------------------
           05  LBENTPL PIC S9(0004) COMP.
           05  LBENTPF PIC  X(0001).
           05  FILLER REDEFINES LBENTPF.
               10  LBENTPA PIC  X(0001).
           05  LBENTPI PIC  X(0002).
      *    -------------------------------
           05  LBENSEQL PIC S9(0004) COMP.
           05  LBENSEQF PIC  X(0001).
           05  FILLER REDEFINES LBENSEQF.
               10  LBENSEQA PIC  X(0001).
           05  LBENSEQI PIC  X(0001).
      *    -------------------------------
           05  LBEN1L PIC S9(0004) COMP.
           05  LBEN1F PIC  X(0001).
           05  FILLER REDEFINES LBEN1F.
               10  LBEN1A PIC  X(0001).
           05  LBEN1I PIC  X(0002).
      *    -------------------------------
           05  LBEN2L PIC S9(0004) COMP.
           05  LBEN2F PIC  X(0001).
           05  FILLER REDEFINES LBEN2F.
               10  LBEN2A PIC  X(0001).
           05  LBEN2I PIC  X(0002).
      *    -------------------------------
           05  LBEN3L PIC S9(0004) COMP.
           05  LBEN3F PIC  X(0001).
           05  FILLER REDEFINES LBEN3F.
               10  LBEN3A PIC  X(0001).
           05  LBEN3I PIC  X(0002).
      *    -------------------------------
           05  ABENTPL PIC S9(0004) COMP.
           05  ABENTPF PIC  X(0001).
           05  FILLER REDEFINES ABENTPF.
               10  ABENTPA PIC  X(0001).
           05  ABENTPI PIC  X(0002).
      *    -------------------------------
           05  ABENSEQL PIC S9(0004) COMP.
           05  ABENSEQF PIC  X(0001).
           05  FILLER REDEFINES ABENSEQF.
               10  ABENSEQA PIC  X(0001).
           05  ABENSEQI PIC  X(0001).
      *    -------------------------------
           05  ABEN1L PIC S9(0004) COMP.
           05  ABEN1F PIC  X(0001).
           05  FILLER REDEFINES ABEN1F.
               10  ABEN1A PIC  X(0001).
           05  ABEN1I PIC  X(0002).
      *    -------------------------------
           05  ABEN2L PIC S9(0004) COMP.
           05  ABEN2F PIC  X(0001).
           05  FILLER REDEFINES ABEN2F.
               10  ABEN2A PIC  X(0001).
           05  ABEN2I PIC  X(0002).
      *    -------------------------------
           05  ABEN3L PIC S9(0004) COMP.
           05  ABEN3F PIC  X(0001).
           05  FILLER REDEFINES ABEN3F.
               10  ABEN3A PIC  X(0001).
           05  ABEN3I PIC  X(0002).
      *    -------------------------------
           05  REINSEQL PIC S9(0004) COMP.
           05  REINSEQF PIC  X(0001).
           05  FILLER REDEFINES REINSEQF.
               10  REINSEQA PIC  X(0001).
           05  REINSEQI PIC  9.
      *    -------------------------------
           05  REIN1L PIC S9(0004) COMP.
           05  REIN1F PIC  X(0001).
           05  FILLER REDEFINES REIN1F.
               10  REIN1A PIC  X(0001).
           05  REIN1I PIC  X(0006).
      *    -------------------------------
           05  REIN2L PIC S9(0004) COMP.
           05  REIN2F PIC  X(0001).
           05  FILLER REDEFINES REIN2F.
               10  REIN2A PIC  X(0001).
           05  REIN2I PIC  X(0006).
      *    -------------------------------
           05  REIN3L PIC S9(0004) COMP.
           05  REIN3F PIC  X(0001).
           05  FILLER REDEFINES REIN3F.
               10  REIN3A PIC  X(0001).
           05  REIN3I PIC  X(0006).
      *    -------------------------------
           05  ACCTSEQL PIC S9(0004) COMP.
           05  ACCTSEQF PIC  X(0001).
           05  FILLER REDEFINES ACCTSEQF.
               10  ACCTSEQA PIC  X(0001).
           05  ACCTSEQI PIC  9.
      *    -------------------------------
           05  ACCT1L PIC S9(0004) COMP.
           05  ACCT1F PIC  X(0001).
           05  FILLER REDEFINES ACCT1F.
               10  ACCT1A PIC  X(0001).
           05  ACCT1I PIC  X(0010).
      *    -------------------------------
           05  ACCT2L PIC S9(0004) COMP.
           05  ACCT2F PIC  X(0001).
           05  FILLER REDEFINES ACCT2F.
               10  ACCT2A PIC  X(0001).
           05  ACCT2I PIC  X(0010).
      *    -------------------------------
           05  ACCT3L PIC S9(0004) COMP.
           05  ACCT3F PIC  X(0001).
           05  FILLER REDEFINES ACCT3F.
               10  ACCT3A PIC  X(0001).
           05  ACCT3I PIC  X(0010).
      *    -------------------------------
           05  AGNTSEQL PIC S9(0004) COMP.
           05  AGNTSEQF PIC  X(0001).
           05  FILLER REDEFINES AGNTSEQF.
               10  AGNTSEQA PIC  X(0001).
           05  AGNTSEQI PIC  9.
      *    -------------------------------
           05  AGNT1L PIC S9(0004) COMP.
           05  AGNT1F PIC  X(0001).
           05  FILLER REDEFINES AGNT1F.
               10  AGNT1A PIC  X(0001).
           05  AGNT1I PIC  X(0010).
      *    -------------------------------
           05  AGNT2L PIC S9(0004) COMP.
           05  AGNT2F PIC  X(0001).
           05  FILLER REDEFINES AGNT2F.
               10  AGNT2A PIC  X(0001).
           05  AGNT2I PIC  X(0010).
      *    -------------------------------
           05  AGNT3L PIC S9(0004) COMP.
           05  AGNT3F PIC  X(0001).
           05  FILLER REDEFINES AGNT3F.
               10  AGNT3A PIC  X(0001).
           05  AGNT3I PIC  X(0010).
      *    -------------------------------
           05  RPTCD1L PIC S9(0004) COMP.
           05  RPTCD1F PIC  X(0001).
           05  FILLER REDEFINES RPTCD1F.
               10  RPTCD1A PIC  X(0001).
           05  RPTCD1I PIC  X(0012).
      *    -------------------------------
           05  RPT1SEQL PIC S9(0004) COMP.
           05  RPT1SEQF PIC  X(0001).
           05  FILLER REDEFINES RPT1SEQF.
               10  RPT1SEQA PIC  X(0001).
           05  RPT1SEQI PIC  9.
      *    -------------------------------
           05  RPTCD11L PIC S9(0004) COMP.
           05  RPTCD11F PIC  X(0001).
           05  FILLER REDEFINES RPTCD11F.
               10  RPTCD11A PIC  X(0001).
           05  RPTCD11I PIC  X(0010).
      *    -------------------------------
           05  RPTCD12L PIC S9(0004) COMP.
           05  RPTCD12F PIC  X(0001).
           05  FILLER REDEFINES RPTCD12F.
               10  RPTCD12A PIC  X(0001).
           05  RPTCD12I PIC  X(0010).
      *    -------------------------------
           05  RPTCD13L PIC S9(0004) COMP.
           05  RPTCD13F PIC  X(0001).
           05  FILLER REDEFINES RPTCD13F.
               10  RPTCD13A PIC  X(0001).
           05  RPTCD13I PIC  X(0010).
      *    -------------------------------
           05  RPTCD2L PIC S9(0004) COMP.
           05  RPTCD2F PIC  X(0001).
           05  FILLER REDEFINES RPTCD2F.
               10  RPTCD2A PIC  X(0001).
           05  RPTCD2I PIC  X(0012).
      *    -------------------------------
           05  RPT2SEQL PIC S9(0004) COMP.
           05  RPT2SEQF PIC  X(0001).
           05  FILLER REDEFINES RPT2SEQF.
               10  RPT2SEQA PIC  X(0001).
           05  RPT2SEQI PIC  9.
      *    -------------------------------
           05  RPTCD21L PIC S9(0004) COMP.
           05  RPTCD21F PIC  X(0001).
           05  FILLER REDEFINES RPTCD21F.
               10  RPTCD21A PIC  X(0001).
           05  RPTCD21I PIC  X(0010).
      *    -------------------------------
           05  RPTCD22L PIC S9(0004) COMP.
           05  RPTCD22F PIC  X(0001).
           05  FILLER REDEFINES RPTCD22F.
               10  RPTCD22A PIC  X(0001).
           05  RPTCD22I PIC  X(0010).
      *    -------------------------------
           05  RPTCD23L PIC S9(0004) COMP.
           05  RPTCD23F PIC  X(0001).
           05  FILLER REDEFINES RPTCD23F.
               10  RPTCD23A PIC  X(0001).
           05  RPTCD23I PIC  X(0010).
      *    -------------------------------
           05  USR1SEQL PIC S9(0004) COMP.
           05  USR1SEQF PIC  X(0001).
           05  FILLER REDEFINES USR1SEQF.
               10  USR1SEQA PIC  X(0001).
           05  USR1SEQI PIC  9.
      *    -------------------------------
           05  USR11L PIC S9(0004) COMP.
           05  USR11F PIC  X(0001).
           05  FILLER REDEFINES USR11F.
               10  USR11A PIC  X(0001).
           05  USR11I PIC  X(0010).
      *    -------------------------------
           05  USR12L PIC S9(0004) COMP.
           05  USR12F PIC  X(0001).
           05  FILLER REDEFINES USR12F.
               10  USR12A PIC  X(0001).
           05  USR12I PIC  X(0010).
      *    -------------------------------
           05  USR13L PIC S9(0004) COMP.
           05  USR13F PIC  X(0001).
           05  FILLER REDEFINES USR13F.
               10  USR13A PIC  X(0001).
           05  USR13I PIC  X(0010).
      *    -------------------------------
           05  USR2SEQL PIC S9(0004) COMP.
           05  USR2SEQF PIC  X(0001).
           05  FILLER REDEFINES USR2SEQF.
               10  USR2SEQA PIC  X(0001).
           05  USR2SEQI PIC  9.
      *    -------------------------------
           05  USR21L PIC S9(0004) COMP.
           05  USR21F PIC  X(0001).
           05  FILLER REDEFINES USR21F.
               10  USR21A PIC  X(0001).
           05  USR21I PIC  X(0010).
      *    -------------------------------
           05  USR22L PIC S9(0004) COMP.
           05  USR22F PIC  X(0001).
           05  FILLER REDEFINES USR22F.
               10  USR22A PIC  X(0001).
           05  USR22I PIC  X(0010).
      *    -------------------------------
           05  USR23L PIC S9(0004) COMP.
           05  USR23F PIC  X(0001).
           05  FILLER REDEFINES USR23F.
               10  USR23A PIC  X(0001).
           05  USR23I PIC  X(0010).
      *    -------------------------------
           05  USR3SEQL PIC S9(0004) COMP.
           05  USR3SEQF PIC  X(0001).
           05  FILLER REDEFINES USR3SEQF.
               10  USR3SEQA PIC  X(0001).
           05  USR3SEQI PIC  9.
      *    -------------------------------
           05  USR31L PIC S9(0004) COMP.
           05  USR31F PIC  X(0001).
           05  FILLER REDEFINES USR31F.
               10  USR31A PIC  X(0001).
           05  USR31I PIC  X(0010).
      *    -------------------------------
           05  USR32L PIC S9(0004) COMP.
           05  USR32F PIC  X(0001).
           05  FILLER REDEFINES USR32F.
               10  USR32A PIC  X(0001).
           05  USR32I PIC  X(0010).
      *    -------------------------------
           05  USR33L PIC S9(0004) COMP.
           05  USR33F PIC  X(0001).
           05  FILLER REDEFINES USR33F.
               10  USR33A PIC  X(0001).
           05  USR33I PIC  X(0010).
      *    -------------------------------
           05  USR4SEQL PIC S9(0004) COMP.
           05  USR4SEQF PIC  X(0001).
           05  FILLER REDEFINES USR4SEQF.
               10  USR4SEQA PIC  X(0001).
           05  USR4SEQI PIC  9.
      *    -------------------------------
           05  USR41L PIC S9(0004) COMP.
           05  USR41F PIC  X(0001).
           05  FILLER REDEFINES USR41F.
               10  USR41A PIC  X(0001).
           05  USR41I PIC  X(0010).
      *    -------------------------------
           05  USR42L PIC S9(0004) COMP.
           05  USR42F PIC  X(0001).
           05  FILLER REDEFINES USR42F.
               10  USR42A PIC  X(0001).
           05  USR42I PIC  X(0010).
      *    -------------------------------
           05  USR43L PIC S9(0004) COMP.
           05  USR43F PIC  X(0001).
           05  FILLER REDEFINES USR43F.
               10  USR43A PIC  X(0001).
           05  USR43I PIC  X(0010).
      *    -------------------------------
           05  USR5SEQL PIC S9(0004) COMP.
           05  USR5SEQF PIC  X(0001).
           05  FILLER REDEFINES USR5SEQF.
               10  USR5SEQA PIC  X(0001).
           05  USR5SEQI PIC  9.
      *    -------------------------------
           05  USR51L PIC S9(0004) COMP.
           05  USR51F PIC  X(0001).
           05  FILLER REDEFINES USR51F.
               10  USR51A PIC  X(0001).
           05  USR51I PIC  X(0010).
      *    -------------------------------
           05  USR52L PIC S9(0004) COMP.
           05  USR52F PIC  X(0001).
           05  FILLER REDEFINES USR52F.
               10  USR52A PIC  X(0001).
           05  USR52I PIC  X(0010).
      *    -------------------------------
           05  USR53L PIC S9(0004) COMP.
           05  USR53F PIC  X(0001).
           05  FILLER REDEFINES USR53F.
               10  USR53A PIC  X(0001).
           05  USR53I PIC  X(0010).
      *    -------------------------------
           05  LOLOSSL PIC S9(0004) COMP.
           05  LOLOSSF PIC  X(0001).
           05  FILLER REDEFINES LOLOSSF.
               10  LOLOSSA PIC  X(0001).
           05  LOLOSSI PIC  X(0006).
      *    -------------------------------
           05  HILOSSL PIC S9(0004) COMP.
           05  HILOSSF PIC  X(0001).
           05  FILLER REDEFINES HILOSSF.
               10  HILOSSA PIC  X(0001).
           05  HILOSSI PIC  X(0006).
      *    -------------------------------
           05  LOENTDTL PIC S9(0004) COMP.
           05  LOENTDTF PIC  X(0001).
           05  FILLER REDEFINES LOENTDTF.
               10  LOENTDTA PIC  X(0001).
           05  LOENTDTI PIC  X(0008).
      *    -------------------------------
           05  HIENTDTL PIC S9(0004) COMP.
           05  HIENTDTF PIC  X(0001).
           05  FILLER REDEFINES HIENTDTF.
               10  HIENTDTA PIC  X(0001).
           05  HIENTDTI PIC  X(0008).
      *    -------------------------------
           05  LOEFFDTL PIC S9(0004) COMP.
           05  LOEFFDTF PIC  X(0001).
           05  FILLER REDEFINES LOEFFDTF.
               10  LOEFFDTA PIC  X(0001).
           05  LOEFFDTI PIC  X(0008).
      *    -------------------------------
           05  HIEFFDTL PIC S9(0004) COMP.
           05  HIEFFDTF PIC  X(0001).
           05  FILLER REDEFINES HIEFFDTF.
               10  HIEFFDTA PIC  X(0001).
           05  HIEFFDTI PIC  X(0008).
      *    -------------------------------
           05  EXPRPTL PIC S9(0004) COMP.
           05  EXPRPTF PIC  X(0001).
           05  FILLER REDEFINES EXPRPTF.
               10  EXPRPTA PIC  X(0001).
           05  EXPRPTI PIC  X(0001).
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0079).
      *    -------------------------------
           05  ERRMSG2L PIC S9(0004) COMP.
           05  ERRMSG2F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG2F.
               10  ERRMSG2A PIC  X(0001).
           05  ERRMSG2I PIC  X(0079).
      *    -------------------------------
           05  PFKEYL PIC S9(0004) COMP.
           05  PFKEYF PIC  X(0001).
           05  FILLER REDEFINES PFKEYF.
               10  PFKEYA PIC  X(0001).
           05  PFKEYI PIC  99.
       01  EL604AO REDEFINES EL604AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REPORTO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATUSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPYRPTO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARSEQO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAR1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAR2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAR3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRPSEQO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRP1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRP2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRP3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STSEQO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ST1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ST2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ST3O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BUSSEQO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BUS1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BUS2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BUS3O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LBENTPO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LBENSEQO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LBEN1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LBEN2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LBEN3O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABENTPO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABENSEQO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABEN1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABEN2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABEN3O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REINSEQO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REIN1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REIN2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REIN3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCTSEQO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT1O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT3O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGNTSEQO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGNT1O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGNT2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGNT3O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RPTCD1O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RPT1SEQO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RPTCD11O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RPTCD12O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RPTCD13O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RPTCD2O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RPT2SEQO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RPTCD21O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RPTCD22O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RPTCD23O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USR1SEQO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USR11O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USR12O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USR13O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USR2SEQO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USR21O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USR22O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USR23O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USR3SEQO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USR31O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USR32O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USR33O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USR4SEQO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USR41O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USR42O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USR43O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USR5SEQO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USR51O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USR52O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USR53O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOLOSSO PIC  ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HILOSSO PIC  ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOENTDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HIENTDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LOEFFDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HIEFFDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXPRPTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFKEYO PIC  ZZ.
      *    -------------------------------
00191      EJECT
00192
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
00194  01  DFHCOMMAREA             PIC X(1024).
00195
00196      EJECT
00197
00198 *01 PARMLIST .
00199 *    12  FILLER                      PIC S9(8)   COMP.
00200 *    12  ELCNTL-POINTER              PIC S9(8)   COMP.
00201
00202      EJECT
00203
00204 *                            COPY ELCCNTL.
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
031808
031808         16  FILLER                         PIC X(82).
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
00492
00493          16  CF-PROC-MAX-EXP-PMT            PIC S9(7)V99  COMP-3.
00494
00495          16  CF-LANGUAGE-TYPE                   PIC X.
00496              88  CF-LANG-IS-ENG                     VALUE 'E'.
00497              88  CF-LANG-IS-FR                      VALUE 'F'.
00498          16  FILLER                             PIC  X(240).
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
00607              20  FILLER                     PIC X.
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
011410         16  FILLER                         PIC X(187).
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
082603             20  FILLER                     PIC X(11).
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
092705         16  FILLER                         PIC X(448).
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
00205
00206      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CONTROL-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL604' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00208
00209      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00210      MOVE '5'                    TO DC-OPTION-CODE.
00211      PERFORM 9700-DATE-LINK.
00212      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00213      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00214
00215      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00216      MOVE 2                      TO EMI-NUMBER-OF-LINES.
00217
00218      IF EIBCALEN = 0
00219          GO TO 8800-UNAUTHORIZED-ACCESS.
00220
00221      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00222          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00223              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00224              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00225              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00226              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00227              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00228              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00229              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00230              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00231              PERFORM 9910-INITIALIZE-SECURITY THRU 9910-EXIT
00232          ELSE
00233              MOVE PI-CALLING-PROGRAM   TO RETURN-FROM
00234              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00235              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00236              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00237              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00238              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00239              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00240              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00241              MOVE SPACES               TO PI-SAVED-PROGRAM-6.
00242
00243      
      * EXEC CICS    HANDLE    CONDITION
00244 *         PGMIDERR          (9600-PGMID-ERROR)
00245 *         ERROR             (9990-ABEND)
00246 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00003346' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303033333436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00247
00248      IF  EIBTRNID NOT = TRANS-ID
00249          MOVE LOW-VALUES         TO EL604AI
00250          GO TO 8100-SEND-INITIAL-MAP.
00251
00252      IF  EIBAID = DFHCLEAR
00253              OR
00254          NOT DISPLAY-CAP
00255          GO TO 9400-CLEAR.
00256
00257      EJECT
00258
00259  0200-RECEIVE.
00260      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00261         MOVE ER-7008            TO EMI-ERROR
00262         PERFORM 9900-ERROR-FORMAT
00263         MOVE -1                 TO MAINTL
00264         GO TO 8200-SEND-DATAONLY.
00265
00266      
      * EXEC CICS RECEIVE
00267 *        MAP      (MAP-NAME)
00268 *        MAPSET   (MAPSET-NAME)
00269 *        INTO     (EL604AI)
00270 *    END-EXEC.
           MOVE LENGTH OF
            EL604AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00003369' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL604AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00271
00272      IF  PFKEYL = +0
00273          GO TO 0300-CHECK-PFKEYS.
00274
00275      IF  EIBAID NOT = DFHENTER
00276          MOVE ER-0004            TO EMI-ERROR
00277          GO TO 0320-INPUT-ERROR.
00278
00279      IF PFKEYI NOT NUMERIC
00280         MOVE ER-0029     TO EMI-ERROR
00281         GO TO 0320-INPUT-ERROR.
00282
00283      IF PFKEYI GREATER 01 AND LESS 25
00284         MOVE PF-VALUES (PFKEYI) TO EIBAID
00285      ELSE
00286         MOVE ER-0029        TO EMI-ERROR
00287         GO TO 0320-INPUT-ERROR.
00288
00289  0300-CHECK-PFKEYS.
00290      IF EIBAID = DFHPF23
00291          GO TO 8810-PF23.
00292
00293      IF EIBAID = DFHPF24
00294          GO TO 9200-RETURN-MAIN-MENU.
00295
00296      IF EIBAID = DFHPF12
00297          GO TO 9500-PF12.
00298
00299      IF MAINTL GREATER THAN +0 AND
00300               EIBAID NOT = DFHENTER
00301         MOVE -1             TO  MAINTL
00302         MOVE  ER-0050       TO  EMI-ERROR
00303         PERFORM 9900-ERROR-FORMAT
00304         GO TO 8200-SEND-DATAONLY.
00305
00306      IF  EIBAID = DFHPF1
00307          PERFORM 7000-BROWSE-FWRD-NEXT-ACCOUNT THRU 7090-EXIT
00308          GO TO 4000-SHOW-REPORT.
00309
00310      IF  EIBAID = DFHPF2
00311          PERFORM 7100-BROWSE-BWRD-NEXT-ACCOUNT THRU 7190-EXIT
00312          GO TO 4000-SHOW-REPORT.
00313
00314      IF EIBAID = DFHPF3
00315          GO TO 8900-PF03.
00316
00317      IF EIBAID = DFHENTER
00318          GO TO 0400-EDIT-INPUT-DATA.
00319
00320      MOVE ER-0029                TO EMI-ERROR.
00321
00322  0320-INPUT-ERROR.
00323      PERFORM 9900-ERROR-FORMAT.
00324      MOVE AL-UNBON               TO PFKEYA.
00325      IF PFKEYL = 0
00326          MOVE -1                 TO MAINTL
00327      ELSE
00328          MOVE -1                 TO PFKEYL.
00329
00330      GO TO 8200-SEND-DATAONLY.
00331
00332      EJECT
00333
00334  0400-EDIT-INPUT-DATA.
00335      IF MAINTI = 'S'
00336         GO TO 4000-SHOW-REPORT.
00337
00338      IF   NOT MODIFY-CAP
00339           MOVE 'UPDATE'       TO SM-READ
00340           PERFORM 9995-SECURITY-VIOLATION
00341           MOVE ER-0070        TO EMI-ERROR
00342           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00343           GO TO 8100-SEND-INITIAL-MAP.
00344
00345      IF MAINTI = 'A'
00346         GO TO 1000-ADD-REPORT.
00347
00348      IF MAINTI = 'C'
00349         GO TO 2000-CHANGE-REPORT.
00350
00351      IF MAINTI = 'D'
00352         GO TO 2500-DELETE-REPORT.
00353
00354      IF MAINTI = 'K'
00355         GO TO 3000-COPY-REPORT.
00356
00357      MOVE  ER-0023            TO EMI-ERROR
00358      MOVE -1                  TO MAINTL
00359      MOVE AL-UABON            TO MAINTA
00360      PERFORM 9900-ERROR-FORMAT
00361      GO TO 8200-SEND-DATAONLY.
00362
00363      EJECT
00364
00365  1000-ADD-REPORT.
00366      IF REPORTL GREATER +0 AND
00367         REPORTI NUMERIC
00368         NEXT SENTENCE
00369      ELSE
00370         MOVE ER-7680     TO EMI-ERROR
00371         MOVE -1          TO REPORTL
00372         MOVE AL-UNBON    TO REPORTA
00373         PERFORM 9900-ERROR-FORMAT
00374         GO TO 8200-SEND-DATAONLY.
00375
00376      MOVE SPACES            TO ELCNTL-KEY.
00377      MOVE PI-COMPANY-ID     TO ELCNTL-COMPANY-ID.
00378      MOVE REPORTI           TO ELCNTL-REPORT.
00379      MOVE 'C'               TO ELCNTL-RECORD-TYPE.
00380      MOVE +0                TO ELCNTL-SEQ-NO.
00381
00382      
      * EXEC CICS HANDLE CONDITION
00383 *         NOTFND     (1100-EDIT-REPORT-SCREEN)
00384 *    END-EXEC.
      *    MOVE '"$I                   ! # #00003485' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303033343835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00385
00386      
      * EXEC CICS READ
00387 *         DATASET    (ELCNTL-FILE-ID)
00388 *         RIDFLD     (ELCNTL-KEY)
00389 *         SET        (ADDRESS OF CONTROL-FILE)
00390 *    END-EXEC.
      *    MOVE '&"S        E          (   #00003489' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033343839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00391
00392      MOVE -1                     TO REPORTL.
00393      MOVE  ER-0132               TO  EMI-ERROR.
00394      PERFORM 9900-ERROR-FORMAT.
00395      GO TO 8200-SEND-DATAONLY.
00396
00397  1100-EDIT-REPORT-SCREEN.
00398      IF STATUSL GREATER +0
121307        IF STATUSI = 'A' OR 'I' OR 'B' OR 'C'
00400            NEXT SENTENCE
00401         ELSE
00402            MOVE ER-7681            TO EMI-ERROR
00403            MOVE -1                 TO STATUSL
00404            MOVE AL-UNBON           TO STATUSA
00405            PERFORM 9900-ERROR-FORMAT
00406      ELSE
00407         IF MAINTI = 'A'
00408            MOVE ER-7681            TO EMI-ERROR
00409            MOVE -1                 TO STATUSL
00410            MOVE AL-UNBON           TO STATUSA
00411            PERFORM 9900-ERROR-FORMAT.
00412
00413      IF CARSEQL GREATER THAN +0
00414         INSPECT CARSEQI CONVERTING SPACES TO ZEROS
00415         IF CARSEQI NUMERIC AND
00416            CARSEQI LESS THAN 7
00417            NEXT SENTENCE
00418         ELSE
00419            MOVE ER-7682             TO EMI-ERROR
00420            MOVE -1                  TO CARSEQL
00421            MOVE AL-UNBON            TO CARSEQA
00422            PERFORM 9900-ERROR-FORMAT.
00423
00424      IF CAR1L GREATER THAN +0 AND
00425         CAR1I NOT = SPACES
00426         MOVE CAR1I         TO WS-TEST-CARRIER
00427         MOVE CAR1L         TO WS-TEST-LEN
00428         MOVE CAR1A         TO WS-TEST-ATTRB
00429         PERFORM 5000-VERIFY-CARRIER THRU 5019-EXIT
00430         MOVE WS-TEST-LEN   TO CAR1L
00431         MOVE WS-TEST-ATTRB TO CAR1A.
00432
00433      IF CAR2L GREATER THAN +0 AND
00434         CAR2I NOT = SPACES
00435         MOVE CAR2I         TO WS-TEST-CARRIER
00436         MOVE CAR2L         TO WS-TEST-LEN
00437         MOVE CAR2A         TO WS-TEST-ATTRB
00438         PERFORM 5000-VERIFY-CARRIER THRU 5019-EXIT
00439         MOVE WS-TEST-LEN   TO CAR2L
00440         MOVE WS-TEST-ATTRB TO CAR2A.
00441
00442      IF CAR3L GREATER THAN +0 AND
00443         CAR3I NOT = SPACES
00444         MOVE CAR3I         TO WS-TEST-CARRIER
00445         MOVE CAR3L         TO WS-TEST-LEN
00446         MOVE CAR3A         TO WS-TEST-ATTRB
00447         PERFORM 5000-VERIFY-CARRIER THRU 5019-EXIT
00448         MOVE WS-TEST-LEN   TO CAR3L
00449         MOVE WS-TEST-ATTRB TO CAR3A.
00450
00451      IF GRPSEQL GREATER THAN +0
00452         INSPECT GRPSEQI CONVERTING SPACES TO ZEROS
00453         IF GRPSEQI NUMERIC AND
00454            GRPSEQI LESS THAN 7
00455            NEXT SENTENCE
00456         ELSE
00457            MOVE ER-7682             TO EMI-ERROR
00458            MOVE -1                  TO GRPSEQL
00459            MOVE AL-UNBON            TO GRPSEQA
00460            PERFORM 9900-ERROR-FORMAT.
00461
00462      IF STSEQL GREATER THAN +0
00463         INSPECT STSEQI CONVERTING SPACES TO ZEROS
00464         IF STSEQI NUMERIC AND
00465            STSEQI LESS THAN 7
00466            NEXT SENTENCE
00467         ELSE
00468            MOVE ER-7682             TO EMI-ERROR
00469            MOVE -1                  TO STSEQL
00470            MOVE AL-UNBON            TO STSEQA
00471            PERFORM 9900-ERROR-FORMAT.
00472
00473      IF ST1L GREATER THAN +0 AND
00474         ST1I NOT = SPACES AND ZEROS
00475         MOVE ST1I            TO WS-TEST-STATE
00476         MOVE ST1L            TO WS-TEST-LEN
00477         MOVE ST1A            TO WS-TEST-ATTRB
00478         PERFORM 5020-VERIFY-STATE THRU 5039-EXIT
00479         MOVE WS-TEST-LEN     TO ST1L
00480         MOVE WS-TEST-ATTRB   TO ST1A.
00481
00482      IF ST2L GREATER THAN +0 AND
00483         ST2I NOT = SPACES AND ZEROS
00484         MOVE ST2I            TO WS-TEST-STATE
00485         MOVE ST2L            TO WS-TEST-LEN
00486         MOVE ST2A            TO WS-TEST-ATTRB
00487         PERFORM 5020-VERIFY-STATE THRU 5039-EXIT
00488         MOVE WS-TEST-LEN     TO ST2L
00489         MOVE WS-TEST-ATTRB   TO ST2A.
00490
00491      IF ST3L GREATER THAN +0 AND
00492         ST3I NOT = SPACES AND ZEROS
00493         MOVE ST3I            TO WS-TEST-STATE
00494         MOVE ST3L            TO WS-TEST-LEN
00495         MOVE ST3A            TO WS-TEST-ATTRB
00496         PERFORM 5020-VERIFY-STATE THRU 5039-EXIT
00497         MOVE WS-TEST-LEN     TO ST3L
00498         MOVE WS-TEST-ATTRB   TO ST3A.
00499
00500      IF BUSSEQL GREATER THAN +0
00501         INSPECT BUSSEQI CONVERTING SPACES TO ZEROS
00502         IF BUSSEQI NUMERIC AND
00503            BUSSEQI LESS THAN 7
00504            NEXT SENTENCE
00505         ELSE
00506            MOVE ER-7682             TO EMI-ERROR
00507            MOVE -1                  TO BUSSEQL
00508            MOVE AL-UNBON            TO BUSSEQA
00509            PERFORM 9900-ERROR-FORMAT.
00510
00511      IF BUS1L GREATER THAN +0
00512         INSPECT BUS1I CONVERTING SPACES TO ZEROS
00513         IF BUS1I NUMERIC AND
00514            BUS1I NOT = '00'
00515            MOVE BUS1I         TO WS-TEST-BUSTYP
00516            MOVE BUS1L         TO WS-TEST-LEN
00517            MOVE BUS1A         TO WS-TEST-ATTRB
00518            PERFORM 5060-VERIFY-BUS-TYPE THRU 5079-EXIT
00519            MOVE WS-TEST-LEN   TO BUS1L
00520            MOVE WS-TEST-ATTRB TO BUS1A
00521         ELSE
00522            IF BUS1I NOT NUMERIC
00523               MOVE ER-2178      TO EMI-ERROR
00524               MOVE -1           TO BUS1L
00525               MOVE AL-UNBON     TO BUS1A
00526               PERFORM 9900-ERROR-FORMAT.
00527
00528      IF BUS2L GREATER THAN +0
00529         INSPECT BUS2I CONVERTING SPACES TO ZEROS
00530         IF BUS2I NUMERIC AND
00531                  BUS2I NOT = '00'
00532            MOVE BUS2I          TO WS-TEST-BUSTYP
00533            MOVE BUS2L          TO WS-TEST-LEN
00534            MOVE BUS2A          TO WS-TEST-ATTRB
00535            PERFORM 5060-VERIFY-BUS-TYPE THRU 5079-EXIT
00536            MOVE WS-TEST-LEN    TO BUS2L
00537            MOVE WS-TEST-ATTRB  TO BUS2A
00538         ELSE
00539            IF BUS2I NOT NUMERIC
00540               MOVE ER-2178      TO EMI-ERROR
00541               MOVE -1           TO BUS2L
00542               MOVE AL-UNBON     TO BUS2A
00543               PERFORM 9900-ERROR-FORMAT.
00544
00545      IF BUS3L GREATER THAN +0
00546         INSPECT BUS3I CONVERTING SPACES TO ZEROS
00547         IF BUS3I NUMERIC AND
00548            BUS3I NOT = '00'
00549            MOVE BUS3I         TO WS-TEST-BUSTYP
00550            MOVE BUS3L         TO WS-TEST-LEN
00551            MOVE BUS3A         TO WS-TEST-ATTRB
00552            PERFORM 5060-VERIFY-BUS-TYPE THRU 5079-EXIT
00553            MOVE WS-TEST-LEN   TO BUS3L
00554            MOVE WS-TEST-ATTRB TO BUS3A
00555         ELSE
00556            IF BUS3I NOT NUMERIC
00557               MOVE ER-2178      TO EMI-ERROR
00558               MOVE -1           TO BUS3L
00559               MOVE AL-UNBON     TO BUS3A
00560               PERFORM 9900-ERROR-FORMAT.
00561
00562      IF LBEN1L GREATER THAN +0
00563         MOVE LBEN1I           TO WS-TEST-BEN
00564         IF NOT INVALID-BENEFIT-CODE
00565            MOVE LBEN1L        TO WS-TEST-LEN
00566            MOVE LBEN1A        TO WS-TEST-ATTRB
00567            MOVE SPACES        TO ELCNTL-TEST-KEY
00568            MOVE '4'           TO ELCNTL-TEST-REC-TYPE
00569            PERFORM 5040-VERIFY-BEN THRU 5059-EXIT
00570            MOVE WS-TEST-LEN   TO LBEN1L
00571            MOVE WS-TEST-ATTRB TO LBEN1A
00572         ELSE
00573            MOVE ER-7125       TO EMI-ERROR
00574            MOVE -1            TO LBEN1L
00575            MOVE AL-UABON      TO LBEN1A
00576            PERFORM 9900-ERROR-FORMAT.
00577
00578      IF LBEN2L GREATER THAN +0
00579         MOVE LBEN2I           TO WS-TEST-BEN
00580         IF NOT INVALID-BENEFIT-CODE
00581            MOVE LBEN2L        TO WS-TEST-LEN
00582            MOVE LBEN2A        TO WS-TEST-ATTRB
00583            MOVE SPACES        TO ELCNTL-TEST-KEY
00584            MOVE '4'           TO ELCNTL-TEST-REC-TYPE
00585            PERFORM 5040-VERIFY-BEN THRU 5059-EXIT
00586            MOVE WS-TEST-LEN   TO LBEN2L
00587            MOVE WS-TEST-ATTRB TO LBEN2A
00588         ELSE
00589            MOVE ER-7125       TO EMI-ERROR
00590            MOVE -1            TO LBEN2L
00591            MOVE AL-UABON      TO LBEN2A
00592            PERFORM 9900-ERROR-FORMAT.
00593
00594      IF LBEN3L GREATER THAN +0
00595         MOVE LBEN3I           TO WS-TEST-BEN
00596         IF NOT INVALID-BENEFIT-CODE
00597            MOVE LBEN3L        TO WS-TEST-LEN
00598            MOVE LBEN3A        TO WS-TEST-ATTRB
00599            MOVE SPACES        TO ELCNTL-TEST-KEY
00600            MOVE '4'           TO ELCNTL-TEST-REC-TYPE
00601            PERFORM 5040-VERIFY-BEN THRU 5059-EXIT
00602            MOVE WS-TEST-LEN   TO LBEN3L
00603            MOVE WS-TEST-ATTRB TO LBEN3A
00604         ELSE
00605            MOVE ER-7125       TO EMI-ERROR
00606            MOVE -1            TO LBEN3L
00607            MOVE AL-UABON      TO LBEN3A
00608            PERFORM 9900-ERROR-FORMAT.
00609
00610      IF ABEN1L GREATER THAN +0
00611         MOVE ABEN1I           TO WS-TEST-BEN
00612         IF NOT INVALID-BENEFIT-CODE
00613            MOVE ABEN1L        TO WS-TEST-LEN
00614            MOVE ABEN1A        TO WS-TEST-ATTRB
00615            MOVE SPACES        TO ELCNTL-TEST-KEY
00616            MOVE '5'           TO ELCNTL-TEST-REC-TYPE
00617            PERFORM 5040-VERIFY-BEN THRU 5059-EXIT
00618            MOVE WS-TEST-LEN   TO ABEN1L
00619            MOVE WS-TEST-ATTRB TO ABEN1A
00620         ELSE
00621            MOVE ER-7125       TO EMI-ERROR
00622            MOVE -1            TO ABEN1L
00623            MOVE AL-UABON      TO ABEN1A
00624            PERFORM 9900-ERROR-FORMAT.
00625
00626      IF ABEN2L GREATER THAN +0
00627         MOVE ABEN2I           TO WS-TEST-BEN
00628         IF NOT INVALID-BENEFIT-CODE
00629            MOVE ABEN2L        TO WS-TEST-LEN
00630            MOVE ABEN2A        TO WS-TEST-ATTRB
00631            MOVE SPACES        TO ELCNTL-TEST-KEY
00632            MOVE '5'           TO ELCNTL-TEST-REC-TYPE
00633            PERFORM 5040-VERIFY-BEN THRU 5059-EXIT
00634            MOVE WS-TEST-LEN   TO ABEN2L
00635            MOVE WS-TEST-ATTRB TO ABEN2A
00636         ELSE
00637            MOVE ER-7125       TO EMI-ERROR
00638            MOVE -1            TO ABEN2L
00639            MOVE AL-UABON      TO ABEN2A
00640            PERFORM 9900-ERROR-FORMAT.
00641
00642      IF ABEN3L GREATER THAN +0
00643         MOVE ABEN3I           TO WS-TEST-BEN
00644         IF NOT INVALID-BENEFIT-CODE
00645            MOVE ABEN3L        TO WS-TEST-LEN
00646            MOVE ABEN3A        TO WS-TEST-ATTRB
00647            MOVE SPACES        TO ELCNTL-TEST-KEY
00648            MOVE '5'           TO ELCNTL-TEST-REC-TYPE
00649            PERFORM 5040-VERIFY-BEN THRU 5059-EXIT
00650            MOVE WS-TEST-LEN   TO ABEN3L
00651            MOVE WS-TEST-ATTRB TO ABEN3A
00652         ELSE
00653            MOVE ER-7125       TO EMI-ERROR
00654            MOVE -1            TO ABEN3L
00655            MOVE AL-UABON      TO ABEN3A
00656            PERFORM 9900-ERROR-FORMAT.
00657
00658      IF ACCTSEQL GREATER THAN +0
00659         INSPECT ACCTSEQI CONVERTING SPACES TO ZEROS
00660         IF ACCTSEQI NUMERIC AND
00661            ACCTSEQI LESS THAN 7
00662            NEXT SENTENCE
00663         ELSE
00664            MOVE ER-7682             TO EMI-ERROR
00665            MOVE -1                  TO ACCTSEQL
00666            MOVE AL-UNBON            TO ACCTSEQA
00667            PERFORM 9900-ERROR-FORMAT.
00668
00669      IF AGNTSEQL GREATER THAN +0
00670         INSPECT AGNTSEQI CONVERTING SPACES TO ZEROS
00671         IF AGNTSEQI NUMERIC AND
00672            AGNTSEQI LESS THAN 7
00673            NEXT SENTENCE
00674         ELSE
00675            MOVE ER-7682             TO EMI-ERROR
00676            MOVE -1                  TO AGNTSEQL
00677            MOVE AL-UNBON            TO AGNTSEQA
00678            PERFORM 9900-ERROR-FORMAT.
00679
00680      IF REINSEQL GREATER THAN +0
00681         INSPECT REINSEQI CONVERTING SPACES TO ZEROS
00682         IF REINSEQI NUMERIC AND
00683            REINSEQI LESS THAN 7
00684            NEXT SENTENCE
00685         ELSE
00686            MOVE ER-7682             TO EMI-ERROR
00687            MOVE -1                  TO REINSEQL
00688            MOVE AL-UNBON            TO REINSEQA
00689            PERFORM 9900-ERROR-FORMAT.
00690
00691      IF RPT1SEQL GREATER THAN +0
00692         INSPECT RPT1SEQI CONVERTING SPACES TO ZEROS
00693         IF RPT1SEQI NUMERIC AND
00694            RPT1SEQI LESS THAN 7
00695            NEXT SENTENCE
00696         ELSE
00697            MOVE ER-7682             TO EMI-ERROR
00698            MOVE -1                  TO RPT1SEQL
00699            MOVE AL-UNBON            TO RPT1SEQA
00700            PERFORM 9900-ERROR-FORMAT.
00701
00702      IF RPT2SEQL GREATER THAN +0
00703         INSPECT RPT2SEQI CONVERTING SPACES TO ZEROS
00704         IF RPT2SEQI NUMERIC AND
00705            RPT2SEQI LESS THAN 7
00706            NEXT SENTENCE
00707         ELSE
00708            MOVE ER-7682             TO EMI-ERROR
00709            MOVE -1                  TO RPT2SEQL
00710            MOVE AL-UNBON            TO RPT2SEQA
00711            PERFORM 9900-ERROR-FORMAT.
00712
00713      IF USR1SEQL GREATER THAN +0
00714         INSPECT USR1SEQI CONVERTING SPACES TO ZEROS
00715         IF USR1SEQI NUMERIC AND
00716            USR1SEQI LESS THAN 7
00717            NEXT SENTENCE
00718         ELSE
00719            MOVE ER-7682             TO EMI-ERROR
00720            MOVE -1                  TO USR1SEQL
00721            MOVE AL-UNBON            TO USR1SEQA
00722            PERFORM 9900-ERROR-FORMAT.
00723
00724      IF USR2SEQL GREATER THAN +0
00725         INSPECT USR2SEQI CONVERTING SPACES TO ZEROS
00726         IF USR2SEQI NUMERIC AND
00727            USR2SEQI LESS THAN 7
00728            NEXT SENTENCE
00729         ELSE
00730            MOVE ER-7682             TO EMI-ERROR
00731            MOVE -1                  TO USR2SEQL
00732            MOVE AL-UNBON            TO USR2SEQA
00733            PERFORM 9900-ERROR-FORMAT.
00734
00735      IF USR3SEQL GREATER THAN +0
00736         INSPECT USR3SEQI CONVERTING SPACES TO ZEROS
00737         IF USR3SEQI NUMERIC AND
00738            USR3SEQI LESS THAN 7
00739            NEXT SENTENCE
00740         ELSE
00741            MOVE ER-7682             TO EMI-ERROR
00742            MOVE -1                  TO USR3SEQL
00743            MOVE AL-UNBON            TO USR3SEQA
00744            PERFORM 9900-ERROR-FORMAT.
00745
00746      IF USR4SEQL GREATER THAN +0
00747         INSPECT USR4SEQI CONVERTING SPACES TO ZEROS
00748         IF USR4SEQI NUMERIC AND
00749            USR4SEQI LESS THAN 7
00750            NEXT SENTENCE
00751         ELSE
00752            MOVE ER-7682             TO EMI-ERROR
00753            MOVE -1                  TO USR4SEQL
00754            MOVE AL-UNBON            TO USR4SEQA
00755            PERFORM 9900-ERROR-FORMAT.
00756
00757      IF USR5SEQL GREATER THAN +0
00758         INSPECT USR5SEQI CONVERTING SPACES TO ZEROS
00759         IF USR5SEQI NUMERIC AND
00760            USR5SEQI LESS THAN 7
00761            NEXT SENTENCE
00762         ELSE
00763            MOVE ER-7682             TO EMI-ERROR
00764            MOVE -1                  TO USR5SEQL
00765            MOVE AL-UNBON            TO USR5SEQA
00766            PERFORM 9900-ERROR-FORMAT.
00767
00768      IF EXPRPTL GREATER THAN +0
00769         IF EXPRPTI EQUAL 'Y' OR 'N'
00770             NEXT SENTENCE
00771         ELSE
00772            MOVE ER-7745             TO EMI-ERROR
00773            MOVE -1                  TO EXPRPTL
00774            MOVE AL-UNBON            TO EXPRPTA
00775            PERFORM 9900-ERROR-FORMAT.
00776
00777      IF NOT EMI-NO-ERRORS
00778         GO TO 8200-SEND-DATAONLY.
00779
00780      MOVE SPACES TO WS-SEQ-AREA.
00781
00782      IF CARSEQL GREATER THAN +0 AND
00783         CARSEQI NOT = 0
00784         IF WS-SEQ-NO (CARSEQI) NOT = 'X'
00785            MOVE 'X'              TO WS-SEQ-NO (CARSEQI)
00786         ELSE
00787            MOVE ER-7682          TO EMI-ERROR
00788            MOVE -1               TO CARSEQL
00789            MOVE AL-UNBON         TO CARSEQA
00790            PERFORM 9900-ERROR-FORMAT.
00791
00792      IF GRPSEQL GREATER THAN +0 AND
00793         GRPSEQI NOT = 0
00794         IF WS-SEQ-NO (GRPSEQI) NOT = 'X'
00795            MOVE 'X'              TO WS-SEQ-NO (GRPSEQI)
00796         ELSE
00797            MOVE ER-7682          TO EMI-ERROR
00798            MOVE -1               TO GRPSEQL
00799            MOVE AL-UNBON         TO GRPSEQA
00800            PERFORM 9900-ERROR-FORMAT.
00801
00802      IF REINSEQL GREATER THAN +0 AND
00803         REINSEQI NOT = 0
00804         IF WS-SEQ-NO (REINSEQI) NOT = 'X'
00805            MOVE 'X'              TO WS-SEQ-NO (REINSEQI)
00806         ELSE
00807            MOVE ER-7682          TO EMI-ERROR
00808            MOVE -1               TO REINSEQL
00809            MOVE AL-UNBON         TO REINSEQA
00810            PERFORM 9900-ERROR-FORMAT.
00811
00812      IF STSEQL GREATER THAN +0 AND
00813         STSEQI NOT = 0
00814         IF WS-SEQ-NO (STSEQI) NOT = 'X'
00815            MOVE 'X'              TO WS-SEQ-NO (STSEQI)
00816         ELSE
00817            MOVE ER-7682          TO EMI-ERROR
00818            MOVE -1               TO STSEQL
00819            MOVE AL-UNBON         TO STSEQA
00820            PERFORM 9900-ERROR-FORMAT.
00821
00822      IF BUSSEQL GREATER THAN +0 AND
00823         BUSSEQI NOT = 0
00824         IF WS-SEQ-NO (BUSSEQI) NOT = 'X'
00825            MOVE 'X'              TO WS-SEQ-NO (BUSSEQI)
00826         ELSE
00827            MOVE ER-7682          TO EMI-ERROR
00828            MOVE -1               TO BUSSEQL
00829            MOVE AL-UNBON         TO BUSSEQA
00830            PERFORM 9900-ERROR-FORMAT.
00831
00832      IF ACCTSEQL GREATER THAN +0 AND
00833         ACCTSEQI NOT = 0
00834         IF WS-SEQ-NO (ACCTSEQI) NOT = 'X'
00835            MOVE 'X'              TO WS-SEQ-NO (ACCTSEQI)
00836         ELSE
00837            MOVE ER-7682          TO EMI-ERROR
00838            MOVE -1               TO ACCTSEQL
00839            MOVE AL-UNBON         TO ACCTSEQA
00840            PERFORM 9900-ERROR-FORMAT.
00841
00842      IF AGNTSEQL GREATER THAN +0 AND
00843         AGNTSEQI NOT = 0
00844         IF WS-SEQ-NO (AGNTSEQI) NOT = 'X'
00845            MOVE 'X'              TO WS-SEQ-NO (AGNTSEQI)
00846         ELSE
00847            MOVE ER-7682          TO EMI-ERROR
00848            MOVE -1               TO AGNTSEQL
00849            MOVE AL-UNBON         TO AGNTSEQA
00850            PERFORM 9900-ERROR-FORMAT.
00851
00852      IF RPT1SEQL GREATER THAN +0 AND
00853         RPT1SEQI NOT = 0
00854         IF WS-SEQ-NO (RPT1SEQI) NOT = 'X'
00855            MOVE 'X'              TO WS-SEQ-NO (RPT1SEQI)
00856         ELSE
00857            MOVE ER-7682          TO EMI-ERROR
00858            MOVE -1               TO RPT1SEQL
00859            MOVE AL-UNBON         TO RPT1SEQA
00860            PERFORM 9900-ERROR-FORMAT.
00861
00862      IF RPT2SEQL GREATER THAN +0 AND
00863         RPT2SEQI NOT = 0
00864         IF WS-SEQ-NO (RPT2SEQI) NOT = 'X'
00865            MOVE 'X'              TO WS-SEQ-NO (RPT2SEQI)
00866         ELSE
00867            MOVE ER-7682          TO EMI-ERROR
00868            MOVE -1               TO RPT2SEQL
00869            MOVE AL-UNBON         TO RPT2SEQA
00870            PERFORM 9900-ERROR-FORMAT.
00871
00872      IF USR1SEQL GREATER THAN +0 AND
00873         USR1SEQI NOT = 0
00874         IF WS-SEQ-NO (USR1SEQI) NOT = 'X'
00875            MOVE 'X'              TO WS-SEQ-NO (USR1SEQI)
00876         ELSE
00877            MOVE ER-7682          TO EMI-ERROR
00878            MOVE -1               TO USR1SEQL
00879            MOVE AL-UNBON         TO USR1SEQA
00880            PERFORM 9900-ERROR-FORMAT.
00881
00882      IF USR2SEQL GREATER THAN +0 AND
00883         USR2SEQI NOT = 0
00884         IF WS-SEQ-NO (USR2SEQI) NOT = 'X'
00885            MOVE 'X'              TO WS-SEQ-NO (USR2SEQI)
00886         ELSE
00887            MOVE ER-7682          TO EMI-ERROR
00888            MOVE -1               TO USR2SEQL
00889            MOVE AL-UNBON         TO USR2SEQA
00890            PERFORM 9900-ERROR-FORMAT.
00891
00892      IF USR3SEQL GREATER THAN +0 AND
00893         USR3SEQI NOT = 0
00894         IF WS-SEQ-NO (USR3SEQI) NOT = 'X'
00895            MOVE 'X'              TO WS-SEQ-NO (USR3SEQI)
00896         ELSE
00897            MOVE ER-7682          TO EMI-ERROR
00898            MOVE -1               TO USR3SEQL
00899            MOVE AL-UNBON         TO USR3SEQA
00900            PERFORM 9900-ERROR-FORMAT.
00901
00902      IF USR4SEQL GREATER THAN +0 AND
00903         USR4SEQI NOT = 0
00904         IF WS-SEQ-NO (USR4SEQI) NOT = 'X'
00905            MOVE 'X'              TO WS-SEQ-NO (USR4SEQI)
00906         ELSE
00907            MOVE ER-7682          TO EMI-ERROR
00908            MOVE -1               TO USR4SEQL
00909            MOVE AL-UNBON         TO USR4SEQA
00910            PERFORM 9900-ERROR-FORMAT.
00911
00912      IF USR5SEQL GREATER THAN +0 AND
00913         USR5SEQI NOT = 0
00914         IF WS-SEQ-NO (USR5SEQI) NOT = 'X'
00915            MOVE 'X'              TO WS-SEQ-NO (USR5SEQI)
00916         ELSE
00917            MOVE ER-7682          TO EMI-ERROR
00918            MOVE -1               TO USR5SEQL
00919            MOVE AL-UNBON         TO USR5SEQA
00920            PERFORM 9900-ERROR-FORMAT.
00921
00922      IF NOT EMI-NO-ERRORS
00923         GO TO 8200-SEND-DATAONLY
00924
00925      MOVE +0 TO WS-LO-LOSS WS-HI-LOSS.
00926
00927      IF  LOLOSSL GREATER THAN +0
00928          MOVE LOLOSSI                TO DEEDIT-FIELD
00929          PERFORM 8600-DEEDIT
00930          IF  DEEDIT-FIELD-V0  NOT NUMERIC
00931              MOVE -1             TO LOLOSSL
00932              MOVE AL-UNBON       TO LOLOSSA
00933              MOVE  ER-7683       TO EMI-ERROR
00934              PERFORM 9900-ERROR-FORMAT
00935          ELSE
00936              MOVE DEEDIT-FIELD-V2     TO WS-LO-LOSS.
00937
00938      IF  HILOSSL GREATER THAN +0
00939          MOVE HILOSSI                TO DEEDIT-FIELD
00940          PERFORM 8600-DEEDIT
00941          IF  DEEDIT-FIELD-V0     NOT NUMERIC
00942              MOVE -1             TO HILOSSL
00943              MOVE AL-UNBON       TO HILOSSA
00944              MOVE  ER-7683       TO EMI-ERROR
00945              PERFORM 9900-ERROR-FORMAT
00946          ELSE
00947              MOVE DEEDIT-FIELD-V2     TO WS-HI-LOSS.
00948
00949      IF MAINTI = 'A'
00950         IF WS-LO-LOSS GREATER THAN WS-HI-LOSS
00951            MOVE ER-7683             TO EMI-ERROR
00952            MOVE -1                  TO LOLOSSL
00953            MOVE AL-UNBON            TO LOLOSSA
00954            PERFORM 9900-ERROR-FORMAT.
00955
00956      MOVE LOW-VALUES TO WS-LO-EFF     WS-HI-EFF
00957                         WS-LO-ENT     WS-HI-ENT.
00958
00959      IF LOEFFDTL GREATER THAN +0
00960         MOVE LOEFFDTI               TO DEEDIT-FIELD
00961         PERFORM 8600-DEEDIT
00962         IF DEEDIT-FIELD-V0     NOT NUMERIC
00963            MOVE -1             TO LOEFFDTL
00964            MOVE AL-UNBON       TO LOEFFDTA
00965            MOVE  ER-0021       TO EMI-ERROR
00966            PERFORM 9900-ERROR-FORMAT
00967         ELSE
00968            IF DEEDIT-FIELD-V0 = ZEROS
00969               MOVE LOW-VALUES      TO WS-LO-EFF
00970            ELSE
00971               MOVE DEEDIT-FIELD-V0 TO DC-GREG-DATE-1-MDY
00972               MOVE '4'             TO DC-OPTION-CODE
00973               PERFORM 9700-DATE-LINK
00974               IF NO-CONVERSION-ERROR
00975                  MOVE DC-BIN-DATE-1 TO WS-LO-EFF
00976               ELSE
00977                  MOVE ER-0021       TO EMI-ERROR
00978                  MOVE -1            TO LOEFFDTL
00979                  MOVE AL-UNBON      TO LOEFFDTA
00980                  PERFORM 9900-ERROR-FORMAT.
00981
00982      IF HIEFFDTL GREATER THAN +0
00983         MOVE HIEFFDTI               TO DEEDIT-FIELD
00984         PERFORM 8600-DEEDIT
00985         IF DEEDIT-FIELD-V0     NOT NUMERIC
00986            MOVE -1             TO HIEFFDTL
00987            MOVE AL-UNBON       TO HIEFFDTA
00988            MOVE  ER-0021       TO EMI-ERROR
00989            PERFORM 9900-ERROR-FORMAT
00990         ELSE
00991            IF DEEDIT-FIELD-V0 = ZEROS
00992               MOVE LOW-VALUES TO WS-HI-EFF
00993            ELSE
00994               MOVE DEEDIT-FIELD-V0 TO DC-GREG-DATE-1-MDY
00995               MOVE '4'             TO DC-OPTION-CODE
00996               PERFORM 9700-DATE-LINK
00997               IF NO-CONVERSION-ERROR
00998                  MOVE DC-BIN-DATE-1 TO WS-HI-EFF
00999               ELSE
01000                  IF DEEDIT-FIELD-V0 = 999999 OR 9999999 OR
01001                                           99999999
01002                     MOVE HIGH-VALUES TO WS-HI-EFF
01003                  ELSE
01004                     MOVE ER-0021       TO EMI-ERROR
01005                     MOVE -1            TO HIEFFDTL
01006                     MOVE AL-UNBON      TO HIEFFDTA
01007                     PERFORM 9900-ERROR-FORMAT.
01008
01009      IF MAINTI = 'A'
01010         IF WS-LO-EFF GREATER THAN WS-HI-EFF
01011            MOVE ER-7684            TO EMI-ERROR
01012            MOVE -1                 TO LOEFFDTL
01013            MOVE AL-UNBON           TO LOEFFDTA
01014            PERFORM 9900-ERROR-FORMAT.
01015
01016      IF LOENTDTL GREATER THAN +0
01017         MOVE LOENTDTI               TO DEEDIT-FIELD
01018         PERFORM 8600-DEEDIT
01019         IF DEEDIT-FIELD-V0     NOT NUMERIC
01020            MOVE -1             TO LOENTDTL
01021            MOVE AL-UNBON       TO LOENTDTA
01022            MOVE  ER-0021       TO EMI-ERROR
01023            PERFORM 9900-ERROR-FORMAT
01024         ELSE
01025            IF DEEDIT-FIELD-V0 = ZEROS
01026               MOVE LOW-VALUES TO WS-LO-ENT
01027            ELSE
01028               MOVE DEEDIT-FIELD-V0 TO DC-GREG-DATE-1-MDY
01029               MOVE '4'             TO DC-OPTION-CODE
01030               PERFORM 9700-DATE-LINK
01031               IF NO-CONVERSION-ERROR
01032                  MOVE DC-BIN-DATE-1 TO WS-LO-ENT
01033               ELSE
01034                  MOVE ER-0021       TO EMI-ERROR
01035                  MOVE -1            TO LOENTDTL
01036                  MOVE AL-UNBON      TO LOENTDTA
01037                  PERFORM 9900-ERROR-FORMAT.
01038
01039      IF HIENTDTL GREATER THAN +0
01040         MOVE HIENTDTI               TO DEEDIT-FIELD
01041         PERFORM 8600-DEEDIT
01042         IF DEEDIT-FIELD-V0     NOT NUMERIC
01043            MOVE -1             TO HIENTDTL
01044            MOVE AL-UNBON       TO HIENTDTA
01045            MOVE  ER-0021       TO EMI-ERROR
01046            PERFORM 9900-ERROR-FORMAT
01047          ELSE
01048            IF DEEDIT-FIELD-V0 = ZEROS
01049               MOVE LOW-VALUES TO WS-LO-ENT
01050            ELSE
01051               MOVE DEEDIT-FIELD-V0 TO DC-GREG-DATE-1-MDY
01052               MOVE '4'             TO DC-OPTION-CODE
01053               PERFORM 9700-DATE-LINK
01054               IF NO-CONVERSION-ERROR
01055                  MOVE DC-BIN-DATE-1 TO WS-HI-ENT
01056               ELSE
01057                  IF DEEDIT-FIELD-V0 = 999999 OR 9999999 OR
01058                                           99999999
01059                     MOVE HIGH-VALUES TO WS-HI-ENT
01060                  ELSE
01061                     MOVE ER-0021       TO EMI-ERROR
01062                     MOVE -1            TO HIENTDTL
01063                     MOVE AL-UNBON      TO HIENTDTA
01064                     PERFORM 9900-ERROR-FORMAT.
01065
01066      IF MAINTI = 'A'
01067         IF WS-LO-ENT GREATER THAN WS-HI-ENT
01068            MOVE ER-7684            TO EMI-ERROR
01069            MOVE -1                 TO LOENTDTL
01070            MOVE AL-UNBON           TO LOENTDTA
01071            PERFORM 9900-ERROR-FORMAT.
01072
01073      IF NOT EMI-NO-ERRORS
01074         GO TO 8200-SEND-DATAONLY.
01075
01076  1200-ADD-REPORT-RECORD.
01077     
      * EXEC CICS GETMAIN
01078 *        SET       (ADDRESS OF CONTROL-FILE)
01079 *        LENGTH    (ELCNTL-LENGTH)
01080 *        INITIMG   (GETMAIN-SPACE)
01081 *    END-EXEC.
      *    MOVE ',"IL                  $   #00004180' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ELCNTL-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01082
01083      MOVE ZEROS                 TO CF-CARRIER-OPT-SEQ
01084                                    CF-GROUP-OPT-SEQ
01085                                    CF-STATE-OPT-SEQ
01086                                    CF-ACCOUNT-OPT-SEQ
01087                                    CF-AGENT-OPT-SEQ
01088                                    CF-REINS-OPT-SEQ
01089                                    CF-BUS-TYP-OPT-SEQ
01090                                    CF-LF-TYP-OPT-SEQ
01091                                    CF-AH-TYP-OPT-SEQ
01092                                    CF-REPTCD1-OPT-SEQ
01093                                    CF-REPTCD2-OPT-SEQ
01094                                    CF-USER1-OPT-SEQ
01095                                    CF-USER2-OPT-SEQ
01096                                    CF-USER3-OPT-SEQ
01097                                    CF-USER4-OPT-SEQ
01098                                    CF-USER5-OPT-SEQ.
01099      MOVE +0                    TO CF-SEL-LO-LOSS-RATIO
01100                                    CF-SEL-HI-LOSS-RATIO.
01101
01102      MOVE LOW-VALUES            TO CF-SEL-LO-ENTRY-DATE
01103                                    CF-SEL-HI-ENTRY-DATE
01104                                    CF-SEL-LO-EFFECTIVE-DATE
01105                                    CF-SEL-HI-EFFECTIVE-DATE.
01106
01107  1500-BUILD-RECORD.
01108
01109      MOVE 'CF'                  TO CF-RECORD-ID
01110      MOVE PI-COMPANY-ID         TO CF-COMPANY-ID
01111      MOVE 'C'                   TO CF-RECORD-TYPE
01112      MOVE REPORTI               TO CF-CUSTOM-REPORT-NO
01113      MOVE +0                    TO CF-SEQUENCE-NO.
01114
01115      MOVE SAVE-BIN-DATE         TO CF-LAST-MAINT-DT
01116      MOVE PI-PROCESSOR-ID       TO CF-LAST-MAINT-BY
01117      MOVE EIBTIME               TO CF-LAST-MAINT-HHMMSS.
01118
01119      IF CF-CARRIER-OPT-SEQ NOT NUMERIC
01120         MOVE ZEROS              TO CF-CARRIER-OPT-SEQ.
01121
01122      IF CF-GROUP-OPT-SEQ        NOT NUMERIC
01123         MOVE ZEROS              TO CF-GROUP-OPT-SEQ.
01124
01125      IF CF-STATE-OPT-SEQ        NOT NUMERIC
01126         MOVE ZEROS              TO CF-STATE-OPT-SEQ.
01127      IF CF-ACCOUNT-OPT-SEQ      NOT NUMERIC
01128         MOVE ZEROS              TO CF-ACCOUNT-OPT-SEQ.
01129      IF CF-AGENT-OPT-SEQ        NOT NUMERIC
01130         MOVE ZEROS              TO CF-AGENT-OPT-SEQ.
01131      IF CF-REINS-OPT-SEQ        NOT NUMERIC
01132         MOVE ZEROS              TO CF-REINS-OPT-SEQ.
01133      IF CF-BUS-TYP-OPT-SEQ      NOT NUMERIC
01134         MOVE ZEROS              TO CF-BUS-TYP-OPT-SEQ.
01135      IF CF-LF-TYP-OPT-SEQ       NOT NUMERIC
01136         MOVE ZEROS              TO CF-LF-TYP-OPT-SEQ.
01137      IF CF-AH-TYP-OPT-SEQ       NOT NUMERIC
01138         MOVE ZEROS              TO CF-AH-TYP-OPT-SEQ.
01139      IF CF-REPTCD1-OPT-SEQ      NOT NUMERIC
01140         MOVE ZEROS              TO CF-REPTCD1-OPT-SEQ.
01141      IF CF-REPTCD2-OPT-SEQ      NOT NUMERIC
01142         MOVE ZEROS              TO CF-REPTCD2-OPT-SEQ.
01143      IF CF-USER1-OPT-SEQ        NOT NUMERIC
01144         MOVE ZEROS              TO CF-USER1-OPT-SEQ.
01145      IF CF-USER2-OPT-SEQ        NOT NUMERIC
01146         MOVE ZEROS              TO CF-USER2-OPT-SEQ.
01147      IF CF-USER3-OPT-SEQ        NOT NUMERIC
01148         MOVE ZEROS              TO CF-USER3-OPT-SEQ.
01149      IF CF-USER4-OPT-SEQ        NOT NUMERIC
01150         MOVE ZEROS              TO CF-USER4-OPT-SEQ.
01151      IF CF-USER5-OPT-SEQ        NOT NUMERIC
01152         MOVE ZEROS              TO CF-USER5-OPT-SEQ.
01153      IF CF-SEL-LO-LOSS-RATIO    NOT NUMERIC
01154         MOVE +0                 TO CF-SEL-LO-LOSS-RATIO.
01155      IF CF-SEL-HI-LOSS-RATIO    NOT NUMERIC
01156         MOVE +0                 TO CF-SEL-HI-LOSS-RATIO.
01157
01158      IF STATUSL GREATER THAN +0
01159         MOVE STATUSI            TO CF-ACCOUNT-MASTER-STATUS.
01160      IF CARSEQL GREATER THAN +0
01161         MOVE CARSEQI            TO CF-CARRIER-OPT-SEQ.
01162      IF CAR1L GREATER THAN +0
01163         MOVE CAR1I              TO CF-CARRIER-SELECT (1).
01164      IF CAR2L GREATER THAN +0
01165         MOVE CAR2I              TO CF-CARRIER-SELECT (2).
01166      IF CAR3L GREATER THAN +0
01167         MOVE CAR3I              TO CF-CARRIER-SELECT (3).
01168      IF GRPSEQL GREATER THAN +0
01169         MOVE GRPSEQI            TO CF-GROUP-OPT-SEQ.
01170      IF GRP1L GREATER THAN +0
01171         MOVE GRP1I              TO CF-GROUP-SELECT (1).
01172      IF GRP2L GREATER THAN +0
01173         MOVE GRP2I              TO CF-GROUP-SELECT (2).
01174      IF GRP3L GREATER THAN +0
01175         MOVE GRP3I              TO CF-GROUP-SELECT (3).
01176      IF STSEQL GREATER THAN +0
01177         MOVE STSEQI             TO CF-STATE-OPT-SEQ.
01178      IF ST1L GREATER THAN +0
01179         MOVE ST1I               TO CF-STATE-SELECT (1).
01180      IF ST2L GREATER THAN +0
01181         MOVE ST2I               TO CF-STATE-SELECT (2).
01182      IF ST3L GREATER THAN +0
01183         MOVE ST3I               TO CF-STATE-SELECT (3).
01184      IF ACCTSEQL GREATER THAN +0
01185         MOVE ACCTSEQI           TO CF-ACCOUNT-OPT-SEQ.
01186      IF ACCT1L GREATER THAN +0
01187         MOVE ACCT1I             TO CF-ACCOUNT-SELECT (1).
01188      IF ACCT2L GREATER THAN +0
01189         MOVE ACCT2I             TO CF-ACCOUNT-SELECT (2).
01190      IF ACCT3L GREATER THAN +0
01191         MOVE ACCT3I             TO CF-ACCOUNT-SELECT (3).
01192      IF AGNTSEQL GREATER THAN +0
01193         MOVE AGNTSEQI           TO CF-AGENT-OPT-SEQ.
01194      IF AGNT1L GREATER THAN +0
01195         MOVE AGNT1I             TO CF-AGENT-SELECT (1).
01196      IF AGNT2L GREATER THAN +0
01197         MOVE AGNT2I             TO CF-AGENT-SELECT (2).
01198      IF AGNT3L GREATER THAN +0
01199         MOVE AGNT3I             TO CF-AGENT-SELECT (3).
01200      IF REINSEQL GREATER THAN +0
01201         MOVE REINSEQI           TO CF-REINS-OPT-SEQ.
01202      IF REIN1L GREATER THAN +0
01203         MOVE REIN1I             TO CF-REINS-SELECT (1).
01204      IF REIN2L GREATER THAN +0
01205         MOVE REIN2I             TO CF-REINS-SELECT (2).
01206      IF REIN3L GREATER THAN +0
01207         MOVE REIN3I             TO CF-REINS-SELECT (3).
01208      IF BUSSEQL GREATER THAN +0
01209         MOVE BUSSEQI            TO CF-BUS-TYP-OPT-SEQ.
01210      IF BUS1L GREATER THAN +0
01211         MOVE BUS1I              TO CF-BUS-TYP-SELECT (1).
01212      IF BUS2L GREATER THAN +0
01213         MOVE BUS2I              TO CF-BUS-TYP-SELECT (2).
01214      IF BUS3L GREATER THAN +0
01215         MOVE BUS3I              TO CF-BUS-TYP-SELECT (3).
01216      IF LBENSEQL GREATER THAN +0
01217         MOVE LBENSEQI           TO CF-LF-TYP-OPT-SEQ.
01218      IF LBEN1L GREATER THAN +0
01219         MOVE LBEN1I             TO CF-BUS-LF-SELECT (1).
01220      IF LBEN2L GREATER THAN +0
01221         MOVE LBEN2I             TO CF-BUS-LF-SELECT (2).
01222      IF LBEN3L GREATER THAN +0
01223         MOVE LBEN3I             TO CF-BUS-LF-SELECT (3).
01224      IF ABENSEQL GREATER THAN +0
01225         MOVE ABENSEQI           TO CF-AH-TYP-OPT-SEQ.
01226      IF ABEN1L GREATER THAN +0
01227         MOVE ABEN1I             TO CF-BUS-AH-SELECT (1).
01228      IF ABEN2L GREATER THAN +0
01229         MOVE ABEN2I             TO CF-BUS-AH-SELECT (2).
01230      IF ABEN3L GREATER THAN +0
01231         MOVE ABEN3I             TO CF-BUS-AH-SELECT (3).
01232      IF RPT1SEQL GREATER THAN +0
01233         MOVE RPT1SEQI           TO CF-REPTCD1-OPT-SEQ.
01234      IF RPTCD11L GREATER THAN +0
01235         MOVE RPTCD11I           TO CF-REPTCD1-SELECT (1).
01236      IF RPTCD12L GREATER THAN +0
01237         MOVE RPTCD12I           TO CF-REPTCD1-SELECT (2).
01238      IF RPTCD13L GREATER THAN +0
01239         MOVE RPTCD13I           TO CF-REPTCD1-SELECT (3).
01240      IF RPT2SEQL GREATER THAN +0
01241         MOVE RPT2SEQI           TO CF-REPTCD2-OPT-SEQ.
01242      IF RPTCD21L GREATER THAN +0
01243         MOVE RPTCD21I           TO CF-REPTCD2-SELECT (1).
01244      IF RPTCD22L GREATER THAN +0
01245         MOVE RPTCD22I           TO CF-REPTCD2-SELECT (2).
01246      IF RPTCD23L GREATER THAN +0
01247         MOVE RPTCD23I           TO CF-REPTCD2-SELECT (3).
01248      IF USR1SEQL GREATER THAN +0
01249         MOVE USR1SEQI           TO CF-USER1-OPT-SEQ.
01250      IF USR11L GREATER THAN +0
01251         MOVE USR11I             TO CF-USER1-SELECT (1).
01252      IF USR12L GREATER THAN +0
01253         MOVE USR12I             TO CF-USER1-SELECT (2).
01254      IF USR13L GREATER THAN +0
01255         MOVE USR13I             TO CF-USER1-SELECT (3).
01256      IF USR2SEQL GREATER THAN +0
01257         MOVE USR2SEQI           TO CF-USER2-OPT-SEQ.
01258      IF USR21L GREATER THAN +0
01259         MOVE USR21I             TO CF-USER2-SELECT (1).
01260      IF USR22L GREATER THAN +0
01261         MOVE USR22I             TO CF-USER2-SELECT (2).
01262      IF USR23L GREATER THAN +0
01263         MOVE USR23I             TO CF-USER2-SELECT (3).
01264      IF USR3SEQL GREATER THAN +0
01265         MOVE USR3SEQI           TO CF-USER3-OPT-SEQ.
01266      IF USR31L GREATER THAN +0
01267         MOVE USR31I             TO CF-USER3-SELECT (1).
01268      IF USR32L GREATER THAN +0
01269         MOVE USR32I             TO CF-USER3-SELECT (2).
01270      IF USR33L GREATER THAN +0
01271         MOVE USR33I             TO CF-USER3-SELECT (3).
01272      IF USR4SEQL GREATER THAN +0
01273         MOVE USR4SEQI           TO CF-USER4-OPT-SEQ.
01274      IF USR41L GREATER THAN +0
01275         MOVE USR41I             TO CF-USER4-SELECT (1).
01276      IF USR42L GREATER THAN +0
01277         MOVE USR42I             TO CF-USER4-SELECT (2).
01278      IF USR43L GREATER THAN +0
01279         MOVE USR43I             TO CF-USER4-SELECT (3).
01280      IF USR5SEQL GREATER THAN +0
01281         MOVE USR5SEQI           TO CF-USER5-OPT-SEQ.
01282      IF USR51L GREATER THAN +0
01283         MOVE USR51I             TO CF-USER5-SELECT (1).
01284      IF USR52L GREATER THAN +0
01285         MOVE USR52I             TO CF-USER5-SELECT (2).
01286      IF USR53L GREATER THAN +0
01287         MOVE USR53I             TO CF-USER5-SELECT (3).
01288      IF LOLOSSL GREATER THAN +0
01289         MOVE WS-LO-LOSS         TO CF-SEL-LO-LOSS-RATIO.
01290      IF HILOSSL GREATER THAN +0
01291         MOVE WS-HI-LOSS         TO CF-SEL-HI-LOSS-RATIO.
01292      IF LOENTDTL GREATER THAN +0
01293         MOVE WS-LO-ENT          TO CF-SEL-LO-ENTRY-DATE.
01294      IF HIENTDTL GREATER THAN +0
01295         MOVE WS-HI-ENT          TO CF-SEL-HI-ENTRY-DATE.
01296      IF LOEFFDTL GREATER THAN +0
01297         MOVE WS-LO-EFF          TO CF-SEL-LO-EFFECTIVE-DATE.
01298      IF HIEFFDTL GREATER THAN +0
01299         MOVE WS-HI-EFF          TO CF-SEL-HI-EFFECTIVE-DATE.
01300      IF EXPRPTL GREATER THAN +0
01301         MOVE EXPRPTI            TO CF-EXCEPTION-LIST-IND.
01302
01303      IF CF-EXCEPTION-LIST-IND EQUAL 'Y'
01304         IF CF-ACCOUNT-OPT-USED
01305             NEXT SENTENCE
01306         ELSE
01307            MOVE ER-7746             TO EMI-ERROR
01308            MOVE -1                  TO ACCTSEQL
01309            MOVE AL-UNBON            TO ACCTSEQA
01310            PERFORM 9900-ERROR-FORMAT
01311            GO TO 8200-SEND-DATAONLY.
01312
01313  1600-WRITE-REPORT-RECORD.
01314
01315      
      * EXEC CICS WRITE
01316 *         DATASET(ELCNTL-FILE-ID)
01317 *         FROM   (CONTROL-FILE)
01318 *         RIDFLD (ELCNTL-KEY)
01319 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004418' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01320
01321      MOVE ER-0000      TO EMI-ERROR.
01322      PERFORM 9900-ERROR-FORMAT.
01323      GO TO 4000-SHOW-REPORT.
01324
01325  2000-CHANGE-REPORT.
01326      IF REPORTI NOT NUMERIC
01327         MOVE ER-7680            TO EMI-ERROR
01328         MOVE -1                 TO REPORTL
01329         MOVE AL-UNBON           TO REPORTA
01330         PERFORM 9900-ERROR-FORMAT
01331         GO TO 8200-SEND-DATAONLY.
01332
01333      IF REPORTI NOT = PI-LAST-REPORT
01334         MOVE ER-0138            TO EMI-ERROR
01335         MOVE -1                 TO REPORTL
01336         MOVE AL-UNBON           TO REPORTA
01337         PERFORM 9900-ERROR-FORMAT
01338         GO TO 8200-SEND-DATAONLY.
01339
01340      MOVE SPACES                 TO ELCNTL-KEY
01341      MOVE PI-COMPANY-ID          TO ELCNTL-COMPANY-ID.
01342      MOVE 'C'                    TO ELCNTL-RECORD-TYPE.
01343      MOVE REPORTI                TO ELCNTL-REPORT.
01344      MOVE +0                     TO ELCNTL-SEQ-NO.
01345
01346      
      * EXEC CICS HANDLE CONDITION
01347 *         NOTFND     (2100-NOT-FOUND)
01348 *    END-EXEC.
      *    MOVE '"$I                   ! $ #00004449' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303034343439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01349
01350      
      * EXEC CICS READ
01351 *         DATASET    (ELCNTL-FILE-ID)
01352 *         SET        (ADDRESS OF CONTROL-FILE)
01353 *         RIDFLD     (ELCNTL-KEY)
01354 *    END-EXEC.
      *    MOVE '&"S        E          (   #00004453' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01355
01356      IF (CF-LAST-MAINT-BY NOT = PI-UPDATE-BY) OR
01357         (CF-LAST-MAINT-HHMMSS NOT = PI-UPDATE-HHMMSS)
01358         MOVE ER-0068          TO EMI-ERROR
01359         MOVE -1               TO MAINTL
01360         PERFORM 9900-ERROR-FORMAT
01361         GO TO 8200-SEND-DATAONLY.
01362
01363      PERFORM 1100-EDIT-REPORT-SCREEN.
01364
01365      IF NOT EMI-NO-ERRORS
01366         GO TO 8200-SEND-DATAONLY.
01367
01368      
      * EXEC CICS READ
01369 *         DATASET    (ELCNTL-FILE-ID)
01370 *         SET        (ADDRESS OF CONTROL-FILE)
01371 *         RIDFLD     (ELCNTL-KEY)
01372 *         UPDATE
01373 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00004471' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01374
01375      PERFORM 1500-BUILD-RECORD.
01376
01377      MOVE SPACES TO WS-SEQ-AREA.
01378
01379      IF CF-CARRIER-OPT-SEQ NOT = 0
01380         IF WS-SEQ-NO (CF-CARRIER-OPT-SEQ) NOT = 'X'
01381            MOVE 'X'     TO WS-SEQ-NO (CF-CARRIER-OPT-SEQ)
01382         ELSE
01383            MOVE ER-7682          TO EMI-ERROR
01384            MOVE -1               TO CARSEQL
01385            MOVE AL-UNBON         TO CARSEQA
01386            PERFORM 9900-ERROR-FORMAT.
01387
01388      IF CF-GROUP-OPT-SEQ NOT = 0
01389         IF WS-SEQ-NO (CF-GROUP-OPT-SEQ) NOT = 'X'
01390            MOVE 'X'     TO WS-SEQ-NO (CF-GROUP-OPT-SEQ)
01391         ELSE
01392            MOVE ER-7682          TO EMI-ERROR
01393            MOVE -1               TO GRPSEQL
01394            MOVE AL-UNBON         TO GRPSEQA
01395            PERFORM 9900-ERROR-FORMAT.
01396
01397      IF CF-STATE-OPT-SEQ NOT = 0
01398         IF WS-SEQ-NO (CF-STATE-OPT-SEQ) NOT = 'X'
01399            MOVE 'X'     TO WS-SEQ-NO (CF-STATE-OPT-SEQ)
01400         ELSE
01401            MOVE ER-7682          TO EMI-ERROR
01402            MOVE -1               TO STSEQL
01403            MOVE AL-UNBON         TO STSEQA
01404            PERFORM 9900-ERROR-FORMAT.
01405
01406      IF CF-BUS-TYP-OPT-SEQ NOT = 0
01407         IF WS-SEQ-NO (CF-BUS-TYP-OPT-SEQ) NOT = 'X'
01408            MOVE 'X'     TO WS-SEQ-NO (CF-BUS-TYP-OPT-SEQ)
01409         ELSE
01410            MOVE ER-7682          TO EMI-ERROR
01411            MOVE -1               TO BUSSEQL
01412            MOVE AL-UNBON         TO BUSSEQA
01413            PERFORM 9900-ERROR-FORMAT.
01414
01415      IF CF-LF-TYP-OPT-SEQ NOT = 0
01416         IF WS-SEQ-NO (CF-LF-TYP-OPT-SEQ) NOT = 'X'
01417            MOVE 'X'     TO WS-SEQ-NO (CF-LF-TYP-OPT-SEQ)
01418         ELSE
01419            MOVE ER-7682          TO EMI-ERROR
01420            MOVE -1               TO LBENSEQL
01421            MOVE AL-UNBON         TO LBENSEQA
01422            PERFORM 9900-ERROR-FORMAT.
01423
01424      IF CF-AH-TYP-OPT-SEQ NOT = 0
01425         IF WS-SEQ-NO (CF-AH-TYP-OPT-SEQ) NOT = 'X'
01426            MOVE 'X'     TO WS-SEQ-NO (CF-AH-TYP-OPT-SEQ)
01427         ELSE
01428            MOVE ER-7682          TO EMI-ERROR
01429            MOVE -1               TO ABENSEQL
01430            MOVE AL-UNBON         TO ABENSEQA
01431            PERFORM 9900-ERROR-FORMAT.
01432
01433      IF CF-ACCOUNT-OPT-SEQ NOT = 0
01434         IF WS-SEQ-NO (CF-ACCOUNT-OPT-SEQ) NOT = 'X'
01435            MOVE 'X'     TO WS-SEQ-NO (CF-ACCOUNT-OPT-SEQ)
01436         ELSE
01437            MOVE ER-7682          TO EMI-ERROR
01438            MOVE -1               TO ACCTSEQL
01439            MOVE AL-UNBON         TO ACCTSEQA
01440            PERFORM 9900-ERROR-FORMAT.
01441
01442      IF CF-AGENT-OPT-SEQ NOT NUMERIC
01443          MOVE ZEROS              TO CF-AGENT-OPT-SEQ.
01444
01445      IF CF-AGENT-OPT-SEQ NOT = 0
01446         IF WS-SEQ-NO (CF-AGENT-OPT-SEQ) NOT = 'X'
01447            MOVE 'X'     TO WS-SEQ-NO (CF-AGENT-OPT-SEQ)
01448         ELSE
01449            MOVE ER-7682          TO EMI-ERROR
01450            MOVE -1               TO AGNTSEQL
01451            MOVE AL-UNBON         TO AGNTSEQA
01452            PERFORM 9900-ERROR-FORMAT.
01453
01454      IF CF-REINS-OPT-SEQ NOT NUMERIC
01455          MOVE ZEROS              TO CF-REINS-OPT-SEQ.
01456
01457      IF CF-REINS-OPT-SEQ NOT = 0
01458         IF WS-SEQ-NO (CF-REINS-OPT-SEQ) NOT = 'X'
01459            MOVE 'X'     TO WS-SEQ-NO (CF-REINS-OPT-SEQ)
01460         ELSE
01461            MOVE ER-7682          TO EMI-ERROR
01462            MOVE -1               TO REINSEQL
01463            MOVE AL-UNBON         TO REINSEQA
01464            PERFORM 9900-ERROR-FORMAT.
01465
01466      IF CF-REPTCD1-OPT-SEQ NOT = 0
01467         IF WS-SEQ-NO (CF-REPTCD1-OPT-SEQ) NOT = 'X'
01468            MOVE 'X'     TO WS-SEQ-NO (CF-REPTCD1-OPT-SEQ)
01469         ELSE
01470            MOVE ER-7682          TO EMI-ERROR
01471            MOVE -1               TO RPT1SEQL
01472            MOVE AL-UNBON         TO RPT1SEQA
01473            PERFORM 9900-ERROR-FORMAT.
01474
01475      IF CF-REPTCD2-OPT-SEQ NOT = 0
01476         IF WS-SEQ-NO (CF-REPTCD2-OPT-SEQ) NOT = 'X'
01477            MOVE 'X'     TO WS-SEQ-NO (CF-REPTCD2-OPT-SEQ)
01478         ELSE
01479            MOVE ER-7682          TO EMI-ERROR
01480            MOVE -1               TO RPT2SEQL
01481            MOVE AL-UNBON         TO RPT2SEQA
01482            PERFORM 9900-ERROR-FORMAT.
01483
01484      IF CF-USER1-OPT-SEQ NOT = 0
01485         IF WS-SEQ-NO (CF-USER1-OPT-SEQ) NOT = 'X'
01486            MOVE 'X'     TO WS-SEQ-NO (CF-USER1-OPT-SEQ)
01487         ELSE
01488            MOVE ER-7682          TO EMI-ERROR
01489            MOVE -1               TO USR1SEQL
01490            MOVE AL-UNBON         TO USR1SEQA
01491            PERFORM 9900-ERROR-FORMAT.
01492
01493      IF CF-USER2-OPT-SEQ NOT = 0
01494         IF WS-SEQ-NO (CF-USER2-OPT-SEQ) NOT = 'X'
01495            MOVE 'X'     TO WS-SEQ-NO (CF-USER2-OPT-SEQ)
01496         ELSE
01497            MOVE ER-7682          TO EMI-ERROR
01498            MOVE -1               TO USR2SEQL
01499            MOVE AL-UNBON         TO USR2SEQA
01500            PERFORM 9900-ERROR-FORMAT.
01501
01502      IF CF-USER3-OPT-SEQ NOT = 0
01503         IF WS-SEQ-NO (CF-USER3-OPT-SEQ) NOT = 'X'
01504            MOVE 'X'     TO WS-SEQ-NO (CF-USER3-OPT-SEQ)
01505         ELSE
01506            MOVE ER-7682          TO EMI-ERROR
01507            MOVE -1               TO USR3SEQL
01508            MOVE AL-UNBON         TO USR3SEQA
01509            PERFORM 9900-ERROR-FORMAT.
01510
01511      IF CF-USER4-OPT-SEQ NOT = 0
01512         IF WS-SEQ-NO (CF-USER4-OPT-SEQ) NOT = 'X'
01513            MOVE 'X'     TO WS-SEQ-NO (CF-USER4-OPT-SEQ)
01514         ELSE
01515            MOVE ER-7682          TO EMI-ERROR
01516            MOVE -1               TO USR4SEQL
01517            MOVE AL-UNBON         TO USR4SEQA
01518            PERFORM 9900-ERROR-FORMAT.
01519
01520      IF CF-USER5-OPT-SEQ NOT = 0
01521         IF WS-SEQ-NO (CF-USER5-OPT-SEQ) NOT = 'X'
01522            MOVE 'X'     TO WS-SEQ-NO (CF-USER5-OPT-SEQ)
01523         ELSE
01524            MOVE ER-7682          TO EMI-ERROR
01525            MOVE -1               TO USR5SEQL
01526            MOVE AL-UNBON         TO USR5SEQA
01527            PERFORM 9900-ERROR-FORMAT.
01528
01529      IF CF-SEL-LO-LOSS-RATIO GREATER THAN CF-SEL-HI-LOSS-RATIO
01530         MOVE ER-7683             TO EMI-ERROR
01531         MOVE -1                  TO LOLOSSL
01532         MOVE AL-UNBON            TO LOLOSSA
01533         PERFORM 9900-ERROR-FORMAT.
01534
01535      IF CF-SEL-LO-ENTRY-DATE GREATER THAN CF-SEL-HI-ENTRY-DATE
01536         MOVE ER-7684             TO EMI-ERROR
01537         MOVE -1                  TO LOENTDTL
01538         MOVE AL-UNBON            TO LOENTDTA
01539         PERFORM 9900-ERROR-FORMAT.
01540
01541      IF CF-SEL-LO-EFFECTIVE-DATE GREATER THAN
01542                           CF-SEL-HI-EFFECTIVE-DATE
01543         MOVE ER-7684             TO EMI-ERROR
01544         MOVE -1                  TO LOEFFDTL
01545         MOVE AL-UNBON            TO LOEFFDTA
01546         PERFORM 9900-ERROR-FORMAT.
01547
01548      IF CF-EXCEPTION-LIST-IND EQUAL 'Y'
01549         IF CF-ACCOUNT-OPT-USED
01550             NEXT SENTENCE
01551         ELSE
01552            MOVE ER-7746             TO EMI-ERROR
01553            MOVE -1                  TO ACCTSEQL
01554            MOVE AL-UNBON            TO ACCTSEQA
01555            PERFORM 9900-ERROR-FORMAT.
01556
01557      IF NOT EMI-NO-ERRORS
01558         
      * EXEC CICS UNLOCK
01559 *            DATASET   (ELCNTL-FILE-ID)
01560 *       END-EXEC
      *    MOVE '&*                    #   #00004661' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01561         GO TO 8200-SEND-DATAONLY.
01562
01563      
      * EXEC CICS REWRITE
01564 *         DATASET   (ELCNTL-FILE-ID)
01565 *         FROM      (CONTROL-FILE)
01566 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004666' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01567
01568      MOVE ER-0000      TO EMI-ERROR.
01569      PERFORM 9900-ERROR-FORMAT.
01570      GO TO 4000-SHOW-REPORT.
01571
01572  2100-NOT-FOUND.
01573      MOVE ER-0139              TO EMI-ERROR.
01574      MOVE -1                   TO REPORTL.
01575      MOVE AL-UNBON             TO REPORTA.
01576      PERFORM 9900-ERROR-FORMAT.
01577      GO TO 8200-SEND-DATAONLY.
01578
01579      EJECT
01580
01581  2500-DELETE-REPORT.
01582      IF REPORTI NOT NUMERIC
01583         MOVE ER-7680            TO EMI-ERROR
01584         MOVE -1                 TO REPORTL
01585         MOVE AL-UNBON           TO REPORTA
01586         PERFORM 9900-ERROR-FORMAT
01587         GO TO 8200-SEND-DATAONLY.
01588
01589      IF REPORTI NOT = PI-LAST-REPORT
01590         MOVE ER-0138            TO EMI-ERROR
01591         MOVE -1                 TO REPORTL
01592         MOVE AL-UNBON           TO REPORTA
01593         PERFORM 9900-ERROR-FORMAT
01594         GO TO 8200-SEND-DATAONLY.
01595
01596      
      * EXEC CICS HANDLE CONDITION
01597 *        NOTFND   (2890-NOT-FOUND)
01598 *    END-EXEC.
      *    MOVE '"$I                   ! % #00004699' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303034363939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01599
01600      MOVE SPACES                 TO ELCNTL-KEY.
01601
01602      MOVE PI-COMPANY-ID          TO ELCNTL-COMPANY-ID.
01603      MOVE 'C'                    TO ELCNTL-RECORD-TYPE.
01604      MOVE REPORTI                TO ELCNTL-REPORT.
01605      MOVE +0                     TO ELCNTL-SEQ-NO.
01606
01607      
      * EXEC CICS DELETE
01608 *         DATASET   (ELCNTL-FILE-ID)
01609 *         RIDFLD    (ELCNTL-KEY)
01610 *    END-EXEC.
      *    MOVE '&(  R                 &   #00004710' TO DFHEIV0
           MOVE X'262820205220202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01611
01612      MOVE LOW-VALUES TO EL604AI.
01613      MOVE ER-0000      TO EMI-ERROR.
01614      PERFORM 9900-ERROR-FORMAT.
01615      GO TO 8100-SEND-INITIAL-MAP.
01616
01617  2890-NOT-FOUND.
01618      MOVE  ER-0142               TO EMI-ERROR.
01619      MOVE -1                     TO REPORTL.
01620      MOVE AL-UNBON               TO REPORTA.
01621      PERFORM 9900-ERROR-FORMAT.
01622      GO TO 8200-SEND-DATAONLY.
01623
01624      EJECT
01625  3000-COPY-REPORT.
01626      IF REPORTL GREATER +0 AND
01627         REPORTI NUMERIC
01628         NEXT SENTENCE
01629      ELSE
01630         MOVE ER-7680     TO EMI-ERROR
01631         MOVE -1          TO REPORTL
01632         MOVE AL-UNBON    TO REPORTA
01633         PERFORM 9900-ERROR-FORMAT
01634         GO TO 8200-SEND-DATAONLY.
01635
01636      IF CPYRPTL GREATER +0 AND
01637         CPYRPTI NUMERIC
01638         NEXT SENTENCE
01639      ELSE
01640         MOVE ER-7680     TO EMI-ERROR
01641         MOVE -1          TO CPYRPTL
01642         MOVE AL-UNBON    TO CPYRPTA
01643         PERFORM 9900-ERROR-FORMAT
01644         GO TO 8200-SEND-DATAONLY.
01645
01646      MOVE SPACES            TO ELCNTL-KEY.
01647      MOVE PI-COMPANY-ID     TO ELCNTL-COMPANY-ID.
01648      MOVE REPORTI           TO ELCNTL-REPORT.
01649      MOVE 'C'               TO ELCNTL-RECORD-TYPE.
01650      MOVE +0                TO ELCNTL-SEQ-NO.
01651
01652      
      * EXEC CICS HANDLE CONDITION
01653 *         NOTFND     (3100-CONTINUE)
01654 *    END-EXEC.
      *    MOVE '"$I                   ! & #00004755' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303034373535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01655
01656      
      * EXEC CICS READ
01657 *         DATASET    (ELCNTL-FILE-ID)
01658 *         RIDFLD     (ELCNTL-KEY)
01659 *         SET        (ADDRESS OF CONTROL-FILE)
01660 *    END-EXEC.
      *    MOVE '&"S        E          (   #00004759' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01661
01662      MOVE -1                     TO REPORTL.
01663      MOVE  ER-0132               TO  EMI-ERROR.
01664      PERFORM 9900-ERROR-FORMAT.
01665      GO TO 8200-SEND-DATAONLY.
01666
01667  3100-CONTINUE.
01668      MOVE SPACES            TO ELCNTL-KEY.
01669      MOVE PI-COMPANY-ID     TO ELCNTL-COMPANY-ID.
01670      MOVE CPYRPTI           TO ELCNTL-REPORT.
01671      MOVE 'C'               TO ELCNTL-RECORD-TYPE.
01672      MOVE +0                TO ELCNTL-SEQ-NO.
01673
01674      
      * EXEC CICS HANDLE CONDITION
01675 *         NOTFND     (3900-NOT-FOUND)
01676 *    END-EXEC.
      *    MOVE '"$I                   ! '' #00004777' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303034373737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01677
01678      
      * EXEC CICS READ
01679 *         DATASET    (ELCNTL-FILE-ID)
01680 *         RIDFLD     (ELCNTL-KEY)
01681 *         SET        (ADDRESS OF CONTROL-FILE)
01682 *    END-EXEC.
      *    MOVE '&"S        E          (   #00004781' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01683
01684      MOVE SPACES            TO ELCNTL-KEY.
01685      MOVE 'CF'                  TO CF-RECORD-ID
01686      MOVE PI-COMPANY-ID         TO CF-COMPANY-ID
01687                                    ELCNTL-COMPANY-ID.
01688      MOVE 'C'                   TO CF-RECORD-TYPE
01689                                    ELCNTL-RECORD-TYPE.
01690      MOVE REPORTI               TO CF-CUSTOM-REPORT-NO
01691                                    ELCNTL-REPORT.
01692      MOVE +0                    TO CF-SEQUENCE-NO
01693                                    ELCNTL-SEQ-NO.
01694
01695      MOVE SAVE-BIN-DATE         TO CF-LAST-MAINT-DT
01696      MOVE PI-PROCESSOR-ID       TO CF-LAST-MAINT-BY
01697      MOVE EIBTIME               TO CF-LAST-MAINT-HHMMSS.
01698
01699      MOVE CF-CUSTOM-REPORT-NO          TO PI-LAST-REPORT
01700                                           REPORTO.
01701
01702      MOVE CF-LAST-MAINT-BY            TO PI-UPDATE-BY.
01703      MOVE CF-LAST-MAINT-HHMMSS        TO PI-UPDATE-HHMMSS.
01704
01705      
      * EXEC CICS WRITE
01706 *         DATASET(ELCNTL-FILE-ID)
01707 *         FROM   (CONTROL-FILE)
01708 *         RIDFLD (ELCNTL-KEY)
01709 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004808' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01710
01711      MOVE ER-0000      TO EMI-ERROR.
01712      PERFORM 9900-ERROR-FORMAT.
01713      GO TO 4000-SHOW-REPORT.
01714
01715  3900-NOT-FOUND.
01716      MOVE  ER-0142               TO EMI-ERROR.
01717      MOVE -1                     TO CPYRPTL.
01718      MOVE AL-UNBON               TO CPYRPTA.
01719      PERFORM 9900-ERROR-FORMAT.
01720      GO TO 8200-SEND-DATAONLY.
01721
01722  4000-SHOW-REPORT.
01723      IF MAINTI = 'S'
01724         IF REPORTL GREATER THAN +0 AND
01725            REPORTI NUMERIC
01726            NEXT SENTENCE
01727         ELSE
01728            MOVE ER-7680     TO EMI-ERROR
01729            MOVE -1          TO REPORTL
01730            MOVE AL-UNBON    TO REPORTA
01731            PERFORM 9900-ERROR-FORMAT
01732            GO TO 8200-SEND-DATAONLY.
01733
01734      IF MAINTI = 'S'
01735         MOVE SPACES              TO ELCNTL-KEY
01736         MOVE PI-COMPANY-ID       TO ELCNTL-COMPANY-ID
01737         MOVE 'C'                 TO ELCNTL-RECORD-TYPE
01738         MOVE REPORTI             TO ELCNTL-REPORT
01739         MOVE +0                  TO ELCNTL-SEQ-NO.
01740
01741      
      * EXEC CICS HANDLE CONDITION
01742 *         NOTFND     (4100-NOT-FOUND)
01743 *    END-EXEC.
      *    MOVE '"$I                   ! ( #00004844' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303034383434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01744
01745      
      * EXEC CICS READ
01746 *         DATASET     (ELCNTL-FILE-ID)
01747 *         RIDFLD      (ELCNTL-KEY)
01748 *         SET         (ADDRESS OF CONTROL-FILE)
01749 *    END-EXEC.
      *    MOVE '&"S        E          (   #00004848' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01750
01751      MOVE LOW-VALUES                   TO EL604AI.
01752
01753      MOVE CF-CUSTOM-REPORT-NO          TO PI-LAST-REPORT
01754                                           REPORTO.
01755      MOVE CF-ACCOUNT-MASTER-STATUS     TO STATUSO.
01756      IF CF-CARRIER-OPT-SEQ NOT = 0
01757         MOVE CF-CARRIER-OPT-SEQ        TO CARSEQO.
01758      MOVE CF-CARRIER-SELECT (1)        TO CAR1O.
01759      MOVE CF-CARRIER-SELECT (2)        TO CAR2O.
01760      MOVE CF-CARRIER-SELECT (3)        TO CAR3O.
01761      IF CF-GROUP-OPT-SEQ NOT = 0
01762         MOVE CF-GROUP-OPT-SEQ          TO GRPSEQO.
01763      MOVE CF-GROUP-SELECT (1)          TO GRP1O.
01764      MOVE CF-GROUP-SELECT (2)          TO GRP2O.
01765      MOVE CF-GROUP-SELECT (3)          TO GRP3O.
01766      IF CF-STATE-OPT-SEQ NOT = 0
01767         MOVE CF-STATE-OPT-SEQ          TO STSEQO.
01768      MOVE CF-STATE-SELECT (1)          TO ST1O.
01769      MOVE CF-STATE-SELECT (2)          TO ST2O.
01770      MOVE CF-STATE-SELECT (3)          TO ST3O.
01771      IF CF-ACCOUNT-OPT-SEQ NOT = 0
01772         MOVE CF-ACCOUNT-OPT-SEQ        TO ACCTSEQO.
01773      MOVE CF-ACCOUNT-SELECT (1)        TO ACCT1O.
01774      MOVE CF-ACCOUNT-SELECT (2)        TO ACCT2O.
01775      MOVE CF-ACCOUNT-SELECT (3)        TO ACCT3O.
01776      IF CF-AGENT-OPT-SEQ NOT NUMERIC
01777         MOVE ZEROS                     TO CF-AGENT-OPT-SEQ.
01778      IF CF-AGENT-OPT-SEQ NOT = 0
01779         MOVE CF-AGENT-OPT-SEQ          TO AGNTSEQO.
01780      MOVE CF-AGENT-SELECT (1)          TO AGNT1O.
01781      MOVE CF-AGENT-SELECT (2)          TO AGNT2O.
01782      MOVE CF-AGENT-SELECT (3)          TO AGNT3O.
01783
01784      IF CF-REINS-OPT-SEQ NOT NUMERIC
01785          MOVE ZEROS              TO CF-REINS-OPT-SEQ.
01786
01787      IF CF-REINS-OPT-SEQ NOT = 0
01788         MOVE CF-REINS-OPT-SEQ          TO REINSEQO.
01789
01790      MOVE CF-REINS-SELECT (1)          TO REIN1O.
01791      MOVE CF-REINS-SELECT (2)          TO REIN2O.
01792      MOVE CF-REINS-SELECT (3)          TO REIN3O.
01793
01794      IF CF-BUS-TYP-OPT-SEQ NOT = 0
01795         MOVE CF-BUS-TYP-OPT-SEQ        TO BUSSEQO.
01796      IF CF-BUS-TYP-SELECT (1) NOT = ZEROS
01797         MOVE CF-BUS-TYP-SELECT (1)     TO BUS1O.
01798      IF CF-BUS-TYP-SELECT (2) NOT = ZEROS
01799         MOVE CF-BUS-TYP-SELECT (2)     TO BUS2O.
01800      IF CF-BUS-TYP-SELECT (3) NOT = ZEROS
01801         MOVE CF-BUS-TYP-SELECT (3)     TO BUS3O.
01802      IF CF-LF-TYP-OPT-SEQ NOT = 0
01803         MOVE CF-LF-TYP-OPT-SEQ         TO LBENSEQO.
01804      IF CF-BUS-LF-SELECT (1) NOT = ZEROS
01805         MOVE CF-BUS-LF-SELECT (1)      TO LBEN1O.
01806      IF CF-BUS-LF-SELECT (2) NOT = ZEROS
01807         MOVE CF-BUS-LF-SELECT (2)      TO LBEN2O.
01808      IF CF-BUS-LF-SELECT (3) NOT = ZEROS
01809         MOVE CF-BUS-LF-SELECT (3)      TO LBEN3O.
01810      IF CF-AH-TYP-OPT-SEQ NOT = 0
01811         MOVE CF-AH-TYP-OPT-SEQ         TO ABENSEQO.
01812      IF CF-BUS-AH-SELECT (1) NOT = ZEROS
01813         MOVE CF-BUS-AH-SELECT (1)      TO ABEN1O.
01814      IF CF-BUS-AH-SELECT (2) NOT = ZEROS
01815         MOVE CF-BUS-AH-SELECT (2)      TO ABEN2O.
01816      IF CF-BUS-AH-SELECT (3) NOT = ZEROS
01817         MOVE CF-BUS-AH-SELECT (3)      TO ABEN3O.
01818      IF CF-REPTCD1-OPT-SEQ NOT = 0
01819         MOVE CF-REPTCD1-OPT-SEQ        TO RPT1SEQO.
01820      MOVE CF-REPTCD1-SELECT (1)        TO RPTCD11O.
01821      MOVE CF-REPTCD1-SELECT (2)        TO RPTCD12O.
01822      MOVE CF-REPTCD1-SELECT (3)        TO RPTCD13O.
01823      IF CF-REPTCD2-OPT-SEQ NOT = 0
01824         MOVE CF-REPTCD2-OPT-SEQ        TO RPT2SEQO.
01825      MOVE CF-REPTCD2-SELECT (1)        TO RPTCD21O.
01826      MOVE CF-REPTCD2-SELECT (2)        TO RPTCD22O.
01827      MOVE CF-REPTCD2-SELECT (3)        TO RPTCD23O.
01828      IF CF-USER1-OPT-SEQ NOT = 0
01829         MOVE CF-USER1-OPT-SEQ          TO USR1SEQO.
01830      MOVE CF-USER1-SELECT (1)          TO USR11O.
01831      MOVE CF-USER1-SELECT (2)          TO USR12O.
01832      MOVE CF-USER1-SELECT (3)          TO USR13O.
01833      IF CF-USER2-OPT-SEQ NOT = 0
01834         MOVE CF-USER2-OPT-SEQ          TO USR2SEQO.
01835      MOVE CF-USER2-SELECT (1)          TO USR21O.
01836      MOVE CF-USER2-SELECT (2)          TO USR22O.
01837      MOVE CF-USER2-SELECT (3)          TO USR23O.
01838      IF CF-USER3-OPT-SEQ NOT = 0
01839         MOVE CF-USER3-OPT-SEQ          TO USR3SEQO.
01840      MOVE CF-USER3-SELECT (1)          TO USR31O.
01841      MOVE CF-USER3-SELECT (2)          TO USR32O.
01842      MOVE CF-USER3-SELECT (3)          TO USR33O.
01843      IF CF-USER4-OPT-SEQ NOT = 0
01844         MOVE CF-USER4-OPT-SEQ          TO USR4SEQO.
01845      MOVE CF-USER4-SELECT (1)          TO USR41O.
01846      MOVE CF-USER4-SELECT (2)          TO USR42O.
01847      MOVE CF-USER4-SELECT (3)          TO USR43O.
01848      IF CF-USER5-OPT-SEQ NOT = 0
01849         MOVE CF-USER5-OPT-SEQ          TO USR5SEQO.
01850      MOVE CF-USER5-SELECT (1)          TO USR51O.
01851      MOVE CF-USER5-SELECT (2)          TO USR52O.
01852      MOVE CF-USER5-SELECT (3)          TO USR53O.
pemuni     IF CF-SEL-LO-LOSS-RATIO    NOT NUMERIC
pemuni        MOVE +0                 TO CF-SEL-LO-LOSS-RATIO
pemuni     end-if
pemuni     IF CF-SEL-HI-LOSS-RATIO    NOT NUMERIC
pemuni        MOVE +0                 TO CF-SEL-HI-LOSS-RATIO
pemuni     end-if
01853      MOVE CF-SEL-LO-LOSS-RATIO         TO LOLOSSO.
01854      MOVE CF-SEL-HI-LOSS-RATIO         TO HILOSSO.
01855
01856      IF CF-SEL-LO-ENTRY-DATE NOT = LOW-VALUES
01857         MOVE CF-SEL-LO-ENTRY-DATE TO DC-BIN-DATE-1
01858         MOVE ' '                  TO DC-OPTION-CODE
01859         PERFORM 9700-DATE-LINK
01860         IF NO-CONVERSION-ERROR
01861            MOVE DC-GREG-DATE-1-EDIT     TO LOENTDTO.
01862
01863      IF CF-SEL-HI-ENTRY-DATE = LOW-VALUES
01864         NEXT SENTENCE
01865      ELSE
01866      IF CF-SEL-HI-ENTRY-DATE = HIGH-VALUES
01867         MOVE '99/99/99'                TO HIENTDTO
01868      ELSE
01869         MOVE CF-SEL-HI-ENTRY-DATE TO DC-BIN-DATE-1
01870         MOVE ' '                  TO DC-OPTION-CODE
01871         PERFORM 9700-DATE-LINK
01872         IF NO-CONVERSION-ERROR
01873            MOVE DC-GREG-DATE-1-EDIT     TO HIENTDTO.
01874
01875      IF CF-SEL-LO-EFFECTIVE-DATE = LOW-VALUES
01876         MOVE SPACES                    TO LOEFFDTO
01877      ELSE
01878         MOVE CF-SEL-LO-EFFECTIVE-DATE TO DC-BIN-DATE-1
01879         MOVE ' '                  TO DC-OPTION-CODE
01880         PERFORM 9700-DATE-LINK
01881         IF NO-CONVERSION-ERROR
01882            MOVE DC-GREG-DATE-1-EDIT     TO LOEFFDTO.
01883
01884      IF CF-SEL-HI-EFFECTIVE-DATE = LOW-VALUES
01885         MOVE SPACES                    TO HIEFFDTO
01886      ELSE
01887      IF CF-SEL-HI-EFFECTIVE-DATE = HIGH-VALUES
01888         MOVE '99/99/99'                TO HIEFFDTO
01889      ELSE
01890         MOVE CF-SEL-HI-EFFECTIVE-DATE TO DC-BIN-DATE-1
01891         MOVE ' '                  TO DC-OPTION-CODE
01892         PERFORM 9700-DATE-LINK
01893         IF NO-CONVERSION-ERROR
01894            MOVE DC-GREG-DATE-1-EDIT     TO HIEFFDTO.
01895
01896      MOVE CF-EXCEPTION-LIST-IND TO EXPRPTO.
01897
01898      MOVE CF-LAST-MAINT-BY            TO PI-UPDATE-BY.
01899      MOVE CF-LAST-MAINT-HHMMSS        TO PI-UPDATE-HHMMSS.
01900
01901      GO TO 8100-SEND-INITIAL-MAP.
01902
01903  4100-NOT-FOUND.
01904      MOVE  ER-0142               TO EMI-ERROR.
01905      MOVE -1                     TO REPORTL.
01906      MOVE AL-UNBON               TO REPORTA.
01907      PERFORM 9900-ERROR-FORMAT.
01908      GO TO 8200-SEND-DATAONLY.
01909
01910  5000-VERIFY-CARRIER.
01911      
      * EXEC CICS HANDLE CONDITION
01912 *         NOTFND    (5010-NOT-FOUND)
01913 *         ENDFILE   (5010-NOT-FOUND)
01914 *    END-EXEC.
      *    MOVE '"$I''                  ! ) #00005020' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303035303230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01915
01916      MOVE SPACES TO ELCNTL-TEST-KEY.
01917      MOVE PI-COMPANY-ID      TO ELCNTL-TEST-COMP-ID.
01918      MOVE '6'                TO ELCNTL-TEST-REC-TYPE.
01919      MOVE WS-TEST-CARRIER    TO ELCNTL-TEST-CAR.
01920      MOVE +0                 TO ELCNTL-TEST-SEQ-NO.
01921
01922      
      * EXEC CICS READ
01923 *         DATASET    (ELCNTL-FILE-ID)
01924 *         RIDFLD     (ELCNTL-TEST-KEY)
01925 *         SET        (ADDRESS OF CONTROL-FILE)
01926 *    END-EXEC.
      *    MOVE '&"S        E          (   #00005031' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-TEST-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01927
01928      GO TO 5019-EXIT.
01929
01930  5010-NOT-FOUND.
01931      MOVE ER-2845           TO EMI-ERROR
01932      MOVE -1                TO WS-TEST-LEN.
01933      MOVE AL-UNBON          TO WS-TEST-ATTRB.
01934      PERFORM 9900-ERROR-FORMAT.
01935
01936  5019-EXIT.
01937      EXIT.
01938
01939  5020-VERIFY-STATE.
01940      
      * EXEC CICS HANDLE CONDITION
01941 *         NOTFND    (5030-NOT-FOUND)
01942 *         ENDFILE   (5030-NOT-FOUND)
01943 *    END-EXEC.
      *    MOVE '"$I''                  ! * #00005049' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303035303439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01944
01945      MOVE SPACES TO ELCNTL-TEST-KEY.
01946      MOVE PI-COMPANY-ID      TO ELCNTL-TEST-COMP-ID.
01947      MOVE '3'                TO ELCNTL-TEST-REC-TYPE.
01948      MOVE WS-TEST-STATE      TO ELCNTL-TEST-STATE.
01949      MOVE +0                 TO ELCNTL-TEST-SEQ-NO.
01950
01951      
      * EXEC CICS READ
01952 *         DATASET    (ELCNTL-FILE-ID)
01953 *         RIDFLD     (ELCNTL-TEST-KEY)
01954 *         SET        (ADDRESS OF CONTROL-FILE)
01955 *    END-EXEC.
      *    MOVE '&"S        E          (   #00005060' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-TEST-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01956
01957      GO TO 5039-EXIT.
01958
01959  5030-NOT-FOUND.
01960      MOVE ER-2848           TO EMI-ERROR
01961      MOVE -1                TO WS-TEST-LEN.
01962      MOVE AL-UNBON          TO WS-TEST-ATTRB.
01963      PERFORM 9900-ERROR-FORMAT.
01964
01965  5039-EXIT.
01966      EXIT.
01967
01968  5040-VERIFY-BEN.
01969      IF WS-TEST-BEN = SPACES
01970          GO TO 5059-EXIT.
01971
01972      MOVE SPACES TO WS-BROWSE-SW.
01973
01974      
      * EXEC CICS HANDLE CONDITION
01975 *         NOTFND    (5050-NOT-FOUND)
01976 *         ENDFILE   (5050-NOT-FOUND)
01977 *    END-EXEC.
      *    MOVE '"$I''                  ! + #00005083' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303035303833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01978
01979      MOVE PI-COMPANY-ID      TO ELCNTL-TEST-COMP-ID.
01980      MOVE WS-TEST-BEN        TO ELCNTL-TEST-BEN.
01981      MOVE +0                 TO ELCNTL-TEST-SEQ-NO.
01982
01983      
      * EXEC CICS STARTBR
01984 *         DATASET    (ELCNTL-FILE-ID)
01985 *         RIDFLD     (ELCNTL-TEST-KEY)
01986 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005092' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 ELCNTL-TEST-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01987
01988      MOVE 'Y'    TO WS-BROWSE-SW.
01989
01990      
      * EXEC CICS READNEXT
01991 *         DATASET    (ELCNTL-FILE-ID)
01992 *         RIDFLD     (ELCNTL-TEST-KEY)
01993 *         SET        (ADDRESS OF CONTROL-FILE)
01994 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005099' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-TEST-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01995
01996      MOVE +1  TO WS-SUB.
01997
01998  5041-SEARCH-BEN-TYPE.
01999      IF WS-SUB  GREATER THAN +8
02000         GO TO 5050-NOT-FOUND.
02001
02002      IF CF-BENEFIT-CODE (WS-SUB) = WS-TEST-BEN
02003         GO TO 5055-ENDBR
02004      ELSE
02005         ADD +1 TO WS-SUB
02006         GO TO 5041-SEARCH-BEN-TYPE.
02007
02008  5050-NOT-FOUND.
02009      MOVE ER-7685           TO EMI-ERROR
02010      MOVE -1                TO WS-TEST-LEN.
02011      MOVE AL-UNBON          TO WS-TEST-ATTRB.
02012      PERFORM 9900-ERROR-FORMAT.
02013
02014  5055-ENDBR.
02015      IF WS-BROWSE-SW = 'Y'
02016         
      * EXEC CICS ENDBR
02017 *            DATASET    (ELCNTL-FILE-ID)
02018 *       END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005125' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02019
02020  5059-EXIT.
02021      EXIT.
02022
02023  5060-VERIFY-BUS-TYPE.
02024      MOVE SPACES TO WS-BROWSE-SW.
02025
02026      
      * EXEC CICS HANDLE CONDITION
02027 *         NOTFND    (5070-NOT-FOUND)
02028 *         ENDFILE   (5070-NOT-FOUND)
02029 *    END-EXEC.
      *    MOVE '"$I''                  ! , #00005135' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303035313335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02030
02031      MOVE SPACES             TO ELCNTL-TEST-KEY.
02032      MOVE PI-COMPANY-ID      TO ELCNTL-TEST-COMP-ID.
02033      MOVE '8'                TO ELCNTL-TEST-REC-TYPE.
02034      MOVE WS-TEST-BUSTYP     TO ELCNTL-TEST-BUSTYP.
02035      MOVE +0                 TO ELCNTL-TEST-SEQ-NO.
02036
02037      
      * EXEC CICS STARTBR
02038 *         DATASET    (ELCNTL-FILE-ID)
02039 *         RIDFLD     (ELCNTL-TEST-KEY)
02040 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005146' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 ELCNTL-TEST-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02041
02042      MOVE 'Y' TO WS-BROWSE-SW.
02043
02044      
      * EXEC CICS READNEXT
02045 *         DATASET    (ELCNTL-FILE-ID)
02046 *         RIDFLD     (ELCNTL-TEST-KEY)
02047 *         SET        (ADDRESS OF CONTROL-FILE)
02048 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005153' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-TEST-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02049
02050      COMPUTE WS-SUB = WS-TEST-BUSTYP -
02051                     (ELCNTL-TEST-BUSTYP - 20).
02052
02053      IF ELCNTL-TEST-BUSTYP = 99
02054         SUBTRACT +1 FROM WS-SUB.
02055
02056      IF CF-BUSINESS-TITLE (WS-SUB) = SPACES
02057         GO TO 5070-NOT-FOUND.
02058
02059      GO TO 5075-ENDBR.
02060
02061  5070-NOT-FOUND.
02062
02063      MOVE ER-2178           TO EMI-ERROR
02064      MOVE -1                TO WS-TEST-LEN.
02065      MOVE AL-UNBON          TO WS-TEST-ATTRB.
02066      PERFORM 9900-ERROR-FORMAT.
02067
02068  5075-ENDBR.
02069      IF WS-BROWSE-SW = 'Y'
02070         
      * EXEC CICS ENDBR
02071 *            DATASET    (ELCNTL-FILE-ID)
02072 *       END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005179' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02073
02074  5079-EXIT.
02075      EXIT.
02076      EJECT
02077
02078  7000-BROWSE-FWRD-NEXT-ACCOUNT.
02079      MOVE SPACES            TO ELCNTL-KEY.
02080      MOVE PI-COMPANY-ID     TO ELCNTL-COMPANY-ID.
02081      MOVE 'C'               TO ELCNTL-RECORD-TYPE.
02082      MOVE +1                TO ELCNTL-SEQ-NO.
02083
02084      IF REPORTL GREATER THAN +0
02085         MOVE REPORTI            TO ELCNTL-REPORT
02086      ELSE
02087         MOVE ZEROS              TO ELCNTL-REPORT.
02088
02089      MOVE 'N'                    TO WS-REPORT-FOUND-SW.
02090
02091      
      * EXEC CICS HANDLE CONDITION
02092 *        NOTFND   (7080-END-OF-SEARCH)
02093 *    END-EXEC.
      *    MOVE '"$I                   ! - #00005200' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303035323030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02094
02095      
      * EXEC CICS STARTBR
02096 *        DATASET  (ELCNTL-FILE-ID)
02097 *        RIDFLD   (ELCNTL-KEY)
02098 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005204' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02099
02100      
      * EXEC CICS HANDLE CONDITION
02101 *        NOTFND   (7070-END-OF-BROWSE)
02102 *        ENDFILE  (7070-END-OF-BROWSE)
02103 *    END-EXEC.
      *    MOVE '"$I''                  ! . #00005209' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303035323039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02104
02105  7010-READ-FWRD-NEXT-RECORD.
02106      
      * EXEC CICS READNEXT
02107 *        DATASET  (ELCNTL-FILE-ID)
02108 *        RIDFLD   (ELCNTL-KEY)
02109 *        SET      (ADDRESS OF CONTROL-FILE)
02110 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005215' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02111
02112      IF ELCNTL-COMPANY-ID = PI-COMPANY-ID AND
02113         ELCNTL-RECORD-TYPE = 'C'
02114         NEXT SENTENCE
02115      ELSE
02116         GO TO 7070-END-OF-BROWSE.
02117
02118      IF ELCNTL-SEQ-NO = +0
02119          NEXT SENTENCE
02120      ELSE
02121          GO TO 7010-READ-FWRD-NEXT-RECORD.
02122
02123      MOVE 'Y'                    TO WS-REPORT-FOUND-SW.
02124
02125  7070-END-OF-BROWSE.
02126      
      * EXEC CICS ENDBR
02127 *        DATASET  (ELCNTL-FILE-ID)
02128 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005235' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02129
02130       IF REPORT-WAS-NOT-FOUND
02131          NEXT SENTENCE
02132       ELSE
02133          GO TO 7090-EXIT.
02134
02135  7080-END-OF-SEARCH.
02136      MOVE -1                     TO MAINTL.
02137      MOVE  ER-2237               TO EMI-ERROR.
02138      PERFORM 9900-ERROR-FORMAT.
02139      GO TO 8200-SEND-DATAONLY.
02140
02141  7090-EXIT.
02142      EXIT.
02143
02144      EJECT
02145  7100-BROWSE-BWRD-NEXT-ACCOUNT.
02146      MOVE 'N'                    TO WS-REPORT-FOUND-SW.
02147
02148      MOVE SPACES                 TO ELCNTL-KEY.
02149      MOVE PI-COMPANY-ID          TO ELCNTL-COMPANY-ID.
02150      MOVE 'C'                    TO ELCNTL-RECORD-TYPE.
02151      MOVE +0                     TO ELCNTL-SEQ-NO.
02152
02153      IF REPORTL GREATER THAN +0
02154         MOVE REPORTI            TO ELCNTL-REPORT
02155      ELSE
02156         MOVE PI-LAST-REPORT     TO ELCNTL-REPORT.
02157
02158      
      * EXEC CICS HANDLE CONDITION
02159 *        NOTFND   (7180-END-OF-SEARCH)
02160 *    END-EXEC.
      *    MOVE '"$I                   ! / #00005267' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2F20233030303035323637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02161
02162      
      * EXEC CICS STARTBR
02163 *        DATASET  (ELCNTL-FILE-ID)
02164 *        RIDFLD   (ELCNTL-KEY)
02165 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005271' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02166
02167      
      * EXEC CICS HANDLE CONDITION
02168 *        NOTFND   (7170-END-OF-BROWSE)
02169 *        ENDFILE  (7170-END-OF-BROWSE)
02170 *    END-EXEC.
      *    MOVE '"$I''                  ! 0 #00005276' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3020233030303035323736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02171
02172  7110-READ-FWRD-NEXT-RECORD.
02173      
      * EXEC CICS READNEXT
02174 *        DATASET  (ELCNTL-FILE-ID)
02175 *        RIDFLD   (ELCNTL-KEY)
02176 *        SET      (ADDRESS OF CONTROL-FILE)
02177 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005282' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02178
02179      
      * EXEC CICS READPREV
02180 *        DATASET  (ELCNTL-FILE-ID)
02181 *        RIDFLD   (ELCNTL-KEY)
02182 *        SET      (ADDRESS OF CONTROL-FILE)
02183 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00005288' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02184
02185  7110-READ-BWRD-NEXT-RECORD.
02186      
      * EXEC CICS READPREV
02187 *        DATASET  (ELCNTL-FILE-ID)
02188 *        RIDFLD   (ELCNTL-KEY)
02189 *        SET      (ADDRESS OF CONTROL-FILE)
02190 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00005295' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02191
02192      IF ELCNTL-COMPANY-ID = PI-COMPANY-ID AND
02193         ELCNTL-RECORD-TYPE = 'C'
02194         NEXT SENTENCE
02195      ELSE
02196         GO TO 7170-END-OF-BROWSE.
02197
02198      IF ELCNTL-SEQ-NO = +0
02199          NEXT SENTENCE
02200      ELSE
02201          GO TO 7110-READ-BWRD-NEXT-RECORD.
02202
02203      MOVE 'Y'                    TO WS-REPORT-FOUND-SW.
02204
02205  7170-END-OF-BROWSE.
02206      
      * EXEC CICS ENDBR
02207 *        DATASET  (ELCNTL-FILE-ID)
02208 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005315' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02209
02210      IF REPORT-WAS-NOT-FOUND
02211         NEXT SENTENCE
02212      ELSE
02213         GO TO 7190-EXIT.
02214
02215  7180-END-OF-SEARCH.
02216      MOVE -1                     TO MAINTL.
02217      MOVE  ER-2238               TO EMI-ERROR.
02218      PERFORM 9900-ERROR-FORMAT.
02219      GO TO 8200-SEND-DATAONLY.
02220
02221   7190-EXIT.
02222      EXIT.
02223
02224      EJECT
02225  8100-SEND-INITIAL-MAP.
02226      MOVE SAVE-DATE              TO RUNDTEO.
02227      MOVE EIBTIME                TO TIME-IN.
02228      MOVE TIME-OUT               TO RUNTIMEO.
02229      MOVE SPACES                 TO MAINTO.
02230      MOVE -1                     TO MAINTL.
02231      MOVE PI-LIFE-OVERRIDE-L2    TO LBENTPO.
02232      MOVE PI-AH-OVERRIDE-L2      TO ABENTPO.
02233      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.
02234      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.
02235
02236  8150-SEND-INITIAL-MAP.
02237      
      * EXEC CICS SEND
02238 *        MAP      (MAP-NAME)
02239 *        MAPSET   (MAPSET-NAME)
02240 *        FROM     (EL604AO)
02241 *        ERASE
02242 *        CURSOR
02243 *    END-EXEC.
           MOVE LENGTH OF
            EL604AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00005346' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL604AO, 
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
           
02244
02245      GO TO 9100-RETURN-TRAN.
02246
02247  8200-SEND-DATAONLY.
02248      MOVE SAVE-DATE              TO RUNDTEO
02249      MOVE EIBTIME                TO TIME-IN.
02250      MOVE TIME-OUT               TO RUNTIMEO
02251      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSG1O.
02252      MOVE EMI-MESSAGE-AREA (2)   TO ERRMSG2O.
02253      
      * EXEC CICS SEND
02254 *        MAP      (MAP-NAME)
02255 *        MAPSET   (MAPSET-NAME)
02256 *        FROM     (EL604AO)
02257 *        DATAONLY
02258 *        CURSOR
02259 *    END-EXEC.
           MOVE LENGTH OF
            EL604AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00005362' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL604AO, 
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
           
02260
02261      GO TO 9100-RETURN-TRAN.
02262
02263      EJECT
02264  8300-SEND-TEXT.
02265      
      * EXEC CICS SEND TEXT
02266 *        FROM     (LOGOFF-TEXT)
02267 *        LENGTH   (LOGOFF-LENGTH)
02268 *        ERASE
02269 *        FREEKB
02270 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00005374' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333734' TO DFHEIV0(25:11)
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
           
02271
02272      
      * EXEC CICS RETURN
02273 *    END-EXEC.
      *    MOVE '.(                    &   #00005381' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02274
02275      EJECT
02276  8400-LOG-JOURNAL-RECORD.
02277      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.
02278      MOVE ELCNTL-FILE-ID         TO JP-FILE-ID.
02279      MOVE THIS-PGM               TO JP-PROGRAM-ID.
pemuni*    EXEC CICS JOURNAL
pemuni*        JFILEID     (PI-JOURNAL-FILE-ID)
pemuni*        JTYPEID     ('CR')
pemuni*        FROM        (JOURNAL-RECORD)
pemuni*        LENGTH      (WS-JOURNAL-RECORD-LENGTH)
pemuni*    END-EXEC.
02286
02287  8400-EXIT.
02288      EXIT.
02289
02290  8600-DEEDIT.
02291      
      * EXEC CICS BIF DEEDIT
02292 *         FIELD   (DEEDIT-FIELD)
02293 *         LENGTH  (15)
02294 *     END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005400' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02295
02296  8800-UNAUTHORIZED-ACCESS.
02297      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
02298      GO TO 8300-SEND-TEXT.
02299
02300  8810-PF23.
02301      MOVE EIBAID                 TO PI-ENTRY-CD-1.
02302      MOVE XCTL-005               TO PGM-NAME.
02303      GO TO 9300-XCTL.
02304
02305  8900-PF03.
02306      MOVE XCTL-EL6041            TO PGM-NAME.
02307      GO TO 9300-XCTL.
02308
02309  9000-RETURN-CICS.
02310      
      * EXEC CICS RETURN
02311 *    END-EXEC.
      *    MOVE '.(                    &   #00005419' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02312
02313  9100-RETURN-TRAN.
02314      MOVE    EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
02315      MOVE SCREEN-NUMBER             TO PI-CURRENT-SCREEN-NO.
02316      
      * EXEC CICS RETURN
02317 *        TRANSID    (TRANS-ID)
02318 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
02319 *        LENGTH     (PI-COMM-LENGTH)
02320 *    END-EXEC.
      *    MOVE '.(CT                  &   #00005425' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02321
02322  9200-RETURN-MAIN-MENU.
02323
02324      IF  CREDIT-SESSION
02325          MOVE XCTL-EL626         TO PGM-NAME
02326
02327      ELSE
02328          IF  CLAIM-SESSION
02329              MOVE XCTL-EL126     TO PGM-NAME
02330
02331          ELSE
02332              IF  MORTGAGE-SESSION
02333                  MOVE XCTL-EM626 TO PGM-NAME
02334
02335              ELSE
02336                  IF  GENERAL-LEDGER-SESSION
02337                      MOVE XCTL-GL800
02338                                  TO PGM-NAME.
02339
02340      GO TO 9300-XCTL.
02341
02342  9300-XCTL.
02343      
      * EXEC CICS XCTL
02344 *        PROGRAM    (PGM-NAME)
02345 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
02346 *        LENGTH     (PI-COMM-LENGTH)
02347 *    END-EXEC.
      *    MOVE '.$C                   $   #00005452' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02348
02349  9400-CLEAR.
02350      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.
02351      GO TO 9300-XCTL.
02352
02353  9500-PF12.
02354      MOVE XCTL-010               TO PGM-NAME.
02355      GO TO 9300-XCTL.
02356
02357  9600-PGMID-ERROR.
02358      
      * EXEC CICS HANDLE CONDITION
02359 *        PGMIDERR    (8300-SEND-TEXT)
02360 *    END-EXEC.
      *    MOVE '"$L                   ! 1 #00005467' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3120233030303035343637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02361
02362      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
02363      MOVE ' '                    TO PI-ENTRY-CD-1.
02364      MOVE XCTL-005               TO PGM-NAME.
02365      MOVE PGM-NAME               TO LOGOFF-PGM.
02366      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
02367      GO TO 9300-XCTL.
02368
02369  9700-DATE-LINK.
02370      MOVE LINK-ELDATCV           TO PGM-NAME
02371      
      * EXEC CICS LINK
02372 *        PROGRAM    (PGM-NAME)
02373 *        COMMAREA   (DATE-CONVERSION-DATA)
02374 *        LENGTH     (DC-COMM-LENGTH)
02375 *    END-EXEC.
      *    MOVE '."C                   ''   #00005480' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02376
02377  9900-ERROR-FORMAT.
02378      IF NOT EMI-ERRORS-COMPLETE
02379          MOVE LINK-001           TO PGM-NAME
02380          
      * EXEC CICS LINK
02381 *            PROGRAM    (PGM-NAME)
02382 *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
02383 *            LENGTH     (EMI-COMM-LENGTH)
02384 *        END-EXEC.
      *    MOVE '."C                   ''   #00005489' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02385
02386  9900-EXIT.
02387      EXIT.
02388
02389  9990-ABEND.
02390      MOVE LINK-004               TO PGM-NAME.
02391      MOVE DFHEIBLK               TO EMI-LINE1
02392
02393      
      * EXEC CICS LINK
02394 *        PROGRAM   (PGM-NAME)
02395 *        COMMAREA  (EMI-LINE1)
02396 *        LENGTH    (72)
02397 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00005502' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02398
02399      MOVE -1                     TO MAINTL.
02400      GO TO 8200-SEND-DATAONLY.
02401
02402      EJECT
092308 9910-INITIALIZE-SECURITY.
      ******************************************************************
      *                                                                *
      *       THIS LOGIC SETS UP AND READS THE TEMPORARY STORAGE       *
      *       USER SECURITY RECORD SET UP BY EL125.  THIS PROGRAM      *
      *       MOVES THE APPROPRIATE APPLICATION AUTHORIZATION INTO     *
      *       PI-DISPLAY-CAP AND PI-MODIFY-CAP.  IT THEN CHECKS FOR    *
      *       BROWSE AUTHORITY AND IF NOT AUTHORIZED WILL SET AN       *
      *       ERROR CONDITION AND EXITS THE PROGRAM.                   *
      *                                                                *
      ******************************************************************
           IF  PI-PROCESSOR-ID NOT = 'LGXX'
               IF  MORTGAGE-SESSION
                   MOVE '125E'             TO SC-QUID-SYSTEM
                   MOVE EIBTRMID           TO SC-QUID-TERMINAL
                   
      * EXEC CICS READQ TS
      *                QUEUE  (SC-QUID-KEY)
      *                INTO   (SECURITY-CONTROL-E)
      *                LENGTH (SC-COMM-LENGTH-E)
      *                ITEM   (SC-ITEM)
      *            END-EXEC
      *    MOVE '*$II   L              ''   #00005527' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 SC-QUID-KEY, 
                 SECURITY-CONTROL-E, 
                 SC-COMM-LENGTH-E, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                   MOVE SC-MP-DISPLAY (W-APPL-SCRTY-NDX)
                                           TO PI-DISPLAY-CAP
                   MOVE SC-MP-UPDATE (W-APPL-SCRTY-NDX)
                                           TO PI-MODIFY-CAP
                   IF  NOT DISPLAY-CAP
                       MOVE 'READ'         TO SM-READ
                       PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
                       MOVE ER-9097        TO EMI-ERROR
                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                       GO TO 8100-SEND-INITIAL-MAP
                   ELSE
                       GO TO 9910-EXIT
               ELSE
                   
      * EXEC CICS  READQ TS
      *                QUEUE   (PI-SECURITY-TEMP-STORE-ID)
      *                INTO    (SECURITY-CONTROL)
      *                LENGTH  (SC-COMM-LENGTH)
      *                ITEM    (SC-ITEM-CL-CR)
      *                END-EXEC
      *    MOVE '*$II   L              ''   #00005546' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM-CL-CR, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
                   MOVE SC-CREDIT-DISPLAY (30)
                                       TO PI-DISPLAY-CAP
                   MOVE SC-CREDIT-UPDATE  (30)
                                       TO PI-MODIFY-CAP
                   IF  NOT DISPLAY-CAP
                       MOVE 'READ'     TO SM-READ
                       PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
                       MOVE ER-0070    TO  EMI-ERROR
                       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                       GO TO 8100-SEND-INITIAL-MAP.
092308 9910-EXIT.
           EXIT.
02432  9995-SECURITY-VIOLATION.
02433 *           COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00005582' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 SECURITY-MESSAGE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00021
00022 ******************************************************************
00023
02434
02435  9995-EXIT.
02436       EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL604' TO DFHEIV1
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
               GO TO 1100-EDIT-REPORT-SCREEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 2100-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 2890-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 3100-CONTINUE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 3900-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 4100-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 5010-NOT-FOUND,
                     5010-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 5030-NOT-FOUND,
                     5030-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 5050-NOT-FOUND,
                     5050-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 5070-NOT-FOUND,
                     5070-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 7080-END-OF-SEARCH
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 7070-END-OF-BROWSE,
                     7070-END-OF-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 7180-END-OF-SEARCH
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 7170-END-OF-BROWSE,
                     7170-END-OF-BROWSE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL604' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
