00001  ID DIVISION.
00002
00003  PROGRAM-ID.                 EL6504.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 08:07:16.
00007 *                            VMOD=2.011
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
00024 *REMARKS.    TRANSACTION - EXC8 - ACCOUNT MAINT (REIN & COMM).
00023 *
101101******************************************************************
101101*                   C H A N G E   L O G
101101*
101101* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101101*-----------------------------------------------------------------
101101*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101101* EFFECTIVE    NUMBER
101101*-----------------------------------------------------------------
101101* 101101    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
101101******************************************************************
00025
00026  ENVIRONMENT DIVISION.
00027
00028      EJECT
00029  DATA DIVISION.
00030  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00031  77  FILLER  PIC X(32)  VALUE '********************************'.
00032  77  FILLER  PIC X(32)  VALUE '*    EL6504 WORKING STORAGE    *'.
00033  77  FILLER  PIC X(32)  VALUE '************ V/M 2.011 *********'.
00034
00035  01  WS-DATE-AREA.
00036      05  SAVE-DATE                   PIC X(8)    VALUE SPACES.
00037      05  SAVE-BIN-DATE               PIC XX      VALUE SPACES.
00038
00039  01  STANDARD-AREAS.
PEMMOD     12  WS-YYYYMMDD             PIC X(8).
PEMMOD     12  WS-COMP-DATE REDEFINES WS-YYYYMMDD
PEMMOD                                 PIC 9(8).
00040      12  WS-COMM-LENGTH              PIC S9(4) COMP VALUE +1500.
00041      12  MAP-NAME                    PIC X(8)    VALUE 'EL6504A'.
00042      12  MAPSET-NAME                 PIC X(8)    VALUE 'EL6504S'.
00043      12  SCREEN-NUMBER               PIC X(4)    VALUE '650E'.
00044      12  TRANS-ID                    PIC X(4)    VALUE 'EXC8'.
00045      12  THIS-PGM                    PIC X(8)    VALUE 'EL6504'.
00046      12  PGM-NAME                    PIC X(8)    VALUE SPACES.
00047      12  TIME-IN                     PIC S9(7)   VALUE ZEROS.
00048      12  TIME-OUT-R  REDEFINES TIME-IN.
00049          16  FILLER                  PIC X.
00050          16  TIME-OUT                PIC 99V99.
00051          16  FILLER                  PIC XX.
00052      12  XCTL-005                    PIC X(8)    VALUE 'EL005'.
00053      12  XCTL-010                    PIC X(8)    VALUE 'EL010'.
00054      12  XCTL-626                    PIC X(8)    VALUE 'EL626'.
00055      12  XCTL-650                    PIC X(8)    VALUE 'EL650'.
00056      12  XCTL-6501                   PIC X(8)    VALUE 'EL6501'.
00057      12  XCTL-6502                   PIC X(8)    VALUE 'EL6502'.
00058      12  XCTL-6503                   PIC X(8)    VALUE 'EL6503'.
00059      12  XCTL-6505                   PIC X(8)    VALUE 'EL6505'.
00060      12  XCTL-6506                   PIC X(8)    VALUE 'EL6506'.
00061      12  LINK-001                    PIC X(8)    VALUE 'EL001'.
00062      12  LINK-004                    PIC X(8)    VALUE 'EL004'.
00063      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'.
00064      12  FILE-ID                     PIC X(8)    VALUE SPACES.
00065      12  ERACCT-FILE                 PIC X(8)    VALUE 'ERACCT'.
00066      12  CNTL-FILE-ID                PIC X(8)    VALUE 'ELCNTL'.
00067      12  BIN-CURRENT-SAVE            PIC XX      VALUE SPACES.
00068      12  YMD-CURRENT-SAVE            PIC X(6)    VALUE SPACES.
00069
00070      12  ERACCT-LENGTH               PIC S9(4)   VALUE +2023 COMP.
00071      12  SC-ITEM                     PIC S9(4)   VALUE +1    COMP.
00072      12  ELCNTL-LENGTH               PIC S9(4)   VALUE +527  COMP.
00073      12  WS-JOURNAL-FILE-LENGTH      PIC S9(4)   VALUE +0    COMP.
00074      12  SUB1                        PIC S9(4)   VALUE +0    COMP.
00075      12  SUB2                        PIC S9(4)   VALUE +0    COMP.
00076
00077      12  DEEDIT-FIELD                PIC X(15).
00078      12  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD PIC S9(15).
00079      12  DEEDIT-FIELD-V3  REDEFINES DEEDIT-FIELD PIC S9(12)V999.
00080      12  DEEDIT-FIELD-V4  REDEFINES DEEDIT-FIELD PIC S9(11)V9999.
00081
00082      12  WS-EDIT-FIELD-CONV          PIC S9(4)   VALUE +0.
00083      12  WS-TOT-PERCENT              PIC S9V9999 VALUE +0.
00084
00085      12  WS-VALID-EARNINGS           PIC X       VALUE SPACE.
00086          88  VALID-EARNINGS                      VALUE ' ' 'R' 'P'
00087                                                       'A' 'C' 'M'.
00088
00089      12  WS-LF-FEE                   PIC S9V9999 VALUE ZEROS.
00090      12  WS-LF-TAX                   PIC S9V9999 VALUE ZEROS.
00091      12  WS-AH-FEE                   PIC S9V9999 VALUE ZEROS.
00092      12  WS-AH-TAX                   PIC S9V9999 VALUE ZEROS.
00093      12  WS-AH-R78                   PIC S9V9999 VALUE ZEROS.
00094      12  WS-AH-PRATA                 PIC S9V9999 VALUE ZEROS.
00095      12  WS-LF-RET                   PIC S9V9999 VALUE ZEROS.
00096      12  WS-AH-RET                   PIC S9V9999 VALUE ZEROS.
00097      12  WS-LF-LOSS                  PIC SV999   VALUE ZEROS.
00098      12  WS-AH-LOSS                  PIC SV999   VALUE ZEROS.
00099      12  WS-LF-PCT-1                 PIC S9V9999 VALUE ZEROS.
00100      12  WS-LF-PCT-2                 PIC S9V9999 VALUE ZEROS.
00101      12  WS-LF-PCT-3                 PIC S9V9999 VALUE ZEROS.
00102      12  WS-AH-PCT-1                 PIC S9V9999 VALUE ZEROS.
00103      12  WS-AH-PCT-2                 PIC S9V9999 VALUE ZEROS.
00104      12  WS-AH-PCT-3                 PIC S9V9999 VALUE ZEROS.
00105      12  WS-LF-THRU-1                PIC S9(7)   VALUE ZEROS.
00106      12  WS-LF-THRU-2                PIC S9(7)   VALUE ZEROS.
00107      12  WS-LF-THRU-3                PIC S9(7)   VALUE ZEROS.
00108      12  WS-AH-THRU-1                PIC S9(7)   VALUE ZEROS.
00109      12  WS-AH-THRU-2                PIC S9(7)   VALUE ZEROS.
00110      12  WS-AH-THRU-3                PIC S9(7)   VALUE ZEROS.
00111      12  WS-QUALIFY-LIMIT            PIC S9(7)   VALUE ZEROS.
00112
00113      EJECT
00114      12  ERROR-MESSAGES.
00115          16  ER-0000                 PIC X(4)    VALUE '0000'.
00116          16  ER-0002                 PIC X(4)    VALUE '0002'.
00117          16  ER-0004                 PIC X(4)    VALUE '0004'.
00118          16  ER-0008                 PIC X(4)    VALUE '0008'.
00119          16  ER-0029                 PIC X(4)    VALUE '0029'.
00120          16  ER-0068                 PIC X(4)    VALUE '0068'.
00121          16  ER-0070                 PIC X(4)    VALUE '0070'.
00122          16  ER-0620                 PIC X(4)    VALUE '0620'.
00123          16  ER-0621                 PIC X(4)    VALUE '0621'.
00124          16  ER-0626                 PIC X(4)    VALUE '0626'.
00125          16  ER-2039                 PIC X(4)    VALUE '2039'.
00126          16  ER-2081                 PIC X(4)    VALUE '2081'.
00127          16  ER-2082                 PIC X(4)    VALUE '2082'.
00128          16  ER-2083                 PIC X(4)    VALUE '2083'.
00129          16  ER-2084                 PIC X(4)    VALUE '2084'.
00130          16  ER-2085                 PIC X(4)    VALUE '2085'.
00131          16  ER-2086                 PIC X(4)    VALUE '2086'.
00132          16  ER-2098                 PIC X(4)    VALUE '2098'.
00133          16  ER-2099                 PIC X(4)    VALUE '2099'.
00134          16  ER-2100                 PIC X(4)    VALUE '2100'.
00135          16  ER-2101                 PIC X(4)    VALUE '2101'.
00136          16  ER-2102                 PIC X(4)    VALUE '2102'.
00137          16  ER-2103                 PIC X(4)    VALUE '2103'.
00138          16  ER-2161                 PIC X(4)    VALUE '2161'.
00139          16  ER-2162                 PIC X(4)    VALUE '2162'.
00140          16  ER-2163                 PIC X(4)    VALUE '2163'.
00141          16  ER-2164                 PIC X(4)    VALUE '2164'.
00142          16  ER-2165                 PIC X(4)    VALUE '2165'.
00143          16  ER-2171                 PIC X(4)    VALUE '2171'.
00144          16  ER-2572                 PIC X(4)    VALUE '2572'.
00145
00146      12  ELCNTL-KEY.
00147          16  CNTL-COMP-ID            PIC X(3)    VALUE SPACES.
00148          16  CNTL-REC-TYPE           PIC X       VALUE SPACES.
00149          16  CNTL-ACCESS             PIC X(4)    VALUE SPACES.
00150          16  CNTL-SEQ-NO             PIC S9(4)   VALUE +0  COMP.
00151
00152      EJECT
00153 *                          COPY ELCSCTM.
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
00154      EJECT
00155 *                          COPY ELCSCRTY.
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
00156      EJECT
00157 *                          COPY ELCLOGOF.
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
00158
00159      EJECT
00160 *                          COPY ELCDATE.
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
00161
00162      EJECT
00163 *                          COPY ELCATTR.
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
00164
00165      EJECT
00166 *                          COPY ELCEMIB.
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
00167
00168      EJECT
00169 *                          COPY ELCINTF.
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
00170 *                          COPY ELC650PI.
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
00171
00172 *                          COPY ELCJPFX.
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
00173                                      PIC X(2000).
00174
00175      EJECT
00176 *                          COPY ELCAID.
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
00177  01  FILLER    REDEFINES DFHAID.
00178      12  FILLER                      PIC X(8).
00179      12  PF-VALUES                   PIC X       OCCURS 2.
00180
00181      EJECT
00182 *                          COPY EL6504S.
       01  EL6504AI.
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
           05  LFREIHDL PIC S9(0004) COMP.
           05  LFREIHDF PIC  X(0001).
           05  FILLER REDEFINES LFREIHDF.
               10  LFREIHDA PIC  X(0001).
           05  LFREIHDI PIC  X(0006).
      *    -------------------------------
           05  LCFEEL PIC S9(0004) COMP.
           05  LCFEEF PIC  X(0001).
           05  FILLER REDEFINES LCFEEF.
               10  LCFEEA PIC  X(0001).
           05  LCFEEI PIC  X(0006).
      *    -------------------------------
           05  LCBASISL PIC S9(0004) COMP.
           05  LCBASISF PIC  X(0001).
           05  FILLER REDEFINES LCBASISF.
               10  LCBASISA PIC  X(0001).
           05  LCBASISI PIC  X(0001).
      *    -------------------------------
           05  LTAXL PIC S9(0004) COMP.
           05  LTAXF PIC  X(0001).
           05  FILLER REDEFINES LTAXF.
               10  LTAXA PIC  X(0001).
           05  LTAXI PIC  X(0006).
      *    -------------------------------
           05  TAXOPTL PIC S9(0004) COMP.
           05  TAXOPTF PIC  X(0001).
           05  FILLER REDEFINES TAXOPTF.
               10  TAXOPTA PIC  X(0001).
           05  TAXOPTI PIC  X(0001).
      *    -------------------------------
           05  PRTOPTL PIC S9(0004) COMP.
           05  PRTOPTF PIC  X(0001).
           05  FILLER REDEFINES PRTOPTF.
               10  PRTOPTA PIC  X(0001).
           05  PRTOPTI PIC  X(0001).
      *    -------------------------------
           05  GROUPAL PIC S9(0004) COMP.
           05  GROUPAF PIC  X(0001).
           05  FILLER REDEFINES GROUPAF.
               10  GROUPAA PIC  X(0001).
           05  GROUPAI PIC  X(0006).
      *    -------------------------------
           05  GROUPBL PIC S9(0004) COMP.
           05  GROUPBF PIC  X(0001).
           05  FILLER REDEFINES GROUPBF.
               10  GROUPBA PIC  X(0001).
           05  GROUPBI PIC  X(0006).
      *    -------------------------------
           05  MORTALL PIC S9(0004) COMP.
           05  MORTALF PIC  X(0001).
           05  FILLER REDEFINES MORTALF.
               10  MORTALA PIC  X(0001).
           05  MORTALI PIC  X(0004).
      *    -------------------------------
           05  AHREIHDL PIC S9(0004) COMP.
           05  AHREIHDF PIC  X(0001).
           05  FILLER REDEFINES AHREIHDF.
               10  AHREIHDA PIC  X(0001).
           05  AHREIHDI PIC  X(0006).
      *    -------------------------------
           05  ACFEEL PIC S9(0004) COMP.
           05  ACFEEF PIC  X(0001).
           05  FILLER REDEFINES ACFEEF.
               10  ACFEEA PIC  X(0001).
           05  ACFEEI PIC  X(0006).
      *    -------------------------------
           05  ACBASISL PIC S9(0004) COMP.
           05  ACBASISF PIC  X(0001).
           05  FILLER REDEFINES ACBASISF.
               10  ACBASISA PIC  X(0001).
           05  ACBASISI PIC  X(0001).
      *    -------------------------------
           05  ATAXL PIC S9(0004) COMP.
           05  ATAXF PIC  X(0001).
           05  FILLER REDEFINES ATAXF.
               10  ATAXA PIC  X(0001).
           05  ATAXI PIC  X(0006).
      *    -------------------------------
           05  ARULEL PIC S9(0004) COMP.
           05  ARULEF PIC  X(0001).
           05  FILLER REDEFINES ARULEF.
               10  ARULEA PIC  X(0001).
           05  ARULEI PIC  X(0006).
      *    -------------------------------
           05  ARATAL PIC S9(0004) COMP.
           05  ARATAF PIC  X(0001).
           05  FILLER REDEFINES ARATAF.
               10  ARATAA PIC  X(0001).
           05  ARATAI PIC  X(0006).
      *    -------------------------------
           05  LRETHD1L PIC S9(0004) COMP.
           05  LRETHD1F PIC  X(0001).
           05  FILLER REDEFINES LRETHD1F.
               10  LRETHD1A PIC  X(0001).
           05  LRETHD1I PIC  X(0002).
      *    -------------------------------
           05  ARETHD1L PIC S9(0004) COMP.
           05  ARETHD1F PIC  X(0001).
           05  FILLER REDEFINES ARETHD1F.
               10  ARETHD1A PIC  X(0001).
           05  ARETHD1I PIC  X(0002).
      *    -------------------------------
           05  ARETHD2L PIC S9(0004) COMP.
           05  ARETHD2F PIC  X(0001).
           05  FILLER REDEFINES ARETHD2F.
               10  ARETHD2A PIC  X(0001).
           05  ARETHD2I PIC  X(0002).
      *    -------------------------------
           05  ARETHD3L PIC S9(0004) COMP.
           05  ARETHD3F PIC  X(0001).
           05  FILLER REDEFINES ARETHD3F.
               10  ARETHD3A PIC  X(0001).
           05  ARETHD3I PIC  X(0002).
      *    -------------------------------
           05  YNRETROL PIC S9(0004) COMP.
           05  YNRETROF PIC  X(0001).
           05  FILLER REDEFINES YNRETROF.
               10  YNRETROA PIC  X(0001).
           05  YNRETROI PIC  X(0001).
      *    -------------------------------
           05  PERETROL PIC S9(0004) COMP.
           05  PERETROF PIC  X(0001).
           05  FILLER REDEFINES PERETROF.
               10  PERETROA PIC  X(0001).
           05  PERETROI PIC  X(0001).
      *    -------------------------------
           05  PECOMML PIC S9(0004) COMP.
           05  PECOMMF PIC  X(0001).
           05  FILLER REDEFINES PECOMMF.
               10  PECOMMA PIC  X(0001).
           05  PECOMMI PIC  X(0001).
      *    -------------------------------
           05  PICLAIML PIC S9(0004) COMP.
           05  PICLAIMF PIC  X(0001).
           05  FILLER REDEFINES PICLAIMF.
               10  PICLAIMA PIC  X(0001).
           05  PICLAIMI PIC  X(0001).
      *    -------------------------------
           05  GPRETROL PIC S9(0004) COMP.
           05  GPRETROF PIC  X(0001).
           05  FILLER REDEFINES GPRETROF.
               10  GPRETROA PIC  X(0001).
           05  GPRETROI PIC  X(0006).
      *    -------------------------------
           05  RELIMITL PIC S9(0004) COMP.
           05  RELIMITF PIC  X(0001).
           05  FILLER REDEFINES RELIMITF.
               10  RELIMITA PIC  X(0001).
           05  RELIMITI PIC  X(0007).
      *    -------------------------------
           05  LFLOSSL PIC S9(0004) COMP.
           05  LFLOSSF PIC  X(0001).
           05  FILLER REDEFINES LFLOSSF.
               10  LFLOSSA PIC  X(0001).
           05  LFLOSSI PIC  X(0004).
      *    -------------------------------
           05  AHLOSSL PIC S9(0004) COMP.
           05  AHLOSSF PIC  X(0001).
           05  FILLER REDEFINES AHLOSSF.
               10  AHLOSSA PIC  X(0001).
           05  AHLOSSI PIC  X(0004).
      *    -------------------------------
           05  REDEARNL PIC S9(0004) COMP.
           05  REDEARNF PIC  X(0001).
           05  FILLER REDEFINES REDEARNF.
               10  REDEARNA PIC  X(0001).
           05  REDEARNI PIC  X(0001).
      *    -------------------------------
           05  LEVEARNL PIC S9(0004) COMP.
           05  LEVEARNF PIC  X(0001).
           05  FILLER REDEFINES LEVEARNF.
               10  LEVEARNA PIC  X(0001).
           05  LEVEARNI PIC  X(0001).
      *    -------------------------------
           05  AHEARNL PIC S9(0004) COMP.
           05  AHEARNF PIC  X(0001).
           05  FILLER REDEFINES AHEARNF.
               10  AHEARNA PIC  X(0001).
           05  AHEARNI PIC  X(0001).
      *    -------------------------------
           05  BEGREDL PIC S9(0004) COMP.
           05  BEGREDF PIC  X(0001).
           05  FILLER REDEFINES BEGREDF.
               10  BEGREDA PIC  X(0001).
           05  BEGREDI PIC  X(0001).
      *    -------------------------------
           05  BEGLEVL PIC S9(0004) COMP.
           05  BEGLEVF PIC  X(0001).
           05  FILLER REDEFINES BEGLEVF.
               10  BEGLEVA PIC  X(0001).
           05  BEGLEVI PIC  X(0001).
      *    -------------------------------
           05  BEGAHL PIC S9(0004) COMP.
           05  BEGAHF PIC  X(0001).
           05  FILLER REDEFINES BEGAHF.
               10  BEGAHA PIC  X(0001).
           05  BEGAHI PIC  X(0001).
      *    -------------------------------
           05  TAXOPL PIC S9(0004) COMP.
           05  TAXOPF PIC  X(0001).
           05  FILLER REDEFINES TAXOPF.
               10  TAXOPA PIC  X(0001).
           05  TAXOPI PIC  X(0001).
      *    -------------------------------
           05  LFRETHDL PIC S9(0004) COMP.
           05  LFRETHDF PIC  X(0001).
           05  FILLER REDEFINES LFRETHDF.
               10  LFRETHDA PIC  X(0001).
           05  LFRETHDI PIC  X(0006).
      *    -------------------------------
           05  LFRETENL PIC S9(0004) COMP.
           05  LFRETENF PIC  X(0001).
           05  FILLER REDEFINES LFRETENF.
               10  LFRETENA PIC  X(0001).
           05  LFRETENI PIC  X(0006).
      *    -------------------------------
           05  LFMETHDL PIC S9(0004) COMP.
           05  LFMETHDF PIC  X(0001).
           05  FILLER REDEFINES LFMETHDF.
               10  LFMETHDA PIC  X(0001).
           05  LFMETHDI PIC  X(0001).
      *    -------------------------------
           05  LFBASISL PIC S9(0004) COMP.
           05  LFBASISF PIC  X(0001).
           05  FILLER REDEFINES LFBASISF.
               10  LFBASISA PIC  X(0001).
           05  LFBASISI PIC  X(0001).
      *    -------------------------------
           05  LFPCT1L PIC S9(0004) COMP.
           05  LFPCT1F PIC  X(0001).
           05  FILLER REDEFINES LFPCT1F.
               10  LFPCT1A PIC  X(0001).
           05  LFPCT1I PIC  X(0006).
      *    -------------------------------
           05  LFTHRU1L PIC S9(0004) COMP.
           05  LFTHRU1F PIC  X(0001).
           05  FILLER REDEFINES LFTHRU1F.
               10  LFTHRU1A PIC  X(0001).
           05  LFTHRU1I PIC  X(0009).
      *    -------------------------------
           05  LFPCT2L PIC S9(0004) COMP.
           05  LFPCT2F PIC  X(0001).
           05  FILLER REDEFINES LFPCT2F.
               10  LFPCT2A PIC  X(0001).
           05  LFPCT2I PIC  X(0006).
      *    -------------------------------
           05  LFTHRU2L PIC S9(0004) COMP.
           05  LFTHRU2F PIC  X(0001).
           05  FILLER REDEFINES LFTHRU2F.
               10  LFTHRU2A PIC  X(0001).
           05  LFTHRU2I PIC  X(0009).
      *    -------------------------------
           05  LFPCT3L PIC S9(0004) COMP.
           05  LFPCT3F PIC  X(0001).
           05  FILLER REDEFINES LFPCT3F.
               10  LFPCT3A PIC  X(0001).
           05  LFPCT3I PIC  X(0006).
      *    -------------------------------
           05  LFTHRU3L PIC S9(0004) COMP.
           05  LFTHRU3F PIC  X(0001).
           05  FILLER REDEFINES LFTHRU3F.
               10  LFTHRU3A PIC  X(0001).
           05  LFTHRU3I PIC  X(0009).
      *    -------------------------------
           05  AHRETHDL PIC S9(0004) COMP.
           05  AHRETHDF PIC  X(0001).
           05  FILLER REDEFINES AHRETHDF.
               10  AHRETHDA PIC  X(0001).
           05  AHRETHDI PIC  X(0006).
      *    -------------------------------
           05  AHRETENL PIC S9(0004) COMP.
           05  AHRETENF PIC  X(0001).
           05  FILLER REDEFINES AHRETENF.
               10  AHRETENA PIC  X(0001).
           05  AHRETENI PIC  X(0006).
      *    -------------------------------
           05  AHMETHDL PIC S9(0004) COMP.
           05  AHMETHDF PIC  X(0001).
           05  FILLER REDEFINES AHMETHDF.
               10  AHMETHDA PIC  X(0001).
           05  AHMETHDI PIC  X(0001).
      *    -------------------------------
           05  AHBASISL PIC S9(0004) COMP.
           05  AHBASISF PIC  X(0001).
           05  FILLER REDEFINES AHBASISF.
               10  AHBASISA PIC  X(0001).
           05  AHBASISI PIC  X(0001).
      *    -------------------------------
           05  AHPCT1L PIC S9(0004) COMP.
           05  AHPCT1F PIC  X(0001).
           05  FILLER REDEFINES AHPCT1F.
               10  AHPCT1A PIC  X(0001).
           05  AHPCT1I PIC  X(0006).
      *    -------------------------------
           05  AHTHRU1L PIC S9(0004) COMP.
           05  AHTHRU1F PIC  X(0001).
           05  FILLER REDEFINES AHTHRU1F.
               10  AHTHRU1A PIC  X(0001).
           05  AHTHRU1I PIC  X(0009).
      *    -------------------------------
           05  AHPCT2L PIC S9(0004) COMP.
           05  AHPCT2F PIC  X(0001).
           05  FILLER REDEFINES AHPCT2F.
               10  AHPCT2A PIC  X(0001).
           05  AHPCT2I PIC  X(0006).
      *    -------------------------------
           05  AHTHRU2L PIC S9(0004) COMP.
           05  AHTHRU2F PIC  X(0001).
           05  FILLER REDEFINES AHTHRU2F.
               10  AHTHRU2A PIC  X(0001).
           05  AHTHRU2I PIC  X(0009).
      *    -------------------------------
           05  AHPCT3L PIC S9(0004) COMP.
           05  AHPCT3F PIC  X(0001).
           05  FILLER REDEFINES AHPCT3F.
               10  AHPCT3A PIC  X(0001).
           05  AHPCT3I PIC  X(0006).
      *    -------------------------------
           05  AHTHRU3L PIC S9(0004) COMP.
           05  AHTHRU3F PIC  X(0001).
           05  FILLER REDEFINES AHTHRU3F.
               10  AHTHRU3A PIC  X(0001).
           05  AHTHRU3I PIC  X(0009).
      *    -------------------------------
           05  COM1L PIC S9(0004) COMP.
           05  COM1F PIC  X(0001).
           05  FILLER REDEFINES COM1F.
               10  COM1A PIC  X(0001).
           05  COM1I PIC  X(0050).
      *    -------------------------------
           05  COM2L PIC S9(0004) COMP.
           05  COM2F PIC  X(0001).
           05  FILLER REDEFINES COM2F.
               10  COM2A PIC  X(0001).
           05  COM2I PIC  X(0050).
      *    -------------------------------
           05  COM3L PIC S9(0004) COMP.
           05  COM3F PIC  X(0001).
           05  FILLER REDEFINES COM3F.
               10  COM3A PIC  X(0001).
           05  COM3I PIC  X(0050).
      *    -------------------------------
           05  COM4L PIC S9(0004) COMP.
           05  COM4F PIC  X(0001).
           05  FILLER REDEFINES COM4F.
               10  COM4A PIC  X(0001).
           05  COM4I PIC  X(0050).
      *    -------------------------------
           05  COM5L PIC S9(0004) COMP.
           05  COM5F PIC  X(0001).
           05  FILLER REDEFINES COM5F.
               10  COM5A PIC  X(0001).
           05  COM5I PIC  X(0050).
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
           05  PFENTERI PIC  9(2).
       01  EL6504AO REDEFINES EL6504AI.
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
           05  LFREIHDO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCFEEO PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCBASISO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LTAXO PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TAXOPTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRTOPTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GROUPAO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GROUPBO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MORTALO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHREIHDO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACFEEO PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACBASISO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATAXO PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARULEO PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARATAO PIC  9.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LRETHD1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARETHD1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARETHD2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARETHD3O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  YNRETROO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PERETROO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PECOMMO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PICLAIMO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GPRETROO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RELIMITO PIC  ZZZZZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFLOSSO PIC  .999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHLOSSO PIC  .999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REDEARNO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LEVEARNO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHEARNO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEGREDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEGLEVO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEGAHO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TAXOPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFRETHDO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFRETENO PIC  Z.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFMETHDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFBASISO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFPCT1O PIC  Z.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFTHRU1O PIC  Z,ZZZ,ZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFPCT2O PIC  Z.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFTHRU2O PIC  Z,ZZZ,ZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFPCT3O PIC  Z.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFTHRU3O PIC  Z,ZZZ,ZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHRETHDO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHRETENO PIC  Z.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHMETHDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHBASISO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHPCT1O PIC  Z.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHTHRU1O PIC  Z,ZZZ,ZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHPCT2O PIC  Z.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHTHRU2O PIC  Z,ZZZ,ZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHPCT3O PIC  Z.9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHTHRU3O PIC  Z,ZZZ,ZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COM1O PIC  X(0050).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COM2O PIC  X(0050).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COM3O PIC  X(0050).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COM4O PIC  X(0050).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COM5O PIC  X(0050).
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
00183
00184      EJECT
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
00186  01  DFHCOMMAREA                     PIC X(1500).
00187
00188      EJECT
00189 *01 PARMLIST .
00190 *    02  FILLER                      PIC S9(8)   COMP.
00191 *    02  ERACCT-POINTER              PIC S9(8)   COMP.
00192 *    02  ELCNTL-POINTER              PIC S9(8)   COMP.
00193
00194 *                          COPY ERCACCT.
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
00195      EJECT
00196 *                          COPY ELCCNTL.
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
00197      EJECT
00198
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA ACCOUNT-MASTER
                                CONTROL-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL6504' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00200      CONTINUE.
00201
00202      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00203      MOVE '5'                    TO  DC-OPTION-CODE.
00204      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
00205      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00206      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00207      MOVE DC-GREG-DATE-1-YMD     TO  YMD-CURRENT-SAVE.
00208
00209      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00210      IF EIBCALEN = 0
00211          GO TO 8800-UNAUTHORIZED-ACCESS.
00212
00213      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00214          IF PI-CALLING-PROGRAM = XCTL-6501
00215              MOVE PI-CALLING-PROGRAM
00216                                  TO  PI-RETURN-TO-PROGRAM
00217              MOVE THIS-PGM       TO  PI-CALLING-PROGRAM
00218          ELSE
00219              MOVE THIS-PGM       TO  PI-CALLING-PROGRAM.
00220
00221      MOVE LOW-VALUES             TO  EL6504AI.
00222
00223      IF EIBTRNID NOT = TRANS-ID
00224          MOVE PI-MAINT           TO  MAINTYPO
00225          MOVE AL-UANON           TO  MAINTYPA
00226          MOVE -1                 TO  MAINTYPL
00227
00228          IF PI-MAINT = 'S' OR 'C'
00229              GO TO 4000-SHOW
00230          ELSE
00231              IF PI-MAINT = 'A'
00232                  MOVE 'C'            TO  PI-MAINT
00233                  GO TO 4000-SHOW
00234              ELSE
00235                  GO TO 8100-SEND-INITIAL-MAP.
00236
00237      
      * EXEC CICS HANDLE CONDITION
00238 *        PGMIDERR  (9600-PGMID-ERROR)
00239 *        ERROR     (9990-ABEND)
00240 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00003869' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303033383639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00241
00242      IF EIBAID = DFHCLEAR
00243          GO TO 9400-CLEAR.
00244
00245      EJECT
00246  0200-RECEIVE.
00247      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00248          MOVE ER-0008            TO  EMI-ERROR
00249          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00250          MOVE -1                 TO  PFENTERL
00251          GO TO 8200-SEND-DATAONLY.
00252
00253      
      * EXEC CICS RECEIVE
00254 *        MAP      (MAP-NAME)
00255 *        MAPSET   (MAPSET-NAME)
00256 *        INTO     (EL6504AI)
00257 *    END-EXEC.
           MOVE LENGTH OF
            EL6504AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00003885' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6504AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00258
00259      IF PFENTERL = 0
00260          GO TO 0300-CHECK-PFKEYS.
00261      IF EIBAID NOT = DFHENTER
00262          MOVE ER-0004            TO  EMI-ERROR
00263          GO TO 0320-INPUT-ERROR.
00264      IF (PFENTERI NUMERIC) AND (PFENTERI GREATER 0 AND LESS 25)
00265          MOVE PF-VALUES (PFENTERI) TO  EIBAID
00266      ELSE
00267          MOVE ER-0029            TO  EMI-ERROR
00268          GO TO 0320-INPUT-ERROR.
00269      EJECT
00270  0300-CHECK-PFKEYS.
00271      IF EIBAID = DFHPF23
00272          GO TO 8810-PF23.
00273      IF EIBAID = DFHPF24
00274          GO TO 9200-RETURN-MAIN-MENU.
00275      IF EIBAID = DFHPF12
00276          GO TO 9500-PF12.
00277
00278      IF EIBAID = DFHPF5
00279          MOVE XCTL-6502          TO  PGM-NAME
00280          GO TO 9300-XCTL.
00281
00282      IF EIBAID = DFHPF7
00283          MOVE XCTL-6501          TO  PGM-NAME
00284          GO TO 9300-XCTL.
00285
00286      IF EIBAID = DFHPF8
00287          MOVE XCTL-6506          TO  PGM-NAME
00288          GO TO 9300-XCTL.
00289
00290      IF EIBAID = DFHPF9
00291          MOVE XCTL-6505          TO  PGM-NAME
00292          GO TO 9300-XCTL.
00293
00294      IF EIBAID = DFHENTER
00295          GO TO 0330-CHECK-MAINTYP.
00296
00297      MOVE ER-0029                TO  EMI-ERROR.
00298  0320-INPUT-ERROR.
00299      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00300      MOVE AL-UNBON               TO  PFENTERA.
00301      MOVE -1                     TO  PFENTERL.
00302      GO TO 8200-SEND-DATAONLY.
00303
00304  0330-CHECK-MAINTYP.
00305      IF MAINTYPL GREATER ZERO
00306          IF MAINTYPI = 'S' OR 'C' OR 'A'
00307              MOVE AL-UANON       TO  MAINTYPA
00308              MOVE MAINTYPI       TO  PI-MAINT
00309          ELSE
00310              MOVE -1             TO  MAINTYPL
00311              MOVE AL-UABON       TO  MAINTYPA
00312              MOVE ER-2039        TO  EMI-ERROR
00313              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00314              GO TO 8200-SEND-DATAONLY
00315      ELSE
00316          MOVE -1                 TO  MAINTYPL
00317          MOVE AL-UABON           TO  MAINTYPA
00318          MOVE ER-2039            TO  EMI-ERROR
00319          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00320          GO TO 8200-SEND-DATAONLY.
00321
00322      IF PI-MAINT = 'S'
00323          GO TO 4000-SHOW.
00324
00325      PERFORM 7800-COMPANY-REC-READ THRU 7899-EXIT.
00326
00327      IF EMI-ERROR NOT = ZEROS
00328          MOVE -1                 TO  MAINTYPL
00329          GO TO 8200-SEND-DATAONLY.
00330
00331      GO TO 4200-MAINT.
00332
00333      EJECT
00334
00335  4000-SHOW.
00336      PERFORM 7100-READ-ERACCT THRU 7100-EXIT.
00337      MOVE LOW-VALUES             TO  EL6504AO.
00338      GO TO 5000-BUILD-INITIAL-SCREEN.
00339
00340      EJECT
00341
00342  4200-MAINT.
00343      IF NOT MODIFY-CAP
00344          MOVE 'UPDATE'            TO SM-READ
00345          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00346          MOVE ER-0070             TO  EMI-ERROR
00347          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00348          GO TO 8100-SEND-INITIAL-MAP.
00349
00350      PERFORM 7000-EDIT THRU 7099-EXIT.
00351
00352      IF EMI-NO-ERRORS
00353          NEXT SENTENCE
00354      ELSE
00355          GO TO 8200-SEND-DATAONLY.
00356
00357      PERFORM 7300-READ-ERACCT-UPDATE THRU 7300-EXIT.
00358
00359      MOVE ACCOUNT-MASTER         TO  JP-RECORD-AREA.
00360      MOVE ERACCT-FILE            TO  FILE-ID.
00361
00362      PERFORM 6000-CHECK-FOR-UPDATE   THRU 6049-EXIT.
00363
00364      IF AM-LAST-MAINT-USER   = PI-UPDATE-BY OR
00365         AM-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS
00366          NEXT SENTENCE
00367      ELSE
00368          
      * EXEC CICS UNLOCK
00369 *             DATASET  (ERACCT-FILE)
00370 *        END-EXEC
      *    MOVE '&*                    #   #00004000' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-FILE, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00371          MOVE ER-0068            TO  EMI-ERROR
00372          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00373          PERFORM 7100-READ-ERACCT  THRU 7100-EXIT
00374          MOVE LOW-VALUES         TO  EL6504AO
00375          MOVE -1                 TO  MAINTYPL
00376          MOVE 'S'                TO  PI-MAINT
00377          GO TO 5000-BUILD-INITIAL-SCREEN.
00378
00379      MOVE PI-PROCESSOR-ID        TO  AM-LAST-MAINT-USER.
00380      MOVE EIBTIME                TO  AM-LAST-MAINT-HHMMSS.
00381      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00382      MOVE '5'                    TO  DC-OPTION-CODE.
00383      MOVE LINK-ELDATCV           TO  PGM-NAME.
00384
00385      
      * EXEC CICS LINK
00386 *        PROGRAM (PGM-NAME)
00387 *        COMMAREA(DATE-CONVERSION-DATA)
00388 *        LENGTH  (DC-COMM-LENGTH)
00389 *    END-EXEC.
      *    MOVE '."C                   (   #00004017' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00390
00391      MOVE DC-BIN-DATE-1          TO  AM-LAST-MAINT-DT
00392                                      BIN-CURRENT-SAVE.
00393 *    MOVE YMD-CURRENT-SAVE       TO  ACC-LST-75-UPD.
00394      MOVE 'B'                    TO  JP-RECORD-TYPE.
00395      MOVE ERACCT-LENGTH          TO  WS-JOURNAL-FILE-LENGTH.
00396      PERFORM 8400-LOG-JOURNAL-RECORD.
00397      MOVE ACCOUNT-MASTER         TO  JP-RECORD-AREA.
00398
00399      
      * EXEC CICS REWRITE
00400 *        DATASET  (ERACCT-FILE)
00401 *        FROM     (ACCOUNT-MASTER)
00402 *    END-EXEC.
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004031' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-FILE, 
                 ACCOUNT-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00403
00404      MOVE 'C'                    TO  JP-RECORD-TYPE.
00405      MOVE ERACCT-FILE            TO  FILE-ID.
00406      MOVE ERACCT-LENGTH          TO  WS-JOURNAL-FILE-LENGTH.
00407      PERFORM 8400-LOG-JOURNAL-RECORD.
00408      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.
00409      MOVE ER-0000                TO  EMI-ERROR.
00410      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00411
00412      PERFORM 7100-READ-ERACCT THRU 7100-EXIT.
00413
00414      MOVE LOW-VALUES             TO  EL6504AO.
00415      MOVE 'C'                    TO  PI-MAINT.
00416
00417      EJECT
00418  5000-BUILD-INITIAL-SCREEN.
00419      MOVE AM-REI-PRT-ST          TO  TAXOPTO.
00420      MOVE AM-REI-PRT-OW          TO  PRTOPTO.
00421      MOVE AM-REI-MORT            TO  MORTALO.
00422
00423      MOVE AL-UANON               TO  TAXOPTA
00424                                      PRTOPTA
00425                                      MORTALA.
00426
00427      IF AM-REI-78-PCT NUMERIC
00428       IF AM-REI-78-PCT NOT = ZEROS
00429          MOVE AM-REI-78-PCT      TO  ARULEO
00430          MOVE AL-UNNON           TO  ARULEA.
00431
00432      IF AM-REI-PR-PCT NUMERIC
00433       IF AM-REI-PR-PCT NOT = ZEROS
00434          MOVE AM-REI-PR-PCT      TO  ARATAO
00435          MOVE AL-UNNON           TO  ARATAA.
00436
00437      IF AM-REI-FEE-LF NUMERIC
00438       IF AM-REI-FEE-LF NOT = ZEROS
00439          MOVE AM-REI-FEE-LF      TO  LCFEEO
00440          MOVE AL-UNNON           TO  LCFEEA.
00441
00442      IF AM-REI-PE-LF  NOT = SPACES
00443          MOVE AM-REI-PE-LF       TO  LCBASISO
00444          MOVE AL-UANON           TO  LCBASISA.
00445
00446      IF AM-REI-LF-TAX NUMERIC
00447       IF AM-REI-LF-TAX NOT = ZEROS
00448          MOVE AM-REI-LF-TAX      TO  LTAXO
00449          MOVE AL-UNNON           TO  LTAXA.
00450
00451      IF AM-REI-FEE-AH NUMERIC
00452       IF AM-REI-FEE-AH NOT = ZEROS
00453          MOVE AM-REI-FEE-AH      TO  ACFEEO
00454          MOVE AL-UNNON           TO  ACFEEA.
00455
00456      IF AM-REI-PE-AH  NOT = SPACES
00457          MOVE AM-REI-PE-AH       TO  ACBASISO
00458          MOVE AL-UANON           TO  ACBASISA.
00459
00460      IF AM-REI-AH-TAX NOT = ZEROS
00461          MOVE AM-REI-AH-TAX      TO  ATAXO
00462          MOVE AL-UNNON           TO  ATAXA.
00463
00464      IF AM-REI-GROUP-A  NOT = SPACES
00465          MOVE AM-REI-GROUP-A     TO  GROUPAO.
00466          MOVE AL-UANON           TO  GROUPAA.
00467
00468      IF AM-REI-GROUP-B  NOT = SPACES
00469          MOVE AM-REI-GROUP-B     TO  GROUPBO.
00470          MOVE AL-UANON           TO  GROUPBA.
00471
CIDMOD     IF AM-RETRO-PREM-P-E = ZERO
CIDMOD         MOVE SPACE              TO AM-RETRO-PREM-P-E.
CIDMOD     IF AM-RETRO-CLMS-P-I = ZERO
CIDMOD         MOVE SPACE              TO AM-RETRO-CLMS-P-I.
CIDMOD
00472      MOVE AM-RET-Y-N             TO  YNRETROO.
00473      MOVE AM-RETRO-PREM-P-E      TO  PERETROO.
00474      MOVE AM-RET-P-E             TO  PECOMMO.
00475      MOVE AM-RETRO-CLMS-P-I      TO  PICLAIMO.
00476
00477      MOVE AL-UANON               TO  YNRETROA
00478                                      PERETROA
00479                                      PECOMMA
00480                                      PICLAIMA.
00481
00482      IF AM-RETRO-POOL NOT = SPACES
00483          MOVE AM-RETRO-POOL      TO  GPRETROO
00484          MOVE AL-UANON           TO  GPRETROA.
00485
00486      IF AM-RETRO-QUALIFY-LIMIT NOT NUMERIC
00487          MOVE ZEROS              TO  AM-RETRO-QUALIFY-LIMIT.
00488
00489      IF AM-RETRO-QUALIFY-LIMIT NOT = ZEROS
00490          MOVE AM-RETRO-QUALIFY-LIMIT
00491                                  TO  RELIMITO
00492          MOVE AL-UNNON           TO  RELIMITA.
00493
00494      IF AM-RET-MIN-LOSS-L NOT NUMERIC OR
00495         AM-RET-MIN-LOSS-L = ZEROS
00496          NEXT SENTENCE
00497      ELSE
00498          MOVE AM-RET-MIN-LOSS-L  TO  LFLOSSO
00499          MOVE AL-UNNON           TO  LFLOSSA.
00500
00501      IF AM-RET-MIN-LOSS-A NOT NUMERIC OR
00502         AM-RET-MIN-LOSS-A = ZEROS
00503          NEXT SENTENCE
00504      ELSE
00505          MOVE AM-RET-MIN-LOSS-A  TO  AHLOSSO
00506          MOVE AL-UNNON           TO  AHLOSSA.
00507
00508      IF AM-RET-EARN-R NOT = SPACES
00509          MOVE AM-RET-EARN-R      TO  REDEARNO
00510          MOVE AL-UANON           TO  REDEARNA.
00511
00512      IF AM-RET-EARN-L NOT = SPACES
00513          MOVE AM-RET-EARN-L      TO  LEVEARNO
00514          MOVE AL-UANON           TO  LEVEARNA.
00515
00516      IF AM-RET-EARN-A NOT = SPACES
00517          MOVE AM-RET-EARN-A      TO  AHEARNO
00518          MOVE AL-UANON           TO  AHEARNA.
00519
00520      IF AM-RET-BEG-EARN-R NOT = SPACES
00521          MOVE AM-RET-BEG-EARN-R  TO  BEGREDO
00522          MOVE AL-UANON           TO  BEGREDA.
00523
00524      IF AM-RET-BEG-EARN-L NOT = SPACES
00525          MOVE AM-RET-BEG-EARN-L  TO  BEGLEVO
00526          MOVE AL-UANON           TO  BEGLEVA.
00527
00528      IF AM-RET-BEG-EARN-A NOT = SPACES
00529          MOVE AM-RET-BEG-EARN-A  TO  BEGAHO
00530          MOVE AL-UANON           TO  BEGAHA.
00531
00532      IF AM-RET-ST-TAX-USE NOT = SPACES
00533          MOVE AM-RET-ST-TAX-USE  TO  TAXOPO
00534          MOVE AL-UANON           TO  TAXOPA.
00535
00536      IF AM-LF-RET NOT = ZEROS
00537          MOVE AM-LF-RET          TO  LFRETENO
00538          MOVE AL-UNNON           TO  LFRETENA.
00539
00540      IF AM-AH-RET NOT = ZEROS
00541          MOVE AM-AH-RET          TO  AHRETENO
00542          MOVE AL-UNNON           TO  AHRETENA.
00543
00544 *    MOVE AM-RETRO-RET-METHOD-LF TO  LFMETHDO.
00545 *    MOVE AL-UANON               TO  LFMETHDA.
00546 *    MOVE AM-RETRO-RET-BASIS-LF  TO  LFBASISO.
00547 *    MOVE AL-UANON               TO  LFBASISA.
00548
00549      IF AM-RETRO-RET-PCT-LF (1) NOT NUMERIC
00550          MOVE ZEROS              TO  AM-RETRO-RET-PCT-LF (1).
00551      IF AM-RETRO-RET-PCT-LF (2) NOT NUMERIC
00552          MOVE ZEROS              TO  AM-RETRO-RET-PCT-LF (2).
00553      IF AM-RETRO-RET-PCT-LF (3) NOT NUMERIC
00554          MOVE ZEROS              TO  AM-RETRO-RET-PCT-LF (3).
00555      IF AM-RETRO-RET-THRU-LF (1) NOT NUMERIC
00556          MOVE ZEROS              TO  AM-RETRO-RET-THRU-LF (1).
00557      IF AM-RETRO-RET-THRU-LF (2) NOT NUMERIC
00558          MOVE ZEROS              TO  AM-RETRO-RET-THRU-LF (2).
00559      IF AM-RETRO-RET-THRU-LF (3) NOT NUMERIC
00560          MOVE ZEROS              TO  AM-RETRO-RET-THRU-LF (3).
00561
00562 *    IF AM-RETRO-RET-PCT-LF (1) NOT = ZEROS
00563 *        MOVE AM-RETRO-RET-PCT-LF (1)  TO  LFPCT1O
00564 *        MOVE AL-UNNON                 TO  LFPCT1A.
00565 *
00566 *    IF AM-RETRO-RET-THRU-LF (1) NOT = ZEROS
00567 *        MOVE AM-RETRO-RET-THRU-LF (1) TO  LFTHRU1O
00568 *        MOVE AL-UNNON                 TO  LFTHRU1A.
00569 *
00570 *    IF AM-RETRO-RET-PCT-LF (2) NOT = ZEROS
00571 *        MOVE AM-RETRO-RET-PCT-LF (2)  TO  LFPCT2O
00572 *        MOVE AL-UNNON                 TO  LFPCT2A.
00573 *
00574 *    IF AM-RETRO-RET-THRU-LF (2) NOT = ZEROS
00575 *        MOVE AM-RETRO-RET-THRU-LF (2) TO  LFTHRU2O
00576 *        MOVE AL-UNNON                 TO  LFTHRU2A.
00577 *
00578 *    IF AM-RETRO-RET-PCT-LF (3) NOT = ZEROS
00579 *        MOVE AM-RETRO-RET-PCT-LF (3)  TO  LFPCT3O
00580 *        MOVE AL-UNNON                 TO  LFPCT3A.
00581 *
00582 *    IF AM-RETRO-RET-THRU-LF (3) NOT = ZEROS
00583 *        MOVE AM-RETRO-RET-THRU-LF (3) TO  LFTHRU3O
00584 *        MOVE AL-UNNON                 TO  LFTHRU3A.
00585 *
00586 *    MOVE AM-RETRO-RET-METHOD-AH TO  AHMETHDO.
00587 *    MOVE AL-UANON               TO  AHMETHDA.
00588 *    MOVE AM-RETRO-RET-BASIS-AH  TO  AHBASISO.
00589 *    MOVE AL-UANON               TO  AHBASISA.
00590
00591      IF AM-RETRO-RET-PCT-AH (1) NOT NUMERIC
00592          MOVE ZEROS              TO  AM-RETRO-RET-PCT-AH (1).
00593      IF AM-RETRO-RET-PCT-AH (2) NOT NUMERIC
00594          MOVE ZEROS              TO  AM-RETRO-RET-PCT-AH (2).
00595      IF AM-RETRO-RET-PCT-AH (3) NOT NUMERIC
00596          MOVE ZEROS              TO  AM-RETRO-RET-PCT-AH (3).
00597      IF AM-RETRO-RET-THRU-AH (1) NOT NUMERIC
00598          MOVE ZEROS              TO  AM-RETRO-RET-THRU-AH (1).
00599      IF AM-RETRO-RET-THRU-AH (2) NOT NUMERIC
00600          MOVE ZEROS              TO  AM-RETRO-RET-THRU-AH (2).
00601      IF AM-RETRO-RET-THRU-AH (3) NOT NUMERIC
00602          MOVE ZEROS              TO  AM-RETRO-RET-THRU-AH (3).
00603
00604 *    IF AM-RETRO-RET-PCT-AH (1) NOT = ZEROS
00605 *        MOVE AM-RETRO-RET-PCT-AH (1)  TO  AHPCT1O
00606 *        MOVE AL-UNNON                 TO  AHPCT1A.
00607 *
00608 *    IF AM-RETRO-RET-THRU-AH (1) NOT = ZEROS
00609 *        MOVE AM-RETRO-RET-THRU-AH (1) TO  AHTHRU1O
00610 *        MOVE AL-UNNON                 TO  AHTHRU1A.
00611 *
00612 *    IF AM-RETRO-RET-PCT-AH (2) NOT = ZEROS
00613 *        MOVE AM-RETRO-RET-PCT-AH (2)  TO  AHPCT2O
00614 *        MOVE AL-UNNON                 TO  AHPCT2A.
00615 *
00616 *    IF AM-RETRO-RET-THRU-AH (2) NOT = ZEROS
00617 *        MOVE AM-RETRO-RET-THRU-AH (2) TO  AHTHRU2O
00618 *        MOVE AL-UNNON                 TO  AHTHRU2A.
00619 *
00620 *    IF AM-RETRO-RET-PCT-AH (3) NOT = ZEROS
00621 *        MOVE AM-RETRO-RET-PCT-AH (3)  TO  AHPCT3O
00622 *        MOVE AL-UNNON                 TO  AHPCT3A.
00623 *
00624 *    IF AM-RETRO-RET-THRU-AH (3) NOT = ZEROS
00625 *        MOVE AM-RETRO-RET-THRU-AH (3) TO  AHTHRU3O
00626 *        MOVE AL-UNNON                 TO  AHTHRU3A.
00627
00628
00629      IF AM-FLI-BANK-BALANCE    NUMERIC AND
00630         AM-FLI-BANK-1ST-6-PREM NUMERIC AND
00631         AM-FLI-BANK-CAP-AMT    NUMERIC
00632            MOVE AL-SADOF            TO  COM1A
00633                                         COM2A
00634                                         COM3A
00635                                         COM4A
00636                                         COM5A
00637          ELSE
00638            MOVE AM-COMMENT-LINE (1)  TO  COM1O
00639            MOVE AM-COMMENT-LINE (2)  TO  COM2O
00640            MOVE AM-COMMENT-LINE (3)  TO  COM3O
00641            MOVE AM-COMMENT-LINE (4)  TO  COM4O
00642            MOVE AM-COMMENT-LINE (5)  TO  COM5O
00643            MOVE AL-UANON             TO  COM1A
00644                                          COM2A
00645                                          COM3A
00646                                          COM4A
00647                                          COM5A.
00648
00649      MOVE PI-MAINT               TO  MAINTYPO.
00650      MOVE AL-UANON               TO  MAINTYPA.
00651      MOVE -1                     TO  MAINTYPL.
00652
00653      GO TO 8100-SEND-INITIAL-MAP.
00654
00655  5099-EXIT.
00656      EXIT.
00657      EJECT
00658  6000-CHECK-FOR-UPDATE.
00659      IF LCFEEL GREATER ZERO
00660          MOVE WS-LF-FEE          TO  AM-REI-FEE-LF.
00661 *        MOVE LCFEEI             TO  AM-REI-FEE-LF.
00662
00663      IF LCBASISL GREATER ZERO
00664          MOVE LCBASISI           TO  AM-REI-PE-LF.
00665
00666      IF LTAXL GREATER ZERO
00667          MOVE WS-LF-TAX          TO  AM-REI-LF-TAX.
00668 *        MOVE LTAXI              TO  AM-REI-LF-TAX.
00669
00670      IF ACFEEL GREATER ZERO
00671          MOVE WS-AH-FEE          TO  AM-REI-FEE-AH.
00672 *        MOVE ACFEEI             TO  AM-REI-FEE-AH.
00673
00674      IF ACBASISL GREATER ZERO
00675          MOVE ACBASISI           TO  AM-REI-PE-AH.
00676
00677      IF ATAXL GREATER ZERO
00678          MOVE WS-AH-TAX          TO  AM-REI-AH-TAX.
00679 *        MOVE ATAXI              TO  AM-REI-AH-TAX.
00680
00681      IF ARULEL GREATER ZERO
00682          MOVE WS-AH-R78          TO  AM-REI-78-PCT.
00683 *        MOVE ARULEI             TO  AM-REI-78-PCT.
00684
00685      IF ARATAL GREATER ZERO
00686          MOVE WS-AH-PRATA        TO  AM-REI-PR-PCT.
00687 *        MOVE ARATAI             TO  AM-REI-PR-PCT.
00688
00689      IF TAXOPTL GREATER ZERO
00690          MOVE TAXOPTI            TO  AM-REI-PRT-ST.
00691
00692      IF PRTOPTL GREATER ZERO
00693          MOVE PRTOPTI            TO  AM-REI-PRT-OW.
00694
00695      IF GROUPAL GREATER ZERO
00696          MOVE GROUPAI            TO  AM-REI-GROUP-A.
00697
00698      IF GROUPBL GREATER ZERO
00699          MOVE GROUPBI            TO  AM-REI-GROUP-B.
00700
00701      IF MORTALL GREATER ZERO
00702          MOVE MORTALI            TO  AM-REI-MORT.
00703
00704      IF YNRETROL GREATER ZERO
00705         MOVE YNRETROI            TO  AM-RET-Y-N.
00706
00707      IF PERETROL GREATER ZERO
00708         MOVE PERETROI            TO  AM-RETRO-PREM-P-E.
00709
00710      IF PECOMML GREATER ZERO
00711         MOVE PECOMMI             TO  AM-RET-P-E.
00712
00713      IF PICLAIML GREATER ZERO
00714         MOVE PICLAIMI            TO  AM-RETRO-CLMS-P-I.
00715
00716      IF GPRETROL GREATER ZERO
00717         MOVE GPRETROI            TO  AM-RETRO-POOL.
00718
00719      IF RELIMITL GREATER ZERO
00720         MOVE WS-QUALIFY-LIMIT    TO  AM-RETRO-QUALIFY-LIMIT.
00721
00722      IF LFLOSSL GREATER ZERO
00723          MOVE WS-LF-LOSS         TO  AM-RET-MIN-LOSS-L.
00724 *        MOVE LFLOSSI            TO  AM-RET-MIN-LOSS-L.
00725
00726      IF AHLOSSL GREATER ZERO
00727          MOVE WS-AH-LOSS         TO  AM-RET-MIN-LOSS-A.
00728 *        MOVE AHLOSSI            TO  AM-RET-MIN-LOSS-A.
00729
00730      IF REDEARNL GREATER ZERO
00731          MOVE REDEARNI           TO  AM-RET-EARN-R.
00732
00733      IF LEVEARNL GREATER ZERO
00734          MOVE LEVEARNI           TO  AM-RET-EARN-L.
00735
00736      IF AHEARNL GREATER ZERO
00737          MOVE AHEARNI            TO  AM-RET-EARN-A.
00738
00739      IF BEGREDL GREATER ZERO
00740          MOVE BEGREDI            TO  AM-RET-BEG-EARN-R.
00741
00742      IF BEGLEVL GREATER ZERO
00743          MOVE BEGLEVI            TO  AM-RET-BEG-EARN-L.
00744
00745      IF BEGAHL GREATER ZERO
00746          MOVE BEGAHI             TO  AM-RET-BEG-EARN-A.
00747
00748      IF TAXOPL GREATER ZERO
00749          MOVE TAXOPI             TO  AM-RET-ST-TAX-USE.
00750
00751      IF LFRETENL GREATER ZERO
00752         MOVE WS-LF-RET           TO  AM-LF-RET.
00753 *       MOVE LFRETENI            TO  AM-LF-RET.
00754
00755 *    IF LFMETHDL GREATER ZERO
00756 *        MOVE LFMETHDI           TO  AM-RETRO-RET-METHOD-LF.
00757 *
00758 *    IF LFBASISL GREATER ZERO
00759 *        MOVE LFBASISI           TO  AM-RETRO-RET-BASIS-LF.
00760 *
00761 *    IF LFPCT1L GREATER ZERO
00762 *        MOVE WS-LF-PCT-1        TO  AM-RETRO-RET-PCT-LF (1).
00763 *
00764 *    IF LFTHRU1L GREATER ZERO
00765 *        MOVE WS-LF-THRU-1       TO  AM-RETRO-RET-THRU-LF (1).
00766 *
00767 *    IF LFPCT2L GREATER ZERO
00768 *        MOVE WS-LF-PCT-2        TO  AM-RETRO-RET-PCT-LF (2).
00769 *
00770 *    IF LFTHRU2L GREATER ZERO
00771 *        MOVE WS-LF-THRU-2       TO  AM-RETRO-RET-THRU-LF (2).
00772 *
00773 *    IF LFPCT3L GREATER ZERO
00774 *        MOVE WS-LF-PCT-3        TO  AM-RETRO-RET-PCT-LF (3).
00775 *
00776 *    IF LFTHRU3L GREATER ZERO
00777 *        MOVE WS-LF-THRU-3       TO  AM-RETRO-RET-THRU-LF (3).
00778
00779      IF AHRETENL GREATER ZERO
00780         MOVE WS-AH-RET           TO  AM-AH-RET.
00781 *       MOVE AHRETENI            TO  AM-AH-RET.
00782
00783 *    IF AHMETHDL GREATER ZERO
00784 *        MOVE AHMETHDI           TO  AM-RETRO-RET-METHOD-AH.
00785 *
00786 *    IF AHBASISL GREATER ZERO
00787 *        MOVE AHBASISI           TO  AM-RETRO-RET-BASIS-AH.
00788 *
00789 *    IF AHPCT1L GREATER ZERO
00790 *        MOVE WS-AH-PCT-1        TO  AM-RETRO-RET-PCT-AH (1).
00791 *
00792 *    IF AHTHRU1L GREATER ZERO
00793 *        MOVE WS-AH-THRU-1       TO  AM-RETRO-RET-THRU-AH (1).
00794 *
00795 *    IF AHPCT2L GREATER ZERO
00796 *        MOVE WS-AH-PCT-2        TO  AM-RETRO-RET-PCT-AH (2).
00797 *
00798 *    IF AHTHRU2L GREATER ZERO
00799 *        MOVE WS-AH-THRU-2       TO  AM-RETRO-RET-THRU-AH (2).
00800 *
00801 *    IF AHPCT3L GREATER ZERO
00802 *        MOVE WS-AH-PCT-3        TO  AM-RETRO-RET-PCT-AH (3).
00803 *
00804 *    IF AHTHRU3L GREATER ZERO
00805 *        MOVE WS-AH-THRU-3       TO  AM-RETRO-RET-THRU-AH (3).
00806 *
00807      IF COM1L GREATER ZERO
00808          MOVE COM1I              TO  AM-COMMENT-LINE (1).
00809
00810      IF COM2L GREATER ZERO
00811          MOVE COM2I              TO  AM-COMMENT-LINE (2).
00812
00813      IF COM3L GREATER ZERO
00814          MOVE COM3I              TO  AM-COMMENT-LINE (3).
00815
00816      IF COM4L GREATER ZERO
00817          MOVE COM4I              TO  AM-COMMENT-LINE (4).
00818
00819      IF COM5L GREATER ZERO
00820          MOVE COM5I              TO  AM-COMMENT-LINE (5).
00821
00822
00823  6049-EXIT.
00824      EXIT.
00825      EJECT
00826  7000-EDIT.
00827      IF LCFEEL GREATER ZERO
00828          MOVE LCFEEI                 TO  DEEDIT-FIELD
00829          PERFORM 7200-DEEDIT THRU 7200-EXIT
00830          IF DEEDIT-FIELD-V4 NUMERIC
00831              MOVE DEEDIT-FIELD-V4    TO  WS-LF-FEE
00832              MOVE AL-UNNON           TO  LCFEEA
00833          ELSE
00834              MOVE -1                 TO  LCFEEL
00835              MOVE AL-UNBON           TO  LCFEEA
00836              MOVE ER-2081            TO  EMI-ERROR
00837              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00838
00839      IF LCBASISL GREATER ZERO
00840          IF LCBASISI = SPACES
00841              NEXT SENTENCE
00842          ELSE
00843              IF LCBASISI = 'P' OR 'E' OR ' '
00844                  MOVE AL-UANON       TO  LCBASISA
00845              ELSE
00846                  MOVE -1             TO  LCBASISL
00847                  MOVE AL-UABON       TO  LCBASISA
00848                  MOVE ER-2098        TO  EMI-ERROR
00849                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00850
00851      IF LTAXL GREATER ZERO
00852          MOVE LTAXI                  TO  DEEDIT-FIELD
00853          PERFORM 7200-DEEDIT THRU 7200-EXIT
00854          IF DEEDIT-FIELD-V4 NUMERIC
00855              MOVE DEEDIT-FIELD-V4    TO  WS-LF-TAX
00856              MOVE AL-UNNON           TO  LTAXA
00857          ELSE
00858              MOVE -1                 TO  LTAXL
00859              MOVE AL-UNBON           TO  LTAXA
00860              MOVE ER-2082            TO  EMI-ERROR
00861              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00862
00863      IF TAXOPTL GREATER ZERO
00864          IF TAXOPTI = 'Y' OR 'N' OR ' ' OR 'F'
00865              MOVE AL-UANON       TO  TAXOPTA
00866          ELSE
00867              MOVE -1             TO  TAXOPTL
00868              MOVE AL-UABON       TO  TAXOPTA
00869              MOVE ER-2100        TO  EMI-ERROR
00870              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00871
00872      IF PRTOPTL GREATER ZERO
00873          IF PRTOPTI = 'A' OR 'F' OR 'Y' OR ' '
00874              MOVE AL-UANON       TO  PRTOPTA
00875          ELSE
00876              MOVE -1             TO  PRTOPTL
00877              MOVE AL-UABON       TO  PRTOPTA
00878              MOVE ER-2101        TO  EMI-ERROR
00879              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00880
00881      IF GROUPAL GREATER ZERO
00882          MOVE AL-UANON           TO  GROUPAA.
00883
00884      IF GROUPBL GREATER ZERO
00885          MOVE AL-UANON           TO  GROUPBA.
00886
00887      IF MORTALL GREATER ZERO
00888          PERFORM 7400-EDIT-MORTALITY
00889             THRU 7499-EXIT.
00890
00891      IF ACFEEL GREATER ZERO
00892          MOVE ACFEEI                 TO  DEEDIT-FIELD
00893          PERFORM 7200-DEEDIT THRU 7200-EXIT
00894          IF DEEDIT-FIELD-V4 NUMERIC
00895              MOVE DEEDIT-FIELD-V4    TO  WS-AH-FEE
00896              MOVE AL-UNNON           TO  ACFEEA
00897          ELSE
00898              MOVE -1                 TO  ACFEEL
00899              MOVE AL-UNBON           TO  ACFEEA
00900              MOVE ER-2083            TO  EMI-ERROR
00901              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00902
00903      IF ACBASISL GREATER ZERO
00904          IF ACBASISI = SPACES
00905              NEXT SENTENCE
00906          ELSE
00907              IF ACBASISI = 'P' OR 'E' OR ' '
00908                  MOVE AL-UANON       TO  ACBASISA
00909              ELSE
00910                  MOVE -1             TO  ACBASISL
00911                  MOVE AL-UABON       TO  ACBASISA
00912                  MOVE ER-2099        TO  EMI-ERROR
00913                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00914
00915      IF ATAXL GREATER ZERO
00916          MOVE ATAXI                  TO  DEEDIT-FIELD
00917          PERFORM 7200-DEEDIT THRU 7200-EXIT
00918          IF DEEDIT-FIELD-V4 NUMERIC
00919              MOVE DEEDIT-FIELD-V4    TO  WS-AH-TAX
00920              MOVE AL-UNNON           TO  ATAXA
00921          ELSE
00922              MOVE -1                 TO  ATAXL
00923              MOVE AL-UNBON           TO  ATAXA
00924              MOVE ER-2084            TO  EMI-ERROR
00925              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00926
00927      IF ARULEL GREATER ZERO
00928          MOVE ARULEI                 TO  DEEDIT-FIELD
00929          PERFORM 7200-DEEDIT THRU 7200-EXIT
00930          IF DEEDIT-FIELD-V4 NUMERIC
00931              MOVE DEEDIT-FIELD-V4    TO  WS-AH-R78
00932              MOVE AL-UNNON           TO  ARULEA
00933              ADD WS-AH-R78           TO  WS-TOT-PERCENT
00934          ELSE
00935              MOVE -1                 TO  ARULEL
00936              MOVE AL-UNBON           TO  ARULEA
00937              MOVE ER-2085            TO  EMI-ERROR
00938              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00939
00940      IF ARATAL GREATER ZERO
00941          MOVE ARATAI                 TO  DEEDIT-FIELD
00942          PERFORM 7200-DEEDIT THRU 7200-EXIT
00943          IF DEEDIT-FIELD-V4 NUMERIC
00944              MOVE DEEDIT-FIELD-V4    TO  WS-AH-PRATA
00945              MOVE AL-UNNON           TO  ARATAA
00946              ADD WS-AH-PRATA         TO  WS-TOT-PERCENT
00947              IF WS-TOT-PERCENT = +1 OR ZERO
00948                  NEXT SENTENCE
00949              ELSE
00950                  MOVE -1             TO  ARULEL
00951                  MOVE AL-UNBON       TO  ARULEA
00952                                          ARATAA
00953                  MOVE ER-2102        TO  EMI-ERROR
00954                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00955          ELSE
00956              MOVE -1             TO  ARATAL
00957              MOVE AL-UNBON       TO  ARATAA
00958              MOVE ER-2086        TO  EMI-ERROR
00959              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00960
00961      IF YNRETROL GREATER ZERO
00962          IF YNRETROI = 'Y' OR 'N' OR 'Q' OR 'S' OR
00963                        'G' OR 'I' OR ' '
00964              NEXT SENTENCE
00965          ELSE
00966              MOVE -1             TO  YNRETROL
00967              MOVE AL-UABON       TO  YNRETROA
00968              MOVE ER-2161        TO  EMI-ERROR
00969              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00970
00971      IF PECOMML GREATER ZERO
00972          IF PECOMMI = 'P' OR 'E' OR ' '
00973              NEXT SENTENCE
00974          ELSE
00975              MOVE ER-2162        TO  EMI-ERROR
00976              MOVE -1             TO  PECOMML
00977              MOVE AL-UABON       TO  PECOMMA
00978              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00979      ELSE
00980          IF YNRETROI NOT = 'N'
00981              MOVE ER-2171        TO  EMI-ERROR
00982              MOVE -1             TO  PECOMML
00983              MOVE AL-UABON       TO  PECOMMA
00984              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00985
00986      IF PERETROL GREATER ZERO
00987          IF PERETROI = 'P' OR 'E' OR ' '
00988              NEXT SENTENCE
00989          ELSE
00990              MOVE ER-2162        TO  EMI-ERROR
00991              MOVE -1             TO  PERETROL
00992              MOVE AL-UABON       TO  PERETROA
00993              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00994
00995      IF PICLAIML GREATER ZERO
00996          IF PICLAIMI = 'P' OR 'I' OR ' '
00997              NEXT SENTENCE
00998          ELSE
00999              MOVE ER-2162        TO  EMI-ERROR
01000              MOVE -1             TO  PICLAIML
01001              MOVE AL-UABON       TO  PICLAIMA
01002              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01003
01004      IF GPRETROL GREATER ZERO
01005          MOVE AL-UANON           TO  GPRETROA.
01006
01007      IF RELIMITL GREATER ZERO
01008          MOVE RELIMITI               TO  DEEDIT-FIELD
01009          PERFORM 7200-DEEDIT THRU 7200-EXIT
01010          IF DEEDIT-FIELD-V0 NUMERIC
01011              MOVE DEEDIT-FIELD-V0    TO  WS-QUALIFY-LIMIT
01012              MOVE AL-UNNON           TO  RELIMITA
01013          ELSE
01014              MOVE -1                 TO  RELIMITL
01015              MOVE AL-UNBON           TO  RELIMITA
01016              MOVE ER-0620            TO  EMI-ERROR
01017              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01018
01019      IF LFLOSSL GREATER ZERO
01020          MOVE LFLOSSI                TO  DEEDIT-FIELD
01021          PERFORM 7200-DEEDIT THRU 7200-EXIT
01022          IF DEEDIT-FIELD-V3 NUMERIC
01023              MOVE DEEDIT-FIELD-V3    TO  WS-LF-LOSS
01024              MOVE AL-UNNON           TO  LFLOSSA
01025          ELSE
01026              MOVE -1                 TO  LFLOSSL
01027              MOVE AL-UNBON           TO  LFLOSSA
01028              MOVE ER-0620            TO  EMI-ERROR
01029              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01030
01031      IF AHLOSSL GREATER ZERO
01032          MOVE AHLOSSI                TO  DEEDIT-FIELD
01033          PERFORM 7200-DEEDIT THRU 7200-EXIT
01034          IF DEEDIT-FIELD-V3 NUMERIC
01035              MOVE DEEDIT-FIELD-V3    TO  WS-AH-LOSS
01036              MOVE AL-UNNON           TO  AHLOSSA
01037          ELSE
01038              MOVE -1                 TO  AHLOSSL
01039              MOVE AL-UNBON           TO  AHLOSSA
01040              MOVE ER-0621            TO  EMI-ERROR
01041              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01042
01043      IF REDEARNL GREATER ZERO
01044          MOVE REDEARNI           TO  WS-VALID-EARNINGS
01045          IF VALID-EARNINGS
01046              MOVE AL-UANON       TO  REDEARNA
01047          ELSE
01048              MOVE -1             TO  REDEARNL
01049              MOVE AL-UABON       TO  REDEARNA
01050              MOVE ER-2165        TO  EMI-ERROR
01051              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01052
01053      IF LEVEARNL GREATER ZERO
01054          MOVE LEVEARNI           TO  WS-VALID-EARNINGS
01055          IF VALID-EARNINGS
01056              MOVE AL-UANON       TO  LEVEARNA
01057          ELSE
01058              MOVE -1             TO  LEVEARNL
01059              MOVE AL-UABON       TO  LEVEARNA
01060              MOVE ER-2165        TO  EMI-ERROR
01061              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01062
01063      IF AHEARNL GREATER ZERO
01064          MOVE AHEARNI            TO  WS-VALID-EARNINGS
01065          IF VALID-EARNINGS
01066              MOVE AL-UANON       TO  AHEARNA
01067          ELSE
01068              MOVE -1             TO  AHEARNL
01069              MOVE AL-UABON       TO  AHEARNA
01070              MOVE ER-2165        TO  EMI-ERROR
01071              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01072
01073      IF BEGREDL GREATER ZERO
01074          MOVE BEGREDI            TO  WS-VALID-EARNINGS
01075          IF VALID-EARNINGS
01076              MOVE AL-UANON       TO  BEGREDA
01077          ELSE
01078              MOVE -1             TO  BEGREDL
01079              MOVE AL-UABON       TO  BEGREDA
01080              MOVE ER-2165        TO  EMI-ERROR
01081              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01082
01083      IF BEGLEVL GREATER ZERO
01084          MOVE BEGLEVI            TO  WS-VALID-EARNINGS
01085          IF VALID-EARNINGS
01086              MOVE AL-UANON       TO  BEGLEVA
01087          ELSE
01088              MOVE -1             TO  BEGLEVL
01089              MOVE AL-UABON       TO  BEGLEVA
01090              MOVE ER-2165        TO  EMI-ERROR
01091              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01092
01093      IF BEGAHL GREATER ZERO
01094          MOVE BEGAHI             TO  WS-VALID-EARNINGS
01095          IF VALID-EARNINGS
01096              MOVE AL-UANON       TO  BEGAHA
01097          ELSE
01098              MOVE -1             TO  BEGAHL
01099              MOVE AL-UABON       TO  BEGAHA
01100              MOVE ER-2165        TO  EMI-ERROR
01101              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01102
01103      IF TAXOPL GREATER ZERO
01104          IF TAXOPI = 'P' OR 'E' OR 'Y' OR ' '
01105              MOVE AL-UANON       TO  TAXOPA
01106          ELSE
01107              MOVE -1             TO  TAXOPL
01108              MOVE AL-UABON       TO  TAXOPA
01109              MOVE ER-0626        TO  EMI-ERROR
01110              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01111
01112      IF LFRETENL GREATER ZERO
01113          MOVE LFRETENI               TO  DEEDIT-FIELD
01114          PERFORM 7200-DEEDIT THRU 7200-EXIT
01115          IF DEEDIT-FIELD-V4 NUMERIC
01116              MOVE DEEDIT-FIELD-V4    TO  WS-LF-RET
01117              IF WS-LF-RET GREATER 1
01118                  MOVE -1             TO  LFRETENL
01119                  MOVE AL-UNBON       TO  LFRETENA
01120                  MOVE ER-2163        TO  EMI-ERROR
01121                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01122              ELSE
01123                  MOVE AL-UNNON       TO  LFRETENA
01124          ELSE
01125              MOVE -1             TO  LFRETENL
01126              MOVE AL-UNBON       TO  LFRETENA
01127              MOVE ER-2163        TO  EMI-ERROR
01128              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01129
01130 *    IF LFMETHDL GREATER ZERO
01131 *        IF LFMETHDI = 'P' OR 'S' OR ' '
01132 *            NEXT SENTENCE
01133 *        ELSE
01134 *            MOVE ER-2162        TO  EMI-ERROR
01135 *            MOVE -1             TO  LFMETHDL
01136 *            MOVE AL-UABON       TO  LFMETHDA
01137 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01138 *
01139 *    IF LFBASISL GREATER ZERO
01140 *        IF LFBASISI = 'P' OR 'E' OR ' '
01141 *            NEXT SENTENCE
01142 *        ELSE
01143 *            MOVE ER-2162        TO  EMI-ERROR
01144 *            MOVE -1             TO  LFBASISL
01145 *            MOVE AL-UABON       TO  LFBASISA
01146 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01147 *
01148 *    IF LFPCT1L GREATER ZERO
01149 *        MOVE LFPCT1I                TO  DEEDIT-FIELD
01150 *        PERFORM 7200-DEEDIT THRU 7200-EXIT
01151 *        IF DEEDIT-FIELD-V4 NUMERIC
01152 *            MOVE DEEDIT-FIELD-V4    TO  WS-LF-PCT-1
01153 *            IF WS-LF-PCT-1 GREATER 1
01154 *                MOVE -1             TO  LFPCT1L
01155 *                MOVE AL-UNBON       TO  LFPCT1A
01156 *                MOVE ER-2163        TO  EMI-ERROR
01157 *                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01158 *            ELSE
01159 *                MOVE AL-UNNON       TO  LFPCT1A
01160 *        ELSE
01161 *            MOVE -1             TO  LFPCT1L
01162 *            MOVE AL-UNBON       TO  LFPCT1A
01163 *            MOVE ER-2163        TO  EMI-ERROR
01164 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01165 *
01166 *    IF LFTHRU1L GREATER ZERO
01167 *        MOVE LFTHRU1I               TO  DEEDIT-FIELD
01168 *        PERFORM 7200-DEEDIT THRU 7200-EXIT
01169 *        IF DEEDIT-FIELD-V0 NUMERIC
01170 *            MOVE DEEDIT-FIELD-V0    TO  WS-LF-THRU-1
01171 *            MOVE AL-UNNON           TO  LFTHRU1A
01172 *        ELSE
01173 *            MOVE -1             TO  LFTHRU1L
01174 *            MOVE AL-UNBON       TO  LFTHRU1A
01175 *            MOVE ER-2163        TO  EMI-ERROR
01176 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01177 *
01178 *    IF LFPCT2L GREATER ZERO
01179 *        MOVE LFPCT2I                TO  DEEDIT-FIELD
01180 *        PERFORM 7200-DEEDIT THRU 7200-EXIT
01181 *        IF DEEDIT-FIELD-V4 NUMERIC
01182 *            MOVE DEEDIT-FIELD-V4    TO  WS-LF-PCT-2
01183 *            IF WS-LF-PCT-2 GREATER 1
01184 *                MOVE -1             TO  LFPCT2L
01185 *                MOVE AL-UNBON       TO  LFPCT2A
01186 *                MOVE ER-2163        TO  EMI-ERROR
01187 *                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01188 *            ELSE
01189 *                MOVE AL-UNNON       TO  LFPCT2A
01190 *        ELSE
01191 *            MOVE -1             TO  LFPCT2L
01192 *            MOVE AL-UNBON       TO  LFPCT2A
01193 *            MOVE ER-2163        TO  EMI-ERROR
01194 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01195 *
01196 *    IF LFTHRU2L GREATER ZERO
01197 *        MOVE LFTHRU2I               TO  DEEDIT-FIELD
01198 *        PERFORM 7200-DEEDIT THRU 7200-EXIT
01199 *        IF DEEDIT-FIELD-V0 NUMERIC
01200 *            MOVE DEEDIT-FIELD-V0    TO  WS-LF-THRU-2
01201 *            MOVE AL-UNNON           TO  LFTHRU2A
01202 *        ELSE
01203 *            MOVE -1             TO  LFTHRU2L
01204 *            MOVE AL-UNBON       TO  LFTHRU2A
01205 *            MOVE ER-2163        TO  EMI-ERROR
01206 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01207 *
01208 *    IF LFPCT3L GREATER ZERO
01209 *        MOVE LFPCT3I                TO  DEEDIT-FIELD
01210 *        PERFORM 7200-DEEDIT THRU 7200-EXIT
01211 *        IF DEEDIT-FIELD-V4 NUMERIC
01212 *            MOVE DEEDIT-FIELD-V4    TO  WS-LF-PCT-3
01213 *            IF WS-LF-PCT-3 GREATER 1
01214 *                MOVE -1             TO  LFPCT3L
01215 *                MOVE AL-UNBON       TO  LFPCT3A
01216 *                MOVE ER-2163        TO  EMI-ERROR
01217 *                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01218 *            ELSE
01219 *                MOVE AL-UNNON       TO  LFPCT3A
01220 *        ELSE
01221 *            MOVE -1             TO  LFPCT3L
01222 *            MOVE AL-UNBON       TO  LFPCT3A
01223 *            MOVE ER-2163        TO  EMI-ERROR
01224 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01225 *
01226 *    IF LFTHRU3L GREATER ZERO
01227 *        MOVE LFTHRU3I               TO  DEEDIT-FIELD
01228 *        PERFORM 7200-DEEDIT THRU 7200-EXIT
01229 *        IF DEEDIT-FIELD-V0 NUMERIC
01230 *            MOVE DEEDIT-FIELD-V0    TO  WS-LF-THRU-3
01231 *            MOVE AL-UNNON           TO  LFTHRU3A
01232 *        ELSE
01233 *            MOVE -1             TO  LFTHRU3L
01234 *            MOVE AL-UNBON       TO  LFTHRU3A
01235 *            MOVE ER-2163        TO  EMI-ERROR
01236 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01237
01238      IF AHRETENL GREATER ZERO
01239          MOVE AHRETENI               TO  DEEDIT-FIELD
01240          PERFORM 7200-DEEDIT THRU 7200-EXIT
01241          IF DEEDIT-FIELD-V4 NUMERIC
01242              MOVE DEEDIT-FIELD-V4    TO  WS-AH-RET
01243              IF WS-AH-RET GREATER 1
01244                  MOVE -1             TO  AHRETENL
01245                  MOVE AL-UNBON       TO  AHRETENA
01246                  MOVE ER-2164        TO  EMI-ERROR
01247                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01248              ELSE
01249                  MOVE AL-UNNON       TO  AHRETENA
01250          ELSE
01251              MOVE -1             TO  AHRETENL
01252              MOVE AL-UNBON       TO  AHRETENA
01253              MOVE ER-2164        TO  EMI-ERROR
01254              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01255
01256 *    IF AHMETHDL GREATER ZERO
01257 *        IF AHMETHDI = 'P' OR 'S' OR ' '
01258 *            NEXT SENTENCE
01259 *        ELSE
01260 *            MOVE ER-2162        TO  EMI-ERROR
01261 *            MOVE -1             TO  AHMETHDL
01262 *            MOVE AL-UABON       TO  AHMETHDA
01263 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01264 *
01265 *    IF AHBASISL GREATER ZERO
01266 *        IF AHBASISI = 'P' OR 'E' OR ' '
01267 *            NEXT SENTENCE
01268 *        ELSE
01269 *            MOVE ER-2162        TO  EMI-ERROR
01270 *            MOVE -1             TO  AHBASISL
01271 *            MOVE AL-UABON       TO  AHBASISA
01272 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01273 *
01274 *    IF AHPCT1L GREATER ZERO
01275 *        MOVE AHPCT1I                TO  DEEDIT-FIELD
01276 *        PERFORM 7200-DEEDIT THRU 7200-EXIT
01277 *        IF DEEDIT-FIELD-V4 NUMERIC
01278 *            MOVE DEEDIT-FIELD-V4    TO  WS-AH-PCT-1
01279 *            IF WS-AH-PCT-1 GREATER 1
01280 *                MOVE -1             TO  AHPCT1L
01281 *                MOVE AL-UNBON       TO  AHPCT1A
01282 *                MOVE ER-2163        TO  EMI-ERROR
01283 *                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01284 *            ELSE
01285 *                MOVE AL-UNNON       TO  AHPCT1A
01286 *        ELSE
01287 *            MOVE -1             TO  AHPCT1L
01288 *            MOVE AL-UNBON       TO  AHPCT1A
01289 *            MOVE ER-2163        TO  EMI-ERROR
01290 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01291 *
01292 *    IF AHTHRU1L GREATER ZERO
01293 *        MOVE AHTHRU1I               TO  DEEDIT-FIELD
01294 *        PERFORM 7200-DEEDIT THRU 7200-EXIT
01295 *        IF DEEDIT-FIELD-V0 NUMERIC
01296 *            MOVE DEEDIT-FIELD-V0    TO  WS-AH-THRU-1
01297 *            MOVE AL-UNNON           TO  AHTHRU1A
01298 *        ELSE
01299 *            MOVE -1             TO  AHTHRU1L
01300 *            MOVE AL-UNBON       TO  AHTHRU1A
01301 *            MOVE ER-2163        TO  EMI-ERROR
01302 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01303 *
01304 *    IF AHPCT2L GREATER ZERO
01305 *        MOVE AHPCT2I                TO  DEEDIT-FIELD
01306 *        PERFORM 7200-DEEDIT THRU 7200-EXIT
01307 *        IF DEEDIT-FIELD-V4 NUMERIC
01308 *            MOVE DEEDIT-FIELD-V4    TO  WS-AH-PCT-2
01309 *            IF WS-AH-PCT-2 GREATER 1
01310 *                MOVE -1             TO  AHPCT2L
01311 *                MOVE AL-UNBON       TO  AHPCT2A
01312 *                MOVE ER-2163        TO  EMI-ERROR
01313 *                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01314 *            ELSE
01315 *                MOVE AL-UNNON       TO  AHPCT2A
01316 *        ELSE
01317 *            MOVE -1             TO  AHPCT2L
01318 *            MOVE AL-UNBON       TO  AHPCT2A
01319 *            MOVE ER-2163        TO  EMI-ERROR
01320 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01321 *
01322 *    IF AHTHRU2L GREATER ZERO
01323 *        MOVE AHTHRU2I               TO  DEEDIT-FIELD
01324 *        PERFORM 7200-DEEDIT THRU 7200-EXIT
01325 *        IF DEEDIT-FIELD-V0 NUMERIC
01326 *            MOVE DEEDIT-FIELD-V0    TO  WS-AH-THRU-2
01327 *            MOVE AL-UNNON           TO  AHTHRU2A
01328 *        ELSE
01329 *            MOVE -1             TO  AHTHRU2L
01330 *            MOVE AL-UNBON       TO  AHTHRU2A
01331 *            MOVE ER-2163        TO  EMI-ERROR
01332 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01333 *
01334 *    IF AHPCT3L GREATER ZERO
01335 *        MOVE AHPCT3I                TO  DEEDIT-FIELD
01336 *        PERFORM 7200-DEEDIT THRU 7200-EXIT
01337 *        IF DEEDIT-FIELD-V4 NUMERIC
01338 *            MOVE DEEDIT-FIELD-V4    TO  WS-AH-PCT-3
01339 *            IF WS-AH-PCT-3 GREATER 1
01340 *                MOVE -1             TO  AHPCT3L
01341 *                MOVE AL-UNBON       TO  AHPCT3A
01342 *                MOVE ER-2163        TO  EMI-ERROR
01343 *                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01344 *            ELSE
01345 *                MOVE AL-UNNON       TO  AHPCT3A
01346 *        ELSE
01347 *            MOVE -1             TO  AHPCT3L
01348 *            MOVE AL-UNBON       TO  AHPCT3A
01349 *            MOVE ER-2163        TO  EMI-ERROR
01350 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01351 *
01352 *    IF AHTHRU3L GREATER ZERO
01353 *        MOVE AHTHRU3I               TO  DEEDIT-FIELD
01354 *        PERFORM 7200-DEEDIT THRU 7200-EXIT
01355 *        IF DEEDIT-FIELD-V0 NUMERIC
01356 *            MOVE DEEDIT-FIELD-V0    TO  WS-AH-THRU-3
01357 *            MOVE AL-UNNON           TO  AHTHRU3A
01358 *        ELSE
01359 *            MOVE -1             TO  LFTHRU3L
01360 *            MOVE AL-UNBON       TO  LFTHRU3A
01361 *            MOVE ER-2163        TO  EMI-ERROR
01362 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01363 *
01364      IF COM1L GREATER ZERO
01365          MOVE AL-UANON           TO  COM1A.
01366
01367      IF COM2L GREATER ZERO
01368          MOVE AL-UANON           TO  COM2A.
01369
01370      IF COM3L GREATER ZERO
01371          MOVE AL-UANON           TO  COM3A.
01372
01373      IF COM4L GREATER ZERO
01374          MOVE AL-UANON           TO  COM4A.
01375
01376      IF COM5L GREATER ZERO
01377          MOVE AL-UANON           TO  COM5A.
01378
01379  7099-EXIT.
01380      EXIT.
01381      EJECT
01382  7100-READ-ERACCT.
01383      
      * EXEC CICS READ
01384 *         DATASET  (ERACCT-FILE)
01385 *         SET      (ADDRESS OF ACCOUNT-MASTER)
01386 *         RIDFLD   (PI-ACCT-KEY)
01387 *    END-EXEC.
      *    MOVE '&"S        E          (   #00005020' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303230' TO DFHEIV0(25:11)
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
           
01388
01389      CONTINUE.
01390
01391      MOVE AM-LAST-MAINT-USER     TO  PI-UPDATE-BY.
01392      MOVE AM-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.
01393
01394  7100-EXIT.
01395      EXIT.
01396      EJECT
01397  7200-DEEDIT.
01398      
      * EXEC CICS BIF
01399 *         DEEDIT
01400 *         FIELD  (DEEDIT-FIELD)
01401 *         LENGTH (15)
01402 *    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005035' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01403
01404  7200-EXIT.
01405      EXIT.
01406      EJECT
01407  7300-READ-ERACCT-UPDATE.
01408      
      * EXEC CICS READ
01409 *         DATASET  (ERACCT-FILE)
01410 *         SET      (ADDRESS OF ACCOUNT-MASTER)
01411 *         RIDFLD   (PI-ACCT-KEY)
01412 *         UPDATE
01413 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005045' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303435' TO DFHEIV0(25:11)
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
           
01414
01415      CONTINUE.
01416
01417  7300-EXIT.
01418      EXIT.
01419      EJECT
01420  7400-EDIT-MORTALITY.
01421      IF MORTALI = SPACES
01422          GO TO 7499-EXIT.
01423
01424      MOVE SPACES                 TO  ELCNTL-KEY.
01425      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
01426      MOVE '7'                    TO  CNTL-REC-TYPE.
01427      MOVE +0                     TO  CNTL-SEQ-NO.
01428
01429      
      * EXEC CICS HANDLE CONDITION
01430 *        NOTFND  (7490-NOT-FOUND)
01431 *        ENDFILE (7490-NOT-FOUND)
01432 *    END-EXEC.
      *    MOVE '"$I''                  ! # #00005066' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303035303636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01433
01434  7405-READ-MORTALITY.
01435
01436      
      * EXEC CICS READ
01437 *         DATASET  (CNTL-FILE-ID)
01438 *         SET      (ADDRESS OF CONTROL-FILE)
01439 *         RIDFLD   (ELCNTL-KEY)
01440 *    END-EXEC.
      *    MOVE '&"S        E          (   #00005073' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01441
01442      CONTINUE.
01443
01444      IF PI-COMPANY-ID NOT EQUAL  TO CF-COMPANY-ID
01445          OR CF-RECORD-TYPE NOT EQUAL '7'
01446          GO TO 7490-NOT-FOUND
01447      ELSE
01448          MOVE +1                 TO  SUB2
01449          GO TO 7410-SEARCH-MORTAL-TABLE.
01450
01451  7410-SEARCH-MORTAL-TABLE.
01452
01453      IF CF-MORT-TABLE-CODE (SUB2) = MORTALI
01454          GO TO 7499-EXIT.
01455
01456      IF CF-MORT-TABLE-CODE (SUB2) = LOW-VALUES
01457             OR
01458         CF-MORT-TABLE-CODE (SUB2) GREATER THAN MORTALI
01459         GO TO 7490-NOT-FOUND.
01460
01461      ADD +1                      TO  SUB2.
01462
01463      IF SUB2 GREATER +9
01464          ADD +1                  TO CNTL-SEQ-NO
01465          GO TO 7405-READ-MORTALITY
01466      ELSE
01467          GO TO 7410-SEARCH-MORTAL-TABLE.
01468
01469  7490-NOT-FOUND.
01470      MOVE -1                     TO  MORTALL.
01471      MOVE AL-UABON               TO  MORTALA.
01472      MOVE ER-2103                TO  EMI-ERROR
01473      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01474
01475  7499-EXIT.
01476      EXIT.
01477      EJECT
01478  7800-COMPANY-REC-READ.
01479      MOVE SPACES                 TO  ELCNTL-KEY.
01480      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
01481      MOVE '1'                    TO  CNTL-REC-TYPE.
01482      MOVE +0                     TO  CNTL-SEQ-NO.
01483      
      * EXEC CICS HANDLE CONDITION
01484 *        NOTFND   (7880-NO-COMP)
01485 *    END-EXEC.
      *    MOVE '"$I                   ! $ #00005120' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303035313230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01486
01487      
      * EXEC CICS READ
01488 *        DATASET   (CNTL-FILE-ID)
01489 *        SET       (ADDRESS OF CONTROL-FILE)
01490 *        RIDFLD    (ELCNTL-KEY)
01491 *    END-EXEC.
      *    MOVE '&"S        E          (   #00005124' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01492
01493      CONTINUE.
01494
01495      IF CF-ACCOUNT-MSTR-MAINT-DT = LOW-VALUES
01496          MOVE ER-2572            TO  EMI-ERROR
01497          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01498
01499      GO TO 7899-EXIT.
01500
01501  7880-NO-COMP.
01502      MOVE ER-0002                TO  EMI-ERROR
01503      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01504
01505  7899-EXIT.
01506      EXIT.
01507      EJECT
01508
01509  8000-UPDATE-MAINT-DATE.
01510      MOVE SPACES                 TO  ELCNTL-KEY.
01511
01512      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
01513      MOVE '1'                    TO  CNTL-REC-TYPE.
01514      MOVE +0                     TO  CNTL-SEQ-NO.
01515
01516      
      * EXEC CICS HANDLE CONDITION
01517 *        NOTFND   (8000-EXIT)
01518 *    END-EXEC.
      *    MOVE '"$I                   ! % #00005153' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303035313533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01519
01520      
      * EXEC CICS READ
01521 *        UPDATE
01522 *        DATASET   (CNTL-FILE-ID)
01523 *        SET       (ADDRESS OF CONTROL-FILE)
01524 *        RIDFLD    (ELCNTL-KEY)
01525 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005157' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01526
01527      CONTINUE.
01528
01529      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.
01530      MOVE 'B'                    TO  JP-RECORD-TYPE.
01531      MOVE ELCNTL-LENGTH          TO  WS-JOURNAL-FILE-LENGTH.
01532      MOVE CNTL-FILE-ID           TO  FILE-ID.
01533      PERFORM 8400-LOG-JOURNAL-RECORD.
01534
01535      MOVE BIN-CURRENT-SAVE       TO  CF-ACCOUNT-MSTR-MAINT-DT.
01536
01537      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.
01538      MOVE 'C'                    TO  JP-RECORD-TYPE.
01539      MOVE CNTL-FILE-ID           TO  FILE-ID.
01540
01541      
      * EXEC CICS REWRITE
01542 *        DATASET   (CNTL-FILE-ID)
01543 *        FROM      (CONTROL-FILE)
01544 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005178' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01545
01546      MOVE ELCNTL-LENGTH          TO  WS-JOURNAL-FILE-LENGTH.
01547      PERFORM 8400-LOG-JOURNAL-RECORD.
01548
01549  8000-EXIT.
01550       EXIT.
01551      EJECT
01552
01553  8100-SEND-INITIAL-MAP.
01554      MOVE SAVE-DATE              TO  DATEO.
01555      MOVE EIBTIME                TO  TIME-IN.
01556      MOVE TIME-OUT               TO  TIMEO.
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01557      MOVE -1                     TO  PFENTERL.
01558      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
01559
01560      MOVE PI-LIFE-OVERRIDE-L6    TO  LFREIHDO
01561                                      LFRETHDO.
01562      MOVE PI-AH-OVERRIDE-L6      TO  AHREIHDO
01563                                      AHRETHDO.
01564      MOVE PI-LIFE-OVERRIDE-L2    TO  LRETHD1O.
01565      MOVE PI-AH-OVERRIDE-L2      TO  ARETHD1O
01566                                      ARETHD2O
01567                                      ARETHD3O.
01568
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
PEMMOD     IF PI-PROCESSOR-ID NOT = 'PEMA'
PEMMOD        MOVE AL-SADON            TO LTAXA
PEMMOD                                    ATAXA
PEMMOD     END-IF
PEMMOD
01569      
      * EXEC CICS SEND
01570 *        MAP      (MAP-NAME)
01571 *        MAPSET   (MAPSET-NAME)
01572 *        FROM     (EL6504AO)
01573 *        ERASE
01574 *        CURSOR
01575 *    END-EXEC.
           MOVE LENGTH OF
            EL6504AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00005264' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6504AO, 
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
           
01576
01577      GO TO 9100-RETURN-TRAN.
01578
01579  8200-SEND-DATAONLY.
01580      MOVE SAVE-DATE              TO  DATEO.
01581      MOVE EIBTIME                TO  TIME-IN.
01582      MOVE TIME-OUT               TO  TIMEO.
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01583      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O
01584
01585      MOVE PI-LIFE-OVERRIDE-L6    TO  LFREIHDO
01586                                      LFRETHDO.
01587      MOVE PI-AH-OVERRIDE-L6      TO  AHREIHDO
01588                                      AHRETHDO.
01589      MOVE PI-LIFE-OVERRIDE-L2    TO  LRETHD1O.
01590      MOVE PI-AH-OVERRIDE-L2      TO  ARETHD1O
01591                                      ARETHD2O
01592                                      ARETHD3O.
01593
PEMMOD     IF PI-PROCESSOR-ID NOT = 'PEMA'
PEMMOD        MOVE AL-SADON            TO LTAXA
PEMMOD                                    ATAXA
PEMMOD     END-IF
PEMMOD
01594      
      * EXEC CICS SEND
01595 *        MAP      (MAP-NAME)
01596 *        MAPSET   (MAPSET-NAME)
01597 *        FROM     (EL6504AO)
01598 *        DATAONLY
01599 *        ERASEAUP
01600 *        CURSOR
01601 *    END-EXEC.
           MOVE LENGTH OF
            EL6504AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT  A    H L F ,   #00005296' TO DFHEIV0
           MOVE X'382444202020204354202041' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL6504AO, 
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
           
01602
01603      GO TO 9100-RETURN-TRAN.
01604
01605  8300-SEND-TEXT.
01606      
      * EXEC CICS SEND TEXT
01607 *        FROM     (LOGOFF-TEXT)
01608 *        LENGTH   (LOGOFF-LENGTH)
01609 *        ERASE
01610 *        FREEKB
01611 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00005308' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333038' TO DFHEIV0(25:11)
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
           
01612
01613      
      * EXEC CICS RETURN
01614 *    END-EXEC.
      *    MOVE '.(                    ''   #00005315' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01615
01616  8400-LOG-JOURNAL-RECORD.
01617      MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.
01618      MOVE FILE-ID                TO  JP-FILE-ID.
01619      MOVE THIS-PGM               TO  JP-PROGRAM-ID.
pemuni*    IF PI-JOURNAL-FILE-ID NOT = ZERO
pemuni*        EXEC CICS JOURNAL
pemuni*            JFILEID     (PI-JOURNAL-FILE-ID)
pemuni*            JTYPEID     ('ER')
pemuni*            FROM        (JOURNAL-RECORD)
pemuni*            LENGTH      (WS-JOURNAL-FILE-LENGTH)
pemuni*        END-EXEC.
01627
01628  8800-UNAUTHORIZED-ACCESS.
01629      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
01630      GO TO 8300-SEND-TEXT.
01631
01632  8810-PF23.
01633      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
01634      MOVE XCTL-005               TO  PGM-NAME.
01635      GO TO 9300-XCTL.
01636
01637  9000-RETURN-CICS.
01638      
      * EXEC CICS RETURN
01639 *    END-EXEC.
      *    MOVE '.(                    ''   #00005340' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01640
01641  9100-RETURN-TRAN.
01642      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
01643      MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.
01644      
      * EXEC CICS RETURN
01645 *        TRANSID    (TRANS-ID)
01646 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
01647 *        LENGTH     (WS-COMM-LENGTH)
01648 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00005346' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01649
01650  9200-RETURN-MAIN-MENU.
01651      MOVE XCTL-626               TO  PGM-NAME.
01652      GO TO 9300-XCTL.
01653
01654  9300-XCTL.
01655      
      * EXEC CICS XCTL
01656 *        PROGRAM    (PGM-NAME)
01657 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
01658 *        LENGTH     (WS-COMM-LENGTH)
01659 *    END-EXEC.
      *    MOVE '.$C                   %   #00005357' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01660
01661  9400-CLEAR.
01662      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.
01663      GO TO 9300-XCTL.
01664
01665  9500-PF12.
01666      MOVE XCTL-010               TO  PGM-NAME.
01667      GO TO 9300-XCTL.
01668
01669  9600-PGMID-ERROR.
01670      
      * EXEC CICS HANDLE CONDITION
01671 *        PGMIDERR    (8300-SEND-TEXT)
01672 *    END-EXEC.
      *    MOVE '"$L                   ! & #00005372' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303035333732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01673
01674      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.
01675      MOVE ' '                    TO  PI-ENTRY-CD-1.
01676      MOVE XCTL-005               TO  PGM-NAME.
01677      MOVE PGM-NAME               TO  LOGOFF-PGM.
01678      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
01679      GO TO 9300-XCTL.
01680
01681  9700-LINK-DATE-CONVERT.
01682
01683      
      * EXEC CICS LINK
01684 *        PROGRAM    ('ELDATCV')
01685 *        COMMAREA   (DATE-CONVERSION-DATA)
01686 *        LENGTH     (DC-COMM-LENGTH)
01687 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00005385' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01688
01689  9700-EXIT.
01690      EXIT.
01691
01692  9900-ERROR-FORMAT.
01693      IF NOT EMI-ERRORS-COMPLETE
01694          MOVE LINK-001           TO  PGM-NAME
01695          
      * EXEC CICS LINK
01696 *            PROGRAM    (PGM-NAME)
01697 *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
01698 *            LENGTH     (EMI-COMM-LENGTH)
01699 *        END-EXEC.
      *    MOVE '."C                   (   #00005397' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01700
01701  9900-EXIT.
01702      EXIT.
01703
01704  9990-ABEND.
01705      MOVE LINK-004               TO  PGM-NAME.
01706      MOVE DFHEIBLK               TO  EMI-LINE1.
01707      
      * EXEC CICS LINK
01708 *        PROGRAM   (PGM-NAME)
01709 *        COMMAREA  (EMI-LINE1)
01710 *        LENGTH    (72)
01711 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00005409' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01712
01713      GO TO 8200-SEND-DATAONLY.
01714
01715      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6504' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
01716
01717  9995-SECURITY-VIOLATION.
01718 *           COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00005437' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343337' TO DFHEIV0(25:11)
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
01719  9995-EXIT.
01720       EXIT.


       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6504' TO DFHEIV1
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
               GO TO 7490-NOT-FOUND,
                     7490-NOT-FOUND
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
           MOVE 'EL6504' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
