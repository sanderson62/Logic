00001  ID DIVISION.
00002
00003  PROGRAM-ID.                 EL656.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 11:18:33.
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE
00008 *                            VMOD=2.013
00009
00010 *AUTHOR.     LOGIC,INC.
00011 *            DALLAS, TEXAS.
00012
00013 *DATE-COMPILED.
00014 *SECURITY.   *****************************************************
00015 *            *                                                   *
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00021 *            *                                                   *
00022 *            *****************************************************
00023 *
00024 *REMARKS.    TRANSACTION - EXE1 - RATE MASTER MAINTENANCE.
00025 *
101501******************************************************************
101501*                   C H A N G E   L O G
101501*
101501* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101501*-----------------------------------------------------------------
101501*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101501* EFFECTIVE    NUMBER
101501*-----------------------------------------------------------------
101501* 101501    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
101501*                              ADJUST REDEFINES EL656AI FILLER
081413* 081413    2013080700002  PEMA ADD JOURNALING OF ERRATE FILE
101501******************************************************************
00027  ENVIRONMENT DIVISION.
00028  DATA DIVISION.
00029  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00030  77  FILLER  PIC X(32)  VALUE '********************************'.
00031  77  FILLER  PIC X(32)  VALUE '*    EL656 WORKING STORAGE     *'.
00032  77  FILLER  PIC X(32)  VALUE '************ V/M 2.013 *********'.
00033
00034  01  WS-DATE-AREA.
00035      05  SAVE-DATE           PIC X(8)    VALUE SPACES.
00036      05  SAVE-BIN-DATE       PIC XX      VALUE SPACES.
00037
00038  01  STANDARD-AREAS.
00039      05  GETMAIN-SPACE       PIC X       VALUE SPACE.
00040      05  WS-MAPNAME          PIC X(8)    VALUE 'EL656A'.
00041      05  WS-MAPSET-NAME      PIC X(8)    VALUE 'EL656S'.
00042      05  TRANS-ID            PIC X(4)    VALUE 'EXE1'.
00043      05  THIS-PGM            PIC X(8)    VALUE 'EL656'.
00044      05  PGM-NAME            PIC X(8).
00045      05  SUB1                PIC S999    VALUE +0.
00046      05  SUB2                PIC S99     VALUE +0.
00047      05  SC-ITEM             PIC S9(4)   VALUE +1     COMP.
00048      05  WS-COMP-CD-R.
00049          10  FILLER          PIC X.
00050          10  WS-COMP-CD-X    PIC X.
00051      05  WS-COMP-CD   REDEFINES WS-COMP-CD-R  PIC S9(4)     COMP.
00052      05  TIME-IN             PIC S9(7).
00053      05  TIME-OUT-R  REDEFINES TIME-IN.
00054          10  FILLER          PIC X.
00055          10  TIME-OUT        PIC 99V99.
00056          10  FILLER          PIC XX.
00057      05  TIME-MT             PIC S9(7).
00058      05  TIME-MT-R  REDEFINES TIME-MT.
00059          10  FILLER          PIC X.
00060          10  TIME-LMT        PIC 99V99.
00061          10  FILLER          PIC XX.
00062      05  XCTL-005            PIC X(8)    VALUE 'EL005'.
00063      05  XCTL-010            PIC X(8)    VALUE 'EL010'.
00064      05  XCTL-626            PIC X(8)    VALUE 'EL126'.
00065      05  XCTL-6561           PIC X(8)    VALUE 'EL6561'.
00066      05  LINK-001            PIC X(8)    VALUE 'EL001'.
00067      05  LINK-004            PIC X(8)    VALUE 'EL004'.
00068      05  LINK-ELDATCV        PIC X(8)    VALUE 'ELDATCV'.
00069
00070      05  ER-0000             PIC X(4)    VALUE  '0000'.
00071      05  ER-0004             PIC X(4)    VALUE  '0004'.
00072      05  ER-0008             PIC X(4)    VALUE  '0008'.
00073      05  ER-0029             PIC X(4)    VALUE  '0029'.
00074      05  ER-0050             PIC X(4)    VALUE  '0050'.
00075      05  ER-0068             PIC X(4)    VALUE  '0068'.
00076      05  ER-0070             PIC X(4)    VALUE  '0070'.
00077      05  ER-0142             PIC X(4)    VALUE  '0142'.
081413     05  ER-0755             PIC x(4)    value  '0755'.
00078      05  ER-2039             PIC X(4)    VALUE  '2039'.
00079      05  ER-2055             PIC X(4)    VALUE  '2055'.
00080      05  ER-2056             PIC X(4)    VALUE  '2056'.
00081      05  ER-2187             PIC X(4)    VALUE  '2187'.
00082      05  ER-2189             PIC X(4)    VALUE  '2189'.
00083      05  ER-2261             PIC X(4)    VALUE  '2261'.
00084      05  ER-2262             PIC X(4)    VALUE  '2262'.
00085      05  ER-2263             PIC X(4)    VALUE  '2263'.
00086      05  ER-2266             PIC X(4)    VALUE  '2266'.
00087      05  ER-2267             PIC X(4)    VALUE  '2267'.
00088      05  ER-2268             PIC X(4)    VALUE  '2268'.
00089      05  ER-2270             PIC X(4)    VALUE  '2270'.
00090      05  ER-2271             PIC X(4)    VALUE  '2271'.
00091      05  ER-2272             PIC X(4)    VALUE  '2272'.
00092      05  ER-2293             PIC X(4)    VALUE  '2293'.
00093      05  ER-2296             PIC X(4)    VALUE  '2296'.
00094      05  ER-2395             PIC X(4)    VALUE  '2395'.
00095      05  ER-2396             PIC X(4)    VALUE  '2396'.
00096      05  ER-7743             PIC X(4)    VALUE  '7743'.
00097
00098      05  RATE-FILE-ID        PIC X(8)    VALUE 'ERRATE'.
00099      05  OE-RATE-FILE-ID     PIC X(8)    VALUE 'OERATE'.
00100      05  CNTL-FILE-ID        PIC X(8)    VALUE 'ELCNTL'.
00101      05  FILE-ID             PIC X(8)    VALUE  SPACES.
00102      05  BIN-CURRENT-SAVE    PIC XX      VALUE  SPACES.
00103
00104      05  WS-HOLD-RATE-EXP-AL PIC X(11).
00105      05  WS-HOLD-RATE-EXP-DT REDEFINES
00106          WS-HOLD-RATE-EXP-AL PIC 9(11).
00107
081413 01  filler.
081413     12  WS-RESPONSE             PIC S9(8)   COMP.
081413         88  WS-RESP-NORMAL              VALUE +00.
081413         88  WS-RESP-ERROR               VALUE +01.
081413         88  WS-RESP-NOTFND              VALUE +13.
081413         88  WS-RESP-DUPKEY              VALUE +15.
081413         88  WS-RESP-NOTOPEN             VALUE +19.
081413         88  WS-RESP-ENDFILE             VALUE +20.
081413
00108  01  MISC-WORK-AREAS.
00109      05  WS-PHONE-IN         PIC 9(10).
00110      05  WS-PHONE-IN-R  REDEFINES WS-PHONE-IN.
00111          10  WSPI-AREA       PIC XXX.
00112          10  WSPI-PFX        PIC XXX.
00113          10  WSPI-SFX        PIC X(4).
00114      05  WS-PHONE-OUT.
00115          10  WSPO-AREA       PIC XXX.
00116          10  FILLER          PIC X       VALUE '-'.
00117          10  WSPO-PFX        PIC XXX.
00118          10  FILLER          PIC X       VALUE '-'.
00119          10  WSPO-SFX        PIC X(4).
00120
00121      05  DEEDIT-FIELD            PIC X(15).
00122      05  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD PIC S9(15).
00123      05  DEEDIT-FIELD-V1  REDEFINES DEEDIT-FIELD PIC S9(14)V9(1).
00124      05  DEEDIT-FIELD-V2  REDEFINES DEEDIT-FIELD PIC S9(13)V9(2).
00125      05  DEEDIT-FIELD-V3  REDEFINES DEEDIT-FIELD PIC S9(12)V9(3).
00126      05  DEEDIT-FIELD-V4  REDEFINES DEEDIT-FIELD PIC S9(11)V9(4).
00127      05  DEEDIT-FIELD-V5  REDEFINES DEEDIT-FIELD PIC S9(10)V9(5).
00128      05  W-FIELD-V0          PIC S9(15).
00129      05  W-FIELD-V1          PIC S9(14)V9(1).
00130      05  W-FIELD-V2          PIC S9(13)V9(2).
00131      05  W-FIELD-V3          PIC S9(12)V9(3).
00132      05  W-FIELD-V4          PIC S9(11)V9(4).
00133      05  W-FIELD-V5          PIC S9(10)V9(5).
00134      05  W-DECIMAL-NUMBER    PIC S9(01).
00135
00136      05  WS-SAVE-FACTOR      PIC S999V99    VALUE +0.
00137      05  WS-DEVIATE-FACTOR   PIC S9V9999    VALUE +0.
00138      05  WS-NEW-RATE         PIC S99V9(5) VALUE +0.
00139
00140      05  ERRATE-LENGTH       PIC S9(4)      VALUE +1765 COMP.
00141      05  SV-CLMTOL           PIC 999V99     VALUE ZEROS.
00142      05  DATE-TEST-AREA      PIC 9(6).
00143      05  DATE-TEST-AREA-R  REDEFINES DATE-TEST-AREA.
00144          10  DATE-TEST-MM    PIC 99.
00145          10  DATE-TEST-DD    PIC 99.
00146          10  DATE-TEST-YY    PIC 99.
00147      05  DIVIDE-RESULT       PIC 99.
00148      05  DIVIDE-REMAINDER    PIC 9.
00149      05  WS-PREV-AGE         PIC 99       VALUE ZERO.
00150      05  WS-PREV-TRM         PIC 999      VALUE ZERO.
00151      05  WS-PREV-AMT         PIC S9(7)V99 VALUE ZERO.
00152      05  WS-SAVE-KEY         PIC X(28)    VALUE SPACES.
00153      05  WS-SAVE-STRUCTURE   PIC X(7)     VALUE SPACES.
00154
00155      05  WS-STATE-FOUND-SW   PIC X        VALUE SPACE.
00156          88  STATE-FOUND                  VALUE 'Y'.
00157      05  WS-BENEFIT-FOUND-SW PIC X        VALUE SPACE.
00158          88  BENEFIT-FOUND                VALUE 'Y'.
00159      05  WS-FIRST-TIME-SW    PIC X        VALUE 'Y'.
00160          88  FIRST-TIME                   VALUE 'Y'.
00161      05  WS-FIRST-REC-SW     PIC X        VALUE 'Y'.
00162          88  FIRST-REC-IN-STRUCTURE       VALUE 'Y'.
00163      05  WS-SHOW-PLAN-SW     PIC X        VALUE SPACE.
00164          88  SHOW-AH-PLANS                VALUE '1'.
00165          88  SHOW-LF-PLANS                VALUE '2'.
00166      05  WS-EOF-DISPLAY-SW   PIC X        VALUE 'N'.
00167          88  EOF-HIT-DISPLAY-FIRST        VALUE 'Y'.
00168
00169      05  WS-CURRENT-EXPIRE       PIC 9(6)    VALUE 999999.
00170
00171      05  WS-ACCESS.
00172          10  WS-STATE-CODE       PIC XX      VALUE SPACES.
00173          10  FILLER              PIC XX      VALUE SPACES.
00174      05  WS-REC-TYPE             PIC X       VALUE SPACE.
00175      05  WS-BENEFIT-ACCESS.
00176          10  FILLER              PIC XX      VALUE SPACES.
00177          10  WS-HI-BENEFIT       PIC XX      VALUE SPACES.
00178              88  INVALID-PLAN-CODE  VALUE '  ' '00'
00179                                           '90' THRU '99'.
00180
00181      05  ELCNTL-KEY.
00182          10  CNTL-COMP-ID        PIC XXX     VALUE SPACES.
00183          10  CNTL-REC-TYPE       PIC X       VALUE SPACES.
00184          10  CNTL-ACCESS         PIC X(4)    VALUE SPACES.
00185          10  CNTL-SEQ-NO         PIC S9(4)   VALUE +0  COMP.
00186
00187      05  MISC-SAVE-AREAS.
00188          10  WS-MAX-AGE               PIC 99.
00189          10  WS-L-MORT-CODE           PIC X(4).
00190          10  WS-EXCEPTIONS            OCCURS 8 TIMES.
00191              15  WS-LIMIT-AGE         PIC 99.
00192              15  WS-LIMIT-TERM        PIC S999 COMP-3.
00193              15  WS-LIMIT-MO-BEN      PIC S9(7) COMP-3.
00194              15  WS-LIMIT-TO-BEN      PIC S9(7) COMP-3.
00195
00196  EJECT
00197 *                                  COPY ELCSCTM.
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
00198  EJECT
00199 *                                  COPY ELCSCRTY.
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
00200      EJECT
00201 *                                  COPY ELCDATE.
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
00202      EJECT
00203 *                                  COPY ELCLOGOF.
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
00204      EJECT
00205 *                                  COPY ELCATTR.
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
00206      EJECT
00207 *                                  COPY ELCEMIB.
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
00208      EJECT
00209 *                                  COPY ELCINTF.
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
00210      12  PI-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.
00211          16  PI-FILE-ID                    PIC XX.
00212          16  PI-MAINT                      PIC X.
00213              88  VALID-MAINT-TYPE                VALUE 'S' 'A' 'C'
00214                                                    'D' 'P' 'E'.
00215              88  ADD-FUNCTION                    VALUE 'A'.
00216              88  SHOW-FUNCTION                   VALUE 'S'.
00217              88  DELETE-FUNCTION                 VALUE 'D'.
00218              88  CHANGE-FUNCTION                 VALUE 'C'.
00219              88  COPY-FUNCTION                   VALUE 'P'.
00220              88  DEVIATE-FUNCTION                VALUE 'E'.
00221
00222          16  PI-ERRATE-KEY.
00223              20  PI-RATE-COMPANY-CD         PIC X.
00224              20  PI-RATE-STATE-CODE.
00225                  24  PI-RATE-CODE           PIC XX.
00226                  24  PI-RATE-CLASS          PIC XX.
00227                  24  PI-RATE-DEV            PIC XXX.
00228              20  PI-RATE-L-AH-CODE.
00229                  24  PI-RATE-L-AH           PIC X.
00230                  24  PI-RATE-LAH-NUM        PIC XX.
00231              20  PI-RATE-LIMITS.
00232                  24  PI-RATE-HIGH-AGE       PIC 99.
00233                  24  PI-RATE-HIGH-AMT       PIC 9(6).
00234                  24  PI-RATE-FUTURE         PIC XX.
00235                  24  PI-RATE-SEX            PIC X.
00236              20  PI-RATE-EXPIRY-DATE        PIC 9(11) COMP-3.
00237
00238          16  PI-SAVE-ERRATE-KEY             PIC X(28).
00239
00240          16  PI-BROWSE-SW                  PIC X.
00241              88  BROWSE-STARTED                  VALUE 'Y'.
00242          16  PI-ERRATE-EOF-SW              PIC X.
00243              88  ERRATE-EOF                      VALUE 'Y'.
00244          16  PI-SHOW-SW                    PIC X.
00245              88  SHOWN-ONCE                      VALUE 'Y'.
00246          16  PI-RETURN-SW                  PIC X.
00247              88  RETURN-FROM-LIMITS              VALUE 'Y'.
00248          16  PI-FIRST-TIME-SW              PIC X.
00249              88  FIRST-PLAN-SHOWN                VALUE 'Y'.
081413         16  pi-delete-sw                  pic x.
081413             88  FIRST-REQUEST VALUE ' '.
081413             88  DELETE-OK     VALUE 'Y'.
081413         16  FILLER                        PIC X(575).
00251
00252      EJECT
00253 *                            COPY ELCJPFX.
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
081413                             PIC X(2000).
00255
00256      EJECT
00257 *                            COPY ELCAID.
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
00258
00259  01  FILLER    REDEFINES DFHAID.
00260      05  FILLER              PIC X(8).
00261      05  PF-VALUES           PIC X       OCCURS 2.
00262
00263      EJECT
00264 *                           COPY EL656S.
       01  EL656AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  RUNDATEL PIC S9(0004) COMP.
           05  RUNDATEF PIC  X(0001).
           05  FILLER REDEFINES RUNDATEF.
               10  RUNDATEA PIC  X(0001).
           05  RUNDATEI PIC  X(0008).
      *    -------------------------------
           05  RUNTIMEL PIC S9(0004) COMP.
           05  RUNTIMEF PIC  X(0001).
           05  FILLER REDEFINES RUNTIMEF.
               10  RUNTIMEA PIC  X(0001).
           05  RUNTIMEI PIC  X(0005).
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
           05  STATE1L PIC S9(0004) COMP.
           05  STATE1F PIC  X(0001).
           05  FILLER REDEFINES STATE1F.
               10  STATE1A PIC  X(0001).
           05  STATE1I PIC  X(0002).
      *    -------------------------------
           05  CLASS1L PIC S9(0004) COMP.
           05  CLASS1F PIC  X(0001).
           05  FILLER REDEFINES CLASS1F.
               10  CLASS1A PIC  X(0001).
           05  CLASS1I PIC  X(0002).
      *    -------------------------------
           05  DEV1L PIC S9(0004) COMP.
           05  DEV1F PIC  X(0001).
           05  FILLER REDEFINES DEV1F.
               10  DEV1A PIC  X(0001).
           05  DEV1I PIC  X(0003).
      *    -------------------------------
           05  TYPE1L PIC S9(0004) COMP.
           05  TYPE1F PIC  X(0001).
           05  FILLER REDEFINES TYPE1F.
               10  TYPE1A PIC  X(0001).
           05  TYPE1I PIC  X(0001).
      *    -------------------------------
           05  PLAN1L PIC S9(0004) COMP.
           05  PLAN1F PIC  X(0001).
           05  FILLER REDEFINES PLAN1F.
               10  PLAN1A PIC  X(0001).
           05  PLAN1I PIC  X(0002).
      *    -------------------------------
           05  RTAGE1L PIC S9(0004) COMP.
           05  RTAGE1F PIC  X(0001).
           05  FILLER REDEFINES RTAGE1F.
               10  RTAGE1A PIC  X(0001).
           05  RTAGE1I PIC  99.
      *    -------------------------------
           05  EXPIRE1L PIC S9(0004) COMP.
           05  EXPIRE1F PIC  X(0001).
           05  FILLER REDEFINES EXPIRE1F.
               10  EXPIRE1A PIC  X(0001).
           05  EXPIRE1I PIC  9(6).
      *    -------------------------------
           05  RTAMT1L PIC S9(0004) COMP.
           05  RTAMT1F PIC  X(0001).
           05  FILLER REDEFINES RTAMT1F.
               10  RTAMT1A PIC  X(0001).
           05  RTAMT1I PIC  9999999.
      *    -------------------------------
           05  STATE2L PIC S9(0004) COMP.
           05  STATE2F PIC  X(0001).
           05  FILLER REDEFINES STATE2F.
               10  STATE2A PIC  X(0001).
           05  STATE2I PIC  X(0002).
      *    -------------------------------
           05  CLASS2L PIC S9(0004) COMP.
           05  CLASS2F PIC  X(0001).
           05  FILLER REDEFINES CLASS2F.
               10  CLASS2A PIC  X(0001).
           05  CLASS2I PIC  X(0002).
      *    -------------------------------
           05  DEV2L PIC S9(0004) COMP.
           05  DEV2F PIC  X(0001).
           05  FILLER REDEFINES DEV2F.
               10  DEV2A PIC  X(0001).
           05  DEV2I PIC  X(0003).
      *    -------------------------------
           05  TYPE2L PIC S9(0004) COMP.
           05  TYPE2F PIC  X(0001).
           05  FILLER REDEFINES TYPE2F.
               10  TYPE2A PIC  X(0001).
           05  TYPE2I PIC  X(0001).
      *    -------------------------------
           05  PLAN2L PIC S9(0004) COMP.
           05  PLAN2F PIC  X(0001).
           05  FILLER REDEFINES PLAN2F.
               10  PLAN2A PIC  X(0001).
           05  PLAN2I PIC  X(0002).
      *    -------------------------------
           05  RTAGE2L PIC S9(0004) COMP.
           05  RTAGE2F PIC  X(0001).
           05  FILLER REDEFINES RTAGE2F.
               10  RTAGE2A PIC  X(0001).
           05  RTAGE2I PIC  99.
      *    -------------------------------
           05  EXPIRE2L PIC S9(0004) COMP.
           05  EXPIRE2F PIC  X(0001).
           05  FILLER REDEFINES EXPIRE2F.
               10  EXPIRE2A PIC  X(0001).
           05  EXPIRE2I PIC  9(6).
      *    -------------------------------
           05  RTAMT2L PIC S9(0004) COMP.
           05  RTAMT2F PIC  X(0001).
           05  FILLER REDEFINES RTAMT2F.
               10  RTAMT2A PIC  X(0001).
           05  RTAMT2I PIC  9999999.
      *    -------------------------------
           05  DEVPCTL PIC S9(0004) COMP.
           05  DEVPCTF PIC  X(0001).
           05  FILLER REDEFINES DEVPCTF.
               10  DEVPCTA PIC  X(0001).
           05  DEVPCTI PIC  9(6).
      *    -------------------------------
           05  DECINUML PIC S9(0004) COMP.
           05  DECINUMF PIC  X(0001).
           05  FILLER REDEFINES DECINUMF.
               10  DECINUMA PIC  X(0001).
           05  DECINUMI PIC  X(0001).
      *    -------------------------------
           05  STCOMML PIC S9(0004) COMP.
           05  STCOMMF PIC  X(0001).
           05  FILLER REDEFINES STCOMMF.
               10  STCOMMA PIC  X(0001).
           05  STCOMMI PIC  X(0050).
      *    -------------------------------
           05  PLANTYPL PIC S9(0004) COMP.
           05  PLANTYPF PIC  X(0001).
           05  FILLER REDEFINES PLANTYPF.
               10  PLANTYPA PIC  X(0001).
           05  PLANTYPI PIC  X(0006).
      *    -------------------------------
           05  PLCOD01L PIC S9(0004) COMP.
           05  PLCOD01F PIC  X(0001).
           05  FILLER REDEFINES PLCOD01F.
               10  PLCOD01A PIC  X(0001).
           05  PLCOD01I PIC  X(0002).
      *    -------------------------------
           05  PLDES01L PIC S9(0004) COMP.
           05  PLDES01F PIC  X(0001).
           05  FILLER REDEFINES PLDES01F.
               10  PLDES01A PIC  X(0001).
           05  PLDES01I PIC  X(0010).
      *    -------------------------------
           05  PLAGE01L PIC S9(0004) COMP.
           05  PLAGE01F PIC  X(0001).
           05  FILLER REDEFINES PLAGE01F.
               10  PLAGE01A PIC  X(0001).
           05  PLAGE01I PIC  X(0002).
      *    -------------------------------
           05  PLEXP01L PIC S9(0004) COMP.
           05  PLEXP01F PIC  X(0001).
           05  FILLER REDEFINES PLEXP01F.
               10  PLEXP01A PIC  X(0001).
           05  PLEXP01I PIC  X(0008).
      *    -------------------------------
           05  PLCOD02L PIC S9(0004) COMP.
           05  PLCOD02F PIC  X(0001).
           05  FILLER REDEFINES PLCOD02F.
               10  PLCOD02A PIC  X(0001).
           05  PLCOD02I PIC  X(0002).
      *    -------------------------------
           05  PLDES02L PIC S9(0004) COMP.
           05  PLDES02F PIC  X(0001).
           05  FILLER REDEFINES PLDES02F.
               10  PLDES02A PIC  X(0001).
           05  PLDES02I PIC  X(0010).
      *    -------------------------------
           05  PLAGE02L PIC S9(0004) COMP.
           05  PLAGE02F PIC  X(0001).
           05  FILLER REDEFINES PLAGE02F.
               10  PLAGE02A PIC  X(0001).
           05  PLAGE02I PIC  X(0002).
      *    -------------------------------
           05  PLEXP02L PIC S9(0004) COMP.
           05  PLEXP02F PIC  X(0001).
           05  FILLER REDEFINES PLEXP02F.
               10  PLEXP02A PIC  X(0001).
           05  PLEXP02I PIC  X(0008).
      *    -------------------------------
           05  PLCOD03L PIC S9(0004) COMP.
           05  PLCOD03F PIC  X(0001).
           05  FILLER REDEFINES PLCOD03F.
               10  PLCOD03A PIC  X(0001).
           05  PLCOD03I PIC  X(0002).
      *    -------------------------------
           05  PLDES03L PIC S9(0004) COMP.
           05  PLDES03F PIC  X(0001).
           05  FILLER REDEFINES PLDES03F.
               10  PLDES03A PIC  X(0001).
           05  PLDES03I PIC  X(0010).
      *    -------------------------------
           05  PLAGE03L PIC S9(0004) COMP.
           05  PLAGE03F PIC  X(0001).
           05  FILLER REDEFINES PLAGE03F.
               10  PLAGE03A PIC  X(0001).
           05  PLAGE03I PIC  X(0002).
      *    -------------------------------
           05  PLEXP03L PIC S9(0004) COMP.
           05  PLEXP03F PIC  X(0001).
           05  FILLER REDEFINES PLEXP03F.
               10  PLEXP03A PIC  X(0001).
           05  PLEXP03I PIC  X(0008).
      *    -------------------------------
           05  PLCOD04L PIC S9(0004) COMP.
           05  PLCOD04F PIC  X(0001).
           05  FILLER REDEFINES PLCOD04F.
               10  PLCOD04A PIC  X(0001).
           05  PLCOD04I PIC  X(0002).
      *    -------------------------------
           05  PLDES04L PIC S9(0004) COMP.
           05  PLDES04F PIC  X(0001).
           05  FILLER REDEFINES PLDES04F.
               10  PLDES04A PIC  X(0001).
           05  PLDES04I PIC  X(0010).
      *    -------------------------------
           05  PLAGE04L PIC S9(0004) COMP.
           05  PLAGE04F PIC  X(0001).
           05  FILLER REDEFINES PLAGE04F.
               10  PLAGE04A PIC  X(0001).
           05  PLAGE04I PIC  X(0002).
      *    -------------------------------
           05  PLEXP04L PIC S9(0004) COMP.
           05  PLEXP04F PIC  X(0001).
           05  FILLER REDEFINES PLEXP04F.
               10  PLEXP04A PIC  X(0001).
           05  PLEXP04I PIC  X(0008).
      *    -------------------------------
           05  PLCOD05L PIC S9(0004) COMP.
           05  PLCOD05F PIC  X(0001).
           05  FILLER REDEFINES PLCOD05F.
               10  PLCOD05A PIC  X(0001).
           05  PLCOD05I PIC  X(0002).
      *    -------------------------------
           05  PLDES05L PIC S9(0004) COMP.
           05  PLDES05F PIC  X(0001).
           05  FILLER REDEFINES PLDES05F.
               10  PLDES05A PIC  X(0001).
           05  PLDES05I PIC  X(0010).
      *    -------------------------------
           05  PLAGE05L PIC S9(0004) COMP.
           05  PLAGE05F PIC  X(0001).
           05  FILLER REDEFINES PLAGE05F.
               10  PLAGE05A PIC  X(0001).
           05  PLAGE05I PIC  X(0002).
      *    -------------------------------
           05  PLEXP05L PIC S9(0004) COMP.
           05  PLEXP05F PIC  X(0001).
           05  FILLER REDEFINES PLEXP05F.
               10  PLEXP05A PIC  X(0001).
           05  PLEXP05I PIC  X(0008).
      *    -------------------------------
           05  PLCOD06L PIC S9(0004) COMP.
           05  PLCOD06F PIC  X(0001).
           05  FILLER REDEFINES PLCOD06F.
               10  PLCOD06A PIC  X(0001).
           05  PLCOD06I PIC  X(0002).
      *    -------------------------------
           05  PLDES06L PIC S9(0004) COMP.
           05  PLDES06F PIC  X(0001).
           05  FILLER REDEFINES PLDES06F.
               10  PLDES06A PIC  X(0001).
           05  PLDES06I PIC  X(0010).
      *    -------------------------------
           05  PLAGE06L PIC S9(0004) COMP.
           05  PLAGE06F PIC  X(0001).
           05  FILLER REDEFINES PLAGE06F.
               10  PLAGE06A PIC  X(0001).
           05  PLAGE06I PIC  X(0002).
      *    -------------------------------
           05  PLEXP06L PIC S9(0004) COMP.
           05  PLEXP06F PIC  X(0001).
           05  FILLER REDEFINES PLEXP06F.
               10  PLEXP06A PIC  X(0001).
           05  PLEXP06I PIC  X(0008).
      *    -------------------------------
           05  PLCOD07L PIC S9(0004) COMP.
           05  PLCOD07F PIC  X(0001).
           05  FILLER REDEFINES PLCOD07F.
               10  PLCOD07A PIC  X(0001).
           05  PLCOD07I PIC  X(0002).
      *    -------------------------------
           05  PLDES07L PIC S9(0004) COMP.
           05  PLDES07F PIC  X(0001).
           05  FILLER REDEFINES PLDES07F.
               10  PLDES07A PIC  X(0001).
           05  PLDES07I PIC  X(0010).
      *    -------------------------------
           05  PLAGE07L PIC S9(0004) COMP.
           05  PLAGE07F PIC  X(0001).
           05  FILLER REDEFINES PLAGE07F.
               10  PLAGE07A PIC  X(0001).
           05  PLAGE07I PIC  X(0002).
      *    -------------------------------
           05  PLEXP07L PIC S9(0004) COMP.
           05  PLEXP07F PIC  X(0001).
           05  FILLER REDEFINES PLEXP07F.
               10  PLEXP07A PIC  X(0001).
           05  PLEXP07I PIC  X(0008).
      *    -------------------------------
           05  PLCOD08L PIC S9(0004) COMP.
           05  PLCOD08F PIC  X(0001).
           05  FILLER REDEFINES PLCOD08F.
               10  PLCOD08A PIC  X(0001).
           05  PLCOD08I PIC  X(0002).
      *    -------------------------------
           05  PLDES08L PIC S9(0004) COMP.
           05  PLDES08F PIC  X(0001).
           05  FILLER REDEFINES PLDES08F.
               10  PLDES08A PIC  X(0001).
           05  PLDES08I PIC  X(0010).
      *    -------------------------------
           05  PLAGE08L PIC S9(0004) COMP.
           05  PLAGE08F PIC  X(0001).
           05  FILLER REDEFINES PLAGE08F.
               10  PLAGE08A PIC  X(0001).
           05  PLAGE08I PIC  X(0002).
      *    -------------------------------
           05  PLEXP08L PIC S9(0004) COMP.
           05  PLEXP08F PIC  X(0001).
           05  FILLER REDEFINES PLEXP08F.
               10  PLEXP08A PIC  X(0001).
           05  PLEXP08I PIC  X(0008).
      *    -------------------------------
           05  PLCOD09L PIC S9(0004) COMP.
           05  PLCOD09F PIC  X(0001).
           05  FILLER REDEFINES PLCOD09F.
               10  PLCOD09A PIC  X(0001).
           05  PLCOD09I PIC  X(0002).
      *    -------------------------------
           05  PLDES09L PIC S9(0004) COMP.
           05  PLDES09F PIC  X(0001).
           05  FILLER REDEFINES PLDES09F.
               10  PLDES09A PIC  X(0001).
           05  PLDES09I PIC  X(0010).
      *    -------------------------------
           05  PLAGE09L PIC S9(0004) COMP.
           05  PLAGE09F PIC  X(0001).
           05  FILLER REDEFINES PLAGE09F.
               10  PLAGE09A PIC  X(0001).
           05  PLAGE09I PIC  X(0002).
      *    -------------------------------
           05  PLEXP09L PIC S9(0004) COMP.
           05  PLEXP09F PIC  X(0001).
           05  FILLER REDEFINES PLEXP09F.
               10  PLEXP09A PIC  X(0001).
           05  PLEXP09I PIC  X(0008).
      *    -------------------------------
           05  PLCOD10L PIC S9(0004) COMP.
           05  PLCOD10F PIC  X(0001).
           05  FILLER REDEFINES PLCOD10F.
               10  PLCOD10A PIC  X(0001).
           05  PLCOD10I PIC  X(0002).
      *    -------------------------------
           05  PLDES10L PIC S9(0004) COMP.
           05  PLDES10F PIC  X(0001).
           05  FILLER REDEFINES PLDES10F.
               10  PLDES10A PIC  X(0001).
           05  PLDES10I PIC  X(0010).
      *    -------------------------------
           05  PLAGE10L PIC S9(0004) COMP.
           05  PLAGE10F PIC  X(0001).
           05  FILLER REDEFINES PLAGE10F.
               10  PLAGE10A PIC  X(0001).
           05  PLAGE10I PIC  X(0002).
      *    -------------------------------
           05  PLEXP10L PIC S9(0004) COMP.
           05  PLEXP10F PIC  X(0001).
           05  FILLER REDEFINES PLEXP10F.
               10  PLEXP10A PIC  X(0001).
           05  PLEXP10I PIC  X(0008).
      *    -------------------------------
           05  PLCOD11L PIC S9(0004) COMP.
           05  PLCOD11F PIC  X(0001).
           05  FILLER REDEFINES PLCOD11F.
               10  PLCOD11A PIC  X(0001).
           05  PLCOD11I PIC  X(0002).
      *    -------------------------------
           05  PLDES11L PIC S9(0004) COMP.
           05  PLDES11F PIC  X(0001).
           05  FILLER REDEFINES PLDES11F.
               10  PLDES11A PIC  X(0001).
           05  PLDES11I PIC  X(0010).
      *    -------------------------------
           05  PLAGE11L PIC S9(0004) COMP.
           05  PLAGE11F PIC  X(0001).
           05  FILLER REDEFINES PLAGE11F.
               10  PLAGE11A PIC  X(0001).
           05  PLAGE11I PIC  X(0002).
      *    -------------------------------
           05  PLEXP11L PIC S9(0004) COMP.
           05  PLEXP11F PIC  X(0001).
           05  FILLER REDEFINES PLEXP11F.
               10  PLEXP11A PIC  X(0001).
           05  PLEXP11I PIC  X(0008).
      *    -------------------------------
           05  PLCOD12L PIC S9(0004) COMP.
           05  PLCOD12F PIC  X(0001).
           05  FILLER REDEFINES PLCOD12F.
               10  PLCOD12A PIC  X(0001).
           05  PLCOD12I PIC  X(0002).
      *    -------------------------------
           05  PLDES12L PIC S9(0004) COMP.
           05  PLDES12F PIC  X(0001).
           05  FILLER REDEFINES PLDES12F.
               10  PLDES12A PIC  X(0001).
           05  PLDES12I PIC  X(0010).
      *    -------------------------------
           05  PLAGE12L PIC S9(0004) COMP.
           05  PLAGE12F PIC  X(0001).
           05  FILLER REDEFINES PLAGE12F.
               10  PLAGE12A PIC  X(0001).
           05  PLAGE12I PIC  X(0002).
      *    -------------------------------
           05  PLEXP12L PIC S9(0004) COMP.
           05  PLEXP12F PIC  X(0001).
           05  FILLER REDEFINES PLEXP12F.
               10  PLEXP12A PIC  X(0001).
           05  PLEXP12I PIC  X(0008).
      *    -------------------------------
           05  PLCOD13L PIC S9(0004) COMP.
           05  PLCOD13F PIC  X(0001).
           05  FILLER REDEFINES PLCOD13F.
               10  PLCOD13A PIC  X(0001).
           05  PLCOD13I PIC  X(0002).
      *    -------------------------------
           05  PLDES13L PIC S9(0004) COMP.
           05  PLDES13F PIC  X(0001).
           05  FILLER REDEFINES PLDES13F.
               10  PLDES13A PIC  X(0001).
           05  PLDES13I PIC  X(0010).
      *    -------------------------------
           05  PLAGE13L PIC S9(0004) COMP.
           05  PLAGE13F PIC  X(0001).
           05  FILLER REDEFINES PLAGE13F.
               10  PLAGE13A PIC  X(0001).
           05  PLAGE13I PIC  X(0002).
      *    -------------------------------
           05  PLEXP13L PIC S9(0004) COMP.
           05  PLEXP13F PIC  X(0001).
           05  FILLER REDEFINES PLEXP13F.
               10  PLEXP13A PIC  X(0001).
           05  PLEXP13I PIC  X(0008).
      *    -------------------------------
           05  PLCOD14L PIC S9(0004) COMP.
           05  PLCOD14F PIC  X(0001).
           05  FILLER REDEFINES PLCOD14F.
               10  PLCOD14A PIC  X(0001).
           05  PLCOD14I PIC  X(0002).
      *    -------------------------------
           05  PLDES14L PIC S9(0004) COMP.
           05  PLDES14F PIC  X(0001).
           05  FILLER REDEFINES PLDES14F.
               10  PLDES14A PIC  X(0001).
           05  PLDES14I PIC  X(0010).
      *    -------------------------------
           05  PLAGE14L PIC S9(0004) COMP.
           05  PLAGE14F PIC  X(0001).
           05  FILLER REDEFINES PLAGE14F.
               10  PLAGE14A PIC  X(0001).
           05  PLAGE14I PIC  X(0002).
      *    -------------------------------
           05  PLEXP14L PIC S9(0004) COMP.
           05  PLEXP14F PIC  X(0001).
           05  FILLER REDEFINES PLEXP14F.
               10  PLEXP14A PIC  X(0001).
           05  PLEXP14I PIC  X(0008).
      *    -------------------------------
           05  PLCOD15L PIC S9(0004) COMP.
           05  PLCOD15F PIC  X(0001).
           05  FILLER REDEFINES PLCOD15F.
               10  PLCOD15A PIC  X(0001).
           05  PLCOD15I PIC  X(0002).
      *    -------------------------------
           05  PLDES15L PIC S9(0004) COMP.
           05  PLDES15F PIC  X(0001).
           05  FILLER REDEFINES PLDES15F.
               10  PLDES15A PIC  X(0001).
           05  PLDES15I PIC  X(0010).
      *    -------------------------------
           05  PLAGE15L PIC S9(0004) COMP.
           05  PLAGE15F PIC  X(0001).
           05  FILLER REDEFINES PLAGE15F.
               10  PLAGE15A PIC  X(0001).
           05  PLAGE15I PIC  X(0002).
      *    -------------------------------
           05  PLEXP15L PIC S9(0004) COMP.
           05  PLEXP15F PIC  X(0001).
           05  FILLER REDEFINES PLEXP15F.
               10  PLEXP15A PIC  X(0001).
           05  PLEXP15I PIC  X(0008).
      *    -------------------------------
           05  PLCOD16L PIC S9(0004) COMP.
           05  PLCOD16F PIC  X(0001).
           05  FILLER REDEFINES PLCOD16F.
               10  PLCOD16A PIC  X(0001).
           05  PLCOD16I PIC  X(0002).
      *    -------------------------------
           05  PLDES16L PIC S9(0004) COMP.
           05  PLDES16F PIC  X(0001).
           05  FILLER REDEFINES PLDES16F.
               10  PLDES16A PIC  X(0001).
           05  PLDES16I PIC  X(0010).
      *    -------------------------------
           05  PLAGE16L PIC S9(0004) COMP.
           05  PLAGE16F PIC  X(0001).
           05  FILLER REDEFINES PLAGE16F.
               10  PLAGE16A PIC  X(0001).
           05  PLAGE16I PIC  X(0002).
      *    -------------------------------
           05  PLEXP16L PIC S9(0004) COMP.
           05  PLEXP16F PIC  X(0001).
           05  FILLER REDEFINES PLEXP16F.
               10  PLEXP16A PIC  X(0001).
           05  PLEXP16I PIC  X(0008).
      *    -------------------------------
           05  PLCOD17L PIC S9(0004) COMP.
           05  PLCOD17F PIC  X(0001).
           05  FILLER REDEFINES PLCOD17F.
               10  PLCOD17A PIC  X(0001).
           05  PLCOD17I PIC  X(0002).
      *    -------------------------------
           05  PLDES17L PIC S9(0004) COMP.
           05  PLDES17F PIC  X(0001).
           05  FILLER REDEFINES PLDES17F.
               10  PLDES17A PIC  X(0001).
           05  PLDES17I PIC  X(0010).
      *    -------------------------------
           05  PLAGE17L PIC S9(0004) COMP.
           05  PLAGE17F PIC  X(0001).
           05  FILLER REDEFINES PLAGE17F.
               10  PLAGE17A PIC  X(0001).
           05  PLAGE17I PIC  X(0002).
      *    -------------------------------
           05  PLEXP17L PIC S9(0004) COMP.
           05  PLEXP17F PIC  X(0001).
           05  FILLER REDEFINES PLEXP17F.
               10  PLEXP17A PIC  X(0001).
           05  PLEXP17I PIC  X(0008).
      *    -------------------------------
           05  PLCOD18L PIC S9(0004) COMP.
           05  PLCOD18F PIC  X(0001).
           05  FILLER REDEFINES PLCOD18F.
               10  PLCOD18A PIC  X(0001).
           05  PLCOD18I PIC  X(0002).
      *    -------------------------------
           05  PLDES18L PIC S9(0004) COMP.
           05  PLDES18F PIC  X(0001).
           05  FILLER REDEFINES PLDES18F.
               10  PLDES18A PIC  X(0001).
           05  PLDES18I PIC  X(0010).
      *    -------------------------------
           05  PLAGE18L PIC S9(0004) COMP.
           05  PLAGE18F PIC  X(0001).
           05  FILLER REDEFINES PLAGE18F.
               10  PLAGE18A PIC  X(0001).
           05  PLAGE18I PIC  X(0002).
      *    -------------------------------
           05  PLEXP18L PIC S9(0004) COMP.
           05  PLEXP18F PIC  X(0001).
           05  FILLER REDEFINES PLEXP18F.
               10  PLEXP18A PIC  X(0001).
           05  PLEXP18I PIC  X(0008).
      *    -------------------------------
           05  PLCOD19L PIC S9(0004) COMP.
           05  PLCOD19F PIC  X(0001).
           05  FILLER REDEFINES PLCOD19F.
               10  PLCOD19A PIC  X(0001).
           05  PLCOD19I PIC  X(0002).
      *    -------------------------------
           05  PLDES19L PIC S9(0004) COMP.
           05  PLDES19F PIC  X(0001).
           05  FILLER REDEFINES PLDES19F.
               10  PLDES19A PIC  X(0001).
           05  PLDES19I PIC  X(0010).
      *    -------------------------------
           05  PLAGE19L PIC S9(0004) COMP.
           05  PLAGE19F PIC  X(0001).
           05  FILLER REDEFINES PLAGE19F.
               10  PLAGE19A PIC  X(0001).
           05  PLAGE19I PIC  X(0002).
      *    -------------------------------
           05  PLEXP19L PIC S9(0004) COMP.
           05  PLEXP19F PIC  X(0001).
           05  FILLER REDEFINES PLEXP19F.
               10  PLEXP19A PIC  X(0001).
           05  PLEXP19I PIC  X(0008).
      *    -------------------------------
           05  PLCOD20L PIC S9(0004) COMP.
           05  PLCOD20F PIC  X(0001).
           05  FILLER REDEFINES PLCOD20F.
               10  PLCOD20A PIC  X(0001).
           05  PLCOD20I PIC  X(0002).
      *    -------------------------------
           05  PLDES20L PIC S9(0004) COMP.
           05  PLDES20F PIC  X(0001).
           05  FILLER REDEFINES PLDES20F.
               10  PLDES20A PIC  X(0001).
           05  PLDES20I PIC  X(0010).
      *    -------------------------------
           05  PLAGE20L PIC S9(0004) COMP.
           05  PLAGE20F PIC  X(0001).
           05  FILLER REDEFINES PLAGE20F.
               10  PLAGE20A PIC  X(0001).
           05  PLAGE20I PIC  X(0002).
      *    -------------------------------
           05  PLEXP20L PIC S9(0004) COMP.
           05  PLEXP20F PIC  X(0001).
           05  FILLER REDEFINES PLEXP20F.
               10  PLEXP20A PIC  X(0001).
           05  PLEXP20I PIC  X(0008).
      *    -------------------------------
           05  PLCOD21L PIC S9(0004) COMP.
           05  PLCOD21F PIC  X(0001).
           05  FILLER REDEFINES PLCOD21F.
               10  PLCOD21A PIC  X(0001).
           05  PLCOD21I PIC  X(0002).
      *    -------------------------------
           05  PLDES21L PIC S9(0004) COMP.
           05  PLDES21F PIC  X(0001).
           05  FILLER REDEFINES PLDES21F.
               10  PLDES21A PIC  X(0001).
           05  PLDES21I PIC  X(0010).
      *    -------------------------------
           05  PLAGE21L PIC S9(0004) COMP.
           05  PLAGE21F PIC  X(0001).
           05  FILLER REDEFINES PLAGE21F.
               10  PLAGE21A PIC  X(0001).
           05  PLAGE21I PIC  X(0002).
      *    -------------------------------
           05  PLEXP21L PIC S9(0004) COMP.
           05  PLEXP21F PIC  X(0001).
           05  FILLER REDEFINES PLEXP21F.
               10  PLEXP21A PIC  X(0001).
           05  PLEXP21I PIC  X(0008).
      *    -------------------------------
           05  PLCOD22L PIC S9(0004) COMP.
           05  PLCOD22F PIC  X(0001).
           05  FILLER REDEFINES PLCOD22F.
               10  PLCOD22A PIC  X(0001).
           05  PLCOD22I PIC  X(0002).
      *    -------------------------------
           05  PLDES22L PIC S9(0004) COMP.
           05  PLDES22F PIC  X(0001).
           05  FILLER REDEFINES PLDES22F.
               10  PLDES22A PIC  X(0001).
           05  PLDES22I PIC  X(0010).
      *    -------------------------------
           05  PLAGE22L PIC S9(0004) COMP.
           05  PLAGE22F PIC  X(0001).
           05  FILLER REDEFINES PLAGE22F.
               10  PLAGE22A PIC  X(0001).
           05  PLAGE22I PIC  X(0002).
      *    -------------------------------
           05  PLEXP22L PIC S9(0004) COMP.
           05  PLEXP22F PIC  X(0001).
           05  FILLER REDEFINES PLEXP22F.
               10  PLEXP22A PIC  X(0001).
           05  PLEXP22I PIC  X(0008).
      *    -------------------------------
           05  PLCOD23L PIC S9(0004) COMP.
           05  PLCOD23F PIC  X(0001).
           05  FILLER REDEFINES PLCOD23F.
               10  PLCOD23A PIC  X(0001).
           05  PLCOD23I PIC  X(0002).
      *    -------------------------------
           05  PLDES23L PIC S9(0004) COMP.
           05  PLDES23F PIC  X(0001).
           05  FILLER REDEFINES PLDES23F.
               10  PLDES23A PIC  X(0001).
           05  PLDES23I PIC  X(0010).
      *    -------------------------------
           05  PLAGE23L PIC S9(0004) COMP.
           05  PLAGE23F PIC  X(0001).
           05  FILLER REDEFINES PLAGE23F.
               10  PLAGE23A PIC  X(0001).
           05  PLAGE23I PIC  X(0002).
      *    -------------------------------
           05  PLEXP23L PIC S9(0004) COMP.
           05  PLEXP23F PIC  X(0001).
           05  FILLER REDEFINES PLEXP23F.
               10  PLEXP23A PIC  X(0001).
           05  PLEXP23I PIC  X(0008).
      *    -------------------------------
           05  PLCOD24L PIC S9(0004) COMP.
           05  PLCOD24F PIC  X(0001).
           05  FILLER REDEFINES PLCOD24F.
               10  PLCOD24A PIC  X(0001).
           05  PLCOD24I PIC  X(0002).
      *    -------------------------------
           05  PLDES24L PIC S9(0004) COMP.
           05  PLDES24F PIC  X(0001).
           05  FILLER REDEFINES PLDES24F.
               10  PLDES24A PIC  X(0001).
           05  PLDES24I PIC  X(0010).
      *    -------------------------------
           05  PLAGE24L PIC S9(0004) COMP.
           05  PLAGE24F PIC  X(0001).
           05  FILLER REDEFINES PLAGE24F.
               10  PLAGE24A PIC  X(0001).
           05  PLAGE24I PIC  X(0002).
      *    -------------------------------
           05  PLEXP24L PIC S9(0004) COMP.
           05  PLEXP24F PIC  X(0001).
           05  FILLER REDEFINES PLEXP24F.
               10  PLEXP24A PIC  X(0001).
           05  PLEXP24I PIC  X(0008).
      *    -------------------------------
           05  LUDATEL PIC S9(0004) COMP.
           05  LUDATEF PIC  X(0001).
           05  FILLER REDEFINES LUDATEF.
               10  LUDATEA PIC  X(0001).
           05  LUDATEI PIC  X(0008).
      *    -------------------------------
           05  LUTIMEL PIC S9(0004) COMP.
           05  LUTIMEF PIC  X(0001).
           05  FILLER REDEFINES LUTIMEF.
               10  LUTIMEA PIC  X(0001).
           05  LUTIMEI PIC  X(0005).
      *    -------------------------------
           05  LUBYL PIC S9(0004) COMP.
           05  LUBYF PIC  X(0001).
           05  FILLER REDEFINES LUBYF.
               10  LUBYA PIC  X(0001).
           05  LUBYI PIC  X(0004).
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0076).
      *    -------------------------------
           05  PFENTERL PIC S9(0004) COMP.
           05  PFENTERF PIC  X(0001).
           05  FILLER REDEFINES PFENTERF.
               10  PFENTERA PIC  X(0001).
           05  PFENTERI PIC  9(2).
      *    -------------------------------
           05  AHPLANL PIC S9(0004) COMP.
           05  AHPLANF PIC  X(0001).
           05  FILLER REDEFINES AHPLANF.
               10  AHPLANA PIC  X(0001).
           05  AHPLANI PIC  X(0006).
      *    -------------------------------
           05  LFPLANL PIC S9(0004) COMP.
           05  LFPLANF PIC  X(0001).
           05  FILLER REDEFINES LFPLANF.
               10  LFPLANA PIC  X(0001).
           05  LFPLANI PIC  X(0006).
       01  EL656AO REDEFINES EL656AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNTIMEO PIC  99.99.
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
           05  STATE1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLASS1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DEV1O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLAN1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTAGE1O PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXPIRE1O PIC  9(6).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTAMT1O PIC  ZZZ,999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATE2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLASS2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DEV2O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLAN2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTAGE2O PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXPIRE2O PIC  9(6).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTAMT2O PIC  ZZZ,999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DEVPCTO PIC  999.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DECINUMO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STCOMMO PIC  X(0050).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLANTYPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLCOD01O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLDES01O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLAGE01O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLEXP01O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLCOD02O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLDES02O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLAGE02O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLEXP02O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLCOD03O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLDES03O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLAGE03O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLEXP03O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLCOD04O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLDES04O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLAGE04O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLEXP04O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLCOD05O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLDES05O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLAGE05O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLEXP05O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLCOD06O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLDES06O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLAGE06O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLEXP06O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLCOD07O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLDES07O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLAGE07O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLEXP07O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLCOD08O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLDES08O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLAGE08O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLEXP08O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLCOD09O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLDES09O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLAGE09O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLEXP09O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLCOD10O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLDES10O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLAGE10O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLEXP10O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLCOD11O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLDES11O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLAGE11O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLEXP11O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLCOD12O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLDES12O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLAGE12O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLEXP12O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLCOD13O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLDES13O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLAGE13O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLEXP13O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLCOD14O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLDES14O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLAGE14O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLEXP14O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLCOD15O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLDES15O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLAGE15O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLEXP15O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLCOD16O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLDES16O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLAGE16O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLEXP16O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLCOD17O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLDES17O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLAGE17O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLEXP17O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLCOD18O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLDES18O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLAGE18O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLEXP18O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLCOD19O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLDES19O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLAGE19O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLEXP19O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLCOD20O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLDES20O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLAGE20O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLEXP20O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLCOD21O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLDES21O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLAGE21O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLEXP21O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLCOD22O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLDES22O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLAGE22O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLEXP22O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLCOD23O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLDES23O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLAGE23O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLEXP23O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLCOD24O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLDES24O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLAGE24O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PLEXP24O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LUDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LUTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LUBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0076).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHPLANO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFPLANO PIC  X(0006).
      *    -------------------------------
00265
00266  01  EL656AO-R   REDEFINES EL656AI.
101501     05  FILLER             PIC X(221).
00268      05  PLAN-TABLE         OCCURS 24 TIMES
00269                             INDEXED BY PT-INDX.
00270          10  PT-CODE-L      PIC S9(4)         COMP.
00271          10  PT-CODE-A      PIC X.
00272          10  PT-CODE        PIC XX.
00273
00274          10  PT-DESC-L      PIC S9(4)         COMP.
00275          10  PT-DESC-A      PIC X.
00276          10  PT-DESC        PIC X(10).
00277
00278          10  PT-AGE-L       PIC S9(4)         COMP.
00279          10  PT-AGE-A       PIC X.
00280          10  PT-AGE         PIC 99.
00281
00282          10  PT-EXPIRE-L    PIC S9(4)         COMP.
00283          10  PT-EXPIRE-A    PIC X.
00284          10  PT-EXPIRE      PIC 9(8).
00285      05  FILLER             PIC X(128).
00286
00287      EJECT
00288
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
00290
00291  01  DFHCOMMAREA             PIC X(1024).
00292
00293 *01 PARMLIST .
00294 *    02  FILLER              PIC S9(8)   COMP.
00295 *    02  ERRATE-POINTER      PIC S9(8)   COMP.
00296 *    02  ELCNTL-POINTER      PIC S9(8)   COMP.
00297      EJECT
00298 *                            COPY ERCRATE.
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
00095      12  RT-POLICY-FEE                     PIC S9(3)V99   COMP-3.
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
00299      EJECT
00300 *                            COPY ELCCNTL.
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
032813         16  FILLER                         PIC X(79).
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
061511         16  CF-ST-VFY-2ND-BENE             PIC X.
012913         16  CF-ST-CAUSAL-STATE             PIC X.
012913         16  FILLER                         PIC X(185).
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
00301      EJECT
00302
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA RATE-RECORD
                                CONTROL-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL656' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00304
00305      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
00306      MOVE '5'                   TO DC-OPTION-CODE.
00307      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
00308      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
00309      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
00310
00311      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.
00312
00313  0100-START.
00314      IF EIBCALEN = 0
00315          GO TO 8800-UNAUTHORIZED-ACCESS.
00316
00317      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00318          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00319              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00320              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00321              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00322              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00323              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00324              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00325              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00326              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00327          ELSE
00328              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00329              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00330              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00331              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00332              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00333              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00334              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00335              MOVE SPACES               TO PI-SAVED-PROGRAM-6.
00336
00337      
      * EXEC CICS HANDLE CONDITION
00338 *        NOTOPEN  (9990-ABEND)
00339 *        NOTFND   (8880-NOT-FOUND)
00340 *        PGMIDERR (9600-PGMID-ERROR)
00341 *        ERROR    (9990-ABEND)
00342 *    END-EXEC.
      *    MOVE '"$JIL.                ! " #00003962' TO DFHEIV0
           MOVE X'22244A494C2E202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303033393632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00343
00344      IF PI-FILE-ID = 'OE'
00345         MOVE OE-RATE-FILE-ID TO RATE-FILE-ID.
00346
00347      IF EIBTRNID NOT = TRANS-ID
00348          MOVE LOW-VALUES TO EL656AI
00349          MOVE ZEROS      TO MISC-SAVE-AREAS
081413         move ' '                to pi-delete-sw
00350          IF RETURN-FROM-LIMITS
00351              GO TO 3000-BUILD-SCREEN-A
00352          ELSE
00353              GO TO 8100-SEND-INITIAL-MAP.
00354
00355      IF EIBAID = DFHCLEAR
00356          GO TO 9400-CLEAR.
00357
00358      IF PI-PROCESSOR-ID = 'LGXX'
00359          GO TO 0200-RECEIVE.
00360
00361      
      * EXEC CICS  READQ TS
00362 *        QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00363 *        INTO    (SECURITY-CONTROL)
00364 *        LENGTH  (SC-COMM-LENGTH)
00365 *        ITEM    (SC-ITEM)
00366 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00003987' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00367
00368      MOVE SC-CREDIT-DISPLAY (06)  TO  PI-DISPLAY-CAP.
00369      MOVE SC-CREDIT-UPDATE  (06)  TO  PI-MODIFY-CAP.
00370
00371      IF NOT DISPLAY-CAP
00372          MOVE 'READ'          TO SM-READ
00373          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00374          MOVE ER-0070        TO  EMI-ERROR
00375          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00376          GO TO 8100-SEND-INITIAL-MAP.
00377
00378      EJECT
00379
00380  0200-RECEIVE.
00381      MOVE LOW-VALUES TO EL656AI.
00382
00383      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00384          MOVE ER-0008 TO EMI-ERROR
00385          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00386          MOVE -1   TO MAINTL
00387          GO TO 8200-SEND-DATAONLY.
00388
00389      
      * EXEC CICS RECEIVE
00390 *        MAP    (WS-MAPNAME)
00391 *        MAPSET (WS-MAPSET-NAME)
00392 *        INTO   (EL656AI)
00393 *    END-EXEC.
           MOVE LENGTH OF
            EL656AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00004015' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAPNAME, 
                 EL656AI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00394
00395      IF PFENTERL = 0
00396          GO TO 0300-CHECK-PFKEYS.
00397
00398      IF EIBAID NOT = DFHENTER
00399          MOVE ER-0004 TO EMI-ERROR
00400          GO TO 0310-INPUT-ERROR.
00401
00402      IF (PFENTERI NUMERIC) AND (PFENTERI GREATER 0 AND LESS 25)
00403          MOVE PF-VALUES (PFENTERI) TO EIBAID
00404      ELSE
00405          MOVE ER-0029 TO EMI-ERROR
00406          GO TO 0310-INPUT-ERROR.
00407
00408  0300-CHECK-PFKEYS.
00409      IF EIBAID = DFHPF23
00410          GO TO 8810-PF23.
00411
00412      IF EIBAID = DFHPF24
00413          GO TO 9200-RETURN-MAIN-MENU.
00414
00415      IF EIBAID = DFHPF12
00416          GO TO 9500-PF12.
00417
00418      IF MAINTL GREATER ZERO
00419          IF MAINTI NOT = SPACE
00420              IF EIBAID NOT = DFHENTER
00421                  MOVE -1      TO MAINTL
00422                  MOVE ER-0050 TO EMI-ERROR
00423                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00424                  GO TO 8200-SEND-DATAONLY.
00425
00426      IF EIBAID = DFHPF1
00427          MOVE 'Y'                TO  PI-FIRST-TIME-SW
00428          GO TO 7100-NEXT-STRUCTURE.
00429
00430      IF EIBAID = DFHPF2
00431          MOVE 'Y'                TO  PI-FIRST-TIME-SW
00432          GO TO 7200-PRIOR-STRUCTURE.
00433
00434      IF EIBAID = DFHPF3
00435          GO TO 7100-NEXT-STRUCTURE.
00436
00437      IF EIBAID = DFHPF4
00438          GO TO 7200-PRIOR-STRUCTURE.
00439
00440      IF EIBAID = DFHPF5
00441          MOVE '1' TO WS-SHOW-PLAN-SW
00442          MOVE 'S' TO MAINTI
00443          MOVE +1  TO MAINTL
00444          GO TO 0400-EDIT-MAINT.
00445
00446      IF EIBAID = DFHPF6
00447          MOVE '2' TO WS-SHOW-PLAN-SW
00448          MOVE 'S' TO MAINTI
00449          MOVE +1  TO MAINTL
00450          GO TO 0400-EDIT-MAINT.
00451
00452      IF EIBAID = DFHENTER
00453          GO TO 0400-EDIT-MAINT.
00454
00455      MOVE ER-0029 TO EMI-ERROR.
00456
00457  0310-INPUT-ERROR.
00458      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00459
00460      MOVE AL-UNBON TO PFENTERA.
00461      MOVE -1       TO PFENTERL.
00462
00463      GO TO 8200-SEND-DATAONLY.
00464
00465      EJECT
00466  0400-EDIT-MAINT.
00467      MOVE SPACES        TO PI-ERRATE-KEY.
00468      MOVE PI-COMPANY-CD TO PI-RATE-COMPANY-CD.
00469
00470      MOVE ALL  '9'      TO PI-RATE-SEX
00471                            PI-RATE-FUTURE.
00472
00473      MOVE ZEROS         TO PI-RATE-LAH-NUM
00474                            PI-RATE-EXPIRY-DATE.
00475
00476      IF MAINTL GREATER ZERO
00477          MOVE MAINTI TO PI-MAINT
00478          IF VALID-MAINT-TYPE
00479              MOVE AL-UANON TO MAINTA
00480          ELSE
00481              MOVE -1       TO MAINTL
00482              MOVE AL-UABON TO MAINTA
00483              MOVE ER-2039  TO EMI-ERROR
00484              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00485      ELSE
00486          MOVE -1           TO MAINTL
00487          MOVE AL-UABON     TO MAINTA
00488          MOVE ER-2039   TO EMI-ERROR
00489          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00490
00491      IF STATE1L GREATER ZERO
00492          MOVE SPACES       TO WS-ACCESS
00493          MOVE STATE1I      TO WS-STATE-CODE
00494          PERFORM 7400-EDIT-STATE THRU 7499-EXIT
00495          IF STATE-FOUND
00496              MOVE AL-UANON TO STATE1A
00497              MOVE STATE1I  TO PI-RATE-CODE
00498          ELSE
00499              MOVE -1       TO STATE1L
00500              MOVE AL-UABON TO STATE1A
00501              MOVE ER-2261  TO EMI-ERROR
00502              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00503      ELSE
00504          MOVE -1           TO STATE1L
00505          MOVE AL-UABON     TO STATE1A
00506          MOVE ER-2261   TO EMI-ERROR
00507          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00508
00509      IF CLASS1L GREATER ZERO
00510          MOVE AL-UANON     TO CLASS1A
00511          MOVE CLASS1I      TO PI-RATE-CLASS
00512      ELSE
00513          IF ADD-FUNCTION
00514              MOVE ZEROS    TO CLASS1O
00515                               PI-RATE-CLASS
00516              MOVE AL-UANON TO CLASS1A
00517          ELSE
00518              MOVE -1       TO CLASS1L
00519              MOVE AL-UABON TO CLASS1A
00520              MOVE ER-2262  TO EMI-ERROR
00521              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00522
00523      IF RTAGE1L GREATER ZERO
00524         MOVE RTAGE1I       TO  DEEDIT-FIELD
00525         PERFORM 7500-DEEDIT THRU 7500-EXIT
00526         IF DEEDIT-FIELD-V0 GREATER ZERO
00527            MOVE AL-UNNON   TO RTAGE1A
00528            MOVE RTAGE1I    TO PI-RATE-HIGH-AGE
00529         ELSE
00530            MOVE -1         TO RTAGE1L
00531            MOVE AL-UNBON   TO RTAGE1A
00532            MOVE ER-2187 TO EMI-ERROR
00533            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00534      ELSE
00535          IF ADD-FUNCTION
00536              MOVE ALL '9'  TO RTAGE1O
00537                               PI-RATE-HIGH-AGE
00538              MOVE AL-UNNON TO RTAGE1A
00539          ELSE
00540            MOVE -1         TO RTAGE1L
00541            MOVE AL-UNBON   TO RTAGE1A
00542            MOVE ER-2187 TO EMI-ERROR
00543            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00544
00545      IF RTAMT1L GREATER ZERO
00546         MOVE RTAMT1I       TO  DEEDIT-FIELD
00547         PERFORM 7500-DEEDIT THRU 7500-EXIT
00548         IF DEEDIT-FIELD-V0 GREATER ZERO
00549            MOVE AL-UNNON   TO RTAMT1A
00550            MOVE DEEDIT-FIELD-V0 TO PI-RATE-HIGH-AMT
00551         ELSE
00552            MOVE -1         TO RTAMT1L
00553            MOVE AL-UNBON   TO RTAMT1A
00554            MOVE ER-2189 TO EMI-ERROR
00555            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00556      ELSE
00557          IF ADD-FUNCTION
00558              MOVE ALL '9'  TO RTAMT1O
00559                               PI-RATE-HIGH-AMT
00560              MOVE AL-UNNON TO RTAMT1A
00561          ELSE
00562            MOVE -1         TO RTAMT1L
00563            MOVE AL-UNBON   TO RTAMT1A
00564            MOVE ER-2189 TO EMI-ERROR
00565            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00566
00567      IF DEV1L GREATER ZERO
00568          MOVE AL-UANON     TO DEV1A
00569          MOVE DEV1I        TO PI-RATE-DEV
00570      ELSE
00571          IF ADD-FUNCTION
00572              MOVE AL-UANON TO DEV1A
00573              MOVE ZEROS    TO DEV1O
00574                               PI-RATE-DEV
00575          ELSE
00576              MOVE -1       TO DEV1L
00577              MOVE AL-UABON TO DEV1A
00578              MOVE ER-2263  TO EMI-ERROR
00579              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00580
00581      IF TYPE1L GREATER ZERO
00582          IF TYPE1I = PI-LIFE-OVERRIDE-L1 OR PI-AH-OVERRIDE-L1
00583              MOVE AL-UANON TO TYPE1A
00584              MOVE TYPE1I   TO PI-RATE-L-AH
00585          ELSE
00586              MOVE -1       TO TYPE1L
00587              MOVE AL-UABON TO TYPE1A
00588              MOVE ER-2267  TO EMI-ERROR
00589              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00590      ELSE
00591          MOVE -1           TO TYPE1L
00592          MOVE AL-UABON     TO TYPE1A
00593          MOVE ER-2267   TO EMI-ERROR
00594          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00595
00596      IF TYPE1I = PI-LIFE-OVERRIDE-L1
00597          MOVE '4'          TO WS-REC-TYPE
00598      ELSE
00599          MOVE '5'          TO WS-REC-TYPE.
00600
00601      IF PLAN1L GREATER ZERO
00602          MOVE PLAN1I       TO WS-HI-BENEFIT
00603          PERFORM 7300-EDIT-BENEFIT THRU 7300-EXIT
00604      ELSE
00605          MOVE -1           TO PLAN1L
00606          MOVE AL-UABON     TO PLAN1A
00607          MOVE ER-2268   TO EMI-ERROR
00608          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00609
00610      IF BENEFIT-FOUND
00611          MOVE PLAN1I       TO PI-RATE-LAH-NUM
00612          MOVE AL-UANON     TO PLAN1A
00613      ELSE
00614          MOVE -1           TO PLAN1L
00615          MOVE AL-UABON     TO PLAN1A
00616          MOVE ER-2268   TO EMI-ERROR
00617          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00618
00619      IF EXPIRE1L GREATER ZERO
00620          IF EXPIRE1I NUMERIC
00621              IF EXPIRE1I = WS-CURRENT-EXPIRE
00622                  MOVE AL-UNNON TO EXPIRE1A
00623                  MOVE ZEROS    TO WS-HOLD-RATE-EXP-DT
00624                  MOVE EXPIRE1I TO WS-HOLD-RATE-EXP-AL(6:6)
00625                                   DC-GREG-DATE-1-YMD
00626                  MOVE '3'      TO DC-OPTION-CODE
00627                  PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
00628                  IF NO-CONVERSION-ERROR
00629                      MOVE DC-ALPHA-CENTURY TO
00630                           WS-HOLD-RATE-EXP-AL(4:2)
00631                      MOVE WS-HOLD-RATE-EXP-DT TO
00632                                               PI-RATE-EXPIRY-DATE
00633                  ELSE
00634                      IF WS-HOLD-RATE-EXP-DT(6:6) = 999999
00635                          MOVE 99999999999         TO
00636                               WS-HOLD-RATE-EXP-DT
00637                          MOVE WS-HOLD-RATE-EXP-DT TO
00638                                               PI-RATE-EXPIRY-DATE
00639                      ELSE
00640                          MOVE -1       TO EXPIRE1L
00641                          MOVE AL-UNBON TO EXPIRE1A
00642                          MOVE ER-2296  TO EMI-ERROR
00643                          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00644                      END-IF
00645                  END-IF
00646              ELSE
00647                  MOVE ZEROS    TO WS-HOLD-RATE-EXP-DT
00648                  MOVE EXPIRE1I TO DC-GREG-DATE-1-YMD
00649                                   WS-HOLD-RATE-EXP-AL(6:6)
00650                  MOVE '3'      TO DC-OPTION-CODE
00651                  PERFORM 9700-LINK-DATE-CONVERT
00652                     THRU 9700-EXIT
00653                  IF NO-CONVERSION-ERROR
00654                      MOVE AL-UNNON TO EXPIRE1A
00655                      MOVE EXPIRE1I TO WS-HOLD-RATE-EXP-AL(6:6)
00656                                       DC-GREG-DATE-1-YMD
00657                      MOVE '3'      TO DC-OPTION-CODE
00658                      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
00659                      IF NO-CONVERSION-ERROR
00660                          MOVE DC-ALPHA-CENTURY TO
00661                               WS-HOLD-RATE-EXP-AL(4:2)
00662                          MOVE WS-HOLD-RATE-EXP-DT TO
00663                               PI-RATE-EXPIRY-DATE
00664                      ELSE
00665                          IF WS-HOLD-RATE-EXP-DT(6:6) = 999999
00666                              MOVE 99999999999         TO
00667                                   WS-HOLD-RATE-EXP-DT
00668                              MOVE WS-HOLD-RATE-EXP-DT TO
00669                                               PI-RATE-EXPIRY-DATE
00670                          ELSE
00671                              MOVE -1       TO EXPIRE1L
00672                              MOVE AL-UNBON TO EXPIRE1A
00673                              MOVE ER-2296  TO EMI-ERROR
00674                              PERFORM 9900-ERROR-FORMAT THRU
00675                                                          9900-EXIT
00676                          END-IF
00677                      END-IF
00678                  ELSE
00679                      IF WS-HOLD-RATE-EXP-DT(6:6) = 999999
00680                          MOVE 99999999999         TO
00681                               WS-HOLD-RATE-EXP-DT
00682                          MOVE WS-HOLD-RATE-EXP-DT TO
00683                                               PI-RATE-EXPIRY-DATE
00684                      ELSE
00685                          MOVE -1       TO EXPIRE1L
00686                          MOVE AL-UNBON TO EXPIRE1A
00687                          MOVE ER-2296  TO EMI-ERROR
00688                          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00689                      END-IF
00690                  END-IF
00691              END-IF
00692          ELSE
00693              MOVE -1          TO EXPIRE1L
00694              MOVE AL-UNBON    TO EXPIRE1A
00695              MOVE ER-2266  TO EMI-ERROR
00696              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00697          END-IF
00698      ELSE
00699          IF ADD-FUNCTION
00700              MOVE AL-UNNON    TO EXPIRE1A
00701              MOVE ALL '9'     TO EXPIRE1O
00702              MOVE ZEROS       TO WS-HOLD-RATE-EXP-DT
00703              MOVE EXPIRE1I    TO WS-HOLD-RATE-EXP-AL(6:6)
00704                                  DC-GREG-DATE-1-YMD
00705              MOVE '3'         TO DC-OPTION-CODE
00706              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
00707              IF NO-CONVERSION-ERROR
00708                  MOVE DC-ALPHA-CENTURY TO
00709                       WS-HOLD-RATE-EXP-AL(4:2)
00710                  MOVE WS-HOLD-RATE-EXP-DT TO
00711                       PI-RATE-EXPIRY-DATE
00712              ELSE
00713                  IF WS-HOLD-RATE-EXP-DT(6:6) = 999999
00714                      MOVE 99999999999         TO
00715                           WS-HOLD-RATE-EXP-DT
00716                      MOVE WS-HOLD-RATE-EXP-DT TO
00717                                           PI-RATE-EXPIRY-DATE
00718                  ELSE
00719                      MOVE -1       TO EXPIRE1L
00720                      MOVE AL-UNBON TO EXPIRE1A
00721                      MOVE ER-2296  TO EMI-ERROR
00722                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00723
00724      IF EMI-NO-ERRORS
00725          NEXT SENTENCE
00726      ELSE
00727          GO TO 8200-SEND-DATAONLY.
00728
00729      IF NOT MODIFY-CAP
00730          IF SHOW-FUNCTION
00731              NEXT SENTENCE
00732          ELSE
00733              MOVE 'UPDATE'       TO SM-READ
00734              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00735              MOVE ER-0070        TO  EMI-ERROR
00736              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00737              GO TO 8100-SEND-INITIAL-MAP.
00738
00739      IF ADD-FUNCTION
00740          GO TO 1000-ADD.
00741
00742      IF CHANGE-FUNCTION
00743          GO TO 1200-CHANGE.
00744
00745      IF COPY-FUNCTION
00746          GO TO 1400-COPY.
081413     IF DELETE-FUNCTION
081413        if delete-ok
081413           move ' '              to pi-delete-sw
081413           GO TO 1600-DELETE
081413        else
081413           SET DELETE-OK         TO TRUE
081413           MOVE ER-0755          TO EMI-ERROR
081413           PERFORM 9900-ERROR-FORMAT
081413                                 THRU 9900-EXIT
081413           move ' '              to mainto
081413           move -1               to maintl
081413           go to 8200-send-dataonly
081413        END-IF
081413     END-IF
00751      IF DEVIATE-FUNCTION
00752          GO TO 1800-DEVIATE.
00753
00754  0400-SHOW.
00755      IF SHOW-FUNCTION
00756          IF WS-SHOW-PLAN-SW NOT = SPACE
00757              GO TO 3000-BUILD-SCREEN-A
00758          ELSE
00759              IF SHOWN-ONCE
00760                  IF PI-ERRATE-KEY = PI-SAVE-ERRATE-KEY
00761                      MOVE XCTL-6561 TO PGM-NAME
00762                      GO TO 9300-XCTL
00763                  ELSE
00764                      MOVE SPACE TO PI-SHOW-SW
00765                      GO TO 3000-BUILD-SCREEN-A
00766              ELSE
00767                  GO TO 3000-BUILD-SCREEN-A.
00768
00769  0400-EXIT.
00770      EXIT.
00771      EJECT
00772
00773  1000-ADD.
00774      
      * EXEC CICS HANDLE CONDITION
00775 *        NOTOPEN  (9990-ABEND)
00776 *        NOTFND   (1025-ADD-REC)
00777 *    END-EXEC.
      *    MOVE '"$JI                  ! # #00004410' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303034343130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00778
00779      PERFORM 7650-READ-ERRATE THRU 7650-EXIT.
00780
00781      MOVE ER-2270   TO EMI-ERROR.
00782      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00783      MOVE -1     TO MAINTL.
00784      GO TO 8200-SEND-DATAONLY.
00785
00786  1025-ADD-REC.
00787      PERFORM 7700-ERRATE-GETMAIN THRU 7700-EXIT.
00788
00789      MOVE SPACES              TO RATE-RECORD.
00790      MOVE ZEROS               TO RT-HIGH-AGE  RT-HIGH-AMT
00791                                  RT-LAH-NUM   RT-EXPIRY-DATE
00792                                  RT-MAX-AGE
00793                                  RT-LAST-MAINT-HHMMSS.
00794
00795      PERFORM 4000-UPDATE-SCREEN-A THRU 4099-EXIT.
00796
00797      MOVE +1 TO SUB1.
00798
00799  1050-ZERO-LIMITS.
00800      IF PI-RATE-L-AH = PI-LIFE-OVERRIDE-L1
00801          MOVE ZEROS TO RT-L-EX-AGE  (SUB1)
00802                        RT-L-EX-TERM (SUB1)
00803                        RT-L-EX-FACE (SUB1).
00804
00805      IF PI-RATE-L-AH = PI-AH-OVERRIDE-L1
00806          MOVE ZEROS TO RT-AH-AGE   (SUB1)
00807                        RT-AH-TERM  (SUB1)
00808                        RT-AH-BEN-M (SUB1)
00809                        RT-AH-BEN-F (SUB1).
00810
00811      ADD +1 TO SUB1.
00812      IF SUB1 GREATER +8
00813          MOVE +1 TO SUB1
00814      ELSE
00815          GO TO 1050-ZERO-LIMITS.
00816
00817  1075-ZERO-RATES.
00818      IF PI-RATE-L-AH = PI-LIFE-OVERRIDE-L1
00819          MOVE ZEROS TO RT-L-RATE (SUB1).
00820
00821      IF PI-RATE-L-AH = PI-AH-OVERRIDE-L1
00822          MOVE ZEROS TO RT-AH-RATE (SUB1).
00823
00824      ADD +1 TO SUB1.
00825      IF SUB1 GREATER +360
00826          MOVE +1 TO SUB1
00827      ELSE
00828          GO TO 1075-ZERO-RATES.
00829
00830      MOVE PI-PROCESSOR-ID TO RT-LAST-MAINT-USER.
00831      MOVE EIBTIME         TO RT-LAST-MAINT-HHMMSS.
00832
00833      MOVE SAVE-BIN-DATE   TO RT-LAST-MAINT-DT
00834                              BIN-CURRENT-SAVE.
00835      MOVE PI-COMPANY-CD   TO RT-COMPANY-CD.
00836      MOVE 'RT'            TO RT-RECORD-ID.
00837      MOVE RATE-FILE-ID    TO FILE-ID.
00838      MOVE 'A'             TO JP-RECORD-TYPE.
00839
00840      
      * EXEC CICS WRITE
00841 *        DATASET (RATE-FILE-ID)
00842 *        FROM    (RATE-RECORD)
00843 *        RIDFLD  (RT-CONTROL-PRIMARY)
00844 *    END-EXEC.
           MOVE LENGTH OF
            RATE-RECORD
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004476' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RATE-FILE-ID, 
                 RATE-RECORD, 
                 DFHEIV11, 
                 RT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00845
00846      MOVE RATE-RECORD TO JP-RECORD-AREA.
00847
081413     PERFORM 8400-LOG-JOURNAL-RECORD
081413                                 thru 8400-exit
00849      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.
00850      MOVE ER-0000 TO EMI-ERROR.
00851      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00852
00853      MOVE XCTL-6561 TO PGM-NAME.
00854      GO TO 9300-XCTL.
00855
00856  1099-EXIT.
00857      EXIT.
00858      EJECT
00859
00860  1200-CHANGE.
00861      IF PI-ERRATE-KEY = PI-SAVE-ERRATE-KEY
00862          NEXT SENTENCE
00863      ELSE
00864          MOVE ER-2056  TO EMI-ERROR
00865          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00866          MOVE -1    TO MAINTL
00867          GO TO 8200-SEND-DATAONLY.
00868
00869      
      * EXEC CICS HANDLE CONDITION
00870 *        NOTFND   (8880-NOT-FOUND)
00871 *    END-EXEC.
      *    MOVE '"$I                   ! $ #00004506' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303034353036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00872
00873      PERFORM 7650-READ-ERRATE THRU 7650-EXIT.
00874
00875      IF EMI-NO-ERRORS
00876          NEXT SENTENCE
00877      ELSE
00878          GO TO 8200-SEND-DATAONLY.
00879
00880      PERFORM 7750-READ-ERRATE-UPDATE THRU 7750-EXIT.
00881
00882      MOVE RATE-RECORD TO JP-RECORD-AREA.
00883
00884      IF DEVIATE-FUNCTION
00885          PERFORM 6000-DEVIATE-RATES THRU 6099-EXIT
00886      ELSE
00887          PERFORM 4000-UPDATE-SCREEN-A THRU 4099-EXIT.
00888
00889      IF RT-LAST-MAINT-USER   = PI-UPDATE-BY OR
00890         RT-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS
00891          NEXT SENTENCE
00892      ELSE
00893          
      * EXEC CICS UNLOCK
00894 *             DATASET  (RATE-FILE-ID)
00895 *        END-EXEC
      *    MOVE '&*                    #   #00004530' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RATE-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00896          MOVE ER-0068 TO EMI-ERROR
00897          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00898          GO TO 3000-BUILD-SCREEN-A.
00899
00900      MOVE PI-PROCESSOR-ID     TO RT-LAST-MAINT-USER.
00901      MOVE EIBTIME             TO RT-LAST-MAINT-HHMMSS.
00902
00903      MOVE SAVE-BIN-DATE       TO RT-LAST-MAINT-DT
00904                                  BIN-CURRENT-SAVE.
00905      MOVE 'B'                 TO JP-RECORD-TYPE
00906      MOVE RATE-FILE-ID        TO FILE-ID.
081413     PERFORM 8400-LOG-JOURNAL-RECORD
081413                                 thru 8400-exit
00908
00909      
      * EXEC CICS REWRITE
00910 *        DATASET  (RATE-FILE-ID)
00911 *        FROM     (RATE-RECORD)
00912 *    END-EXEC.
           MOVE LENGTH OF
            RATE-RECORD
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004547' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RATE-FILE-ID, 
                 RATE-RECORD, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00913
00914      MOVE RATE-RECORD         TO JP-RECORD-AREA.
00915      MOVE 'C'                 TO JP-RECORD-TYPE
00916      MOVE RATE-FILE-ID        TO FILE-ID.
081413     PERFORM 8400-LOG-JOURNAL-RECORD
081413                                 thru 8400-exit
00918      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.
00919      MOVE ER-0000 TO EMI-ERROR.
00920      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00921
00922      MOVE XCTL-6561 TO PGM-NAME.
00923      GO TO 9300-XCTL.
00924
00925  1299-EXIT.
00926      EXIT.
00927      EJECT
00928
00929  1400-COPY.
00930      MOVE PI-ERRATE-KEY TO PI-SAVE-ERRATE-KEY.
00931      MOVE SPACES        TO PI-ERRATE-KEY.
00932      MOVE ALL '9'       TO PI-RATE-FUTURE
00933                            PI-RATE-SEX.
00934
00935      PERFORM 5000-EDIT-SCREEN-A THRU 5099-EXIT.
00936
00937      IF EMI-NO-ERRORS
00938          NEXT SENTENCE
00939      ELSE
00940          GO TO 8200-SEND-DATAONLY.
00941
00942      
      * EXEC CICS HANDLE CONDITION
00943 *        NOTOPEN  (9990-ABEND)
00944 *        NOTFND   (1450-COPY-REC)
00945 *    END-EXEC.
      *    MOVE '"$JI                  ! % #00004581' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303034353831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00946
00947      PERFORM 7650-READ-ERRATE THRU 7650-EXIT.
00948
00949      MOVE -1    TO MAINTL.
00950      MOVE ER-2270  TO EMI-ERROR.
00951      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00952
00953      GO TO 8200-SEND-DATAONLY.
00954
00955  1450-COPY-REC.
00956      MOVE PI-ERRATE-KEY       TO WS-SAVE-KEY.
00957      MOVE PI-SAVE-ERRATE-KEY  TO PI-ERRATE-KEY.
00958
00959      
      * EXEC CICS HANDLE CONDITION
00960 *        NOTFND   (8880-NOT-FOUND)
00961 *    END-EXEC.
      *    MOVE '"$I                   ! & #00004598' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303034353938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00962
00963      PERFORM 7650-READ-ERRATE THRU 7650-EXIT.
00964
00965      MOVE WS-SAVE-KEY         TO PI-ERRATE-KEY.
00966
00967      PERFORM 4000-UPDATE-SCREEN-A THRU 4099-EXIT.
00968
00969      MOVE PI-PROCESSOR-ID TO RT-LAST-MAINT-USER.
00970      MOVE EIBTIME         TO RT-LAST-MAINT-HHMMSS.
00971
00972      MOVE SAVE-BIN-DATE   TO RT-LAST-MAINT-DT
00973                              BIN-CURRENT-SAVE.
00974      MOVE PI-COMPANY-CD   TO RT-COMPANY-CD.
00975      MOVE 'RT'            TO RT-RECORD-ID.
00976      MOVE RATE-FILE-ID    TO FILE-ID.
00977      MOVE 'A'             TO JP-RECORD-TYPE.
00978
00979      
      * EXEC CICS WRITE
00980 *        DATASET (RATE-FILE-ID)
00981 *        FROM    (RATE-RECORD)
00982 *        RIDFLD  (RT-CONTROL-PRIMARY)
00983 *    END-EXEC.
           MOVE LENGTH OF
            RATE-RECORD
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004618' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RATE-FILE-ID, 
                 RATE-RECORD, 
                 DFHEIV11, 
                 RT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00984
00985      MOVE RATE-RECORD TO JP-RECORD-AREA.
00986
081413     PERFORM 8400-LOG-JOURNAL-RECORD
081413                                 thru 8400-exit
00988      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.
00989      MOVE ER-0000 TO EMI-ERROR.
00990      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00991
00992      MOVE XCTL-6561 TO PGM-NAME.
00993      GO TO 9300-XCTL.
00994
00995  1499-EXIT.
00996      EXIT.
00997      EJECT
00998
00999  1600-DELETE.
01000      IF PI-ERRATE-KEY = PI-SAVE-ERRATE-KEY
01001          NEXT SENTENCE
01002      ELSE
01003          MOVE ER-2056  TO EMI-ERROR
01004          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01005          MOVE -1    TO MAINTL
01006          GO TO 8200-SEND-DATAONLY.
01007
01008      
      * EXEC CICS HANDLE CONDITION
01009 *        NOTFND   (8880-NOT-FOUND)
01010 *    END-EXEC.
      *    MOVE '"$I                   ! '' #00004648' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303034363438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01011
01012      PERFORM 7750-READ-ERRATE-UPDATE THRU 7750-EXIT.
01013
01014      MOVE 'D'                 TO JP-RECORD-TYPE.
01015      MOVE RATE-RECORD         TO JP-RECORD-AREA.
01016      MOVE RATE-FILE-ID        TO FILE-ID.
01017
01018      IF RT-LAST-MAINT-USER   = PI-UPDATE-BY OR
01019         RT-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS
01020          NEXT SENTENCE
01021      ELSE
01022          
      * EXEC CICS UNLOCK
01023 *             DATASET  (RATE-FILE-ID)
01024 *        END-EXEC
      *    MOVE '&*                    #   #00004662' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RATE-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01025          MOVE ER-0068 TO EMI-ERROR
01026          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01027          GO TO 3000-BUILD-SCREEN-A.
01028
01029      
      * EXEC CICS DELETE
01030 *         DATASET  (RATE-FILE-ID)
01031 *    END-EXEC.
      *    MOVE '&(                    &   #00004669' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RATE-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01032
081413     PERFORM 8400-LOG-JOURNAL-RECORD
081413                                 thru 8400-exit
01034
01035      MOVE SAVE-BIN-DATE  TO BIN-CURRENT-SAVE.
01036      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.
01037      MOVE ER-0000     TO EMI-ERROR.
01038      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01039      MOVE ' '                       TO  PI-SHOW-SW.
01040
01041      MOVE LOW-VALUES TO EL656AO.
01042
01043      MOVE PI-RATE-CODE              TO  STATE1O.
01044      MOVE PI-RATE-CLASS             TO  CLASS1O.
01045      MOVE PI-RATE-DEV               TO  DEV1O.
01046      MOVE PI-RATE-L-AH              TO  TYPE1O.
01047      MOVE PI-RATE-LAH-NUM           TO  PLAN1O.
01048      MOVE PI-RATE-HIGH-AGE          TO  RTAGE1O.
01049      MOVE PI-RATE-HIGH-AMT          TO  RTAMT1O.
01050      MOVE PI-RATE-EXPIRY-DATE       TO  EXPIRE1O.
01051
01052      MOVE AL-UANON                  TO  CLASS1A
01053                                         TYPE1A
01054                                         DEV1A
01055                                         STATE1A
01056                                         PLAN1A.
01057      MOVE AL-UNNON                  TO  RTAGE1A
01058                                         RTAMT1A
01059                                         EXPIRE1A.
01060      GO TO 8100-SEND-INITIAL-MAP.
01061
01062  1699-EXIT.
01063      EXIT.
01064      EJECT
01065
01066  1800-DEVIATE.
01067      IF DEV1I = ZEROS
01068         IF PI-PROCESSOR-ID NOT = 'LGXX'
01069            MOVE -1                  TO MAINTL
01070            MOVE SPACE               TO MAINTO
01071            MOVE ER-2396          TO EMI-ERROR
01072            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01073
01074      IF  DECINUML GREATER ZERO
01075          
      * EXEC CICS BIF DEEDIT
01076 *            FIELD   (DECINUMI)
01077 *            LENGTH  (1)
01078 *            END-EXEC
           MOVE 1
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004716' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DECINUMI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01079
01080          IF  DECINUMI NOT NUMERIC
01081                  OR
01082              DECINUMI GREATER THAN 5
01083                  OR
01084              DECINUMI LESS THAN 1
01085              MOVE -1             TO DECINUML
01086              MOVE AL-UNBON       TO DECINUMA
01087              MOVE ER-7743        TO EMI-ERROR
01088              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01089
01090          ELSE
01091              MOVE DECINUMI       TO W-DECIMAL-NUMBER
01092
01093      ELSE
01094          MOVE +5                 TO DECINUMO
01095                                     W-DECIMAL-NUMBER.
01096
01097      IF DEVPCTL GREATER ZERO
01098          MOVE DEVPCTI TO DEEDIT-FIELD
01099          PERFORM 7500-DEEDIT THRU 7500-EXIT
01100          IF DEEDIT-FIELD-V2 GREATER ZERO
01101              MOVE DEEDIT-FIELD-V2 TO DEVPCTO
01102                                      WS-SAVE-FACTOR
01103              MOVE AL-UNNON        TO DEVPCTA
01104          ELSE
01105              MOVE -1              TO DEVPCTL
01106              MOVE AL-UNBON        TO DEVPCTA
01107              MOVE ER-2395      TO EMI-ERROR
01108              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01109      ELSE
01110          MOVE -1                  TO DEVPCTL
01111          MOVE AL-UNBON            TO DEVPCTA
01112          MOVE ER-2395          TO EMI-ERROR
01113          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01114
01115      IF EMI-NO-ERRORS
01116          GO TO 1200-CHANGE
01117      ELSE
01118          GO TO 8200-SEND-DATAONLY.
01119
01120  1899-EXIT.
01121      EXIT.
01122      EJECT
01123  3000-BUILD-SCREEN-A.
01124      MOVE LOW-VALUES                    TO  EL656AO.
01125      MOVE ZEROS                         TO  MISC-SAVE-AREAS.
01126      MOVE PI-COMPANY-CD                 TO  PI-RATE-COMPANY-CD.
01127
01128      
      * EXEC CICS HANDLE CONDITION
01129 *        NOTOPEN  (9990-ABEND)
01130 *        NOTFND   (8880-NOT-FOUND)
01131 *        PGMIDERR (9600-PGMID-ERROR)
01132 *        ERROR    (9990-ABEND)
01133 *    END-EXEC.
      *    MOVE '"$JIL.                ! ( #00004769' TO DFHEIV0
           MOVE X'22244A494C2E202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303034373639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01134
01135      PERFORM 7650-READ-ERRATE THRU 7650-EXIT.
01136
01137      MOVE PI-ERRATE-KEY                 TO PI-SAVE-ERRATE-KEY.
01138
01139  3025-SET-UP-SCREEN-A.
01140      MOVE RT-ST-CODE                    TO STATE1O.
01141      MOVE RT-ST-CLASS                   TO CLASS1O.
01142      MOVE RT-HIGH-AGE                   TO RTAGE1O.
01143      MOVE RT-HIGH-AMT                   TO RTAMT1O.
01144      MOVE RT-ST-DEV                     TO DEV1O.
01145
01146      MOVE RT-EXPIRY-DATE                TO WS-HOLD-RATE-EXP-DT.
01147      MOVE WS-HOLD-RATE-EXP-AL(6:6)      TO EXPIRE1O.
01148
01149      MOVE RT-L-AH                       TO TYPE1O.
01150      MOVE RT-LAH-NUM                    TO PLAN1O.
01151
01152      MOVE RT-STRUCTURE-COMMENT          TO STCOMMO.
01153
01154      MOVE RT-LAST-MAINT-DT   TO DC-BIN-DATE-1.
01155      MOVE SPACE              TO DC-OPTION-CODE.
01156      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
01157      MOVE DC-GREG-DATE-1-EDIT  TO LUDATEO.
01158      MOVE RT-LAST-MAINT-USER   TO LUBYO.
01159      MOVE RT-LAST-MAINT-HHMMSS TO TIME-MT.
01160      MOVE TIME-LMT             TO LUTIMEO.
01161
01162      MOVE AL-UANON                      TO CLASS1A
01163                                            STATE1A
01164                                            TYPE1A
01165                                            DEV1A
01166                                            PLAN1A.
01167
01168      MOVE AL-UNNON                      TO RTAGE1A
01169                                            RTAMT1A
01170                                            EXPIRE1A.
01171
01172      IF EIBAID = DFHPF1 OR DFHPF2
01173          MOVE AL-UADON                  TO TYPE1A
01174                                            PLAN1A
01175          MOVE AL-UNDON                  TO RTAGE1A
01176                                            RTAMT1A
01177                                            EXPIRE1A.
01178      MOVE 'Y'       TO PI-SHOW-SW.
01179
01180      IF WS-SHOW-PLAN-SW NOT = SPACE
01181          PERFORM 6500-SHOW-PLANS THRU 6599-EXIT.
01182
01183      IF RETURN-FROM-LIMITS OR
01184         EIBAID NOT = DFHENTER
01185          NEXT SENTENCE
01186       ELSE
01187      IF PI-ERRATE-KEY = PI-SAVE-ERRATE-KEY
01188          MOVE XCTL-6561 TO PGM-NAME
01189          GO TO 9300-XCTL.
01190
01191      MOVE SPACE TO PI-RETURN-SW.
01192
01193      MOVE ER-0000 TO EMI-ERROR.
01194      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01195
01196      GO TO 8100-SEND-INITIAL-MAP.
01197
01198  3099-EXIT.
01199      EXIT.
01200      EJECT
01201  4000-UPDATE-SCREEN-A.
01202       IF CHANGE-FUNCTION
01203           GO TO 4025-CONT.
01204
01205       MOVE PI-RATE-COMPANY-CD   TO RT-COMPANY-CD.
01206
01207       MOVE PI-RATE-STATE-CODE   TO RT-STATE-CODE
01208                                    RTC-1.
01209       MOVE PI-RATE-HIGH-AGE     TO RT-HIGH-AGE.
01210
01211       MOVE PI-RATE-HIGH-AMT     TO RT-HIGH-AMT.
01212
01213       MOVE PI-RATE-LIMITS       TO RT-LIMITS
01214                                    RTC-3.
01215
01216       MOVE PI-RATE-L-AH-CODE    TO RT-L-AH-CODE
01217                                    RTC-2.
01218
01219       MOVE ZEROS                TO WS-HOLD-RATE-EXP-DT.
01220       MOVE PI-RATE-EXPIRY-DATE  TO WS-HOLD-RATE-EXP-AL
01221                                    RTC-4.
01222
01223       MOVE WS-HOLD-RATE-EXP-DT  TO DC-GREG-DATE-1-YMD.
01224       MOVE '3'                  TO DC-OPTION-CODE.
01225       PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
01226       IF NO-CONVERSION-ERROR
01227           MOVE DC-ALPHA-CENTURY TO WS-HOLD-RATE-EXP-AL(4:2)
01228       ELSE
01229           IF WS-HOLD-RATE-EXP-DT(6:6) = 999999
01230               CONTINUE
01231           ELSE
01232               MOVE ZEROS        TO WS-HOLD-RATE-EXP-AL(4:2).
01233
01234       MOVE WS-HOLD-RATE-EXP-DT  TO RT-EXPIRY-DATE.
01235
01236  4025-CONT.
01237       IF STCOMML GREATER ZERO
01238           MOVE STCOMMI           TO RT-STRUCTURE-COMMENT.
01239
01240  4099-EXIT.
01241      EXIT.
01242      EJECT
01243  5000-EDIT-SCREEN-A.
01244      IF STATE2L GREATER ZERO
01245          MOVE SPACES       TO WS-ACCESS
01246          MOVE STATE2I      TO WS-STATE-CODE
01247          PERFORM 7400-EDIT-STATE THRU 7499-EXIT
01248          IF STATE-FOUND
01249              MOVE AL-UANON TO STATE2A
01250              MOVE STATE2I  TO PI-RATE-CODE
01251          ELSE
01252              MOVE -1       TO STATE2L
01253              MOVE AL-UABON TO STATE2A
01254              MOVE ER-2261  TO EMI-ERROR
01255              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01256      ELSE
01257          IF COPY-FUNCTION
01258              MOVE AL-UANON TO STATE2A
01259              MOVE STATE1I  TO STATE2O
01260                               PI-RATE-CODE
01261          ELSE
01262              MOVE -1       TO STATE2L
01263              MOVE AL-UABON TO STATE2A
01264              MOVE ER-2261  TO EMI-ERROR
01265              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01266
01267      IF CLASS2L GREATER ZERO
01268          MOVE AL-UANON     TO CLASS2A
01269          MOVE CLASS2I      TO PI-RATE-CLASS
01270      ELSE
01271          IF COPY-FUNCTION
01272              MOVE AL-UANON TO CLASS2A
01273              MOVE ZEROS    TO CLASS2O
01274                               PI-RATE-CLASS
01275          ELSE
01276              MOVE -1       TO CLASS2L
01277              MOVE AL-UABON TO CLASS2A
01278              MOVE ER-2262  TO EMI-ERROR
01279              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01280
01281      IF RTAGE2L GREATER ZERO
01282         MOVE RTAGE2I       TO  DEEDIT-FIELD
01283         PERFORM 7500-DEEDIT THRU 7500-EXIT
01284         IF DEEDIT-FIELD-V0 GREATER ZERO
01285            MOVE AL-UNNON   TO RTAGE2A
01286            MOVE RTAGE2I    TO PI-RATE-HIGH-AGE
01287         ELSE
01288            MOVE -1         TO RTAGE2L
01289            MOVE AL-UNBON   TO RTAGE2A
01290            MOVE ER-2187 TO EMI-ERROR
01291            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01292      ELSE
01293          IF COPY-FUNCTION
01294              MOVE ALL '9'  TO RTAGE2O
01295                               PI-RATE-HIGH-AGE
01296              MOVE AL-UNNON TO RTAGE2A
01297          ELSE
01298            MOVE -1         TO RTAGE2L
01299            MOVE AL-UNBON   TO RTAGE2A
01300            MOVE ER-2187 TO EMI-ERROR
01301            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01302
01303      IF RTAMT2L GREATER ZERO
01304         MOVE RTAMT2I       TO  DEEDIT-FIELD
01305         PERFORM 7500-DEEDIT THRU 7500-EXIT
01306         IF DEEDIT-FIELD-V0 GREATER ZERO
01307            MOVE AL-UNNON   TO RTAMT2A
01308            MOVE DEEDIT-FIELD-V0 TO PI-RATE-HIGH-AMT
01309         ELSE
01310            MOVE -1         TO RTAMT2L
01311            MOVE AL-UNBON   TO RTAMT2A
01312            MOVE ER-2189 TO EMI-ERROR
01313            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01314      ELSE
01315          IF COPY-FUNCTION
01316              MOVE ALL '9'  TO RTAMT2O
01317                               PI-RATE-HIGH-AMT
01318              MOVE AL-UNNON TO RTAMT2A
01319          ELSE
01320            MOVE -1         TO RTAMT2L
01321            MOVE AL-UNBON   TO RTAMT2A
01322            MOVE ER-2189 TO EMI-ERROR
01323            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01324
01325      IF DEV2L GREATER ZERO
01326          MOVE AL-UANON     TO DEV2A
01327          MOVE DEV2I        TO PI-RATE-DEV
01328      ELSE
01329          IF COPY-FUNCTION
01330              MOVE AL-UANON TO DEV2A
01331              MOVE ZEROS    TO DEV2O
01332                               PI-RATE-DEV
01333          ELSE
01334              MOVE -1       TO DEV2L
01335              MOVE AL-UABON TO DEV2A
01336              MOVE ER-2263  TO EMI-ERROR
01337              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01338
01339      IF TYPE2L GREATER ZERO
01340          IF TYPE2I = TYPE1I
01341              MOVE AL-UANON TO TYPE2A
01342              MOVE TYPE2I   TO PI-RATE-L-AH
01343          ELSE
01344              MOVE -1       TO TYPE2L
01345              MOVE AL-UABON TO TYPE2A
01346              MOVE ER-2293  TO EMI-ERROR
01347              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01348      ELSE
01349          IF COPY-FUNCTION
01350              MOVE AL-UANON TO TYPE2A
01351              MOVE TYPE1I   TO TYPE2O
01352                               PI-RATE-L-AH.
01353
01354      IF TYPE1I = PI-LIFE-OVERRIDE-L1
01355          MOVE '4'          TO WS-REC-TYPE
01356      ELSE
01357          MOVE '5'          TO WS-REC-TYPE.
01358
01359      IF PLAN2L GREATER ZERO
01360          MOVE PLAN2I       TO WS-HI-BENEFIT
01361          PERFORM 7300-EDIT-BENEFIT THRU 7300-EXIT
01362      ELSE
01363          IF COPY-FUNCTION
01364              MOVE AL-UNNON TO PLAN2L
01365              MOVE PLAN1I   TO PLAN2O
01366                               PI-RATE-LAH-NUM
01367          ELSE
01368              MOVE -1       TO PLAN2L
01369              MOVE AL-UABON TO PLAN2A
01370              MOVE ER-2268  TO EMI-ERROR
01371              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01372
01373      IF BENEFIT-FOUND
01374          MOVE PLAN2I       TO PI-RATE-LAH-NUM
01375          MOVE AL-UANON     TO PLAN2A
01376      ELSE
01377          MOVE -1           TO PLAN2L
01378          MOVE AL-UABON     TO PLAN2A
01379          MOVE ER-2268   TO EMI-ERROR
01380          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01381
01382      IF EXPIRE2L GREATER ZERO
01383          IF EXPIRE2I NUMERIC
01384              IF EXPIRE2I = WS-CURRENT-EXPIRE
01385                  MOVE AL-UNNON TO EXPIRE2A
01386                  MOVE ZEROS    TO WS-HOLD-RATE-EXP-DT
01387                  MOVE EXPIRE2I TO WS-HOLD-RATE-EXP-AL(6:6)
01388                                   DC-GREG-DATE-1-YMD
01389                  MOVE '3'      TO DC-OPTION-CODE
01390                  PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01391                  IF NO-CONVERSION-ERROR
01392                      MOVE DC-ALPHA-CENTURY TO
01393                           WS-HOLD-RATE-EXP-AL(4:2)
01394                      MOVE WS-HOLD-RATE-EXP-DT TO
01395                                              PI-RATE-EXPIRY-DATE
01396                  ELSE
01397                      IF WS-HOLD-RATE-EXP-DT(6:6) = 999999
01398                          MOVE 99999999999         TO
01399                               WS-HOLD-RATE-EXP-DT
01400                          MOVE WS-HOLD-RATE-EXP-DT TO
01401                                               PI-RATE-EXPIRY-DATE
01402                      ELSE
01403                          MOVE -1       TO EXPIRE1L
01404                          MOVE AL-UNBON TO EXPIRE1A
01405                          MOVE ER-2296  TO EMI-ERROR
01406                          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01407                      END-IF
01408                  END-IF
01409              ELSE
01410                  MOVE ZEROS    TO WS-HOLD-RATE-EXP-DT
01411                  MOVE EXPIRE2I TO DC-GREG-DATE-1-YMD
01412                                   WS-HOLD-RATE-EXP-AL(6:6)
01413                  MOVE '3'      TO DC-OPTION-CODE
01414                  PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01415                  IF NO-CONVERSION-ERROR
01416                      MOVE AL-UNNON TO EXPIRE2A
01417                      MOVE EXPIRE2I TO WS-HOLD-RATE-EXP-AL(6:6)
01418                      MOVE DC-ALPHA-CENTURY TO
01419                           WS-HOLD-RATE-EXP-AL(4:2)
01420                      MOVE WS-HOLD-RATE-EXP-DT TO
01421                                              PI-RATE-EXPIRY-DATE
01422                  ELSE
01423                      IF WS-HOLD-RATE-EXP-DT(6:6) = 999999
01424                          MOVE 99999999999         TO
01425                               WS-HOLD-RATE-EXP-DT
01426                          MOVE WS-HOLD-RATE-EXP-DT TO
01427                                               PI-RATE-EXPIRY-DATE
01428                      ELSE
01429                          MOVE -1 TO EXPIRE2L
01430                          MOVE AL-UNBON TO EXPIRE2A
01431                          MOVE ER-2296  TO EMI-ERROR
01432                          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01433                      END-IF
01434                  END-IF
01435              END-IF
01436          ELSE
01437              MOVE -1       TO EXPIRE2L
01438              MOVE AL-UNBON TO EXPIRE2A
01439              MOVE ER-2266  TO EMI-ERROR
01440              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01441          END-IF
01442      ELSE
01443          IF COPY-FUNCTION
01444              MOVE AL-UNNON TO EXPIRE2A
01445              MOVE ALL '9'  TO EXPIRE2O
01446              MOVE ZEROS    TO WS-HOLD-RATE-EXP-DT
01447              MOVE EXPIRE2I TO WS-HOLD-RATE-EXP-AL(6:6)
01448                               DC-GREG-DATE-1-YMD
01449              MOVE '3'      TO DC-OPTION-CODE
01450              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01451              IF NO-CONVERSION-ERROR
01452                  MOVE DC-ALPHA-CENTURY TO
01453                       WS-HOLD-RATE-EXP-AL(4:2)
01454                  MOVE WS-HOLD-RATE-EXP-DT TO
01455                       PI-RATE-EXPIRY-DATE
01456              ELSE
01457                  IF WS-HOLD-RATE-EXP-DT(6:6) = 999999
01458                      MOVE 99999999999         TO
01459                           WS-HOLD-RATE-EXP-DT
01460                      MOVE WS-HOLD-RATE-EXP-DT TO
01461                                           PI-RATE-EXPIRY-DATE
01462                  ELSE
01463                      MOVE -1       TO EXPIRE1L
01464                      MOVE AL-UNBON TO EXPIRE1A
01465                      MOVE ER-2296  TO EMI-ERROR
01466                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01467
01468  5099-EXIT.
01469      EXIT.
01470      EJECT
01471  6000-DEVIATE-RATES.
01472      MOVE +1  TO  SUB1.
01473
01474      DIVIDE WS-SAVE-FACTOR BY 100
01475          GIVING WS-DEVIATE-FACTOR.
01476
01477  6025-CONT.
01478      MOVE ZEROS                  TO WS-NEW-RATE.
01479
01480      MULTIPLY  WS-DEVIATE-FACTOR BY RT-L-RATE (SUB1)
01481          GIVING DEEDIT-FIELD-V5.
01482
01483      PERFORM 6100-ROUND-AS-REQUESTED THRU 6100-EXIT.
01484
01485      MOVE DEEDIT-FIELD-V5        TO RT-L-RATE (SUB1).
01486      ADD +1                      TO SUB1.
01487
01488      IF SUB1 GREATER 360
01489          GO TO 6099-EXIT
01490      ELSE
01491          GO TO 6025-CONT.
01492
01493  6099-EXIT.
01494      EXIT.
01495      EJECT
01496                                  EJECT
01497  6100-ROUND-AS-REQUESTED.
01498
01499      IF W-DECIMAL-NUMBER = 5
01500          GO TO 6100-EXIT.
01501
01502      IF W-DECIMAL-NUMBER = 0
01503          COMPUTE W-FIELD-V0 ROUNDED = DEEDIT-FIELD-V5
01504          MOVE W-FIELD-V0           TO DEEDIT-FIELD-V5
01505      ELSE
01506      IF W-DECIMAL-NUMBER = 1
01507          COMPUTE W-FIELD-V1 ROUNDED = DEEDIT-FIELD-V5
01508          MOVE W-FIELD-V1           TO DEEDIT-FIELD-V5
01509      ELSE
01510      IF W-DECIMAL-NUMBER = 2
01511          COMPUTE W-FIELD-V2 ROUNDED = DEEDIT-FIELD-V5
01512          MOVE W-FIELD-V2           TO DEEDIT-FIELD-V5
01513      ELSE
01514      IF W-DECIMAL-NUMBER = 3
01515          COMPUTE W-FIELD-V3 ROUNDED = DEEDIT-FIELD-V5
01516          MOVE W-FIELD-V3           TO DEEDIT-FIELD-V5
01517      ELSE
01518      IF W-DECIMAL-NUMBER = 4
01519           COMPUTE W-FIELD-V4 ROUNDED = DEEDIT-FIELD-V5
01520           MOVE W-FIELD-V4           TO DEEDIT-FIELD-V5.
01521
01522  6100-EXIT.
01523      EXIT.
01524                                  EJECT
01525  6500-SHOW-PLANS.
01526      IF SHOW-AH-PLANS
01527          MOVE PI-AH-OVERRIDE-L6   TO PLANTYPO.
01528
01529      IF SHOW-LF-PLANS
01530          MOVE PI-LIFE-OVERRIDE-L6 TO PLANTYPO.
01531
01532      MOVE PI-RATE-STATE-CODE     TO  WS-SAVE-STRUCTURE.
01533      MOVE LOW-VALUES             TO  PI-RATE-LIMITS
01534                                      PI-RATE-L-AH-CODE.
01535      MOVE ZEROS                  TO  PI-RATE-EXPIRY-DATE.
01536      SET PT-INDX                 TO  +1.
01537
01538      PERFORM 7800-START-BROWSE THRU 7800-EXIT.
01539
01540  6525-CONT.
01541      IF PT-INDX GREATER +24
01542          GO TO 6590-RESET-KEY.
01543
01544      
      * EXEC CICS HANDLE CONDITION
01545 *        ENDFILE  (6590-RESET-KEY)
01546 *    END-EXEC.
      *    MOVE '"$''                   ! ) #00005185' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303035313835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01547
01548      PERFORM 7850-READNEXT THRU 7850-EXIT.
01549
01550      IF ERRATE-EOF
01551          MOVE 'N'                TO  PI-ERRATE-EOF-SW
01552          IF BROWSE-STARTED
01553              PERFORM 7950-END-BROWSE THRU 7950-EXIT
01554              GO TO 6590-RESET-KEY.
01555
01556      IF PI-RATE-STATE-CODE = WS-SAVE-STRUCTURE
01557          NEXT SENTENCE
01558      ELSE
01559          GO TO 6590-RESET-KEY.
01560
01561      IF SHOW-AH-PLANS
01562          IF PI-RATE-L-AH = PI-AH-OVERRIDE-L1
01563              MOVE '5'            TO WS-REC-TYPE
01564          ELSE
01565              GO TO 6525-CONT.
01566
01567      IF SHOW-LF-PLANS
01568          IF PI-RATE-L-AH = PI-LIFE-OVERRIDE-L1
01569              MOVE '4'            TO WS-REC-TYPE
01570          ELSE
01571              GO TO 6525-CONT.
01572
01573      IF FIRST-REC-IN-STRUCTURE
01574          MOVE 'N'                  TO WS-FIRST-REC-SW
01575          MOVE RT-ST-CODE           TO STATE1O
01576          MOVE RT-ST-CLASS          TO CLASS1O
01577          MOVE RT-HIGH-AGE          TO RTAGE1O
01578          MOVE RT-HIGH-AMT          TO RTAMT1O
01579          MOVE RT-ST-DEV            TO DEV1O
01580          MOVE RT-EXPIRY-DATE       TO WS-HOLD-RATE-EXP-DT
01581          MOVE WS-HOLD-RATE-EXP-AL  TO EXPIRE1O
01582          MOVE RT-L-AH              TO TYPE1O
01583          MOVE RT-LAH-NUM           TO PLAN1O
01584          MOVE RT-STRUCTURE-COMMENT TO STCOMMO
01585          MOVE RT-LAST-MAINT-DT     TO DC-BIN-DATE-1
01586          MOVE ' '                  TO DC-OPTION-CODE
01587          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01588          MOVE DC-GREG-DATE-1-EDIT  TO LUDATEO
01589          MOVE RT-LAST-MAINT-USER   TO LUBYO
01590          MOVE RT-LAST-MAINT-HHMMSS TO TIME-LMT
01591          MOVE TIME-LMT             TO LUTIMEO
01592          MOVE 'N'                  TO PI-FIRST-TIME-SW
01593          MOVE PI-ERRATE-KEY        TO PI-SAVE-ERRATE-KEY.
01594
01595      MOVE PI-RATE-LAH-NUM          TO PT-CODE (PT-INDX)
01596      MOVE PI-RATE-EXPIRY-DATE      TO PT-EXPIRE (PT-INDX)
01597      MOVE PI-RATE-LAH-NUM          TO WS-HI-BENEFIT.
01598      MOVE RT-HIGH-AGE              TO PT-AGE (PT-INDX).
01599
01600      PERFORM 7300-EDIT-BENEFIT   THRU 7300-EXIT.
01601
01602      IF BENEFIT-FOUND
01603          MOVE +1 TO SUB1
01604      ELSE
01605          MOVE ALL '*' TO PT-DESC (PT-INDX)
01606          SET PT-INDX UP BY +1
01607          GO TO 6525-CONT.
01608
01609  6575-SEARCH-BENEFIT-TABLE.
01610      IF SUB1 GREATER +8
01611          MOVE ALL '*' TO PT-DESC (PT-INDX)
01612          SET PT-INDX UP BY +1
01613          GO TO 6525-CONT.
01614
01615      IF CF-BENEFIT-CODE (SUB1) = PI-RATE-LAH-NUM
01616          MOVE CF-BENEFIT-DESCRIP (SUB1) TO PT-DESC (PT-INDX)
01617      ELSE
01618          ADD +1 TO SUB1
01619          GO TO 6575-SEARCH-BENEFIT-TABLE.
01620
01621      SET PT-INDX UP BY +1.
01622      GO TO 6525-CONT.
01623
01624  6590-RESET-KEY.
01625      MOVE PI-SAVE-ERRATE-KEY TO PI-ERRATE-KEY.
01626
01627  6599-EXIT.
01628      EXIT.
01629      EJECT
01630
01631  7100-NEXT-STRUCTURE.
01632      MOVE SPACES             TO  PI-ERRATE-EOF-SW.
01633      MOVE LOW-VALUES         TO  PI-ERRATE-KEY.
01634
01635      IF STATE1L GREATER ZERO
01636          MOVE AL-UANON   TO STATE1A
01637          MOVE STATE1I    TO PI-RATE-CODE
01638      ELSE
01639          MOVE LOW-VALUES TO PI-RATE-CODE.
01640
01641      IF CLASS1L GREATER ZERO
01642          MOVE AL-UANON   TO CLASS1A
01643          MOVE CLASS1I    TO PI-RATE-CLASS
01644      ELSE
01645          MOVE LOW-VALUES TO PI-RATE-CLASS.
01646
01647      IF DEV1L GREATER ZERO
01648          MOVE AL-UANON   TO DEV1A
01649          MOVE DEV1I      TO PI-RATE-DEV
01650      ELSE
01651          MOVE LOW-VALUES TO PI-RATE-DEV.
01652
01653      IF EIBAID = DFHPF1
01654          GO TO 7000-START-BROWSE.
01655
01656      IF RTAGE1L GREATER ZERO
01657          MOVE AL-UANON   TO RTAGE1A
01658          MOVE RTAGE1I    TO PI-RATE-HIGH-AGE
01659      ELSE
01660          MOVE ZEROS      TO PI-RATE-HIGH-AGE.
01661
01662      IF RTAMT1L GREATER ZERO
01663         MOVE RTAMT1I             TO  DEEDIT-FIELD
01664         PERFORM 7500-DEEDIT THRU 7500-EXIT
01665         IF DEEDIT-FIELD-V0 GREATER ZERO
01666            MOVE AL-UNNON         TO RTAMT1A
01667            MOVE DEEDIT-FIELD-V0  TO PI-RATE-HIGH-AMT
01668         ELSE
01669            MOVE ZEROS            TO PI-RATE-HIGH-AMT
01670      ELSE
01671         MOVE ZEROS               TO PI-RATE-HIGH-AMT.
01672
01673      IF TYPE1L GREATER ZERO
01674          MOVE AL-UANON   TO TYPE1A
01675          MOVE TYPE1I     TO PI-RATE-L-AH
01676      ELSE
01677          MOVE LOW-VALUES TO PI-RATE-L-AH.
01678
01679      IF EXPIRE1L GREATER ZERO
01680          MOVE AL-UNNON   TO EXPIRE1A
01681          MOVE ZEROS      TO WS-HOLD-RATE-EXP-DT
01682          MOVE EXPIRE1I   TO WS-HOLD-RATE-EXP-AL(6:6)
01683                             DC-GREG-DATE-1-YMD
01684          MOVE '3'        TO DC-OPTION-CODE
01685          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01686          IF NO-CONVERSION-ERROR
01687              MOVE DC-ALPHA-CENTURY TO
01688                   WS-HOLD-RATE-EXP-AL(4:2)
01689              MOVE WS-HOLD-RATE-EXP-DT TO PI-RATE-EXPIRY-DATE
01690          ELSE
01691              IF WS-HOLD-RATE-EXP-DT(6:6) = 999999
01692                  MOVE 99999999999         TO
01693                       WS-HOLD-RATE-EXP-DT
01694                  MOVE WS-HOLD-RATE-EXP-DT TO
01695                                       PI-RATE-EXPIRY-DATE
01696              ELSE
01697                  MOVE -1       TO EXPIRE1L
01698                  MOVE AL-UNBON TO EXPIRE1A
01699                  MOVE ER-2296  TO EMI-ERROR
01700                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01701              END-IF
01702          END-IF
01703      ELSE
01704          MOVE ZEROS               TO WS-HOLD-RATE-EXP-AL
01705          MOVE 99999999999         TO WS-HOLD-RATE-EXP-AL
01706          MOVE WS-HOLD-RATE-EXP-DT TO PI-RATE-EXPIRY-DATE.
01707
01708      IF PLAN1L GREATER ZERO
01709          MOVE AL-UANON   TO PLAN1A
01710          MOVE PLAN1I     TO PI-RATE-LAH-NUM
01711      ELSE
01712          MOVE ZEROS      TO PI-RATE-LAH-NUM.
01713
01714      MOVE ALL '9'        TO PI-RATE-SEX
01715                             PI-RATE-FUTURE.
01716
01717  7000-START-BROWSE.
01718      MOVE PI-COMPANY-CD      TO  PI-RATE-COMPANY-CD.
01719      MOVE PI-RATE-STATE-CODE TO  WS-SAVE-STRUCTURE.
01720
01721      PERFORM 7800-START-BROWSE THRU 7800-EXIT.
01722
01723  7125-READ-NEXT.
01724      
      * EXEC CICS HANDLE CONDITION
01725 *        ENDFILE  (7150-ENDFILE)
01726 *        NOTFND   (7175-NOTFOUND)
01727 *    END-EXEC.
      *    MOVE '"$''I                  ! * #00005365' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303035333635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01728
01729      PERFORM 7850-READNEXT THRU 7850-EXIT.
01730
01731      IF ERRATE-EOF
01732          IF FIRST-TIME
01733              MOVE LOW-VALUES TO EL656AO
01734              MOVE ER-2272 TO EMI-ERROR
01735              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01736              MOVE -1         TO MAINTL
01737              GO TO 8100-SEND-INITIAL-MAP
01738          ELSE
01739              PERFORM 7950-END-BROWSE THRU 7950-EXIT
01740              MOVE LOW-VALUES TO EL656AO
01741              MOVE ER-2271 TO EMI-ERROR
01742              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01743              MOVE 'Y'        TO WS-FIRST-TIME-SW
01744                                 WS-EOF-DISPLAY-SW
01745              GO TO 7100-NEXT-STRUCTURE.
01746
01747      IF EIBAID = DFHPF1
01748          IF PI-RATE-STATE-CODE = WS-SAVE-STRUCTURE
01749              MOVE SPACES     TO WS-FIRST-TIME-SW
01750              MOVE 'Z'        TO PI-RATE-L-AH
01751              GO TO 7125-READ-NEXT.
01752
01753      IF NOT EOF-HIT-DISPLAY-FIRST
01754          IF PI-ERRATE-KEY = PI-SAVE-ERRATE-KEY
01755              IF FIRST-PLAN-SHOWN
01756                 MOVE LOW-VALUES TO PI-RATE-L-AH-CODE
01757                                    PI-RATE-LIMITS
01758                 MOVE ZEROS      TO PI-RATE-EXPIRY-DATE
01759                 MOVE 'N'        TO PI-FIRST-TIME-SW
01760                 GO TO 7125-READ-NEXT
01761              ELSE
01762                 MOVE SPACES     TO WS-FIRST-TIME-SW
01763                 GO TO 7125-READ-NEXT.
01764
01765      MOVE RT-LAST-MAINT-USER     TO PI-UPDATE-BY.
01766      MOVE RT-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.
01767
01768      MOVE PI-ERRATE-KEY          TO PI-SAVE-ERRATE-KEY.
01769
01770      GO TO 3025-SET-UP-SCREEN-A.
01771
01772  7150-ENDFILE.
01773      IF BROWSE-STARTED
01774          PERFORM 7950-END-BROWSE THRU 7950-EXIT.
01775
01776      IF FIRST-TIME
01777          MOVE LOW-VALUES TO EL656AO
01778          MOVE ER-2272 TO EMI-ERROR
01779          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01780          MOVE -1         TO MAINTL
01781          GO TO 8100-SEND-INITIAL-MAP
01782      ELSE
01783          MOVE LOW-VALUES TO EL656AO
01784          MOVE ER-2271 TO EMI-ERROR
01785          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01786          MOVE 'Y'        TO WS-FIRST-TIME-SW
01787                             WS-EOF-DISPLAY-SW
01788          GO TO 7100-NEXT-STRUCTURE.
01789
01790  7175-NOTFOUND.
01791      IF BROWSE-STARTED
01792          PERFORM 7950-END-BROWSE THRU 7950-EXIT.
01793
01794      GO TO 8880-NOT-FOUND.
01795
01796  7199-EXIT.
01797      EXIT.
01798      EJECT
01799
01800  7200-PRIOR-STRUCTURE.
01801      MOVE SPACES             TO  PI-ERRATE-EOF-SW.
01802
01803      MOVE LOW-VALUES         TO  PI-ERRATE-KEY.
01804
01805      IF STATE1L GREATER ZERO
01806          MOVE AL-UANON   TO STATE1A
01807          MOVE STATE1I    TO PI-RATE-CODE
01808      ELSE
01809          MOVE LOW-VALUES TO PI-RATE-CODE.
01810
01811      IF CLASS1L GREATER ZERO
01812          MOVE AL-UANON   TO CLASS1A
01813          MOVE CLASS1I    TO PI-RATE-CLASS
01814      ELSE
01815          MOVE LOW-VALUES TO PI-RATE-CLASS.
01816
01817      IF DEV1L GREATER ZERO
01818          MOVE AL-UANON   TO DEV1A
01819          MOVE DEV1I      TO PI-RATE-DEV
01820      ELSE
01821          MOVE LOW-VALUES TO PI-RATE-DEV.
01822
01823      IF EIBAID = DFHPF2
01824          GO TO 7200-START-BROWSE.
01825
01826      IF RTAGE1L GREATER ZERO
01827          MOVE AL-UANON   TO RTAGE1A
01828          MOVE RTAGE1I    TO PI-RATE-HIGH-AGE
01829      ELSE
01830          MOVE ZEROS      TO PI-RATE-HIGH-AGE.
01831
01832      IF RTAMT1L GREATER ZERO
01833         MOVE RTAMT1I             TO  DEEDIT-FIELD
01834         PERFORM 7500-DEEDIT THRU 7500-EXIT
01835         IF DEEDIT-FIELD-V0 GREATER ZERO
01836            MOVE AL-UNNON         TO RTAMT1A
01837            MOVE DEEDIT-FIELD-V0  TO PI-RATE-HIGH-AMT
01838         ELSE
01839            MOVE ZEROS            TO PI-RATE-HIGH-AMT
01840      ELSE
01841         MOVE ZEROS               TO PI-RATE-HIGH-AMT.
01842
01843      IF TYPE1L GREATER ZERO
01844          MOVE AL-UANON   TO TYPE1A
01845          MOVE TYPE1I     TO PI-RATE-L-AH
01846      ELSE
01847          MOVE LOW-VALUES TO PI-RATE-L-AH.
01848
01849      IF EXPIRE1L GREATER ZERO
01850          MOVE AL-UNNON   TO EXPIRE1A
01851          MOVE ZEROS      TO WS-HOLD-RATE-EXP-DT
01852          MOVE EXPIRE1I   TO WS-HOLD-RATE-EXP-AL(6:6)
01853                             DC-GREG-DATE-1-YMD
01854          MOVE '3'        TO DC-OPTION-CODE
01855          PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01856          IF NO-CONVERSION-ERROR
01857              MOVE DC-ALPHA-CENTURY TO
01858                   WS-HOLD-RATE-EXP-AL(4:2)
01859              MOVE WS-HOLD-RATE-EXP-DT TO PI-RATE-EXPIRY-DATE
01860          ELSE
01861              IF WS-HOLD-RATE-EXP-DT(6:6) = 999999
01862                  MOVE 99999999999         TO
01863                       WS-HOLD-RATE-EXP-DT
01864                  MOVE WS-HOLD-RATE-EXP-DT TO
01865                                       PI-RATE-EXPIRY-DATE
01866              ELSE
01867                  MOVE -1       TO EXPIRE1L
01868                  MOVE AL-UNBON TO EXPIRE1A
01869                  MOVE ER-2296  TO EMI-ERROR
01870                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01871              END-IF
01872          END-IF
01873      ELSE
01874          MOVE ZEROS               TO WS-HOLD-RATE-EXP-AL
01875          MOVE 99999999999         TO WS-HOLD-RATE-EXP-AL
01876          MOVE WS-HOLD-RATE-EXP-DT TO PI-RATE-EXPIRY-DATE.
01877
01878      IF PLAN1L GREATER ZERO
01879          MOVE AL-UANON   TO PLAN1A
01880          MOVE PLAN1I     TO PI-RATE-LAH-NUM
01881      ELSE
01882          MOVE ZEROS      TO PI-RATE-LAH-NUM.
01883
01884  7200-START-BROWSE.
01885      MOVE PI-COMPANY-CD      TO  PI-RATE-COMPANY-CD.
01886      MOVE PI-RATE-STATE-CODE TO  WS-SAVE-STRUCTURE.
01887
01888      PERFORM 7800-START-BROWSE THRU 7800-EXIT.
01889
01890      
      * EXEC CICS HANDLE CONDITION
01891 *        ENDFILE  (7250-ENDFILE)
01892 *        NOTFND   (7275-NOTFOUND)
01893 *    END-EXEC.
      *    MOVE '"$''I                  ! + #00005531' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303035353331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01894
01895      PERFORM 7850-READNEXT THRU 7850-EXIT.
01896
01897      IF ERRATE-EOF
01898          IF FIRST-TIME
01899              PERFORM 7950-END-BROWSE THRU 7950-EXIT
01900              MOVE LOW-VALUES TO EL656AO
01901              MOVE ER-2272 TO EMI-ERROR
01902              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01903              GO TO 8100-SEND-INITIAL-MAP
01904          ELSE
01905              PERFORM 7950-END-BROWSE THRU 7950-EXIT
01906              MOVE LOW-VALUES TO EL656AO
01907              MOVE ER-2271 TO EMI-ERROR
01908              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01909              GO TO 7100-NEXT-STRUCTURE.
01910
01911      MOVE SPACES TO WS-FIRST-TIME-SW.
01912
01913  7225-READ-PREV.
01914      
      * EXEC CICS HANDLE CONDITION
01915 *        ENDFILE  (7250-ENDFILE)
01916 *        NOTFND   (7275-NOTFOUND)
01917 *    END-EXEC.
      *    MOVE '"$''I                  ! , #00005555' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303035353535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01918
01919      PERFORM 7900-READPREV THRU 7900-EXIT.
01920
01921      IF ERRATE-EOF
01922          PERFORM 7950-END-BROWSE THRU 7950-EXIT
01923          MOVE LOW-VALUES TO EL656AO
01924          MOVE ER-2271 TO EMI-ERROR
01925          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01926          MOVE 'Y'        TO WS-FIRST-TIME-SW
01927          GO TO 7100-NEXT-STRUCTURE.
01928
01929      IF EIBAID = DFHPF2
01930          IF PI-RATE-STATE-CODE = WS-SAVE-STRUCTURE
01931              MOVE SPACES     TO WS-FIRST-TIME-SW
01932              GO TO 7225-READ-PREV.
01933
01934      IF PI-ERRATE-KEY = PI-SAVE-ERRATE-KEY
01935          IF FIRST-PLAN-SHOWN
01936              MOVE SPACES         TO PI-FIRST-TIME-SW
01937          ELSE
01938              MOVE SPACES         TO WS-FIRST-TIME-SW
01939              GO TO 7225-READ-PREV.
01940
01941      IF BROWSE-STARTED
01942          PERFORM 7950-END-BROWSE THRU 7950-EXIT.
01943
01944      
      * EXEC CICS HANDLE CONDITION
01945 *        ENDFILE  (7250-ENDFILE)
01946 *        NOTFND   (7275-NOTFOUND)
01947 *    END-EXEC.
      *    MOVE '"$''I                  ! - #00005585' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303035353835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01948
01949      PERFORM 7600-READ-ERRATE-GTEQ THRU 7600-EXIT.
01950
01951      IF ERRATE-EOF
01952          IF BROWSE-STARTED
01953              PERFORM 7950-END-BROWSE THRU 7950-EXIT
01954              MOVE LOW-VALUES TO EL656AO
01955              MOVE 'Y'        TO WS-FIRST-TIME-SW
01956              GO TO 7100-NEXT-STRUCTURE.
01957
01958      MOVE RT-LAST-MAINT-USER     TO PI-UPDATE-BY.
01959      MOVE RT-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.
01960      MOVE RT-CONTROL-PRIMARY     TO PI-ERRATE-KEY
01961                                     PI-SAVE-ERRATE-KEY.
01962
01963      GO TO 3025-SET-UP-SCREEN-A.
01964
01965  7250-ENDFILE.
01966      IF BROWSE-STARTED
01967          PERFORM 7950-END-BROWSE THRU 7950-EXIT.
01968
01969      IF FIRST-TIME
01970          MOVE LOW-VALUES TO EL656AO
01971          MOVE ER-2272 TO EMI-ERROR
01972          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01973          MOVE -1         TO MAINTL
01974          GO TO 8100-SEND-INITIAL-MAP
01975      ELSE
01976          MOVE ER-2271 TO EMI-ERROR
01977          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01978          MOVE LOW-VALUES TO EL656AO
01979          MOVE 'Y'        TO WS-FIRST-TIME-SW
01980          GO TO 7100-NEXT-STRUCTURE.
01981
01982  7275-NOTFOUND.
01983      IF BROWSE-STARTED
01984          PERFORM 7950-END-BROWSE THRU 7950-EXIT.
01985
01986      GO TO 8880-NOT-FOUND.
01987
01988  7299-EXIT.
01989      EXIT.
01990      EJECT
01991
01992  7300-EDIT-BENEFIT.
01993      MOVE SPACES                 TO WS-BENEFIT-FOUND-SW.
01994
01995      IF INVALID-PLAN-CODE
01996          GO TO 7300-EXIT.
01997
01998      MOVE LOW-VALUES             TO ELCNTL-KEY.
01999      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
02000      MOVE +0                     TO CNTL-SEQ-NO.
02001      MOVE WS-BENEFIT-ACCESS      TO CNTL-ACCESS.
02002      MOVE WS-REC-TYPE            TO CNTL-REC-TYPE.
02003
02004      
      * EXEC CICS HANDLE CONDITION
02005 *        NOTFND   (7300-EXIT)
02006 *    END-EXEC.
      *    MOVE '"$I                   ! . #00005645' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303035363435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02007
02008      
      * EXEC CICS READ
02009 *        DATASET   (CNTL-FILE-ID)
02010 *        SET       (ADDRESS OF CONTROL-FILE)
02011 *        RIDFLD    (ELCNTL-KEY)
02012 *        GTEQ
02013 *    END-EXEC.
      *    MOVE '&"S        G          (   #00005649' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363439' TO DFHEIV0(25:11)
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
           
02014
02015      IF PI-RATE-L-AH = PI-LIFE-OVERRIDE-L1
02016          IF CF-LF-BENEFIT-MASTER
02017              NEXT SENTENCE
02018          ELSE
02019              GO TO 7300-EXIT.
02020
02021      IF PI-RATE-L-AH = PI-AH-OVERRIDE-L1
02022          IF CF-AH-BENEFIT-MASTER
02023              NEXT SENTENCE
02024          ELSE
02025              GO TO 7300-EXIT.
02026
02027      MOVE 'Y'  TO  WS-BENEFIT-FOUND-SW.
02028
02029  7300-EXIT.
02030      EXIT.
02031      EJECT
02032
02033  7400-EDIT-STATE.
02034      MOVE SPACES                 TO WS-STATE-FOUND-SW.
02035      MOVE LOW-VALUES             TO ELCNTL-KEY.
02036      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
02037      MOVE '3'                    TO CNTL-REC-TYPE.
02038      MOVE +0                     TO CNTL-SEQ-NO.
02039      MOVE WS-ACCESS              TO CNTL-ACCESS.
02040
02041      
      * EXEC CICS HANDLE CONDITION
02042 *        NOTFND   (7499-EXIT)
02043 *    END-EXEC.
      *    MOVE '"$I                   ! / #00005682' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2F20233030303035363832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02044
02045      
      * EXEC CICS READ
02046 *        DATASET   (CNTL-FILE-ID)
02047 *        SET       (ADDRESS OF CONTROL-FILE)
02048 *        RIDFLD    (ELCNTL-KEY)
02049 *    END-EXEC.
      *    MOVE '&"S        E          (   #00005686' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363836' TO DFHEIV0(25:11)
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
           
02050
02051      MOVE 'Y' TO WS-STATE-FOUND-SW.
02052
02053  7499-EXIT.
02054      EXIT.
02055      EJECT
02056
02057  7500-DEEDIT.
02058      
      * EXEC CICS BIF
02059 *         DEEDIT
02060 *         FIELD  (DEEDIT-FIELD)
02061 *         LENGTH (15)
02062 *     END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005699' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02063
02064  7500-EXIT.
02065      EXIT.
02066      EJECT
02067
02068  7600-READ-ERRATE-GTEQ.
02069      MOVE PI-COMPANY-CD  TO  PI-RATE-COMPANY-CD.
02070
02071      
      * EXEC CICS READ
02072 *         DATASET  (RATE-FILE-ID)
02073 *         SET      (ADDRESS OF RATE-RECORD)
02074 *         RIDFLD   (PI-ERRATE-KEY)
02075 *         GTEQ
02076 *    END-EXEC.
      *    MOVE '&"S        G          (   #00005712' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RATE-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERRATE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF RATE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02077
02078      MOVE RT-LAST-MAINT-USER     TO PI-UPDATE-BY.
02079      MOVE RT-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.
02080
02081  7600-EXIT.
02082      EXIT.
02083      EJECT
02084
02085  7650-READ-ERRATE.
02086      MOVE PI-COMPANY-CD  TO  PI-RATE-COMPANY-CD.
02087
02088      
      * EXEC CICS READ
02089 *         DATASET  (RATE-FILE-ID)
02090 *         SET      (ADDRESS OF RATE-RECORD)
02091 *         RIDFLD   (PI-ERRATE-KEY)
02092 *    END-EXEC.
      *    MOVE '&"S        E          (   #00005729' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RATE-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERRATE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF RATE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02093
02094      MOVE RT-LAST-MAINT-USER     TO PI-UPDATE-BY.
02095      MOVE RT-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.
02096
02097  7650-EXIT.
02098      EXIT.
02099      EJECT
02100
02101  7700-ERRATE-GETMAIN.
02102      
      * EXEC CICS GETMAIN
02103 *         SET     (ADDRESS OF RATE-RECORD)
02104 *         LENGTH  (ERRATE-LENGTH)
02105 *         INITIMG (GETMAIN-SPACE)
02106 *    END-EXEC.
      *    MOVE ',"IL                  $   #00005743' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERRATE-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF RATE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02107
02108  7700-EXIT.
02109      EXIT.
02110      EJECT
02111
02112  7750-READ-ERRATE-UPDATE.
02113      MOVE PI-COMPANY-CD  TO  PI-RATE-COMPANY-CD.
02114
02115      
      * EXEC CICS READ
02116 *         DATASET  (RATE-FILE-ID)
02117 *         SET      (ADDRESS OF RATE-RECORD)
02118 *         RIDFLD   (PI-ERRATE-KEY)
02119 *         UPDATE
02120 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005756' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RATE-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERRATE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF RATE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02121
02122  7750-EXIT.
02123      EXIT.
02124      EJECT
02125
02126  7800-START-BROWSE.
02127      
      * EXEC CICS STARTBR
02128 *         DATASET  (RATE-FILE-ID)
02129 *         RIDFLD   (PI-ERRATE-KEY)
02130 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005768' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RATE-FILE-ID, 
                 PI-ERRATE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02131
02132      MOVE 'Y' TO PI-BROWSE-SW.
02133
02134  7800-EXIT.
02135      EXIT.
02136      EJECT
02137
02138  7850-READNEXT.
02139      
      * EXEC CICS READNEXT
02140 *         DATASET  (RATE-FILE-ID)
02141 *         SET      (ADDRESS OF RATE-RECORD)
02142 *         RIDFLD   (PI-ERRATE-KEY)
02143 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005780' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RATE-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERRATE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF RATE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02144
02145      IF PI-COMPANY-CD NOT = RT-COMPANY-CD
02146          MOVE 'Y'        TO PI-ERRATE-EOF-SW.
02147
02148  7850-EXIT.
02149      EXIT.
02150      EJECT
02151
02152  7900-READPREV.
02153      
      * EXEC CICS READPREV
02154 *         DATASET  (RATE-FILE-ID)
02155 *         SET      (ADDRESS OF RATE-RECORD)
02156 *         RIDFLD   (PI-ERRATE-KEY)
02157 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00005794' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RATE-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERRATE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF RATE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02158
02159      IF PI-COMPANY-CD NOT = RT-COMPANY-CD
02160          MOVE 'Y'        TO PI-ERRATE-EOF-SW.
02161
02162  7900-EXIT.
02163      EXIT.
02164      EJECT
02165
02166  7950-END-BROWSE.
02167      
      * EXEC CICS ENDBR
02168 *         DATASET  (RATE-FILE-ID)
02169 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005808' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RATE-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02170
02171      MOVE SPACE TO PI-BROWSE-SW.
02172
02173  7950-EXIT.
02174      EXIT.
02175      EJECT
02176  8000-UPDATE-MAINT-DATE.
02177      MOVE SPACES                 TO ELCNTL-KEY.
02178
02179      MOVE PI-COMPANY-ID          TO CNTL-COMP-ID.
02180      MOVE '1'                    TO CNTL-REC-TYPE.
02181      MOVE +0                     TO CNTL-SEQ-NO.
02182
02183      
      * EXEC CICS HANDLE CONDITION
02184 *        NOTFND   (8000-EXIT)
02185 *    END-EXEC.
      *    MOVE '"$I                   ! 0 #00005824' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3020233030303035383234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02186
02187      
      * EXEC CICS READ
02188 *        UPDATE
02189 *        DATASET   (CNTL-FILE-ID)
02190 *        SET       (ADDRESS OF CONTROL-FILE)
02191 *        RIDFLD    (ELCNTL-KEY)
02192 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005828' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383238' TO DFHEIV0(25:11)
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
           
02193
02194      MOVE CONTROL-FILE           TO JP-RECORD-AREA.
02195      MOVE 'B'                    TO JP-RECORD-TYPE.
02196      MOVE CNTL-FILE-ID           TO FILE-ID.
081413*    PERFORM 8400-LOG-JOURNAL-RECORD
081413*                                thru 8400-exit
02198
02199      MOVE BIN-CURRENT-SAVE       TO CF-RATES-FILE-MAINT-DT.
02200
02201      MOVE CONTROL-FILE           TO JP-RECORD-AREA.
02202      MOVE 'C'                    TO JP-RECORD-TYPE.
02203      MOVE CNTL-FILE-ID           TO FILE-ID.
02204
02205      
      * EXEC CICS REWRITE
02206 *        DATASET   (CNTL-FILE-ID)
02207 *        FROM      (CONTROL-FILE)
02208 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005847' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02209
081413*    PERFORM 8400-LOG-JOURNAL-RECORD
081413*                                thru 8400-exit
           .
02212  8000-EXIT.
02213       EXIT.
02214      EJECT
02215
02216  8100-SEND-INITIAL-MAP.
02217      MOVE EIBTIME              TO TIME-IN.
02218      MOVE SAVE-DATE            TO RUNDATEO.
02219      MOVE TIME-OUT             TO RUNTIMEO.
101501     MOVE PI-COMPANY-ID        TO CMPNYIDO.
101501     MOVE PI-PROCESSOR-ID      TO USERIDO.
02220      MOVE EMI-MESSAGE-AREA (1) TO ERRMSG1O.
02221      MOVE -1                   TO MAINTL.
02222      MOVE PI-AH-OVERRIDE-L6    TO AHPLANO.
02223      MOVE PI-LIFE-OVERRIDE-L6  TO LFPLANO.
02224
02225      
      * EXEC CICS SEND
02226 *        MAP   (WS-MAPNAME)
02227 *        MAPSET(WS-MAPSET-NAME)
02228 *        FROM  (EL656AO)
02229 *        ERASE
02230 *        CURSOR
02231 *    END-EXEC.
           MOVE LENGTH OF
            EL656AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00005870' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAPNAME, 
                 EL656AO, 
                 DFHEIV12, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02232
02233      GO TO 9100-RETURN-TRAN.
02234
02235  8200-SEND-DATAONLY.
02236      MOVE EIBTIME              TO TIME-IN.
02237      MOVE SAVE-DATE            TO RUNDATEO.
02238      MOVE TIME-OUT             TO RUNTIMEO.
101501     MOVE PI-COMPANY-ID        TO CMPNYIDO.
101501     MOVE PI-PROCESSOR-ID      TO USERIDO.
02239      MOVE EMI-MESSAGE-AREA (1) TO ERRMSG1O.
02240
02241      
      * EXEC CICS SEND
02242 *        MAP   (WS-MAPNAME)
02243 *        MAPSET(WS-MAPSET-NAME)
02244 *        FROM  (EL656AO)
02245 *        DATAONLY
02246 *        CURSOR
02247 *    END-EXEC.
           MOVE LENGTH OF
            EL656AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00005888' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAPNAME, 
                 EL656AO, 
                 DFHEIV12, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02248
02249      GO TO 9100-RETURN-TRAN.
02250
02251  8300-SEND-TEXT.
02252      
      * EXEC CICS SEND TEXT
02253 *        FROM  (LOGOFF-TEXT)
02254 *        LENGTH(LOGOFF-LENGTH)
02255 *        ERASE
02256 *        FREEKB
02257 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00005899' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383939' TO DFHEIV0(25:11)
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
           
02258
02259      
      * EXEC CICS RETURN
02260 *    END-EXEC.
      *    MOVE '.(                    ''   #00005906' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
081413 8400-LOG-JOURNAL-RECORD.
081413
081413     if pi-journal-file-id = 0
081413        go to 8400-exit
081413     end-if
081413
081413     move eibdate                to jp-date
081413     move eibtime                to jp-time
081413     move RATE-FILE-ID           TO JP-FILE-ID
081413     MOVE PI-PROCESSOR-ID        TO JP-USER-ID
081413     MOVE 02                     TO PI-JOURNAL-FILE-ID
081413     MOVE THIS-PGM               TO JP-PROGRAM-ID
081413
081413     
      * EXEC CICS JOURNAL
081413*       JFILEID   (PI-JOURNAL-FILE-ID)
081413*       JTYPEID   ('EL')
081413*       FROM      (JOURNAL-RECORD)
081413*       LENGTH    (2000)
081413*       resp      (ws-response)
081413*    END-EXEC
           MOVE 'EL' TO DFHEIV7
           MOVE 2000
             TO DFHEIV11
      *    MOVE '4"LF                  (  N#00005921' TO DFHEIV0
           MOVE X'34224C462020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303035393231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-JOURNAL-FILE-ID, 
                 DFHEIV7, 
                 JOURNAL-RECORD, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
081413
081413     if ws-resp-normal
081413        continue
081413     else
081413        display ' error-el656-journal ' ws-response
081413     end-if
081413
081413     .
081413 8400-exit.
081413     exit.
02275  8800-UNAUTHORIZED-ACCESS.
02276      MOVE UNACCESS-MSG TO LOGOFF-MSG.
02277      GO TO 8300-SEND-TEXT.
02278
02279  8810-PF23.
02280      MOVE EIBAID   TO PI-ENTRY-CD-1.
02281      MOVE XCTL-005 TO PGM-NAME.
02282      GO TO 9300-XCTL.
02283
02284  8880-NOT-FOUND.
02285      MOVE ER-0142 TO EMI-ERROR.
02286      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02287      MOVE -1   TO MAINTL.
02288
02289      IF EIBTRNID NOT = TRANS-ID
02290          GO TO 8100-SEND-INITIAL-MAP.
02291
02292      GO TO 8200-SEND-DATAONLY.
02293
02294  9100-RETURN-TRAN.
02295      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.
02296      MOVE '656A'               TO PI-CURRENT-SCREEN-NO.
02297      
      * EXEC CICS RETURN
02298 *        TRANSID (TRANS-ID)
02299 *        COMMAREA(PROGRAM-INTERFACE-BLOCK)
02300 *        LENGTH  (PI-COMM-LENGTH)
02301 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00005960' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02302
02303  9200-RETURN-MAIN-MENU.
02304      MOVE XCTL-626 TO PGM-NAME.
02305      GO TO 9300-XCTL.
02306
02307  9300-XCTL.
02308      
      * EXEC CICS XCTL
02309 *        PROGRAM (PGM-NAME)
02310 *        COMMAREA(PROGRAM-INTERFACE-BLOCK)
02311 *        LENGTH  (PI-COMM-LENGTH)
02312 *    END-EXEC.
      *    MOVE '.$C                   %   #00005971' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02313
02314  9400-CLEAR.
02315      MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME.
02316      GO TO 9300-XCTL.
02317
02318  9500-PF12.
02319      MOVE XCTL-010 TO PGM-NAME.
02320      GO TO 9300-XCTL.
02321
02322  9600-PGMID-ERROR.
02323      
      * EXEC CICS HANDLE CONDITION
02324 *        PGMIDERR(8300-SEND-TEXT)
02325 *    END-EXEC.
      *    MOVE '"$L                   ! 1 #00005986' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3120233030303035393836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02326
02327      MOVE PGM-NAME     TO PI-CALLING-PROGRAM.
02328      MOVE ' '          TO PI-ENTRY-CD-1.
02329      MOVE XCTL-005     TO PGM-NAME.
02330      MOVE PGM-NAME     TO LOGOFF-PGM.
02331      MOVE PGMIDERR-MSG TO LOGOFF-FILL.
02332      GO TO 9300-XCTL.
02333
02334  9700-LINK-DATE-CONVERT.
02335      MOVE LINK-ELDATCV TO PGM-NAME.
02336
02337      
      * EXEC CICS LINK
02338 *        PROGRAM (PGM-NAME)
02339 *        COMMAREA(DATE-CONVERSION-DATA)
02340 *        LENGTH  (DC-COMM-LENGTH)
02341 *    END-EXEC.
      *    MOVE '."C                   (   #00006000' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02342
02343  9700-EXIT.
02344      EXIT.
02345
02346  9900-ERROR-FORMAT.
02347      IF NOT EMI-ERRORS-COMPLETE
02348          MOVE LINK-001 TO PGM-NAME
02349          
      * EXEC CICS LINK
02350 *            PROGRAM (PGM-NAME)
02351 *            COMMAREA(ERROR-MESSAGE-INTERFACE-BLOCK)
02352 *            LENGTH  (EMI-COMM-LENGTH)
02353 *        END-EXEC.
      *    MOVE '."C                   (   #00006012' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02354  9900-EXIT.
02355      EXIT.
02356
02357  9990-ABEND.
02358      MOVE LINK-004 TO PGM-NAME.
02359      MOVE DFHEIBLK               TO EMI-LINE1.
02360
02361      
      * EXEC CICS LINK
02362 *        PROGRAM   (PGM-NAME)
02363 *        COMMAREA  (EMI-LINE1)
02364 *        LENGTH    (72)
02365 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00006024' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02366
02367      GO TO 8200-SEND-DATAONLY.
02368
02369      MOVE ZEROS  TO RETURN-CODE.
02369      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL656' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
02370
02371  9995-SECURITY-VIOLATION.
02372 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00006053' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303533' TO DFHEIV0(25:11)
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
02373
02374  9995-EXIT.
02375       EXIT.


       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL656' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9990-ABEND,
                     8880-NOT-FOUND,
                     9600-PGMID-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 9990-ABEND,
                     1025-ADD-REC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 8880-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 9990-ABEND,
                     1450-COPY-REC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8880-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 8880-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 9990-ABEND,
                     8880-NOT-FOUND,
                     9600-PGMID-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 6590-RESET-KEY
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 7150-ENDFILE,
                     7175-NOTFOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 7250-ENDFILE,
                     7275-NOTFOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 7250-ENDFILE,
                     7275-NOTFOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 7250-ENDFILE,
                     7275-NOTFOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 7300-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 7499-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 8000-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL656' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
