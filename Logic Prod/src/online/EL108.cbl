00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL108 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 09:45:26.
00007 *                            VMOD=2.011.
00008 *
00008 *
00009 *AUTHOR.        LOGIC, INC.
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
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00021 *            *                                                   *
00022 *            *****************************************************
00023
00024 *REMARKS.
00025 *        TRANSACTION EX13 - PROGRAM OPTION MAINTENANCE.
00026 *
00027  ENVIRONMENT DIVISION.
00028  DATA DIVISION.
00029  EJECT
00030  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00031  77  FILLER  PIC X(32)  VALUE '********************************'.
00032  77  FILLER  PIC X(32)  VALUE '*    EL108 WORKING STORAGE     *'.
00033  77  FILLER  PIC X(32)  VALUE '************VMOD=2.011 *********'.
00034
00035  01  FILE-PGMO-KEY.
00036      12  PROGRAM-NUMBER          PIC X(5).
00037      12  OPTION-TYPE             PIC X.
00038      12  OPTION-CODE             PIC X.
00039
00040  01  PROGRAM-OPT-HOLD.
00041      12  HOLD-PROGRAM-OPT OCCURS 4 TIMES.
00042          16  FREQ-CODE           PIC X(4).
00043          16  PRINT-OPT           PIC X.
00044          16  FORMAT-OPT          PIC X.
00045          16  PROCESS-OPT         PIC X.
00046          16  TOTAL-OPT           PIC X.
00047
00048 *    COPY ELCSCTM.
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
00049 *    COPY ELCSCRTY.
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
00050
00051     EJECT
00052
00053  01  WS-DATE-AREA.
00054      12  SAVE-DATE           PIC X(8)    VALUE SPACES.
00055      12  SAVE-BIN-DATE       PIC XX      VALUE SPACES.
00056
00057  01  WS-AREA.
00058      12  TIME-IN                 PIC S9(7).
00059      12  FILLER REDEFINES TIME-IN.
00060         16  FILLER               PIC X.
00061         16  TIME-OUT             PIC 99V99.
00062         16  FILLER               PIC XX.
00063      12  SC-ITEM                 PIC S9(4) COMP VALUE +1.
00064      12  XCTL-EL005              PIC X(8)    VALUE 'EL005'.
00065      12  XCTL-EL010              PIC X(8)    VALUE 'EL010'.
00066      12  XCTL-EL126              PIC X(8)    VALUE 'EL126'.
00067      12  XCTL-EL626              PIC X(8)    VALUE 'EL626'.
00068      12  XCTL-EM626              PIC X(8)    VALUE 'EM626'.
00069      12  XCTL-GL800              PIC X(8)    VALUE 'GL800'.
00070      12  THIS-PGM                PIC X(8)    VALUE 'EL108'.
00071      12  LIT-EX13                PIC X(4)    VALUE 'EX13'.
00072      12  LIT-MAP                 PIC X(4)    VALUE '108A'.
00073      12  LIT-SPACE               PIC X       VALUE SPACE.
00074  EJECT
00075  01  EDIT-WORK-AREA.
00076      12  COUNT-1                 PIC 9.
00077      12  CALL-PGM                PIC X(8).
00078      12  TRANS-ID                PIC X(4).
00079      12  CHECK-PFKEYS            PIC 99.
00080      12  CHECK-MAINT             PIC X.
00081          88  SHOW-OPTION                     VALUE 'S'.
00082          88  CHANGE-OPTION                   VALUE 'C'.
00083          88  VALID-CODE                      VALUE 'C' 'S'.
00084      12  CHECK-FREQ              PIC X(4).
00085          88  OPTION-ALWAYS-SET               VALUE 'NONE'.
00086 *        88  SET-ON-IF-EDIT                  VALUE 'ED=Y'.
00087 *        88  SET-ON-IF-NOT-EDIT              VALUE 'ED=N'.
00088 *        88  SET-ON-IF-UPDATE                VALUE 'UP=Y'.
00089 *        88  SET-ON-IF-NOT-UPDATE            VALUE 'UP=N'.
00090          88  SET-ON-IF-YEAR-END              VALUE 'MO=Y'.
00091          88  SET-ON-IF-NOT-YEAR-END          VALUE 'MO/Y'.
00092          88  SET-ON-IF-MO-3-6-9-12           VALUE 'MO=Q'.
00093          88  SET-ON-IF-NOT-QTR-MO            VALUE 'MO/Q'.
00094          88  SET-ON-IF-JAN                   VALUE 'MO=1'.
00095          88  SET-ON-IF-FEB                   VALUE 'MO=2'.
00096          88  SET-ON-IF-MARCH                 VALUE 'MO=3'.
00097          88  SET-ON-IF-APRIL                 VALUE 'MO=4'.
00098          88  SET-ON-IF-MAY                   VALUE 'MO=5'.
00099          88  SET-ON-IF-JUNE                  VALUE 'MO=6'.
00100          88  SET-ON-IF-JULY                  VALUE 'MO=7'.
00101          88  SET-ON-IF-AUG                   VALUE 'MO=8'.
00102          88  SET-ON-IF-SEPT                  VALUE 'MO=9'.
00103          88  SET-ON-IF-OCT                   VALUE 'MO=A'.
00104          88  SET-ON-IF-NOV                   VALUE 'MO=B'.
00105          88  SET-ON-IF-DEC                   VALUE 'MO=C'.
00106      12  CHECK-PRINT             PIC X.
00107          88  PRINT-TO-HARDCOPY               VALUE 'P'.
00108          88  PRINT-TO-FICHE-TAPE             VALUE 'F'.
00109          88  PRINT-TO-BOTH-MEDIA             VALUE 'B'.
00110          88  SAVE-REPORT-ONLINE-NO-PRINT     VALUE 'S'.
00111          88  SAVE-REPORT-ONLINE-AND-PRINT    VALUE 'T'.
00112      12  HOLD-PROGRAM            PIC X(5).
00113      12  HOLD-TYPE               PIC X.
00114      12  BROWSE-STARTED-SW       PIC X       VALUE ' '.
00115          88  BROWSE-STARTED                  VALUE '1'.
00116  EJECT
00117  01  ERROR-NUMBERS.
00118      12  ER-0000                 PIC X(4)    VALUE '0000'.
00119      12  ER-0023                 PIC X(4)    VALUE '0023'.
00120      12  ER-0029                 PIC X(4)    VALUE '0029'.
00121      12  ER-0050                 PIC X(4)    VALUE '0050'.
00122      12  ER-0066                 PIC X(4)    VALUE '0066'.
00123      12  ER-0067                 PIC X(4)    VALUE '0067'.
00124      12  ER-0070                 PIC X(4)    VALUE '0070'.
00125      12  ER-0162                 PIC X(4)    VALUE '0162'.
00126      12  ER-0163                 PIC X(4)    VALUE '0163'.
00127      12  ER-0164                 PIC X(4)    VALUE '0164'.
00128      12  ER-0165                 PIC X(4)    VALUE '0165'.
00129      12  ER-0166                 PIC X(4)    VALUE '0166'.
00130      12  ER-0167                 PIC X(4)    VALUE '0167'.
00131      12  ER-0267                 PIC X(4)    VALUE '0267'.
00132      12  ER-0269                 PIC X(4)    VALUE '0269'.
00133      12  ER-0486                 PIC X(4)    VALUE '0486'.
00134      12  ER-0599                 PIC X(4)    VALUE '0599'.
00135      12  ER-7008                 PIC X(4)    VALUE '7008'.
00136  EJECT
00137  01  ERROR-SWITCHES.
00138      12  ERROR-SWITCH            PIC X.
00139          88  SCREEN-ERROR                    VALUE 'X'.
00140      12  TYPE-SWITCH             PIC X.
00141          88  END-OF-TYPE                     VALUE 'X'.
00142
00143  01  FILE-PGMS-KEY.
00144      12  COMPANY-CD              PIC X.
00145      12  PROGRAM-NO.
00146          16  PROGRAM-LIT         PIC XX.
00147          16  PROGRAM-SEQ         PIC XXX.
00148
00149  01  COMP-LENGTHS.
00150      12  PGMS-LENGTH             PIC S9(4)   COMP VALUE +250.
00151      12  GENERIC-LENGTH          PIC S9(4)   COMP VALUE +5.
00152
00153  01  PROGRAM-OPT-DESC.
00154      12  DESC-PROGRAM-OPT OCCURS 9 TIMES.
00155          16  FILLER              PIC XXX.
00156          16  OPT-NO              PIC X.
00157          16  FILLER              PIC X.
00158          16  CONST-FILL          PIC X.
00159          16  FILLER              PIC X.
00160          16  PGMO-DESC           PIC X(40).
00161          16  FILLER              PIC X(32).
00162
00163  01  FREQUENCY-OPTIONS.
00164      12  FILLER                  PIC X(76)
00165          VALUE 'NONE - OPTION ALWAYS SET'.
00166 *    12  FILLER                  PIC X(76)
00167 *        VALUE 'ED=Y RUN WHEN EDIT = YES'.
00168 *    12  FILLER                  PIC X(76)
00169 *        VALUE 'ED=N RUN WHEN EDIT = NO'.
00170 *    12  FILLER                  PIC X(76)
00171 *        VALUE 'UP=Y RUN WHEN UPDATE = YES'.
00172 *    12  FILLER                  PIC X(76)
00173 *        VALUE 'UP=N RUN WHEN UPDATE = NO'.
00174      12  FILLER                  PIC X(76)
00175          VALUE 'MO=Y RUN WHEN YEARLY'.
00176      12  FILLER                  PIC X(35)
00177          VALUE 'MO=X RUN WHEN MONTH = X. '.
00178      12  FILLER                  PIC X(41)
00179          VALUE 'X MAY BE 1 THRU C WHERE 1=JAN,A=OCT,C=DEC'.
00180      12  FILLER                  PIC X(76)
00181          VALUE 'MO=Q RUN AT QUARTER END (MAR,JUNE,SEPT,DEC)'.
00182
00183  01  OPTIONS-FOR-FEQUENCY REDEFINES FREQUENCY-OPTIONS.
00184      12  OPTIONS-FREQ OCCURS 4 TIMES.
00185          16  FILLER              PIC X(76).
00186
00187  01  PRINT-OPTIONS.
00188      12  FILLER                  PIC X(36)
00189          VALUE '   P = PRINT TO HARD COPY'.
00190      12  FILLER                  PIC X(36)
00191          VALUE '   F = PRINT TO FICHE TAPE'.
00192      12  FILLER                  PIC X(36)
00193          VALUE '   B = PRINT TO BOTH MEDIA'.
00194      12  FILLER                  PIC X(36)
00195          VALUE '   S = SAVE REPORT ONLINE - NO PRINT'.
00196      12  FILLER                  PIC X(36)
00197          VALUE '   T = SAVE REPORT ONLINE AND PRINT'.
00198
00199  01  OPTIONS-TO-PRINT REDEFINES PRINT-OPTIONS.
00200      12  OPTIONS-PRINT OCCURS 5 TIMES.
00201          16  FILLER              PIC X(36).
00202
00203  01  DO-NOT-RUN                  PIC X(36)
00204          VALUE '   X = DO NOT RUN'.
00205  EJECT
00206 *    COPY ELCLOGOF.
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
00207  EJECT
00208 *    COPY ELCDATE.
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
00209  EJECT
00210 *    COPY ELCATTR.
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
00211  EJECT
00212 *    COPY ELCAID.
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
00213
00214  01  FILLER REDEFINES DFHAID.
00215      12  FILLER                  PIC X(8).
00216      12  AID-KEYS OCCURS 24 TIMES.
00217          16  FILLER              PIC X(1).
00218  EJECT
00219 *    COPY ELCINTF.
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
00220
00221      12  PI-PRG-WRK-AREA REDEFINES PI-PROGRAM-WORK-AREA.
00222          16  PI-PROGRAM-NO   PIC X(5).
00223          16  FILLER          PIC X(635).
00224  EJECT
00225 *    COPY ELCEMIB.
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
00226  EJECT
00227 *    COPY ELCJPFX.
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
00228                                  PIC X(259).
00229  EJECT
00230 *    COPY EL108S.
       01  EL108AI.
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
           05  MAINTL PIC S9(0004) COMP.
           05  MAINTF PIC  X(0001).
           05  FILLER REDEFINES MAINTF.
               10  MAINTA PIC  X(0001).
           05  MAINTI PIC  X(0001).
      *    -------------------------------
           05  PGRML PIC S9(0004) COMP.
           05  PGRMF PIC  X(0001).
           05  FILLER REDEFINES PGRMF.
               10  PGRMA PIC  X(0001).
           05  PGRMI PIC  X(0005).
      *    -------------------------------
           05  LSTUSRL PIC S9(0004) COMP.
           05  LSTUSRF PIC  X(0001).
           05  FILLER REDEFINES LSTUSRF.
               10  LSTUSRA PIC  X(0001).
           05  LSTUSRI PIC  X(0004).
      *    -------------------------------
           05  LSTDTEL PIC S9(0004) COMP.
           05  LSTDTEF PIC  X(0001).
           05  FILLER REDEFINES LSTDTEF.
               10  LSTDTEA PIC  X(0001).
           05  LSTDTEI PIC  X(0008).
      *    -------------------------------
           05  LSTTIMEL PIC S9(0004) COMP.
           05  LSTTIMEF PIC  X(0001).
           05  FILLER REDEFINES LSTTIMEF.
               10  LSTTIMEA PIC  X(0001).
           05  LSTTIMEI PIC  X(0005).
      *    -------------------------------
           05  FREQ1L PIC S9(0004) COMP.
           05  FREQ1F PIC  X(0001).
           05  FILLER REDEFINES FREQ1F.
               10  FREQ1A PIC  X(0001).
           05  FREQ1I PIC  X(0004).
      *    -------------------------------
           05  PRT1L PIC S9(0004) COMP.
           05  PRT1F PIC  X(0001).
           05  FILLER REDEFINES PRT1F.
               10  PRT1A PIC  X(0001).
           05  PRT1I PIC  X(0001).
      *    -------------------------------
           05  FMT1L PIC S9(0004) COMP.
           05  FMT1F PIC  X(0001).
           05  FILLER REDEFINES FMT1F.
               10  FMT1A PIC  X(0001).
           05  FMT1I PIC  X(0001).
      *    -------------------------------
           05  PROC1L PIC S9(0004) COMP.
           05  PROC1F PIC  X(0001).
           05  FILLER REDEFINES PROC1F.
               10  PROC1A PIC  X(0001).
           05  PROC1I PIC  X(0001).
      *    -------------------------------
           05  TOT1L PIC S9(0004) COMP.
           05  TOT1F PIC  X(0001).
           05  FILLER REDEFINES TOT1F.
               10  TOT1A PIC  X(0001).
           05  TOT1I PIC  X(0001).
      *    -------------------------------
           05  FREQ2L PIC S9(0004) COMP.
           05  FREQ2F PIC  X(0001).
           05  FILLER REDEFINES FREQ2F.
               10  FREQ2A PIC  X(0001).
           05  FREQ2I PIC  X(0004).
      *    -------------------------------
           05  PRT2L PIC S9(0004) COMP.
           05  PRT2F PIC  X(0001).
           05  FILLER REDEFINES PRT2F.
               10  PRT2A PIC  X(0001).
           05  PRT2I PIC  X(0001).
      *    -------------------------------
           05  FMT2L PIC S9(0004) COMP.
           05  FMT2F PIC  X(0001).
           05  FILLER REDEFINES FMT2F.
               10  FMT2A PIC  X(0001).
           05  FMT2I PIC  X(0001).
      *    -------------------------------
           05  PROC2L PIC S9(0004) COMP.
           05  PROC2F PIC  X(0001).
           05  FILLER REDEFINES PROC2F.
               10  PROC2A PIC  X(0001).
           05  PROC2I PIC  X(0001).
      *    -------------------------------
           05  TOT2L PIC S9(0004) COMP.
           05  TOT2F PIC  X(0001).
           05  FILLER REDEFINES TOT2F.
               10  TOT2A PIC  X(0001).
           05  TOT2I PIC  X(0001).
      *    -------------------------------
           05  FREQ3L PIC S9(0004) COMP.
           05  FREQ3F PIC  X(0001).
           05  FILLER REDEFINES FREQ3F.
               10  FREQ3A PIC  X(0001).
           05  FREQ3I PIC  X(0004).
      *    -------------------------------
           05  PRT3L PIC S9(0004) COMP.
           05  PRT3F PIC  X(0001).
           05  FILLER REDEFINES PRT3F.
               10  PRT3A PIC  X(0001).
           05  PRT3I PIC  X(0001).
      *    -------------------------------
           05  FMT3L PIC S9(0004) COMP.
           05  FMT3F PIC  X(0001).
           05  FILLER REDEFINES FMT3F.
               10  FMT3A PIC  X(0001).
           05  FMT3I PIC  X(0001).
      *    -------------------------------
           05  PROC3L PIC S9(0004) COMP.
           05  PROC3F PIC  X(0001).
           05  FILLER REDEFINES PROC3F.
               10  PROC3A PIC  X(0001).
           05  PROC3I PIC  X(0001).
      *    -------------------------------
           05  TOT3L PIC S9(0004) COMP.
           05  TOT3F PIC  X(0001).
           05  FILLER REDEFINES TOT3F.
               10  TOT3A PIC  X(0001).
           05  TOT3I PIC  X(0001).
      *    -------------------------------
           05  FREQ4L PIC S9(0004) COMP.
           05  FREQ4F PIC  X(0001).
           05  FILLER REDEFINES FREQ4F.
               10  FREQ4A PIC  X(0001).
           05  FREQ4I PIC  X(0004).
      *    -------------------------------
           05  PRT4L PIC S9(0004) COMP.
           05  PRT4F PIC  X(0001).
           05  FILLER REDEFINES PRT4F.
               10  PRT4A PIC  X(0001).
           05  PRT4I PIC  X(0001).
      *    -------------------------------
           05  FMT4L PIC S9(0004) COMP.
           05  FMT4F PIC  X(0001).
           05  FILLER REDEFINES FMT4F.
               10  FMT4A PIC  X(0001).
           05  FMT4I PIC  X(0001).
      *    -------------------------------
           05  PROC4L PIC S9(0004) COMP.
           05  PROC4F PIC  X(0001).
           05  FILLER REDEFINES PROC4F.
               10  PROC4A PIC  X(0001).
           05  PROC4I PIC  X(0001).
      *    -------------------------------
           05  TOT4L PIC S9(0004) COMP.
           05  TOT4F PIC  X(0001).
           05  FILLER REDEFINES TOT4F.
               10  TOT4A PIC  X(0001).
           05  TOT4I PIC  X(0001).
      *    -------------------------------
           05  VARDESCL PIC S9(0004) COMP.
           05  VARDESCF PIC  X(0001).
           05  FILLER REDEFINES VARDESCF.
               10  VARDESCA PIC  X(0001).
           05  VARDESCI PIC  X(0009).
           05  OPTD OCCURS 9  TIMES.
      *    -------------------------------
               10  OPTL PIC S9(0004) COMP.
               10  OPTF PIC  X(0001).
               10  FILLER REDEFINES OPTF.
                   15  OPTA PIC  X(0001).
               10  OPTI PIC  X(0079).
      *    -------------------------------
           05  MSGL PIC S9(0004) COMP.
           05  MSGF PIC  X(0001).
           05  FILLER REDEFINES MSGF.
               10  MSGA PIC  X(0001).
           05  MSGI PIC  X(0075).
      *    -------------------------------
           05  PFKEYL PIC S9(0004) COMP.
           05  PFKEYF PIC  X(0001).
           05  FILLER REDEFINES PFKEYF.
               10  PFKEYA PIC  X(0001).
           05  PFKEYI PIC  X(0002).
       01  EL108AO REDEFINES EL108AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PGRMO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LSTUSRO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LSTDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LSTTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FREQ1O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRT1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FMT1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PROC1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TOT1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FREQ2O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRT2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FMT2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PROC2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TOT2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FREQ3O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRT3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FMT3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PROC3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TOT3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FREQ4O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRT4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FMT4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PROC4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TOT4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  VARDESCO PIC  X(0009).
      *    -------------------------------
           05  OPTD OCCURS 9  TIMES.
               10  FILLER        PIC  X(0003).
               10  OPTO PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSGO PIC  X(0075).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFKEYO PIC  X(0002).
      *    -------------------------------
00231  EJECT
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
00233  01  DFHCOMMAREA                 PIC X(1024).
00234
00235 *01 PARM-LIST .
00236 *    12  FILLER                  PIC S9(8)   COMP.
00237 *    12  PGMS-PNT                PIC S9(8)   COMP.
00238 *    12  PGMO-PNT                PIC S9(8)   COMP.
00239  EJECT
00240 *    COPY ELCPGMS.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCPGMS.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = PROGRAM OPTIONS SELECTED BY COMPANY       *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 250   RECFORM = FIX                            *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ELPGMS                   RKP=2,LEN=6     *
00013 *       ALTERNATE PATH  = NOT USED                               *
00014 *                                                                *
00015 *   LOG = NO                                                     *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
00018  01  PROGRAM-OPTIONS-SELECTED.
00019      12  PS-RECORD-ID                PIC XX.
00020          88  VALID-PS-ID                VALUE 'PS'.
00021
00022      12  PS-CONTROL-PRIMARY.
00023          16  PS-COMPANY-CD           PIC X.
00024          16  PS-PROGRAM-NUMBER.
00025              20  PS-SYSTEM-CODE      PIC XX.
00026                  88 CLAS-IC-ONLINE      VALUE 'EL'.
00027                  88 CLAS-IC-REPORT      VALUE 'EC'.
00028                  88 CLAS-GL-BATCH       VALUE 'GL'.
00029              20  PS-PROGRAM-SEQUENCE PIC 999.
00030
00031      12  PS-PROGRAM-OPTIONS    OCCURS 4 TIMES.
00032          16  PS-FREQUENCY-CODE       PIC X(4).
00033 *            88  OPTION-ALWAYS-SET      VALUE 'NONE'.
00034 *            88  SET-ON-IF-EDIT         VALUE 'ED=Y'.
00035 *            88  SET-ON-IF-NOT-EDIT     VALUE 'ED=N'.
00036 *            88  SET-ON-IF-UPDATE       VALUE 'UP=Y'.
00037 *            88  SET-ON-IF-NOT-UPDATE   VALUE 'UP=N'.
00038 *            88  SET-ON-IF-YEAR-END     VALUE 'MO=Y'.
00039 *            88  SET-ON-IF-NOT-YEAR-END VALUE 'MO/Y'.
00040 *            88  SET-ON-IF-MO-3-6-9-12  VALUE 'MO=Q'.
00041 *            88  SET-ON-IF-NOT-QTR-MO   VALUE 'MO/Q'.
00042 *            88  SET-ON-IF-JAN          VALUE 'MO=1'.
00043 *            88  SET-ON-IF-FEB          VALUE 'MO=2'.
00044 *            88  SET-ON-IF-MARCH        VALUE 'MO=3'.
00045 *            88  SET-ON-IF-APRIL        VALUE 'MO=4'.
00046 *            88  SET-ON-IF-MAY          VALUE 'MO=5'.
00047 *            88  SET-ON-IF-JUNE         VALUE 'MO=6'.
00048 *            88  SET-ON-IF-JULY         VALUE 'MO=7'.
00049 *            88  SET-ON-IF-AUG          VALUE 'MO=8'.
00050 *            88  SET-ON-IF-SEPT         VALUE 'MO=9'.
00051 *            88  SET-ON-IF-OCT          VALUE 'MO=A'.
00052 *            88  SET-ON-IF-NOV          VALUE 'MO=B'.
00053 *            88  SET-ON-IF-DEC          VALUE 'MO=C'.
00054          16  PS-PRINT-OPTION         PIC X.
00055 *            88  PRINT-TO-HARDCOPY      VALUE 'P'.
00056 *            88  PRINT-TO-FICHE-TAPE    VALUE 'F'.
00057 *            88  PRINT-TO-BOTH-MEDIA    VALUE 'B'.
00058          16  PS-FORMAT-OPTION        PIC X.
00059          16  PS-PROCESS-OPTION       PIC X.
00060          16  PS-TOTAL-OPTION         PIC X.
00061
00062      12  PS-PARAMETER-INPUTS.
00063          16  PS-INPUT-1              PIC X(80).
00064          16  PS-INPUT-2              PIC X(80).
00065
00066      12  PS-MAINT-INFORMATION.
00067          16  PS-LAST-MAINT-DT        PIC XX.
00068          16  PS-LAST-MAINT-HHMMSS    PIC S9(7)    COMP-3.
00069          16  PS-LAST-MAINT-USER      PIC X(4).
00070          16  FILLER                  PIC XX.
00071
00072      12  FILLER                      PIC X(38).
00073
00074 ******************************************************************
00241  EJECT
00242 *    COPY ELCPGMO.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCPGMO.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = AVAILABLE PROGRAM OPTIONS                 *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 49    RECFORM = FIX                            *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ELPGMO                RKP=2,LEN=7        *
00013 *       ALTERNATE PATH  = NOT USED                               *
00014 *                                                                *
00015 *   LOG = NO                                                     *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
00018  01  PROGRAM-OPTIONS-AVAILABLE.
00019      12  PO-RECORD-ID                PIC XX.
00020          88  VALID-PO-ID                VALUE 'PO'.
00021
00022      12  PO-CONTROL-PRIMARY.
00023          16  PO-PROGRAM-NUMBER.
00024              20  PO-SYSTEM-CODE      PIC XX.
00025                  88  CLAS-IC-ONLINE     VALUE 'EL'.
00026                  88  CLAS-IC-REPORT     VALUE 'EC'.
00027                  88  CLAS-GL-BATCH      VALUE 'GL'.
00028              20  PO-PROGRAM-SEQUENCE PIC 999.
00029          16  PO-OPTION-TYPE          PIC X.
00030              88  PO-FORMAT-OPTION       VALUE 'F'.
00031              88  PO-PROCESS-OPTION      VALUE 'P'.
00032              88  PO-TOTAL-OPTION        VALUE 'T'.
00033          16  PO-PGM-OPTION-CD        PIC X.
00034
00035      12  PO-OPTION-DESCRIPTION       PIC X(40).
00036
00037 ******************************************************************
00243  EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA
                                PROGRAM-OPTIONS-SELECTED
                                PROGRAM-OPTIONS-AVAILABLE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL108' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00245
00246      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
00247      MOVE '5'                   TO DC-OPTION-CODE.
00248      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
00249      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
00250      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
00251
00252      IF EIBCALEN = ZERO
00253          GO TO 8800-UNAUTHORIZED-ACCESS.
00254
00255      
      * EXEC CICS HANDLE CONDITION
00256 *        PGMIDERR (8820-XCTL-ERROR)
00257 *        ERROR    (9990-ABEND)
00258 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00001492' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031343932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00259
00260      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.
00261      MOVE SPACES      TO ERROR-SWITCHES MSGO.
00262      MOVE LIT-EX13    TO TRANS-ID.
00263
00264      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00265          MOVE LOW-VALUES TO EL108AO
00266          MOVE ER-7008    TO EMI-ERROR
00267          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00268          MOVE -1         TO MAINTL
00269          GO TO 8110-SEND-DATA.
00270
00271      IF THIS-PGM = PI-CALLING-PROGRAM
00272          GO TO 0050-CHECK-CLEAR.
00273
00274      IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00275          MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00276          MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00277          MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00278          MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00279          MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00280          MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00281          MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00282          MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00283        ELSE
00284          MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00285          MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00286          MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00287          MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00288          MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00289          MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00290          MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00291          MOVE SPACES               TO PI-SAVED-PROGRAM-6.
00292
00293      MOVE LOW-VALUES           TO EL108AO.
00294      MOVE ZEROS                TO COUNT-1.
00295      PERFORM 5130-SPACE-SCREEN THRU 5140-EXIT 9 TIMES.
00296      MOVE -1                   TO MAINTL.
00297      GO TO 8100-SEND-INITIAL-MAP.
00298
00299  0050-CHECK-CLEAR.
00300      IF EIBAID = DFHCLEAR
00301          GO TO 8200-RETURN-PRIOR.
00302
00303      IF PI-PROCESSOR-ID = 'LGXX'
00304          GO TO 0200-RECEIVE.
00305
00306      
      * EXEC CICS READQ TS
00307 *        QUEUE  (PI-SECURITY-TEMP-STORE-ID)
00308 *        INTO   (SECURITY-CONTROL)
00309 *        LENGTH (SC-COMM-LENGTH)
00310 *        ITEM   (SC-ITEM)
00311 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00001543' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00312
00313      MOVE SC-CREDIT-DISPLAY (02)  TO PI-DISPLAY-CAP.
00314      MOVE SC-CREDIT-UPDATE  (02)  TO PI-MODIFY-CAP.
00315
00316      IF NOT DISPLAY-CAP
00317          MOVE 'READ'          TO SM-READ
00318          PERFORM 9995-SECURITY-VIOLATION
00319          MOVE ER-0070         TO  EMI-ERROR
00320          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00321          GO TO 8100-SEND-INITIAL-MAP.
00322
00323       EJECT
00324  0200-RECEIVE.
00325      
      * EXEC CICS RECEIVE
00326 *        MAP    ('EL108A')
00327 *        MAPSET ('EL108S')
00328 *    END-EXEC.
           MOVE 'EL108A' TO DFHEIV1
           MOVE 'EL108S' TO DFHEIV2
      *    MOVE '8"T                   ''   #00001562' TO DFHEIV0
           MOVE X'382254202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL108AI, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00329
00330      IF PFKEYL GREATER THAN ZERO
00331          PERFORM 0200-TRANS-PF THRU 0210-EXIT.
00332
00333      IF EIBAID = DFHPF1
00334          PERFORM 0300-RESET-ATTRB THRU 0310-EXIT
00335          GO TO 5000-SHOW-FREQ.
00336
00337      IF EIBAID = DFHPF2
00338          PERFORM 0300-RESET-ATTRB THRU 0310-EXIT
00339          GO TO 5100-SHOW-PRINT.
00340
00341      IF EIBAID = DFHPF3
00342          PERFORM 0300-RESET-ATTRB THRU 0310-EXIT
00343          GO TO 5400-SHOW-FORMAT.
00344
00345      IF EIBAID = DFHPF4
00346          PERFORM 0300-RESET-ATTRB THRU 0310-EXIT
00347          GO TO 5300-SHOW-PROCESS.
00348
00349      IF EIBAID = DFHPF5
00350          PERFORM 0300-RESET-ATTRB THRU 0310-EXIT
00351          GO TO 5200-SHOW-TOTAL.
00352
00353      MOVE SPACES                 TO  VARDESCO.
00354      MOVE ZEROS                  TO  COUNT-1.
00355
00356      PERFORM 5130-SPACE-SCREEN  THRU  5140-EXIT  9  TIMES.
00357
00358      IF EIBAID = DFHPF6
00359          PERFORM 0300-RESET-ATTRB THRU 0310-EXIT
00360          GO TO 3100-SHOW-OPTIONS.
00361
00362      IF EIBAID = DFHPF7
00363          PERFORM 0300-RESET-ATTRB THRU 0310-EXIT
00364          GO TO 3100-SHOW-OPTIONS.
00365
00366      IF EIBAID = DFHPF12
00367          GO TO 8300-GET-HELP.
00368      IF EIBAID = DFHPF23
00369          GO TO 8810-PF23-ENTERED.
00370      IF EIBAID = DFHPF24
00371          GO TO 8400-RETURN-MASTER.
00372
00373      IF EIBAID NOT = DFHENTER
00374          MOVE ER-0029 TO EMI-ERROR
00375          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00376          MOVE -1 TO MAINTL
00377          GO TO 8110-SEND-DATA.
00378
00379      PERFORM 0300-RESET-ATTRB THRU 0310-EXIT.
00380      PERFORM 1000-EDIT-SCREEN THRU 1010-EXIT.
00381
00382      IF SCREEN-ERROR
00383          GO TO 8110-SEND-DATA.
00384
00385      MOVE SPACES TO PROGRAM-OPT-HOLD.
00386
00387      IF  CHANGE-OPTION
00388          PERFORM 2000-CHANGE-OPTIONS THRU 2040-EXIT
00389
00390          IF  NOT EMI-NO-ERRORS
00391              GO TO 8110-SEND-DATA
00392
00393          ELSE
00394              PERFORM 4000-SHOW-OPTIONS THRU 4020-EXIT
00395              MOVE ER-0000 TO EMI-ERROR
00396              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00397
00398      IF SHOW-OPTION
00399          PERFORM 4000-SHOW-OPTIONS THRU 4020-EXIT.
00400
00401      MOVE -1    TO MAINTL.
00402      MOVE SPACE TO MAINTO.
00403      GO TO 8110-SEND-DATA.
00404  EJECT
00405
00406  0200-TRANS-PF.
00407      IF PFKEYI NOT NUMERIC
00408          MOVE ER-7008         TO EMI-ERROR
00409          GO TO 0205-ERROR.
00410
00411      MOVE PFKEYI TO CHECK-PFKEYS.
00412
00413      IF CHECK-PFKEYS LESS 1 OR GREATER 24
00414          MOVE ER-7008         TO EMI-ERROR
00415          GO TO 0205-ERROR.
00416
00417      MOVE AID-KEYS (CHECK-PFKEYS) TO EIBAID.
00418
00419      GO TO 0210-EXIT.
00420
00421  0205-ERROR.
00422      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00423      MOVE -1                     TO MAINTL.
00424      GO TO 8110-SEND-DATA.
00425
00426  0210-EXIT.
00427      EXIT.
00428
00429  0300-RESET-ATTRB.
00430      MOVE AL-UANON TO MAINTA
00431                       PGRMA.
00432  0310-EXIT.
00433      EXIT.
00434  EJECT
00435  1000-EDIT-SCREEN.
00436      MOVE MAINTI TO CHECK-MAINT.
00437      IF NOT VALID-CODE
00438          MOVE AL-UABON TO MAINTA
00439          MOVE -1       TO MAINTL
00440          MOVE 'X'      TO ERROR-SWITCH
00441          MOVE ER-0023  TO EMI-ERROR
00442          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00443
00444      IF PGRMI = ZEROS OR LOW-VALUES
00445          MOVE -1       TO PGRML
00446          MOVE AL-UABON TO PGRMA
00447          MOVE 'X'      TO ERROR-SWITCH
00448          MOVE ER-0162  TO EMI-ERROR
00449          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00450          GO TO 1010-EXIT.
00451
00452      MOVE PGRMI TO PROGRAM-NO.
00453
00454      IF PROGRAM-LIT NOT = 'EL' AND 'EC' AND 'GL' AND 'EM'
00455          MOVE -1       TO PGRML
00456          MOVE AL-UABON TO PGRMA
00457          MOVE 'X'      TO ERROR-SWITCH
00458          MOVE ER-0162  TO EMI-ERROR
00459          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00460          GO TO 1010-EXIT.
00461
00462      IF CHANGE-OPTION
00463         IF NOT MODIFY-CAP
00464             MOVE 'UPDATE'       TO SM-READ
00465             PERFORM 9995-SECURITY-VIOLATION
00466             MOVE 'X'        TO ERROR-SWITCH
00467             MOVE AL-UABON   TO MAINTA
00468             MOVE ER-0070    TO EMI-ERROR
00469             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00470
00471      MOVE PI-COMPANY-CD TO COMPANY-CD.
00472      MOVE PGRMI         TO PROGRAM-NUMBER.
00473
00474  1010-EXIT.
00475      EXIT.
00476
00477  EJECT
00478  2000-CHANGE-OPTIONS.
00479      MOVE ZEROS TO COUNT-1.
00480      PERFORM 2100-VERIFY-FREQ THRU 2110-EXIT.
00481
00482      MOVE ZEROS TO COUNT-1.
00483      PERFORM 2200-VERIFY-PRINT THRU 2210-EXIT.
00484
00485      MOVE ZEROS TO COUNT-1.
00486      PERFORM 2300-VERIFY-FORMAT THRU 2310-EXIT.
00487
00488      MOVE ZEROS TO COUNT-1.
00489      PERFORM 2400-VERIFY-PROCESS THRU 2410-EXIT.
00490
00491      MOVE ZEROS TO COUNT-1.
00492      PERFORM 2500-VERIFY-TOTAL THRU 2510-EXIT.
00493
00494      PERFORM 2600-CHECK-REQUIRE THRU 2650-EXIT.
00495
00496      IF NOT EMI-NO-ERRORS
00497         GO TO 2040-EXIT.
00498
00499      
      * EXEC CICS HANDLE CONDITION
00500 *        NOTFND (2010-PGMS-NOT-FOUND)
00501 *    END-EXEC.
      *    MOVE '"$I                   ! # #00001736' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303031373336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00502
00503      
      * EXEC CICS READ
00504 *        SET     (ADDRESS OF PROGRAM-OPTIONS-SELECTED)
00505 *        DATASET ('ELPGMS')
00506 *        RIDFLD  (FILE-PGMS-KEY)
00507 *        UPDATE
00508 *    END-EXEC.
           MOVE 'ELPGMS' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00001740' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 FILE-PGMS-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PROGRAM-OPTIONS-SELECTED TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00509
00510      MOVE SAVE-BIN-DATE          TO  PS-LAST-MAINT-DT.
00511      MOVE EIBTIME                TO  PS-LAST-MAINT-HHMMSS.
00512      MOVE PI-PROCESSOR-ID        TO  PS-LAST-MAINT-USER.
00513
00514      MOVE ZEROS TO COUNT-1.
00515      PERFORM 3000-FORMAT-RECORD THRU 3010-EXIT 4 TIMES.
00516
00517      
      * EXEC CICS ASKTIME END-EXEC.
      *    MOVE '0"                    "   #00001754' TO DFHEIV0
           MOVE X'302220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00518
00519      
      * EXEC CICS REWRITE
00520 *        FROM (PROGRAM-OPTIONS-SELECTED)
00521 *        DATASET ('ELPGMS')
00522 *    END-EXEC.
           MOVE LENGTH OF
            PROGRAM-OPTIONS-SELECTED
             TO DFHEIV11
           MOVE 'ELPGMS' TO DFHEIV1
      *    MOVE '&& L                  %   #00001756' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 PROGRAM-OPTIONS-SELECTED, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00523
00524      GO TO 2040-EXIT.
00525
00526  2010-PGMS-NOT-FOUND.
00527      
      * EXEC CICS GETMAIN
00528 *        SET     (ADDRESS OF PROGRAM-OPTIONS-SELECTED)
00529 *        LENGTH  (PGMS-LENGTH)
00530 *        INITIMG (LIT-SPACE)
00531 *    END-EXEC.
      *    MOVE ',"IL                  $   #00001764' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 PGMS-LENGTH, 
                 LIT-SPACE
           SET ADDRESS OF PROGRAM-OPTIONS-SELECTED TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00532
00533      MOVE 'PS'          TO PS-RECORD-ID.
00534      MOVE FILE-PGMS-KEY TO PS-CONTROL-PRIMARY.
00535
00536      MOVE SAVE-BIN-DATE          TO  PS-LAST-MAINT-DT.
00537      MOVE EIBTIME                TO  PS-LAST-MAINT-HHMMSS.
00538      MOVE PI-PROCESSOR-ID        TO  PS-LAST-MAINT-USER.
00539
00540      MOVE ZEROS TO COUNT-1.
00541      PERFORM 3000-FORMAT-RECORD THRU 3010-EXIT 4 TIMES.
00542
00543      
      * EXEC CICS WRITE
00544 *        FROM (PROGRAM-OPTIONS-SELECTED)
00545 *        DATASET ('ELPGMS')
00546 *        RIDFLD (FILE-PGMS-KEY)
00547 *    END-EXEC.
           MOVE LENGTH OF
            PROGRAM-OPTIONS-SELECTED
             TO DFHEIV11
           MOVE 'ELPGMS' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00001780' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 PROGRAM-OPTIONS-SELECTED, 
                 DFHEIV11, 
                 FILE-PGMS-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00548
00549  2040-EXIT.
00550      EXIT.
00551  EJECT
00552  2100-VERIFY-FREQ.
00553      MOVE FREQ1I TO CHECK-FREQ.
00554      PERFORM 2140-FREQ-CHECK THRU 2150-EXIT.
00555
00556      IF SCREEN-ERROR
00557          MOVE AL-UABON TO FREQ1A
00558          MOVE -1       TO FREQ1L
00559          MOVE SPACE    TO ERROR-SWITCH
00560      ELSE
00561          MOVE AL-UANON TO FREQ1A
00562          PERFORM 2120-MOVE-FREQ THRU 2130-EXIT.
00563
00564      MOVE FREQ2I TO CHECK-FREQ.
00565      PERFORM 2140-FREQ-CHECK THRU 2150-EXIT.
00566
00567      IF SCREEN-ERROR
00568          MOVE AL-UABON TO FREQ2A
00569          MOVE -1       TO FREQ2L
00570          MOVE SPACE    TO ERROR-SWITCH
00571      ELSE
00572          MOVE AL-UANON TO FREQ2A
00573          PERFORM 2120-MOVE-FREQ THRU 2130-EXIT.
00574
00575      MOVE FREQ3I TO CHECK-FREQ.
00576      PERFORM 2140-FREQ-CHECK THRU 2150-EXIT.
00577
00578      IF SCREEN-ERROR
00579          MOVE AL-UABON TO FREQ3A
00580          MOVE -1       TO FREQ3L
00581          MOVE SPACE    TO ERROR-SWITCH
00582      ELSE
00583          MOVE AL-UANON TO FREQ3A
00584          PERFORM 2120-MOVE-FREQ THRU 2130-EXIT.
00585
00586      MOVE FREQ4I TO CHECK-FREQ.
00587      PERFORM 2140-FREQ-CHECK THRU 2150-EXIT
00588
00589      IF SCREEN-ERROR
00590          MOVE SPACE    TO ERROR-SWITCH
00591          MOVE -1       TO FREQ4L
00592          MOVE AL-UABON TO FREQ4A
00593      ELSE
00594          MOVE AL-UANON TO FREQ4A
00595          PERFORM 2120-MOVE-FREQ THRU 2130-EXIT.
00596
00597  2110-EXIT.
00598      EXIT.
00599
00600  2120-MOVE-FREQ.
00601      ADD 1 TO COUNT-1.
00602      MOVE CHECK-FREQ TO FREQ-CODE (COUNT-1).
00603
00604  2130-EXIT.
00605      EXIT.
00606
00607  2140-FREQ-CHECK.
00608      IF CHECK-FREQ = SPACES OR = LOW-VALUES
00609         OR OPTION-ALWAYS-SET
00610 *       OR SET-ON-IF-EDIT
00611 *       OR SET-ON-IF-NOT-EDIT
00612 *       OR SET-ON-IF-UPDATE
00613 *       OR SET-ON-IF-NOT-UPDATE
00614         OR SET-ON-IF-YEAR-END
00615         OR SET-ON-IF-NOT-YEAR-END
00616         OR SET-ON-IF-MO-3-6-9-12
00617         OR SET-ON-IF-NOT-QTR-MO
00618         OR SET-ON-IF-JAN
00619         OR SET-ON-IF-FEB
00620         OR SET-ON-IF-MARCH
00621         OR SET-ON-IF-APRIL
00622         OR SET-ON-IF-MAY
00623         OR SET-ON-IF-JUNE
00624         OR SET-ON-IF-JULY
00625         OR SET-ON-IF-AUG
00626         OR SET-ON-IF-SEPT
00627         OR SET-ON-IF-OCT
00628         OR SET-ON-IF-NOV
00629         OR SET-ON-IF-DEC
00630          GO TO 2150-EXIT.
00631
00632      MOVE 'X' TO ERROR-SWITCH.
00633      MOVE ER-0163 TO EMI-ERROR.
00634      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00635
00636  2150-EXIT.
00637      EXIT.
00638  EJECT
00639  2200-VERIFY-PRINT.
00640      MOVE PRT1I TO CHECK-PRINT.
00641      PERFORM 2240-PRT-CHECK THRU 2250-EXIT.
00642
00643      IF SCREEN-ERROR
00644          MOVE SPACE    TO ERROR-SWITCH
00645          MOVE -1       TO PRT1L
00646          MOVE AL-UABON TO PRT1A
00647      ELSE
00648          MOVE AL-UANON TO PRT1A
00649          PERFORM 2220-MOVE-PRT THRU 2230-EXIT.
00650
00651      MOVE PRT2I TO CHECK-PRINT.
00652      PERFORM 2240-PRT-CHECK THRU 2250-EXIT.
00653
00654      IF SCREEN-ERROR
00655          MOVE SPACE    TO ERROR-SWITCH
00656          MOVE -1       TO PRT2L
00657          MOVE AL-UABON TO PRT2A
00658      ELSE
00659          MOVE AL-UANON TO PRT2A
00660          PERFORM 2220-MOVE-PRT THRU 2230-EXIT.
00661
00662      MOVE PRT3I TO CHECK-PRINT.
00663      PERFORM 2240-PRT-CHECK THRU 2250-EXIT.
00664
00665      IF SCREEN-ERROR
00666          MOVE AL-UABON TO PRT3A
00667          MOVE -1       TO PRT3L
00668          MOVE SPACE    TO ERROR-SWITCH
00669      ELSE
00670          MOVE AL-UANON TO PRT3A
00671          PERFORM 2220-MOVE-PRT THRU 2230-EXIT.
00672
00673      MOVE PRT4I TO CHECK-PRINT.
00674      PERFORM 2240-PRT-CHECK THRU 2250-EXIT.
00675
00676      IF SCREEN-ERROR
00677          MOVE AL-UABON TO PRT4A
00678          MOVE -1       TO PRT4L
00679          MOVE SPACE    TO ERROR-SWITCH
00680      ELSE
00681          MOVE AL-UANON TO PRT4A
00682          PERFORM 2220-MOVE-PRT THRU 2230-EXIT.
00683
00684  2210-EXIT.
00685      EXIT.
00686
00687  2220-MOVE-PRT.
00688      ADD 1 TO COUNT-1.
00689      MOVE CHECK-PRINT TO PRINT-OPT (COUNT-1).
00690
00691  2230-EXIT.
00692      EXIT.
00693
00694  2240-PRT-CHECK.
00695      IF CHECK-PRINT = SPACES OR LOW-VALUES
00696         OR PRINT-TO-HARDCOPY
00697         OR PRINT-TO-FICHE-TAPE
00698         OR PRINT-TO-BOTH-MEDIA
00699         OR SAVE-REPORT-ONLINE-NO-PRINT
00700         OR SAVE-REPORT-ONLINE-AND-PRINT
00701          GO TO 2250-EXIT.
00702
00703      MOVE 'X'     TO ERROR-SWITCH.
00704      MOVE ER-0164 TO EMI-ERROR.
00705      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00706
00707  2250-EXIT.
00708      EXIT.
00709  EJECT
00710
00711  2300-VERIFY-FORMAT.
00712      MOVE FMT1I TO OPTION-CODE.
00713      PERFORM 2340-FMT-CHECK THRU 2360-EXIT.
00714
00715      IF SCREEN-ERROR
00716          MOVE AL-UABON TO FMT1A
00717          MOVE -1       TO FMT1L
00718          MOVE SPACE    TO ERROR-SWITCH
00719      ELSE
00720          MOVE AL-UANON TO FMT1A
00721          PERFORM 2320-MOVE-FMT THRU 2330-EXIT.
00722
00723
00724      MOVE FMT2I TO OPTION-CODE.
00725      PERFORM 2340-FMT-CHECK THRU 2360-EXIT.
00726
00727      IF SCREEN-ERROR
00728          MOVE AL-UABON TO FMT2A
00729          MOVE -1       TO FMT2L
00730          MOVE SPACE    TO ERROR-SWITCH
00731      ELSE
00732          MOVE AL-UANON TO FMT2A
00733          PERFORM 2320-MOVE-FMT THRU 2330-EXIT.
00734
00735
00736      MOVE FMT3I TO OPTION-CODE.
00737      PERFORM 2340-FMT-CHECK THRU 2360-EXIT.
00738
00739      IF SCREEN-ERROR
00740          MOVE AL-UABON TO FMT3A
00741          MOVE -1       TO FMT3L
00742          MOVE SPACE    TO ERROR-SWITCH
00743      ELSE
00744          MOVE AL-UANON TO FMT3A
00745          PERFORM 2320-MOVE-FMT THRU 2330-EXIT.
00746
00747      MOVE FMT4I TO OPTION-CODE.
00748      PERFORM 2340-FMT-CHECK THRU 2360-EXIT.
00749
00750      IF SCREEN-ERROR
00751          MOVE AL-UABON TO FMT4A
00752          MOVE -1       TO FMT4L
00753          MOVE SPACE    TO ERROR-SWITCH
00754      ELSE
00755          MOVE AL-UANON TO FMT4A
00756          PERFORM 2320-MOVE-FMT THRU 2330-EXIT.
00757
00758  2310-EXIT.
00759      EXIT.
00760
00761  2320-MOVE-FMT.
00762      ADD 1 TO COUNT-1.
00763      MOVE OPTION-CODE TO FORMAT-OPT (COUNT-1).
00764
00765  2330-EXIT.
00766      EXIT.
00767
00768  2340-FMT-CHECK.
00769      IF OPTION-CODE = SPACE OR LOW-VALUES
00770          GO TO 2360-EXIT.
00771
00772      MOVE PGRMI TO PROGRAM-NUMBER.
00773      MOVE 'F'   TO OPTION-TYPE.
00774
00775      
      * EXEC CICS HANDLE CONDITION
00776 *        NOTFND (2350-FMT-NOT-FOUND)
00777 *    END-EXEC.
      *    MOVE '"$I                   ! $ #00002012' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303032303132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00778
00779      
      * EXEC CICS READ
00780 *        SET     (ADDRESS OF PROGRAM-OPTIONS-AVAILABLE)
00781 *        DATASET ('ELPGMO')
00782 *        RIDFLD  (FILE-PGMO-KEY)
00783 *    END-EXEC.
           MOVE 'ELPGMO' TO DFHEIV1
      *    MOVE '&"S        E          (   #00002016' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032303136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 FILE-PGMO-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PROGRAM-OPTIONS-AVAILABLE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00784
00785      GO TO 2360-EXIT.
00786
00787  2350-FMT-NOT-FOUND.
00788      MOVE 'X'     TO ERROR-SWITCH.
00789      MOVE ER-0165 TO EMI-ERROR.
00790      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00791
00792  2360-EXIT.
00793      EXIT.
00794  EJECT
00795  2400-VERIFY-PROCESS.
00796      MOVE PROC1I TO OPTION-CODE.
00797      PERFORM 2440-PROC-CHECK THRU 2460-EXIT.
00798
00799      IF SCREEN-ERROR
00800          MOVE AL-UABON TO PROC1A
00801          MOVE -1       TO PROC1L
00802          MOVE SPACE    TO ERROR-SWITCH
00803      ELSE
00804          MOVE AL-UANON TO PROC1A
00805          PERFORM 2420-MOVE-PROC THRU 2430-EXIT.
00806
00807      MOVE PROC2I TO OPTION-CODE.
00808      PERFORM 2440-PROC-CHECK THRU 2460-EXIT.
00809
00810      IF SCREEN-ERROR
00811          MOVE AL-UABON TO PROC2A
00812          MOVE -1       TO PROC2L
00813          MOVE SPACE    TO ERROR-SWITCH
00814      ELSE
00815          MOVE AL-UANON TO PROC2A
00816          PERFORM 2420-MOVE-PROC THRU 2430-EXIT.
00817
00818      MOVE PROC3I TO OPTION-CODE.
00819      PERFORM 2440-PROC-CHECK THRU 2460-EXIT.
00820
00821      IF SCREEN-ERROR
00822          MOVE AL-UABON TO PROC3A
00823          MOVE -1       TO PROC3L
00824          MOVE SPACE    TO ERROR-SWITCH
00825      ELSE
00826          MOVE AL-UANON TO PROC3A
00827          PERFORM 2420-MOVE-PROC THRU 2430-EXIT.
00828
00829      MOVE PROC4I TO OPTION-CODE.
00830      PERFORM 2440-PROC-CHECK THRU 2460-EXIT.
00831
00832      IF SCREEN-ERROR
00833          MOVE AL-UABON TO PROC4A
00834          MOVE -1       TO PROC4L
00835          MOVE SPACE    TO ERROR-SWITCH
00836      ELSE
00837          MOVE AL-UANON TO PROC4A
00838          PERFORM 2420-MOVE-PROC THRU 2430-EXIT.
00839
00840  2410-EXIT.
00841      EXIT.
00842
00843  2420-MOVE-PROC.
00844      ADD 1 TO COUNT-1.
00845      MOVE OPTION-CODE TO PROCESS-OPT (COUNT-1).
00846
00847  2430-EXIT.
00848      EXIT.
00849
00850  2440-PROC-CHECK.
00851      IF OPTION-CODE = SPACE OR LOW-VALUES OR  'X'
00852          GO TO 2460-EXIT.
00853
00854      MOVE PGRMI TO PROGRAM-NUMBER.
00855      MOVE 'P'   TO OPTION-TYPE.
00856
00857      
      * EXEC CICS HANDLE CONDITION
00858 *        NOTFND (2450-PROC-NOT-FOUND)
00859 *    END-EXEC.
      *    MOVE '"$I                   ! % #00002094' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303032303934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00860
00861      
      * EXEC CICS READ
00862 *        SET     (ADDRESS OF PROGRAM-OPTIONS-AVAILABLE)
00863 *        DATASET ('ELPGMO')
00864 *        RIDFLD  (FILE-PGMO-KEY)
00865 *    END-EXEC.
           MOVE 'ELPGMO' TO DFHEIV1
      *    MOVE '&"S        E          (   #00002098' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032303938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 FILE-PGMO-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PROGRAM-OPTIONS-AVAILABLE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00866
00867      GO TO 2460-EXIT.
00868
00869  2450-PROC-NOT-FOUND.
00870      MOVE 'X'     TO ERROR-SWITCH.
00871      MOVE ER-0166 TO EMI-ERROR.
00872      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00873
00874  2460-EXIT.
00875      EXIT.
00876  EJECT
00877  2500-VERIFY-TOTAL.
00878      MOVE TOT1I TO OPTION-CODE.
00879      PERFORM 2540-TOTAL-CHECK THRU 2560-EXIT.
00880
00881      IF SCREEN-ERROR
00882          MOVE AL-UABON TO TOT1A
00883          MOVE -1       TO TOT1L
00884          MOVE SPACE    TO ERROR-SWITCH
00885      ELSE
00886          MOVE AL-UANON TO TOT1A
00887          PERFORM 2520-MOVE-TOTAL THRU 2530-EXIT.
00888
00889      MOVE TOT2I TO OPTION-CODE.
00890      PERFORM 2540-TOTAL-CHECK THRU 2560-EXIT.
00891
00892      IF SCREEN-ERROR
00893          MOVE AL-UABON TO TOT2A
00894          MOVE -1       TO TOT2L
00895          MOVE SPACE    TO ERROR-SWITCH
00896      ELSE
00897          MOVE AL-UANON TO TOT2A
00898          PERFORM 2520-MOVE-TOTAL THRU 2530-EXIT.
00899
00900      MOVE TOT3I TO OPTION-CODE.
00901      PERFORM 2540-TOTAL-CHECK THRU 2560-EXIT.
00902
00903      IF SCREEN-ERROR
00904          MOVE AL-UABON TO TOT3A
00905          MOVE -1       TO TOT3L
00906          MOVE SPACE    TO ERROR-SWITCH
00907      ELSE
00908          MOVE AL-UANON TO TOT3A
00909          PERFORM 2520-MOVE-TOTAL THRU 2530-EXIT.
00910
00911      MOVE TOT4I TO OPTION-CODE.
00912      PERFORM 2540-TOTAL-CHECK THRU 2560-EXIT.
00913
00914      IF SCREEN-ERROR
00915          MOVE AL-UABON TO TOT4A
00916          MOVE -1       TO TOT4L
00917          MOVE SPACE    TO ERROR-SWITCH
00918      ELSE
00919          MOVE AL-UANON TO TOT4A
00920          PERFORM 2520-MOVE-TOTAL THRU 2530-EXIT.
00921
00922  2510-EXIT.
00923      EXIT.
00924
00925  2520-MOVE-TOTAL.
00926      ADD 1 TO COUNT-1.
00927      MOVE OPTION-CODE TO TOTAL-OPT (COUNT-1).
00928
00929  2530-EXIT.
00930      EXIT.
00931
00932  2540-TOTAL-CHECK.
00933      IF OPTION-CODE = SPACE OR LOW-VALUES
00934          GO TO 2560-EXIT.
00935
00936      MOVE PGRMI TO PROGRAM-NUMBER.
00937      MOVE 'T'   TO OPTION-TYPE.
00938
00939      
      * EXEC CICS HANDLE CONDITION
00940 *        NOTFND (2550-TOT-NOT-FOUND)
00941 *    END-EXEC.
      *    MOVE '"$I                   ! & #00002176' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303032313736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00942
00943      
      * EXEC CICS READ
00944 *        SET     (ADDRESS OF PROGRAM-OPTIONS-AVAILABLE)
00945 *        DATASET ('ELPGMO')
00946 *        RIDFLD  (FILE-PGMO-KEY)
00947 *    END-EXEC.
           MOVE 'ELPGMO' TO DFHEIV1
      *    MOVE '&"S        E          (   #00002180' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 FILE-PGMO-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PROGRAM-OPTIONS-AVAILABLE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00948
00949      GO TO 2560-EXIT.
00950
00951  2550-TOT-NOT-FOUND.
00952      MOVE 'X'     TO ERROR-SWITCH.
00953      MOVE ER-0167 TO EMI-ERROR.
00954      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00955
00956  2560-EXIT.
00957      EXIT.
00958  EJECT
00959  2600-CHECK-REQUIRE.
00960      IF (FREQ1I    = SPACES OR LOW-VALUES)
00961         IF (PRT1I  = SPACES OR LOW-VALUES) AND
00962            (FMT1I  = SPACES OR LOW-VALUES) AND
00963            (PROC1I = SPACES OR LOW-VALUES) AND
00964            (TOT1I  = SPACES OR LOW-VALUES)
00965            NEXT SENTENCE
00966           ELSE
00967            MOVE AL-UABON TO FREQ1A
00968            MOVE -1       TO FREQ1L
00969            MOVE 'X'      TO ERROR-SWITCH
00970        ELSE
00971         IF (PRT1I  = SPACES OR = LOW-VALUES)
00972            MOVE AL-UABON TO PRT1A
00973            MOVE -1       TO PRT1L
00974            MOVE 'X'      TO ERROR-SWITCH.
00975
00976      IF (FREQ2I    = SPACES OR LOW-VALUES)
00977         IF (PRT2I  = SPACES OR LOW-VALUES) AND
00978            (FMT2I  = SPACES OR LOW-VALUES) AND
00979            (PROC2I = SPACES OR LOW-VALUES) AND
00980            (TOT2I  = SPACES OR LOW-VALUES)
00981            NEXT SENTENCE
00982           ELSE
00983            MOVE AL-UABON TO FREQ2A
00984            MOVE -1       TO FREQ2L
00985            MOVE 'X'      TO ERROR-SWITCH
00986        ELSE
00987         IF (PRT2I  = SPACES OR LOW-VALUES)
00988            MOVE AL-UABON TO PRT2A
00989            MOVE -1       TO PRT2L
00990            MOVE 'X'      TO ERROR-SWITCH.
00991
00992      IF (FREQ3I    = SPACES OR LOW-VALUES)
00993         IF (PRT3I  = SPACES OR LOW-VALUES) AND
00994            (FMT3I  = SPACES OR LOW-VALUES) AND
00995            (PROC3I = SPACES OR LOW-VALUES) AND
00996            (TOT3I  = SPACES OR LOW-VALUES)
00997            NEXT SENTENCE
00998           ELSE
00999            MOVE AL-UABON TO FREQ3A
01000            MOVE -1       TO FREQ3L
01001            MOVE 'X'      TO ERROR-SWITCH
01002        ELSE
01003         IF (PRT3I  = SPACES OR LOW-VALUES)
01004            MOVE AL-UABON TO PRT3A
01005            MOVE -1       TO PRT3L
01006            MOVE 'X'      TO ERROR-SWITCH.
01007
01008      IF (FREQ4I    = SPACES OR LOW-VALUES)
01009         IF (PRT4I  = SPACES OR LOW-VALUES) AND
01010            (FMT4I  = SPACES OR LOW-VALUES) AND
01011            (PROC4I = SPACES OR LOW-VALUES) AND
01012            (TOT4I  = SPACES OR LOW-VALUES)
01013            NEXT SENTENCE
01014           ELSE
01015            MOVE AL-UABON TO FREQ4A
01016            MOVE -1       TO FREQ4L
01017            MOVE 'X'      TO ERROR-SWITCH
01018        ELSE
01019         IF (PRT4I  = SPACES OR LOW-VALUES)
01020            MOVE AL-UABON TO PRT4A
01021            MOVE -1       TO PRT4L
01022            MOVE 'X'      TO ERROR-SWITCH.
01023
01024      IF SCREEN-ERROR
01025         MOVE ER-0486          TO EMI-ERROR
01026          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01027
01028  2650-EXIT.
01029      EXIT.
01030  EJECT
01031  3000-FORMAT-RECORD.
01032      ADD 1 TO COUNT-1.
01033      MOVE HOLD-PROGRAM-OPT (COUNT-1)
01034              TO PS-PROGRAM-OPTIONS (COUNT-1).
01035
01036  3010-EXIT.
01037      EXIT.
01038  EJECT
01039  3100-SHOW-OPTIONS.
01040      MOVE PI-COMPANY-CD TO COMPANY-CD.
01041
01042      IF PGRML = ZERO
01043          MOVE LOW-VALUES  TO  PROGRAM-NO
01044      ELSE
01045          MOVE PGRMI       TO  PROGRAM-NO.
01046
01047      IF EIBAID      = DFHPF7   AND
01048         PROGRAM-NO  = LOW-VALUES
01049          GO TO 8700-ENDFILE.
01050
01051      
      * EXEC CICS HANDLE CONDITION
01052 *        NOTFND    (8700-ENDFILE)
01053 *        ENDFILE   (8700-ENDFILE)
01054 *    END-EXEC.
      *    MOVE '"$I''                  ! '' #00002288' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303032323838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01055
01056      
      * EXEC CICS STARTBR
01057 *        DATASET    ('ELPGMS')
01058 *        RIDFLD     (FILE-PGMS-KEY)
01059 *    END-EXEC.
           MOVE 'ELPGMS' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00002293' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032323933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 FILE-PGMS-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01060
01061      MOVE '1'  TO  BROWSE-STARTED-SW.
01062
01063  3110-READ-NEXT.
01064      IF EIBAID  = DFHPF7
01065          
      * EXEC CICS READPREV
01066 *            DATASET    ('ELPGMS')
01067 *            SET        (ADDRESS OF PROGRAM-OPTIONS-SELECTED)
01068 *            RIDFLD     (FILE-PGMS-KEY)
01069 *        END-EXEC
           MOVE 'ELPGMS' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00002302' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 FILE-PGMS-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PROGRAM-OPTIONS-SELECTED TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01070      ELSE
01071          
      * EXEC CICS READNEXT
01072 *            DATASET    ('ELPGMS')
01073 *            SET        (ADDRESS OF PROGRAM-OPTIONS-SELECTED)
01074 *            RIDFLD     (FILE-PGMS-KEY)
01075 *        END-EXEC.
           MOVE 'ELPGMS' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00002308' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 FILE-PGMS-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PROGRAM-OPTIONS-SELECTED TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01076
01077      IF PS-COMPANY-CD  NOT = PI-COMPANY-CD
01078          GO TO 8700-ENDFILE.
01079
01080      IF PS-PROGRAM-NUMBER  = PI-PROGRAM-NO
01081          GO TO 3110-READ-NEXT.
01082
01083      MOVE ZEROS   TO  COUNT-1.
01084
01085      PERFORM 4100-FORMAT-WS THRU 4110-EXIT 4 TIMES.
01086
01087      MOVE ZEROS   TO  COUNT-1.
01088
01089      PERFORM 4200-FORMAT-SCREEN  THRU  4210-EXIT.
01090
01091      MOVE -1                 TO  MAINTL.
01092      MOVE SPACE              TO  MAINTO.
01093      MOVE AL-UANON           TO  PGRMA.
01094      MOVE PS-PROGRAM-NUMBER  TO  PGRMO  PI-PROGRAM-NO.
01095
01096      IF BROWSE-STARTED
01097          MOVE ' '  TO  BROWSE-STARTED-SW
01098          PERFORM 3200-END-BROWSE  THRU  3299-EXIT.
01099
01100      GO TO 8110-SEND-DATA.
01101  EJECT
01102  3200-END-BROWSE.
01103      
      * EXEC CICS ENDBR
01104 *        DATASET    ('ELPGMS')
01105 *    END-EXEC.
           MOVE 'ELPGMS' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00002340' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01106
01107  3299-EXIT.
01108      EXIT.
01109  EJECT
01110  4000-SHOW-OPTIONS.
01111      MOVE PI-COMPANY-CD TO COMPANY-CD.
01112      MOVE PGRMI         TO PROGRAM-NO.
01113
01114      
      * EXEC CICS HANDLE CONDITION
01115 *        NOTFND (4010-FORMAT-OPTIONS)
01116 *    END-EXEC.
      *    MOVE '"$I                   ! ( #00002351' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303032333531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01117
01118      
      * EXEC CICS READ
01119 *        SET     (ADDRESS OF PROGRAM-OPTIONS-SELECTED)
01120 *        DATASET ('ELPGMS')
01121 *        RIDFLD  (FILE-PGMS-KEY)
01122 *    END-EXEC.
           MOVE 'ELPGMS' TO DFHEIV1
      *    MOVE '&"S        E          (   #00002355' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 FILE-PGMS-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PROGRAM-OPTIONS-SELECTED TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01123
01124      MOVE ZEROS TO COUNT-1.
01125      PERFORM 4100-FORMAT-WS THRU 4110-EXIT 4 TIMES.
01126
01127  4010-FORMAT-OPTIONS.
01128      IF PROGRAM-OPT-HOLD = SPACES
01129          MOVE ER-0269 TO EMI-ERROR
01130          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01131          MOVE -1                 TO  MAINTL
01132          GO TO 8110-SEND-DATA.
01133
01134      MOVE ZEROS TO COUNT-1.
01135      PERFORM 4200-FORMAT-SCREEN THRU 4210-EXIT.
01136
01137  4020-EXIT.
01138      EXIT.
01139  EJECT
01140  4100-FORMAT-WS.
01141      ADD 1 TO COUNT-1.
01142      MOVE PS-PROGRAM-OPTIONS (COUNT-1)
01143              TO HOLD-PROGRAM-OPT (COUNT-1).
01144
01145  4110-EXIT.
01146      EXIT.
01147  EJECT
01148  4200-FORMAT-SCREEN.
01149      MOVE ' '                    TO  DC-OPTION-CODE.
01150
01151      IF PS-LAST-MAINT-DT  IS EQUAL TO  SPACES
01152          MOVE LOW-VALUES         TO  PS-LAST-MAINT-DT.
01153
01154      MOVE PS-LAST-MAINT-DT       TO  DC-BIN-DATE-1.
01155
01156      PERFORM 9700-LINK-DATE-CONVERT  THRU  9700-EXIT.
01157
01158      IF NO-CONVERSION-ERROR
01159          MOVE DC-GREG-DATE-1-EDIT
01160                                  TO  LSTDTEO
01161      ELSE
01162          MOVE '00/00/00'         TO  LSTDTEO.
01163
01164      IF PS-LAST-MAINT-HHMMSS  IS NUMERIC
01165          MOVE PS-LAST-MAINT-HHMMSS
01166                                  TO  TIME-IN
01167          MOVE TIME-OUT           TO  LSTTIMEO
01168      ELSE
01169          MOVE ZEROS              TO  TIME-IN
01170          MOVE TIME-OUT           TO  LSTTIMEO.
01171
01172      MOVE PS-LAST-MAINT-USER     TO  LSTUSRO.
01173
01174      ADD 1 TO COUNT-1.
01175      MOVE FREQ-CODE (COUNT-1)   TO FREQ1O.
01176      MOVE PRINT-OPT (COUNT-1)   TO PRT1O.
01177      MOVE FORMAT-OPT (COUNT-1)  TO FMT1O.
01178      MOVE PROCESS-OPT (COUNT-1) TO PROC1O.
01179      MOVE TOTAL-OPT (COUNT-1)   TO TOT1O.
01180      MOVE AL-UANON              TO FREQ1A
01181                                    PRT1A
01182                                    FMT1A
01183                                    PROC1A
01184                                    TOT1A.
01185      ADD 1 TO COUNT-1.
01186      MOVE FREQ-CODE (COUNT-1)   TO FREQ2O.
01187      MOVE PRINT-OPT (COUNT-1)   TO PRT2O.
01188      MOVE FORMAT-OPT (COUNT-1)  TO FMT2O.
01189      MOVE PROCESS-OPT (COUNT-1) TO PROC2O.
01190      MOVE TOTAL-OPT (COUNT-1)   TO TOT2O.
01191      MOVE AL-UANON              TO FREQ2A
01192                                    PRT2A
01193                                    FMT2A
01194                                    PROC2A
01195                                    TOT2A.
01196      ADD 1 TO COUNT-1.
01197      MOVE FREQ-CODE (COUNT-1)   TO FREQ3O.
01198      MOVE PRINT-OPT (COUNT-1)   TO PRT3O.
01199      MOVE FORMAT-OPT (COUNT-1)  TO FMT3O.
01200      MOVE PROCESS-OPT (COUNT-1) TO PROC3O.
01201      MOVE TOTAL-OPT (COUNT-1)   TO TOT3O.
01202      MOVE AL-UANON              TO FREQ3A
01203                                    PRT3A
01204                                    FMT3A
01205                                    PROC3A
01206                                    TOT3A.
01207      ADD 1 TO COUNT-1.
01208      MOVE FREQ-CODE (COUNT-1)   TO FREQ4O.
01209      MOVE PRINT-OPT (COUNT-1)   TO PRT4O.
01210      MOVE FORMAT-OPT (COUNT-1)  TO FMT4O.
01211      MOVE PROCESS-OPT (COUNT-1) TO PROC4O.
01212      MOVE TOTAL-OPT (COUNT-1)   TO TOT4O.
01213      MOVE AL-UANON              TO FREQ4A
01214                                    PRT4A
01215                                    FMT4A
01216                                    PROC4A
01217                                    TOT4A.
01218
01219  4210-EXIT.
01220      EXIT.
01221  EJECT
01222  5000-SHOW-FREQ.
01223      MOVE SPACE TO ERROR-SWITCH.
01224      MOVE ZEROS TO COUNT-1.
01225      PERFORM 5010-FREQ-OPTION THRU 5020-EXIT 4 TIMES.
01226
01227      PERFORM 5130-SPACE-SCREEN THRU 5140-EXIT 5 TIMES.
01228
01229      MOVE 'FREQUENCY' TO VARDESCO.
01230      MOVE -1          TO MAINTL.
01231      GO TO 8110-SEND-DATA.
01232
01233  5010-FREQ-OPTION.
01234      ADD 1 TO COUNT-1.
01235      MOVE OPTIONS-FREQ (COUNT-1) TO OPTO (COUNT-1).
01236
01237  5020-EXIT.
01238      EXIT.
01239  EJECT
01240  5100-SHOW-PRINT.
01241      MOVE SPACES TO ERROR-SWITCH.
01242      MOVE ZEROS  TO COUNT-1.
01243      PERFORM 5110-PRINT-OPTION THRU 5120-EXIT 5 TIMES.
01244
01245      PERFORM 5130-SPACE-SCREEN THRU 5140-EXIT 4 TIMES.
01246
01247      MOVE 'PRINT' TO VARDESCO.
01248      MOVE -1      TO MAINTL.
01249      GO TO 8110-SEND-DATA.
01250
01251  5110-PRINT-OPTION.
01252      ADD 1 TO COUNT-1.
01253      MOVE OPTIONS-PRINT (COUNT-1) TO OPTO (COUNT-1).
01254
01255  5120-EXIT.
01256      EXIT.
01257
01258  5130-SPACE-SCREEN.
01259      ADD 1 TO COUNT-1.
01260      MOVE SPACES TO OPTO (COUNT-1).
01261
01262  5140-EXIT.
01263      EXIT.
01264  EJECT
01265  5200-SHOW-TOTAL.
01266      MOVE SPACE TO ERROR-SWITCH.
01267      
      * EXEC CICS HANDLE CONDITION
01268 *        NOTFND (5210-TOTAL-NOTFND)
01269 *        ENDFILE (6030-END-OF-FILE)
01270 *    END-EXEC.
      *    MOVE '"$I''                  ! ) #00002504' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303032353034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01271
01272      MOVE PGRMI  TO PROGRAM-NUMBER HOLD-PROGRAM.
01273      MOVE 'T'    TO OPTION-TYPE HOLD-TYPE.
01274      MOVE SPACES TO OPTION-CODE TYPE-SWITCH PROGRAM-OPT-DESC.
01275
01276      PERFORM 6000-START-BROWSE THRU 6010-EXIT.
01277
01278      PERFORM 6020-READ-FILE THRU 6040-EXIT
01279          UNTIL END-OF-TYPE.
01280
01281      PERFORM 6050-END-BROWSE THRU 6080-EXIT.
01282
01283  5210-TOTAL-NOTFND.
01284      IF PROGRAM-OPT-DESC = SPACES
01285          MOVE ER-0267 TO EMI-ERROR
01286          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01287
01288      MOVE ZEROS TO COUNT-1.
01289      PERFORM 5500-FORMAT-SCREEN THRU 5510-EXIT 8 TIMES.
01290
01291      PERFORM 5130-SPACE-SCREEN THRU 5140-EXIT.
01292
01293      MOVE 'TOTAL' TO VARDESCO.
01294      MOVE -1      TO MAINTL.
01295      GO TO 8110-SEND-DATA.
01296  EJECT
01297  5300-SHOW-PROCESS.
01298      MOVE SPACE TO ERROR-SWITCH.
01299      
      * EXEC CICS HANDLE CONDITION
01300 *        NOTFND  (5310-PROCESS-NOTFND)
01301 *        ENDFILE (6030-END-OF-FILE)
01302 *    END-EXEC.
      *    MOVE '"$I''                  ! * #00002536' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303032353336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01303
01304      MOVE PGRMI  TO PROGRAM-NUMBER HOLD-PROGRAM.
01305      MOVE 'P'    TO OPTION-TYPE HOLD-TYPE.
01306      MOVE SPACES TO OPTION-CODE TYPE-SWITCH PROGRAM-OPT-DESC.
01307
01308      PERFORM 6000-START-BROWSE THRU 6010-EXIT.
01309
01310      PERFORM 6020-READ-FILE THRU 6040-EXIT
01311          UNTIL END-OF-TYPE.
01312
01313      PERFORM 6050-END-BROWSE THRU 6080-EXIT.
01314
01315  5310-PROCESS-NOTFND.
01316      IF PROGRAM-OPT-DESC = SPACES
01317          MOVE ER-0267 TO EMI-ERROR
01318          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01319
01320      MOVE ZEROS TO COUNT-1.
01321      PERFORM 5500-FORMAT-SCREEN THRU 5510-EXIT 8 TIMES.
01322
01323      ADD 1                       TO COUNT-1.
01324      MOVE DO-NOT-RUN             TO OPTO (COUNT-1).
01325      MOVE 'PROCESS' TO VARDESCO.
01326      MOVE -1        TO MAINTL.
01327      GO TO 8110-SEND-DATA.
01328  EJECT
01329  5400-SHOW-FORMAT.
01330      MOVE SPACE TO ERROR-SWITCH.
01331      
      * EXEC CICS HANDLE CONDITION
01332 *        NOTFND  (5410-FORMAT-NOTFND)
01333 *        ENDFILE (6030-END-OF-FILE)
01334 *    END-EXEC.
      *    MOVE '"$I''                  ! + #00002568' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303032353638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01335
01336      MOVE PGRMI  TO PROGRAM-NUMBER HOLD-PROGRAM.
01337      MOVE 'F'    TO OPTION-TYPE HOLD-TYPE.
01338      MOVE SPACES TO OPTION-CODE TYPE-SWITCH PROGRAM-OPT-DESC.
01339
01340      PERFORM 6000-START-BROWSE THRU 6010-EXIT.
01341
01342      PERFORM 6020-READ-FILE THRU 6040-EXIT
01343          UNTIL END-OF-TYPE.
01344
01345      PERFORM 6050-END-BROWSE THRU 6080-EXIT.
01346
01347  5410-FORMAT-NOTFND.
01348      IF PROGRAM-OPT-DESC = SPACES
01349          MOVE ER-0267 TO EMI-ERROR
01350          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01351
01352      MOVE ZEROS TO COUNT-1.
01353      PERFORM 5500-FORMAT-SCREEN THRU 5510-EXIT 8 TIMES.
01354
01355      PERFORM 5130-SPACE-SCREEN THRU 5140-EXIT.
01356
01357      MOVE 'FORMAT' TO VARDESCO.
01358      MOVE -1       TO MAINTL.
01359      GO TO 8110-SEND-DATA.
01360  EJECT
01361  5500-FORMAT-SCREEN.
01362      ADD 1 TO COUNT-1.
01363      MOVE COUNT-1 TO OPT-NO (COUNT-1).
01364      MOVE '='     TO CONST-FILL (COUNT-1).
01365      MOVE DESC-PROGRAM-OPT (COUNT-1) TO OPTO (COUNT-1).
01366
01367  5510-EXIT.
01368      EXIT.
01369  EJECT
01370  6000-START-BROWSE.
01371      
      * EXEC CICS STARTBR
01372 *        DATASET ('ELPGMO')
01373 *        RIDFLD (FILE-PGMO-KEY)
01374 *    END-EXEC.
           MOVE 'ELPGMO' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00002608' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 FILE-PGMO-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01375
01376  6010-EXIT.
01377      EXIT.
01378
01379  6020-READ-FILE.
01380      
      * EXEC CICS READNEXT
01381 *        SET     (ADDRESS OF PROGRAM-OPTIONS-AVAILABLE)
01382 *        DATASET ('ELPGMO')
01383 *        RIDFLD  (FILE-PGMO-KEY)
01384 *    END-EXEC.
           MOVE 'ELPGMO' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00002617' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 FILE-PGMO-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PROGRAM-OPTIONS-AVAILABLE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01385
01386      IF PO-PROGRAM-NUMBER NOT = HOLD-PROGRAM OR
01387         PO-OPTION-TYPE NOT = HOLD-TYPE
01388          GO TO 6030-END-OF-FILE.
01389
01390      MOVE PO-PGM-OPTION-CD      TO COUNT-1.
01391      MOVE PO-OPTION-DESCRIPTION TO PGMO-DESC (COUNT-1).
01392      GO TO 6040-EXIT.
01393
01394  6030-END-OF-FILE.
01395      MOVE 'X' TO TYPE-SWITCH.
01396
01397  6040-EXIT.
01398      EXIT.
01399
01400  6050-END-BROWSE.
01401      
      * EXEC CICS ENDBR
01402 *        DATASET ('ELPGMO')
01403 *    END-EXEC.
           MOVE 'ELPGMO' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00002638' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01404
01405  6080-EXIT.
01406      EXIT.
01407  EJECT
01408  8100-SEND-INITIAL-MAP.
01409      PERFORM 8120-FORMAT-TIME-DATE THRU 8130-EXIT.
01410
01411      
      * EXEC CICS SEND
01412 *        MAP    ('EL108A')
01413 *        MAPSET ('EL108S')
01414 *        ERASE
01415 *        FREEKB
01416 *        CURSOR
01417 *    END-EXEC.
           MOVE 'EL108A' TO DFHEIV1
           MOVE -1
             TO DFHEIV11
           MOVE 'EL108S' TO DFHEIV2
      *    MOVE '8$     CT  E F  H     ,   #00002648' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202020202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL108AO, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01418
01419      GO TO 9000-RETURN-TRANS.
01420
01421  8110-SEND-DATA.
01422      PERFORM 8120-FORMAT-TIME-DATE THRU 8130-EXIT.
01423
01424      
      * EXEC CICS SEND
01425 *        MAP    ('EL108A')
01426 *        MAPSET ('EL108S')
01427 *        DATAONLY
01428 *        FREEKB
01429 *        CURSOR
01430 *    END-EXEC.
           MOVE 'EL108A' TO DFHEIV1
           MOVE -1
             TO DFHEIV11
           MOVE 'EL108S' TO DFHEIV2
      *    MOVE '8$D    CT    F  H     ,   #00002661' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'204620204820202020202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL108AO, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01431
01432      GO TO 9000-RETURN-TRANS.
01433
01434  8120-FORMAT-TIME-DATE.
01435      MOVE SAVE-DATE            TO DATEO.
01436      MOVE EIBTIME              TO TIME-IN.
01437      MOVE TIME-OUT             TO TIMEO.
01438      MOVE LIT-MAP              TO PI-CURRENT-SCREEN-NO.
01439      MOVE EMI-ERROR-NUMBER (1) TO PI-LAST-ERROR-NO.
01440      MOVE EMI-MESSAGE-AREA (1) TO MSGO.
01441
01442  8130-EXIT.
01443      EXIT.
01444
01445  8200-RETURN-PRIOR.
01446      MOVE PI-RETURN-TO-PROGRAM TO CALL-PGM.
01447      GO TO 9200-XCTL.
01448
01449  8300-GET-HELP.
01450      MOVE XCTL-EL010 TO CALL-PGM.
01451      GO TO 9200-XCTL.
01452
01453  8400-RETURN-MASTER.
01454
01455      IF  CREDIT-SESSION
01456          MOVE XCTL-EL626         TO CALL-PGM
01457
01458      ELSE
01459          IF  CLAIM-SESSION
01460              MOVE XCTL-EL126     TO CALL-PGM
01461
01462          ELSE
01463              IF  MORTGAGE-SESSION
01464                  MOVE XCTL-EM626 TO CALL-PGM
01465
01466              ELSE
01467                  IF  GENERAL-LEDGER-SESSION
01468                      MOVE XCTL-GL800
01469                                  TO CALL-PGM.
01470
01471      GO TO 9200-XCTL.
01472
01473  8700-ENDFILE.
01474      IF BROWSE-STARTED
01475          MOVE ' '  TO  BROWSE-STARTED-SW
01476          PERFORM 3200-END-BROWSE  THRU  3299-EXIT.
01477
01478      IF EIBAID  = DFHPF7
01479          MOVE ER-0067  TO  EMI-ERROR
01480      ELSE
01481          MOVE ER-0066  TO  EMI-ERROR.
01482
01483      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01484
01485      MOVE -1      TO  MAINTL.
01486      MOVE SPACE   TO  MAINTO.
01487
01488      GO TO 8110-SEND-DATA.
01489
01490  8800-UNAUTHORIZED-ACCESS.
01491      MOVE UNACCESS-MSG TO LOGOFF-MSG.
01492      GO TO 8990-SEND-TEXT.
01493
01494  8810-PF23-ENTERED.
01495      MOVE EIBAID     TO PI-ENTRY-CD-1.
01496      MOVE XCTL-EL005 TO CALL-PGM.
01497      GO TO 9200-XCTL.
01498
01499  8820-XCTL-ERROR.
01500      
      * EXEC CICS HANDLE CONDITION
01501 *        PGMIDERR (8990-SEND-TEXT)
01502 *    END-EXEC.
      *    MOVE '"$L                   ! , #00002737' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303032373337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01503
01504      MOVE SPACE      TO PI-ENTRY-CD-1.
01505      MOVE CALL-PGM   TO PI-CALLING-PROGRAM.
01506      MOVE XCTL-EL005 TO CALL-PGM
01507                         LOGOFF-PGM.
01508      MOVE PGMIDERR-MSG TO LOGOFF-FILL.
01509      GO TO 9200-XCTL.
01510
01511  8990-SEND-TEXT.
01512      
      * EXEC CICS SEND TEXT
01513 *        FROM   (LOGOFF-TEXT)
01514 *        LENGTH (LOGOFF-LENGTH)
01515 *        ERASE
01516 *        FREEKB
01517 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00002749' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373439' TO DFHEIV0(25:11)
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
           
01518
01519      GO TO 9100-RETURN-CICS.
01520  EJECT
01521  9000-RETURN-TRANS.
01522      
      * EXEC CICS RETURN
01523 *        TRANSID (TRANS-ID)
01524 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
01525 *        LENGTH (PI-COMM-LENGTH)
01526 *    END-EXEC.
      *    MOVE '.(CT                  &   #00002759' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01527
01528      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL108' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
01529
01530  9100-RETURN-CICS.
01531      
      * EXEC CICS RETURN
01532 *    END-EXEC.
      *    MOVE '.(                    &   #00002768' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01533      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL108' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
01534
01535  9200-XCTL.
01536      
      * EXEC CICS XCTL
01537 *        PROGRAM (CALL-PGM)
01538 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
01539 *        LENGTH (PI-COMM-LENGTH)
01540 *    END-EXEC.
      *    MOVE '.$C                   $   #00002773' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CALL-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01541
01542  9700-LINK-DATE-CONVERT.
01543      
      * EXEC CICS LINK
01544 *        PROGRAM    ('ELDATCV')
01545 *        COMMAREA   (DATE-CONVERSION-DATA)
01546 *        LENGTH     (DC-COMM-LENGTH)
01547 *        END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00002780' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01548
01549  9700-EXIT.
01550      EXIT.
01551
01552  9900-ERROR-FORMAT.
01553      IF NOT EMI-ERRORS-COMPLETE
01554          
      * EXEC CICS LINK
01555 *            PROGRAM ('EL001')
01556 *            COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
01557 *            LENGTH (EMI-COMM-LENGTH)
01558 *        END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   ''   #00002791' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01559
01560  9900-EXIT.
01561      EXIT.
01562
01563  9990-ABEND.
01564      MOVE DFHEIBLK               TO EMI-LINE1
01565      MOVE -1                     TO MAINTL.
01566      
      * EXEC CICS LINK
01567 *        PROGRAM   ('EL004')
01568 *        COMMAREA  (EMI-LINE1)
01569 *        LENGTH    (72)
01570 *        END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00002803' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01571
01572      GO TO 8110-SEND-DATA.
01573
01574  9995-SECURITY-VIOLATION.
01575 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00002829' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383239' TO DFHEIV0(25:11)
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
01576
01577  9995-EXIT.
01578      EXIT.
01579

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL108' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 8820-XCTL-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 2010-PGMS-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 2350-FMT-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 2450-PROC-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 2550-TOT-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 8700-ENDFILE,
                     8700-ENDFILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 4010-FORMAT-OPTIONS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 5210-TOTAL-NOTFND,
                     6030-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 5310-PROCESS-NOTFND,
                     6030-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 5410-FORMAT-NOTFND,
                     6030-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 8990-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL108' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
