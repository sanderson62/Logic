00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL634 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 09:51:04.
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE
00008 *                            VMOD=2.009
00009
00010 *AUTHOR.        LOGIC,INC.
00011 *               DALLAS, TEXAS.
00012
00013 *DATE-COMPILED.
00014
00015 *SECURITY.   *****************************************************
00016 *            *                                                   *
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00018 *            *                                                   *
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00022 *            *                                                   *
00023 *            *****************************************************
00024
00025 *REMARKS.
00026 *        TRANSACTION - EXB9 - RETRO/REINSURANCE ADJUSTMENTS
00027
00028  ENVIRONMENT DIVISION.
00029  DATA DIVISION.
00030  EJECT
00031  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00032  77  FILLER  PIC X(32)  VALUE '********************************'.
00033  77  FILLER  PIC X(32)  VALUE '*    EL634 WORKING STORAGE     *'.
00034  77  FILLER  PIC X(32)  VALUE '************ V/M 2.009 *********'.
00035
00036 *                            COPY ELCSCTM.
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
00037 *                            COPY ELCSCRTY.
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
00038     EJECT
00039
00040  01  STANDARD-AREAS.
00041      12  SC-ITEM             PIC  S9(4) COMP VALUE +1.
00042      12  GETMAIN-SPACE       PIC  X          VALUE SPACE.
00043      12  EL634A              PIC  X(8)       VALUE 'EL634A'.
00044      12  MAPSET-NAME         PIC  X(8)       VALUE 'EL634S'.
00045      12  SCREEN-NUMBER       PIC  X(4)       VALUE '634A'.
00046      12  TRANS-ID            PIC  X(4)       VALUE 'EXB9'.
00047      12  THIS-PGM            PIC  X(8)       VALUE 'EL634'.
00048      12  PGM-NAME            PIC  X(8).
00049      12  TIME-IN             PIC S9(7).
00050      12  TIME-OUT-R  REDEFINES  TIME-IN.
00051          16  FILLER          PIC  X.
00052          16  TIME-OUT        PIC  99V99.
00053          16  FILLER          PIC  X(2).
00054      12  XCTL-005            PIC  X(8)       VALUE 'EL005'.
00055      12  XCTL-010            PIC  X(8)       VALUE 'EL010'.
00056      12  XCTL-626            PIC  X(8)       VALUE 'EL626'.
00057      12  LINK-001            PIC  X(8)       VALUE 'EL001'.
00058      12  LINK-004            PIC  X(8)       VALUE 'EL004'.
00059      12  LINK-CLDATCV        PIC  X(8)       VALUE 'ELDATCV'.
00060      12  ELCNTL-FILE-ID      PIC  X(8)       VALUE 'ELCNTL'.
00061      12  ERACCT-FILE-ID      PIC  X(8)       VALUE 'ERACCT'.
00062      12  ERACCT-ALT-FILE-ID  PIC  X(8)       VALUE 'ERACCT2'.
00063      12  ERREPY-FILE-ID      PIC  X(8)       VALUE 'ERREPY'.
00064      12  ERREIN-FILE-ID      PIC  X(8)       VALUE 'ERREIN'.
00065      12  RETURNED-FROM       PIC  X(8)       VALUE SPACES.
00066      12  WS-CURRENT-DT       PIC  X(8)       VALUE SPACES.
00067      12  WS-CURRENT-BIN-DT   PIC  X(2)       VALUE SPACES.
00068      12  WS-EXP-DT-HLD       PIC  X(2)       VALUE SPACES.
00069      12  WS-EFF-DT-HLD       PIC  X(2)       VALUE SPACES.
00070      12  WS-INFORCEI         PIC  S9(9)V99.
00071      12  WS-MORTAMTI         PIC  S9(9)V99.
00072      12  WS-FUTUREI          PIC  S9(9)V99.
00073      12  WS-PTCI             PIC  S9(9)V99.
00074      12  WS-IBNRI            PIC  S9(9)V99.
00075      12  WS-CLAIMI           PIC  S9(9)V99.
00076      12  WS-EXPI             PIC  S9(9)V99.
00077      12  WS-PYMNTI           PIC  S9(9)V99.
00078      12  WS-OCOMMI           PIC  S9(9)V99.
00079      12  WS-RPREMI           PIC  S9(9)V99.
00080
00081  01  WORK-AREAS.
00082      12  WS-SV-CARRIER               PIC  X      VALUE SPACES.
00083      12  WS-SV-GROUPING              PIC  X(6)   VALUE SPACES.
00084      12  WS-SV-STATE                 PIC  X(2)   VALUE SPACES.
CIDMOD     12  WS-DEEDIT-FIELD             PIC S9(9)V9(2).
CIDMOD     12  WS-DEEDIT-FIELD-V0  REDEFINES
CIDMOD         WS-DEEDIT-FIELD             PIC S9(11).
00085      12  WS-DEEDIT-DATE              PIC  X(8)   VALUE SPACES.
00086      12  FILLER  REDEFINES  WS-DEEDIT-DATE.
00087          16  FILLER                  PIC  X(2).
00088          16  WS-DEEDIT-DATE-6        PIC  X(6).
00089      12  EDIT-SUB                    PIC  9(3)   VALUE ZEROS.
00090      12  ED-SUB                      PIC  9(3)   VALUE ZEROS.
00091      12  WS-EDITED-BEN-CD            PIC  X(2)   VALUE SPACES.
00092      12  WS-BIN-EFF-DATE-ENTERED     PIC  X(2)   VALUE SPACES.
00093      12  WS-EFF-DATE-ENTERED         PIC  X(8).
00094      12  WS-PI-END-DATE.
00095          16  WS-PI-END-MO            PIC  99.
00096          16  WS-PI-END-DA            PIC  99.
00097          16  WS-PI-END-YR            PIC  99.
00098      12  WS-END-DATE1.
               16  WS-END-CENT1            PIC  99   VALUE 20.
00099          16  WS-END-YR1              PIC  99.
00100          16  WS-END-MO1              PIC  99.
00101      12  WS-END-DATE2.
               16  WS-END-CENT2            PIC  99   VALUE 20.
00102          16  WS-END-YR2              PIC  99.
00103          16  WS-END-MO2              PIC  99.
00104  EJECT
00105  01  ACCESS-KEYS.
00106      12  ELCNTL-KEY.
00107          16  CNTL-COMP-ID        PIC  X(3)   VALUE SPACES.
00108          16  CNTL-REC-TYPE       PIC  X      VALUE SPACES.
00109          16  CNTL-ACCESS.
00110              20  CNTL-STATE      PIC  X(2)   VALUE SPACES.
00111              20  CNTL-HI-BEN-CD.
00112                  24  FILLER       PIC  X      VALUE SPACES.
00113                  24  CNTL-CARRIER PIC  X      VALUE SPACES.
00114          16  CNTL-SEQ            PIC S9(4)   VALUE +0   COMP.
00115      12  ERACCT-KEY.
00116          16  ERACCT-COMP-KEY.
00117              20  ACCT-CO         PIC  X      VALUE SPACES.
00118              20  ACCT-CARRIER    PIC  X      VALUE SPACES.
00119              20  ACCT-GROUPING   PIC  X(6)   VALUE SPACES.
00120              20  ACCT-STATE      PIC  X(2)   VALUE SPACES.
00121              20  ACCT-ACCOUNT    PIC  X(10)  VALUE SPACES.
00122          16  ACCT-EXP-DATE       PIC  X(2)   VALUE SPACES.
00123      12  ERACCT-SAVE-KEY-22.
00124          16  ERACCT-SAVE-KEY     PIC  X(20)  VALUE SPACES.
00125          16  FILLER              PIC  XX.
00126      12  ERREPY-KEY.
00127          16  REPY-COMPANY-CD     PIC  X      VALUE SPACE.
00128          16  REPY-CARRIER        PIC  X      VALUE SPACE.
00129          16  REPY-GROUPING       PIC  X(6)   VALUE SPACES.
00130          16  REPY-STATE          PIC  X(2)   VALUE SPACES.
00131          16  REPY-ACCOUNT        PIC  X(10)  VALUE SPACES.
00132          16  REPY-RECORD-SEQ     PIC S9(8)   VALUE +0   COMP.
00133          16  REPY-RECORD-TYPE    PIC  X      VALUE '1'.
00134      12  ERREIN-KEY.
00135          16  REIN-COMPANY-CD     PIC  X      VALUE SPACE.
00136          16  REIN-CODE           PIC  X      VALUE SPACE.
00137          16  REIN-TABLE          PIC  X(6)   VALUE SPACES.
00138  EJECT
00139  01  ERROR-NUMBERS.
00140      12  ER-0000             PIC  X(4)       VALUE '0000'.
00141      12  ER-0004             PIC  X(4)       VALUE '0004'.
00142      12  ER-0008             PIC  X(4)       VALUE '0008'.
00143      12  ER-0023             PIC  X(4)       VALUE '0023'.
00144      12  ER-0026             PIC  X(4)       VALUE '0026'.
00145      12  ER-0029             PIC  X(4)       VALUE '0029'.
00146      12  ER-0070             PIC  X(4)       VALUE '0070'.
00147      12  ER-0194             PIC  X(4)       VALUE '0194'.
00148      12  ER-0195             PIC  X(4)       VALUE '0195'.
00149      12  ER-0196             PIC  X(4)       VALUE '0196'.
00150      12  ER-0197             PIC  X(4)       VALUE '0197'.
00151      12  ER-0216             PIC  X(4)       VALUE '0216'.
00152      12  ER-0231             PIC  X(4)       VALUE '0231'.
00153      12  ER-2056             PIC  X(4)       VALUE '2056'.
00154      12  ER-2208             PIC  X(4)       VALUE '2208'.
00155      12  ER-2209             PIC  X(4)       VALUE '2209'.
00156      12  ER-2210             PIC  X(4)       VALUE '2210'.
00157      12  ER-2237             PIC  X(4)       VALUE '2237'.
00158      12  ER-2238             PIC  X(4)       VALUE '2238'.
00159      12  ER-2423             PIC  X(4)       VALUE '2423'.
00160      12  ER-2427             PIC  X(4)       VALUE '2427'.
00161      12  ER-2540             PIC  X(4)       VALUE '2540'.
00162      12  ER-2543             PIC  X(4)       VALUE '2543'.
00163      12  ER-2575             PIC  X(4)       VALUE '2575'.
00164      12  ER-2576             PIC  X(4)       VALUE '2576'.
00165      12  ER-2577             PIC  X(4)       VALUE '2577'.
00166      12  ER-2578             PIC  X(4)       VALUE '2578'.
00167      12  ER-2579             PIC  X(4)       VALUE '2579'.
00168      12  ER-2580             PIC  X(4)       VALUE '2580'.
00169      12  ER-2581             PIC  X(4)       VALUE '2581'.
00170      12  ER-2582             PIC  X(4)       VALUE '2582'.
00171      12  ER-2584             PIC  X(4)       VALUE '2584'.
00172      12  ER-2585             PIC  X(4)       VALUE '2585'.
00173      12  ER-2586             PIC  X(4)       VALUE '2586'.
00174      12  ER-2604             PIC  X(4)       VALUE '2604'.
00175      12  ER-2605             PIC  X(4)       VALUE '2605'.
00176      12  ER-2705             PIC  X(4)       VALUE '2705'.
00177  EJECT
00178 *                                    COPY ELCDATE.
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
00179  EJECT
00180 *                                    COPY ELCLOGOF.
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
00181  EJECT
00182 *                                    COPY ELCATTR.
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
00183  EJECT
00184 *                                    COPY ELCEMIB.
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
00185  EJECT
00186 *                                    COPY ELCINTF.
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
00187      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.
00188          16  PI-EOF-SW               PIC  X.
00189              88  PI-FILE-EOF                 VALUE 'Y'.
00190          16  PI-PREV-MAINT           PIC  X.
00191          16  PI-WORK-SEQ-NO          PIC S9(9)          COMP-3.
00192          16  PI-ERREPY-KEY.
00193              20  PI-REPY-COMP-CD     PIC  X.
00194              20  PI-REPY-CARRIER     PIC  X.
00195              20  PI-REPY-GROUPING    PIC  X(6).
00196              20  PI-REPY-STATE       PIC  X(2).
00197              20  PI-REPY-ACCOUNT     PIC  X(10).
00198              20  PI-RECORD-SEQUENCE  PIC S9(8)          COMP.
00199              20  PI-REPY-REC-TYP     PIC  X.
00200          16  FILLER                  PIC  X(608).
00201 *
00202 *01  JOURNAL-RECORD          COPY ELCJPFX.
00203 *                            PIC  X(200).
00204  EJECT
00205 *                            COPY ELCAID.
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
00206
00207  01  FILLER REDEFINES DFHAID.
00208      12  FILLER              PIC  X(8).
00209      12  PF-VALUES           PIC  X          OCCURS 2 TIMES.
00210  EJECT
00211 *                            COPY EL634S.
       01  EL634AI.
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
           05  MAINTBYL PIC S9(0004) COMP.
           05  MAINTBYF PIC  X(0001).
           05  FILLER REDEFINES MAINTBYF.
               10  MAINTBYA PIC  X(0001).
           05  MAINTBYI PIC  X(0004).
      *    -------------------------------
           05  MAINTDTL PIC S9(0004) COMP.
           05  MAINTDTF PIC  X(0001).
           05  FILLER REDEFINES MAINTDTF.
               10  MAINTDTA PIC  X(0001).
           05  MAINTDTI PIC  X(0008).
      *    -------------------------------
           05  MAINTATL PIC S9(0004) COMP.
           05  MAINTATF PIC  X(0001).
           05  FILLER REDEFINES MAINTATF.
               10  MAINTATA PIC  X(0001).
           05  MAINTATI PIC  X(0005).
      *    -------------------------------
           05  CARHDGL PIC S9(0004) COMP.
           05  CARHDGF PIC  X(0001).
           05  FILLER REDEFINES CARHDGF.
               10  CARHDGA PIC  X(0001).
           05  CARHDGI PIC  X(0008).
      *    -------------------------------
           05  CARRIERL PIC S9(0004) COMP.
           05  CARRIERF PIC  X(0001).
           05  FILLER REDEFINES CARRIERF.
               10  CARRIERA PIC  X(0001).
           05  CARRIERI PIC  X(0001).
      *    -------------------------------
           05  GRPHDGL PIC S9(0004) COMP.
           05  GRPHDGF PIC  X(0001).
           05  FILLER REDEFINES GRPHDGF.
               10  GRPHDGA PIC  X(0001).
           05  GRPHDGI PIC  X(0009).
      *    -------------------------------
           05  GROUPL PIC S9(0004) COMP.
           05  GROUPF PIC  X(0001).
           05  FILLER REDEFINES GROUPF.
               10  GROUPA PIC  X(0001).
           05  GROUPI PIC  X(0006).
      *    -------------------------------
           05  STHDGL PIC S9(0004) COMP.
           05  STHDGF PIC  X(0001).
           05  FILLER REDEFINES STHDGF.
               10  STHDGA PIC  X(0001).
           05  STHDGI PIC  X(0006).
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
           05  RCOMPL PIC S9(0004) COMP.
           05  RCOMPF PIC  X(0001).
           05  FILLER REDEFINES RCOMPF.
               10  RCOMPA PIC  X(0001).
           05  RCOMPI PIC  X(0006).
      *    -------------------------------
           05  BENCDL PIC S9(0004) COMP.
           05  BENCDF PIC  X(0001).
           05  FILLER REDEFINES BENCDF.
               10  BENCDA PIC  X(0001).
           05  BENCDI PIC  X(0003).
      *    -------------------------------
           05  BENTYPL PIC S9(0004) COMP.
           05  BENTYPF PIC  X(0001).
           05  FILLER REDEFINES BENTYPF.
               10  BENTYPA PIC  X(0001).
           05  BENTYPI PIC  X(0001).
      *    -------------------------------
           05  MTHYRL PIC S9(0004) COMP.
           05  MTHYRF PIC  X(0001).
           05  FILLER REDEFINES MTHYRF.
               10  MTHYRA PIC  X(0001).
           05  MTHYRI.
               10  MTHI PIC  9(2).
               10  YRI PIC  9(2).
      *    -------------------------------
           05  EFFDTL PIC S9(0004) COMP.
           05  EFFDTF PIC  X(0001).
           05  FILLER REDEFINES EFFDTF.
               10  EFFDTA PIC  X(0001).
           05  EFFDTI PIC  X(0008).
      *    -------------------------------
           05  EXPDTL PIC S9(0004) COMP.
           05  EXPDTF PIC  X(0001).
           05  FILLER REDEFINES EXPDTF.
               10  EXPDTA PIC  X(0001).
           05  EXPDTI PIC  X(0008).
      *    -------------------------------
           05  LFHDGL PIC S9(0004) COMP.
           05  LFHDGF PIC  X(0001).
           05  FILLER REDEFINES LFHDGF.
               10  LFHDGA PIC  X(0001).
           05  LFHDGI PIC  X(0006).
      *    -------------------------------
           05  INFORCEL PIC S9(0004) COMP.
           05  INFORCEF PIC  X(0001).
           05  FILLER REDEFINES INFORCEF.
               10  INFORCEA PIC  X(0001).
           05  INFORCEI PIC  S9(9)V9(2).
      *    -------------------------------
           05  MORTAMTL PIC S9(0004) COMP.
           05  MORTAMTF PIC  X(0001).
           05  FILLER REDEFINES MORTAMTF.
               10  MORTAMTA PIC  X(0001).
           05  MORTAMTI PIC  S9(9)V9(2).
      *    -------------------------------
           05  EOMDTL PIC S9(0004) COMP.
           05  EOMDTF PIC  X(0001).
           05  FILLER REDEFINES EOMDTF.
               10  EOMDTA PIC  X(0001).
           05  EOMDTI PIC  X(0008).
      *    -------------------------------
           05  FUTUREL PIC S9(0004) COMP.
           05  FUTUREF PIC  X(0001).
           05  FILLER REDEFINES FUTUREF.
               10  FUTUREA PIC  X(0001).
           05  FUTUREI PIC  S9(9)V9(2).
      *    -------------------------------
           05  PTCL PIC S9(0004) COMP.
           05  PTCF PIC  X(0001).
           05  FILLER REDEFINES PTCF.
               10  PTCA PIC  X(0001).
           05  PTCI PIC  S9(9)V9(2).
      *    -------------------------------
           05  IBNRL PIC S9(0004) COMP.
           05  IBNRF PIC  X(0001).
           05  FILLER REDEFINES IBNRF.
               10  IBNRA PIC  X(0001).
           05  IBNRI PIC  S9(9)V9(2).
      *    -------------------------------
           05  CLAIML PIC S9(0004) COMP.
           05  CLAIMF PIC  X(0001).
           05  FILLER REDEFINES CLAIMF.
               10  CLAIMA PIC  X(0001).
           05  CLAIMI PIC  S9(9)V9(2).
      *    -------------------------------
           05  EXPL PIC S9(0004) COMP.
           05  EXPF PIC  X(0001).
           05  FILLER REDEFINES EXPF.
               10  EXPA PIC  X(0001).
           05  EXPI PIC  S9(9)V9(2).
      *    -------------------------------
           05  PYMNTL PIC S9(0004) COMP.
           05  PYMNTF PIC  X(0001).
           05  FILLER REDEFINES PYMNTF.
               10  PYMNTA PIC  X(0001).
           05  PYMNTI PIC  S9(9)V9(2).
      *    -------------------------------
           05  OCOMML PIC S9(0004) COMP.
           05  OCOMMF PIC  X(0001).
           05  FILLER REDEFINES OCOMMF.
               10  OCOMMA PIC  X(0001).
           05  OCOMMI PIC  S9(9)V9(2).
      *    -------------------------------
           05  RPREML PIC S9(0004) COMP.
           05  RPREMF PIC  X(0001).
           05  FILLER REDEFINES RPREMF.
               10  RPREMA PIC  X(0001).
           05  RPREMI PIC  S9(9)V9(2).
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0077).
      *    -------------------------------
           05  ERRMSG2L PIC S9(0004) COMP.
           05  ERRMSG2F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG2F.
               10  ERRMSG2A PIC  X(0001).
           05  ERRMSG2I PIC  X(0077).
      *    -------------------------------
           05  ERRMSG3L PIC S9(0004) COMP.
           05  ERRMSG3F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG3F.
               10  ERRMSG3A PIC  X(0001).
           05  ERRMSG3I PIC  X(0077).
      *    -------------------------------
           05  PFENTERL PIC S9(0004) COMP.
           05  PFENTERF PIC  X(0001).
           05  FILLER REDEFINES PFENTERF.
               10  PFENTERA PIC  X(0001).
           05  PFENTERI PIC  9(2).
       01  EL634AO REDEFINES EL634AI.
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
           05  MAINTBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTATO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARHDGO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRIERO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRPHDGO PIC  X(0009).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STHDGO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RCOMPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENCDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MTHYRO.
               10  MTHO PIC  X(0002).
               10  YRO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EFFDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXPDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFHDGO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INFORCEO PIC  Z(7).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MORTAMTO PIC  Z(7).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EOMDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FUTUREO PIC  Z(7).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PTCO PIC  Z(7).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  IBNRO PIC  Z(7).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLAIMO PIC  Z(7).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXPO PIC  Z(7).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PYMNTO PIC  Z(7).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OCOMMO PIC  Z(7).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RPREMO PIC  Z(7).99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0077).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG2O PIC  X(0077).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG3O PIC  X(0077).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  99.
      *    -------------------------------
00212  EJECT
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
00214
00215  01  DFHCOMMAREA             PIC  X(1024).
00216  EJECT
00217 *01 PARMLIST         COMP.
00218 *    12  FILLER              PIC S9(8).
00219 *    12  ELCNTL-POINTER      PIC S9(8).
00220 *    12  ERACCT-POINTER      PIC S9(8).
00221 *    12  ERREPY-POINTER      PIC S9(8).
00222 *    12  ERREIN-POINTER      PIC S9(8).
00223  EJECT
00224 *                                    COPY ELCCNTL.
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
00225  EJECT
00226 *                                    COPY ERCACCT.
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
090803     12  FILLER                            PIC X(5).
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
00227  EJECT
00228 *                                    COPY ERCREPY.
000010******************************************************************
000020*                                                                *
000020*                                                                *
000030*                            ERCREPY                             *
000031*           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
000040*                            VMOD=2.003                          *
000050*                                                                *
000060*   FILE DESCRIPTION = PENDING RETRO/EPEC PAYMENTS AND ADJUSTS   *
000070*                                                                *
000080*                                                                *
000090*   FILE TYPE = VSAM,KSDS                                        *
000100*   RECORD SIZE = 200  RECFORM = FIXED                           *
000110*                                                                *
000120*   BASE CLUSTER = ERREPY                         RKP=2,LEN=25   *
000130*       ALTERNATE PATHS = NONE                                   *
000140*                                                                *
000150*   LOG = YES                                                    *
000160*   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
000170******************************************************************
000180
000190 01  PENDING-RETRO-REIN-ADJUSTMENTS.
000200     12  RP-RECORD-ID                     PIC XX.
000210         88  VALID-RP-ID                        VALUE 'RP'.
000220
000230     12  RP-CONTROL-PRIMARY.
000240         16  RP-COMPANY-CD                PIC X.
000250         16  RP-CARRIER                   PIC X.
000260         16  RP-GROUPING.
000270             20  RP-GROUPING-PREFIX       PIC XXX.
000280             20  RP-GROUPING-PRIME        PIC XXX.
000290         16  RP-STATE                     PIC XX.
000300         16  RP-ACCOUNT.
000310             20  RP-ACCOUNT-PREFIX        PIC X(4).
000320             20  RP-ACCOUNT-PRIME         PIC X(6).
000330         16  RP-FILE-SEQ-NO               PIC S9(8)     COMP.
000340         16  RP-RECORD-TYPE               PIC X.
000350             88  EPEC-ADJUSTMENT                VALUE '1'.
000360
000370     12  RP-LAST-MAINT-DT                 PIC XX.
000380     12  RP-LAST-MAINT-HHMMSS             PIC S9(7)     COMP-3.
000390     12  RP-LAST-MAINT-BY                 PIC X(4).
000400
000410     12  RP-PYADJ-RECORD.
000420         16  RP-REIN-COMP-NO.
000430             20  RP-REIN-COMP-PRIME       PIC XXX.
000440             20  RP-REIN-COMP-SUFFIX      PIC XXX.
000450         16  RP-BENEFIT-CD                PIC XX.
000460         16  RP-BENEFIT-TYPE              PIC X.
CIDVAO*10/13/98
CIDVAO         16  RP-EPEC-ADJ-DT.
CIDVAO             20  RP-ADJ-MO                PIC 99.
CIDVAO             20  RP-ADJ-YR                PIC 99.
CIDVAO*
CIDVAO****     16  RP-EPEC-ADJ-DATE.
CIDVAO****         20  RP-EPEC-ADJ-MO           PIC 99.
CIDVAO****         20  RP-EPEC-ADJ-YR           PIC 99.
CIDVAO****     16  RP-EPEC-ADJ-DT  REDEFINES
CIDVAO****         RP-EPEC-ADJ-DATE             PIC 9(7)        COMP-3.
000500         16  RP-ACCOUNT-EFF-DT            PIC XX.
000510         16  RP-ACCOUNT-EXP-DT            PIC XX.
000520
000530         16  RP-ADJUSTMENT-INPUTS.
000540             20  RP-INS-AMT-INFORCE       PIC S9(9)V99    COMP-3.
000550             20  RP-LIFE-MORTALITY-AMT    PIC S9(9)V99    COMP-3.
000560             20  FILLER                   PIC X(7).
000570             20  RP-FUTURE-RESERVE        PIC S9(7)V99    COMP-3.
000580             20  RP-PTC-RESERVE           PIC S9(7)V99    COMP-3.
000590             20  RP-IBNR-RESERVE          PIC S9(7)V99    COMP-3.
000600             20  RP-CLAIM-ADJ-AMT         PIC S9(7)V99    COMP-3.
000610             20  RP-EXPENSES              PIC S9(7)V99    COMP-3.
000620             20  RP-PAYMENTS              PIC S9(7)V99    COMP-3.
000630             20  RP-OTHER-COMM            PIC S9(7)V99    COMP-3.
000640             20  RP-REIN-PREM-ADJ         PIC S9(7)V99    COMP-3.
000650             20  FILLER                   PIC X(50).
000660
000670         16  RP-SV-CARRIER                PIC X.
000680         16  RP-SV-GROUPING.
000690             20  RP-SV-GROUPING-PREFIX    PIC XXX.
000700             20  RP-SV-GROUPING-PRIME     PIC XXX.
000710         16  RP-SV-STATE                  PIC XX.
000720         16  RP-EFF-DATE-ENTERED          PIC XX.
000730         16  FILLER                       PIC X(12).
000740
000750     12  RP-RECORD-STATUS.
000760         16  RP-CREDIT-SELECT-DT          PIC XX.
000770         16  RP-CREDIT-ACCEPT-DT          PIC XX.
000780         16  RP-INPUT-DT                  PIC XX.
000790         16  FILLER                       PIC X(8).
000800******************************************************************
00229  EJECT
00230 *                                    COPY ERCREIN.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ERCREIN                             *
00003 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00004 *                            VMOD=2.010                          *
00005 *                                                                *
00006 *   ONLINE CREDIT SYSTEM                                         *
00007 *                                                                *
00008 *   FILE DESCRIPTION = REINSURANCE MASTER FILE                   *
00009 *                                                                *
00010 *   FILE TYPE = VSAM,KSDS                                        *
00011 *   RECORD SIZE = 4000  RECFORM = FIXED                          *
00012 *                                                                *
00013 *   BASE CLUSTER NAME = ERREIN                   RKP=2,LEN=8     *
00014 *       ALTERNATE PATH = NONE                                    *
00015 *                                                                *
00016 *   LOG = NO                                                     *
00017 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00018 *                                                                *
00019 ******************************************************************
103101*                   C H A N G E   L O G
103101*
103101* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
103101*-----------------------------------------------------------------
103101*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
103101* EFFECTIVE    NUMBER
103101*-----------------------------------------------------------------
103101* 103101    2001100100006  SMVA  ADD STATE EXHIBIT REPORT OPTION F
032707* 032707    2007032100006  PEMA  ADD EXCISE TAX CAPABILITY
103101******************************************************************
00021  01  REINSURANCE-RECORD.
00022      12  RE-RECORD-ID                      PIC XX.
00023          88  VALID-RE-ID                      VALUE 'RE'.
00024
00025      12  RE-CONTROL-PRIMARY.
00026          16  RE-COMPANY-CD                 PIC X.
00027          16  RE-KEY.
00028              20  RE-CODE                   PIC X.
00029                  88  RE-TABLE-RECORD          VALUE 'A'.
00030                  88  RE-COMPANY-RECORD        VALUE 'B'.
00031              20  RE-TABLE                  PIC XXX.
00032              20  FILLER                    PIC XXX.
00033          16  RE-COMPANY-KEY REDEFINES RE-KEY.
00034              20  FILLER                    PIC X.
00035              20  RE-COMPANY.
00036                  24  RE-COMP-PRIME         PIC XXX.
00037                  24  RE-COMP-SUB           PIC XXX.
00038
00039      12  RE-MAINT-INFORMATION.
00040          16  RE-LAST-MAINT-DT              PIC XX.
00041          16  RE-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
00042          16  RE-LAST-MAINT-USER            PIC X(4).
00043          16  FILLER                        PIC X(10).
00044
00045      12  RE-TABLE-DATA.
00046          16  RE-100-COMP                   PIC 99.
00047
00048          16  RE-COMP-INFO    OCCURS 30 TIMES.
00049              20  RE-REI-COMP-NO.
00050                  24  RE-REI-COMP           PIC XXX.
00051                  24  RE-REI-COMP-SUB       PIC XXX.
00052              20  RE-LF-QC                  PIC X.
00053              20  RE-AH-QC                  PIC X.
00054              20  RE-LO-DATE                PIC 9(11)     COMP-3.
00055              20  RE-HI-DATE                PIC 9(11)     COMP-3.
00056              20  RE-LFAGE-LO               PIC 99.
00057              20  RE-LFAGE-HI               PIC 99.
00058              20  RE-AHAGE-LO               PIC 99.
00059              20  RE-AHAGE-HI               PIC 99.
00060              20  RE-LFTRM-LO               PIC S999       COMP-3.
00061              20  RE-LFTRM-HI               PIC S999       COMP-3.
00062              20  RE-AHTRM-LO               PIC S999       COMP-3.
00063              20  RE-AHTRM-HI               PIC S999       COMP-3.
00064              20  RE-LF-PCT                 PIC S9V9999    COMP-3.
00065              20  RE-AH-PCT                 PIC S9V9999    COMP-3.
00066              20  RE-LF-LIM-LO              PIC S9(9)V99   COMP-3.
00067              20  RE-LF-LIM-HI              PIC S9(9)V99   COMP-3.
00068              20  RE-LF-LO                  PIC S9(9)V99   COMP-3.
00069              20  RE-LF-HI                  PIC S9(9)V99   COMP-3.
00070              20  RE-AHBEN-LIM-LO           PIC S9(7)V99   COMP-3.
00071              20  RE-AHBEN-LIM-HI           PIC S9(7)V99   COMP-3.
00072              20  RE-AHBEN-LO               PIC S9(7)V99   COMP-3.
00073              20  RE-AHBEN-HI               PIC S9(7)V99   COMP-3.
00074              20  RE-AHMOA-LIM-LO           PIC S9(7)V99   COMP-3.
00075              20  RE-AHMOA-LIM-HI           PIC S9(7)V99   COMP-3.
00076              20  RE-AHMOA-LO               PIC S9(7)V99   COMP-3.
00077              20  RE-AHMOA-HI               PIC S9(7)V99   COMP-3.
00078              20  RE-LF-BEN-CODE            PIC X.
00079              20  RE-AH-BEN-CODE            PIC X.
00080              20  RE-INTERACTIVE            PIC X.
00081              20  RE-REMAINING              PIC X.
CIDMOD             20  RE-LF-RUNOFF-SW           PIC X.
CIDMOD             20  RE-AH-RUNOFF-SW           PIC X.
CIDMOD             20  FILLER                    PIC X(19).
00083
00084          16  RE-COMP-INFO-END              PIC X(6).
00085          16  RE-NSP-ST-CD-LF               PIC XX.
00086          16  RE-NSP-ST-CD-AH               PIC XX.
00087          16  RE-TABLE-CARRIER-SECURITY     PIC X.
00088              88  NO-TABLE-CARRIER-SECURITY    VALUE SPACE.
00089
00090          16  FILLER                        PIC X(27).
00091
00092      12  RE-COMPANY-DATA   REDEFINES   RE-TABLE-DATA.
00093          16  RE-NAME                       PIC X(30).
00094          16  RE-LF-PE                      PIC X.
00095          16  RE-AH-PE                      PIC X.
00096          16  RE-LF-FEE                     PIC S9V9999    COMP-3.
00097          16  RE-AH-FEE                     PIC S9V9999    COMP-3.
00098          16  RE-AH-PR-PCT                  PIC S9V9999    COMP-3.
00099          16  RE-AH-78-PCT                  PIC S9V9999    COMP-3.
00100          16  RE-PRT-ST                     PIC X.
00101          16  RE-PRT-OW                     PIC X.
00102          16  RE-MORT-CODE                  PIC X(4).
00103          16  RE-CLAIM-CODE                 PIC X.
00104          16  RE-ZERO-LF-FEE                PIC X.
00105          16  RE-ZERO-AH-FEE                PIC X.
00106          16  RE-CEDE-NAME                  PIC X(30).
00107          16  RE-LF-COMM                    PIC X.
00108          16  RE-AH-COMM                    PIC X.
00109          16  RE-LF-TAX                     PIC X.
00110          16  RE-AH-TAX                     PIC X.
00111          16  RE-CLM-INCURRED-LIM           PIC 9(11)  COMP-3.
00116          16  RE-LF-IBNR-PCT                PIC SV999      COMP-3.
00117          16  RE-AH-IBNR-PCT                PIC SV999      COMP-3.
00118
00119          16  RE-COMP-CARRIER-SECURITY      PIC X.
00120              88  NO-COMP-CARRIER-SECURITY     VALUE SPACE.
00121
00122          16  RE-LF-CEDING-FEE-BRACKETS.
00123              20  RE-LF-FEE-METHOD          PIC X.
00124                  88  RE-LF-FEE-BRACKETED         VALUE '1' '2'.
00125                  88  RE-LF-FEE-METHOD-1          VALUE '1'.
00126                  88  RE-LF-FEE-METHOD-2          VALUE '2'.
00127                  88  RE-LF-FEE-PERCENT           VALUE ' ' 'P'.
00128              20  RE-LF-FEE-BASIS           PIC X.
00129                  88  RE-LF-GROSS-CEDED             VALUE '1'.
00130                  88  RE-LF-NET-CEDED               VALUE '2'.
00131                  88  RE-LF-GROSS-WRITTEN           VALUE '3'.
00132                  88  RE-LF-NET-WRITTEN             VALUE '4'.
00133                  88  RE-LF-COMBINE-GROSS-CEDED     VALUE '5'.
00134                  88  RE-LF-COMBINE-NET-CEDED       VALUE '6'.
00135                  88  RE-LF-COMBINE-GROSS-WRITTEN   VALUE '7'.
00136                  88  RE-LF-COMBINE-NET-WRITTEN     VALUE '8'.
00137              20  FILLER                    PIC XXX.
00138              20  RE-LF-FEE-RANGES  OCCURS 6 TIMES.
00139                  24  RE-LF-FEE-RANGE-PCT   PIC S9V9999    COMP-3.
00140                  24  RE-LF-FEE-THRU-AMT    PIC S9(7)V99   COMP-3.
00141
00142          16  RE-AH-CEDING-FEE-BRACKETS.
00143              20  RE-AH-FEE-METHOD          PIC X.
00144                  88  RE-AH-FEE-BRACKETED         VALUE '1' '2'.
00145                  88  RE-AH-FEE-METHOD-1          VALUE '1'.
00146                  88  RE-AH-FEE-METHOD-2          VALUE '2'.
00147                  88  RE-AH-FEE-PERCENT           VALUE ' ' 'P'.
00148              20  RE-AH-FEE-BASIS           PIC X.
00149                  88  RE-AH-GROSS-CEDED             VALUE '1'.
00150                  88  RE-AH-NET-CEDED               VALUE '2'.
00151                  88  RE-AH-GROSS-WRITTEN           VALUE '3'.
00152                  88  RE-AH-NET-WRITTEN             VALUE '4'.
00153                  88  RE-AH-COMBINE-GROSS-CEDED     VALUE '5'.
00154                  88  RE-AH-COMBINE-NET-CEDED       VALUE '6'.
00155                  88  RE-AH-COMBINE-GROSS-WRITTEN   VALUE '7'.
00156                  88  RE-AH-COMBINE-NET-WRITTEN     VALUE '8'.
00157              20  FILLER                    PIC XXX.
00158              20  RE-AH-FEE-RANGES  OCCURS 6 TIMES.
00159                  24  RE-AH-FEE-RANGE-PCT   PIC S9V9999    COMP-3.
00160                  24  RE-AH-FEE-THRU-AMT    PIC S9(7)V99   COMP-3.
00161
00162          16  RE-EARNING-START-DT           PIC 9(11)  COMP-3.
00166
00167          16  RE-OLD-CEDING-STMT            PIC X.
00168
00169          16  RE-LF-CLM-PCT                 PIC S9V9999    COMP-3.
00170          16  RE-AH-CLM-PCT                 PIC S9V9999    COMP-3.
00171          16  RE-LF-CLM-MAX                 PIC S9(7)V99   COMP-3.
00172          16  RE-AH-CLM-MAX                 PIC S9(7)V99   COMP-3.
00173          16  RE-LF-PR-PCT                  PIC S9V9999    COMP-3.
00174          16  RE-LF-78-PCT                  PIC S9V9999    COMP-3.
00175          16  RE-REINS-GROUPING-CODE        PIC X(6).
00176          16  RE-MORT-SW                    PIC X.
00177          16  RE-CEDING-TYPE-FLAG           PIC X.
00178              88  RE-NO-CESSION-TYPE                VALUE ' '.
00179              88  RE-CEDED                          VALUE 'C'.
00180              88  RE-ASSUMED                        VALUE 'A'.
00181              88  RE-PHANTOM                        VALUE 'P'.
00182
00183          16  RE-CEDING-STMT-OPT-A          PIC X.
00184              88  REPORT-A-WANTED    VALUE ' ' 'Y'.
00185          16  RE-CEDING-STMT-OPT-B          PIC X.
00186              88  REPORT-B-WANTED    VALUE ' ' 'Y'.
00187          16  RE-CEDING-STMT-OPT-C          PIC X.
00188              88  REPORT-C-WANTED    VALUE ' ' 'Y'.
00189          16  RE-CEDING-STMT-OPT-D          PIC X.
00190              88  REPORT-D-WANTED    VALUE ' ' 'Y'.
00191          16  RE-CEDING-STMT-OPT-E          PIC X.
00192              88  REPORT-E-WANTED    VALUE ' ' 'Y'.
00193
00194          16  RE-PRT-CRSV                   PIC X.
00195
00196          16  RE-GL-CENTER                  PIC X(4).
00197
00198          16  RE-CUSTODIAL-BAL              PIC S9(7)V99   COMP-3.
00199
00200          16  RE-EARNING-STOP-DT            PIC 9(11)  COMP-3.
00204
00205          16  RE-EARN-STOP-CODE             PIC X.
00206              88  STOP-LIFE-EARNING  VALUE 'L' 'B'.
00207              88  STOP-AH-EARNING    VALUE 'A' 'B'.
00208
103101         16  RE-STATE-EXHIBIT-OPT-F        PIC X.
103101             88  RPTF-ECS152-WANTED VALUE ' ' 'Y'.
103101
032707         16  RE-EXCISE-TAX                 PIC S9V9999 COMP-3.
032707         16  FILLER                        PIC X(2281).
00210
00211          16  RE-DESC OCCURS 18 TIMES       PIC X(79).
00212
00213 ******************************************************************
00231  EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CONTROL-FILE
                                ACCOUNT-MASTER
                                PENDING-RETRO-REIN-ADJUSTMENTS
                                REINSURANCE-RECORD.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL634' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00233
00234      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00235      MOVE PI-LIFE-OVERRIDE-L6    TO  EMI-LIFE-OVERRIDE-L6.
00236      MOVE PI-AH-OVERRIDE-L6      TO  EMI-AH-OVERRIDE-L6.
00237
00238      IF EIBCALEN = ZERO
00239          GO TO 8800-UNAUTHORIZED-ACCESS.
00240
00241      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00242      MOVE '5'                    TO  DC-OPTION-CODE.
00243
00244      PERFORM 8500-DATE-CONVERT  THRU  8599-EXIT.
00245
00246      MOVE DC-BIN-DATE-1          TO  WS-CURRENT-BIN-DT.
00247      MOVE DC-GREG-DATE-1-EDIT    TO  WS-CURRENT-DT.
00248      MOVE 3                      TO  EMI-NUMBER-OF-LINES.
00249      MOVE 2                      TO  EMI-SWITCH2.
00250
00251      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00252          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00253              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6
00254              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5
00255              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4
00256              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3
00257              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2
00258              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1
00259              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM
00260              MOVE THIS-PGM              TO  PI-CALLING-PROGRAM
00261          ELSE
00262              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM
00263              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM
00264              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1
00265              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2
00266              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3
00267              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4
00268              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5
00269              MOVE SPACES                TO  PI-SAVED-PROGRAM-6.
00270  EJECT
00271      MOVE LOW-VALUES             TO  EL634AI.
00272
00273      IF EIBTRNID NOT = TRANS-ID
00274          GO TO 8100-SEND-INITIAL-MAP.
00275
00276      COMPUTE PI-WORK-SEQ-NO  =  EIBTIME  *  10.
00277
00278      MOVE PI-COMPANY-CD          TO  REPY-COMPANY-CD
00279                                      PI-REPY-COMP-CD.
00280
00281      
      * EXEC CICS HANDLE CONDITION
00282 *        PGMIDERR  (9600-PGMID-ERROR)
00283 *        ERROR     (9999-ABEND)
00284 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00003673' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303033363733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00285
00286      IF EIBAID = DFHCLEAR
00287          GO TO 9400-CLEAR.
00288
00289      IF PI-PROCESSOR-ID = 'LGXX'
00290          GO TO 0200-RECEIVE.
00291
00292      
      * EXEC CICS READQ TS
00293 *        QUEUE  (PI-SECURITY-TEMP-STORE-ID)
00294 *        INTO   (SECURITY-CONTROL)
00295 *        LENGTH (SC-COMM-LENGTH)
00296 *        ITEM   (SC-ITEM)
00297 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00003684' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00298
00299      MOVE SC-CREDIT-DISPLAY (16)  TO PI-DISPLAY-CAP.
00300      MOVE SC-CREDIT-UPDATE  (16)  TO PI-MODIFY-CAP.
00301
00302      IF NOT DISPLAY-CAP
00303          MOVE 'READ'          TO SM-READ
00304          PERFORM 9995-SECURITY-VIOLATION
00305          MOVE ER-0070         TO  EMI-ERROR
00306          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00307          GO TO 8100-SEND-INITIAL-MAP.
00308
00309  EJECT
00310  0200-RECEIVE.
00311      IF EIBAID = DFHPA1 OR  DFHPA2  OR  DFHPA3
00312          MOVE ER-0008            TO  EMI-ERROR
00313          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00314          MOVE -1                 TO  PFENTERL
00315          GO TO 8200-SEND-DATAONLY.
00316
00317      
      * EXEC CICS RECEIVE
00318 *        MAP     (EL634A)
00319 *        MAPSET  (MAPSET-NAME)
00320 *        INTO    (EL634AI)
00321 *    END-EXEC.
           MOVE LENGTH OF
            EL634AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00003709' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL634A, 
                 EL634AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00322
00323      IF PFENTERL GREATER ZERO
00324          IF EIBAID NOT = DFHENTER
00325              MOVE ER-0004                   TO  EMI-ERROR
00326              MOVE AL-UNBOF                  TO  PFENTERA
00327              MOVE -1                        TO  PFENTERL
00328              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00329              GO TO 8200-SEND-DATAONLY
00330          ELSE
00331              IF PFENTERI NUMERIC
00332                AND PFENTERI GREATER ZERO
00333                AND PFENTERI LESS 25
00334                  MOVE PF-VALUES (PFENTERI)  TO  EIBAID
00335              ELSE
00336                  MOVE ER-0029               TO  EMI-ERROR
00337                  MOVE AL-UNBOF              TO  PFENTERA
00338                  MOVE -1                    TO  PFENTERL
00339                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00340                  GO TO 8200-SEND-DATAONLY.
00341  EJECT
00342  0300-CHECK-PFKEYS.
00343      IF EIBAID = DFHPF23
00344          GO TO 8900-PF23.
00345
00346      IF EIBAID = DFHPF24
00347          GO TO 9200-RETURN-MAIN-MENU.
00348
00349      IF EIBAID = DFHPF12
00350          GO TO 9500-PF12.
00351
00352      IF EIBAID = DFHPF1              OR  DFHPF2
00353          GO TO 5000-BROWSE-FILE.
00354
00355      IF EIBAID = DFHENTER
00356          GO TO 1000-EDIT-DATA.
00357
00358      MOVE ER-0029                TO  EMI-ERROR.
00359
00360  0310-INPUT-ERROR.
00361      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00362
00363      MOVE -1                     TO  PFENTERL
00364
00365      GO TO 8200-SEND-DATAONLY.
00366  EJECT
00367  1000-EDIT-DATA.
00368      IF MAINTI = 'S' OR  'C'  OR  'A'  OR  'D'
00369          MOVE AL-UANON           TO  MAINTA
00370          IF (MAINTI = 'C' OR  'D')
00371              IF PI-PREV-MAINT = 'S'
00372                  NEXT SENTENCE
00373              ELSE
00374                  MOVE ER-2056    TO  EMI-ERROR
00375                  MOVE -1         TO  MAINTL
00376                  MOVE AL-UABON   TO  MAINTA
00377                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00378                  GO TO 1010-EDIT-COMPLETE
00379          ELSE
00380              NEXT SENTENCE
00381      ELSE
00382          MOVE ER-0023            TO  EMI-ERROR
00383          MOVE -1                 TO  MAINTL
00384          MOVE AL-UABON           TO  MAINTA
00385          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00386
00387      IF MODIFY-CAP
00388          NEXT SENTENCE
00389        ELSE
00390      IF MAINTI NOT = 'S'
00391          MOVE 'UPDATE'       TO SM-READ
00392          PERFORM 9995-SECURITY-VIOLATION
00393          MOVE ER-0070        TO EMI-ERROR
00394          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00395          GO TO 8100-SEND-INITIAL-MAP.
00396
00397      IF CARRIERI NOT = LOW-VALUES
00398          MOVE AL-UANON           TO  CARRIERA
00399          PERFORM 1200-VERIFY-CARRIER-ID  THRU  1299-EXIT
00400          MOVE CARRIERI           TO  PI-REPY-CARRIER
00401      ELSE
00402          IF NOT  ST-ACCNT-CNTL  AND  NOT  ACCNT-CNTL
00403              MOVE -1             TO  CARRIERL
00404              MOVE AL-UABON       TO  CARRIERA
00405              MOVE ER-0194        TO  EMI-ERROR
00406              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00407
00408      IF  GROUPI NOT = LOW-VALUES
00409          MOVE AL-UANON           TO  GROUPA
00410          MOVE GROUPI             TO  PI-REPY-GROUPING
00411      ELSE
00412          IF CARR-GROUP-ST-ACCNT-CNTL
00413              MOVE -1             TO  GROUPL
00414              MOVE AL-UABON       TO  GROUPA
00415              MOVE ER-0195        TO  EMI-ERROR
00416              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00417
00418      IF STATEI NOT = LOW-VALUES
00419          MOVE AL-UANON           TO  STATEA
00420          PERFORM 1300-VERIFY-STATE-ID  THRU  1399-EXIT
00421          MOVE STATEI             TO  PI-REPY-STATE
00422      ELSE
00423          IF NOT  ACCNT-CNTL  AND  NOT  CARR-ACCNT-CNTL
00424              MOVE -1             TO  STATEL
00425              MOVE AL-UABON       TO  STATEA
00426              MOVE ER-0196        TO  EMI-ERROR
00427              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00428
00429      IF ACCTI NOT = LOW-VALUES
00430          MOVE AL-UANON           TO  ACCTA
00431          PERFORM 1400-VERIFY-ACCOUNT  THRU  1499-EXIT
00432          MOVE ACCTI              TO  PI-REPY-ACCOUNT
00433      ELSE
00434          MOVE -1                 TO  ACCTL
00435          MOVE AL-UABON           TO  ACCTA
00436          MOVE ER-0197            TO  EMI-ERROR
00437          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00438
00439      IF EMI-ERROR = ZEROS
00440          PERFORM 1600-VALID-EFF-DATE  THRU  1699-EXIT.
00441
00442      IF MAINTI = 'S' OR  'D'
00443          GO TO 1010-EDIT-COMPLETE.
00444
00445      IF RCOMPI NOT = LOW-VALUES
00446          MOVE AL-UANON           TO  RCOMPA
00447          PERFORM 1100-VERIFY-REIN-COMP  THRU  1199-EXIT.
00448
00449      IF BENCDI NOT = LOW-VALUES
00450          MOVE AL-UANON           TO  BENCDA
00451      ELSE
00452          MOVE ER-0026            TO  EMI-ERROR
00453          MOVE -1                 TO  BENCDL
00454          MOVE AL-UABON           TO  BENCDA
00455          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00456
00457      IF BENTYPI NOT = LOW-VALUES
00458          IF BENTYPI = PI-AH-OVERRIDE-L1
00459            OR  BENTYPI = PI-LIFE-OVERRIDE-L1
00460              MOVE AL-UANON       TO  BENTYPA
00461          ELSE
00462              MOVE ER-2580        TO  EMI-ERROR
00463              MOVE -1             TO  BENTYPL
00464              MOVE AL-UABON       TO  BENTYPA
00465              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00466      ELSE
00467          MOVE ER-2575            TO  EMI-ERROR
00468          MOVE -1                 TO  BENTYPL
00469          MOVE AL-UABON           TO  BENTYPA
00470          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00471
00472      IF BENCDI NOT = LOW-VALUES
00473        AND  BENTYPI = PI-AH-OVERRIDE-L1
00474            OR  BENTYPI = PI-LIFE-OVERRIDE-L1
00475          PERFORM 1500-VERIFY-CODES  THRU  1599-EXIT.
00476  EJECT
00477      IF MTHYRI = LOW-VALUES
00478          MOVE ER-2581               TO  EMI-ERROR
00479          MOVE -1                    TO  MTHYRL
00480          MOVE AL-UNBON              TO  MTHYRA
00481          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00482      ELSE
00483          IF MTHYRI NUMERIC
00484              IF MTHI GREATER ZERO
00485                AND LESS 13
CIDMOD*                IF YRI GREATER ZEROS
CIDMOD                 IF YRI NOT < ZEROS
00487                      MOVE AL-UNNON  TO  MTHYRA
00488                  ELSE
00489                      MOVE ER-2578   TO  EMI-ERROR
00490                      MOVE -1        TO  MTHYRL
00491                      MOVE AL-UNBON  TO  MTHYRA
00492                      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00493              ELSE
00494                  MOVE ER-2576       TO  EMI-ERROR
00495                  MOVE -1            TO  MTHYRL
00496                  MOVE AL-UNBON      TO  MTHYRA
00497                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00498          ELSE
00499              MOVE ER-2577           TO  EMI-ERROR
00500              MOVE -1                TO  MTHYRL
00501              MOVE AL-UNBON          TO  MTHYRA
00502              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00503
00504      MOVE PI-CR-MONTH-END-DT        TO DC-BIN-DATE-1.
00505      MOVE ' '                       TO DC-OPTION-CODE.
00506
00507      PERFORM 8500-DATE-CONVERT     THRU   8599-EXIT.
00508
00509      MOVE DC-GREG-DATE-1-MDY         TO  WS-PI-END-DATE.
00510
00511      MOVE WS-PI-END-MO               TO  WS-END-MO1.
00512      MOVE WS-PI-END-YR               TO  WS-END-YR1.
00513      MOVE MTHI                       TO  WS-END-MO2.
00514      MOVE YRI                        TO  WS-END-YR2.
           IF WS-END-YR1 > 80
              MOVE 19                  TO WS-END-CENT1
           END-IF
           IF WS-END-YR2 > 80
              MOVE 19                  TO WS-END-CENT2
           END-IF
00516      IF WS-END-DATE1 < WS-END-DATE2
00517          MOVE ER-2582                TO  EMI-ERROR
00518          MOVE -1                     TO  MTHYRL
00519          MOVE AL-UNBON               TO  MTHYRA
00520          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00521
00522      IF EOMDTL IS NOT EQUAL TO ZEROS
00523          MOVE AL-UANON           TO  EOMDTA
00524          MOVE EOMDTI             TO  WS-DEEDIT-DATE
00525          PERFORM 8700-DEEDIT-DATE THRU 8799-EXIT
00526          MOVE WS-DEEDIT-DATE     TO  EOMDTI.
00527
00528 *    IF INFORCEL NOT = ZEROS
00529 *        MOVE AL-UNNON           TO  INFORCEA
CIDMOD*        MOVE INFORCEI           TO  WS-DEEDIT-FIELD
CIDMOD*        PERFORM 8600-DEEDIT  THRU  8699-EXIT
CIDMOD*        MOVE WS-DEEDIT-FIELD    TO  WS-INFORCEI.
00535
00536 *    IF MORTAMTL NOT = ZEROS
00537 *        MOVE AL-UNNON           TO  MORTAMTA
CIDMOD*        MOVE MORTAMTI           TO  WS-DEEDIT-FIELD
CIDMOD*        PERFORM 8600-DEEDIT  THRU  8699-EXIT
CIDMOD*        MOVE WS-DEEDIT-FIELD    TO  WS-MORTAMTI.
00543
00544 *    IF FUTUREL NOT = ZEROS
00545 *        MOVE AL-UNNON           TO  FUTUREA
CIDMOD*        MOVE FUTUREI            TO  WS-DEEDIT-FIELD
CIDMOD*        PERFORM 8600-DEEDIT  THRU  8699-EXIT
CIDMOD*        MOVE WS-DEEDIT-FIELD    TO  WS-FUTUREI.
00551
00552 *    IF PTCL NOT = ZEROS
00553 *        MOVE AL-UNNON           TO  PTCA
CIDMOD*        MOVE PTCI               TO  WS-DEEDIT-FIELD
CIDMOD*        PERFORM 8600-DEEDIT  THRU  8699-EXIT
CIDMOD*        MOVE WS-DEEDIT-FIELD    TO  WS-PTCI.
00559
00560 *    IF IBNRL NOT = ZEROS
00561 *        MOVE AL-UNNON           TO  IBNRA
CIDMOD*        MOVE IBNRI              TO  WS-DEEDIT-FIELD
CIDMOD*        PERFORM 8600-DEEDIT  THRU  8699-EXIT
CIDMOD*        MOVE WS-DEEDIT-FIELD    TO  WS-IBNRI.
00567
00568 *    IF CLAIML NOT = ZEROS
00569 *        MOVE AL-UNNON           TO  CLAIMA
CIDMOD*        MOVE CLAIMI             TO  WS-DEEDIT-FIELD
CIDMOD*        PERFORM 8600-DEEDIT  THRU  8699-EXIT
CIDMOD*        MOVE WS-DEEDIT-FIELD    TO  WS-CLAIMI.
00575
00576 *    IF EXPL NOT = ZEROS
00577 *        MOVE AL-UNNON           TO  EXPA
CIDMOD*        MOVE EXPI               TO  WS-DEEDIT-FIELD
CIDMOD*        PERFORM 8600-DEEDIT  THRU  8699-EXIT
CIDMOD*        MOVE WS-DEEDIT-FIELD    TO  WS-EXPI.
00583
00528      IF INFORCEL NOT = ZEROS
00529          MOVE AL-UNNON           TO  INFORCEA
00530          
      * EXEC CICS BIF DEEDIT
00531 *            FIELD   (INFORCEI)
00532 *            LENGTH  (11)
00533 *        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00003970' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 INFORCEI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00534          MOVE INFORCEI           TO  WS-INFORCEI.
00535
00536      IF MORTAMTL NOT = ZEROS
00537          MOVE AL-UNNON           TO  MORTAMTA
00538          
      * EXEC CICS BIF DEEDIT
00539 *            FIELD   (MORTAMTI)
00540 *            LENGTH  (11)
00541 *        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00003978' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MORTAMTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00542          MOVE MORTAMTI           TO  WS-MORTAMTI.
00543
00544      IF FUTUREL NOT = ZEROS
00545          MOVE AL-UNNON           TO  FUTUREA
00546          
      * EXEC CICS BIF DEEDIT
00547 *            FIELD   (FUTUREI)
00548 *            LENGTH  (11)
00549 *        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00003986' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FUTUREI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00550          MOVE FUTUREI            TO  WS-FUTUREI.
00551
00552      IF PTCL NOT = ZEROS
00553          MOVE AL-UNNON           TO  PTCA
00554          
      * EXEC CICS BIF DEEDIT
00555 *            FIELD   (PTCI)
00556 *            LENGTH  (11)
00557 *        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00003994' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PTCI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00558          MOVE PTCI               TO  WS-PTCI.
00559
00560      IF IBNRL NOT = ZEROS
00561          MOVE AL-UNNON           TO  IBNRA
00562          
      * EXEC CICS BIF DEEDIT
00563 *            FIELD   (IBNRI)
00564 *            LENGTH  (11)
00565 *        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004002' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 IBNRI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00566          MOVE IBNRI              TO  WS-IBNRI.
00567
00568      IF CLAIML NOT = ZEROS
00569          MOVE AL-UNNON           TO  CLAIMA
00570          
      * EXEC CICS BIF DEEDIT
00571 *            FIELD   (CLAIMI)
00572 *            LENGTH  (11)
00573 *        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004010' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLAIMI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00574          MOVE CLAIMI             TO  WS-CLAIMI.
00575
00576      IF EXPL NOT = ZEROS
00577          MOVE AL-UNNON           TO  EXPA
00578          
      * EXEC CICS BIF DEEDIT
00579 *            FIELD   (EXPI)
00580 *            LENGTH  (11)
00581 *        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004018' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EXPI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00582          MOVE EXPI               TO  WS-EXPI.
00583
00584      IF PYMNTL NOT = ZEROS
00585          MOVE AL-UNNON           TO  PYMNTA
00586          
      * EXEC CICS BIF DEEDIT
00587 *            FIELD   (PYMNTI)
00588 *            LENGTH  (11)
00589 *        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004026' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYMNTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00590          MOVE PYMNTI             TO  WS-PYMNTI.
00584 *    IF PYMNTL NOT = ZEROS
00585 *        MOVE AL-UNNON           TO  PYMNTA
CIDMOD*        MOVE PYMNTI             TO  WS-DEEDIT-FIELD
CIDMOD*        PERFORM 8600-DEEDIT  THRU  8699-EXIT
CIDMOD*        MOVE WS-DEEDIT-FIELD    TO  WS-PYMNTI.
00591  EJECT
00592      IF OCOMML NOT = ZEROS
00593          MOVE AL-UNNON           TO  OCOMMA
00594          
      * EXEC CICS BIF DEEDIT
00595 *            FIELD   (OCOMMI)
00596 *            LENGTH  (11)
00597 *        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004039' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 OCOMMI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00598          MOVE OCOMMI             TO  WS-OCOMMI.
00599
00592 *    IF OCOMML NOT = ZEROS
00593 *        MOVE AL-UNNON           TO  OCOMMA
CIDMOD*        MOVE OCOMMI             TO  WS-DEEDIT-FIELD
CIDMOD*        PERFORM 8600-DEEDIT  THRU  8699-EXIT
CIDMOD*        MOVE WS-DEEDIT-FIELD    TO  WS-OCOMMI.
00599
00600      IF RPREML NOT = ZEROS
00601          MOVE AL-UNNON           TO  RPREMA
00602          
      * EXEC CICS BIF DEEDIT
00603 *            FIELD   (RPREMI)
00604 *            LENGTH  (11)
00605 *        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004053' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RPREMI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00606          MOVE RPREMI             TO  WS-RPREMI.
00607
00600 *    IF RPREML NOT = ZEROS
00601 *        MOVE AL-UNNON           TO  RPREMA
CIDMOD*        MOVE RPREMI             TO  WS-DEEDIT-FIELD
CIDMOD*        PERFORM 8600-DEEDIT  THRU  8699-EXIT
CIDMOD*        MOVE WS-DEEDIT-FIELD    TO  WS-RPREMI.
CIDMOD*
00608      IF MAINTI = 'A'
00609          NEXT SENTENCE
00610      ELSE
00611          GO TO 1010-EDIT-COMPLETE.
00612
00613      IF INFORCEL = ZEROS
00614        AND  MORTAMTL = ZEROS
00615        AND  FUTUREL = ZEROS
00616        AND  PTCL = ZEROS
00617        AND  IBNRL = ZEROS
00618        AND  CLAIML = ZEROS
00619        AND  EXPL = ZEROS
00620        AND  PYMNTL = ZEROS
00621        AND  OCOMML = ZEROS
00622        AND  RPREML = ZEROS
00623          MOVE ER-2585            TO  EMI-ERROR
00624          MOVE -1                 TO  INFORCEL
00625          MOVE AL-UNBON           TO  INFORCEA
00626                                      MORTAMTA
00627                                      FUTUREA
00628                                      PTCA
00629                                      IBNRA
00630                                      CLAIMA
00631                                      EXPA
00632                                      PYMNTA
00633                                      OCOMMA
00634                                      RPREMA
00635          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00636
00637      IF EXPL NOT EQUAL TO ZEROS
00638          IF RCOMPI NOT EQUAL TO LOW-VALUES
00639              MOVE ER-2543        TO  EMI-ERROR
00640              MOVE -1             TO  EXPL
00641              MOVE AL-UNBON       TO  EXPA
00642              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00643
00644      IF OCOMML NOT EQUAL TO ZEROS
00645          IF RCOMPI NOT EQUAL TO LOW-VALUES
00646              MOVE ER-2543        TO  EMI-ERROR
00647              MOVE -1             TO  OCOMML
00648              MOVE AL-UNBON       TO  OCOMMA
00649              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00650
00651      IF RPREML NOT EQUAL TO ZEROS
00652          IF RCOMPI  EQUAL TO LOW-VALUES
00653              MOVE ER-2540        TO  EMI-ERROR
00654              MOVE -1             TO  RPREML
00655              MOVE AL-UNBON       TO  RPREMA
00656              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00657
00658  1010-EDIT-COMPLETE.
00659      IF EMI-ERROR NOT = ZEROS
00660          GO TO 8200-SEND-DATAONLY.
00661
00662      MOVE MAINTI                 TO  PI-PREV-MAINT.
00663
00664      IF MAINTI = 'A'
00665          GO TO 2000-ADD-RECORD.
00666
00667      IF MAINTI = 'C'
00668          GO TO 3000-CHANGE-RECORD.
00669
00670      IF MAINTI = 'D'
00671          GO TO 4000-DELETE-RECORD.
00672
00673      IF MAINTI = 'S'
00674          GO TO 5000-BROWSE-FILE.
00675  EJECT
00676  1100-VERIFY-REIN-COMP.
00677      
      * EXEC CICS HANDLE CONDITION
00678 *        NOTFND  (1110-NO-REIN-REC)
00679 *        END-EXEC.
      *    MOVE '"$I                   ! # #00004134' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303034313334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00680
00681      MOVE PI-COMPANY-CD          TO  REIN-COMPANY-CD.
00682      MOVE 'B'                    TO  REIN-CODE.
00683      MOVE RCOMPI                 TO  REIN-TABLE.
00684
00685      
      * EXEC CICS READ
00686 *        DATASET  (ERREIN-FILE-ID)
00687 *        SET      (ADDRESS OF REINSURANCE-RECORD)
00688 *        RIDFLD   (ERREIN-KEY)
00689 *    END-EXEC.
      *    MOVE '&"S        E          (   #00004142' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERREIN-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERREIN-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF REINSURANCE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00690
00691      GO TO 1199-EXIT.
00692
00693  1110-NO-REIN-REC.
00694      MOVE ER-2579                TO  EMI-ERROR.
00695      MOVE -1                     TO  RCOMPL.
00696      MOVE AL-UABON               TO  RCOMPA.
00697
00698      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00699
00700  1199-EXIT.
00701      EXIT.
00702  EJECT
00703  1200-VERIFY-CARRIER-ID.
00704      MOVE SPACES                 TO  ELCNTL-KEY.
00705      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
00706      MOVE '6'                    TO  CNTL-REC-TYPE.
00707      MOVE CARRIERI               TO  CNTL-CARRIER.
00708      MOVE +0                     TO  CNTL-SEQ.
00709
00710      
      * EXEC CICS HANDLE CONDITION
00711 *        NOTFND  (1210-NO-CARRIER)
00712 *    END-EXEC.
      *    MOVE '"$I                   ! $ #00004167' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303034313637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00713
00714      
      * EXEC CICS READ
00715 *        DATASET  (ELCNTL-FILE-ID)
00716 *        SET      (ADDRESS OF CONTROL-FILE)
00717 *        RIDFLD   (ELCNTL-KEY)
00718 *    END-EXEC.
      *    MOVE '&"S        E          (   #00004171' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313731' TO DFHEIV0(25:11)
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
           
00719
00720      GO TO 1299-EXIT.
00721
00722  1210-NO-CARRIER.
00723      MOVE ER-2208                TO  EMI-ERROR.
00724      MOVE -1                     TO  CARRIERL.
00725      MOVE AL-UABON               TO  CARRIERA.
00726
00727      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00728
00729  1299-EXIT.
00730      EXIT.
00731  EJECT
00732  1300-VERIFY-STATE-ID.
00733      MOVE SPACES                 TO  ELCNTL-KEY.
00734      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
00735      MOVE '3'                    TO  CNTL-REC-TYPE.
00736      MOVE STATEI                 TO  CNTL-STATE.
00737      MOVE +0                     TO  CNTL-SEQ.
00738
00739      
      * EXEC CICS HANDLE CONDITION
00740 *        NOTFND  (1310-NO-STATE)
00741 *    END-EXEC.
      *    MOVE '"$I                   ! % #00004196' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303034313936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00742
00743      
      * EXEC CICS READ
00744 *        DATASET  (ELCNTL-FILE-ID)
00745 *        SET      (ADDRESS OF CONTROL-FILE)
00746 *        RIDFLD   (ELCNTL-KEY)
00747 *    END-EXEC.
      *    MOVE '&"S        E          (   #00004200' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323030' TO DFHEIV0(25:11)
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
           
00748
00749      GO TO 1399-EXIT.
00750
00751  1310-NO-STATE.
00752      MOVE ER-2209                TO  EMI-ERROR.
00753      MOVE -1                     TO  STATEL.
00754      MOVE AL-UABON               TO  STATEA.
00755
00756      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00757
00758  1399-EXIT.
00759      EXIT.
00760  EJECT
00761  1400-VERIFY-ACCOUNT.
00762      IF CARRIERL GREATER ZEROS
00763          MOVE CARRIERI           TO  ACCT-CARRIER
00764      ELSE
00765          MOVE SPACES             TO  ACCT-CARRIER.
00766
00767      IF GROUPL GREATER ZEROS
00768          MOVE GROUPI             TO  ACCT-GROUPING
00769      ELSE
00770          MOVE SPACES             TO  ACCT-GROUPING.
00771
00772      IF STATEL GREATER ZEROS
00773          MOVE STATEI             TO  ACCT-STATE
00774      ELSE
00775          MOVE SPACES             TO  ACCT-STATE.
00776
00777      MOVE ACCTI                  TO  ACCT-ACCOUNT.
00778      MOVE PI-COMPANY-CD          TO  ACCT-CO.
00779      MOVE LOW-VALUES             TO  ACCT-EXP-DATE.
00780
00781      
      * EXEC CICS HANDLE CONDITION
00782 *        NOTFND  (1410-ACCOUNT-INVALID)
00783 *        END-EXEC.
      *    MOVE '"$I                   ! & #00004238' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303034323338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00784
00785      
      * EXEC CICS READ
00786 *        DATASET  (ERACCT-ALT-FILE-ID)
00787 *        SET      (ADDRESS OF ACCOUNT-MASTER)
00788 *        RIDFLD   (ERACCT-KEY)
00789 *        GTEQ
00790 *    END-EXEC.
      *    MOVE '&"S        G          (   #00004242' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-ALT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00791
00792      MOVE AM-CONTROL-BY-VAR-GRP  TO  ERACCT-SAVE-KEY-22.
00793
00794      IF ERACCT-COMP-KEY NOT = ERACCT-SAVE-KEY
00795          GO TO 1410-ACCOUNT-INVALID.
00796
00797      GO TO 1499-EXIT.
00798
00799  1410-ACCOUNT-INVALID.
00800      PERFORM 6000-SET-VG-CONTROL  THRU  6099-EXIT.
00801
00802      MOVE ER-2210                TO  EMI-ERROR.
00803
00804      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00805
00806  1499-EXIT.
00807      EXIT.
00808  EJECT
00809  1500-VERIFY-CODES.
00810      MOVE SPACES                 TO  ELCNTL-KEY.
00811      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
00812
00813      IF BENTYPI = PI-LIFE-OVERRIDE-L1
00814         MOVE 'L'                    TO  CNTL-REC-TYPE
00815         ELSE
00816         MOVE 'A'                    TO  CNTL-REC-TYPE.
00817
00818      MOVE +0                     TO  CNTL-SEQ.
00819
00820      
      * EXEC CICS HANDLE CONDITION
00821 *        NOTFND  (1530-NO-RECORD)
00822 *    END-EXEC.
      *    MOVE '"$I                   ! '' #00004277' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303034323737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00823
00824      
      * EXEC CICS READ
00825 *        DATASET  (ELCNTL-FILE-ID)
00826 *        SET      (ADDRESS OF CONTROL-FILE)
00827 *        RIDFLD   (ELCNTL-KEY)
00828 *    END-EXEC.
      *    MOVE '&"S        E          (   #00004281' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323831' TO DFHEIV0(25:11)
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
           
00829
00830      MOVE BENCDI                TO  WS-EDITED-BEN-CD.
00831
00832      MOVE +1                     TO  EDIT-SUB.
00833
00834      IF BENTYPI = PI-AH-OVERRIDE-L1
00835          GO TO 1520-AH-SEARCH-LOOP.
00836
00837  1510-LIFE-SEARCH-LOOP.
00838      IF CF-LIFE-CODE-OUT (EDIT-SUB) = ZEROS
00839          GO TO 1540-LF-BEN-CNTL-SEARCH.
00840
00841      IF BENCDI = CF-LIFE-CODE-IN (EDIT-SUB)
00842          MOVE CF-LIFE-CODE-OUT (EDIT-SUB)  TO  WS-EDITED-BEN-CD
00843          GO TO 1540-LF-BEN-CNTL-SEARCH.
00844
00845      ADD 1                       TO  EDIT-SUB.
00846
00847      IF EDIT-SUB GREATER 120
00848          GO TO 1540-LF-BEN-CNTL-SEARCH.
00849
00850      GO TO 1510-LIFE-SEARCH-LOOP.
00851
00852  1520-AH-SEARCH-LOOP.
00853      IF CF-AH-CODE-OUT (EDIT-SUB) = ZEROS
00854          GO TO 1550-AH-BEN-CNTL-SEARCH.
00855
00856      IF BENCDI = CF-AH-CODE-IN (EDIT-SUB)
00857          MOVE CF-AH-CODE-OUT (EDIT-SUB)  TO  WS-EDITED-BEN-CD
00858          GO TO 1550-AH-BEN-CNTL-SEARCH.
00859
00860      ADD 1                       TO  EDIT-SUB.
00861
00862      IF EDIT-SUB GREATER 96
00863          GO TO 1550-AH-BEN-CNTL-SEARCH.
00864
00865      GO TO 1520-AH-SEARCH-LOOP.
00866
00867
00868  1530-NO-RECORD.
00869
00870      IF BENTYPI = PI-LIFE-OVERRIDE-L1
00871          MOVE ER-2423           TO  EMI-ERROR
00872      ELSE
00873          MOVE ER-2427           TO  EMI-ERROR.
00874
00875      MOVE  -1                   TO  BENCDL.
00876      MOVE  AL-UABON             TO  BENCDA.
00877
00878      PERFORM  9900-ERROR-FORMAT  THRU  9900-EXIT.
00879
00880      GO TO 1599-EXIT.
00881
00882  1540-LF-BEN-CNTL-SEARCH.
00883
00884      MOVE  SPACES               TO  ELCNTL-KEY.
00885      MOVE  PI-COMPANY-ID        TO  CNTL-COMP-ID.
00886      MOVE  '4'                  TO  CNTL-REC-TYPE.
00887      MOVE  +0                   TO  CNTL-SEQ.
00888      MOVE  WS-EDITED-BEN-CD     TO  CNTL-HI-BEN-CD.
00889
00890      
      * EXEC CICS READ
00891 *        DATASET   (ELCNTL-FILE-ID)
00892 *        SET       (ADDRESS OF CONTROL-FILE)
00893 *        RIDFLD    (ELCNTL-KEY)
00894 *        GTEQ
00895 *    END-EXEC.
      *    MOVE '&"S        G          (   #00004347' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333437' TO DFHEIV0(25:11)
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
           
00896
00897      PERFORM 1560-FIND-BENEFIT  THRU 1560-EXIT.
00898
00899      IF  CF-BENEFIT-CODE (ED-SUB)   NOT =  WS-EDITED-BEN-CD
00900          MOVE  ER-2604          TO  EMI-ERROR
00901          MOVE  -1               TO  BENCDL
00902          MOVE  AL-UABON         TO  BENCDA
00903          PERFORM  9900-ERROR-FORMAT  THRU  9900-EXIT.
00904
00905      GO TO  1599-EXIT.
00906
00907  1550-AH-BEN-CNTL-SEARCH.
00908
00909      MOVE  SPACES               TO  ELCNTL-KEY.
00910      MOVE  PI-COMPANY-ID        TO  CNTL-COMP-ID.
00911      MOVE  '5'                  TO  CNTL-REC-TYPE.
00912      MOVE  +0                   TO  CNTL-SEQ.
00913      MOVE  WS-EDITED-BEN-CD     TO  CNTL-HI-BEN-CD.
00914
00915      
      * EXEC CICS READ
00916 *        DATASET   (ELCNTL-FILE-ID)
00917 *        SET       (ADDRESS OF CONTROL-FILE)
00918 *        RIDFLD    (ELCNTL-KEY)
00919 *        GTEQ
00920 *    END-EXEC.
      *    MOVE '&"S        G          (   #00004372' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333732' TO DFHEIV0(25:11)
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
           
00921
00922      PERFORM 1560-FIND-BENEFIT  THRU 1560-EXIT.
00923
00924      IF  CF-BENEFIT-CODE (ED-SUB)  NOT =  WS-EDITED-BEN-CD
00925          MOVE  ER-2605          TO  EMI-ERROR
00926          MOVE  -1               TO  BENCDL
00927          MOVE  AL-UABON         TO  BENCDA
00928          PERFORM  9900-ERROR-FORMAT  THRU  9900-EXIT.
00929
00930      GO TO  1599-EXIT.
00931
00932  1560-FIND-BENEFIT.
00933
00934      PERFORM  1560-BENEFIT-DUMMY  THRU  1560-DUMMY-EXIT
00935         VARYING  ED-SUB  FROM  +1  BY  +1  UNTIL
00936            ((ED-SUB   GREATER THAN  8)  OR
00937                (CF-BENEFIT-NUMERIC (ED-SUB)  =
00938                                     WS-EDITED-BEN-CD)).
00939
00940  1560-EXIT.
00941
00942  1560-BENEFIT-DUMMY.
00943
00944  1560-DUMMY-EXIT.
00945      EXIT.
00946
00947  1599-EXIT.
00948      EXIT.
00949  EJECT
00950  1600-VALID-EFF-DATE.
00951      IF EFFDTI = LOW-VALUES
00952          MOVE WS-CURRENT-DT      TO  EFFDTI.
00953
00954      MOVE AL-UNNON               TO  EFFDTA.
00955      MOVE EFFDTI                 TO  WS-DEEDIT-DATE.
00956
00957      PERFORM 8700-DEEDIT-DATE  THRU  8799-EXIT.
00958
00959      MOVE WS-DEEDIT-DATE-6       TO  DC-GREG-DATE-1-MDY.
00960      MOVE '4'                    TO  DC-OPTION-CODE.
00961
00962      PERFORM 8500-DATE-CONVERT  THRU  8599-EXIT.
00963
00964      IF NO-CONVERSION-ERROR
00965          MOVE DC-BIN-DATE-1      TO  WS-BIN-EFF-DATE-ENTERED
00966      ELSE
00967          MOVE ER-0231            TO  EMI-ERROR
00968          MOVE -1                 TO  EFFDTL
00969          MOVE AL-UNBON           TO  EFFDTA
00970          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00971 *        GO TO 1699-EXIT
00972 *    ELSE
00973 *        MOVE ER-0216            TO  EMI-ERROR
00974 *        MOVE -1                 TO  EFFDTL
00975 *        MOVE AL-UNBON           TO  EFFDTA
00976 *        PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00977          GO TO 1699-EXIT.
00978
00979  1610-HANDLE.
00980      
      * EXEC CICS HANDLE CONDITION
00981 *        ENDFILE  (1630-NO-MATCH)
00982 *    END-EXEC.
      *    MOVE '"$''                   ! ( #00004437' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303034343337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00983
00984 *    MOVE WS-SV-CARRIER            TO  ACCT-CARRIER.
00985 *    MOVE WS-SV-GROUPING           TO  ACCT-GROUPING.
00986 *    MOVE WS-SV-STATE              TO  ACCT-STATE.
00987 *    MOVE ACCTI                    TO  ACCT-ACCOUNT.
00988      MOVE WS-BIN-EFF-DATE-ENTERED  TO  ACCT-EXP-DATE.
00989      MOVE ERACCT-KEY               TO  ERACCT-SAVE-KEY-22.
00990
00991      
      * EXEC CICS STARTBR
00992 *        DATASET  (ERACCT-ALT-FILE-ID)
00993 *        RIDFLD   (ERACCT-SAVE-KEY-22)
00994 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00004448' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-ALT-FILE-ID, 
                 ERACCT-SAVE-KEY-22, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00995  EJECT
00996  1620-READ-LOOP.
00997      
      * EXEC CICS READNEXT
00998 *        DATASET  (ERACCT-ALT-FILE-ID)
00999 *        SET      (ADDRESS OF ACCOUNT-MASTER)
01000 *        RIDFLD   (ERACCT-SAVE-KEY-22)
01001 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00004454' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-ALT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERACCT-SAVE-KEY-22, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01002
01003      IF AM-VG-CARRIER NOT = ACCT-CARRIER
01004        OR AM-VG-STATE NOT = ACCT-STATE
01005        OR AM-VG-GROUPING NOT = ACCT-GROUPING
01006        OR AM-VG-ACCOUNT NOT = ACCT-ACCOUNT
01007          GO TO 1630-NO-MATCH.
01008
01009      IF WS-BIN-EFF-DATE-ENTERED LESS AM-VG-EXPIRATION-DT
01010          NEXT SENTENCE
01011      ELSE
01012          GO TO 1620-READ-LOOP.
01013
01014      IF WS-BIN-EFF-DATE-ENTERED LESS AM-EFFECTIVE-DT
01015          GO TO 1630-NO-MATCH.
01016
01017      MOVE AM-CARRIER             TO  WS-SV-CARRIER.
01018      MOVE AM-GROUPING            TO  WS-SV-GROUPING.
01019      MOVE AM-STATE               TO  WS-SV-STATE.
01020      MOVE AM-EFFECTIVE-DT        TO  WS-EFF-DT-HLD
01021                                      DC-BIN-DATE-1.
01022      MOVE SPACE                  TO  DC-OPTION-CODE.
01023
01024      PERFORM 8500-DATE-CONVERT  THRU  8599-EXIT.
01025
01026      MOVE DC-GREG-DATE-1-EDIT    TO  EFFDTI.
01027
01028 ***  Y2K PROJ 7744
01029      IF AM-HI-CERT-DATE = ZEROS
01030          MOVE ER-2705            TO  EMI-ERROR
01031          MOVE -1                 TO  MAINTL
01032          PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.
01033 ***  Y2K PROJ 7744
01034
01035      MOVE AM-EXPIRATION-DT       TO  WS-EXP-DT-HLD.
01036
01037      IF AM-EXPIRATION-DT = HIGH-VALUES
01038          MOVE '99/99/99'           TO  EXPDTI
01039      ELSE
01040          MOVE AM-EXPIRATION-DT     TO  DC-BIN-DATE-1
01041          MOVE SPACE                TO  DC-OPTION-CODE
01042          PERFORM 8500-DATE-CONVERT  THRU  8599-EXIT
01043          MOVE DC-GREG-DATE-1-EDIT  TO  EXPDTI
01044          GO TO 1640-END-BROWSE.
01045
01046      IF WS-BIN-EFF-DATE-ENTERED = AM-EXPIRATION-DT
01047          GO TO 1620-READ-LOOP.
01048
01049      GO TO 1699-EXIT.
01050  EJECT
01051  1630-NO-MATCH.
01052      MOVE ER-2584                TO  EMI-ERROR.
01053      MOVE -1                     TO  EFFDTL.
01054      MOVE AL-UNBON               TO  EFFDTA.
01055
01056      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01057
01058  1640-END-BROWSE.
01059      
      * EXEC CICS ENDBR
01060 *        DATASET  (ERACCT-ALT-FILE-ID)
01061 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004516' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERACCT-ALT-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01062
01063  1699-EXIT.
01064      EXIT.
01065  EJECT
01066  2000-ADD-RECORD.
01067      
      * EXEC CICS GETMAIN
01068 *        SET      (ADDRESS OF PENDING-RETRO-REIN-ADJUSTMENTS)
01069 *        LENGTH   (200)
01070 *        INITIMG  (GETMAIN-SPACE)
01071 *    END-EXEC.
           MOVE 200
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00004524' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF PENDING-RETRO-REIN-ADJUSTMENTS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01072
01073      MOVE 'RP'                   TO  RP-RECORD-ID.
01074      MOVE PI-COMPANY-CD          TO  RP-COMPANY-CD.
01075
01076      IF CARR-GROUP-ST-ACCNT-CNTL
01077        OR CARR-ST-ACCNT-CNTL
01078        OR CARR-ACCNT-CNTL
01079          MOVE CARRIERI           TO  RP-CARRIER
01080      ELSE
01081          MOVE SPACES             TO  RP-CARRIER.
01082
01083      IF CARR-GROUP-ST-ACCNT-CNTL
01084          MOVE GROUPI             TO  RP-GROUPING
01085      ELSE
01086          MOVE SPACES             TO  RP-GROUPING.
01087
01088      IF CARR-GROUP-ST-ACCNT-CNTL
01089        OR CARR-ST-ACCNT-CNTL
01090        OR ST-ACCNT-CNTL
01091          MOVE STATEI             TO  RP-STATE
01092      ELSE
01093          MOVE SPACES             TO  RP-STATE.
01094
01095      MOVE ACCTI                  TO  RP-ACCOUNT.
01096      MOVE PI-WORK-SEQ-NO         TO  RP-FILE-SEQ-NO.
01097      ADD +1                      TO  PI-WORK-SEQ-NO.
01098      MOVE '1'                    TO  RP-RECORD-TYPE.
01099      MOVE PI-PROCESSOR-ID        TO  RP-LAST-MAINT-BY.
01100      MOVE EIBTIME                TO  RP-LAST-MAINT-HHMMSS.
01101      MOVE WS-CURRENT-BIN-DT      TO  RP-LAST-MAINT-DT
01102                                      RP-INPUT-DT.
01103      MOVE LOW-VALUES             TO  RP-CREDIT-ACCEPT-DT.
01104
01105      IF EOMDTL IS GREATER THAN ZEROS
01106          MOVE EOMDTI                 TO  DC-GREG-DATE-1-MDY
01107          MOVE '4'                    TO  DC-OPTION-CODE
01108          PERFORM 8500-DATE-CONVERT  THRU 8599-EXIT
01109          IF NO-CONVERSION-ERROR
01110              MOVE DC-BIN-DATE-1      TO  RP-CREDIT-SELECT-DT
01111          ELSE
01112              MOVE PI-CR-MONTH-END-DT TO  RP-CREDIT-SELECT-DT
01113      ELSE
01114          MOVE PI-CR-MONTH-END-DT     TO  RP-CREDIT-SELECT-DT.
01115
01116      MOVE RP-CREDIT-SELECT-DT        TO  DC-BIN-DATE-1.
01117      MOVE ' '                        TO  DC-OPTION-CODE.
01118      PERFORM 8500-DATE-CONVERT THRU 8599-EXIT.
01119      IF NO-CONVERSION-ERROR
01120          MOVE DC-GREG-DATE-1-EDIT    TO  EOMDTO
01121      ELSE
01122          MOVE SPACES                 TO  EOMDTO.
01123
01124      IF RCOMPL NOT = ZEROS
01125          MOVE RCOMPI             TO  RP-REIN-COMP-NO.
01126
01127      MOVE WS-EDITED-BEN-CD       TO  RP-BENEFIT-CD.
01128      MOVE BENTYPI                TO  RP-BENEFIT-TYPE.
01129
01130      IF MTHYRL NOT = ZEROS
01131          MOVE MTHYRI             TO  RP-EPEC-ADJ-DT
01132      ELSE
01133          MOVE ZEROS              TO  RP-EPEC-ADJ-DT.
01134
01135      MOVE WS-EFF-DT-HLD            TO  RP-ACCOUNT-EFF-DT
01136      MOVE WS-EXP-DT-HLD            TO  RP-ACCOUNT-EXP-DT
01137      MOVE WS-BIN-EFF-DATE-ENTERED  TO  RP-EFF-DATE-ENTERED
01138      MOVE WS-SV-CARRIER            TO  RP-SV-CARRIER.
01139      MOVE WS-SV-GROUPING           TO  RP-SV-GROUPING.
01140      MOVE WS-SV-STATE              TO  RP-SV-STATE.
01141      MOVE ZEROS                    TO  RP-INS-AMT-INFORCE
01142                                        RP-LIFE-MORTALITY-AMT
01143                                        RP-FUTURE-RESERVE
01144                                        RP-PTC-RESERVE
01145                                        RP-IBNR-RESERVE
01146                                        RP-CLAIM-ADJ-AMT
01147                                        RP-EXPENSES
01148                                        RP-PAYMENTS
01149                                        RP-OTHER-COMM
01150                                        RP-REIN-PREM-ADJ.
01151
01152      MOVE RP-LAST-MAINT-BY       TO  MAINTBYO.
01153      MOVE WS-CURRENT-DT          TO  MAINTDTO.
01154      MOVE RP-LAST-MAINT-HHMMSS   TO  TIME-IN.
01155      MOVE TIME-OUT               TO  MAINTATO.
01156
01157      IF INFORCEL NOT = ZEROS
01158          MOVE WS-INFORCEI         TO  RP-INS-AMT-INFORCE
01159          MOVE RP-INS-AMT-INFORCE  TO  INFORCEO.
01160
01161      IF MORTAMTL NOT = ZEROS
01162          MOVE WS-MORTAMTI            TO  RP-LIFE-MORTALITY-AMT
01163          MOVE RP-LIFE-MORTALITY-AMT  TO  MORTAMTO.
01164
01165      IF FUTUREL NOT = ZEROS
01166          MOVE WS-FUTUREI         TO  RP-FUTURE-RESERVE
01167          MOVE RP-FUTURE-RESERVE  TO  FUTUREO.
01168
01169      IF PTCL NOT = ZEROS
01170          MOVE WS-PTCI            TO  RP-PTC-RESERVE
01171          MOVE RP-PTC-RESERVE     TO  PTCO.
01172
01173      IF IBNRL NOT = ZEROS
01174          MOVE WS-IBNRI           TO  RP-IBNR-RESERVE
01175          MOVE RP-IBNR-RESERVE    TO  IBNRO.
01176
01177      IF CLAIML NOT = ZEROS
01178          MOVE WS-CLAIMI          TO  RP-CLAIM-ADJ-AMT
01179          MOVE RP-CLAIM-ADJ-AMT   TO  CLAIMO.
01180
01181      IF EXPL NOT = ZEROS
01182          MOVE WS-EXPI            TO  RP-EXPENSES
01183          MOVE RP-EXPENSES        TO  EXPO.
01184
01185      IF PYMNTL NOT = ZEROS
01186          MOVE WS-PYMNTI          TO  RP-PAYMENTS
01187          MOVE RP-PAYMENTS        TO  PYMNTO.
01188
01189      IF OCOMML NOT = ZEROS
01190          MOVE WS-OCOMMI          TO  RP-OTHER-COMM
01191          MOVE RP-OTHER-COMM      TO  OCOMMO.
01192
01193      IF RPREML NOT = ZEROS
01194          MOVE WS-RPREMI          TO  RP-REIN-PREM-ADJ
01195          MOVE RP-REIN-PREM-ADJ   TO  RPREMO.
01196
01197  2100-WRITE-RECORD.
01198      MOVE RP-CONTROL-PRIMARY              TO  PI-ERREPY-KEY.
01199 *    MOVE 'A'                             TO  JP-RECORD-TYPE.
01200 *    MOVE PENDING-RETRO-REIN-ADJUSTMENTS  TO  JP-RECORD-AREA.
01201
01202      
      * EXEC CICS WRITE
01203 *        DATASET  (ERREPY-FILE-ID)
01204 *        FROM     (PENDING-RETRO-REIN-ADJUSTMENTS)
01205 *        RIDFLD   (RP-CONTROL-PRIMARY)
01206 *        END-EXEC.
           MOVE LENGTH OF
            PENDING-RETRO-REIN-ADJUSTMENTS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004659' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERREPY-FILE-ID, 
                 PENDING-RETRO-REIN-ADJUSTMENTS, 
                 DFHEIV11, 
                 RP-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01207
01208 *    PERFORM 8400-LOG-JOURNAL-RECORD.
01209
01210      IF NOT  EMI-NO-ERRORS
01211          GO TO 8200-SEND-DATAONLY.
01212
01213      MOVE ER-0000                TO  EMI-ERROR
01214
01215      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01216
01217      GO TO 8100-SEND-INITIAL-MAP.
01218  EJECT
01219  3000-CHANGE-RECORD.
01220      
      * EXEC CICS HANDLE CONDITION
01221 *        NOTFND  (3900-RECORD-NOTFND)
01222 *    END-EXEC.
      *    MOVE '"$I                   ! ) #00004677' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303034363737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01223
01224      
      * EXEC CICS READ
01225 *        DATASET  (ERREPY-FILE-ID)
01226 *        SET      (ADDRESS OF PENDING-RETRO-REIN-ADJUSTMENTS)
01227 *        RIDFLD   (PI-ERREPY-KEY)
01228 *        UPDATE
01229 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00004681' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERREPY-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERREPY-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-RETRO-REIN-ADJUSTMENTS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01230
01231 *    MOVE 'B'                             TO  JP-RECORD-TYPE.
01232 *    MOVE PENDING-RETRO-REIN-ADJUSTMENTS  TO  JP-RECORD-AREA.
01233
01234 *    PERFORM 8400-LOG-JOURNAL-RECORD.
01235
01236      IF RCOMPL NOT = ZEROS
01237          MOVE RCOMPI             TO  RP-REIN-COMP-NO.
01238
01239      IF BENCDL NOT = ZEROS
01240          MOVE WS-EDITED-BEN-CD   TO  RP-BENEFIT-CD.
01241
01242      IF BENTYPL NOT = ZEROS
01243          MOVE BENTYPI            TO  RP-BENEFIT-TYPE.
01244
01245      IF MTHYRL NOT = ZEROS
01246          MOVE MTHYRI             TO  RP-EPEC-ADJ-DT.
01247
01248      MOVE WS-EFF-DT-HLD            TO  RP-ACCOUNT-EFF-DT.
01249      MOVE WS-EXP-DT-HLD            TO  RP-ACCOUNT-EXP-DT.
01250      MOVE WS-BIN-EFF-DATE-ENTERED  TO  RP-EFF-DATE-ENTERED.
01251
01252      IF EOMDTL IS NOT EQUAL TO ZEROS
01253          MOVE EOMDTI                  TO  DC-GREG-DATE-1-MDY
01254          MOVE '4'                     TO  DC-OPTION-CODE
01255          PERFORM 8500-DATE-CONVERT THRU 8599-EXIT
01256          IF NO-CONVERSION-ERROR
01257              MOVE DC-BIN-DATE-1       TO  RP-CREDIT-SELECT-DT
01258          ELSE
01259              MOVE PI-CR-MONTH-END-DT  TO  RP-CREDIT-SELECT-DT.
01260
01261      MOVE RP-CREDIT-SELECT-DT         TO  DC-BIN-DATE-1.
01262      MOVE ' '                         TO  DC-OPTION-CODE.
01263      PERFORM 8500-DATE-CONVERT THRU 8599-EXIT.
01264      IF NO-CONVERSION-ERROR
01265          MOVE DC-GREG-DATE-1-EDIT     TO  EOMDTO
01266      ELSE
01267          MOVE SPACES                  TO  EOMDTO.
01268
01269  EJECT
01270      IF INFORCEL NOT = ZEROS
01271          MOVE WS-INFORCEI         TO  RP-INS-AMT-INFORCE
01272          MOVE RP-INS-AMT-INFORCE  TO  INFORCEO.
01273
01274      IF MORTAMTL NOT = ZEROS
01275          MOVE WS-MORTAMTI         TO  RP-LIFE-MORTALITY-AMT
01276          MOVE RP-LIFE-MORTALITY-AMT  TO  MORTAMTO.
01277      IF FUTUREL NOT = ZEROS
01278          MOVE WS-FUTUREI         TO  RP-FUTURE-RESERVE
01279          MOVE RP-FUTURE-RESERVE  TO  FUTUREO.
01280
01281      IF PTCL NOT = ZEROS
01282          MOVE WS-PTCI            TO  RP-PTC-RESERVE
01283          MOVE RP-PTC-RESERVE     TO  PTCO.
01284
01285      IF IBNRL NOT = ZEROS
01286          MOVE WS-IBNRI           TO  RP-IBNR-RESERVE
01287          MOVE RP-IBNR-RESERVE    TO  IBNRO.
01288
01289      IF CLAIML NOT = ZEROS
01290          MOVE WS-CLAIMI          TO  RP-CLAIM-ADJ-AMT
01291          MOVE RP-CLAIM-ADJ-AMT   TO  CLAIMO.
01292
01293      IF EXPL NOT = ZEROS
01294          MOVE WS-EXPI            TO  RP-EXPENSES
01295          MOVE RP-EXPENSES        TO  EXPO.
01296
01297      IF PYMNTL NOT = ZEROS
01298          MOVE WS-PYMNTI          TO  RP-PAYMENTS
01299          MOVE RP-PAYMENTS        TO  PYMNTO.
01300
01301      IF OCOMML NOT = ZEROS
01302          MOVE WS-OCOMMI          TO  RP-OTHER-COMM
01303          MOVE RP-OTHER-COMM      TO  OCOMMO.
01304
01305      IF RPREML NOT = ZEROS
01306          MOVE WS-RPREMI          TO  RP-REIN-PREM-ADJ
01307          MOVE RP-REIN-PREM-ADJ   TO  RPREMO.
01308
01309  3100-REWRITE-RECORD.
01310      MOVE PI-PROCESSOR-ID        TO  RP-LAST-MAINT-BY.
01311      MOVE EIBTIME                TO  RP-LAST-MAINT-HHMMSS.
01312      MOVE WS-CURRENT-BIN-DT      TO  RP-LAST-MAINT-DT.
01313      MOVE RP-LAST-MAINT-BY       TO  MAINTBYO.
01314      MOVE WS-CURRENT-DT          TO  MAINTDTO.
01315      MOVE RP-LAST-MAINT-HHMMSS   TO  TIME-IN.
01316      MOVE TIME-OUT               TO  MAINTATO.
01317 *    MOVE 'C'                    TO  JP-RECORD-TYPE.
01318 *    MOVE PENDING-RETRO-REIN-ADJUSTMENTS
01319 *                                TO  JP-RECORD-AREA.
01320
01321      
      * EXEC CICS REWRITE
01322 *        DATASET  (ERREPY-FILE-ID)
01323 *        FROM     (PENDING-RETRO-REIN-ADJUSTMENTS)
01324 *    END-EXEC.
           MOVE LENGTH OF
            PENDING-RETRO-REIN-ADJUSTMENTS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004778' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERREPY-FILE-ID, 
                 PENDING-RETRO-REIN-ADJUSTMENTS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01325
01326 *    PERFORM 8400-LOG-JOURNAL-RECORD.
01327
01328      MOVE ER-0000  TO  EMI-ERROR
01329
01330      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01331
01332      GO TO 8100-SEND-INITIAL-MAP.
01333
01334  3900-RECORD-NOTFND.
01335      MOVE ER-2586                TO  EMI-ERROR.
01336
01337      PERFORM 6000-SET-VG-CONTROL  THRU  6099-EXIT.
01338
01339      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01340
01341      GO TO 8200-SEND-DATAONLY.
01342  EJECT
01343  4000-DELETE-RECORD.
01344      
      * EXEC CICS HANDLE CONDITION
01345 *        NOTFND  (4900-RECORD-NOTFND)
01346 *    END-EXEC.
      *    MOVE '"$I                   ! * #00004801' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303034383031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01347
01348 *    MOVE 'D'                             TO  JP-RECORD-TYPE.
01349 *    MOVE PENDING-RETRO-REIN-ADJUSTMENTS  TO  JP-RECORD-AREA.
01350
01351      
      * EXEC CICS DELETE
01352 *        DATASET  (ERREPY-FILE-ID)
01353 *        RIDFLD   (PI-ERREPY-KEY)
01354 *    END-EXEC.
      *    MOVE '&(  R                 &   #00004808' TO DFHEIV0
           MOVE X'262820205220202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERREPY-FILE-ID, 
                 PI-ERREPY-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01355
01356 *    PERFORM 8400-LOG-JOURNAL-RECORD.
01357
01358      MOVE LOW-VALUES             TO  EL634AO.
01359      MOVE ER-0000                TO  EMI-ERROR
01360
01361      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01362
01363      GO TO 8100-SEND-INITIAL-MAP.
01364
01365  4900-RECORD-NOTFND.
01366      MOVE ER-2586                TO  EMI-ERROR.
01367
01368      PERFORM 6000-SET-VG-CONTROL  THRU  6099-EXIT.
01369
01370      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01371
01372      GO TO 8200-SEND-DATAONLY.
01373  EJECT
01374  5000-BROWSE-FILE.
01375      
      * EXEC CICS HANDLE CONDITION
01376 *        NOTFND   (5800-NO-RECORD)
01377 *        ENDFILE  (5900-END-OF-FILE)
01378 *    END-EXEC.
      *    MOVE '"$I''                  ! + #00004832' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303034383332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01379
01380      MOVE PI-ERREPY-KEY          TO  ERREPY-KEY.
01381
01382      IF EIBAID = DFHENTER
01383          MOVE ZEROS              TO  REPY-RECORD-SEQ.
01384
01385      IF EIBAID = DFHPF2
01386          GO TO 5100-BROWSE-BKWD.
01387
01388  5010-READ-LOOP.
01389 *    IF EIBAID = DFHPF1
01390          ADD +1                  TO  REPY-RECORD-SEQ.
01391
01392      
      * EXEC CICS READ
01393 *        DATASET  (ERREPY-FILE-ID)
01394 *        SET      (ADDRESS OF PENDING-RETRO-REIN-ADJUSTMENTS)
01395 *        RIDFLD   (ERREPY-KEY)
01396 *        GTEQ
01397 *    END-EXEC.
      *    MOVE '&"S        G          (   #00004849' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERREPY-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERREPY-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-RETRO-REIN-ADJUSTMENTS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01398
01399      IF RP-COMPANY-CD NOT = PI-COMPANY-CD
01400          IF EIBAID = DFHENTER
01401              GO TO 5800-NO-RECORD
01402          ELSE
01403              GO TO 5900-END-OF-FILE.
01404
01405      IF RP-CREDIT-ACCEPT-DT NOT = LOW-VALUES
01406 *        IF EIBAID = DFHENTER
01407 *            GO TO 5800-NO-RECORD
01408 *        ELSE
01409              MOVE RP-CONTROL-PRIMARY  TO  ERREPY-KEY
01410              GO TO 5010-READ-LOOP.
01411
01412      IF EIBAID = DFHENTER
01413          IF REPY-CARRIER = RP-CARRIER
01414            AND REPY-GROUPING = RP-GROUPING
01415            AND REPY-STATE = RP-STATE
01416            AND REPY-ACCOUNT = RP-ACCOUNT
01417              GO TO 5200-FORMAT-SCREEN
01418          ELSE
01419              GO TO 5800-NO-RECORD.
01420
01421      GO TO 5200-FORMAT-SCREEN.
01422  EJECT
01423  5100-BROWSE-BKWD.
01424      
      * EXEC CICS STARTBR
01425 *        DATASET  (ERREPY-FILE-ID)
01426 *        RIDFLD   (ERREPY-KEY)
01427 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00004881' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERREPY-FILE-ID, 
                 ERREPY-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01428
01429      
      * EXEC CICS READPREV
01430 *        DATASET  (ERREPY-FILE-ID)
01431 *        SET      (ADDRESS OF PENDING-RETRO-REIN-ADJUSTMENTS)
01432 *        RIDFLD   (ERREPY-KEY)
01433 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00004886' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERREPY-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERREPY-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-RETRO-REIN-ADJUSTMENTS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01434
01435  5110-READ-LOOP.
01436      IF PI-FILE-EOF
01437          MOVE SPACE              TO  PI-EOF-SW
01438      ELSE
01439          
      * EXEC CICS READPREV
01440 *            DATASET  (ERREPY-FILE-ID)
01441 *            SET      (ADDRESS OF PENDING-RETRO-REIN-ADJUSTMENTS)
01442 *            RIDFLD   (ERREPY-KEY)
01443 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00004896' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERREPY-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERREPY-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-RETRO-REIN-ADJUSTMENTS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01444
01445      IF RP-COMPANY-CD NOT = PI-COMPANY-CD
01446          GO TO 5900-END-OF-FILE.
01447
01448      IF RP-CREDIT-ACCEPT-DT NOT = LOW-VALUES
01449          GO TO 5110-READ-LOOP.
01450  EJECT
01451  5200-FORMAT-SCREEN.
01452      MOVE 'S'                    TO  MAINTI  PI-PREV-MAINT.
01453      MOVE 1                      TO  MAINTL.
01454      MOVE RP-CONTROL-PRIMARY     TO  PI-ERREPY-KEY.
01455      MOVE LOW-VALUES             TO  EL634AI.
01456      MOVE RP-CARRIER             TO  CARRIERO.
01457      MOVE RP-GROUPING            TO  GROUPO.
01458      MOVE RP-STATE               TO  STATEO.
01459      MOVE RP-ACCOUNT             TO  ACCTO.
01460
01461      IF RP-REIN-COMP-NO NOT = SPACES
01462          MOVE RP-REIN-COMP-NO    TO  RCOMPO.
01463
01464      MOVE RP-BENEFIT-CD          TO  BENCDO.
01465      MOVE RP-BENEFIT-TYPE        TO  BENTYPO.
01466
01467      IF RP-CREDIT-SELECT-DT IS NOT EQUAL TO LOW-VALUES AND SPACES
01468          MOVE RP-CREDIT-SELECT-DT      TO  DC-BIN-DATE-1
01469          MOVE ' '                      TO  DC-OPTION-CODE
01470          PERFORM 8500-DATE-CONVERT THRU 8599-EXIT
01471          IF NO-CONVERSION-ERROR
01472              MOVE DC-GREG-DATE-1-EDIT  TO  EOMDTO
01473          ELSE
01474              MOVE SPACES               TO  EOMDTO.
01475
01476      IF RP-EPEC-ADJ-DT NOT = ZEROS
01477          MOVE RP-EPEC-ADJ-DT           TO  MTHYRO
01478          MOVE RP-ACCOUNT-EFF-DT        TO  DC-BIN-DATE-1
01479          MOVE SPACE                    TO  DC-OPTION-CODE
01480          PERFORM 8500-DATE-CONVERT  THRU  8599-EXIT
01481          MOVE DC-GREG-DATE-1-EDIT      TO  EFFDTO
01482          IF RP-ACCOUNT-EXP-DT = HIGH-VALUES
01483              MOVE '99/99/99'           TO  EXPDTO
01484          ELSE
01485              MOVE RP-ACCOUNT-EXP-DT    TO  DC-BIN-DATE-1
01486              MOVE SPACE                TO  DC-OPTION-CODE
01487              PERFORM 8500-DATE-CONVERT  THRU  8599-EXIT
01488              MOVE DC-GREG-DATE-1-EDIT  TO  EXPDTO.
01489
01490      MOVE RP-LAST-MAINT-BY       TO  MAINTBYO.
01491      MOVE ' '                    TO  DC-OPTION-CODE.
01492      MOVE RP-LAST-MAINT-DT       TO  DC-BIN-DATE-1.
01493
01494      PERFORM 8500-DATE-CONVERT  THRU  8599-EXIT.
01495
01496      IF DATE-CONVERSION-ERROR
01497          MOVE ZEROS                TO  MAINTDTO
01498      ELSE
01499          MOVE DC-GREG-DATE-1-EDIT  TO  MAINTDTO.
01500
01501      MOVE RP-LAST-MAINT-HHMMSS   TO  TIME-IN.
01502      MOVE TIME-OUT               TO  MAINTATO.
01503
01504      IF RP-INS-AMT-INFORCE NOT = ZEROS
01505          MOVE RP-INS-AMT-INFORCE  TO  INFORCEO.
01506
01507      IF RP-LIFE-MORTALITY-AMT NOT = ZEROS
01508          MOVE RP-LIFE-MORTALITY-AMT  TO  MORTAMTO.
01509
01510      IF RP-FUTURE-RESERVE NOT = ZEROS
01511          MOVE RP-FUTURE-RESERVE  TO  FUTUREO.
01512
01513      IF RP-PTC-RESERVE NOT = ZEROS
01514          MOVE RP-PTC-RESERVE     TO  PTCO.
01515
01516      IF RP-IBNR-RESERVE NOT = ZEROS
01517          MOVE RP-IBNR-RESERVE    TO  IBNRO.
01518
01519      IF RP-CLAIM-ADJ-AMT NOT = ZEROS
01520          MOVE RP-CLAIM-ADJ-AMT   TO  CLAIMO.
01521
01522      IF RP-EXPENSES NOT = ZEROS
01523          MOVE RP-EXPENSES        TO  EXPO.
01524
01525      IF RP-PAYMENTS NOT = ZEROS
01526          MOVE RP-PAYMENTS        TO  PYMNTO.
01527
01528      IF RP-OTHER-COMM NOT = ZEROS
01529          MOVE RP-OTHER-COMM      TO  OCOMMO.
01530
01531      IF RP-REIN-PREM-ADJ NOT = ZEROS
01532          MOVE RP-REIN-PREM-ADJ   TO  RPREMO.
01533
01534      GO TO 8100-SEND-INITIAL-MAP.
01535  EJECT
01536  5800-NO-RECORD.
01537      IF EIBAID = DFHPF2
01538          GO TO 5900-END-OF-FILE.
01539
01540      MOVE ER-2586                TO  EMI-ERROR.
01541
01542      PERFORM 6000-SET-VG-CONTROL  THRU  6099-EXIT.
01543
01544      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01545
01546      GO TO 8200-SEND-DATAONLY.
01547
01548  5900-END-OF-FILE.
01549      IF EIBAID = DFHPF1
01550          MOVE 'Y'                TO  PI-EOF-SW
01551          MOVE ER-2237            TO  EMI-ERROR
01552      ELSE
01553          MOVE SPACES             TO  PI-ERREPY-KEY
01554          MOVE ER-2238            TO  EMI-ERROR.
01555
01556      MOVE -1                     TO  MAINTL.
01557
01558      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01559
01560      GO TO 8200-SEND-DATAONLY.
01561  EJECT
01562  6000-SET-VG-CONTROL.
01563      IF CARR-GROUP-ST-ACCNT-CNTL
01564          MOVE -1                    TO  CARRIERL
01565          MOVE AL-UABON              TO  CARRIERA
01566                                         GROUPA
01567                                         STATEA
01568                                         ACCTA
01569      ELSE
01570          IF ST-ACCNT-CNTL
01571              MOVE -1                TO  STATEL
01572              MOVE AL-UABON          TO  STATEA
01573                                         ACCTA
01574          ELSE
01575              IF CARR-ST-ACCNT-CNTL
01576                  MOVE -1            TO  CARRIERL
01577                  MOVE AL-UABON      TO  CARRIERA
01578                                         STATEA
01579                                         ACCTA
01580              ELSE
01581                  IF ACCNT-CNTL
01582                      MOVE -1        TO  ACCTL
01583                      MOVE AL-UABON  TO  ACCTA
01584                  ELSE
01585                      MOVE -1        TO  CARRIERL
01586                      MOVE AL-UABON  TO  CARRIERA
01587                                         ACCTA.
01588
01589  6099-EXIT.
01590      EXIT.
01591  EJECT
01592  8100-SEND-INITIAL-MAP.
01593      MOVE WS-CURRENT-DT          TO  DATEO.
01594      MOVE EIBTIME                TO  TIME-IN.
01595      MOVE TIME-OUT               TO  TIMEO.
01596      MOVE -1                     TO  MAINTL.
01597      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
01598      MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.
01599      MOVE EMI-MESSAGE-AREA (3)   TO  ERRMSG3O.
01600      MOVE PI-LIFE-OVERRIDE-L6    TO  LFHDGO.
01601
01602      IF CARR-GROUP-ST-ACCNT-CNTL
01603          NEXT SENTENCE
01604      ELSE
01605          IF ST-ACCNT-CNTL
01606              MOVE AL-SADOF              TO  CARHDGA
01607                                             GRPHDGA
01608              MOVE AL-SANOF              TO  CARRIERA
01609                                             GROUPA
01610          ELSE
01611              IF CARR-ST-ACCNT-CNTL
01612                  MOVE AL-SADOF          TO  GRPHDGA
01613                  MOVE AL-SANOF          TO  GROUPA
01614              ELSE
01615                  IF ACCNT-CNTL
01616                      MOVE AL-SADOF      TO  CARHDGA
01617                                             GRPHDGA
01618                                             STHDGA
01619                      MOVE AL-SANOF      TO  CARRIERA
01620                                             GROUPA
01621                                             STATEA
01622                  ELSE
01623                      IF CARR-ACCNT-CNTL
01624                          MOVE AL-SADOF  TO  GRPHDGA
01625                                             STHDGA
01626                          MOVE AL-SANOF  TO  GROUPA
01627                                             STATEA.
01628
01629      
      * EXEC CICS SEND
01630 *        MAP     (EL634A)
01631 *        MAPSET  (MAPSET-NAME)
01632 *        FROM    (EL634AO)
01633 *        ERASE
01634 *        CURSOR
01635 *    END-EXEC.
           MOVE LENGTH OF
            EL634AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00005086' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL634A, 
                 EL634AO, 
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
           
01636
01637      GO TO 9100-RETURN-TRAN.
01638  EJECT
01639  8200-SEND-DATAONLY.
01640      MOVE WS-CURRENT-DT          TO  DATEO.
01641      MOVE EIBTIME                TO  TIME-IN.
01642      MOVE TIME-OUT               TO  TIMEO.
01643      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
01644      MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.
01645      MOVE EMI-MESSAGE-AREA (3)   TO  ERRMSG3O.
01646      MOVE PI-LIFE-OVERRIDE-L6    TO  LFHDGO.
01647
01648      
      * EXEC CICS SEND
01649 *        MAP     (EL634A)
01650 *        MAPSET  (MAPSET-NAME)
01651 *        FROM    (EL634AO)
01652 *        DATAONLY
01653 *        ERASEAUP
01654 *        CURSOR
01655 *    END-EXEC.
           MOVE LENGTH OF
            EL634AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT  A    H L F ,   #00005105' TO DFHEIV0
           MOVE X'382444202020204354202041' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL634A, 
                 EL634AO, 
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
           
01656
01657      GO TO 9100-RETURN-TRAN.
01658  EJECT
01659  8300-SEND-TEXT.
01660      
      * EXEC CICS SEND TEXT
01661 *        FROM    (LOGOFF-TEXT)
01662 *        LENGTH  (LOGOFF-LENGTH)
01663 *        ERASE
01664 *        FREEKB
01665 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00005117' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313137' TO DFHEIV0(25:11)
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
           
01666
01667      
      * EXEC CICS RETURN
01668 *    END-EXEC.
      *    MOVE '.(                    &   #00005124' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01669
01670 *8400-LOG-JOURNAL-RECORD.
01671 *    MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.
01672 *    MOVE ERREPY-FILE-ID         TO  JP-FILE-ID.
01673 *    MOVE THIS-PGM               TO  JP-PROGRAM-ID.
01674
01675 *    EXEC CICS JOURNAL
01676 *        JFILEID  (PI-JOURNAL-FILE-ID)
01677 *        JTYPEID  ('EL')
01678 *        FROM     (JOURNAL-RECORD)
01679 *        LENGTH   (423)
01680 *    END-EXEC.
01681
01682  8500-DATE-CONVERT.
01683      MOVE LINK-CLDATCV           TO  PGM-NAME.
01684
01685      
      * EXEC CICS LINK
01686 *        PROGRAM   (PGM-NAME)
01687 *        COMMAREA  (DATE-CONVERSION-DATA)
01688 *        LENGTH    (DC-COMM-LENGTH)
01689 *    END-EXEC.
      *    MOVE '."C                   ''   #00005142' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01690
01691  8599-EXIT.
01692      EXIT.
01693
CIDMOD/
CIDMOD 8600-DEEDIT.
CIDMOD     
      * EXEC CICS BIF DEEDIT
CIDMOD*        FIELD   (WS-DEEDIT-FIELD)
CIDMOD*        LENGTH  (11)
CIDMOD*    END-EXEC.
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005153' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
CIDMOD
CIDMOD 8699-EXIT.
CIDMOD     EXIT.
CIDMOD/
01695  8700-DEEDIT-DATE.
01696      
      * EXEC CICS BIF DEEDIT
01697 *        FIELD   (WS-DEEDIT-DATE)
01698 *        LENGTH  (8)
01699 *    END-EXEC.
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005162' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-DEEDIT-DATE, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01700
01701  8799-EXIT.
01702      EXIT.
01703
01704  8800-UNAUTHORIZED-ACCESS.
01705      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
01706
01707      GO TO 8300-SEND-TEXT.
01708
01709  8900-PF23.
01710      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
01711      MOVE XCTL-005               TO  PGM-NAME.
01712
01713      GO TO 9300-XCTL.
01714  EJECT
01715  9000-RETURN-CICS.
01716      
      * EXEC CICS RETURN
01717 *        END-EXEC.
      *    MOVE '.(                    &   #00005182' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01718
01719  9100-RETURN-TRAN.
01720      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
01721      MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.
01722
01723      
      * EXEC CICS RETURN
01724 *        TRANSID   (TRANS-ID)
01725 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
01726 *        LENGTH    (PI-COMM-LENGTH)
01727 *    END-EXEC.
      *    MOVE '.(CT                  &   #00005189' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01728
01729  9200-RETURN-MAIN-MENU.
01730      MOVE XCTL-626               TO  PGM-NAME.
01731
01732      GO TO 9300-XCTL.
01733
01734  9300-XCTL.
01735      
      * EXEC CICS XCTL
01736 *        PROGRAM   (PGM-NAME)
01737 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
01738 *        LENGTH    (PI-COMM-LENGTH)
01739 *    END-EXEC.
      *    MOVE '.$C                   $   #00005201' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01740
01741  9400-CLEAR.
01742      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.
01743
01744      GO TO 9300-XCTL.
01745
01746  9500-PF12.
01747      MOVE XCTL-010               TO  PGM-NAME.
01748
01749      GO TO 9300-XCTL.
01750
01751  9600-PGMID-ERROR.
01752      
      * EXEC CICS HANDLE CONDITION
01753 *        PGMIDERR  (8300-SEND-TEXT)
01754 *    END-EXEC.
      *    MOVE '"$L                   ! , #00005218' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303035323138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01755
01756      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.
01757      MOVE ' '                    TO  PI-ENTRY-CD-1.
01758      MOVE XCTL-005               TO  PGM-NAME.
01759      MOVE PGM-NAME               TO  LOGOFF-PGM.
01760      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
01761
01762      GO TO 9300-XCTL.
01763  EJECT
01764  9900-ERROR-FORMAT.
01765      IF NOT  EMI-ERRORS-COMPLETE
01766          MOVE LINK-001           TO  PGM-NAME
01767          
      * EXEC CICS LINK
01768 *            PROGRAM   (PGM-NAME)
01769 *            COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
01770 *            LENGTH    (EMI-COMM-LENGTH)
01771 *        END-EXEC.
      *    MOVE '."C                   ''   #00005233' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01772
01773  9900-EXIT.
01774      EXIT.
01775
01776  9999-ABEND.
01777      MOVE LINK-004               TO  PGM-NAME.
01778      MOVE DFHEIBLK               TO  EMI-LINE1
01779
01780      
      * EXEC CICS LINK
01781 *        PROGRAM   (PGM-NAME)
01782 *        COMMAREA  (EMI-LINE1)
01783 *        LENGTH    (72)
01784 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00005246' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01785
01786      MOVE -1                     TO  PFENTERL.
01787
01788      GO TO 8200-SEND-DATAONLY.
01789
01790      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL634' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
01791
01792  9995-SECURITY-VIOLATION.
01793 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00005276' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323736' TO DFHEIV0(25:11)
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
01794
01795  9995-EXIT.
01796      EXIT.
01797

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL634' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMID-ERROR,
                     9999-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 1110-NO-REIN-REC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 1210-NO-CARRIER
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 1310-NO-STATE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 1410-ACCOUNT-INVALID
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 1530-NO-RECORD
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 1630-NO-MATCH
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 3900-RECORD-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 4900-RECORD-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 5800-NO-RECORD,
                     5900-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL634' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
