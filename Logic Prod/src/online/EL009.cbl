00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL009 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 09:25:21.
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE
00008 *                            VMOD=2.006
00009 *
00009 *
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
00023
00024 *REMARKS.    TRANSACTION - EXDT - DATE CONVERSION WORKSHEET
00025 *        THIS PROGRAM IS USED TO REVIEW THE DATE CONVERSIONS
00026 *        DONE BY THE SUBROUTINE ELDATCV.
00027
00028
00029      EJECT
00030  ENVIRONMENT DIVISION.
00031  DATA DIVISION.
00032  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00033  01  LCP-TIME-OF-DAY-XX.
00034      05  LCP-TIME-OF-DAY-68        PIC 9(6).
00035      05  FILLER                    PIC 99.
00036  01  LCP-CICS-TIME                 PIC 9(15).
00037  77  FILLER  PIC X(32)  VALUE '********************************'.
00038  77  FILLER  PIC X(32)  VALUE '*    EL009 WORKING STORAGE     *'.
00039  77  FILLER  PIC X(32)  VALUE '************ VMOD=2.006 ********'.
00040
00041  01  WS-DATE-AREA.
00042      12  SAVE-DATE           PIC X(8).
00043
00044      12  W-CURRENT-DATE      PIC 9(7)      VALUE 0.
00045      12  FILLER REDEFINES W-CURRENT-DATE.
00046          16 FILLER           PIC 9(1).
00047          16 W-CD-CCYY        PIC 9(3).
00048          16 W-CD-CCYY-R REDEFINES W-CD-CCYY.
00049             20 W-JULIAN-CEN  PIC 9(1).
00050             20 W-CD-YEAR     PIC 9(2).
00051          16 FILLER           PIC 9(3).
00052      12  W-CURRENT-DT REDEFINES W-CURRENT-DATE
00053                              PIC 9(07).
00054      12  W-CURRENT-DT-R REDEFINES W-CURRENT-DATE.
00055          16  W-CD-YEAR-CD        PIC  9.
00056          16  W-CURRENT-YEAR-YY   PIC  99.
00057          16  W-CURRENT-MONTH     PIC  99.
00058          16  W-CURRENT-DAY       PIC  99.
00059
00060      12  W-FLOAT-YEAR        PIC  9(3).
00061      12  W-FLOAT-YY REDEFINES W-FLOAT-YEAR.
00062          16  W-FLOAT-YEAR-CD         PIC  9.
00063          16  W-FLOAT-YEAR-YY         PIC  99.
00064      12  SAVE-BIN-DATE       PIC  X(2)   VALUE SPACES.
00065
00066 *    COPY ELCDATE.
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
00067
00068      EJECT
00069 *    COPY ELCDATW1.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCDATW1.                           *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *   DESCRIPTION:  WORKING STORAGE AREA FOR DATE CONVERSION RTN.  *
00008 *
00009 ******************************************************************
00010
00011  01  WS-DATE-WORK-AREA.
LOGIC      05  WS-BINARY-DATE-1             PIC S9(9)      VALUE ZERO.
LOGIC      05  WS-BINARY-DATE-2             PIC S9(9)      VALUE ZERO.
LOGIC      05  WS-ELAPSED-DAYS              PIC S9(5)      VALUE ZERO.
LOGIC      05  WS-ELAPSED-MONTHS            PIC S9(5)      VALUE ZERO.
LOGIC      05  DAYS-REMAINING               PIC S999       VALUE ZERO.
LOGIC      05  CHECK-YEAR                   PIC S999       VALUE ZERO.
00018      05  DAYS-ELAPSED                 PIC S9(5)      VALUE ZERO.
00019      05  DAYS-NEEDED                  PIC S9(5)      VALUE ZERO.
LOGIC      05  WS-DAY-OF-CENTURY            PIC S9(5)      VALUE ZERO.
LOGIC      05  DAY-OF-WEEK-X                PIC S9         VALUE ZERO.
00022      05  WORK-JULIAN.
LOGIC          10  JULIAN-YEAR              PIC S9(3)      VALUE ZERO.
LOGIC          10  JULIAN-DAY               PIC S9(3)      VALUE ZERO.
00025      05  WORK-JULIAN-1.
LOGIC          10  JULIAN-YEAR1             PIC S9(3)      VALUE ZERO.
LOGIC          10  JULIAN-DAY1              PIC S9(3)      VALUE ZERO.
00028      05  WORK-JULIAN-2.
LOGIC          10  JULIAN-YEAR2             PIC S9(3)      VALUE ZERO.
LOGIC          10  JULIAN-DAY2              PIC S9(3)      VALUE ZERO.
00031
00032  01  LEAP-YEAR-FIELDS.
00033      05  DIVIDE-LEAP-YEAR             PIC 999V99     VALUE ZERO.
00034      05  DIVIDE-LEAP-YEAR-R REDEFINES DIVIDE-LEAP-YEAR.
00035          10  DIVIDE-RESULT            PIC 999.
00036          10  DIVIDE-REMAINDER         PIC V99.
00037              88  A-LEAP-YEAR                         VALUE ZERO.
00038 /
00039  01  FILLER.
00040      05  BIN-DATE            COMP    PIC S9(9)       VALUE ZERO.
00041
00042      05  FILLER                      REDEFINES
00043          BIN-DATE.
00044          10  FILLER                  PIC XX.
00045          10  BIN-DATE-WORK           PIC XX.
00046
00047      05  BIN-DATE-3                  REDEFINES
00048          BIN-DATE.
00049          10  FILLER                  PIC X.
00050          10  BIN-DATE-3-WORK         PIC XXX.
00051
CIDMOD      05  GREG-ALPHA-MASK         PIC X(18)
CIDMOD                        VALUE '            , 0000'.
00054
00055       05  WORK-DATE-1.
LOGIC           10  YEAR1                   PIC S9(3)     VALUE ZERO.
LOGIC           10  MONTH1                  PIC S9(3)     VALUE ZERO.
LOGIC           10  DAY1                    PIC S9(3)     VALUE ZERO.
00059
00060      05  WORK-DATE-2.
LOGIC          10  YEAR2                    PIC S9(3)     VALUE ZERO.
LOGIC          10  MONTH2                   PIC S9(3)     VALUE ZERO.
LOGIC          10  DAY2                     PIC S9(3)     VALUE ZERO.
00064
00065  01  YR-2                        PIC 9(3).
00066  01  YR-2-R REDEFINES YR-2.
00067      05  YEAR2-CEN-CD            PIC 9.
00068      05  YEAR2-YR                PIC 99.
00069
00070  01  MONTH-AND-DAYS-TABLE.
00071      05  FILLER             PIC X(10)  VALUE ' JANUARY  '.
00072      05  FILLER             PIC 9(3)   VALUE ZERO.
00073      05  FILLER             PIC 9(3)   VALUE ZERO.
00074      05  FILLER             PIC 9(3)   VALUE 31.
00075      05  FILLER             PIC X(10)  VALUE 'FEBRUARY  '.
00076      05  FILLER             PIC 9(3)   VALUE 31.
00077      05  FILLER             PIC 9(3)   VALUE 31.
00078      05  FILLER             PIC 9(3)   VALUE 28.
00079      05  FILLER             PIC X(10)  VALUE '  MARCH   '.
00080      05  FILLER             PIC 9(3)   VALUE 59.
00081      05  FILLER             PIC 9(3)   VALUE 60.
00082      05  FILLER             PIC 9(3)   VALUE 31.
00083      05  FILLER             PIC X(10)  VALUE '  APRIL   '.
00084      05  FILLER             PIC 9(3)   VALUE 90.
00085      05  FILLER             PIC 9(3)   VALUE 91.
00086      05  FILLER             PIC 9(3)   VALUE 30.
00087      05  FILLER             PIC X(10)  VALUE '   MAY    '.
00088      05  FILLER             PIC 9(3)   VALUE 120.
00089      05  FILLER             PIC 9(3)   VALUE 121.
00090      05  FILLER             PIC 9(3)   VALUE 31.
00091      05  FILLER             PIC X(10)  VALUE '  JUNE    '.
00092      05  FILLER             PIC 9(3)   VALUE 151.
00093      05  FILLER             PIC 9(3)   VALUE 152.
00094      05  FILLER             PIC 9(3)   VALUE 30.
00095      05  FILLER             PIC X(10)  VALUE '  JULY    '.
00096      05  FILLER             PIC 9(3)   VALUE 181.
00097      05  FILLER             PIC 9(3)   VALUE 182.
00098      05  FILLER             PIC 9(3)   VALUE 31.
00099      05  FILLER             PIC X(10)  VALUE ' AUGUST   '.
00100      05  FILLER             PIC 9(3)   VALUE 212.
00101      05  FILLER             PIC 9(3)   VALUE 213.
00102      05  FILLER             PIC 9(3)   VALUE 31.
00103      05  FILLER             PIC X(10)  VALUE 'SEPTEMBER '.
00104      05  FILLER             PIC 9(3)   VALUE 243.
00105      05  FILLER             PIC 9(3)   VALUE 244.
00106      05  FILLER             PIC 9(3)   VALUE 30.
00107      05  FILLER             PIC X(10)  VALUE ' OCTOBER  '.
00108      05  FILLER             PIC 9(3)   VALUE 273.
00109      05  FILLER             PIC 9(3)   VALUE 274.
00110      05  FILLER             PIC 9(3)   VALUE 31.
00111      05  FILLER             PIC X(10)  VALUE 'NOVEMBER  '.
00112      05  FILLER             PIC 9(3)   VALUE 304.
00113      05  FILLER             PIC 9(3)   VALUE 305.
00114      05  FILLER             PIC 9(3)   VALUE 30.
00115      05  FILLER             PIC X(10)  VALUE 'DECEMBER  '.
00116      05  FILLER             PIC 9(3)   VALUE 334.
00117      05  FILLER             PIC 9(3)   VALUE 335.
00118      05  FILLER             PIC 9(3)   VALUE 31.
00119      05  FILLER             PIC X(10)  VALUE 'XXXXXXXX  '.
00120      05  FILLER             PIC 9(3)   VALUE 365.
00121      05  FILLER             PIC 9(3)   VALUE 366.
00122      05  FILLER             PIC 9(3)   VALUE ZERO.
00123
00124  01  TABLE-OF-MONTHS-AND-DAYS  REDEFINES  MONTH-AND-DAYS-TABLE.
00125      05  MONTHS-AND-DAYS
00126          OCCURS 13 TIMES             INDEXED BY MTHX
00127                                                 SUB1.
00128          10  NAME-OF-MONTH            PIC X(10).
00129          10  REGULAR-DAYS             PIC 999.
00130          10  LEAP-YR-DAYS             PIC 999.
00131          10  DAYS-IN-MONTH            PIC 999.
00132
00133  01  CENTURY-CONVERSION-TABLE.
00134      05  FILLER                      PIC 9(4)  VALUE 0019.
00135      05  FILLER                      PIC 9(4)  VALUE 0120.
00136      05  FILLER                      PIC 9(4)  VALUE 0221.
00137      05  FILLER                      PIC 9(4)  VALUE 0322.
00138      05  FILLER                      PIC 9(4)  VALUE 0423.
00139      05  FILLER                      PIC 9(4)  VALUE 0524.
00140      05  FILLER                      PIC 9(4)  VALUE 0625.
00141      05  FILLER                      PIC 9(4)  VALUE 0726.
00142      05  FILLER                      PIC 9(4)  VALUE 0827.
00143      05  FILLER                      PIC 9(4)  VALUE 0928.
00144  01  TABLE-CENTURY  REDEFINES CENTURY-CONVERSION-TABLE.
00145      05  CENTURY-TBL OCCURS 10 TIMES INDEXED BY CNTRY-NDX.
00146          10  CENTURY-CODE            PIC 99.
00147          10  CENTURY                 PIC 99.
00148
00149  01  CENTURY-SEARCH-VARIABLES.
00150      05  FIND-CENTURY-FLAG       PIC  X.
00151      05  SEARCH-CENTURY.
00152          10  SEARCH-CENTURY-N    PIC  99.
00153      05  SEARCH-CENTURY-CODE.
00154          10  SEARCH-CENTURY-CD-N PIC  99.
00155      05  FOUND-CENTURY.
00156          10  FOUND-CENTURY-N     PIC  99.
00157      05  FOUND-CENTURY-CODE.
00158          10  FOUND-CENTURY-CD-N  PIC  99.
00159      05  CURRENT-CENTURY-1.
00160          10  CURRENT-CENTURY-1-N PIC 99     VALUE 19.
00161      05  CONTROL-YEAR            PIC 9(03)  VALUE 070.
00162      05  CONTROL-YR REDEFINES CONTROL-YEAR.
00163          10 CONTROL-YEAR-CD      PIC  9.
00164          10 CONTROL-YEAR-YY      PIC  99.
00165      05  W-PRESENT-YEAR          PIC  9(03) VALUE ZEROS.
00166      05  PREVIOUS-CENTURY        PIC  99    VALUE ZEROS.
00167      05  NEXT-CENTURY            PIC  99    VALUE ZEROS.
00168      05  CENTURY-MARK            PIC  9(03) VALUE 100.
00169      05  CHECK-CCYY              PIC  9(04) VALUE ZEROES.
00170      05  CHECK-CCYY-R REDEFINES CHECK-CCYY.
00171          10  CHECK-CCYY-CC       PIC 99.
00172          10  CHECK-CCYY-YY       PIC 99.
00173      05  BREAK-PERIOD            PIC  9(03) VALUE 0.
00174
00070
00071      EJECT
00072  01  MISC-SWITCHS.
00073      12  EDIT-ERROR-FLAG     PIC X       VALUE 'N'.
00074          88  EDIT-ERROR          VALUE 'Y'.
00075
00076  01  FILLER.
00077      05  WS-DAY-OF-WEEK-ARRAY.
00078          10  FILLER              PICTURE X(9) VALUE 'SUNDAY'.
00079          10  FILLER              PICTURE X(9) VALUE 'MONDAY'.
00080          10  FILLER              PICTURE X(9) VALUE 'TUESDAY'.
00081          10  FILLER              PICTURE X(9) VALUE 'WEDNESDAY'.
00082          10  FILLER              PICTURE X(9) VALUE 'THURSDAY'.
00083          10  FILLER              PICTURE X(9) VALUE 'FRIDAY'.
00084          10  FILLER              PICTURE X(9) VALUE 'SATURDAY'.
00085
00086      05  WS-DAY-OF-WEEK          REDEFINES
00087          WS-DAY-OF-WEEK-ARRAY    PICTURE X(9)
00088          OCCURS 7 TIMES.
00089
00090  01  BINARY-WORK             PIC 9(8)     COMP.
00091  01  WORK-BINARY  REDEFINES BINARY-WORK.
00092      12  FILLER              PIC XX.
00093      12  BINARY-DTE          PIC XX.
00094
00095  01  HEXADECIMAL-DAYS.
00096      05  HEX-CHARACTER               PIC X
00097                        OCCURS 4 TIMES  INDEXED BY U.
00098
00099  01  CONVERSION-TABLE.
00100      05  FILLER          PIC X(21)  VALUE '104096002560001600001'.
00101      05  FILLER          PIC X(21)  VALUE '208192005120003200002'.
00102      05  FILLER          PIC X(21)  VALUE '312288007680004800003'.
00103      05  FILLER          PIC X(21)  VALUE '416384010240006400004'.
00104      05  FILLER          PIC X(21)  VALUE '520480012800008000005'.
00105      05  FILLER          PIC X(21)  VALUE '624576015360009600006'.
00106      05  FILLER          PIC X(21)  VALUE '728672017920011200007'.
00107      05  FILLER          PIC X(21)  VALUE '832768020480012800008'.
00108      05  FILLER          PIC X(21)  VALUE '936864023040014400009'.
00109      05  FILLER          PIC X(21)  VALUE 'A40960025600016000010'.
00110      05  FILLER          PIC X(21)  VALUE 'B45056028160017600011'.
00111      05  FILLER          PIC X(21)  VALUE 'C49152030720019200012'.
00112      05  FILLER          PIC X(21)  VALUE 'D53248033280020800013'.
00113      05  FILLER          PIC X(21)  VALUE 'E57344035840022400014'.
00114      05  FILLER          PIC X(21)  VALUE 'F61440038400024000015'.
00115
00116  01  HEXADECIMAL-TABLE  REDEFINES  CONVERSION-TABLE.
00117      05  HEXADECIMAL-VALUES  OCCURS 15 TIMES  INDEXED BY V.
00118          10  HEX-VALUE               PIC X.
00119          10  DECIMAL-NUMBERS  OCCURS 4 TIMES  INDEXED BY W.
00120              15  DECIMAL-VALUE       PIC 9(5).
00121
00122  01  MISC-WORK-AREAS.
00123      12  TIME-IN             PIC 9(6).
00124      12  TIME-WRK  REDEFINES TIME-IN.
00125          16  TIME-HHMM       PIC 99V99.
00126          16  FILLER          PIC XX.
00127      12  TIME-OUT            PIC 99.99.
00128      12  TEXT-AREA           PIC X(66).
00129
00130  01  MISC-COMP        COMP.
00131      12  WS-IC               PIC S9(4)   VALUE -1.
00132      12  TEXT-LENGTH         PIC S9(4)   VALUE +66.
00133      12  BLANK-TEXT          PIC S9(4)   VALUE +1.
00134
00135  01  TEXT-MESSAGES.
00136      12  MAPFAIL-MSG.
00137          16  FILLER          PIC X(40)
00138            VALUE '     MAPFAIL ENCOUNTERED - TRANSACTION A'.
00139          16  FILLER          PIC X(6)
00140            VALUE 'BORTED'.
00141      12  TRAN-COMPLETE-MSG.
00142          16  FILLER          PIC X(45)
00143            VALUE '     CLEAR ENTERED - SESSION ENDED          '.
00144
00145      EJECT
00146 *    COPY ELCDATE REPLACING
00147 *    DATE-CONVERSION-DATA        BY  DATE-CONVERSION-DATA-INIT
00148 *    DC-COMM-LENGTH              BY  INIT-DC-COMM-LENGTH
00149 *    DC-OPTION-CODE              BY  INIT-OPTION-CODE
00150 *    BIN-TO-GREG                 BY  INIT-BIN-TO-GREG
00151 *    ELAPSED-BETWEEN-BIN         BY  INIT-ELAPSED-BETWEEN-BIN
00152 *    ELAPSED-BETWEEN-BIN-30      BY  INIT-ELAPSED-BETWEEN-BIN-30
00153 *    EDIT-GREG-TO-BIN            BY  INIT-EDIT-GREG-TO-BIN
00154 *    YMD-GREG-TO-BIN             BY  INIT-YMD-GREG-TO-BIN
00155 *    MDY-GREG-TO-BIN             BY  INIT-MDY-GREG-TO-BIN
00156 *    JULIAN-TO-BIN               BY  INIT-JULIAN-TO-BIN
00157 *    BIN-PLUS-ELAPSED            BY  INIT-BIN-PLUS-ELAPSED
00158 *    FIND-CENTURY                BY  INIT-FIND-CENTURY
00159 *    ELAPSED-BETWEEN-BIN-3       BY  INIT-ELAPSED-BETWEEN-BIN-3
00160 *    EDIT-GREG-TO-BIN-3          BY  INIT-EDIT-GREG-TO-BIN-3
00161 *    YMD-GREG-TO-BIN-3           BY  INIT-YMD-GREG-TO-BIN-3
00162 *    MDY-GREG-TO-BIN-3           BY  INIT-MDY-GREG-TO-BIN-3
00163 *    JULIAN-TO-BIN-3             BY  INIT-JULIAN-TO-BIN-3
00164 *    BIN-PLUS-ELAPSED-3          BY  INIT-BIN-PLUS-ELAPSED-3
00165 *    JULIAN-EXPANDED-TO-BIN      BY  INIT-JULIAN-EXPANDED-TO-BIN
00166 *    JULIAN-EXPANDED-TO-BIN-3    BY  INIT-JULIAN-EXPANDED-TO-BIN-3
00167 *    BIN-TO-JULIAN-EXPANDED      BY  INIT-BIN-TO-JULIAN-EXPANDED
00168 *    JULIAN-EXPANDED             BY  INIT-JULIAN-EXPANDED
00169 *    CHECK-LEAP-YEAR             BY  INIT-CHECK-LEAP-YEAR
00170 *    BIN-3-TO-GREG               BY  INIT-BIN-3-TO-GREG
00171 *    CYMD-GREG-TO-BIN-3          BY  INIT-CYMD-GREG-TO-BIN-3
00172 *    MDCY-GREG-TO-BIN-3          BY  INIT-MDCY-GREG-TO-BIN-3
00173 *    CYMD-GREG-TO-BIN            BY  INIT-CYMD-GREG-TO-BIN
00174 *    MDCY-GREG-TO-BIN            BY  INIT-MDCY-GREG-TO-BIN
00175 *    MDY-GREG-TO-JULIAN          BY  INIT-MDY-GREG-TO-JULIAN
00176 *    MDCY-GREG-TO-JULIAN         BY  INIT-MDCY-GREG-TO-JULIAN
00177 *    YMD-GREG-TO-JULIAN          BY  INIT-YMD-GREG-TO-JULIAN
00178 *    CYMD-GREG-TO-JULIAN         BY  INIT-CYMD-GREG-TO-JULIAN
00179 *    THREE-CHARACTER-BIN         BY  INIT-THREE-CHARACTER-BIN
00180 *    GREGORIAN-TO-BIN            BY  INIT-GREGORIAN-TO-BIN
00181 *    BIN-TO-GREGORIAN            BY  INIT-BIN-TO-GREGORIAN
00182 *    JULIAN-TO-BINARY            BY  INIT-JULIAN-TO-BINARY
00183 *    DC-ERROR-CODE               BY  INIT-DC-ERROR-CODE
00184 *    NO-CONVERSION-ERROR         BY  INIT-NO-CONVERSION-ERROR
00185 *    DATE-CONVERSION-ERROR       BY  INIT-DATE-CONVERSION-ERROR
00186 *    DATE-IS-ZERO                BY  INIT-DATE-IS-ZERO
00187 *    DATE-IS-NON-NUMERIC         BY  INIT-DATE-IS-NON-NUMERIC
00188 *    DATE-IS-INVALID             BY  INIT-DATE-IS-INVALID
00189 *    DATE1-GREATER-DATE2         BY  INIT-DATE1-GREATER-DATE2
00190 *    ELAPSED-PLUS-NEGATIVE       BY  INIT-ELAPSED-PLUS-NEGATIVE
00191 *    DATE-INVALID-OPTION         BY  INIT-DATE-INVALID-OPTION
00192 *    INVALID-CENTURY             BY  INIT-INVALID-CENTURY
00193 *    ONLY-CENTURY                BY  INIT-ONLY-CENTURY
00194 *    ONLY-LEAP-YEAR              BY  INIT-ONLY-LEAP-YEAR
00195 *    VALID-CENTURY-LEAP-YEAR     BY  INIT-VALID-CENTURY-LEAP-YEAR
00196 *    DC-END-OF-MONTH             BY  INIT-DC-END-OF-MONTH
00197 *    CALCULATE-END-OF-MONTH      BY  INIT-CALCULATE-END-OF-MONTH
00198 *    DC-CENTURY-ADJUSTMENT       BY  INIT-DC-CENTURY-ADJUSTMENT
00199 *    USE-NORMAL-PROCESS          BY  INIT-USE-NORMAL-PROCESS
00200 *    ADJUST-DOWN-100-YRS         BY  INIT-ADJUST-DOWN-100-YRS
00201 *    ADJUST-UP-100-YRS           BY  INIT-ADJUST-UP-100-YRS
00202 *    DC-CONVERSION-DATES         BY  INIT-DC-CONVERSION-DATES
00203 *    DC-BIN-DATE-1               BY  INIT-DC-BIN-DATE-1
00204 *    DC-BIN-DATE-2               BY  INIT-DC-BIN-DATE-2
00205 *    DC-GREG-DATE-1-EDIT         BY  INIT-DC-GREG-DATE-1-EDIT
00206 *    DC-GREG-DATE-1-EDIT-R       BY  INIT-DC-GREG-DATE-1-EDIT-R
00207 *    DC-EDIT1-MONTH              BY  INIT-DC-EDIT1-MONTH
00208 *    SLASH1-1                    BY  INIT-SLASH1-1
00209 *    DC-EDIT1-DAY                BY  INIT-DC-EDIT1-DAY
00210 *    SLASH1-2                    BY  INIT-SLASH1-2
00211 *    DC-EDIT1-YEAR               BY  INIT-DC-EDIT1-YEAR
00212 *    DC-GREG-DATE-2-EDIT         BY  INIT-DC-GREG-DATE-2-EDIT
00213 *    DC-GREG-DATE-2-EDIT-R       BY  INIT-DC-GREG-DATE-2-EDIT-R
00214 *    DC-EDIT2-MONTH              BY  INIT-DC-EDIT2-MONTH
00215 *    SLASH2-1                    BY  INIT-SLASH2-1
00216 *    DC-EDIT2-DAY                BY  INIT-DC-EDIT2-DAY
00217 *    SLASH2-2                    BY  INIT-SLASH2-2
00218 *    DC-EDIT2-YEAR               BY  INIT-DC-EDIT2-YEAR
00219 *    DC-GREG-DATE-1-YMD          BY  INIT-DC-GREG-DATE-1-YMD
00220 *    DC-GREG-DATE-1-YMD-R        BY  INIT-DC-GREG-DATE-1-YMD-R
00221 *    DC-YMD-YEAR                 BY  INIT-DC-YMD-YEAR
00222 *    DC-YMD-MONTH                BY  INIT-DC-YMD-MONTH
00223 *    DC-YMD-DAY                  BY  INIT-DC-YMD-DAY
00224 *    DC-GREG-DATE-1-MDY          BY  INIT-DC-GREG-DATE-1-MDY
00225 *    DC-GREG-DATE-1-MDY-R        BY  INIT-DC-GREG-DATE-1-MDY-R
00226 *    DC-MDY-MONTH                BY  INIT-DC-MDY-MONTH
00227 *    DC-MDY-DAY                  BY  INIT-DC-MDY-DAY
00228 *    DC-MDY-YEAR                 BY  INIT-DC-MDY-YEAR
00229 *    DC-GREG-DATE-1-ALPHA        BY  INIT-DC-GREG-DATE-1-ALPHA
00230 *    DC-ALPHA-MONTH              BY  INIT-DC-ALPHA-MONTH
00231 *    DC-ALPHA-DAY                BY  INIT-DC-ALPHA-DAY
00232 *    DC-ALPHA-CENTURY            BY  INIT-DC-ALPHA-CENTURY
00233 *    DC-ALPHA-CEN-N              BY  INIT-DC-ALPHA-CEN-N
00234 *    DC-ALPHA-YEAR               BY  INIT-DC-ALPHA-YEAR
00235 *    DC-ELAPSED-MONTHS           BY  INIT-DC-ELAPSED-MONTHS
00236 *    DC-ODD-DAYS-OVER            BY  INIT-DC-ODD-DAYS-OVER
00237 *    DC-ELAPSED-DAYS             BY  INIT-DC-ELAPSED-DAYS
00238 *    DC-JULIAN-YYDDD             BY  INIT-DC-JULIAN-YYDDD
00239 *    DC-JULIAN-DT                BY  INIT-DC-JULIAN-DT
00240 *    DC-JULIAN-YEAR              BY  INIT-DC-JULIAN-YEAR
00241 *    DC-JULIAN-DAYS              BY  INIT-DC-JULIAN-DAYS
00242 *    DC-DAYS-IN-MONTH            BY  INIT-DC-DAYS-IN-MONTH
00243 *    DC-DAY-OF-WEEK              BY  INIT-DC-DAY-OF-WEEK
00244 *    DC-DAY-OF-WEEK2             BY  INIT-DC-DAY-OF-WEEK2
00245 *    DATE-CONVERSION-VARIBLES
00246 *                       BY  INIT-DATE-CONVERSION-VARIBLES
00247 *    HOLD-CENTURY-1              BY  INIT-HOLD-CENTURY-1
00248 *    HOLD-CENTURY-1-SPLIT        BY  INIT-HOLD-CENTURY-1-SPLIT
00249 *    HOLD-CEN-1-CCYY             BY  INIT-HOLD-CEN-1-CCYY
00250 *    HOLD-CEN-1-CC               BY  INIT-HOLD-CEN-1-CC
00251 *    HOLD-CEN-1-YY               BY  INIT-HOLD-CEN-1-YY
00252 *    HOLD-CEN-1-MO               BY  INIT-HOLD-CEN-1-MO
00253 *    HOLD-CEN-1-DA               BY  INIT-HOLD-CEN-1-DA
00254 *    HOLD-CENTURY-1-R            BY  INIT-HOLD-CENTURY-1-R
00255 *    HOLD-CEN-1-R-MO             BY  INIT-HOLD-CEN-1-R-MO
00256 *    HOLD-CEN-1-R-DA             BY  INIT-HOLD-CEN-1-R-DA
00257 *    HOLD-CEN-1-R-CCYY           BY  INIT-HOLD-CEN-1-R-CCYY
00258 *    HOLD-CEN-1-R-C              BY  INIT-HOLD-CEN-1-R-C
00259 *    HOLD-CEN-1-R-Y              BY  INIT-HOLD-CEN-1-R-Y
00260 *    HOLD-CENTURY-1-X            BY  INIT-HOLD-CENTURY-1-X
00261 *    HOLD-CEN-1-X-CCYY           BY  INIT-HOLD-CEN-1-X-CCYY
00262 *    HOLD-CEN-1-X-CC             BY  INIT-HOLD-CEN-1-X-CC
00263 *    HOLD-CEN-1-X-YY             BY  INIT-HOLD-CEN-1-X-YY
00264 *    HOLD-CEN-1-X-MO             BY  INIT-HOLD-CEN-1-X-MO
00265 *    HOLD-CEN-1-X-DA             BY  INIT-HOLD-CEN-1-X-DA
00266 *    HOLD-CENTURY-1-R-X          BY  INIT-HOLD-CENTURY-1-R-X
00267 *    HOLD-CEN-1-R-X-MO           BY  INIT-HOLD-CEN-1-R-X-MO
00268 *    HOLD-CEN-1-R-X-DA           BY  INIT-HOLD-CEN-1-R-X-DA
00269 *    HOLD-CEN-1-R-X-CCYY         BY  INIT-HOLD-CEN-1-R-X-CCYY
00270 *    HOLD-CEN-1-R-X-CC           BY  INIT-HOLD-CEN-1-R-X-CC
00271 *    HOLD-CEN-1-R-X-YY           BY  INIT-HOLD-CEN-1-R-X-YY
00272 *    DC-BIN-DATE-EXPAND-1        BY  INIT-DC-BIN-DATE-EXPAND-1
00273 *    DC-BIN-DATE-EXPAND-2        BY  INIT-DC-BIN-DATE-EXPAND-2
00274 *    DC-JULIAN-DATE-1            BY  INIT-DC-JULIAN-DATE-1
00275 *    DC-JULIAN-DATE-1-R          BY  INIT-DC-JULIAN-DATE-1-R
00276 *    DC-JULIAN-1-CCYY            BY  INIT-DC-JULIAN-1-CCYY
00277 *    DC-JULIAN-1-CC              BY  INIT-DC-JULIAN-1-CC
00278 *    DC-JULIAN-1-YR              BY  INIT-DC-JULIAN-1-YR
00279 *    DC-JULIAN-DA-1              BY  INIT-DC-JULIAN-DA-1
00280 *    DC-JULIAN-DATE-2            BY  INIT-DC-JULIAN-DATE-2
00281 *    DC-JULIAN-DATE-2-R          BY  INIT-DC-JULIAN-DATE-2-R
00282 *    DC-JULIAN-2-CCYY            BY  INIT-DC-JULIAN-2-CCYY
00283 *    DC-JULIAN-2-CC              BY  INIT-DC-JULIAN-2-CC
00284 *    DC-JULIAN-2-YR              BY  INIT-DC-JULIAN-2-YR
00285 *    DC-JULIAN-DA-2              BY  INIT-DC-JULIAN-DA-2
00286 *    DC-GREG-DATE-A-EDIT         BY  INIT-DC-GREG-DATE-A-EDIT
00287 *    DC-EDITA-MONTH              BY  INIT-DC-EDITA-MONTH
00288 *    SLASHA-1                    BY  INIT-SLASHA-1
00289 *    DC-EDITA-DAY                BY  INIT-DC-EDITA-DAY
00290 *    SLASHA-2                    BY  INIT-SLASHA-2
00291 *    DC-EDITA-CCYY               BY  INIT-DC-EDITA-CCYY
00292 *    DC-EDITA-CENT               BY  INIT-DC-EDITA-CENT
00293 *    DC-EDITA-YEAR               BY  INIT-DC-EDITA-YEAR
00294 *    DC-GREG-DATE-B-EDIT         BY  INIT-DC-GREG-DATE-B-EDIT
00295 *    DC-EDITB-MONTH              BY  INIT-DC-EDITB-MONTH
00296 *    SLASHB-1                    BY  INIT-SLASHA-1
00297 *    DC-EDITB-DAY                BY  INIT-DC-EDITA-DAY
00298 *    SLASHB-2                    BY  INIT-SLASHB-2
00299 *    DC-EDITB-CCYY               BY  INIT-DC-EDITB-CCYY
00300 *    DC-EDITB-CENT               BY  INIT-DC-EDITB-CENT
00301 *    DC-EDITB-YEAR               BY  INIT-DC-EDITB-YEAR
00302 *    DC-GREG-DATE-CYMD           BY  INIT-DC-GREG-DATE-CYMD
00303 *    DC-GREG-DATE-CYMD-R         BY  INIT-DC-GREG-DATE-CYMD-R
00304 *    DC-CYMD-CEN                 BY  INIT-DC-CYMD-CEN
00305 *    DC-CYMD-YEAR                BY  INIT-DC-CYMD-YEAR
00306 *    DC-CYMD-MONTH               BY  INIT-DC-CYMD-MONTH
00307 *    DC-CYMD-DAY                 BY  INIT-DC-CYMD-DAY
00308 *    DC-GREG-DATE-MDCY           BY  INIT-DC-GREG-DATE-MDCY
00309 *    DC-GREG-DATE-MDCY-R         BY  INIT-DC-GREG-DATE-MDCY-R
00310 *    DC-MDCY-MONTH               BY  INIT-DC-MDCY-MONTH
00311 *    DC-MDCY-DAY                 BY  INIT-DC-MDCY-DAY
00312 *    DC-MDCY-CEN                 BY  INIT-DC-MDCY-CEN
00313 *    DC-MDCY-YEAR                BY  INIT-DC-MDCY-YEAR.
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
00012  01  DATE-CONVERSION-DATA-INIT.
00013      12  INIT-DC-COMM-LENGTH                PIC S9(4) COMP VALUE 
           +200.
00014      12  INIT-OPTION-CODE                PIC X.
00015          88  INIT-BIN-TO-GREG                VALUE ' '.
00016          88  INIT-ELAPSED-BETWEEN-BIN        VALUE '1'.
00017          88  INIT-EDIT-GREG-TO-BIN           VALUE '2'.
00018          88  INIT-YMD-GREG-TO-BIN            VALUE '3'.
00019          88  INIT-MDY-GREG-TO-BIN            VALUE '4'.
00020          88  INIT-JULIAN-TO-BIN              VALUE '5'.
00021          88  INIT-BIN-PLUS-ELAPSED           VALUE '6'.
00022          88  INIT-FIND-CENTURY               VALUE '7'.
00023          88  INIT-ELAPSED-BETWEEN-BIN-3      VALUE '8'.
00024          88  INIT-EDIT-GREG-TO-BIN-3         VALUE '9'.
00025          88  INIT-YMD-GREG-TO-BIN-3          VALUE 'A'.
00026          88  INIT-MDY-GREG-TO-BIN-3          VALUE 'B'.
00027          88  INIT-JULIAN-TO-BIN-3            VALUE 'C'.
00028          88  INIT-BIN-PLUS-ELAPSED-3         VALUE 'D'.
00029          88  INIT-JULIAN-EXPANDED-TO-BIN     VALUE 'E'.
00030          88  INIT-JULIAN-EXPANDED-TO-BIN-3   VALUE 'F'.
00031          88  INIT-BIN-TO-JULIAN-EXPANDED     VALUE 'G'.
00032          88  INIT-JULIAN-EXPANDED            VALUE 'E', 'F', 'G'.
00033          88  INIT-CHECK-LEAP-YEAR            VALUE 'H'.
00034          88  INIT-BIN-3-TO-GREG              VALUE 'I'.
00035          88  INIT-CYMD-GREG-TO-BIN-3         VALUE 'J'.
00036          88  INIT-MDCY-GREG-TO-BIN-3         VALUE 'K'.
00037          88  INIT-CYMD-GREG-TO-BIN           VALUE 'L'.
00038          88  INIT-MDCY-GREG-TO-BIN           VALUE 'M'.
00039          88  INIT-MDY-GREG-TO-JULIAN         VALUE 'N'.
00040          88  INIT-MDCY-GREG-TO-JULIAN        VALUE 'O'.
00041          88  INIT-YMD-GREG-TO-JULIAN         VALUE 'P'.
00042          88  INIT-CYMD-GREG-TO-JULIAN        VALUE 'Q'.
00043          88  INIT-THREE-CHARACTER-BIN
00044                   VALUES  '8' '9' 'A' 'B' 'C' 'D' 'I' 'J' 'K'.
00045          88  INIT-GREGORIAN-TO-BIN
00046                   VALUES '2' '3' '4' '9' 'A' 'B' 'J' 'K' 'L' 'M'.
00047          88  INIT-BIN-TO-GREGORIAN
00048                   VALUES ' ' '1' 'I' '8' 'G'.
00049          88  INIT-JULIAN-TO-BINARY
00050                   VALUES '5' 'C' 'E' 'F'.
00051      12  INIT-DC-ERROR-CODE                 PIC X.
00052          88  INIT-NO-CONVERSION-ERROR        VALUE ' '.
00053          88  INIT-DATE-CONVERSION-ERROR
00054                   VALUES '1' '2' '3' '4' '5' '9' 'A' 'B' 'C'.
00055          88  INIT-DATE-IS-ZERO               VALUE '1'.
00056          88  INIT-DATE-IS-NON-NUMERIC        VALUE '2'.
00057          88  INIT-DATE-IS-INVALID            VALUE '3'.
00058          88  INIT-DATE1-GREATER-DATE2        VALUE '4'.
00059          88  INIT-ELAPSED-PLUS-NEGATIVE      VALUE '5'.
00060          88  INIT-DATE-INVALID-OPTION        VALUE '9'.
00061          88  INIT-INVALID-CENTURY            VALUE 'A'.
00062          88  INIT-ONLY-CENTURY               VALUE 'B'.
00063          88  INIT-ONLY-LEAP-YEAR             VALUE 'C'.
00064          88  INIT-VALID-CENTURY-LEAP-YEAR    VALUE 'B', 'C'.
00065      12  INIT-DC-END-OF-MONTH               PIC X.
00066          88  INIT-CALCULATE-END-OF-MONTH     VALUE '1'.
00067      12  INIT-DC-CENTURY-ADJUSTMENT         PIC X   VALUE SPACES.
00068          88  INIT-USE-NORMAL-PROCESS         VALUE ' '.
00069          88  INIT-ADJUST-DOWN-100-YRS        VALUE '1'.
00070          88  INIT-ADJUST-UP-100-YRS          VALUE '2'.
00071      12  FILLER                        PIC X.
00072      12  INIT-DC-CONVERSION-DATES.
00073          16  INIT-DC-BIN-DATE-1             PIC XX.
00074          16  INIT-DC-BIN-DATE-2             PIC XX.
00075          16  INIT-DC-GREG-DATE-1-EDIT       PIC X(08).
00076          16  INIT-DC-GREG-DATE-1-EDIT-R REDEFINES
00077                        INIT-DC-GREG-DATE-1-EDIT.
00078              20  INIT-DC-EDIT1-MONTH        PIC 99.
00079              20  INIT-SLASH1-1              PIC X.
00080              20  INIT-DC-EDIT1-DAY          PIC 99.
00081              20  INIT-SLASH1-2              PIC X.
00082              20  INIT-DC-EDIT1-YEAR         PIC 99.
00083          16  INIT-DC-GREG-DATE-2-EDIT       PIC X(08).
00084          16  INIT-DC-GREG-DATE-2-EDIT-R REDEFINES
00085                      INIT-DC-GREG-DATE-2-EDIT.
00086              20  INIT-DC-EDIT2-MONTH        PIC 99.
00087              20  INIT-SLASH2-1              PIC X.
00088              20  INIT-DC-EDIT2-DAY          PIC 99.
00089              20  INIT-SLASH2-2              PIC X.
00090              20  INIT-DC-EDIT2-YEAR         PIC 99.
00091          16  INIT-DC-GREG-DATE-1-YMD        PIC 9(06).
00092          16  INIT-DC-GREG-DATE-1-YMD-R  REDEFINES
00093                      INIT-DC-GREG-DATE-1-YMD.
00094              20  INIT-DC-YMD-YEAR           PIC 99.
00095              20  INIT-DC-YMD-MONTH          PIC 99.
00096              20  INIT-DC-YMD-DAY            PIC 99.
00097          16  INIT-DC-GREG-DATE-1-MDY        PIC 9(06).
00098          16  INIT-DC-GREG-DATE-1-MDY-R REDEFINES
00099                       INIT-DC-GREG-DATE-1-MDY.
00100              20  INIT-DC-MDY-MONTH          PIC 99.
00101              20  INIT-DC-MDY-DAY            PIC 99.
00102              20  INIT-DC-MDY-YEAR           PIC 99.
00103          16  INIT-DC-GREG-DATE-1-ALPHA.
00104              20  INIT-DC-ALPHA-MONTH        PIC X(10).
00105              20  INIT-DC-ALPHA-DAY          PIC 99.
00106              20  FILLER                PIC XX.
00107              20  INIT-DC-ALPHA-CENTURY.
00108                  24 INIT-DC-ALPHA-CEN-N     PIC 99.
00109              20  INIT-DC-ALPHA-YEAR         PIC 99.
00110          16  INIT-DC-ELAPSED-MONTHS         PIC S9(4)     COMP.
00111          16  INIT-DC-ODD-DAYS-OVER          PIC S9(4)     COMP.
00112          16  INIT-DC-ELAPSED-DAYS           PIC S9(4)     COMP.
00113          16  DC-JULIAN-DATE            PIC 9(05).
00114          16  INIT-DC-JULIAN-YYDDD REDEFINES DC-JULIAN-DATE
00115                                        PIC 9(05).
00116          16  INIT-DC-JULIAN-DT REDEFINES DC-JULIAN-DATE.
00117              20  INIT-DC-JULIAN-YEAR        PIC 99.
00118              20  INIT-DC-JULIAN-DAYS        PIC 999.
00119          16  INIT-DC-DAYS-IN-MONTH          PIC S9(3)       
           COMP-3.
00120          16  INIT-DC-DAY-OF-WEEK            PIC S9  VALUE ZERO 
           COMP-3.
00121          16  INIT-DC-DAY-OF-WEEK2           PIC S9  VALUE ZERO 
           COMP-3.
00122      12  INIT-DATE-CONVERSION-VARIBLES.
00123          16  INIT-HOLD-CENTURY-1            PIC 9(11) VALUE 0.
00124          16  INIT-HOLD-CENTURY-1-SPLIT REDEFINES 
           INIT-HOLD-CENTURY-1.
00125              20  FILLER                PIC 9(3).
00126              20  INIT-HOLD-CEN-1-CCYY.
00127                  24  INIT-HOLD-CEN-1-CC     PIC 99.
00128                  24  INIT-HOLD-CEN-1-YY     PIC 99.
00129              20  INIT-HOLD-CEN-1-MO         PIC 99.
00130              20  INIT-HOLD-CEN-1-DA         PIC 99.
00131          16  INIT-HOLD-CENTURY-1-R   REDEFINES 
           INIT-HOLD-CENTURY-1.
00132              20  INIT-HOLD-CEN-1-R-MO       PIC 99.
00133              20  INIT-HOLD-CEN-1-R-DA       PIC 99.
00134              20  INIT-HOLD-CEN-1-R-CCYY.
00135                  24  HOLD-CEN-1-R-CC   PIC 99.
00136                  24  HOLD-CEN-1-R-YY   PIC 99.
00137              20  FILLER                PIC 9(3).
00138          16  INIT-HOLD-CENTURY-1-X.
00139              20  FILLER                PIC X(3)  VALUE SPACES.
00140              20  INIT-HOLD-CEN-1-X-CCYY.
00141                  24  INIT-HOLD-CEN-1-X-CC   PIC XX VALUE SPACES.
00142                  24  INIT-HOLD-CEN-1-X-YY   PIC XX VALUE SPACES.
00143              20  INIT-HOLD-CEN-1-X-MO       PIC XX VALUE SPACES.
00144              20  INIT-HOLD-CEN-1-X-DA       PIC XX VALUE SPACES.
00145          16  INIT-HOLD-CENTURY-1-R-X REDEFINES 
           INIT-HOLD-CENTURY-1-X.
00146              20  INIT-HOLD-CEN-1-R-X-MO     PIC XX.
00147              20  INIT-HOLD-CEN-1-R-X-DA     PIC XX.
00148              20  INIT-HOLD-CEN-1-R-X-CCYY.
00149                  24  INIT-HOLD-CEN-1-R-X-CC PIC XX.
00150                  24  INIT-HOLD-CEN-1-R-X-YY PIC XX.
00151              20  FILLER                PIC XXX.
00152          16  INIT-DC-BIN-DATE-EXPAND-1      PIC XXX.
00153          16  INIT-DC-BIN-DATE-EXPAND-2      PIC XXX.
00154          16  INIT-DC-JULIAN-DATE-1          PIC 9(07).
00155          16  INIT-DC-JULIAN-DATE-1-R REDEFINES 
           INIT-DC-JULIAN-DATE-1.
00156              20  INIT-DC-JULIAN-1-CCYY.
00157                  24  INIT-DC-JULIAN-1-CC    PIC 99.
00158                  24  INIT-DC-JULIAN-1-YR    PIC 99.
00159              20  INIT-DC-JULIAN-DA-1        PIC 999.
00160          16  INIT-DC-JULIAN-DATE-2          PIC 9(07).
00161          16  INIT-DC-JULIAN-DATE-2-R REDEFINES 
           INIT-DC-JULIAN-DATE-2.
00162              20  INIT-DC-JULIAN-2-CCYY.
00163                  24  INIT-DC-JULIAN-2-CC    PIC 99.
00164                  24  INIT-DC-JULIAN-2-YR    PIC 99.
00165              20  INIT-DC-JULIAN-DA-2        PIC 999.
00166          16  INIT-DC-GREG-DATE-A-EDIT.
00167              20  INIT-DC-EDITA-MONTH        PIC 99.
00168              20  INIT-SLASHA-1              PIC X VALUE '/'.
00169              20  INIT-DC-EDITA-DAY          PIC 99.
00170              20  INIT-SLASHA-2              PIC X VALUE '/'.
00171              20  INIT-DC-EDITA-CCYY.
00172                  24  INIT-DC-EDITA-CENT     PIC 99.
00173                  24  INIT-DC-EDITA-YEAR     PIC 99.
00174          16  INIT-DC-GREG-DATE-B-EDIT.
00175              20  INIT-DC-EDITB-MONTH        PIC 99.
00176              20  INIT-SLASHA-1              PIC X VALUE '/'.
00177              20  INIT-DC-EDITA-DAY          PIC 99.
00178              20  INIT-SLASHB-2              PIC X VALUE '/'.
00179              20  INIT-DC-EDITB-CCYY.
00180                  24  INIT-DC-EDITB-CENT     PIC 99.
00181                  24  INIT-DC-EDITB-YEAR     PIC 99.
00182          16  INIT-DC-GREG-DATE-CYMD         PIC 9(08).
00183          16  INIT-DC-GREG-DATE-CYMD-R REDEFINES
00184                               INIT-DC-GREG-DATE-CYMD.
00185              20  INIT-DC-CYMD-CEN           PIC 99.
00186              20  INIT-DC-CYMD-YEAR          PIC 99.
00187              20  INIT-DC-CYMD-MONTH         PIC 99.
00188              20  INIT-DC-CYMD-DAY           PIC 99.
00189          16  INIT-DC-GREG-DATE-MDCY         PIC 9(08).
00190          16  INIT-DC-GREG-DATE-MDCY-R REDEFINES
00191                               INIT-DC-GREG-DATE-MDCY.
00192              20  INIT-DC-MDCY-MONTH         PIC 99.
00193              20  INIT-DC-MDCY-DAY           PIC 99.
00194              20  INIT-DC-MDCY-CEN           PIC 99.
00195              20  INIT-DC-MDCY-YEAR          PIC 99.
CIDMOD    12  DC-FORCE-EL310-DATE-SW         PIC X    VALUE SPACE.
CIDMOD        88  DC-FORCE-EL310-DATE                 VALUE 'Y'.
CIDMOD    12  DC-EL310-DATE                  PIC X(21).
CIDMOD    12  FILLER                         PIC X(28).
00314
00315      EJECT
00316 *    COPY ELCATTR.
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
00317      EJECT
00318 *    COPY ELCAID.
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
00319      EJECT
00320 *    COPY EL009S.
       01  EL009AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  SDATEL PIC S9(0004) COMP.
           05  SDATEF PIC  X(0001).
           05  FILLER REDEFINES SDATEF.
               10  SDATEA PIC  X(0001).
           05  SDATEI PIC  X(0009).
      *    -------------------------------
           05  STIMEL PIC S9(0004) COMP.
           05  STIMEF PIC  X(0001).
           05  FILLER REDEFINES STIMEF.
               10  STIMEA PIC  X(0001).
           05  STIMEI PIC  X(0005).
      *    -------------------------------
           05  BSTRINL PIC S9(0004) COMP.
           05  BSTRINF PIC  X(0001).
           05  FILLER REDEFINES BSTRINF.
               10  BSTRINA PIC  X(0001).
           05  BSTRINI PIC  X(0004).
      *    -------------------------------
           05  BENDINL PIC S9(0004) COMP.
           05  BENDINF PIC  X(0001).
           05  FILLER REDEFINES BENDINF.
               10  BENDINA PIC  X(0001).
           05  BENDINI PIC  X(0004).
      *    -------------------------------
           05  JINL PIC S9(0004) COMP.
           05  JINF PIC  X(0001).
           05  FILLER REDEFINES JINF.
               10  JINA PIC  X(0001).
           05  JINI PIC  X(0005).
      *    -------------------------------
           05  GEDTINL PIC S9(0004) COMP.
           05  GEDTINF PIC  X(0001).
           05  FILLER REDEFINES GEDTINF.
               10  GEDTINA PIC  X(0001).
           05  GEDTINI PIC  X(0008).
      *    -------------------------------
           05  GMDYINL PIC S9(0004) COMP.
           05  GMDYINF PIC  X(0001).
           05  FILLER REDEFINES GMDYINF.
               10  GMDYINA PIC  X(0001).
           05  GMDYINI PIC  X(0006).
      *    -------------------------------
           05  GYMDINL PIC S9(0004) COMP.
           05  GYMDINF PIC  X(0001).
           05  FILLER REDEFINES GYMDINF.
               10  GYMDINA PIC  X(0001).
           05  GYMDINI PIC  X(0006).
      *    -------------------------------
           05  EMOSINL PIC S9(0004) COMP.
           05  EMOSINF PIC  X(0001).
           05  FILLER REDEFINES EMOSINF.
               10  EMOSINA PIC  X(0001).
           05  EMOSINI PIC  S9(6).
      *    -------------------------------
           05  MONENDL PIC S9(0004) COMP.
           05  MONENDF PIC  X(0001).
           05  FILLER REDEFINES MONENDF.
               10  MONENDA PIC  X(0001).
           05  MONENDI PIC  X(0001).
      *    -------------------------------
           05  EDAYINL PIC S9(0004) COMP.
           05  EDAYINF PIC  X(0001).
           05  FILLER REDEFINES EDAYINF.
               10  EDAYINA PIC  X(0001).
           05  EDAYINI PIC  S9(6).
      *    -------------------------------
           05  CENTURYL PIC S9(0004) COMP.
           05  CENTURYF PIC  X(0001).
           05  FILLER REDEFINES CENTURYF.
               10  CENTURYA PIC  X(0001).
           05  CENTURYI PIC  X(0001).
      *    -------------------------------
           05  BOUTL PIC S9(0004) COMP.
           05  BOUTF PIC  X(0001).
           05  FILLER REDEFINES BOUTF.
               10  BOUTA PIC  X(0001).
           05  BOUTI PIC  X(0004).
      *    -------------------------------
           05  BOUT2L PIC S9(0004) COMP.
           05  BOUT2F PIC  X(0001).
           05  FILLER REDEFINES BOUT2F.
               10  BOUT2A PIC  X(0001).
           05  BOUT2I PIC  X(0004).
      *    -------------------------------
           05  DAY1L PIC S9(0004) COMP.
           05  DAY1F PIC  X(0001).
           05  FILLER REDEFINES DAY1F.
               10  DAY1A PIC  X(0001).
           05  DAY1I PIC  X(0009).
      *    -------------------------------
           05  DAY2L PIC S9(0004) COMP.
           05  DAY2F PIC  X(0001).
           05  FILLER REDEFINES DAY2F.
               10  DAY2A PIC  X(0001).
           05  DAY2I PIC  X(0009).
      *    -------------------------------
           05  GEDTOUTL PIC S9(0004) COMP.
           05  GEDTOUTF PIC  X(0001).
           05  FILLER REDEFINES GEDTOUTF.
               10  GEDTOUTA PIC  X(0001).
           05  GEDTOUTI PIC  X(0008).
      *    -------------------------------
           05  GEDOUT2L PIC S9(0004) COMP.
           05  GEDOUT2F PIC  X(0001).
           05  FILLER REDEFINES GEDOUT2F.
               10  GEDOUT2A PIC  X(0001).
           05  GEDOUT2I PIC  X(0008).
      *    -------------------------------
           05  JOUTL PIC S9(0004) COMP.
           05  JOUTF PIC  X(0001).
           05  FILLER REDEFINES JOUTF.
               10  JOUTA PIC  X(0001).
           05  JOUTI PIC  X(0005).
      *    -------------------------------
           05  ALFOUTL PIC S9(0004) COMP.
           05  ALFOUTF PIC  X(0001).
           05  FILLER REDEFINES ALFOUTF.
               10  ALFOUTA PIC  X(0001).
           05  ALFOUTI PIC  X(0025).
      *    -------------------------------
           05  EMOSOUTL PIC S9(0004) COMP.
           05  EMOSOUTF PIC  X(0001).
           05  FILLER REDEFINES EMOSOUTF.
               10  EMOSOUTA PIC  X(0001).
           05  EMOSOUTI PIC  X(0006).
      *    -------------------------------
           05  ODDOUTL PIC S9(0004) COMP.
           05  ODDOUTF PIC  X(0001).
           05  FILLER REDEFINES ODDOUTF.
               10  ODDOUTA PIC  X(0001).
           05  ODDOUTI PIC  X(0006).
      *    -------------------------------
           05  EDAYOUTL PIC S9(0004) COMP.
           05  EDAYOUTF PIC  X(0001).
           05  FILLER REDEFINES EDAYOUTF.
               10  EDAYOUTA PIC  X(0001).
           05  EDAYOUTI PIC  X(0006).
      *    -------------------------------
           05  EDAYMONL PIC S9(0004) COMP.
           05  EDAYMONF PIC  X(0001).
           05  FILLER REDEFINES EDAYMONF.
               10  EDAYMONA PIC  X(0001).
           05  EDAYMONI PIC  X(0002).
      *    -------------------------------
           05  ERROUTL PIC S9(0004) COMP.
           05  ERROUTF PIC  X(0001).
           05  FILLER REDEFINES ERROUTF.
               10  ERROUTA PIC  X(0001).
           05  ERROUTI PIC  X(0071).
       01  EL009AO REDEFINES EL009AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SDATEO PIC  X(0009).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STIMEO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTRINO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENDINO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JINO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GEDTINO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GMDYINO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GYMDINO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EMOSINO PIC  9(5)-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MONENDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EDAYINO PIC  9(5)-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CENTURYO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BOUTO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BOUT2O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DAY1O PIC  X(0009).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DAY2O PIC  X(0009).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GEDTOUTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GEDOUT2O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  JOUTO PIC  9(5).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFOUTO PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EMOSOUTO PIC  9(5)-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ODDOUTO PIC  9(5)-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EDAYOUTO PIC  9(5)-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EDAYMONO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERROUTO PIC  X(0071).
      *    -------------------------------
00321
00322 /
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
       01  DFHCOMMAREA       PIC X(01).
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL009' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00324 ***  Y2K, PROJ 7744
00325      INITIALIZE  DATE-CONVERSION-DATA-INIT
00326                  BINARY-WORK
00327                  HEXADECIMAL-DAYS
00328
00329      MOVE +200                  TO INIT-DC-COMM-LENGTH
00330
00331      MOVE SPACES                TO DC-ERROR-CODE.
00332
00333      MOVE ZEROS                 TO DC-ELAPSED-MONTHS
00334                                    DC-ODD-DAYS-OVER
00335                                    DC-ELAPSED-DAYS
00336                                    DC-JULIAN-YEAR
00337                                    DC-JULIAN-DAYS
00338                                    DC-DAYS-IN-MONTH.
00339
00340      MOVE LOW-VALUES            TO DC-BIN-DATE-1
00341                                    DC-BIN-DATE-2
00342                                    INIT-DC-BIN-DATE-1
00343                                    INIT-DC-BIN-DATE-2.
00344 ***  Y2K, PROJ 7744
00345
00346      MOVE EIBDATE               TO W-CURRENT-DATE.
00347 *    ACCEPT W-CURRENT-DATE FROM DATE.
00348 ***  Y2K, PROJ 7744
00349      MOVE '1'                  TO FIND-CENTURY-FLAG
00350      MOVE W-JULIAN-CEN         TO SEARCH-CENTURY-CD-N
00351                                   W-FLOAT-YEAR-CD.
00352      PERFORM 0800-FIND-CENTURY
00353      IF NO-CONVERSION-ERROR
00354          MOVE FOUND-CENTURY-N  TO CURRENT-CENTURY-1-N
00355      END-IF.
00356 ***  Y2K, PROJ 7744
00357      COMPUTE W-FLOAT-YEAR = W-CD-CCYY -
00358                            ((W-JULIAN-CEN*100)+ 70)
00359
00360      MOVE W-CURRENT-DATE        TO DC-JULIAN-YYDDD.
00361      MOVE '5'                   TO DC-OPTION-CODE.
00362      PERFORM 8500-DATE-CONVERSION.
00363      MOVE DC-GREG-DATE-1-EDIT   TO SAVE-DATE.
00364      MOVE DC-BIN-DATE-1         TO SAVE-BIN-DATE.
00365
00366      MOVE DATE-CONVERSION-DATA-INIT  TO  DATE-CONVERSION-DATA
00367
00368      
      * EXEC CICS HANDLE AID
00369 *        CLEAR   (8950-CLEAR-RETURN)
00370 *        PF1     (4010-BIN-TO-GREG)
00371 *        PF2     (4020-COMPUTE-ELAPSED)
00372 *        PF6     (4020-COMPUTE-ELAPSED)
00373 *        PF3     (4030-GREG-TO-BIN)
00374 *        PF4     (4040-JULIAN-TO-OTHER)
00375 *        PF5     (4050-BINARY-PLUS-ELAPSED)
00376 *        ANYKEY  (4060-UNSUPPORTED)
00377 *        END-EXEC.
      *    MOVE '"&=$&*''()@           V! " #00001369' TO DFHEIV0
           MOVE X'22263D24262A272829402020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020562120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031333639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00378
00379      
      * EXEC CICS HANDLE CONDITION
00380 *        MAPFAIL (8900-MAPFAIL)
00381 *        END-EXEC.
      *    MOVE '"$?                   ! # #00001380' TO DFHEIV0
           MOVE X'22243F202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303031333830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00382
00383      IF EIBCALEN GREATER THAN ZERO
00384          GO TO 0300-RECEIVE-MAP.
00385
00386      MOVE LOW-VALUES             TO EL009AO.
00387      
      * EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)
00388 *    END-EXEC
      *    MOVE '0"A                   "   #00001388' TO DFHEIV0
           MOVE X'302241202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00389      
      * EXEC CICS FORMATTIME
00390 *              ABSTIME(LCP-CICS-TIME)
00391 *              TIME(LCP-TIME-OF-DAY-XX)
00392 *    END-EXEC
      *    MOVE 'j$(     (             #   #00001390' TO DFHEIV0
           MOVE X'6A2428202020202028202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME, 
                 LCP-TIME-OF-DAY-XX
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00393      MOVE  LCP-TIME-OF-DAY-68 TO TIME-IN.
00394      MOVE TIME-HHMM              TO TIME-OUT.
00395      MOVE TIME-OUT               TO STIMEO.
00396      MOVE SAVE-DATE              TO SDATEO.
00397
00398  0100-SEND-INITIAL-MAP.
00399      MOVE WS-IC                  TO BSTRINL.
00400
00401      
      * EXEC CICS SEND
00402 *        MAP     ('EL009A')
00403 *        MAPSET  ('EL009S')
00404 *        ERASE
00405 *        FREEKB
00406 *        CURSOR
00407 *        END-EXEC.
           MOVE 'EL009A' TO DFHEIV1
           MOVE -1
             TO DFHEIV11
           MOVE 'EL009S' TO DFHEIV2
      *    MOVE '8$     CT  E F  H     ,   #00001402' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202020202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL009AO, 
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
           
00408
00409      GO TO 0250-RETURN.
00410
00411  0200-SEND-MAP-DATAONLY.
00412      MOVE WS-IC                  TO BSTRINL.
00413
00414      
      * EXEC CICS SEND
00415 *        MAP     ('EL009A')
00416 *        MAPSET  ('EL009S')
00417 *        DATAONLY
00418 *        FREEKB
00419 *        CURSOR
00420 *        END-EXEC.
           MOVE 'EL009A' TO DFHEIV1
           MOVE -1
             TO DFHEIV11
           MOVE 'EL009S' TO DFHEIV2
      *    MOVE '8$D    CT    F  H     ,   #00001415' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'204620204820202020202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL009AO, 
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
           
00421      MOVE LOW-VALUES             TO EL009AO.
00422
00423  0250-RETURN.
00424      
      * EXEC CICS RETURN
00425 *        TRANSID  (EIBTRNID)
00426 *        COMMAREA (DATE-CONVERSION-DATA)
00427 *        LENGTH   (DC-COMM-LENGTH) END-EXEC.
      *    MOVE '.(CT                  &   #00001425' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EIBTRNID, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00428
00429
00430  0300-RECEIVE-MAP.
00431      
      * EXEC CICS RECEIVE
00432 *        MAP     ('EL009A')
00433 *        MAPSET  ('EL009S')
00434 *        END-EXEC.
           MOVE 'EL009A' TO DFHEIV1
           MOVE 'EL009S' TO DFHEIV2
      *    MOVE '8"T                   ''   #00001432' TO DFHEIV0
           MOVE X'382254202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EL009AI, 
                 DFHEIV99, 
                 DFHEIV2, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00435
00436      EJECT
00437  4010-BIN-TO-GREG.
00438
00439      IF  CENTURYL GREATER THAN ZERO
00440          IF  CENTURYI EQUAL '1' OR '2' OR ' '
00441              MOVE CENTURYI       TO  DC-CENTURY-ADJUSTMENT
00442          ELSE
00443              GO TO 6000-INPUT-ERROR.
00444
00445      IF BSTRINL NOT = ZERO
00446          MOVE BSTRINI            TO HEXADECIMAL-DAYS
00447          MOVE ZERO               TO BINARY-WORK
00448          SET U, V, W TO 1
00449          PERFORM 7500-FIND-HEX-CHAR THRU 7999-EXIT
00450          MOVE BINARY-DTE         TO DC-BIN-DATE-1
00451          MOVE SPACE              TO DC-OPTION-CODE
00452          GO TO 5000-FORMAT-OUTPUT
00453      ELSE
00454          GO TO 6000-INPUT-ERROR.
00455
00456  4020-COMPUTE-ELAPSED.
00457      IF BSTRINL = ZERO OR
00458         BENDINL = ZERO
00459          GO TO 6000-INPUT-ERROR.
00460
00461      IF  CENTURYL GREATER THAN ZERO
00462
00463          IF  CENTURYI EQUAL '1' OR '2' OR ' '
00464              MOVE CENTURYI       TO  DC-CENTURY-ADJUSTMENT
00465
00466          ELSE
00467              GO TO 6000-INPUT-ERROR.
00468
00469      MOVE BSTRINI                TO HEXADECIMAL-DAYS.
00470      MOVE ZERO                   TO BINARY-WORK.
00471      SET U, V, W TO 1.
00472      PERFORM 7500-FIND-HEX-CHAR THRU 7999-EXIT.
00473      MOVE BINARY-DTE             TO DC-BIN-DATE-1.
00474      MOVE BENDINI                TO HEXADECIMAL-DAYS.
00475      MOVE ZERO                   TO BINARY-WORK.
00476      SET U, V, W TO 1.
00477      PERFORM 7500-FIND-HEX-CHAR THRU 7999-EXIT.
00478      MOVE BINARY-DTE             TO DC-BIN-DATE-2.
00479
00480      IF EIBAID = DFHPF6
00481          MOVE '7'                TO DC-OPTION-CODE
00482      ELSE
00483          MOVE '1'                TO DC-OPTION-CODE.
00484
00485      GO TO 5000-FORMAT-OUTPUT.
00486
00487  4030-GREG-TO-BIN.
00488
00489      IF  CENTURYL GREATER THAN ZERO
00490
00491          IF  CENTURYI EQUAL '1' OR '2' OR ' '
00492              MOVE CENTURYI       TO  DC-CENTURY-ADJUSTMENT
00493
00494          ELSE
00495              GO TO 6000-INPUT-ERROR.
00496
00497      IF GEDTINL = 8
00498          MOVE GEDTINI            TO DC-GREG-DATE-1-EDIT
00499          MOVE '2'                TO DC-OPTION-CODE
00500          GO TO 5000-FORMAT-OUTPUT.
00501
00502      IF GMDYINL = 6
00503          MOVE GMDYINI            TO DC-GREG-DATE-1-MDY
00504          MOVE '4'                TO DC-OPTION-CODE
00505          GO TO 5000-FORMAT-OUTPUT.
00506
00507      IF GYMDINL = 6
00508          MOVE GYMDINI            TO DC-GREG-DATE-1-YMD
00509          MOVE '3'                TO DC-OPTION-CODE
00510          GO TO 5000-FORMAT-OUTPUT.
00511
00512      GO TO 6000-INPUT-ERROR.
00513
00514  4040-JULIAN-TO-OTHER.
00515      IF JINL = ZERO
00516          GO TO 6000-INPUT-ERROR
00517      ELSE
00518          MOVE JINI               TO DC-JULIAN-YYDDD
00519          MOVE '5'                TO DC-OPTION-CODE.
00520
00521      IF  CENTURYL GREATER THAN ZERO
00522
00523          IF  CENTURYI EQUAL '1' OR '2' OR ' '
00524              MOVE CENTURYI       TO  DC-CENTURY-ADJUSTMENT
00525
00526          ELSE
00527              GO TO 6000-INPUT-ERROR.
00528
00529      GO TO 5000-FORMAT-OUTPUT.
00530
00531  4050-BINARY-PLUS-ELAPSED.
00532      IF BSTRINL = ZERO
00533          GO TO 6000-INPUT-ERROR.
00534
00535      IF MONENDL GREATER THAN ZERO
00536          MOVE '1'                TO  DC-END-OF-MONTH.
00537
00538      IF  CENTURYL GREATER THAN ZERO
00539          MOVE '1'                TO  DC-CENTURY-ADJUSTMENT.
00540
00541      IF EMOSINL GREATER THAN ZERO
00542          
      * EXEC CICS BIF DEEDIT
00543 *            FIELD  (EMOSINI)
00544 *            LENGTH (6) END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00001543' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EMOSINI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00545          IF EMOSINI IS NOT NUMERIC
00546              GO TO 6200-NON-NUMERIC-DATA
00547          ELSE
00548              MOVE EMOSINI        TO  DC-ELAPSED-MONTHS
00549                                      EMOSINO.
00550
00551      IF EDAYINL GREATER THAN ZERO
00552          
      * EXEC CICS BIF DEEDIT
00553 *            FIELD  (EDAYINI)
00554 *            LENGTH (6) END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00001553' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EDAYINI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00555          IF EDAYINI IS NOT NUMERIC
00556              GO TO 6200-NON-NUMERIC-DATA
00557          ELSE
00558              MOVE EDAYINI        TO  DC-ELAPSED-DAYS
00559                                      EDAYINO.
00560
00561      MOVE BSTRINI                TO HEXADECIMAL-DAYS.
00562      MOVE ZERO                   TO BINARY-WORK.
00563      SET U V W TO 1.
00564      PERFORM 7500-FIND-HEX-CHAR THRU 7999-EXIT.
00565      MOVE BINARY-DTE             TO DC-BIN-DATE-1.
00566      MOVE '6'                    TO DC-OPTION-CODE.
00567      GO TO 5000-FORMAT-OUTPUT.
00568
00569  4060-UNSUPPORTED.
00570      MOVE 'KEY PRESSED IS NOT SUPPORTED - PF1 THRU PF4 OR CLEAR'
00571                                  TO ERROUTO.
00572      GO TO 0200-SEND-MAP-DATAONLY.
00573
00574      EJECT
00575  5000-FORMAT-OUTPUT.
00576      PERFORM 8500-DATE-CONVERSION
00577
00578 ***  Y2K, PROJ 7744
00579      MOVE DC-GREG-DATE-1-EDIT    TO GEDTOUTO.
00580      MOVE DC-GREG-DATE-2-EDIT    TO GEDOUT2O.
00581
00582      IF GEDOUT2O = '  /  /  ' OR '00/00/00'
00583          MOVE '        '         TO GEDOUT2O
00584      END-IF.
00585
00586      MOVE DC-JULIAN-YYDDD        TO JOUTO.
00587      MOVE DC-GREG-DATE-1-ALPHA   TO ALFOUTO.
00588
00589      MOVE DC-ELAPSED-MONTHS      TO EMOSOUTO.
00590      MOVE DC-ODD-DAYS-OVER       TO ODDOUTO.
00591      MOVE DC-ELAPSED-DAYS        TO EDAYOUTO.
00592      MOVE DC-DAYS-IN-MONTH       TO EDAYMONO.
00593
00594      IF DC-DAY-OF-WEEK NOT = ZERO
00595          MOVE WS-DAY-OF-WEEK (DC-DAY-OF-WEEK)  TO  DAY1O
00596      ELSE
00597          MOVE SPACES                           TO  DAY1O
00598      END-IF
00599
00600      IF DC-DAY-OF-WEEK2 NOT = ZERO
00601          MOVE WS-DAY-OF-WEEK (DC-DAY-OF-WEEK2) TO  DAY2O
00602      ELSE
00603          MOVE SPACES                           TO  DAY2O
00604      END-IF
00605
00606      MOVE ZERO                   TO HEXADECIMAL-DAYS BINARY-WORK.
00607      MOVE DC-BIN-DATE-1          TO BINARY-DTE.
00608      SET V W TO 1.
00609      PERFORM 7000-GENERATE-HEX-CHARACTERS THRU 7499-EXIT.
00610
00611      IF HEXADECIMAL-DAYS NOT = ZEROS
00612          MOVE HEXADECIMAL-DAYS   TO BOUTO
00613      ELSE
00614          MOVE SPACES             TO BOUTO
00615      END-IF
00616
00617      MOVE ZERO                   TO HEXADECIMAL-DAYS BINARY-WORK.
00618      MOVE DC-BIN-DATE-2          TO BINARY-DTE.
00619      SET V W TO 1.
00620      PERFORM 7000-GENERATE-HEX-CHARACTERS THRU 7499-EXIT.
00621
00622      IF HEXADECIMAL-DAYS NOT = ZEROS
00623          MOVE HEXADECIMAL-DAYS   TO BOUT2O
00624      ELSE
00625          MOVE SPACES             TO BOUT2O
00626      END-IF
00627 ***  Y2K, PROJ 7744
00628
00629      IF DC-ERROR-CODE = SPACE
00630          MOVE 'DATE CONVERSION SUCCESSFUL   RETURN CODE SPACE'
00631                                  TO ERROUTO.
00632
00633      IF DC-ERROR-CODE = '1'
00634          MOVE 'DATE CONVERSION ERROR (DATE IS ZEROCODE = 1)'
00635                                  TO ERROUTO.
00636
00637      IF DC-ERROR-CODE = '2'
00638          MOVE 'DATE CONVERSION ERROR (DATE NOT NUMERICCODE = 2)'
00639                                  TO ERROUTO.
00640
00641      IF DC-ERROR-CODE = '3'
00642          MOVE 'DATE CONVERSION ERROR (DATE IS INVALIDCODE = 3)'
00643                                  TO ERROUTO.
00644
00645      IF DC-ERROR-CODE = '4'
00646          MOVE 'DATE CONVERSION ERROR (DATE1 IS AFTER DATE2 = 4)'
00647                                  TO ERROUTO.
00648
00649      IF DC-ERROR-CODE = '5'
00650          MOVE 'DATE CONVERSION ERROR (BIN PLUS ELAPSED NEG = 5)'
00651                                  TO ERROUTO.
00652
00653      IF DC-ERROR-CODE = '9'
00654          MOVE 'DATE CONVERSION ERROR = 9)'
00655                                  TO ERROUTO.
00656
00657      GO TO 0200-SEND-MAP-DATAONLY.
00658
00659      EJECT
00660  6000-INPUT-ERROR.
00661      MOVE 'DATA GIVEN IS NOT CONSISTENT WITH REQUEST - RE-ENTER'
00662          TO ERROUTO.
00663      INSPECT GEDTINI CONVERTING ' ' TO LOW-VALUES
00664      INSPECT GMDYINI CONVERTING ' ' TO LOW-VALUES
00665      INSPECT GYMDINI CONVERTING ' ' TO LOW-VALUES
00666      GO TO 0200-SEND-MAP-DATAONLY.
00667
00668
00669  6200-NON-NUMERIC-DATA.
00670      MOVE 'ELAPSED MONTHS OR DAYS IS NON-NUMERIC - RE-ENTER'
00671          TO ERROUTO.
00672      GO TO 0200-SEND-MAP-DATAONLY.
00673      EJECT
00674
00675  7000-GENERATE-HEX-CHARACTERS.
00676      IF BINARY-WORK LESS THAN DECIMAL-VALUE (V, W)
00677          IF W LESS THAN 4
00678              SET W UP BY 1
00679              GO TO 7000-GENERATE-HEX-CHARACTERS
00680          ELSE
00681              GO TO 7499-EXIT.
00682
00683      SET V TO 15.
00684
00685  7050-GET-HEXADECIMAL-DIGIT.
00686      IF BINARY-WORK NOT LESS THAN DECIMAL-VALUE (V, W)
00687          SET U TO W
00688          MOVE HEX-VALUE (V)      TO HEX-CHARACTER (U)
00689          COMPUTE BINARY-WORK =
00690                        BINARY-WORK - DECIMAL-VALUE (V, W)
00691          SET V TO 1
00692          GO TO 7000-GENERATE-HEX-CHARACTERS.
00693
00694      SET V DOWN BY 1.
00695      GO TO 7050-GET-HEXADECIMAL-DIGIT.
00696
00697  7499-EXIT.
00698      EXIT.
00699
00700  7500-FIND-HEX-CHAR.
00701      IF U = 5
00702          GO TO 7999-EXIT.
00703
00704      IF HEX-CHARACTER (U) = HEX-VALUE (V)
00705          GO TO 7550-ADD-VALUE-IN.
00706
00707      IF V LESS 15
00708          SET V UP BY 1
00709          GO TO 7500-FIND-HEX-CHAR
00710      ELSE
00711          SET U, W UP BY 1
00712          SET V TO 1
00713          GO TO 7500-FIND-HEX-CHAR.
00714
00715  7550-ADD-VALUE-IN.
00716      COMPUTE BINARY-WORK = BINARY-WORK + DECIMAL-VALUE (V, W).
00717      SET U, W UP BY 1.
00718      SET V TO 1.
00719      GO TO 7500-FIND-HEX-CHAR.
00720
00721  7999-EXIT.
00722      EXIT.
00723
00724  8500-DATE-CONVERSION SECTION.
00725      
      * EXEC CICS ASKTIME
00726 *        END-EXEC.
      *    MOVE '0"                    "   #00001726' TO DFHEIV0
           MOVE X'302220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00727
uktdel*8510-DATE-CONVERSION. COPY ELCDATP1.
uktins 8510-DATE-CONVERSION.
uktins*    COPY ELCDATP1.
00001 *****************************************************************
00002 *                                                               *
00002 *                                                               *
00003 *                            ELCDATP1.                          *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                         *
00006 *                                                               *
00007 *****************************************************************
00008
CIDMOD     IF NOT FIND-CENTURY
CIDMOD        MOVE GREG-ALPHA-MASK   TO DC-GREG-DATE-1-ALPHA
CIDMOD     END-IF
CIDMOD
00009      MOVE 2                    TO FIND-CENTURY-FLAG.
00010      MOVE CURRENT-CENTURY-1-N  TO SEARCH-CENTURY-N
00011      PERFORM 0800-FIND-CENTURY.
00012
00013      MOVE FOUND-CENTURY-CD-N   TO CONTROL-YEAR-CD
00014                                   W-CD-YEAR-CD.
00015 *Y2K
00016 *                                 W-FLOAT-YEAR-CD.
00017
00018      COMPUTE BREAK-PERIOD = (FOUND-CENTURY-CD-N * 100) + 69
00019
00020      EVALUATE TRUE
00021         WHEN W-CD-CCYY < BREAK-PERIOD
00022            COMPUTE CONTROL-YEAR-CD = FOUND-CENTURY-CD-N - 1
00023            ADD +1 TO CURRENT-CENTURY-1-N
00024                             GIVING NEXT-CENTURY
00025            SUBTRACT +1 FROM CURRENT-CENTURY-1-N
00026                             GIVING PREVIOUS-CENTURY
00027         WHEN OTHER
00028            MOVE FOUND-CENTURY-CD-N TO CONTROL-YEAR-CD
00029            COMPUTE CENTURY-MARK = 100 +
00030                            (FOUND-CENTURY-CD-N * 100)
00031            ADD +1 TO CURRENT-CENTURY-1-N
00032                             GIVING NEXT-CENTURY
00033            SUBTRACT +1 FROM CURRENT-CENTURY-1-N
00034                             GIVING PREVIOUS-CENTURY
00035      END-EVALUATE.
00036
00037      MOVE SPACES                 TO DC-ERROR-CODE.
00038
00039      MOVE ZERO                   TO  BIN-DATE.
00040
00041      EVALUATE TRUE
00042         WHEN BIN-TO-GREG
00043              OR ELAPSED-BETWEEN-BIN
00044              OR BIN-PLUS-ELAPSED
00045            MOVE DC-BIN-DATE-1    TO  BIN-DATE-WORK
00046         WHEN BIN-3-TO-GREG
00047              OR ELAPSED-BETWEEN-BIN-3
00048              OR BIN-TO-JULIAN-EXPANDED
00049              OR BIN-PLUS-ELAPSED-3
00050            MOVE DC-BIN-DATE-EXPAND-1 TO BIN-DATE-3-WORK
00051      END-EVALUATE.
00052
00053      MOVE BIN-DATE               TO  WS-BINARY-DATE-1.
00054
00055      MOVE ZERO                   TO  BIN-DATE.
00056
00057      EVALUATE TRUE
00058         WHEN ELAPSED-BETWEEN-BIN
00059            MOVE DC-BIN-DATE-2          TO  BIN-DATE-WORK
00060         WHEN ELAPSED-BETWEEN-BIN-3
00061            MOVE DC-BIN-DATE-EXPAND-2 TO BIN-DATE-3-WORK
00062      END-EVALUATE.
00063
00064      MOVE BIN-DATE               TO  WS-BINARY-DATE-2.
00065
00066      MOVE DC-ELAPSED-MONTHS      TO  WS-ELAPSED-MONTHS.
00067      MOVE DC-ELAPSED-DAYS        TO  WS-ELAPSED-DAYS.
00068
00069      EVALUATE TRUE
00070         WHEN JULIAN-TO-BIN OR
00071              JULIAN-TO-BIN-3
00072            MOVE DC-JULIAN-YEAR   TO  JULIAN-YEAR1
00073            MOVE DC-JULIAN-DAYS   TO  JULIAN-DAY1
00074         WHEN JULIAN-EXPANDED-TO-BIN OR
00075              JULIAN-EXPANDED-TO-BIN-3
00076            MOVE DC-JULIAN-1-CC   TO DC-ALPHA-CEN-N
00077            MOVE DC-JULIAN-1-YR   TO JULIAN-YEAR1
00078            MOVE DC-JULIAN-DA-1   TO JULIAN-DAY1
00079      END-EVALUATE.
00080
00081      EVALUATE TRUE
00082         WHEN BIN-PLUS-ELAPSED OR
00083              BIN-PLUS-ELAPSED-3
00084            PERFORM 0700-CALCULATE-END-DATE
00085         WHEN BIN-TO-GREGORIAN
00086            PERFORM 0300-REFORMAT-BINARY-DAYS
00087         WHEN GREGORIAN-TO-BIN
00088            PERFORM 0400-REFORMAT-GREGORIAN-DATE
00089         WHEN JULIAN-TO-BINARY
00090            PERFORM 0500-REFORMAT-JULIAN-DATE
00091         WHEN FIND-CENTURY
00092            MOVE SPACE         TO DC-ERROR-CODE
00093            MOVE 0             TO YEAR1
00094            MOVE 3             TO FIND-CENTURY-FLAG
00095            MOVE DC-ALPHA-YEAR TO YEAR1
pemuni*          display ' cpdbebug task prior if ' eibtaskn
00096            IF YEAR1 < W-FLOAT-YEAR
pemuni*             display ' cpdebug task inside if ' eibtaskn
00097               ADD 100 TO YEAR1
00098            END-IF
pemuni*          display ' cpdebug task prior  ' eibtaskn
00099            PERFORM 0800-FIND-CENTURY
pemuni*          display ' cpdebug task after  ' eibtaskn
00100            IF NO-CONVERSION-ERROR
00101               MOVE 'B'        TO DC-ERROR-CODE
00102            END-IF
00103         WHEN CHECK-LEAP-YEAR
00104            MOVE HOLD-CEN-1-YY TO CHECK-YEAR
00105            MOVE HOLD-CEN-1-CC TO SEARCH-CENTURY
00106            MOVE 2             TO FIND-CENTURY-FLAG
00107            PERFORM 0800-FIND-CENTURY
00108            IF NO-CONVERSION-ERROR
00109               PERFORM 0900-CHECK-FOR-LEAP-YEAR THRU 0900-EXIT
00110               IF A-LEAP-YEAR
00111                  MOVE LEAP-YR-DAYS (2) TO HOLD-CEN-1-DA
00112               ELSE
00113                  MOVE REGULAR-DAYS (2) TO HOLD-CEN-1-DA
00114               END-IF
00115               MOVE 'C'        TO DC-ERROR-CODE
00116            END-IF
00117         WHEN OTHER
00118            MOVE '9'           TO DC-ERROR-CODE
00119      END-EVALUATE.
00120
00121      IF NO-CONVERSION-ERROR
00122          PERFORM 0200-DATE-CONVERSION-ROUTINES.
00123
00729      
      * EXEC CICS ASKTIME
00730 *        END-EXEC.
      *    MOVE '0"                    "   #00001864' TO DFHEIV0
           MOVE X'302220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00731
00732  8590-EXIT.
00733      EXIT.
00734
00735      EJECT
uktdel*0200-DATE-CONVERSION-ROUTINES SECTION. COPY ELCDATP2 REPLACING
uktins 0200-DATE-CONVERSION-ROUTINES SECTION.
uktins*    COPY ELCDATP2 REPLACING
00737 *    DC-JULIAN-DATE              BY  DC-JULIAN-YYDDD.
CIDMOD* 11-16-98: JJPA -  ADDED CODE TO FIX Y2K ERROR -
CIDMOD* 11-16-98: JJPA -  MOVE CCYY TO DC-EDITB-CENT/-YEAR
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCDATP2.                           *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.005                          *
00006 *                                                                *
00007 *                       DATE CONVERSION ROUTINE                  *
00008 *                                                                *
00009 ******************************************************************
00010
00011      IF BIN-PLUS-ELAPSED  OR
00012         BIN-PLUS-ELAPSED-3
00013          COMPUTE WS-BINARY-DATE-2 = (((YEAR1 * 12) +
00014                                     (MONTH1 - 1)) * 32) + DAY1
00015          MOVE WS-BINARY-DATE-2   TO  BIN-DATE
00016          MOVE BIN-DATE-WORK      TO  DC-BIN-DATE-2
00017          MOVE BIN-DATE-3-WORK    TO  DC-BIN-DATE-EXPAND-2
00018      ELSE
00019          COMPUTE WS-BINARY-DATE-1 = (((YEAR1 * 12) +
00020                                     (MONTH1 - 1)) * 32) + DAY1
00021          MOVE WS-BINARY-DATE-1   TO  BIN-DATE
00022          MOVE BIN-DATE-WORK      TO  DC-BIN-DATE-1
00023          MOVE BIN-DATE-3-WORK    TO  DC-BIN-DATE-EXPAND-1.
00024
00025      SET MTHX TO MONTH1.
00026
CIDMOD*    MOVE GREG-ALPHA-MASK        TO  DC-GREG-DATE-1-ALPHA.
00028
00029      MOVE '/'                    TO  SLASH1-1  SLASH1-2
00030                                      SLASH2-1  SLASH2-2
00031                                      SLASHA-1  SLASHA-2
00032                                      SLASHB-1  SLASHB-2.
00033
00034      EVALUATE TRUE
00035         WHEN ADJUST-DOWN-100-YRS
00036            IF CURRENT-CENTURY-1-N = 19
00037               MOVE 19               TO DC-ALPHA-CEN-N
00038            ELSE
00039               MOVE PREVIOUS-CENTURY TO DC-ALPHA-CEN-N
00040            END-IF
00041         WHEN ADJUST-UP-100-YRS
00042            MOVE NEXT-CENTURY TO DC-ALPHA-CEN-N
00043         WHEN DC-ALPHA-CEN-N > 0
00044            CONTINUE
00045         WHEN OTHER
00046            MOVE 3 TO FIND-CENTURY-FLAG
00047            PERFORM 0800-FIND-CENTURY
00048      END-EVALUATE.
00049
00050      MOVE MONTH1                TO DC-EDIT1-MONTH
00051                                    DC-YMD-MONTH
00052                                    DC-MDY-MONTH
00053                                    DC-CYMD-MONTH
00054                                    DC-MDCY-MONTH
00055                                    DC-EDITA-MONTH
00056                                    DC-EDITB-MONTH.
00057
00058      MOVE NAME-OF-MONTH (MTHX)  TO DC-ALPHA-MONTH.
00059
00060      MOVE DAYS-IN-MONTH (MTHX)  TO DC-DAYS-IN-MONTH.
00061
00062      MOVE DAY1                  TO DC-EDIT1-DAY
00063                                    DC-YMD-DAY
00064                                    DC-MDY-DAY
00065                                    DC-CYMD-DAY
00066                                    DC-MDCY-DAY
00067                                    DC-ALPHA-DAY
00068                                    DC-EDITA-DAY
00069                                    DC-EDITB-DAY.
00070
00071      MOVE YEAR1                 TO DC-EDIT1-YEAR
00072                                    DC-YMD-YEAR
00073                                    DC-MDY-YEAR
00074                                    DC-CYMD-YEAR
00075                                    DC-MDCY-YEAR
00076                                    DC-ALPHA-YEAR
CIDMOD                                   DC-EDITA-YEAR
CIDMOD                                   DC-EDITB-YEAR.
00078
00079      MOVE DC-ALPHA-CEN-N        TO DC-EDITA-CENT
CIDMOD                                   DC-EDITB-CENT
00080                                    DC-CYMD-CEN
00081                                    DC-MDCY-CEN.
00082
00083      IF JULIAN-TO-BIN
00084          MOVE WORK-JULIAN-1     TO WORK-JULIAN
00085          PERFORM 0250-CALCULATE-DAY-OF-WEEK
00086          MOVE DAY-OF-WEEK-X     TO DC-DAY-OF-WEEK
00087          GO TO 0200-EXIT.
00088
00089      MOVE YEAR1                 TO JULIAN-YEAR1.
00090
00091      IF A-LEAP-YEAR
00092          MOVE LEAP-YR-DAYS (MTHX) TO JULIAN-DAY1
00093        ELSE
00094          MOVE REGULAR-DAYS (MTHX) TO JULIAN-DAY1.
00095
00096      ADD DAY1 TO JULIAN-DAY1.
00097
00098      MOVE JULIAN-YEAR1           TO DC-JULIAN-YEAR
00099                                     DC-JULIAN-1-YR.
00100      MOVE JULIAN-DAY1            TO DC-JULIAN-DAYS
00101                                     DC-JULIAN-DA-1.
00102      MOVE DC-ALPHA-CEN-N         TO DC-JULIAN-1-CC.
00103
00104
00105      MOVE WORK-JULIAN-1          TO WORK-JULIAN.
00106      PERFORM 0250-CALCULATE-DAY-OF-WEEK.
00107      MOVE DAY-OF-WEEK-X          TO DC-DAY-OF-WEEK.
00108
00109      IF ELAPSED-BETWEEN-BIN OR
00110         ELAPSED-BETWEEN-BIN-3
00111          MOVE YEAR2              TO YR-2
00112          MOVE MONTH2             TO DC-EDIT2-MONTH
00113          MOVE DAY2               TO DC-EDIT2-DAY
00114          MOVE YEAR2-YR           TO DC-EDIT2-YEAR
00115                                     DC-EDITB-YEAR
00116          MOVE YEAR2-CEN-CD       TO SEARCH-CENTURY-CD-N
00117          MOVE 1                  TO FIND-CENTURY-FLAG
00118          PERFORM 0800-FIND-CENTURY
00119          IF NO-CONVERSION-ERROR
00120             MOVE FOUND-CENTURY-N TO DC-EDITB-CENT
00121          ELSE
00122             GO TO 0200-EXIT
00123          END-IF
00124          PERFORM 0600-ELAPSED-TIME-CALCULATIONS.
00125
00126      IF ELAPSED-BETWEEN-BIN   OR
00127         ELAPSED-BETWEEN-BIN-3 OR
00128         BIN-PLUS-ELAPSED      OR
00129         BIN-PLUS-ELAPSED-3
00130          MOVE WORK-JULIAN-2      TO WORK-JULIAN
00131          PERFORM 0250-CALCULATE-DAY-OF-WEEK
00132          MOVE DAY-OF-WEEK-X      TO DC-DAY-OF-WEEK2
00133        ELSE
00134          MOVE ZERO               TO DC-DAY-OF-WEEK2.
00135
00136  0200-EXIT.
00137      EXIT.
00138
00139      EJECT
00140  0250-CALCULATE-DAY-OF-WEEK SECTION.
00141
00142      COMPUTE WS-DAY-OF-CENTURY =
00143              (JULIAN-YEAR * +365.25) + JULIAN-DAY.
00144
00145      DIVIDE WS-DAY-OF-CENTURY BY +7 GIVING DIVIDE-RESULT
00146              REMAINDER DAY-OF-WEEK-X.
00147
00148      MOVE JULIAN-YEAR        TO  CHECK-YEAR.
00149
00150      PERFORM 0900-CHECK-FOR-LEAP-YEAR.
00151
00152      IF NOT A-LEAP-YEAR
00153          ADD +1  TO  DAY-OF-WEEK-X
00154        ELSE
00155          IF DAY-OF-WEEK-X NOT > ZERO
00156              MOVE +7  TO  DAY-OF-WEEK-X.
00157
00158  0250-EXIT.
00159      EXIT.
00160
00161      EJECT
00162  0300-REFORMAT-BINARY-DAYS SECTION.
00163
00164      MOVE '00/00/00'             TO  DC-GREG-DATE-1-EDIT
00165                                      DC-GREG-DATE-2-EDIT.
00166      MOVE '00/00/0000'           TO  DC-GREG-DATE-A-EDIT
00167                                      DC-GREG-DATE-B-EDIT.
00168
00169      MOVE ZERO                   TO  DC-GREG-DATE-1-YMD
00170                                      DC-GREG-DATE-1-MDY
00171                                      DC-GREG-DATE-CYMD
00172                                      DC-GREG-DATE-MDCY
00173                                      DC-JULIAN-YYDDD
00174                                      DC-ELAPSED-MONTHS
00175                                      DC-ODD-DAYS-OVER
00176                                      DC-ELAPSED-DAYS
00177                                      DC-DAY-OF-WEEK
00178                                      DC-DAY-OF-WEEK2.
00179
00180      IF (ELAPSED-BETWEEN-BIN AND
00181          WS-BINARY-DATE-1 > WS-BINARY-DATE-2)
00182                     OR
00183         (ELAPSED-BETWEEN-BIN-3 AND
00184          WS-BINARY-DATE-1 > WS-BINARY-DATE-2)
00185            MOVE '4'               TO DC-ERROR-CODE
00186            GO TO 0300-EXIT.
00187
00188      IF WS-BINARY-DATE-1 = ZERO
00189          MOVE '1'                TO DC-ERROR-CODE
00190          MOVE ZERO               TO DC-GREG-DATE-1-EDIT
00191          GO TO 0300-EXIT.
00192
00193      DIVIDE WS-BINARY-DATE-1 BY +384 GIVING YEAR1
00194                REMAINDER DAYS-REMAINING.
00195
00196      DIVIDE DAYS-REMAINING BY +32 GIVING MONTH1
00197                REMAINDER DAY1.
00198
00199 ***  Y2K, PROJ 7744
00200      IF DAY1 = ZEROS
00201          MOVE '3'                TO DC-ERROR-CODE
00202          GO TO 0300-EXIT
00203      END-IF
00204 ***  Y2K, PROJ 7744
00205
00206      ADD +1 TO MONTH1.
00207
00208      MOVE YEAR1              TO CHECK-YEAR.
00209      PERFORM 0900-CHECK-FOR-LEAP-YEAR.
00210
00211      IF ELAPSED-BETWEEN-BIN OR
00212         ELAPSED-BETWEEN-BIN-3
00213         CONTINUE
00214      ELSE
00215         GO TO 0300-EXIT.
00216
00217      IF WS-BINARY-DATE-2 = ZERO
00218          MOVE '1'               TO DC-ERROR-CODE
00219          GO TO 0300-EXIT.
00220
00221      DIVIDE WS-BINARY-DATE-2 BY +384 GIVING YEAR2
00222                                  REMAINDER DAYS-REMAINING.
00223
00224      DIVIDE DAYS-REMAINING BY +32 GIVING MONTH2
00225                                  REMAINDER DAY2.
00226
00227      ADD +1 TO MONTH2.
00228
00229  0300-EXIT.
00230      EXIT.
00231
00232      EJECT
00233  0400-REFORMAT-GREGORIAN-DATE SECTION.
00234
00235      MOVE LOW-VALUES             TO  DC-BIN-DATE-1
00236                                      DC-BIN-DATE-2.
00237
00238      EVALUATE TRUE
00239         WHEN EDIT-GREG-TO-BIN OR
00240              EDIT-GREG-TO-BIN-3
00241            IF (DC-EDIT1-MONTH NOT NUMERIC) OR
00242               (DC-EDIT1-DAY   NOT NUMERIC) OR
00243               (DC-EDIT1-YEAR  NOT NUMERIC)
00244              MOVE '2'            TO DC-ERROR-CODE
00245              GO TO 0400-EXIT
00246            END-IF
00247            MOVE DC-EDIT1-MONTH   TO MONTH1
00248            MOVE DC-EDIT1-DAY     TO DAY1
00249            MOVE DC-EDIT1-YEAR    TO YEAR1
00250         WHEN YMD-GREG-TO-BIN OR
00251              YMD-GREG-TO-BIN-3
00252            IF DC-GREG-DATE-1-YMD NOT NUMERIC
00253               MOVE '2'           TO DC-ERROR-CODE
00254               GO TO 0400-EXIT
00255            END-IF
00256            MOVE DC-YMD-YEAR      TO YEAR1
00257            MOVE DC-YMD-MONTH     TO MONTH1
00258            MOVE DC-YMD-DAY       TO DAY1
00259            MOVE 0                TO DC-ALPHA-CEN-N
00260         WHEN MDY-GREG-TO-BIN OR
00261              MDY-GREG-TO-BIN-3
00262            IF DC-GREG-DATE-1-MDY NOT NUMERIC
00263               MOVE '2'           TO DC-ERROR-CODE
00264               GO TO 0400-EXIT
00265            END-IF
00266            MOVE DC-MDY-MONTH     TO MONTH1
00267            MOVE DC-MDY-DAY       TO DAY1
00268            MOVE DC-MDY-YEAR      TO YEAR1
00269            MOVE 0                TO DC-ALPHA-CEN-N
00270         WHEN MDCY-GREG-TO-BIN OR
00271              MDCY-GREG-TO-BIN-3
00272            IF DC-GREG-DATE-MDCY NOT NUMERIC
00273               MOVE '2'           TO DC-ERROR-CODE
00274               GO TO 0400-EXIT
00275            END-IF
00276            MOVE DC-MDCY-MONTH    TO MONTH1
00277            MOVE DC-MDCY-DAY      TO DAY1
00278            MOVE DC-MDCY-YEAR     TO YEAR1
00279            MOVE DC-MDCY-CEN      TO DC-ALPHA-CEN-N
00280         WHEN CYMD-GREG-TO-BIN OR
00281              CYMD-GREG-TO-BIN-3
00282            IF DC-GREG-DATE-CYMD NOT NUMERIC
00283               MOVE '2'           TO DC-ERROR-CODE
00284               GO TO 0400-EXIT
00285            END-IF
00286            MOVE DC-CYMD-CEN      TO DC-ALPHA-CEN-N
00287            MOVE DC-CYMD-YEAR     TO YEAR1
00288            MOVE DC-CYMD-MONTH    TO MONTH1
00289            MOVE DC-CYMD-DAY      TO DAY1
00290      END-EVALUATE
00291
00292      EVALUATE TRUE
00293         WHEN MONTH1 NOT NUMERIC OR
00294              DAY1   NOT NUMERIC OR
00295              YEAR1  NOT NUMERIC
00296               MOVE '3'           TO DC-ERROR-CODE
00297               GO TO 0400-EXIT
00298         WHEN MONTH1 = ZERO  AND
00299              DAY1   = ZERO  AND
00300              YEAR1  = ZERO
00301               MOVE '1'           TO DC-ERROR-CODE
00302               GO TO 0400-EXIT
00303         WHEN MONTH1 < 1 OR > 12
00304               MOVE '3'           TO DC-ERROR-CODE
00305               GO TO 0400-EXIT
00306         WHEN DAY1 < 1 OR > 31
00307               MOVE '3'           TO DC-ERROR-CODE
00308               GO TO 0400-EXIT
00309      END-EVALUATE
00310
00311      EVALUATE TRUE
00312         WHEN ADJUST-DOWN-100-YRS
00313            IF CURRENT-CENTURY-1-N = 19
00314               CONTINUE
00315            ELSE
00316               MOVE '2'              TO FIND-CENTURY-FLAG
00317               MOVE PREVIOUS-CENTURY TO SEARCH-CENTURY-N
00318               PERFORM 0800-FIND-CENTURY
00319               IF NO-CONVERSION-ERROR
00320                  COMPUTE YEAR1 = YEAR1 +
00321                                     (FOUND-CENTURY-CD-N * 100)
00322               ELSE
00323                  GO TO 0400-EXIT
00324               END-IF
00325            END-IF
00326         WHEN ADJUST-UP-100-YRS
00327            MOVE '2'              TO FIND-CENTURY-FLAG
00328            MOVE NEXT-CENTURY TO SEARCH-CENTURY-N
00329            PERFORM 0800-FIND-CENTURY
00330            IF NO-CONVERSION-ERROR
00331               COMPUTE YEAR1 = YEAR1 +
00332                                  (FOUND-CENTURY-CD-N * 100)
00333            ELSE
00334               GO TO 0400-EXIT
00335            END-IF
00336         WHEN DC-ALPHA-CEN-N > 0
00337            MOVE '2'              TO FIND-CENTURY-FLAG
00338            MOVE DC-ALPHA-CEN-N   TO SEARCH-CENTURY-N
00339            PERFORM 0800-FIND-CENTURY
00340            IF NO-CONVERSION-ERROR
00341               COMPUTE YEAR1 = YEAR1 + (FOUND-CENTURY-CD-N * 100)
00342            ELSE
00343               GO TO 0400-EXIT
00344            END-IF
CIDMOD        WHEN YEAR1 < W-FLOAT-YEAR
00346            COMPUTE YEAR1 = YEAR1 + CENTURY-MARK
00347      END-EVALUATE.
00348
00349      MOVE YEAR1                  TO CHECK-YEAR.
00350      PERFORM 0900-CHECK-FOR-LEAP-YEAR THRU 0900-EXIT.
00351
00352      IF DAY1 > DAYS-IN-MONTH (MONTH1)
00353          MOVE '3'                TO DC-ERROR-CODE
00354          GO TO 0400-EXIT.
00355
00356  0400-EXIT.
00357      EXIT.
00358
00359      EJECT
00360  0500-REFORMAT-JULIAN-DATE SECTION.
00361
00362      MOVE '00/00/00'             TO  DC-GREG-DATE-1-EDIT
00363                                      DC-GREG-DATE-2-EDIT.
00364
CIDMOD*    MOVE GREG-ALPHA-MASK        TO  DC-GREG-DATE-1-ALPHA.
00366
00367      MOVE ZERO                   TO  DC-GREG-DATE-1-YMD
00368                                      DC-GREG-DATE-1-MDY
00369                                      DC-GREG-DATE-CYMD
00370                                      DC-GREG-DATE-MDCY
00371                                      DC-ELAPSED-MONTHS
00372                                      DC-ODD-DAYS-OVER
00373                                      DC-ALPHA-CEN-N
00374                                      DC-ELAPSED-DAYS.
00375
00376      MOVE LOW-VALUES             TO  DC-BIN-DATE-1
00377                                      DC-BIN-DATE-2.
00378
00379      IF DC-JULIAN-YYDDD = ZERO
00380          MOVE '1'             TO DC-ERROR-CODE
00381          GO TO 0500-EXIT.
00382
00383      IF DC-JULIAN-DAYS = ZERO
00384          MOVE '3'             TO DC-ERROR-CODE
00385          GO TO 0500-EXIT.
00386
00387      IF JULIAN-EXPANDED-TO-BIN OR
00388         JULIAN-EXPANDED-TO-BIN-3
00389           MOVE DC-JULIAN-1-CC   TO DC-ALPHA-CEN-N.
00390
00391      EVALUATE TRUE
00392         WHEN ADJUST-DOWN-100-YRS
00393            IF CURRENT-CENTURY-1-N = 19
00394               CONTINUE
00395            ELSE
00396               MOVE '2'              TO FIND-CENTURY-FLAG
00397               MOVE PREVIOUS-CENTURY TO SEARCH-CENTURY-N
00398               PERFORM 0800-FIND-CENTURY
00399               IF NO-CONVERSION-ERROR
00400                  COMPUTE JULIAN-YEAR1 = JULIAN-YEAR1 +
00401                                     (FOUND-CENTURY-CD-N * 100)
00402               ELSE
00403                  GO TO 0500-EXIT
00404               END-IF
00405            END-IF
00406         WHEN ADJUST-UP-100-YRS
00407            MOVE '2'              TO FIND-CENTURY-FLAG
00408            MOVE NEXT-CENTURY     TO SEARCH-CENTURY-N
00409            PERFORM 0800-FIND-CENTURY
00410            IF NO-CONVERSION-ERROR
00411               COMPUTE JULIAN-YEAR1 = JULIAN-YEAR1 +
00412                                  (FOUND-CENTURY-CD-N * 100)
00413            ELSE
00414               GO TO 0500-EXIT
00415            END-IF
00416         WHEN DC-ALPHA-CEN-N > 0
00417            MOVE DC-ALPHA-CEN-N        TO SEARCH-CENTURY-N
00418            MOVE 2                     TO FIND-CENTURY-FLAG
00419            PERFORM 0800-FIND-CENTURY
00420            IF NO-CONVERSION-ERROR
00421               COMPUTE JULIAN-YEAR1 =
00422                       JULIAN-YEAR1 + (FOUND-CENTURY-CD-N * 100)
00423            ELSE
00424               GO TO 0500-EXIT
00425            END-IF
00426          WHEN JULIAN-YEAR1 NOT > W-FLOAT-YEAR
00427                  COMPUTE JULIAN-YEAR1 =
00428                             JULIAN-YEAR1 + 100
00429      END-EVALUATE.
00430
00431      MOVE JULIAN-YEAR1           TO YEAR1
00432                                     CHECK-YEAR.
00433
00434      PERFORM 0900-CHECK-FOR-LEAP-YEAR THRU 0900-EXIT.
00435
00436      IF (A-LEAP-YEAR AND JULIAN-DAY1 > LEAP-YR-DAYS (13))
00437                        OR
00438         (NOT A-LEAP-YEAR AND DC-JULIAN-DAYS > REGULAR-DAYS (13))
00439            MOVE '3'              TO DC-ERROR-CODE
00440            GO TO 0500-EXIT.
00441
00442      SET MTHX TO +1.
00443
00444      IF A-LEAP-YEAR
00445          PERFORM 0550-JULIAN-LEAP-YEAR
00446      ELSE
00447          PERFORM 0575-JULIAN-REGULAR-YEAR
00448      END-IF
00449      .
00450  0500-EXIT.
00451      EXIT.
00452 /
00453
00454  0550-JULIAN-LEAP-YEAR SECTION.
00455
00456      IF JULIAN-DAY1 NOT > LEAP-YR-DAYS (MTHX)
00457          SET MTHX DOWN BY +1
00458          SET MONTH1 TO MTHX
00459          SUBTRACT LEAP-YR-DAYS (MTHX) FROM JULIAN-DAY1
00460                                       GIVING DAY1
00461          MOVE JULIAN-YEAR1       TO  YEAR1
00462        ELSE
00463          SET MTHX UP BY +1
00464          GO TO 0550-JULIAN-LEAP-YEAR.
00465
00466  0550-EXIT.
00467      EXIT.
00468
00469
00470  0575-JULIAN-REGULAR-YEAR SECTION.
00471
00472      IF JULIAN-DAY1 NOT > REGULAR-DAYS (MTHX)
00473          SET MTHX DOWN BY +1
00474          SET MONTH1 TO MTHX
00475          SUBTRACT REGULAR-DAYS (MTHX) FROM JULIAN-DAY1
00476                                       GIVING DAY1
00477          MOVE JULIAN-YEAR1       TO  YEAR1
00478        ELSE
00479          SET MTHX UP BY +1
00480          GO TO 0575-JULIAN-REGULAR-YEAR.
00481
00482  0575-EXIT.
00483      EXIT.
00484
00485      EJECT
00486  0600-ELAPSED-TIME-CALCULATIONS SECTION.
00487
00488      MOVE YEAR2                 TO JULIAN-YEAR2
00489                                    CHECK-YEAR.
00490      PERFORM 0900-CHECK-FOR-LEAP-YEAR.
00491
00492      SET MTHX TO MONTH2.
00493
00494      IF A-LEAP-YEAR
00495          MOVE LEAP-YR-DAYS (MTHX) TO JULIAN-DAY2
00496        ELSE
00497          MOVE REGULAR-DAYS (MTHX) TO JULIAN-DAY2.
00498
00499      ADD DAY2 TO JULIAN-DAY2.
00500
00501      MOVE ZERO                   TO WS-ELAPSED-DAYS.
00502      MOVE YEAR1                  TO CHECK-YEAR.
00503
00504  0600-LOOP.
00505      IF CHECK-YEAR = JULIAN-YEAR2
00506          GO TO 0600-COMPUTE.
00507
00508      PERFORM 0900-CHECK-FOR-LEAP-YEAR.
00509
00510      IF A-LEAP-YEAR
00511          ADD +366 TO WS-ELAPSED-DAYS
00512        ELSE
00513          ADD +365 TO WS-ELAPSED-DAYS.
00514
00515      ADD +1  TO  CHECK-YEAR.
00516
00517      GO TO 0600-LOOP.
00518
00519  0600-COMPUTE.
00520      ADD JULIAN-DAY2        TO WS-ELAPSED-DAYS.
00521
00522      SUBTRACT JULIAN-DAY1 FROM WS-ELAPSED-DAYS.
00523
00524      COMPUTE WS-ELAPSED-MONTHS =
00525          ((YEAR2 - YEAR1) * 12) + (MONTH2 - MONTH1)
00526
00527      IF DAY2 = DAY1
00528          MOVE ZERO               TO  DC-ODD-DAYS-OVER
00529        ELSE
00530      IF DAY1 NOT < DAYS-IN-MONTH (MONTH1)
00531          MOVE DAY2               TO  DC-ODD-DAYS-OVER
00532          SUBTRACT +1 FROM WS-ELAPSED-MONTHS
00533        ELSE
00534      IF DAY2 > DAY1
00535          SUBTRACT DAY1 FROM DAY2 GIVING DC-ODD-DAYS-OVER
00536        ELSE
00537          COMPUTE DC-ODD-DAYS-OVER =
00538               (DAYS-IN-MONTH (MONTH1) - DAY1) + DAY2
00539          SUBTRACT +1 FROM WS-ELAPSED-MONTHS.
00540
00541      MOVE YEAR2                  TO CHECK-YEAR.
00542      PERFORM 0900-CHECK-FOR-LEAP-YEAR.
00543
00544      IF DC-ODD-DAYS-OVER NOT < DAYS-IN-MONTH (MONTH2) AND
00545         DAY2 = DAYS-IN-MONTH (MONTH2)
00546          MOVE ZERO               TO  DC-ODD-DAYS-OVER
00547          ADD +1 TO WS-ELAPSED-MONTHS.
00548
00549  0600-MOVE.
00550      MOVE WS-ELAPSED-MONTHS      TO  DC-ELAPSED-MONTHS.
00551      MOVE WS-ELAPSED-DAYS        TO  DC-ELAPSED-DAYS.
00552
00553  0600-EXIT.
00554      EXIT.
00555
00556      EJECT
00557  0700-CALCULATE-END-DATE SECTION.
00558
00559      IF NOT CALCULATE-END-OF-MONTH
00560          GO TO 0700-CONTINUE.
00561
00562      IF WS-ELAPSED-MONTHS = ZERO  OR
00563         WS-ELAPSED-DAYS NOT = ZERO
00564          MOVE '2'                TO  DC-END-OF-MONTH
00565          GO TO 0700-CONTINUE.
00566
00567      DIVIDE WS-BINARY-DATE-1 BY +384 GIVING YEAR1
00568              REMAINDER DAYS-REMAINING.
00569
00570      DIVIDE DAYS-REMAINING BY +32 GIVING MONTH1
00571              REMAINDER DAY1.
00572
00573      ADD +1  TO  MONTH1.
00574
00575      SET MTHX TO MONTH1.
00576
00577      MOVE YEAR1                 TO CHECK-YEAR.
00578      PERFORM 0900-CHECK-FOR-LEAP-YEAR.
00579
00580      IF DAY1 < DAYS-IN-MONTH (MTHX)
00581          MOVE '3'            TO  DC-END-OF-MONTH.
00582
00583  0700-CONTINUE.
00584      COMPUTE WS-BINARY-DATE-2 = WS-BINARY-DATE-1 +
00585              (WS-ELAPSED-MONTHS * 32).
00586
00587      IF WS-BINARY-DATE-2 NOT > ZERO
00588          MOVE '5'                TO  DC-ERROR-CODE
00589          GO TO 0700-EXIT.
00590
00591      DIVIDE WS-BINARY-DATE-2 BY +384 GIVING YEAR2
00592              REMAINDER DAYS-REMAINING.
00593
00594      DIVIDE DAYS-REMAINING BY +32 GIVING MONTH2
00595              REMAINDER DAY2.
00596
00597      ADD +1 TO MONTH2.
00598
00599      SET MTHX TO MONTH2.
00600
00601      MOVE YEAR2 TO YEAR1
00602                    CHECK-YEAR
00603                    JULIAN-YEAR2.
00604
00605      PERFORM 0900-CHECK-FOR-LEAP-YEAR.
00606
00607      IF A-LEAP-YEAR
00608          MOVE LEAP-YR-DAYS (MTHX) TO JULIAN-DAY2
00609        ELSE
00610          MOVE REGULAR-DAYS (MTHX) TO JULIAN-DAY2.
00611
00612      IF DAY2 NOT < DAYS-IN-MONTH (MTHX)
00613          MOVE DAYS-IN-MONTH (MTHX) TO DAY2.
00614
00615      ADD DAY2 TO JULIAN-DAY2.
00616
00617  0700-ADD-DAYS.
00618      IF WS-ELAPSED-DAYS = ZERO
00619          GO TO 0700-CONVERT-NEW-DATE.
00620
00621      MOVE WS-ELAPSED-DAYS TO DAYS-ELAPSED.
00622
00623      IF WS-ELAPSED-DAYS < ZERO
00624          MULTIPLY -1  BY  WS-ELAPSED-DAYS
00625          GO TO 0700-SUBTRACT-DAYS.
00626
00627      EJECT
00628  0700-ADD-DAYS-LOOP.
00629      IF A-LEAP-YEAR
00630          MOVE 366 TO DAYS-NEEDED
00631        ELSE
00632          MOVE 365 TO DAYS-NEEDED.
00633
00634      SUBTRACT JULIAN-DAY2 FROM DAYS-NEEDED.
00635
00636      IF DAYS-ELAPSED NOT > DAYS-NEEDED
00637          GO TO 0700-ADD-REMAINING-DAYS.
00638
00639      IF DAYS-ELAPSED > ZERO
00640          SUBTRACT +1  FROM DAYS-ELAPSED.
00641
00642      SUBTRACT DAYS-NEEDED FROM DAYS-ELAPSED.
00643
00644      ADD +1 TO JULIAN-YEAR2.
00645
00646      MOVE +1                     TO JULIAN-DAY2.
00647
00648      MOVE JULIAN-YEAR2           TO CHECK-YEAR.
00649
00650      PERFORM 0900-CHECK-FOR-LEAP-YEAR.
00651
00652      GO TO 0700-ADD-DAYS-LOOP.
00653
00654  0700-ADD-REMAINING-DAYS.
00655      ADD DAYS-ELAPSED TO JULIAN-DAY2.
00656
00657      GO TO 0700-CALCULATE-JULIAN-DATE.
00658
00659      EJECT
00660  0700-SUBTRACT-DAYS.
00661      IF WS-ELAPSED-DAYS < JULIAN-DAY2
00662          SUBTRACT WS-ELAPSED-DAYS FROM JULIAN-DAY2
00663          GO TO 0700-CALCULATE-JULIAN-DATE.
00664
00665      SUBTRACT JULIAN-DAY2 FROM WS-ELAPSED-DAYS.
00666
00667      SUBTRACT +1  FROM JULIAN-YEAR2.
00668
00669      MOVE JULIAN-YEAR2           TO  CHECK-YEAR.
00670      PERFORM 0900-CHECK-FOR-LEAP-YEAR.
00671
00672      IF A-LEAP-YEAR
00673          MOVE +366               TO  JULIAN-DAY2
00674        ELSE
00675          MOVE +365               TO  JULIAN-DAY2.
00676
00677      GO TO 0700-SUBTRACT-DAYS.
00678
00679      EJECT
00680  0700-CALCULATE-JULIAN-DATE.
00681      MOVE WORK-JULIAN-2          TO  WORK-JULIAN-1.
00682      SET MTHX TO +1.
00683
00684      IF A-LEAP-YEAR
00685          PERFORM 0550-JULIAN-LEAP-YEAR
00686        ELSE
00687          PERFORM 0575-JULIAN-REGULAR-YEAR.
00688
00689      MOVE WORK-DATE-1 TO WORK-DATE-2.
00690
00691  0700-CONVERT-NEW-DATE.
00692      IF CALCULATE-END-OF-MONTH
00693          SET MTHX TO MONTH2
00694          IF DAY2 < DAYS-IN-MONTH (MTHX)
00695              MOVE DAYS-IN-MONTH (MTHX) TO  DAY2.
00696
00697      MOVE WORK-DATE-2 TO WORK-DATE-1.
00698
00699  0700-EXIT.
00700      EXIT.
00701
pemuni*0800-FIND-CENTURY.
pemuni 0800-FIND-CENTURY section.
00703
00704      SET CNTRY-NDX TO +1.
00705      EVALUATE FIND-CENTURY-FLAG
00706         WHEN 1
00707            SEARCH  CENTURY-TBL
00708              AT END
00709                MOVE 'A' TO DC-ERROR-CODE
00710            WHEN CENTURY-CODE(CNTRY-NDX) = SEARCH-CENTURY-CD-N
00711               MOVE CENTURY(CNTRY-NDX) TO FOUND-CENTURY-N
00712            END-SEARCH
00713         WHEN 2
00714            SEARCH  CENTURY-TBL
00715               AT END
00716                  MOVE 'A' TO DC-ERROR-CODE
00717            WHEN CENTURY(CNTRY-NDX) = SEARCH-CENTURY-N
00718               MOVE CENTURY-CODE(CNTRY-NDX) TO FOUND-CENTURY-CD-N
00719            END-SEARCH
00720         WHEN 3
00721 ***  Y2K
00722 *          IF YEAR1 <= CENTURY-MARK
00723            EVALUATE YEAR1 < CENTURY-MARK
00724                     ALSO W-FLOAT-YEAR-YY > 34
00725                 WHEN TRUE ALSO TRUE
00726                     MOVE PREVIOUS-CENTURY TO
00727                                    DC-ALPHA-CEN-N
00728                 WHEN TRUE ALSO FALSE
00729                  IF W-FLOAT-YEAR-YY < 30
00730                     MOVE CURRENT-CENTURY-1-N TO
00731                                    DC-ALPHA-CEN-N
00732                  ELSE
00733                     MOVE NEXT-CENTURY TO DC-ALPHA-CEN-N
00734                  END-IF
00735                 WHEN FALSE ALSO FALSE
00736                  IF YEAR1 > BREAK-PERIOD
00737                     MOVE NEXT-CENTURY TO DC-ALPHA-CEN-N
00738                  ELSE
00739                     MOVE PREVIOUS-CENTURY TO DC-ALPHA-CEN-N
00740                  END-IF
00741                 WHEN  OTHER
00742                  IF YEAR1 > BREAK-PERIOD
00743                     MOVE PREVIOUS-CENTURY TO DC-ALPHA-CEN-N
00744                  ELSE
00745                     MOVE CURRENT-CENTURY-1-N
00746                                          TO DC-ALPHA-CEN-N
00747                  END-IF
00748            END-EVALUATE
00749      END-EVALUATE.
00750 ***  Y2K
pemuni
pemuni 0800-EXIT.
pemuni     EXIT.
00751
00752  0900-CHECK-FOR-LEAP-YEAR SECTION.
00753
00754      MOVE CHECK-YEAR TO CHECK-CCYY.
00755
00756 ***  Y2K, PROJ 7744
00757      IF CHECK-CCYY-YY > 00
00758          COMPUTE DIVIDE-LEAP-YEAR = CHECK-CCYY / 4
00759      ELSE
00760          MOVE CHECK-CCYY-CC      TO SEARCH-CENTURY-CD-N
00761          MOVE 1                  TO FIND-CENTURY-FLAG
00762          PERFORM 0800-FIND-CENTURY
00763          IF NO-CONVERSION-ERROR
00764              MOVE FOUND-CENTURY-N TO CHECK-CCYY-CC
00765              COMPUTE DIVIDE-LEAP-YEAR = CHECK-CCYY / 400
00766          END-IF
00767      END-IF
00768 ***  Y2K, PROJ 7744
00769
00770      IF A-LEAP-YEAR
00771          MOVE +29                TO  DAYS-IN-MONTH (2)
00772      ELSE
00773          MOVE +28                TO  DAYS-IN-MONTH (2).
00774
00775  0900-EXIT.
00776      EXIT.
00738
00739      EJECT
00740  8900-MAPFAIL SECTION.
00741      MOVE MAPFAIL-MSG            TO TEXT-AREA.
00742      GO TO 8990-SEND-TEXT.
00743
00744  8950-CLEAR-RETURN.
00745      MOVE TRAN-COMPLETE-MSG      TO TEXT-AREA.
00746
00747  8990-SEND-TEXT.
00748      
      * EXEC CICS SEND TEXT
00749 *        FROM    (TEXT-AREA)
00750 *        LENGTH  (TEXT-LENGTH)
00751 *        ERASE
00752 *        FREEKB
00753 *        END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00002670' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TEXT-AREA, 
                 TEXT-LENGTH, 
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
           
00754
00755  9000-RETURN-TO-CICS.
00756      
      * EXEC CICS RETURN
00757 *        END-EXEC.
      *    MOVE '.(                    &   #00002678' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00758
00759      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL009' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00760

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL009' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 8950-CLEAR-RETURN,
                     4010-BIN-TO-GREG,
                     4020-COMPUTE-ELAPSED,
                     4020-COMPUTE-ELAPSED,
                     4030-GREG-TO-BIN,
                     4040-JULIAN-TO-OTHER,
                     4050-BINARY-PLUS-ELAPSED,
                     4060-UNSUPPORTED
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 8900-MAPFAIL
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL009' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
