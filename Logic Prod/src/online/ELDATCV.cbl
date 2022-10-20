00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 ELDATCV.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 03/05/96 14:47:01.
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE
00008 *                            VMOD=2.007
00009
00010 *AUTHOR.     LOGIC, INC.
00011 *            DALLAS, TEXAS.
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
00025 *REMARKS.    *****************************************************
00026 *            *                                                   *
00027 *            *    THIS 'SUBROUTINE' WILL, DEPENDING UPON THE     *
00028 *            *    OPTION SPECIFIED, CONVERT A GIVEN DATE TO      *
00029 *            *    THE FOLLOWING:  I. EDITED GREGORIAN (MM/DD/YY) *
00030 *            *                   II. Y-M-D GREGORIAN  (YYMMDD)   *
00031 *            *                  III. M-D-Y GREGORIAN  (MMDDYY)   *
00032 *            *                   IV. ALPHA GREG  (MMM DD, YYYY)  *
00033 *            *                    V. JULIAN  (YYDDD)             *
00034 *            *                   VI. TOTAL DAYS SINCE 1900.      *
00035 *            *    ADDITIONALLY, IF TWO DATES (EXPRESSED AS DAYS) *
00036 *            *    ARE GIVEN, THE ELAPSED MONTHS AND DAYS CAN BE  *
00037 *            *    CALCULATED.                                    *
00038 *            *                                                   *
00039 *            *****************************************************
00040  ENVIRONMENT DIVISION.
00041
00042  DATA DIVISION.
00043      EJECT
00044  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00045  77  FILLER   PIC X(32) VALUE '********************************'.
00046  77  FILLER   PIC X(32) VALUE '*   ELDATCV WORKING STORAGE    *'.
00047  77  FILLER   PIC X(32) VALUE '********* VMOD 2.007 ***********'.
00048
pemuni 01 WKS-IN.
pemuni    10  FILLER    PIC X(6) VALUE 'DATEIN'.
pemuni    10  WKS-IN-TN PIC 9(10).
pemuni
pemuni 01 WKS-OT.
pemuni    10  FILLER    PIC X(6) VALUE 'DATEOT'.
pemuni    10  WKS-OT-TN PIC 9(10).
pemuni
00049  01  W-PROGRAM-WORK-AREA.
00050      12  W-CURRENT-DATE              PIC  9(07).
00051      12  FILLER REDEFINES W-CURRENT-DATE.
00052          16  FILLER                  PIC  9.
00053          16  W-CD-CCYY               PIC  999.
00054          16  W-CD-CCYY-R  REDEFINES W-CD-CCYY.
00055              20  W-CD-YEAR-CD        PIC  9.
00056              20  W-CURRENT-YEAR-YY   PIC  99.
00057          16  W-CURRENT-DAYS          PIC  999.
00058      12  W-FLOAT-YEAR                PIC  999.
00059      12  W-FLOAT-YY REDEFINES W-FLOAT-YEAR.
00060          16  W-FLOAT-YEAR-CD         PIC  9.
00061          16  W-FLOAT-YEAR-YY         PIC  99.
00062      EJECT
00063 *                            COPY ELCDATW1.
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
00064      EJECT
00065 *                            COPY ELCDATE.
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
00066      EJECT
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
00068
00069  01  DFHCOMMAREA                     PIC X(200).
00070
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'ELDATCV' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
           MOVE eibtaskn to WKS-IN-TN.
pemuni*    EXEC CICS WRITE OPERATOR
pemuni*         TEXT(WKS-IN)
pemuni*          TEXTLENGTH(16)
pemuni*    END-EXEC.
00072
00073      MOVE DFHCOMMAREA          TO  DATE-CONVERSION-DATA.
00074
00075      MOVE SPACES                TO DC-ERROR-CODE.
00076
00077      INITIALIZE DC-GREG-DATE-1-ALPHA.
00078
00079      MOVE EIBDATE              TO  W-CURRENT-DATE.
00080
00081      MOVE W-CD-YEAR-CD         TO CONTROL-YEAR-CD
00082                                   SEARCH-CENTURY-CD-N.
00083
00084      MOVE '1'                  TO FIND-CENTURY-FLAG.
00085      PERFORM 0800-FIND-CENTURY.
00086      IF NO-CONVERSION-ERROR
00087          MOVE FOUND-CENTURY-N  TO CURRENT-CENTURY-1-N.
00088
00089      COMPUTE W-FLOAT-YEAR = W-CD-CCYY - CONTROL-YEAR.
pemuni*    display 'Test cobsw cpdebug ' eibtaskn.
00090
00091  0100-PROGRAM-PROCESSING SECTION. 
      *                                 COPY ELCDATP1.
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
00092
00093      MOVE DATE-CONVERSION-DATA   TO  DFHCOMMAREA.
pemuni*    MOVE eibtaskn to WKS-OT-TN.
pemuni*    EXEC CICS WRITE OPERATOR
pemuni*         TEXT(WKS-OT)
pemuni*         TEXTLENGTH(16)
pemuni*    END-EXEC.
00094
00095      
      * EXEC CICS RETURN
00096 *         END-EXEC.
      *    MOVE '.(                    &   #00000711' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303030373131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00097
00098        
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELDATCV' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00099      EJECT
00100  0200-DATE-CONVERSION-ROUTINES SECTION. 
      *                                       COPY ELCDATP2.
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
00173                                      DC-JULIAN-DATE
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
00379      IF DC-JULIAN-DATE = ZERO
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
00101

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELDATCV' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'ELDATCV' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
