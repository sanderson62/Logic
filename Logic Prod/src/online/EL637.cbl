00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL637 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 09:53:54.
00007 *                            VMOD=2.012
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
00018 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00019 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00020 *            *                                                   *
00021 *            *****************************************************
00022
00023 *REMARKS. TRANSACTION - EXJB - CHECK VOID PROGRAM.
00024
00025  ENVIRONMENT DIVISION.
00026
00027      EJECT
00028  DATA DIVISION.
00029  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00030
00031  77  FILLER  PIC X(32)  VALUE '********************************'.
00032  77  FILLER  PIC X(32)  VALUE '*    EL637 WORKING STORAGE     *'.
00033  77  FILLER  PIC X(32)  VALUE '************ V/M 2.012 *********'.
00034
00035     EJECT
00036
00037 *                            COPY ELCSCTM.
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
00038 *                            COPY ELCSCRTY.
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
00039
00040     EJECT
00041
00042 ******************************************************************
00043 *                                                                *
00044 *              S T A N D A R D   A R E A S                       *
00045 *                                                                *
00046 ******************************************************************
00047
00048  01  STANDARD-AREAS.
00049      12  SC-ITEM                     PIC S9(4)   VALUE +1   COMP.
00050      12  GETMAIN-SPACE               PIC X       VALUE SPACE.
00051      12  EL637A                      PIC X(8)    VALUE 'EL637A'.
00052      12  MAPSET-EL637S               PIC X(8)    VALUE 'EL637S'.
00053      12  TRANS-EXJB                  PIC X(4)    VALUE 'EXJB'.
00054      12  THIS-PGM                    PIC X(8)    VALUE 'EL637 '.
00055      12  PGM-NAME                    PIC X(8).
00056      12  TIME-IN                     PIC S9(7).
00057      12  TIME-OUT-R  REDEFINES TIME-IN.
00058          16  FILLER                  PIC X.
00059          16  TIME-OUT                PIC 99V99.
00060          16  FILLER                  PIC X(2).
00061      12  LINK-EL001                  PIC X(8)    VALUE 'EL001'.
00062      12  LINK-EL004                  PIC X(8)    VALUE 'EL004'.
00063      12  XCTL-EL005                  PIC X(8)    VALUE 'EL005'.
00064      12  XCTL-EL010                  PIC X(8)    VALUE 'EL010'.
00065      12  XCTL-EL626                  PIC X(8)    VALUE 'EL626'.
00066      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'.
00067      12  FILE-ID-ERCMKQ              PIC X(8)    VALUE 'ERCMKQ '.
00068      12  FILE-ID-ERCMKQ2             PIC X(8)    VALUE 'ERCMKQ2'.
00069      12  FILE-ID-ERCMCK              PIC X(8)    VALUE 'ERCMCK '.
00070      12  FILE-ID-ERCKWK              PIC X(8)    VALUE 'ERCKWK '.
00071      12  FILE-ID-ERPYAJ              PIC X(8)    VALUE 'ERPYAJ '.
00072      12  WS-CURRENT-DT               PIC X(8)    VALUE SPACES.
00073      12  WS-CURRENT-BIN-DT           PIC XX      VALUE SPACES.
00074      12  QID.
00075          16  QID-TERM                PIC X(4)    VALUE SPACES.
00076          16  FILLER                  PIC X(4)    VALUE '125D'.
00077      12  WS-TIME                     PIC 9(6)    VALUE ZEROS.
00078      12  WS-HR-MINS-SECS REDEFINES WS-TIME.
00079          16  WS-HR-MINS              PIC 99V99.
00080          16  FILLER                  PIC XX.
00081
00082      EJECT
00083
00084  01  WORK-AREA.
00085      12  DEEDIT-FIELD               PIC X(12).
00086      12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD   PIC S9(10)V99.
00087      12  WS-WORK-SEQ-NO             PIC S9(9)   VALUE +0 COMP.
00088      12  WS-WORK-SEQ-HOLD           PIC S9(3)   VALUE +0.
00089      12  WS-SUB1                    PIC S9(4)   VALUE +0 COMP.
00090      12  WS-ERCKWK-PROCESS-SW       PIC X       VALUE SPACE.
00091          88  WS-ERCKWK-PROCESSED                VALUE 'Y'.
00092      12  WS-BROWSE-SW               PIC X       VALUE SPACE.
00093          88  WS-BROWSE-STARTED                  VALUE 'Y'.
00094      12  WS-FIRST-TIME-SW           PIC X       VALUE 'Y'.
00095          88  WS-FIRST-TIME                      VALUE 'Y'.
00096      12  WS-STRT-CK-NO              PIC 9(6)  VALUE ZEROS.
00097
00098  01  WS-MAX-TEXT-SEQ                PIC S9(4)   VALUE +9002 COMP.
00099
00100      EJECT
00101
00102 ******************************************************************
00103 *                                                                *
00104 *                E R R O R   M E S S A G E S                     *
00105 *                                                                *
00106 ******************************************************************
00107
00108  01  ERROR-MESSAGES.
00109      12  ER-0000                 PIC X(4)  VALUE '0000'.
00110      12  ER-0004                 PIC X(4)  VALUE '0004'.
00111      12  ER-0008                 PIC X(4)  VALUE '0008'.
00112      12  ER-0023                 PIC X(4)  VALUE '0023'.
00113      12  ER-0029                 PIC X(4)  VALUE '0029'.
00114      12  ER-0070                 PIC X(4)  VALUE '0070'.
00115      12  ER-0194                 PIC X(4)  VALUE '0194'.
00116      12  ER-0195                 PIC X(4)  VALUE '0195'.
00117      12  ER-2056                 PIC X(4)  VALUE '2056'.
00118      12  ER-2132                 PIC X(4)  VALUE '2132'.
00119      12  ER-2223                 PIC X(4)  VALUE '2223'.
00120      12  ER-2251                 PIC X(4)  VALUE '2251'.
00121      12  ER-2252                 PIC X(4)  VALUE '2252'.
00122      12  ER-2800                 PIC X(4)  VALUE '2800'.
00123      12  ER-3129                 PIC X(4)  VALUE '3129'.
00124      12  ER-3147                 PIC X(4)  VALUE '3147'.
00125      12  ER-3148                 PIC X(4)  VALUE '3148'.
00126      12  ER-3158                 PIC X(4)  VALUE '3158'.
00127      12  ER-3159                 PIC X(4)  VALUE '3159'.
00128      12  ER-3160                 PIC X(4)  VALUE '3160'.
00129      12  ER-3161                 PIC X(4)  VALUE '3161'.
00130      12  ER-3162                 PIC X(4)  VALUE '3162'.
00131      12  ER-3163                 PIC X(4)  VALUE '3163'.
00132      12  ER-3164                 PIC X(4)  VALUE '3164'.
00133      12  ER-3165                 PIC X(4)  VALUE '3165'.
00134      12  ER-3171                 PIC X(4)  VALUE '3171'.
00135      12  ER-3176                 PIC X(4)  VALUE '3176'.
00136      12  ER-3185                 PIC X(4)  VALUE '3185'.
00137
00138      EJECT
00139
00140 ******************************************************************
00141 *                                                                *
00142 *              A C C E S S   K E Y S                             *
00143 *                                                                *
00144 ******************************************************************
00145
00146  01  ACCESS-KEYS.
00147
00148      12  ERCMKQ-KEY.
00149          16  ERCMKQ-COMPANY-CD       PIC X           VALUE SPACES.
00150          16  ERCMKQ-CONTROL-NUMBER   PIC S9(8) COMP VALUE +0.
00151          16  ERCMKQ-SEQUENCE-NUMBER  PIC S9(4) COMP VALUE +0.
00152
00153      12  ERCMKQ2-KEY.
00154          16  ERCMKQ2-COMPANY-CD      PIC X          VALUE SPACES.
00155          16  ERCMKQ2-CSR             PIC X(4)       VALUE SPACES.
00156          16  ERCMKQ2-CARRIER         PIC X          VALUE SPACES.
00157          16  ERCMKQ2-GROUPING        PIC X(6)       VALUE SPACES.
00158          16  ERCMKQ2-PAYEE           PIC X(10)      VALUE SPACES.
00159          16  ERCMKQ2-PAYEE-SEQ       PIC S9(4) COMP VALUE +0.
00160          16  ERCMKQ2-CONTROL-NUMBER  PIC S9(8) COMP VALUE +0.
00161          16  ERCMKQ2-SEQUENCE-NUMBER PIC S9(4) COMP VALUE +0.
00162
00163      12  ERCMKQ2-PREV-KEY            PIC X(30)      VALUE SPACES.
00164
00165      12  ERCMKQ2-COMPARE-KEY         PIC X(28)      VALUE SPACES.
00166
00167      12  SVCMKQ2-COMPARE-KEY         PIC X(28)      VALUE SPACES.
00168
00169      12  ERCMKQ-RECORD-LENGTH        PIC S9(4) COMP VALUE +1800.
00170
00171      12  ERCMCK-KEY.
00172          16  ERCMCK-COMPANY-CD       PIC X          VALUE SPACES.
00173          16  ERCMCK-CSR              PIC X(4)       VALUE SPACES.
00174          16  ERCMCK-CARRIER          PIC X          VALUE SPACES.
00175          16  ERCMCK-GROUPING         PIC X(6)       VALUE SPACES.
00176          16  ERCMCK-PAYEE            PIC X(10)      VALUE SPACES.
00177          16  ERCMCK-PAYEE-SEQ        PIC S9(4) COMP VALUE +0.
00178          16  ERCMCK-SEQUENCE-NO      PIC S9(4) COMP VALUE +0.
00179
00180      12  ERCMCK-RECORD-LENGTH        PIC S9(4) COMP VALUE +2000.
00181
00182      12  ERCKWK-KEY.
00183          16  ERCKWK-COMPANY-CD       PIC X           VALUE SPACES.
00184          16  ERCKWK-CSR              PIC X(4)        VALUE SPACES.
00185          16  ERCKWK-CARRIER          PIC X           VALUE SPACES.
00186          16  ERCKWK-GROUPING         PIC X(6)        VALUE SPACES.
00187          16  ERCKWK-PAYEE            PIC X(10)       VALUE SPACES.
00188          16  ERCKWK-PAYEE-SEQ        PIC S9(4)  COMP VALUE +0.
00189          16  ERCKWK-SEQUENCE-NO      PIC S9(4)  COMP VALUE +0.
00190
00191
00192      12  ERPYAJ-KEY.
00193          16  ERPYAJ-COMPANY-CD       PIC X           VALUE SPACE.
00194          16  ERPYAJ-CARRIER          PIC X           VALUE SPACE.
00195          16  ERPYAJ-GROUPING         PIC X(6)        VALUE SPACE.
00196          16  ERPYAJ-FIN-RESP         PIC X(10)       VALUE SPACE.
00197          16  ERPYAJ-ACCOUNT          PIC X(10)       VALUE SPACE.
00198          16  ERPYAJ-FILE-SEQ-NO      PIC S9(8) COMP  VALUE +0.
00199          16  ERPYAJ-RECORD-TYPE      PIC X.
00200
00201      12  ERPYAJ-RECORD-LENGTH        PIC S9(4) COMP  VALUE +200.
00202
00203      EJECT
00204
00205 *                            COPY ELCDATE.
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
00206
00207      EJECT
00208 *                            COPY ELCLOGOF.
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
00209
00210      EJECT
00211 *                            COPY ELCATTR.
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
00212
00213      EJECT
00214 *                            COPY ELCEMIB.
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
00215
00216      EJECT
00217 *                            COPY ELCINTF.
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
00218
00219      12  FILLER  REDEFINES PI-PROGRAM-WORK-AREA.
00220          16  PI-MAINT-FUNCTION               PIC X.
00221              88  PI-VOID-FUNCTION                VALUE 'V'.
00222              88  PI-SHOW-FUNCTION                VALUE 'S'.
00223
00224          16  PI-END-OF-FILE-SW               PIC X.
00225              88  PI-END-OF-FILE                  VALUE 'Y'.
00226          16  PI-ERCMKQ2-KEY.
00227              20  PI-ERCMKQ2-COMPANY-CD       PIC X.
00228              20  PI-ERCMKQ2-CSR              PIC X(4).
00229              20  PI-ERCMKQ2-CARRIER          PIC X.
00230              20  PI-ERCMKQ2-GROUPING         PIC X(6).
00231              20  PI-ERCMKQ2-PAYEE            PIC X(10).
00232              20  PI-ERCMKQ2-PAYEE-SEQ        PIC S9(4) COMP.
00233              20  PI-ERCMKQ2-CONTROL-NUMBER   PIC S9(8) COMP.
00234              20  PI-ERCMKQ2-SEQUENCE-NUMBER  PIC S9(4) COMP.
00235
00236          16  PI-SAVE-ERCMKQ2-KEY.
00237              20  PI-SVCMKQ2-COMPANY-CD       PIC X.
00238              20  PI-SVCMKQ2-CSR              PIC X(4).
00239              20  PI-SVCMKQ2-CARRIER          PIC X.
00240              20  PI-SVCMKQ2-GROUPING         PIC X(6).
00241              20  PI-SVCMKQ2-PAYEE            PIC X(10).
00242              20  PI-SVCMKQ2-PAYEE-SEQ        PIC S9(4) COMP.
00243              20  PI-SVCMKQ2-CONTROL-NUMBER   PIC S9(8) COMP.
00244              20  PI-SVCMKQ2-SEQUENCE-NUMBER  PIC S9(4) COMP.
00245          16  FILLER                          PIC X(578).
00246
00247
00248      EJECT
00249 *                            COPY ELCJPFX.
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
00250                              PIC X(1583).
00251
00252      EJECT
00253 *                            COPY ELCAID.
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
00254  01  FILLER    REDEFINES DFHAID.
00255      12  FILLER              PIC X(8).
00256      12  PF-VALUES           PIC X       OCCURS 2.
00257
00258      EJECT
00259 *                            COPY EL637S.
       01  EL637AI.
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
           05  CSRL PIC S9(0004) COMP.
           05  CSRF PIC  X(0001).
           05  FILLER REDEFINES CSRF.
               10  CSRA PIC  X(0001).
           05  CSRI PIC  X(0004).
      *    -------------------------------
           05  CARL PIC S9(0004) COMP.
           05  CARF PIC  X(0001).
           05  FILLER REDEFINES CARF.
               10  CARA PIC  X(0001).
           05  CARI PIC  X(0001).
      *    -------------------------------
           05  GROUPL PIC S9(0004) COMP.
           05  GROUPF PIC  X(0001).
           05  FILLER REDEFINES GROUPF.
               10  GROUPA PIC  X(0001).
           05  GROUPI PIC  X(0006).
      *    -------------------------------
           05  PAYEEL PIC S9(0004) COMP.
           05  PAYEEF PIC  X(0001).
           05  FILLER REDEFINES PAYEEF.
               10  PAYEEA PIC  X(0001).
           05  PAYEEI PIC  X(0010).
      *    -------------------------------
           05  PAYSEQL PIC S9(0004) COMP.
           05  PAYSEQF PIC  X(0001).
           05  FILLER REDEFINES PAYSEQF.
               10  PAYSEQA PIC  X(0001).
           05  PAYSEQI PIC  9(4).
      *    -------------------------------
           05  CNTRLNOL PIC S9(0004) COMP.
           05  CNTRLNOF PIC  X(0001).
           05  FILLER REDEFINES CNTRLNOF.
               10  CNTRLNOA PIC  X(0001).
           05  CNTRLNOI PIC  9(8).
      *    -------------------------------
           05  PAYTOL PIC S9(0004) COMP.
           05  PAYTOF PIC  X(0001).
           05  FILLER REDEFINES PAYTOF.
               10  PAYTOA PIC  X(0001).
           05  PAYTOI PIC  X(0030).
      *    -------------------------------
           05  MAINTBYL PIC S9(0004) COMP.
           05  MAINTBYF PIC  X(0001).
           05  FILLER REDEFINES MAINTBYF.
               10  MAINTBYA PIC  X(0001).
           05  MAINTBYI PIC  X(0004).
      *    -------------------------------
           05  ADDRS1L PIC S9(0004) COMP.
           05  ADDRS1F PIC  X(0001).
           05  FILLER REDEFINES ADDRS1F.
               10  ADDRS1A PIC  X(0001).
           05  ADDRS1I PIC  X(0030).
      *    -------------------------------
           05  MAINTATL PIC S9(0004) COMP.
           05  MAINTATF PIC  X(0001).
           05  FILLER REDEFINES MAINTATF.
               10  MAINTATA PIC  X(0001).
           05  MAINTATI PIC  X(0005).
      *    -------------------------------
           05  MAINTONL PIC S9(0004) COMP.
           05  MAINTONF PIC  X(0001).
           05  FILLER REDEFINES MAINTONF.
               10  MAINTONA PIC  X(0001).
           05  MAINTONI PIC  X(0008).
      *    -------------------------------
           05  ADDRS2L PIC S9(0004) COMP.
           05  ADDRS2F PIC  X(0001).
           05  FILLER REDEFINES ADDRS2F.
               10  ADDRS2A PIC  X(0001).
           05  ADDRS2I PIC  X(0030).
      *    -------------------------------
           05  CITYSTL PIC S9(0004) COMP.
           05  CITYSTF PIC  X(0001).
           05  FILLER REDEFINES CITYSTF.
               10  CITYSTA PIC  X(0001).
           05  CITYSTI PIC  X(0030).
      *    -------------------------------
           05  CHKRELL PIC S9(0004) COMP.
           05  CHKRELF PIC  X(0001).
           05  FILLER REDEFINES CHKRELF.
               10  CHKRELA PIC  X(0001).
           05  CHKRELI PIC  X(0008).
      *    -------------------------------
           05  ZIPL PIC S9(0004) COMP.
           05  ZIPF PIC  X(0001).
           05  FILLER REDEFINES ZIPF.
               10  ZIPA PIC  X(0001).
           05  ZIPI PIC  X(0005).
      *    -------------------------------
           05  ZIPEXTL PIC S9(0004) COMP.
           05  ZIPEXTF PIC  X(0001).
           05  FILLER REDEFINES ZIPEXTF.
               10  ZIPEXTA PIC  X(0001).
           05  ZIPEXTI PIC  X(0004).
      *    -------------------------------
           05  CHKWRTNL PIC S9(0004) COMP.
           05  CHKWRTNF PIC  X(0001).
           05  FILLER REDEFINES CHKWRTNF.
               10  CHKWRTNA PIC  X(0001).
           05  CHKWRTNI PIC  X(0008).
      *    -------------------------------
           05  VOIDDTL PIC S9(0004) COMP.
           05  VOIDDTF PIC  X(0001).
           05  FILLER REDEFINES VOIDDTF.
               10  VOIDDTA PIC  X(0001).
           05  VOIDDTI PIC  X(0008).
      *    -------------------------------
           05  CHKAMTL PIC S9(0004) COMP.
           05  CHKAMTF PIC  X(0001).
           05  FILLER REDEFINES CHKAMTF.
               10  CHKAMTA PIC  X(0001).
           05  CHKAMTI PIC  X(0013).
      *    -------------------------------
           05  STRTCHKL PIC S9(0004) COMP.
           05  STRTCHKF PIC  X(0001).
           05  FILLER REDEFINES STRTCHKF.
               10  STRTCHKA PIC  X(0001).
           05  STRTCHKI PIC  X(0006).
      *    -------------------------------
           05  DASHL PIC S9(0004) COMP.
           05  DASHF PIC  X(0001).
           05  FILLER REDEFINES DASHF.
               10  DASHA PIC  X(0001).
           05  DASHI PIC  X(0001).
      *    -------------------------------
           05  ENDCHKL PIC S9(0004) COMP.
           05  ENDCHKF PIC  X(0001).
           05  FILLER REDEFINES ENDCHKF.
               10  ENDCHKA PIC  X(0001).
           05  ENDCHKI PIC  X(0006).
      *    -------------------------------
           05  ERMESGL PIC S9(0004) COMP.
           05  ERMESGF PIC  X(0001).
           05  FILLER REDEFINES ERMESGF.
               10  ERMESGA PIC  X(0001).
           05  ERMESGI PIC  X(0079).
      *    -------------------------------
           05  PFENTERL PIC S9(0004) COMP.
           05  PFENTERF PIC  X(0001).
           05  FILLER REDEFINES PFENTERF.
               10  PFENTERA PIC  X(0001).
           05  PFENTERI PIC  9(2).
       01  EL637AO REDEFINES EL637AI.
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
           05  CSRO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PAYEEO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PAYSEQO PIC  9(4).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CNTRLNOO PIC  9(8).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PAYTOO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADDRS1O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTATO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTONO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADDRS2O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CITYSTO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHKRELO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ZIPO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ZIPEXTO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHKWRTNO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  VOIDDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHKAMTO PIC  Z,ZZZ,ZZZ.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STRTCHKO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DASHO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENDCHKO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERMESGO PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  X(0002).
      *    -------------------------------
00260
00261
00262
00263      EJECT
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
00265  01  DFHCOMMAREA             PIC X(1024).
00266
00267      EJECT
00268 *01 PARMLIST .
00269 *    02  FILLER              PIC S9(8)   COMP.
00270 *    02  ERCKWK-POINTER      PIC S9(8)   COMP.
00271 *    02  ERCMCK-POINTER      PIC S9(8)   COMP.
00272 *    02  ERCMKQ-POINTER      PIC S9(8)   COMP.
00273 *    02  ERPYAJ-POINTER      PIC S9(8)   COMP.
00274
00275      EJECT
00276
00277 *                                COPY ERCCKWK.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCKWK                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.011                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CHECK WORK RECORDS                        *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 200    RECFORM = FIXED                         *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ERCKWK             RKP=2,LEN=26          *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
00018  01  CHECK-WORK-RECORDS.
00019      12  CW-RECORD-ID                      PIC XX.
00020          88  VALID-CW-ID                      VALUE 'CW'.
00021
00022      12  CW-CONTROL-PRIMARY.
00023          16  CW-COMPANY-CD                 PIC X.
00024          16  CW-CSR                        PIC X(4).
00025          16  CW-CARRIER                    PIC X.
00026          16  CW-GROUPING                   PIC X(6).
00027          16  CW-PAYEE                      PIC X(10).
00028          16  CW-PAYEE-SEQ                  PIC S9(4)     COMP.
00029          16  CW-SEQUENCE-NO                PIC S9(4)     COMP.
00030
00031      12  CW-RECORD-TYPE                    PIC X.
00032          88 CW-HEADER                            VALUE '0'.
00033          88 CW-DETAIL                            VALUE '1'.
00034          88 CW-TEXT                              VALUE '2'.
00035
00036      12  CW-RECORDED-DT                    PIC XX.
00037      12  CW-RECORDED-BY                    PIC X(4).
00038      12  CW-LAST-MAINT-HHMMSS              PIC S9(6)     COMP-3.
00039
00040      12  CW-RELEASE-DT                     PIC XX.
00041
00042      12  CW-HEADER-RECORD.
00043          16  CW-PAYEE-NAME                 PIC X(30).
00044          16  CW-ADDRESS-1                  PIC X(30).
00045          16  CW-ADDRESS-2                  PIC X(30).
00046          16  CW-PAYEE-CITY-ST              PIC X(30).
00047          16  CW-PAYEE-ZIP-CODE.
00048              20  CW-PAYEE-ZIP              PIC X(5).
00049              20  CW-PAYEE-ZIP-EXT          PIC X(4).
00050          16  CW-TOTAL-COMMISSION           PIC S9(7)V99   COMP-3.
00051          16  CW-TOTAL-ENTRIES              PIC S9(3)      COMP-3.
00052
00053      12  CW-DETAIL-RECORD REDEFINES CW-HEADER-RECORD.
00054          16  CW-COMMENT                    PIC X(23).
00055          16  CW-ACCT-AGENT                 PIC X(10).
00056          16  CW-INVOICE                    PIC X(6).
00057          16  CW-REFERENCE                  PIC X(12).
00058          16  CW-LEDGER-NO.
00059              20  CW-LEDGER-PREFIX          PIC X(7).
00060              20  CW-LEDGER-SUFFIX          PIC X(7).
00061          16  CW-DETAIL-AMOUNT              PIC S9(7)V99  COMP-3.
00062          16  CW-PAYMENT-TYPE               PIC X.
00063          16  CW-LAST-MAINT-APPLIED         PIC X.
00064          16  CW-NON-AR-ITEM                PIC X.
00065          16  FILLER                        PIC X(63).
00066
00067      12  CW-TEXT-RECORD REDEFINES CW-HEADER-RECORD.
00068          16  CW-STUB-TEXT                  PIC X(70).
00069          16  CW-FILLER                     PIC X(66).
00070
00071      12  CW-CREDIT-SELECT-DT               PIC XX.
00072      12  CW-CREDIT-ACCEPT-DT               PIC XX.
00073
00074      12  CW-AR-STATEMENT-DT                PIC XX.
00075      12  CW-PMT-APPLIED                    PIC X.
00076
00077      12  CW-PYAJ-MADE                      PIC X.
00078
00079      12  FILLER                            PIC X(15).
00080
00278      EJECT
00279
00280 *                                COPY ERCCMCK.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCMCK                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.012                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = COMMISSION CHECK RECORDS                  *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 2000   RECFORM = FIXED                         *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ERCMCK             RKP=2,LEN=26          *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = NO                                                     *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
00018  01  COMM-CHECK-RECORDS.
00019      12  CK-RECORD-ID                      PIC XX.
00020          88  VALID-CK-ID                      VALUE 'CK'.
00021
00022      12  CK-CONTROL-PRIMARY.
00023          16  CK-COMPANY-CD                 PIC X.
00024          16  CK-CSR                        PIC X(4).
00025          16  CK-CARRIER                    PIC X.
00026          16  CK-GROUPING                   PIC X(6).
00027          16  CK-PAYEE                      PIC X(10).
00028          16  CK-PAYEE-SEQ                  PIC S9(4)     COMP.
00029          16  CK-SEQUENCE-NO                PIC S9(4)     COMP.
00030
00031      12  CK-RECORDED-DT                    PIC XX.
00032      12  CK-RECORDED-BY                    PIC X(4).
00033      12  CK-LAST-MAINT-HHMMSS              PIC S9(6)     COMP-3.
00034
00035      12  CK-AMOUNT-PAID                    PIC S9(7)V99  COMP-3.
00036      12  CK-RECORD-TYPE                    PIC X.
00037          88  CK-DETAIL                        VALUE 'D'.
00038          88  CK-TEXT                          VALUE 'T'.
00039      12  CK-CHECK-NO                       PIC X(6).
00040      12  CK-CHECK-WRITTEN-DT               PIC XX.
00041      12  CK-PAYEE-INFO.
00042          16  CK-PAYEE-NAME                 PIC X(30).
00043          16  CK-PAYEE-ADDRESS-1            PIC X(30).
00044          16  CK-PAYEE-ADDRESS-2            PIC X(30).
00045          16  CK-PAYEE-CITY-ST              PIC X(30).
00046          16  CK-PAYEE-ZIP-CODE.
00047              20  CK-PAYEE-ZIP              PIC X(5).
00048              20  CK-PAYEE-ZIP-EXT          PIC X(4).
00049
00050      12  CK-CHECK-STUB-DATA.
00051          16  CK-CHECK-STUB-INFO  OCCURS  15  TIMES.
00052              20  CK-STUB-LINE.
00053                  24  CK-STUB-COMMENT       PIC X(23).
00054                  24  CK-ACCT-AGENT         PIC X(10).
00055                  24  CK-INVOICE            PIC X(6).
00056                  24  CK-REFERENCE          PIC X(12).
00057                  24  CK-LEDGER-NO          PIC X(14).
00058                  24  CK-DETAIL-AMT         PIC S9(7)V99 COMP-3.
00059                  24  CK-LAST-MAINT-APPLIED PIC X.
00060                  24  CK-NON-AR-ITEM        PIC X.
00061                  24  FILLER                PIC X(19).
00062
00063              20  CK-CHECK-WORK-CONTROL.
00064                   24  CK-CKWK-CSR          PIC X(4).
00065                   24  CK-CKWK-CARRIER      PIC X.
00066                   24  CK-CKWK-GROUPING     PIC X(6).
00067                   24  CK-CKWK-PAYEE        PIC X(10).
00068                   24  CK-CKWK-PAYEE-SEQ    PIC S9(4) COMP.
00069                   24  CK-CKWK-SEQ-NO       PIC S9(4) COMP.
00070              20  CK-PAYMENT-TYPE           PIC X.
00071              20  CK-PYAJ-PMT-APPLIED       PIC X.
00072
00073      12  CK-CHECK-STUB-TEXT  REDEFINES CK-CHECK-STUB-DATA.
00074          16  CK-CHECK-TEXT-ITEMS OCCURS 3 TIMES.
00075              20  CK-STUB-TEXT              PIC X(70).
00076          16  CK-STUB-FILL-AREA             PIC X(1560).
00077
00078      12  CK-CHECK-QUE-CONTROL.
00079          20  CK-QUE-CONTROL-NUMBER         PIC S9(8) COMP.
00080          20  CK-QUE-SEQ-NO                 PIC S9(4) COMP.
00081
00082      12  CK-CREDIT-SELECT-DT               PIC XX.
00083      12  CK-CREDIT-ACCEPT-DT               PIC XX.
00084
00085      12  CK-VOID-DATA.
00086          20  CK-VOID-DT                    PIC XX.
00087          20  CK-VOID-REASON                PIC X(25).
00088
00089      12  CK-AR-STATEMENT-DT                PIC XX.
00090      12  CK-PMT-APPLIED                    PIC X.
00091      12  CK-ACH-PAYMENT                    PIC X.
00092           88 PAID-BY-ACH                   VALUE 'P'.
00093           88 NOT-PAID-BY-ACH               VALUE 'N'.
00094      12  FILLER                            PIC X(08).
00095
00281      EJECT
00282
00283 *                                COPY ERCCMKQ.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCMKQ                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.014                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CHECK QUE FILE FOR THE COMMISSION         *
00008 *                      CHECK OF THE CREDIT SYSTEM                *
00009 *                                                                *
00010 *   FILE TYPE = VSAM,KSDS                                        *
00011 *   RECORD SIZE = 1800 RECFORM = FIXED                           *
00012 *                                                                *
00013 *   BASE CLUSTER = ERCMKQ                         RKP=2,LEN=7    *
00014 *       ALTERNATE PATH  = ERCMKQ2  (BY PAYEE CONTRO AND          *
00015 *                                      CONTROL NUMBER)           *
00016 *                                                 RKP=9,LEN=30   *
00017 *                                                                *
00018 *   LOG = NO                                                     *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00020 ******************************************************************
00021  01  COMMISSION-CHECK-QUE.
00022      12  MQ-RECORD-ID                PIC XX.
00023          88  VALID-MQ-ID                     VALUE 'MQ'.
00024
00025      12  MQ-CONTROL-PRIMARY.
00026          16  MQ-COMPANY-CD           PIC X.
00027          16  MQ-CONTROL-NUMBER       PIC S9(8)       COMP.
00028          16  MQ-SEQUENCE-NUMBER      PIC S9(4)       COMP.
00029
00030      12  MQ-CONTROL-BY-PAYEE.
00031          16  MQ-COMPANY-CD-A1        PIC X.
00032          16  MQ-CSR-A1               PIC X(4).
00033          16  MQ-CARRIER-A1           PIC X.
00034          16  MQ-GROUPING-A1          PIC X(6).
00035          16  MQ-PAYEE-A1             PIC X(10).
00036          16  MQ-PAYEE-SEQ-A1         PIC S9(4)       COMP.
00037          16  MQ-CONTROL-NUMBER-A1    PIC S9(8)       COMP.
00038          16  MQ-SEQUENCE-NUMBER-A1   PIC S9(4)       COMP.
00039
00040      12  MQ-ENTRY-TYPE               PIC X.
00041              88  CHECK-ON-QUE           VALUE 'Q'.
00042              88  ALIGNMENT-CHECK        VALUE 'A'.
00043              88  SPOILED-CHECK          VALUE 'S'.
00044              88  PAYMENT-ABORTED        VALUE 'X'.
00045              88  ACH-PAYMENT            VALUE 'P'.
00046
00047      12  FILLER                      PIC X(10).
00048
00049      12  MQ-CREDIT-CHEK-CNTL.
00050          16  MQ-CHEK-CSR             PIC X(4).
00051          16  MQ-CHEK-CARRIER         PIC X.
00052          16  MQ-CHEK-GROUPING        PIC X(6).
00053          16  MQ-CHEK-PAYEE           PIC X(10).
00054          16  MQ-CHEK-PAYEE-SEQ       PIC S9(4)       COMP.
00055          16  MQ-CHEK-SEQ-NO          PIC S9(4)       COMP.
00056
00057      12  FILLER                      PIC X(10).
00058
00059      12  MQ-PAYEE-INFO.
00060          16  MQ-PAYEE-NAME           PIC X(30).
00061          16  MQ-PAYEE-ADDRESS-1      PIC X(30).
00062          16  MQ-PAYEE-ADDRESS-2      PIC X(30).
00063          16  MQ-PAYEE-CITY-ST        PIC X(30).
00064          16  MQ-PAYEE-ZIP-CODE.
00065              20  MQ-PAYEE-ZIP.
00066                  24  FILLER          PIC X(1).
00067                      88 MQ-PAYEE-CANADIAN-POST-CODE
00068                                      VALUE 'A' THRU 'Z'.
00069                  24  FILLER          PIC X(4).
00070              20  MQ-PAYEE-ZIP-EXT    PIC X(4).
00071          16  MQ-PAYEE-CANADIAN-POSTAL-CODES
00072                  REDEFINES MQ-PAYEE-ZIP-CODE.
00073              20  MQ-PAY-CAN-POSTAL-CD-1
00074                                      PIC X(3).
00075              20  MQ-PAY-CAN-POSTAL-CD-2
00076                                      PIC X(3).
00077              20  FILLER              PIC X(3).
00078
00079      12  MQ-CREDIT-PYAJ-CNTL.
00080          16  MQ-PYAJ-CARRIER         PIC X.
00081          16  MQ-PYAJ-GROUPING        PIC X(6).
00082          16  MQ-PYAJ-FIN-RESP        PIC X(10).
00083          16  FILLER                  PIC X(6).
00084
00085      12  MQ-CHECK-NUMBER             PIC X(6).
00086      12  MQ-CHECK-AMOUNT             PIC S9(7)V99    COMP-3.
00087      12  MQ-NUMBER-OF-CK-STUBS       PIC S9(3)       COMP-3.
00088      12  MQ-VOID-DT                  PIC XX.
00089      12  MQ-TIMES-PRINTED            PIC S9(4)       COMP.
00090      12  MQ-PRINT-AT-HHMM            PIC S9(4)       COMP.
00091      12  MQ-CHECK-BY-USER            PIC X(4).
00092      12  MQ-PRE-NUMBERING-SW         PIC X.
00093        88  CHECKS-WERE-NOT-PRE-NUMBERED    VALUE SPACE.
00094        88  CHECKS-WERE-PRE-NUMBERED        VALUE '1'.
00095
00096      12  MQ-CHECK-WRITTEN-DT         PIC XX.
00097      12  MQ-ACH-WRITTEN-DT REDEFINES  MQ-CHECK-WRITTEN-DT
00098                                      PIC XX.
00099      12  MQ-LAST-MAINT-BY            PIC X(4).
00100      12  MQ-LAST-MAINT-HHMMSS        PIC S9(6)       COMP-3.
00101      12  MQ-LAST-MAINT-DT            PIC XX.
00102      12  MQ-CHECK-RELEASE-DT         PIC XX.
00103      12  MQ-RECORD-TYPE              PIC X.
00104          88  MQ-DETAIL                     VALUE 'D'.
00105          88  MQ-TEXT                       VALUE 'T'.
00106
00107      12  MQ-DETAIL-INFORMATION.
00108          16  MQ-DETAIL-INFO        OCCURS 15 TIMES.
00109              20  MQ-CHECK-STUB-LINE.
00110                  24  MQ-STUB-COMMENT        PIC X(23).
00111                  24  MQ-ACCT-AGENT          PIC X(10).
00112                  24  MQ-INVOICE             PIC X(6).
00113                  24  MQ-REFERENCE           PIC X(12).
00114                  24  MQ-LEDGER-NO           PIC X(14).
00115                  24  MQ-PYAJ-AMT            PIC S9(7)V99 COMP-3.
00116                  24  MQ-PYAJ-REC-TYPE       PIC X.
00117                  24  MQ-PYAJ-SEQ            PIC S9(8)    COMP.
00118                  24  MQ-PAYMENT-TYPE        PIC X.
00119                  24  MQ-PYAJ-PMT-APPLIED    PIC X.
00120                  24  MQ-LAST-MAINT-APPLIED  PIC X.
00121                  24  MQ-NON-AR-ITEM         PIC X.
00122                  24  FILLER                 PIC X(19).
00123
00124      12  MQ-CHECK-STUB-TEXT REDEFINES MQ-DETAIL-INFORMATION.
00125          16  MQ-CHECK-TEXT-ITEMS   OCCURS 3 TIMES.
00126              20  MQ-STUB-TEXT        PIC X(70).
00127          16  MQ-STUB-FILLER          PIC X(1260).
00128
00129      12  MQ-CREDIT-SELECT-DATE       PIC XX.
00130      12  MQ-CREDIT-ACCEPT-DATE       PIC XX.
00131
00132      12  MQ-AR-STATEMENT-DT          PIC XX.
00133      12  MQ-CO-TYPE                  PIC X.
00134
00135      12  MQ-STARTING-CHECK-NUMBER    PIC X(06).
00136      12  FILLER                      PIC X(41).
00137 ******************************************************************
00284      EJECT
00285 *                                COPY ERCPYAJ.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ERCPYAJ                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.015                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = PENDING PAYMENT AND ADJUSTMENTS           *
00008 *                                                                *
00009 *                                                                *
00010 *   FILE TYPE = VSAM,KSDS                                        *
00011 *   RECORD SIZE = 200  RECFORM = FIXED                           *
00012 *                                                                *
00013 *   BASE CLUSTER = ERPYAJ                         RKP=2,LEN=33   *
00014 *       ALTERNATE PATHS = NONE                                   *
00015 *                                                                *
00016 *   LOG = YES                                                    *
00017 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00018 ******************************************************************
042303******************************************************************
042303*                   C H A N G E   L O G
042303*
042303* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
042303*-----------------------------------------------------------------
042303*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
042303* EFFECTIVE    NUMBER
042303*-----------------------------------------------------------------
042303* 042303                   PEMA ADD PROCESSING FOR DUE PREM ADJS
060205* 060205                   PEMA ADD ERCOMP TYPE TO ERPYAJ
042303******************************************************************
00019
00020  01  PENDING-PAY-ADJ.
00021      12  PY-RECORD-ID                     PIC XX.
00022          88  VALID-PY-ID                        VALUE 'PY'.
00023
00024      12  PY-CONTROL-PRIMARY.
00025          16  PY-COMPANY-CD                PIC X.
00026          16  PY-CARRIER                   PIC X.
00027          16  PY-GROUPING                  PIC X(6).
00028          16  PY-FIN-RESP                  PIC X(10).
00029          16  PY-ACCOUNT                   PIC X(10).
00030          16  PY-PRODUCER REDEFINES PY-ACCOUNT
00031                                           PIC X(10).
00032          16  PY-FILE-SEQ-NO               PIC S9(8)     COMP.
00033          16  PY-RECORD-TYPE               PIC X.
00034              88  PY-REMIT-RECEIVED            VALUE 'R'.
00035              88  PY-DEPOSIT                   VALUE 'D'.
00036              88  PY-CHARGE-TO-AGENT           VALUE 'C'.
00037              88  PY-ADJ-REM-RECEIVED          VALUE 'S'.
00038              88  PY-ADJ-DEPOSIT               VALUE 'T'.
00039              88  PY-ADJ-CHG-TO-AGT            VALUE 'U'.
00040              88  PY-ADD-TO-YTD-COMP           VALUE 'X'.
00041              88  PY-SUBTRACT-YTD-COMP         VALUE 'Y'.
00042              88  PY-ADD-TO-BALANCE            VALUE 'Z'.
00043              88  PY-FICA-ENTRY                VALUE 'F'.
00044              88  PY-REMIT-IND-GROUPING        VALUE 'G'.
00045              88  PY-POLICY-FEE                VALUE 'W'.
042303             88  PY-DUE-PREM-ADJ              VALUE 'P'.
00046
00047      12  PY-PYMT-TYPE                     PIC X.
00048              88  PY-NEW-BUS-PYMT              VALUE 'B'.
00049              88  PY-REINS-PYMT                VALUE 'R'.
00050              88  PY-EXP-PYMT                  VALUE 'E'.
00051
00052      12  PY-BIL-INV                       PIC X(6).
00053      12  PY-REF-NO                        PIC X(12).
00054
00055      12  PY-LAST-MAINT-DT                 PIC XX.
00056      12  PY-LAST-MAINT-BY                 PIC X(4).
00057      12  PY-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.
00058
00059      12  PY-PYADJ-RECORD.
00060          16  PY-ENTRY-AMT                 PIC S9(7)V99  COMP-3.
00061          16  PY-ENTRY-COMMENT             PIC X(30).
CIDMOD         16  PY-GL-DATA      REDEFINES PY-ENTRY-COMMENT.
CIDMOD             20  PY-GL-ACCOUNT            PIC X(10).
CIDMOD             20  PY-GL-STATE              PIC X(02).
CIDMOD             20  PY-GL-CANC-SW            PIC X(01).
CIDMOD                 88  PY-GL-CANC-SW-ON     VALUE 'Y'.
CIDMOD                 88  PY-GL-CANC-SW-OFF    VALUE 'N'.
CIDMOD             20  PY-GL-COMMENT            PIC X(10).
CIDMOD             20  FILLER      REDEFINES PY-GL-COMMENT.
CIDMOD                 24  PY-GL-CHECK-NO       PIC 9(06).
CIDMOD                 24  FILLER               PIC X(04).
CIDMOD             20  FILLER                   PIC X(07).
00074          16  PY-SAVE-ACCOUNT              PIC X(10).
00075          16  PY-SAVE-TYPE                 PIC X(01).
00076
00077          16  PY-LETTERS.
00078              20  PY-LETTER OCCURS 3 TIMES
00079                            INDEXED BY PY-LET-NDX
00080                                           PIC X(04).
00081
060205         16  PY-ERCOMP-TYPE               PIC X.
060205             88  PY-ACCOUNT-TYPE              VALUE 'A'.
060205             88  PY-GA-TYPE                   VALUE 'G'.
060205             88  PY-BANK-TYPE                 VALUE 'B'.
060205         16  FILLER                       PIC X(05).
00083
00084      12  PY-RECORD-STATUS.
00085          16  PY-CREDIT-SELECT-DT          PIC XX.
00086          16  PY-CREDIT-ACCEPT-DT          PIC XX.
00087          16  PY-BILLED-DATE               PIC XX.
00088          16  PY-REPORTED-DT               PIC XX.
00089          16  PY-PMT-APPLIED               PIC X.
00090              88  PY-ACCOUNT-PMT               VALUE 'A'.
00091              88  PY-GA-PMT                    VALUE 'G'.
00092              88  PY-OVWRITE-PMT               VALUE 'O'.
00093              88  PY-NON-AR-PMT                VALUE 'N'.
00094          16  FILLER                       PIC X(5).
00095          16  PY-INPUT-DT                  PIC XX.
00096          16  PY-CHECK-NUMBER              PIC X(6).
00097          16  PY-VOID-SW                   PIC X.
00098              88  PY-CHECK-VOIDED              VALUE 'V'.
00099          16  PY-CHECK-ORIGIN-SW           PIC X.
00100              88  PY-BILLING-CHECK             VALUE 'B'.
00101              88  PY-REFUND-CHECK              VALUE 'R'.
00102              88  PY-GA-CHECK                  VALUE 'G'.
00103              88  PY-CHECK-WRITTEN             VALUE 'W'.
00104              88  PY-CHECK-REVERSAL            VALUE 'V'.
00105          16  PY-CHECK-WRITTEN-DT          PIC XX.
00106          16  PY-CHECK-QUE-CONTROL         PIC S9(8) COMP.
00107          16  PY-CHECK-QUE-SEQUENCE        PIC S9(4) COMP.
00108          16  PY-BILL-FLAG                 PIC X.
00109              88  PY-BILLED                    VALUE 'B'.
00110          16  PY-AR-FLAG                   PIC X.
00111              88  PY-AR-CYCLE                  VALUE 'C'.
00112              88  PY-AR-MONTH-END              VALUE 'M'.
00113          16  PY-AR-DATE                   PIC XX.
00114
00115      12  PY-GL-CODES.
00116          16  PY-GL-DB                     PIC X(14).
00117          16  PY-GL-CR                     PIC X(14).
00118          16  PY-GL-FLAG                   PIC X.
00119          16  PY-GL-DATE                   PIC XX.
00120
00121      12  PY-CANCEL-FEE-FLAG               PIC X(2).
00122      12  FILLER                           PIC X(3).
00123 ******************************************************************
00286      EJECT
00287
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA
                                CHECK-WORK-RECORDS COMM-CHECK-RECORDS
                                COMMISSION-CHECK-QUE PENDING-PAY-ADJ.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL637' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00289
00290      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00291      MOVE 1                      TO EMI-NUMBER-OF-LINES.
00292
00293      IF EIBCALEN = 0
00294          GO TO 8800-UNAUTHORIZED-ACCESS.
00295
00296      MOVE EIBTRMID               TO QID-TERM.
00297      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00298      MOVE '5'                    TO DC-OPTION-CODE.
00299      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
00300      MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.
00301      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DT.
00302
00303      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00304          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00305              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00306              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00307              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00308              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00309              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00310              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00311              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00312              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00313          ELSE
00314              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00315              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00316              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00317              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00318              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00319              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00320              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00321              MOVE SPACES               TO PI-SAVED-PROGRAM-6.
00322
00323      MOVE LOW-VALUES             TO EL637AI.
00324
00325      IF EIBTRNID NOT = TRANS-EXJB
00326         MOVE SPACES              TO PI-PROGRAM-WORK-AREA
00327         MOVE PI-COMPANY-CD       TO PI-ERCMKQ2-COMPANY-CD
00328         MOVE +9999               TO PI-ERCMKQ2-SEQUENCE-NUMBER
00329         GO TO 8100-SEND-INITIAL-MAP.
00330
00331      
      * EXEC CICS HANDLE CONDITION
00332 *        PGMIDERR  (9600-PGMID-ERROR)
00333 *        ERROR     (9990-ABEND)
00334 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00001870' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031383730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00335
00336      IF EIBAID = DFHCLEAR
00337          GO TO 9400-CLEAR.
00338
00339      IF PI-PROCESSOR-ID = 'LGXX'
00340          GO TO 0200-RECEIVE.
00341
00342      
      * EXEC CICS READQ TS
00343 *        QUEUE  (QID)
00344 *        INTO   (SECURITY-CONTROL)
00345 *        LENGTH (SC-COMM-LENGTH)
00346 *        ITEM   (SC-ITEM)
00347 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00001881' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00348
00349      MOVE SC-CREDIT-DISPLAY (7)   TO PI-DISPLAY-CAP.
00350      MOVE SC-CREDIT-UPDATE  (7)   TO PI-MODIFY-CAP.
00351
00352      IF NOT DISPLAY-CAP
00353          MOVE 'READ'          TO SM-READ
00354          PERFORM 9995-SECURITY-VIOLATION
00355          MOVE ER-0070         TO  EMI-ERROR
00356          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00357          GO TO 8100-SEND-INITIAL-MAP.
00358
00359      EJECT
00360
00361 ******************************************************************
00362 *                                                                *
00363 *              R E C E I V E   M A P                             *
00364 *                                                                *
00365 ******************************************************************
00366
00367  0200-RECEIVE.
00368
00369      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00370          MOVE ER-0008            TO EMI-ERROR
00371          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00372          MOVE -1                 TO MAINTL
00373          GO TO 8200-SEND-DATAONLY.
00374
00375      
      * EXEC CICS RECEIVE
00376 *        MAP      (EL637A)
00377 *        MAPSET   (MAPSET-EL637S)
00378 *        INTO     (EL637AI)
00379 *    END-EXEC.
           MOVE LENGTH OF
            EL637AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00001914' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031393134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL637A, 
                 EL637AI, 
                 DFHEIV11, 
                 MAPSET-EL637S, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00380
00381      IF PFENTERL GREATER THAN ZERO
00382         IF EIBAID NOT = DFHENTER
00383            MOVE ER-0004          TO EMI-ERROR
00384            MOVE AL-UNBOF         TO PFENTERA
00385            MOVE -1               TO PFENTERL
00386            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00387            GO TO 8200-SEND-DATAONLY
00388         ELSE
00389            IF PFENTERI NUMERIC AND
00390               PFENTERI GREATER 0 AND LESS 23
00391               MOVE PF-VALUES (PFENTERI) TO EIBAID
00392            ELSE
00393               MOVE ER-0029       TO EMI-ERROR
00394               MOVE AL-UNBOF      TO PFENTERA
00395               MOVE -1            TO PFENTERL
00396               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00397               GO TO 8200-SEND-DATAONLY.
00398
00399      EJECT
00400
00401 ******************************************************************
00402 *                                                                *
00403 *              C H E C K   P F K E Y S                           *
00404 *                                                                *
00405 ******************************************************************
00406
00407  0300-CHECK-PFKEYS.
00408
00409      IF EIBAID = DFHPF23
00410          GO TO 8810-PF23.
00411
00412      IF EIBAID = DFHPF24
00413          GO TO 9200-RETURN-MAIN-MENU.
00414
00415      IF EIBAID = DFHPF12
00416          GO TO 9500-PF12.
00417
00418      IF EIBAID = DFHPF1
00419         IF PI-END-OF-FILE
00420            MOVE -1               TO MAINTL
00421            MOVE ER-2251          TO EMI-ERROR
00422            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00423            GO TO 8200-SEND-DATAONLY
00424         ELSE
00425            MOVE SPACE            TO PI-MAINT-FUNCTION
00426            GO TO 5000-DISPLAY-PAYEE.
00427
00428      IF EIBAID = DFHPF2
00429          MOVE SPACE              TO PI-MAINT-FUNCTION
00430          GO TO 6100-DISPLAY-PREV-PAYEE.
00431
00432
00433      IF EIBAID = DFHENTER
00434          GO TO 1000-EDIT-MAP.
00435
00436      MOVE ER-0008 TO EMI-ERROR.
00437      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00438      MOVE -1                     TO PFENTERL.
00439
00440      GO TO 8200-SEND-DATAONLY.
00441
00442      EJECT
00443
00444 ******************************************************************
00445 *                                                                *
00446 *                  E D I T    M A P                              *
00447 *                                                                *
00448 ******************************************************************
00449
00450  1000-EDIT-MAP.
00451
00452      IF MAINTI = 'S' OR 'R'
00453         MOVE MAINTI              TO PI-MAINT-FUNCTION
00454         MOVE AL-UANON            TO MAINTA
00455      ELSE
00456         MOVE ER-0023             TO EMI-ERROR
00457         MOVE -1                  TO MAINTL
00458         MOVE AL-UABON            TO MAINTA
00459         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00460         GO TO 8200-SEND-DATAONLY.
00461
00462      IF NOT MODIFY-CAP
00463         IF MAINTI = 'S'
00464            MOVE MAINTI              TO PI-MAINT-FUNCTION
00465         ELSE
00466            MOVE 'UPDATE'            TO SM-READ
00467            PERFORM 9995-SECURITY-VIOLATION
00468            MOVE ER-0070             TO EMI-ERROR
00469            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00470            GO TO 8100-SEND-INITIAL-MAP.
00471
00472      IF MAINTI = 'R'
00473         IF PI-COMPANY-CD = PI-SVCMKQ2-COMPANY-CD AND
00474            CSRI          = PI-SVCMKQ2-CSR        AND
00475            CARI          = PI-SVCMKQ2-CARRIER    AND
00476            GROUPI        = PI-SVCMKQ2-GROUPING   AND
00477            PAYEEI        = PI-SVCMKQ2-PAYEE      AND
00478            PAYSEQI       = PI-SVCMKQ2-PAYEE-SEQ  AND
00479            CNTRLNOI      = PI-SVCMKQ2-CONTROL-NUMBER
00480            GO TO 7500-REVERSE-COMM-CHECK
00481         ELSE
00482            MOVE ER-2056             TO EMI-ERROR
00483            MOVE -1                  TO MAINTL
00484            MOVE AL-UABON            TO MAINTA
00485            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00486            GO TO 8200-SEND-DATAONLY.
00487
00488      IF CSRL GREATER THAN ZERO
00489         MOVE AL-UANON            TO CSRA
00490      ELSE
00491         MOVE LOW-VALUES          TO CSRI.
00492
00493      IF CARL GREATER THAN ZERO
00494         MOVE AL-UANON            TO CARA
00495      ELSE
00496         MOVE -1                  TO CARL
00497         MOVE ER-0194             TO EMI-ERROR
00498         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00499
00500      IF GROUPL GREATER THAN ZEROS
00501         MOVE AL-UANON            TO GROUPA
00502      ELSE
00503         MOVE -1                  TO GROUPL
00504         MOVE ER-0195             TO EMI-ERROR
00505         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00506
00507      IF PAYEEL GREATER THAN ZEROS
00508         MOVE AL-UANON            TO PAYEEA
00509      ELSE
00510         MOVE -1                  TO PAYEEL
00511         MOVE ER-3148             TO EMI-ERROR
00512         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00513
00514      IF PAYSEQL GREATER THAN ZEROS
00515         MOVE AL-UANON            TO PAYSEQA
00516      ELSE
00517         MOVE -1                  TO PAYSEQL
00518         MOVE ER-3176             TO EMI-ERROR
00519         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00520
00521      IF CNTRLNOL GREATER THAN ZEROS
00522         MOVE AL-UANON            TO CNTRLNOA
00523      ELSE
00524         MOVE -1                  TO CNTRLNOL
00525         MOVE ER-3129             TO EMI-ERROR
00526         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00527
00528
00529  1050-EDIT-COMPLETE.
00530
00531      IF EMI-ERROR NOT = ZEROS
00532         GO TO 8200-SEND-DATAONLY.
00533
00534      MOVE PI-COMPANY-CD          TO PI-ERCMKQ2-COMPANY-CD.
00535      MOVE CSRI                   TO PI-ERCMKQ2-CSR.
00536      MOVE CARI                   TO PI-ERCMKQ2-CARRIER.
00537      MOVE GROUPI                 TO PI-ERCMKQ2-GROUPING.
00538      MOVE PAYEEI                 TO PI-ERCMKQ2-PAYEE.
00539      MOVE PAYSEQI                TO PI-ERCMKQ2-PAYEE-SEQ.
00540      MOVE CNTRLNOI               TO PI-ERCMKQ2-CONTROL-NUMBER.
00541
00542      IF MAINTI = 'S'
00543         MOVE +0                 TO PI-ERCMKQ2-SEQUENCE-NUMBER
00544         GO TO 5000-DISPLAY-PAYEE.
00545
00546
00547      EJECT
00548
00549 ******************************************************************
00550 *                                                                *
00551 *               D I S P L A Y   P A Y E E                        *
00552 *                                                                *
00553 ******************************************************************
00554
00555  5000-DISPLAY-PAYEE.
00556
00557      MOVE SPACE                  TO  PI-END-OF-FILE-SW.
00558
00559      IF EIBAID = DFHPF1 OR DFHPF2
00560          IF CSRI EQUAL SPACES OR LOW-VALUES OR ALL '_'
00561              MOVE LOW-VALUES TO PI-ERCMKQ2-CSR.
00562
00563      MOVE LOW-VALUES             TO  EL637AI.
00564      MOVE 'S'                    TO  MAINTI.
00565      MOVE AL-UANON               TO  MAINTA.
00566
00567      MOVE PI-ERCMKQ2-KEY         TO  SVCMKQ2-COMPARE-KEY.
00568
00569      
      * EXEC CICS HANDLE CONDITION
00570 *        NOTFND    (5090-CHECKS-NOTFND)
00571 *    END-EXEC.
      *    MOVE '"$I                   ! # #00002108' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303032313038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00572
00573      MOVE SPACE                  TO WS-BROWSE-SW.
00574
00575      
      * EXEC CICS STARTBR
00576 *        DATASET (FILE-ID-ERCMKQ2)
00577 *        RIDFLD  (PI-ERCMKQ2-KEY)
00578 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00002114' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCMKQ2, 
                 PI-ERCMKQ2-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00579
00580      
      * EXEC CICS HANDLE CONDITION
00581 *        NOTFND    (5070-CHECK-PROCESSED)
00582 *        ENDFILE   (5060-END-OF-FILE)
00583 *    END-EXEC.
      *    MOVE '"$I''                  ! $ #00002119' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303032313139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00584
00585      MOVE 'Y'                    TO WS-BROWSE-SW.
00586
00587  5010-READ-CHECK-QUE-FILE.
00588
00589      
      * EXEC CICS READNEXT
00590 *        SET     (ADDRESS OF COMMISSION-CHECK-QUE)
00591 *        DATASET (FILE-ID-ERCMKQ2)
00592 *        RIDFLD  (PI-ERCMKQ2-KEY)
00593 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00002128' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCMKQ2, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERCMKQ2-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMMISSION-CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00594
00595      IF PI-COMPANY-CD  NOT =  MQ-COMPANY-CD
00596          GO TO      5060-END-OF-FILE.
00597
00598      IF CHECK-ON-QUE OR ACH-PAYMENT
00599          NEXT SENTENCE
00600      ELSE
00601          GO TO 5010-READ-CHECK-QUE-FILE.
00602
00603      MOVE PI-ERCMKQ2-KEY TO ERCMKQ2-COMPARE-KEY.
00604
00605      IF WS-FIRST-TIME
00606         IF PI-SHOW-FUNCTION
00607            IF ERCMKQ2-COMPARE-KEY NOT = SVCMKQ2-COMPARE-KEY
00608               MOVE PI-SAVE-ERCMKQ2-KEY TO PI-ERCMKQ2-KEY
00609               GO TO 5090-CHECKS-NOTFND
00610            ELSE
00611               MOVE PI-ERCMKQ2-KEY    TO PI-SAVE-ERCMKQ2-KEY
00612               MOVE 'N'               TO WS-FIRST-TIME-SW
00613               GO TO 5025-PROCESS-PAYEE
00614         ELSE
00615            MOVE PI-ERCMKQ2-KEY       TO PI-SAVE-ERCMKQ2-KEY
00616            MOVE 'N'                  TO WS-FIRST-TIME-SW
00617            GO TO 5025-PROCESS-PAYEE.
00618
00619      IF ERCMKQ2-COMPARE-KEY  NOT =  SVCMKQ2-COMPARE-KEY
00620          GO TO 5070-CHECK-PROCESSED.
00621
00622      IF MQ-CHECK-AMOUNT NOT = ZERO
00623          MOVE MQ-CHECK-AMOUNT    TO CHKAMTO.
00624
00625      IF MQ-CHECK-NUMBER GREATER THAN WS-STRT-CK-NO
00626          MOVE '-'                TO DASHO
00627          MOVE MQ-CHECK-NUMBER    TO ENDCHKO.
00628
00629      GO TO 5010-READ-CHECK-QUE-FILE.
00630
00631  5025-PROCESS-PAYEE.
00632
00633      MOVE AL-UANON               TO  CSRA.
00634      MOVE MQ-CSR-A1              TO  CSRO.
00635      MOVE AL-UANON               TO  CARA.
00636      MOVE MQ-CARRIER-A1          TO  CARO.
00637      MOVE AL-UANON               TO  CARA.
00638      MOVE MQ-GROUPING-A1         TO  GROUPO.
00639      MOVE AL-UANON               TO  GROUPA.
00640      MOVE MQ-PAYEE-A1            TO  PAYEEO.
00641      MOVE AL-UANON               TO  PAYEEA.
00642      MOVE MQ-PAYEE-SEQ-A1        TO  PAYSEQO.
00643      MOVE AL-UANON               TO  PAYSEQA.
00644      MOVE MQ-CONTROL-NUMBER-A1   TO  CNTRLNOO.
00645      MOVE AL-UANON               TO  CNTRLNOA.
00646      MOVE MQ-PAYEE-NAME          TO  PAYTOO.
00647      MOVE MQ-PAYEE-ADDRESS-1     TO  ADDRS1O.
00648      MOVE MQ-PAYEE-ADDRESS-2     TO  ADDRS2O.
00649      MOVE MQ-PAYEE-CITY-ST       TO  CITYSTO.
00650
00651      IF  MQ-PAYEE-CANADIAN-POST-CODE
00652          MOVE MQ-PAY-CAN-POSTAL-CD-1
00653                                  TO  ZIPO
00654          MOVE MQ-PAY-CAN-POSTAL-CD-2
00655                                  TO  ZIPEXTO
00656
00657      ELSE
00658          MOVE MQ-PAYEE-ZIP       TO  ZIPO
00659          MOVE MQ-PAYEE-ZIP-EXT   TO  ZIPEXTO.
00660
00661      MOVE MQ-LAST-MAINT-BY       TO  MAINTBYO.
00662      MOVE MQ-LAST-MAINT-HHMMSS   TO  WS-TIME.
00663      MOVE WS-HR-MINS             TO  MAINTATO.
00664      MOVE MQ-LAST-MAINT-DT       TO  DC-BIN-DATE-1.
00665      MOVE SPACE                  TO  DC-OPTION-CODE.
00666      PERFORM 8500-DATE-CONVERT.
00667
00668      IF NO-CONVERSION-ERROR
00669         MOVE DC-GREG-DATE-1-EDIT TO  MAINTONO.
00670
00671      IF MQ-CHECK-WRITTEN-DT GREATER THAN LOW-VALUES
00672         MOVE MQ-CHECK-WRITTEN-DT TO  DC-BIN-DATE-1
00673         MOVE SPACE               TO  DC-OPTION-CODE
00674         PERFORM 8500-DATE-CONVERT
00675         IF NO-CONVERSION-ERROR
00676            MOVE DC-GREG-DATE-1-EDIT TO  CHKWRTNO.
00677
00678      IF MQ-CHECK-RELEASE-DT GREATER THAN LOW-VALUES
00679         MOVE MQ-CHECK-RELEASE-DT TO  DC-BIN-DATE-1
00680         MOVE SPACE               TO  DC-OPTION-CODE
00681         PERFORM 8500-DATE-CONVERT
00682         IF NO-CONVERSION-ERROR
00683            MOVE DC-GREG-DATE-1-EDIT TO  CHKRELO.
00684
00685      IF MQ-VOID-DT   GREATER THAN LOW-VALUES
00686         MOVE MQ-VOID-DT TO  DC-BIN-DATE-1
00687         MOVE SPACE               TO  DC-OPTION-CODE
00688         PERFORM 8500-DATE-CONVERT
00689         IF NO-CONVERSION-ERROR
00690            MOVE DC-GREG-DATE-1-EDIT TO  VOIDDTO.
00691
00692      IF MQ-CHECK-AMOUNT NOT = ZERO
00693          MOVE MQ-CHECK-AMOUNT    TO CHKAMTO.
00694
00695      IF MQ-CHECK-NUMBER GREATER THAN WS-STRT-CK-NO
00696          MOVE '-'                TO DASHO
00697          MOVE MQ-CHECK-NUMBER    TO ENDCHKO.
00698
00699      MOVE ' '                    TO DASHO.
00700
00701      MOVE MQ-CHECK-NUMBER        TO STRTCHKO.
00702
00703      GO TO 5010-READ-CHECK-QUE-FILE.
00704
00705
00706  5060-END-OF-FILE.
00707
00708      MOVE 'Y'                    TO PI-END-OF-FILE-SW.
00709      MOVE ER-2251                TO EMI-ERROR.
00710      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00711
00712  5070-CHECK-PROCESSED.
00713
00714      IF WS-BROWSE-STARTED
00715         
      * EXEC CICS ENDBR
00716 *            DATASET (FILE-ID-ERCMKQ2)
00717 *       END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00002254' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032323534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCMKQ2, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00718
00719      MOVE 'S'                    TO  PI-MAINT-FUNCTION.
00720      MOVE -1                     TO  MAINTL.
00721      GO TO 8100-SEND-INITIAL-MAP.
00722
00723  5080-END-OF-ENTRIES.
00724
00725      MOVE -1                     TO MAINTL.
00726      GO TO 8200-SEND-DATAONLY.
00727
00728  5090-CHECKS-NOTFND.
00729
00730      MOVE SPACE                  TO PI-MAINT-FUNCTION.
00731      MOVE ER-3160                TO EMI-ERROR.
00732      MOVE -1                     TO CARL.
00733      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00734      GO TO 8200-SEND-DATAONLY.
00735
00736      EJECT
00737
00738 ******************************************************************
00739 *                                                                *
00740 *            D I S P L A Y   P R E V   P A Y E E                 *
00741 *                                                                *
00742 ******************************************************************
00743
00744  6100-DISPLAY-PREV-PAYEE.
00745
00746
00747      IF PI-SAVE-ERCMKQ2-KEY = SPACES
00748         GO TO 6160-END-OF-FILE.
00749
00750      MOVE PI-SAVE-ERCMKQ2-KEY    TO PI-ERCMKQ2-KEY.
00751
00752      
      * EXEC CICS HANDLE CONDITION
00753 *        ENDFILE   (6160-END-OF-FILE)
00754 *    END-EXEC.
      *    MOVE '"$''                   ! % #00002291' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303032323931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00755
00756      
      * EXEC CICS STARTBR
00757 *        DATASET (FILE-ID-ERCMKQ2)
00758 *        RIDFLD  (PI-ERCMKQ2-KEY)
00759 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00002295' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032323935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCMKQ2, 
                 PI-ERCMKQ2-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00760
00761  6110-READ-PREV-ENTRY.
00762
00763      
      * EXEC CICS READPREV
00764 *        SET     (ADDRESS OF COMMISSION-CHECK-QUE)
00765 *        DATASET (FILE-ID-ERCMKQ2)
00766 *        RIDFLD  (PI-ERCMKQ2-KEY)
00767 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00002302' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCMKQ2, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERCMKQ2-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMMISSION-CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00768
00769      IF PI-COMPANY-CD  NOT =  MQ-COMPANY-CD
00770          GO TO      6160-END-OF-FILE.
00771
00772      IF PI-ERCMKQ2-KEY = PI-SAVE-ERCMKQ2-KEY
00773         GO TO 6110-READ-PREV-ENTRY.
00774
00775      IF CHECK-ON-QUE OR ACH-PAYMENT
00776          NEXT SENTENCE
00777      ELSE
00778          GO TO 6110-READ-PREV-ENTRY.
00779
00780      IF MQ-TEXT
00781         GO TO 6110-READ-PREV-ENTRY.
00782
00783      
      * EXEC CICS ENDBR
00784 *         DATASET (FILE-ID-ERCMKQ2)
00785 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00002322' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCMKQ2, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00786
00787      IF PI-ERCMKQ2-COMPANY-CD NOT = PI-COMPANY-CD
00788         GO TO 6160-END-OF-FILE
00789      ELSE
00790         MOVE +0                  TO PI-ERCMKQ2-SEQUENCE-NUMBER
00791         GO TO 5000-DISPLAY-PAYEE.
00792
00793  6160-END-OF-FILE.
00794
00795      MOVE SPACE                  TO PI-MAINT-FUNCTION.
00796      MOVE ER-2252                TO EMI-ERROR.
00797      MOVE -1                     TO CARL.
00798      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00799      MOVE  PI-SAVE-ERCMKQ2-KEY   TO  PI-ERCMKQ2-KEY.
00800      GO TO 8200-SEND-DATAONLY.
00801
00802      EJECT
00803
00804 ******************************************************************
00805 *                                                                *
00806 *       R E V E R S E   C O M M I S I O N   C H E C K            *
00807 *                CHECK QUE FILE (ERCMKQ)                         *
00808 *                                                                *
00809 ******************************************************************
00810
00811  7500-REVERSE-COMM-CHECK.
00812
00813      
      * EXEC CICS HANDLE CONDITION
00814 *        NOTFND    (7580-PAYEE-NOTFND)
00815 *    END-EXEC.
      *    MOVE '"$I                   ! & #00002352' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303032333532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00816
00817      MOVE SPACE                  TO WS-BROWSE-SW.
00818      MOVE PI-SAVE-ERCMKQ2-KEY    TO PI-ERCMKQ2-KEY
00819                                     SVCMKQ2-COMPARE-KEY.
00820
00821  7505-PROCESS-CHECK-QUE-FILE.
00822
00823      
      * EXEC CICS STARTBR
00824 *        DATASET (FILE-ID-ERCMKQ2)
00825 *        RIDFLD  (PI-ERCMKQ2-KEY)
00826 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00002362' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCMKQ2, 
                 PI-ERCMKQ2-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00827
00828      
      * EXEC CICS HANDLE CONDITION
00829 *        NOTFND    (7570-COMM-CHECK-REVERSED)
00830 *        ENDFILE   (7570-COMM-CHECK-REVERSED)
00831 *    END-EXEC.
      *    MOVE '"$I''                  ! '' #00002367' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303032333637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00832
00833      MOVE 'Y'                    TO WS-BROWSE-SW.
00834
00835  7510-READ-CHECK-QUE-FILE.
00836
00837      
      * EXEC CICS READNEXT
00838 *        SET     (ADDRESS OF COMMISSION-CHECK-QUE)
00839 *        DATASET (FILE-ID-ERCMKQ2)
00840 *        RIDFLD  (PI-ERCMKQ2-KEY)
00841 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00002376' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCMKQ2, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERCMKQ2-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMMISSION-CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00842
00843      IF PI-COMPANY-CD  NOT =  MQ-COMPANY-CD
00844          GO TO      7570-COMM-CHECK-REVERSED.
00845
00846      IF CHECK-ON-QUE OR ACH-PAYMENT
00847          NEXT SENTENCE
00848      ELSE
00849          GO TO 7510-READ-CHECK-QUE-FILE.
00850
00851      IF PI-ERCMKQ2-KEY = ERCMKQ2-PREV-KEY
00852         GO TO 7510-READ-CHECK-QUE-FILE.
00853
00854      MOVE PI-ERCMKQ2-KEY TO ERCMKQ2-COMPARE-KEY.
00855
00856      IF WS-FIRST-TIME
00857         IF ERCMKQ2-COMPARE-KEY NOT = SVCMKQ2-COMPARE-KEY
00858            GO TO 7580-PAYEE-NOTFND
00859         ELSE
00860            MOVE 'N'        TO WS-FIRST-TIME-SW
00861            GO TO 7525-PROCESS-PAYEE.
00862
00863      IF ERCMKQ2-COMPARE-KEY  NOT =  SVCMKQ2-COMPARE-KEY
00864         GO TO 7570-COMM-CHECK-REVERSED.
00865
00866  7525-PROCESS-PAYEE.
00867
00868      IF MQ-VOID-DT GREATER THAN LOW-VALUES
00869         GO TO 7585-REVERSAL-ERROR.
00870
00871      MOVE PI-ERCMKQ2-KEY         TO ERCMKQ2-PREV-KEY.
00872
00873      IF MQ-TEXT
00874          NEXT SENTENCE
00875      ELSE
00876         IF MQ-CHECK-WRITTEN-DT EQUAL LOW-VALUES OR SPACES
00877             NEXT SENTENCE
00878         ELSE
00879             PERFORM 7600-REVERSE-PMTS-ADJS  THRU 7690-EXIT.
00880
00881      PERFORM 7700-REVERSE-COMM-CHECK THRU 7790-EXIT.
00882
00883      MOVE MQ-COMPANY-CD          TO ERCMKQ-COMPANY-CD.
00884      MOVE MQ-CONTROL-NUMBER      TO ERCMKQ-CONTROL-NUMBER.
00885      MOVE MQ-SEQUENCE-NUMBER     TO ERCMKQ-SEQUENCE-NUMBER.
00886
00887      
      * EXEC CICS ENDBR
00888 *         DATASET (FILE-ID-ERCMKQ2)
00889 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00002426' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCMKQ2, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00890
00891      
      * EXEC CICS READ
00892 *        SET     (ADDRESS OF COMMISSION-CHECK-QUE)
00893 *        DATASET (FILE-ID-ERCMKQ)
00894 *        RIDFLD  (ERCMKQ-KEY)
00895 *        UPDATE
00896 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00002430' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCMKQ, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCMKQ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMMISSION-CHECK-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00897
00898      MOVE PI-PROCESSOR-ID        TO MQ-LAST-MAINT-BY.
00899      MOVE EIBTIME                TO MQ-LAST-MAINT-HHMMSS.
00900      MOVE WS-CURRENT-BIN-DT      TO MQ-LAST-MAINT-DT
00901                                     MQ-VOID-DT.
00902
00903      MOVE LOW-VALUES             TO MQ-CREDIT-SELECT-DATE
00904                                     MQ-CREDIT-ACCEPT-DATE.
00905
00906      
      * EXEC CICS REWRITE
00907 *         FROM    (COMMISSION-CHECK-QUE)
00908 *         DATASET (FILE-ID-ERCMKQ)
00909 *    END-EXEC.
           MOVE LENGTH OF
            COMMISSION-CHECK-QUE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00002445' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCMKQ, 
                 COMMISSION-CHECK-QUE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00910
00911      GO TO 7505-PROCESS-CHECK-QUE-FILE.
00912
00913  7570-COMM-CHECK-REVERSED.
00914
00915      IF WS-BROWSE-STARTED
00916         
      * EXEC CICS ENDBR
00917 *            DATASET (FILE-ID-ERCMKQ2)
00918 *       END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00002455' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCMKQ2, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00919
00920      MOVE SPACE                     TO WS-BROWSE-SW.
00921      MOVE 'Y'                       TO WS-FIRST-TIME-SW.
00922
00923      MOVE PI-SAVE-ERCMKQ2-KEY       TO PI-ERCMKQ2-KEY.
00924
00925      MOVE ER-0000                TO EMI-ERROR.
00926      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00927      GO TO 5000-DISPLAY-PAYEE.
00928
00929  7580-PAYEE-NOTFND.
00930
00931      
      * EXEC CICS ENDBR
00932 *         DATASET (FILE-ID-ERCMKQ2)
00933 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00002470' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCMKQ2, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00934
00935      MOVE SPACE                  TO PI-MAINT-FUNCTION.
00936      MOVE ER-3160                TO EMI-ERROR.
00937      MOVE -1                     TO CARL.
00938      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00939      GO TO 8200-SEND-DATAONLY.
00940
00941  7585-REVERSAL-ERROR.
00942
00943      
      * EXEC CICS ENDBR
00944 *         DATASET (FILE-ID-ERCMKQ2)
00945 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00002482' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCMKQ2, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00946
00947      MOVE SPACE                  TO PI-MAINT-FUNCTION.
00948      MOVE ER-3171                TO EMI-ERROR.
00949      MOVE -1                     TO MAINTL.
00950      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00951      GO TO 8200-SEND-DATAONLY.
00952
00953  7590-EXIT.
00954      EXIT.
00955      EJECT
00956
00957 ******************************************************************
00958 *                                                                *
00959 *   R E V E R S E   P A Y M E N T S  &  A D J U S T M E N T S    *
00960 *                                                                *
00961 ******************************************************************
00962
00963  7600-REVERSE-PMTS-ADJS.
00964
00965      
      * EXEC CICS GETMAIN
00966 *         SET     (ADDRESS OF PENDING-PAY-ADJ)
00967 *         LENGTH  (ERPYAJ-RECORD-LENGTH)
00968 *         INITIMG (GETMAIN-SPACE)
00969 *    END-EXEC.
      *    MOVE ',"IL                  $   #00002504' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERPYAJ-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00970
00971      COMPUTE WS-WORK-SEQ-NO = EIBTIME * 10.
00972
00973      MOVE SPACES                 TO PENDING-PAY-ADJ.
00974      MOVE 'PY'                   TO PY-RECORD-ID.
00975      MOVE MQ-COMPANY-CD-A1       TO PY-COMPANY-CD.
00976      MOVE MQ-CARRIER-A1          TO PY-CARRIER.
00977      MOVE MQ-GROUPING-A1         TO PY-GROUPING.
00978      MOVE MQ-PAYEE-A1            TO PY-FIN-RESP.
00979      MOVE 'B'                    TO PY-PYMT-TYPE.
00980      MOVE WS-CURRENT-BIN-DT      TO PY-LAST-MAINT-DT.
00981      MOVE PI-PROCESSOR-ID        TO PY-LAST-MAINT-BY.
00982      MOVE EIBTIME                TO PY-LAST-MAINT-HHMMSS.
00983      MOVE PI-AR-MONTH-END-DT     TO PY-CREDIT-SELECT-DT.
00984      MOVE LOW-VALUES             TO PY-CREDIT-ACCEPT-DT.
00985      MOVE LOW-VALUES             TO PY-BILLED-DATE.
00986      MOVE LOW-VALUES             TO PY-REPORTED-DT.
00987      MOVE WS-CURRENT-BIN-DT      TO PY-INPUT-DT.
00988      MOVE MQ-CHECK-NUMBER        TO PY-CHECK-NUMBER.
00989      MOVE ' '                    TO PY-VOID-SW.
00990      MOVE LOW-VALUES             TO PY-BILLED-DATE.
00991      MOVE LOW-VALUE              TO PY-CHECK-WRITTEN-DT.
00992      MOVE ZEROS                  TO PY-CHECK-QUE-CONTROL.
00993      MOVE ZEROS                  TO PY-CHECK-QUE-SEQUENCE.
00994      MOVE LOW-VALUES             TO PY-AR-DATE.
00995      MOVE 'V'                    TO PY-CHECK-ORIGIN-SW.
00996
00997      MOVE +0                     TO WS-SUB1.
00998
00999  7610-PROCESS-PMT-ADJS.
01000
01001      ADD +1                      TO WS-SUB1.
01002
01003      IF WS-SUB1 GREATER THAN +15
01004         GO TO 7690-EXIT.
01005
01006      IF MQ-LAST-MAINT-APPLIED (WS-SUB1) EQUAL 'H'
01007          GO TO 7610-PROCESS-PMT-ADJS
01008      ELSE
01009          IF MQ-PYAJ-SEQ    (WS-SUB1) = ZEROS
01010              GO TO 7690-EXIT.
01011
01012 ******************************************************************
01013 *    MODIFIED 07/14 TO MATCH FIX APPLIED TO PRODUCTION MODULE.
01014 *    THIS WAS APPLIED TO ELIMINATE DUPREC CONDITION ON WRITE OF
01015 *    PYAJ RECORDS WRITTEN IN REVERSAL
01016 ******************************************************************
01017
01018      ADD +1                          TO WS-WORK-SEQ-HOLD.
01019      COMPUTE PY-FILE-SEQ-NO = WS-WORK-SEQ-NO + WS-WORK-SEQ-HOLD.
01020
01021      MOVE MQ-PYAJ-REC-TYPE    (WS-SUB1) TO PY-RECORD-TYPE.
01022      MOVE MQ-ACCT-AGENT       (WS-SUB1) TO PY-ACCOUNT.
01023      MOVE MQ-INVOICE          (WS-SUB1) TO PY-BIL-INV.
01024      MOVE MQ-REFERENCE        (WS-SUB1) TO PY-REF-NO.
01025      MOVE MQ-LEDGER-NO        (WS-SUB1) TO PY-GL-CR.
01026      MOVE MQ-PYAJ-PMT-APPLIED (WS-SUB1) TO PY-PMT-APPLIED.
01027
01028      COMPUTE PY-ENTRY-AMT = MQ-PYAJ-AMT (WS-SUB1) * -1.
01029
01030      MOVE 'CHECK REVERSAL'           TO PY-ENTRY-COMMENT.
01031
01032      
      * EXEC CICS WRITE
01033 *         DATASET   (FILE-ID-ERPYAJ)
01034 *         FROM      (PENDING-PAY-ADJ)
01035 *         RIDFLD    (PY-CONTROL-PRIMARY)
01036 *    END-EXEC.
           MOVE LENGTH OF
            PENDING-PAY-ADJ
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00002571' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPYAJ, 
                 PENDING-PAY-ADJ, 
                 DFHEIV11, 
                 PY-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01037
01038      GO TO 7610-PROCESS-PMT-ADJS.
01039
01040  7690-EXIT.
01041      EXIT.
01042
01043      EJECT
01044
01045
01046 ******************************************************************
01047 *                                                                *
01048 *       R E V E R S E   C O M M I S S I O N   C H E C K          *
01049 *              COMMISSION CHECK FILE (ERCMCK)                    *
01050 *                                                                *
01051 ******************************************************************
01052
01053  7700-REVERSE-COMM-CHECK.
01054
01055      
      * EXEC CICS HANDLE CONDITION
01056 *        NOTFND    (7790-EXIT)
01057 *    END-EXEC.
      *    MOVE '"$I                   ! ( #00002594' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303032353934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01058
01059      MOVE MQ-COMPANY-CD          TO ERCMCK-COMPANY-CD.
01060      MOVE MQ-CHEK-CSR            TO ERCMCK-CSR.
01061      MOVE MQ-CHEK-CARRIER        TO ERCMCK-CARRIER.
01062      MOVE MQ-CHEK-GROUPING       TO ERCMCK-GROUPING.
01063      MOVE MQ-CHEK-PAYEE          TO ERCMCK-PAYEE.
01064      MOVE MQ-CHEK-PAYEE-SEQ      TO ERCMCK-PAYEE-SEQ.
01065      MOVE MQ-CHEK-SEQ-NO         TO ERCMCK-SEQUENCE-NO.
01066
01067      
      * EXEC CICS READ
01068 *        SET     (ADDRESS OF COMM-CHECK-RECORDS)
01069 *        DATASET (FILE-ID-ERCMCK)
01070 *        RIDFLD  (ERCMCK-KEY)
01071 *        UPDATE
01072 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00002606' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCMCK, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCMCK-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMM-CHECK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01073
01074      IF MQ-TEXT
01075          PERFORM 7900-REVERSE-CHECK-TEXT-RECS THRU 7990-EXIT
01076      ELSE
01077          PERFORM 7800-REVERSE-CHECK-WORK-RECS THRU 7890-EXIT.
01078
01079      
      * EXEC CICS DELETE
01080 *         DATASET (FILE-ID-ERCMCK)
01081 *    END-EXEC.
      *    MOVE '&(                    &   #00002618' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCMCK, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01082
01083  7790-EXIT.
01084      EXIT.
01085
01086      EJECT
01087
01088 ******************************************************************
01089 *                                                                *
01090 *       R E V E R S E   C H E C K   W O R K   R E C S .          *
01091 *               CHECK WORK FILE (ERCKWK)                         *
01092 *                                                                *
01093 ******************************************************************
01094
01095  7800-REVERSE-CHECK-WORK-RECS.
01096
01097      
      * EXEC CICS HANDLE CONDITION
01098 *        NOTFND    (7810-PROCESS-CHECK-WORK-RECS)
01099 *    END-EXEC.
      *    MOVE '"$I                   ! ) #00002636' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303032363336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01100
01101      MOVE +0                     TO WS-SUB1.
01102
01103  7810-PROCESS-CHECK-WORK-RECS.
01104
01105      ADD +1                      TO WS-SUB1.
01106
01107      IF WS-SUB1 GREATER THAN +15
01108         GO TO 7870-UPDATE-HEADER-REC.
01109
01110      IF CK-CKWK-PAYEE      (WS-SUB1) = SPACES
01111         GO TO 7870-UPDATE-HEADER-REC.
01112
01113      MOVE CK-COMPANY-CD              TO ERCKWK-COMPANY-CD.
01114      MOVE CK-CKWK-CSR      (WS-SUB1) TO ERCKWK-CSR.
01115      MOVE CK-CKWK-CARRIER  (WS-SUB1) TO ERCKWK-CARRIER.
01116      MOVE CK-CKWK-GROUPING (WS-SUB1) TO ERCKWK-GROUPING.
01117      MOVE CK-CKWK-PAYEE    (WS-SUB1) TO ERCKWK-PAYEE.
01118      MOVE CK-CKWK-PAYEE-SEQ (WS-SUB1)
01119                                      TO ERCKWK-PAYEE-SEQ.
01120      MOVE CK-CKWK-SEQ-NO   (WS-SUB1) TO ERCKWK-SEQUENCE-NO.
01121
01122      
      * EXEC CICS READ
01123 *        SET     (ADDRESS OF CHECK-WORK-RECORDS)
01124 *        DATASET (FILE-ID-ERCKWK)
01125 *        RIDFLD  (ERCKWK-KEY)
01126 *        UPDATE
01127 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00002661' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCKWK-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-WORK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01128
01129      MOVE 'Y'                        TO WS-ERCKWK-PROCESS-SW.
01130
01131      MOVE WS-CURRENT-BIN-DT          TO CW-RECORDED-DT.
01132      MOVE PI-PROCESSOR-ID            TO CW-RECORDED-BY.
01133      MOVE EIBTIME                    TO CW-LAST-MAINT-HHMMSS.
01134
01135      MOVE LOW-VALUES                 TO CW-RELEASE-DT
01136                                         CW-CREDIT-SELECT-DT
01137                                         CW-CREDIT-ACCEPT-DT.
01138
01139      
      * EXEC CICS REWRITE
01140 *         FROM    (CHECK-WORK-RECORDS)
01141 *         DATASET (FILE-ID-ERCKWK)
01142 *    END-EXEC.
           MOVE LENGTH OF
            CHECK-WORK-RECORDS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00002678' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 CHECK-WORK-RECORDS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01143
01144      GO TO 7810-PROCESS-CHECK-WORK-RECS.
01145
01146  7870-UPDATE-HEADER-REC.
01147
01148      MOVE +0                        TO ERCKWK-SEQUENCE-NO.
01149
01150      
      * EXEC CICS HANDLE CONDITION
01151 *        NOTFND    (7880-HEADER-NOTFND)
01152 *    END-EXEC.
      *    MOVE '"$I                   ! * #00002689' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303032363839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01153
01154      
      * EXEC CICS READ
01155 *        SET     (ADDRESS OF CHECK-WORK-RECORDS)
01156 *        DATASET (FILE-ID-ERCKWK)
01157 *        RIDFLD  (ERCKWK-KEY)
01158 *        UPDATE
01159 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00002693' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCKWK-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-WORK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01160
01161      MOVE WS-CURRENT-BIN-DT          TO CW-RECORDED-DT.
01162      MOVE PI-PROCESSOR-ID            TO CW-RECORDED-BY.
01163      MOVE EIBTIME                    TO CW-LAST-MAINT-HHMMSS.
01164
01165      MOVE LOW-VALUES                 TO CW-RELEASE-DT
01166                                         CW-CREDIT-SELECT-DT
01167                                         CW-CREDIT-ACCEPT-DT.
01168
01169      
      * EXEC CICS REWRITE
01170 *         FROM    (CHECK-WORK-RECORDS)
01171 *         DATASET (FILE-ID-ERCKWK)
01172 *    END-EXEC.
           MOVE LENGTH OF
            CHECK-WORK-RECORDS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00002708' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 CHECK-WORK-RECORDS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01173
01174      GO TO 7890-EXIT.
01175
01176  7880-HEADER-NOTFND.
01177
01178      IF WS-ERCKWK-PROCESSED
01179         NEXT SENTENCE
01180      ELSE
01181         GO TO 7890-EXIT.
01182
01183      
      * EXEC CICS SYNCPOINT ROLLBACK
01184 *    END-EXEC.
      *    MOVE '6"R                   !   #00002722' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01185
01186      MOVE SPACE                  TO PI-MAINT-FUNCTION.
01187      MOVE ER-3161                TO EMI-ERROR.
01188      MOVE -1                     TO CARL.
01189      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01190      GO TO 8200-SEND-DATAONLY.
01191
01192  7890-EXIT.
01193      EXIT.
01194
01195      EJECT
01196
01197 ******************************************************************
01198 *                                                                *
01199 *       R E V E R S E   C H E C K   T E X T   R E C S .          *
01200 *              COMMISSION CHECK FILE (ERCMCK)                    *
01201 *                                                                *
01202 ******************************************************************
01203
01204  7900-REVERSE-CHECK-TEXT-RECS.
01205
01206      
      * EXEC CICS HANDLE CONDITION
01207 *        NOTFND    (7990-EXIT)
01208 *    END-EXEC.
      *    MOVE '"$I                   ! + #00002745' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303032373435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01209
01210      MOVE MQ-COMPANY-CD              TO ERCKWK-COMPANY-CD.
01211      MOVE MQ-CHEK-CSR                TO ERCKWK-CSR.
01212      MOVE MQ-CHEK-CARRIER            TO ERCKWK-CARRIER.
01213      MOVE MQ-CHEK-GROUPING           TO ERCKWK-GROUPING.
01214      MOVE MQ-CHEK-PAYEE              TO ERCKWK-PAYEE.
01215      MOVE MQ-CHEK-PAYEE-SEQ          TO ERCKWK-PAYEE-SEQ.
01216      MOVE +8999                      TO ERCKWK-SEQUENCE-NO.
01217
01218  7910-PROCESS-CHECK-TEXT-RECS.
01219
01220      IF ERCKWK-SEQUENCE-NO LESS THAN WS-MAX-TEXT-SEQ
01221          ADD +1                      TO ERCKWK-SEQUENCE-NO
01222      ELSE
01223          GO TO 7990-EXIT.
01224
01225      
      * EXEC CICS READ
01226 *        SET     (ADDRESS OF CHECK-WORK-RECORDS)
01227 *        DATASET (FILE-ID-ERCKWK)
01228 *        RIDFLD  (ERCKWK-KEY)
01229 *        UPDATE
01230 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00002764' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCKWK-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-WORK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01231
01232      MOVE 'Y'                        TO WS-ERCKWK-PROCESS-SW.
01233
01234      MOVE WS-CURRENT-BIN-DT          TO CW-RECORDED-DT.
01235      MOVE PI-PROCESSOR-ID            TO CW-RECORDED-BY.
01236      MOVE EIBTIME                    TO CW-LAST-MAINT-HHMMSS.
01237
01238      MOVE LOW-VALUES                 TO CW-RELEASE-DT
01239                                         CW-CREDIT-SELECT-DT
01240                                         CW-CREDIT-ACCEPT-DT.
01241
01242      
      * EXEC CICS REWRITE
01243 *         FROM    (CHECK-WORK-RECORDS)
01244 *         DATASET (FILE-ID-ERCKWK)
01245 *    END-EXEC.
           MOVE LENGTH OF
            CHECK-WORK-RECORDS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00002781' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 CHECK-WORK-RECORDS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01246
01247      GO TO 7910-PROCESS-CHECK-TEXT-RECS.
01248
01249  7990-EXIT.
01250      EXIT.
01251
01252      EJECT
01253
01254 ******************************************************************
01255 *                                                                *
01256 *             S E N D    I N I T I A L   M A P                   *
01257 *                                                                *
01258 ******************************************************************
01259
01260  8100-SEND-INITIAL-MAP.
01261
01262      MOVE WS-CURRENT-DT          TO DATEO.
01263      MOVE EIBTIME                TO TIME-IN.
01264      MOVE TIME-OUT               TO TIMEO.
01265      MOVE -1                     TO MAINTL.
01266
01267      MOVE EMI-MESSAGE-AREA (1)   TO ERMESGO.
01268
01269      
      * EXEC CICS SEND
01270 *        MAP      (EL637A)
01271 *        MAPSET   (MAPSET-EL637S)
01272 *        FROM     (EL637AI)
01273 *        ERASE
01274 *        CURSOR
01275 *    END-EXEC.
           MOVE LENGTH OF
            EL637AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00002808' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL637A, 
                 EL637AI, 
                 DFHEIV12, 
                 MAPSET-EL637S, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01276
01277      GO TO 9100-RETURN-TRAN.
01278
01279      EJECT
01280
01281 ******************************************************************
01282 *                                                                *
01283 *              S E N D    D A T A O N L Y                        *
01284 *                                                                *
01285 ******************************************************************
01286
01287  8200-SEND-DATAONLY.
01288
01289      MOVE WS-CURRENT-DT          TO DATEO.
01290      MOVE EIBTIME                TO TIME-IN.
01291      MOVE TIME-OUT               TO TIMEO.
01292
01293      MOVE EMI-MESSAGE-AREA (1)   TO ERMESGO.
01294
01295      
      * EXEC CICS SEND
01296 *         MAP      (EL637A)
01297 *         MAPSET   (MAPSET-EL637S)
01298 *         FROM     (EL637AI)
01299 *         DATAONLY
01300 *         CURSOR
01301 *    END-EXEC.
           MOVE LENGTH OF
            EL637AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00002834' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL637A, 
                 EL637AI, 
                 DFHEIV12, 
                 MAPSET-EL637S, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01302
01303      GO TO 9100-RETURN-TRAN.
01304
01305      EJECT
01306
01307  8300-SEND-TEXT.
01308      
      * EXEC CICS SEND TEXT
01309 *        FROM     (LOGOFF-TEXT)
01310 *        LENGTH   (LOGOFF-LENGTH)
01311 *        ERASE
01312 *        FREEKB
01313 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00002847' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383437' TO DFHEIV0(25:11)
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
           
01314
01315      
      * EXEC CICS RETURN
01316 *    END-EXEC.
      *    MOVE '.(                    &   #00002854' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01317
01318
01319  8400-LOG-JOURNAL-RECORD.
01320      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.
01321      MOVE THIS-PGM                TO JP-PROGRAM-ID.
01322
01323 *    EXEC CICS JOURNAL
01324 *        JFILEID     (PI-JOURNAL-FILE-ID)
01325 *        JTYPEID     ('EL')
01326 *        FROM        (JOURNAL-RECORD)
01327 *        LENGTH      (WS-JOURNAL-RECORD-LENGTH)
01328 *        END-EXEC.
01329
01330  8500-DATE-CONVERT.
01331      
      * EXEC CICS LINK
01332 *        PROGRAM  (LINK-ELDATCV)
01333 *        COMMAREA (DATE-CONVERSION-DATA)
01334 *        LENGTH   (DC-COMM-LENGTH)
01335 *    END-EXEC.
      *    MOVE '."C                   ''   #00002870' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LINK-ELDATCV, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01336
01337  8500-EXIT.
01338      EXIT.
01339
01340      EJECT
01341
01342  8600-DEEDIT.
01343
01344      
      * EXEC CICS BIF DEEDIT
01345 *         FIELD   (DEEDIT-FIELD)
01346 *         LENGTH  (12)
01347 *    END-EXEC.
           MOVE 12
             TO DFHEIV11
      *    MOVE '@"L                   #   #00002883' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01348
01349  8800-UNAUTHORIZED-ACCESS.
01350      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
01351      GO TO 8300-SEND-TEXT.
01352
01353  8810-PF23.
01354      MOVE EIBAID                 TO PI-ENTRY-CD-1.
01355      MOVE XCTL-EL005             TO PGM-NAME.
01356      GO TO 9300-XCTL.
01357
01358  9200-RETURN-MAIN-MENU.
01359      MOVE XCTL-EL626             TO PGM-NAME.
01360      GO TO 9300-XCTL.
01361
01362  9000-RETURN-CICS.
01363      
      * EXEC CICS RETURN
01364 *    END-EXEC.
      *    MOVE '.(                    &   #00002902' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01365
01366  9100-RETURN-TRAN.
01367
01368      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
01369      MOVE '852A'                 TO PI-CURRENT-SCREEN-NO.
01370
01371      
      * EXEC CICS RETURN
01372 *        TRANSID    (TRANS-EXJB)
01373 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
01374 *        LENGTH     (PI-COMM-LENGTH)
01375 *    END-EXEC.
      *    MOVE '.(CT                  &   #00002910' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-EXJB, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01376
01377  9300-XCTL.
01378      
      * EXEC CICS XCTL
01379 *        PROGRAM    (PGM-NAME)
01380 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
01381 *        LENGTH     (PI-COMM-LENGTH)
01382 *    END-EXEC.
      *    MOVE '.$C                   $   #00002917' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01383
01384  9400-CLEAR.
01385      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME
01386      GO TO 9300-XCTL.
01387
01388  9500-PF12.
01389      MOVE XCTL-EL010             TO PGM-NAME.
01390      GO TO 9300-XCTL.
01391
01392  9600-PGMID-ERROR.
01393
01394      
      * EXEC CICS HANDLE CONDITION
01395 *        PGMIDERR    (8300-SEND-TEXT)
01396 *    END-EXEC.
      *    MOVE '"$L                   ! , #00002933' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303032393333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01397
01398      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
01399      MOVE ' '                    TO PI-ENTRY-CD-1.
01400      MOVE XCTL-EL005             TO PGM-NAME.
01401      MOVE PGM-NAME               TO LOGOFF-PGM.
01402      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
01403      GO TO 9300-XCTL.
01404
01405  9900-ERROR-FORMAT.
01406
01407      IF NOT EMI-ERRORS-COMPLETE
01408          MOVE LINK-EL001         TO PGM-NAME
01409          
      * EXEC CICS LINK
01410 *            PROGRAM    (PGM-NAME)
01411 *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
01412 *            LENGTH     (EMI-COMM-LENGTH)
01413 *        END-EXEC.
      *    MOVE '."C                   ''   #00002948' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01414
01415  9900-EXIT.
01416      EXIT.
01417
01418  9990-ABEND.
01419      MOVE LINK-EL004             TO PGM-NAME.
01420      MOVE DFHEIBLK               TO EMI-LINE1.
01421      
      * EXEC CICS LINK
01422 *        PROGRAM   (PGM-NAME)
01423 *        COMMAREA  (EMI-LINE1)
01424 *        LENGTH    (72)
01425 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00002960' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01426
01427      MOVE -1                     TO PFENTERL.
01428
01429      GO TO 8200-SEND-DATAONLY.
01430
01431      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL637' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
01432
01433      EJECT
01434
01435  9995-SECURITY-VIOLATION.
01436 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00002992' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393932' TO DFHEIV0(25:11)
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
01437
01438  9995-EXIT.
01439      EXIT.
01440

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL637' TO DFHEIV1
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
               GO TO 5090-CHECKS-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 5070-CHECK-PROCESSED,
                     5060-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 6160-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 7580-PAYEE-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 7570-COMM-CHECK-REVERSED,
                     7570-COMM-CHECK-REVERSED
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 7790-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 7810-PROCESS-CHECK-WORK-RECS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 7880-HEADER-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 7990-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL637' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
