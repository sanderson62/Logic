00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL852 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/14/96 08:03:10.
00007 *                            VMOD=2.007
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
00024 *REMARKS. TRANSACTION - EXJ2 - ACCOUNTS RECEIVABLE
00025 *                              REQUEST FILE SELECTION.
00026
00027  ENVIRONMENT DIVISION.
00028
00029      EJECT
00030  DATA DIVISION.
00031  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00032
00033  77  FILLER  PIC X(32)  VALUE '********************************'.
00034  77  FILLER  PIC X(32)  VALUE '*    EL852 WORKING STORAGE     *'.
00035  77  FILLER  PIC X(32)  VALUE '************ V/M 2.007 *********'.
00036
00037     EJECT
00038
00039 *                            COPY ELCSCTM.
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
00040 *                            COPY ELCSCRTY.
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
00041
00042     EJECT
00043
00044 ******************************************************************
00045 *                                                                *
00046 *              S T A N D A R D   A R E A S                       *
00047 *                                                                *
00048 ******************************************************************
00049
00050  01  STANDARD-AREAS.
00051      12  SC-ITEM             PIC S9(4)   VALUE +1   COMP.
00052      12  GETMAIN-SPACE       PIC X       VALUE SPACE.
00053      12  EL852A              PIC X(8)    VALUE 'EL852A'.
00054      12  EL852B              PIC X(8)    VALUE 'EL852B'.
00055      12  EL852C              PIC X(8)    VALUE 'EL852C'.
00056      12  EL852D              PIC X(8)    VALUE 'EL852D'.
00057      12  MAPSET-EL852S       PIC X(8)    VALUE 'EL852S'.
00058      12  TRANS-EXJ2          PIC X(4)    VALUE 'EXJ2'.
00059      12  THIS-PGM            PIC X(8)    VALUE 'EL852 '.
00060      12  PGM-NAME            PIC X(8).
00061      12  TIME-IN             PIC S9(7).
00062      12  TIME-OUT-R  REDEFINES TIME-IN.
00063          16  FILLER          PIC X.
00064          16  TIME-OUT        PIC 99V99.
00065          16  FILLER              PIC X(2).
00066      12  LINK-EL001              PIC X(8)    VALUE 'EL001'.
00067      12  LINK-EL004              PIC X(8)    VALUE 'EL004'.
00068      12  XCTL-EL005              PIC X(8)    VALUE 'EL005'.
00069      12  XCTL-EL010              PIC X(8)    VALUE 'EL010'.
00070      12  XCTL-EL626              PIC X(8)    VALUE 'EL626'.
00071      12  XCTL-EL8521             PIC X(8)    VALUE 'EL8521'.
00072      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.
00073      12  FILE-ID-ERRQST          PIC X(8)    VALUE 'ERRQST'.
00074      12  FILE-ID-ERRQST2         PIC X(8)    VALUE 'ERRQST2'.
00075      12  FILE-ID-ERRQST3         PIC X(8)    VALUE 'ERRQST3'.
00076      12  FILE-ID-ERRQST4         PIC X(8)    VALUE 'ERRQST4'.
00077      12  FILE-ID-ERRQST5         PIC X(8)    VALUE 'ERRQST5'.
00078      12  FILE-ID-ERSUMM          PIC X(8)    VALUE 'ERSUMM'.
00079      12  WS-CURRENT-DT           PIC X(8)    VALUE SPACES.
00080      12  WS-CURRENT-BIN-DT       PIC XX      VALUE SPACES.
00081      12  WS-DATA-SELECTED-SW     PIC X       VALUE SPACE.
00082          88 WS-DATA-SELECTED                 VALUE 'Y'.
00083      12  WS-OPTION-COUNTER       PIC S999    VALUE ZEROS COMP-3.
00084      12  WS-MAINT-FUNCTION       PIC X       VALUE SPACE.
00085          88 WS-BROWSE-FUNCTION               VALUE 'B'.
00086          88 WS-SUBMIT-FUNCTION               VALUE 'S'.
00087          88 WS-RESUBMIT-FUNCTION             VALUE 'R'.
00088      12  WS-REQUEST-BROWSE-SW    PIC X       VALUE SPACE.
00089          88 WS-REQUEST-BROWSE-STARTED        VALUE 'Y'.
00090      12  WS-SUMMARY-BROWSE-SW    PIC X       VALUE SPACE.
00091          88 WS-SUMMARY-BROWSE-STARTED        VALUE 'Y'.
00092      12  WS-SAVE-ACCESS-KEY      PIC X(46)   VALUE SPACES.
00093      12  WS-RQST-ACCT-CONTROL    PIC X(20)   VALUE SPACES.
00094      12  WS-RQST-FIN-RESP-CNTL   PIC X(18)   VALUE SPACES.
00095      12  WS-ACCESS-KEY           PIC X(46)   VALUE SPACES.
00096      12  QID.
00097          16  QID-TERM            PIC X(4)      VALUE SPACES.
00098          16  FILLER              PIC X(4)      VALUE '125D'.
00099
00100
00101      EJECT
00102
00103 ******************************************************************
00104 *                                                                *
00105 *                E R R O R   M E S S A G E S                     *
00106 *                                                                *
00107 ******************************************************************
00108
00109  01  ERROR-MESSAGES.
00110      12  ER-0000                 PIC X(4)  VALUE '0000'.
00111      12  ER-0004                 PIC X(4)  VALUE '0004'.
00112      12  ER-0008                 PIC X(4)  VALUE '0008'.
00113      12  ER-0023                 PIC X(4)  VALUE '0023'.
00114      12  ER-0029                 PIC X(4)  VALUE '0029'.
00115      12  ER-0070                 PIC X(4)  VALUE '0070'.
00116      12  ER-2132                 PIC X(4)  VALUE '2132'.
00117      12  ER-2134                 PIC X(4)  VALUE '2134'.
00118      12  ER-2919                 PIC X(4)  VALUE '2919'.
00119      12  ER-2935                 PIC X(4)  VALUE '2935'.
00120      12  ER-3133                 PIC X(4)  VALUE '3133'.
00121
00122      EJECT
00123
00124 ******************************************************************
00125 *                                                                *
00126 *              A C C E S S   K E Y S                             *
00127 *                                                                *
00128 ******************************************************************
00129
00130  01  ACCESS-KEYS.
00131      12  ERRQST-KEY.
00132          16  ERRQST-COMPANY-CD       PIC X     VALUE SPACE.
00133          16  ERRQST-ENTRY-BATCH      PIC X(6)  VALUE SPACES.
00134
00135      12  ERRQST-ALT-KEY1.
00136          16  ERRQST-ACCT-CONTROL.
00137              20 ERRQST-COMPANY-CD-A1 PIC X     VALUE SPACES.
00138              20 ERRQST-CARRIER-A1    PIC X     VALUE SPACES.
00139              20 ERRQST-GROUPING-A1   PIC X(6)  VALUE SPACES.
00140              20 ERRQST-STATE-A1      PIC XX    VALUE SPACES.
00141              20 ERRQST-ACCOUNT-A1    PIC X(10) VALUE SPACES.
00142          16  ERRQST-REFERENCE-A1     PIC X(12) VALUE SPACES.
00143          16  ERRQST-BATCH-A1         PIC X(6)  VALUE SPACES.
00144
00145      12  ERRQST-ALT-KEY2.
00146          16  ERRQST-FIN-RESP-CONTROL.
00147              20 ERRQST-COMPANY-CD-A2 PIC X     VALUE SPACES.
00148              20 ERRQST-CARRIER-A2    PIC X     VALUE SPACES.
00149              20 ERRQST-GROUPING-A2   PIC X(6)  VALUE SPACES.
00150              20 ERRQST-FIN-RESP-A2   PIC X(10) VALUE SPACES.
00151          16  ERRQST-ACCT-AGENT-A2    PIC X(10) VALUE SPACES.
00152          16  ERRQST-REFERENCE-A2     PIC X(12) VALUE SPACES.
00153          16  ERRQST-BATCH-A2         PIC X(6)  VALUE SPACES.
00154
00155      12  ERRQST-ALT-KEY3.
00156          16  ERRQST-COMPANY-CD-A3    PIC X     VALUE SPACES.
00157          16  ERRQST-CARRIER-A3       PIC X     VALUE SPACES.
00158          16  ERRQST-GROUPING-A3      PIC X     VALUE SPACES.
00159          16  ERRQST-ACCOUNT-AGENT    PIC X     VALUE SPACES.
00160          16  ERRQST-BATCH-A3         PIC X     VALUE SPACES.
00161
00162      12  ERRQST-ALT-KEY4.
00163          16  ERRQST-COMPANY-CD-A4    PIC X     VALUE SPACES.
00164          16  ERRQST-SUMMARY-CODE     PIC X(6)  VALUE SPACES.
00165          16  ERRQST-ACCOUNT-A4       PIC X(10) VALUE SPACES.
00166          16  ERRQST-REFERENCE-A4     PIC X(12) VALUE SPACES.
00167          16  ERRQST-BATCH-A4         PIC X(6)  VALUE SPACES.
00168
00169      12  ERRQST-RECORD-LENGTH        PIC S9(4) COMP VALUE +200.
00170      12  ERRQST-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +223.
00171
00172
00173
00174      EJECT
00175
00176 *                            COPY ELCDATE.
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
00177
00178      EJECT
00179 *                            COPY ELCLOGOF.
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
00180
00181      EJECT
00182 *                            COPY ELCATTR.
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
00183
00184      EJECT
00185 *                                  COPY ELCEMIB.
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
00186
00187      EJECT
00188 *                            COPY ELCINTF.
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
00189 *    COPY ELC852PI.
00001
00002 ******************************************************************
00003 *                            ELC852PI                            *
00004 *                            VMOD=2.002                          *
00005 *                                                                *
00006 *                                                                *
00007 ******************************************************************
00008
00009      12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.
00010          16  PI-OPTION               PIC X.
00011              88  PI-NO-OPTION-SELECTED     VALUE ' '.
00012              88  PI-OPTION-ONE-SELECTED    VALUE '1'.
00013              88  PI-OPTION-TWO-SELECTED    VALUE '2'.
00014              88  PI-OPTION-THREE-SELECTED  VALUE '3'.
00015              88  PI-OPTION-FOUR-SELECTED   VALUE '4'.
00016
00017          16  PI-FILE-ID              PIC X(8).
00018          16  PI-ACCESS-KEY           PIC X(46).
00019          16  PI-SELECT-KEY           PIC X(46).
00020
00021          16  PI-1ST-REQUEST-KEY      PIC X(46).
00022
00023          16  PI-MAP-NAME             PIC X(8).
00024
00025          16  PI-PREV-MAP-NAME        PIC X(8).
00026
00027          16  PI-TOP-OF-FILE-SW       PIC X.
00028              88 PI-TOP-OF-FILE              VALUE 'Y'.
00029
00030          16  PI-END-OF-FILE-SW       PIC X.
00031              88 PI-END-OF-FILE              VALUE 'Y'.
00032
00033          16  PI-REQUEST-KEY OCCURS 10 PIC X(46).
00034
00035          16  FILLER                  PIC X(15).
00036
00037      EJECT
00190      EJECT
00191 *                            COPY ELCJPFX.
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
00192                              PIC X(223).
00193
00194      EJECT
00195 *                            COPY ELCAID.
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
00196  01  FILLER    REDEFINES DFHAID.
00197      12  FILLER              PIC X(8).
00198      12  PF-VALUES           PIC X       OCCURS 2.
00199
00200      EJECT
00201 *                            COPY EL852S.
       01  EL852AI.
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
           05  FRCARL PIC S9(0004) COMP.
           05  FRCARF PIC  X(0001).
           05  FILLER REDEFINES FRCARF.
               10  FRCARA PIC  X(0001).
           05  FRCARI PIC  X(0001).
      *    -------------------------------
           05  FRGRPL PIC S9(0004) COMP.
           05  FRGRPF PIC  X(0001).
           05  FILLER REDEFINES FRGRPF.
               10  FRGRPA PIC  X(0001).
           05  FRGRPI PIC  X(0006).
      *    -------------------------------
           05  FRESPL PIC S9(0004) COMP.
           05  FRESPF PIC  X(0001).
           05  FILLER REDEFINES FRESPF.
               10  FRESPA PIC  X(0001).
           05  FRESPI PIC  X(0010).
      *    -------------------------------
           05  ACCTCARL PIC S9(0004) COMP.
           05  ACCTCARF PIC  X(0001).
           05  FILLER REDEFINES ACCTCARF.
               10  ACCTCARA PIC  X(0001).
           05  ACCTCARI PIC  X(0001).
      *    -------------------------------
           05  ACCTGRPL PIC S9(0004) COMP.
           05  ACCTGRPF PIC  X(0001).
           05  FILLER REDEFINES ACCTGRPF.
               10  ACCTGRPA PIC  X(0001).
           05  ACCTGRPI PIC  X(0006).
      *    -------------------------------
           05  ACCTSTL PIC S9(0004) COMP.
           05  ACCTSTF PIC  X(0001).
           05  FILLER REDEFINES ACCTSTF.
               10  ACCTSTA PIC  X(0001).
           05  ACCTSTI PIC  X(0002).
      *    -------------------------------
           05  ACCOUNTL PIC S9(0004) COMP.
           05  ACCOUNTF PIC  X(0001).
           05  FILLER REDEFINES ACCOUNTF.
               10  ACCOUNTA PIC  X(0001).
           05  ACCOUNTI PIC  X(0010).
      *    -------------------------------
           05  ACCTREFL PIC S9(0004) COMP.
           05  ACCTREFF PIC  X(0001).
           05  FILLER REDEFINES ACCTREFF.
               10  ACCTREFA PIC  X(0001).
           05  ACCTREFI PIC  X(0012).
      *    -------------------------------
           05  SUMMARYL PIC S9(0004) COMP.
           05  SUMMARYF PIC  X(0001).
           05  FILLER REDEFINES SUMMARYF.
               10  SUMMARYA PIC  X(0001).
           05  SUMMARYI PIC  X(0006).
      *    -------------------------------
           05  BATCHL PIC S9(0004) COMP.
           05  BATCHF PIC  X(0001).
           05  FILLER REDEFINES BATCHF.
               10  BATCHA PIC  X(0001).
           05  BATCHI PIC  X(0006).
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
       01  EL852AO REDEFINES EL852AI.
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
           05  FRCARO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FRGRPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FRESPO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCTCARO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCTGRPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCTSTO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCOUNTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCTREFO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SUMMARYO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BATCHO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERMESGO PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  X(0002).
      *    -------------------------------
00202
00203      EJECT
00204
00205
00206      EJECT
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
00208  01  DFHCOMMAREA             PIC X(1024).
00209
00210      EJECT
00211
00212 *                                COPY ERCRQST.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCRQST.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = ACCOUNTS RECEIVABLE REQUEST RECORD        *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 200  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ERRQST                         RKP=2,LEN=7    *
00013 *       ALTERNATE PATH1 = ERRQST2  (BY CO, CAR, GROUP, ST,       *
00014 *                                   ACCOUNT, REF, BATCH)         *
00015 *                                                RKP=9, LEN=38   *
00016 *       ALTERNATE PATH2 = ERRQST3  (BY CO, CAR, GROUP, FIN  RESP *
00017 *                                   ACCOUNT, REF, BATCH)         *
00018 *                                                RKP=47, LEN=46  *
00019 *       ALTERNATE PATH3 = ERRQST4  (BY CO, CAR, GROUP, AGENT,    *
00020 *                                   BATCH)                       *
00021 *                                                RKP=93, LEN=24  *
00022 *       ALTERNATE PATH4 = ERRQST5  (BY CO, SUMMARY CODE, ACCT,   *
00023 *                                   REF, BATCH)                  *
00024 *                                                RKP=117, LEN=35 *
00025 *   LOG = NO                                                     *
00026 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00027 ******************************************************************
00028
00029  01  AR-REQUEST-RECORD.
00030      12  RQ-RECORD-ID                     PIC XX.
00031          88  VALID-RQ-ID                        VALUE 'RQ'.
00032
00033      12  RQ-CONTROL-PRIMARY.
00034          16  RQ-COMPANY-CD                PIC X.
00035          16  RQ-ENTRY-BATCH               PIC X(6).
00036
00037      12  RQ-CONTROL-BY-ACCT-REF.
00038          16  RQ-COMPANY-CD-A1             PIC X.
00039          16  RQ-CARRIER-A1                PIC X.
00040          16  RQ-GROUPING-A1               PIC X(6).
00041          16  RQ-STATE-A1                  PIC XX.
00042          16  RQ-ACCOUNT-A1                PIC X(10).
00043          16  RQ-REFERENCE-A1              PIC X(12).
00044          16  RQ-BATCH-A1                  PIC X(6).
00045
00046      12  RQ-CONTROL-BY-FIN-RESP.
00047          16  RQ-COMPANY-CD-A2             PIC X.
00048          16  RQ-CARRIER-A2                PIC X.
00049          16  RQ-GROUPING-A2               PIC X(6).
00050          16  RQ-FIN-RESP-A2               PIC X(10).
00051          16  RQ-ACCT-AGENT-A2             PIC X(10).
00052          16  RQ-REFERENCE-A2              PIC X(12).
00053          16  RQ-BATCH-A2                  PIC X(6).
00054
00055      12  RQ-CONTROL-BY-ACCT-AGENT.
00056          16  RQ-COMPANY-CD-A3             PIC X.
00057          16  RQ-CARRIER-A3                PIC X.
00058          16  RQ-GROUPING-A3               PIC X(6).
00059          16  RQ-ACCT-AGENT-A3             PIC X(10).
00060          16  RQ-BATCH-A3                  PIC X(6).
00061
00062      12  RQ-CONTROL-BY-SUMMARY.
00063          16  RQ-COMPANY-CD-A4             PIC X.
00064          16  RQ-SUMMARY-CODE              PIC X(6).
00065          16  RQ-ACCOUNT-A4                PIC X(10).
00066          16  RQ-REFERENCE-A4              PIC X(12).
00067          16  RQ-BATCH-A4                  PIC X(6).
00068
00069      12  RQ-REQUEST-METHOD                PIC X.
00070          88 RQ-FIN-RESP-REQUEST               VALUE 'F'.
00071          88 RQ-ACCT-AGENT-REQUEST             VALUE 'A'.
00072          88 RQ-SUMMARY-REQUEST                VALUE 'S'.
00073          88 RQ-BATCH-REQUEST                  VALUE 'B'.
00074      12  FILLER                           PIC X.
00075      12  RQ-STATUS                        PIC X.
00076          88  RQ-REQUEST-ERROR                 VALUE 'E'.
00077          88  RQ-RESUBMIT                      VALUE 'R'.
00078      12  RQ-PROCESSOR-ID                  PIC X(4).
00079      12  RQ-ENTRY-DT                      PIC XX.
00080      12  RQ-MO-END-DT                     PIC XX.
00081      12  RQ-REQUEST-DT                    PIC XX.
00082      12  RQ-STMT-DT                       PIC XX.
00083      12  RQ-REVERSAL-DT                   PIC XX.
00084      12  RQ-CREDIT-SELECT-DT              PIC XX.
00085      12  RQ-CREDIT-ACCEPT-DT              PIC XX.
00086
00087      12  FILLER                           PIC X(27).
00088
00089 ******************************************************************
00090
00213      EJECT
00214
00215 *                                COPY ERCSUMM.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCSUMM                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   ONLINE CREDIT SYSTEM                                         *
00008 *                                                                *
00009 *   FILE DESCRIPTION = AR SUMMARY CROSS REFERENCE                *
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 150           RECFORM = FIXED                  *
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ERSUMM                   RKP=2,LEN=34    *
00015 *                                                                *
00016 *       ALTERNATE PATH1 = ERSUMM2  (BY CO SUMMARY CARR           *
00017 *                                      GROUP F.R. AGENT)         *
00018 *                                                 RKP=36 ,LEN=34 *
00019 *                                                                *
00020 *   LOG = NO                                                     *
00021 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00022 *                                                                *
00023 ******************************************************************
00024
00025  01  SUMM-CROSS-REFERENCE.
00026      12  SX-RECORD-ID                PIC XX.
00027          88  VALID-SX-ID             VALUE 'SX'.
00028
00029      12  SX-CONTROL-PRIMARY.
00030          16  SX-COMPANY-CD           PIC X.
00031          16  SX-SUMMARY              PIC X(6).
00032          16  SX-CARRIER              PIC X.
00033          16  SX-GROUP                PIC X(6).
00034          16  SX-FIN-RESP             PIC X(10).
00035          16  SX-ACCT-AGENT           PIC X(10).
00036
00037      12  SX-CONTROL-A1.
00038          16  SX-COMPANY-A1           PIC X.
00039          16  SX-ACCT-AGENT-A1        PIC X(10).
00040          16  SX-SUMMARY-A1           PIC X(6).
00041          16  SX-CARR-A1              PIC X.
00042          16  SX-GROUP-A1             PIC X(6).
00043          16  SX-FIN-RESP-A1          PIC X(10).
00044
00045      12  SX-MAINT-INFORMATION.
00046          16  SX-LAST-MAINT-DT        PIC XX.
00047          16  SX-LAST-MAINT-BY        PIC X(4).
00048          16  SX-LAST-MAINT-HHMMSS    PIC S9(7)  COMP-3.
00049
00050      12  SX-SUMM-OR-AGT-NAME         PIC X(30).
00051
00052      12  FILLER                      PIC X(40).
00053
00054 ******************************************************************
00216      EJECT
00217
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA AR-REQUEST-RECORD
                                SUMM-CROSS-REFERENCE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL852' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00219
00220      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00221      MOVE 1                      TO EMI-NUMBER-OF-LINES.
00222
00223      IF EIBCALEN = 0
00224          GO TO 8800-UNAUTHORIZED-ACCESS.
00225
00226      MOVE EIBTRMID               TO QID-TERM.
00227      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00228      MOVE '5'                    TO DC-OPTION-CODE.
00229      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
00230      MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.
00231      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DT.
00232
00233      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00234          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00235              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00236              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00237              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00238              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00239              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00240              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00241              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00242              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00243          ELSE
00244              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00245              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00246              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00247              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00248              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00249              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00250              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00251              MOVE SPACES               TO PI-SAVED-PROGRAM-6.
00252
00253      MOVE LOW-VALUES             TO EL852AI.
00254
00255      IF EIBTRNID NOT = TRANS-EXJ2
00256         GO TO 8100-SEND-INITIAL-MAP.
00257
00258      
      * EXEC CICS HANDLE CONDITION
00259 *        PGMIDERR  (9600-PGMID-ERROR)
00260 *        ERROR     (9990-ABEND)
00261 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00001418' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031343138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00262
00263      IF EIBAID = DFHCLEAR
00264          GO TO 9400-CLEAR.
00265
00266
00267      IF PI-PROCESSOR-ID = 'LGXX'
00268          GO TO 0200-RECEIVE.
00269
00270      
      * EXEC CICS READQ TS
00271 *        QUEUE  (QID)
00272 *        INTO   (SECURITY-CONTROL)
00273 *        LENGTH (SC-COMM-LENGTH)
00274 *        ITEM   (SC-ITEM)
00275 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00001430' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00276
00277      MOVE SC-CREDIT-DISPLAY (2)   TO PI-DISPLAY-CAP.
00278      MOVE SC-CREDIT-UPDATE  (2)   TO PI-MODIFY-CAP.
00279
00280      IF NOT DISPLAY-CAP
00281          MOVE 'READ'          TO SM-READ
00282          PERFORM 9995-SECURITY-VIOLATION
00283          MOVE ER-0070         TO  EMI-ERROR
00284          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00285          GO TO 8100-SEND-INITIAL-MAP.
00286
00287      EJECT
00288
00289 ******************************************************************
00290 *                                                                *
00291 *              R E C E I V E   M A P                             *
00292 *                                                                *
00293 ******************************************************************
00294
00295  0200-RECEIVE.
00296
00297      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00298          MOVE ER-0008            TO EMI-ERROR
00299          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00300          MOVE -1                 TO MAINTL
00301          GO TO 8200-SEND-DATAONLY.
00302
00303      
      * EXEC CICS RECEIVE
00304 *        MAP      (EL852A)
00305 *        MAPSET   (MAPSET-EL852S)
00306 *        INTO     (EL852AI)
00307 *    END-EXEC.
           MOVE LENGTH OF
            EL852AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00001463' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL852A, 
                 EL852AI, 
                 DFHEIV11, 
                 MAPSET-EL852S, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00308
00309
00310      IF PFENTERL GREATER THAN ZERO
00311         IF EIBAID NOT = DFHENTER
00312            MOVE ER-0004          TO EMI-ERROR
00313            MOVE AL-UNBOF         TO PFENTERA
00314            MOVE -1               TO PFENTERL
00315            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00316            GO TO 8200-SEND-DATAONLY
00317         ELSE
00318            IF (PFENTERI NUMERIC) AND
00319               (PFENTERI GREATER 0 AND LESS 25)
00320               MOVE PF-VALUES (PFENTERI) TO EIBAID
00321            ELSE
00322               MOVE ER-0029       TO EMI-ERROR
00323               MOVE AL-UNBOF      TO PFENTERA
00324               MOVE -1            TO PFENTERL
00325               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00326               GO TO 8200-SEND-DATAONLY.
00327
00328      EJECT
00329
00330 ******************************************************************
00331 *                                                                *
00332 *              C H E C K   P F K E Y S                           *
00333 *                                                                *
00334 ******************************************************************
00335
00336  0300-CHECK-PFKEYS.
00337
00338      IF EIBAID = DFHPF23
00339          GO TO 8810-PF23.
00340
00341      IF EIBAID = DFHPF24
00342          GO TO 9200-RETURN-MAIN-MENU.
00343
00344      IF EIBAID = DFHPF12
00345          GO TO 9500-PF12.
00346
00347      IF EIBAID = DFHENTER
00348          GO TO 1000-EDIT-MAP.
00349
00350      MOVE ER-0008 TO EMI-ERROR.
00351      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00352      MOVE -1                     TO PFENTERL.
00353
00354      GO TO 8200-SEND-DATAONLY.
00355
00356      EJECT
00357
00358 ******************************************************************
00359 *                                                                *
00360 *                  E D I T    M A P                              *
00361 *                                                                *
00362 ******************************************************************
00363
00364  1000-EDIT-MAP.
00365
00366      MOVE SPACES                 TO PI-PROGRAM-WORK-AREA.
00367
00368      IF MAINTI = 'S' OR 'R' OR 'B'
00369         MOVE MAINTI              TO WS-MAINT-FUNCTION
00370         MOVE AL-UANON            TO MAINTA
00371      ELSE
00372         MOVE ER-0023             TO EMI-ERROR
00373         MOVE -1                  TO MAINTL
00374         MOVE AL-UABON            TO MAINTA
00375         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00376         GO TO 8200-SEND-DATAONLY.
00377
00378      IF NOT MODIFY-CAP
00379         IF MAINTI = 'B'
00380            NEXT SENTENCE
00381         ELSE
00382            MOVE 'UPDATE'            TO SM-READ
00383            PERFORM 9995-SECURITY-VIOLATION
00384            MOVE ER-0070             TO EMI-ERROR
00385            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00386            GO TO 8100-SEND-INITIAL-MAP.
00387
00388      IF FRCARL GREATER THAN ZERO OR
00389         FRGRPL GREATER THAN ZERO OR
00390         FRESPL GREATER THAN ZERO
00391         MOVE '1'                 TO PI-OPTION
00392         ADD +1                   TO WS-OPTION-COUNTER.
00393
00394      IF ACCTCARL GREATER THAN ZERO OR
00395         ACCTGRPL GREATER THAN ZERO OR
00396         ACCTSTL  GREATER THAN ZERO OR
00397         ACCOUNTL GREATER THAN ZERO OR
00398         ACCTREFL GREATER THAN ZERO
00399         MOVE '2'                 TO PI-OPTION
00400         ADD +1                   TO WS-OPTION-COUNTER.
00401
00402      IF SUMMARYL GREATER THAN ZERO
00403         MOVE '3'                 TO PI-OPTION
00404         ADD +1                   TO WS-OPTION-COUNTER.
00405
00406      IF BATCHL   GREATER THAN ZERO
00407         MOVE '4'                 TO PI-OPTION
00408         ADD +1                   TO WS-OPTION-COUNTER.
00409
00410      IF WS-OPTION-COUNTER GREATER THAN +1
00411         MOVE SPACE               TO PI-OPTION
00412         MOVE ER-2919             TO EMI-ERROR
00413         MOVE -1                  TO FRCARL
00414         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00415         GO TO 8200-SEND-DATAONLY.
00416
00417      IF (WS-SUBMIT-FUNCTION OR WS-RESUBMIT-FUNCTION)
00418         IF WS-OPTION-COUNTER GREATER THAN +0
00419            NEXT SENTENCE
00420         ELSE
00421            MOVE SPACE            TO PI-OPTION
00422            MOVE ER-2935          TO EMI-ERROR
00423            MOVE -1               TO MAINTL
00424            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00425            GO TO 8200-SEND-DATAONLY.
00426
00427      IF PI-OPTION-ONE-SELECTED
00428         GO TO 1100-PROCESS-OPTION-1.
00429
00430      IF PI-OPTION-TWO-SELECTED
00431         GO TO 1200-PROCESS-OPTION-2.
00432
00433      IF PI-OPTION-THREE-SELECTED
00434         GO TO 1300-PROCESS-OPTION-3.
00435
00436      MOVE '4'                    TO PI-OPTION.
00437      GO TO 1400-PROCESS-OPTION-4.
00438
00439      EJECT
00440
00441 ******************************************************************
00442 *                                                                *
00443 *           P R O C E S S   O P T I O N   O N E                  *
00444 *                                                                *
00445 ******************************************************************
00446
00447  1100-PROCESS-OPTION-1.
00448
00449      MOVE LOW-VALUES             TO ERRQST-ALT-KEY2.
00450      MOVE PI-COMPANY-CD          TO ERRQST-COMPANY-CD-A2.
00451
00452      IF FRCARL GREATER THAN ZERO
00453         MOVE FRCARI              TO ERRQST-CARRIER-A2.
00454
00455      IF FRGRPL GREATER THAN ZERO
00456         MOVE FRGRPI              TO ERRQST-GROUPING-A2.
00457
00458      IF FRESPL GREATER THAN ZERO
00459         MOVE FRESPI              TO ERRQST-FIN-RESP-A2.
00460
00461      MOVE ERRQST-ALT-KEY2        TO PI-SELECT-KEY
00462                                     PI-ACCESS-KEY.
00463
00464      MOVE FILE-ID-ERRQST3        TO PI-FILE-ID.
00465
00466      PERFORM 4000-READ-REQUEST-FILE THRU 4090-EXIT.
00467
00468      IF ERRQST-CARRIER-A2 = RQ-CARRIER-A2
00469         AND ERRQST-GROUPING-A2 = RQ-GROUPING-A2
00470            AND ERRQST-FIN-RESP-A2 = RQ-FIN-RESP-A2
00471               NEXT SENTENCE
00472            ELSE
00473               GO TO 1190-SELECTION-ERROR.
00474
00475      IF (WS-SUBMIT-FUNCTION OR WS-RESUBMIT-FUNCTION)
00476         PERFORM 5000-PROCESS-REQUEST-FILE THRU 5090-EXIT.
00477
00478      MOVE EL852B              TO PI-MAP-NAME.
00479      MOVE XCTL-EL8521         TO PGM-NAME.
00480      GO TO 9300-XCTL.
00481
00482  1190-SELECTION-ERROR.
00483      MOVE SPACE                  TO PI-OPTION.
00484      MOVE ER-2132                TO EMI-ERROR.
00485      MOVE -1                     TO FRCARL.
00486      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00487      GO TO 8200-SEND-DATAONLY.
00488
00489      EJECT
00490
00491 ******************************************************************
00492 *                                                                *
00493 *           P R O C E S S   O P T I O N   T W O                  *
00494 *                                                                *
00495 ******************************************************************
00496
00497  1200-PROCESS-OPTION-2.
00498
00499      MOVE LOW-VALUES             TO ERRQST-ALT-KEY1.
00500      MOVE PI-COMPANY-CD          TO ERRQST-COMPANY-CD-A1.
00501
00502      IF ACCTCARL GREATER THAN ZERO
00503         MOVE ACCTCARI            TO ERRQST-CARRIER-A1.
00504
00505      IF ACCTGRPL GREATER THAN ZERO
00506         MOVE ACCTGRPI            TO ERRQST-GROUPING-A1.
00507
00508      IF ACCTSTL GREATER THAN ZERO
00509         MOVE ACCTSTI             TO ERRQST-STATE-A1.
00510
00511      IF ACCOUNTL GREATER THAN ZERO
00512         MOVE ACCOUNTI            TO ERRQST-ACCOUNT-A1.
00513
00514      IF ACCTREFL GREATER THAN ZERO
00515         MOVE ACCTREFI            TO ERRQST-REFERENCE-A1.
00516
00517      MOVE FILE-ID-ERRQST2        TO PI-FILE-ID.
00518      MOVE ERRQST-ALT-KEY1        TO PI-ACCESS-KEY
00519                                     PI-SELECT-KEY.
00520
00521      PERFORM 4000-READ-REQUEST-FILE THRU 4090-EXIT.
00522
00523      IF ERRQST-CARRIER-A1 = RQ-CARRIER-A1
00524         AND ERRQST-GROUPING-A1 = RQ-GROUPING-A1
00525            AND ERRQST-STATE-A1 = RQ-STATE-A1
00526               AND ERRQST-ACCOUNT-A1 = RQ-ACCOUNT-A1
00527                     NEXT SENTENCE
00528                  ELSE
00529                     GO TO 1290-SELECTION-ERROR.
00530
00531      IF ACCTREFL GREATER THAN ZERO
00532         IF ERRQST-REFERENCE-A1 = RQ-REFERENCE-A1
00533            NEXT SENTENCE
00534         ELSE
00535            GO TO 1290-SELECTION-ERROR.
00536
00537      IF (WS-SUBMIT-FUNCTION OR WS-RESUBMIT-FUNCTION)
00538         PERFORM 5000-PROCESS-REQUEST-FILE THRU 5090-EXIT.
00539
00540
00541      MOVE EL852C                 TO PI-MAP-NAME.
00542      MOVE XCTL-EL8521            TO PGM-NAME.
00543      GO TO 9300-XCTL.
00544
00545  1290-SELECTION-ERROR.
00546
00547      MOVE SPACE                  TO PI-OPTION.
00548      MOVE ER-2132                TO EMI-ERROR.
00549      MOVE -1                     TO ACCTCARL.
00550      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00551      GO TO 8200-SEND-DATAONLY.
00552
00553      EJECT
00554
00555 ******************************************************************
00556 *                                                                *
00557 *           P R O C E S S   O P T I O N   T H R E E              *
00558 *                                                                *
00559 ******************************************************************
00560
00561  1300-PROCESS-OPTION-3.
00562
00563      MOVE LOW-VALUES             TO ERRQST-ALT-KEY4.
00564      MOVE PI-COMPANY-CD          TO ERRQST-COMPANY-CD-A4.
00565
00566      IF SUMMARYL GREATER THAN ZERO
00567         MOVE SUMMARYI            TO ERRQST-SUMMARY-CODE.
00568
00569
00570      IF (WS-SUBMIT-FUNCTION OR WS-RESUBMIT-FUNCTION)
00571         PERFORM 6000-PROCESS-SUMMARY-FILE THRU 6090-EXIT.
00572
00573      MOVE FILE-ID-ERRQST5        TO PI-FILE-ID.
00574      MOVE ERRQST-ALT-KEY4        TO PI-SELECT-KEY
00575                                     PI-ACCESS-KEY.
00576
00577      PERFORM 4000-READ-REQUEST-FILE THRU 4090-EXIT.
00578
00579      IF ERRQST-SUMMARY-CODE = RQ-SUMMARY-CODE
00580         NEXT SENTENCE
00581      ELSE
00582         GO TO 1390-SELECTION-ERROR.
00583
00584      MOVE EL852B                 TO PI-MAP-NAME
00585      MOVE XCTL-EL8521            TO PGM-NAME
00586      GO TO 9300-XCTL.
00587
00588  1390-SELECTION-ERROR.
00589
00590      MOVE SPACE                  TO PI-OPTION.
00591      MOVE ER-2132                TO EMI-ERROR.
00592      MOVE -1                     TO SUMMARYL.
00593      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00594      GO TO 8200-SEND-DATAONLY.
00595
00596      EJECT
00597
00598 ******************************************************************
00599 *                                                                *
00600 *           P R O C E S S   O P T I O N   F O U R                *
00601 *                                                                *
00602 ******************************************************************
00603
00604  1400-PROCESS-OPTION-4.
00605
00606      MOVE LOW-VALUES             TO ERRQST-KEY.
00607      MOVE PI-COMPANY-CD          TO ERRQST-COMPANY-CD.
00608
00609      IF BATCHL GREATER THAN ZERO
00610         MOVE BATCHI              TO ERRQST-ENTRY-BATCH.
00611
00612      MOVE FILE-ID-ERRQST         TO PI-FILE-ID.
00613      MOVE ERRQST-KEY             TO PI-ACCESS-KEY
00614                                     PI-SELECT-KEY.
00615
00616      PERFORM 4000-READ-REQUEST-FILE THRU 4090-EXIT.
00617
00618      IF ERRQST-ENTRY-BATCH = LOW-VALUES
00619         MOVE EL852B                 TO PI-MAP-NAME
00620         MOVE XCTL-EL8521            TO PGM-NAME
00621         GO TO 9300-XCTL.
00622
00623      IF ERRQST-ENTRY-BATCH = RQ-ENTRY-BATCH
00624         NEXT SENTENCE
00625      ELSE
00626         GO TO 1490-SELECTION-ERROR.
00627
00628      IF (WS-SUBMIT-FUNCTION OR WS-RESUBMIT-FUNCTION)
00629         PERFORM 5000-PROCESS-REQUEST-FILE THRU 5090-EXIT.
00630
00631      MOVE EL852B                 TO PI-MAP-NAME.
00632      MOVE XCTL-EL8521            TO PGM-NAME
00633      GO TO 9300-XCTL.
00634
00635  1490-SELECTION-ERROR.
00636
00637      MOVE SPACE                  TO PI-OPTION.
00638      MOVE ER-2132                TO EMI-ERROR.
00639      MOVE -1                     TO BATCHL.
00640      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00641      GO TO 8200-SEND-DATAONLY.
00642
00643      EJECT
00644
00645 ******************************************************************
00646 *                                                                *
00647 *           R E A D   R E Q U E S T   F I L E                    *
00648 *                                                                *
00649 ******************************************************************
00650
00651  4000-READ-REQUEST-FILE.
00652
00653      
      * EXEC CICS HANDLE CONDITION
00654 *        NOTFND  (4080-REQUEST-NOTFND)
00655 *        ENDFILE (4080-REQUEST-NOTFND)
00656 *    END-EXEC.
      *    MOVE '"$I''                  ! # #00001813' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303031383133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00657
00658      
      * EXEC CICS READ
00659 *        SET    (ADDRESS OF AR-REQUEST-RECORD)
00660 *        DATASET(PI-FILE-ID)
00661 *        RIDFLD (PI-ACCESS-KEY)
00662 *        GTEQ
00663 *    END-EXEC.
      *    MOVE '&"S        G          (   #00001818' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACCESS-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF AR-REQUEST-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00664
00665      IF RQ-COMPANY-CD NOT = PI-COMPANY-CD
00666         GO TO 4080-REQUEST-NOTFND.
00667
00668      GO TO 4090-EXIT.
00669
00670  4080-REQUEST-NOTFND.
00671
00672       MOVE SPACE                 TO PI-OPTION.
00673       MOVE ER-2132               TO EMI-ERROR.
00674       MOVE -1                    TO FRCARL.
00675       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00676       GO TO 8200-SEND-DATAONLY.
00677
00678  4090-EXIT.
00679       EXIT.
00680      EJECT
00681
00682 ******************************************************************
00683 *                                                                *
00684 *           P R O C E S S   R E Q U E S T   F I L E              *
00685 *                                                                *
00686 *   . BROWSE THE REQUEST FILE FOR THE SELECTED OPTION AND        *
00687 *     SET THE REQUEST DATE TO THE CURRENT DATE.                  *
00688 *                                                                *
00689 *   . IF THE OPTION IS BEING RE-REQUESTED, FLAG THEM AS SUCH.    *
00690 *                                                                *
00691 ******************************************************************
00692
00693  5000-PROCESS-REQUEST-FILE.
00694
00695      IF WS-REQUEST-BROWSE-STARTED
00696         
      * EXEC CICS HANDLE CONDITION
00697 *            NOTFND  (5070-REQUEST-PROCESSED)
00698 *            ENDFILE (5070-REQUEST-PROCESSED)
00699 *       END-EXEC
      *    MOVE '"$I''                  ! $ #00001856' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303031383536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00700      ELSE
00701         
      * EXEC CICS HANDLE CONDITION
00702 *            NOTFND  (5080-REQUEST-NOTFND)
00703 *            ENDFILE (5080-REQUEST-NOTFND)
00704 *       END-EXEC.
      *    MOVE '"$I''                  ! % #00001861' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303031383631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00705
00706      
      * EXEC CICS STARTBR
00707 *        DATASET (PI-FILE-ID)
00708 *        RIDFLD  (PI-ACCESS-KEY)
00709 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00001866' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-FILE-ID, 
                 PI-ACCESS-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00710
00711  5020-READ-REQUEST-FILE.
00712
00713      
      * EXEC CICS READNEXT
00714 *        SET    (ADDRESS OF AR-REQUEST-RECORD)
00715 *        DATASET(PI-FILE-ID)
00716 *        RIDFLD (PI-ACCESS-KEY)
00717 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00001873' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ACCESS-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF AR-REQUEST-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00718
00719      MOVE 'Y'                    TO WS-REQUEST-BROWSE-SW.
00720
00721      IF RQ-COMPANY-CD NOT = PI-COMPANY-CD
00722         GO TO 5070-REQUEST-PROCESSED.
00723
00724      IF WS-SAVE-ACCESS-KEY = PI-ACCESS-KEY
00725         GO TO 5020-READ-REQUEST-FILE.
00726
00727      MOVE PI-ACCESS-KEY          TO WS-SAVE-ACCESS-KEY.
00728
00729      IF RQ-REQUEST-DT GREATER THAN LOW-VALUES OR
00730         RQ-REQUEST-DT = SPACES
00731         IF MAINTI = 'R'
00732            NEXT SENTENCE
00733         ELSE
00734            GO TO 5020-READ-REQUEST-FILE.
00735
00736      IF RQ-MO-END-DT GREATER THAN PI-AR-MONTH-END-DT
00737         GO TO 5020-READ-REQUEST-FILE.
00738
00739      IF PI-OPTION-ONE-SELECTED
00740         MOVE RQ-CONTROL-BY-FIN-RESP TO WS-RQST-FIN-RESP-CNTL
00741         IF WS-RQST-FIN-RESP-CNTL GREATER THAN
00742               ERRQST-FIN-RESP-CONTROL
00743            GO TO 5070-REQUEST-PROCESSED.
00744
00745      IF PI-OPTION-TWO-SELECTED
00746         MOVE RQ-CONTROL-BY-ACCT-REF TO WS-RQST-ACCT-CONTROL
00747         IF WS-RQST-ACCT-CONTROL GREATER THAN ERRQST-ACCT-CONTROL
00748            GO TO 5070-REQUEST-PROCESSED.
00749
00750      IF PI-OPTION-TWO-SELECTED
00751         IF ERRQST-REFERENCE-A1 GREATER THAN LOW-VALUES
00752            IF ERRQST-REFERENCE-A1 = RQ-REFERENCE-A1
00753               NEXT SENTENCE
00754            ELSE
00755               GO TO 5070-REQUEST-PROCESSED.
00756
00757      IF NOT PI-OPTION-THREE-SELECTED
00758         NEXT SENTENCE
00759      ELSE
00760         IF  RQ-CARRIER-A2 = ERRQST-CARRIER-A2
00761             AND RQ-GROUPING-A2 = ERRQST-GROUPING-A2
00762                 AND RQ-FIN-RESP-A2 = ERRQST-FIN-RESP-A2
00763                     AND RQ-ACCT-AGENT-A2 = ERRQST-ACCT-AGENT-A2
00764                         NEXT SENTENCE
00765         ELSE
00766             GO TO 5070-REQUEST-PROCESSED.
00767
00768      IF PI-OPTION-FOUR-SELECTED
00769         IF ERRQST-ENTRY-BATCH = RQ-ENTRY-BATCH
00770            NEXT SENTENCE
00771         ELSE
00772            GO TO 5070-REQUEST-PROCESSED.
00773
00774      MOVE RQ-CONTROL-PRIMARY     TO ERRQST-KEY.
00775
00776      
      * EXEC CICS ENDBR
00777 *        DATASET (PI-FILE-ID)
00778 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00001936' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031393336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00779
00780      
      * EXEC CICS READ
00781 *        SET     (ADDRESS OF AR-REQUEST-RECORD)
00782 *        DATASET (FILE-ID-ERRQST)
00783 *        RIDFLD  (ERRQST-KEY)
00784 *        UPDATE
00785 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00001940' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303031393430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERRQST, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERRQST-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF AR-REQUEST-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00786
00787      MOVE 'B'                    TO JP-RECORD-TYPE.
00788      MOVE AR-REQUEST-RECORD      TO JP-RECORD-AREA.
00789
00790      PERFORM 8400-LOG-JOURNAL-RECORD.
00791
00792      MOVE 'Y'                    TO WS-DATA-SELECTED-SW.
00793
00794      IF PI-OPTION-THREE-SELECTED
00795         MOVE SX-SUMMARY          TO RQ-SUMMARY-CODE.
00796
00797      IF RQ-STATUS = 'E'
00798          GO TO 5050-WRITE.
00799
00800      IF (RQ-CARRIER-A1  = SPACES OR LOW-VALUES) OR
00801         (RQ-GROUPING-A1 = SPACES OR LOW-VALUES) OR
00802         (RQ-STATE-A1    = SPACES OR LOW-VALUES)
00803          GO TO 5050-WRITE.
00804
00805      IF WS-RESUBMIT-FUNCTION
00806          IF RQ-STMT-DT = LOW-VALUES
00807              MOVE ' '            TO RQ-STATUS
00808          ELSE
00809              MOVE 'R'            TO RQ-STATUS.
00810
00811      MOVE PI-PROCESSOR-ID        TO RQ-PROCESSOR-ID.
00812      MOVE WS-CURRENT-BIN-DT      TO RQ-REQUEST-DT.
00813
00814      IF PI-OPTION-ONE-SELECTED
00815         MOVE 'F'                 TO RQ-REQUEST-METHOD.
00816
00817      IF PI-OPTION-TWO-SELECTED
00818         MOVE 'A'                 TO RQ-REQUEST-METHOD.
00819
00820      IF PI-OPTION-THREE-SELECTED
00821         MOVE 'S'                 TO RQ-REQUEST-METHOD.
00822
00823      IF PI-OPTION-FOUR-SELECTED
00824         MOVE 'B'                 TO RQ-REQUEST-METHOD.
00825
00826  5050-WRITE.
00827
00828      MOVE 'C'                    TO JP-RECORD-TYPE.
00829      MOVE AR-REQUEST-RECORD      TO JP-RECORD-AREA.
00830
00831      
      * EXEC CICS REWRITE
00832 *        DATASET (FILE-ID-ERRQST)
00833 *        FROM    (AR-REQUEST-RECORD)
00834 *    END-EXEC.
           MOVE LENGTH OF
            AR-REQUEST-RECORD
             TO DFHEIV11
      *    MOVE '&& L                  %   #00001991' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303031393931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERRQST, 
                 AR-REQUEST-RECORD, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00835
00836      PERFORM 8400-LOG-JOURNAL-RECORD.
00837
00838      GO TO 5000-PROCESS-REQUEST-FILE.
00839
00840  5070-REQUEST-PROCESSED.
00841
00842      
      * EXEC CICS ENDBR
00843 *        DATASET (PI-FILE-ID)
00844 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00002002' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032303032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00845
00846      IF PI-OPTION-FOUR-SELECTED
00847         GO TO 5090-EXIT.
00848
00849      IF PI-OPTION-THREE-SELECTED
00850         GO TO 5090-EXIT.
00851
00852      IF WS-DATA-SELECTED
00853         GO TO 5090-EXIT.
00854
00855      MOVE SPACE                 TO PI-OPTION.
00856      MOVE ER-2134               TO EMI-ERROR.
00857      MOVE -1                    TO FRCARL.
00858      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00859      GO TO 8200-SEND-DATAONLY.
00860
00861  5080-REQUEST-NOTFND.
00862
00863      IF PI-OPTION-FOUR-SELECTED
00864         GO TO 5090-EXIT.
00865
00866       MOVE SPACE                 TO PI-OPTION.
00867       MOVE ER-2134               TO EMI-ERROR.
00868       MOVE -1                    TO FRCARL.
00869       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00870       GO TO 8200-SEND-DATAONLY.
00871
00872  5090-EXIT.
00873       EXIT.
00874
00875      EJECT
00876
00877
00878 ******************************************************************
00879 *                                                                *
00880 *           P R O C E S S   S U M M A R Y   F I L E              *
00881 *                                                                *
00882 *   . BROWSE THE SUMMARY FILE FOR THE SELECTED SUMMARY CODE.     *
00883 *                                                                *
00884 *   . BROWSE THE REQUEST FILE FOR THE SELECTED SUMMARY CODE.     *
00885 *                                                                *
00886 *   . UPDATE THE THOSE REQUEST RECORDS THAT ARE ASSOCIATED       *
00887 *     WITH THE SELECTED SUMMARY RECORDS.                         *
00888 *                                                                *
00889 ******************************************************************
00890
00891  6000-PROCESS-SUMMARY-FILE.
00892
00893       
      * EXEC CICS HANDLE CONDITION
00894 *          NOTFND  (6080-REQUEST-NOTFND)
00895 *          ENDFILE (6080-REQUEST-NOTFND)
00896 *     END-EXEC.
      *    MOVE '"$I''                  ! & #00002053' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303032303533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00897
00898      MOVE ERRQST-ALT-KEY4        TO WS-ACCESS-KEY.
00899
00900      
      * EXEC CICS STARTBR
00901 *        DATASET (FILE-ID-ERSUMM)
00902 *        RIDFLD  (WS-ACCESS-KEY)
00903 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00002060' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032303630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERSUMM, 
                 WS-ACCESS-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00904
00905      
      * EXEC CICS HANDLE CONDITION
00906 *         NOTFND  (6070-REQUEST-PROCESSED)
00907 *         ENDFILE (6070-REQUEST-PROCESSED)
00908 *    END-EXEC.
      *    MOVE '"$I''                  ! '' #00002065' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303032303635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00909
00910  6020-READ-SUMMARY-FILE.
00911
00912      
      * EXEC CICS READNEXT
00913 *        SET    (ADDRESS OF SUMM-CROSS-REFERENCE)
00914 *        DATASET(FILE-ID-ERSUMM)
00915 *        RIDFLD (WS-ACCESS-KEY)
00916 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00002072' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303032303732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERSUMM, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ACCESS-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF SUMM-CROSS-REFERENCE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00917
00918      MOVE 'Y'                    TO WS-SUMMARY-BROWSE-SW.
00919
00920      IF SX-COMPANY-CD NOT = PI-COMPANY-CD
00921         GO TO 6070-REQUEST-PROCESSED.
00922
00923      IF SX-SUMMARY GREATER THAN ERRQST-SUMMARY-CODE
00924         GO TO 6070-REQUEST-PROCESSED.
00925
00926 ******************************************************************
00927 *           BYPASS SUMMARY NAME RECORD                           *
00928 ******************************************************************
00929
00930      IF SX-CARRIER = LOW-VALUES
00931         GO 6020-READ-SUMMARY-FILE.
00932
00933      MOVE LOW-VALUES            TO ERRQST-ALT-KEY2.
00934      MOVE SX-COMPANY-CD         TO ERRQST-COMPANY-CD-A2.
00935      MOVE SX-CARRIER            TO ERRQST-CARRIER-A2.
00936      MOVE SX-GROUP              TO ERRQST-GROUPING-A2.
00937      MOVE SX-FIN-RESP           TO ERRQST-FIN-RESP-A2.
00938      MOVE SX-ACCT-AGENT         TO ERRQST-ACCT-AGENT-A2.
00939      MOVE ERRQST-ALT-KEY2       TO PI-ACCESS-KEY.
00940      MOVE FILE-ID-ERRQST3       TO PI-FILE-ID.
00941
00942      MOVE LOW-VALUES TO WS-SAVE-ACCESS-KEY.
00943
00944      PERFORM 5000-PROCESS-REQUEST-FILE THRU 5090-EXIT.
00945
00946      GO TO 6020-READ-SUMMARY-FILE.
00947
00948  6070-REQUEST-PROCESSED.
00949
00950      
      * EXEC CICS ENDBR
00951 *        DATASET (FILE-ID-ERSUMM)
00952 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00002110' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERSUMM, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00953
00954      IF WS-DATA-SELECTED
00955         GO TO 6090-EXIT.
00956
00957      MOVE SPACE                 TO PI-OPTION.
00958      MOVE ER-2134               TO EMI-ERROR.
00959      MOVE -1                    TO FRCARL.
00960      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00961      GO TO 8200-SEND-DATAONLY.
00962
00963  6080-REQUEST-NOTFND.
00964
00965      MOVE SPACE                 TO PI-OPTION.
00966      MOVE ER-3133               TO EMI-ERROR.
00967      MOVE -1                    TO FRCARL.
00968      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00969      GO TO 8200-SEND-DATAONLY.
00970
00971  6090-EXIT.
00972       EXIT.
00973
00974      EJECT
00975 ******************************************************************
00976 *                                                                *
00977 *            S  E N D    I N I T I A L   M A P                   *
00978 *                                                                *
00979 ******************************************************************
00980
00981  8100-SEND-INITIAL-MAP.
00982
00983      MOVE WS-CURRENT-DT          TO DATEO.
00984      MOVE EIBTIME                TO TIME-IN.
00985      MOVE TIME-OUT               TO TIMEO.
00986      MOVE -1                     TO MAINTL.
00987
00988      MOVE EMI-MESSAGE-AREA (1)   TO ERMESGO.
00989
00990      
      * EXEC CICS SEND
00991 *        MAP      (EL852A)
00992 *        MAPSET   (MAPSET-EL852S)
00993 *        FROM     (EL852AI)
00994 *        ERASE
00995 *        CURSOR
00996 *    END-EXEC.
           MOVE LENGTH OF
            EL852AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00002150' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL852A, 
                 EL852AI, 
                 DFHEIV12, 
                 MAPSET-EL852S, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00997
00998      GO TO 9100-RETURN-TRAN.
00999
01000      EJECT
01001
01002 ******************************************************************
01003 *                                                                *
01004 *              S E N D    D A T A O N L Y                        *
01005 *                                                                *
01006 ******************************************************************
01007
01008  8200-SEND-DATAONLY.
01009
01010      MOVE WS-CURRENT-DT          TO DATEO.
01011      MOVE EIBTIME                TO TIME-IN.
01012      MOVE TIME-OUT               TO TIMEO.
01013
01014      MOVE EMI-MESSAGE-AREA (1)   TO ERMESGO.
01015
01016      
      * EXEC CICS SEND
01017 *         MAP      (EL852A)
01018 *         MAPSET   (MAPSET-EL852S)
01019 *         FROM     (EL852AI)
01020 *         DATAONLY
01021 *         CURSOR
01022 *    END-EXEC.
           MOVE LENGTH OF
            EL852AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00002176' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL852A, 
                 EL852AI, 
                 DFHEIV12, 
                 MAPSET-EL852S, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01023
01024      GO TO 9100-RETURN-TRAN.
01025
01026      EJECT
01027
01028  8300-SEND-TEXT.
01029      
      * EXEC CICS SEND TEXT
01030 *        FROM     (LOGOFF-TEXT)
01031 *        LENGTH   (LOGOFF-LENGTH)
01032 *        ERASE
01033 *        FREEKB
01034 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00002189' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313839' TO DFHEIV0(25:11)
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
           
01035
01036      
      * EXEC CICS RETURN
01037 *    END-EXEC.
      *    MOVE '.(                    &   #00002196' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01038
01039
01040  8400-LOG-JOURNAL-RECORD.
01041      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.
01042      MOVE THIS-PGM                TO JP-PROGRAM-ID.
01043
01044 *    EXEC CICS JOURNAL
01045 *        JFILEID     (PI-JOURNAL-FILE-ID)
01046 *        JTYPEID     ('EL')
01047 *        FROM        (JOURNAL-RECORD)
01048 *        LENGTH      (WS-JOURNAL-RECORD-LENGTH)
01049 *        END-EXEC.
01050
01051  8500-DATE-CONVERT.
01052      
      * EXEC CICS LINK
01053 *        PROGRAM  (LINK-ELDATCV)
01054 *        COMMAREA (DATE-CONVERSION-DATA)
01055 *        LENGTH   (DC-COMM-LENGTH)
01056 *    END-EXEC.
      *    MOVE '."C                   ''   #00002212' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032323132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LINK-ELDATCV, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01057
01058  8500-EXIT.
01059      EXIT.
01060
01061      EJECT
01062
01063  8800-UNAUTHORIZED-ACCESS.
01064      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
01065      GO TO 8300-SEND-TEXT.
01066
01067  8810-PF23.
01068      MOVE EIBAID                 TO PI-ENTRY-CD-1.
01069      MOVE XCTL-EL005             TO PGM-NAME.
01070      GO TO 9300-XCTL.
01071
01072  9200-RETURN-MAIN-MENU.
01073      MOVE XCTL-EL626             TO PGM-NAME.
01074      GO TO 9300-XCTL.
01075
01076  9000-RETURN-CICS.
01077      
      * EXEC CICS RETURN
01078 *    END-EXEC.
      *    MOVE '.(                    &   #00002237' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032323337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01079
01080  9100-RETURN-TRAN.
01081
01082      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
01083      MOVE '852A'                 TO PI-CURRENT-SCREEN-NO.
01084
01085      
      * EXEC CICS RETURN
01086 *        TRANSID    (TRANS-EXJ2)
01087 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
01088 *        LENGTH     (PI-COMM-LENGTH)
01089 *    END-EXEC.
      *    MOVE '.(CT                  &   #00002245' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032323435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-EXJ2, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01090
01091  9300-XCTL.
01092      
      * EXEC CICS XCTL
01093 *        PROGRAM    (PGM-NAME)
01094 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
01095 *        LENGTH     (PI-COMM-LENGTH)
01096 *    END-EXEC.
      *    MOVE '.$C                   $   #00002252' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032323532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01097
01098  9400-CLEAR.
01099      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME
01100      GO TO 9300-XCTL.
01101
01102  9500-PF12.
01103      MOVE XCTL-EL010             TO PGM-NAME.
01104      GO TO 9300-XCTL.
01105
01106  9600-PGMID-ERROR.
01107
01108      
      * EXEC CICS HANDLE CONDITION
01109 *        PGMIDERR    (8300-SEND-TEXT)
01110 *    END-EXEC.
      *    MOVE '"$L                   ! ( #00002268' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303032323638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01111
01112      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
01113      MOVE ' '                    TO PI-ENTRY-CD-1.
01114      MOVE XCTL-EL005             TO PGM-NAME.
01115      MOVE PGM-NAME               TO LOGOFF-PGM.
01116      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
01117      GO TO 9300-XCTL.
01118
01119  9900-ERROR-FORMAT.
01120
01121      IF NOT EMI-ERRORS-COMPLETE
01122          MOVE LINK-EL001         TO PGM-NAME
01123          
      * EXEC CICS LINK
01124 *            PROGRAM    (PGM-NAME)
01125 *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
01126 *            LENGTH     (EMI-COMM-LENGTH)
01127 *        END-EXEC.
      *    MOVE '."C                   ''   #00002283' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032323833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01128
01129  9900-EXIT.
01130      EXIT.
01131
01132  9990-ABEND.
01133      MOVE LINK-EL004             TO PGM-NAME.
01134      MOVE DFHEIBLK               TO EMI-LINE1.
01135      
      * EXEC CICS LINK
01136 *        PROGRAM   (PGM-NAME)
01137 *        COMMAREA  (EMI-LINE1)
01138 *        LENGTH    (72)
01139 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00002295' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032323935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01140
01141      MOVE -1                     TO PFENTERL.
01142
01143      GO TO 8200-SEND-DATAONLY.
01144
01145      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL852' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
01146
01147      EJECT
01148
01149  9995-SECURITY-VIOLATION.
01150 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00002327' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333237' TO DFHEIV0(25:11)
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
01151
01152  9995-EXIT.
01153      EXIT.
01154

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL852' TO DFHEIV1
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
               GO TO 4080-REQUEST-NOTFND,
                     4080-REQUEST-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 5070-REQUEST-PROCESSED,
                     5070-REQUEST-PROCESSED
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 5080-REQUEST-NOTFND,
                     5080-REQUEST-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 6080-REQUEST-NOTFND,
                     6080-REQUEST-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 6070-REQUEST-PROCESSED,
                     6070-REQUEST-PROCESSED
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL852' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
