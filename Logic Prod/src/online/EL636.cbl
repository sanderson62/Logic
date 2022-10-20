00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL636 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 12/13/94 13:34:15.
00007 *                            VMOD=2.026
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
00024 *REMARKS. TRANSACTION - EXJA - COMMISSION CHECK WORK FILE
00025 *                              MAINTENANCE.
00026
00027  ENVIRONMENT DIVISION.
00028
00029      EJECT
00030  DATA DIVISION.
00031  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00032
00033  77  FILLER  PIC X(32)  VALUE '********************************'.
00034  77  FILLER  PIC X(32)  VALUE '*    EL636 WORKING STORAGE     *'.
00035  77  FILLER  PIC X(32)  VALUE '************ V/M 2.026 *********'.
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
00051      12  SC-ITEM                     PIC S9(4)   VALUE +1   COMP.
00052      12  GETMAIN-SPACE               PIC X       VALUE SPACE.
00053      12  EL636A                      PIC X(8)    VALUE 'EL636A'.
00054      12  MAPSET-EL636S               PIC X(8)    VALUE 'EL636S'.
00055      12  TRANS-EXJA                  PIC X(4)    VALUE 'EXJA'.
00056      12  THIS-PGM                    PIC X(8)    VALUE 'EL636 '.
00057      12  PGM-NAME                    PIC X(8).
00058      12  RETURNED-FROM               PIC X(8)    VALUE SPACES.
00059      12  TIME-IN                     PIC S9(7).
00060      12  TIME-OUT-R  REDEFINES TIME-IN.
00061          16  FILLER                  PIC X.
00062          16  TIME-OUT                PIC 99V99.
00063          16  FILLER                  PIC X(2).
00064      12  LINK-EL001                  PIC X(8)    VALUE 'EL001'.
00065      12  LINK-EL004                  PIC X(8)    VALUE 'EL004'.
00066      12  XCTL-EL005                  PIC X(8)    VALUE 'EL005'.
00067      12  XCTL-EL6361                 PIC X(8)    VALUE 'EL6361'.
00068      12  XCTL-EL010                  PIC X(8)    VALUE 'EL010'.
00069      12  XCTL-EL626                  PIC X(8)    VALUE 'EL626'.
00070      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'.
00071      12  FILE-ID-ERCMCK              PIC X(8)    VALUE 'ERCMCK'.
00072      12  FILE-ID-ERCKWK              PIC X(8)    VALUE 'ERCKWK '.
00073      12  FILE-ID-ERCOMP              PIC X(8)    VALUE 'ERCOMP '.
00074      12  CNTL-FILE-ID                PIC X(8)    VALUE 'ELCNTL '.
00075      12  WS-CURRENT-DT               PIC X(8)    VALUE SPACES.
00076      12  WS-CURRENT-BIN-DT           PIC XX      VALUE SPACES.
00077      12  QID.
00078          16  QID-TERM                PIC X(4)    VALUE SPACES.
00079          16  FILLER                  PIC X(4)    VALUE '125D'.
00080      12  WS-TIME                     PIC 9(6)    VALUE ZEROS.
00081      12  WS-HR-MINS-SECS REDEFINES WS-TIME.
00082          16  WS-HR-MINS              PIC 99V99.
00083          16  FILLER                  PIC XX.
00084      12  CLIENT-HER                  PIC X(3)    VALUE 'HER'.
00085      12  CLIENT-LGX                  PIC X(3)    VALUE 'LGX'.
00086      12  ERCOMP-SW                   PIC X       VALUE ' '.
00087
00088      EJECT
00089
00090  01  WORK-AREA.
00091      12  DEEDIT-FIELD                PIC X(12).
00092      12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD   PIC S9(10)V99.
00093      12  WS-SEQUENCE-NO              PIC S9(4)    VALUE +0 COMP.
00094      12  WS-DETAMT OCCURS 5 TIMES    PIC S9(7)V99.
00095      12  WS-DETAIL-AMOUNT            PIC S9(7)V99 VALUE +0 COMP-3.
00096      12  WS-SPACE-AGENT-SW           PIC X        VALUE SPACE.
00097      12  WS-UPDATE-SW                PIC X        VALUE SPACE.
00098          88  WS-ENTRIES-UPDATED                   VALUE 'Y'.
00099      12  WS-SUB1                     PIC S9(4)    VALUE +0 COMP.
00100      12  WS-SUB2                     PIC S9(4)    VALUE +0 COMP.
00101      12  WS-BROWSE-SW                PIC X        VALUE SPACE.
00102          88  WS-BROWSE-STARTED                    VALUE 'Y'.
00103      12  WS-FIRST-DETAIL-SW          PIC X        VALUE SPACE.
00104          88  FIRST-DETAIL-READ                    VALUE 'Y'.
00105      12  WS-HEADER-INFO.
00106          16  WS-PAYEE-NAME           PIC X(30)    VALUE SPACES.
00107          16  WS-PAYEE-ADDRESS-1      PIC X(30)    VALUE SPACES.
00108          16  WS-PAYEE-ADDRESS-2      PIC X(30)    VALUE SPACES.
00109          16  WS-PAYEE-CITY-ST        PIC X(30)    VALUE SPACES.
00110          16  WS-PAYEE-ZIP-CODE.
00111              20  WS-PAYEE-ZIP        PIC X(5)     VALUE SPACES.
00112              20  WS-PAYEE-ZIP-EXT    PIC X(4)     VALUE SPACES.
00113          16  WS-AMOUNT-PAID          PIC S9(7)V99 VALUE +0 COMP-3.
00114          16  WS-TOTAL-ENTRIES        PIC S9(3)    VALUE +0 COMP-3.
00115      12  WS-SEQ-NINES                PIC S9(4)    VALUE +9999
00116                                                   COMP.
00117      12  WS-PAY-SEQ-NUM              PIC S9(4)    VALUE +0 COMP.
00118      12  WS-NOFIRST                  PIC S9(4)    VALUE +0 COMP.
00119      12  WS-DELETE-SEQ-NO            PIC S9(4)    VALUE +0 COMP.
00120      12  WS-DELETE-AMOUNT            PIC S9(7)V99 VALUE +0 COMP-3.
00121      12  WS-DELETE-COUNT             PIC S9(3)    VALUE +0 COMP-3.
00122      12  WS-HOLD-AMOUNT              PIC S9(7)V99 VALUE +0 COMP-3.
00123      12  WS-HOLD-COUNT               PIC S9(3)    VALUE +0 COMP-3.
00124      12  WS-OFF-HOLD-AMOUNT          PIC S9(7)V99 VALUE +0 COMP-3.
00125      12  WS-OFF-HOLD-COUNT           PIC S9(3)    VALUE +0 COMP-3.
00126      12  WS-SAVE-ACTION-CODE         PIC X        VALUE SPACES.
00127          88 SHOW-FUNCTION            VALUE 'S'.
00128      12  WS-HOLD-MAINT OCCURS 5 TIMES    PIC X.
00129      12  FILLER        PIC X(16) VALUE 'ZZZZZZZZZZZZZZZZ'.
00130      12  HOLD-CSR                 PIC X(04)  VALUE LOW-VALUES.
00131
00132      EJECT
00133
00134 ******************************************************************
00135 *                                                                *
00136 *                E R R O R   M E S S A G E S                     *
00137 *                                                                *
00138 ******************************************************************
00139
00140  01  ERROR-MESSAGES.
00141      12  ER-0000                 PIC X(4)  VALUE '0000'.
00142      12  ER-0004                 PIC X(4)  VALUE '0004'.
00143      12  ER-0008                 PIC X(4)  VALUE '0008'.
00144      12  ER-0023                 PIC X(4)  VALUE '0023'.
00145      12  ER-0029                 PIC X(4)  VALUE '0029'.
00146      12  ER-0070                 PIC X(4)  VALUE '0070'.
00147      12  ER-0194                 PIC X(4)  VALUE '0194'.
00148      12  ER-0195                 PIC X(4)  VALUE '0195'.
00149      12  ER-1883                 PIC X(4)  VALUE '1883'.
00150      12  ER-1999                 PIC X(4)  VALUE '1999'.
00151      12  ER-2056                 PIC X(4)  VALUE '2056'.
00152      12  ER-2132                 PIC X(4)  VALUE '2132'.
00153      12  ER-2223                 PIC X(4)  VALUE '2223'.
00154      12  ER-2251                 PIC X(4)  VALUE '2251'.
00155      12  ER-2252                 PIC X(4)  VALUE '2252'.
00156      12  ER-2800                 PIC X(4)  VALUE '2800'.
00157      12  ER-2869                 PIC X(4)  VALUE '2869'.
00158      12  ER-3139                 PIC X(4)  VALUE '3139'.
00159      12  ER-3146                 PIC X(4)  VALUE '3146'.
00160      12  ER-3147                 PIC X(4)  VALUE '3147'.
00161      12  ER-3148                 PIC X(4)  VALUE '3148'.
00162      12  ER-3149                 PIC X(4)  VALUE '3149'.
00163      12  ER-3158                 PIC X(4)  VALUE '3158'.
00164      12  ER-3159                 PIC X(4)  VALUE '3159'.
00165      12  ER-3160                 PIC X(4)  VALUE '3160'.
00166      12  ER-3161                 PIC X(4)  VALUE '3161'.
00167      12  ER-3162                 PIC X(4)  VALUE '3162'.
00168      12  ER-3163                 PIC X(4)  VALUE '3163'.
00169      12  ER-3164                 PIC X(4)  VALUE '3164'.
00170      12  ER-3165                 PIC X(4)  VALUE '3165'.
00171      12  ER-3167                 PIC X(4)  VALUE '3167'.
00172      12  ER-3169                 PIC X(4)  VALUE '3169'.
00173      12  ER-3173                 PIC X(4)  VALUE '3173'.
00174      12  ER-3179                 PIC X(4)  VALUE '3179'.
00175      12  ER-3189                 PIC X(4)  VALUE '3189'.
00176      12  ER-3190                 PIC X(4)  VALUE '3190'.
00177      12  ER-3198                 PIC X(4)  VALUE '3198'.
00178      12  ER-3256                 PIC X(4)  VALUE '3256'.
00179      12  ER-7043                 PIC X(4)  VALUE '7043'.
00180      12  ER-7082                 PIC X(4)  VALUE '7082'.
00181      12  ER-7804                 PIC X(4)  VALUE '7804'.
00182
00183      EJECT
00184
00185 ******************************************************************
00186 *                                                                *
00187 *              A C C E S S   K E Y S                             *
00188 *                                                                *
00189 ******************************************************************
00190
00191  01  ACCESS-KEYS.
00192
00193      12  ELCNTL-KEY.
00194           16  CNTL-COMP-ID           PIC X(3)       VALUE SPACES.
00195           16  CNTL-REC-TYPE          PIC X          VALUE SPACES.
00196           16  CNTL-ACCESS            PIC X(4)       VALUE SPACES.
00197           16  CNTL-SEQ-NO            PIC S9(4)     VALUE +0  COMP.
00198      12  ERCMCK-KEY.
00199          16  ERCMCK-COMPANY-CD       PIC X          VALUE SPACES.
00200          16  ERCMCK-CSR              PIC X(4)   VALUE LOW-VALUES.
00201          16  ERCMCK-CARRIER          PIC X          VALUE SPACES.
00202          16  ERCMCK-GROUPING         PIC X(6)       VALUE SPACES.
00203          16  ERCMCK-PAYEE            PIC X(10)      VALUE SPACES.
00204          16  ERCMCK-PAYEE-SEQ        PIC S9(4) COMP VALUE +0.
00205          16  ERCMCK-SEQUENCE-NO      PIC S9(4) COMP VALUE +0.
00206
00207      12  SAVE-ERCMCK-CONTROL.
00208          16  SVCMCK-COMPANY-CD       PIC X          VALUE SPACES.
00209          16  SVCMCK-CSR              PIC X(4)    VALUE LOW-VALUES.
00210          16  SVCMCK-CARRIER          PIC X          VALUE SPACES.
00211          16  SVCMCK-GROUPING         PIC X(6)       VALUE SPACES.
00212          16  SVCMCK-PAYEE            PIC X(10)      VALUE SPACES.
00213          16  SVCMCK-PAYEE-SEQ        PIC S9(4) COMP VALUE +0.
00214          16  SVCMCK-SEQUENCE-NO      PIC S9(4) COMP VALUE +0.
00215
00216      12  ERCKWK-RECORD-LENGTH        PIC S9(4) COMP VALUE +200.
00217
00218      12  ERCKWK-KEY.
00219          16  ERCKWK-COMPANY-CD       PIC X           VALUE SPACES.
00220          16  ERCKWK-CSR              PIC X(4)   VALUE LOW-VALUES.
00221          16  ERCKWK-CARRIER          PIC X           VALUE SPACES.
00222          16  ERCKWK-GROUPING         PIC X(6)        VALUE SPACES.
00223          16  ERCKWK-PAYEE            PIC X(10)       VALUE SPACES.
00224          16  ERCKWK-PAYEE-SEQ        PIC S9(4)  COMP VALUE +0.
00225          16  ERCKWK-SEQUENCE-NO      PIC S9(4)  COMP VALUE +0.
00226
00227      12  ERCKWK-COMPARE-KEY          PIC X(26)       VALUE SPACE.
00228
00229      12  SAVE-ERCKWK-CONTROL.
00230          16  SVCKWK-COMPANY-CD       PIC X           VALUE SPACES.
00231          16  SVCKWK-CSR              PIC X(4)  VALUE LOW-VALUES.
00232          16  SVCKWK-CARRIER          PIC X           VALUE SPACES.
00233          16  SVCKWK-GROUPING         PIC X(6)        VALUE SPACES.
00234          16  SVCKWK-PAYEE            PIC X(10)       VALUE SPACES.
00235          16  SVCKWK-PAYEE-SEQ        PIC S9(4) COMP  VALUE +0.
00236          16  SVCKWK-SEQUENCE-NO      PIC S9(4) COMP  VALUE +0.
00237
00238      12  SVCKWK-COMPARE-KEY          PIC X(26)       VALUE SPACE.
00239
00240      12  SAVE-ERCKWK-RECORD.
00241          16  FILLER                  PIC X(26).
00242          16  SAVE-CKWK-SEQ-NO        PIC S9(4) COMP  VALUE +0.
00243          16  FILLER                  PIC X(172).
00244
00245
00246
00247      12  ERCMCK-RECORD-LENGTH        PIC S9(4) COMP VALUE +2000.
00248
00249      12  ERCOMP-KEY.
00250          16  ERCOMP-COMP-CD      PIC X     VALUE SPACE.
00251          16  ERCOMP-CARRIER      PIC X     VALUE SPACES.
00252          16  ERCOMP-GROUPING     PIC X(6)  VALUE SPACES.
00253          16  ERCOMP-FIN-RESP     PIC X(10) VALUE SPACES.
00254          16  ERCOMP-ACCT         PIC X(10) VALUE SPACES.
00255          16  ERCOMP-RECORD-TYPE  PIC X     VALUE SPACES.
00256
00257      EJECT
00258
00259 *                            COPY ELCDATE.
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
00260
00261      EJECT
00262 *                            COPY ELCLOGOF.
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
00263
00264      EJECT
00265 *                            COPY ELCATTR.
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
00266
00267      EJECT
00268 *                            COPY ELCEMIB.
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
00269
00270      EJECT
00271 *                            COPY ELCINTF.
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
00272      12  FILLER  REDEFINES PI-PROGRAM-WORK-AREA.
00273          16  PI-MAINT-FUNCTION       PIC X.
00274              88  PI-ADD-FUNCTION         VALUE 'A'.
00275              88  PI-CHG-FUNCTION         VALUE 'C'.
00276              88  PI-DEL-FUNCTION         VALUE 'D'.
00277              88  PI-SHOW-FUNCTION        VALUE 'S'.
00278          16  PI-ERCKWK-KEY.
00279              20  PI-ERCKWK-COMP-CD   PIC X.
00280              20  PI-ERCKWK-CSR       PIC X(4).
00281              20  PI-ERCKWK-CARRIER   PIC X.
00282              20  PI-ERCKWK-GROUPING  PIC X(6).
00283              20  PI-ERCKWK-PAYEE     PIC X(10).
00284              20  PI-ERCKWK-PAYEE-SEQ PIC S9(4)   COMP.
00285              20  PI-ERCKWK-SEQ-NO    PIC S9(4)   COMP.
00286          16  PI-SAVE-ERCKWK-KEY.
00287              20  PI-SVCKWK-COMP-CD   PIC X.
00288              20  PI-SVCKWK-CSR       PIC X(4).
00289              20  PI-SVCKWK-CARRIER   PIC X.
00290              20  PI-SVCKWK-GROUPING  PIC X(6).
00291              20  PI-SVCKWK-PAYEE     PIC X(10).
00292              20  PI-SVCKWK-PAYEE-SEQ PIC S9(4)   COMP.
00293              20  PI-SVCKWK-SEQ-NO    PIC S9(4)   COMP.
00294          16  PI-STUB-KEY OCCURS 5    PIC X(26).
00295          16  PI-STUB-ACCT OCCURS 5   PIC X(10).
00296          16  PI-NO-ENTRIES-DISPLAYED PIC S9.
00297          16  PI-PREV-FUNCTION        PIC X.
00298          16  PI-LAST-STUB-KEY.
00299              20  PI-LAST.
00300                  22  FILLER          PIC X.
00301                  22  PI-LAST-STUB-CSR   PIC  X(4).
00302                  22  FILLER          PIC X(19).
00303              20  PI-LAST-STUB-SEQ-NO PIC S9(4)   COMP.
00304          16  FILLER                  PIC X(379).
00305
00306      EJECT
00307 *                            COPY ELCJPFX.
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
00308                              PIC X(1583).
00309
00310      EJECT
00311 *                            COPY ELCAID.
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
00312  01  FILLER    REDEFINES DFHAID.
00313      12  FILLER              PIC X(8).
00314      12  PF-VALUES           PIC X       OCCURS 24.
00315
00316      EJECT
00317 *                            COPY EL636S.
       01  EL636AI.
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
           05  SHOWL PIC S9(0004) COMP.
           05  SHOWF PIC  X(0001).
           05  FILLER REDEFINES SHOWF.
               10  SHOWA PIC  X(0001).
           05  SHOWI PIC  X(0021).
      *    -------------------------------
           05  DELL PIC S9(0004) COMP.
           05  DELF PIC  X(0001).
           05  FILLER REDEFINES DELF.
               10  DELA PIC  X(0001).
           05  DELI PIC  X(0028).
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
           05  PAYSEQI PIC  X(0004).
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
           05  MAINTBYI PIC  X(0008).
      *    -------------------------------
           05  ADDRS1L PIC S9(0004) COMP.
           05  ADDRS1F PIC  X(0001).
           05  FILLER REDEFINES ADDRS1F.
               10  ADDRS1A PIC  X(0001).
           05  ADDRS1I PIC  X(0030).
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
           05  MAINTATL PIC S9(0004) COMP.
           05  MAINTATF PIC  X(0001).
           05  FILLER REDEFINES MAINTATF.
               10  MAINTATA PIC  X(0001).
           05  MAINTATI PIC  X(0005).
      *    -------------------------------
           05  CITYSTL PIC S9(0004) COMP.
           05  CITYSTF PIC  X(0001).
           05  FILLER REDEFINES CITYSTF.
               10  CITYSTA PIC  X(0001).
           05  CITYSTI PIC  X(0030).
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
           05  RELDTL PIC S9(0004) COMP.
           05  RELDTF PIC  X(0001).
           05  FILLER REDEFINES RELDTF.
               10  RELDTA PIC  X(0001).
           05  RELDTI PIC  X(0008).
      *    -------------------------------
           05  CHKAMTL PIC S9(0004) COMP.
           05  CHKAMTF PIC  X(0001).
           05  FILLER REDEFINES CHKAMTF.
               10  CHKAMTA PIC  X(0001).
           05  CHKAMTI PIC  X(0013).
      *    -------------------------------
           05  NOFIRSTL PIC S9(0004) COMP.
           05  NOFIRSTF PIC  X(0001).
           05  FILLER REDEFINES NOFIRSTF.
               10  NOFIRSTA PIC  X(0001).
           05  NOFIRSTI PIC  X(0004).
      *    -------------------------------
           05  NOLASTL PIC S9(0004) COMP.
           05  NOLASTF PIC  X(0001).
           05  FILLER REDEFINES NOLASTF.
               10  NOLASTA PIC  X(0001).
           05  NOLASTI PIC  X(0004).
      *    -------------------------------
           05  NOENTRSL PIC S9(0004) COMP.
           05  NOENTRSF PIC  X(0001).
           05  FILLER REDEFINES NOENTRSF.
               10  NOENTRSA PIC  X(0001).
           05  NOENTRSI PIC  X(0004).
      *    -------------------------------
           05  M1L PIC S9(0004) COMP.
           05  M1F PIC  X(0001).
           05  FILLER REDEFINES M1F.
               10  M1A PIC  X(0001).
           05  M1I PIC  X(0001).
      *    -------------------------------
           05  COMNTS1L PIC S9(0004) COMP.
           05  COMNTS1F PIC  X(0001).
           05  FILLER REDEFINES COMNTS1F.
               10  COMNTS1A PIC  X(0001).
           05  COMNTS1I PIC  X(0021).
      *    -------------------------------
           05  AGENT1L PIC S9(0004) COMP.
           05  AGENT1F PIC  X(0001).
           05  FILLER REDEFINES AGENT1F.
               10  AGENT1A PIC  X(0001).
           05  AGENT1I PIC  X(0010).
      *    -------------------------------
           05  INVREF1L PIC S9(0004) COMP.
           05  INVREF1F PIC  X(0001).
           05  FILLER REDEFINES INVREF1F.
               10  INVREF1A PIC  X(0001).
           05  INVREF1I PIC  X(0012).
      *    -------------------------------
           05  LEDGER1L PIC S9(0004) COMP.
           05  LEDGER1F PIC  X(0001).
           05  FILLER REDEFINES LEDGER1F.
               10  LEDGER1A PIC  X(0001).
           05  LEDGER1I PIC  X(0014).
      *    -------------------------------
           05  AGOCD1L PIC S9(0004) COMP.
           05  AGOCD1F PIC  X(0001).
           05  FILLER REDEFINES AGOCD1F.
               10  AGOCD1A PIC  X(0001).
           05  AGOCD1I PIC  X(0001).
      *    -------------------------------
           05  DETAMT1L PIC S9(0004) COMP.
           05  DETAMT1F PIC  X(0001).
           05  FILLER REDEFINES DETAMT1F.
               10  DETAMT1A PIC  X(0001).
           05  DETAMT1I PIC  X(0012).
      *    -------------------------------
           05  M2L PIC S9(0004) COMP.
           05  M2F PIC  X(0001).
           05  FILLER REDEFINES M2F.
               10  M2A PIC  X(0001).
           05  M2I PIC  X(0001).
      *    -------------------------------
           05  COMNTS2L PIC S9(0004) COMP.
           05  COMNTS2F PIC  X(0001).
           05  FILLER REDEFINES COMNTS2F.
               10  COMNTS2A PIC  X(0001).
           05  COMNTS2I PIC  X(0021).
      *    -------------------------------
           05  AGENT2L PIC S9(0004) COMP.
           05  AGENT2F PIC  X(0001).
           05  FILLER REDEFINES AGENT2F.
               10  AGENT2A PIC  X(0001).
           05  AGENT2I PIC  X(0010).
      *    -------------------------------
           05  INVREF2L PIC S9(0004) COMP.
           05  INVREF2F PIC  X(0001).
           05  FILLER REDEFINES INVREF2F.
               10  INVREF2A PIC  X(0001).
           05  INVREF2I PIC  X(0012).
      *    -------------------------------
           05  LEDGER2L PIC S9(0004) COMP.
           05  LEDGER2F PIC  X(0001).
           05  FILLER REDEFINES LEDGER2F.
               10  LEDGER2A PIC  X(0001).
           05  LEDGER2I PIC  X(0014).
      *    -------------------------------
           05  AGOCD2L PIC S9(0004) COMP.
           05  AGOCD2F PIC  X(0001).
           05  FILLER REDEFINES AGOCD2F.
               10  AGOCD2A PIC  X(0001).
           05  AGOCD2I PIC  X(0001).
      *    -------------------------------
           05  DETAMT2L PIC S9(0004) COMP.
           05  DETAMT2F PIC  X(0001).
           05  FILLER REDEFINES DETAMT2F.
               10  DETAMT2A PIC  X(0001).
           05  DETAMT2I PIC  X(0012).
      *    -------------------------------
           05  M3L PIC S9(0004) COMP.
           05  M3F PIC  X(0001).
           05  FILLER REDEFINES M3F.
               10  M3A PIC  X(0001).
           05  M3I PIC  X(0001).
      *    -------------------------------
           05  COMNTS3L PIC S9(0004) COMP.
           05  COMNTS3F PIC  X(0001).
           05  FILLER REDEFINES COMNTS3F.
               10  COMNTS3A PIC  X(0001).
           05  COMNTS3I PIC  X(0021).
      *    -------------------------------
           05  AGENT3L PIC S9(0004) COMP.
           05  AGENT3F PIC  X(0001).
           05  FILLER REDEFINES AGENT3F.
               10  AGENT3A PIC  X(0001).
           05  AGENT3I PIC  X(0010).
      *    -------------------------------
           05  INVREF3L PIC S9(0004) COMP.
           05  INVREF3F PIC  X(0001).
           05  FILLER REDEFINES INVREF3F.
               10  INVREF3A PIC  X(0001).
           05  INVREF3I PIC  X(0012).
      *    -------------------------------
           05  LEDGER3L PIC S9(0004) COMP.
           05  LEDGER3F PIC  X(0001).
           05  FILLER REDEFINES LEDGER3F.
               10  LEDGER3A PIC  X(0001).
           05  LEDGER3I PIC  X(0014).
      *    -------------------------------
           05  AGOCD3L PIC S9(0004) COMP.
           05  AGOCD3F PIC  X(0001).
           05  FILLER REDEFINES AGOCD3F.
               10  AGOCD3A PIC  X(0001).
           05  AGOCD3I PIC  X(0001).
      *    -------------------------------
           05  DETAMT3L PIC S9(0004) COMP.
           05  DETAMT3F PIC  X(0001).
           05  FILLER REDEFINES DETAMT3F.
               10  DETAMT3A PIC  X(0001).
           05  DETAMT3I PIC  X(0012).
      *    -------------------------------
           05  M4L PIC S9(0004) COMP.
           05  M4F PIC  X(0001).
           05  FILLER REDEFINES M4F.
               10  M4A PIC  X(0001).
           05  M4I PIC  X(0001).
      *    -------------------------------
           05  COMNTS4L PIC S9(0004) COMP.
           05  COMNTS4F PIC  X(0001).
           05  FILLER REDEFINES COMNTS4F.
               10  COMNTS4A PIC  X(0001).
           05  COMNTS4I PIC  X(0021).
      *    -------------------------------
           05  AGENT4L PIC S9(0004) COMP.
           05  AGENT4F PIC  X(0001).
           05  FILLER REDEFINES AGENT4F.
               10  AGENT4A PIC  X(0001).
           05  AGENT4I PIC  X(0010).
      *    -------------------------------
           05  INVREF4L PIC S9(0004) COMP.
           05  INVREF4F PIC  X(0001).
           05  FILLER REDEFINES INVREF4F.
               10  INVREF4A PIC  X(0001).
           05  INVREF4I PIC  X(0012).
      *    -------------------------------
           05  LEDGER4L PIC S9(0004) COMP.
           05  LEDGER4F PIC  X(0001).
           05  FILLER REDEFINES LEDGER4F.
               10  LEDGER4A PIC  X(0001).
           05  LEDGER4I PIC  X(0014).
      *    -------------------------------
           05  AGOCD4L PIC S9(0004) COMP.
           05  AGOCD4F PIC  X(0001).
           05  FILLER REDEFINES AGOCD4F.
               10  AGOCD4A PIC  X(0001).
           05  AGOCD4I PIC  X(0001).
      *    -------------------------------
           05  DETAMT4L PIC S9(0004) COMP.
           05  DETAMT4F PIC  X(0001).
           05  FILLER REDEFINES DETAMT4F.
               10  DETAMT4A PIC  X(0001).
           05  DETAMT4I PIC  X(0012).
      *    -------------------------------
           05  M5L PIC S9(0004) COMP.
           05  M5F PIC  X(0001).
           05  FILLER REDEFINES M5F.
               10  M5A PIC  X(0001).
           05  M5I PIC  X(0001).
      *    -------------------------------
           05  COMNTS5L PIC S9(0004) COMP.
           05  COMNTS5F PIC  X(0001).
           05  FILLER REDEFINES COMNTS5F.
               10  COMNTS5A PIC  X(0001).
           05  COMNTS5I PIC  X(0021).
      *    -------------------------------
           05  AGENT5L PIC S9(0004) COMP.
           05  AGENT5F PIC  X(0001).
           05  FILLER REDEFINES AGENT5F.
               10  AGENT5A PIC  X(0001).
           05  AGENT5I PIC  X(0010).
      *    -------------------------------
           05  INVREF5L PIC S9(0004) COMP.
           05  INVREF5F PIC  X(0001).
           05  FILLER REDEFINES INVREF5F.
               10  INVREF5A PIC  X(0001).
           05  INVREF5I PIC  X(0012).
      *    -------------------------------
           05  LEDGER5L PIC S9(0004) COMP.
           05  LEDGER5F PIC  X(0001).
           05  FILLER REDEFINES LEDGER5F.
               10  LEDGER5A PIC  X(0001).
           05  LEDGER5I PIC  X(0014).
      *    -------------------------------
           05  AGOCD5L PIC S9(0004) COMP.
           05  AGOCD5F PIC  X(0001).
           05  FILLER REDEFINES AGOCD5F.
               10  AGOCD5A PIC  X(0001).
           05  AGOCD5I PIC  X(0001).
      *    -------------------------------
           05  DETAMT5L PIC S9(0004) COMP.
           05  DETAMT5F PIC  X(0001).
           05  FILLER REDEFINES DETAMT5F.
               10  DETAMT5A PIC  X(0001).
           05  DETAMT5I PIC  X(0012).
      *    -------------------------------
           05  ERMESG1L PIC S9(0004) COMP.
           05  ERMESG1F PIC  X(0001).
           05  FILLER REDEFINES ERMESG1F.
               10  ERMESG1A PIC  X(0001).
           05  ERMESG1I PIC  X(0079).
      *    -------------------------------
           05  ERMESG2L PIC S9(0004) COMP.
           05  ERMESG2F PIC  X(0001).
           05  FILLER REDEFINES ERMESG2F.
               10  ERMESG2A PIC  X(0001).
           05  ERMESG2I PIC  X(0079).
      *    -------------------------------
           05  PFENTERL PIC S9(0004) COMP.
           05  PFENTERF PIC  X(0001).
           05  FILLER REDEFINES PFENTERF.
               10  PFENTERA PIC  X(0001).
           05  PFENTERI PIC  9(2).
      *    -------------------------------
           05  PFKEY8L PIC S9(0004) COMP.
           05  PFKEY8F PIC  X(0001).
           05  FILLER REDEFINES PFKEY8F.
               10  PFKEY8A PIC  X(0001).
           05  PFKEY8I PIC  X(0014).
       01  EL636AO REDEFINES EL636AI.
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
           05  SHOWO PIC  X(0021).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DELO PIC  X(0028).
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
           05  PAYSEQO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PAYTOO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTBYO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADDRS1O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTONO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADDRS2O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTATO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CITYSTO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ZIPO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ZIPEXTO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RELDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHKAMTO PIC  Z,ZZZ,ZZZ.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NOFIRSTO PIC  ZZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NOLASTO PIC  ZZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NOENTRSO PIC  ZZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMNTS1O PIC  X(0021).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGENT1O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INVREF1O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LEDGER1O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGOCD1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DETAMT1O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMNTS2O PIC  X(0021).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGENT2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INVREF2O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LEDGER2O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGOCD2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DETAMT2O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMNTS3O PIC  X(0021).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGENT3O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INVREF3O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LEDGER3O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGOCD3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DETAMT3O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMNTS4O PIC  X(0021).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGENT4O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INVREF4O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LEDGER4O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGOCD4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DETAMT4O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  M5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMNTS5O PIC  X(0021).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGENT5O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INVREF5O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LEDGER5O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGOCD5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DETAMT5O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERMESG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERMESG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFKEY8O PIC  X(0014).
      *    -------------------------------
00318
00319  01  DISPLAY-MAP REDEFINES EL636AI.
00320      12  FILLER                  PIC X(355).
00321      12  CK-STUB-INFO OCCURS 5 TIMES.
00322          16  CK-MAINT-LEN        PIC S9(4)   COMP.
00323          16  CK-MAINT-ATTRB      PIC X.
00324          16  CK-MAINT            PIC X.
00325          16  CK-COMNTS-LEN       PIC S9(4)   COMP.
00326          16  CK-COMNTS-ATTRB     PIC X.
00327          16  CK-COMNTS           PIC X(21).
00328          16  CK-AGENT-LEN        PIC S9(4)   COMP.
00329          16  CK-AGENT-ATTRB      PIC X.
00330          16  CK-AGENT            PIC X(10).
00331          16  CK-INVREF-LEN       PIC S9(4)   COMP.
00332          16  CK-INVREF-ATTRB     PIC X.
00333          16  CK-INVREF           PIC X(12).
00334          16  CK-LEDGER-LEN       PIC S9(4)   COMP.
00335          16  CK-LEDGER-ATTRB     PIC X.
00336          16  CK-LEDGER           PIC X(14).
00337          16  CK-AGOCD-LEN        PIC S9(4)   COMP.
00338          16  CK-AGOCD-ATTRB      PIC X.
00339          16  CK-AGOCD            PIC X(1).
00340          16  CK-DETAMT-LEN       PIC S9(4)   COMP.
00341          16  CK-DETAMT-ATTRB     PIC X.
00342          16  CK-DETAMT-IN        PIC X(12).
00343          16  CK-DETAMT-OUT REDEFINES CK-DETAMT-IN
00344                                  PIC ZZZZ,ZZZ.99-.
00345      EJECT
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
00347  01  DFHCOMMAREA             PIC X(1024).
00348
00349      EJECT
00350 *01 PARMLIST .
00351 *    02  FILLER              PIC S9(8)   COMP.
00352 *    02  ERCKWK-POINTER      PIC S9(8)   COMP.
00353 *    02  ERCMCK-POINTER      PIC S9(8)   COMP.
00354 *    02  ERCOMP-POINTER      PIC S9(8)   COMP.
00355 *    02  ELCNTL-POINTER      PIC S9(8)   COMP.
00356
00357      EJECT
00358
00359 *                             COPY ERCCKWK.
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
00360      EJECT
00361
00362 *                             COPY ERCCMCK.
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
00363      EJECT
00364
00365 *                             COPY ERCCOMP.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCOMP                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.019                          *
00006 *                                                                *
00007 *   ONLINE CREDIT SYSTEM                                         *
00008 *                                                                *
00009 *   FILE DESCRIPTION = COMPENSATION MASTER                       *
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 700   RECFORM = FIXED                          *
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ERCOMP                   RKP=2,LEN=29    *
00015 *       ALTERNATE PATH = NONE                                    *
00016 *                                                                *
00017 *   LOG = NO                                                     *
00018 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00019 *                                                                *
00020 ******************************************************************
100703*                   C H A N G E   L O G
100703*
100703* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
100703*-----------------------------------------------------------------
100703*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
100703* EFFECTIVE    NUMBER
100703*-----------------------------------------------------------------
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
041105* 041105    2005031100003  PEMA  ADD TYPE CODE FOR BANKS
092205* 092205    2005050300006  PEMA  ADD LEASE FEE
032406* 032406    2006022800001  AJRA  ADD FIRST WRITTEN DATE
072406* 072406    2006022400001  PEMA  ADD REF EDIT FLD ON B RECS
062907* 062907    2004020600003  PEMA  ADD WITHOLDING PERCENT
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
020310* 020310  CR2008100900004  PEMA  ADD REF4 EXTRACT PROCESSING
100703******************************************************************
00021
00022  01  COMPENSATION-MASTER.
00023      12  CO-RECORD-ID                          PIC XX.
00024          88  VALID-CO-ID                          VALUE 'CO'.
00025
00026      12  CO-CONTROL-PRIMARY.
00027          16  CO-COMPANY-CD                     PIC X.
00028          16  CO-CONTROL.
00029              20  CO-CTL-1.
00030                  24  CO-CARR-GROUP.
00031                      28  CO-CARRIER            PIC X.
00032                      28  CO-GROUPING.
00033                          32  CO-GROUP-PREFIX   PIC XXX.
00034                          32  CO-GROUP-PRIME    PIC XXX.
00035                  24  CO-RESP-NO.
00036                      28  CO-RESP-PREFIX        PIC X(4).
00037                      28  CO-RESP-PRIME         PIC X(6).
00038              20  CO-CTL-2.
00039                  24  CO-ACCOUNT.
00040                      28  CO-ACCT-PREFIX        PIC X(4).
00041                      28  CO-ACCT-PRIME         PIC X(6).
00042          16  CO-TYPE                           PIC X.
00043              88  CO-COMPANY-TYPE                  VALUE 'C'.
041105             88  CO-GEN-AGENT-TYPE     VALUE 'G' 'B'.
00045              88  CO-ACCOUNT-TYPE                  VALUE 'A'.
00046
00047      12  CO-MAINT-INFORMATION.
00048          16  CO-LAST-MAINT-DT                  PIC XX.
00049          16  CO-LAST-MAINT-HHMMSS              PIC S9(7)  COMP-3.
00050          16  CO-LAST-MAINT-USER                PIC X(4).
011410     12  FILLER                                PIC XX.
020210     12  CO-STMT-TYPE                          PIC XXX.
011410     12  CO-COMP-TYPE                          PIC X.
011410         88  CO-COMP-IS-SPPDD                    VALUE '1'.
           12  CO-STMT-OWNER                         PIC X(4).
00053      12  CO-BALANCE-CONTROL                    PIC X.
00054          88  CO-CARRY-BALANCE                     VALUE 'Y'.
00055          88  CO-NO-BALANCE                        VALUE 'N'.
00056
00057      12  CO-INTERNAL-CONTROL-1                 PIC X.
00058          88  CO-AUTO-GENERATED-THIS-RUN           VALUE 'X'.
00059          88  CO-AUTO-GENERATED                    VALUE 'Y'.
00060          88  CO-NOT-AUTO-GENERATED                VALUE 'N'.
00061
00062      12  CO-INTERNAL-CONTROL-2                 PIC X.
00063          88  CO-STATEMENT-THIS-RUN                VALUE 'Y'.
00064          88  CO-NO-STATEMENT-THIS-RUN             VALUE 'N'.
00065
062907     12  CO-GA-WITHOLD-PCT                     PIC S9V9999 COMP-3.
062907     12  CO-GA-DIRECT-DEP                      PIC X.
062907     12  CO-FUTURE-SPACE                       PIC X.
062907         88  CO-FUTURE-NOT-USED                   VALUE ' '.
00068
00069      12  CO-ACCT-NAME                          PIC X(30).
00070      12  CO-MAIL-NAME                          PIC X(30).
00071      12  CO-ADDR-1                             PIC X(30).
00072      12  CO-ADDR-2                             PIC X(30).
CIDMOD     12  CO-ADDR-3.
               16  CO-ADDR-CITY                      PIC X(27).
               16  CO-ADDR-STATE                     PIC XX.
CIDMOD     12  CO-CSO-1099                           PIC X.
00074      12  CO-ZIP.
00075          16  CO-ZIP-PRIME.
00076              20  CO-ZIP-PRI-1ST                PIC X.
00077                  88  CO-CANADIAN-POST-CODE  VALUE 'A' THRU 'Z'.
00078              20  FILLER                        PIC X(4).
00079          16  CO-ZIP-PLUS4                      PIC X(4).
00080      12  CO-CANADIAN-POSTAL-CODE  REDEFINES  CO-ZIP.
00081          16  CO-CAN-POSTAL-1                   PIC XXX.
00082          16  CO-CAN-POSTAL-2                   PIC XXX.
00083          16  FILLER                            PIC XXX.
00084      12  CO-SOC-SEC                            PIC X(13).
00085      12  CO-TELEPHONE.
00086          16  CO-AREA-CODE                      PIC XXX.
00087          16  CO-PREFIX                         PIC XXX.
00088          16  CO-PHONE                          PIC X(4).
00089
00090      12  CO-ROLADEX-PRINT-DT                   PIC XX.
00091
00092      12  CO-AR-BAL-LEVEL                       PIC X.
00093          88  CO-AR-REF-LVL                        VALUE '1'.
00094          88  CO-AR-BILL-REF-LVL                   VALUE '1'.
00095          88  CO-AR-BILL-LVL                       VALUE '2'.
00096          88  CO-AR-AGT-LVL                        VALUE '3'.
00097          88  CO-AR-FR-LVL                         VALUE '4'.
00098
00099      12  CO-AR-NORMAL-PRINT                    PIC X.
00100          88  CO-AR-BILL-IS-PRINTED                VALUE 'Y'.
00101          88  CO-AR-BILL-NOT-PRINTED               VALUE 'N'.
00102
00103      12  CO-AR-SUMMARY-CODE                    PIC X(6).
00104
00105      12  CO-AR-REPORTING                       PIC X.
00106          88  CO-AR-NET-REPORT                     VALUE 'N'.
00107          88  CO-AR-GROSS-REPORT                   VALUE 'G'.
00108
00109      12  CO-AR-PULL-CHECK                      PIC X.
00110          88  CO-AR-CHECKS-PULLED                  VALUE 'Y'.
00111          88  CO-AR-CHECKS-NOT-PULLED              VALUE 'N'.
00112
00113      12  CO-AR-BALANCE-PRINT                   PIC X.
00114          88  CO-AR-PRINT-NO-BALANCE               VALUE 'N'.
00115
00116      12  CO-AR-LAST-RUN-CODE                   PIC X.
00117          88  CO-AR-LAST-RUN-ANNUAL                VALUE 'A'.
00118          88  CO-AR-LAST-RUN-CYCLE                 VALUE 'C'.
00119          88  CO-AR-LAST-RUN-EOM                   VALUE 'M'.
00120
00121      12  CO-LAST-EOM-STMT-DT                   PIC XX.
00122
00123      12  CO-USER-CODE                          PIC X.
00124      12  CO-REPORT-GROUP-ID                    PIC X(12).
00125
00126 ******************************************************************
00127 *    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN THE TOTALS AS OF
00128 *    THE LAST MONTH END RUN.
00129 ******************************************************************
00130
00131      12  CO-LAST-ACTIVITY-DATE.
00132          16  CO-ACT-YEAR                       PIC 99.
00133          16  CO-ACT-MONTH                      PIC 99.
00134          16  CO-ACT-DAY                        PIC 99.
00135
00136      12  CO-LAST-STMT-DT.
00137          16  CO-LAST-STMT-YEAR                 PIC 99.
00138          16  CO-LAST-STMT-MONTH                PIC 99.
00139          16  CO-LAST-STMT-DAY                  PIC 99.
00140
00141      12  CO-MO-END-TOTALS.
00142          16  CO-MONTHLY-TOTALS.
00143              20  CO-BAL-FWD                PIC S9(7)V99   COMP-3.
00144              20  CO-CUR-COM                PIC S9(7)V99   COMP-3.
00145              20  CO-CUR-CHG                PIC S9(7)V99   COMP-3.
00146              20  CO-CUR-PMT                PIC S9(7)V99   COMP-3.
00147              20  CO-END-BAL                PIC S9(7)V99   COMP-3.
00148
00149          16  CO-AGING-TOTALS.
00150              20  CO-CUR                    PIC S9(7)V99   COMP-3.
00151              20  CO-OV30                   PIC S9(7)V99   COMP-3.
00152              20  CO-OV60                   PIC S9(7)V99   COMP-3.
00153              20  CO-OV90                   PIC S9(7)V99   COMP-3.
00154
00155          16  CO-YTD-TOTALS.
00156              20  CO-YTD-COM                PIC S9(7)V99   COMP-3.
00157              20  CO-YTD-OV                 PIC S9(7)V99   COMP-3.
00158
00159          16  CO-OVER-UNDER-TOTALS.
00160              20  CO-CUR-OVR-UNDR           PIC S9(7)V99   COMP-3.
00161              20  CO-YTD-OVR-UNDR           PIC S9(7)V99   COMP-3.
00162
00163      12  CO-MISCELLANEOUS-TOTALS.
00164          16  CO-FICA-TOTALS.
00165              20  CO-CUR-FICA               PIC S9(7)V99   COMP-3.
00166              20  CO-YTD-FICA               PIC S9(7)V99   COMP-3.
00167
00168          16  CO-CLAIM-TOTALS.
00169              20  CO-LF-CLM-AMT             PIC S9(9)V99   COMP-3.
00170              20  CO-AH-CLM-AMT             PIC S9(9)V99   COMP-3.
00171
00172 ******************************************************************
00173 *    FOR A/R USERS THE FOLLOWING FIELDS CONTAIN TOTALS THAT
00174 *    REPRESENT CURRENT MONTH (TOTALS OF CYCLES).
00175 ******************************************************************
00176
00177      12  CO-CURRENT-TOTALS.
00178          16  CO-CURRENT-LAST-STMT-DT.
00179              20  CO-CURRENT-LAST-STMT-YEAR     PIC 99.
00180              20  CO-CURRENT-LAST-STMT-MONTH    PIC 99.
00181              20  CO-CURRENT-LAST-STMT-DAY      PIC 99.
00182
00183          16  CO-CURRENT-MONTHLY-TOTALS.
00184              20  CO-CURRENT-BAL-FWD        PIC S9(7)V99   COMP-3.
00185              20  CO-CURRENT-CUR-COM        PIC S9(7)V99   COMP-3.
00186              20  CO-CURRENT-CUR-CHG        PIC S9(7)V99   COMP-3.
00187              20  CO-CURRENT-CUR-PMT        PIC S9(7)V99   COMP-3.
00188              20  CO-CURRENT-END-BAL        PIC S9(7)V99   COMP-3.
00189
00190          16  CO-CURRENT-AGING-TOTALS.
00191              20  CO-CURRENT-CUR            PIC S9(7)V99   COMP-3.
00192              20  CO-CURRENT-OV30           PIC S9(7)V99   COMP-3.
00193              20  CO-CURRENT-OV60           PIC S9(7)V99   COMP-3.
00194              20  CO-CURRENT-OV90           PIC S9(7)V99   COMP-3.
00195
00196          16  CO-CURRENT-YTD-TOTALS.
00197              20  CO-CURRENT-YTD-COM        PIC S9(7)V99   COMP-3.
00198              20  CO-CURRENT-YTD-OV         PIC S9(7)V99   COMP-3.
00199
00200      12  CO-PAID-COMM-TOTALS.
00201          16  CO-YTD-PAID-COMMS.
00202              20  CO-YTD-PAID-COM           PIC S9(7)V99   COMP-3.
00203              20  CO-YTD-PAID-OV            PIC S9(7)V99   COMP-3.
00204
00205      12  CO-CURRENT-MONTH-ACTIVITY         PIC X.
00206          88  CO-HAS-CURR-MONTH-ACTIVITY       VALUE 'Y'.
00207          88  CO-NO-CURR-MONTH-ACTIVITY        VALUE 'N'.
00208
00209      12  CO-DELINQUENT-LETTER-CODE         PIC X.
00210          88  CO-ACCOUNT-1ST-LETTER            VALUE 'A'.
00211          88  CO-ACCOUNT-2ND-LETTER            VALUE 'B'.
00212          88  CO-AGENT-1ST-LETTER              VALUE 'B'.
00213          88  CO-AGENT-2ND-LETTER              VALUE 'G'.
00214          88  CO-OVERWRITE-LETTER              VALUE 'O'.
00215          88  CO-MEMO-TO-REGION-MGR            VALUE 'M'.
00216          88  CO-FINAL-LETTER                  VALUE 'F'.
00217          88  CO-RECONCILING                   VALUE 'R'.
00218          88  CO-PHONE-CALL                    VALUE 'P'.
00219          88  CO-LEGAL                         VALUE 'L'.
00220          88  CO-COLLECTION-AGENCY             VALUE 'C'.
00221          88  CO-WRITE-OFF                     VALUE 'W'.
00222          88  CO-NO-ACTION                     VALUE 'N' ' '.
00223
00224      12  CO-CSR-CODE                       PIC X(4).
00225
00226      12  CO-GA-STATUS-INFO.
00227          16  CO-GA-EFFECTIVE-DT            PIC XX.
00228          16  CO-GA-TERMINATION-DT          PIC XX.
00229          16  CO-GA-STATUS-CODE             PIC X.
00230              88  CO-GA-ACTIVE                 VALUE 'A'.
00231              88  CO-GA-INACTIVE               VALUE 'I'.
00232              88  CO-GA-PENDING                VALUE 'P'.
00233          16  CO-GA-COMMENTS.
00234              20  CO-GA-COMMENT-1           PIC X(40).
00235              20  CO-GA-COMMENT-2           PIC X(40).
00236              20  CO-GA-COMMENT-3           PIC X(40).
00237              20  CO-GA-COMMENT-4           PIC X(40).
00238
00239      12  CO-RPTCD2                         PIC X(10).
00240
00241      12  CO-TYPE-AGENT                     PIC X(01).
00242          88  CO-CORPORATION                   VALUE 'C'.
00243          88  CO-PARTNERSHIP                   VALUE 'P'.
00244          88  CO-SOLE-PROPRIETOR               VALUE 'S'.
00245          88  CO-TRUST                         VALUE 'T'.
00246          88  CO-UNKNOWN                       VALUE ' ' 'X'.
00247
00248      12  CO-FAXNO.
00249          16  CO-FAX-AREA-CODE                  PIC XXX.
00250          16  CO-FAX-PREFIX                     PIC XXX.
00251          16  CO-FAX-PHONE                      PIC X(4).
00252
00253      12  CO-BANK-INFORMATION.
00254          16  CO-BANK-TRANSIT-NO                PIC X(8).
00255          16  CO-BANK-TRANSIT-NON REDEFINES
00256              CO-BANK-TRANSIT-NO                PIC 9(8).
00257
00258          16  CO-BANK-ACCOUNT-NUMBER            PIC X(17).
           12  CO-MISC-DEDUCT-INFO REDEFINES
                        CO-BANK-INFORMATION.
               16  CO-MD-GL-ACCT                     PIC X(10).
               16  CO-MD-DIV                         PIC XX.
               16  CO-MD-CENTER                      PIC X(4).
               16  CO-MD-AMT                        PIC S9(5)V99 COMP-3.
092707         16  CO-CREATE-AP-CHECK                PIC X.
092707         16  CO-DELIVER-CK-TO-MEL              PIC X.
092707         16  FILLER                            PIC XXX.
00259      12  CO-ACH-STATUS                         PIC X.
00260          88  CO-ACH-ACTIVE                         VALUE 'A'.
00261          88  CO-ACH-PENDING                        VALUE 'P'.
00262
CIDMOD     12  CO-BILL-SW                            PIC X.
CIDMOD     12  CO-CONTROL-NAME                       PIC X(30).
092205     12  CO-MAX-BANK-FEE-LEASE                 PIC S999V99 COMP-3.
111504     12  CO-MAX-BANK-FEE                       PIC S999V99 COMP-3.
100703     12  CO-CLP-STATE                          PIC XX.
032406     12  CO-FIRST-WRITTEN-DT                   PIC XX.
072406     12  CO-SPP-REFUND-EDIT                    PIC X.
00264
00265 ******************************************************************
00366      EJECT
00367 *                             COPY ELCCNTL.
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
00368
00369
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA
                                CHECK-WORK-RECORDS COMM-CHECK-RECORDS
                                COMPENSATION-MASTER CONTROL-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL636' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00371
00372      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00373      MOVE 2                      TO EMI-NUMBER-OF-LINES.
00374
00375      IF EIBCALEN = 0
00376          GO TO 8800-UNAUTHORIZED-ACCESS.
00377
00378      MOVE EIBTRMID               TO QID-TERM.
00379      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00380      MOVE '5'                    TO DC-OPTION-CODE.
00381      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
00382      MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.
00383      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DT.
00384      MOVE ZEROS                  TO WS-NOFIRST.
00385
00386      IF PI-RETURN-TO-PROGRAM = THIS-PGM
00387          MOVE PI-CALLING-PROGRAM TO RETURNED-FROM
00388      ELSE
00389          MOVE SPACES             TO RETURNED-FROM.
00390
00391      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00392          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00393              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00394              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00395              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00396              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00397              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00398              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00399              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00400              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00401          ELSE
00402              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00403              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00404              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00405              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00406              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00407              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00408              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00409              MOVE SPACES               TO PI-SAVED-PROGRAM-6.
00410
00411      MOVE LOW-VALUES             TO EL636AI.
00412
00413      IF EIBTRNID NOT = TRANS-EXJA
00414          IF RETURNED-FROM EQUAL XCTL-EL6361
00415              GO TO 5000-DISPLAY-PAYEE
00416          ELSE
00417              MOVE SPACES              TO PI-PROGRAM-WORK-AREA
00418              MOVE PI-COMPANY-CD       TO PI-ERCKWK-COMP-CD
00419              MOVE +0                  TO PI-ERCKWK-SEQ-NO
00420              MOVE +0                  TO PI-NO-ENTRIES-DISPLAYED
00421              GO TO 8100-SEND-INITIAL-MAP.
00422
00423      
      * EXEC CICS HANDLE CONDITION
00424 *        PGMIDERR  (9600-PGMID-ERROR)
00425 *        ERROR     (9990-ABEND)
00426 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00003786' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303033373836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00427
00428      IF EIBAID = DFHCLEAR
00429          GO TO 9400-CLEAR.
00430
00431      IF PI-PROCESSOR-ID = 'LGXX'
00432          GO TO 0200-RECEIVE.
00433
00434      
      * EXEC CICS READQ TS
00435 *        QUEUE  (QID)
00436 *        INTO   (SECURITY-CONTROL)
00437 *        LENGTH (SC-COMM-LENGTH)
00438 *        ITEM   (SC-ITEM)
00439 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00003797' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00440
00441      MOVE SC-CREDIT-DISPLAY (6)   TO PI-DISPLAY-CAP.
00442      MOVE SC-CREDIT-UPDATE  (6)   TO PI-MODIFY-CAP.
00443
00444      IF NOT DISPLAY-CAP
00445          MOVE 'READ'          TO SM-READ
00446          PERFORM 9995-SECURITY-VIOLATION
00447          MOVE ER-0070         TO  EMI-ERROR
00448          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00449          GO TO 8100-SEND-INITIAL-MAP.
00450
00451      EJECT
00452
00453 ******************************************************************
00454 *                                                                *
00455 *              R E C E I V E   M A P                             *
00456 *                                                                *
00457 ******************************************************************
00458
00459  0200-RECEIVE.
00460
00461      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00462          MOVE ER-0008            TO EMI-ERROR
00463          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00464          MOVE -1                 TO MAINTL
00465          GO TO 8200-SEND-DATAONLY.
00466
00467      
      * EXEC CICS RECEIVE
00468 *        MAP      (EL636A)
00469 *        MAPSET   (MAPSET-EL636S)
00470 *        INTO     (EL636AI)
00471 *    END-EXEC.
           MOVE LENGTH OF
            EL636AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00003830' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL636A, 
                 EL636AI, 
                 DFHEIV11, 
                 MAPSET-EL636S, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00472
00473
00474      IF PFENTERL GREATER THAN ZERO
00475         IF EIBAID NOT = DFHENTER
00476            MOVE ER-0004          TO EMI-ERROR
00477            MOVE AL-UNBOF         TO PFENTERA
00478            MOVE -1               TO PFENTERL
00479            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00480            GO TO 8200-SEND-DATAONLY
00481         ELSE
00482            IF PFENTERI NUMERIC AND
00483               PFENTERI GREATER 0 AND LESS 25
00484               MOVE PF-VALUES (PFENTERI) TO EIBAID
00485            ELSE
00486               MOVE ER-0029       TO EMI-ERROR
00487               MOVE AL-UNBOF      TO PFENTERA
00488               MOVE -1            TO PFENTERL
00489               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00490               GO TO 8200-SEND-DATAONLY.
00491
00492      EJECT
00493
00494 ******************************************************************
00495 *                                                                *
00496 *              C H E C K   P F K E Y S                           *
00497 *                                                                *
00498 ******************************************************************
00499
00500  0300-CHECK-PFKEYS.
00501
00502      IF EIBAID = DFHPF23
00503          GO TO 8810-PF23.
00504
00505      IF EIBAID = DFHPF24
00506          GO TO 9200-RETURN-MAIN-MENU.
00507
00508      IF EIBAID = DFHPF12
00509          GO TO 9500-PF12.
00510
00511      IF EIBAID = DFHPF1
00512          GO TO 6000-DISPLAY-NEXT-PAYEE.
00513
00514      IF EIBAID = DFHPF2
00515          GO TO 6100-DISPLAY-PREV-PAYEE.
00516
00517      IF EIBAID = DFHPF3
00518          IF PI-LAST-STUB-KEY NOT EQUAL SPACES
00519              GO TO 5000-DISPLAY-PAYEE.
00520
00521      IF EIBAID = DFHPF4
00522          IF PI-STUB-KEY (1) NOT EQUAL SPACES
00523              GO TO 6600-DISPLAY-PREV-ENTRIES.
00524
00525      IF EIBAID = DFHPF5
00526          GO TO 7000-RELEASE-ENTRIES.
00527
00528      IF EIBAID = DFHPF6
00529          MOVE LOW-VALUES         TO EL636AI
00530          MOVE 'A'                TO MAINTI
00531                                     PI-MAINT-FUNCTION
00532          MOVE AL-UANON           TO MAINTA
00533          GO TO 8100-SEND-INITIAL-MAP.
00534
00535      IF EIBAID = DFHPF7
00536         IF MAINTI = 'D'
00537            GO TO 1000-EDIT-MAP.
00538
00539      IF EIBAID = DFHPF8
00540          GO TO 8820-PF08.
00541
00542      IF EIBAID = DFHENTER
00543          GO TO 1000-EDIT-MAP.
00544
00545      MOVE ER-0008 TO EMI-ERROR.
00546      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00547      MOVE -1                     TO PFENTERL.
00548
00549      GO TO 8200-SEND-DATAONLY.
00550
00551      EJECT
00552
00553 ******************************************************************
00554 *                                                                *
00555 *                  E D I T    M A P                              *
00556 *                                                                *
00557 ******************************************************************
00558
00559  1000-EDIT-MAP.
00560
00561      IF MAINTI = 'C' OR 'D' OR 'S' OR 'A' OR 'V'
00562         MOVE MAINTI              TO PI-MAINT-FUNCTION
00563         MOVE AL-UANON            TO MAINTA
00564      ELSE
00565         MOVE ER-0023             TO EMI-ERROR
00566         MOVE -1                  TO MAINTL
00567         MOVE AL-UABON            TO MAINTA
00568         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00569         GO TO 8200-SEND-DATAONLY.
00570
00571      IF PAYSEQL GREATER THAN 0
00572           MOVE PAYSEQI           TO WS-PAY-SEQ-NUM
00573      ELSE
00574           MOVE +1                TO WS-PAY-SEQ-NUM.
00575
00576      IF MAINTI = 'S' AND
00577         NOFIRSTL GREATER THAN 0
00578         IF NOFIRSTI NUMERIC
00579             MOVE NOFIRSTI          TO WS-NOFIRST
00580         ELSE
00581             MOVE -1                  TO NOFIRSTL
00582             MOVE ER-2223             TO EMI-ERROR
00583             MOVE AL-UNBON            TO NOFIRSTA
00584             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00585
00586      IF MAINTI = 'C' OR 'D' OR 'V'
00587         IF PI-COMPANY-CD   = PI-SVCKWK-COMP-CD  AND
00588            CSRI            = PI-SVCKWK-CSR      AND
00589            CARI            = PI-SVCKWK-CARRIER  AND
00590            GROUPI          = PI-SVCKWK-GROUPING AND
00591            PAYEEI          = PI-SVCKWK-PAYEE    AND
00592            WS-PAY-SEQ-NUM  = PI-SVCKWK-PAYEE-SEQ
00593            NEXT SENTENCE
00594         ELSE
00595            MOVE ER-2056             TO EMI-ERROR
00596            MOVE -1                  TO MAINTL
00597            MOVE AL-UABON            TO MAINTA
00598            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00599            GO TO 8200-SEND-DATAONLY.
00600
00601      IF MAINTI = 'D'
00602         IF EIBAID = DFHPF7
00603            MOVE MAINTI              TO PI-MAINT-FUNCTION
00604            MOVE AL-UANON            TO MAINTA
00605         ELSE
00606            MOVE ER-3147             TO EMI-ERROR
00607            MOVE -1                  TO MAINTL
00608            MOVE AL-UABON            TO MAINTA
00609            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00610            GO TO 8200-SEND-DATAONLY.
00611
00612      IF NOT MODIFY-CAP
00613         IF MAINTI = 'S'
00614            MOVE MAINTI              TO PI-MAINT-FUNCTION
00615         ELSE
00616            MOVE 'UPDATE'            TO SM-READ
00617            PERFORM 9995-SECURITY-VIOLATION
00618            MOVE ER-0070             TO EMI-ERROR
00619            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00620            GO TO 8100-SEND-INITIAL-MAP.
00621
00622      IF MAINTI = 'S' OR 'D' OR 'V'
00623         GO TO 1050-EDIT-COMPLETE.
00624
00625      IF CARL GREATER THAN ZERO
00626         MOVE AL-UANON            TO CARA
00627      ELSE
00628         MOVE -1                  TO CARL
00629         MOVE ER-0194             TO EMI-ERROR
00630         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00631
00632      IF GROUPL GREATER THAN ZEROS
00633         MOVE AL-UANON            TO GROUPA
00634      ELSE
00635         MOVE -1                  TO GROUPL
00636         MOVE ER-0195             TO EMI-ERROR
00637         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00638
00639      IF PAYEEL GREATER THAN ZEROS
00640         MOVE AL-UANON            TO PAYEEA
00641      ELSE
00642         MOVE -1                  TO PAYEEL
00643         MOVE ER-3148             TO EMI-ERROR
00644         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00645
00646      IF CSRL GREATER THAN ZERO
00647         MOVE AL-UANON            TO CSRA
00648         PERFORM 1600-READ-CNTL-FILE  THRU 1690-EXIT.
00649
00650
00651      IF PAYTOL  GREATER THAN ZEROS
00652         MOVE AL-UANON            TO PAYTOA.
00653
00654      IF ADDRS1L GREATER THAN ZEROS
00655         MOVE AL-UANON            TO ADDRS1A.
00656
00657      IF ADDRS2L GREATER THAN ZEROS
00658         MOVE AL-UANON            TO ADDRS2A.
00659
00660      IF CITYSTL GREATER THAN ZEROS
00661         MOVE AL-UANON            TO CITYSTA.
00662
00663      IF ZIPL    GREATER THAN ZEROS
00664         MOVE AL-UANON            TO ZIPA.
00665
00666      IF ZIPEXTL GREATER THAN ZEROS
00667         MOVE AL-UANON            TO ZIPEXTL.
00668
00669      MOVE +0                     TO WS-SUB1.
00670
00671  1025-EDIT-STUB-INFO.
00672
00673      ADD +1                      TO WS-SUB1.
00674
00675      IF WS-SUB1 GREATER THAN +5
00676         GO TO 1050-EDIT-COMPLETE.
00677
00678      IF CK-MAINT-LEN (WS-SUB1) NOT GREATER THAN ZEROS
00679          IF CK-AGENT-LEN (WS-SUB1)   GREATER THAN ZERO  OR
00680             CK-COMNTS-LEN (WS-SUB1)  GREATER THAN ZERO  OR
00681             CK-INVREF-LEN (WS-SUB1)  GREATER THAN ZERO  OR
00682             CK-LEDGER-LEN (WS-SUB1)  GREATER THAN ZERO  OR
00683             CK-AGOCD-LEN  (WS-SUB1)  GREATER THAN ZERO  OR
00684             CK-DETAMT-LEN (WS-SUB1)  GREATER THAN ZERO
00685              NEXT SENTENCE
00686          ELSE
00687              GO TO 1025-EDIT-STUB-INFO.
00688
00689      IF PI-COMPANY-ID = 'NSL' AND
00690          CK-MAINT (WS-SUB1) = 'H'
00691              MOVE ER-0023        TO EMI-ERROR
00692              MOVE -1             TO CK-MAINT-LEN    (WS-SUB1)
00693              MOVE AL-UABON       TO CK-MAINT-ATTRB  (WS-SUB1)
00694              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00695      ELSE
00696          IF CK-MAINT (WS-SUB1) = 'A' OR 'C' OR 'D' OR 'I'
00697                                      OR 'R' OR 'N' OR 'H'
00698              MOVE AL-UANON       TO CK-MAINT-ATTRB  (WS-SUB1)
00699          ELSE
00700              MOVE ER-0023        TO EMI-ERROR
00701              MOVE -1             TO CK-MAINT-LEN    (WS-SUB1)
00702              MOVE AL-UABON       TO CK-MAINT-ATTRB  (WS-SUB1)
00703              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00704
00705      IF (CK-MAINT-LEN (WS-SUB1) GREATER THAN ZEROS) AND
00706             (CK-MAINT (WS-SUB1) = 'H')
00707          IF CK-AGENT-LEN (WS-SUB1)   GREATER THAN ZERO  OR
00708             CK-COMNTS-LEN (WS-SUB1)  GREATER THAN ZERO  OR
00709             CK-INVREF-LEN (WS-SUB1)  GREATER THAN ZERO  OR
00710             CK-LEDGER-LEN (WS-SUB1)  GREATER THAN ZERO  OR
00711             CK-AGOCD-LEN  (WS-SUB1)  GREATER THAN ZERO  OR
00712             CK-DETAMT-LEN (WS-SUB1)  GREATER THAN ZERO
00713               MOVE ER-7804          TO EMI-ERROR
00714               MOVE -1               TO CK-MAINT-LEN    (WS-SUB1)
00715               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00716          ELSE
00717               GO TO 1025-EDIT-STUB-INFO.
00718
00719      IF CK-MAINT        (WS-SUB1) = 'D'
00720          GO TO 1025-EDIT-STUB-INFO.
00721
00722 ******    TO DISTINGUISH BETWEEN AN R OR I USED AS A CHANGE OR
00723 ******    AN ADDITION CHANGE I TO J AND R TO S FOR AN ADD
00724      IF CK-MAINT (WS-SUB1) = 'I' AND
00725         WS-SUB1 GREATER THAN PI-NO-ENTRIES-DISPLAYED
00726          MOVE 'J' TO CK-MAINT (WS-SUB1).
00727
00728      IF CK-MAINT (WS-SUB1) = 'R' AND
00729         WS-SUB1 GREATER THAN PI-NO-ENTRIES-DISPLAYED
00730          MOVE 'S' TO CK-MAINT (WS-SUB1).
00731
00732 **********************
00733
00734      IF CK-INVREF-LEN (WS-SUB1)  GREATER THAN ZEROS
00735          IF CK-MAINT  (WS-SUB1) = 'R' OR 'I' OR 'J' OR 'S'
00736              NEXT SENTENCE
00737          ELSE
00738            MOVE ER-3189          TO EMI-ERROR
00739            MOVE -1               TO CK-INVREF-LEN   (WS-SUB1)
00740            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00741
00742      IF CK-MAINT  (WS-SUB1) = 'R' OR 'I' OR 'J' OR 'S'
00743          IF CK-INVREF (WS-SUB1)  NOT EQUAL LOW-VALUES
00744              NEXT SENTENCE
00745          ELSE
00746            MOVE ER-3256          TO EMI-ERROR
00747            MOVE -1               TO CK-INVREF-LEN   (WS-SUB1)
00748            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00749
00750      IF CK-MAINT        (WS-SUB1) = 'N' AND
00751         MAINTI                    = ('A' OR 'C')
00752         IF CK-INVREF-LEN (WS-SUB1)  GREATER THAN ZEROS
00753            MOVE ER-3173          TO EMI-ERROR
00754            MOVE -1               TO CK-INVREF-LEN   (WS-SUB1)
00755            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00756
00757      IF CK-AGOCD-LEN (WS-SUB1) GREATER THAN ZERO
00758          IF CK-AGOCD (WS-SUB1)  EQUAL 'A' OR 'G' OR 'O'
00759              NEXT SENTENCE
00760          ELSE
00761              MOVE ER-3146          TO EMI-ERROR
00762              MOVE -1               TO CK-AGOCD-LEN   (WS-SUB1)
00763              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00764
00765      IF CK-AGOCD (WS-SUB1) EQUAL 'A'
00766          IF CK-AGENT (WS-SUB1) GREATER THAN SPACES
00767              NEXT SENTENCE
00768          ELSE
00769              MOVE ER-3190          TO EMI-ERROR
00770              MOVE -1               TO CK-AGENT-LEN   (WS-SUB1)
00771              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00772
00773      IF CK-MAINT    (WS-SUB1) = 'A' OR 'J' OR 'S'
00774          IF CK-AGOCD-LEN  (WS-SUB1)  GREATER THAN ZERO
00775              NEXT SENTENCE
00776          ELSE
00777              MOVE ER-3146          TO EMI-ERROR
00778              MOVE -1               TO CK-AGOCD-LEN   (WS-SUB1)
00779              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00780
00781      IF CK-MAINT    (WS-SUB1) = 'R' OR 'I' OR 'J' OR 'S'
00782          IF CK-AGOCD-LEN  (WS-SUB1)  GREATER THAN ZERO
00783              NEXT SENTENCE
00784          ELSE
00785              IF CK-AGOCD (WS-SUB1) = SPACE OR LOW-VALUE
00786                  NEXT SENTENCE
00787              ELSE
00788                  MOVE ER-3146      TO EMI-ERROR
00789                  MOVE -1           TO CK-AGOCD-LEN   (WS-SUB1)
00790                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00791
00792      IF CK-LEDGER-LEN (WS-SUB1) GREATER THAN ZEROS
00793         MOVE AL-UANON            TO  CK-LEDGER-LEN  (WS-SUB1).
00794
00795      IF CK-MAINT (WS-SUB1)     = 'A' OR 'C' OR 'N'
00796          IF PI-COMPANY-ID = 'CRI'
00797              IF CK-LEDGER-LEN (WS-SUB1) GREATER THAN ZEROS
00798                 PERFORM 1500-VERIFY-LEDGER THRU 1590-EXIT
00799              ELSE
00800                  MOVE ER-3149    TO EMI-ERROR
00801                  MOVE -1         TO CK-LEDGER-LEN    (WS-SUB1)
00802                  MOVE AL-UABON   TO CK-LEDGER-ATTRB  (WS-SUB1)
00803                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00804
00805      IF CK-MAINT (WS-SUB1)     = 'R'  AND MAINTI = 'A'
00806          IF PI-COMPANY-ID = 'CRI'
00807              IF CK-LEDGER-LEN (WS-SUB1) GREATER THAN ZEROS
00808                 PERFORM 1500-VERIFY-LEDGER THRU 1590-EXIT
00809              ELSE
00810                 MOVE ER-3149     TO EMI-ERROR
00811                 MOVE -1          TO CK-LEDGER-LEN    (WS-SUB1)
00812                 MOVE AL-UABON    TO CK-LEDGER-ATTRB  (WS-SUB1)
00813                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00814
00815      IF CK-DETAMT-LEN (WS-SUB1)  GREATER THAN ZEROS
00816         MOVE CK-DETAMT-IN (WS-SUB1) TO DEEDIT-FIELD
00817         PERFORM 8600-DEEDIT
00818         IF DEEDIT-FIELD-V2 NUMERIC AND
00819            DEEDIT-FIELD-V2 NOT EQUAL ZERO
00820            MOVE DEEDIT-FIELD-V2  TO WS-DETAMT       (WS-SUB1)
00821            MOVE AL-UNNON         TO CK-DETAMT-ATTRB (WS-SUB1)
00822         ELSE
00823            MOVE ER-2223          TO EMI-ERROR
00824            MOVE -1               TO CK-DETAMT-LEN   (WS-SUB1)
00825            MOVE AL-UNBON         TO CK-DETAMT-ATTRB (WS-SUB1)
00826            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00827
00828      IF CK-MAINT (WS-SUB1) = 'A' OR 'N' OR 'J' OR 'S'
00829         IF CK-DETAMT-LEN (WS-SUB1) GREATER THAN ZEROS
00830            NEXT SENTENCE
00831         ELSE
00832            MOVE ER-3158          TO EMI-ERROR
00833            MOVE -1               TO CK-DETAMT-LEN   (WS-SUB1)
00834            MOVE AL-UABON         TO CK-DETAMT-ATTRB (WS-SUB1)
00835            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00836
00837      IF CK-MAINT (WS-SUB1)     = 'N'
00838          GO TO 1025-EDIT-STUB-INFO.
00839
00840      IF CK-MAINT (WS-SUB1) = 'A' OR 'J' OR 'S'
00841          IF CK-AGENT-LEN (WS-SUB1) GREATER THAN ZERO
00842              IF CK-AGENT (WS-SUB1) = SPACES OR LOW-VALUES
00843                  MOVE '*'        TO  WS-SPACE-AGENT-SW
00844                  PERFORM 1480-VERIFY-GA-ONLY  THRU  1490-EXIT
00845                  GO TO 1040-STUB-EDIT-COMPLETE
00846              ELSE
00847                  PERFORM 1400-VERIFY-AGENT  THRU  1470-EXIT
00848                  GO TO 1040-STUB-EDIT-COMPLETE
00849          ELSE
00850                  PERFORM 1480-VERIFY-GA-ONLY  THRU   1490-EXIT
00851                  GO TO 1040-STUB-EDIT-COMPLETE.
00852
00853      IF CK-MAINT (WS-SUB1) = 'C' OR 'I' OR 'R'
00854          IF CK-AGENT-LEN (WS-SUB1) GREATER THAN ZERO
00855              IF CK-AGENT (WS-SUB1) = SPACES OR LOW-VALUES
00856                  MOVE '*'        TO  WS-SPACE-AGENT-SW
00857                  PERFORM 1480-VERIFY-GA-ONLY  THRU  1490-EXIT
00858                  GO TO 1040-STUB-EDIT-COMPLETE
00859              ELSE
00860                  PERFORM 1400-VERIFY-AGENT  THRU  1470-EXIT
00861                  GO TO 1040-STUB-EDIT-COMPLETE
00862          ELSE
00863              IF CK-AGOCD-LEN (WS-SUB1) GREATER THAN ZERO
00864                  MOVE PI-STUB-ACCT (WS-SUB1)
00865                                  TO  CK-AGENT (WS-SUB1)
00866                  IF PI-STUB-ACCT (WS-SUB1) = SPACES OR LOW-VALUES
00867                      MOVE '*'    TO  WS-SPACE-AGENT-SW
00868                      PERFORM 1480-VERIFY-GA-ONLY  THRU  1490-EXIT
00869                      GO TO 1040-STUB-EDIT-COMPLETE
00870                  ELSE
00871                      PERFORM 1400-VERIFY-AGENT  THRU  1470-EXIT
00872                      GO TO 1040-STUB-EDIT-COMPLETE
00873      ELSE
00874          GO TO 1040-STUB-EDIT-COMPLETE.
00875
00876  1040-STUB-EDIT-COMPLETE.
00877
00878      IF CK-MAINT (WS-SUB1) = 'J'
00879          MOVE 'I'               TO CK-MAINT (WS-SUB1).
00880
00881      IF CK-MAINT (WS-SUB1) = 'S'
00882          MOVE 'R'               TO CK-MAINT (WS-SUB1).
00883
00884      GO TO 1025-EDIT-STUB-INFO.
00885
00886  1050-EDIT-COMPLETE.
00887
00888      IF CSRL GREATER THAN ZERO
00889          MOVE CSRI              TO PI-ERCKWK-CSR
00890      ELSE
00891          MOVE HOLD-CSR          TO PI-ERCKWK-CSR
00892                                    CSRI.
00893
00894      IF EMI-FATAL-CTR NOT = ZEROS
00895          GO TO 8200-SEND-DATAONLY.
00896
00897      MOVE PI-COMPANY-CD          TO PI-ERCKWK-COMP-CD.
00898      MOVE CARI                   TO PI-ERCKWK-CARRIER.
00899      MOVE GROUPI                 TO PI-ERCKWK-GROUPING.
00900      MOVE PAYEEI                 TO PI-ERCKWK-PAYEE.
00901      MOVE WS-PAY-SEQ-NUM         TO PI-ERCKWK-PAYEE-SEQ.
00902
00903      IF MAINTI = 'A'
00904         GO TO 2000-ADD-PAYEE.
00905
00906      IF MAINTI = 'C'
00907         GO TO 3000-CHANGE-PAYEE.
00908
00909      IF MAINTI = 'D'
00910         GO TO 4000-DELETE-PAYEE.
00911
00912      IF MAINTI = 'S'
00913         MOVE +0                  TO PI-ERCKWK-SEQ-NO
00914         GO TO 5000-DISPLAY-PAYEE.
00915
00916      IF MAINTI = 'V'
00917         GO TO 7500-VOID-COMM-CHECK.
00918
00919      EJECT
00920
00921 ******************************************************************
00922 *                                                                *
00923 *      V E R I F Y   A G E N T  IS ON   C O M P.   M A S T E R   *
00924 *                                                                *
00925 ******************************************************************
00926
00927  1400-VERIFY-AGENT.
00928
00929      MOVE PI-COMPANY-CD          TO ERCOMP-COMP-CD.
00930      MOVE CARI                   TO ERCOMP-CARRIER.
00931      MOVE GROUPI                 TO ERCOMP-GROUPING.
00932      MOVE PAYEEI                 TO ERCOMP-FIN-RESP.
00933
00934      IF CK-AGOCD (WS-SUB1) = 'O'
00935          GO TO 1430-SECOND-READ.
00936
00937      MOVE CK-AGENT (WS-SUB1)     TO ERCOMP-ACCT.
00938      MOVE 'A'                    TO ERCOMP-RECORD-TYPE.
00939
00940  1420-FIRST-READ.
00941
00942      
      * EXEC CICS HANDLE CONDITION
00943 *        NOTFND   (1420-FIRST-NOTFND)
00944 *        END-EXEC.
      *    MOVE '"$I                   ! # #00004305' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303034333035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00945
00946      
      * EXEC CICS READ
00947 *         DATASET   (FILE-ID-ERCOMP)
00948 *         SET       (ADDRESS OF COMPENSATION-MASTER)
00949 *         RIDFLD    (ERCOMP-KEY)
00950 *    END-EXEC.
      *    MOVE '&"S        E          (   #00004309' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCOMP, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00951
00952      IF   CO-CSR-CODE  =  SPACES
00953          MOVE LOW-VALUES         TO CO-CSR-CODE.
00954
00955      MOVE CO-CSR-CODE            TO HOLD-CSR.
00956
00957      IF CK-AGOCD (WS-SUB1) = 'A'
00958          GO TO 1470-EXIT
00959      ELSE
00960          GO TO 1430-SECOND-READ.
00961
00962  1420-FIRST-NOTFND.
00963
00964      MOVE -1                     TO PAYEEL
00965      MOVE ER-2869                TO EMI-ERROR
00966      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00967      GO TO 1470-EXIT.
00968
00969  1430-SECOND-READ.
00970
00971      
      * EXEC CICS HANDLE CONDITION
00972 *        NOTFND   (1440-SECOND-NOTFND)
00973 *        END-EXEC.
      *    MOVE '"$I                   ! $ #00004334' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303034333334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00974
00975      MOVE LOW-VALUES             TO ERCOMP-ACCT.
00976      MOVE 'G'                    TO ERCOMP-RECORD-TYPE.
00977
00978      
      * EXEC CICS READ
00979 *         DATASET   (FILE-ID-ERCOMP)
00980 *         SET       (ADDRESS OF COMPENSATION-MASTER)
00981 *         RIDFLD    (ERCOMP-KEY)
00982 *    END-EXEC.
      *    MOVE '&"S        E          (   #00004341' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCOMP, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00983      IF   CO-CSR-CODE  =  SPACES
00984          MOVE LOW-VALUES         TO CO-CSR-CODE.
00985
00986      MOVE CO-CSR-CODE            TO HOLD-CSR.
00987      GO TO 1470-EXIT.
00988
00989  1440-SECOND-NOTFND.
00990
00991      MOVE -1                     TO PAYEEL.
00992      MOVE ER-7043                TO EMI-ERROR.
00993      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00994      GO TO 1470-EXIT.
00995 ******************************************************************
00996 *      V E R I F Y   C O M P.   M A S T E R    IS ACH            *
00997 ******************************************************************
00998 *                                                                *
00999  1450-ACH-READ.
01000
01001      
      * EXEC CICS HANDLE CONDITION
01002 *        NOTFND   (1440-SECOND-NOTFND)
01003 *        END-EXEC.
      *    MOVE '"$I                   ! % #00004364' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303034333634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01004
01005      
      * EXEC CICS READ
01006 *         DATASET   (FILE-ID-ERCOMP)
01007 *         SET       (ADDRESS OF COMPENSATION-MASTER)
01008 *         RIDFLD    (ERCOMP-KEY)
01009 *    END-EXEC.
      *    MOVE '&"S        E          (   #00004368' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCOMP, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01010
01011 *COMPENSATION MASTER IS ACH PROCESSING WHEN CO-ACH-STATUS = 'A'
01012      MOVE SPACE                  TO ERCOMP-SW.
01013      IF   CO-ACH-STATUS    = 'A'
01014          MOVE 'P'                TO ERCOMP-SW.
01015
01016      GO TO 1470-EXIT.
01017  1470-EXIT.
01018      EXIT.
01019      EJECT
01020
01021 ******************************************************************
01022 *                                                                *
01023 *      V E R I F Y   G A   ONLY IS ON   C O M P.   M A S T E R   *
01024 *                                                                *
01025 ******************************************************************
01026  1480-VERIFY-GA-ONLY.
01027
01028      IF WS-SPACE-AGENT-SW   =  '*'
01029          NEXT SENTENCE
01030      ELSE
01031          IF CK-AGENT (WS-SUB1)  =  SPACES OR LOW-VALUES
01032              NEXT SENTENCE
01033          ELSE
01034              GO TO 1489-CLEAR.
01035
01036      IF CK-MAINT (WS-SUB1)  =  'N'
01037          GO TO 1489-CLEAR.
01038
01039      MOVE PI-COMPANY-CD          TO ERCOMP-COMP-CD.
01040      MOVE CARI                   TO ERCOMP-CARRIER.
01041      MOVE GROUPI                 TO ERCOMP-GROUPING.
01042      MOVE PAYEEI                 TO ERCOMP-FIN-RESP.
01043      MOVE LOW-VALUES             TO ERCOMP-ACCT.
01044      MOVE 'G'                    TO ERCOMP-RECORD-TYPE.
01045
01046      
      * EXEC CICS HANDLE CONDITION
01047 *        NOTFND   (1485-NO-COMP-MSTR)
01048 *        END-EXEC.
      *    MOVE '"$I                   ! & #00004409' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303034343039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01049
01050      
      * EXEC CICS READ
01051 *         DATASET   (FILE-ID-ERCOMP)
01052 *         SET       (ADDRESS OF COMPENSATION-MASTER)
01053 *         RIDFLD    (ERCOMP-KEY)
01054 *    END-EXEC.
      *    MOVE '&"S        E          (   #00004413' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCOMP, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01055
01056      IF CO-AR-BAL-LEVEL  =  '4'
01057         IF CK-INVREF-LEN (WS-SUB1)  GREATER THAN ZEROS
01058             MOVE -1             TO CK-AGENT-LEN (WS-SUB1)
01059             MOVE ER-3164        TO EMI-ERROR
01060             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01061             GO TO 1489-CLEAR
01062         ELSE
01063             GO TO 1489-CLEAR
01064      ELSE
01065          IF CO-AR-BAL-LEVEL  =  '3'
01066              MOVE -1             TO CK-AGENT-LEN (WS-SUB1)
01067              MOVE ER-7082        TO EMI-ERROR
01068              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01069              GO TO 1489-CLEAR
01070             IF CK-INVREF-LEN (WS-SUB1)  GREATER THAN ZEROS
01071                 MOVE -1          TO CK-AGENT-LEN (WS-SUB1)
01072                 MOVE ER-3164     TO EMI-ERROR
01073                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01074                 GO TO 1489-CLEAR
01075             ELSE
01076                 GO TO 1489-CLEAR
01077          ELSE
01078              IF CO-AR-BAL-LEVEL  =  '1'  OR '2'
01079                  MOVE -1         TO CK-AGENT-LEN (WS-SUB1)
01080                  MOVE ER-7082    TO EMI-ERROR
01081                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01082                  GO TO 1489-CLEAR
01083              ELSE
01084                  GO TO 1489-CLEAR.
01085
01086  1485-NO-COMP-MSTR.
01087
01088      MOVE -1                     TO PAYEEL.
01089      MOVE ER-3179                TO EMI-ERROR.
01090      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01091
01092  1489-CLEAR.
01093      MOVE  SPACES                TO  WS-SPACE-AGENT-SW.
01094
01095  1490-EXIT.
01096      EXIT.
01097
01098
01099      EJECT
01100
01101 ******************************************************************
01102 *                                                                *
01103 *          V E R I F Y   L E D G E R   N U M B E R               *
01104 *                                                                *
01105 ******************************************************************
01106
01107  1500-VERIFY-LEDGER.
01108  1590-EXIT.
01109       EXIT.
01110
01111     EJECT
01112
01113  1600-READ-CNTL-FILE.
01114 *****  NCL REQUIRES A CSR CODE WHICH IS VALIDATED TO THE  *****
01115 *****  USER IDENTIFICATION FILE. CSR CODES CAN BE WITHOUT *****
01116 *****  VALIDATION.                                        *****
01117
01118      IF   PI-COMPANY-ID  =  'NCL'
01119          NEXT SENTENCE
01120      ELSE
01121          GO TO 1690-EXIT.
01122
01123      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
01124      MOVE '2'                    TO  CNTL-REC-TYPE.
01125      MOVE CSRI                   TO  CNTL-ACCESS.
01126      MOVE ZEROS                  TO  CNTL-SEQ-NO.
01127
01128      
      * EXEC CICS HANDLE CONDITION
01129 *        NOTFND  (1680-NOT-FOUND)
01130 *        ERROR   (9990-ABEND)
01131 *    END-EXEC.
      *    MOVE '"$I.                  ! '' #00004491' TO DFHEIV0
           MOVE X'2224492E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303034343931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01132
01133      
      * EXEC CICS READ
01134 *        DATASET  (CNTL-FILE-ID)
01135 *        SET      (ADDRESS OF CONTROL-FILE)
01136 *        RIDFLD   (ELCNTL-KEY)
01137 *    END-EXEC.
      *    MOVE '&"S        E          (   #00004496' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343936' TO DFHEIV0(25:11)
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
           
01138
01139      GO TO 1690-EXIT.
01140
01141  1680-NOT-FOUND.
01142      MOVE ER-1883                TO EMI-ERROR.
01143      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01144      MOVE -1                     TO CSRL.
01145      MOVE AL-UABON               TO CSRA.
01146
01147  1690-EXIT.
01148       EXIT.
01149
01150     EJECT
01151 ******************************************************************
01152 *                                                                *
01153 *               A D D   P A Y E E                                *
01154 *                                                                *
01155 ******************************************************************
01156
01157  2000-ADD-PAYEE.
01158
01159      
      * EXEC CICS GETMAIN
01160 *        SET     (ADDRESS OF CHECK-WORK-RECORDS)
01161 *        LENGTH  (ERCKWK-RECORD-LENGTH)
01162 *        INITIMG (GETMAIN-SPACE)
01163 *    END-EXEC.
      *    MOVE ',"IL                  $   #00004522' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERCKWK-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF CHECK-WORK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01164
01165      IF PI-ERCKWK-PAYEE-SEQ = +1
01166          ADD +1 TO PI-ERCKWK-PAYEE-SEQ
01167      ELSE
01168          MOVE +2 TO PI-ERCKWK-PAYEE-SEQ.
01169
01170  2010-READ-FOR-UPDATE.
01171      MOVE PI-COMPANY-CD          TO ERCKWK-COMPANY-CD.
01172      MOVE PI-ERCKWK-CSR          TO ERCKWK-CSR.
01173      MOVE PI-ERCKWK-CARRIER      TO ERCKWK-CARRIER.
01174      MOVE PI-ERCKWK-GROUPING     TO ERCKWK-GROUPING.
01175      MOVE PI-ERCKWK-PAYEE        TO ERCKWK-PAYEE.
01176      MOVE PI-ERCKWK-PAYEE-SEQ    TO ERCKWK-PAYEE-SEQ.
01177      MOVE ZEROS                  TO ERCKWK-SEQUENCE-NO.
01178
01179  2025-ADD-HEADER.
01180
01181      MOVE 'CW'                   TO CW-RECORD-ID.
01182      MOVE ERCKWK-KEY             TO CW-CONTROL-PRIMARY.
01183      MOVE '0'                    TO CW-RECORD-TYPE.
01184      MOVE WS-CURRENT-BIN-DT      TO CW-RECORDED-DT.
01185      MOVE PI-PROCESSOR-ID        TO CW-RECORDED-BY.
01186      MOVE EIBTIME                TO CW-LAST-MAINT-HHMMSS.
01187
01188      IF PAYTOL  GREATER THAN ZEROS
01189         MOVE PAYTOI              TO CW-PAYEE-NAME.
01190
01191      IF ADDRS1L GREATER THAN ZEROS
01192         MOVE ADDRS1I             TO CW-ADDRESS-1.
01193
01194      IF ADDRS2L GREATER THAN ZEROS
01195         MOVE ADDRS2I             TO CW-ADDRESS-2.
01196
01197      IF CITYSTL GREATER THAN ZEROS
01198         MOVE CITYSTI             TO CW-PAYEE-CITY-ST.
01199
01200      IF ZIPL    GREATER THAN ZEROS
01201         MOVE ZIPI                TO CW-PAYEE-ZIP.
01202
01203      IF ZIPEXTL GREATER THAN ZEROS
01204         MOVE ZIPEXTI             TO CW-PAYEE-ZIP-EXT.
01205
01206      MOVE +0                     TO CW-TOTAL-COMMISSION
01207                                     CW-TOTAL-ENTRIES.
01208
01209      MOVE LOW-VALUES             TO CW-CREDIT-SELECT-DT
01210                                     CW-CREDIT-ACCEPT-DT
01211                                     CW-RELEASE-DT.
01212
01213  2030-WRITE-HEADER.
01214      
      * EXEC CICS HANDLE CONDITION
01215 *        DUPREC    (2060-DUPLICATE-CHECK)
01216 *    END-EXEC.
      *    MOVE '"$%                   ! ( #00004577' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303034353737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01217
01218      
      * EXEC CICS WRITE
01219 *         FROM    (CHECK-WORK-RECORDS)
01220 *         DATASET (FILE-ID-ERCKWK)
01221 *         RIDFLD  (ERCKWK-KEY)
01222 *    END-EXEC.
           MOVE LENGTH OF
            CHECK-WORK-RECORDS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004581' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 CHECK-WORK-RECORDS, 
                 DFHEIV11, 
                 ERCKWK-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01223
01224      MOVE +0                     TO WS-AMOUNT-PAID
01225                                     WS-SEQUENCE-NO
01226                                     WS-TOTAL-ENTRIES.
01227
01228      MOVE +0                     TO WS-SUB1.
01229
01230  2050-ADD-DETAIL-ENTRIES.
01231
01232      ADD +1                      TO WS-SUB1.
01233
01234      IF WS-SUB1 GREATER THAN +5
01235         GO TO 2090-NEW-PAYEE-PROCESSED.
01236
01237      IF CK-MAINT-LEN (WS-SUB1) NOT GREATER THAN +0
01238         GO TO 2050-ADD-DETAIL-ENTRIES.
01239
01240      MOVE SPACES                 TO CHECK-WORK-RECORDS.
01241      MOVE 'CW'                   TO CW-RECORD-ID.
01242      MOVE ERCKWK-KEY             TO CW-CONTROL-PRIMARY.
01243      ADD  +1                     TO WS-SEQUENCE-NO.
01244      MOVE WS-SEQUENCE-NO         TO CW-SEQUENCE-NO.
01245      MOVE CW-SEQUENCE-NO         TO ERCKWK-SEQUENCE-NO.
01246      MOVE '1'                    TO CW-RECORD-TYPE.
01247      MOVE WS-CURRENT-BIN-DT      TO CW-RECORDED-DT.
01248      MOVE PI-PROCESSOR-ID        TO CW-RECORDED-BY.
01249      MOVE EIBTIME                TO CW-LAST-MAINT-HHMMSS.
01250
01251      MOVE LOW-VALUES             TO CW-CREDIT-SELECT-DT
01252                                     CW-CREDIT-ACCEPT-DT
01253                                     CW-RELEASE-DT.
01254
01255      MOVE CK-MAINT (WS-SUB1)     TO CW-LAST-MAINT-APPLIED.
01256
01257      IF CK-COMNTS-LEN  (WS-SUB1) GREATER THAN ZEROS
01258         MOVE CK-COMNTS (WS-SUB1) TO CW-COMMENT.
01259
01260      IF CK-AGENT-LEN   (WS-SUB1) GREATER THAN ZEROS
01261         MOVE CK-AGENT  (WS-SUB1) TO CW-ACCT-AGENT.
01262
01263      IF CK-INVREF-LEN  (WS-SUB1) GREATER THAN ZEROS
01264         IF CK-MAINT    (WS-SUB1) = 'R'
01265            MOVE CK-INVREF (WS-SUB1) TO CW-REFERENCE.
01266
01267      IF CK-INVREF-LEN  (WS-SUB1) GREATER THAN ZEROS
01268         IF CK-MAINT    (WS-SUB1) = 'I'
01269            MOVE CK-INVREF (WS-SUB1) TO CW-INVOICE.
01270
01271      IF CK-LEDGER-LEN  (WS-SUB1) GREATER THAN ZEROS
01272         MOVE CK-LEDGER (WS-SUB1) TO CW-LEDGER-NO.
01273
01274      IF CK-AGOCD-LEN   (WS-SUB1) GREATER THAN ZEROS
01275         MOVE CK-AGOCD  (WS-SUB1) TO CW-PMT-APPLIED.
01276
01277      MOVE 'C'                    TO CW-PAYMENT-TYPE.
01278
01279      IF CK-DETAMT-LEN  (WS-SUB1) GREATER THAN ZEROS
01280         MOVE WS-DETAMT (WS-SUB1) TO CW-DETAIL-AMOUNT
01281      ELSE
01282         MOVE ZEROS               TO CW-DETAIL-AMOUNT.
01283
01284      ADD +1                      TO WS-TOTAL-ENTRIES.
01285
01286      IF CK-MAINT (WS-SUB1) NOT EQUAL 'H'
01287          ADD WS-DETAMT (WS-SUB1) TO WS-AMOUNT-PAID.
01288
01289      IF CW-NON-AR-ITEM EQUAL 'Y'
01290          NEXT SENTENCE
01291      ELSE
01292         IF CK-MAINT (WS-SUB1) = 'N'
01293             MOVE 'Y'             TO CW-NON-AR-ITEM
01294             MOVE 'N'             TO CW-PMT-APPLIED
01295         ELSE
01296             MOVE 'N'             TO CW-NON-AR-ITEM.
01297
01298      
      * EXEC CICS HANDLE CONDITION
01299 *         DUPREC  (2080-DUPLICATE-CHECK)
01300 *    END-EXEC.
      *    MOVE '"$%                   ! ) #00004661' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303034363631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01301
01302      
      * EXEC CICS WRITE
01303 *         FROM    (CHECK-WORK-RECORDS)
01304 *         DATASET (FILE-ID-ERCKWK)
01305 *         RIDFLD  (ERCKWK-KEY)
01306 *    END-EXEC.
           MOVE LENGTH OF
            CHECK-WORK-RECORDS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004665' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 CHECK-WORK-RECORDS, 
                 DFHEIV11, 
                 ERCKWK-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01307
01308      MOVE ERCKWK-KEY             TO  PI-STUB-KEY     (WS-SUB1).
01309      MOVE 'A'                    TO  PI-PREV-FUNCTION.
01310
01311      GO TO 2050-ADD-DETAIL-ENTRIES.
01312
01313  2060-DUPLICATE-CHECK.
01314      ADD +1 TO PI-ERCKWK-PAYEE-SEQ.
01315      MOVE PI-ERCKWK-PAYEE-SEQ    TO ERCKWK-PAYEE-SEQ.
01316      MOVE ERCKWK-KEY             TO CW-CONTROL-PRIMARY.
01317      GO TO 2030-WRITE-HEADER.
01318
01319  2080-DUPLICATE-CHECK.
01320
01321      
      * EXEC CICS SYNCPOINT ROLLBACK
01322 *    END-EXEC.
      *    MOVE '6"R                   !   #00004684' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01323
01324      MOVE SPACE                  TO PI-MAINT-FUNCTION.
01325      MOVE ER-3159                TO EMI-ERROR.
01326      MOVE -1                     TO CARL.
01327      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01328      GO TO 8200-SEND-DATAONLY.
01329
01330  2090-NEW-PAYEE-PROCESSED.
01331
01332      MOVE ZEROS                  TO ERCKWK-SEQUENCE-NO
01333                                     PI-ERCKWK-SEQ-NO.
01334
01335      
      * EXEC CICS HANDLE CONDITION
01336 *        NOTFND    (2095-HEADER-ROLLBACK)
01337 *    END-EXEC.
      *    MOVE '"$I                   ! * #00004698' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303034363938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01338
01339      
      * EXEC CICS READ
01340 *        SET     (ADDRESS OF CHECK-WORK-RECORDS)
01341 *        DATASET (FILE-ID-ERCKWK)
01342 *        RIDFLD  (ERCKWK-KEY)
01343 *        UPDATE
01344 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00004702' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373032' TO DFHEIV0(25:11)
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
           
01345
01346      MOVE WS-AMOUNT-PAID          TO   CW-TOTAL-COMMISSION.
01347      MOVE WS-TOTAL-ENTRIES        TO   CW-TOTAL-ENTRIES.
01348
01349      IF CW-TOTAL-COMMISSION = ZERO
01350          GO TO 2095-HEADER-ROLLBACK.
01351
01352      
      * EXEC CICS REWRITE
01353 *         FROM    (CHECK-WORK-RECORDS)
01354 *         DATASET (FILE-ID-ERCKWK)
01355 *    END-EXEC.
           MOVE LENGTH OF
            CHECK-WORK-RECORDS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004715' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 CHECK-WORK-RECORDS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01356
01357      IF EMI-WARNING-CTR = ZEROS
01358          MOVE ER-0000                TO  EMI-ERROR
01359          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01360
01361      GO TO 5000-DISPLAY-PAYEE.
01362
01363  2095-HEADER-ROLLBACK.
01364
01365      
      * EXEC CICS SYNCPOINT ROLLBACK
01366 *    END-EXEC.
      *    MOVE '6"R                   !   #00004728' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01367
01368      MOVE SPACE                  TO PI-MAINT-FUNCTION.
01369      MOVE ER-3160                TO EMI-ERROR.
01370      MOVE -1                     TO CARL.
01371      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01372      GO TO 8200-SEND-DATAONLY.
01373
01374
01375      EJECT
01376
01377 ******************************************************************
01378 *                                                                *
01379 *              C H A N G E   P A Y E E                           *
01380 *                                                                *
01381 ******************************************************************
01382
01383  3000-CHANGE-PAYEE.
01384
01385      MOVE 'C'                    TO PI-MAINT-FUNCTION.
01386      MOVE +0                     TO WS-SUB1
01387                                     WS-DELETE-COUNT
01388                                     WS-DELETE-AMOUNT
01389                                     WS-DELETE-SEQ-NO
01390                                     WS-HOLD-COUNT
01391                                     WS-HOLD-AMOUNT
01392                                     WS-OFF-HOLD-COUNT
01393                                     WS-OFF-HOLD-AMOUNT.
01394      MOVE SPACE                  TO WS-UPDATE-SW.
01395
01396  3010-CHANGE-ENTRIES.
01397
01398      ADD +1                      TO WS-SUB1.
01399
01400      IF WS-SUB1 GREATER THAN +5
01401         IF  WS-DELETE-COUNT GREATER THAN +0
01402             PERFORM 3400-RESEQ-SLUG-LINES THRU 3499-EXIT
01403             GO TO 3050-UPDATE-HEADER
01404         ELSE
01405             GO TO 3050-UPDATE-HEADER.
01406
01407      IF CK-MAINT-LEN    (WS-SUB1) GREATER THAN ZEROS OR
01408         CK-COMNTS-LEN   (WS-SUB1) GREATER THAN ZEROS OR
01409         CK-AGENT-LEN    (WS-SUB1) GREATER THAN ZEROS OR
01410         CK-INVREF-LEN   (WS-SUB1) GREATER THAN ZEROS OR
01411         CK-LEDGER-LEN   (WS-SUB1) GREATER THAN ZEROS OR
01412         CK-AGOCD-LEN    (WS-SUB1) GREATER THAN ZEROS OR
01413         CK-DETAMT-LEN   (WS-SUB1) GREATER THAN ZEROS
01414         NEXT SENTENCE
01415      ELSE
01416         GO TO 3010-CHANGE-ENTRIES.
01417
01418      IF CK-MAINT-LEN (WS-SUB1) NOT GREATER THAN ZEROS
01419         GO TO 3010-CHANGE-ENTRIES.
01420
01421      IF CK-MAINT (WS-SUB1) = 'A' OR 'N'
01422         PERFORM 3200-ADD-DETAIL-ENTRY THRU 3290-EXIT
01423         GO TO 3010-CHANGE-ENTRIES.
01424
01425      IF CK-MAINT (WS-SUB1) = 'R' OR 'I'
01426         IF WS-SUB1 GREATER THAN PI-NO-ENTRIES-DISPLAYED
01427            PERFORM 3200-ADD-DETAIL-ENTRY THRU 3290-EXIT
01428            GO TO 3010-CHANGE-ENTRIES.
01429
01430      IF WS-SUB1 GREATER THAN PI-NO-ENTRIES-DISPLAYED
01431         GO TO 3010-CHANGE-ENTRIES.
01432
01433      MOVE PI-STUB-KEY (WS-SUB1)  TO ERCKWK-KEY.
01434
01435      
      * EXEC CICS HANDLE CONDITION
01436 *        NOTFND    (3010-CHANGE-ENTRIES)
01437 *    END-EXEC.
      *    MOVE '"$I                   ! + #00004798' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303034373938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01438
01439      
      * EXEC CICS READ
01440 *        SET     (ADDRESS OF CHECK-WORK-RECORDS)
01441 *        DATASET (FILE-ID-ERCKWK)
01442 *        RIDFLD  (ERCKWK-KEY)
01443 *        UPDATE
01444 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00004802' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383032' TO DFHEIV0(25:11)
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
           
01445
01446      IF CW-RELEASE-DT GREATER THAN LOW-VALUES
01447          GO TO 3085-RELEASE-ERROR.
01448
01449      IF CK-MAINT  (WS-SUB1) = 'D'
01450         MOVE 'Y'                 TO WS-UPDATE-SW
01451         PERFORM 3100-DELETE-ENTRY THRU 3190-EXIT
01452         GO TO 3010-CHANGE-ENTRIES.
01453
01454      IF CK-MAINT (WS-SUB1) = 'H'
01455         IF CW-LAST-MAINT-APPLIED EQUAL 'H'
01456             NEXT SENTENCE
01457         ELSE
01458             MOVE 'Y'                 TO WS-UPDATE-SW
01459             ADD CW-DETAIL-AMOUNT     TO WS-HOLD-AMOUNT
01460             ADD +1                   TO WS-HOLD-COUNT
01461      ELSE
01462         IF CW-LAST-MAINT-APPLIED EQUAL 'H'
01463             MOVE 'Y'                 TO WS-UPDATE-SW
01464             ADD CW-DETAIL-AMOUNT     TO WS-OFF-HOLD-AMOUNT
01465             ADD +1                   TO WS-OFF-HOLD-COUNT.
01466
01467      IF CK-MAINT (WS-SUB1) = 'C' AND
01468         CW-LAST-MAINT-APPLIED = 'N'
01469          MOVE -1              TO CK-MAINT-LEN (WS-SUB1)
01470          MOVE ER-3198         TO EMI-ERROR
01471          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01472          GO TO 8200-SEND-DATAONLY.
01473
01474      MOVE CK-MAINT (WS-SUB1)     TO CW-LAST-MAINT-APPLIED.
01475
01476      IF (CK-COMNTS-LEN  (WS-SUB1) GREATER THAN ZEROS)
01477         MOVE 'Y'                 TO WS-UPDATE-SW
01478         MOVE CK-COMNTS (WS-SUB1) TO CW-COMMENT.
01479
01480      IF CK-AGENT-LEN   (WS-SUB1) GREATER THAN ZEROS
01481         MOVE 'Y'                 TO WS-UPDATE-SW
01482         MOVE CK-AGENT  (WS-SUB1) TO CW-ACCT-AGENT.
01483
01484      IF CK-INVREF-LEN  (WS-SUB1) GREATER THAN ZEROS
01485         IF CK-MAINT    (WS-SUB1) = 'R'
01486            MOVE CK-INVREF (WS-SUB1) TO CW-REFERENCE.
01487
01488      IF CK-INVREF-LEN  (WS-SUB1) GREATER THAN ZEROS
01489         IF CK-MAINT    (WS-SUB1) = 'I'
01490            MOVE CK-INVREF (WS-SUB1) TO CW-INVOICE.
01491
01492      IF CK-LEDGER-LEN  (WS-SUB1) GREATER THAN ZEROS
01493         MOVE CK-LEDGER (WS-SUB1) TO CW-LEDGER-NO.
01494
01495      IF CK-AGOCD-LEN   (WS-SUB1) GREATER THAN ZEROS
01496         MOVE CK-AGOCD  (WS-SUB1) TO CW-PMT-APPLIED.
01497
01498      MOVE 'C'                    TO CW-PAYMENT-TYPE.
01499
01500      IF CK-DETAMT-LEN  (WS-SUB1) GREATER THAN ZEROS
01501         MOVE CW-DETAIL-AMOUNT    TO WS-DETAIL-AMOUNT
01502         MOVE WS-DETAMT (WS-SUB1) TO CW-DETAIL-AMOUNT.
01503
01504      
      * EXEC CICS REWRITE
01505 *         FROM    (CHECK-WORK-RECORDS)
01506 *         DATASET (FILE-ID-ERCKWK)
01507 *    END-EXEC.
           MOVE LENGTH OF
            CHECK-WORK-RECORDS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004867' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 CHECK-WORK-RECORDS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01508
01509      IF CK-DETAMT-LEN  (WS-SUB1) GREATER THAN ZEROS
01510         PERFORM 3300-UPDATE-TOTAL-COMMISSION THRU 3390-EXIT.
01511
01512      GO TO 3010-CHANGE-ENTRIES.
01513
01514  3050-UPDATE-HEADER.
01515
01516      IF PAYTOL          GREATER THAN ZEROS OR
01517         ADDRS1L         GREATER THAN ZEROS OR
01518         ADDRS2L         GREATER THAN ZEROS OR
01519         CITYSTL         GREATER THAN ZEROS OR
01520         ZIPL            GREATER THAN ZEROS OR
01521         ZIPEXTL         GREATER THAN ZEROS OR
01522         WS-ENTRIES-UPDATED
01523         NEXT SENTENCE
01524      ELSE
01525         GO TO 3080-CHANGES-PROCESSED.
01526
01527      MOVE PI-COMPANY-CD          TO ERCKWK-COMPANY-CD.
01528      MOVE PI-ERCKWK-CSR          TO ERCKWK-CSR.
01529      MOVE PI-ERCKWK-CARRIER      TO ERCKWK-CARRIER.
01530      MOVE PI-ERCKWK-GROUPING     TO ERCKWK-GROUPING.
01531      MOVE PI-ERCKWK-PAYEE        TO ERCKWK-PAYEE.
01532      MOVE PI-ERCKWK-PAYEE-SEQ    TO ERCKWK-PAYEE-SEQ.
01533      MOVE ZEROS                  TO ERCKWK-SEQUENCE-NO.
01534
01535      
      * EXEC CICS HANDLE CONDITION
01536 *        NOTFND    (3090-CHECKS-NOTFND)
01537 *        ENDFILE   (3090-CHECKS-NOTFND)
01538 *    END-EXEC.
      *    MOVE '"$I''                  ! , #00004898' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303034383938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01539
01540      
      * EXEC CICS READ
01541 *        SET     (ADDRESS OF CHECK-WORK-RECORDS)
01542 *        DATASET (FILE-ID-ERCKWK)
01543 *        RIDFLD  (ERCKWK-KEY)
01544 *        UPDATE
01545 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00004903' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393033' TO DFHEIV0(25:11)
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
           
01546
01547      IF CW-RELEASE-DT GREATER THAN LOW-VALUES
01548          GO TO 3085-RELEASE-ERROR.
01549
01550      IF PAYTOL  GREATER THAN ZEROS
01551         MOVE PAYTOI              TO CW-PAYEE-NAME.
01552
01553      IF ADDRS1L GREATER THAN ZEROS
01554         MOVE ADDRS1I             TO CW-ADDRESS-1.
01555
01556      IF ADDRS2L GREATER THAN ZEROS
01557         MOVE ADDRS2I             TO CW-ADDRESS-2.
01558
01559      IF CITYSTL GREATER THAN ZEROS
01560         MOVE CITYSTI             TO CW-PAYEE-CITY-ST.
01561
01562      IF ZIPL    GREATER THAN ZEROS
01563         MOVE ZIPI                TO CW-PAYEE-ZIP.
01564
01565      IF ZIPEXTL GREATER THAN ZEROS
01566         MOVE ZIPEXTI             TO CW-PAYEE-ZIP-EXT.
01567
01568      IF WS-DELETE-COUNT GREATER THAN +0
01569          SUBTRACT WS-DELETE-AMOUNT    FROM CW-TOTAL-COMMISSION
01570          SUBTRACT WS-DELETE-COUNT     FROM CW-TOTAL-ENTRIES.
01571
01572      IF WS-HOLD-COUNT GREATER THAN +0
01573          SUBTRACT WS-HOLD-AMOUNT      FROM CW-TOTAL-COMMISSION.
01574
01575      IF WS-OFF-HOLD-COUNT GREATER THAN +0
01576          ADD WS-OFF-HOLD-AMOUNT  TO  CW-TOTAL-COMMISSION.
01577
01578
01579      MOVE WS-CURRENT-BIN-DT      TO  CW-RECORDED-DT.
01580      MOVE PI-PROCESSOR-ID        TO  CW-RECORDED-BY.
01581      MOVE EIBTIME                TO  CW-LAST-MAINT-HHMMSS.
01582
01583      
      * EXEC CICS REWRITE
01584 *         FROM    (CHECK-WORK-RECORDS)
01585 *         DATASET (FILE-ID-ERCKWK)
01586 *    END-EXEC.
           MOVE LENGTH OF
            CHECK-WORK-RECORDS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004946' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 CHECK-WORK-RECORDS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01587
01588  3080-CHANGES-PROCESSED.
01589
01590      IF PI-NO-ENTRIES-DISPLAYED GREATER THAN ZEROS
01591          MOVE PI-STUB-KEY (1)        TO PI-ERCKWK-KEY
01592          IF WS-DELETE-COUNT  GREATER THAN +0   AND
01593             WS-DELETE-COUNT  EQUAL PI-NO-ENTRIES-DISPLAYED
01594             IF  CW-TOTAL-ENTRIES GREATER THAN +5 AND
01595                 PI-ERCKWK-SEQ-NO GREATER THAN +5
01596                 SUBTRACT +5 FROM PI-ERCKWK-SEQ-NO
01597             ELSE
01598                 MOVE ZEROS TO    PI-ERCKWK-SEQ-NO.
01599
01600      IF EMI-WARNING-CTR = ZEROS
01601          MOVE ER-0000                TO EMI-ERROR
01602          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01603
01604      GO TO 5000-DISPLAY-PAYEE.
01605
01606  3085-RELEASE-ERROR.
01607
01608      
      * EXEC CICS UNLOCK
01609 *         DATASET (FILE-ID-ERCKWK)
01610 *    END-EXEC.
      *    MOVE '&*                    #   #00004971' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01611
01612      MOVE ER-3139                TO EMI-ERROR.
01613      MOVE -1                     TO MAINTL.
01614      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01615      GO TO 8200-SEND-DATAONLY.
01616  3090-CHECKS-NOTFND.
01617
01618      
      * EXEC CICS SYNCPOINT ROLLBACK
01619 *    END-EXEC.
      *    MOVE '6"R                   !   #00004981' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01620
01621      MOVE SPACE                  TO PI-MAINT-FUNCTION.
01622      MOVE ER-3161                TO EMI-ERROR.
01623      MOVE -1                     TO CARL.
01624      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01625      GO TO 8200-SEND-DATAONLY.
01626
01627      EJECT
01628
01629 ******************************************************************
01630 *                                                                *
01631 *              D E L E T E   E N T R Y                           *
01632 *                                                                *
01633 ******************************************************************
01634
01635  3100-DELETE-ENTRY.
01636
01637      IF CW-LAST-MAINT-APPLIED NOT = 'H'
01638          ADD  CW-DETAIL-AMOUNT   TO WS-DELETE-AMOUNT.
01639
01640      ADD  +1                     TO WS-DELETE-COUNT.
01641
01642      IF  WS-DELETE-COUNT EQUAL +1
01643          MOVE ERCKWK-SEQUENCE-NO TO WS-DELETE-SEQ-NO.
01644
01645      
      * EXEC CICS DELETE
01646 *         DATASET   (FILE-ID-ERCKWK)
01647 *    END-EXEC.
      *    MOVE '&(                    &   #00005008' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01648
01649  3190-EXIT.
01650      EXIT.
01651
01652      EJECT
01653
01654 ******************************************************************
01655 *                                                                *
01656 *           A D D   D E T A I L   E N T R Y                      *
01657 *                                                                *
01658 ******************************************************************
01659
01660  3200-ADD-DETAIL-ENTRY.
01661
01662      MOVE PI-COMPANY-CD          TO ERCKWK-COMPANY-CD.
01663      MOVE PI-ERCKWK-CSR          TO ERCKWK-CSR.
01664      MOVE PI-ERCKWK-CARRIER      TO ERCKWK-CARRIER.
01665      MOVE PI-ERCKWK-GROUPING     TO ERCKWK-GROUPING.
01666      MOVE PI-ERCKWK-PAYEE        TO ERCKWK-PAYEE.
01667      MOVE PI-ERCKWK-PAYEE-SEQ    TO ERCKWK-PAYEE-SEQ.
01668      MOVE ZEROS                  TO ERCKWK-SEQUENCE-NO.
01669
01670      
      * EXEC CICS HANDLE CONDITION
01671 *        NOTFND    (3280-HEADER-NOTFND)
01672 *    END-EXEC.
      *    MOVE '"$I                   ! - #00005033' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303035303333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01673
01674      
      * EXEC CICS READ
01675 *        SET     (ADDRESS OF CHECK-WORK-RECORDS)
01676 *        DATASET (FILE-ID-ERCKWK)
01677 *        RIDFLD  (ERCKWK-KEY)
01678 *        UPDATE
01679 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005037' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303337' TO DFHEIV0(25:11)
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
           
01680
01681      IF CW-RELEASE-DT GREATER THAN LOW-VALUES
01682          GO TO 3285-RELEASE-ERROR.
01683
01684      ADD WS-DETAMT (WS-SUB1)     TO CW-TOTAL-COMMISSION.
01685      ADD +1                      TO CW-TOTAL-ENTRIES.
01686      MOVE CW-TOTAL-ENTRIES       TO WS-TOTAL-ENTRIES.
01687
01688      MOVE WS-CURRENT-BIN-DT      TO  CW-RECORDED-DT.
01689      MOVE PI-PROCESSOR-ID        TO  CW-RECORDED-BY.
01690      MOVE EIBTIME                TO  CW-LAST-MAINT-HHMMSS.
01691
01692      
      * EXEC CICS REWRITE
01693 *         FROM    (CHECK-WORK-RECORDS)
01694 *         DATASET (FILE-ID-ERCKWK)
01695 *    END-EXEC.
           MOVE LENGTH OF
            CHECK-WORK-RECORDS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005055' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 CHECK-WORK-RECORDS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01696
01697      
      * EXEC CICS GETMAIN
01698 *        SET     (ADDRESS OF CHECK-WORK-RECORDS)
01699 *        LENGTH  (ERCKWK-RECORD-LENGTH)
01700 *        INITIMG (GETMAIN-SPACE)
01701 *    END-EXEC.
      *    MOVE ',"IL                  $   #00005060' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERCKWK-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF CHECK-WORK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01702
01703      MOVE 'CW'                   TO CW-RECORD-ID.
01704      MOVE ERCKWK-KEY             TO CW-CONTROL-PRIMARY.
01705
01706      MOVE WS-TOTAL-ENTRIES       TO CW-SEQUENCE-NO
01707                                     ERCKWK-SEQUENCE-NO.
01708
01709      MOVE '1'                    TO CW-RECORD-TYPE.
01710      MOVE WS-CURRENT-BIN-DT      TO CW-RECORDED-DT.
01711      MOVE PI-PROCESSOR-ID        TO CW-RECORDED-BY.
01712      MOVE EIBTIME                TO CW-LAST-MAINT-HHMMSS.
01713
01714      MOVE CK-MAINT     (WS-SUB1) TO CW-LAST-MAINT-APPLIED.
01715
01716      IF CK-COMNTS-LEN  (WS-SUB1) GREATER THAN ZEROS
01717         MOVE CK-COMNTS (WS-SUB1) TO CW-COMMENT.
01718
01719      IF CK-AGENT-LEN   (WS-SUB1) GREATER THAN ZEROS
01720         MOVE CK-AGENT  (WS-SUB1) TO CW-ACCT-AGENT.
01721
01722      IF CK-INVREF-LEN  (WS-SUB1) GREATER THAN ZEROS
01723         IF CK-MAINT    (WS-SUB1) = 'R'
01724            MOVE CK-INVREF (WS-SUB1) TO CW-REFERENCE.
01725
01726      IF CK-INVREF-LEN  (WS-SUB1) GREATER THAN ZEROS
01727         IF CK-MAINT    (WS-SUB1) = 'I'
01728            MOVE CK-INVREF (WS-SUB1) TO CW-INVOICE.
01729
01730      IF CK-LEDGER-LEN  (WS-SUB1) GREATER THAN ZEROS
01731         MOVE CK-LEDGER (WS-SUB1) TO CW-LEDGER-NO.
01732
01733      IF CK-AGOCD-LEN   (WS-SUB1) GREATER THAN ZEROS
01734         MOVE CK-AGOCD  (WS-SUB1) TO CW-PMT-APPLIED.
01735
01736      MOVE 'C'                    TO CW-PAYMENT-TYPE.
01737
01738      IF CK-DETAMT-LEN  (WS-SUB1) GREATER THAN ZEROS
01739         MOVE WS-DETAMT (WS-SUB1) TO CW-DETAIL-AMOUNT.
01740
01741      MOVE LOW-VALUES             TO CW-CREDIT-SELECT-DT
01742                                     CW-CREDIT-ACCEPT-DT
01743                                     CW-RELEASE-DT.
01744
01745      IF CW-NON-AR-ITEM EQUAL 'Y'
01746          NEXT SENTENCE
01747      ELSE
01748         IF CK-MAINT (WS-SUB1) = 'N'
01749             MOVE 'Y'                TO CW-NON-AR-ITEM
01750             MOVE 'N'                TO CW-PMT-APPLIED
01751         ELSE
01752             MOVE 'N'                TO CW-NON-AR-ITEM.
01753
01754      
      * EXEC CICS HANDLE CONDITION
01755 *         DUPREC  (3270-DUP-ENTRY)
01756 *    END-EXEC.
      *    MOVE '"$%                   ! . #00005117' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303035313137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01757
01758  3250-WRITE-DETAIL-ENTRY.
01759
01760      
      * EXEC CICS WRITE
01761 *         FROM    (CHECK-WORK-RECORDS)
01762 *         DATASET (FILE-ID-ERCKWK)
01763 *         RIDFLD  (ERCKWK-KEY)
01764 *    END-EXEC.
           MOVE LENGTH OF
            CHECK-WORK-RECORDS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00005123' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 CHECK-WORK-RECORDS, 
                 DFHEIV11, 
                 ERCKWK-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01765
01766      MOVE 'A'                   TO PI-PREV-FUNCTION.
01767
01768      GO TO 3290-EXIT.
01769
01770  3270-DUP-ENTRY.
01771
01772      ADD +1                      TO CW-SEQUENCE-NO.
01773      MOVE CW-SEQUENCE-NO         TO ERCKWK-SEQUENCE-NO.
01774      GO TO 3250-WRITE-DETAIL-ENTRY.
01775
01776  3280-HEADER-NOTFND.
01777
01778      
      * EXEC CICS SYNCPOINT ROLLBACK
01779 *    END-EXEC.
      *    MOVE '6"R                   !   #00005141' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01780
01781      MOVE SPACE                  TO PI-MAINT-FUNCTION.
01782      MOVE ER-3161                TO EMI-ERROR.
01783      MOVE -1                     TO CARL.
01784      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01785      GO TO 8200-SEND-DATAONLY.
01786
01787  3285-RELEASE-ERROR.
01788
01789      
      * EXEC CICS UNLOCK
01790 *         DATASET (FILE-ID-ERCKWK)
01791 *    END-EXEC.
      *    MOVE '&*                    #   #00005152' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01792
01793      MOVE ER-3139                TO EMI-ERROR.
01794      MOVE -1                     TO MAINTL.
01795      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01796      GO TO 8200-SEND-DATAONLY.
01797
01798  3290-EXIT.
01799      EXIT.
01800
01801      EJECT
01802
01803 ******************************************************************
01804 *                                                                *
01805 *         U P D A T E   T O T A L   C O M M I S S I O N          *
01806 *                                                                *
01807 ******************************************************************
01808
01809  3300-UPDATE-TOTAL-COMMISSION.
01810
01811      MOVE PI-COMPANY-CD          TO ERCKWK-COMPANY-CD.
01812      MOVE PI-ERCKWK-CSR          TO ERCKWK-CSR.
01813      MOVE PI-ERCKWK-CARRIER      TO ERCKWK-CARRIER.
01814      MOVE PI-ERCKWK-GROUPING     TO ERCKWK-GROUPING.
01815      MOVE PI-ERCKWK-PAYEE        TO ERCKWK-PAYEE.
01816      MOVE PI-ERCKWK-PAYEE-SEQ    TO ERCKWK-PAYEE-SEQ.
01817      MOVE ZEROS                  TO ERCKWK-SEQUENCE-NO.
01818
01819      
      * EXEC CICS HANDLE CONDITION
01820 *        NOTFND    (3380-HEADER-NOTFND)
01821 *    END-EXEC.
      *    MOVE '"$I                   ! / #00005182' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2F20233030303035313832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01822
01823      
      * EXEC CICS READ
01824 *        SET     (ADDRESS OF CHECK-WORK-RECORDS)
01825 *        DATASET (FILE-ID-ERCKWK)
01826 *        RIDFLD  (ERCKWK-KEY)
01827 *        UPDATE
01828 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005186' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313836' TO DFHEIV0(25:11)
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
           
01829
01830      SUBTRACT WS-DETAIL-AMOUNT    FROM CW-TOTAL-COMMISSION.
01831      ADD      WS-DETAMT (WS-SUB1) TO   CW-TOTAL-COMMISSION.
01832
01833      MOVE WS-CURRENT-BIN-DT      TO  CW-RECORDED-DT.
01834      MOVE PI-PROCESSOR-ID        TO  CW-RECORDED-BY.
01835      MOVE EIBTIME                TO  CW-LAST-MAINT-HHMMSS.
01836
01837      
      * EXEC CICS REWRITE
01838 *         FROM    (CHECK-WORK-RECORDS)
01839 *         DATASET (FILE-ID-ERCKWK)
01840 *    END-EXEC.
           MOVE LENGTH OF
            CHECK-WORK-RECORDS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005200' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 CHECK-WORK-RECORDS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01841
01842      GO TO 3390-EXIT.
01843
01844  3380-HEADER-NOTFND.
01845
01846      
      * EXEC CICS SYNCPOINT ROLLBACK
01847 *    END-EXEC.
      *    MOVE '6"R                   !   #00005209' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01848
01849      MOVE SPACE                  TO PI-MAINT-FUNCTION.
01850      MOVE ER-3161                TO EMI-ERROR.
01851      MOVE -1                     TO CARL.
01852      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01853      GO TO 8200-SEND-DATAONLY.
01854
01855  3390-EXIT.
01856      EXIT.
01857      EJECT
01858 ******************************************************************
01859 *                                                                *
01860 *             R E S E Q U E N C E   S L U G   L I N E S          *
01861 *                                                                *
01862 ******************************************************************
01863
01864  3400-RESEQ-SLUG-LINES.
01865
01866      
      * EXEC CICS GETMAIN
01867 *        SET     (ADDRESS OF CHECK-WORK-RECORDS)
01868 *        LENGTH  (ERCKWK-RECORD-LENGTH)
01869 *        INITIMG (GETMAIN-SPACE)
01870 *    END-EXEC.
      *    MOVE ',"IL                  $   #00005229' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERCKWK-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF CHECK-WORK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01871
01872      MOVE ERCKWK-KEY             TO SAVE-ERCKWK-CONTROL.
01873      MOVE WS-DELETE-SEQ-NO       TO ERCKWK-SEQUENCE-NO
01874                                     WS-SEQUENCE-NO.
01875
01876      
      * EXEC CICS HANDLE CONDITION
01877 *        NOTFND    (3490-FINISHED)
01878 *        ENDFILE   (3490-FINISHED)
01879 *    END-EXEC.
      *    MOVE '"$I''                  ! 0 #00005239' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3020233030303035323339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01880
01881  3410-BROWSE-CHECK-WORK-FILE.
01882
01883      
      * EXEC CICS STARTBR
01884 *        DATASET (FILE-ID-ERCKWK)
01885 *        RIDFLD  (ERCKWK-KEY)
01886 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005246' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 ERCKWK-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01887
01888  3420-READ-CHECK-WORK-FILE.
01889
01890      
      * EXEC CICS READNEXT
01891 *        SET     (ADDRESS OF CHECK-WORK-RECORDS)
01892 *        DATASET (FILE-ID-ERCKWK)
01893 *        RIDFLD  (ERCKWK-KEY)
01894 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005253' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCKWK-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-WORK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01895
01896      IF PI-COMPANY-CD  NOT =  CW-COMPANY-CD
01897          GO TO      3490-FINISHED.
01898
01899      IF CW-COMPANY-CD = SVCKWK-COMPANY-CD AND
01900         CW-CARRIER    = SVCKWK-CARRIER    AND
01901         CW-CSR        = SVCKWK-CSR        AND
01902         CW-GROUPING   = SVCKWK-GROUPING   AND
01903         CW-PAYEE      = SVCKWK-PAYEE      AND
01904         CW-PAYEE-SEQ  = SVCKWK-PAYEE-SEQ
01905         NEXT SENTENCE
01906      ELSE
01907         GO TO 3490-FINISHED.
01908
01909      IF CW-HEADER OR CW-TEXT
01910          GO TO 3420-READ-CHECK-WORK-FILE.
01911
01912      
      * EXEC CICS ENDBR
01913 *         DATASET (FILE-ID-ERCKWK)
01914 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005275' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01915
01916      
      * EXEC CICS READ
01917 *        SET     (ADDRESS OF CHECK-WORK-RECORDS)
01918 *        DATASET (FILE-ID-ERCKWK)
01919 *        RIDFLD  (ERCKWK-KEY)
01920 *        UPDATE
01921 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005279' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323739' TO DFHEIV0(25:11)
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
           
01922
01923      MOVE CHECK-WORK-RECORDS     TO SAVE-ERCKWK-RECORD.
01924
01925      
      * EXEC CICS DELETE
01926 *         DATASET (FILE-ID-ERCKWK)
01927 *    END-EXEC.
      *    MOVE '&(                    &   #00005288' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01928
01929      MOVE SAVE-ERCKWK-RECORD     TO CHECK-WORK-RECORDS.
01930
01931      MOVE WS-SEQUENCE-NO         TO SAVE-CKWK-SEQ-NO
01932                                     ERCKWK-SEQUENCE-NO.
01933
01934      
      * EXEC CICS HANDLE CONDITION
01935 *         DUPREC  (9990-ABEND)
01936 *    END-EXEC.
      *    MOVE '"$%                   ! 1 #00005297' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3120233030303035323937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01937
01938      
      * EXEC CICS WRITE
01939 *         FROM    (SAVE-ERCKWK-RECORD)
01940 *         DATASET (FILE-ID-ERCKWK)
01941 *         RIDFLD  (ERCKWK-KEY)
01942 *    END-EXEC.
           MOVE LENGTH OF
            SAVE-ERCKWK-RECORD
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00005301' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 SAVE-ERCKWK-RECORD, 
                 DFHEIV11, 
                 ERCKWK-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01943
01944      ADD +1                      TO WS-SEQUENCE-NO
01945                                     ERCKWK-SEQUENCE-NO.
01946      GO TO 3410-BROWSE-CHECK-WORK-FILE.
01947
01948  3490-FINISHED.
01949
01950      MOVE SAVE-ERCKWK-CONTROL  TO ERCKWK-KEY.
01951
01952      
      * EXEC CICS ENDBR
01953 *         DATASET (FILE-ID-ERCKWK)
01954 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005315' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01955
01956  3499-EXIT.
01957      EXIT.
01958
01959      EJECT
01960 ******************************************************************
01961 *                                                                *
01962 *             D E L E T E   P A Y E E                            *
01963 *                                                                *
01964 ******************************************************************
01965
01966  4000-DELETE-PAYEE.
01967
01968      MOVE 'D'                    TO PI-MAINT-FUNCTION.
01969
01970      MOVE PI-COMPANY-CD          TO ERCKWK-COMPANY-CD.
01971      MOVE CSRI                   TO ERCKWK-CSR.
01972      MOVE CARI                   TO ERCKWK-CARRIER.
01973      MOVE GROUPI                 TO ERCKWK-GROUPING.
01974      MOVE PAYEEI                 TO ERCKWK-PAYEE.
01975      MOVE PAYSEQI                TO ERCKWK-PAYEE-SEQ.
01976      MOVE +0                     TO ERCKWK-SEQUENCE-NO.
01977
01978      MOVE ERCKWK-KEY             TO SAVE-ERCKWK-CONTROL.
01979
01980      
      * EXEC CICS HANDLE CONDITION
01981 *        NOTFND    (4080-PAYEE-NOTFND)
01982 *        ENDFILE   (4070-PAYEE-DELETED)
01983 *    END-EXEC.
      *    MOVE '"$I''                  ! 2 #00005343' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3220233030303035333433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01984
01985  4010-READ-CHECK-WORK-FILE.
01986
01987      
      * EXEC CICS STARTBR
01988 *        DATASET (FILE-ID-ERCKWK)
01989 *        RIDFLD  (ERCKWK-KEY)
01990 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005350' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 ERCKWK-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01991
01992      
      * EXEC CICS READNEXT
01993 *        SET     (ADDRESS OF CHECK-WORK-RECORDS)
01994 *        DATASET (FILE-ID-ERCKWK)
01995 *        RIDFLD  (ERCKWK-KEY)
01996 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005355' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCKWK-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-WORK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01997
01998      IF PI-COMPANY-CD  NOT =  CW-COMPANY-CD
01999          GO TO      4070-PAYEE-DELETED.
02000
02001      IF CW-COMPANY-CD = SVCKWK-COMPANY-CD AND
02002         CW-CSR        = SVCKWK-CSR        AND
02003         CW-CARRIER    = SVCKWK-CARRIER    AND
02004         CW-GROUPING   = SVCKWK-GROUPING   AND
02005         CW-PAYEE      = SVCKWK-PAYEE      AND
02006         CW-PAYEE-SEQ  = SVCKWK-PAYEE-SEQ
02007         NEXT SENTENCE
02008      ELSE
02009         GO TO 4070-PAYEE-DELETED.
02010
02011      IF CW-RELEASE-DT GREATER THAN LOW-VALUES
02012          GO TO 4090-PAYEE-RELEASED.
02013
02014      
      * EXEC CICS ENDBR
02015 *         DATASET (FILE-ID-ERCKWK)
02016 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005377' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02017
02018      
      * EXEC CICS READ
02019 *        SET     (ADDRESS OF CHECK-WORK-RECORDS)
02020 *        DATASET (FILE-ID-ERCKWK)
02021 *        RIDFLD  (ERCKWK-KEY)
02022 *        UPDATE
02023 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005381' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333831' TO DFHEIV0(25:11)
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
           
02024
02025      
      * EXEC CICS DELETE
02026 *         DATASET (FILE-ID-ERCKWK)
02027 *    END-EXEC.
      *    MOVE '&(                    &   #00005388' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02028
02029
02030      GO TO 4010-READ-CHECK-WORK-FILE.
02031
02032  4070-PAYEE-DELETED.
02033
02034      MOVE LOW-VALUES             TO  EL636AI.
02035      MOVE SVCKWK-CSR             TO  CSRO.
02036      MOVE SVCKWK-CARRIER         TO  CARO.
02037      MOVE SVCKWK-GROUPING        TO  GROUPO.
02038      MOVE SVCKWK-PAYEE           TO  PAYEEO.
02039      MOVE SVCKWK-PAYEE-SEQ       TO  PAYSEQO.
02040
02041      MOVE AL-UANON               TO  CSRA
02042                                      CARA
02043                                      GROUPA
02044                                      PAYEEA
02045                                      PAYSEQA.
02046
02047      MOVE ER-0000                TO  EMI-ERROR.
02048      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02049      MOVE -1                     TO  MAINTL.
02050      GO TO 8100-SEND-INITIAL-MAP.
02051
02052  4080-PAYEE-NOTFND.
02053
02054      MOVE ER-3160                TO EMI-ERROR.
02055      MOVE -1                     TO CARL.
02056      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02057      GO TO 8200-SEND-DATAONLY.
02058
02059  4090-PAYEE-RELEASED.
02060
02061      
      * EXEC CICS UNLOCK
02062 *         DATASET (FILE-ID-ERCKWK)
02063 *    END-EXEC.
      *    MOVE '&*                    #   #00005424' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02064
02065      MOVE ER-3139                TO EMI-ERROR.
02066      MOVE -1                     TO MAINTL.
02067      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02068      GO TO 8200-SEND-DATAONLY.
02069
02070      EJECT
02071
02072 ******************************************************************
02073 *                                                                *
02074 *               D I S P L A Y   P A Y E E                        *
02075 *                                                                *
02076 ******************************************************************
02077
02078  5000-DISPLAY-PAYEE.
02079
02080      MOVE 'Y' TO WS-FIRST-DETAIL-SW.
02081
02082      MOVE PI-ERCKWK-COMP-CD      TO ERCKWK-COMPANY-CD.
02083      MOVE PI-ERCKWK-CSR          TO ERCKWK-CSR.
02084      MOVE PI-ERCKWK-CARRIER      TO ERCKWK-CARRIER.
02085      MOVE PI-ERCKWK-GROUPING     TO ERCKWK-GROUPING.
02086      MOVE PI-ERCKWK-PAYEE        TO ERCKWK-PAYEE.
02087      MOVE PI-ERCKWK-PAYEE-SEQ    TO ERCKWK-PAYEE-SEQ.
02088      MOVE ZEROS                  TO ERCKWK-SEQUENCE-NO.
02089
02090      IF EIBAID = DFHPF3
02091          MOVE PI-LAST-STUB-KEY    TO ERCKWK-KEY
02092                                      PI-ERCKWK-KEY
02093          MOVE ZEROS               TO ERCKWK-SEQUENCE-NO
02094                                      PI-ERCKWK-SEQ-NO
02095          MOVE PI-LAST-STUB-SEQ-NO TO WS-NOFIRST.
02096
02097      
      * EXEC CICS HANDLE CONDITION
02098 *        NOTFND    (5090-CHECKS-NOTFND)
02099 *        ENDFILE   (5090-CHECKS-NOTFND)
02100 *    END-EXEC.
      *    MOVE '"$I''                  ! 3 #00005460' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3320233030303035343630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02101
02102      
      * EXEC CICS READ
02103 *        SET     (ADDRESS OF CHECK-WORK-RECORDS)
02104 *        DATASET (FILE-ID-ERCKWK)
02105 *        RIDFLD  (ERCKWK-KEY)
02106 *    END-EXEC.
      *    MOVE '&"S        E          (   #00005465' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343635' TO DFHEIV0(25:11)
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
           
02107
02108      IF PI-COMPANY-CD  NOT =  CW-COMPANY-CD
02109          GO TO      5090-CHECKS-NOTFND.
02110
02111  5020-FOUND.
02112      IF NOT CW-HEADER
02113         GO TO 5090-CHECKS-NOTFND.
02114
02115      MOVE +1 TO WS-SUB1.
02116  5000-SAVE-MAINT.
02117
02118      IF WS-SUB1 LESS THAN +6
02119          MOVE CK-MAINT (WS-SUB1) TO WS-HOLD-MAINT (WS-SUB1)
02120          ADD +1 TO WS-SUB1
02121          GO TO 5000-SAVE-MAINT.
02122
02123      MOVE MAINTI TO WS-SAVE-ACTION-CODE.
02124      MOVE LOW-VALUES             TO  EL636AI.
02125      MOVE 'C'                    TO  MAINTI.
02126      MOVE AL-UANON               TO  MAINTA.
02127      MOVE CW-CSR                 TO  CSRO.
02128      MOVE AL-UANON               TO  CSRA.
02129      MOVE CW-CARRIER             TO  CARO.
02130      MOVE AL-UANON               TO  CARA.
02131      MOVE CW-GROUPING            TO  GROUPO.
02132      MOVE AL-UANON               TO  GROUPA.
02133      MOVE CW-PAYEE               TO  PAYEEO.
02134      MOVE AL-UANON               TO  PAYEEA.
02135      MOVE CW-PAYEE-SEQ           TO  PAYSEQO.
02136      MOVE AL-UANON               TO  PAYSEQA.
02137      MOVE CW-PAYEE-NAME          TO  PAYTOO.
02138      MOVE AL-UANON               TO  PAYTOA.
02139      MOVE CW-ADDRESS-1           TO  ADDRS1O.
02140      MOVE AL-UANON               TO  ADDRS1A.
02141      MOVE CW-ADDRESS-2           TO  ADDRS2O.
02142      MOVE AL-UANON               TO  ADDRS2A.
02143      MOVE CW-PAYEE-CITY-ST       TO  CITYSTO.
02144      MOVE AL-UANON               TO  CITYSTA.
02145      MOVE CW-PAYEE-ZIP           TO  ZIPO.
02146      MOVE AL-UANON               TO  ZIPA.
02147      MOVE CW-PAYEE-ZIP-EXT       TO  ZIPEXTO.
02148      MOVE AL-UANON               TO  ZIPEXTA.
02149
02150
02151      MOVE AL-SANON               TO  PFKEY8A.
02152
02153      IF CW-RECORDED-BY = 'OFFL'
02154         MOVE 'OFF-LINE'          TO  MAINTBYO
02155      ELSE
02156         MOVE CW-RECORDED-BY      TO  MAINTBYO.
02157
02158      MOVE CW-LAST-MAINT-HHMMSS   TO  WS-TIME.
02159      MOVE WS-HR-MINS             TO  MAINTATO.
02160      MOVE CW-RECORDED-DT         TO  DC-BIN-DATE-1.
02161      MOVE SPACE                  TO  DC-OPTION-CODE.
02162      PERFORM 8500-DATE-CONVERT.
02163
02164      IF NO-CONVERSION-ERROR
02165         MOVE DC-GREG-DATE-1-EDIT TO  MAINTONO.
02166
02167      MOVE CW-RELEASE-DT          TO  DC-BIN-DATE-1.
02168      MOVE SPACE                  TO  DC-OPTION-CODE.
02169      PERFORM 8500-DATE-CONVERT.
02170
02171      IF NO-CONVERSION-ERROR
02172         MOVE DC-GREG-DATE-1-EDIT TO  RELDTO.
02173
02174      MOVE CW-TOTAL-COMMISSION    TO  CHKAMTO.
02175      MOVE CW-TOTAL-ENTRIES       TO  NOENTRSO.
02176      MOVE AL-UNNON               TO  NOFIRSTA.
02177      MOVE ZEROS                  TO  NOFIRSTO
02178                                      NOLASTO.
02179
02180      MOVE PI-ERCKWK-KEY          TO  PI-LAST-STUB-KEY.
02181
02182      IF SHOW-FUNCTION  AND
02183          WS-NOFIRST GREATER +0
02184          IF WS-NOFIRST GREATER THAN CW-TOTAL-ENTRIES
02185              IF CW-TOTAL-ENTRIES GREATER THAN +5
02186                  SUBTRACT +5 FROM CW-TOTAL-ENTRIES
02187                                     GIVING  PI-ERCKWK-SEQ-NO
02188              ELSE
02189                  MOVE +1 TO  PI-ERCKWK-SEQ-NO
02190          ELSE
02191              MOVE WS-NOFIRST TO  PI-ERCKWK-SEQ-NO.
02192
02193      IF  EIBAID = DFHPF3
02194          IF WS-NOFIRST LESS THAN CW-TOTAL-ENTRIES
02195              ADD +1 WS-NOFIRST GIVING PI-ERCKWK-SEQ-NO
02196          ELSE
02197              MOVE WS-NOFIRST          TO PI-ERCKWK-SEQ-NO.
02198
02199      MOVE PI-ERCKWK-KEY          TO SAVE-ERCKWK-CONTROL
02200                                     PI-SAVE-ERCKWK-KEY.
02201      
      * EXEC CICS HANDLE CONDITION
02202 *        NOTFND    (5070-CHECK-PROCESSED)
02203 *        ENDFILE   (5060-END-OF-FILE)
02204 *    END-EXEC.
      *    MOVE '"$I''                  ! 4 #00005564' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3420233030303035353634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02205
02206      IF WS-BROWSE-SW  =  'Y'
02207          GO TO 5049-SET.
02208      MOVE SPACE                  TO WS-BROWSE-SW.
02209
02210      
      * EXEC CICS STARTBR
02211 *        DATASET (FILE-ID-ERCKWK)
02212 *        RIDFLD  (PI-ERCKWK-KEY)
02213 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005573' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 PI-ERCKWK-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02214
02215  5049-SET.
02216      MOVE +0                     TO WS-SUB1
02217                                     PI-NO-ENTRIES-DISPLAYED.
02218      MOVE 'Y'                    TO WS-BROWSE-SW.
02219
02220  5050-READ-CHECK-WORK-FILE.
02221
02222      
      * EXEC CICS READNEXT
02223 *        SET     (ADDRESS OF CHECK-WORK-RECORDS)
02224 *        DATASET (FILE-ID-ERCKWK)
02225 *        RIDFLD  (PI-ERCKWK-KEY)
02226 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005585' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERCKWK-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-WORK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02227
02228      IF CW-COMPANY-CD = SVCKWK-COMPANY-CD AND
02229         CW-CSR        = SVCKWK-CSR        AND
02230         CW-CARRIER    = SVCKWK-CARRIER    AND
02231         CW-GROUPING   = SVCKWK-GROUPING   AND
02232         CW-PAYEE      = SVCKWK-PAYEE      AND
02233         CW-PAYEE-SEQ  = SVCKWK-PAYEE-SEQ
02234         NEXT SENTENCE
02235      ELSE
02236         GO TO 5070-CHECK-PROCESSED.
02237
02238      IF CW-HEADER OR CW-TEXT
02239         GO TO 5050-READ-CHECK-WORK-FILE
02240      ELSE
02241          IF  FIRST-DETAIL-READ
02242              MOVE SPACES TO WS-FIRST-DETAIL-SW
02243              MOVE AL-UNNON       TO  NOFIRSTA
02244              MOVE CW-SEQUENCE-NO TO  NOFIRSTO.
02245
02246      ADD +1                      TO  WS-SUB1.
02247
02248      IF  WS-SUB1 GREATER THAN +5
02249          GO TO 5070-CHECK-PROCESSED.
02250
02251      MOVE CW-SEQUENCE-NO         TO  NOLASTO.
02252
02253      MOVE CW-COMMENT             TO  CK-COMNTS      (WS-SUB1).
02254      MOVE CW-ACCT-AGENT          TO  CK-AGENT       (WS-SUB1).
02255
02256      IF WS-HOLD-MAINT (WS-SUB1) EQUAL 'I'
02257         MOVE CW-INVOICE          TO  CK-INVREF      (WS-SUB1)
02258      ELSE
02259         IF WS-HOLD-MAINT (WS-SUB1) EQUAL 'R'
02260             MOVE CW-REFERENCE    TO  CK-INVREF      (WS-SUB1)
02261         ELSE
02262             IF CW-INVOICE GREATER THAN SPACES
02263                 MOVE CW-INVOICE   TO  CK-INVREF     (WS-SUB1)
02264             ELSE
02265                 MOVE CW-REFERENCE TO  CK-INVREF     (WS-SUB1).
02266
02267      MOVE CW-LEDGER-NO           TO  CK-LEDGER      (WS-SUB1).
02268      MOVE CW-PMT-APPLIED         TO  CK-AGOCD       (WS-SUB1).
02269      MOVE CW-DETAIL-AMOUNT       TO  CK-DETAMT-OUT  (WS-SUB1).
02270
02271      MOVE PI-ERCKWK-KEY          TO  PI-STUB-KEY     (WS-SUB1)
02272                                      PI-LAST-STUB-KEY.
02273      MOVE CW-ACCT-AGENT          TO  PI-STUB-ACCT   (WS-SUB1).
02274
02275      IF CW-LAST-MAINT-APPLIED EQUAL 'H'
02276          MOVE CW-LAST-MAINT-APPLIED
02277                                  TO  CK-MAINT (WS-SUB1)
02278          MOVE 'THIS ITEM ON HOLD'
02279                                  TO  CK-COMNTS       (WS-SUB1)
02280          MOVE +0                 TO  CK-COMNTS-LEN   (WS-SUB1)
02281          MOVE AL-UNBOF           TO  CK-DETAMT-ATTRB (WS-SUB1)
02282          MOVE AL-UABOF           TO  CK-COMNTS-ATTRB (WS-SUB1)
02283                                      CK-MAINT-ATTRB  (WS-SUB1)
02284                                      CK-AGENT-ATTRB  (WS-SUB1)
02285                                      CK-INVREF-ATTRB (WS-SUB1)
02286                                      CK-LEDGER-ATTRB (WS-SUB1)
02287                                      CK-AGOCD-ATTRB  (WS-SUB1).
02288
02289      MOVE WS-SUB1                TO  PI-NO-ENTRIES-DISPLAYED.
02290
02291      GO TO 5050-READ-CHECK-WORK-FILE.
02292
02293  5060-END-OF-FILE.
02294
02295      MOVE ER-2251                TO EMI-ERROR.
02296      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02297
02298  5070-CHECK-PROCESSED.
02299
02300      IF WS-BROWSE-STARTED
02301         
      * EXEC CICS ENDBR
02302 *            DATASET (FILE-ID-ERCKWK)
02303 *       END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005664' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02304
02305      IF EIBAID = DFHPF3
02306          MOVE ' '                TO  PI-PREV-FUNCTION.
02307
02308      MOVE 'S'                    TO  PI-MAINT-FUNCTION.
02309      MOVE -1                     TO  MAINTL.
02310      GO TO 8100-SEND-INITIAL-MAP.
02311
02312  5090-CHECKS-NOTFND.
02313
02314      IF WS-BROWSE-STARTED
02315         
      * EXEC CICS ENDBR
02316 *            DATASET (FILE-ID-ERCKWK)
02317 *       END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005678' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02318
02319      
      * EXEC CICS HANDLE CONDITION
02320 *        NOTFND    (5095-CHECKS-NOTFND)
02321 *        ENDFILE   (5095-CHECKS-NOTFND)
02322 *    END-EXEC.
      *    MOVE '"$I''                  ! 5 #00005682' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3520233030303035363832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02323
02324      MOVE SPACE                  TO WS-BROWSE-SW.
02325
02326      
      * EXEC CICS STARTBR
02327 *        DATASET (FILE-ID-ERCKWK)
02328 *        RIDFLD  (PI-ERCKWK-KEY)
02329 *        GTEQ
02330 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005689' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 PI-ERCKWK-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02331
02332      MOVE 'Y'                    TO WS-BROWSE-SW.
02333
02334      
      * EXEC CICS READNEXT
02335 *        SET     (ADDRESS OF CHECK-WORK-RECORDS)
02336 *        DATASET (FILE-ID-ERCKWK)
02337 *        RIDFLD  (PI-ERCKWK-KEY)
02338 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005697' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERCKWK-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-WORK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02339
02340      IF PI-COMPANY-CD  NOT =  CW-COMPANY-CD
02341          GO TO      5095-CHECKS-NOTFND
02342      ELSE
02343          GO TO      5020-FOUND.
02344
02345
02346  5095-CHECKS-NOTFND.
02347      MOVE SPACE                  TO PI-MAINT-FUNCTION.
02348      MOVE ER-3160                TO EMI-ERROR.
02349      MOVE -1                     TO CARL.
02350      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02351      GO TO 8200-SEND-DATAONLY.
02352
02353      EJECT
02354
02355 ******************************************************************
02356 *                                                                *
02357 *            D I S P L A Y   N E X T   P A Y E E                 *
02358 *                                                                *
02359 ******************************************************************
02360
02361  6000-DISPLAY-NEXT-PAYEE.
02362
02363      MOVE PI-LAST-STUB-KEY       TO PI-ERCKWK-KEY.
02364      MOVE PI-COMPANY-CD          TO PI-ERCKWK-COMP-CD.
02365
02366      IF PI-LAST-STUB-CSR = SPACES
02367          MOVE LOW-VALUES         TO PI-LAST-STUB-CSR.
02368
02369      MOVE  PI-LAST-STUB-CSR      TO PI-ERCKWK-CSR
02370
02371      MOVE +9999                  TO PI-ERCKWK-SEQ-NO.
02372
02373      
      * EXEC CICS HANDLE CONDITION
02374 *        ENDFILE   (6060-END-OF-FILE)
02375 *    END-EXEC.
      *    MOVE '"$''                   ! 6 #00005736' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3620233030303035373336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02376
02377  6010-READ-NEXT-PAYEE.
02378
02379      
      * EXEC CICS READ
02380 *        SET     (ADDRESS OF CHECK-WORK-RECORDS)
02381 *        DATASET (FILE-ID-ERCKWK)
02382 *        RIDFLD  (PI-ERCKWK-KEY)
02383 *        GTEQ
02384 *    END-EXEC.
      *    MOVE '&"S        G          (   #00005742' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERCKWK-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-WORK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02385
02386      IF CW-COMPANY-CD NOT = PI-COMPANY-CD
02387         GO TO 6060-END-OF-FILE.
02388
02389      MOVE  CW-CSR                TO  PI-ERCKWK-CSR.
02390      MOVE  CW-CARRIER            TO  PI-ERCKWK-CARRIER.
02391      MOVE  CW-GROUPING           TO  PI-ERCKWK-GROUPING.
02392      MOVE  CW-PAYEE              TO  PI-ERCKWK-PAYEE.
02393      MOVE  CW-PAYEE-SEQ          TO  PI-ERCKWK-PAYEE-SEQ.
02394      MOVE  CW-SEQUENCE-NO        TO  PI-ERCKWK-SEQ-NO.
02395
02396      GO TO 5000-DISPLAY-PAYEE.
02397
02398  6060-END-OF-FILE.
02399
02400      MOVE SPACE                  TO PI-MAINT-FUNCTION.
02401      MOVE ER-2251                TO EMI-ERROR.
02402      MOVE -1                     TO CARL.
02403      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02404      GO TO 8200-SEND-DATAONLY.
02405
02406      EJECT
02407
02408 ******************************************************************
02409 *                                                                *
02410 *            D I S P L A Y   P R E V   P A Y E E                 *
02411 *                                                                *
02412 ******************************************************************
02413
02414  6100-DISPLAY-PREV-PAYEE.
02415
02416      IF PI-LAST-STUB-KEY EQUAL SPACES
02417          GO TO 6160-END-OF-FILE.
02418
02419      MOVE PI-LAST-STUB-KEY       TO PI-ERCKWK-KEY.
02420
02421      IF PI-ERCKWK-SEQ-NO GREATER THAN +5
02422          MOVE +1 TO PI-ERCKWK-SEQ-NO
02423          GO TO 5000-DISPLAY-PAYEE.
02424
02425      MOVE PI-COMPANY-CD          TO PI-ERCKWK-COMP-CD.
02426      MOVE +0000                  TO PI-ERCKWK-SEQ-NO.
02427
02428      
      * EXEC CICS HANDLE CONDITION
02429 *        ENDFILE   (6160-END-OF-FILE)
02430 *    END-EXEC.
      *    MOVE '"$''                   ! 7 #00005791' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3720233030303035373931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02431
02432      
      * EXEC CICS STARTBR
02433 *        DATASET (FILE-ID-ERCKWK)
02434 *        RIDFLD  (PI-ERCKWK-KEY)
02435 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005795' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 PI-ERCKWK-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02436
02437      MOVE PI-ERCKWK-KEY          TO SVCKWK-COMPARE-KEY.
02438
02439      
      * EXEC CICS READNEXT
02440 *        SET     (ADDRESS OF CHECK-WORK-RECORDS)
02441 *        DATASET (FILE-ID-ERCKWK)
02442 *        RIDFLD  (PI-ERCKWK-KEY)
02443 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005802' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERCKWK-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-WORK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02444
02445      
      * EXEC CICS RESETBR
02446 *        DATASET (FILE-ID-ERCKWK)
02447 *        RIDFLD  (PI-ERCKWK-KEY)
02448 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&4         G          &   #00005808' TO DFHEIV0
           MOVE X'263420202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 PI-ERCKWK-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02449
02450  6110-READ-PREV-ENTRY.
02451
02452      
      * EXEC CICS READPREV
02453 *        SET     (ADDRESS OF CHECK-WORK-RECORDS)
02454 *        DATASET (FILE-ID-ERCKWK)
02455 *        RIDFLD  (PI-ERCKWK-KEY)
02456 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00005815' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERCKWK-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-WORK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02457
02458      IF PI-COMPANY-CD  NOT =  CW-COMPANY-CD
02459          GO TO      6160-END-OF-FILE.
02460
02461      MOVE PI-ERCKWK-KEY          TO ERCKWK-COMPARE-KEY.
02462
02463      IF ERCKWK-COMPARE-KEY LESS THAN SVCKWK-COMPARE-KEY
02464         NEXT SENTENCE
02465      ELSE
02466         GO TO 6110-READ-PREV-ENTRY.
02467
02468      MOVE +1                     TO PI-ERCKWK-SEQ-NO.
02469
02470      
      * EXEC CICS ENDBR
02471 *         DATASET (FILE-ID-ERCKWK)
02472 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005833' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02473
02474      IF PI-ERCKWK-COMP-CD NOT = PI-COMPANY-CD
02475         GO TO 6160-END-OF-FILE
02476      ELSE
02477         GO TO 5000-DISPLAY-PAYEE.
02478
02479  6160-END-OF-FILE.
02480
02481      MOVE SPACE                  TO PI-MAINT-FUNCTION.
02482      MOVE ER-2252                TO EMI-ERROR.
02483      MOVE -1                     TO CARL.
02484      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02485      GO TO 8200-SEND-DATAONLY.
02486
02487      EJECT
02488
02489 ******************************************************************
02490 *                                                                *
02491 *         D I S P L A Y   P R E V   E N T E R I E S              *
02492 *                                                                *
02493 ******************************************************************
02494
02495  6600-DISPLAY-PREV-ENTRIES.
02496
02497      IF PI-NO-ENTRIES-DISPLAYED GREATER THAN ZEROS
02498          MOVE PI-STUB-KEY (1)        TO PI-ERCKWK-KEY
02499                                         SAVE-ERCKWK-CONTROL
02500      ELSE
02501          MOVE PI-LAST-STUB-KEY       TO PI-ERCKWK-KEY
02502          GO TO 5000-DISPLAY-PAYEE.
02503
02504      MOVE +0                     TO WS-SUB1.
02505
02506      
      * EXEC CICS STARTBR
02507 *        DATASET (FILE-ID-ERCKWK)
02508 *        RIDFLD  (PI-ERCKWK-KEY)
02509 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005869' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 PI-ERCKWK-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02510
02511      MOVE PI-ERCKWK-KEY          TO SAVE-ERCKWK-CONTROL.
02512
02513      
      * EXEC CICS READNEXT
02514 *        SET     (ADDRESS OF CHECK-WORK-RECORDS)
02515 *        DATASET (FILE-ID-ERCKWK)
02516 *        RIDFLD  (PI-ERCKWK-KEY)
02517 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005876' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERCKWK-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-WORK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02518
02519      
      * EXEC CICS RESETBR
02520 *        DATASET (FILE-ID-ERCKWK)
02521 *        RIDFLD  (PI-ERCKWK-KEY)
02522 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&4         G          &   #00005882' TO DFHEIV0
           MOVE X'263420202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 PI-ERCKWK-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02523
02524  6610-READ-PREV-ENTRY.
02525
02526      
      * EXEC CICS READPREV
02527 *        SET     (ADDRESS OF CHECK-WORK-RECORDS)
02528 *        DATASET (FILE-ID-ERCKWK)
02529 *        RIDFLD  (PI-ERCKWK-KEY)
02530 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00005889' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERCKWK-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-WORK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02531
02532      IF PI-COMPANY-CD  NOT =  CW-COMPANY-CD
02533          GO TO      5000-DISPLAY-PAYEE.
02534
02535      IF PI-ERCKWK-PAYEE GREATER THAN SVCKWK-PAYEE
02536         GO TO 6610-READ-PREV-ENTRY.
02537
02538      IF PI-ERCKWK-SEQ-NO = ZEROS
02539         GO TO 6660-PREV-ENTRIES-PRIMED.
02540
02541      ADD +1                      TO WS-SUB1.
02542
02543      IF WS-SUB1 GREATER +5
02544         GO TO 6660-PREV-ENTRIES-PRIMED.
02545
02546      GO TO 6610-READ-PREV-ENTRY.
02547
02548  6660-PREV-ENTRIES-PRIMED.
02549
02550      
      * EXEC CICS ENDBR
02551 *         DATASET (FILE-ID-ERCKWK)
02552 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005913' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02553
02554      GO TO 5000-DISPLAY-PAYEE.
02555
02556      EJECT
02557
02558 ******************************************************************
02559 *                                                                *
02560 *    R E L E A S E   E N T E R I E S   T O   CHECK-FILE          *
02561 *                                                                *
02562 ******************************************************************
02563
02564  7000-RELEASE-ENTRIES.
02565      MOVE ' '                    TO ERCOMP-SW.
02566
02567      
      * EXEC CICS GETMAIN
02568 *        SET     (ADDRESS OF COMM-CHECK-RECORDS)
02569 *        LENGTH  (ERCMCK-RECORD-LENGTH)
02570 *        INITIMG (GETMAIN-SPACE)
02571 *    END-EXEC.
      *    MOVE ',"IL                  $   #00005930' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERCMCK-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF COMM-CHECK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02572
02573      
      * EXEC CICS HANDLE CONDITION
02574 *        NOTFND    (7090-CHECKS-NOTFND)
02575 *        ENDFILE   (7090-CHECKS-NOTFND)
02576 *    END-EXEC.
      *    MOVE '"$I''                  ! 8 #00005936' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3820233030303035393336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02577
02578      MOVE PI-COMPANY-CD          TO ERCKWK-COMPANY-CD.
02579      MOVE CSRI                   TO ERCKWK-CSR.
02580      MOVE CARI                   TO ERCKWK-CARRIER.
02581      MOVE GROUPI                 TO ERCKWK-GROUPING.
02582      MOVE PAYEEI                 TO ERCKWK-PAYEE.
02583      MOVE PAYSEQI                TO ERCKWK-PAYEE-SEQ.
02584      MOVE +0                     TO ERCKWK-SEQUENCE-NO.
02585
02586      
      * EXEC CICS READ
02587 *        SET     (ADDRESS OF CHECK-WORK-RECORDS)
02588 *        DATASET (FILE-ID-ERCKWK)
02589 *        RIDFLD  (ERCKWK-KEY)
02590 *        UPDATE
02591 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005949' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393439' TO DFHEIV0(25:11)
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
           
02592
02593      IF CW-RELEASE-DT GREATER THAN LOW-VALUES
02594         GO TO 7095-RELEASE-ERROR.
02595
02596      IF CW-TOTAL-COMMISSION NOT GREATER THAN ZEROS
02597         GO TO 7085-COMMISSION-ERROR.
02598
02599      IF CW-PAYEE-NAME = SPACES OR
02600         CW-PAYEE-NAME = 'NEED MAILING NAME'
02601         GO TO 7075-PAYEE-NAME-ERROR.
02602
02603      MOVE WS-CURRENT-BIN-DT      TO  CW-RELEASE-DT.
02604      MOVE WS-CURRENT-BIN-DT      TO  CW-RECORDED-DT.
02605      MOVE PI-PROCESSOR-ID        TO  CW-RECORDED-BY.
02606      MOVE EIBTIME                TO  CW-LAST-MAINT-HHMMSS.
02607
02608      MOVE +0 TO WS-SEQUENCE-NO.
02609
02610      MOVE CW-PAYEE-NAME          TO WS-PAYEE-NAME.
02611      MOVE CW-ADDRESS-1           TO WS-PAYEE-ADDRESS-1.
02612      MOVE CW-ADDRESS-2           TO WS-PAYEE-ADDRESS-2.
02613      MOVE CW-PAYEE-CITY-ST       TO WS-PAYEE-CITY-ST.
02614      MOVE CW-PAYEE-ZIP           TO WS-PAYEE-ZIP.
02615      MOVE CW-PAYEE-ZIP-EXT       TO WS-PAYEE-ZIP-EXT.
02616
02617      MOVE CW-TOTAL-COMMISSION    TO WS-AMOUNT-PAID.
02618      MOVE CW-TOTAL-ENTRIES       TO WS-TOTAL-ENTRIES.
02619
02620      MOVE ERCKWK-KEY             TO SAVE-ERCKWK-CONTROL.
02621
02622      MOVE +1                     TO WS-SUB2.
02623
02624      
      * EXEC CICS REWRITE
02625 *         FROM    (CHECK-WORK-RECORDS)
02626 *         DATASET (FILE-ID-ERCKWK)
02627 *    END-EXEC.
           MOVE LENGTH OF
            CHECK-WORK-RECORDS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005987' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 CHECK-WORK-RECORDS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02628
02629      
      * EXEC CICS HANDLE CONDITION
02630 *        NOTFND    (7070-CHECK-PROCESSED)
02631 *        ENDFILE   (7070-CHECK-PROCESSED)
02632 *    END-EXEC.
      *    MOVE '"$I''                  ! 9 #00005992' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3920233030303035393932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02633
02634  7010-READ-CHECK-WORK-FILE.
02635
02636      
      * EXEC CICS STARTBR
02637 *        DATASET (FILE-ID-ERCKWK)
02638 *        RIDFLD  (ERCKWK-KEY)
02639 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005999' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 ERCKWK-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02640
02641  7020-READ-NEXT-DETAIL.
02642
02643      
      * EXEC CICS READNEXT
02644 *        SET     (ADDRESS OF CHECK-WORK-RECORDS)
02645 *        DATASET (FILE-ID-ERCKWK)
02646 *        RIDFLD  (ERCKWK-KEY)
02647 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006006' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCKWK-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-WORK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02648
02649      IF ERCKWK-KEY = SAVE-ERCKWK-CONTROL
02650         GO TO 7020-READ-NEXT-DETAIL.
02651
02652      IF CW-COMPANY-CD = SVCKWK-COMPANY-CD AND
02653         CW-CSR        = SVCKWK-CSR        AND
02654         CW-CARRIER    = SVCKWK-CARRIER    AND
02655         CW-GROUPING   = SVCKWK-GROUPING   AND
02656         CW-PAYEE      = SVCKWK-PAYEE      AND
02657         CW-PAYEE-SEQ  = SVCKWK-PAYEE-SEQ
02658         NEXT SENTENCE
02659      ELSE
02660         GO TO 7080-CHECK-ERROR.
02661
02662      IF CW-TEXT
02663         GO TO 7020-READ-NEXT-DETAIL.
02664
02665      IF ERCOMP-SW  IS EQUAL      TO ' '
02666         MOVE CW-COMPANY-CD       TO ERCOMP-COMP-CD
02667         MOVE CW-CARRIER          TO ERCOMP-CARRIER
02668         MOVE CW-GROUPING         TO ERCOMP-GROUPING
02669         MOVE CW-PAYEE            TO ERCOMP-FIN-RESP
02670         IF CW-PMT-APPLIED = 'O' OR 'G'
02671            MOVE LOW-VALUES       TO ERCOMP-ACCT
02672            MOVE 'G'              TO ERCOMP-RECORD-TYPE
02673            PERFORM 1450-ACH-READ    THRU 1470-EXIT
02674         ELSE
02675            MOVE CW-ACCT-AGENT    TO ERCOMP-ACCT
02676            MOVE 'A'              TO ERCOMP-RECORD-TYPE
02677            PERFORM 1450-ACH-READ    THRU 1470-EXIT
02678         END-IF
02679      END-IF.
02680
02681
02682      MOVE CW-COMMENT             TO  CK-STUB-COMMENT  (WS-SUB2).
02683      MOVE CW-ACCT-AGENT          TO  CK-ACCT-AGENT    (WS-SUB2).
02684      MOVE CW-INVOICE             TO  CK-INVOICE       (WS-SUB2).
02685      MOVE CW-REFERENCE           TO  CK-REFERENCE     (WS-SUB2).
02686      MOVE CW-LEDGER-NO           TO  CK-LEDGER-NO     (WS-SUB2).
02687      MOVE CW-LAST-MAINT-APPLIED
02688                              TO  CK-LAST-MAINT-APPLIED (WS-SUB2).
02689
02690      MOVE CW-NON-AR-ITEM         TO  CK-NON-AR-ITEM   (WS-SUB2).
02691      MOVE CW-PMT-APPLIED         TO  CK-PYAJ-PMT-APPLIED (WS-SUB2)
02692
02693      MOVE 'C'                    TO  CK-PAYMENT-TYPE  (WS-SUB2).
02694
02695      MOVE CW-DETAIL-AMOUNT       TO  CK-DETAIL-AMT    (WS-SUB2).
02696
02697      MOVE CW-CSR                 TO  CK-CKWK-CSR      (WS-SUB2).
02698      MOVE CW-CARRIER             TO  CK-CKWK-CARRIER  (WS-SUB2).
02699      MOVE CW-GROUPING            TO  CK-CKWK-GROUPING (WS-SUB2).
02700      MOVE CW-PAYEE               TO  CK-CKWK-PAYEE    (WS-SUB2).
02701      MOVE CW-PAYEE-SEQ           TO  CK-CKWK-PAYEE-SEQ (WS-SUB2).
02702      MOVE CW-SEQUENCE-NO         TO  CK-CKWK-SEQ-NO   (WS-SUB2).
02703
02704      
      * EXEC CICS ENDBR
02705 *         DATASET (FILE-ID-ERCKWK)
02706 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006067' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02707
02708      MOVE ERCKWK-KEY             TO SAVE-ERCKWK-CONTROL.
02709
02710      
      * EXEC CICS READ
02711 *        SET     (ADDRESS OF CHECK-WORK-RECORDS)
02712 *        DATASET (FILE-ID-ERCKWK)
02713 *        RIDFLD  (ERCKWK-KEY)
02714 *        UPDATE
02715 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00006073' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303733' TO DFHEIV0(25:11)
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
           
02716
02717      MOVE WS-CURRENT-BIN-DT      TO  CW-RELEASE-DT.
02718
02719      
      * EXEC CICS REWRITE
02720 *         FROM    (CHECK-WORK-RECORDS)
02721 *         DATASET (FILE-ID-ERCKWK)
02722 *    END-EXEC.
           MOVE LENGTH OF
            CHECK-WORK-RECORDS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00006082' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 CHECK-WORK-RECORDS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02723
02724      SUBTRACT +1                 FROM WS-TOTAL-ENTRIES.
02725
02726      IF WS-TOTAL-ENTRIES = +0
02727         GO TO 7070-CHECK-PROCESSED.
02728
02729      ADD +1                      TO   WS-SUB2.
02730
02731      IF WS-SUB2 NOT GREATER THAN +15
02732         GO TO 7010-READ-CHECK-WORK-FILE.
02733
02734      MOVE ZEROS                  TO  CK-AMOUNT-PAID.
02735      MOVE 'D'                    TO  CK-RECORD-TYPE.
02736      PERFORM 7400-WRITE-COMM-CHECK-RECORD THRU 7490-EXIT.
02737
02738      MOVE SPACES                 TO COMM-CHECK-RECORDS.
02739
02740      MOVE +1                     TO WS-SUB2.
02741
02742      GO TO 7010-READ-CHECK-WORK-FILE.
02743
02744  7070-CHECK-PROCESSED.
02745
02746      MOVE WS-AMOUNT-PAID         TO  CK-AMOUNT-PAID.
02747
02748      MOVE 'D'                    TO  CK-RECORD-TYPE.
02749      PERFORM 7400-WRITE-COMM-CHECK-RECORD THRU 7490-EXIT.
02750
02751      PERFORM 7100-WRITE-COMM-CHECK-TEXT THRU 7190-EXIT.
02752
02753      IF PI-NO-ENTRIES-DISPLAYED GREATER THAN ZEROS
02754         MOVE PI-STUB-KEY (1)        TO PI-ERCKWK-KEY.
02755
02756      MOVE ER-0000                TO  EMI-ERROR.
02757      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02758      GO TO 5000-DISPLAY-PAYEE.
02759
02760  7075-PAYEE-NAME-ERROR.
02761
02762      
      * EXEC CICS UNLOCK
02763 *         DATASET (FILE-ID-ERCKWK)
02764 *    END-EXEC.
      *    MOVE '&*                    #   #00006125' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036313235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02765
02766      MOVE ER-3163                TO EMI-ERROR.
02767      MOVE -1                     TO PAYTOL.
02768      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02769      GO TO 8200-SEND-DATAONLY.
02770
02771  7080-CHECK-ERROR.
02772
02773      
      * EXEC CICS SYNCPOINT ROLLBACK
02774 *    END-EXEC.
      *    MOVE '6"R                   !   #00006136' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036313336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02775
02776      MOVE ER-3162                TO EMI-ERROR.
02777      MOVE -1                     TO CARL.
02778      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02779      GO TO 8200-SEND-DATAONLY.
02780
02781  7085-COMMISSION-ERROR.
02782
02783      
      * EXEC CICS UNLOCK
02784 *         DATASET (FILE-ID-ERCKWK)
02785 *    END-EXEC.
      *    MOVE '&*                    #   #00006146' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036313436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02786
02787      MOVE AL-SABOF               TO CHKAMTA.
02788      MOVE ER-3165                TO EMI-ERROR.
02789      MOVE -1                     TO CARL.
02790      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02791      GO TO 8200-SEND-DATAONLY.
02792
02793  7090-CHECKS-NOTFND.
02794
02795      MOVE ER-3160                TO EMI-ERROR.
02796      MOVE -1                     TO CARL.
02797      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02798      GO TO 8200-SEND-DATAONLY.
02799
02800  7095-RELEASE-ERROR.
02801
02802      
      * EXEC CICS UNLOCK
02803 *         DATASET (FILE-ID-ERCKWK)
02804 *    END-EXEC.
      *    MOVE '&*                    #   #00006165' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036313635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02805
02806      MOVE ER-3139                TO EMI-ERROR.
02807      MOVE -1                     TO PAYTOL.
02808      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02809      GO TO 8200-SEND-DATAONLY.
02810
02811      EJECT
02812
02813 ******************************************************************
02814 *                                                                *
02815 *         W R I T E   C H E C K   S T U B   T E X T              *
02816 *                                                                *
02817 ******************************************************************
02818
02819  7100-WRITE-COMM-CHECK-TEXT.
02820
02821      
      * EXEC CICS HANDLE CONDITION
02822 *        NOTFND    (7190-EXIT)
02823 *        ENDFILE   (7190-EXIT)
02824 *    END-EXEC.
      *    MOVE '"$I''                  ! : #00006184' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3A20233030303036313834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02825
02826      MOVE +0                     TO WS-SUB2.
02827      MOVE SAVE-ERCKWK-CONTROL    TO ERCKWK-KEY.
02828      MOVE +8999                  TO ERCKWK-SEQUENCE-NO.
02829
02830  7110-READ-TEXT-LOOP.
02831      ADD +1                      TO WS-SUB2.
02832      ADD +1                      TO ERCKWK-SEQUENCE-NO.
02833
02834      
      * EXEC CICS READ
02835 *        SET     (ADDRESS OF CHECK-WORK-RECORDS)
02836 *        DATASET (FILE-ID-ERCKWK)
02837 *        RIDFLD  (ERCKWK-KEY)
02838 *        UPDATE
02839 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00006197' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036313937' TO DFHEIV0(25:11)
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
           
02840
02841      MOVE WS-CURRENT-BIN-DT      TO  CW-RELEASE-DT.
02842      MOVE CW-STUB-TEXT           TO  CK-STUB-TEXT (WS-SUB2).
02843      MOVE SPACES                 TO  CK-STUB-FILL-AREA.
02844
02845      IF ERCOMP-SW IS EQUAL       TO  'P'
02846         MOVE ERCOMP-SW           TO  CK-ACH-PAYMENT
02847      ELSE
02848         MOVE 'N'                 TO  CK-ACH-PAYMENT.
02849
02850
02851      
      * EXEC CICS REWRITE
02852 *         FROM    (CHECK-WORK-RECORDS)
02853 *         DATASET (FILE-ID-ERCKWK)
02854 *    END-EXEC.
           MOVE LENGTH OF
            CHECK-WORK-RECORDS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00006214' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 CHECK-WORK-RECORDS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02855
02856      IF WS-SUB2 LESS THAN +3
02857         GO TO 7110-READ-TEXT-LOOP.
02858
02859      MOVE ZEROS                  TO  CK-AMOUNT-PAID.
02860      MOVE 'T'                    TO  CK-RECORD-TYPE.
02861      PERFORM 7400-WRITE-COMM-CHECK-RECORD THRU 7490-EXIT.
02862
02863      MOVE SPACES                 TO COMM-CHECK-RECORDS.
02864
02865  7180-TEXT-PROCESSED.
02866
02867  7190-EXIT.
02868       EXIT.
02869
02870      EJECT
02871
02872 ******************************************************************
02873 *                                                                *
02874 *         W R I T E   C O M M I S S I O N   C H E C K S          *
02875 *                                                                *
02876 ******************************************************************
02877
02878  7400-WRITE-COMM-CHECK-RECORD.
02879
02880      ADD +1                      TO  WS-SEQUENCE-NO.
02881      MOVE WS-SEQUENCE-NO         TO  CK-SEQUENCE-NO.
02882
02883      MOVE 'CK'                   TO  CK-RECORD-ID.
02884      MOVE CW-COMPANY-CD          TO  CK-COMPANY-CD.
02885      MOVE CW-CSR                 TO  CK-CSR.
02886      MOVE CW-CARRIER             TO  CK-CARRIER.
02887      MOVE CW-GROUPING            TO  CK-GROUPING.
02888      MOVE CW-PAYEE               TO  CK-PAYEE.
02889      MOVE CW-PAYEE-SEQ           TO  CK-PAYEE-SEQ.
02890      MOVE WS-CURRENT-BIN-DT      TO  CK-RECORDED-DT.
02891      MOVE CW-AR-STATEMENT-DT     TO  CK-AR-STATEMENT-DT.
02892 *    MOVE CW-PMT-APPLIED         TO  CK-PMT-APPLIED.
02893      MOVE PI-PROCESSOR-ID        TO  CK-RECORDED-BY.
02894      MOVE EIBTIME                TO  CK-LAST-MAINT-HHMMSS.
02895      MOVE WS-PAYEE-NAME          TO  CK-PAYEE-NAME.
02896      MOVE WS-PAYEE-ADDRESS-1     TO  CK-PAYEE-ADDRESS-1.
02897      MOVE WS-PAYEE-ADDRESS-2     TO  CK-PAYEE-ADDRESS-2.
02898      MOVE WS-PAYEE-CITY-ST       TO  CK-PAYEE-CITY-ST.
02899      MOVE WS-PAYEE-ZIP-CODE      TO  CK-PAYEE-ZIP-CODE.
02900
02901      MOVE ZEROS                  TO  CK-QUE-CONTROL-NUMBER
02902                                      CK-QUE-SEQ-NO.
02903
02904      MOVE LOW-VALUES             TO  CK-CHECK-WRITTEN-DT
02905                                      CK-CREDIT-SELECT-DT
02906                                      CK-CREDIT-ACCEPT-DT
02907                                      CK-VOID-DT.
02908
02909      IF ERCOMP-SW IS EQUAL       TO  'P'
02910         MOVE ERCOMP-SW           TO  CK-ACH-PAYMENT
02911      ELSE
02912         MOVE 'N'                 TO  CK-ACH-PAYMENT.
02913
02914      
      * EXEC CICS HANDLE CONDITION
02915 *         DUPREC  (7480-DUPREC)
02916 *    END-EXEC.
      *    MOVE '"$%                   ! ; #00006277' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3B20233030303036323737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02917
02918  7425-WRITE-COMM-CHECK-RECORD.
02919
02920      
      * EXEC CICS WRITE
02921 *         DATASET   (FILE-ID-ERCMCK)
02922 *         FROM      (COMM-CHECK-RECORDS)
02923 *         RIDFLD    (CK-CONTROL-PRIMARY)
02924 *    END-EXEC.
           MOVE LENGTH OF
            COMM-CHECK-RECORDS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00006283' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCMCK, 
                 COMM-CHECK-RECORDS, 
                 DFHEIV11, 
                 CK-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02925
02926      GO TO 7490-EXIT.
02927
02928  7480-DUPREC.
02929
02930      ADD +1                      TO CK-SEQUENCE-NO.
02931      GO TO 7425-WRITE-COMM-CHECK-RECORD.
02932
02933  7490-EXIT.
02934       EXIT.
02935
02936      EJECT
02937
02938 ******************************************************************
02939 *                                                                *
02940 *           V O I D   C O M M I S I O N   C H E C K              *
02941 *                                                                *
02942 ******************************************************************
02943
02944  7500-VOID-COMM-CHECK.
02945
02946      MOVE 'V'                    TO PI-MAINT-FUNCTION.
02947
02948      MOVE PI-COMPANY-CD          TO ERCMCK-COMPANY-CD.
02949      MOVE CSRI                   TO ERCMCK-CSR.
02950      MOVE CARI                   TO ERCMCK-CARRIER.
02951      MOVE GROUPI                 TO ERCMCK-GROUPING.
02952      MOVE PAYEEI                 TO ERCMCK-PAYEE.
02953      MOVE PAYSEQI                TO ERCMCK-PAYEE-SEQ.
02954      MOVE +0                     TO ERCMCK-SEQUENCE-NO.
02955
02956      
      * EXEC CICS HANDLE CONDITION
02957 *        NOTFND    (7550-VOID-WORK-RECS)
02958 *        ENDFILE   (7550-VOID-WORK-RECS)
02959 *    END-EXEC.
      *    MOVE '"$I''                  ! < #00006319' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3C20233030303036333139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02960
02961  7510-READ-COMM-CHECK.
02962
02963      
      * EXEC CICS STARTBR
02964 *        DATASET (FILE-ID-ERCMCK)
02965 *        RIDFLD  (ERCMCK-KEY)
02966 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006326' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCMCK, 
                 ERCMCK-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02967
02968  7520-PROCESS-COMM-CHECK.
02969
02970      
      * EXEC CICS READNEXT
02971 *        SET     (ADDRESS OF COMM-CHECK-RECORDS)
02972 *        DATASET (FILE-ID-ERCMCK)
02973 *        RIDFLD  (ERCMCK-KEY)
02974 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006333' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCMCK, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCMCK-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMM-CHECK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02975
02976      IF PI-COMPANY-CD  NOT =  CK-COMPANY-CD
02977          GO TO      7550-VOID-WORK-RECS.
02978
02979
02980      MOVE ERCMCK-KEY             TO  SAVE-ERCMCK-CONTROL.
02981
02982      IF CK-COMPANY-CD = PI-SVCKWK-COMP-CD  AND
02983         CK-CSR        = PI-SVCKWK-CSR      AND
02984         CK-CARRIER    = PI-SVCKWK-CARRIER  AND
02985         CK-GROUPING   = PI-SVCKWK-GROUPING AND
02986         CK-PAYEE      = PI-SVCKWK-PAYEE    AND
02987         CK-PAYEE-SEQ  = PI-SVCKWK-PAYEE-SEQ
02988         NEXT SENTENCE
02989      ELSE
02990         
      * EXEC CICS ENDBR
02991 *            DATASET (FILE-ID-ERCMCK)
02992 *       END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006353' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCMCK, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02993         GO TO 7550-VOID-WORK-RECS.
02994
02995      IF CK-QUE-CONTROL-NUMBER = ZEROS
02996         NEXT SENTENCE
02997      ELSE
02998         GO TO 7580-VOID-ERROR.
02999
03000      
      * EXEC CICS ENDBR
03001 *         DATASET (FILE-ID-ERCMCK)
03002 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006363' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCMCK, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03003
03004      
      * EXEC CICS READ
03005 *        SET     (ADDRESS OF COMM-CHECK-RECORDS)
03006 *        DATASET (FILE-ID-ERCMCK)
03007 *        RIDFLD  (ERCMCK-KEY)
03008 *        UPDATE
03009 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00006367' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333637' TO DFHEIV0(25:11)
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
           
03010
03011      
      * EXEC CICS DELETE
03012 *         DATASET (FILE-ID-ERCMCK)
03013 *    END-EXEC.
      *    MOVE '&(                    &   #00006374' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCMCK, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03014
03015      GO TO 7510-READ-COMM-CHECK.
03016
03017  7550-VOID-WORK-RECS.
03018
03019      
      * EXEC CICS HANDLE CONDITION
03020 *        NOTFND    (7570-COMM-CHECK-VOIDED)
03021 *        ENDFILE   (7570-COMM-CHECK-VOIDED)
03022 *    END-EXEC.
      *    MOVE '"$I''                  ! = #00006382' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3D20233030303036333832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03023
03024      MOVE PI-COMPANY-CD          TO ERCKWK-COMPANY-CD.
03025      MOVE CSRI                   TO ERCKWK-CSR.
03026      MOVE CARI                   TO ERCKWK-CARRIER.
03027      MOVE GROUPI                 TO ERCKWK-GROUPING.
03028      MOVE PAYEEI                 TO ERCKWK-PAYEE.
03029      MOVE PAYSEQI                TO ERCKWK-PAYEE-SEQ.
03030      MOVE +0                     TO ERCKWK-SEQUENCE-NO.
03031
03032  7560-READ-CHECK-WORK-FILE.
03033
03034      
      * EXEC CICS STARTBR
03035 *        DATASET (FILE-ID-ERCKWK)
03036 *        RIDFLD  (ERCKWK-KEY)
03037 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006397' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 ERCKWK-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03038
03039      MOVE 'Y'                    TO WS-BROWSE-SW.
03040
03041  7565-PROCESS-WORK-FILE.
03042
03043      
      * EXEC CICS READNEXT
03044 *        SET     (ADDRESS OF CHECK-WORK-RECORDS)
03045 *        DATASET (FILE-ID-ERCKWK)
03046 *        RIDFLD  (ERCKWK-KEY)
03047 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006406' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCKWK-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-WORK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03048
03049      IF PI-COMPANY-CD  NOT =  CW-COMPANY-CD
03050          GO TO      7570-COMM-CHECK-VOIDED.
03051
03052      IF ERCKWK-KEY = SAVE-ERCKWK-CONTROL
03053         GO TO 7565-PROCESS-WORK-FILE.
03054
03055      MOVE ERCKWK-KEY             TO SAVE-ERCKWK-CONTROL.
03056
03057      IF CW-COMPANY-CD = PI-SVCKWK-COMP-CD  AND
03058         CW-CSR        = PI-SVCKWK-CSR      AND
03059         CW-CARRIER    = PI-SVCKWK-CARRIER  AND
03060         CW-GROUPING   = PI-SVCKWK-GROUPING AND
03061         CW-PAYEE      = PI-SVCKWK-PAYEE    AND
03062         CW-PAYEE-SEQ  = PI-SVCKWK-PAYEE-SEQ
03063         NEXT SENTENCE
03064      ELSE
03065         
      * EXEC CICS ENDBR
03066 *            DATASET (FILE-ID-ERCKWK)
03067 *       END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006428' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
03068         MOVE 'N'                 TO WS-BROWSE-SW
03069         GO TO 7570-COMM-CHECK-VOIDED.
03070
03071      
      * EXEC CICS ENDBR
03072 *         DATASET (FILE-ID-ERCKWK)
03073 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006434' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03074
03075      
      * EXEC CICS READ
03076 *        SET     (ADDRESS OF CHECK-WORK-RECORDS)
03077 *        DATASET (FILE-ID-ERCKWK)
03078 *        RIDFLD  (ERCKWK-KEY)
03079 *        UPDATE
03080 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00006438' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343338' TO DFHEIV0(25:11)
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
           
03081
03082      MOVE LOW-VALUES             TO  CW-RELEASE-DT.
03083
03084      
      * EXEC CICS REWRITE
03085 *         FROM    (CHECK-WORK-RECORDS)
03086 *         DATASET (FILE-ID-ERCKWK)
03087 *    END-EXEC.
           MOVE LENGTH OF
            CHECK-WORK-RECORDS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00006447' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 CHECK-WORK-RECORDS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03088
03089      GO TO 7560-READ-CHECK-WORK-FILE.
03090
03091  7570-COMM-CHECK-VOIDED.
03092
03093      IF WS-BROWSE-SW = 'Y'
03094          MOVE 'N'                TO  WS-BROWSE-SW
03095          
      * EXEC CICS ENDBR
03096 *             DATASET (FILE-ID-ERCKWK)
03097 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006458' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCKWK, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03098
03099      IF PI-NO-ENTRIES-DISPLAYED GREATER THAN ZEROS
03100         MOVE PI-STUB-KEY (1)        TO PI-ERCKWK-KEY.
03101
03102      MOVE ER-0000                TO EMI-ERROR.
03103      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03104      GO TO 5000-DISPLAY-PAYEE.
03105
03106  7580-VOID-ERROR.
03107
03108      
      * EXEC CICS ENDBR
03109 *         DATASET (FILE-ID-ERCMCK)
03110 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006471' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERCMCK, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03111
03112      MOVE ER-3169                TO EMI-ERROR.
03113      MOVE -1                     TO CARL.
03114      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03115      GO TO 8200-SEND-DATAONLY.
03116
03117      EJECT
03118
03119 ******************************************************************
03120 *                                                                *
03121 *             S E N D    I N I T I A L   M A P                   *
03122 *                                                                *
03123 ******************************************************************
03124
03125  8100-SEND-INITIAL-MAP.
03126
03127      MOVE WS-CURRENT-DT          TO DATEO.
03128      MOVE EIBTIME                TO TIME-IN.
03129      MOVE TIME-OUT               TO TIMEO.
03130      MOVE -1                     TO MAINTL.
03131
03132      MOVE EMI-MESSAGE-AREA (1)   TO ERMESG1O.
03133      MOVE EMI-MESSAGE-AREA (2)   TO ERMESG2O.
03134
03135
03136      MOVE AL-SANON               TO  PFKEY8A.
03137
03138      
      * EXEC CICS SEND
03139 *        MAP      (EL636A)
03140 *        MAPSET   (MAPSET-EL636S)
03141 *        FROM     (EL636AI)
03142 *        ERASE
03143 *        CURSOR
03144 *    END-EXEC.
           MOVE LENGTH OF
            EL636AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00006501' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL636A, 
                 EL636AI, 
                 DFHEIV12, 
                 MAPSET-EL636S, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03145
03146      GO TO 9100-RETURN-TRAN.
03147
03148      EJECT
03149
03150 ******************************************************************
03151 *                                                                *
03152 *              S E N D    D A T A O N L Y                        *
03153 *                                                                *
03154 ******************************************************************
03155
03156  8200-SEND-DATAONLY.
03157
03158      MOVE WS-CURRENT-DT          TO DATEO.
03159      MOVE EIBTIME                TO TIME-IN.
03160      MOVE TIME-OUT               TO TIMEO.
03161
03162      MOVE EMI-MESSAGE-AREA (1)   TO ERMESG1O.
03163      MOVE EMI-MESSAGE-AREA (2)   TO ERMESG2O.
03164
03165      
      * EXEC CICS SEND
03166 *         MAP      (EL636A)
03167 *         MAPSET   (MAPSET-EL636S)
03168 *         FROM     (EL636AI)
03169 *         DATAONLY
03170 *         CURSOR
03171 *    END-EXEC.
           MOVE LENGTH OF
            EL636AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00006528' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EL636A, 
                 EL636AI, 
                 DFHEIV12, 
                 MAPSET-EL636S, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03172
03173      GO TO 9100-RETURN-TRAN.
03174
03175      EJECT
03176
03177  8300-SEND-TEXT.
03178      
      * EXEC CICS SEND TEXT
03179 *        FROM     (LOGOFF-TEXT)
03180 *        LENGTH   (LOGOFF-LENGTH)
03181 *        ERASE
03182 *        FREEKB
03183 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00006541' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353431' TO DFHEIV0(25:11)
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
           
03184
03185      
      * EXEC CICS RETURN
03186 *    END-EXEC.
      *    MOVE '.(                    &   #00006548' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03187
03188
03189  8400-LOG-JOURNAL-RECORD.
03190      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.
03191      MOVE THIS-PGM                TO JP-PROGRAM-ID.
03192
03193 *    EXEC CICS JOURNAL
03194 *        JFILEID     (PI-JOURNAL-FILE-ID)
03195 *        JTYPEID     ('EL')
03196 *        FROM        (JOURNAL-RECORD)
03197 *        LENGTH      (WS-JOURNAL-RECORD-LENGTH)
03198 *        END-EXEC.
03199
03200  8500-DATE-CONVERT.
03201      
      * EXEC CICS LINK
03202 *        PROGRAM  (LINK-ELDATCV)
03203 *        COMMAREA (DATE-CONVERSION-DATA)
03204 *        LENGTH   (DC-COMM-LENGTH)
03205 *    END-EXEC.
      *    MOVE '."C                   ''   #00006564' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LINK-ELDATCV, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03206
03207  8500-EXIT.
03208      EXIT.
03209
03210      EJECT
03211
03212  8600-DEEDIT.
03213
03214      
      * EXEC CICS BIF DEEDIT
03215 *         FIELD   (DEEDIT-FIELD)
03216 *         LENGTH  (12)
03217 *    END-EXEC.
           MOVE 12
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006577' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03218
03219  8800-UNAUTHORIZED-ACCESS.
03220      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
03221      GO TO 8300-SEND-TEXT.
03222
03223  8810-PF23.
03224      MOVE EIBAID                 TO PI-ENTRY-CD-1.
03225      MOVE XCTL-EL005             TO PGM-NAME.
03226      GO TO 9300-XCTL.
03227
03228  8820-PF08.
03229      MOVE PI-LAST-STUB-KEY       TO PI-ERCKWK-KEY.
03230      MOVE ZEROS                  TO PI-ERCKWK-SEQ-NO.
03231      MOVE EIBAID                 TO PI-ENTRY-CD-1.
03232      MOVE XCTL-EL6361            TO PGM-NAME.
03233      GO TO 9300-XCTL.
03234
03235  9200-RETURN-MAIN-MENU.
03236      MOVE XCTL-EL626             TO PGM-NAME.
03237      GO TO 9300-XCTL.
03238
03239  9100-RETURN-TRAN.
03240
03241      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
03242      MOVE '852A'                 TO PI-CURRENT-SCREEN-NO.
03243
03244      
      * EXEC CICS RETURN
03245 *        TRANSID    (TRANS-EXJA)
03246 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
03247 *        LENGTH     (PI-COMM-LENGTH)
03248 *    END-EXEC.
      *    MOVE '.(CT                  &   #00006607' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-EXJA, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03249
03250  9300-XCTL.
03251      
      * EXEC CICS XCTL
03252 *        PROGRAM    (PGM-NAME)
03253 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
03254 *        LENGTH     (PI-COMM-LENGTH)
03255 *    END-EXEC.
      *    MOVE '.$C                   $   #00006614' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03256
03257  9400-CLEAR.
03258      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME
03259      GO TO 9300-XCTL.
03260
03261  9500-PF12.
03262      MOVE XCTL-EL010             TO PGM-NAME.
03263      GO TO 9300-XCTL.
03264
03265  9600-PGMID-ERROR.
03266
03267      
      * EXEC CICS HANDLE CONDITION
03268 *        PGMIDERR    (8300-SEND-TEXT)
03269 *    END-EXEC.
      *    MOVE '"$L                   ! > #00006630' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3E20233030303036363330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03270
03271      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
03272      MOVE ' '                    TO PI-ENTRY-CD-1.
03273      MOVE XCTL-EL005             TO PGM-NAME.
03274      MOVE PGM-NAME               TO LOGOFF-PGM.
03275      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
03276      GO TO 9300-XCTL.
03277
03278  9900-ERROR-FORMAT.
03279
03280      IF NOT EMI-ERRORS-COMPLETE
03281          MOVE LINK-EL001         TO PGM-NAME
03282          
      * EXEC CICS LINK
03283 *            PROGRAM    (PGM-NAME)
03284 *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
03285 *            LENGTH     (EMI-COMM-LENGTH)
03286 *        END-EXEC.
      *    MOVE '."C                   ''   #00006645' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03287
03288  9900-EXIT.
03289      EXIT.
03290
03291  9990-ABEND.
03292      MOVE LINK-EL004             TO PGM-NAME.
03293      MOVE DFHEIBLK               TO EMI-LINE1.
03294      
      * EXEC CICS LINK
03295 *        PROGRAM   (PGM-NAME)
03296 *        COMMAREA  (EMI-LINE1)
03297 *        LENGTH    (72)
03298 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00006657' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03299
03300      MOVE -1                     TO PFENTERL.
03301
03302      GO TO 8200-SEND-DATAONLY.
03303
03304      EJECT
03305
03306  9995-SECURITY-VIOLATION.
03307 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00006687' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363837' TO DFHEIV0(25:11)
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
03308
03309  9995-EXIT.
03310      EXIT.
03311

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL636' TO DFHEIV1
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
               GO TO 1420-FIRST-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 1440-SECOND-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 1440-SECOND-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 1485-NO-COMP-MSTR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 1680-NOT-FOUND,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 2060-DUPLICATE-CHECK
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 2080-DUPLICATE-CHECK
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 2095-HEADER-ROLLBACK
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 3010-CHANGE-ENTRIES
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 3090-CHECKS-NOTFND,
                     3090-CHECKS-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 3280-HEADER-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 3270-DUP-ENTRY
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 3380-HEADER-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 3490-FINISHED,
                     3490-FINISHED
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 18
               GO TO 4080-PAYEE-NOTFND,
                     4070-PAYEE-DELETED
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 19
               GO TO 5090-CHECKS-NOTFND,
                     5090-CHECKS-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 20
               GO TO 5070-CHECK-PROCESSED,
                     5060-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 21
               GO TO 5095-CHECKS-NOTFND,
                     5095-CHECKS-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 22
               GO TO 6060-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 23
               GO TO 6160-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 24
               GO TO 7090-CHECKS-NOTFND,
                     7090-CHECKS-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 25
               GO TO 7070-CHECK-PROCESSED,
                     7070-CHECK-PROCESSED
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 26
               GO TO 7190-EXIT,
                     7190-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 27
               GO TO 7480-DUPREC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 28
               GO TO 7550-VOID-WORK-RECS,
                     7550-VOID-WORK-RECS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 29
               GO TO 7570-COMM-CHECK-VOIDED,
                     7570-COMM-CHECK-VOIDED
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 30
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL636' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
