00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL6351.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 09:52:13.
00007 *                            VMOD=2.031
00008 *
00008 *
00009 *AUTHOR.        LOGIC,INC.
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
00025 *        TRANSACTION - EXJ5 - COMPENSATION PAYMENTS/ADJUSTMENTS.
00026
00027  ENVIRONMENT DIVISION.
00028  DATA DIVISION.
00029  EJECT
00030  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00031  77  FILLER  PIC X(32)  VALUE '********************************'.
00032  77  FILLER  PIC X(32)  VALUE '*    EL6351 WORKING STORAGE    *'.
00033  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.031 *********'.
00034
00035 *    COPY ELCSCTM.
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
00036 *    COPY ELCSCRTY.
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
00037 *    COPY MPCSCRT.
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
00038
00039     EJECT
00040
00041  01  STANDARD-AREAS.
00042      12  GETMAIN-SPACE       PIC  X          VALUE SPACE.
00043      12  MAP-NAME            PIC  X(8)       VALUE 'EL635B '.
00044      12  MAPSET-NAME         PIC  X(8)       VALUE 'EL6351S'.
00045      12  SCREEN-NUMBER       PIC  X(4)       VALUE '633B'.
00046      12  TRANS-ID            PIC  X(4)       VALUE 'EXJ5'.
00047      12  THIS-PGM            PIC  X(8)       VALUE 'EL6351'.
00048      12  PGM-NAME            PIC  X(8).
00049      12  TIME-IN             PIC S9(7).
00050      12  TIME-OUT-R  REDEFINES  TIME-IN.
00051          16  FILLER          PIC  X.
00052          16  TIME-OUT        PIC  9(2)V9(2).
00053          16  FILLER          PIC  X(2).
00054      12  XCTL-005            PIC  X(8)       VALUE 'EL005'.
00055      12  XCTL-010            PIC  X(8)       VALUE 'EL010'.
00056      12  XCTL-EL126          PIC  X(8)       VALUE 'EL126'.
00057      12  XCTL-EL626          PIC  X(8)       VALUE 'EL626'.
00058      12  XCTL-EM626          PIC  X(8)       VALUE 'EM626'.
00059      12  XCTL-GL800          PIC  X(8)       VALUE 'GL800'.
00060      12  XCTL-652            PIC  X(8)       VALUE 'EL652'.
00061      12  LINK-001            PIC  X(8)       VALUE 'EL001'.
00062      12  LINK-004            PIC  X(8)       VALUE 'EL004'.
00063      12  LINK-CLDATCV        PIC  X(8)       VALUE 'ELDATCV'.
00064      12  PYAJ-FILE-ID        PIC  X(8)       VALUE 'ERPYAJ'.
00065      12  MPPYAJ-FILE-ID      PIC  X(8)       VALUE 'MPPYAJ'.
00066      12  MPPRCN-FILE-ID      PIC  X(8)       VALUE 'MPPRCN'.
00067      12  COMP-FILE-ID        PIC  X(8)       VALUE 'ERCOMP'.
00068      12  RECV-FILE-ID        PIC  X(8)       VALUE 'ERRECV'.
00069      12  WS-CURRENT-DT       PIC  X(8)       VALUE SPACES.
00070      12  WS-CURRENT-MDY      PIC  X(6)       VALUE SPACES.
00071      12  WS-CURRENT-BIN-DT   PIC  X(2)       VALUE SPACES.
00072      12  WS-COMMENT-FULL.
00073          16  WS-COMMENT-24-POS    PIC  X(24) VALUE SPACES.
00074          16  FILLER               PIC  X(06) VALUE SPACES.
00075      12  WORK-SEQ-NO         PIC S9(9)                  COMP-3.
00076      12  CHECK-REC-TYPE      PIC  X          VALUE SPACE.
00077          88  VALID-REC-TYPE                  VALUE  'R' 'D' 'C'
00078                                                     'S' 'T' 'U'
00079                                                     'X' 'Y' 'Z'
00080                                                     'F'.
00081          88  MON-VALID-REC-TYPE              VALUE  'R' 'D' 'C'
00082                                                     'Z'.
00083          88  ANL-VALID-REC-TYPE              VALUE  'R' 'D' 'C'
00084                                                     'S' 'T' 'U'
00085                                                     'X' 'Y' 'Z'.
00086          88  NCL-VALID-REC-TYPE              VALUE  'R' 'D' 'C'
00087                                                     'X' 'Y' 'Z'.
00088      12  ERROR-3183          PIC  X          VALUE SPACE.
00089          88  ER-3183-OCCURRED                VALUE 'Y'.
00090
00091      12  DEEDIT-FIELD            PIC X(11).
00092      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD PIC S9(11).
00093      12  DATE-TEST-AREA          PIC 9(6).
00094      12  DATE-TEST-AREA-R  REDEFINES DATE-TEST-AREA.
00095          16  DATE-TEST-MM        PIC 99.
00096          16  DATE-TEST-DD        PIC 99.
00097          16  DATE-TEST-YY        PIC 99.
00098      12  DIVIDE-RESULT           PIC 99.
00099      12  DIVIDE-REMAINDER        PIC 9.
00100      12  WS-EOM-DTS     OCCURS 5 TIMES
00101                                 INDEXED BY MINDEX.
00102          16  WS-EOM-DT           PIC XX.
00103      12  WS-INP-DTS     OCCURS 5 TIMES
00104                                 INDEXED BY DINDEX.
00105          16  WS-INP-DT           PIC XX.
00106      12  WS-EDITED-AMTS OCCURS 6 TIMES
00107                                 INDEXED BY WS-INDX.
00108          16  WS-EDITED-AMT       PIC S9(9)V99.
00109
00110      12  WS-ENTRY-AMT        PIC S9(7)V99      VALUE ZEROS.
00111      12  WS-PREV-PF5         PIC X.
00112      12  WS-ACCEPT.
00113          16  WS-ACCEPT-1   OCCURS 5 TIMES INDEXED BY W-INDX
00114                              PIC  X.
00115
00116  01  ACCESS-KEYS.
00117      12  ERPYAJ-KEY.
00118          16  PYAJ-COMP-CD        PIC  X      VALUE SPACE.
00119          16  PYAJ-CARRIER        PIC  X      VALUE SPACES.
00120          16  PYAJ-GROUPING       PIC  X(6)   VALUE SPACES.
00121          16  PYAJ-FIN-RESP       PIC  X(10)  VALUE SPACES.
00122          16  PYAJ-ACCOUNT        PIC  X(10)  VALUE SPACES.
00123          16  PYAJ-FILE-SEQ-NO    PIC S9(8)   VALUE +0   COMP.
00124          16  PYAJ-RECORD-TYPE    PIC  X      VALUE SPACES.
00125
00126      12  ERPYAJ-RECORD-LENGTH    PIC S9(4)   VALUE +200 COMP.
00127      12  ERPYAJ-JOURNAL-LENGTH   PIC S9(4)   VALUE +223 COMP.
00128
00129      12  ERCOMP-KEY.
00130          16  COMP-COMP-CD        PIC  X      VALUE SPACE.
00131          16  COMP-CARRIER        PIC  X      VALUE SPACES.
00132          16  COMP-GROUPING       PIC  X(6)   VALUE SPACES.
00133          16  COMP-FIN-RESP       PIC  X(10)  VALUE SPACES.
00134          16  COMP-ACCOUNT        PIC  X(10)  VALUE SPACES.
00135          16  COMP-RECORD-TYPE    PIC  X      VALUE SPACES.
00136
00137      12  ERCOMP-RECORD-LENGTH    PIC S9(4)   VALUE +700 COMP.
00138
00139      12  ERRECV-KEY.
00140          16  RECV-COMP-CD        PIC X       VALUE SPACES.
00141          16  RECV-TYPE           PIC X       VALUE SPACES.
00142          16  RECV-CARRIER        PIC X       VALUE SPACES.
00143          16  RECV-GROUPING       PIC X(6)    VALUE SPACES.
00144          16  RECV-BAL-LVL        PIC X       VALUE SPACES.
00145          16  RECV-ENTRY-TYPE     PIC X       VALUE SPACES.
00146          16  RECV-FIN-RESP       PIC X(10)   VALUE SPACES.
00147          16  RECV-ACCOUNT        PIC X(10)   VALUE SPACES.
00148          16  RECV-INVOICE        PIC X(6)    VALUE SPACES.
00149          16  RECV-REFERENCE      PIC X(12)   VALUE SPACES.
00150          16  RECV-RESPONSIBLE    PIC X       VALUE SPACES.
00151          16  RECV-RECORD-TYPE    PIC X       VALUE SPACES.
00152          16  RECV-RECORD-SEQ     PIC S9(4)   VALUE +0 COMP.
00153
00154      12  MPPRCN-KEY.
00155          16  MPPRCN-COMPANY-CD       PIC X       VALUE SPACES.
00156          16  MPPRCN-INVOICE          PIC X(6)    VALUE SPACES.
00157          16  MPPRCN-RECORD-SEQU      PIC S9(7)   VALUE +0  COMP-3.
00158
00159  EJECT
00160  01  ERROR-NUMBERS.
00161      12  ER-0000             PIC  X(4)       VALUE '0000'.
00162      12  ER-0008             PIC  X(4)       VALUE '0008'.
00163      12  ER-0029             PIC  X(4)       VALUE '0029'.
00164      12  ER-0070             PIC  X(4)       VALUE '0070'.
00165      12  ER-0194             PIC  X(4)       VALUE '0194'.
00166      12  ER-0195             PIC  X(4)       VALUE '0195'.
00167      12  ER-0197             PIC  X(4)       VALUE '0197'.
00168      12  ER-0587             PIC  X(4)       VALUE '0587'.
00169      12  ER-0714             PIC  X(4)       VALUE '0714'.
00170      12  ER-0761             PIC  X(4)       VALUE '0761'.
00171      12  ER-2232             PIC  X(4)       VALUE '2232'.
00172      12  ER-2233             PIC  X(4)       VALUE '2233'.
00173      12  ER-2234             PIC  X(4)       VALUE '2234'.
00174      12  ER-2235             PIC  X(4)       VALUE '2235'.
00175      12  ER-2236             PIC  X(4)       VALUE '2236'.
00176      12  ER-2245             PIC  X(4)       VALUE '2245'.
00177      12  ER-2562             PIC  X(4)       VALUE '2562'.
00178      12  ER-2587             PIC  X(4)       VALUE '2587'.
00179      12  ER-2588             PIC  X(4)       VALUE '2588'.
00180      12  ER-2595             PIC  X(4)       VALUE '2595'.
00181      12  ER-2596             PIC  X(4)       VALUE '2596'.
00182      12  ER-7806             PIC  X(4)       VALUE '7806'.
00183      12  ER-9296             PIC  X(4)       VALUE '9296'.
00184      12  ER-3146             PIC  X(4)       VALUE '3146'.
00185      12  ER-3172             PIC  X(4)       VALUE '3172'.
00186      12  ER-3175             PIC  X(4)       VALUE '3175'.
00187      12  ER-3177             PIC  X(4)       VALUE '3177'.
00188      12  ER-3178             PIC  X(4)       VALUE '3178'.
00189      12  ER-3179             PIC  X(4)       VALUE '3179'.
00190      12  ER-3180             PIC  X(4)       VALUE '3180'.
00191      12  ER-3183             PIC  X(4)       VALUE '3183'.
00192      12  ER-3184             PIC  X(4)       VALUE '3184'.
00193      12  ER-9280             PIC  X(4)       VALUE '9280'.
00194      12  ER-9094             PIC  X(4)       VALUE '9094'.
00195      12  ER-9095             PIC  X(4)       VALUE '9095'.
00196      12  ER-9179             PIC  X(4)       VALUE '9179'.
00197
00198  EJECT
00199 *    COPY ELCDATE.
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
00200  EJECT
00201 *    COPY ELCLOGOF.
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
00202  EJECT
00203 *    COPY ELCATTR.
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
00204  EJECT
00205 *    COPY ELCEMIB.
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
00206  EJECT
00207 *    COPY ELCINTF.
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
00208      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.
00209          16  PI-PYAJ-FILE-SW         PIC  X.
00210              88  END-OF-ACCT                 VALUE 'A'.
00211              88  END-OF-FILE                 VALUE 'X'.
00212              88  NO-RECORDS                  VALUE 'Y'.
00213              88  NOT-OPEN                    VALUE 'Z'.
00214          16  PI-PREV-FUNCTION        PIC  X.
00215          16  PI-SAV-FUNCTION         PIC  X.
00216          16  PI-SEQ-NOS.
00217              20  FILLER  OCCURS  5 TIMES
00218                              INDEXED BY NDX.
00219                  24  PI-REC-TYPE     PIC  X.
00220                  24  PI-FILE-SEQ-NO  PIC S9(8).
00221          16  PI-SAV-ENDING-PYAJ-KEY.
00222              20  PI-SAV-COMP-CD      PIC  X.
00223              20  PI-SAV-CARRIER      PIC  X.
00224              20  PI-SAV-GROUPING     PIC  X(3).
00225              20  PI-SAV-FIN-RESP     PIC  X(6).
00226              20  PI-SAV-ACCOUNT      PIC  X(6).
00227              20  PI-SAV-FILE-SEQ-NO  PIC S9(8)          COMP.
00228              20  PI-SAV-RECORD-TYPE  PIC  X.
00229          16  PI-ACCEPT-DESC          PIC  X(6).
00230          16  PI-ACCEPT               PIC  X(5).
00231          16  PI-SAVE-COMP-BAL-CODE   PIC  X.
00232          16  PI-SEQ-FIRST-TIME-SW    PIC  X.
00233              88  PI-SEQ-FST-TIME       VALUE 'Y'.
00234          16  FILLER                  PIC  X(557).
00235  EJECT
00236 *    COPY ELCJPFX.
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
00237                              PIC  X(223).
00238  EJECT
00239 *    COPY ELCAID.
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
00240
00241  01  FILLER    REDEFINES DFHAID.
00242      12  FILLER              PIC  X(8).
00243      12  PF-VALUES           PIC  X          OCCURS 2 TIMES.
00244  EJECT
00245 *    COPY EL6351S.
       01  EL635BI.
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
           05  CAR1L PIC S9(0004) COMP.
           05  CAR1F PIC  X(0001).
           05  FILLER REDEFINES CAR1F.
               10  CAR1A PIC  X(0001).
           05  CAR1I PIC  X(0001).
      *    -------------------------------
           05  GRP1L PIC S9(0004) COMP.
           05  GRP1F PIC  X(0001).
           05  FILLER REDEFINES GRP1F.
               10  GRP1A PIC  X(0001).
           05  GRP1I PIC  X(0006).
      *    -------------------------------
           05  FRESP1L PIC S9(0004) COMP.
           05  FRESP1F PIC  X(0001).
           05  FILLER REDEFINES FRESP1F.
               10  FRESP1A PIC  X(0001).
           05  FRESP1I PIC  X(0010).
      *    -------------------------------
           05  ACTHDG1L PIC S9(0004) COMP.
           05  ACTHDG1F PIC  X(0001).
           05  FILLER REDEFINES ACTHDG1F.
               10  ACTHDG1A PIC  X(0001).
           05  ACTHDG1I PIC  X(0005).
      *    -------------------------------
           05  ACCT1L PIC S9(0004) COMP.
           05  ACCT1F PIC  X(0001).
           05  FILLER REDEFINES ACCT1F.
               10  ACCT1A PIC  X(0001).
           05  ACCT1I PIC  X(0010).
      *    -------------------------------
           05  TYPE1L PIC S9(0004) COMP.
           05  TYPE1F PIC  X(0001).
           05  FILLER REDEFINES TYPE1F.
               10  TYPE1A PIC  X(0001).
           05  TYPE1I PIC  X(0001).
      *    -------------------------------
           05  AMT1L PIC S9(0004) COMP.
           05  AMT1F PIC  X(0001).
           05  FILLER REDEFINES AMT1F.
               10  AMT1A PIC  X(0001).
           05  AMT1I PIC  X(0011).
      *    -------------------------------
           05  COMM1L PIC S9(0004) COMP.
           05  COMM1F PIC  X(0001).
           05  FILLER REDEFINES COMM1F.
               10  COMM1A PIC  X(0001).
           05  COMM1I PIC  X(0030).
      *    -------------------------------
           05  REF1L PIC S9(0004) COMP.
           05  REF1F PIC  X(0001).
           05  FILLER REDEFINES REF1F.
               10  REF1A PIC  X(0001).
           05  REF1I PIC  X(0012).
      *    -------------------------------
           05  INV1L PIC S9(0004) COMP.
           05  INV1F PIC  X(0001).
           05  FILLER REDEFINES INV1F.
               10  INV1A PIC  X(0001).
           05  INV1I PIC  X(0006).
      *    -------------------------------
           05  APLID1L PIC S9(0004) COMP.
           05  APLID1F PIC  X(0001).
           05  FILLER REDEFINES APLID1F.
               10  APLID1A PIC  X(0001).
           05  APLID1I PIC  X(0001).
      *    -------------------------------
           05  CR1L PIC S9(0004) COMP.
           05  CR1F PIC  X(0001).
           05  FILLER REDEFINES CR1F.
               10  CR1A PIC  X(0001).
           05  CR1I PIC  X(0014).
      *    -------------------------------
           05  DR1L PIC S9(0004) COMP.
           05  DR1F PIC  X(0001).
           05  FILLER REDEFINES DR1F.
               10  DR1A PIC  X(0001).
           05  DR1I PIC  X(0014).
      *    -------------------------------
           05  EOM1L PIC S9(0004) COMP.
           05  EOM1F PIC  X(0001).
           05  FILLER REDEFINES EOM1F.
               10  EOM1A PIC  X(0001).
           05  EOM1I PIC  X(0006).
      *    -------------------------------
           05  INDTE1L PIC S9(0004) COMP.
           05  INDTE1F PIC  X(0001).
           05  FILLER REDEFINES INDTE1F.
               10  INDTE1A PIC  X(0001).
           05  INDTE1I PIC  X(0006).
      *    -------------------------------
           05  CAR2L PIC S9(0004) COMP.
           05  CAR2F PIC  X(0001).
           05  FILLER REDEFINES CAR2F.
               10  CAR2A PIC  X(0001).
           05  CAR2I PIC  X(0001).
      *    -------------------------------
           05  GRP2L PIC S9(0004) COMP.
           05  GRP2F PIC  X(0001).
           05  FILLER REDEFINES GRP2F.
               10  GRP2A PIC  X(0001).
           05  GRP2I PIC  X(0006).
      *    -------------------------------
           05  FRESP2L PIC S9(0004) COMP.
           05  FRESP2F PIC  X(0001).
           05  FILLER REDEFINES FRESP2F.
               10  FRESP2A PIC  X(0001).
           05  FRESP2I PIC  X(0010).
      *    -------------------------------
           05  ACTHDG2L PIC S9(0004) COMP.
           05  ACTHDG2F PIC  X(0001).
           05  FILLER REDEFINES ACTHDG2F.
               10  ACTHDG2A PIC  X(0001).
           05  ACTHDG2I PIC  X(0005).
      *    -------------------------------
           05  ACCT2L PIC S9(0004) COMP.
           05  ACCT2F PIC  X(0001).
           05  FILLER REDEFINES ACCT2F.
               10  ACCT2A PIC  X(0001).
           05  ACCT2I PIC  X(0010).
      *    -------------------------------
           05  TYPE2L PIC S9(0004) COMP.
           05  TYPE2F PIC  X(0001).
           05  FILLER REDEFINES TYPE2F.
               10  TYPE2A PIC  X(0001).
           05  TYPE2I PIC  X(0001).
      *    -------------------------------
           05  AMT2L PIC S9(0004) COMP.
           05  AMT2F PIC  X(0001).
           05  FILLER REDEFINES AMT2F.
               10  AMT2A PIC  X(0001).
           05  AMT2I PIC  X(0011).
      *    -------------------------------
           05  COMM2L PIC S9(0004) COMP.
           05  COMM2F PIC  X(0001).
           05  FILLER REDEFINES COMM2F.
               10  COMM2A PIC  X(0001).
           05  COMM2I PIC  X(0030).
      *    -------------------------------
           05  REF2L PIC S9(0004) COMP.
           05  REF2F PIC  X(0001).
           05  FILLER REDEFINES REF2F.
               10  REF2A PIC  X(0001).
           05  REF2I PIC  X(0012).
      *    -------------------------------
           05  INV2L PIC S9(0004) COMP.
           05  INV2F PIC  X(0001).
           05  FILLER REDEFINES INV2F.
               10  INV2A PIC  X(0001).
           05  INV2I PIC  X(0006).
      *    -------------------------------
           05  APLID2L PIC S9(0004) COMP.
           05  APLID2F PIC  X(0001).
           05  FILLER REDEFINES APLID2F.
               10  APLID2A PIC  X(0001).
           05  APLID2I PIC  X(0001).
      *    -------------------------------
           05  CR2L PIC S9(0004) COMP.
           05  CR2F PIC  X(0001).
           05  FILLER REDEFINES CR2F.
               10  CR2A PIC  X(0001).
           05  CR2I PIC  X(0014).
      *    -------------------------------
           05  DR2L PIC S9(0004) COMP.
           05  DR2F PIC  X(0001).
           05  FILLER REDEFINES DR2F.
               10  DR2A PIC  X(0001).
           05  DR2I PIC  X(0014).
      *    -------------------------------
           05  EOM2L PIC S9(0004) COMP.
           05  EOM2F PIC  X(0001).
           05  FILLER REDEFINES EOM2F.
               10  EOM2A PIC  X(0001).
           05  EOM2I PIC  X(0006).
      *    -------------------------------
           05  INDTE2L PIC S9(0004) COMP.
           05  INDTE2F PIC  X(0001).
           05  FILLER REDEFINES INDTE2F.
               10  INDTE2A PIC  X(0001).
           05  INDTE2I PIC  X(0006).
      *    -------------------------------
           05  CAR3L PIC S9(0004) COMP.
           05  CAR3F PIC  X(0001).
           05  FILLER REDEFINES CAR3F.
               10  CAR3A PIC  X(0001).
           05  CAR3I PIC  X(0001).
      *    -------------------------------
           05  GRP3L PIC S9(0004) COMP.
           05  GRP3F PIC  X(0001).
           05  FILLER REDEFINES GRP3F.
               10  GRP3A PIC  X(0001).
           05  GRP3I PIC  X(0006).
      *    -------------------------------
           05  FRESP3L PIC S9(0004) COMP.
           05  FRESP3F PIC  X(0001).
           05  FILLER REDEFINES FRESP3F.
               10  FRESP3A PIC  X(0001).
           05  FRESP3I PIC  X(0010).
      *    -------------------------------
           05  ACTHDG3L PIC S9(0004) COMP.
           05  ACTHDG3F PIC  X(0001).
           05  FILLER REDEFINES ACTHDG3F.
               10  ACTHDG3A PIC  X(0001).
           05  ACTHDG3I PIC  X(0005).
      *    -------------------------------
           05  ACCT3L PIC S9(0004) COMP.
           05  ACCT3F PIC  X(0001).
           05  FILLER REDEFINES ACCT3F.
               10  ACCT3A PIC  X(0001).
           05  ACCT3I PIC  X(0010).
      *    -------------------------------
           05  TYPE3L PIC S9(0004) COMP.
           05  TYPE3F PIC  X(0001).
           05  FILLER REDEFINES TYPE3F.
               10  TYPE3A PIC  X(0001).
           05  TYPE3I PIC  X(0001).
      *    -------------------------------
           05  AMT3L PIC S9(0004) COMP.
           05  AMT3F PIC  X(0001).
           05  FILLER REDEFINES AMT3F.
               10  AMT3A PIC  X(0001).
           05  AMT3I PIC  X(0011).
      *    -------------------------------
           05  COMM3L PIC S9(0004) COMP.
           05  COMM3F PIC  X(0001).
           05  FILLER REDEFINES COMM3F.
               10  COMM3A PIC  X(0001).
           05  COMM3I PIC  X(0030).
      *    -------------------------------
           05  REF3L PIC S9(0004) COMP.
           05  REF3F PIC  X(0001).
           05  FILLER REDEFINES REF3F.
               10  REF3A PIC  X(0001).
           05  REF3I PIC  X(0012).
      *    -------------------------------
           05  INV3L PIC S9(0004) COMP.
           05  INV3F PIC  X(0001).
           05  FILLER REDEFINES INV3F.
               10  INV3A PIC  X(0001).
           05  INV3I PIC  X(0006).
      *    -------------------------------
           05  APLID3L PIC S9(0004) COMP.
           05  APLID3F PIC  X(0001).
           05  FILLER REDEFINES APLID3F.
               10  APLID3A PIC  X(0001).
           05  APLID3I PIC  X(0001).
      *    -------------------------------
           05  CR3L PIC S9(0004) COMP.
           05  CR3F PIC  X(0001).
           05  FILLER REDEFINES CR3F.
               10  CR3A PIC  X(0001).
           05  CR3I PIC  X(0014).
      *    -------------------------------
           05  DR3L PIC S9(0004) COMP.
           05  DR3F PIC  X(0001).
           05  FILLER REDEFINES DR3F.
               10  DR3A PIC  X(0001).
           05  DR3I PIC  X(0014).
      *    -------------------------------
           05  EOM3L PIC S9(0004) COMP.
           05  EOM3F PIC  X(0001).
           05  FILLER REDEFINES EOM3F.
               10  EOM3A PIC  X(0001).
           05  EOM3I PIC  X(0006).
      *    -------------------------------
           05  INDTE3L PIC S9(0004) COMP.
           05  INDTE3F PIC  X(0001).
           05  FILLER REDEFINES INDTE3F.
               10  INDTE3A PIC  X(0001).
           05  INDTE3I PIC  X(0006).
      *    -------------------------------
           05  CAR4L PIC S9(0004) COMP.
           05  CAR4F PIC  X(0001).
           05  FILLER REDEFINES CAR4F.
               10  CAR4A PIC  X(0001).
           05  CAR4I PIC  X(0001).
      *    -------------------------------
           05  GRP4L PIC S9(0004) COMP.
           05  GRP4F PIC  X(0001).
           05  FILLER REDEFINES GRP4F.
               10  GRP4A PIC  X(0001).
           05  GRP4I PIC  X(0006).
      *    -------------------------------
           05  FRESP4L PIC S9(0004) COMP.
           05  FRESP4F PIC  X(0001).
           05  FILLER REDEFINES FRESP4F.
               10  FRESP4A PIC  X(0001).
           05  FRESP4I PIC  X(0010).
      *    -------------------------------
           05  ACTHDG4L PIC S9(0004) COMP.
           05  ACTHDG4F PIC  X(0001).
           05  FILLER REDEFINES ACTHDG4F.
               10  ACTHDG4A PIC  X(0001).
           05  ACTHDG4I PIC  X(0005).
      *    -------------------------------
           05  ACCT4L PIC S9(0004) COMP.
           05  ACCT4F PIC  X(0001).
           05  FILLER REDEFINES ACCT4F.
               10  ACCT4A PIC  X(0001).
           05  ACCT4I PIC  X(0010).
      *    -------------------------------
           05  TYPE4L PIC S9(0004) COMP.
           05  TYPE4F PIC  X(0001).
           05  FILLER REDEFINES TYPE4F.
               10  TYPE4A PIC  X(0001).
           05  TYPE4I PIC  X(0001).
      *    -------------------------------
           05  AMT4L PIC S9(0004) COMP.
           05  AMT4F PIC  X(0001).
           05  FILLER REDEFINES AMT4F.
               10  AMT4A PIC  X(0001).
           05  AMT4I PIC  X(0011).
      *    -------------------------------
           05  COMM4L PIC S9(0004) COMP.
           05  COMM4F PIC  X(0001).
           05  FILLER REDEFINES COMM4F.
               10  COMM4A PIC  X(0001).
           05  COMM4I PIC  X(0030).
      *    -------------------------------
           05  REF4L PIC S9(0004) COMP.
           05  REF4F PIC  X(0001).
           05  FILLER REDEFINES REF4F.
               10  REF4A PIC  X(0001).
           05  REF4I PIC  X(0012).
      *    -------------------------------
           05  INV4L PIC S9(0004) COMP.
           05  INV4F PIC  X(0001).
           05  FILLER REDEFINES INV4F.
               10  INV4A PIC  X(0001).
           05  INV4I PIC  X(0006).
      *    -------------------------------
           05  APLID4L PIC S9(0004) COMP.
           05  APLID4F PIC  X(0001).
           05  FILLER REDEFINES APLID4F.
               10  APLID4A PIC  X(0001).
           05  APLID4I PIC  X(0001).
      *    -------------------------------
           05  CR4L PIC S9(0004) COMP.
           05  CR4F PIC  X(0001).
           05  FILLER REDEFINES CR4F.
               10  CR4A PIC  X(0001).
           05  CR4I PIC  X(0014).
      *    -------------------------------
           05  DR4L PIC S9(0004) COMP.
           05  DR4F PIC  X(0001).
           05  FILLER REDEFINES DR4F.
               10  DR4A PIC  X(0001).
           05  DR4I PIC  X(0014).
      *    -------------------------------
           05  EOM4L PIC S9(0004) COMP.
           05  EOM4F PIC  X(0001).
           05  FILLER REDEFINES EOM4F.
               10  EOM4A PIC  X(0001).
           05  EOM4I PIC  X(0006).
      *    -------------------------------
           05  INDTE4L PIC S9(0004) COMP.
           05  INDTE4F PIC  X(0001).
           05  FILLER REDEFINES INDTE4F.
               10  INDTE4A PIC  X(0001).
           05  INDTE4I PIC  X(0006).
      *    -------------------------------
           05  CAR5L PIC S9(0004) COMP.
           05  CAR5F PIC  X(0001).
           05  FILLER REDEFINES CAR5F.
               10  CAR5A PIC  X(0001).
           05  CAR5I PIC  X(0001).
      *    -------------------------------
           05  GRP5L PIC S9(0004) COMP.
           05  GRP5F PIC  X(0001).
           05  FILLER REDEFINES GRP5F.
               10  GRP5A PIC  X(0001).
           05  GRP5I PIC  X(0006).
      *    -------------------------------
           05  FRESP5L PIC S9(0004) COMP.
           05  FRESP5F PIC  X(0001).
           05  FILLER REDEFINES FRESP5F.
               10  FRESP5A PIC  X(0001).
           05  FRESP5I PIC  X(0010).
      *    -------------------------------
           05  ACTHDG5L PIC S9(0004) COMP.
           05  ACTHDG5F PIC  X(0001).
           05  FILLER REDEFINES ACTHDG5F.
               10  ACTHDG5A PIC  X(0001).
           05  ACTHDG5I PIC  X(0005).
      *    -------------------------------
           05  ACCT5L PIC S9(0004) COMP.
           05  ACCT5F PIC  X(0001).
           05  FILLER REDEFINES ACCT5F.
               10  ACCT5A PIC  X(0001).
           05  ACCT5I PIC  X(0010).
      *    -------------------------------
           05  TYPE5L PIC S9(0004) COMP.
           05  TYPE5F PIC  X(0001).
           05  FILLER REDEFINES TYPE5F.
               10  TYPE5A PIC  X(0001).
           05  TYPE5I PIC  X(0001).
      *    -------------------------------
           05  AMT5L PIC S9(0004) COMP.
           05  AMT5F PIC  X(0001).
           05  FILLER REDEFINES AMT5F.
               10  AMT5A PIC  X(0001).
           05  AMT5I PIC  X(0011).
      *    -------------------------------
           05  COMM5L PIC S9(0004) COMP.
           05  COMM5F PIC  X(0001).
           05  FILLER REDEFINES COMM5F.
               10  COMM5A PIC  X(0001).
           05  COMM5I PIC  X(0030).
      *    -------------------------------
           05  REF5L PIC S9(0004) COMP.
           05  REF5F PIC  X(0001).
           05  FILLER REDEFINES REF5F.
               10  REF5A PIC  X(0001).
           05  REF5I PIC  X(0012).
      *    -------------------------------
           05  INV5L PIC S9(0004) COMP.
           05  INV5F PIC  X(0001).
           05  FILLER REDEFINES INV5F.
               10  INV5A PIC  X(0001).
           05  INV5I PIC  X(0006).
      *    -------------------------------
           05  APLID5L PIC S9(0004) COMP.
           05  APLID5F PIC  X(0001).
           05  FILLER REDEFINES APLID5F.
               10  APLID5A PIC  X(0001).
           05  APLID5I PIC  X(0001).
      *    -------------------------------
           05  CR5L PIC S9(0004) COMP.
           05  CR5F PIC  X(0001).
           05  FILLER REDEFINES CR5F.
               10  CR5A PIC  X(0001).
           05  CR5I PIC  X(0014).
      *    -------------------------------
           05  DR5L PIC S9(0004) COMP.
           05  DR5F PIC  X(0001).
           05  FILLER REDEFINES DR5F.
               10  DR5A PIC  X(0001).
           05  DR5I PIC  X(0014).
      *    -------------------------------
           05  EOM5L PIC S9(0004) COMP.
           05  EOM5F PIC  X(0001).
           05  FILLER REDEFINES EOM5F.
               10  EOM5A PIC  X(0001).
           05  EOM5I PIC  X(0006).
      *    -------------------------------
           05  INDTE5L PIC S9(0004) COMP.
           05  INDTE5F PIC  X(0001).
           05  FILLER REDEFINES INDTE5F.
               10  INDTE5A PIC  X(0001).
           05  INDTE5I PIC  X(0006).
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
           05  PFENTERL PIC S9(0004) COMP.
           05  PFENTERF PIC  X(0001).
           05  FILLER REDEFINES PFENTERF.
               10  PFENTERA PIC  X(0001).
           05  PFENTERI PIC  9(2).
       01  EL635BO REDEFINES EL635BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAR1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRP1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FRESP1O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTHDG1O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT1O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT1O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM1O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REF1O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INV1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APLID1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CR1O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DR1O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EOM1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INDTE1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAR2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRP2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FRESP2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTHDG2O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT2O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM2O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REF2O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INV2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APLID2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CR2O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DR2O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EOM2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INDTE2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAR3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRP3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FRESP3O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTHDG3O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT3O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT3O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM3O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REF3O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INV3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APLID3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CR3O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DR3O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EOM3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INDTE3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAR4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRP4O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FRESP4O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTHDG4O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT4O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT4O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM4O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REF4O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INV4O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APLID4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CR4O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DR4O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EOM4O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INDTE4O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAR5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRP5O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FRESP5O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTHDG5O PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT5O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT5O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMM5O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REF5O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INV5O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APLID5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CR5O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DR5O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EOM5O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INDTE5O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  X(0002).
      *    -------------------------------
00246
00247  01  MAP-EL635B   REDEFINES  EL635BI.
00248      12  FILLER                  PIC  X(31).
00249      12  DATA-AREA       OCCURS  5 TIMES
00250                              INDEXED BY INDX.
00251          16  CARR-LEN            PIC S9(4)              COMP.
00252          16  CARR-ATTRB          PIC  X.
00253          16  CARRIER             PIC  X.
00254          16  GRP-LEN             PIC S9(4)              COMP.
00255          16  GRP-ATTRB           PIC  X.
00256          16  GROUPING            PIC  X(6).
00257          16  FIN-LEN             PIC S9(4)              COMP.
00258          16  FIN-ATTRB           PIC  X.
00259          16  FIN-RESP            PIC  X(10).
00260          16  AHDG-LEN            PIC S9(4)              COMP.
00261          16  AHDG-ATTRB          PIC  X.
00262          16  AHDG                PIC  X(5).
00263          16  ACCT-LEN            PIC S9(4)              COMP.
00264          16  ACCT-ATTRB          PIC  X.
00265          16  ACCT                PIC  X(10).
00266          16  RTYPE-LEN           PIC S9(4)              COMP.
00267          16  RTYPE-ATTRB         PIC  X.
00268          16  RTYPE               PIC  X.
00269          16  AMT-LEN             PIC S9(4)              COMP.
00270          16  AMT-ATTRB           PIC  X.
00271          16  AMT                 PIC 9(11).
00272          16  AMTO  REDEFINES
00273              AMT                 PIC S9(9)V99.
00274          16  COMM-LEN            PIC S9(4)              COMP.
00275          16  COMM-ATTRB          PIC  X.
00276          16  COMM                PIC  X(30).
00277          16  NCL-COMM  REDEFINES  COMM.
00278              20  NCL-COMM-DTE.
00279                  24  NCL-MO      PIC  X(2).
00280                  24  NCL-DA      PIC  X(2).
00281                  24  NCL-YR      PIC  X(2).
00282              20  NCL-COMM-REST   PIC  X(24).
00283          16  REF-LEN             PIC S9(4)              COMP.
00284          16  REF-ATTRB           PIC  X.
00285          16  REF                 PIC  X(12).
00286          16  INVOICE-LEN         PIC S9(4)              COMP.
00287          16  INVOICE-ATTRB       PIC  X.
00288          16  INVOICE             PIC  X(6).
00289          16  APPLIED-LEN         PIC S9(4)              COMP.
00290          16  APPLIED-ATTRB       PIC  X.
00291          16  APPLIED             PIC  X.
00292          16  CREDIT-LEN          PIC S9(4)              COMP.
00293          16  CREDIT-ATTRB        PIC  X.
00294          16  CREDIT              PIC  X(14).
00295          16  DEBIT-LEN           PIC S9(4)              COMP.
00296          16  DEBIT-ATTRB         PIC  X.
00297          16  DEBIT               PIC  X(14).
00298          16  EOM-DT-LEN          PIC S9(4)              COMP.
00299          16  EOM-DT-ATTRB        PIC  X.
00300          16  EOM-DT              PIC X(6).
00301          16  INDATE-LEN          PIC S9(4)              COMP.
00302          16  INDATE-ATTRB        PIC  X.
00303          16  INDATE              PIC X(6).
00304      12  FILLER                  PIC X(169).
00305  EJECT
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
00307  01  DFHCOMMAREA             PIC  X(1024).
00308  EJECT
00309 *01 PARMLIST         COMP.
00310 *    12  FILLER              PIC S9(8).
00311 *    12  ERPYAJ-POINTER      PIC S9(8).
00312 *    12  ERCOMP-POINTER      PIC S9(8).
00313 *    12  ERRECV-POINTER      PIC S9(8).
00314 *    12  MPPRCN-POINTER      PIC S9(8).
00315
00316  EJECT
00317 *    COPY ERCPYAJ.
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
00318  EJECT
00319 *    COPY ERCCOMP.
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
00320  EJECT
00321 *    COPY ERCRECV.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCRECV.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.007                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = ACCOUNTS RECEIVABLE                       *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE =  300 RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ERRECV                         RKP=2,LEN=53   *
00013 *       ALTERNATE PATH1 = ERRECV2  (BY CO, CARR, GROUP, AGENT,   *
00014 *                                      EMO-DT, BAL, ENTRY-TYPE,  *
00015 *                                      F.R, INVOICE, REFERENCE,  *
00016 *                                      RESPONSIBILITY, REC TYPE, *
00017 *                                      SEQ. NO.)                 *
00018 *                                                RKP=64 ,LEN=54  *
00019 *   LOG = NO                                                     *
00020 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00021 ******************************************************************
00022
00023
00024  01  ACCOUNTS-RECEIVABLE.
00025      12  AR-RECORD-ID                      PIC XX.
00026          88  VALID-AR-ID                      VALUE 'AR'.
00027
00028      12  AR-CONTROL-PRIMARY.
00029          16  AR-COMPANY-CD                 PIC X.
00030          16  AR-TYPE                       PIC X.
00031              88 AR-NEW-BUSINESS VALUE '1'.
00032          16  AR-CARRIER                    PIC X.
00033          16  AR-GROUPING.
00034              20 AR-GROUPING-PREFIX         PIC XXX.
00035              20 AR-GROUPING-PRIME          PIC XXX.
00036          16  AR-BAL-LEVEL                  PIC X.
00037              88  AR-BAL-LVL-REF               VALUE '1'.
00038              88  AR-BAL-LVL-BILL-REF          VALUE '1'.
00039              88  AR-BAL-LVL-BILL              VALUE '2'.
00040              88  AR-BAL-LVL-AGT-FR            VALUE '3'.
00041              88  AR-BAL-LVL-FIN-RES           VALUE '4'.
00042          16  AR-ENTRY-TYPE                 PIC X.
00043              88  AR-AGENT-ENTRY               VALUE '1'.
00044              88  AR-GA-ENTRY                  VALUE '2'.
00045              88  AR-OVWRT-ENTRY               VALUE '3'.
00046          16  AR-FIN-RES                    PIC X(10).
00047          16  AR-AGENT-NO                   PIC X(10).
00048          16  AR-INVOICE-NO                 PIC X(6).
00049          16  AR-REFERENCE                  PIC X(12).
00050          16  AR-RESPONSIBILITY             PIC X.
00051              88  AR-AGENT-RESPONSIBLE         VALUE 'A'.
00052              88  AR-GA-RESPONSIBLE            VALUE 'G'.
00053          16  AR-RECORD-TYPE                PIC X.
00054              88  AR-BALANCE                   VALUE '0'.
00055              88  AR-PREMIUM                   VALUE '1'.
00056              88  AR-COMMISSION                VALUE '2'.
00057              88  AR-PAY-ADJ                   VALUE '3'.
00058              88  AR-TRANSFER                  VALUE '4'.
00059              88  AR-WRITE-OFF                 VALUE '5'.
00060              88  AR-NOTE                      VALUE '6'.
00061          16  AR-RECORD-SEQ                 PIC S9(4)    COMP.
00062
00063      12  FILLER                            PIC X(9).
00064
00065      12  AR-ACCOUNT-AGENT-CONTROL.
00066          16  AR-ACCOUNT-AGENT.
00067              20  AR-COMPANY-CD-A1          PIC X.
00068              20  AR-CARRIER-A1             PIC X.
00069              20  AR-GROUPING-A1            PIC X(6).
00070              20  AR-AGENT-NO-A1            PIC X(10).
00071          16  AR-EOM-DT-A1                  PIC XX.
00072          16  AR-BAL-LEVEL-A1               PIC X.
00073          16  AR-ENTRY-TYPE-A1              PIC X.
00074              88  AR-ACCT-AGENT               VALUE '1'.
00075              88  AR-GEN-AGENT                VALUE '2'.
00076              88  AR-OVERWRITE-AGENT          VALUE '3'.
00077          16  AR-FIN-RES-A1                 PIC X(10).
00078          16  AR-INVOICE-A1                 PIC X(6).
00079          16  AR-REFERENCE-A1               PIC X(12).
00080          16  AR-RESPONSIBILITY-A1          PIC X.
00081              88  AR-AGENT-RESPONSIBLE-A1      VALUE 'A'.
00082              88  AR-GA-RESPONSIBLE-A1         VALUE 'G'.
00083          16  AR-RECORD-TYPE-A1             PIC X.
00084          16  AR-RECORD-SEQ-A1              PIC S9(4)    COMP.
00085
00086      12  AR-CONTROL-GA                     PIC X(10).
00087
00088      12  AR-LAST-MAINT-DT                  PIC XX.
00089      12  AR-LAST-MAINT-BY                  PIC X(4).
00090      12  AR-LST-MAINT-HHMMSS               PIC S9(6)    COMP-3.
00091      12  AR-MONTH-END-DT                   PIC XX.
00092      12  FILLER                            PIC X.
00093      12  AR-REVERSAL-INFO.
00094          16  AR-REVERSAL-ID                PIC X(4).
00095          16  AR-REVERSAL-DT                PIC XX.
00096      12  AR-UPDATE-CODE                    PIC X.
00097          88  AR-NEW-RECORD                      VALUE 'N'.
00098          88  AR-UPDATED-RECORD                  VALUE 'U'.
00099      12  AR-SYSTEM                         PIC X.
00100          88  AR-MORTGAGE-SYSTEM                 VALUE 'M'.
00101      12  AR-CSR-TEMP                       PIC X(04).
00102      12  AR-CARRIER-TEMP                   PIC X(01).
00103      12  AR-GROUPING-TEMP                  PIC X(06).
00104      12  FILLER                            PIC X(20).
00105
00106      12  AR-RECORD-BODY                    PIC X(120).
00107
00108      12  AR-PREM-COMM-RECORD   REDEFINES AR-RECORD-BODY.
00109          16  AR-P-C-TRAN-DATE              PIC XX.
00110          16  AR-P-C-STMT-DATE              PIC XX.
00111          16  AR-P-C-BATCH                  PIC X(6).
00112          16  AR-P-C-AMOUNT                 PIC S9(7)V99  COMP-3.
00113          16  AR-P-C-DB-CR                  PIC XX.
00114              88 AR-P-C-DEBIT                   VALUE 'DB'.
00115              88 AR-P-C-CREDIT                  VALUE 'CR'.
00116          16  AR-P-C-DESCRIPTION            PIC X(30).
00117          16  AR-P-C-REMITTER-CODE          PIC X.
00118              88 AR-P-C-REMITTER-PAYS           VALUE 'Y'.
00119              88 AR-P-C-ACCT-AGT-PAYS           VALUE 'Y'.
00120          16  FILLER                        PIC X(33).
00121          16  AR-P-C-REMITTER               PIC X(10).
00122          16  AR-P-C-AMOUNT-LF              PIC S9(7)V99  COMP-3.
00123          16  AR-P-C-AMOUNT-AH              PIC S9(7)V99  COMP-3.
00124          16  FILLER                        PIC X(19).
00125
00126      12  AR-PAY-ADJ-RECORD  REDEFINES AR-RECORD-BODY.
00127          16  AR-PA-TRAN-DATE               PIC XX.
00128          16  AR-PA-STMT-DATE               PIC XX.
00129          16  FILLER                        PIC X(6).
00130          16  AR-PA-AMOUNT                  PIC S9(7)V99  COMP-3.
00131          16  AR-PA-DB-CR                   PIC XX.
00132              88  AR-PA-DEBIT                   VALUE 'DB'.
00133              88  AR-PA-CREDIT                  VALUE 'CR'.
00134          16  AR-PA-SEQ-NO                  PIC S9(8)     COMP.
00135          16  AR-PA-TYPE                    PIC X.
00136          16  AR-PA-DEBIT-LEDGER            PIC X(14).
00137          16  AR-PA-CREDIT-LEDGER           PIC X(14).
00138          16  AR-PA-COMMENT                 PIC X(30).
00139          16  AR-PA-REMITTER-CODE           PIC X.
00140              88 AR-PA-REMITTER-PAYS            VALUE 'Y'.
00141              88 AR-PA-ACCT-AGT-PAYS            VALUE 'Y'.
00142          16  AR-PA-REMITTER                PIC X(10).
00143          16  AR-NOTE-COUNT                 PIC S9(5).
00144          16  FILLER                        PIC X(24).
00145
00146      12  AR-TRANSFER-RECORD REDEFINES AR-RECORD-BODY.
00147          16  AR-XRF-TRAN-DATE              PIC XX.
00148          16  AR-XRF-STMT-DATE              PIC XX.
00149          16  AR-XRF-BATCH                  PIC X(6).
00150          16  AR-XRF-AMOUNT                 PIC S9(7)V99  COMP-3.
00151          16  AR-XRF-DB-CR                  PIC XX.
00152              88  AR-XRF-DEBIT                  VALUE 'DB'.
00153              88  AR-XRF-CREDIT                 VALUE 'CR'.
00154          16  AR-BAL-DESCRIPTION            PIC X(30).
00155          16  AR-XRF-REMITTER-CODE          PIC X.
00156              88 AR-XRF-REMITTER-PAYS           VALUE 'Y'.
00157              88 AR-XRF-ACCT-AGT-PAYS           VALUE 'Y'.
00158          16  FILLER                        PIC X(33).
00159          16  AR-XRF-REMITTER               PIC X(10).
00160          16  FILLER                        PIC X(29).
00161
00162      12  AR-BALANCE-RECORD REDEFINES AR-RECORD-BODY.
00163          16  AR-FST-TRAN-DATE              PIC XX.
00164          16  AR-LST-TRAN-DATE              PIC XX.
00165          16  FILLER                        PIC X(6).
00166          16  AR-BAL-AMOUNT                 PIC S9(7)V99  COMP-3.
00167          16  AR-BAL-DB-CR                  PIC XX.
00168              88  AR-BAL-DEBIT                  VALUE 'DB'.
00169              88  AR-BAL-CREDIT                 VALUE 'CR'.
00170          16  AR-BAL-DESCRIPTION            PIC X(30).
00171          16  FILLER                        PIC X(73).
00322  EJECT
00323 *    COPY MPCPRCN.
00001 ******************************************************************
00002 *                                                                *
00003 *                            MPCPRCN                             *
00004 *                            VMOD=1.014                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = PAYMENT RECONCILIATION                    *
00007 *   (ANY CHANGES MADE TO THIS COPYBOOK MUST ALSO BE MADE         *
00008 *   TO MPCPEXT)                                                  *
00009 *                                                                *
00010 *   FILE TYPE = VSAM,KSDS                                        *
00011 *   RECORD SIZE = 835  RECFORM = FIXED                           *
00012 *                                                                *
00013 *   BASE CLUSTER = MPPRCN (SEE NOTE ABOVE)          RKP=2,LEN=11 *
00014 *   ALTERNATE PATH2= MPPRCN2(PRODUCER BILLING SEQU)RKP=33,LEN=44 *
00015 *   ALTERNATE PATH3= MPPRCN3(POLICY PRIMARY CNTL  )RKP=97,LEN=46 *
00016 *   ALTERNATE PATH4= MPPRCN4(BY RECORD TYPE CNTL  )RKP=163,LEN=46*
00017 *                                                                *
00018 *   LOG = YES                                                    *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00020 ******************************************************************
00021
00022  01  PAYMENT-RECONCILIATION.
00023      12  PR-RECORD-ID                      PIC XX.
00024          88  VALID-PR-ID                      VALUE 'PR'.
00025
00026 ******************************************************************
00027 *   BASE CLUSTER = MPPRCN         (BASE KEY)      RKP=2,LEN=11   *
00028 ******************************************************************
00029
00030      12  PR-CONTROL-PRIMARY.
00031          16  PR-COMPANY-CD                 PIC X.
00032          16  PR-INVOICE-NUMBER.
00033              20  PR-INVOICE-YMD.
00034                  24  PR-INVOICE-YR         PIC X.
00035                  24  PR-INVOICE-MO         PIC X.
00036                  24  PR-INVOICE-DAY        PIC X.
00037              20  PR-INVOICE-SEQU           PIC X(3).
00038                  88  PR-INDIVIDUAL-BILL       VALUE 'AAA'.
00039          16  PR-RECORD-SEQU                PIC S9(7)     COMP-3.
00040              88  PR-INVOICE-HEADER            VALUE +9999999.
00041      12  FILLER                            PIC X(20).
00042
00043 ******************************************************************
00044 * ALTERNATE PATH2 = MPPRCN2(PRODUCER BILLING SEQU)RKP=33,LEN=44  *
00045 ******************************************************************
00046
00047      12  PR-CONTROL-BY-PRODUCER.
00048          16  PR-COMPANY-CD-A2              PIC X.
00049          16  PR-CARRIER-A2                 PIC X.
00050          16  PR-GROUPING-A2.
00051              20  PR-GROUPING-PREFIX-A2     PIC X(3).
00052              20  PR-GROUPING-PRIME-A2      PIC X(3).
00053          16  PR-STATE-A2                   PIC XX.
00054          16  PR-PRODUCER-A2.
00055              20  PR-PRODUCER-PREFIX-A2     PIC X(4).
00056              20  PR-PRODUCER-PRIME-A2      PIC X(6).
00057          16  PR-PRODUCER-BILLING-SEQU-A2   PIC X(20).
00058          16  PR-CURRENT-DATE-BIN-A2        PIC X(02).
00059          16  PR-UNIQUE-KEY-A2              PIC S9(04) COMP.
00060      12  FILLER                            PIC X(20).
00061
00062 ******************************************************************
00063 *  ALTERNATE PATH3 = MPPRCN3(POLICY CONTROL SEQU)RKP=97,LEN=46   *
00064 ******************************************************************
00065
00066      12  PR-CONTROL-BY-POLICY.
00067          16  PR-COMPANY-CD-A3              PIC X.
00068          16  PR-POLICY-NO-A3.
00069              20  PR-POLICY-PRIME-A3        PIC X(18).
00070              20  PR-POLICY-SFX-A3          PIC XX.
00071          16  PR-POLICY-CNTL-PARTIAL.
00072              20  PR-CARRIER-A3             PIC X.
00073              20  PR-GROUPING-A3.
00074                  24  PR-GROUPING-PREFIX-A3 PIC X(3).
00075                  24  PR-GROUPING-PRIME-A3  PIC X(3).
00076              20  PR-STATE-A3               PIC XX.
00077              20  PR-PRODUCER-A3.
00078                  24  PR-PRODUCER-PREFIX-A3 PIC X(4).
00079                  24  PR-PRODUCER-PRIME-A3  PIC X(6).
00080              20  PR-POLICY-EFF-DT-A3       PIC XX.
00081          16  PR-CURRENT-DATE-BIN-A3        PIC X(02).
00082          16  PR-UNIQUE-KEY-A3              PIC S9(04) COMP.
00083      12  FILLER                            PIC X(20).
00084
00085 ******************************************************************
00086 *  ALTERNATE PATH4 = MPPRCN4(RECON RECORD TYPE  )RKP=163 LEN=25  *
00087 ******************************************************************
00088
00089      12  PR-CONTROL-BY-RECORD-TYPE.
00090          16  PR-COMPANY-CD-A4              PIC X.
00091          16  PR-RECORD-TYPE-A4             PIC X.
00092              88  PR-HEADER-RECORD              VALUE '1'.
00093              88  PR-DETAIL-RECORD              VALUE '2'.
00094          16  PR-CARRIER-A4                 PIC X.
00095          16  PR-GROUPING-A4.
00096              20  PR-GROUPING-PREFIX-A4     PIC X(3).
00097              20  PR-GROUPING-PRIME-A4      PIC X(3).
00098          16  PR-STATE-A4                   PIC XX.
00099          16  PR-PRODUCER-A4.
00100              20  PR-PRODUCER-PREFIX-A4     PIC X(4).
00101              20  PR-PRODUCER-PRIME-A4      PIC X(6).
00102          16  PR-CURRENT-DATE-BIN-A4        PIC X(02).
00103          16  PR-UNIQUE-KEY-A4              PIC S9(04) COMP.
00104      12  FILLER                            PIC X(20).
00105
00106 ******************************************************************
00107 *                 FILE SYNCHRONIZATION DATA                      *
00108 ******************************************************************
00109
00110      12  PR-FILE-SYNCH-DATA.
00111          16  PR-LAST-CHANGE-DT             PIC XX.
00112          16  PR-LAST-CHANGE-TIME           PIC S9(7)     COMP-3.
00113          16  PR-LAST-CHANGE-PROCESSOR      PIC X(4).
00114          16  PR-SECURITY-ACCESS-CODE       PIC X.
00115      12  FILLER                            PIC X(16).
00116
00117 ******************************************************************
00118 *              PREMIUM RECONCILIATION RECORD BODY                *
00119 ******************************************************************
00120
00121      12  PR-RECORD-BODY                    PIC X(600).
00122
00123 ******************************************************************
00124 *             PREMIUM RECONCILIATION BATCH HEADER                *
00125 ******************************************************************
00126
00127      12  PR-BATCH-HEADER  REDEFINES  PR-RECORD-BODY.
00128          16  PR-POSTING-DATA.
00129              20  PR-POSTING-DT             PIC XX.
00130              20  PR-POSTING-TIME           PIC S9(7)      COMP-3.
00131              20  PR-POSTING-PROCESSOR      PIC X(4).
00132          16  PR-SUMMARY-DATA.
00133              20  PR-DOCU-COUNTS.
00134                  24  PR-ACTUAL-DOCU-CNT    PIC S9(5)      COMP-3.
00135                  24  PR-EXPECTED-DOCU-CNT  PIC S9(5)      COMP-3.
00136              20  PR-PREMIUM-AMTS.
00137                  24  PR-RECEIVED-PREMIUM   PIC S9(7)V99   COMP-3.
00138                  24  PR-ACTUAL-PREMIUM     PIC S9(7)V99   COMP-3.
00139                  24  PR-EXPECTED-PREMIUM   PIC S9(7)V99   COMP-3.
00140              20  PR-COMPENSATION-AMTS.
00141                  24  PR-PROD-EXPECTED-COM  PIC S9(7)V99   COMP-3.
00142                  24  PR-PROD-ACTUAL-COM    PIC S9(7)V99   COMP-3.
00143                  24  PR-OVRWT-EXPECTED-COM PIC S9(7)V99   COMP-3.
00144                  24  PR-OVRWT-ACTUAL-COM   PIC S9(7)V99   COMP-3.
00145                  24  PR-REMIT-EXPECTED-COM PIC S9(7)V99   COMP-3.
00146                  24  PR-REMIT-ACTUAL-COM   PIC S9(7)V99   COMP-3.
00147          16  PR-HDR-BILLING-SEQUENCE       PIC X(01).
00148              88  PR-HDR-BILL-NAME-SEQU        VALUE '1'.
00149              88  PR-HDR-BILL-LOAN-SEQU        VALUE '2'.
00150              88  PR-HDR-BILL-PLCY-SEQU        VALUE '3'.
00151          16  PR-HDR-BILLING-DT             PIC XX.
00152          16  PR-HDR-BILL-TO-DT             PIC XX.
00153          16  PR-HDR-POSTING-STATUS         PIC X.
00154              88  PR-HDR-NEEDS-EDITING         VALUE '0'.
00155              88  PR-HDR-EDITED                VALUE '1'.
00156              88  PR-HDR-OK-TO-POST            VALUE '2'.
00157              88  PR-HDR-POSTED                VALUE '3'.
00158              88  PR-HDR-REVERSED              VALUE '4'.
00159              88  PR-HDR-POSTING               VALUE '5'.
00160          16  PR-RECEIVED-DT                PIC XX.
00161          16  PR-RESPONSIBLE-AGENT          PIC X(10).
00162          16  PR-PAC-INFORMATION.
00163              20  PR-HDR-BANK-TRANSIT-NUMBER.
00164                  24  PR-HDR-FEDERAL-NUMBER PIC X(4).
00165                  24  PR-HDR-BANK-NUMBER    PIC X(4).
00166              20  PR-HDR-BANK-ACCOUNT-NUMBER
00167                                            PIC X(20).
00168              20  PR-SIGNATURE-NAME         PIC X(25).
00169
00170 ******************************************************************
00171 *         HEADER  AGENT AND COMMISSION DATA                      *
00172 ******************************************************************
00173
00174          16  PR-HDR-1STYR-RENEW-SW           PIC X.
00175              88  PR-HDR-1STYR-COMMISSIONS            VALUE '1'.
00176              88  PR-HDR-RENEW-COMMISSIONS            VALUE '2'.
00177              88  PR-HDR-CROSS-BOUNDRIES              VALUE '3'.
00178          16  PR-HDR-COMMISSION-DATA.
00179              20  PR-HDR-REMIT-TO             PIC S9(3) COMP-3.
00180              20  PR-HDR-COMM-CHANGE-SW       PIC X.
00181                  88  PR-HDR-COMMISSION-CHANGE         VALUE 'Y'.
00182              20  PR-HDR-AGENT-INFORMATION   OCCURS   5 TIMES.
00183                  24  PR-HDR-AGENT-NUMBER     PIC X(10).
00184                  24  PR-HDR-AGENT-TYPE       PIC X.
00185                      88  PR-HDR-AGENT-GROSS           VALUE 'C'.
00186                      88  PR-HDR-AGENT-REINS           VALUE 'R'.
00187                      88  PR-HDR-AGENT-GROSS-REINS     VALUE 'D'.
00188                      88  PR-HDR-OVERWRITE-GROSS       VALUE 'O'.
00189                      88  PR-HDR-OVERWRITE-GROSS-REINS
00190                                                       VALUE 'P'.
00191                      88  PR-HDR-OVERWRITE-REINS       VALUE 'T'.
00192                      88  PR-HDR-REINS-ONLY            VALUE 'W'.
00193                  24  PR-HDR-COMMISSION-BILL-PAID PIC X(1).
00194                      88  PR-HDR-GENERATE-BILL         VALUE 'B'.
00195                      88  PR-HDR-GENERATE-PAID         VALUE 'P'.
00196                  24  PR-HDR-AGENT-COMP-1ST-YEAR  PIC S99V999.
00197                  24  PR-HDR-COMP-1ST-YEAR-TYPE   PIC X(1).
00198                      88  PR-HDR-COMP-1ST-YEAR-PERCENT
00199                                                   VALUE '1'.
00200                      88  PR-HDR-COMP-1ST-YEAR-DOLLARS
00201                                                   VALUE '2'.
00202                      88  PR-HDR-COMP-1ST-YEAR-NOT-USED
00203                                                   VALUE '3'.
00204                  24  PR-HDR-RENEW-DATA  OCCURS 6 TIMES.
00205                      28  PR-HDR-RENEW-MONTHS PIC S999    COMP-3.
00206                      28  PR-HDR-RENEW-COMM   PIC S99V999 COMP-3.
00207                      28  PR-HDR-RENEW-TYPE   PIC X(1).
00208                          88  PR-HDR-COMP-RENEW-PERCENT
00209                                                         VALUE '1'.
00210                          88  PR-HDR-COMP-RENEW-DOLLARS
00211                                                         VALUE '2'.
00212                          88  PR-HDR-COMP-RENEW-NOT-USED
00213                                                         VALUE '3'.
00214                  24  PR-HDR-COMP-RECALC-FLAG     PIC X(1).
00215                      88  PR-HDR-BYPASS-RECALC          VALUE 'N'.
00216          16  FILLER                        PIC X(189).
00217
00218 ******************************************************************
00219 *                PREMIUM RECONCILIATION DETAIL                   *
00220 ******************************************************************
00221
00222      12  PR-RECON-DETAIL  REDEFINES  PR-RECORD-BODY.
00223          16  PR-TRANSACTION-TYPE           PIC X.
00224              88  PR-BILL                      VALUE '1'.
00225              88  PR-CANCEL                    VALUE '2'.
00226              88  PR-PYMT-REVERSAL             VALUE '3'.
00227              88  PR-BILL-REVERSAL             VALUE '4'.
00228              88  PR-PAID-IN-ADVANCE           VALUE '5'.
00229              88  PR-AR-BILL-REVERSAL          VALUE '6'.
00230          16  PR-POSTING-STATUS             PIC X.
00231              88  PR-NEEDS-EDITING             VALUE 'N'.
00232              88  PR-EDITED                    VALUE 'E'.
00233              88  PR-POSTED                    VALUE 'P'.
00234              88  PR-FORCE-TO-POST             VALUE 'F'.
00235              88  PR-DELETED                   VALUE 'D'.
00236              88  PR-REVERSED                  VALUE 'R'.
00237          16  PR-POLICY-STATUS              PIC X.
00238              88  PR-P-ACTIVE                  VALUE  '1'.
00239              88  PR-P-LAPSE                   VALUE  '0'.
00240              88  PR-P-CLAIM-APPLIED           VALUE  '6'.
00241              88  PR-P-CANCEL                  VALUE  '7'.
00242              88  PR-BILLABLE                  VALUES '0' '1' '6'.
00243          16  PR-PYMT-REVERSAL-DT           PIC XX.
00244          16  PR-INSURED-NAME.
00245              20  PR-INSURED-FIRST-NAME.
00246                  24  PR-INSURED-1ST-INIT   PIC X.
00247                  24  FILLER                PIC X(9).
00248              20  PR-INSURED-MIDDLE-INITIAL PIC X.
00249              20  PR-INSURED-LAST-NAME      PIC X(15).
00250          16  PR-LOAN-NUMBER                PIC X(20).
00251          16  PR-PLAN-CODE                  PIC X(02).
00252          16  PR-PLAN-REVISION              PIC X(03).
00253          16  PR-INS-MONTH-PREMIUM          PIC S9(5)V9(6) COMP-3.
00254          16  PR-BILLING-INFORMATION.
00255              20  PR-BILLING-TYPE           PIC X.
00256                  88  PR-LIST-BILL             VALUE '1'.
00257                  88  PR-TAPE-BILL             VALUE '2'.
00258                  88  PR-TAPE-LIST-BILL        VALUE '3'.
00259                  88  PR-GROUP-BILL         VALUES ARE '1' '2' '3'.
00260                  88  PR-DIRECT-BILL           VALUE '4'.
00261                  88  PR-PAC-BILL         VALUES ARE '5' 'C' 'S'.
00262                  88  PR-CHARGE-CARD-BILL      VALUE '6'.
00263                  88  PR-PAC-REFUND            VALUE 'D'.
00264                  88  PR-CHARGE-CARD-REFUND    VALUE 'E'.
00265                  88  PR-INDIV-BILL
00266                      VALUES ARE '4' '5' '6' 'C' 'S'.
00267                  88  PR-EFT-CHECKING          VALUE 'C'.
00268                  88  PR-EFT-SAVINGS           VALUE 'S'.
00269              20  PR-BILLING-SEQUENCE       PIC X(01).
00270                  88  PR-BILL-NAME-SEQU        VALUE '1'.
00271                  88  PR-BILL-LOAN-SEQU        VALUE '2'.
00272                  88  PR-BILL-PLCY-SEQU        VALUE '3'.
00273              20  PR-BILLING-MODE           PIC X(01).
00274                  88  PR-ANNUAL                VALUE '1'.
00275                  88  PR-SEMI-ANNUAL           VALUE '2'.
00276                  88  PR-QUARTERLY             VALUE '3'.
00277                  88  PR-MONTHLY               VALUE '4'.
00278                  88  PR-BI-MONTHLY            VALUE '5'.
00279                  88  PR-SINGLE-PREM           VALUE '6'.
00280              20  PR-MONTHS-BILLED          PIC S9(03)     COMP-3.
00281              20  PR-BILLED-AMT             PIC S9(5)V99   COMP-3.
00282              20  PR-BILLED-TO-DT           PIC XX.
00283              20  PR-BILLING-DT             PIC XX.
00284              20  PR-BILLING-SW             PIC X.
00285                  88  PR-FIRST-BILLING         VALUE 'Y'.
00286                  88  PR-PLCY-PAID-IN-ADVANCE  VALUE 'A'.
00287              20  PR-BANK-TRANSIT-NUMBER.
00288                  24  PR-FEDERAL-NUMBER     PIC X(4).
00289                  24  PR-BANK-NUMBER        PIC X(4).
00290              20  PR-BANK-ACCOUNT-NUMBER    PIC X(20).
00291              20  PR-CHARGE-CARD-TYPE       PIC X(2).
00292                  88  PR-VISA                  VALUE 'VI'.
00293                  88  PR-MSTR-CARD             VALUE 'MC'.
00294                  88  PR-DINERS-CLUB           VALUE 'DN'.
00295                  88  PR-DISCOVER              VALUE 'DS'.
00296                  88  PR-CARTE-BLANCHE         VALUE 'CB'.
00297                  88  PR-AMERICAN-EXPRESS      VALUE 'AE'.
00298              20  PR-CHARGE-CARD-EXP-DT     PIC X(2).
00299              20  PR-LOAN-OFFICER           PIC X(5).
00300              20  PR-BILLING-GROUPING-CODE  PIC X(6).
00301          16  PR-BILLING-RECONCILE-DATA.
00302              20  PR-EXPECTED-DATA.
00303                  24  PR-EXPECTED-PAYMENT   PIC S9(5)V99   COMP-3.
00304                  24  PR-EXPECTED-TO-DT     PIC XX.
00305              20  PR-EXP-COMPENSATION-DATA.
00306                  24  PR-EXP-PROD-LVL-COMP  PIC S9(5)V99   COMP-3.
00307                  24  PR-EXP-OVWR-LVL-COMP  PIC S9(5)V99   COMP-3.
00308                  24  PR-EXP-REMT-LVL-COMP  PIC S9(5)V99   COMP-3.
00309              20  PR-ENTERED-DATA.
00310                  24  PR-ENTERED-PAYMENT    PIC S9(5)V99   COMP-3.
00311                  24  PR-ENTERED-OVSH       PIC S9(5)V99   COMP-3.
00312                  24  PR-ENTERED-TO-DT      PIC XX.
00313                  24  PR-ENTERED-SOURCE     PIC X.
00314                      88  PR-BILLING-PGM       VALUE 'B'.
00315                      88  PR-OPERATOR          VALUE 'O'.
00316                      88  PR-PAYMENT-PGM       VALUE 'P'.
00317              20  PR-ENT-COMPENSATION-DATA.
00318                  24  PR-ENT-PROD-LVL-COMP  PIC S9(5)V99   COMP-3.
00319                  24  PR-ENT-OVWR-LVL-COMP  PIC S9(5)V99   COMP-3.
00320                  24  PR-ENT-REMT-LVL-COMP  PIC S9(5)V99   COMP-3.
00321          16  PR-PAYMENT-TYPE               PIC X.
00322              88  PR-CHECK                     VALUE 'C'.
00323              88  PR-MONEY-ORDER               VALUE 'M'.
00324          16  PR-CHECK-NUMBER               PIC X(5).
00325          16  PR-PREVIOUS-PAYMENT-DATA.
00326              20  PR-PREV-INVOICE-NUMBER    PIC X(6).
00327              20  PR-PREV-LAST-PYMT-DT      PIC XX.
00328              20  PR-PREV-PAID-TO-DT        PIC XX.
00329              20  PR-PREV-PAYMENT-AMT       PIC S9(5)V99  COMP-3.
00330              20  PR-PREV-OVER-SHORT-AMT    PIC S9(5)V99  COMP-3.
00331              20  PR-PREV-TOTAL-PREM-RECVD  PIC S9(7)V99  COMP-3.
00332              20  PR-PREV-MONTHS-PAID       PIC S9(3)     COMP-3.
00333          16  PR-PREVIOUS-BILLING-DATA.
00334              20  PR-PREV-LAST-BILL-DT      PIC XX.
00335              20  PR-PREV-BILL-TO-DT        PIC XX.
00336              20  PR-PREV-LAST-BILL-AMT     PIC S9(5)V99  COMP-3.
00337              20  PR-PREV-BILLING-SW        PIC X.
00338              20  PR-PREV-EXIT-DT           PIC XX.
00339              20  PR-PREV-LAST-LAPSE-DT     PIC XX.
00340          16  PR-MONTH-END-DT               PIC XX.
00341          16  PR-PLAN-TYPE                  PIC X.
00342              88  PR-AH-MORT-PLAN              VALUE 'A'.
00343              88  PR-AD-D-MORT-PLAN            VALUE 'E'.
00344              88  PR-DISMEM-MORT-PLAN          VALUE 'D'.
00345              88  PR-LIFE-MORT-PLAN            VALUE 'L'.
00346          16  PR-EXPECTED-TAX               PIC S9(5)V99   COMP-3.
00347          16  PR-BILL-DAY                   PIC S99        COMP-3.
00348          16  FILLER                        PIC X(14).
00349
00350 ******************************************************************
00351 *                 AGENT AND COMMISSION DATA                      *
00352 ******************************************************************
00353
00354          16  PR-1STYR-RENEW-SW               PIC X.
00355              88  PR-1STYR-COMMISSIONS         VALUE '1'.
00356              88  PR-RENEW-COMMISSIONS         VALUE '2'.
00357              88  PR-CROSS-BOUNDRIES           VALUE '3'.
00358          16  PR-COMMISSION-DATA.
00359              20  PR-REMIT-TO                 PIC S9(3) COMP-3.
00360              20  PR-COMM-CHANGE-SW           PIC X.
00361                  88  PR-COMMISSION-CHANGE         VALUE 'Y'.
00362              20  PR-AGENT-INFORMATION   OCCURS   5 TIMES.
00363                  24  PR-AGENT-NUMBER         PIC X(10).
00364                  24  PR-AGENT-TYPE           PIC X.
00365                      88  PR-AGENT-GROSS           VALUE 'C'.
00366                      88  PR-AGENT-REINS           VALUE 'R'.
00367                      88  PR-AGENT-GROSS-REINS     VALUE 'D'.
00368                      88  PR-OVERWRITE-GROSS       VALUE 'O'.
00369                      88  PR-OVERWRITE-GROSS-REINS VALUE 'P'.
00370                      88  PR-OVERWRITE-REINS       VALUE 'T'.
00371                      88  PR-REINS-ONLY            VALUE 'W'.
00372                  24  PR-COMMISSION-BILL-PAID PIC X(1).
00373                      88  PR-GENERATE-BILL         VALUE 'B'.
00374                      88  PR-GENERATE-PAID         VALUE 'P'.
00375                  24  PR-AGENT-COMP-1ST-YEAR  PIC S99V999.
00376                  24  PR-COMP-1ST-YEAR-TYPE   PIC X(1).
00377                      88  PR-COMP-1ST-YEAR-PERCENT   VALUE '1'.
00378                      88  PR-COMP-1ST-YEAR-DOLLARS   VALUE '2'.
00379                      88  PR-COMP-1ST-YEAR-NOT-USED  VALUE '3'.
00380                  24  PR-RENEW-DATA OCCURS 6 TIMES.
00381                      28  PR-RENEW-MONTHS      PIC S999    COMP-3.
00382                      28  PR-RESNEW-COMMISSION
00383                                               PIC S99V999 COMP-3.
00384                      28  PR-COMP-RENEW-TYPE   PIC X(1).
00385                          88  PR-COMP-RENEW-PERCENT      VALUE '1'.
00386                          88  PR-COMP-RENEW-DOLLARS      VALUE '2'.
00387                          88  PR-COMP-RENEW-NOT-USED     VALUE '3'.
00388                  24  PR-COMP-RECALC-FLAG     PIC X(1).
00389                      88  PR-BYPASS-RECALC         VALUE 'N'.
00390          16  FILLER                        PIC X(94).
00391
00324  EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA PENDING-PAY-ADJ
                                COMPENSATION-MASTER
                                ACCOUNTS-RECEIVABLE
                                PAYMENT-RECONCILIATION.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL6351' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00326
00327      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00328      MOVE 2                      TO  EMI-NUMBER-OF-LINES.
00329
00330      IF EIBCALEN = ZERO
00331          GO TO 8800-UNAUTHORIZED-ACCESS.
00332
00333      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00334      MOVE '5'                    TO  DC-OPTION-CODE.
00335
00336      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.
00337
00338      MOVE DC-BIN-DATE-1          TO  WS-CURRENT-BIN-DT.
00339      MOVE DC-GREG-DATE-1-EDIT    TO  WS-CURRENT-DT.
00340      MOVE DC-GREG-DATE-1-MDY     TO  WS-CURRENT-MDY.
00341
00342      IF MORTGAGE-SESSION
00343         MOVE XCTL-EM626           TO XCTL-EL626
00344         MOVE MPPYAJ-FILE-ID       TO PYAJ-FILE-ID.
00345
00346      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00347          MOVE  'Y'                TO  PI-SEQ-FIRST-TIME-SW
00348          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00349              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6
00350              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5
00351              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4
00352              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3
00353              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2
00354              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1
00355              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM
00356              MOVE THIS-PGM              TO  PI-CALLING-PROGRAM
00357          ELSE
00358              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM
00359              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM
00360              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1
00361              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2
00362              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3
00363              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4
00364              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5
00365              MOVE SPACES                TO  PI-SAVED-PROGRAM-6.
00366
00367      IF PI-SEQ-FIRST-TIME-SW    =   'Y'
00368          MOVE ZEROS               TO  PI-FILE-SEQ-NO (1)
00369          MOVE ZEROS               TO  PI-FILE-SEQ-NO (2)
00370          MOVE ZEROS               TO  PI-FILE-SEQ-NO (3)
00371          MOVE ZEROS               TO  PI-FILE-SEQ-NO (4)
00372          MOVE ZEROS               TO  PI-FILE-SEQ-NO (5)
00373          MOVE 'N'                 TO  PI-SEQ-FIRST-TIME-SW.
00374
00375      MOVE LOW-VALUES             TO  EL635BI.
00376
00377      COMPUTE WORK-SEQ-NO  =  EIBTIME  *  10.
00378
00379      IF EIBTRNID NOT = TRANS-ID
00380          MOVE SPACE              TO  PI-PYAJ-FILE-SW
00381          GO TO 8100-SEND-INITIAL-MAP.
00382
00383      
      * EXEC CICS HANDLE CONDITION
00384 *        PGMIDERR  (9600-PGMID-ERROR)
00385 *        ERROR     (9990-ABEND)
00386 *        END-EXEC.
      *    MOVE '"$L.                  ! " #00002980' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303032393830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00387
00388      IF EIBAID = DFHCLEAR
00389          GO TO 9400-CLEAR.
00390  EJECT
00391  0200-RECEIVE.
00392      IF EIBAID = DFHPA1              OR  DFHPA2  OR  DFHPA3
00393          MOVE ER-0008            TO  EMI-ERROR
00394          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00395          MOVE -1                 TO  PFENTERL
00396          GO TO 8200-SEND-DATAONLY.
00397
00398      
      * EXEC CICS RECEIVE
00399 *        MAP     (MAP-NAME)
00400 *        MAPSET  (MAPSET-NAME)
00401 *        INTO    (EL635BI)
00402 *        END-EXEC.
           MOVE LENGTH OF
            EL635BI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00002995' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL635BI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00403
00404      IF PFENTERL = ZERO
00405          GO TO 0300-CHECK-PFKEYS.
00406
00407      IF (PFENTERI  IS NUMERIC)
00408        AND (PFENTERI  IS GREATER THAN  ZERO
00409        AND  IS LESS THAN  25)
00410          MOVE PF-VALUES (PFENTERI)  TO  EIBAID
00411      ELSE
00412          MOVE ER-0029               TO  EMI-ERROR
00413          GO TO 0320-INPUT-ERROR.
00414
00415  0300-CHECK-PFKEYS.
00416      IF EIBAID = DFHPF23
00417          GO TO 8810-PF23.
00418
00419      IF EIBAID = DFHPF24
00420          GO TO 9200-RETURN-MAIN-MENU.
00421
00422      IF EIBAID = DFHPF12
00423          GO TO 9500-PF12.
00424
00425      IF EIBAID = DFHENTER
00426          MOVE SPACES             TO  WS-ACCEPT
00427          MOVE SPACES             TO  WS-PREV-PF5
00428          GO TO 1000-EDIT-DATA.
00429
00430      IF EIBAID = DFHPF5
00431          MOVE PI-ACCEPT          TO  WS-ACCEPT
00432          GO TO 1000-EDIT-DATA.
00433
00434  0320-INPUT-ERROR.
00435      MOVE ER-0029                TO  EMI-ERROR.
00436
00437      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00438
00439      MOVE AL-UNBON               TO  PFENTERA.
00440
00441      IF PFENTERL = ZERO
00442          MOVE -1                 TO  PFENTERL
00443      ELSE
00444          MOVE -1                 TO  PFENTERL.
00445
00446      GO TO 8200-SEND-DATAONLY.
00447  EJECT
00448  1000-EDIT-DATA.
00449
00450      IF NOT MODIFY-CAP
00451          MOVE 'UPDATE'       TO SM-READ
00452          PERFORM 9995-SECURITY-VIOLATION
00453          MOVE ER-0070        TO EMI-ERROR
00454          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00455          GO TO 8100-SEND-INITIAL-MAP.
00456
00457      MOVE PI-COMPANY-CD          TO  PI-SAV-COMP-CD
00458                                      COMP-COMP-CD.
00459      SET INDX                    TO  1.
00460      MOVE 'ACCEPT'               TO  PI-ACCEPT-DESC.
00461      MOVE ZEROS                  TO  WS-EOM-DT (1)  WS-INP-DT (1)
00462                                      WS-EOM-DT (2)  WS-INP-DT (2)
00463                                      WS-EOM-DT (3)  WS-INP-DT (3)
00464                                      WS-EOM-DT (4)  WS-INP-DT (4)
00465                                      WS-EOM-DT (5)  WS-INP-DT (5).
00466
00467  1010-EDIT-LOOP.
00468
00469      IF CARR-LEN        (INDX) = ZEROS
00470         AND GRP-LEN     (INDX) = ZEROS
00471         AND FIN-LEN     (INDX) = ZEROS
00472         AND ACCT-LEN    (INDX) = ZEROS
00473         AND COMM-LEN    (INDX) = ZEROS
00474         AND RTYPE-LEN   (INDX) = ZEROS
00475         AND AMT-LEN     (INDX) = ZEROS
00476         AND APPLIED-LEN (INDX) = ZEROS
00477         AND INVOICE-LEN (INDX) = ZEROS
00478         AND EOM-DT-LEN  (INDX) = ZEROS
00479         AND INDATE-LEN  (INDX) = ZEROS
00480          NEXT SENTENCE
00481      ELSE
00482          GO TO 1010-EDIT-PROCESS.
00483
00484      IF EIBAID = DFHPF5
00485          GO TO 1010-EDIT-PROCESS.
00486
00487      IF WS-PREV-PF5 = 'Y' AND
00488          CARRIER    (INDX) NOT = LOW-VALUES AND
00489          GROUPING   (INDX) NOT = LOW-VALUES AND
00490          FIN-RESP   (INDX) NOT = LOW-VALUES AND
00491          ACCT       (INDX) NOT = LOW-VALUES AND
00492          RTYPE      (INDX) NOT = LOW-VALUES AND
00493          AMT        (INDX) NOT = LOW-VALUES AND
00494          COMM       (INDX) NOT = LOW-VALUES AND
00495          APPLIED    (INDX) NOT = LOW-VALUES AND
00496          INVOICE    (INDX) NOT = LOW-VALUES AND
00497          REF        (INDX) NOT = LOW-VALUES
00498           MOVE 'N'                TO  WS-PREV-PF5
00499           GO TO 1010-EDIT-PROCESS.
00500
00501      GO TO 1060-INCREMENT-INDX.
00502
00503  1010-EDIT-PROCESS.
00504
00505 **************************************************************
00506 *                                                            *
00507 *    THE COMPENSATION CONTROL IS NOT REQUIRED FOR SPECIAL    *
00508 *    GROUPING PROCESS IF THE TYPE IS 'G' AND AN INVOICE      *
00509 *    IS ENTERED.                                             *
00510 *                                                            *
00511 **************************************************************
00512
00513      IF MORTGAGE-SESSION
00514         IF RTYPE            (INDX) = 'G'
00515            IF CARR-LEN      (INDX) GREATER THAN +0
00516               OR  GRP-LEN   (INDX) GREATER THAN +0
00517               OR  FIN-LEN   (INDX) GREATER THAN +0
00518               OR  ACCT-LEN  (INDX) GREATER THAN +0
00519                  MOVE ER-9179        TO  EMI-ERROR
00520                  MOVE -1             TO  CARR-LEN   (INDX)
00521                  MOVE AL-UABON       TO  CARR-ATTRB (INDX)
00522                                          FIN-ATTRB  (INDX)
00523                                          GRP-ATTRB  (INDX)
00524                                          ACCT-ATTRB (INDX)
00525                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00526                  GO TO 1040-CONTINUE-EDIT
00527            ELSE
00528               GO TO 1040-CONTINUE-EDIT
00529         ELSE
00530            NEXT SENTENCE.
00531
00532      IF CARR-LEN (INDX) NOT = ZEROS
00533          MOVE AL-UANON           TO  CARR-ATTRB (INDX)
00534          MOVE CARRIER (INDX)     TO  COMP-CARRIER
00535                                      PI-SAV-CARRIER
00536          IF CARRIER (INDX) NOT = ZEROS
00537            AND (PI-ZERO-CARRIER  OR  PI-ZERO-CAR-GROUP)
00538              MOVE ER-2587        TO  EMI-ERROR
00539              MOVE -1             TO  CARR-LEN (INDX)
00540              MOVE AL-UABON       TO  CARR-ATTRB (INDX)
00541              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00542          ELSE
00543              NEXT SENTENCE
00544      ELSE
00545          MOVE ER-0194            TO  EMI-ERROR
00546          MOVE -1                 TO  CARR-LEN (INDX)
00547          MOVE AL-UABON           TO  CARR-ATTRB (INDX)
00548          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00549
00550      IF GRP-LEN (INDX) NOT = ZEROS
00551          MOVE AL-UANON           TO  GRP-ATTRB (INDX)
00552          MOVE GROUPING (INDX)    TO  COMP-GROUPING
00553                                      PI-SAV-GROUPING
00554          IF GROUPING (INDX) NOT = ZEROS
00555            AND (PI-ZERO-GROUPING  OR  PI-ZERO-CAR-GROUP)
00556              MOVE ER-2588        TO  EMI-ERROR
00557              MOVE -1             TO  GRP-LEN (INDX)
00558              MOVE AL-UABON       TO  GRP-ATTRB (INDX)
00559              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00560          ELSE
00561              NEXT SENTENCE
00562      ELSE
00563          MOVE ER-0195            TO  EMI-ERROR
00564          MOVE -1                 TO  GRP-LEN (INDX)
00565          MOVE AL-UABON           TO  GRP-ATTRB (INDX)
00566          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00567
00568      IF FIN-LEN (INDX) NOT = ZEROS
00569          MOVE AL-UANON           TO  FIN-ATTRB (INDX)
00570          MOVE FIN-RESP (INDX)    TO  COMP-FIN-RESP
00571                                      PI-SAV-FIN-RESP
00572                                      PI-CR-FIN-RESP
00573      ELSE
00574          MOVE ER-2562            TO  EMI-ERROR
00575          MOVE -1                 TO  FIN-LEN (INDX)
00576          MOVE AL-UABON           TO  FIN-ATTRB (INDX)
00577          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00578
00579      IF ACCT-LEN (INDX) NOT = ZEROS
00580          MOVE AL-UANON           TO  ACCT-ATTRB (INDX)
00581          MOVE ACCT (INDX)        TO  COMP-ACCOUNT
00582                                      PI-SAV-ACCOUNT
00583      ELSE
00584          MOVE +1                 TO  ACCT-LEN (INDX)
00585          MOVE LOW-VALUES         TO  COMP-ACCOUNT
00586                                      ACCT (INDX)
00587                                      PI-SAV-ACCOUNT.
00588
00589      MOVE 'A'                    TO  COMP-RECORD-TYPE.
00590
00591      IF ACCT (INDX) = LOW-VALUES
00592          IF APPLIED (INDX) NOT = 'G' AND 'O'
00593              MOVE ER-3183        TO  EMI-ERROR
00594              MOVE -1             TO  APPLIED-LEN (INDX)
00595              MOVE AL-UABON       TO  APPLIED-ATTRB(INDX)
00596              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00597              GO TO 1040-CONTINUE-EDIT
00598          ELSE
00599              GO TO 1030-VERIFY-GA-ONLY.
00600
00601      IF ACCT (INDX) = FIN-RESP (INDX)
00602          IF APPLIED (INDX) = 'A'
00603              GO TO 1015-VERIFY-ACCT-RESP
00604          ELSE
00605          IF APPLIED-LEN (INDX) = ZERO
00606              MOVE 'A'            TO  APPLIED (INDX)
00607              MOVE +1             TO  APPLIED-ATTRB (INDX)
00608              GO TO 1015-VERIFY-ACCT-RESP
00609          ELSE
00610              IF PI-COMPANY-ID = 'NCL'
00611                  IF APPLIED (INDX) = 'G'
00612                      GO TO 1015-VERIFY-ACCT-RESP
00613                  ELSE
00614                      MOVE ER-3184
00615                                  TO  EMI-ERROR
00616                      MOVE -1     TO  APPLIED-LEN (INDX)
00617                      MOVE AL-UABON
00618                                  TO  APPLIED-ATTRB(INDX)
00619                      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00620                      GO TO 1040-CONTINUE-EDIT
00621              ELSE
00622                  MOVE ER-3184    TO  EMI-ERROR
00623                  MOVE -1         TO  APPLIED-LEN (INDX)
00624                  MOVE AL-UABON   TO  APPLIED-ATTRB(INDX)
00625                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00626                  GO TO 1040-CONTINUE-EDIT.
00627
00628      IF APPLIED (INDX) = 'G'
00629          GO TO 1020-VERIFY-RESP-GA.
00630
00631      IF APPLIED (INDX) = 'O'
00632          GO TO 1030-VERIFY-GA-ONLY.
00633
00634      
      * EXEC CICS GETMAIN
00635 *        SET      (ADDRESS OF COMPENSATION-MASTER)
00636 *        LENGTH   (ERCOMP-RECORD-LENGTH)
00637 *        INITIMG  (GETMAIN-SPACE)
00638 *        END-EXEC.
      *    MOVE ',"IL                  $   #00003231' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033323331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERCOMP-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00639
00640      
      * EXEC CICS HANDLE CONDITION
00641 *        NOTFND   (1015-NO-COMP-MSTR)
00642 *        NOTOPEN  (7100-COMP-FILE-NOTOPEN)
00643 *        END-EXEC.
      *    MOVE '"$IJ                  ! # #00003237' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303033323337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00644
00645      
      * EXEC CICS READ
00646 *        DATASET  (COMP-FILE-ID)
00647 *        INTO     (COMPENSATION-MASTER)
00648 *        RIDFLD   (ERCOMP-KEY)
00649 *        END-EXEC.
           MOVE LENGTH OF
            COMPENSATION-MASTER
             TO DFHEIV11
      *    MOVE '&"IL       E          (   #00003242' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033323432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 COMPENSATION-MASTER, 
                 DFHEIV11, 
                 ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00650
00651      IF RTYPE (INDX) = 'R' OR 'D' OR 'S' OR 'T' OR 'Z'
00652            MOVE 'G'              TO  APPLIED (INDX)
00653         ELSE
00654            MOVE 'A'              TO  APPLIED (INDX).
00655
00656      MOVE +1                     TO  APPLIED-LEN (INDX).
00657      GO TO 1020-VERIFY-RESP-GA.
00658
00659  1012-NO-COMP-MSTR.
00660      MOVE 'O'                    TO  APPLIED (INDX).
00661      MOVE +1                     TO  APPLIED-LEN (INDX).
00662      GO TO 1030-VERIFY-GA-ONLY.
00663
00664  1015-VERIFY-ACCT-RESP.
00665
00666      
      * EXEC CICS GETMAIN
00667 *        SET      (ADDRESS OF COMPENSATION-MASTER)
00668 *        LENGTH   (ERCOMP-RECORD-LENGTH)
00669 *        INITIMG  (GETMAIN-SPACE)
00670 *        END-EXEC.
      *    MOVE ',"IL                  $   #00003263' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033323633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERCOMP-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00671
00672      
      * EXEC CICS HANDLE CONDITION
00673 *        NOTFND   (1015-NO-COMP-MSTR)
00674 *        NOTOPEN  (7100-COMP-FILE-NOTOPEN)
00675 *        END-EXEC.
      *    MOVE '"$IJ                  ! $ #00003269' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303033323639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00676
00677      
      * EXEC CICS READ
00678 *        DATASET  (COMP-FILE-ID)
00679 *        INTO     (COMPENSATION-MASTER)
00680 *        RIDFLD   (ERCOMP-KEY)
00681 *        END-EXEC.
           MOVE LENGTH OF
            COMPENSATION-MASTER
             TO DFHEIV11
      *    MOVE '&"IL       E          (   #00003274' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033323734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 COMPENSATION-MASTER, 
                 DFHEIV11, 
                 ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00682
00683      MOVE CO-AR-BAL-LEVEL        TO  PI-SAVE-COMP-BAL-CODE.
00684
00685      GO TO 1040-CONTINUE-EDIT.
00686
00687  1015-NO-COMP-MSTR.
00688
00689      MOVE ER-3178                TO  EMI-ERROR
00690      MOVE -1                     TO  CARR-LEN (INDX)
00691      MOVE AL-UABON               TO  CARR-ATTRB (INDX)
00692                                      GRP-ATTRB (INDX)
00693                                      FIN-ATTRB (INDX)
00694                                      ACCT-ATTRB (INDX).
00695      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00696      GO TO 1040-CONTINUE-EDIT.
00697
00698  1020-VERIFY-RESP-GA.
00699
00700      
      * EXEC CICS GETMAIN
00701 *        SET      (ADDRESS OF COMPENSATION-MASTER)
00702 *        LENGTH   (ERCOMP-RECORD-LENGTH)
00703 *        INITIMG  (GETMAIN-SPACE)
00704 *        END-EXEC.
      *    MOVE ',"IL                  $   #00003297' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033323937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERCOMP-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00705
00706      
      * EXEC CICS HANDLE CONDITION
00707 *        NOTFND   (1015-NO-COMP-MSTR)
00708 *        NOTOPEN  (7100-COMP-FILE-NOTOPEN)
00709 *        END-EXEC.
      *    MOVE '"$IJ                  ! % #00003303' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303033333033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00710
00711      
      * EXEC CICS READ
00712 *        DATASET  (COMP-FILE-ID)
00713 *        INTO     (COMPENSATION-MASTER)
00714 *        RIDFLD   (ERCOMP-KEY)
00715 *        END-EXEC.
           MOVE LENGTH OF
            COMPENSATION-MASTER
             TO DFHEIV11
      *    MOVE '&"IL       E          (   #00003308' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 COMPENSATION-MASTER, 
                 DFHEIV11, 
                 ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00716
00717      MOVE CO-AR-BAL-LEVEL        TO  PI-SAVE-COMP-BAL-CODE.
00718
00719      MOVE LOW-VALUES             TO  COMP-ACCOUNT.
00720      MOVE 'G'                    TO  COMP-RECORD-TYPE.
00721
00722      
      * EXEC CICS GETMAIN
00723 *        SET      (ADDRESS OF COMPENSATION-MASTER)
00724 *        LENGTH   (ERCOMP-RECORD-LENGTH)
00725 *        INITIMG  (GETMAIN-SPACE)
00726 *        END-EXEC.
      *    MOVE ',"IL                  $   #00003319' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERCOMP-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00727
00728      
      * EXEC CICS HANDLE CONDITION
00729 *        NOTFND   (1015-NO-COMP-MSTR)
00730 *        NOTOPEN  (7100-COMP-FILE-NOTOPEN)
00731 *        END-EXEC.
      *    MOVE '"$IJ                  ! & #00003325' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303033333235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00732
00733      
      * EXEC CICS READ
00734 *        DATASET  (COMP-FILE-ID)
00735 *        INTO     (COMPENSATION-MASTER)
00736 *        RIDFLD   (ERCOMP-KEY)
00737 *        END-EXEC.
           MOVE LENGTH OF
            COMPENSATION-MASTER
             TO DFHEIV11
      *    MOVE '&"IL       E          (   #00003330' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 COMPENSATION-MASTER, 
                 DFHEIV11, 
                 ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00738
00739      GO TO 1040-CONTINUE-EDIT.
00740
00741  1020-NO-COMP-MSTR.
00742
00743      IF COMP-RECORD-TYPE = 'A'
00744          MOVE ER-3178            TO  EMI-ERROR
00745      ELSE
00746          MOVE ER-3179            TO  EMI-ERROR.
00747
00748      MOVE -1                     TO  CARR-LEN (INDX)
00749      MOVE AL-UABON               TO  CARR-ATTRB (INDX)
00750                                      GRP-ATTRB (INDX)
00751                                      FIN-ATTRB (INDX)
00752                                      ACCT-ATTRB (INDX).
00753      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00754      GO TO 1040-CONTINUE-EDIT.
00755
00756  1030-VERIFY-GA-ONLY.
00757
00758      MOVE 'G'                    TO  COMP-RECORD-TYPE.
00759      MOVE LOW-VALUES             TO  COMP-ACCOUNT.
00760
00761      
      * EXEC CICS GETMAIN
00762 *        SET      (ADDRESS OF COMPENSATION-MASTER)
00763 *        LENGTH   (ERCOMP-RECORD-LENGTH)
00764 *        INITIMG  (GETMAIN-SPACE)
00765 *        END-EXEC.
      *    MOVE ',"IL                  $   #00003358' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERCOMP-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00766
00767      
      * EXEC CICS HANDLE CONDITION
00768 *        NOTFND   (1015-NO-COMP-MSTR)
00769 *        NOTOPEN  (7100-COMP-FILE-NOTOPEN)
00770 *        END-EXEC.
      *    MOVE '"$IJ                  ! '' #00003364' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303033333634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00771
00772      
      * EXEC CICS READ
00773 *        DATASET  (COMP-FILE-ID)
00774 *        INTO     (COMPENSATION-MASTER)
00775 *        RIDFLD   (ERCOMP-KEY)
00776 *        END-EXEC.
           MOVE LENGTH OF
            COMPENSATION-MASTER
             TO DFHEIV11
      *    MOVE '&"IL       E          (   #00003369' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 COMPENSATION-MASTER, 
                 DFHEIV11, 
                 ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00777
00778      MOVE CO-AR-BAL-LEVEL        TO  PI-SAVE-COMP-BAL-CODE.
00779
00780      GO TO 1040-CONTINUE-EDIT.
00781
00782  1030-NO-COMP-MSTR.
00783
00784      MOVE ER-3179                TO  EMI-ERROR.
00785      MOVE -1                     TO  CARR-LEN (INDX)
00786      MOVE AL-UABON               TO  CARR-ATTRB (INDX)
00787                                      GRP-ATTRB (INDX)
00788                                      FIN-ATTRB (INDX)
00789                                      ACCT-ATTRB (INDX).
00790      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00791      GO TO 1040-CONTINUE-EDIT.
00792
00793  1040-CONTINUE-EDIT.
00794
00795      IF COMM-LEN (INDX) NOT = ZEROS
00796          MOVE AL-UANON           TO  COMM-ATTRB (INDX)
00797          IF PI-COMPANY-ID NOT = 'NCL'
00798              NEXT SENTENCE
00799          ELSE
00800              IF NCL-COMM-DTE (INDX) IS NUMERIC
00801                  MOVE NCL-COMM-DTE (INDX)
00802                                  TO  DC-GREG-DATE-1-MDY
00803                  MOVE '4'        TO  DC-OPTION-CODE
00804                  PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
00805                  IF NO-CONVERSION-ERROR
00806                      NEXT SENTENCE
00807                  ELSE
00808                      MOVE ER-2595
00809                                  TO  EMI-ERROR
00810                      MOVE -1     TO  COMM-LEN (INDX)
00811                      MOVE AL-UABON
00812                                  TO  COMM-ATTRB (INDX)
00813                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00814              ELSE
00815                  IF NCL-COMM-DTE (INDX) = SPACES OR LOW-VALUES
00816                      MOVE WS-CURRENT-MDY
00817                                  TO  NCL-COMM-DTE (INDX)
00818                  ELSE
00819                      MOVE COMM (INDX)
00820                                  TO  WS-COMMENT-FULL
00821                      MOVE WS-COMMENT-24-POS
00822                                  TO  NCL-COMM-REST (INDX)
00823                      MOVE WS-CURRENT-MDY
00824                                  TO  NCL-COMM-DTE (INDX)
00825      ELSE
00826          IF PI-COMPANY-ID = 'NCL'
00827            AND PI-FILE-SEQ-NO (NDX) = ZEROS
00828              MOVE +6             TO  COMM-LEN (INDX)
00829              MOVE WS-CURRENT-MDY TO  NCL-COMM-DTE (INDX).
00830
00831
00832      IF RTYPE-LEN (INDX) NOT = ZEROS
00833          MOVE RTYPE (INDX)       TO  CHECK-REC-TYPE
00834          IF PI-COMPANY-ID = 'MON'
00835              IF NOT MON-VALID-REC-TYPE
00836                  MOVE -1         TO  RTYPE-LEN (INDX)
00837                  MOVE ER-7806    TO  EMI-ERROR
00838                  MOVE AL-UABON   TO  RTYPE-ATTRB (INDX)
00839                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00840              ELSE
00841                  MOVE AL-UANON   TO  RTYPE-ATTRB (INDX)
00842          ELSE
00843              IF PI-COMPANY-ID = 'NCL'
00844                  IF NOT NCL-VALID-REC-TYPE
00845                      MOVE -1     TO  RTYPE-LEN (INDX)
00846                      MOVE ER-7806
00847                                  TO  EMI-ERROR
00848                      MOVE AL-UABON
00849                                  TO  RTYPE-ATTRB (INDX)
00850                      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00851                  ELSE
00852                      MOVE AL-UANON
00853                                  TO  RTYPE-ATTRB (INDX)
00854              ELSE
00855                  IF PI-COMPANY-ID = 'ANL'
00856                      IF NOT ANL-VALID-REC-TYPE
00857                          MOVE -1 TO  RTYPE-LEN (INDX)
00858                          MOVE ER-7806
00859                                  TO  EMI-ERROR
00860                          MOVE AL-UABON
00861                                  TO  RTYPE-ATTRB (INDX)
00862                          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00863                      ELSE
00864                          MOVE AL-UANON
00865                                  TO  RTYPE-ATTRB (INDX)
00866                  ELSE
00867                      IF NOT VALID-REC-TYPE
00868                          MOVE -1 TO  RTYPE-LEN (INDX)
00869                          MOVE ER-2234
00870                                  TO  EMI-ERROR
00871                          MOVE AL-UABON
00872                                  TO  RTYPE-ATTRB (INDX)
00873                          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00874                      ELSE
00875                          MOVE AL-UANON
00876                                  TO  RTYPE-ATTRB (INDX)
00877      ELSE
00878          MOVE -1                 TO  RTYPE-LEN (INDX)
00879          MOVE ER-2235            TO  EMI-ERROR
00880          MOVE AL-UABON           TO  RTYPE-ATTRB (INDX)
00881          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00882
00883      IF AMT-LEN (INDX) NOT = ZEROS
00884          MOVE AL-UNNON           TO  AMT-ATTRB (INDX)
00885          
      * EXEC CICS BIF DEEDIT
00886 *            FIELD   (AMTO (INDX))
00887 *            LENGTH  (11)
00888 *        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00003482' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033343832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AMTO(INDX), 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00889          IF AMTO (INDX) = ZEROS
00890              MOVE ER-2245        TO  EMI-ERROR
00891              MOVE -1             TO  AMT-LEN(INDX)
00892              MOVE AL-UNBON       TO  AMT-ATTRB (INDX)
00893              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00894          ELSE
00895              SET WS-INDX          TO INDX
00896              MOVE AMTO (INDX)    TO WS-EDITED-AMT (WS-INDX)
00897      ELSE
00898          MOVE -1                 TO  AMT-LEN (INDX)
00899          MOVE ER-2236            TO  EMI-ERROR
00900          MOVE AL-UNBON           TO  AMT-ATTRB (INDX)
00901          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00902
00903      IF APPLIED-LEN (INDX) NOT = ZEROS
00904         IF APPLIED  (INDX) = 'A' OR 'G' OR 'O'
00905            NEXT SENTENCE
00906         ELSE
00907            MOVE -1               TO APPLIED-LEN   (INDX)
00908            MOVE ER-3146          TO EMI-ERROR
00909            MOVE AL-UABON         TO APPLIED-ATTRB (INDX)
00910            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00911
00912 ****************************************************************
00913 **   THIS CODE EDITS THE MONTH END DATE. DATE MUT BE VALID, AN
00914 **   EOM DAY, AND NOT MORE THAN TWO MONTHS PASSED THE CREDIT
00915 **   EOM DATE
00916 ****************************************************************
00917
00918      IF EOM-DT-LEN (INDX)  =  ZEROS
00919          GO TO 1040-CHECK-INPUT-DATE.
00920
00921      MOVE AL-UNNON               TO  EOM-DT-ATTRB (INDX).
00922
00923      MOVE EOM-DT (INDX)    TO  DEEDIT-FIELD.
00924      PERFORM 8600-DEEDIT.
00925
00926      IF DEEDIT-FIELD-V0  NOT NUMERIC
00927         GO TO 1040-DAY-ERROR.
00928
00929      MOVE DEEDIT-FIELD-V0      TO  DC-GREG-DATE-1-MDY.
00930      MOVE '4'                  TO  DC-OPTION-CODE.
00931      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.
00932
00933      IF DATE-CONVERSION-ERROR
00934         GO TO 1040-DAY-ERROR
00935      ELSE
00936         SET MINDEX             TO  INDX
00937         MOVE DC-BIN-DATE-1     TO  WS-EOM-DT (MINDEX).
00938
00939      MOVE DEEDIT-FIELD-V0      TO  DATE-TEST-AREA.
00940
00941      IF DATE-TEST-MM    =  4 OR 6 OR 9 OR 11
00942         IF DATE-TEST-DD  =  30
00943            GO TO 1040-CHECK-FUTURE-MO
00944         ELSE
00945            GO TO 1040-DAY-ERROR.
00946
00947      IF DATE-TEST-MM   = 1 OR 3 OR 5 OR 7 OR 8 OR 10 OR 12
00948         IF DATE-TEST-DD  =  31
00949            GO TO 1040-CHECK-FUTURE-MO
00950         ELSE
00951            GO TO 1040-DAY-ERROR.
00952
00953      DIVIDE DATE-TEST-YY  BY  4  GIVING  DIVIDE-RESULT
00954         REMAINDER  DIVIDE-REMAINDER.
00955
00956      IF (DATE-TEST-YY  =  ZERO)  OR
00957         (DIVIDE-REMAINDER  NOT =  ZERO)
00958         IF DATE-TEST-DD  NOT =  28
00959             GO TO 1040-DAY-ERROR
00960         ELSE
00961             GO TO 1040-CHECK-FUTURE-MO
00962      ELSE
00963         IF DATE-TEST-DD  =  29
00964             GO TO 1040-CHECK-FUTURE-MO.
00965
00966  1040-CHECK-FUTURE-MO.
00967
00968      IF WS-EOM-DT (MINDEX) NOT GREATER THAN PI-CR-MONTH-END-DT
00969          GO TO 1040-CHECK-INPUT-DATE.
00970
00971      MOVE PI-CR-MONTH-END-DT  TO  DC-BIN-DATE-1.
00972      MOVE WS-EOM-DT (MINDEX)  TO  DC-BIN-DATE-2.
00973      MOVE '1'                 TO  DC-OPTION-CODE.
00974      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.
00975
00976      IF DATE-CONVERSION-ERROR
00977          GO TO 1040-DAY-ERROR.
00978
00979      IF DC-ELAPSED-MONTHS  GREATER THAN +2
00980          MOVE -1        TO  EOM-DT-LEN (INDX)
00981          MOVE AL-UNBON  TO  EOM-DT-ATTRB (INDX)
00982          MOVE ER-0761   TO  EMI-ERROR
00983          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00984
00985      GO TO 1040-CHECK-INPUT-DATE.
00986
00987  1040-DAY-ERROR.
00988
00989      MOVE -1                    TO  EOM-DT-LEN (INDX).
00990      MOVE AL-UNBON              TO  EOM-DT-ATTRB (INDX).
00991      MOVE ER-0587               TO  EMI-ERROR.
00992      PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.
00993
00994  1040-CHECK-INPUT-DATE.
00995
00996 ****************************************************************
00997 **   THIS CODE EDITS THE INPUT DATE
00998 ****************************************************************
00999
01000      IF INDATE-LEN (INDX)  = ZEROS
01001          GO TO 1040-CONTINUE.
01002
01003      MOVE AL-UNNON              TO  INDATE-ATTRB (INDX).
01004      MOVE INDATE (INDX)         TO  DEEDIT-FIELD.
01005      PERFORM 8600-DEEDIT.
01006
01007      IF DEEDIT-FIELD-V0  NOT NUMERIC
01008         MOVE -1                  TO  INDATE-LEN (INDX)
01009         MOVE AL-UNBON            TO  INDATE-ATTRB (INDX)
01010         MOVE ER-0714             TO  EMI-ERROR
01011         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01012
01013      MOVE DEEDIT-FIELD-V0       TO  DC-GREG-DATE-1-MDY.
01014      MOVE '4'                   TO  DC-OPTION-CODE.
01015      PERFORM 8500-DATE-CONVERT  THRU 8500-EXIT.
01016
01017      IF DATE-CONVERSION-ERROR
01018         MOVE -1                  TO  INDATE-LEN (INDX)
01019         MOVE AL-UNBON            TO  INDATE-ATTRB (INDX)
01020         MOVE ER-0714             TO  EMI-ERROR
01021         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01022      ELSE
01023         SET DINDEX               TO  INDX
01024         MOVE DC-BIN-DATE-1       TO  WS-INP-DT (DINDEX).
01025
01026  1040-CONTINUE.
01027
01028      IF MORTGAGE-SESSION
01029         IF INVOICE-LEN (INDX) = ZEROS
01030            MOVE -1               TO INVOICE-LEN (INDX)
01031            MOVE ER-3175          TO EMI-ERROR
01032            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01033            GO TO 1060-INCREMENT-INDX
01034         ELSE
01035            PERFORM 6500-VERIFY-RECON-HEADER THRU 6590-EXIT.
01036
01037      IF INVOICE (INDX) = LOW-VALUES
01038          GO TO 1060-INCREMENT-INDX.
01039
01040      IF WS-ACCEPT-1 (W-INDX) = 'A'
01041          GO TO 1060-INCREMENT-INDX.
01042
01043      IF EIBAID = DFHPF5
01044          MOVE 'Y'                TO  WS-PREV-PF5
01045          MOVE DFHENTER           TO  EIBAID
01046          MOVE 'A'                TO  WS-ACCEPT-1 (W-INDX)
01047          GO TO 1060-INCREMENT-INDX.
01048
01049      MOVE PI-COMPANY-CD          TO  RECV-COMP-CD.
01050      MOVE '1'                    TO  RECV-TYPE.
01051      MOVE CARRIER (INDX)         TO  RECV-CARRIER.
01052      MOVE GROUPING (INDX)        TO  RECV-GROUPING.
01053      MOVE PI-SAVE-COMP-BAL-CODE  TO  RECV-BAL-LVL.
01054      MOVE FIN-RESP (INDX)        TO  RECV-FIN-RESP.
01055      MOVE ACCT (INDX)            TO  RECV-ACCOUNT.
01056      MOVE INVOICE (INDX)         TO  RECV-INVOICE.
01057
01058      IF APPLIED (INDX) = 'A'
01059          MOVE '1'                TO  RECV-ENTRY-TYPE
01060          MOVE 'A'                TO  RECV-RESPONSIBLE
01061      ELSE
01062          MOVE LOW-VALUES         TO  RECV-RESPONSIBLE
01063          IF APPLIED (INDX) = 'G'
01064              MOVE '2'            TO  RECV-ENTRY-TYPE
01065          ELSE
01066              MOVE '3'            TO  RECV-ENTRY-TYPE.
01067
01068      IF REF-LEN (INDX) NOT = ZEROS
01069          MOVE REF (INDX)         TO  RECV-REFERENCE
01070      ELSE
01071          MOVE LOW-VALUES         TO  RECV-REFERENCE.
01072
01073      MOVE ZERO                   TO  RECV-RECORD-TYPE.
01074      MOVE +0                     TO  RECV-RECORD-SEQ.
01075
01076      INSPECT ERRECV-KEY CONVERTING SPACES TO LOW-VALUES.
01077
01078      
      * EXEC CICS HANDLE CONDITION
01079 *        NOTFND   (1050-RECV-NOTFND)
01080 *        NOTOPEN  (7200-RECV-FILE-NOTOPEN)
01081 *        END-EXEC.
      *    MOVE '"$IJ                  ! ( #00003675' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303033363735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01082
01083      IF RECV-REFERENCE  =  LOW-VALUES
01084          IF MORTGAGE-SESSION
01085              MOVE SPACES              TO RECV-REFERENCE.
01086
01087      
      * EXEC CICS READ
01088 *        DATASET  (RECV-FILE-ID)
01089 *        SET      (ADDRESS OF ACCOUNTS-RECEIVABLE)
01090 *        RIDFLD   (ERRECV-KEY)
01091 *        EQUAL
01092 *        END-EXEC.
      *    MOVE '&"S        E          (   #00003684' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 RECV-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERRECV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNTS-RECEIVABLE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01093
01094      GO TO 1060-INCREMENT-INDX.
01095
01096  1050-RECV-NOTFND.
01097      MOVE ER-3180                TO EMI-ERROR.
01098      MOVE -1                     TO INVOICE-LEN (INDX).
01099      MOVE AL-UABON               TO INVOICE-ATTRB (INDX).
01100
01101      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01102
01103  1060-INCREMENT-INDX.
01104
01105      IF INDX  IS LESS THAN +5
01106          SET INDX  UP  BY  1
01107          SET W-INDX TO INDX
01108          GO TO 1010-EDIT-LOOP.
01109
01110      MOVE WS-ACCEPT             TO  PI-ACCEPT.
01111
01112      IF EMI-ERROR = ZEROS
01113          GO TO 2000-UPDATE-THE-FILE
01114      ELSE
01115          GO TO 8200-SEND-DATAONLY.
01116  EJECT
01117  2000-UPDATE-THE-FILE.
01118      SET INDX                    TO  1.
01119
01120  2100-UPDATE-LOOP.
01121
01122      IF INDX  IS GREATER THAN  +5
01123          GO TO 2200-UPDATE-COMPLETE.
01124
01125      IF CARR-LEN        (INDX) = ZEROS
01126         AND GRP-LEN     (INDX) = ZEROS
01127         AND FIN-LEN     (INDX) = ZEROS
01128         AND ACCT-LEN    (INDX) = ZEROS
01129         AND COMM-LEN    (INDX) = ZEROS
01130         AND RTYPE-LEN   (INDX) = ZEROS
01131         AND AMT-LEN     (INDX) = ZEROS
01132         AND EOM-DT-LEN  (INDX) = ZEROS
01133         AND INDATE-LEN  (INDX) = ZEROS
01134         SET INDX  UP  BY  1
01135         GO TO 2100-UPDATE-LOOP.
01136
01137      
      * EXEC CICS GETMAIN
01138 *        SET      (ADDRESS OF PENDING-PAY-ADJ)
01139 *        LENGTH   (ERPYAJ-RECORD-LENGTH)
01140 *        INITIMG  (GETMAIN-SPACE)
01141 *        END-EXEC.
      *    MOVE ',"IL                  $   #00003734' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERPYAJ-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01142
01143      MOVE SPACES                 TO  PENDING-PAY-ADJ.
01144
01145      MOVE 'PY'                   TO  PY-RECORD-ID.
01146      MOVE PI-COMPANY-CD          TO  PY-COMPANY-CD.
01147
01148      IF CARR-LEN   (INDX)        GREATER THAN ZEROS
01149         MOVE CARRIER     (INDX)  TO  PY-CARRIER.
01150
01151      IF GRP-LEN       (INDX)     GREATER THAN ZEROS
01152         MOVE GROUPING (INDX)     TO  PY-GROUPING.
01153
01154      IF FIN-LEN       (INDX)     GREATER THAN ZEROS
01155         MOVE FIN-RESP (INDX)     TO  PY-FIN-RESP.
01156
01157      IF ACCT-LEN      (INDX)     GREATER THAN ZEROS
01158         MOVE ACCT     (INDX)     TO  PY-ACCOUNT.
01159
01160      MOVE RTYPE       (INDX)     TO  PY-RECORD-TYPE.
01161
01162      IF REF-LEN       (INDX) NOT = ZEROS
01163         MOVE REF      (INDX)     TO  PY-REF-NO.
01164
01165      IF INVOICE-LEN   (INDX) NOT = ZEROS
01166         MOVE INVOICE  (INDX)     TO  PY-BIL-INV.
01167
01168      IF CREDIT-LEN    (INDX) NOT = ZEROS
01169         MOVE CREDIT   (INDX)     TO  PY-GL-CR.
01170
01171      IF DEBIT-LEN     (INDX) NOT = ZEROS
01172         MOVE DEBIT    (INDX)     TO  PY-GL-DB.
01173
01174      SET MINDEX  TO  INDX.
01175
01176      IF WS-EOM-DT (MINDEX)  NOT = ZEROS
01177          MOVE WS-EOM-DT (MINDEX) TO  PY-CREDIT-SELECT-DT
01178      ELSE
01179          MOVE PI-CR-MONTH-END-DT TO  PY-CREDIT-SELECT-DT.
01180
01181      SET DINDEX  TO  INDX.
01182
01183      IF WS-INP-DT (DINDEX)  NOT = ZEROS
01184          MOVE WS-INP-DT (DINDEX) TO  PY-INPUT-DT
01185      ELSE
01186          MOVE WS-CURRENT-BIN-DT  TO  PY-INPUT-DT.
01187
01188      MOVE APPLIED (INDX)         TO  PY-PMT-APPLIED.
01189      MOVE WORK-SEQ-NO            TO  PY-FILE-SEQ-NO.
01190
01191      ADD +1                      TO  WORK-SEQ-NO.
01192
01193      IF COMM-LEN (INDX) NOT = ZEROS
01194          MOVE COMM (INDX)        TO  PY-ENTRY-COMMENT.
01195
01196      SET WS-INDX                 TO  INDX.
01197      MOVE WS-EDITED-AMT(WS-INDX) TO  PY-ENTRY-AMT
01198                                      WS-ENTRY-AMT.
01199      MOVE PI-PROCESSOR-ID        TO  PY-LAST-MAINT-BY.
01200      MOVE EIBTIME                TO  PY-LAST-MAINT-HHMMSS.
01201      MOVE WS-CURRENT-BIN-DT      TO  PY-LAST-MAINT-DT.
01202 *                                    PY-INPUT-DT.
01203      MOVE ZEROS                  TO  PY-CHECK-QUE-CONTROL
01204                                      PY-CHECK-QUE-SEQUENCE.
01205      MOVE LOW-VALUES             TO  PY-CREDIT-ACCEPT-DT
01206                                      PY-BILLED-DATE
01207                                      PY-AR-DATE
01208                                      PY-REPORTED-DT
01209                                      PY-CHECK-WRITTEN-DT
01210                                      PY-GL-DATE.
01211
01212  2115-WRITE-REC.
01213
01214      IF MORTGAGE-SESSION
01215          MOVE PI-MP-MONTH-END-DT TO  PY-CREDIT-SELECT-DT.
01216
01217      MOVE 'A'                    TO  JP-RECORD-TYPE.
01218      MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.
01219
01220      IF MORTGAGE-SESSION
01221         PERFORM 6600-UPDATE-RECON-HEADER THRU 6690-EXIT.
01222
01223      
      * EXEC CICS HANDLE CONDITION
01224 *        DUPREC   (2125-DUP-RECORD)
01225 *        NOTOPEN  (7000-PYAJ-FILE-NOTOPEN)
01226 *        END-EXEC.
      *    MOVE '"$%J                  ! ) #00003820' TO DFHEIV0
           MOVE X'2224254A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303033383230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01227
01228  2120-WRITE-PYAJ-REC.
01229
01230      
      * EXEC CICS WRITE
01231 *        DATASET  (PYAJ-FILE-ID)
01232 *        FROM     (PENDING-PAY-ADJ)
01233 *        RIDFLD   (PY-CONTROL-PRIMARY)
01234 *        END-EXEC.
           MOVE LENGTH OF
            PENDING-PAY-ADJ
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00003827' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 PENDING-PAY-ADJ, 
                 DFHEIV11, 
                 PY-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01235
01236      PERFORM 8400-LOG-JOURNAL-RECORD.
01237
01238      MOVE LOW-VALUES             TO  DATA-AREA (INDX).
01239
01240      GO TO 2100-UPDATE-LOOP.
01241
01242  2125-DUP-RECORD.
01243
01244      COMPUTE PY-FILE-SEQ-NO = PY-FILE-SEQ-NO + 1.
01245
01246      GO TO 2120-WRITE-PYAJ-REC.
01247  2200-UPDATE-COMPLETE.
01248
01249      MOVE LOW-VALUES             TO  EL635BI.
01250      MOVE ER-0000                TO  EMI-ERROR
01251      MOVE -1                     TO  CAR1L.
01252
01253      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01254
01255      GO TO 8100-SEND-INITIAL-MAP.
01256
01257      EJECT
01258
01259  6500-VERIFY-RECON-HEADER.
01260
01261      IF RTYPE-LEN (INDX)  GREATER THAN ZEROS
01262         IF RTYPE  (INDX)  NOT = 'R'
01263            GO TO 6590-EXIT.
01264
01265      MOVE PI-COMPANY-CD           TO MPPRCN-COMPANY-CD.
01266      MOVE INVOICE (INDX)          TO MPPRCN-INVOICE.
01267      MOVE +999999999              TO MPPRCN-RECORD-SEQU.
01268
01269      
      * EXEC CICS HANDLE CONDITION
01270 *        NOTFND   (6580-HEADER-NOTFND)
01271 *        END-EXEC.
      *    MOVE '"$I                   ! * #00003866' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303033383636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01272
01273      
      * EXEC CICS READ
01274 *        DATASET   (MPPRCN-FILE-ID)
01275 *        SET       (ADDRESS OF PAYMENT-RECONCILIATION)
01276 *        RIDFLD    (MPPRCN-KEY)
01277 *        END-EXEC.
      *    MOVE '&"S        E          (   #00003870' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MPPRCN-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 MPPRCN-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PAYMENT-RECONCILIATION TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01278
01279      IF  NOT PI-NO-CARRIER-SECURITY
01280              AND
01281          PI-CARRIER-SECURITY NOT EQUAL PR-CARRIER-A2
01282          MOVE -1                 TO CARR-LEN      (INDX)
01283          MOVE AL-UANON           TO CARR-ATTRB    (INDX)
01284          MOVE ER-9095            TO EMI-ERROR
01285          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01286          GO TO 8200-SEND-DATAONLY.
01287
01288      MOVE PR-SECURITY-ACCESS-CODE
01289                                  TO SC-SECURITY-ACCESS-CODE.
01290      PERFORM 9920-PRODUCER-EVALUATION THRU 9920-EXIT.
01291
01292      IF  SC-PRODUCER-NOT-AUTHORIZED
01293          MOVE -1                 TO ACCT-LEN      (INDX)
01294          MOVE AL-UANON           TO ACCT-ATTRB    (INDX)
01295          MOVE ER-9094            TO EMI-ERROR
01296          GO TO 8200-SEND-DATAONLY.
01297
01298      IF PR-HDR-POSTED
01299         GO TO 6560-POST-ERROR.
01300
01301      GO TO 6590-EXIT.
01302
01303  6560-POST-ERROR.
01304
01305      MOVE -1                     TO INVOICE-LEN (INDX)
01306      MOVE ER-9296                TO EMI-ERROR.
01307      PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.
01308
01309      GO TO 6590-EXIT.
01310
01311  6580-HEADER-NOTFND.
01312
01313      MOVE -1                     TO INVOICE-LEN (INDX)
01314      MOVE ER-9280                TO EMI-ERROR.
01315      PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.
01316
01317  6590-EXIT.
01318      EXIT.
01319
01320      EJECT
01321
01322
01323  6600-UPDATE-RECON-HEADER.
01324
01325      IF (PY-REMIT-RECEIVED OR PY-REMIT-IND-GROUPING)
01326         NEXT SENTENCE
01327      ELSE
01328         GO TO 6690-EXIT.
01329
01330      MOVE PI-COMPANY-CD           TO MPPRCN-COMPANY-CD.
01331      MOVE INVOICE (INDX)          TO MPPRCN-INVOICE.
01332      MOVE +999999999              TO MPPRCN-RECORD-SEQU.
01333
01334      
      * EXEC CICS HANDLE CONDITION
01335 *        NOTFND   (6680-HEADER-NOTFND)
01336 *        END-EXEC.
      *    MOVE '"$I                   ! + #00003931' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303033393331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01337
01338      
      * EXEC CICS READ
01339 *        DATASET   (MPPRCN-FILE-ID)
01340 *        SET       (ADDRESS OF PAYMENT-RECONCILIATION)
01341 *        RIDFLD    (MPPRCN-KEY)
01342 *        UPDATE
01343 *        END-EXEC.
      *    MOVE '&"S        EU         (   #00003935' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MPPRCN-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 MPPRCN-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PAYMENT-RECONCILIATION TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01344
01345      IF PR-HDR-POSTED
01346         GO TO 6660-POST-ERROR.
01347
01348      ADD  WS-ENTRY-AMT           TO PR-RECEIVED-PREMIUM.
01349      MOVE WS-CURRENT-BIN-DT      TO PR-RECEIVED-DT.
01350
01351      MOVE PI-PROCESSOR-ID        TO PR-LAST-CHANGE-PROCESSOR.
01352      MOVE EIBTIME                TO PR-LAST-CHANGE-TIME.
01353      MOVE WS-CURRENT-BIN-DT      TO PR-LAST-CHANGE-DT.
01354
01355      
      * EXEC CICS REWRITE
01356 *        DATASET   (MPPRCN-FILE-ID)
01357 *        FROM      (PAYMENT-RECONCILIATION)
01358 *        END-EXEC.
           MOVE LENGTH OF
            PAYMENT-RECONCILIATION
             TO DFHEIV11
      *    MOVE '&& L                  %   #00003952' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MPPRCN-FILE-ID, 
                 PAYMENT-RECONCILIATION, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01359
01360      GO TO 6690-EXIT.
01361
01362  6660-POST-ERROR.
01363
01364      
      * EXEC CICS SYNCPOINT ROLLBACK
01365 *    END-EXEC.
      *    MOVE '6"R                   !   #00003961' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01366
01367      MOVE -1                     TO INVOICE-LEN (INDX)
01368      MOVE ER-9296                TO EMI-ERROR.
01369      PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.
01370      GO TO 8200-SEND-DATAONLY.
01371
01372  6680-HEADER-NOTFND.
01373
01374      
      * EXEC CICS SYNCPOINT ROLLBACK
01375 *    END-EXEC.
      *    MOVE '6"R                   !   #00003971' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01376
01377      MOVE -1                     TO INVOICE-LEN (INDX).
01378      MOVE ER-9280                TO EMI-ERROR.
01379      PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.
01380      GO TO 8200-SEND-DATAONLY.
01381
01382  6690-EXIT.
01383      EXIT.
01384
01385      EJECT
01386
01387  EJECT
01388  7000-PYAJ-FILE-NOTOPEN.
01389      MOVE -1                     TO  PFENTERL.
01390      MOVE ER-2232                TO  EMI-ERROR.
01391
01392      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01393
01394      GO TO 8200-SEND-DATAONLY.
01395
01396  7100-COMP-FILE-NOTOPEN.
01397      MOVE -1                     TO  PFENTERL.
01398      MOVE ER-2233                TO  EMI-ERROR.
01399
01400      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01401
01402      GO TO 8200-SEND-DATAONLY.
01403  7200-RECV-FILE-NOTOPEN.
01404      MOVE -1                     TO  PFENTERL.
01405      MOVE ER-3177                TO  EMI-ERROR.
01406
01407      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01408
01409      GO TO 8200-SEND-DATAONLY.
01410  EJECT
01411  8100-SEND-INITIAL-MAP.
01412
01413      MOVE WS-CURRENT-DT          TO  DATEO.
01414      MOVE EIBTIME                TO  TIME-IN.
01415      MOVE TIME-OUT               TO  TIMEO.
01416      MOVE -1                     TO  CAR1L.
01417      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
01418      MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.
01419
01420      IF MORTGAGE-SESSION
01421         MOVE  'PROD:'            TO ACTHDG1O
01422         MOVE AL-SANOF            TO ACTHDG1A
01423         MOVE  'PROD:'            TO ACTHDG2O
01424         MOVE AL-SANOF            TO ACTHDG2A
01425         MOVE  'PROD:'            TO ACTHDG3O
01426         MOVE AL-SANOF            TO ACTHDG3A
01427         MOVE  'PROD:'            TO ACTHDG4O
01428         MOVE AL-SANOF            TO ACTHDG4A
01429         MOVE  'PROD:'            TO ACTHDG5O
01430         MOVE AL-SANOF            TO ACTHDG5A.
01431
01432      
      * EXEC CICS SEND
01433 *        MAP     (MAP-NAME)
01434 *        MAPSET  (MAPSET-NAME)
01435 *        FROM    (EL635BO)
01436 *        ERASE
01437 *        CURSOR
01438 *        END-EXEC.
           MOVE LENGTH OF
            EL635BO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00004029' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL635BO, 
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
           
01439
01440      GO TO 9100-RETURN-TRAN.
01441  EJECT
01442  8200-SEND-DATAONLY.
01443
01444      MOVE WS-CURRENT-DT          TO  DATEO.
01445      MOVE EIBTIME                TO  TIME-IN.
01446      MOVE TIME-OUT               TO  TIMEO.
01447      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
01448      MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.
01449
01450      
      * EXEC CICS SEND
01451 *        MAP     (MAP-NAME)
01452 *        MAPSET  (MAPSET-NAME)
01453 *        FROM    (EL635BO)
01454 *        DATAONLY
01455 *        CURSOR
01456 *        END-EXEC.
           MOVE LENGTH OF
            EL635BO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00004047' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL635BO, 
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
           
01457
01458      GO TO 9100-RETURN-TRAN.
01459
01460  8300-SEND-TEXT.
01461      
      * EXEC CICS SEND TEXT
01462 *        FROM    (LOGOFF-TEXT)
01463 *        LENGTH  (LOGOFF-LENGTH)
01464 *        ERASE
01465 *        FREEKB
01466 *        END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00004058' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303538' TO DFHEIV0(25:11)
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
           
01467
01468      
      * EXEC CICS RETURN
01469 *        END-EXEC.
      *    MOVE '.(                    &   #00004065' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01470  EJECT
01471  8400-LOG-JOURNAL-RECORD.
01472 *    MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.
01473 *    MOVE THIS-PGM               TO  JP-PROGRAM-ID.
01474 *    MOVE PYAJ-FILE-ID           TO  JP-FILE-ID.
01475 *    MOVE ZERO                   TO  JP-GENERIC-KEY-LENGTH.
01476
01477 *    EXEC CICS JOURNAL
01478 *        JFILEID  (PI-JOURNAL-FILE-ID)
01479 *        JTYPEID  ('EL')
01480 *        FROM     (JOURNAL-RECORD)
01481 *        LENGTH   (223)
01482 *        END-EXEC.
01483
01484  8500-DATE-CONVERT.
01485      
      * EXEC CICS LINK
01486 *        PROGRAM   (LINK-CLDATCV)
01487 *        COMMAREA  (DATE-CONVERSION-DATA)
01488 *        LENGTH    (DC-COMM-LENGTH)
01489 *        END-EXEC.
      *    MOVE '."C                   ''   #00004082' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LINK-CLDATCV, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01490
01491  8500-EXIT.
01492      EXIT.
01493
01494  8600-DEEDIT.
01495      
      * EXEC CICS BIF DEEDIT
01496 *        FIELD   (DEEDIT-FIELD)
01497 *        LENGTH  (11)
01498 *        END-EXEC.
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004092' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034303932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01499
01500  8600-EXIT.
01501      EXIT.
01502  EJECT
01503  8800-UNAUTHORIZED-ACCESS.
01504      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
01505
01506      GO TO 8300-SEND-TEXT.
01507
01508  8810-PF23.
01509      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
01510      MOVE XCTL-005               TO  PGM-NAME.
01511
01512      GO TO 9300-XCTL.
01513
01514  9000-RETURN-CICS.
01515      
      * EXEC CICS RETURN
01516 *        END-EXEC.
      *    MOVE '.(                    &   #00004112' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01517
01518  9100-RETURN-TRAN.
01519      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
01520      MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.
01521
01522      
      * EXEC CICS RETURN
01523 *        TRANSID   (TRANS-ID)
01524 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
01525 *        LENGTH    (PI-COMM-LENGTH)
01526 *        END-EXEC.
      *    MOVE '.(CT                  &   #00004119' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01527
01528  9200-RETURN-MAIN-MENU.
01529
01530      IF  CREDIT-SESSION
01531          MOVE XCTL-EL626         TO PGM-NAME
01532
01533      ELSE
01534          IF  CLAIM-SESSION
01535              MOVE XCTL-EL126     TO PGM-NAME
01536
01537          ELSE
01538              IF  MORTGAGE-SESSION
01539                  MOVE XCTL-EM626 TO PGM-NAME
01540
01541              ELSE
01542                  IF  GENERAL-LEDGER-SESSION
01543                      MOVE XCTL-GL800
01544                                  TO PGM-NAME.
01545
01546      GO TO 9300-XCTL.
01547
01548  9300-XCTL.
01549      
      * EXEC CICS XCTL
01550 *        PROGRAM   (PGM-NAME)
01551 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
01552 *        LENGTH    (PI-COMM-LENGTH)
01553 *        END-EXEC.
      *    MOVE '.$C                   $   #00004146' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01554
01555  9400-CLEAR.
01556      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.
01557
01558      GO TO 9300-XCTL.
01559
01560  9500-PF12.
01561      MOVE XCTL-010               TO  PGM-NAME.
01562
01563      GO TO 9300-XCTL.
01564
01565  9600-PGMID-ERROR.
01566      
      * EXEC CICS HANDLE CONDITION
01567 *        PGMIDERR  (8300-SEND-TEXT)
01568 *        END-EXEC.
      *    MOVE '"$L                   ! , #00004163' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303034313633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01569
01570      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.
01571      MOVE ' '                    TO  PI-ENTRY-CD-1.
01572      MOVE XCTL-005               TO  PGM-NAME.
01573      MOVE PGM-NAME               TO  LOGOFF-PGM.
01574      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
01575
01576      GO TO 9300-XCTL.
01577
01578  9920-PRODUCER-EVALUATION.
01579 ******************************************************************
01580 *                                                                *
01581 *       THIS AREA CONTAINS THE LOGIC NECESSARY TO CHECK THE      *
01582 *       AUTHORIZATION OF THE USER TO 'ACCESS' A PRODUCER.        *
01583 *       IF THE USER IS NOT AUTHORIZED AN APPROPRIATE MESSAGE     *
01584 *       IS GENERATED.                                            *
01585 *                                                                *
01586 ******************************************************************
01587
01588      MOVE SPACES                 TO SC-PRODUCER-AUTHORIZED-SW.
01589
01590      IF  PI-NO-PRODUCER-SECURITY
01591              OR
01592          PI-PROCESSOR-ID EQUAL 'LGXX'
01593          GO TO 9920-EXIT.
01594
01595      SET PI-ACCESS-NDX           TO +1
01596
01597      SEARCH PI-ACCESS-CODE
01598          VARYING PI-ACCESS-NDX
01599
01600          AT END
01601              MOVE 'N'            TO SC-PRODUCER-AUTHORIZED-SW
01602
01603          WHEN
01604              PI-ACCESS-CODE (PI-ACCESS-NDX)
01605                  EQUAL SC-SECURITY-ACCESS-CODE
01606              GO TO 9920-EXIT.
01607
01608  9920-EXIT.
01609      EXIT.
01610      EJECT
01611  9900-ERROR-FORMAT.
01612      IF NOT EMI-ERRORS-COMPLETE
01613          MOVE LINK-001           TO  PGM-NAME
01614          
      * EXEC CICS LINK
01615 *            PROGRAM   (PGM-NAME)
01616 *            COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
01617 *            LENGTH    (EMI-COMM-LENGTH)
01618 *            END-EXEC.
      *    MOVE '."C                   ''   #00004211' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01619
01620  9900-EXIT.
01621      EXIT.
01622
01623  9990-ABEND.
01624      MOVE LINK-004               TO  PGM-NAME.
01625      MOVE DFHEIBLK               TO  EMI-LINE1.
01626
01627      
      * EXEC CICS LINK
01628 *        PROGRAM   (PGM-NAME)
01629 *        COMMAREA  (EMI-LINE1)
01630 *        LENGTH    (72)
01631 *        END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00004224' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01632
01633      MOVE -1                     TO  PFENTERL.
01634
01635      GO TO 8200-SEND-DATAONLY.
01636
01637      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6351' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
01638
01639  9995-SECURITY-VIOLATION.
01640 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00004254' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323534' TO DFHEIV0(25:11)
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
01641
01642  9995-EXIT.
01643      EXIT.
01644

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6351' TO DFHEIV1
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
               GO TO 1015-NO-COMP-MSTR,
                     7100-COMP-FILE-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 1015-NO-COMP-MSTR,
                     7100-COMP-FILE-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 1015-NO-COMP-MSTR,
                     7100-COMP-FILE-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 1015-NO-COMP-MSTR,
                     7100-COMP-FILE-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 1015-NO-COMP-MSTR,
                     7100-COMP-FILE-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 1050-RECV-NOTFND,
                     7200-RECV-FILE-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 2125-DUP-RECORD,
                     7000-PYAJ-FILE-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 6580-HEADER-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 6680-HEADER-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6351' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
