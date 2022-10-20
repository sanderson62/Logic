00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL8521.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/14/96 08:06:29.
00007 *                            VMOD=2.008
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
00024 *REMARKS. TRANSACTION - EXJ6 - ACCOUNTS RECEIVABLE
00025 *                              REQUEST FILE SELECTION.
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
122002******************************************************************
00026
00027  ENVIRONMENT DIVISION.
00028
00029      EJECT
00030  DATA DIVISION.
00031  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00032
00033  77  FILLER  PIC X(32)  VALUE '********************************'.
00034  77  FILLER  PIC X(32)  VALUE '*   EL8521 WORKING STORAGE     *'.
00035  77  FILLER  PIC X(32)  VALUE '************VMOD=2.008 *********'.
00036
00037     EJECT
00038
00039  77  FIRST-ERROR-SCAN-SW     PIC X         VALUE 'N'.
00040      88  FIRST-ERROR-NOT-FOUND             VALUE 'N'.
00041  77  UPDATE-RQST-SW          PIC X         VALUE 'N'.
00042      88  UPDATE-RQST-DT                    VALUE 'Y'.
00043  77  UPDATE-STMT-SW          PIC X         VALUE 'N'.
00044      88  UPDATE-STMT-DT                    VALUE 'Y'.
00045  77  UPDATE-STATUS-SW        PIC X         VALUE 'N'.
00046      88  UPDATE-STATUS                     VALUE 'Y'.
00047 *                            COPY ELCSCTM.
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
00048 *                            COPY ELCSCRTY.
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
00049
00050     EJECT
00051
00052 ******************************************************************
00053 *                                                                *
00054 *              S T A N D A R D   A R E A S                       *
00055 *                                                                *
00056 ******************************************************************
00057
00058  01  STANDARD-AREAS.
00059      12  SC-ITEM                 PIC S9(4)   VALUE +1    COMP.
00060      12  QID.
00061          16  QID-TERM            PIC X(4)      VALUE SPACES.
00062          16  FILLER              PIC X(4)      VALUE '125D'.
00063      12  GETMAIN-SPACE           PIC X       VALUE SPACE.
00064      12  EL852B                  PIC X(8)    VALUE 'EL852B'.
00065      12  EL852C                  PIC X(8)    VALUE 'EL852C'.
00066      12  EL852D                  PIC X(8)    VALUE 'EL852D'.
00067      12  MAPSET-EL8521S          PIC X(8)    VALUE 'EL8521S'.
00068      12  TRANS-EXJ6              PIC X(4)    VALUE 'EXJ6'.
00069      12  THIS-PGM                PIC X(8)    VALUE 'EL8521'.
00070      12  PGM-NAME                PIC X(8).
00071      12  TIME-IN                 PIC S9(7).
00072      12  TIME-OUT-R  REDEFINES TIME-IN.
00073          16  FILLER              PIC X.
00074          16  TIME-OUT            PIC 99V99.
00075          16  FILLER              PIC X(2).
00076      12  LINK-EL001              PIC X(8)    VALUE 'EL001'.
00077      12  LINK-EL004              PIC X(8)    VALUE 'EL004'.
00078      12  XCTL-EL005              PIC X(8)    VALUE 'EL005'.
00079      12  XCTL-EL010              PIC X(8)    VALUE 'EL010'.
00080      12  XCTL-EL626              PIC X(8)    VALUE 'EL626'.
00081      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.
00082      12  FILE-ID-ERRQST          PIC X(8)    VALUE 'ERRQST'.
00083      12  FILE-ID-ERACCT          PIC X(8)    VALUE 'ERACCT'.
00084      12  FILE-ID-ERCOMP          PIC X(8)    VALUE 'ERCOMP'.
00085      12  FILE-ID-ERSUMM          PIC X(8)    VALUE 'ERSUMM'.
00086      12  FILE-ID-ERPNDB          PIC X(8)    VALUE 'ERPNDB'.
00087      12  WS-CURRENT-DT           PIC X(8)    VALUE SPACES.
00088      12  WS-CURRENT-BIN-DT       PIC XX      VALUE SPACES.
00089      12  WS-DATA-SELECTED-SW     PIC X       VALUE SPACE.
00090          88 WS-DATA-SELECTED                 VALUE 'Y'.
00091      12  WS-OPTION-COUNTER       PIC S999    VALUE ZEROS COMP-3.
00092      12  WS-MAINT-FUNCTION       PIC X       VALUE SPACE.
00093          88 WS-BROWSE-FUNCTION               VALUE 'B'.
00094          88 WS-SUBMIT-FUNCTION               VALUE 'S'.
00095          88 WS-RESUBMIT-FUNCTION             VALUE 'R'.
00096      12  WS-BROWSE-STARTED-SW    PIC X       VALUE SPACE.
00097          88 WS-BROWSE-STARTED                VALUE 'Y'.
00098      12  WS-SUB1                 PIC S9(4)   VALUE ZEROS COMP.
00099      12  WS-AM-NAME              PIC X(30)   VALUE SPACES.
00100      12  WS-FIN-RESP-NAME        PIC X(30)   VALUE SPACES.
00101      12  WS-RQST-ACCT-CONTROL    PIC X(20)   VALUE SPACES.
00102      12  WS-RQST-FIN-RESP-CNTL   PIC X(18)   VALUE SPACES.
00103      12  WS-SUM-NAME             PIC X(30)   VALUE SPACES.
00104      12  WS-SAVE-RQST-DT         PIC XX      VALUE LOW-VALUES.
00105      12  WS-SAVE-STMT-DT         PIC XX      VALUE LOW-VALUES.
00106      12  WS-SAVE-STATUS          PIC X       VALUE SPACES.
00107      12  WS-TRAILER-ONLY-SW      PIC X       VALUE 'Y'.
00108          88 WS-TRAILER-ONLY                  VALUE 'Y'.
00109      12  WS-NEW-ACCT-MAST-SW     PIC X.
00110          88 WS-NEW-ACCT-MAST                 VALUE 'Y'.
00111      12  WS-ACCOUNT-MASTER-SW    PIC X.
00112          88 WS-ACCOUNT-MASTER                VALUE 'Y'.
00113      12  WS-NO-ACCOUNT-MASTER-SW PIC X.
00114          88 WS-NO-ACCOUNT-MASTER             VALUE 'Y'.
00115      12  WS-AGT-ERROR-SW         PIC X.
00116          88 WS-AGT-ERROR                     VALUE 'Y'.
00117      12  WS-FIN-RESP-ERROR-SW    PIC X.
00118          88 WS-FIN-RESP-ERROR                VALUE 'Y'.
00119      12  WS-AGT-COMP-ERROR-SW    PIC X.
00120          88 WS-AGT-COMP-ERROR                VALUE 'Y'.
00121      12  WS-FIN-RESP-COMP-ERROR-SW
00122                                  PIC X.
00123          88 WS-FIN-RESP-COMP-ERROR           VALUE 'Y'.
00124      12  WS-ACCESS-ERPNDB.
00125          16  WS-ACCESS-CO-ID     PIC X.
00126          16  WS-ACCESS-BATCH     PIC X(6).
00127          16  WS-ACCESS-SEQ       PIC S9(4)   COMP.
00128          16  WS-ACCESS-CHG-SEQ   PIC S9(4)   COMP.
00129      12  WS-PREV-PNDB.
00130          16  WS-PREV-COMPANY     PIC X.
00131          16  WS-PREV-BATCH       PIC X(6).
00132      12  WS-SAVE-AM-CONTROLS.
00133          16  WS-SAVE-CARRIER     PIC X.
00134          16  WS-SAVE-GROUPING    PIC X(6).
00135          16  WS-SAVE-STATE       PIC X(2).
00136          16  WS-SAVE-ACCT-AGENT  PIC X(10).
00137          16  WS-SAVE-FIN-RESP    PIC X(10).
00138      12  WS-SAVE-AM-EFF-DT       PIC XX      VALUE LOW-VALUES.
00139      12  WS-SAVE-AM-EXP-DT       PIC XX      VALUE LOW-VALUES.
00140      12  WS-SAVE-SUMMARY         PIC X(6).
00141
00142      EJECT
00143
00144 ******************************************************************
00145 *                                                                *
00146 *                E R R O R   M E S S A G E S                     *
00147 *                                                                *
00148 ******************************************************************
00149
00150  01  ERROR-MESSAGES.
00151      12  ER-0000                 PIC X(4)  VALUE '0000'.
00152      12  ER-0004                 PIC X(4)  VALUE '0004'.
00153      12  ER-0008                 PIC X(4)  VALUE '0008'.
00154      12  ER-0023                 PIC X(4)  VALUE '0023'.
00155      12  ER-0029                 PIC X(4)  VALUE '0029'.
00156      12  ER-0070                 PIC X(4)  VALUE '0070'.
00157      12  ER-2211                 PIC X(4)  VALUE '2211'.
00158      12  ER-2132                 PIC X(4)  VALUE '2132'.
00159      12  ER-2134                 PIC X(4)  VALUE '2134'.
00160      12  ER-2210                 PIC X(4)  VALUE '2210'.
00161      12  ER-2242                 PIC X(4)  VALUE '2242'.
00162      12  ER-2251                 PIC X(4)  VALUE '2251'.
00163      12  ER-2252                 PIC X(4)  VALUE '2252'.
00164      12  ER-2800                 PIC X(4)  VALUE '2800'.
00165      12  ER-2919                 PIC X(4)  VALUE '2919'.
00166      12  ER-2935                 PIC X(4)  VALUE '2935'.
00167      12  ER-3131                 PIC X(4)  VALUE '3131'.
00168      12  ER-3132                 PIC X(4)  VALUE '3132'.
00169      12  ER-3134                 PIC X(4)  VALUE '3134'.
00170      12  ER-3136                 PIC X(4)  VALUE '3136'.
00171      12  ER-3191                 PIC X(4)  VALUE '3191'.
00172
00173      EJECT
00174
00175 ******************************************************************
00176 *                                                                *
00177 *              A C C E S S   K E Y S                             *
00178 *                                                                *
00179 ******************************************************************
00180
00181  01  ACCESS-KEYS.
00182
00183      12  ERRQST-KEY.
00184          16  ERRQST-COMPANY-CD       PIC X     VALUE SPACE.
00185          16  ERRQST-ENTRY-BATCH      PIC X(6)  VALUE SPACES.
00186
00187      12  ERRQST-ALT-KEY1.
00188          16  ERRQST-ACCT-CONTROL.
00189              20 ERRQST-COMPANY-CD-A1 PIC X     VALUE SPACES.
00190              20 ERRQST-CARRIER-A1    PIC X     VALUE SPACES.
00191              20 ERRQST-GROUPING-A1   PIC X(6)  VALUE SPACES.
00192              20 ERRQST-STATE-A1      PIC XX    VALUE SPACES.
00193              20 ERRQST-ACCOUNT-A1    PIC X(10) VALUE SPACES.
00194          16  ERRQST-REFERENCE-A1     PIC X(12) VALUE SPACES.
00195          16  ERRQST-BATCH-A1         PIC X(6)  VALUE SPACES.
00196
00197      12  ERRQST-ALT-KEY2.
00198          16  ERRQST-FIN-RESP-CONTROL.
00199              20 ERRQST-COMPANY-CD-A2 PIC X     VALUE SPACES.
00200              20 ERRQST-CARRIER-A2    PIC X     VALUE SPACES.
00201              20 ERRQST-GROUPING-A2   PIC X(6)  VALUE SPACES.
00202              20 ERRQST-FIN-RESP-A2   PIC X(10) VALUE SPACES.
00203          16  ERRQST-ACCOUNT-A2       PIC X(10) VALUE SPACES.
00204          16  ERRQST-REFERENCE-A2     PIC X(12) VALUE SPACES.
00205          16  ERRQST-BATCH-A2         PIC X(6)  VALUE SPACES.
00206
00207      12  ERRQST-ALT-KEY3.
00208          16  ERRQST-COMPANY-CD-A3    PIC X     VALUE SPACES.
00209          16  ERRQST-CARRIER-A3       PIC X     VALUE SPACES.
00210          16  ERRQST-GROUPING-A3      PIC X     VALUE SPACES.
00211          16  ERRQST-ACCOUNT-AGENT    PIC X     VALUE SPACES.
00212          16  ERRQST-BATCH-A3         PIC X     VALUE SPACES.
00213
00214      12  ERRQST-ALT-KEY4.
00215          16  ERRQST-COMPANY-CD-A4    PIC X     VALUE SPACES.
00216          16  ERRQST-SUMMARY-CODE     PIC X(6)  VALUE SPACES.
00217          16  ERRQST-ACCOUNT-A4       PIC X(10) VALUE SPACES.
00218          16  ERRQST-REFERENCE-A4     PIC X(12) VALUE SPACES.
00219          16  ERRQST-BATCH-A4         PIC X(6)  VALUE SPACES.
00220
00221      12  ERRQST-RECORD-LENGTH        PIC S9(4) COMP VALUE +200.
00222      12  ERRQST-JOURNAL-LENGTH       PIC S9(4) COMP VALUE +223.
00223
00224      12  ERCOMP-KEY.
00225          16  ERCOMP-COMP-CD          PIC X     VALUE SPACE.
00226          16  ERCOMP-CARRIER          PIC X     VALUE SPACES.
00227          16  ERCOMP-GROUPING         PIC X(6)  VALUE SPACES.
00228          16  ERCOMP-FIN-RESP         PIC X(10) VALUE SPACES.
00229          16  ERCOMP-ACCOUNT          PIC X(10) VALUE SPACES.
00230          16  ERCOMP-RECORD-TYPE      PIC X     VALUE SPACES.
00231
00232      12  ERACCT-KEY.
00233          16  ERACCT-COMP-KEY.
00234              20  ERACCT-COMP-CD     PIC X     VALUE SPACES.
00235              20  ERACCT-CARRIER     PIC X     VALUE SPACES.
00236              20  ERACCT-GROUPING    PIC X(6)  VALUE SPACES.
00237              20  ERACCT-STATE       PIC XX    VALUE SPACES.
00238              20  ERACCT-ACCOUNT     PIC X(10) VALUE SPACES.
00239          16  ACCT-EXP-DATE          PIC XX    VALUE SPACES.
00240          16  FILLER                 PIC X(4)  VALUE LOW-VALUES.
00241
00242      12  ERACCT-SAVE-KEY            PIC X(20) VALUE SPACES.
00243
00244      12  ERSUMM-KEY.
00245          16  ERSUMM-COMPANY-CD      PIC X     VALUE SPACE.
00246          16  ERSUMM-SUMMARY         PIC X(6)  VALUE SPACE.
00247          16  ERSUMM-CARRIER         PIC X     VALUE SPACE.
00248          16  ERSUMM-GROUP           PIC X(6)  VALUE SPACE.
00249          16  ERSUMM-FIN-RESP        PIC X(10) VALUE SPACE.
00250          16  ERSUMM-ACCT-AGENT      PIC X(10) VALUE SPACE.
00251
00252      12  ERPNDB-KEY.
00253          16  ERPNDB-COMPANY-CD      PIC X     VALUE SPACE.
00254          16  ERPNDB-BATCH           PIC X(6)  VALUE SPACE.
00255          16  ERPNDB-BATCH-SEQ       PIC S9(4) VALUE ZEROS COMP.
00256          16  ERPNDB-BATCH-CHG-SEQ   PIC S9(4) VALUE ZEROS COMP.
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
00272 *                            COPY ELC852PI.
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
00273      EJECT
00274 *                            COPY ELCJPFX.
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
00275                              PIC X(223).
00276
00277      EJECT
00278 *                            COPY ELCAID.
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
00279  01  FILLER    REDEFINES DFHAID.
00280      12  FILLER              PIC X(8).
00281      12  PF-VALUES           PIC X       OCCURS 2.
00282
00283      EJECT
00284 *                            COPY EL8521S.
       01  EL852BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  BDATEL PIC S9(0004) COMP.
           05  BDATEF PIC  X(0001).
           05  FILLER REDEFINES BDATEF.
               10  BDATEA PIC  X(0001).
           05  BDATEI PIC  X(0008).
      *    -------------------------------
           05  BTIMEL PIC S9(0004) COMP.
           05  BTIMEF PIC  X(0001).
           05  FILLER REDEFINES BTIMEF.
               10  BTIMEA PIC  X(0001).
           05  BTIMEI PIC  X(0005).
      *    -------------------------------
           05  BCARHDGL PIC S9(0004) COMP.
           05  BCARHDGF PIC  X(0001).
           05  FILLER REDEFINES BCARHDGF.
               10  BCARHDGA PIC  X(0001).
           05  BCARHDGI PIC  X(0004).
      *    -------------------------------
           05  BCARRL PIC S9(0004) COMP.
           05  BCARRF PIC  X(0001).
           05  FILLER REDEFINES BCARRF.
               10  BCARRA PIC  X(0001).
           05  BCARRI PIC  X(0001).
      *    -------------------------------
           05  BGRPHDGL PIC S9(0004) COMP.
           05  BGRPHDGF PIC  X(0001).
           05  FILLER REDEFINES BGRPHDGF.
               10  BGRPHDGA PIC  X(0001).
           05  BGRPHDGI PIC  X(0006).
      *    -------------------------------
           05  BGROUPL PIC S9(0004) COMP.
           05  BGROUPF PIC  X(0001).
           05  FILLER REDEFINES BGROUPF.
               10  BGROUPA PIC  X(0001).
           05  BGROUPI PIC  X(0006).
      *    -------------------------------
           05  BAGTHDGL PIC S9(0004) COMP.
           05  BAGTHDGF PIC  X(0001).
           05  FILLER REDEFINES BAGTHDGF.
               10  BAGTHDGA PIC  X(0001).
           05  BAGTHDGI PIC  X(0006).
      *    -------------------------------
           05  BAGENTL PIC S9(0004) COMP.
           05  BAGENTF PIC  X(0001).
           05  FILLER REDEFINES BAGENTF.
               10  BAGENTA PIC  X(0001).
           05  BAGENTI PIC  X(0010).
      *    -------------------------------
           05  BNAMHDGL PIC S9(0004) COMP.
           05  BNAMHDGF PIC  X(0001).
           05  FILLER REDEFINES BNAMHDGF.
               10  BNAMHDGA PIC  X(0001).
           05  BNAMHDGI PIC  X(0005).
      *    -------------------------------
           05  BNAMEL PIC S9(0004) COMP.
           05  BNAMEF PIC  X(0001).
           05  FILLER REDEFINES BNAMEF.
               10  BNAMEA PIC  X(0001).
           05  BNAMEI PIC  X(0030).
      *    -------------------------------
           05  BFILLERL PIC S9(0004) COMP.
           05  BFILLERF PIC  X(0001).
           05  FILLER REDEFINES BFILLERF.
               10  BFILLERA PIC  X(0001).
           05  BFILLERI PIC  X(0010).
      *    -------------------------------
           05  BSEQ1L PIC S9(0004) COMP.
           05  BSEQ1F PIC  X(0001).
           05  FILLER REDEFINES BSEQ1F.
               10  BSEQ1A PIC  X(0001).
           05  BSEQ1I PIC  X(0002).
      *    -------------------------------
           05  BCAR1L PIC S9(0004) COMP.
           05  BCAR1F PIC  X(0001).
           05  FILLER REDEFINES BCAR1F.
               10  BCAR1A PIC  X(0001).
           05  BCAR1I PIC  X(0001).
      *    -------------------------------
           05  BGRP1L PIC S9(0004) COMP.
           05  BGRP1F PIC  X(0001).
           05  FILLER REDEFINES BGRP1F.
               10  BGRP1A PIC  X(0001).
           05  BGRP1I PIC  X(0006).
      *    -------------------------------
           05  BST1L PIC S9(0004) COMP.
           05  BST1F PIC  X(0001).
           05  FILLER REDEFINES BST1F.
               10  BST1A PIC  X(0001).
           05  BST1I PIC  X(0002).
      *    -------------------------------
           05  BACCT1L PIC S9(0004) COMP.
           05  BACCT1F PIC  X(0001).
           05  FILLER REDEFINES BACCT1F.
               10  BACCT1A PIC  X(0001).
           05  BACCT1I PIC  X(0010).
      *    -------------------------------
           05  BREF1L PIC S9(0004) COMP.
           05  BREF1F PIC  X(0001).
           05  FILLER REDEFINES BREF1F.
               10  BREF1A PIC  X(0001).
           05  BREF1I PIC  X(0012).
      *    -------------------------------
           05  BBATCH1L PIC S9(0004) COMP.
           05  BBATCH1F PIC  X(0001).
           05  FILLER REDEFINES BBATCH1F.
               10  BBATCH1A PIC  X(0001).
           05  BBATCH1I PIC  X(0006).
      *    -------------------------------
           05  BSUM1L PIC S9(0004) COMP.
           05  BSUM1F PIC  X(0001).
           05  FILLER REDEFINES BSUM1F.
               10  BSUM1A PIC  X(0001).
           05  BSUM1I PIC  X(0006).
      *    -------------------------------
           05  BENTRY1L PIC S9(0004) COMP.
           05  BENTRY1F PIC  X(0001).
           05  FILLER REDEFINES BENTRY1F.
               10  BENTRY1A PIC  X(0001).
           05  BENTRY1I PIC  X(0006).
      *    -------------------------------
           05  BRQST1L PIC S9(0004) COMP.
           05  BRQST1F PIC  X(0001).
           05  FILLER REDEFINES BRQST1F.
               10  BRQST1A PIC  X(0001).
           05  BRQST1I PIC  X(0006).
      *    -------------------------------
           05  BSTMT1L PIC S9(0004) COMP.
           05  BSTMT1F PIC  X(0001).
           05  FILLER REDEFINES BSTMT1F.
               10  BSTMT1A PIC  X(0001).
           05  BSTMT1I PIC  X(0006).
      *    -------------------------------
           05  BSTATS1L PIC S9(0004) COMP.
           05  BSTATS1F PIC  X(0001).
           05  FILLER REDEFINES BSTATS1F.
               10  BSTATS1A PIC  X(0001).
           05  BSTATS1I PIC  X(0001).
      *    -------------------------------
           05  BSEQ2L PIC S9(0004) COMP.
           05  BSEQ2F PIC  X(0001).
           05  FILLER REDEFINES BSEQ2F.
               10  BSEQ2A PIC  X(0001).
           05  BSEQ2I PIC  X(0002).
      *    -------------------------------
           05  BCAR2L PIC S9(0004) COMP.
           05  BCAR2F PIC  X(0001).
           05  FILLER REDEFINES BCAR2F.
               10  BCAR2A PIC  X(0001).
           05  BCAR2I PIC  X(0001).
      *    -------------------------------
           05  BGRP2L PIC S9(0004) COMP.
           05  BGRP2F PIC  X(0001).
           05  FILLER REDEFINES BGRP2F.
               10  BGRP2A PIC  X(0001).
           05  BGRP2I PIC  X(0006).
      *    -------------------------------
           05  BST2L PIC S9(0004) COMP.
           05  BST2F PIC  X(0001).
           05  FILLER REDEFINES BST2F.
               10  BST2A PIC  X(0001).
           05  BST2I PIC  X(0002).
      *    -------------------------------
           05  BACCT2L PIC S9(0004) COMP.
           05  BACCT2F PIC  X(0001).
           05  FILLER REDEFINES BACCT2F.
               10  BACCT2A PIC  X(0001).
           05  BACCT2I PIC  X(0010).
      *    -------------------------------
           05  BREF2L PIC S9(0004) COMP.
           05  BREF2F PIC  X(0001).
           05  FILLER REDEFINES BREF2F.
               10  BREF2A PIC  X(0001).
           05  BREF2I PIC  X(0012).
      *    -------------------------------
           05  BBATCH2L PIC S9(0004) COMP.
           05  BBATCH2F PIC  X(0001).
           05  FILLER REDEFINES BBATCH2F.
               10  BBATCH2A PIC  X(0001).
           05  BBATCH2I PIC  X(0006).
      *    -------------------------------
           05  BSUM2L PIC S9(0004) COMP.
           05  BSUM2F PIC  X(0001).
           05  FILLER REDEFINES BSUM2F.
               10  BSUM2A PIC  X(0001).
           05  BSUM2I PIC  X(0006).
      *    -------------------------------
           05  BENTRY2L PIC S9(0004) COMP.
           05  BENTRY2F PIC  X(0001).
           05  FILLER REDEFINES BENTRY2F.
               10  BENTRY2A PIC  X(0001).
           05  BENTRY2I PIC  X(0006).
      *    -------------------------------
           05  BRQST2L PIC S9(0004) COMP.
           05  BRQST2F PIC  X(0001).
           05  FILLER REDEFINES BRQST2F.
               10  BRQST2A PIC  X(0001).
           05  BRQST2I PIC  X(0006).
      *    -------------------------------
           05  BSTMT2L PIC S9(0004) COMP.
           05  BSTMT2F PIC  X(0001).
           05  FILLER REDEFINES BSTMT2F.
               10  BSTMT2A PIC  X(0001).
           05  BSTMT2I PIC  X(0006).
      *    -------------------------------
           05  BSTATS2L PIC S9(0004) COMP.
           05  BSTATS2F PIC  X(0001).
           05  FILLER REDEFINES BSTATS2F.
               10  BSTATS2A PIC  X(0001).
           05  BSTATS2I PIC  X(0001).
      *    -------------------------------
           05  BSEQ3L PIC S9(0004) COMP.
           05  BSEQ3F PIC  X(0001).
           05  FILLER REDEFINES BSEQ3F.
               10  BSEQ3A PIC  X(0001).
           05  BSEQ3I PIC  X(0002).
      *    -------------------------------
           05  BCAR3L PIC S9(0004) COMP.
           05  BCAR3F PIC  X(0001).
           05  FILLER REDEFINES BCAR3F.
               10  BCAR3A PIC  X(0001).
           05  BCAR3I PIC  X(0001).
      *    -------------------------------
           05  BGRP3L PIC S9(0004) COMP.
           05  BGRP3F PIC  X(0001).
           05  FILLER REDEFINES BGRP3F.
               10  BGRP3A PIC  X(0001).
           05  BGRP3I PIC  X(0006).
      *    -------------------------------
           05  BST3L PIC S9(0004) COMP.
           05  BST3F PIC  X(0001).
           05  FILLER REDEFINES BST3F.
               10  BST3A PIC  X(0001).
           05  BST3I PIC  X(0002).
      *    -------------------------------
           05  BACCT3L PIC S9(0004) COMP.
           05  BACCT3F PIC  X(0001).
           05  FILLER REDEFINES BACCT3F.
               10  BACCT3A PIC  X(0001).
           05  BACCT3I PIC  X(0010).
      *    -------------------------------
           05  BREF3L PIC S9(0004) COMP.
           05  BREF3F PIC  X(0001).
           05  FILLER REDEFINES BREF3F.
               10  BREF3A PIC  X(0001).
           05  BREF3I PIC  X(0012).
      *    -------------------------------
           05  BBATCH3L PIC S9(0004) COMP.
           05  BBATCH3F PIC  X(0001).
           05  FILLER REDEFINES BBATCH3F.
               10  BBATCH3A PIC  X(0001).
           05  BBATCH3I PIC  X(0006).
      *    -------------------------------
           05  BSUM3L PIC S9(0004) COMP.
           05  BSUM3F PIC  X(0001).
           05  FILLER REDEFINES BSUM3F.
               10  BSUM3A PIC  X(0001).
           05  BSUM3I PIC  X(0006).
      *    -------------------------------
           05  BENTRY3L PIC S9(0004) COMP.
           05  BENTRY3F PIC  X(0001).
           05  FILLER REDEFINES BENTRY3F.
               10  BENTRY3A PIC  X(0001).
           05  BENTRY3I PIC  X(0006).
      *    -------------------------------
           05  BRQST3L PIC S9(0004) COMP.
           05  BRQST3F PIC  X(0001).
           05  FILLER REDEFINES BRQST3F.
               10  BRQST3A PIC  X(0001).
           05  BRQST3I PIC  X(0006).
      *    -------------------------------
           05  BSTMT3L PIC S9(0004) COMP.
           05  BSTMT3F PIC  X(0001).
           05  FILLER REDEFINES BSTMT3F.
               10  BSTMT3A PIC  X(0001).
           05  BSTMT3I PIC  X(0006).
      *    -------------------------------
           05  BSTATS3L PIC S9(0004) COMP.
           05  BSTATS3F PIC  X(0001).
           05  FILLER REDEFINES BSTATS3F.
               10  BSTATS3A PIC  X(0001).
           05  BSTATS3I PIC  X(0001).
      *    -------------------------------
           05  BSEQ4L PIC S9(0004) COMP.
           05  BSEQ4F PIC  X(0001).
           05  FILLER REDEFINES BSEQ4F.
               10  BSEQ4A PIC  X(0001).
           05  BSEQ4I PIC  X(0002).
      *    -------------------------------
           05  BCAR4L PIC S9(0004) COMP.
           05  BCAR4F PIC  X(0001).
           05  FILLER REDEFINES BCAR4F.
               10  BCAR4A PIC  X(0001).
           05  BCAR4I PIC  X(0001).
      *    -------------------------------
           05  BGRP4L PIC S9(0004) COMP.
           05  BGRP4F PIC  X(0001).
           05  FILLER REDEFINES BGRP4F.
               10  BGRP4A PIC  X(0001).
           05  BGRP4I PIC  X(0006).
      *    -------------------------------
           05  BST4L PIC S9(0004) COMP.
           05  BST4F PIC  X(0001).
           05  FILLER REDEFINES BST4F.
               10  BST4A PIC  X(0001).
           05  BST4I PIC  X(0002).
      *    -------------------------------
           05  BACCT4L PIC S9(0004) COMP.
           05  BACCT4F PIC  X(0001).
           05  FILLER REDEFINES BACCT4F.
               10  BACCT4A PIC  X(0001).
           05  BACCT4I PIC  X(0010).
      *    -------------------------------
           05  BREF4L PIC S9(0004) COMP.
           05  BREF4F PIC  X(0001).
           05  FILLER REDEFINES BREF4F.
               10  BREF4A PIC  X(0001).
           05  BREF4I PIC  X(0012).
      *    -------------------------------
           05  BBATCH4L PIC S9(0004) COMP.
           05  BBATCH4F PIC  X(0001).
           05  FILLER REDEFINES BBATCH4F.
               10  BBATCH4A PIC  X(0001).
           05  BBATCH4I PIC  X(0006).
      *    -------------------------------
           05  BSUM4L PIC S9(0004) COMP.
           05  BSUM4F PIC  X(0001).
           05  FILLER REDEFINES BSUM4F.
               10  BSUM4A PIC  X(0001).
           05  BSUM4I PIC  X(0006).
      *    -------------------------------
           05  BENTRY4L PIC S9(0004) COMP.
           05  BENTRY4F PIC  X(0001).
           05  FILLER REDEFINES BENTRY4F.
               10  BENTRY4A PIC  X(0001).
           05  BENTRY4I PIC  X(0006).
      *    -------------------------------
           05  BRQST4L PIC S9(0004) COMP.
           05  BRQST4F PIC  X(0001).
           05  FILLER REDEFINES BRQST4F.
               10  BRQST4A PIC  X(0001).
           05  BRQST4I PIC  X(0006).
      *    -------------------------------
           05  BSTMT4L PIC S9(0004) COMP.
           05  BSTMT4F PIC  X(0001).
           05  FILLER REDEFINES BSTMT4F.
               10  BSTMT4A PIC  X(0001).
           05  BSTMT4I PIC  X(0006).
      *    -------------------------------
           05  BSTATS4L PIC S9(0004) COMP.
           05  BSTATS4F PIC  X(0001).
           05  FILLER REDEFINES BSTATS4F.
               10  BSTATS4A PIC  X(0001).
           05  BSTATS4I PIC  X(0001).
      *    -------------------------------
           05  BSEQ5L PIC S9(0004) COMP.
           05  BSEQ5F PIC  X(0001).
           05  FILLER REDEFINES BSEQ5F.
               10  BSEQ5A PIC  X(0001).
           05  BSEQ5I PIC  X(0002).
      *    -------------------------------
           05  BCAR5L PIC S9(0004) COMP.
           05  BCAR5F PIC  X(0001).
           05  FILLER REDEFINES BCAR5F.
               10  BCAR5A PIC  X(0001).
           05  BCAR5I PIC  X(0001).
      *    -------------------------------
           05  BGRP5L PIC S9(0004) COMP.
           05  BGRP5F PIC  X(0001).
           05  FILLER REDEFINES BGRP5F.
               10  BGRP5A PIC  X(0001).
           05  BGRP5I PIC  X(0006).
      *    -------------------------------
           05  BST5L PIC S9(0004) COMP.
           05  BST5F PIC  X(0001).
           05  FILLER REDEFINES BST5F.
               10  BST5A PIC  X(0001).
           05  BST5I PIC  X(0002).
      *    -------------------------------
           05  BACCT5L PIC S9(0004) COMP.
           05  BACCT5F PIC  X(0001).
           05  FILLER REDEFINES BACCT5F.
               10  BACCT5A PIC  X(0001).
           05  BACCT5I PIC  X(0010).
      *    -------------------------------
           05  BREF5L PIC S9(0004) COMP.
           05  BREF5F PIC  X(0001).
           05  FILLER REDEFINES BREF5F.
               10  BREF5A PIC  X(0001).
           05  BREF5I PIC  X(0012).
      *    -------------------------------
           05  BBATCH5L PIC S9(0004) COMP.
           05  BBATCH5F PIC  X(0001).
           05  FILLER REDEFINES BBATCH5F.
               10  BBATCH5A PIC  X(0001).
           05  BBATCH5I PIC  X(0006).
      *    -------------------------------
           05  BSUM5L PIC S9(0004) COMP.
           05  BSUM5F PIC  X(0001).
           05  FILLER REDEFINES BSUM5F.
               10  BSUM5A PIC  X(0001).
           05  BSUM5I PIC  X(0006).
      *    -------------------------------
           05  BENTRY5L PIC S9(0004) COMP.
           05  BENTRY5F PIC  X(0001).
           05  FILLER REDEFINES BENTRY5F.
               10  BENTRY5A PIC  X(0001).
           05  BENTRY5I PIC  X(0006).
      *    -------------------------------
           05  BRQST5L PIC S9(0004) COMP.
           05  BRQST5F PIC  X(0001).
           05  FILLER REDEFINES BRQST5F.
               10  BRQST5A PIC  X(0001).
           05  BRQST5I PIC  X(0006).
      *    -------------------------------
           05  BSTMT5L PIC S9(0004) COMP.
           05  BSTMT5F PIC  X(0001).
           05  FILLER REDEFINES BSTMT5F.
               10  BSTMT5A PIC  X(0001).
           05  BSTMT5I PIC  X(0006).
      *    -------------------------------
           05  BSTATS5L PIC S9(0004) COMP.
           05  BSTATS5F PIC  X(0001).
           05  FILLER REDEFINES BSTATS5F.
               10  BSTATS5A PIC  X(0001).
           05  BSTATS5I PIC  X(0001).
      *    -------------------------------
           05  BSEQ6L PIC S9(0004) COMP.
           05  BSEQ6F PIC  X(0001).
           05  FILLER REDEFINES BSEQ6F.
               10  BSEQ6A PIC  X(0001).
           05  BSEQ6I PIC  X(0002).
      *    -------------------------------
           05  BCAR6L PIC S9(0004) COMP.
           05  BCAR6F PIC  X(0001).
           05  FILLER REDEFINES BCAR6F.
               10  BCAR6A PIC  X(0001).
           05  BCAR6I PIC  X(0001).
      *    -------------------------------
           05  BGRP6L PIC S9(0004) COMP.
           05  BGRP6F PIC  X(0001).
           05  FILLER REDEFINES BGRP6F.
               10  BGRP6A PIC  X(0001).
           05  BGRP6I PIC  X(0006).
      *    -------------------------------
           05  BST6L PIC S9(0004) COMP.
           05  BST6F PIC  X(0001).
           05  FILLER REDEFINES BST6F.
               10  BST6A PIC  X(0001).
           05  BST6I PIC  X(0002).
      *    -------------------------------
           05  BACCT6L PIC S9(0004) COMP.
           05  BACCT6F PIC  X(0001).
           05  FILLER REDEFINES BACCT6F.
               10  BACCT6A PIC  X(0001).
           05  BACCT6I PIC  X(0010).
      *    -------------------------------
           05  BREF6L PIC S9(0004) COMP.
           05  BREF6F PIC  X(0001).
           05  FILLER REDEFINES BREF6F.
               10  BREF6A PIC  X(0001).
           05  BREF6I PIC  X(0012).
      *    -------------------------------
           05  BBATCH6L PIC S9(0004) COMP.
           05  BBATCH6F PIC  X(0001).
           05  FILLER REDEFINES BBATCH6F.
               10  BBATCH6A PIC  X(0001).
           05  BBATCH6I PIC  X(0006).
      *    -------------------------------
           05  BSUM6L PIC S9(0004) COMP.
           05  BSUM6F PIC  X(0001).
           05  FILLER REDEFINES BSUM6F.
               10  BSUM6A PIC  X(0001).
           05  BSUM6I PIC  X(0006).
      *    -------------------------------
           05  BENTRY6L PIC S9(0004) COMP.
           05  BENTRY6F PIC  X(0001).
           05  FILLER REDEFINES BENTRY6F.
               10  BENTRY6A PIC  X(0001).
           05  BENTRY6I PIC  X(0006).
      *    -------------------------------
           05  BRQST6L PIC S9(0004) COMP.
           05  BRQST6F PIC  X(0001).
           05  FILLER REDEFINES BRQST6F.
               10  BRQST6A PIC  X(0001).
           05  BRQST6I PIC  X(0006).
      *    -------------------------------
           05  BSTMT6L PIC S9(0004) COMP.
           05  BSTMT6F PIC  X(0001).
           05  FILLER REDEFINES BSTMT6F.
               10  BSTMT6A PIC  X(0001).
           05  BSTMT6I PIC  X(0006).
      *    -------------------------------
           05  BSTATS6L PIC S9(0004) COMP.
           05  BSTATS6F PIC  X(0001).
           05  FILLER REDEFINES BSTATS6F.
               10  BSTATS6A PIC  X(0001).
           05  BSTATS6I PIC  X(0001).
      *    -------------------------------
           05  BSEQ7L PIC S9(0004) COMP.
           05  BSEQ7F PIC  X(0001).
           05  FILLER REDEFINES BSEQ7F.
               10  BSEQ7A PIC  X(0001).
           05  BSEQ7I PIC  X(0002).
      *    -------------------------------
           05  BCAR7L PIC S9(0004) COMP.
           05  BCAR7F PIC  X(0001).
           05  FILLER REDEFINES BCAR7F.
               10  BCAR7A PIC  X(0001).
           05  BCAR7I PIC  X(0001).
      *    -------------------------------
           05  BGRP7L PIC S9(0004) COMP.
           05  BGRP7F PIC  X(0001).
           05  FILLER REDEFINES BGRP7F.
               10  BGRP7A PIC  X(0001).
           05  BGRP7I PIC  X(0006).
      *    -------------------------------
           05  BST7L PIC S9(0004) COMP.
           05  BST7F PIC  X(0001).
           05  FILLER REDEFINES BST7F.
               10  BST7A PIC  X(0001).
           05  BST7I PIC  X(0002).
      *    -------------------------------
           05  BACCT7L PIC S9(0004) COMP.
           05  BACCT7F PIC  X(0001).
           05  FILLER REDEFINES BACCT7F.
               10  BACCT7A PIC  X(0001).
           05  BACCT7I PIC  X(0010).
      *    -------------------------------
           05  BREF7L PIC S9(0004) COMP.
           05  BREF7F PIC  X(0001).
           05  FILLER REDEFINES BREF7F.
               10  BREF7A PIC  X(0001).
           05  BREF7I PIC  X(0012).
      *    -------------------------------
           05  BBATCH7L PIC S9(0004) COMP.
           05  BBATCH7F PIC  X(0001).
           05  FILLER REDEFINES BBATCH7F.
               10  BBATCH7A PIC  X(0001).
           05  BBATCH7I PIC  X(0006).
      *    -------------------------------
           05  BSUM7L PIC S9(0004) COMP.
           05  BSUM7F PIC  X(0001).
           05  FILLER REDEFINES BSUM7F.
               10  BSUM7A PIC  X(0001).
           05  BSUM7I PIC  X(0006).
      *    -------------------------------
           05  BENTRY7L PIC S9(0004) COMP.
           05  BENTRY7F PIC  X(0001).
           05  FILLER REDEFINES BENTRY7F.
               10  BENTRY7A PIC  X(0001).
           05  BENTRY7I PIC  X(0006).
      *    -------------------------------
           05  BRQST7L PIC S9(0004) COMP.
           05  BRQST7F PIC  X(0001).
           05  FILLER REDEFINES BRQST7F.
               10  BRQST7A PIC  X(0001).
           05  BRQST7I PIC  X(0006).
      *    -------------------------------
           05  BSTMT7L PIC S9(0004) COMP.
           05  BSTMT7F PIC  X(0001).
           05  FILLER REDEFINES BSTMT7F.
               10  BSTMT7A PIC  X(0001).
           05  BSTMT7I PIC  X(0006).
      *    -------------------------------
           05  BSTATS7L PIC S9(0004) COMP.
           05  BSTATS7F PIC  X(0001).
           05  FILLER REDEFINES BSTATS7F.
               10  BSTATS7A PIC  X(0001).
           05  BSTATS7I PIC  X(0001).
      *    -------------------------------
           05  BSEQ8L PIC S9(0004) COMP.
           05  BSEQ8F PIC  X(0001).
           05  FILLER REDEFINES BSEQ8F.
               10  BSEQ8A PIC  X(0001).
           05  BSEQ8I PIC  X(0002).
      *    -------------------------------
           05  BCAR8L PIC S9(0004) COMP.
           05  BCAR8F PIC  X(0001).
           05  FILLER REDEFINES BCAR8F.
               10  BCAR8A PIC  X(0001).
           05  BCAR8I PIC  X(0001).
      *    -------------------------------
           05  BGRP8L PIC S9(0004) COMP.
           05  BGRP8F PIC  X(0001).
           05  FILLER REDEFINES BGRP8F.
               10  BGRP8A PIC  X(0001).
           05  BGRP8I PIC  X(0006).
      *    -------------------------------
           05  BST8L PIC S9(0004) COMP.
           05  BST8F PIC  X(0001).
           05  FILLER REDEFINES BST8F.
               10  BST8A PIC  X(0001).
           05  BST8I PIC  X(0002).
      *    -------------------------------
           05  BACCT8L PIC S9(0004) COMP.
           05  BACCT8F PIC  X(0001).
           05  FILLER REDEFINES BACCT8F.
               10  BACCT8A PIC  X(0001).
           05  BACCT8I PIC  X(0010).
      *    -------------------------------
           05  BREF8L PIC S9(0004) COMP.
           05  BREF8F PIC  X(0001).
           05  FILLER REDEFINES BREF8F.
               10  BREF8A PIC  X(0001).
           05  BREF8I PIC  X(0012).
      *    -------------------------------
           05  BBATCH8L PIC S9(0004) COMP.
           05  BBATCH8F PIC  X(0001).
           05  FILLER REDEFINES BBATCH8F.
               10  BBATCH8A PIC  X(0001).
           05  BBATCH8I PIC  X(0006).
      *    -------------------------------
           05  BSUM8L PIC S9(0004) COMP.
           05  BSUM8F PIC  X(0001).
           05  FILLER REDEFINES BSUM8F.
               10  BSUM8A PIC  X(0001).
           05  BSUM8I PIC  X(0006).
      *    -------------------------------
           05  BENTRY8L PIC S9(0004) COMP.
           05  BENTRY8F PIC  X(0001).
           05  FILLER REDEFINES BENTRY8F.
               10  BENTRY8A PIC  X(0001).
           05  BENTRY8I PIC  X(0006).
      *    -------------------------------
           05  BRQST8L PIC S9(0004) COMP.
           05  BRQST8F PIC  X(0001).
           05  FILLER REDEFINES BRQST8F.
               10  BRQST8A PIC  X(0001).
           05  BRQST8I PIC  X(0006).
      *    -------------------------------
           05  BSTMT8L PIC S9(0004) COMP.
           05  BSTMT8F PIC  X(0001).
           05  FILLER REDEFINES BSTMT8F.
               10  BSTMT8A PIC  X(0001).
           05  BSTMT8I PIC  X(0006).
      *    -------------------------------
           05  BSTATS8L PIC S9(0004) COMP.
           05  BSTATS8F PIC  X(0001).
           05  FILLER REDEFINES BSTATS8F.
               10  BSTATS8A PIC  X(0001).
           05  BSTATS8I PIC  X(0001).
      *    -------------------------------
           05  BSEQ9L PIC S9(0004) COMP.
           05  BSEQ9F PIC  X(0001).
           05  FILLER REDEFINES BSEQ9F.
               10  BSEQ9A PIC  X(0001).
           05  BSEQ9I PIC  X(0002).
      *    -------------------------------
           05  BCAR9L PIC S9(0004) COMP.
           05  BCAR9F PIC  X(0001).
           05  FILLER REDEFINES BCAR9F.
               10  BCAR9A PIC  X(0001).
           05  BCAR9I PIC  X(0001).
      *    -------------------------------
           05  BGRP9L PIC S9(0004) COMP.
           05  BGRP9F PIC  X(0001).
           05  FILLER REDEFINES BGRP9F.
               10  BGRP9A PIC  X(0001).
           05  BGRP9I PIC  X(0006).
      *    -------------------------------
           05  BST9L PIC S9(0004) COMP.
           05  BST9F PIC  X(0001).
           05  FILLER REDEFINES BST9F.
               10  BST9A PIC  X(0001).
           05  BST9I PIC  X(0002).
      *    -------------------------------
           05  BACCT9L PIC S9(0004) COMP.
           05  BACCT9F PIC  X(0001).
           05  FILLER REDEFINES BACCT9F.
               10  BACCT9A PIC  X(0001).
           05  BACCT9I PIC  X(0010).
      *    -------------------------------
           05  BREF9L PIC S9(0004) COMP.
           05  BREF9F PIC  X(0001).
           05  FILLER REDEFINES BREF9F.
               10  BREF9A PIC  X(0001).
           05  BREF9I PIC  X(0012).
      *    -------------------------------
           05  BBATCH9L PIC S9(0004) COMP.
           05  BBATCH9F PIC  X(0001).
           05  FILLER REDEFINES BBATCH9F.
               10  BBATCH9A PIC  X(0001).
           05  BBATCH9I PIC  X(0006).
      *    -------------------------------
           05  BSUM9L PIC S9(0004) COMP.
           05  BSUM9F PIC  X(0001).
           05  FILLER REDEFINES BSUM9F.
               10  BSUM9A PIC  X(0001).
           05  BSUM9I PIC  X(0006).
      *    -------------------------------
           05  BENTRY9L PIC S9(0004) COMP.
           05  BENTRY9F PIC  X(0001).
           05  FILLER REDEFINES BENTRY9F.
               10  BENTRY9A PIC  X(0001).
           05  BENTRY9I PIC  X(0006).
      *    -------------------------------
           05  BRQST9L PIC S9(0004) COMP.
           05  BRQST9F PIC  X(0001).
           05  FILLER REDEFINES BRQST9F.
               10  BRQST9A PIC  X(0001).
           05  BRQST9I PIC  X(0006).
      *    -------------------------------
           05  BSTMT9L PIC S9(0004) COMP.
           05  BSTMT9F PIC  X(0001).
           05  FILLER REDEFINES BSTMT9F.
               10  BSTMT9A PIC  X(0001).
           05  BSTMT9I PIC  X(0006).
      *    -------------------------------
           05  BSTATS9L PIC S9(0004) COMP.
           05  BSTATS9F PIC  X(0001).
           05  FILLER REDEFINES BSTATS9F.
               10  BSTATS9A PIC  X(0001).
           05  BSTATS9I PIC  X(0001).
      *    -------------------------------
           05  BSEQ0L PIC S9(0004) COMP.
           05  BSEQ0F PIC  X(0001).
           05  FILLER REDEFINES BSEQ0F.
               10  BSEQ0A PIC  X(0001).
           05  BSEQ0I PIC  X(0002).
      *    -------------------------------
           05  BCAR0L PIC S9(0004) COMP.
           05  BCAR0F PIC  X(0001).
           05  FILLER REDEFINES BCAR0F.
               10  BCAR0A PIC  X(0001).
           05  BCAR0I PIC  X(0001).
      *    -------------------------------
           05  BGRP0L PIC S9(0004) COMP.
           05  BGRP0F PIC  X(0001).
           05  FILLER REDEFINES BGRP0F.
               10  BGRP0A PIC  X(0001).
           05  BGRP0I PIC  X(0006).
      *    -------------------------------
           05  BST0L PIC S9(0004) COMP.
           05  BST0F PIC  X(0001).
           05  FILLER REDEFINES BST0F.
               10  BST0A PIC  X(0001).
           05  BST0I PIC  X(0002).
      *    -------------------------------
           05  BACCT0L PIC S9(0004) COMP.
           05  BACCT0F PIC  X(0001).
           05  FILLER REDEFINES BACCT0F.
               10  BACCT0A PIC  X(0001).
           05  BACCT0I PIC  X(0010).
      *    -------------------------------
           05  BREF0L PIC S9(0004) COMP.
           05  BREF0F PIC  X(0001).
           05  FILLER REDEFINES BREF0F.
               10  BREF0A PIC  X(0001).
           05  BREF0I PIC  X(0012).
      *    -------------------------------
           05  BBATCH0L PIC S9(0004) COMP.
           05  BBATCH0F PIC  X(0001).
           05  FILLER REDEFINES BBATCH0F.
               10  BBATCH0A PIC  X(0001).
           05  BBATCH0I PIC  X(0006).
      *    -------------------------------
           05  BSUM0L PIC S9(0004) COMP.
           05  BSUM0F PIC  X(0001).
           05  FILLER REDEFINES BSUM0F.
               10  BSUM0A PIC  X(0001).
           05  BSUM0I PIC  X(0006).
      *    -------------------------------
           05  BENTRY0L PIC S9(0004) COMP.
           05  BENTRY0F PIC  X(0001).
           05  FILLER REDEFINES BENTRY0F.
               10  BENTRY0A PIC  X(0001).
           05  BENTRY0I PIC  X(0006).
      *    -------------------------------
           05  BRQST0L PIC S9(0004) COMP.
           05  BRQST0F PIC  X(0001).
           05  FILLER REDEFINES BRQST0F.
               10  BRQST0A PIC  X(0001).
           05  BRQST0I PIC  X(0006).
      *    -------------------------------
           05  BSTMT0L PIC S9(0004) COMP.
           05  BSTMT0F PIC  X(0001).
           05  FILLER REDEFINES BSTMT0F.
               10  BSTMT0A PIC  X(0001).
           05  BSTMT0I PIC  X(0006).
      *    -------------------------------
           05  BSTATS0L PIC S9(0004) COMP.
           05  BSTATS0F PIC  X(0001).
           05  FILLER REDEFINES BSTATS0F.
               10  BSTATS0A PIC  X(0001).
           05  BSTATS0I PIC  X(0001).
      *    -------------------------------
           05  BSELL PIC S9(0004) COMP.
           05  BSELF PIC  X(0001).
           05  FILLER REDEFINES BSELF.
               10  BSELA PIC  X(0001).
           05  BSELI PIC  X(0002).
      *    -------------------------------
           05  BERMSGL PIC S9(0004) COMP.
           05  BERMSGF PIC  X(0001).
           05  FILLER REDEFINES BERMSGF.
               10  BERMSGA PIC  X(0001).
           05  BERMSGI PIC  X(0079).
      *    -------------------------------
           05  BPFENTRL PIC S9(0004) COMP.
           05  BPFENTRF PIC  X(0001).
           05  FILLER REDEFINES BPFENTRF.
               10  BPFENTRA PIC  X(0001).
           05  BPFENTRI PIC  9(2).
       01  EL852BO REDEFINES EL852BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARHDGO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRPHDGO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGTHDGO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAGENTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAMHDGO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAMEO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BFILLERO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEQ1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCAR1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRP1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BST1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT1O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BREF1O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BBATCH1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSUM1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENTRY1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRQST1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTMT1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTATS1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEQ2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCAR2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRP2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BST2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BREF2O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BBATCH2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSUM2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENTRY2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRQST2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTMT2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTATS2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEQ3O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCAR3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRP3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BST3O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT3O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BREF3O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BBATCH3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSUM3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENTRY3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRQST3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTMT3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTATS3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEQ4O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCAR4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRP4O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BST4O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT4O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BREF4O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BBATCH4O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSUM4O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENTRY4O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRQST4O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTMT4O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTATS4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEQ5O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCAR5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRP5O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BST5O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT5O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BREF5O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BBATCH5O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSUM5O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENTRY5O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRQST5O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTMT5O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTATS5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEQ6O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCAR6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRP6O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BST6O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT6O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BREF6O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BBATCH6O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSUM6O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENTRY6O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRQST6O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTMT6O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTATS6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEQ7O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCAR7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRP7O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BST7O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT7O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BREF7O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BBATCH7O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSUM7O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENTRY7O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRQST7O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTMT7O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTATS7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEQ8O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCAR8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRP8O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BST8O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT8O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BREF8O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BBATCH8O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSUM8O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENTRY8O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRQST8O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTMT8O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTATS8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEQ9O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCAR9O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRP9O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BST9O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT9O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BREF9O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BBATCH9O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSUM9O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENTRY9O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRQST9O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTMT9O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTATS9O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSEQ0O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCAR0O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BGRP0O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BST0O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BACCT0O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BREF0O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BBATCH0O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSUM0O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENTRY0O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BRQST0O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTMT0O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSTATS0O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSELO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BERMSGO PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPFENTRO PIC  99.
      *    -------------------------------
       01  EL852CI REDEFINES EL852BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  CDATEL PIC S9(0004) COMP.
           05  CDATEF PIC  X(0001).
           05  FILLER REDEFINES CDATEF.
               10  CDATEA PIC  X(0001).
           05  CDATEI PIC  X(0008).
      *    -------------------------------
           05  CTIMEL PIC S9(0004) COMP.
           05  CTIMEF PIC  X(0001).
           05  FILLER REDEFINES CTIMEF.
               10  CTIMEA PIC  X(0001).
           05  CTIMEI PIC  X(0005).
      *    -------------------------------
           05  CCARHDGL PIC S9(0004) COMP.
           05  CCARHDGF PIC  X(0001).
           05  FILLER REDEFINES CCARHDGF.
               10  CCARHDGA PIC  X(0001).
           05  CCARHDGI PIC  X(0004).
      *    -------------------------------
           05  CCARRL PIC S9(0004) COMP.
           05  CCARRF PIC  X(0001).
           05  FILLER REDEFINES CCARRF.
               10  CCARRA PIC  X(0001).
           05  CCARRI PIC  X(0001).
      *    -------------------------------
           05  CGRPHDGL PIC S9(0004) COMP.
           05  CGRPHDGF PIC  X(0001).
           05  FILLER REDEFINES CGRPHDGF.
               10  CGRPHDGA PIC  X(0001).
           05  CGRPHDGI PIC  X(0006).
      *    -------------------------------
           05  CGROUPL PIC S9(0004) COMP.
           05  CGROUPF PIC  X(0001).
           05  FILLER REDEFINES CGROUPF.
               10  CGROUPA PIC  X(0001).
           05  CGROUPI PIC  X(0006).
      *    -------------------------------
           05  CSTHDGL PIC S9(0004) COMP.
           05  CSTHDGF PIC  X(0001).
           05  FILLER REDEFINES CSTHDGF.
               10  CSTHDGA PIC  X(0001).
           05  CSTHDGI PIC  X(0003).
      *    -------------------------------
           05  CSTL PIC S9(0004) COMP.
           05  CSTF PIC  X(0001).
           05  FILLER REDEFINES CSTF.
               10  CSTA PIC  X(0001).
           05  CSTI PIC  X(0002).
      *    -------------------------------
           05  CACTHDGL PIC S9(0004) COMP.
           05  CACTHDGF PIC  X(0001).
           05  FILLER REDEFINES CACTHDGF.
               10  CACTHDGA PIC  X(0001).
           05  CACTHDGI PIC  X(0008).
      *    -------------------------------
           05  CACCTL PIC S9(0004) COMP.
           05  CACCTF PIC  X(0001).
           05  FILLER REDEFINES CACCTF.
               10  CACCTA PIC  X(0001).
           05  CACCTI PIC  X(0010).
      *    -------------------------------
           05  CNAMHDGL PIC S9(0004) COMP.
           05  CNAMHDGF PIC  X(0001).
           05  FILLER REDEFINES CNAMHDGF.
               10  CNAMHDGA PIC  X(0001).
           05  CNAMHDGI PIC  X(0005).
      *    -------------------------------
           05  CNAMEL PIC S9(0004) COMP.
           05  CNAMEF PIC  X(0001).
           05  FILLER REDEFINES CNAMEF.
               10  CNAMEA PIC  X(0001).
           05  CNAMEI PIC  X(0030).
      *    -------------------------------
           05  CSEQ1L PIC S9(0004) COMP.
           05  CSEQ1F PIC  X(0001).
           05  FILLER REDEFINES CSEQ1F.
               10  CSEQ1A PIC  X(0001).
           05  CSEQ1I PIC  X(0002).
      *    -------------------------------
           05  CCAR1L PIC S9(0004) COMP.
           05  CCAR1F PIC  X(0001).
           05  FILLER REDEFINES CCAR1F.
               10  CCAR1A PIC  X(0001).
           05  CCAR1I PIC  X(0001).
      *    -------------------------------
           05  CGRP1L PIC S9(0004) COMP.
           05  CGRP1F PIC  X(0001).
           05  FILLER REDEFINES CGRP1F.
               10  CGRP1A PIC  X(0001).
           05  CGRP1I PIC  X(0006).
      *    -------------------------------
           05  CST1L PIC S9(0004) COMP.
           05  CST1F PIC  X(0001).
           05  FILLER REDEFINES CST1F.
               10  CST1A PIC  X(0001).
           05  CST1I PIC  X(0002).
      *    -------------------------------
           05  CACCT1L PIC S9(0004) COMP.
           05  CACCT1F PIC  X(0001).
           05  FILLER REDEFINES CACCT1F.
               10  CACCT1A PIC  X(0001).
           05  CACCT1I PIC  X(0010).
      *    -------------------------------
           05  CREF1L PIC S9(0004) COMP.
           05  CREF1F PIC  X(0001).
           05  FILLER REDEFINES CREF1F.
               10  CREF1A PIC  X(0001).
           05  CREF1I PIC  X(0012).
      *    -------------------------------
           05  CBATCH1L PIC S9(0004) COMP.
           05  CBATCH1F PIC  X(0001).
           05  FILLER REDEFINES CBATCH1F.
               10  CBATCH1A PIC  X(0001).
           05  CBATCH1I PIC  X(0006).
      *    -------------------------------
           05  CSUM1L PIC S9(0004) COMP.
           05  CSUM1F PIC  X(0001).
           05  FILLER REDEFINES CSUM1F.
               10  CSUM1A PIC  X(0001).
           05  CSUM1I PIC  X(0006).
      *    -------------------------------
           05  CENTRY1L PIC S9(0004) COMP.
           05  CENTRY1F PIC  X(0001).
           05  FILLER REDEFINES CENTRY1F.
               10  CENTRY1A PIC  X(0001).
           05  CENTRY1I PIC  X(0006).
      *    -------------------------------
           05  CRQST1L PIC S9(0004) COMP.
           05  CRQST1F PIC  X(0001).
           05  FILLER REDEFINES CRQST1F.
               10  CRQST1A PIC  X(0001).
           05  CRQST1I PIC  X(0006).
      *    -------------------------------
           05  CSTMT1L PIC S9(0004) COMP.
           05  CSTMT1F PIC  X(0001).
           05  FILLER REDEFINES CSTMT1F.
               10  CSTMT1A PIC  X(0001).
           05  CSTMT1I PIC  X(0006).
      *    -------------------------------
           05  CSTATS1L PIC S9(0004) COMP.
           05  CSTATS1F PIC  X(0001).
           05  FILLER REDEFINES CSTATS1F.
               10  CSTATS1A PIC  X(0001).
           05  CSTATS1I PIC  X(0001).
      *    -------------------------------
           05  CSEQ2L PIC S9(0004) COMP.
           05  CSEQ2F PIC  X(0001).
           05  FILLER REDEFINES CSEQ2F.
               10  CSEQ2A PIC  X(0001).
           05  CSEQ2I PIC  X(0002).
      *    -------------------------------
           05  CCAR2L PIC S9(0004) COMP.
           05  CCAR2F PIC  X(0001).
           05  FILLER REDEFINES CCAR2F.
               10  CCAR2A PIC  X(0001).
           05  CCAR2I PIC  X(0001).
      *    -------------------------------
           05  CGRP2L PIC S9(0004) COMP.
           05  CGRP2F PIC  X(0001).
           05  FILLER REDEFINES CGRP2F.
               10  CGRP2A PIC  X(0001).
           05  CGRP2I PIC  X(0006).
      *    -------------------------------
           05  CST2L PIC S9(0004) COMP.
           05  CST2F PIC  X(0001).
           05  FILLER REDEFINES CST2F.
               10  CST2A PIC  X(0001).
           05  CST2I PIC  X(0002).
      *    -------------------------------
           05  CACCT2L PIC S9(0004) COMP.
           05  CACCT2F PIC  X(0001).
           05  FILLER REDEFINES CACCT2F.
               10  CACCT2A PIC  X(0001).
           05  CACCT2I PIC  X(0010).
      *    -------------------------------
           05  CREF2L PIC S9(0004) COMP.
           05  CREF2F PIC  X(0001).
           05  FILLER REDEFINES CREF2F.
               10  CREF2A PIC  X(0001).
           05  CREF2I PIC  X(0012).
      *    -------------------------------
           05  CBATCH2L PIC S9(0004) COMP.
           05  CBATCH2F PIC  X(0001).
           05  FILLER REDEFINES CBATCH2F.
               10  CBATCH2A PIC  X(0001).
           05  CBATCH2I PIC  X(0006).
      *    -------------------------------
           05  CSUM2L PIC S9(0004) COMP.
           05  CSUM2F PIC  X(0001).
           05  FILLER REDEFINES CSUM2F.
               10  CSUM2A PIC  X(0001).
           05  CSUM2I PIC  X(0006).
      *    -------------------------------
           05  CENTRY2L PIC S9(0004) COMP.
           05  CENTRY2F PIC  X(0001).
           05  FILLER REDEFINES CENTRY2F.
               10  CENTRY2A PIC  X(0001).
           05  CENTRY2I PIC  X(0006).
      *    -------------------------------
           05  CRQST2L PIC S9(0004) COMP.
           05  CRQST2F PIC  X(0001).
           05  FILLER REDEFINES CRQST2F.
               10  CRQST2A PIC  X(0001).
           05  CRQST2I PIC  X(0006).
      *    -------------------------------
           05  CSTMT2L PIC S9(0004) COMP.
           05  CSTMT2F PIC  X(0001).
           05  FILLER REDEFINES CSTMT2F.
               10  CSTMT2A PIC  X(0001).
           05  CSTMT2I PIC  X(0006).
      *    -------------------------------
           05  CSTATS2L PIC S9(0004) COMP.
           05  CSTATS2F PIC  X(0001).
           05  FILLER REDEFINES CSTATS2F.
               10  CSTATS2A PIC  X(0001).
           05  CSTATS2I PIC  X(0001).
      *    -------------------------------
           05  CSEQ3L PIC S9(0004) COMP.
           05  CSEQ3F PIC  X(0001).
           05  FILLER REDEFINES CSEQ3F.
               10  CSEQ3A PIC  X(0001).
           05  CSEQ3I PIC  X(0002).
      *    -------------------------------
           05  CCAR3L PIC S9(0004) COMP.
           05  CCAR3F PIC  X(0001).
           05  FILLER REDEFINES CCAR3F.
               10  CCAR3A PIC  X(0001).
           05  CCAR3I PIC  X(0001).
      *    -------------------------------
           05  CGRP3L PIC S9(0004) COMP.
           05  CGRP3F PIC  X(0001).
           05  FILLER REDEFINES CGRP3F.
               10  CGRP3A PIC  X(0001).
           05  CGRP3I PIC  X(0006).
      *    -------------------------------
           05  CST3L PIC S9(0004) COMP.
           05  CST3F PIC  X(0001).
           05  FILLER REDEFINES CST3F.
               10  CST3A PIC  X(0001).
           05  CST3I PIC  X(0002).
      *    -------------------------------
           05  CACCT3L PIC S9(0004) COMP.
           05  CACCT3F PIC  X(0001).
           05  FILLER REDEFINES CACCT3F.
               10  CACCT3A PIC  X(0001).
           05  CACCT3I PIC  X(0010).
      *    -------------------------------
           05  CREF3L PIC S9(0004) COMP.
           05  CREF3F PIC  X(0001).
           05  FILLER REDEFINES CREF3F.
               10  CREF3A PIC  X(0001).
           05  CREF3I PIC  X(0012).
      *    -------------------------------
           05  CBATCH3L PIC S9(0004) COMP.
           05  CBATCH3F PIC  X(0001).
           05  FILLER REDEFINES CBATCH3F.
               10  CBATCH3A PIC  X(0001).
           05  CBATCH3I PIC  X(0006).
      *    -------------------------------
           05  CSUM3L PIC S9(0004) COMP.
           05  CSUM3F PIC  X(0001).
           05  FILLER REDEFINES CSUM3F.
               10  CSUM3A PIC  X(0001).
           05  CSUM3I PIC  X(0006).
      *    -------------------------------
           05  CENTRY3L PIC S9(0004) COMP.
           05  CENTRY3F PIC  X(0001).
           05  FILLER REDEFINES CENTRY3F.
               10  CENTRY3A PIC  X(0001).
           05  CENTRY3I PIC  X(0006).
      *    -------------------------------
           05  CRQST3L PIC S9(0004) COMP.
           05  CRQST3F PIC  X(0001).
           05  FILLER REDEFINES CRQST3F.
               10  CRQST3A PIC  X(0001).
           05  CRQST3I PIC  X(0006).
      *    -------------------------------
           05  CSTMT3L PIC S9(0004) COMP.
           05  CSTMT3F PIC  X(0001).
           05  FILLER REDEFINES CSTMT3F.
               10  CSTMT3A PIC  X(0001).
           05  CSTMT3I PIC  X(0006).
      *    -------------------------------
           05  CSTATS3L PIC S9(0004) COMP.
           05  CSTATS3F PIC  X(0001).
           05  FILLER REDEFINES CSTATS3F.
               10  CSTATS3A PIC  X(0001).
           05  CSTATS3I PIC  X(0001).
      *    -------------------------------
           05  CSEQ4L PIC S9(0004) COMP.
           05  CSEQ4F PIC  X(0001).
           05  FILLER REDEFINES CSEQ4F.
               10  CSEQ4A PIC  X(0001).
           05  CSEQ4I PIC  X(0002).
      *    -------------------------------
           05  CCAR4L PIC S9(0004) COMP.
           05  CCAR4F PIC  X(0001).
           05  FILLER REDEFINES CCAR4F.
               10  CCAR4A PIC  X(0001).
           05  CCAR4I PIC  X(0001).
      *    -------------------------------
           05  CGRP4L PIC S9(0004) COMP.
           05  CGRP4F PIC  X(0001).
           05  FILLER REDEFINES CGRP4F.
               10  CGRP4A PIC  X(0001).
           05  CGRP4I PIC  X(0006).
      *    -------------------------------
           05  CST4L PIC S9(0004) COMP.
           05  CST4F PIC  X(0001).
           05  FILLER REDEFINES CST4F.
               10  CST4A PIC  X(0001).
           05  CST4I PIC  X(0002).
      *    -------------------------------
           05  CACCT4L PIC S9(0004) COMP.
           05  CACCT4F PIC  X(0001).
           05  FILLER REDEFINES CACCT4F.
               10  CACCT4A PIC  X(0001).
           05  CACCT4I PIC  X(0010).
      *    -------------------------------
           05  CREF4L PIC S9(0004) COMP.
           05  CREF4F PIC  X(0001).
           05  FILLER REDEFINES CREF4F.
               10  CREF4A PIC  X(0001).
           05  CREF4I PIC  X(0012).
      *    -------------------------------
           05  CBATCH4L PIC S9(0004) COMP.
           05  CBATCH4F PIC  X(0001).
           05  FILLER REDEFINES CBATCH4F.
               10  CBATCH4A PIC  X(0001).
           05  CBATCH4I PIC  X(0006).
      *    -------------------------------
           05  CSUM4L PIC S9(0004) COMP.
           05  CSUM4F PIC  X(0001).
           05  FILLER REDEFINES CSUM4F.
               10  CSUM4A PIC  X(0001).
           05  CSUM4I PIC  X(0006).
      *    -------------------------------
           05  CENTRY4L PIC S9(0004) COMP.
           05  CENTRY4F PIC  X(0001).
           05  FILLER REDEFINES CENTRY4F.
               10  CENTRY4A PIC  X(0001).
           05  CENTRY4I PIC  X(0006).
      *    -------------------------------
           05  CRQST4L PIC S9(0004) COMP.
           05  CRQST4F PIC  X(0001).
           05  FILLER REDEFINES CRQST4F.
               10  CRQST4A PIC  X(0001).
           05  CRQST4I PIC  X(0006).
      *    -------------------------------
           05  CSTMT4L PIC S9(0004) COMP.
           05  CSTMT4F PIC  X(0001).
           05  FILLER REDEFINES CSTMT4F.
               10  CSTMT4A PIC  X(0001).
           05  CSTMT4I PIC  X(0006).
      *    -------------------------------
           05  CSTATS4L PIC S9(0004) COMP.
           05  CSTATS4F PIC  X(0001).
           05  FILLER REDEFINES CSTATS4F.
               10  CSTATS4A PIC  X(0001).
           05  CSTATS4I PIC  X(0001).
      *    -------------------------------
           05  CSEQ5L PIC S9(0004) COMP.
           05  CSEQ5F PIC  X(0001).
           05  FILLER REDEFINES CSEQ5F.
               10  CSEQ5A PIC  X(0001).
           05  CSEQ5I PIC  X(0002).
      *    -------------------------------
           05  CCAR5L PIC S9(0004) COMP.
           05  CCAR5F PIC  X(0001).
           05  FILLER REDEFINES CCAR5F.
               10  CCAR5A PIC  X(0001).
           05  CCAR5I PIC  X(0001).
      *    -------------------------------
           05  CGRP5L PIC S9(0004) COMP.
           05  CGRP5F PIC  X(0001).
           05  FILLER REDEFINES CGRP5F.
               10  CGRP5A PIC  X(0001).
           05  CGRP5I PIC  X(0006).
      *    -------------------------------
           05  CST5L PIC S9(0004) COMP.
           05  CST5F PIC  X(0001).
           05  FILLER REDEFINES CST5F.
               10  CST5A PIC  X(0001).
           05  CST5I PIC  X(0002).
      *    -------------------------------
           05  CACCT5L PIC S9(0004) COMP.
           05  CACCT5F PIC  X(0001).
           05  FILLER REDEFINES CACCT5F.
               10  CACCT5A PIC  X(0001).
           05  CACCT5I PIC  X(0010).
      *    -------------------------------
           05  CREF5L PIC S9(0004) COMP.
           05  CREF5F PIC  X(0001).
           05  FILLER REDEFINES CREF5F.
               10  CREF5A PIC  X(0001).
           05  CREF5I PIC  X(0012).
      *    -------------------------------
           05  CBATCH5L PIC S9(0004) COMP.
           05  CBATCH5F PIC  X(0001).
           05  FILLER REDEFINES CBATCH5F.
               10  CBATCH5A PIC  X(0001).
           05  CBATCH5I PIC  X(0006).
      *    -------------------------------
           05  CSUM5L PIC S9(0004) COMP.
           05  CSUM5F PIC  X(0001).
           05  FILLER REDEFINES CSUM5F.
               10  CSUM5A PIC  X(0001).
           05  CSUM5I PIC  X(0006).
      *    -------------------------------
           05  CENTRY5L PIC S9(0004) COMP.
           05  CENTRY5F PIC  X(0001).
           05  FILLER REDEFINES CENTRY5F.
               10  CENTRY5A PIC  X(0001).
           05  CENTRY5I PIC  X(0006).
      *    -------------------------------
           05  CRQST5L PIC S9(0004) COMP.
           05  CRQST5F PIC  X(0001).
           05  FILLER REDEFINES CRQST5F.
               10  CRQST5A PIC  X(0001).
           05  CRQST5I PIC  X(0006).
      *    -------------------------------
           05  CSTMT5L PIC S9(0004) COMP.
           05  CSTMT5F PIC  X(0001).
           05  FILLER REDEFINES CSTMT5F.
               10  CSTMT5A PIC  X(0001).
           05  CSTMT5I PIC  X(0006).
      *    -------------------------------
           05  CSTATS5L PIC S9(0004) COMP.
           05  CSTATS5F PIC  X(0001).
           05  FILLER REDEFINES CSTATS5F.
               10  CSTATS5A PIC  X(0001).
           05  CSTATS5I PIC  X(0001).
      *    -------------------------------
           05  CSEQ6L PIC S9(0004) COMP.
           05  CSEQ6F PIC  X(0001).
           05  FILLER REDEFINES CSEQ6F.
               10  CSEQ6A PIC  X(0001).
           05  CSEQ6I PIC  X(0002).
      *    -------------------------------
           05  CCAR6L PIC S9(0004) COMP.
           05  CCAR6F PIC  X(0001).
           05  FILLER REDEFINES CCAR6F.
               10  CCAR6A PIC  X(0001).
           05  CCAR6I PIC  X(0001).
      *    -------------------------------
           05  CGRP6L PIC S9(0004) COMP.
           05  CGRP6F PIC  X(0001).
           05  FILLER REDEFINES CGRP6F.
               10  CGRP6A PIC  X(0001).
           05  CGRP6I PIC  X(0006).
      *    -------------------------------
           05  CST6L PIC S9(0004) COMP.
           05  CST6F PIC  X(0001).
           05  FILLER REDEFINES CST6F.
               10  CST6A PIC  X(0001).
           05  CST6I PIC  X(0002).
      *    -------------------------------
           05  CACCT6L PIC S9(0004) COMP.
           05  CACCT6F PIC  X(0001).
           05  FILLER REDEFINES CACCT6F.
               10  CACCT6A PIC  X(0001).
           05  CACCT6I PIC  X(0010).
      *    -------------------------------
           05  CREF6L PIC S9(0004) COMP.
           05  CREF6F PIC  X(0001).
           05  FILLER REDEFINES CREF6F.
               10  CREF6A PIC  X(0001).
           05  CREF6I PIC  X(0012).
      *    -------------------------------
           05  CBATCH6L PIC S9(0004) COMP.
           05  CBATCH6F PIC  X(0001).
           05  FILLER REDEFINES CBATCH6F.
               10  CBATCH6A PIC  X(0001).
           05  CBATCH6I PIC  X(0006).
      *    -------------------------------
           05  CSUM6L PIC S9(0004) COMP.
           05  CSUM6F PIC  X(0001).
           05  FILLER REDEFINES CSUM6F.
               10  CSUM6A PIC  X(0001).
           05  CSUM6I PIC  X(0006).
      *    -------------------------------
           05  CENTRY6L PIC S9(0004) COMP.
           05  CENTRY6F PIC  X(0001).
           05  FILLER REDEFINES CENTRY6F.
               10  CENTRY6A PIC  X(0001).
           05  CENTRY6I PIC  X(0006).
      *    -------------------------------
           05  CRQST6L PIC S9(0004) COMP.
           05  CRQST6F PIC  X(0001).
           05  FILLER REDEFINES CRQST6F.
               10  CRQST6A PIC  X(0001).
           05  CRQST6I PIC  X(0006).
      *    -------------------------------
           05  CSTMT6L PIC S9(0004) COMP.
           05  CSTMT6F PIC  X(0001).
           05  FILLER REDEFINES CSTMT6F.
               10  CSTMT6A PIC  X(0001).
           05  CSTMT6I PIC  X(0006).
      *    -------------------------------
           05  CSTATS6L PIC S9(0004) COMP.
           05  CSTATS6F PIC  X(0001).
           05  FILLER REDEFINES CSTATS6F.
               10  CSTATS6A PIC  X(0001).
           05  CSTATS6I PIC  X(0001).
      *    -------------------------------
           05  CSEQ7L PIC S9(0004) COMP.
           05  CSEQ7F PIC  X(0001).
           05  FILLER REDEFINES CSEQ7F.
               10  CSEQ7A PIC  X(0001).
           05  CSEQ7I PIC  X(0002).
      *    -------------------------------
           05  CCAR7L PIC S9(0004) COMP.
           05  CCAR7F PIC  X(0001).
           05  FILLER REDEFINES CCAR7F.
               10  CCAR7A PIC  X(0001).
           05  CCAR7I PIC  X(0001).
      *    -------------------------------
           05  CGRP7L PIC S9(0004) COMP.
           05  CGRP7F PIC  X(0001).
           05  FILLER REDEFINES CGRP7F.
               10  CGRP7A PIC  X(0001).
           05  CGRP7I PIC  X(0006).
      *    -------------------------------
           05  CST7L PIC S9(0004) COMP.
           05  CST7F PIC  X(0001).
           05  FILLER REDEFINES CST7F.
               10  CST7A PIC  X(0001).
           05  CST7I PIC  X(0002).
      *    -------------------------------
           05  CACCT7L PIC S9(0004) COMP.
           05  CACCT7F PIC  X(0001).
           05  FILLER REDEFINES CACCT7F.
               10  CACCT7A PIC  X(0001).
           05  CACCT7I PIC  X(0010).
      *    -------------------------------
           05  CREF7L PIC S9(0004) COMP.
           05  CREF7F PIC  X(0001).
           05  FILLER REDEFINES CREF7F.
               10  CREF7A PIC  X(0001).
           05  CREF7I PIC  X(0012).
      *    -------------------------------
           05  CBATCH7L PIC S9(0004) COMP.
           05  CBATCH7F PIC  X(0001).
           05  FILLER REDEFINES CBATCH7F.
               10  CBATCH7A PIC  X(0001).
           05  CBATCH7I PIC  X(0006).
      *    -------------------------------
           05  CSUM7L PIC S9(0004) COMP.
           05  CSUM7F PIC  X(0001).
           05  FILLER REDEFINES CSUM7F.
               10  CSUM7A PIC  X(0001).
           05  CSUM7I PIC  X(0006).
      *    -------------------------------
           05  CENTRY7L PIC S9(0004) COMP.
           05  CENTRY7F PIC  X(0001).
           05  FILLER REDEFINES CENTRY7F.
               10  CENTRY7A PIC  X(0001).
           05  CENTRY7I PIC  X(0006).
      *    -------------------------------
           05  CRQST7L PIC S9(0004) COMP.
           05  CRQST7F PIC  X(0001).
           05  FILLER REDEFINES CRQST7F.
               10  CRQST7A PIC  X(0001).
           05  CRQST7I PIC  X(0006).
      *    -------------------------------
           05  CSTMT7L PIC S9(0004) COMP.
           05  CSTMT7F PIC  X(0001).
           05  FILLER REDEFINES CSTMT7F.
               10  CSTMT7A PIC  X(0001).
           05  CSTMT7I PIC  X(0006).
      *    -------------------------------
           05  CSTATS7L PIC S9(0004) COMP.
           05  CSTATS7F PIC  X(0001).
           05  FILLER REDEFINES CSTATS7F.
               10  CSTATS7A PIC  X(0001).
           05  CSTATS7I PIC  X(0001).
      *    -------------------------------
           05  CSEQ8L PIC S9(0004) COMP.
           05  CSEQ8F PIC  X(0001).
           05  FILLER REDEFINES CSEQ8F.
               10  CSEQ8A PIC  X(0001).
           05  CSEQ8I PIC  X(0002).
      *    -------------------------------
           05  CCAR8L PIC S9(0004) COMP.
           05  CCAR8F PIC  X(0001).
           05  FILLER REDEFINES CCAR8F.
               10  CCAR8A PIC  X(0001).
           05  CCAR8I PIC  X(0001).
      *    -------------------------------
           05  CGRP8L PIC S9(0004) COMP.
           05  CGRP8F PIC  X(0001).
           05  FILLER REDEFINES CGRP8F.
               10  CGRP8A PIC  X(0001).
           05  CGRP8I PIC  X(0006).
      *    -------------------------------
           05  CST8L PIC S9(0004) COMP.
           05  CST8F PIC  X(0001).
           05  FILLER REDEFINES CST8F.
               10  CST8A PIC  X(0001).
           05  CST8I PIC  X(0002).
      *    -------------------------------
           05  CACCT8L PIC S9(0004) COMP.
           05  CACCT8F PIC  X(0001).
           05  FILLER REDEFINES CACCT8F.
               10  CACCT8A PIC  X(0001).
           05  CACCT8I PIC  X(0010).
      *    -------------------------------
           05  CREF8L PIC S9(0004) COMP.
           05  CREF8F PIC  X(0001).
           05  FILLER REDEFINES CREF8F.
               10  CREF8A PIC  X(0001).
           05  CREF8I PIC  X(0012).
      *    -------------------------------
           05  CBATCH8L PIC S9(0004) COMP.
           05  CBATCH8F PIC  X(0001).
           05  FILLER REDEFINES CBATCH8F.
               10  CBATCH8A PIC  X(0001).
           05  CBATCH8I PIC  X(0006).
      *    -------------------------------
           05  CSUM8L PIC S9(0004) COMP.
           05  CSUM8F PIC  X(0001).
           05  FILLER REDEFINES CSUM8F.
               10  CSUM8A PIC  X(0001).
           05  CSUM8I PIC  X(0006).
      *    -------------------------------
           05  CENTRY8L PIC S9(0004) COMP.
           05  CENTRY8F PIC  X(0001).
           05  FILLER REDEFINES CENTRY8F.
               10  CENTRY8A PIC  X(0001).
           05  CENTRY8I PIC  X(0006).
      *    -------------------------------
           05  CRQST8L PIC S9(0004) COMP.
           05  CRQST8F PIC  X(0001).
           05  FILLER REDEFINES CRQST8F.
               10  CRQST8A PIC  X(0001).
           05  CRQST8I PIC  X(0006).
      *    -------------------------------
           05  CSTMT8L PIC S9(0004) COMP.
           05  CSTMT8F PIC  X(0001).
           05  FILLER REDEFINES CSTMT8F.
               10  CSTMT8A PIC  X(0001).
           05  CSTMT8I PIC  X(0006).
      *    -------------------------------
           05  CSTATS8L PIC S9(0004) COMP.
           05  CSTATS8F PIC  X(0001).
           05  FILLER REDEFINES CSTATS8F.
               10  CSTATS8A PIC  X(0001).
           05  CSTATS8I PIC  X(0001).
      *    -------------------------------
           05  CSEQ9L PIC S9(0004) COMP.
           05  CSEQ9F PIC  X(0001).
           05  FILLER REDEFINES CSEQ9F.
               10  CSEQ9A PIC  X(0001).
           05  CSEQ9I PIC  X(0002).
      *    -------------------------------
           05  CCAR9L PIC S9(0004) COMP.
           05  CCAR9F PIC  X(0001).
           05  FILLER REDEFINES CCAR9F.
               10  CCAR9A PIC  X(0001).
           05  CCAR9I PIC  X(0001).
      *    -------------------------------
           05  CGRP9L PIC S9(0004) COMP.
           05  CGRP9F PIC  X(0001).
           05  FILLER REDEFINES CGRP9F.
               10  CGRP9A PIC  X(0001).
           05  CGRP9I PIC  X(0006).
      *    -------------------------------
           05  CST9L PIC S9(0004) COMP.
           05  CST9F PIC  X(0001).
           05  FILLER REDEFINES CST9F.
               10  CST9A PIC  X(0001).
           05  CST9I PIC  X(0002).
      *    -------------------------------
           05  CACCT9L PIC S9(0004) COMP.
           05  CACCT9F PIC  X(0001).
           05  FILLER REDEFINES CACCT9F.
               10  CACCT9A PIC  X(0001).
           05  CACCT9I PIC  X(0010).
      *    -------------------------------
           05  CREF9L PIC S9(0004) COMP.
           05  CREF9F PIC  X(0001).
           05  FILLER REDEFINES CREF9F.
               10  CREF9A PIC  X(0001).
           05  CREF9I PIC  X(0012).
      *    -------------------------------
           05  CBATCH9L PIC S9(0004) COMP.
           05  CBATCH9F PIC  X(0001).
           05  FILLER REDEFINES CBATCH9F.
               10  CBATCH9A PIC  X(0001).
           05  CBATCH9I PIC  X(0006).
      *    -------------------------------
           05  CSUM9L PIC S9(0004) COMP.
           05  CSUM9F PIC  X(0001).
           05  FILLER REDEFINES CSUM9F.
               10  CSUM9A PIC  X(0001).
           05  CSUM9I PIC  X(0006).
      *    -------------------------------
           05  CENTRY9L PIC S9(0004) COMP.
           05  CENTRY9F PIC  X(0001).
           05  FILLER REDEFINES CENTRY9F.
               10  CENTRY9A PIC  X(0001).
           05  CENTRY9I PIC  X(0006).
      *    -------------------------------
           05  CRQST9L PIC S9(0004) COMP.
           05  CRQST9F PIC  X(0001).
           05  FILLER REDEFINES CRQST9F.
               10  CRQST9A PIC  X(0001).
           05  CRQST9I PIC  X(0006).
      *    -------------------------------
           05  CSTMT9L PIC S9(0004) COMP.
           05  CSTMT9F PIC  X(0001).
           05  FILLER REDEFINES CSTMT9F.
               10  CSTMT9A PIC  X(0001).
           05  CSTMT9I PIC  X(0006).
      *    -------------------------------
           05  CSTATS9L PIC S9(0004) COMP.
           05  CSTATS9F PIC  X(0001).
           05  FILLER REDEFINES CSTATS9F.
               10  CSTATS9A PIC  X(0001).
           05  CSTATS9I PIC  X(0001).
      *    -------------------------------
           05  CSEQ0L PIC S9(0004) COMP.
           05  CSEQ0F PIC  X(0001).
           05  FILLER REDEFINES CSEQ0F.
               10  CSEQ0A PIC  X(0001).
           05  CSEQ0I PIC  X(0002).
      *    -------------------------------
           05  CCAR0L PIC S9(0004) COMP.
           05  CCAR0F PIC  X(0001).
           05  FILLER REDEFINES CCAR0F.
               10  CCAR0A PIC  X(0001).
           05  CCAR0I PIC  X(0001).
      *    -------------------------------
           05  CGRP0L PIC S9(0004) COMP.
           05  CGRP0F PIC  X(0001).
           05  FILLER REDEFINES CGRP0F.
               10  CGRP0A PIC  X(0001).
           05  CGRP0I PIC  X(0006).
      *    -------------------------------
           05  CST0L PIC S9(0004) COMP.
           05  CST0F PIC  X(0001).
           05  FILLER REDEFINES CST0F.
               10  CST0A PIC  X(0001).
           05  CST0I PIC  X(0002).
      *    -------------------------------
           05  CACCT0L PIC S9(0004) COMP.
           05  CACCT0F PIC  X(0001).
           05  FILLER REDEFINES CACCT0F.
               10  CACCT0A PIC  X(0001).
           05  CACCT0I PIC  X(0010).
      *    -------------------------------
           05  CREF0L PIC S9(0004) COMP.
           05  CREF0F PIC  X(0001).
           05  FILLER REDEFINES CREF0F.
               10  CREF0A PIC  X(0001).
           05  CREF0I PIC  X(0012).
      *    -------------------------------
           05  CBATCH0L PIC S9(0004) COMP.
           05  CBATCH0F PIC  X(0001).
           05  FILLER REDEFINES CBATCH0F.
               10  CBATCH0A PIC  X(0001).
           05  CBATCH0I PIC  X(0006).
      *    -------------------------------
           05  CSUM0L PIC S9(0004) COMP.
           05  CSUM0F PIC  X(0001).
           05  FILLER REDEFINES CSUM0F.
               10  CSUM0A PIC  X(0001).
           05  CSUM0I PIC  X(0006).
      *    -------------------------------
           05  CENTRY0L PIC S9(0004) COMP.
           05  CENTRY0F PIC  X(0001).
           05  FILLER REDEFINES CENTRY0F.
               10  CENTRY0A PIC  X(0001).
           05  CENTRY0I PIC  X(0006).
      *    -------------------------------
           05  CRQST0L PIC S9(0004) COMP.
           05  CRQST0F PIC  X(0001).
           05  FILLER REDEFINES CRQST0F.
               10  CRQST0A PIC  X(0001).
           05  CRQST0I PIC  X(0006).
      *    -------------------------------
           05  CSTMT0L PIC S9(0004) COMP.
           05  CSTMT0F PIC  X(0001).
           05  FILLER REDEFINES CSTMT0F.
               10  CSTMT0A PIC  X(0001).
           05  CSTMT0I PIC  X(0006).
      *    -------------------------------
           05  CSTATS0L PIC S9(0004) COMP.
           05  CSTATS0F PIC  X(0001).
           05  FILLER REDEFINES CSTATS0F.
               10  CSTATS0A PIC  X(0001).
           05  CSTATS0I PIC  X(0001).
      *    -------------------------------
           05  CSELL PIC S9(0004) COMP.
           05  CSELF PIC  X(0001).
           05  FILLER REDEFINES CSELF.
               10  CSELA PIC  X(0001).
           05  CSELI PIC  X(0002).
      *    -------------------------------
           05  CERMSGL PIC S9(0004) COMP.
           05  CERMSGF PIC  X(0001).
           05  FILLER REDEFINES CERMSGF.
               10  CERMSGA PIC  X(0001).
           05  CERMSGI PIC  X(0079).
      *    -------------------------------
           05  CPFENTRL PIC S9(0004) COMP.
           05  CPFENTRF PIC  X(0001).
           05  FILLER REDEFINES CPFENTRF.
               10  CPFENTRA PIC  X(0001).
           05  CPFENTRI PIC  9(2).
       01  EL852CO REDEFINES EL852BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCARHDGO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CGRPHDGO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CGROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSTHDGO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSTO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CACTHDGO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CACCTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CNAMHDGO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CNAMEO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSEQ1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCAR1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CGRP1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CST1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CACCT1O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CREF1O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CBATCH1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSUM1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CENTRY1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRQST1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSTMT1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSTATS1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSEQ2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCAR2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CGRP2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CST2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CACCT2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CREF2O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CBATCH2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSUM2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CENTRY2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRQST2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSTMT2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSTATS2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSEQ3O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCAR3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CGRP3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CST3O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CACCT3O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CREF3O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CBATCH3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSUM3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CENTRY3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRQST3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSTMT3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSTATS3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSEQ4O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCAR4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CGRP4O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CST4O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CACCT4O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CREF4O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CBATCH4O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSUM4O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CENTRY4O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRQST4O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSTMT4O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSTATS4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSEQ5O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCAR5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CGRP5O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CST5O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CACCT5O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CREF5O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CBATCH5O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSUM5O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CENTRY5O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRQST5O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSTMT5O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSTATS5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSEQ6O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCAR6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CGRP6O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CST6O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CACCT6O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CREF6O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CBATCH6O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSUM6O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CENTRY6O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRQST6O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSTMT6O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSTATS6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSEQ7O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCAR7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CGRP7O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CST7O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CACCT7O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CREF7O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CBATCH7O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSUM7O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CENTRY7O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRQST7O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSTMT7O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSTATS7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSEQ8O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCAR8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CGRP8O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CST8O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CACCT8O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CREF8O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CBATCH8O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSUM8O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CENTRY8O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRQST8O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSTMT8O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSTATS8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSEQ9O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCAR9O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CGRP9O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CST9O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CACCT9O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CREF9O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CBATCH9O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSUM9O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CENTRY9O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRQST9O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSTMT9O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSTATS9O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSEQ0O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCAR0O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CGRP0O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CST0O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CACCT0O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CREF0O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CBATCH0O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSUM0O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CENTRY0O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRQST0O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSTMT0O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSTATS0O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CSELO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERMSGO PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CPFENTRO PIC  99.
      *    -------------------------------
       01  EL852DI REDEFINES EL852BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  DDATEL PIC S9(0004) COMP.
           05  DDATEF PIC  X(0001).
           05  FILLER REDEFINES DDATEF.
               10  DDATEA PIC  X(0001).
           05  DDATEI PIC  X(0008).
      *    -------------------------------
           05  DTIMEL PIC S9(0004) COMP.
           05  DTIMEF PIC  X(0001).
           05  FILLER REDEFINES DTIMEF.
               10  DTIMEA PIC  X(0001).
           05  DTIMEI PIC  X(0005).
      *    -------------------------------
           05  DBATCHL PIC S9(0004) COMP.
           05  DBATCHF PIC  X(0001).
           05  FILLER REDEFINES DBATCHF.
               10  DBATCHA PIC  X(0001).
           05  DBATCHI PIC  X(0006).
      *    -------------------------------
           05  DCARL PIC S9(0004) COMP.
           05  DCARF PIC  X(0001).
           05  FILLER REDEFINES DCARF.
               10  DCARA PIC  X(0001).
           05  DCARI PIC  X(0001).
      *    -------------------------------
           05  DGROUPL PIC S9(0004) COMP.
           05  DGROUPF PIC  X(0001).
           05  FILLER REDEFINES DGROUPF.
               10  DGROUPA PIC  X(0001).
           05  DGROUPI PIC  X(0006).
      *    -------------------------------
           05  DSTL PIC S9(0004) COMP.
           05  DSTF PIC  X(0001).
           05  FILLER REDEFINES DSTF.
               10  DSTA PIC  X(0001).
           05  DSTI PIC  X(0002).
      *    -------------------------------
           05  DACCTL PIC S9(0004) COMP.
           05  DACCTF PIC  X(0001).
           05  FILLER REDEFINES DACCTF.
               10  DACCTA PIC  X(0001).
           05  DACCTI PIC  X(0010).
      *    -------------------------------
           05  DFRESPL PIC S9(0004) COMP.
           05  DFRESPF PIC  X(0001).
           05  FILLER REDEFINES DFRESPF.
               10  DFRESPA PIC  X(0001).
           05  DFRESPI PIC  X(0010).
      *    -------------------------------
           05  DAGENTL PIC S9(0004) COMP.
           05  DAGENTF PIC  X(0001).
           05  FILLER REDEFINES DAGENTF.
               10  DAGENTA PIC  X(0001).
           05  DAGENTI PIC  X(0010).
      *    -------------------------------
           05  DREFL PIC S9(0004) COMP.
           05  DREFF PIC  X(0001).
           05  FILLER REDEFINES DREFF.
               10  DREFA PIC  X(0001).
           05  DREFI PIC  X(0012).
      *    -------------------------------
           05  DSUML PIC S9(0004) COMP.
           05  DSUMF PIC  X(0001).
           05  FILLER REDEFINES DSUMF.
               10  DSUMA PIC  X(0001).
           05  DSUMI PIC  X(0006).
      *    -------------------------------
           05  DSTATUSL PIC S9(0004) COMP.
           05  DSTATUSF PIC  X(0001).
           05  FILLER REDEFINES DSTATUSF.
               10  DSTATUSA PIC  X(0001).
           05  DSTATUSI PIC  X(0001).
      *    -------------------------------
           05  DREQSTL PIC S9(0004) COMP.
           05  DREQSTF PIC  X(0001).
           05  FILLER REDEFINES DREQSTF.
               10  DREQSTA PIC  X(0001).
           05  DREQSTI PIC  X(0008).
      *    -------------------------------
           05  DRQSTRL PIC S9(0004) COMP.
           05  DRQSTRF PIC  X(0001).
           05  FILLER REDEFINES DRQSTRF.
               10  DRQSTRA PIC  X(0001).
           05  DRQSTRI PIC  X(0004).
      *    -------------------------------
           05  DTYPEL PIC S9(0004) COMP.
           05  DTYPEF PIC  X(0001).
           05  FILLER REDEFINES DTYPEF.
               10  DTYPEA PIC  X(0001).
           05  DTYPEI PIC  X(0001).
      *    -------------------------------
           05  DENTRYL PIC S9(0004) COMP.
           05  DENTRYF PIC  X(0001).
           05  FILLER REDEFINES DENTRYF.
               10  DENTRYA PIC  X(0001).
           05  DENTRYI PIC  X(0008).
      *    -------------------------------
           05  DEOMDTL PIC S9(0004) COMP.
           05  DEOMDTF PIC  X(0001).
           05  FILLER REDEFINES DEOMDTF.
               10  DEOMDTA PIC  X(0001).
           05  DEOMDTI PIC  X(0008).
      *    -------------------------------
           05  DSTMTDTL PIC S9(0004) COMP.
           05  DSTMTDTF PIC  X(0001).
           05  FILLER REDEFINES DSTMTDTF.
               10  DSTMTDTA PIC  X(0001).
           05  DSTMTDTI PIC  X(0008).
      *    -------------------------------
           05  DVOIDDTL PIC S9(0004) COMP.
           05  DVOIDDTF PIC  X(0001).
           05  FILLER REDEFINES DVOIDDTF.
               10  DVOIDDTA PIC  X(0001).
           05  DVOIDDTI PIC  X(0008).
      *    -------------------------------
           05  DERMSGL PIC S9(0004) COMP.
           05  DERMSGF PIC  X(0001).
           05  FILLER REDEFINES DERMSGF.
               10  DERMSGA PIC  X(0001).
           05  DERMSGI PIC  X(0079).
      *    -------------------------------
           05  DPFENTRL PIC S9(0004) COMP.
           05  DPFENTRF PIC  X(0001).
           05  FILLER REDEFINES DPFENTRF.
               10  DPFENTRA PIC  X(0001).
           05  DPFENTRI PIC  9(2).
       01  EL852DO REDEFINES EL852BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DBATCHO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DCARO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DGROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DSTO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DACCTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DFRESPO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DAGENTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DREFO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DSUMO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DSTATUSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DREQSTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DRQSTRO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DTYPEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DENTRYO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DEOMDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DSTMTDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DVOIDDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DERMSGO PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DPFENTRO PIC  99.
      *    -------------------------------
00285  01  DISPLAY-MAP REDEFINES EL852BI.
00286      12  FILLER                  PIC X(136).
00287      12  AR-REQUESTS OCCURS 10 TIMES.
00288          16  AR-SEQ-LEN          PIC S9(4)   COMP.
00289          16  AR-SEQ-ATTRB        PIC X.
00290          16  AR-SEQ              PIC 99.
00291          16  AR-CAR-LEN          PIC S9(4)   COMP.
00292          16  AR-CAR-ATTRB        PIC X.
00293          16  AR-CAR              PIC X.
00294          16  AR-GRP-LEN          PIC S9(4)   COMP.
00295          16  AR-GRP-ATTRB        PIC X.
00296          16  AR-GRP              PIC X(6).
00297          16  AR-ST-LEN           PIC S9(4)   COMP.
00298          16  AR-ST-ATTRB         PIC X.
00299          16  AR-ST               PIC XX.
00300          16  AR-ACCT-LEN         PIC S9(4)   COMP.
00301          16  AR-ACCT-ATTRB       PIC X.
00302          16  AR-ACCT             PIC X(10).
00303          16  AR-REF-LEN          PIC S9(4)   COMP.
00304          16  AR-REF-ATTRB        PIC X.
00305          16  AR-REF              PIC X(12).
00306          16  AR-BATCH-LEN        PIC S9(4)   COMP.
00307          16  AR-BATCH-ATTRB      PIC X.
00308          16  AR-BATCH            PIC X(6).
00309          16  AR-SUM-LEN          PIC S9(4)   COMP.
00310          16  AR-SUM-ATTRB        PIC X.
00311          16  AR-SUM              PIC X(6).
00312          16  AR-ENTRY-DT-LEN     PIC S9(4)   COMP.
00313          16  AR-ENTRY-DT-ATTRB   PIC X.
00314          16  AR-ENTRY-DT         PIC X(6).
00315          16  AR-REQST-DT-LEN     PIC S9(4)   COMP.
00316          16  AR-REQST-DT-ATTRB   PIC X.
00317          16  AR-REQST-DT         PIC X(6).
00318          16  AR-STMT-DT-LEN      PIC S9(4)   COMP.
00319          16  AR-STMT-DT-ATTRB    PIC X.
00320          16  AR-STMT-DT          PIC X(6).
00321          16  AR-STATUS-LEN       PIC S9(4)   COMP.
00322          16  AR-STATUS-ATTRB     PIC X.
00323          16  AR-STATUS           PIC X.
00324      12  AR-SELECT-LEN           PIC S9(4)   COMP.
00325      12  AR-SELECT-ATTRB         PIC X.
00326      12  AR-SELECT               PIC 99.
00327      12  AR-SELECT-SPACES  REDEFINES  AR-SELECT
00328                                  PIC XX.
00329
00330      EJECT
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
00332  01  DFHCOMMAREA             PIC X(1024).
00333      EJECT
00334 *                            COPY ERCRQST.
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
00335      EJECT
00336 *                            COPY ERCCOMP.
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
00337      EJECT
00338 *                            COPY ERCACCT.
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
00339      EJECT
00340 *                            COPY ERCSUMM.
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
00341      EJECT
00342 *                            COPY ERCPNDB.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCPNDB.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.025                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = PENDING NEW BUSINESS (ISSUES AND CANCELS) *
00008 *                                                                *
00009 ******************************************************************
00010 *   NOTE: IF THIS FORMAT IS CHANGED, THE SORT RECORD IN THE      *
00011 *         EL861 A/R SYSTEM MAY NEED TO BE CHANGED.               *
00012 ******************************************************************
00013 *                                                                *
00014 *                                                                *
00015 *   FILE TYPE = VSAM,KSDS                                        *
00016 *   RECORD SIZE = 585  RECFORM = FIXED                           *
00017 *                                                                *
00018 *   BASE CLUSTER = ERPNDB                         RKP=2,LEN=11   *
00019 *       ALTERNATE PATH1 = ERPNDB2  (BY CAR GRP STATE ACCOUNT     *
00020 *                                  EFF-DT CERT CHG-SEQ REC-TYPE) *
00021 *                                                 RKP=13,LEN=36  *
00022 *       ALTERNATE PATH2 = ERPNDB3  (BY CO, ORIGINAL BATCH, SEQ.  *
00023 *                                      AND CHG-SEQ.)             *
00024 *                                                RKP=49,LEN=11   *
00025 *       ALTERNATE PATH3 = ERPNDB4  (BY CO, CSR-ID, BATCH, SEQ.   *
00026 *                                      AND CHG-SEQ.)             *
00027 *                                                RKP=60,LEN=15   *
00028 *                                                                *
00029 *   LOG = NO                                                     *
00030 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00031 ******************************************************************
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
011904* 011904                   PEMA  ADD TOTAL FEE PROCESSING
040504* 040504    2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
020305* 020305    2005020000000  PEMA  ADD CLP STATE TO PNDB RECORD
110105* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
032306* 032306                   PEMA  ADD BOW LOAN NUMBER
081606* 081606    2006080800002  PEMA  ADD VIN, ISSUES ONLY
073107* 073107  CR2006051600002  PEMA  ADD OVERCHARGE PROCESSING
072308* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
090408* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
032109* 032109    2009021700002  PEMA  ADD CRED BENE TO PNDB REC
072209* 072209  CR2008101500003  AJRA  ADD BORROWER FIRST NAME TO ENDORS
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
122002******************************************************************
00032
00033  01  PENDING-BUSINESS.
00034      12  PB-RECORD-ID                     PIC XX.
00035          88  VALID-PB-ID                        VALUE 'PB'.
00036
00037      12  PB-CONTROL-PRIMARY.
00038          16  PB-COMPANY-CD                PIC X.
00039          16  PB-ENTRY-BATCH               PIC X(6).
00040          16  PB-BATCH-SEQ-NO              PIC S9(4)     COMP.
00041          16  PB-BATCH-CHG-SEQ-NO          PIC S9(4)     COMP.
00042
00043      12  PB-CONTROL-BY-ACCOUNT.
00044          16  PB-COMPANY-CD-A1             PIC X.
00045          16  PB-CARRIER                   PIC X.
00046          16  PB-GROUPING.
00047              20  PB-GROUPING-PREFIX       PIC XXX.
00048              20  PB-GROUPING-PRIME        PIC XXX.
00049          16  PB-STATE                     PIC XX.
00050          16  PB-ACCOUNT.
00051              20  PB-ACCOUNT-PREFIX        PIC X(4).
00052              20  PB-ACCOUNT-PRIME         PIC X(6).
00053          16  PB-CERT-EFF-DT               PIC XX.
00054          16  PB-CERT-NO.
00055              20  PB-CERT-PRIME            PIC X(10).
00056              20  PB-CERT-SFX              PIC X.
00057          16  PB-ALT-CHG-SEQ-NO            PIC S9(4)     COMP.
00058
00059          16  PB-RECORD-TYPE               PIC X.
00060              88  PB-MAILING-DATA                VALUE '0'.
00061              88  PB-ISSUE                       VALUE '1'.
00062              88  PB-CANCELLATION                VALUE '2'.
00063              88  PB-BATCH-TRAILER               VALUE '9'.
00064
00065      12  PB-CONTROL-BY-ORIG-BATCH.
00066          16  PB-ORIGINAL-COMPANY-CD       PIC X.
00067          16  PB-ORIGINAL-ENTRY-BATCH      PIC X(6).
00068          16  PB-ORIGINAL-SEQ-NO           PIC S9(4)     COMP.
00069          16  PB-ORIGINAL-CHG-SEQ-NO       PIC S9(4)     COMP.
00070
00071      12  PB-CONTROL-BY-CSR.
00072          16  PB-CSR-COMPANY-CD            PIC X.
00073          16  PB-CSR-ID                    PIC X(4).
00074          16  PB-CSR-ENTRY-BATCH           PIC X(6).
00075          16  PB-CSR-BATCH-SEQ-NO          PIC S9(4)     COMP.
00076          16  PB-CSR-BATCH-CHG-SEQ-NO      PIC S9(4)     COMP.
00077 ******************************************************************
00078 *    MAILING DATA IS PROCESSED IN ONLY PRGS. EL512 & EL513       *
00079 ******************************************************************
00080
00081      12  PB-LAST-MAINT-DT                 PIC XX.
00082      12  PB-LAST-MAINT-BY                 PIC X(4).
00083      12  PB-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.
00084
00085      12  PB-RECORD-BODY                   PIC X(375).
00086
00087      12  PB-ISSUE-RECORD   REDEFINES PB-RECORD-BODY.
00088          16  PB-CERT-ORIGIN               PIC X.
00089              88  CLASIC-CREATED-CERT         VALUE '1'.
00090          16  PB-I-NAME.
00091              20  PB-I-INSURED-LAST-NAME   PIC X(15).
00092              20  PB-I-INSURED-FIRST-NAME.
00093                  24  PB-I-INSURED-1ST-INIT PIC X.
00094                  24  FILLER                PIC X(9).
00095              20  PB-I-INSURED-MIDDLE-INIT PIC X.
00096          16  PB-I-AGE                     PIC S99   COMP-3.
00097          16  PB-I-JOINT-AGE               PIC S99   COMP-3.
00098          16  PB-I-BIRTHDAY                PIC XX.
00099          16  PB-I-INSURED-SEX             PIC X.
00100              88  PB-SEX-MALE     VALUE 'M'.
00101              88  PB-SEX-FEMALE   VALUE 'F'.
00102
00103          16  PB-I-LF-TERM                 PIC S999   COMP-3.
00104          16  PB-I-AH-TERM                 PIC S999   COMP-3.
00105          16  PB-I-LOAN-TERM               PIC S999   COMP-3.
00106          16  PB-I-PAY-FREQUENCY           PIC S99    COMP-3.
00107          16  PB-I-SKIP-CODE               PIC X.
00108              88  PB-NO-MONTHS-SKIPPED      VALUE ' ' '0'.
00109              88  PB-SKIP-JULY              VALUE '1'.
00110              88  PB-SKIP-AUGUST            VALUE '2'.
00111              88  PB-SKIP-SEPTEMBER         VALUE '3'.
00112              88  PB-SKIP-JULY-AUG          VALUE '4'.
00113              88  PB-SKIP-AUG-SEPT          VALUE '5'.
00114              88  PB-SKIP-JULY-AUG-SEPT     VALUE '6'.
00115              88  PB-SKIP-JUNE-JULY-AUG     VALUE '7'.
00116              88  PB-SKIP-JUNE              VALUE '8'.
00117              88  PB-SKIP-JUNE-JULY         VALUE '9'.
00118              88  PB-SKIP-AUG-SEPT-OCT      VALUE 'A'.
00119              88  PB-SKIP-BI-WKLY-3RD-PMT   VALUE 'X'.
00120          16  PB-I-TERM-TYPE               PIC X.
00121              88  PB-PAID-MONTHLY           VALUE ' ' 'M'.
00122              88  PB-PAID-WEEKLY            VALUE 'W'.
00123              88  PB-PAID-SEMI-MONTHLY      VALUE 'S'.
00124              88  PB-PAID-BI-WEEKLY         VALUE 'B'.
00125              88  PB-PAID-13-YEARLY         VALUE 'T'.
00126          16  PB-I-NO-OF-PAYMENTS          PIC S999   COMP-3.
00127          16  PB-I-POLICY-FORM-NO          PIC X(12).
00128          16  PB-I-DATA-ENTRY-SW           PIC X.
00129              88  PB-EFF-DT-PROCESSING      VALUE '1' ' '.
00130              88  PB-EXT-DAYS-PROCESSING    VALUE '2'.
00131              88  PB-EXPIRE-DT-PROCESSING   VALUE '3'.
00132              88  PB-1ST-PMT-DT-PROCESSING  VALUE '4'.
00133          16  PB-I-PAYMENT-AMOUNT          PIC S9(7)V99  COMP-3.
073107         16  PB-I-DCC-OVER-CHG-AMT        PIC S9(5)V99  COMP-3.
011410*        16  PB-I-MICROFILM-NO            PIC S9(9)      COMP-3.
011410         16  PB-I-AH-CLP                  PIC S9(5)V99 COMP-3.
011410         16  FILLER                       PIC X.
00136
00137          16  PB-I-LIFE-BENEFIT-CD         PIC XX.
00138              88  PB-VALID-LIFE               VALUE '01' THRU '89'.
00139              88  PB-INVALID-LIFE             VALUE '  ' '00'
00140                                                    '90' THRU '99'.
00141          16  PB-I-LF-BENEFIT-CD   REDEFINES PB-I-LIFE-BENEFIT-CD
00142                                           PIC XX.
00143          16  PB-I-LF-BENEFIT-AMT          PIC S9(9)V99   COMP-3.
100703         16  PB-I-AMOUNT-FINANCED REDEFINES
100703                  PB-I-LF-BENEFIT-AMT     PIC S9(9)V99   COMP-3.
00144          16  PB-I-LF-ALT-BENEFIT-AMT      PIC S9(9)V99   COMP-3.
100703         16  PB-I-UNPAID-CASH-PRICE REDEFINES
100703                  PB-I-LF-ALT-BENEFIT-AMT PIC S9(9)V99   COMP-3.
00145          16  PB-I-LF-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
00146          16  PB-I-LF-ALT-PREMIUM-AMT      PIC S9(7)V99   COMP-3.
100703         16  PB-I-CLP-AMOUNT REDEFINES
100703                  PB-I-LF-ALT-PREMIUM-AMT PIC S9(7)V99   COMP-3.
00147          16  PB-I-LF-CALC-FLAG            PIC X.
00148              88 PB-COMP-LF-PREM               VALUE '?'.
00149          16  PB-I-LF-PREM-CALC            PIC S9(7)V99   COMP-3.
00150          16  PB-I-LF-ALT-PREM-CALC        PIC S9(7)V99   COMP-3.
00151          16  PB-I-LF-RATE                 PIC S99V9(5)   COMP-3.
00152          16  PB-I-LF-ALT-RATE             PIC S99V9(5)   COMP-3.
00153          16  PB-I-LF-POLICY-FEE           PIC S9(3)V99   COMP-3.
00154          16  PB-I-LF-REI-RATE             PIC S99V9(5)   COMP-3.
00155          16  PB-I-LF-ALT-REI-RATE         PIC S99V9(5)   COMP-3.
00156          16  PB-I-LF-ABBR                 PIC XXX.
00157          16  PB-I-LF-INPUT-CD             PIC XX.
00158
00159          16  PB-I-AH-BENEFIT-CD           PIC XX.
00160              88  PB-VALID-AH                 VALUE '01' THRU '89'.
00161              88  PB-INVALID-AH               VALUE '  ' '00'
00162                                                    '90' THRU '99'.
00163          16  PB-I-AH-BENEFIT-AMT          PIC S9(7)V99   COMP-3.
00164          16  PB-I-AH-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
00165          16  PB-I-AH-CALC-FLAG            PIC X.
00166              88 PB-COMP-AH-PREM                  VALUE '?'.
00167          16  PB-I-AH-PREM-CALC            PIC S9(7)V99   COMP-3.
00168          16  PB-I-AH-RATE                 PIC S99V9(5)   COMP-3.
00169          16  PB-I-AH-POLICY-FEE           PIC S9(3)V99   COMP-3.
00170          16  PB-I-AH-REI-RATE             PIC S99V9(5)   COMP-3.
00171          16  PB-I-AH-RATE-TRM             PIC S999       COMP-3.
00172          16  PB-I-AH-ABBR                 PIC XXX.
00173          16  PB-I-AH-INPUT-CD             PIC XXX.
00174
00175          16  PB-I-SPECIAL-REIN-CODE       PIC X.
00176          16  PB-I-REIN-TABLE              PIC XXX.
00177          16  PB-I-BUSINESS-TYPE           PIC 99.
00178          16  PB-I-INDV-GRP-CD             PIC X.
00179          16  PB-I-MORT-CODE.
00180              20  PB-I-TABLE               PIC X.
00181              20  PB-I-INTEREST            PIC XX.
00182              20  PB-I-MORT-TYP            PIC X.
00183          16  PB-I-LF-CRIT-PER             PIC S9(3)      COMP-3.
00184          16  PB-I-AH-CRIT-PER             PIC S9(3)      COMP-3.
011410         16  PB-I-LF-CLP                  PIC S9(5)V99   COMP-3.
00186          16  PB-I-INDV-GRP-OVRD           PIC X.
00187          16  PB-I-RATE-CLASS-OVRD         PIC XX.
00188          16  PB-I-SIG-SW                  PIC X.
00189              88  PB-POLICY-SIGNED             VALUE 'Y'.
00190          16  PB-I-RATE-CLASS              PIC XX.
00191          16  PB-I-RATE-DEVIATION-LF       PIC XXX.
00192          16  PB-I-RATE-DEVIATION-AH       PIC XXX.
00193          16  PB-I-RATE-DEV-PCT-LF         PIC S9V9(6)    COMP-3.
00194          16  PB-I-RATE-DEV-PCT-AH         PIC S9V9(6)    COMP-3.
00195          16  PB-I-LIFE-COMMISSION         PIC SV9(5)     COMP-3.
00196          16  PB-I-JOINT-COMMISSION        PIC SV9(5)     COMP-3.
00197          16  PB-I-AH-COMMISSION           PIC SV9(5)     COMP-3.
00198          16  PB-I-BENEFIT-TYPE            PIC XXX.
00199          16  PB-I-OB-FLAG                 PIC X.
00200              88  PB-I-OB                      VALUE 'B'.
00201              88  PB-I-SUMMARY                 VALUE 'Z'.
00202          16  PB-I-ENTRY-STATUS            PIC X.
00203              88  PB-I-POLICY-IS-ACTIVE        VALUE '1' '3' '4'
122002                                              'M' '5' '9' '2'.
00205              88  PB-I-NORMAL-ENTRY            VALUE '1'.
00206              88  PB-I-POLICY-PENDING          VALUE '2'.
00207              88  PB-I-CONVERSION-ENTRY        VALUE '4'.
00208              88  PB-I-POLICY-IS-REISSUE       VALUE '5'.
122002             88  PB-I-POLICY-IS-MONTHLY       VALUE 'M'.
00209              88  PB-I-REIN-ONLY               VALUE '9'.
00210              88  PB-I-POLICY-IS-DECLINED      VALUE 'D'.
00211              88  PB-I-POLICY-IS-VOIDED        VALUE 'V'.
00212              88  PB-I-PREM-ACCTNG-ONLY        VALUE 'P'.
00213              88  PB-I-UNDERWRITE-POLICY       VALUE 'U'.
00214          16  PB-I-INT-CODE                PIC X.
00215              88  PB-ADD-ON-INTEREST           VALUE 'A'.
00216              88  PB-SIMPLE-INTEREST           VALUE 'S'.
00217          16  PB-I-LOAN-APR                PIC 9(3)V9(4)   COMP-3.
00218          16  PB-I-SOC-SEC-NO              PIC X(11).
00219          16  PB-I-MEMBER-NO               PIC X(12).
00220          16  PB-I-CURR-SEQ                PIC S9(4)       COMP.
110105*        16  PB-I-LOAN-OFFICER            PIC XXX.
110105         16  PB-I-OLD-LOF                 PIC XXX.
00222          16  PB-I-LF-EXPIRE-DT            PIC XX.
00223          16  PB-I-AH-EXPIRE-DT            PIC XX.
00224          16  PB-I-EXTENTION-DAYS          PIC S999        COMP-3.
00225          16  PB-I-TERM-IN-DAYS            PIC S9(5)       COMP-3.
00226          16  PB-I-LIFE-INDICATOR          PIC X.
00227              88  PB-I-JOINT-COVERAGE         VALUE 'J'.
00228          16  PB-I-LIVES                   PIC S9(7)       COMP-3.
00229          16  PB-I-MAIL-ADDRS-SW           PIC X.
00230              88 PB-I-MAIL-ADDRS-NOT-PRESENT  VALUE ' '.
00231              88 PB-I-MAIL-ADDRS-PRESENT      VALUE '1'.
00232          16  PB-I-1ST-PMT-DT              PIC XX.
00233          16  PB-I-JOINT-INSURED.
00234              20 PB-I-JOINT-LAST-NAME      PIC X(15).
00235              20 PB-I-JOINT-FIRST-NAME.
00236                 24  PB-I-JOINT-FIRST-INIT PIC X.
00237                 24  FILLER                PIC X(9).
00238              20 PB-I-JOINT-MIDDLE-INIT    PIC X.
100703*        16  PB-I-BENEFICIARY-NAME        PIC X(25).
100703         16  PB-I-BENEFICIARY-NAME.
100703             20  PB-I-BANK-NUMBER         PIC X(10).
100703             20  FILLER                   PIC X(15).
00240          16  PB-I-LAST-ADD-ON-DT          PIC XX.
011904         16  PB-I-REFERENCE               PIC X(12).
011904         16  FILLER REDEFINES PB-I-REFERENCE.
011904             20  PB-I-TOT-FEES            PIC S9(7)V99 COMP-3.
011904             20  PB-I-TOT-FEES-CALC       PIC S9(7)V99 COMP-3.
020305             20  PB-I-CLP-STATE           PIC XX.
00242          16  PB-I-UNDERWRITING-STATUS     PIC X.
00243              88  PB-I-POLICY-ACCEPTED         VALUE 'A' 'N'.
00244              88  PB-I-POLICY-DECLINED         VALUE 'D'.
00245              88  PB-I-NEEDS-UNDERWRITING      VALUE 'U'.
00246          16  PB-I-STATE-TAX               PIC S9(7)V99 COMP-3.
00247          16  PB-I-MUNI-TAX                PIC S9(7)V99 COMP-3.
00248          16  PB-I-RESIDENT-STATE          PIC XX.
00249          16  PB-I-RATE-CODE               PIC X(4).
00250          16  PB-I-NUM-BILLED              PIC S9(7)    COMP-3.
PEMMOD         16  PB-I-LF-PREM-TAX             PIC S9V9(4)  COMP-3.
PEMMOD         16  PB-I-AH-PREM-TAX             PIC S9V9(4)  COMP-3.
100703         16  PB-I-BANK-FEE                PIC S999V99  COMP-3.
100703         16  PB-I-BANK-NOCHRGB            PIC 99.
040504         16  PB-I-ADDL-CLP                PIC S9(5)V99 COMP-3.
081108         16  PB-I-JOINT-BIRTHDAY          PIC XX.
00252
00253      12  PB-CANCEL-RECORD   REDEFINES PB-RECORD-BODY.
00254          16  PB-C-LF-CANCEL-VOID-SW       PIC X.
00255              88  PB-C-LF-CANCEL-VOIDED        VALUE '1'.
00256          16  PB-C-CANCEL-ORIGIN           PIC X.
00257              88  PB-C-CLAIM-CREATED-CANCEL   VALUE '1'.
00258          16  PB-C-LF-CANCEL-DT            PIC XX.
00259          16  PB-C-LF-CANCEL-AMT           PIC S9(7)V99    COMP-3.
00260          16  PB-C-LF-CALC-REQ             PIC X.
00261              88 PB-COMP-LF-CANCEL            VALUE '?'.
00262          16  PB-C-LF-REF-CALC             PIC S9(7)V99    COMP-3.
00263          16  PB-C-LF-REM-TERM             PIC S9(3)       COMP-3.
00264          16  PB-C-AH-CANCEL-VOID-SW       PIC X.
00265              88  PB-C-AH-CANCEL-VOIDED        VALUE '1'.
00266          16  PB-C-AH-CANCEL-DT            PIC XX.
00267          16  PB-C-AH-CANCEL-AMT           PIC S9(7)V99    COMP-3.
00268          16  PB-C-AH-CALC-REQ             PIC X.
00269              88 PB-COMP-AH-CANCEL            VALUE '?'.
00270          16  PB-C-AH-REF-CALC             PIC S9(7)V99    COMP-3.
00271          16  PB-C-AH-REM-TERM             PIC S9(3)       COMP-3.
00272          16  PB-C-LAST-NAME               PIC X(15).
00273          16  PB-C-REFUND-SW               PIC X.
00274              88  PB-C-REFUND-CREATED          VALUE 'Y'.
00275              88  PB-C-REFUND-REQUESTED        VALUE 'R'.
00276          16  PB-C-LIVES                   PIC S9(3)       COMP-3.
00277          16  PB-C-PAYEE-CODE              PIC X(6).
00278          16  PB-C-LF-REFUND-OVERRIDE      PIC X.
00279          16  PB-C-AH-REFUND-OVERRIDE      PIC X.
00280          16  PB-C-LF-COMM-CHARGEBACK      PIC X.
00281          16  PB-C-AH-COMM-CHARGEBACK      PIC X.
00282          16  PB-C-REFERENCE               PIC X(12).
PEMMOD         16  PB-C-LF-PREM-TAX             PIC S9V9(4)  COMP-3.
PEMMOD         16  PB-C-AH-PREM-TAX             PIC S9V9(4)  COMP-3.
081606         16  PB-C-POST-CARD-IND           PIC X.
081606         16  PB-C-CANCEL-REASON           PIC X.
072308         16  PB-C-REF-INTERFACE-SW        PIC X.
00283          16  FILLER                       PIC X(09).
PEMMOD*        16  FILLER                       PIC X(18).
00284          16  PB-C-POLICY-FORM-NO          PIC X(12).
072308*        16  PB-C-MICROFILM-NO            PIC S9(9)      COMP-3.
072308         16  PB-C-NH-INT-ON-REFS          PIC S9(7)V99   COMP-3.
00286          16  PB-CANCELED-CERT-DATA.
00287              20  PB-CI-INSURED-NAME.
00288                  24  PB-CI-LAST-NAME      PIC X(15).
00289                  24  PB-CI-INITIALS       PIC XX.
00290              20  PB-CI-INSURED-AGE        PIC S99         COMP-3.
00291              20  PB-CI-INSURED-SEX        PIC X.
00292              20  PB-CI-LF-TERM            PIC S999        COMP-3.
00293              20  PB-CI-LF-BENEFIT-CD      PIC XX.
00294              20  PB-CI-LF-BENEFIT-AMT     PIC S9(9)V99    COMP-3.
00295              20  PB-CI-LF-ALT-BENEFIT-AMT PIC S9(9)V99    COMP-3.
00296              20  PB-CI-LF-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
00297              20  PB-CI-LF-ALT-PREMIUM-AMT PIC S9(7)V99    COMP-3.
00298              20  PB-CI-AH-TERM            PIC S999        COMP-3.
00299              20  PB-CI-AH-BENEFIT-CD      PIC XX.
00300              20  PB-CI-AH-BENEFIT-AMT     PIC S9(7)V99    COMP-3.
00301              20  PB-CI-AH-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
00302              20  PB-CI-RATE-CLASS         PIC XX.
00303              20  PB-CI-RATE-DEV-LF        PIC XXX.
00304              20  PB-CI-RATE-DEV-AH        PIC XXX.
00305              20  PB-CI-RATE-DEV-PCT-LF    PIC S9V9(6)     COMP-3.
00306              20  PB-CI-RATE-DEV-PCT-AH    PIC S9V9(6)     COMP-3.
00307              20  PB-CI-LIFE-COMMISSION    PIC SV9(5)      COMP-3.
00308              20  PB-CI-AH-COMMISSION      PIC SV9(5)      COMP-3.
00309              20  PB-CI-LF-ABBR            PIC X(3).
00310              20  PB-CI-AH-ABBR            PIC X(3).
00311              20  PB-CI-OB-FLAG            PIC X.
00312                  88  PB-CI-OB                VALUE 'B'.
00313              20  PB-CI-LF-POLICY-STATUS   PIC X.
00314                  88  PB-CI-LF-POLICY-IS-ACTIVE       VALUE '1' '3'
122002                                           'M' '4' '5' '9' '2'.
00316                  88  PB-CI-LF-NORMAL-ENTRY           VALUE '1'.
00317                  88  PB-CI-LF-POLICY-PENDING         VALUE '2'.
00318                  88  PB-CI-LF-POLICY-IS-RESTORE      VALUE '3'.
00319                  88  PB-CI-LF-CONVERSION-ENTRY       VALUE '4'.
00320                  88  PB-CI-LF-POLICY-IS-REISSUE      VALUE '5'.
122002                 88  PB-CI-LF-POLICY-IS-MONTHLY      VALUE 'M'.
00321                  88  PB-CI-LF-LUMP-SUM-DISAB         VALUE '6'.
00322                  88  PB-CI-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
00323                  88  PB-CI-LF-CANCEL-APPLIED         VALUE '8'.
00324                  88  PB-CI-LF-REIN-ONLY              VALUE '9'.
00325                  88  PB-CI-LF-POLICY-IS-DECLINED     VALUE 'D'.
00326                  88  PB-CI-LF-POLICY-IS-VOID         VALUE 'V'.
00327              20  PB-CI-AH-POLICY-STATUS   PIC X.
00328                  88  PB-CI-AH-POLICY-IS-ACTIVE       VALUE '1' '3'
122002                                           'M' '4' '5' '9' '2'.
00330                  88  PB-CI-AH-NORMAL-ENTRY           VALUE '1'.
00331                  88  PB-CI-AH-POLICY-PENDING         VALUE '2'.
00332                  88  PB-CI-AH-POLICY-IS-RESTORE      VALUE '3'.
00333                  88  PB-CI-AH-CONVERSION-ENTRY       VALUE '4'.
00334                  88  PB-CI-AH-POLICY-IS-REISSUE      VALUE '5'.
122002                 88  PB-CI-AH-POLICY-IS-MONTHLY      VALUE 'M'.
00335                  88  PB-CI-AH-LUMP-SUM-DISAB         VALUE '6'.
00336                  88  PB-CI-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
00337                  88  PB-CI-AH-CANCEL-APPLIED         VALUE '8'.
00338                  88  PB-CI-AH-REIN-ONLY              VALUE '9'.
00339                  88  PB-CI-AH-POLICY-IS-DECLINED     VALUE 'D'.
00340                  88  PB-CI-AH-POLICY-IS-VOID         VALUE 'V'.
00341              20  PB-CI-PAY-FREQUENCY      PIC 99.
00342              20  PB-CI-LOAN-APR           PIC 9(3)V9(4)   COMP-3.
00343              20  PB-CI-SOC-SEC-NO         PIC X(11).
00344              20  PB-CI-MEMBER-NO          PIC X(12).
00345              20  PB-CI-INT-CODE           PIC X.
00346                  88  PB-CI-ADD-ON                  VALUE 'A'.
00347                  88  PB-CI-SIMPLE                  VALUE 'S'.
00348              20  PB-CI-LOAN-TERM          PIC S999        COMP-3.
00349              20  PB-CI-LOAN-1ST-PMT-DT    PIC X(2).
00350              20  PB-CI-COMP-EXCP-SW       PIC X.
00351                  88  PB-CI-NO-COMP-EXCP            VALUE ' '.
00352                  88  PB-CI-CERT-HAS-ERCOMM-ENTRY   VALUE '1'.
00353              20  PB-CI-ENTRY-STATUS       PIC X.
00354              20  PB-CI-CURR-SEQ           PIC S9(4)       COMP.
00355              20  PB-CI-AH-PAID-THRU-DT    PIC XX.
00356              20  PB-CI-AH-SETTLEMENT-DT   PIC XX.
00357              20  PB-CI-DEATH-DT           PIC XX.
00358              20  PB-CI-LF-PRIOR-CANCEL-DT PIC XX.
00359              20  PB-CI-AH-PRIOR-CANCEL-DT PIC XX.
00360              20  PB-CI-AH-CANCEL-AMT      PIC S9(7)V99    COMP-3.
00361              20  PB-CI-LF-CANCEL-AMT      PIC S9(7)V99    COMP-3.
00362              20  PB-CI-CREDIT-INTERFACE-SW-1 PIC X.
00363              20  PB-CI-CREDIT-INTERFACE-SW-2 PIC X.
00364              20  PB-CI-ENTRY-DT              PIC XX.
00365              20  PB-CI-ENTRY-BATCH           PIC X(6).
00366              20  PB-CI-LF-EXPIRE-DT          PIC XX.
00367              20  PB-CI-AH-EXPIRE-DT          PIC XX.
00368              20  PB-CI-EXTENTION-DAYS        PIC S999     COMP-3.
00369              20  PB-CI-TERM-IN-DAYS          PIC S9(5)    COMP-3.
110105             20  PB-CI-OLD-LOF               PIC XXX.
110105*            20  PB-CI-LOAN-OFFICER          PIC XXX.
00371              20  PB-CI-LIVES                 PIC S9(3)    COMP-3.
00372              20  PB-CI-LF-CRIT-PER           PIC S9(3)    COMP-3.
00373              20  PB-CI-AH-CRIT-PER           PIC S9(3)    COMP-3.
00374              20  PB-CI-INDV-GRP-CD           PIC X.
100703             20  PB-CI-BENEFICIARY-NAME.
100703                 24  PB-CI-BANK-NUMBER       PIC X(10).
100703                 24  FILLER                  PIC X(15).
00376              20  PB-CI-NOTE-SW               PIC X.
00377              20  PB-CI-CANCEL-FEE            PIC S9(3)V99 COMP-3.
00378              20  PB-CI-MUNI-TAX              PIC S9(7)V99 COMP-3.
00379              20  PB-CI-STATE-TAX             PIC S9(7)V99 COMP-3.
040504             20  PB-CI-ADDL-CLP              PIC S9(5)V99 COMP-3.
110105             20  PB-CI-LOAN-OFFICER          PIC X(5).
032306             20  PB-CI-BOW-LOAN-NUMBER       PIC X(14).
072209             20  PB-CI-FIRST-NAME            PIC X(10).
00380
072209         16  FILLER                       PIC X(17).
072209*032306  16  FILLER                       PIC X(27).
040504*        16  FILLER                       PIC X(46).
00382
00383      12  PB-MAIL-RECORD    REDEFINES PB-RECORD-BODY.
00384          16  FILLER                       PIC X(10).
00385          16  PB-M-INSURED-LAST-NAME       PIC X(15).
00386          16  PB-M-INSURED-FIRST-NAME      PIC X(10).
00387          16  PB-M-INSURED-MID-INIT        PIC X.
00388          16  PB-M-INSURED-AGE             PIC 99.
00389          16  PB-M-INSURED-BIRTHDAY        PIC XX.
00390          16  PB-M-INSURED-SEX             PIC X.
00391          16  PB-M-INSURED-SOC-SEC-NO      PIC X(11).
00392          16  PB-M-INSURED-ADDRESS-1       PIC X(30).
00393          16  PB-M-INSURED-ADDRESS-2       PIC X(30).
00394          16  PB-M-INSURED-CITY-STATE.
051810             20  PB-M-INSURED-CITY        PIC X(28).
051810             20  PB-M-INSURED-STATE       PIC XX.
00395          16  PB-M-INSURED-ZIP-CODE.
00396              20  PB-M-INSURED-ZIP-PRIME.
00397                  24  PB-M-INSURED-ZIP-1   PIC X.
00398                      88  PB-M-CANADIAN-POST-CODE
00399                                              VALUE 'A' THRU 'Z'.
00400                  24  FILLER               PIC X(4).
00401              20  PB-M-INSURED-ZIP-PLUS4   PIC X(4).
00402          16  PB-M-INSURED-CANADIAN-ZIP  REDEFINES
00403                                         PB-M-INSURED-ZIP-CODE.
00404              20  PM-M-INS-CAN-POST1       PIC XXX.
00405              20  PM-M-INS-CAN-POST2       PIC XXX.
00406              20  FILLER                   PIC XXX.
00407          16  PB-M-INSURED-PHONE-NO        PIC 9(10).
081108         16  PB-M-JOINT-BIRTHDAY          PIC XX.
               16  PB-M-CRED-BENE-NAME          PIC X(30).
               16  PB-M-CRED-BENE-ADDR1         PIC X(30).
               16  PB-M-CRED-BENE-ADDR2         PIC X(30).
               16  PB-M-CRED-BENE-CITYST.
                   20  PB-M-CRED-BENE-CITY      PIC X(28).
                   20  PB-M-CRED-BENE-STATE     PIC XX.
081108         16  FILLER                       PIC X(92).
00409
00410      12  PB-BATCH-RECORD   REDEFINES PB-RECORD-BODY.
00411          16  FILLER                       PIC X(10).
00412          16  PB-B-LF-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00413          16  PB-B-LF-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00414          16  PB-B-LF-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00415          16  PB-B-LF-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00416          16  PB-B-LF-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00417          16  PB-B-LF-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00418          16  PB-B-AH-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00419          16  PB-B-AH-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00420          16  PB-B-AH-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00421          16  PB-B-AH-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00422          16  PB-B-AH-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00423          16  PB-B-AH-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00424          16  PB-B-ISSUE-CNT-REMITTED      PIC S9(5)    COMP-3.
00425          16  PB-B-ISSUE-CNT-ENTERED       PIC S9(5)    COMP-3.
00426          16  PB-B-CANCEL-CNT-REMITTED     PIC S9(5)    COMP-3.
00427          16  PB-B-CANCEL-CNT-ENTERED      PIC S9(5)    COMP-3.
00428          16  PB-B-HIGHEST-SEQ-NO          PIC S9(4)    COMP.
00429          16  PB-ACCOUNT-NAME              PIC X(30).
00430          16  PB-PREM-REF-RPT-FLAG         PIC X.
00431          16  PB-REFERENCE                 PIC X(12).
00432          16  PB-B-RECEIVED-DT             PIC XX.
00433          16  FILLER                       PIC X(234).
00434
00435      12  PB-RECORD-STATUS.
00436          16  PB-CREDIT-SELECT-DT          PIC XX.
00437          16  PB-CREDIT-ACCEPT-DT          PIC XX.
00438          16  PB-BILLED-DT                 PIC XX.
00439          16  PB-BILLING-STATUS            PIC X.
00440              88  PB-ENTRY-REVERSED            VALUE 'R'.
00441              88  PB-EL860-INTERNAL-ERROR      VALUE 'E'.
00442              88  PB-EL860-INTERNAL-PROCESS    VALUE 'P'.
00443          16  PB-RECORD-BILL               PIC X.
00444              88  PB-RECORD-ON-HOLD            VALUE 'H'.
00445              88  PB-RECORD-RETURNED           VALUE 'R'.
00446              88  PB-RECORD-ENDORSED           VALUE 'E'.
00447              88  PB-OVERRIDE-LIFE             VALUE 'L'.
00448              88  PB-OVERRIDE-AH               VALUE 'A'.
00449              88  PB-OVERRIDE-BOTH             VALUE 'B'.
00450          16  PB-BATCH-ENTRY               PIC X.
00451              88  PB-POLICY-IS-DECLINED        VALUE 'D'.
00452              88  PB-REIN-ONLY-CERT            VALUE 'R'.
00453              88  PB-REISSUED-CERT             VALUE 'E'.
122002             88  PB-MONTHLY-CERT              VALUE 'M'.
00454              88  PB-PREM-ACCTNG-ONLY          VALUE 'P'.
00455              88  PB-NEEDS-UNDERWRITING        VALUE 'U'.
00456              88  PB-POLICY-IS-VOIDED          VALUE 'V'.
00457          16  PB-FORCE-CODE                PIC X.
00458              88  PB-FORCE-OFF                 VALUE ' ' '0'.
00459              88  PB-ISSUE-FORCE               VALUE 'A' 'O'.
00460              88  PB-CANCEL-FORCE              VALUE '8'.
00461              88  PB-ALL-ISSUE-FORCED          VALUE 'A' 'O'.
00462              88  PB-ALL-CANCEL-FORCED         VALUE '8'.
00463              88  PB-ALL-CANCEL-FORCED-NO-FEE  VALUE '9'.
00464              88  PB-CANCEL-DATE-FORCED        VALUE 'D'.
00465              88  PB-CANCEL-DATE-FORCED-NO-FEE VALUE 'E'.
00466              88  PB-ISSUE-DATE-FORCED         VALUE 'D'.
073107             88  PB-OVERCHARGE-FORCE          VALUE 'O'.
00467          16  PB-FATAL-FLAG                PIC X.
00468              88  PB-FATAL-ERRORS              VALUE 'X'.
00469          16  PB-FORCE-ER-CD               PIC X.
00470              88  PB-FORCE-ERRORS              VALUE 'F'.
00471              88  PB-UNFORCED-ERRORS           VALUE 'X'.
00472          16  PB-WARN-ER-CD                PIC X.
00473              88  PB-WARNING-ERRORS            VALUE 'W'.
00474          16  FILLER                       PIC X.
00475          16  PB-OUT-BAL-CD                PIC X.
00476              88  PB-OUT-OF-BAL                VALUE 'O'.
00477          16  PB-LIFE-OVERRIDE-L1          PIC X.
00478          16  PB-AH-OVERRIDE-L1            PIC X.
00479          16  PB-INPUT-DT                  PIC XX.
00480          16  PB-INPUT-BY                  PIC X(4).
00481          16  PB-CHG-COUNT                 PIC 9(3)        COMP-3.
00482          16  PB-CALC-TOLERANCE            PIC 9(3)V99     COMP-3.
00483          16  PB-TOLERANCE-REJECT-SW       PIC X.
00484          16  PB-LF-EARNING-METHOD         PIC X.
00485          16  PB-AH-EARNING-METHOD         PIC X.
00486          16  PB-LF-TERM-CALC-METHOD       PIC X.
00487          16  PB-AH-TERM-CALC-METHOD       PIC X.
00488          16  PB-REIN-CD                   PIC XXX.
00489          16  PB-LF-REFUND-TYPE            PIC X.
00490          16  PB-AH-REFUND-TYPE            PIC X.
00491          16  PB-ACCT-EFF-DT               PIC XX.
00492          16  PB-ACCT-EXP-DT               PIC XX.
00493          16  PB-COMPANY-ID                PIC X(3).
00494          16  PB-LF-BILLED-AMTS            PIC S9(7)V99  COMP-3.
00495          16  PB-AH-BILLED-AMTS            PIC S9(7)V99  COMP-3.
00496          16  PB-SV-CARRIER                PIC X.
00497          16  PB-SV-GROUPING               PIC X(6).
00498          16  PB-SV-STATE                  PIC XX.
00499          16  PB-CONFIRMATION-REPT-DT      PIC XX.
00500          16  PB-GA-BILLING-INFO.
00501              20  PB-GA-BILL-DT OCCURS 5 TIMES
00502                                           PIC XX.
00503          16  PB-SV-REMIT-TO  REDEFINES
00504              PB-GA-BILLING-INFO           PIC X(10).
00505          16  PB-NO-OF-ERRORS              PIC S9(3) COMP-3.
110105         16  PB-I-LOAN-OFFICER            PIC X(5).
081606         16  PB-I-VIN                     PIC X(17).
00506
110105         16  FILLER                       PIC X(04).
110105         16  IMNET-BYPASS-SW              PIC X.
00508
00509 ******************************************************************
00510 *                COMMON EDIT ERRORS                              *
00511 ******************************************************************
00512
00513      12  PB-COMMON-ERRORS.
00514          16  PB-COMMON-ERROR    OCCURS 10 TIMES
00515                                            PIC S9(4)     COMP.
00516
00517 ******************************************************************
00343      EJECT
00344
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA AR-REQUEST-RECORD
                                COMPENSATION-MASTER ACCOUNT-MASTER
                                SUMM-CROSS-REFERENCE PENDING-BUSINESS.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL8521' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00346
00347      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00348      MOVE 1                      TO EMI-NUMBER-OF-LINES.
00349
00350      IF EIBCALEN = 0
00351          GO TO 8800-UNAUTHORIZED-ACCESS.
00352
00353      MOVE EIBTRMID               TO QID-TERM.
00354      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00355      MOVE '5'                    TO DC-OPTION-CODE.
00356      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
00357      MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.
00358      MOVE DC-GREG-DATE-1-EDIT    TO WS-CURRENT-DT.
00359
00360      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00361          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00362              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00363              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00364              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00365              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00366              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00367              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00368              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00369              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00370          ELSE
00371              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00372              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00373              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00374              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00375              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00376              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00377              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00378              MOVE SPACES               TO PI-SAVED-PROGRAM-6.
00379
00380      MOVE LOW-VALUES             TO EL852BI.
00381
00382      
      * EXEC CICS HANDLE CONDITION
00383 *        PGMIDERR  (9600-PGMID-ERROR)
00384 *        ERROR     (9990-ABEND)
00385 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00005416' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303035343136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00386
00387      IF EIBTRNID NOT = TRANS-EXJ6
00388         IF PI-MAP-NAME = EL852D
00389            MOVE PI-SELECT-KEY      TO PI-ACCESS-KEY
00390            GO TO 3200-DISPLAY-BATCH-REQUEST.
00391
00392      IF EIBTRNID NOT = TRANS-EXJ6
00393         MOVE PI-SELECT-KEY         TO PI-ACCESS-KEY
00394         GO TO 3000-DISPLAY-REQUESTS.
00395
00396      IF EIBAID = DFHCLEAR
00397         
      * EXEC CICS ASKTIME
00398 *       END-EXEC
      *    MOVE '0"                    "   #00005431' TO DFHEIV0
           MOVE X'302220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00399         IF PI-MAP-NAME = EL852D
00400            IF PI-REQUEST-KEY (1) NOT = SPACES
00401               MOVE PI-PREV-MAP-NAME TO PI-MAP-NAME
00402               MOVE SPACES           TO PI-PREV-MAP-NAME
00403               MOVE PI-REQUEST-KEY (1) TO PI-ACCESS-KEY
00404               GO TO 3000-DISPLAY-REQUESTS.
00405
00406      IF EIBAID = DFHCLEAR
00407          GO TO 9400-CLEAR.
00408
00409      IF PI-PROCESSOR-ID = 'LGXX'
00410          GO TO 0200-RECEIVE.
00411
00412      
      * EXEC CICS READQ TS
00413 *        QUEUE  (QID)
00414 *        INTO   (SECURITY-CONTROL)
00415 *        LENGTH (SC-COMM-LENGTH)
00416 *        ITEM   (SC-ITEM)
00417 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00005446' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00418
00419      MOVE SC-CREDIT-DISPLAY (3)   TO PI-DISPLAY-CAP.
00420      MOVE SC-CREDIT-UPDATE  (3)   TO PI-MODIFY-CAP.
00421
00422      IF NOT DISPLAY-CAP
00423          MOVE 'READ'          TO SM-READ
00424          PERFORM 9995-SECURITY-VIOLATION
00425          MOVE ER-0070         TO  EMI-ERROR
00426          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00427          GO TO 8100-SEND-INITIAL-MAP.
00428
00429      EJECT
00430
00431 ******************************************************************
00432 *                                                                *
00433 *              R E C E I V E   M A P S                           *
00434 *                                                                *
00435 ******************************************************************
00436
00437  0200-RECEIVE.
00438
00439      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00440          MOVE ER-0008            TO EMI-ERROR
00441          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00442          IF PI-MAP-NAME = EL852B
00443             MOVE -1              TO BPFENTRL
00444             GO TO 8200-SEND-DATAONLY
00445          ELSE
00446             IF PI-MAP-NAME = EL852C
00447                MOVE -1           TO CPFENTRL
00448                GO TO 8200-SEND-DATAONLY
00449             ELSE
00450                MOVE -1           TO DPFENTRL
00451                GO TO 8200-SEND-DATAONLY.
00452
00453      
      * EXEC CICS RECEIVE
00454 *        MAP      (PI-MAP-NAME)
00455 *        MAPSET   (MAPSET-EL8521S)
00456 *        INTO     (EL852BI)
00457 *    END-EXEC.
           MOVE LENGTH OF
            EL852BI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00005487' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 EL852BI, 
                 DFHEIV11, 
                 MAPSET-EL8521S, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00458
00459      IF PI-MAP-NAME = EL852B
00460          IF BPFENTRL GREATER THAN ZERO
00461              IF EIBAID NOT = DFHENTER
00462                  MOVE ER-0004    TO EMI-ERROR
00463                  MOVE AL-UNBOF   TO BPFENTRA
00464                  MOVE -1         TO BPFENTRL
00465                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00466                  GO TO 8200-SEND-DATAONLY
00467              ELSE
00468                  IF BPFENTRI NUMERIC AND
00469                     BPFENTRI GREATER 0 AND LESS 25
00470                      MOVE PF-VALUES (BPFENTRI) TO EIBAID
00471                  ELSE
00472                      MOVE ER-0029  TO EMI-ERROR
00473                      MOVE AL-UNBOF TO BPFENTRA
00474                      MOVE -1       TO BPFENTRL
00475                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00476                      GO TO 8200-SEND-DATAONLY.
00477
00478      IF PI-MAP-NAME = EL852C
00479          IF CPFENTRL GREATER THAN ZERO
00480              IF EIBAID NOT = DFHENTER
00481                  MOVE ER-0004    TO EMI-ERROR
00482                  MOVE AL-UNBOF   TO CPFENTRA
00483                  MOVE -1         TO CPFENTRL
00484                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00485                  GO TO 8200-SEND-DATAONLY
00486              ELSE
00487                  IF CPFENTRI NUMERIC AND
00488                     CPFENTRI GREATER 0 AND LESS 25
00489                      MOVE PF-VALUES (CPFENTRI) TO EIBAID
00490                  ELSE
00491                      MOVE ER-0029  TO EMI-ERROR
00492                      MOVE AL-UNBOF TO BPFENTRA
00493                      MOVE -1       TO BPFENTRL
00494                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00495                      GO TO 8200-SEND-DATAONLY.
00496
00497      IF PI-MAP-NAME = EL852D
00498          IF CPFENTRL GREATER THAN ZERO
00499              IF EIBAID NOT = DFHENTER
00500                  MOVE ER-0004    TO EMI-ERROR
00501                  MOVE AL-UNBOF   TO DPFENTRA
00502                  MOVE -1         TO DPFENTRL
00503                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00504                  GO TO 8200-SEND-DATAONLY
00505              ELSE
00506                  IF CPFENTRI NUMERIC AND
00507                     CPFENTRI GREATER 0 AND LESS 25
00508                      MOVE PF-VALUES (DPFENTRI) TO EIBAID
00509                  ELSE
00510                      MOVE ER-0029  TO EMI-ERROR
00511                      MOVE AL-UNBOF TO DPFENTRA
00512                      MOVE -1       TO DPFENTRL
00513                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00514                      GO TO 8200-SEND-DATAONLY.
00515
00516      EJECT
00517
00518 ******************************************************************
00519 *                                                                *
00520 *              C H E C K   P F K E Y S                           *
00521 *                                                                *
00522 ******************************************************************
00523
00524  0300-CHECK-PFKEYS.
00525
00526      IF EIBAID = DFHPF23
00527          GO TO 8810-PF23.
00528
00529      IF EIBAID = DFHPF24
00530          GO TO 9200-RETURN-MAIN-MENU.
00531
00532      IF EIBAID = DFHPF12
00533          GO TO 9500-PF12.
00534
00535      IF EIBAID = DFHENTER
00536          GO TO 1000-EDIT-MAPS.
00537
00538      IF PI-MAP-NAME = EL852D
00539         MOVE ER-0008 TO EMI-ERROR
00540         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00541         MOVE -1                   TO DPFENTRL
00542         GO TO 8200-SEND-DATAONLY.
00543
00544      IF EIBAID = DFHPF1 OR DFHPF3
00545         IF PI-END-OF-FILE
00546            MOVE ER-2251 TO EMI-ERROR
00547            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00548            GO TO 8200-SEND-DATAONLY
00549         ELSE
00550            GO TO 3000-DISPLAY-REQUESTS.
00551
00552      IF EIBAID = DFHPF2 OR DFHPF4
00553         IF PI-TOP-OF-FILE
00554            MOVE ER-2252 TO EMI-ERROR
00555            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00556            GO TO 8200-SEND-DATAONLY
00557         ELSE
00558            GO TO 3100-DISPLAY-PREV-REQUESTS.
00559
00560      IF EIBAID = DFHPF5
00561          PERFORM 5000-EDIT-BATCH  THRU  5999-EXIT
00562          GO TO 8200-SEND-DATAONLY.
00563
00564      MOVE ER-0008 TO EMI-ERROR.
00565      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00566
00567      IF PI-MAP-NAME = EL852B
00568         MOVE -1                  TO BPFENTRL.
00569
00570      IF PI-MAP-NAME = EL852C
00571         MOVE -1                  TO CPFENTRL.
00572
00573      IF PI-MAP-NAME = EL852D
00574         MOVE -1                  TO DPFENTRL.
00575
00576      GO TO 8200-SEND-DATAONLY.
00577
00578      EJECT
00579
00580 ******************************************************************
00581 *                                                                *
00582 *                  E D I T    M A P S                            *
00583 *                                                                *
00584 ******************************************************************
00585
00586  1000-EDIT-MAPS.
00587
00588      IF PI-MAP-NAME = EL852D
00589         GO TO 1200-EDIT-BATCH-DISPLAY.
00590
00591  1100-EDIT-DISPLAY-REQUESTS.
00592
00593      MOVE +0                     TO WS-SUB1.
00594
00595  1110-EDIT-REQUEST.
00596
00597      ADD  +1                     TO WS-SUB1.
00598
00599      IF WS-SUB1 GREATER THAN +10
00600         IF AR-SELECT-LEN GREATER THAN ZEROS
00601            GO TO 1170-EDIT-SELECTION
00602         ELSE
00603            GO TO 3000-DISPLAY-REQUESTS.
00604
00605      IF AR-REQST-DT-LEN (WS-SUB1) GREATER THAN ZEROS
00606         IF AR-REQST-DT  (WS-SUB1) = SPACES
00607            MOVE LOW-VALUES          TO WS-SAVE-RQST-DT
00608            MOVE PI-REQUEST-KEY (WS-SUB1) TO PI-ACCESS-KEY
00609            MOVE 'Y'                 TO UPDATE-RQST-SW
00610         ELSE
00611            MOVE ER-3131             TO EMI-ERROR
00612            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00613            GO TO 8200-SEND-DATAONLY.
00614
00615      IF PI-PROCESSOR-ID = 'LGXX'
00616          IF AR-STMT-DT-LEN (WS-SUB1) GREATER THAN ZEROS
00617              MOVE PI-REQUEST-KEY (WS-SUB1) TO PI-ACCESS-KEY
00618              MOVE 'Y'                 TO UPDATE-STMT-SW
00619                  IF AR-STMT-DT  (WS-SUB1) = SPACES
00620                      MOVE LOW-VALUES    TO WS-SAVE-STMT-DT
00621                  ELSE
00622                      MOVE '3'           TO DC-OPTION-CODE
00623                      MOVE AR-STMT-DT (WS-SUB1)
00624                                         TO DC-GREG-DATE-1-YMD
00625                      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
00626                          IF NO-CONVERSION-ERROR
00627                              MOVE DC-BIN-DATE-1
00628                                          TO WS-SAVE-STMT-DT
00629                          ELSE
00630                              MOVE ER-3131             TO EMI-ERROR
00631                              PERFORM 9900-ERROR-FORMAT
00632                                                    THRU 9900-EXIT
00633                              GO TO 8200-SEND-DATAONLY.
00634
00635      IF AR-STATUS-LEN (WS-SUB1) GREATER THAN ZEROS
00636          IF AR-STATUS   (WS-SUB1) NOT = SPACES
00637              MOVE ER-3191        TO EMI-ERROR
00638              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00639              GO TO 8200-SEND-DATAONLY
00640          ELSE
00641              MOVE PI-REQUEST-KEY (WS-SUB1) TO PI-ACCESS-KEY
00642              MOVE 'Y'            TO UPDATE-STATUS-SW
00643              MOVE SPACES         TO WS-SAVE-STATUS
00644              MOVE WS-SUB1        TO AR-SELECT
00645              PERFORM 5000-EDIT-BATCH  THRU  5999-EXIT
00646              MOVE ' '            TO UPDATE-STATUS-SW
00647              MOVE SPACES         TO AR-SELECT-SPACES
00648              GO TO 1110-EDIT-REQUEST.
00649
00650      IF UPDATE-RQST-DT OR UPDATE-STMT-DT
00651          PERFORM 2000-UPDATE-REQUEST THRU 2090-EXIT
00652          MOVE ' '                TO UPDATE-RQST-SW
00653          MOVE ' '                TO UPDATE-STMT-SW.
00654
00655      GO TO 1110-EDIT-REQUEST.
00656
00657  1170-EDIT-SELECTION.
00658
00659      IF AR-SELECT NOT NUMERIC
00660         MOVE ER-3132             TO EMI-ERROR
00661         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00662         GO TO 8200-SEND-DATAONLY.
00663
00664      IF AR-SELECT GREATER THAN +0 AND LESS THAN +11
00665         NEXT SENTENCE
00666      ELSE
00667         MOVE ER-3132             TO EMI-ERROR
00668         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00669         GO TO 8200-SEND-DATAONLY.
00670
00671      MOVE PI-REQUEST-KEY (AR-SELECT) TO PI-ACCESS-KEY.
00672      MOVE PI-MAP-NAME                TO PI-PREV-MAP-NAME.
00673      MOVE EL852D                     TO PI-MAP-NAME.
00674      GO TO 3200-DISPLAY-BATCH-REQUEST.
00675
00676      EJECT
00677
00678  1200-EDIT-BATCH-DISPLAY.
00679
00680      IF DREQSTL GREATER THAN ZEROS
00681         IF DREQSTI = SPACES
00682            PERFORM 2000-UPDATE-REQUEST THRU 2090-EXIT
00683         ELSE
00684            MOVE ER-3131             TO EMI-ERROR
00685            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00686            MOVE -1                  TO DPFENTRL
00687            GO TO 8200-SEND-DATAONLY.
00688
00689      GO TO 3200-DISPLAY-BATCH-REQUEST.
00690
00691      EJECT
00692
00693 ******************************************************************
00694 *                                                                *
00695 *              U P D A T E   R E Q U E S T                       *
00696 *                                                                *
00697 *    1.  VOID THE REQUEST BY RE-SETTING THE REQUEST DATE         *
00698 *        TO LOW-VALUES.                                          *
00699 *                                                                *
00700 *    2.  SPACE OUT THE REQUEST METHOD AND REQUESTOR.             *
00701 *                                                                *
00702 ******************************************************************
00703
00704  2000-UPDATE-REQUEST.
00705
00706       IF NOT MODIFY-CAP
00707          MOVE 'UPDATE'           TO SM-READ
00708          PERFORM 9995-SECURITY-VIOLATION
00709          MOVE ER-0070            TO EMI-ERROR
00710          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00711          GO TO 8100-SEND-INITIAL-MAP.
00712
00713      
      * EXEC CICS HANDLE CONDITION
00714 *        NOTFND  (2080-REQUEST-NOTFND)
00715 *    END-EXEC.
      *    MOVE '"$I                   ! # #00005747' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303035373437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00716
00717      
      * EXEC CICS READ
00718 *        SET     (ADDRESS OF AR-REQUEST-RECORD)
00719 *        DATASET (PI-FILE-ID)
00720 *        RIDFLD  (PI-ACCESS-KEY)
00721 *    END-EXEC.
      *    MOVE '&"S        E          (   #00005751' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373531' TO DFHEIV0(25:11)
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
           
00722
00723      IF PI-PROCESSOR-ID NOT = 'LGXX'
00724         IF RQ-STMT-DT NOT = LOW-VALUES
00725             GO TO 2070-REQUEST-DT-ERROR.
00726
00727      MOVE RQ-CONTROL-PRIMARY    TO ERRQST-KEY.
00728
00729      
      * EXEC CICS READ
00730 *        SET     (ADDRESS OF AR-REQUEST-RECORD)
00731 *        DATASET (FILE-ID-ERRQST)
00732 *        RIDFLD  (ERRQST-KEY)
00733 *        UPDATE
00734 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005763' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373633' TO DFHEIV0(25:11)
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
           
00735
00736      MOVE 'B'                    TO JP-RECORD-TYPE.
00737      MOVE AR-REQUEST-RECORD      TO JP-RECORD-AREA.
00738
00739      PERFORM 8400-LOG-JOURNAL-RECORD.
00740
00741      IF UPDATE-RQST-DT
00742          MOVE WS-SAVE-RQST-DT    TO RQ-REQUEST-DT.
00743
00744      IF UPDATE-STMT-DT
00745          MOVE WS-SAVE-STMT-DT    TO RQ-STMT-DT.
00746
00747      IF UPDATE-STATUS
00748          MOVE WS-SAVE-STATUS     TO RQ-STATUS.
00749
00750      MOVE SPACES                 TO RQ-PROCESSOR-ID.
00751      MOVE SPACE                  TO RQ-REQUEST-METHOD.
00752
00753      MOVE 'C'                    TO JP-RECORD-TYPE.
00754      MOVE AR-REQUEST-RECORD      TO JP-RECORD-AREA.
00755
00756      
      * EXEC CICS REWRITE
00757 *        DATASET (FILE-ID-ERRQST)
00758 *        FROM    (AR-REQUEST-RECORD)
00759 *    END-EXEC.
           MOVE LENGTH OF
            AR-REQUEST-RECORD
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005790' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERRQST, 
                 AR-REQUEST-RECORD, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00760
00761      PERFORM 8400-LOG-JOURNAL-RECORD.
00762
00763      GO TO 2090-EXIT.
00764
00765  2070-REQUEST-DT-ERROR.
00766
00767      MOVE ER-3136                TO EMI-ERROR.
00768      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00769
00770      IF PI-MAP-NAME = EL852D
00771         MOVE -1                  TO DPFENTRL
00772      ELSE
00773         MOVE -1                  TO BPFENTRL.
00774
00775      GO TO 8200-SEND-DATAONLY.
00776
00777  2080-REQUEST-NOTFND.
00778
00779      MOVE ER-2132                TO EMI-ERROR.
00780      IF PI-MAP-NAME = EL852D
00781         MOVE -1                  TO DPFENTRL
00782      ELSE
00783         MOVE -1                  TO BPFENTRL.
00784      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00785      GO TO 8200-SEND-DATAONLY.
00786
00787  2090-EXIT.
00788      EXIT.
00789      EJECT
00790
00791 ******************************************************************
00792 *                                                                *
00793 *           D I S P L A Y   R E Q U E S T S                      *
00794 *                                                                *
00795 ******************************************************************
00796
00797  3000-DISPLAY-REQUESTS.
00798
00799      
      * EXEC CICS HANDLE CONDITION
00800 *        NOTFND  (3080-REQUEST-NOTFND)
00801 *        ENDFILE (3080-REQUEST-NOTFND)
00802 *    END-EXEC.
      *    MOVE '"$I''                  ! $ #00005833' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303035383333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00803
00804      IF EIBTRNID = TRANS-EXJ6
00805         IF EIBAID = DFHPF1
00806            MOVE PI-REQUEST-KEY (10) TO PI-ACCESS-KEY
00807         ELSE
00808            MOVE PI-REQUEST-KEY (1)  TO PI-ACCESS-KEY.
00809
00810      
      * EXEC CICS STARTBR
00811 *        DATASET (PI-FILE-ID)
00812 *        RIDFLD  (PI-ACCESS-KEY)
00813 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005844' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-FILE-ID, 
                 PI-ACCESS-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00814
00815      MOVE SPACE                  TO PI-END-OF-FILE-SW
00816                                     PI-TOP-OF-FILE-SW.
00817
00818      IF PI-OPTION-ONE-SELECTED
00819         MOVE PI-SELECT-KEY       TO ERRQST-ALT-KEY2.
00820
00821      IF PI-OPTION-TWO-SELECTED
00822         MOVE PI-SELECT-KEY       TO ERRQST-ALT-KEY1.
00823
00824      IF PI-OPTION-THREE-SELECTED
00825         MOVE PI-SELECT-KEY       TO ERRQST-ALT-KEY4.
00826
00827      IF PI-OPTION-FOUR-SELECTED
00828         MOVE PI-SELECT-KEY       TO ERRQST-KEY.
00829
00830      MOVE 'N'                    TO FIRST-ERROR-SCAN-SW.
00831      MOVE +0                     TO WS-SUB1.
00832
00833  3010-READ-REQUEST-FILE.
00834
00835      
      * EXEC CICS HANDLE CONDITION
00836 *        ENDFILE (3060-END-OF-FILE)
00837 *    END-EXEC.
      *    MOVE '"$''                   ! % #00005869' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303035383639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00838
00839      
      * EXEC CICS READNEXT
00840 *        SET     (ADDRESS OF AR-REQUEST-RECORD)
00841 *        DATASET (PI-FILE-ID)
00842 *        RIDFLD  (PI-ACCESS-KEY)
00843 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005873' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383733' TO DFHEIV0(25:11)
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
           
00844
00845      IF EIBAID = DFHPF1
00846         IF PI-REQUEST-KEY (10) = PI-ACCESS-KEY
00847            GO TO 3010-READ-REQUEST-FILE.
00848
00849      IF EIBAID = DFHPF3
00850         IF PI-REQUEST-KEY (1) = PI-ACCESS-KEY
00851            GO TO 3010-READ-REQUEST-FILE.
00852
00853      IF RQ-COMPANY-CD NOT = PI-COMPANY-CD
00854         GO TO 3060-END-OF-FILE.
00855
00856      IF PI-OPTION-ONE-SELECTED
00857         MOVE RQ-CONTROL-BY-FIN-RESP TO WS-RQST-FIN-RESP-CNTL
00858         IF WS-RQST-FIN-RESP-CNTL GREATER THAN
00859               ERRQST-FIN-RESP-CONTROL
00860            GO TO 3060-END-OF-FILE.
00861
00862      IF PI-OPTION-TWO-SELECTED
00863         MOVE RQ-CONTROL-BY-ACCT-REF TO WS-RQST-ACCT-CONTROL
00864         IF WS-RQST-ACCT-CONTROL GREATER THAN ERRQST-ACCT-CONTROL
00865            GO TO 3060-END-OF-FILE.
00866
00867      IF PI-OPTION-TWO-SELECTED
00868         IF ERRQST-REFERENCE-A1 GREATER THAN LOW-VALUES
00869            IF  ERRQST-REFERENCE-A1 = RQ-REFERENCE-A1
00870                NEXT SENTENCE
00871            ELSE
00872                GO TO 3010-READ-REQUEST-FILE
00873         ELSE
00874            NEXT SENTENCE.
00875
00876      IF PI-OPTION-THREE-SELECTED
00877         IF ERRQST-SUMMARY-CODE = RQ-SUMMARY-CODE
00878            NEXT SENTENCE
00879         ELSE
00880            GO TO 3010-READ-REQUEST-FILE.
00881
00882      IF PI-OPTION-FOUR-SELECTED AND EIBAID = DFHPF3
00883          IF RQ-STATUS NOT EQUAL 'E' AND FIRST-ERROR-NOT-FOUND
00884              GO TO 3010-READ-REQUEST-FILE
00885          ELSE
00886              MOVE 'Y' TO FIRST-ERROR-SCAN-SW.
00887
00888      ADD +1                      TO WS-SUB1.
00889
00890      IF WS-SUB1 = +1
00891         IF PI-OPTION-ONE-SELECTED
00892            PERFORM 4100-READ-COMP-MASTER THRU 4190-EXIT
00893            MOVE WS-FIN-RESP-NAME TO BNAMEO
00894            MOVE RQ-CARRIER-A2    TO BCARRO
00895            MOVE RQ-GROUPING-A2   TO BGROUPO
00896            MOVE RQ-FIN-RESP-A2   TO BAGENTO.
00897
00898      IF WS-SUB1 = +1
00899         IF PI-OPTION-TWO-SELECTED
00900            PERFORM 4000-READ-ACCOUNT-MASTER THRU 4090-EXIT
00901            MOVE WS-AM-NAME          TO CNAMEO
00902            MOVE RQ-CARRIER-A1       TO CCARRO
00903            MOVE RQ-GROUPING-A1      TO CGROUPO
00904            MOVE RQ-STATE-A1         TO CSTO
00905            MOVE RQ-ACCOUNT-A1       TO CACCTO.
00906
00907      IF WS-SUB1 = +1
00908         IF PI-OPTION-THREE-SELECTED
00909            PERFORM 4200-READ-SUM-FILE THRU 4290-EXIT
00910            MOVE WS-SUM-NAME      TO BNAMEO
00911            MOVE 'SUM:'           TO BCARHDGO
00912            MOVE AL-SADOF         TO BAGTHDGA
00913            MOVE RQ-SUMMARY-CODE  TO BGRPHDGO.
00914
00915      IF WS-SUB1 = +1
00916         IF PI-OPTION-FOUR-SELECTED
00917            MOVE AL-SADOF         TO BCARHDGA
00918                                     BAGTHDGA
00919                                     BGRPHDGA
00920                                     BNAMHDGA
00921      IF WS-SUB1 = +1
00922         IF PI-OPTION-FOUR-SELECTED
00923             IF EIBAID = DFHPF3
00924                 MOVE 'SCAN ERRORED BATCHES'   TO BNAMEO
00925             ELSE
00926                 MOVE 'BATCH DISPLAY'          TO BNAMEO.
00927
00928      IF WS-SUB1 GREATER THAN +10
00929         GO TO 3070-DISPLAY-PROCESSED.
00930
00931      IF WS-SUB1 = +1
00932         MOVE PI-ACCESS-KEY       TO PI-1ST-REQUEST-KEY.
00933
00934      MOVE PI-ACCESS-KEY          TO PI-REQUEST-KEY (WS-SUB1).
00935
00936      MOVE WS-SUB1                TO AR-SEQ          (WS-SUB1).
00937
00938      IF NOT PI-OPTION-TWO-SELECTED
00939         MOVE RQ-CARRIER-A1       TO AR-CAR          (WS-SUB1)
00940         MOVE RQ-GROUPING-A1      TO AR-GRP          (WS-SUB1)
00941         MOVE RQ-STATE-A1         TO AR-ST           (WS-SUB1)
00942         MOVE RQ-ACCOUNT-A1       TO AR-ACCT         (WS-SUB1)
00943      ELSE
00944         MOVE RQ-CARRIER-A2       TO AR-CAR          (WS-SUB1)
00945         MOVE RQ-GROUPING-A2      TO AR-GRP          (WS-SUB1)
00946         MOVE RQ-FIN-RESP-A2      TO AR-ACCT         (WS-SUB1).
00947
00948      MOVE RQ-REFERENCE-A1        TO AR-REF          (WS-SUB1).
00949      MOVE RQ-BATCH-A1            TO AR-BATCH        (WS-SUB1).
00950      MOVE RQ-SUMMARY-CODE        TO AR-SUM          (WS-SUB1).
00951      MOVE RQ-STATUS              TO AR-STATUS       (WS-SUB1).
00952
00953      MOVE SPACE                  TO DC-OPTION-CODE.
00954      MOVE RQ-ENTRY-DT            TO DC-BIN-DATE-1.
00955      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
00956
00957      IF DATE-CONVERSION-ERROR
00958         MOVE 'DT ERR'            TO AR-ENTRY-DT (WS-SUB1)
00959      ELSE
00960         MOVE DC-GREG-DATE-1-MDY  TO AR-ENTRY-DT       (WS-SUB1).
00961
00962      IF RQ-REQUEST-DT GREATER THAN LOW-VALUES
00963         MOVE SPACE                  TO DC-OPTION-CODE
00964         MOVE RQ-REQUEST-DT          TO DC-BIN-DATE-1
00965         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
00966         IF DATE-CONVERSION-ERROR
00967            MOVE 'DT ERR'            TO AR-REQST-DT    (WS-SUB1)
00968         ELSE
00969            MOVE DC-GREG-DATE-1-MDY  TO AR-REQST-DT    (WS-SUB1)
00970      ELSE
00971         MOVE LOW-VALUES             TO AR-REQST-DT    (WS-SUB1).
00972
00973      MOVE AL-UANOF                 TO AR-REQST-DT-ATTRB (WS-SUB1)
00974                                       AR-STATUS-ATTRB (WS-SUB1).
00975
00976      IF PI-PROCESSOR-ID = 'LGXX'
00977          MOVE AL-UANOF             TO AR-STMT-DT-ATTRB (WS-SUB1).
00978 *                                     AR-STATUS-ATTRB (WS-SUB1).
00979
00980      IF RQ-STMT-DT GREATER THAN LOW-VALUES
00981         MOVE SPACE                  TO DC-OPTION-CODE
00982         MOVE RQ-STMT-DT             TO DC-BIN-DATE-1
00983         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
00984         IF DATE-CONVERSION-ERROR
00985            MOVE 'DT ERR'            TO AR-STMT-DT  (WS-SUB1)
00986         ELSE
00987            MOVE DC-GREG-DATE-1-MDY  TO AR-STMT-DT  (WS-SUB1)
00988      ELSE
00989         MOVE LOW-VALUES             TO AR-STMT-DT  (WS-SUB1).
00990
00991      GO TO 3010-READ-REQUEST-FILE.
00992
00993  3060-END-OF-FILE.
00994
00995      MOVE 'Y'                    TO PI-END-OF-FILE-SW.
00996      MOVE ER-2251                TO EMI-ERROR.
00997      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00998
00999  3070-DISPLAY-PROCESSED.
01000
01001      
      * EXEC CICS ENDBR
01002 *        DATASET (PI-FILE-ID)
01003 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006035' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01004
01005      MOVE -1                     TO AR-SELECT-LEN.
01006      GO TO 8100-SEND-INITIAL-MAP.
01007
01008  3080-REQUEST-NOTFND.
01009
01010      MOVE ER-2132                TO EMI-ERROR.
01011      MOVE -1                     TO AR-SELECT-LEN.
01012      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01013      GO TO 8200-SEND-DATAONLY.
01014
01015  3090-EXIT.
01016       EXIT.
01017
01018      EJECT
01019
01020 ******************************************************************
01021 *                                                                *
01022 *        D I S P L A Y   P R E V.   R E Q U E S T S              *
01023 *                                                                *
01024 ******************************************************************
01025
01026  3100-DISPLAY-PREV-REQUESTS.
01027
01028      
      * EXEC CICS HANDLE CONDITION
01029 *        NOTFND  (3180-REQUEST-NOTFND)
01030 *        ENDFILE (3180-REQUEST-NOTFND)
01031 *    END-EXEC.
      *    MOVE '"$I''                  ! & #00006062' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303036303632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01032
01033      MOVE PI-REQUEST-KEY (1)    TO PI-ACCESS-KEY.
01034
01035      
      * EXEC CICS STARTBR
01036 *        DATASET (PI-FILE-ID)
01037 *        RIDFLD  (PI-ACCESS-KEY)
01038 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006069' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-FILE-ID, 
                 PI-ACCESS-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01039
01040      MOVE 'N'                    TO FIRST-ERROR-SCAN-SW.
01041      MOVE SPACE                  TO PI-END-OF-FILE-SW
01042                                     PI-TOP-OF-FILE-SW.
01043
01044      IF PI-OPTION-ONE-SELECTED
01045         MOVE PI-SELECT-KEY       TO ERRQST-ALT-KEY2.
01046
01047      IF PI-OPTION-TWO-SELECTED
01048         MOVE PI-SELECT-KEY       TO ERRQST-ALT-KEY1.
01049
01050      IF PI-OPTION-THREE-SELECTED
01051         MOVE PI-SELECT-KEY       TO ERRQST-ALT-KEY4.
01052
01053      IF PI-OPTION-FOUR-SELECTED
01054         MOVE PI-SELECT-KEY       TO ERRQST-KEY.
01055
01056
01057      MOVE +11                    TO WS-SUB1.
01058
01059  3110-READ-REQUEST-FILE.
01060
01061      
      * EXEC CICS HANDLE CONDITION
01062 *        ENDFILE (3160-END-OF-FILE)
01063 *    END-EXEC.
      *    MOVE '"$''                   ! '' #00006095' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303036303935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01064
01065      
      * EXEC CICS READPREV
01066 *        SET     (ADDRESS OF AR-REQUEST-RECORD)
01067 *        DATASET (PI-FILE-ID)
01068 *        RIDFLD  (PI-ACCESS-KEY)
01069 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00006099' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303939' TO DFHEIV0(25:11)
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
           
01070
01071      IF RQ-COMPANY-CD NOT = PI-COMPANY-CD
01072         GO TO 3160-END-OF-FILE.
01073
01074      IF PI-REQUEST-KEY (1) = PI-ACCESS-KEY
01075         GO TO 3110-READ-REQUEST-FILE.
01076
01077      IF PI-OPTION-ONE-SELECTED
01078         MOVE RQ-CONTROL-BY-FIN-RESP TO WS-RQST-FIN-RESP-CNTL
01079         IF WS-RQST-FIN-RESP-CNTL LESS THAN
01080               ERRQST-FIN-RESP-CONTROL
01081            GO TO 3160-END-OF-FILE.
01082
01083      IF PI-OPTION-TWO-SELECTED
01084         MOVE RQ-CONTROL-BY-ACCT-REF TO WS-RQST-ACCT-CONTROL
01085         IF WS-RQST-ACCT-CONTROL LESS THAN ERRQST-ACCT-CONTROL
01086            GO TO 3160-END-OF-FILE.
01087
01088      IF PI-OPTION-TWO-SELECTED
01089         IF ERRQST-REFERENCE-A1 GREATER THAN LOW-VALUES
01090            IF ERRQST-REFERENCE-A1 = RQ-REFERENCE-A1
01091               NEXT SENTENCE
01092            ELSE
01093               GO TO 3010-READ-REQUEST-FILE.
01094
01095      IF PI-OPTION-THREE-SELECTED
01096         IF ERRQST-SUMMARY-CODE = RQ-SUMMARY-CODE
01097            NEXT SENTENCE
01098         ELSE
01099            GO TO 3110-READ-REQUEST-FILE.
01100
01101      IF PI-OPTION-FOUR-SELECTED AND EIBAID = DFHPF4
01102          IF RQ-STATUS NOT EQUAL 'E' AND FIRST-ERROR-NOT-FOUND
01103              GO TO 3110-READ-REQUEST-FILE
01104          ELSE
01105              MOVE 'Y' TO FIRST-ERROR-SCAN-SW.
01106
01107      SUBTRACT +1                 FROM WS-SUB1.
01108
01109      IF WS-SUB1 LESS THAN +1
01110         GO TO 3170-DISPLAY-PROCESSED.
01111
01112      IF WS-SUB1 = +10
01113         IF PI-OPTION-ONE-SELECTED
01114            PERFORM 4100-READ-COMP-MASTER THRU 4190-EXIT
01115            MOVE WS-FIN-RESP-NAME TO BNAMEO
01116            MOVE RQ-CARRIER-A2    TO BCARRO
01117            MOVE RQ-GROUPING-A2   TO BGROUPO
01118            MOVE RQ-FIN-RESP-A2   TO BAGENTO.
01119
01120      IF WS-SUB1 = +10
01121         IF PI-OPTION-TWO-SELECTED
01122            PERFORM 4000-READ-ACCOUNT-MASTER THRU 4090-EXIT
01123            MOVE WS-AM-NAME          TO CNAMEO
01124            MOVE RQ-CARRIER-A1       TO CCARRO
01125            MOVE RQ-GROUPING-A1      TO CGROUPO
01126            MOVE RQ-STATE-A1         TO CSTO
01127            MOVE RQ-ACCOUNT-A1       TO CACCTO.
01128
01129      IF WS-SUB1 = +10
01130         IF PI-OPTION-THREE-SELECTED
01131            PERFORM 4200-READ-SUM-FILE THRU 4290-EXIT
01132            MOVE WS-SUM-NAME      TO BNAMEO
01133            MOVE 'SUM:'           TO BCARHDGO
01134            MOVE AL-SADOF         TO BAGTHDGA
01135            MOVE RQ-SUMMARY-CODE  TO BGRPHDGO.
01136
01137      IF WS-SUB1 = +10
01138         IF PI-OPTION-FOUR-SELECTED
01139            MOVE AL-SADOF         TO BCARHDGA
01140                                     BAGTHDGA
01141                                     BGRPHDGA
01142                                     BNAMHDGA.
01143
01144      IF WS-SUB1 = +10
01145         IF PI-OPTION-FOUR-SELECTED
01146            IF EIBAID = DFHPF4
01147                MOVE 'SCAN ERRORED BATCHES'   TO BNAMEO
01148            ELSE
01149                MOVE 'BATCH DISPLAY'          TO BNAMEO.
01150
01151      MOVE PI-ACCESS-KEY          TO PI-REQUEST-KEY (WS-SUB1).
01152
01153      MOVE WS-SUB1                TO AR-SEQ    (WS-SUB1).
01154
01155      IF NOT PI-OPTION-TWO-SELECTED
01156         MOVE RQ-CARRIER-A1       TO AR-CAR    (WS-SUB1)
01157         MOVE RQ-GROUPING-A1      TO AR-GRP    (WS-SUB1)
01158         MOVE RQ-STATE-A1         TO AR-ST     (WS-SUB1)
01159         MOVE RQ-ACCOUNT-A1       TO AR-ACCT   (WS-SUB1)
01160      ELSE
01161         MOVE RQ-CARRIER-A2       TO AR-CAR    (WS-SUB1)
01162         MOVE RQ-GROUPING-A2      TO AR-GRP    (WS-SUB1)
01163         MOVE RQ-FIN-RESP-A2      TO AR-ACCT   (WS-SUB1).
01164
01165      MOVE RQ-REFERENCE-A1        TO AR-REF    (WS-SUB1).
01166      MOVE RQ-BATCH-A1            TO AR-BATCH  (WS-SUB1).
01167      MOVE RQ-SUMMARY-CODE        TO AR-SUM    (WS-SUB1).
01168      MOVE RQ-STATUS              TO AR-STATUS (WS-SUB1).
01169
01170      MOVE SPACE                  TO DC-OPTION-CODE.
01171      MOVE RQ-ENTRY-DT            TO DC-BIN-DATE-1.
01172      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT.
01173
01174      IF DATE-CONVERSION-ERROR
01175         MOVE 'DT ERR'            TO AR-ENTRY-DT (WS-SUB1)
01176      ELSE
01177         MOVE DC-GREG-DATE-1-MDY  TO AR-ENTRY-DT (WS-SUB1).
01178
01179      IF RQ-REQUEST-DT GREATER THAN LOW-VALUES
01180         MOVE SPACE                  TO DC-OPTION-CODE
01181         MOVE RQ-REQUEST-DT          TO DC-BIN-DATE-1
01182         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
01183         IF DATE-CONVERSION-ERROR
01184            MOVE 'DT ERR'            TO AR-REQST-DT (WS-SUB1)
01185         ELSE
01186            MOVE DC-GREG-DATE-1-MDY  TO AR-REQST-DT    (WS-SUB1)
01187      ELSE
01188         MOVE SPACES                 TO AR-REQST-DT    (WS-SUB1).
01189
01190
01191      MOVE AL-UANOF                 TO AR-REQST-DT-ATTRB (WS-SUB1).
01192
01193      IF PI-PROCESSOR-ID = 'LGXX'
01194          MOVE AL-UANOF             TO AR-STMT-DT-ATTRB (WS-SUB1)
01195                                       AR-STATUS-ATTRB (WS-SUB1).
01196
01197      IF RQ-STMT-DT GREATER THAN LOW-VALUES
01198         MOVE SPACE                  TO DC-OPTION-CODE
01199         MOVE RQ-STMT-DT             TO DC-BIN-DATE-1
01200         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
01201         IF DATE-CONVERSION-ERROR
01202            MOVE 'DT ERR'            TO AR-STMT-DT  (WS-SUB1)
01203         ELSE
01204            MOVE DC-GREG-DATE-1-MDY  TO AR-STMT-DT  (WS-SUB1)
01205      ELSE
01206         MOVE SPACES                 TO AR-STMT-DT  (WS-SUB1).
01207
01208
01209      GO TO 3110-READ-REQUEST-FILE.
01210
01211  3160-END-OF-FILE.
01212
01213      MOVE 'Y'                    TO PI-TOP-OF-FILE-SW.
01214      MOVE ER-2252                TO EMI-ERROR.
01215      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01216
01217  3170-DISPLAY-PROCESSED.
01218
01219      
      * EXEC CICS ENDBR
01220 *        DATASET (PI-FILE-ID)
01221 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006253' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01222
01223      MOVE -1                     TO AR-SELECT-LEN.
01224
01225      IF WS-SUB1 LESS +1
01226         GO TO 8100-SEND-INITIAL-MAP
01227      ELSE
01228         MOVE PI-REQUEST-KEY (1)  TO PI-ACCESS-KEY
01229         GO TO 8200-SEND-DATAONLY.
01230
01231  3180-REQUEST-NOTFND.
01232
01233      MOVE ER-2132                TO EMI-ERROR.
01234      MOVE -1                     TO AR-SELECT-LEN.
01235      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01236      GO TO 8200-SEND-DATAONLY.
01237
01238  3190-EXIT.
01239       EXIT.
01240
01241      EJECT
01242
01243 ******************************************************************
01244 *                                                                *
01245 *        D I S P L A Y   B A T C H   R E Q U E S T               *
01246 *                                                                *
01247 ******************************************************************
01248
01249  3200-DISPLAY-BATCH-REQUEST.
01250
01251      
      * EXEC CICS HANDLE CONDITION
01252 *        NOTFND  (3280-REQUEST-NOTFND)
01253 *        ENDFILE (3280-REQUEST-NOTFND)
01254 *    END-EXEC.
      *    MOVE '"$I''                  ! ( #00006285' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303036323835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01255
01256      
      * EXEC CICS READ
01257 *        SET     (ADDRESS OF AR-REQUEST-RECORD)
01258 *        DATASET (PI-FILE-ID)
01259 *        RIDFLD  (PI-ACCESS-KEY)
01260 *    END-EXEC.
      *    MOVE '&"S        E          (   #00006290' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323930' TO DFHEIV0(25:11)
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
           
01261
01262      MOVE RQ-ENTRY-BATCH         TO DBATCHO.
01263      MOVE RQ-CARRIER-A1          TO DCARO.
01264      MOVE RQ-GROUPING-A1         TO DGROUPO.
01265      MOVE RQ-STATE-A1            TO DSTO.
01266      MOVE RQ-ACCOUNT-A1          TO DACCTO.
01267      MOVE RQ-FIN-RESP-A2         TO DFRESPO.
01268      MOVE RQ-ACCT-AGENT-A2       TO DAGENTO.
01269      MOVE RQ-REFERENCE-A2        TO DREFO.
01270      MOVE RQ-SUMMARY-CODE        TO DSUMO.
01271      MOVE RQ-STATUS              TO DSTATUSO.
01272      MOVE RQ-PROCESSOR-ID        TO DRQSTRO.
01273      MOVE RQ-REQUEST-METHOD      TO DTYPEO.
01274
01275      IF RQ-ENTRY-DT GREATER THAN LOW-VALUES
01276         MOVE SPACE                  TO DC-OPTION-CODE
01277         MOVE RQ-ENTRY-DT            TO DC-BIN-DATE-1
01278         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
01279         IF DATE-CONVERSION-ERROR
01280            MOVE 'DT ERR'            TO DENTRYO
01281         ELSE
01282            MOVE DC-GREG-DATE-1-EDIT TO DENTRYO.
01283
01284      IF RQ-REQUEST-DT GREATER THAN LOW-VALUES
01285         MOVE SPACE                  TO DC-OPTION-CODE
01286         MOVE RQ-REQUEST-DT          TO DC-BIN-DATE-1
01287         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
01288         IF DATE-CONVERSION-ERROR
01289            MOVE 'DT ERR'            TO DREQSTO
01290         ELSE
01291            MOVE DC-GREG-DATE-1-EDIT TO DREQSTO.
01292
01293      IF RQ-STMT-DT GREATER THAN LOW-VALUES
01294         MOVE SPACE                  TO DC-OPTION-CODE
01295         MOVE RQ-STMT-DT             TO DC-BIN-DATE-1
01296         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
01297         IF DATE-CONVERSION-ERROR
01298            MOVE 'DT ERR'            TO DSTMTDTO
01299         ELSE
01300            MOVE DC-GREG-DATE-1-EDIT TO DSTMTDTO.
01301
01302      IF RQ-MO-END-DT GREATER THAN LOW-VALUES
01303         MOVE SPACE                  TO DC-OPTION-CODE
01304         MOVE RQ-MO-END-DT           TO DC-BIN-DATE-1
01305         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
01306         IF DATE-CONVERSION-ERROR
01307            MOVE 'DT ERR'            TO DEOMDTO
01308         ELSE
01309            MOVE DC-GREG-DATE-1-EDIT TO DEOMDTO.
01310
01311      IF RQ-REVERSAL-DT GREATER THAN LOW-VALUES
01312         MOVE SPACE                  TO DC-OPTION-CODE
01313         MOVE RQ-REVERSAL-DT         TO DC-BIN-DATE-1
01314         PERFORM 8500-DATE-CONVERT THRU 8500-EXIT
01315         IF DATE-CONVERSION-ERROR
01316            MOVE 'DT ERR'            TO DVOIDDTO
01317         ELSE
01318            MOVE DC-GREG-DATE-1-EDIT TO DVOIDDTO.
01319
01320       MOVE -1                       TO DPFENTRL.
01321       GO TO 8100-SEND-INITIAL-MAP.
01322
01323  3280-REQUEST-NOTFND.
01324
01325       MOVE SPACE                 TO PI-OPTION.
01326       MOVE ER-2132               TO EMI-ERROR.
01327       MOVE -1                    TO DPFENTRL.
01328       PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01329       GO TO 8100-SEND-INITIAL-MAP.
01330
01331      EJECT
01332 ******************************************************************
01333 *                                                                *
01334 *             R E A D   A C C O U N T   M A S T E R              *
01335 *                                                                *
01336 ******************************************************************
01337
01338  4000-READ-ACCOUNT-MASTER.
01339
01340      MOVE LOW-VALUES             TO ERACCT-KEY.
01341      MOVE RQ-COMPANY-CD          TO ERACCT-COMP-CD.
01342      MOVE RQ-CARRIER-A1          TO ERACCT-CARRIER.
01343      MOVE RQ-GROUPING-A1         TO ERACCT-GROUPING.
01344      MOVE RQ-STATE-A1            TO ERACCT-STATE
01345      MOVE RQ-ACCOUNT-A1          TO ERACCT-ACCOUNT.
01346      MOVE ERACCT-KEY             TO ERACCT-SAVE-KEY.
01347
01348      
      * EXEC CICS HANDLE CONDITION
01349 *        NOTFND   (4080-ACCOUNT-NOTFND)
01350 *    END-EXEC.
      *    MOVE '"$I                   ! ) #00006382' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303036333832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01351
01352      
      * EXEC CICS STARTBR
01353 *        DATASET (FILE-ID-ERACCT)
01354 *        RIDFLD  (ERACCT-KEY)
01355 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006386' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036333836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERACCT, 
                 ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01356
01357      MOVE 'Y'                    TO WS-BROWSE-STARTED-SW.
01358
01359      
      * EXEC CICS HANDLE CONDITION
01360 *        NOTFND   (4070-ACCOUNT-PROCESSED)
01361 *        ENDFILE  (4070-ACCOUNT-PROCESSED)
01362 *    END-EXEC.
      *    MOVE '"$I''                  ! * #00006393' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303036333933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01363
01364  4250-READ-LOOP.
01365
01366      
      * EXEC CICS READNEXT
01367 *        DATASET   (FILE-ID-ERACCT)
01368 *        SET       (ADDRESS OF ACCOUNT-MASTER)
01369 *        RIDFLD    (ERACCT-KEY)
01370 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006400' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERACCT, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01371
01372      MOVE AM-NAME                TO WS-AM-NAME.
01373
01374      IF ERACCT-KEY = ERACCT-SAVE-KEY
01375         GO TO 4250-READ-LOOP.
01376
01377  4070-ACCOUNT-PROCESSED.
01378
01379      IF WS-BROWSE-STARTED
01380         
      * EXEC CICS ENDBR
01381 *           DATASET (FILE-ID-ERACCT)
01382 *       END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006414' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERACCT, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01383
01384      GO TO 4090-EXIT.
01385
01386  4080-ACCOUNT-NOTFND.
01387
01388      MOVE ER-2210                TO EMI-ERROR.
01389      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01390
01391  4090-EXIT.
01392      EXIT.
01393
01394      EJECT
01395
01396
01397 ******************************************************************
01398 *                                                                *
01399 *       R E A D   C O M P E N S A T I O N   M A S T E R          *
01400 *                                                                *
01401 ******************************************************************
01402
01403  4100-READ-COMP-MASTER.
01404
01405      
      * EXEC CICS HANDLE CONDITION
01406 *        NOTFND   (4170-COMP-MASTER-NOTFND)
01407 *        END-EXEC.
      *    MOVE '"$I                   ! + #00006439' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303036343339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01408
01409      MOVE RQ-COMPANY-CD          TO ERCOMP-COMP-CD.
01410
01411      IF PI-ZERO-CARRIER OR
01412         PI-ZERO-CAR-GROUP
01413         MOVE ZEROS               TO ERCOMP-CARRIER
01414      ELSE
01415         MOVE RQ-CARRIER-A2       TO ERCOMP-CARRIER.
01416
01417      IF PI-ZERO-GROUPING OR
01418         PI-ZERO-CAR-GROUP
01419         MOVE ZEROS               TO ERCOMP-GROUPING
01420      ELSE
01421         MOVE RQ-GROUPING-A2      TO ERCOMP-GROUPING.
01422
01423      MOVE RQ-FIN-RESP-A2         TO ERCOMP-FIN-RESP.
01424      MOVE RQ-ACCT-AGENT-A2       TO ERCOMP-ACCOUNT.
01425      MOVE 'A'                    TO ERCOMP-RECORD-TYPE.
01426
01427      
      * EXEC CICS READ
01428 *         DATASET   (FILE-ID-ERCOMP)
01429 *         SET       (ADDRESS OF COMPENSATION-MASTER)
01430 *         RIDFLD    (ERCOMP-KEY)
01431 *    END-EXEC.
      *    MOVE '&"S        E          (   #00006461' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343631' TO DFHEIV0(25:11)
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
           
01432
01433      MOVE CO-MAIL-NAME           TO WS-FIN-RESP-NAME.
01434
01435      GO TO 4190-EXIT.
01436
01437  4170-COMP-MASTER-NOTFND.
01438
01439      MOVE ER-2800                TO EMI-ERROR.
01440      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01441
01442  4190-EXIT.
01443      EXIT.
01444
01445      EJECT
01446
01447 ******************************************************************
01448 *                                                                *
01449 *           R E A D   S U M M A R Y   H E A D E R                *
01450 *                                                                *
01451 ******************************************************************
01452
01453  4200-READ-SUM-FILE.
01454
01455      MOVE LOW-VALUES             TO ERSUMM-KEY.
01456      MOVE RQ-COMPANY-CD          TO ERSUMM-COMPANY-CD.
01457      MOVE RQ-SUMMARY-CODE        TO ERSUMM-SUMMARY.
01458
01459      
      * EXEC CICS HANDLE CONDITION
01460 *        NOTFND   (4280-SUMMARY-NOTFND)
01461 *    END-EXEC.
      *    MOVE '"$I                   ! , #00006493' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303036343933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01462
01463      
      * EXEC CICS READ
01464 *        DATASET   (FILE-ID-ERSUMM)
01465 *        SET       (ADDRESS OF SUMM-CROSS-REFERENCE)
01466 *        RIDFLD    (ERSUMM-KEY)
01467 *    END-EXEC.
      *    MOVE '&"S        E          (   #00006497' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERSUMM, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERSUMM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF SUMM-CROSS-REFERENCE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01468
01469      MOVE SX-SUMM-OR-AGT-NAME    TO WS-SUM-NAME.
01470
01471      GO TO 4290-EXIT.
01472
01473  4280-SUMMARY-NOTFND.
01474
01475      MOVE ER-3134                TO EMI-ERROR.
01476      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01477      MOVE SPACES                 TO WS-SUM-NAME.
01478
01479  4290-EXIT.
01480      EXIT.
01481
01482      EJECT
01483
01484 ******************************************************************
01485 *                                                                *
01486 *   S E L E C T   B A T C H    T O    E D I T                    *
01487 *                                                                *
01488 ******************************************************************
01489
01490  5000-EDIT-BATCH.
01491
01492      IF AR-SELECT NOT NUMERIC
01493         MOVE ER-3132             TO EMI-ERROR
01494         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01495         GO TO 8200-SEND-DATAONLY.
01496
01497      IF AR-SELECT GREATER THAN +0 AND LESS THAN +11
01498         NEXT SENTENCE
01499      ELSE
01500         MOVE ER-3132             TO EMI-ERROR
01501         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01502         GO TO 8200-SEND-DATAONLY.
01503
01504      MOVE LOW-VALUES             TO WS-PREV-PNDB.
01505      MOVE SPACES                 TO WS-SAVE-AM-CONTROLS
01506                                     WS-SAVE-SUMMARY.
01507      MOVE 'N'                    TO WS-AGT-ERROR-SW
01508                                     WS-FIN-RESP-ERROR-SW
01509                                     WS-AGT-COMP-ERROR-SW
01510                                     WS-FIN-RESP-COMP-ERROR-SW
01511                                     WS-NO-ACCOUNT-MASTER-SW.
01512      MOVE 'Y'                    TO WS-NEW-ACCT-MAST-SW
01513                                     WS-ACCOUNT-MASTER-SW.
01514
01515      MOVE LOW-VALUES             TO ERPNDB-KEY.
01516      MOVE PI-REQUEST-KEY (AR-SELECT)
01517                                  TO WS-ACCESS-ERPNDB.
01518      MOVE +1                     TO WS-ACCESS-SEQ.
01519      MOVE ZEROS                  TO WS-ACCESS-CHG-SEQ.
01520      MOVE WS-ACCESS-ERPNDB       TO ERPNDB-KEY.
01521
01522      
      * EXEC CICS HANDLE CONDITION
01523 *        NOTFND (5100-NOT-FOUND)
01524 *        ENDFILE (5100-NOT-FOUND)
01525 *    END-EXEC.
      *    MOVE '"$I''                  ! - #00006556' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303036353536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01526
01527      
      * EXEC CICS STARTBR
01528 *        DATASET (FILE-ID-ERPNDB)
01529 *        RIDFLD  (ERPNDB-KEY)
01530 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006561' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01531
01532  5010-READ-PNDB.
01533
01534      
      * EXEC CICS READNEXT
01535 *        DATASET   (FILE-ID-ERPNDB)
01536 *        SET       (ADDRESS OF PENDING-BUSINESS)
01537 *        RIDFLD    (ERPNDB-KEY)
01538 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006568' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01539
01540      IF WS-PREV-PNDB = LOW-VALUES
01541          MOVE PB-CONTROL-PRIMARY TO WS-PREV-PNDB
01542          IF (WS-PREV-COMPANY NOT = PB-COMPANY-CD)  OR
01543             (WS-PREV-BATCH   NOT = PB-ENTRY-BATCH)
01544              GO TO 5100-NOT-FOUND.
01545
01546      IF (PB-COMPANY-CD NOT = WS-PREV-COMPANY) OR
01547         (PB-ENTRY-BATCH NOT = WS-PREV-BATCH)
01548          PERFORM 6000-UPDATE-REQUEST  THRU  6999-EXIT
01549          GO TO 5999-EXIT.
01550
01551      IF PB-MAILING-DATA
01552          GO TO 5010-READ-PNDB.
01553
01554      IF PB-ISSUE
01555          IF PB-I-ENTRY-STATUS = '5' OR '9' OR 'D' OR 'V'
122002                             OR 'M'
01556              GO TO 5010-READ-PNDB.
01557
01558      IF PB-CANCELLATION
01559          IF PB-I-ENTRY-STATUS = '9' OR 'D' OR 'V'
01560              GO TO 5010-READ-PNDB.
01561
01562      IF PB-BATCH-TRAILER
01563          PERFORM 6000-UPDATE-REQUEST  THRU  6999-EXIT
01564          GO TO 5999-EXIT.
01565
01566      MOVE 'N'                    TO WS-TRAILER-ONLY-SW.
01567
01568      IF WS-NEW-ACCT-MAST
01569          PERFORM 7000-ACCOUNT-MATCH  THRU  7999-EXIT
01570              GO TO 5010-READ-PNDB.
01571
01572      IF NOT WS-ACCOUNT-MASTER
01573          PERFORM 7000-ACCOUNT-MATCH  THRU  7999-EXIT
01574              GO TO 5010-READ-PNDB.
01575
01576      IF (PB-CERT-EFF-DT LESS THAN WS-SAVE-AM-EFF-DT) OR
01577         (PB-CERT-EFF-DT GREATER THAN WS-SAVE-AM-EXP-DT)
01578          PERFORM 7000-ACCOUNT-MATCH  THRU  7999-EXIT
01579              GO TO 5010-READ-PNDB.
01580
01581      GO TO 5010-READ-PNDB.
01582
01583  5100-NOT-FOUND.
01584
01585      MOVE ER-2242                TO EMI-ERROR.
01586      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01587      GO TO 8200-SEND-DATAONLY.
01588
01589  5999-EXIT.
01590
01591      EJECT
01592
01593 ******************************************************************
01594 *                                                                *
01595 *   E D I T   B A T C H    A N D   R E B U I D     R E Q U E S T *
01596 *                                                                *
01597 ******************************************************************
01598
01599  6000-UPDATE-REQUEST.
01600
01601     
      * EXEC CICS ENDBR
01602 *       DATASET (FILE-ID-ERPNDB)
01603 *   END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006636' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERPNDB, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01604
01605      IF WS-TRAILER-ONLY
01606          MOVE ER-2211            TO EMI-ERROR
01607          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01608          GO TO 8200-SEND-DATAONLY.
01609
01610      IF WS-NO-ACCOUNT-MASTER   OR
01611         WS-AGT-ERROR           OR
01612         WS-FIN-RESP-ERROR
01613          GO TO 6400-UPDATE.
01614
01615      
      * EXEC CICS HANDLE CONDITION
01616 *        NOTFND   (6100-NO-ACCT-AGENT-COMP)
01617 *        END-EXEC.
      *    MOVE '"$I                   ! . #00006650' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303036363530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01618
01619      MOVE PI-COMPANY-CD          TO ERCOMP-COMP-CD.
01620
01621      IF PI-ZERO-CARRIER OR
01622         PI-ZERO-CAR-GROUP
01623         MOVE ZEROS               TO ERCOMP-CARRIER
01624      ELSE
01625         MOVE WS-SAVE-CARRIER     TO ERCOMP-CARRIER.
01626
01627      IF PI-ZERO-GROUPING OR
01628         PI-ZERO-CAR-GROUP
01629         MOVE ZEROS               TO ERCOMP-GROUPING
01630      ELSE
01631         MOVE WS-SAVE-GROUPING    TO ERCOMP-GROUPING.
01632
01633      MOVE WS-SAVE-FIN-RESP       TO ERCOMP-FIN-RESP.
01634      MOVE WS-SAVE-ACCT-AGENT     TO ERCOMP-ACCOUNT.
01635      MOVE 'A'                    TO ERCOMP-RECORD-TYPE.
01636
01637      
      * EXEC CICS READ
01638 *         DATASET   (FILE-ID-ERCOMP)
01639 *         SET       (ADDRESS OF COMPENSATION-MASTER)
01640 *         RIDFLD    (ERCOMP-KEY)
01641 *    END-EXEC.
      *    MOVE '&"S        E          (   #00006672' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363732' TO DFHEIV0(25:11)
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
           
01642
01643      MOVE CO-AR-SUMMARY-CODE     TO WS-SAVE-SUMMARY.
01644      GO TO 6200-TEST-FIN-RESP-COMP.
01645
01646  6100-NO-ACCT-AGENT-COMP.
01647
01648      MOVE 'Y'                    TO WS-AGT-COMP-ERROR-SW.
01649
01650  6200-TEST-FIN-RESP-COMP.
01651
01652      IF WS-SAVE-ACCT-AGENT = WS-SAVE-FIN-RESP
01653          GO TO 6400-UPDATE.
01654
01655      MOVE LOW-VALUES             TO CO-RESP-NO.
01656      MOVE 'G'                    TO CO-TYPE.
01657
01658      
      * EXEC CICS HANDLE CONDITION
01659 *        NOTFND   (6300-NO-FIN-RESP-COMP)
01660 *        END-EXEC.
      *    MOVE '"$I                   ! / #00006693' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2F20233030303036363933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01661
01662      
      * EXEC CICS READ
01663 *         DATASET   (FILE-ID-ERCOMP)
01664 *         SET       (ADDRESS OF COMPENSATION-MASTER)
01665 *         RIDFLD    (ERCOMP-KEY)
01666 *    END-EXEC.
      *    MOVE '&"S        E          (   #00006697' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363937' TO DFHEIV0(25:11)
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
           
01667
01668      GO TO 6400-UPDATE.
01669
01670  6300-NO-FIN-RESP-COMP.
01671
01672      MOVE 'Y'                    TO WS-FIN-RESP-COMP-ERROR-SW.
01673
01674  6400-UPDATE.
01675
01676      MOVE PI-REQUEST-KEY (AR-SELECT)
01677                                  TO ERRQST-KEY.
01678
01679      
      * EXEC CICS READ
01680 *        SET     (ADDRESS OF AR-REQUEST-RECORD)
01681 *        DATASET (FILE-ID-ERRQST)
01682 *        RIDFLD  (ERRQST-KEY)
01683 *        UPDATE
01684 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00006714' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373134' TO DFHEIV0(25:11)
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
           
01685
01686      IF NOT WS-NO-ACCOUNT-MASTER   AND
01687         NOT WS-AGT-ERROR           AND
01688         NOT WS-FIN-RESP-ERROR      AND
01689         NOT WS-AGT-COMP-ERROR      AND
01690         NOT WS-FIN-RESP-COMP-ERROR
01691          MOVE ' '                TO RQ-STATUS
01692                                     AR-STATUS (AR-SELECT)
01693      ELSE
01694          MOVE 'E'                TO RQ-STATUS
01695                                     AR-STATUS (AR-SELECT).
01696
01697      MOVE WS-SAVE-SUMMARY        TO RQ-SUMMARY-CODE.
01698
01699      IF WS-AGT-ERROR OR WS-NO-ACCOUNT-MASTER
01700          MOVE 'UNKNOWN'          TO RQ-ACCT-AGENT-A2
01701                                     RQ-ACCT-AGENT-A3
01702      ELSE
01703          MOVE WS-SAVE-ACCT-AGENT TO RQ-ACCT-AGENT-A2
01704                                     RQ-ACCT-AGENT-A3.
01705
01706      IF WS-FIN-RESP-ERROR OR WS-NO-ACCOUNT-MASTER
01707          MOVE 'UNKNOWN'          TO RQ-FIN-RESP-A2
01708      ELSE
01709          MOVE WS-SAVE-FIN-RESP   TO RQ-FIN-RESP-A2.
01710
01711      
      * EXEC CICS REWRITE
01712 *        DATASET (FILE-ID-ERRQST)
01713 *        FROM    (AR-REQUEST-RECORD)
01714 *    END-EXEC.
           MOVE LENGTH OF
            AR-REQUEST-RECORD
             TO DFHEIV11
      *    MOVE '&& L                  %   #00006746' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERRQST, 
                 AR-REQUEST-RECORD, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01715
01716      MOVE ER-0000                TO EMI-ERROR.
01717      MOVE -1                     TO AR-SELECT-LEN.
01718      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01719
01720  6999-EXIT.
01721      EXIT.
01722
01723      EJECT
01724
01725 ******************************************************************
01726 *                                                                *
01727 *   M A T C H   T O   A C C O U N T   M A S T E R                *
01728 *                                                                *
01729 ******************************************************************
01730
01731  7000-ACCOUNT-MATCH.
01732
01733      MOVE LOW-VALUES             TO ERACCT-KEY.
01734      MOVE PB-COMPANY-CD          TO ERACCT-COMP-CD.
01735      MOVE PB-SV-CARRIER          TO ERACCT-CARRIER.
01736      MOVE PB-SV-GROUPING         TO ERACCT-GROUPING.
01737      MOVE PB-SV-STATE            TO ERACCT-STATE.
01738      MOVE PB-ACCOUNT             TO ERACCT-ACCOUNT.
01739      MOVE PB-CERT-EFF-DT         TO ACCT-EXP-DATE.
01740
01741      
      * EXEC CICS HANDLE CONDITION
01742 *        ENDFILE  (7100-NO-ACCOUNT-MASTER)
01743 *    END-EXEC.
      *    MOVE '"$''                   ! 0 #00006776' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3020233030303036373736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01744
01745      
      * EXEC CICS STARTBR
01746 *        DATASET (FILE-ID-ERACCT)
01747 *        RIDFLD  (ERACCT-KEY)
01748 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006780' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERACCT, 
                 ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01749
01750  7050-ACCOUNT-MASTER-READNEXT.
01751
01752      
      * EXEC CICS READNEXT
01753 *        DATASET   (FILE-ID-ERACCT)
01754 *        SET       (ADDRESS OF ACCOUNT-MASTER)
01755 *        RIDFLD    (ERACCT-KEY)
01756 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006787' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERACCT, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01757
01758      IF PB-COMPANY-CD NOT = AM-COMPANY-CD-A1 OR
01759         PB-CARRIER    NOT = AM-VG-CARRIER    OR
01760         PB-GROUPING   NOT = AM-VG-GROUPING   OR
01761         PB-STATE      NOT = AM-VG-STATE      OR
01762         PB-ACCOUNT    NOT = AM-VG-ACCOUNT
01763          MOVE 'N'                TO WS-ACCOUNT-MASTER-SW
01764          GO TO 7900-ENDBR.
01765
01766      IF (PB-CERT-EFF-DT = AM-EFFECTIVE-DT OR
01767          PB-CERT-EFF-DT GREATER THAN AM-EFFECTIVE-DT) AND
01768         PB-CERT-EFF-DT LESS THAN AM-EXPIRATION-DT
01769          NEXT SENTENCE
01770      ELSE
01771          GO TO 7050-ACCOUNT-MASTER-READNEXT.
01772
01773      IF WS-NEW-ACCT-MAST
01774          MOVE AM-CARRIER         TO WS-SAVE-CARRIER
01775          MOVE AM-GROUPING        TO WS-SAVE-GROUPING
01776          MOVE AM-STATE           TO WS-SAVE-STATE
01777          MOVE AM-AGT (01)        TO WS-SAVE-ACCT-AGENT
01778          MOVE AM-AGT (AM-REMIT-TO)
01779                                  TO WS-SAVE-FIN-RESP
01780          MOVE AM-EFFECTIVE-DT    TO WS-SAVE-AM-EFF-DT
01781          MOVE AM-EXPIRATION-DT   TO WS-SAVE-AM-EXP-DT
01782          MOVE 'N'                TO WS-NEW-ACCT-MAST-SW
01783          GO TO 7900-ENDBR.
01784
01785      IF AM-AGT (01) NOT = WS-SAVE-ACCT-AGENT
01786          MOVE SPACES             TO WS-SAVE-ACCT-AGENT
01787          MOVE 'Y'                TO WS-AGT-ERROR-SW.
01788
01789      IF AM-AGT (AM-REMIT-TO) NOT = WS-SAVE-FIN-RESP
01790          MOVE SPACES             TO WS-SAVE-FIN-RESP
01791          MOVE 'Y'                TO WS-FIN-RESP-ERROR-SW.
01792
01793      GO TO 7900-ENDBR.
01794
01795  7100-NO-ACCOUNT-MASTER.
01796
01797      MOVE 'Y'                    TO WS-NO-ACCOUNT-MASTER-SW.
01798      MOVE SPACES                 TO WS-SAVE-ACCT-AGENT
01799                                     WS-SAVE-FIN-RESP.
01800
01801  7900-ENDBR.
01802      
      * EXEC CICS ENDBR
01803 *        DATASET (FILE-ID-ERACCT)
01804 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006837' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 FILE-ID-ERACCT, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01805
01806  7999-EXIT.
01807      EXIT.
01808
01809      EJECT
01810
01811 ******************************************************************
01812 *                                                                *
01813 *            S  E N D    I N I T I A L   M A P                   *
01814 *                                                                *
01815 ******************************************************************
01816
01817  8100-SEND-INITIAL-MAP.
01818
01819      MOVE EIBTIME                    TO TIME-IN.
01820
01821      IF PI-MAP-NAME = EL852B
01822         MOVE EMI-MESSAGE-AREA (1)    TO BERMSGO
01823         MOVE WS-CURRENT-DT           TO BDATEO
01824         MOVE TIME-OUT                TO BTIMEO
01825         MOVE -1                      TO AR-SELECT-LEN.
01826
01827      IF PI-MAP-NAME = EL852C
01828         MOVE EMI-MESSAGE-AREA (1)    TO CERMSGO
01829         MOVE WS-CURRENT-DT           TO CDATEO
01830         MOVE TIME-OUT                TO CTIMEO
01831         MOVE -1                      TO AR-SELECT-LEN.
01832
01833      IF PI-MAP-NAME = EL852D
01834         MOVE EMI-MESSAGE-AREA (1)    TO DERMSGO
01835         MOVE WS-CURRENT-DT           TO DDATEO
01836         MOVE TIME-OUT                TO DTIMEO
01837         MOVE -1                      TO DPFENTRL.
01838
01839
01840      
      * EXEC CICS SEND
01841 *        MAP      (PI-MAP-NAME)
01842 *        MAPSET   (MAPSET-EL8521S)
01843 *        FROM     (EL852BI)
01844 *        ERASE
01845 *        CURSOR
01846 *    END-EXEC.
           MOVE LENGTH OF
            EL852BI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00006875' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 EL852BI, 
                 DFHEIV12, 
                 MAPSET-EL8521S, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01847
01848      GO TO 9100-RETURN-TRAN.
01849
01850      EJECT
01851
01852 ******************************************************************
01853 *                                                                *
01854 *              S E N D    D A T A O N L Y                        *
01855 *                                                                *
01856 ******************************************************************
01857
01858  8200-SEND-DATAONLY.
01859
01860      MOVE EIBTIME                TO TIME-IN.
01861
01862      IF PI-MAP-NAME = EL852B
01863         MOVE EMI-MESSAGE-AREA (1)    TO BERMSGO
01864         MOVE WS-CURRENT-DT           TO BDATEO
01865         MOVE TIME-OUT                TO BTIMEO
01866         MOVE -1                      TO AR-SELECT-LEN.
01867
01868      IF PI-MAP-NAME = EL852C
01869         MOVE EMI-MESSAGE-AREA (1)    TO CERMSGO
01870         MOVE WS-CURRENT-DT           TO CDATEO
01871         MOVE TIME-OUT                TO CTIMEO
01872         MOVE -1                      TO AR-SELECT-LEN.
01873
01874      IF PI-MAP-NAME = EL852D
01875         MOVE EMI-MESSAGE-AREA (1)    TO DERMSGO
01876         MOVE WS-CURRENT-DT           TO DDATEO
01877         MOVE TIME-OUT                TO DTIMEO
01878         MOVE -1                      TO DPFENTRL.
01879
01880
01881      
      * EXEC CICS SEND
01882 *         MAP      (PI-MAP-NAME)
01883 *         MAPSET   (MAPSET-EL8521S)
01884 *         FROM     (EL852BI)
01885 *         DATAONLY
01886 *         CURSOR
01887 *    END-EXEC.
           MOVE LENGTH OF
            EL852BI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00006916' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-MAP-NAME, 
                 EL852BI, 
                 DFHEIV12, 
                 MAPSET-EL8521S, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01888
01889      GO TO 9100-RETURN-TRAN.
01890
01891      EJECT
01892
01893  8300-SEND-TEXT.
01894      
      * EXEC CICS SEND TEXT
01895 *        FROM     (LOGOFF-TEXT)
01896 *        LENGTH   (LOGOFF-LENGTH)
01897 *        ERASE
01898 *        FREEKB
01899 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00006929' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393239' TO DFHEIV0(25:11)
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
           
01900
01901      
      * EXEC CICS RETURN
01902 *    END-EXEC.
      *    MOVE '.(                    &   #00006936' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01903
01904
01905  8400-LOG-JOURNAL-RECORD.
01906      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.
01907      MOVE THIS-PGM                TO JP-PROGRAM-ID.
01908
01909 *    EXEC CICS JOURNAL
01910 *        JFILEID     (PI-JOURNAL-FILE-ID)
01911 *        JTYPEID     ('EL')
01912 *        FROM        (JOURNAL-RECORD)
01913 *        LENGTH      (WS-JOURNAL-RECORD-LENGTH)
01914 *        END-EXEC.
01915
01916  8500-DATE-CONVERT.
01917      
      * EXEC CICS LINK
01918 *        PROGRAM  (LINK-ELDATCV)
01919 *        COMMAREA (DATE-CONVERSION-DATA)
01920 *        LENGTH   (DC-COMM-LENGTH)
01921 *    END-EXEC.
      *    MOVE '."C                   ''   #00006952' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LINK-ELDATCV, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01922
01923  8500-EXIT.
01924      EXIT.
01925
01926      EJECT
01927
01928  8800-UNAUTHORIZED-ACCESS.
01929      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
01930      GO TO 8300-SEND-TEXT.
01931
01932  8810-PF23.
01933      MOVE EIBAID                 TO PI-ENTRY-CD-1.
01934      MOVE XCTL-EL005             TO PGM-NAME.
01935      GO TO 9300-XCTL.
01936
01937  9200-RETURN-MAIN-MENU.
01938      MOVE XCTL-EL626             TO PGM-NAME.
01939      GO TO 9300-XCTL.
01940
01941  9000-RETURN-CICS.
01942      
      * EXEC CICS RETURN
01943 *    END-EXEC.
      *    MOVE '.(                    &   #00006977' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01944
01945  9100-RETURN-TRAN.
01946
01947      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
01948      MOVE '852A'                 TO PI-CURRENT-SCREEN-NO.
01949
01950      
      * EXEC CICS RETURN
01951 *        TRANSID    (TRANS-EXJ6)
01952 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
01953 *        LENGTH     (PI-COMM-LENGTH)
01954 *    END-EXEC.
      *    MOVE '.(CT                  &   #00006985' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-EXJ6, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01955
01956  9300-XCTL.
01957      
      * EXEC CICS XCTL
01958 *        PROGRAM    (PGM-NAME)
01959 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
01960 *        LENGTH     (PI-COMM-LENGTH)
01961 *    END-EXEC.
      *    MOVE '.$C                   $   #00006992' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01962
01963  9400-CLEAR.
01964      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME
01965      GO TO 9300-XCTL.
01966
01967  9500-PF12.
01968      MOVE XCTL-EL010             TO PGM-NAME.
01969      GO TO 9300-XCTL.
01970
01971  9600-PGMID-ERROR.
01972
01973      
      * EXEC CICS HANDLE CONDITION
01974 *        PGMIDERR    (8300-SEND-TEXT)
01975 *    END-EXEC.
      *    MOVE '"$L                   ! 1 #00007008' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3120233030303037303038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01976
01977      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
01978      MOVE ' '                    TO PI-ENTRY-CD-1.
01979      MOVE XCTL-EL005             TO PGM-NAME.
01980      MOVE PGM-NAME               TO LOGOFF-PGM.
01981      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
01982      GO TO 9300-XCTL.
01983
01984  9900-ERROR-FORMAT.
01985
01986      IF NOT EMI-ERRORS-COMPLETE
01987          MOVE LINK-EL001         TO PGM-NAME
01988          
      * EXEC CICS LINK
01989 *            PROGRAM    (PGM-NAME)
01990 *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
01991 *            LENGTH     (EMI-COMM-LENGTH)
01992 *        END-EXEC.
      *    MOVE '."C                   ''   #00007023' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01993
01994  9900-EXIT.
01995      EXIT.
01996
01997  9990-ABEND.
01998      MOVE LINK-EL004             TO PGM-NAME.
01999      MOVE DFHEIBLK               TO EMI-LINE1.
02000      
      * EXEC CICS LINK
02001 *        PROGRAM   (PGM-NAME)
02002 *        COMMAREA  (EMI-LINE1)
02003 *        LENGTH    (72)
02004 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00007035' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02005
02006      IF PI-MAP-NAME = EL852B
02007         MOVE -1                  TO BPFENTRL.
02008
02009      IF PI-MAP-NAME = EL852C
02010         MOVE -1                  TO CPFENTRL.
02011
02012      IF PI-MAP-NAME = EL852D
02013         MOVE -1                  TO DPFENTRL.
02014
02015      GO TO 8200-SEND-DATAONLY.
02016
02017      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL8521' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
02018
02019      EJECT
02020
02021  9995-SECURITY-VIOLATION.
02022 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00007074' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303734' TO DFHEIV0(25:11)
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
02023
02024  9995-EXIT.
02025      EXIT.
02026

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL8521' TO DFHEIV1
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
               GO TO 2080-REQUEST-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 3080-REQUEST-NOTFND,
                     3080-REQUEST-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 3060-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 3180-REQUEST-NOTFND,
                     3180-REQUEST-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 3160-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 3280-REQUEST-NOTFND,
                     3280-REQUEST-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 4080-ACCOUNT-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 4070-ACCOUNT-PROCESSED,
                     4070-ACCOUNT-PROCESSED
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 4170-COMP-MASTER-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 4280-SUMMARY-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 5100-NOT-FOUND,
                     5100-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 6100-NO-ACCT-AGENT-COMP
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 6300-NO-FIN-RESP-COMP
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 7100-NO-ACCOUNT-MASTER
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL8521' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
