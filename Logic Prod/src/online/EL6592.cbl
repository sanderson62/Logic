00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL6592.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/14/96 12:02:41.
00007 *                            VMOD=2.010.
00008 *
00008 *
00009 *AUTHOR.        LOGIC INC.
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
00025 *        THIS PROGRAM PROVIDES THE BROWSE NECESSARY FOR THE
00026 *    ACCOUNT MASTER (ACCOUNT NAME AND/OR MAIL TO NAME), COMPEN-
00027 *    SATION MASTER (ACCOUNT NAME AND/OR MAIL TO NAME), AND THE
00028 *    REINSURANCE COMPANY NAME (WITH LEVEL NUMBERS) LOOK-UP.
00029
00030 *    SCREENS     - EL659B - ACCOUNT/COMPANY NAME LOOK-UP
00031
00032 *    ENTERED BY  - EL659 - ACCOUNT/COMPANY NAME QUALIFICATION
00033
00034 *    EXIT TO     - EL659 - ACCOUNT/COMPANY NAME QUALIFICATION
00035 *                  EL650 - ACCOUNT MAINTENANCE
00036 *                  EL652 - COMPENSATION MAINTENANCE
00037 *                  EL651 - REINSURANCE TABLE MAINTENANCE
00038
00039 *    INPUT FILE  - ERNAME - ACCOUNT/COMPANY NAME XREF FILE
00040
00041 *    OUTPUT FILE - NONE
00042
00043 *    COMMAREA    - PASSED.  IF AN ACCOUNT/COMPANY IS SELECTED,
00044 *                  THE CONTROL OF THAT ACCOUNT/COMPANY IS PLACED
00045 *                  IN THE APPROPRIATE FIELDS OF THE COMMAAREA FOR
00046 *                  REFERENCE BY SUCCESSIVE PROGRAMS.  THE PROGRAM
00047 *                  WORK AREA OF THE COMMAREA IS USED TO PASS THE
00048 *                  RECORD KEY INFORMATION NEEDED BY EL650, EL651
00049 *                  AND/OR EL652 TO LOCATE THAT ACCOUNT/COMPANY.
00050 *
00051 *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL659.  ON
00052 *                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE
00053 *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVE
00054 *                  ENTRIES (XCTL FROM CICS VIA EX66) THE SCREEN
00055 *                  WILL BE READ AND ACTION WILL BE BASED ON THE
00056 *                  MAINTENANCE TYPE INDICATED.
00050 *
101501******************************************************************
101501*                   C H A N G E   L O G
101501*
101501* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101501*-----------------------------------------------------------------
101501*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101501* EFFECTIVE    NUMBER
101501*-----------------------------------------------------------------
101501* 101501    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
101501*                              ADJUST REDEFINES EL659BI FILLER
101501******************************************************************
00057  EJECT
00058  ENVIRONMENT DIVISION.
00059  DATA DIVISION.
00060  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00061  77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.
00062  77  FILLER  PIC  X(32) VALUE '********************************'.
00063  77  FILLER  PIC  X(32) VALUE '*   EL6592 WORKING STORAGE     *'.
00064  77  FILLER  PIC  X(32) VALUE '*********** VMOD=2.010 *********'.
00065
00066 *    COPY ELCSCTM.
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
00067
00068 *    COPY ELCSCRTY.
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
00069
00070  01  WS-DATE-AREA.
00071      12  SAVE-DATE               PIC  X(08)      VALUE SPACES.
00072      12  SAVE-BIN-DATE           PIC  X(02)      VALUE SPACES.
00073
00074  01  FILLER              COMP-3.
00075      12  TIME-IN                 PIC S9(07)      VALUE ZERO.
00076      12  TIME-OUT  REDEFINES
00077          TIME-IN                 PIC S9(03)V9(4).
00078      12  WS-NAME-SW              PIC S9(01)      VALUE ZERO.
00079          88  WS-NO-NAME-FOUND                    VALUE ZERO.
00080      12  SUB                     PIC S9(02)      VALUE ZERO.
00081
00082  01  FILLER              COMP.
00083      12  WS-INDEX                PIC S9(04)      VALUE ZERO.
00084      12  WS-TS-LENGTH            PIC S9(04)      VALUE +1920.
00085      12  SC-ITEM                 PIC S9(04)      VALUE +0001.
00086
00087  01  FILLER.
00088      12  QID.
00089          16  QID-TERM                PIC  X(04).
00090          16  FILLER                  PIC  X(04)  VALUE '659B'.
00091      12  QID-PROC-AREA               PIC  X(03).
00092      12  QID-LENGTH                  PIC S9(04)  VALUE +3  COMP.
00093      12  QID-ITEM                    PIC S9(04)  VALUE +1  COMP.
00094      12  WS-MAPSET-NAME              PIC  X(08)  VALUE 'EL6592S'.
00095      12  WS-MAP-NAME                 PIC  X(08)  VALUE 'EL659B'.
00096      12  FILLER  REDEFINES  WS-MAP-NAME.
00097          16  FILLER                  PIC  X(02).
00098          16  WS-MAP-NUMBER           PIC  X(04).
00099          16  FILLER                  PIC  X(02).
00100      12  THIS-PGM                    PIC  X(08)  VALUE 'EL6592'.
00101      12  WS-TRANS-ID                 PIC  X(04)  VALUE 'EX66'.
00102      12  WS-TEMP-STORAGE-KEY.
00103          16  WS-TSK-TERM-ID          PIC  X(04)  VALUE 'XXXX'.
00104          16  FILLER                  PIC  X(04)  VALUE '6592'.
00105      12  WS-TEMP-STORAGE-KEY1.
00106          16  WS-TSK-TERM-ID1         PIC  X(04)  VALUE 'XXXX'.
00107          16  FILLER                  PIC  X(04)  VALUE '659P'.
00108      12  WS-KEY-HOLD.
00109          16  WS-KH-CHAR              PIC  X(01)
00110                  OCCURS  61  TIMES
00111                      INDEXED BY  KEY-INDEX.
00112      12  WS-KEY-INPUT.
00113          16  WS-KI-CHAR              PIC  X(01)
00114                  OCCURS  61  TIMES
00115                      INDEXED BY  KEY-INDEX2.
00116
00117      12  WS-CITY-INDEX               PIC  S9(4)  COMP.
00118      12  WS-COMPARE-INDICATOR        PIC  X.
00119          88 CITY-FOUND                         VALUE SPACE.
00120          88 CITY-NOT-FOUND                     VALUE 'X'.
00121      12  WS-NL-CITY                  PIC  X(15).
00122      12  WS-NL-CITY-CHAR  REDEFINES
00123          WS-NL-CITY                  PIC  X
00124                                      OCCURS 15.
00125      12  WS-PI-CITY                  PIC  X(15).
00126      12  WS-PI-CITY-CHAR  REDEFINES
00127          WS-PI-CITY                  PIC  X
00128                                      OCCURS 15.
00129
00130      12  WS-CALC-RDNXT               PIC S9(08) COMP VALUE ZERO.
00131      12  WS-CONTROL-PRIMARY          PIC X(33).
00132      12  ERROR-MESSAGES.
00133          16  ER-0004                 PIC  X(04)  VALUE '0004'.
00134          16  ER-0008                 PIC  X(04)  VALUE '0008'.
00135          16  ER-0029                 PIC  X(04)  VALUE '0029'.
00136          16  ER-0130                 PIC  X(04)  VALUE '0130'.
00137          16  ER-0200                 PIC  X(04)  VALUE '0200'.
00138          16  ER-0673                 PIC  X(04)  VALUE '0201'.
00139          16  ER-7323                 PIC  X(04)  VALUE '7323'.
00140          16  ER-7324                 PIC  X(04)  VALUE '7324'.
00141          16  ER-9262                 PIC  X(04)  VALUE '9262'.
00142      12  EL650-TRANS-ID              PIC  X(04)  VALUE 'EXC4'.
00143      12  EL651-TRANS-ID              PIC  X(04)  VALUE 'EXD1'.
00144      12  EL652-TRANS-ID              PIC  X(04)  VALUE 'EXD4'.
00145  EJECT
00146  01  ACCOUNT-KEY-LINE.
00147      12  AKL-FILL-1              PIC  X(05).
00148      12  AKL-CARRIER             PIC  X(01).
00149      12  AKL-FILL-2              PIC  X(07).
00150      12  AKL-GROUPING            PIC  X(06).
00151      12  AKL-FILL-3              PIC  X(07).
00152      12  AKL-STATE               PIC  X(02).
00153      12  AKL-FILL-4              PIC  X(09).
00154      12  AKL-ACCOUNT             PIC  X(10).
00155      12  FILLER                  PIC  X(20).
00156
00157  01  COMPENSATION-KEY-LINE.
00158      12  CKL-FILL-1              PIC  X(05).
00159      12  CKL-CARRIER             PIC  X(01).
00160      12  CKL-FILL-2              PIC  X(07).
00161      12  CKL-GROUPING            PIC  X(06).
00162      12  CKL-FILL-3              PIC  X(10).
00163      12  CKL-RESP                PIC  X(10).
00164      12  CKL-FILL-4              PIC  X(09).
00165      12  CKL-ACCOUNT             PIC  X(10).
00166      12  CKL-FILL-5              PIC  X(06).
00167      12  CKL-TYPE                PIC  X(01).
00168      12  FILLER                  PIC  X(02).
00169
00170  01  REINSURANCE-KEY-LINE.
00171      12  RKL-FILL-1              PIC  X(06).
00172      12  RKL-REIN-TABLE          PIC  X(03).
00173      12  RKL-FILL-2              PIC  X(06).
00174      12  RKL-REIN-COMP           PIC  X(03).
00175      12  RKL-FILL-3              PIC  X(05).
00176      12  RKL-REIN-COMP-SUB       PIC  X(03).
00177      12  RKL-FILL-4              PIC  X(09).
00178      12  RKL-LEVELS      OCCURS  11  TIMES.
00179          16  RKL-FILL-5          PIC  X(01).
00180          16  RKL-LEVEL           PIC  9(02).
00181  EJECT
00182 *    COPY ELCINTF.
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
00183
00184 *    COPY ELC659PI.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELC659PI                            *
00004 *                            VMOD=2.002                          *
00005 *                                                                *
00006 ******************************************************************
00007
00008      12  PI-WORK-AREA  REDEFINES  PI-PROGRAM-WORK-AREA.
00009          16  FILLER                      PIC  X(02).
00010          16  PI-ERREIN-KEY.
00011              20  PI-ERR-COMPANY-CD       PIC  X(01).
00012              20  PI-ERR-CODE             PIC  X(01).
00013              20  PI-ERR-TABLE            PIC  X(03).
00014              20  PI-ERR-SUB              PIC  X(03).
00015          16  PI-ERR-LEVEL                PIC  9(02).
00016          16  PI-1ST-TIME-SW              PIC S9(01)  COMP-3.
00017          16  PI-LINE-COUNT               PIC S9(03)  COMP-3.
00018          16  PI-AIX-RECORD-COUNT         PIC S9(04)  COMP SYNC.
00019          16  PI-START-SW                 PIC S9(01)  COMP-3.
00020          16  PI-BROWSE-SW                PIC S9(01)  COMP-3.
00021          16  PI-END-OF-FILE              PIC S9(01)  COMP-3.
00022          16  PI-DSID                     PIC  X(08).
00023          16  PI-OPTION                   PIC  X(01).
00024              88  NO-OPTION-SELECTED              VALUE ZERO.
00025              88  OPTION-ONE-SELECTED             VALUE '1'.
00026              88  OPTION-TWO-SELECTED             VALUE '2'.
00027              88  OPTION-THREE-SELECTED           VALUE '3'.
00028          16  PI-SELECTION-CRITERIA.
00029              20  PI-SC-COMPANY-CD        PIC  X(01).
00030              20  PI-SC-NAME              PIC  X(30).
00031              20  PI-SC-RECORD-TYPE       PIC  X(01).
00032              20  FILLER                  PIC  X(29).
00033          16  PI-NAME-LOOKUP-KEY.
00034              20  PI-NLK-COMPANY-CD       PIC  X(01).
00035              20  PI-NLK-NAME             PIC  X(30).
00036              20  PI-NLK-RECORD-TYPE      PIC  X(01).
00037              20  FILLER                  PIC  X(29).
00038          16  PI-PREV-NAME-LOOKUP-KEY.
00039              20  PI-PREV-NLK-COMPANY-CD  PIC  X(01).
00040              20  PI-PREV-NLK-NAME        PIC  X(30).
00041              20  PI-PREV-NLK-RECORD-TYPE
00042                                          PIC  X(01).
00043              20  FILLERV-CK-STATE        PIC  X(29).
00044          16  PI-CITY                     PIC  X(15).
00045          16  PI-ST                       PIC  XX.
00046          16  PI-CITY-LENGTH              PIC S9(04)  COMP SYNC.
00047          16  PI-KEY-LENGTH               PIC S9(04)  COMP SYNC.
00048          16  PI-TS-ITEM                  PIC S9(04)  COMP SYNC.
00049          16  PI-1ST-KEY                  PIC  X(61).
00050          16  FILLER                      PIC  X(43).
00051          16  PI-PREV-AID                 PIC  X(01).
00052          16  PI-START-NAME-LOOKUP-KEY.
00053              20  PI-START-COMPANY-CD     PIC  X(01).
00054              20  PI-START-NAME           PIC  X(30).
00055              20  PI-START-RECORD-TYPE    PIC  X(01).
00056              20  FILLER                  PIC  X(29).
00057          16  PI-END-NAME-LOOKUP-KEY.
00058              20  PI-END-COMPANY-CD       PIC  X(01).
00059              20  PI-END-NAME             PIC  X(30).
00060              20  PI-END-RECORD-TYPE      PIC  X(01).
00061              20  FILLER                  PIC  X(29).
00062          16  PI-LIN1-NAME-LOOKUP-KEY     PIC  X(61).
00063          16  PI-SCREEN-COUNT             PIC S9(08)  COMP.
00064          16  PI-SUB                      PIC S9(02).
00065          16  PI-FIRST-TIME-SW            PIC  X(01).
00066
00067 ******************************************************************
00185          16  PI-EL659-TO-EL130-CNTRL.
00186              20  PI-EL659-CARRIER    PIC X.
00187              20  PI-EL659-GROUPING   PIC X(6).
00188              20  PI-EL659-STATE      PIC XX.
00189              20  PI-EL659-ACCOUNT    PIC X(10).
00190          16  FILLER                  PIC X(90).
00191  EJECT
00192 *    COPY EL6592S.
       01  EL659BI.
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
           05  BTYP01L PIC S9(0004) COMP.
           05  BTYP01F PIC  X(0001).
           05  FILLER REDEFINES BTYP01F.
               10  BTYP01A PIC  X(0001).
           05  BTYP01I PIC  X(0001).
      *    -------------------------------
           05  BNAM01L PIC S9(0004) COMP.
           05  BNAM01F PIC  X(0001).
           05  FILLER REDEFINES BNAM01F.
               10  BNAM01A PIC  X(0001).
           05  BNAM01I PIC  X(0030).
      *    -------------------------------
           05  BCITY1L PIC S9(0004) COMP.
           05  BCITY1F PIC  X(0001).
           05  FILLER REDEFINES BCITY1F.
               10  BCITY1A PIC  X(0001).
           05  BCITY1I PIC  X(0015).
      *    -------------------------------
           05  BST1L PIC S9(0004) COMP.
           05  BST1F PIC  X(0001).
           05  FILLER REDEFINES BST1F.
               10  BST1A PIC  X(0001).
           05  BST1I PIC  X(0002).
      *    -------------------------------
           05  BAST01L PIC S9(0004) COMP.
           05  BAST01F PIC  X(0001).
           05  FILLER REDEFINES BAST01F.
               10  BAST01A PIC  X(0001).
           05  BAST01I PIC  X(0001).
      *    -------------------------------
           05  BKEY01L PIC S9(0004) COMP.
           05  BKEY01F PIC  X(0001).
           05  FILLER REDEFINES BKEY01F.
               10  BKEY01A PIC  X(0001).
           05  BKEY01I PIC  X(0068).
      *    -------------------------------
           05  BTYP02L PIC S9(0004) COMP.
           05  BTYP02F PIC  X(0001).
           05  FILLER REDEFINES BTYP02F.
               10  BTYP02A PIC  X(0001).
           05  BTYP02I PIC  X(0001).
      *    -------------------------------
           05  BNAM02L PIC S9(0004) COMP.
           05  BNAM02F PIC  X(0001).
           05  FILLER REDEFINES BNAM02F.
               10  BNAM02A PIC  X(0001).
           05  BNAM02I PIC  X(0030).
      *    -------------------------------
           05  BCITY2L PIC S9(0004) COMP.
           05  BCITY2F PIC  X(0001).
           05  FILLER REDEFINES BCITY2F.
               10  BCITY2A PIC  X(0001).
           05  BCITY2I PIC  X(0015).
      *    -------------------------------
           05  BST2L PIC S9(0004) COMP.
           05  BST2F PIC  X(0001).
           05  FILLER REDEFINES BST2F.
               10  BST2A PIC  X(0001).
           05  BST2I PIC  X(0002).
      *    -------------------------------
           05  BAST02L PIC S9(0004) COMP.
           05  BAST02F PIC  X(0001).
           05  FILLER REDEFINES BAST02F.
               10  BAST02A PIC  X(0001).
           05  BAST02I PIC  X(0001).
      *    -------------------------------
           05  BKEY02L PIC S9(0004) COMP.
           05  BKEY02F PIC  X(0001).
           05  FILLER REDEFINES BKEY02F.
               10  BKEY02A PIC  X(0001).
           05  BKEY02I PIC  X(0068).
      *    -------------------------------
           05  BTYP03L PIC S9(0004) COMP.
           05  BTYP03F PIC  X(0001).
           05  FILLER REDEFINES BTYP03F.
               10  BTYP03A PIC  X(0001).
           05  BTYP03I PIC  X(0001).
      *    -------------------------------
           05  BNAM03L PIC S9(0004) COMP.
           05  BNAM03F PIC  X(0001).
           05  FILLER REDEFINES BNAM03F.
               10  BNAM03A PIC  X(0001).
           05  BNAM03I PIC  X(0030).
      *    -------------------------------
           05  BCITY3L PIC S9(0004) COMP.
           05  BCITY3F PIC  X(0001).
           05  FILLER REDEFINES BCITY3F.
               10  BCITY3A PIC  X(0001).
           05  BCITY3I PIC  X(0015).
      *    -------------------------------
           05  BST3L PIC S9(0004) COMP.
           05  BST3F PIC  X(0001).
           05  FILLER REDEFINES BST3F.
               10  BST3A PIC  X(0001).
           05  BST3I PIC  X(0002).
      *    -------------------------------
           05  BAST03L PIC S9(0004) COMP.
           05  BAST03F PIC  X(0001).
           05  FILLER REDEFINES BAST03F.
               10  BAST03A PIC  X(0001).
           05  BAST03I PIC  X(0001).
      *    -------------------------------
           05  BKEY03L PIC S9(0004) COMP.
           05  BKEY03F PIC  X(0001).
           05  FILLER REDEFINES BKEY03F.
               10  BKEY03A PIC  X(0001).
           05  BKEY03I PIC  X(0068).
      *    -------------------------------
           05  BTYP04L PIC S9(0004) COMP.
           05  BTYP04F PIC  X(0001).
           05  FILLER REDEFINES BTYP04F.
               10  BTYP04A PIC  X(0001).
           05  BTYP04I PIC  X(0001).
      *    -------------------------------
           05  BNAM04L PIC S9(0004) COMP.
           05  BNAM04F PIC  X(0001).
           05  FILLER REDEFINES BNAM04F.
               10  BNAM04A PIC  X(0001).
           05  BNAM04I PIC  X(0030).
      *    -------------------------------
           05  BCITY4L PIC S9(0004) COMP.
           05  BCITY4F PIC  X(0001).
           05  FILLER REDEFINES BCITY4F.
               10  BCITY4A PIC  X(0001).
           05  BCITY4I PIC  X(0015).
      *    -------------------------------
           05  BST4L PIC S9(0004) COMP.
           05  BST4F PIC  X(0001).
           05  FILLER REDEFINES BST4F.
               10  BST4A PIC  X(0001).
           05  BST4I PIC  X(0002).
      *    -------------------------------
           05  BAST04L PIC S9(0004) COMP.
           05  BAST04F PIC  X(0001).
           05  FILLER REDEFINES BAST04F.
               10  BAST04A PIC  X(0001).
           05  BAST04I PIC  X(0001).
      *    -------------------------------
           05  BKEY04L PIC S9(0004) COMP.
           05  BKEY04F PIC  X(0001).
           05  FILLER REDEFINES BKEY04F.
               10  BKEY04A PIC  X(0001).
           05  BKEY04I PIC  X(0068).
      *    -------------------------------
           05  BTYP05L PIC S9(0004) COMP.
           05  BTYP05F PIC  X(0001).
           05  FILLER REDEFINES BTYP05F.
               10  BTYP05A PIC  X(0001).
           05  BTYP05I PIC  X(0001).
      *    -------------------------------
           05  BNAM05L PIC S9(0004) COMP.
           05  BNAM05F PIC  X(0001).
           05  FILLER REDEFINES BNAM05F.
               10  BNAM05A PIC  X(0001).
           05  BNAM05I PIC  X(0030).
      *    -------------------------------
           05  BCITY5L PIC S9(0004) COMP.
           05  BCITY5F PIC  X(0001).
           05  FILLER REDEFINES BCITY5F.
               10  BCITY5A PIC  X(0001).
           05  BCITY5I PIC  X(0015).
      *    -------------------------------
           05  BST5L PIC S9(0004) COMP.
           05  BST5F PIC  X(0001).
           05  FILLER REDEFINES BST5F.
               10  BST5A PIC  X(0001).
           05  BST5I PIC  X(0002).
      *    -------------------------------
           05  BAST05L PIC S9(0004) COMP.
           05  BAST05F PIC  X(0001).
           05  FILLER REDEFINES BAST05F.
               10  BAST05A PIC  X(0001).
           05  BAST05I PIC  X(0001).
      *    -------------------------------
           05  BKEY05L PIC S9(0004) COMP.
           05  BKEY05F PIC  X(0001).
           05  FILLER REDEFINES BKEY05F.
               10  BKEY05A PIC  X(0001).
           05  BKEY05I PIC  X(0068).
      *    -------------------------------
           05  BTYP06L PIC S9(0004) COMP.
           05  BTYP06F PIC  X(0001).
           05  FILLER REDEFINES BTYP06F.
               10  BTYP06A PIC  X(0001).
           05  BTYP06I PIC  X(0001).
      *    -------------------------------
           05  BNAM06L PIC S9(0004) COMP.
           05  BNAM06F PIC  X(0001).
           05  FILLER REDEFINES BNAM06F.
               10  BNAM06A PIC  X(0001).
           05  BNAM06I PIC  X(0030).
      *    -------------------------------
           05  BCITY6L PIC S9(0004) COMP.
           05  BCITY6F PIC  X(0001).
           05  FILLER REDEFINES BCITY6F.
               10  BCITY6A PIC  X(0001).
           05  BCITY6I PIC  X(0015).
      *    -------------------------------
           05  BST6L PIC S9(0004) COMP.
           05  BST6F PIC  X(0001).
           05  FILLER REDEFINES BST6F.
               10  BST6A PIC  X(0001).
           05  BST6I PIC  X(0002).
      *    -------------------------------
           05  BAST06L PIC S9(0004) COMP.
           05  BAST06F PIC  X(0001).
           05  FILLER REDEFINES BAST06F.
               10  BAST06A PIC  X(0001).
           05  BAST06I PIC  X(0001).
      *    -------------------------------
           05  BKEY06L PIC S9(0004) COMP.
           05  BKEY06F PIC  X(0001).
           05  FILLER REDEFINES BKEY06F.
               10  BKEY06A PIC  X(0001).
           05  BKEY06I PIC  X(0068).
      *    -------------------------------
           05  BTYP07L PIC S9(0004) COMP.
           05  BTYP07F PIC  X(0001).
           05  FILLER REDEFINES BTYP07F.
               10  BTYP07A PIC  X(0001).
           05  BTYP07I PIC  X(0001).
      *    -------------------------------
           05  BNAM07L PIC S9(0004) COMP.
           05  BNAM07F PIC  X(0001).
           05  FILLER REDEFINES BNAM07F.
               10  BNAM07A PIC  X(0001).
           05  BNAM07I PIC  X(0030).
      *    -------------------------------
           05  BCITY7L PIC S9(0004) COMP.
           05  BCITY7F PIC  X(0001).
           05  FILLER REDEFINES BCITY7F.
               10  BCITY7A PIC  X(0001).
           05  BCITY7I PIC  X(0015).
      *    -------------------------------
           05  BST7L PIC S9(0004) COMP.
           05  BST7F PIC  X(0001).
           05  FILLER REDEFINES BST7F.
               10  BST7A PIC  X(0001).
           05  BST7I PIC  X(0002).
      *    -------------------------------
           05  BAST07L PIC S9(0004) COMP.
           05  BAST07F PIC  X(0001).
           05  FILLER REDEFINES BAST07F.
               10  BAST07A PIC  X(0001).
           05  BAST07I PIC  X(0001).
      *    -------------------------------
           05  BKEY07L PIC S9(0004) COMP.
           05  BKEY07F PIC  X(0001).
           05  FILLER REDEFINES BKEY07F.
               10  BKEY07A PIC  X(0001).
           05  BKEY07I PIC  X(0068).
      *    -------------------------------
           05  BTYP08L PIC S9(0004) COMP.
           05  BTYP08F PIC  X(0001).
           05  FILLER REDEFINES BTYP08F.
               10  BTYP08A PIC  X(0001).
           05  BTYP08I PIC  X(0001).
      *    -------------------------------
           05  BNAM08L PIC S9(0004) COMP.
           05  BNAM08F PIC  X(0001).
           05  FILLER REDEFINES BNAM08F.
               10  BNAM08A PIC  X(0001).
           05  BNAM08I PIC  X(0030).
      *    -------------------------------
           05  BCITY8L PIC S9(0004) COMP.
           05  BCITY8F PIC  X(0001).
           05  FILLER REDEFINES BCITY8F.
               10  BCITY8A PIC  X(0001).
           05  BCITY8I PIC  X(0015).
      *    -------------------------------
           05  BST8L PIC S9(0004) COMP.
           05  BST8F PIC  X(0001).
           05  FILLER REDEFINES BST8F.
               10  BST8A PIC  X(0001).
           05  BST8I PIC  X(0002).
      *    -------------------------------
           05  BAST08L PIC S9(0004) COMP.
           05  BAST08F PIC  X(0001).
           05  FILLER REDEFINES BAST08F.
               10  BAST08A PIC  X(0001).
           05  BAST08I PIC  X(0001).
      *    -------------------------------
           05  BKEY08L PIC S9(0004) COMP.
           05  BKEY08F PIC  X(0001).
           05  FILLER REDEFINES BKEY08F.
               10  BKEY08A PIC  X(0001).
           05  BKEY08I PIC  X(0068).
      *    -------------------------------
           05  BEMSG1L PIC S9(0004) COMP.
           05  BEMSG1F PIC  X(0001).
           05  FILLER REDEFINES BEMSG1F.
               10  BEMSG1A PIC  X(0001).
           05  BEMSG1I PIC  X(0079).
      *    -------------------------------
           05  BSELL PIC S9(0004) COMP.
           05  BSELF PIC  X(0001).
           05  FILLER REDEFINES BSELF.
               10  BSELA PIC  X(0001).
           05  BSELI PIC  9.
      *    -------------------------------
           05  BPFKL PIC S9(0004) COMP.
           05  BPFKF PIC  X(0001).
           05  FILLER REDEFINES BPFKF.
               10  BPFKA PIC  X(0001).
           05  BPFKI PIC  99.
       01  EL659BO REDEFINES EL659BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMPNYIDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYP01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAM01O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCITY1O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BST1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAST01O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BKEY01O PIC  X(0068).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYP02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAM02O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCITY2O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BST2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAST02O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BKEY02O PIC  X(0068).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYP03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAM03O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCITY3O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BST3O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAST03O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BKEY03O PIC  X(0068).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYP04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAM04O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCITY4O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BST4O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAST04O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BKEY04O PIC  X(0068).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYP05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAM05O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCITY5O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BST5O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAST05O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BKEY05O PIC  X(0068).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYP06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAM06O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCITY6O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BST6O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAST06O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BKEY06O PIC  X(0068).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYP07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAM07O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCITY7O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BST7O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAST07O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BKEY07O PIC  X(0068).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTYP08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BNAM08O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCITY8O PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BST8O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BAST08O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BKEY08O PIC  X(0068).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSELO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPFKO PIC  X(0002).
      *    -------------------------------
00193
00194  01  FILLER  REDEFINES  EL659BI.
101501     12  FILLER                      PIC  X(44).
00196      12  EL659B-MAP-LINE     OCCURS  8  TIMES
00197              INDEXED  BY  EL659B-INDEX  EL659B-INDEX2.
00198          16  EL659B-TYPE-LENGTH      PIC S9(04)      COMP.
00199          16  EL659B-TYPE-ATTR        PIC  X(01).
00200          16  EL659B-TYPE             PIC  X(01).
00201          16  EL659B-NAME-LENGTH      PIC S9(04)      COMP.
00202          16  EL659B-NAME-ATTR        PIC  X(01).
00203          16  EL659B-NAME             PIC  X(30).
00204          16  EL659B-CITY-LENGTH      PIC S9(04)      COMP.
00205          16  EL659B-CITY-ATTR        PIC  X(01).
00206          16  EL659B-CITY             PIC  X(15).
00207          16  EL659B-ST-LENGTH        PIC S9(04)      COMP.
00208          16  EL659B-ST-ATTR          PIC  X(01).
00209          16  EL659B-ST               PIC  XX.
00210          16  EL659B-AST-LENGTH       PIC S9(04)      COMP.
00211          16  EL659B-AST-ATTR         PIC  X(01).
00212          16  EL659B-AST              PIC  X(01).
00213          16  EL659B-KEY-LENGTH       PIC S9(04)      COMP.
00214          16  EL659B-KEY-ATTR         PIC  X(01).
00215          16  EL659B-KEY              PIC  X(68).
00216  EJECT
00217 *    COPY ELCDATE.
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
00218  EJECT
00219 *    COPY ELCEMIB.
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
00220  EJECT
00221 *    COPY ELCLOGOF.
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
00222  EJECT
00223 *    COPY ELCATTR.
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
00224  EJECT
00225 *    COPY ELCAID.
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
00226
00227  01  FILLER  REDEFINES  DFHAID.
00228      12  FILLER                  PIC  X(08).
00229      12  PF-VALUES               PIC  X(01)
00230              OCCURS  24  TIMES.
00231
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
00233
00234  01  DFHCOMMAREA                 PIC  X(1024).
00235
00236 *01 PARMLIST             COMP.
00237 *    12  FILLER                  PIC S9(09).
00238 *    12  ERNAME-POINTER          PIC S9(09).
00239  EJECT
00240 *    COPY ERCNAME.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCNAME                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *   CREDIT SYSTEM ACCOUNT MASTER NAME, COMPENSATION MASTER       *
00008 *       NAME, REINSURANCE COMPANY NAME LOOKUP FILE.              *
00009 *                                                                *
00010 *   FILE DESCRIPTION = NAME LOOKUP FILE                          *
00011 *                                                                *
00012 *   FILE TYPE = VSAM,KSDS                                        *
00013 *   RECORD SIZE = 160   RECFORM = FIX                            *
00014 *                                                                *
00015 *   BASE CLUSTER NAME = ERNAME                    RKP=2,LEN=61   *
00016 *                                                                *
00017 *   LOG = NO                                                     *
00018 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00019 *                                                                *
00020 *                                                                *
00021 ******************************************************************
00022
00023  01  NAME-LOOKUP-MASTER.
00024      12  NL-RECORD-ID                PIC  X(02).
00025          88  VALID-NL-ID                         VALUE 'NL'.
00026
00027      12  NL-RECORD-KEY.
00028          16  NL-CONTROL-PRIMARY.
00029              20  NL-COMPANY-CD       PIC  X(01).
00030              20  NL-NAME             PIC  X(30).
00031              20  NL-RECORD-TYPE      PIC  X(01).
00032                  88  NL-ACCOUNT-TYPE             VALUE 'A'.
00033                  88  NL-COMPENSATION-TYPE        VALUE 'C'.
00034                  88  NL-REINSURANCE-TYPE         VALUE 'R'.
00035
00036          16  NL-ACCOUNT-MASTER.
00037              20  NL-AM-COMPANY-CD    PIC  X(01).
00038              20  NL-AM-CARRIER       PIC  X(01).
00039              20  NL-AM-GROUPING      PIC  X(06).
00040              20  NL-AM-STATE         PIC  X(02).
00041              20  NL-AM-ACCOUNT       PIC  X(10).
00042              20  FILLER              PIC  X(09).
00043
00044          16  NL-COMPENSATION-MASTER
00045                                  REDEFINES  NL-ACCOUNT-MASTER.
00046              20  NL-CO-COMPANY-CD    PIC  X(01).
00047              20  NL-CO-CARRIER       PIC  X(01).
00048              20  NL-CO-GROUPING      PIC  X(06).
00049              20  NL-CO-RESP-NO       PIC  X(10).
00050              20  NL-CO-ACCOUNT       PIC  X(10).
00051              20  NL-CO-TYPE          PIC  X(01).
00052
00053          16  NL-REINSURANCE-RECORD
00054                                  REDEFINES  NL-ACCOUNT-MASTER.
00055              20  NL-RE-COMPANY-CD    PIC  X(01).
00056              20  NL-RE-CODE          PIC  X(01).
00057              20  NL-RE-COMPANY.
00058                  24  NL-RE-COMP      PIC  X(03).
00059                  24  NL-RE-CO-SUB    PIC  X(03).
00060              20  NL-RE-TABLE         PIC  X(03).
00061              20  FILLER              PIC  X(18).
00062
00063      12  NL-MAINT-INFORMATION.
00064          16  NL-LAST-MAINT-DT        PIC  X(02).
00065          16  NL-LAST-MAINT-HHMMSS    PIC S9(07)  COMP-3.
00066          16  NL-LAST-MAINT-USER      PIC  X(04).
00067          16  FILLER                  PIC  X(10).
00068
00069      12  NL-RE-LEVELS  OCCURS  30  TIMES.
00070          16  NL-RE-LEVEL             PIC  9(02).
00071
00072      12  NL-CITY                     PIC  X(15).
00073      12  NL-ST                       PIC  XX.
00074
00075 ******************************************************************
00241  EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA
                                NAME-LOOKUP-MASTER.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL6592' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00243
00244      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00245      MOVE '5'                    TO  DC-OPTION-CODE.
00246
00247      PERFORM 8500-DATE-CONVERSION.
00248
00249      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00250      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00251      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00252
00253 *    NOTE *******************************************************
00254 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL *
00255 *         *  FROM ANOTHER MODULE.                               *
00256 *         *******************************************************.
00257
00258      IF EIBCALEN  IS NOT GREATER THAN  ZERO
00259          MOVE UNACCESS-MSG       TO  LOGOFF-MSG
00260          PERFORM 8300-SEND-TEXT.
00261
00262      
      * EXEC CICS HANDLE CONDITION
00263 *        PGMIDERR  (9600-PGMIDERR)
00264 *        NOTFND    (8700-NOT-FOUND)
00265 *        ENDFILE   (4600-ENDFILE)
00266 *        DUPKEY    (4015-DUPKEY)
00267 *        ERROR     (9990-ERROR)
00268 *    END-EXEC.
      *    MOVE '"$LI''$.               ! " #00001722' TO DFHEIV0
           MOVE X'22244C4927242E2020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031373232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00269  EJECT
00270  0010-MAIN-LOGIC.
00271      IF PI-CALLING-PROGRAM  IS EQUAL TO  THIS-PGM
00272          GO TO 0100-MAIN-LOGIC.
00273
00274      IF PI-RETURN-TO-PROGRAM  IS NOT EQUAL TO  THIS-PGM
00275          MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6
00276          MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5
00277          MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4
00278          MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3
00279          MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2
00280          MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1
00281          MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM
00282          MOVE THIS-PGM              TO  PI-CALLING-PROGRAM
00283      ELSE
00284          MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM
00285          MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM
00286          MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1
00287          MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2
00288          MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3
00289          MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4
00290          MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5
00291          MOVE SPACES                TO  PI-SAVED-PROGRAM-6.
00292
00293      MOVE ZERO                   TO  PI-SCREEN-COUNT.
00294      MOVE +0                     TO  PI-SUB.
00295      MOVE PI-NAME-LOOKUP-KEY     TO  PI-1ST-KEY.
00296
00297      IF EIBTRNID = EL650-TRANS-ID OR
00298                    EL651-TRANS-ID OR
00299                    EL652-TRANS-ID
00300          MOVE EIBTRMID           TO  WS-TSK-TERM-ID
00301                                      WS-TSK-TERM-ID1
00302          
      * EXEC CICS READQ TS
00303 *            QUEUE   (WS-TEMP-STORAGE-KEY)
00304 *            ITEM    (SC-ITEM)
00305 *            INTO    (EL659BI)
00306 *            LENGTH  (WS-TS-LENGTH)
00307 *        END-EXEC
      *    MOVE '*$II   L              ''   #00001762' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TEMP-STORAGE-KEY, 
                 EL659BI, 
                 WS-TS-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00308          
      * EXEC CICS DELETEQ TS
00309 *            QUEUE  (WS-TEMP-STORAGE-KEY)
00310 *        END-EXEC
      *    MOVE '*&                    #   #00001768' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TEMP-STORAGE-KEY, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00311          
      * EXEC CICS READQ TS
00312 *            QUEUE   (WS-TEMP-STORAGE-KEY1)
00313 *            ITEM    (SC-ITEM)
00314 *            INTO    (PROGRAM-INTERFACE-BLOCK)
00315 *            LENGTH  (WS-TS-LENGTH)
00316 *        END-EXEC
      *    MOVE '*$II   L              ''   #00001771' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TEMP-STORAGE-KEY1, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-TS-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00317          
      * EXEC CICS DELETEQ TS
00318 *            QUEUE  (WS-TEMP-STORAGE-KEY1)
00319 *        END-EXEC
      *    MOVE '*&                    #   #00001777' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TEMP-STORAGE-KEY1, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00320          MOVE ZERO               TO  PI-1ST-TIME-SW
00321          MOVE LOW-VALUES         TO  BSELO
00322                                      BPFKO
00323          PERFORM 6000-SET-ATTRB
00324              VARYING  EL659B-INDEX  FROM  PI-LINE-COUNT  BY  -1
00325                  UNTIL  EL659B-INDEX  IS NOT GREATER THAN  ZERO
00326          PERFORM 8100-SEND-INITIAL-MAP
00327          GO TO 9100-RETURN-TRAN.
00328
00329      PERFORM 4000-BROWSE-CERT-FILE.
00330  EJECT
00331  0100-MAIN-LOGIC.
00332 *    NOTE *******************************************************
00333 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- *
00334 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    *
00335 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         *
00336 *         *******************************************************.
00337
00338      IF EIBAID  IS EQUAL TO  DFHCLEAR
00339          PERFORM 9400-CLEAR.
00340
00341      IF EIBAID  IS EQUAL TO  DFHPA1  OR  DFHPA2  OR  DFHPA3
00342          MOVE LOW-VALUES         TO  EL659BO
00343          MOVE -1                 TO  BPFKL
00344          MOVE ER-0008            TO  EMI-ERROR
00345          PERFORM 8200-SEND-DATAONLY
00346          GO TO 9100-RETURN-TRAN.
00347
00348      
      * EXEC CICS RECEIVE
00349 *        INTO    (EL659BI)
00350 *        MAPSET  (WS-MAPSET-NAME)
00351 *        MAP     (WS-MAP-NAME)
00352 *    END-EXEC.
           MOVE LENGTH OF
            EL659BI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00001808' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL659BI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00353
00354      IF BPFKL  IS GREATER THAN  ZERO
00355          IF EIBAID  IS NOT EQUAL TO  DFHENTER
00356              MOVE ER-0004        TO  EMI-ERROR
00357              MOVE AL-UNBOF       TO  BPFKA
00358              MOVE -1             TO  BPFKL
00359              PERFORM 8200-SEND-DATAONLY
00360              GO TO 9100-RETURN-TRAN
00361          ELSE
00362              IF (BPFKI  IS NUMERIC)
00363                AND (BPFKI  IS GREATER THAN  ZERO
00364                    AND BPFKI  IS LESS THAN  25)
00365                      MOVE PF-VALUES (BPFKI)
00366                                  TO  EIBAID
00367                  ELSE
00368                      MOVE ER-0029
00369                                  TO  EMI-ERROR
00370                      MOVE AL-UNBOF
00371                                  TO  BPFKA
00372                      MOVE -1     TO  BPFKL
00373                      PERFORM 8200-SEND-DATAONLY
00374                      GO TO 9100-RETURN-TRAN.
00375
00376 *    NOTE *******************************************************
00377 *         *      PF KEY      USAGE                              *
00378 *         *        PF1       SEARCH FORWARD                     *
00379 *         *        PF2       SEARCH BACKWARD                    *
00380 *         *        PF3       NEW CLAIM SETUP                    *
00381 *         *        PF12      HELP                               *
00382 *         *        PF23      LOGOFF                             *
00383 *         *        PF24      RETURN TO MASTER MENU              *
00384 *         *******************************************************.
00385
00386      IF EIBAID  IS EQUAL TO  DFHPF12
00387          MOVE 'EL010'            TO  THIS-PGM
00388          GO TO 9300-XCTL.
00389
00390      IF EIBAID  IS EQUAL TO  DFHPF23
00391          PERFORM 9000-RETURN-CICS.
00392
00393      IF EIBAID  IS EQUAL TO  DFHPF24
00394          MOVE 'EL126'            TO  THIS-PGM
00395          GO TO 9300-XCTL.
00396
00397      IF BSELL  IS GREATER THAN  ZERO
00398          IF BSELI  IS NUMERIC
00399              NEXT SENTENCE
00400          ELSE
00401              MOVE -1             TO  BSELL
00402              MOVE ER-0200        TO  EMI-ERROR
00403              PERFORM 8200-SEND-DATAONLY
00404              GO TO 9100-RETURN-TRAN.
00405
00406      IF BSELL  IS GREATER THAN  ZERO
00407          NEXT SENTENCE
00408      ELSE
00409          GO TO 0120-MAIN-LOGIC.
00410
00411      IF BSELL  IS GREATER THAN  ZERO
00412        AND BSELO  IS GREATER THAN  ZERO
00413        AND BSELO  IS LESS THAN  '9'
00414        AND BSELI  IS NOT GREATER THAN  PI-LINE-COUNT
00415          NEXT SENTENCE
00416      ELSE
00417          MOVE -1                 TO  BSELL
00418          MOVE ER-0200            TO  EMI-ERROR
00419          PERFORM 8200-SEND-DATAONLY
00420          GO TO 9100-RETURN-TRAN.
00421
00422      IF EIBAID = DFHPF3
00423       IF PI-SAVED-PROGRAM-1 NOT = 'EL130'
00424         MOVE -1                 TO  BSELL
00425         MOVE ER-7324            TO  EMI-ERROR
00426         PERFORM 8200-SEND-DATAONLY
00427         GO TO 9100-RETURN-TRAN
00428        ELSE
00429         SET EL659B-INDEX        TO  BSELI
00430        IF EL659B-TYPE (EL659B-INDEX) = 'A'
00431          MOVE EL659B-KEY (EL659B-INDEX)
00432                                  TO  ACCOUNT-KEY-LINE
00433          MOVE AKL-CARRIER        TO PI-EL659-CARRIER
00434          MOVE AKL-GROUPING       TO PI-EL659-GROUPING
00435          MOVE AKL-STATE          TO PI-EL659-STATE
00436          MOVE AKL-ACCOUNT        TO PI-EL659-ACCOUNT
00437          MOVE 'EL130'            TO THIS-PGM
00438          GO TO 9300-XCTL
00439        ELSE
00440          MOVE -1                 TO  BSELL
00441          MOVE ER-9262            TO  EMI-ERROR
00442          PERFORM 8200-SEND-DATAONLY
00443          GO TO 9100-RETURN-TRAN.
00444
00445      IF BSELL  IS GREATER THAN  ZERO
00446          MOVE EIBTRMID       TO  WS-TSK-TERM-ID
00447                                  WS-TSK-TERM-ID1
00448          MOVE -1             TO  BSELL
00449          
      * EXEC CICS WRITEQ TS
00450 *            FROM    (EL659BO)
00451 *            LENGTH  (WS-TS-LENGTH)
00452 *            QUEUE   (WS-TEMP-STORAGE-KEY)
00453 *            ITEM    (SC-ITEM)
00454 *        END-EXEC
      *    MOVE '*" I                  ''   #00001909' TO DFHEIV0
           MOVE X'2A2220492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031393039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TEMP-STORAGE-KEY, 
                 EL659BO, 
                 WS-TS-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00455          
      * EXEC CICS WRITEQ TS
00456 *            FROM    (PROGRAM-INTERFACE-BLOCK)
00457 *            LENGTH  (WS-TS-LENGTH)
00458 *            QUEUE   (WS-TEMP-STORAGE-KEY1)
00459 *            ITEM    (SC-ITEM)
00460 *        END-EXEC.
      *    MOVE '*" I                  ''   #00001915' TO DFHEIV0
           MOVE X'2A2220492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031393135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TEMP-STORAGE-KEY1, 
                 PROGRAM-INTERFACE-BLOCK, 
                 WS-TS-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00461  EJECT
00462  0110-MAIN-LOGIC.
00463      SET EL659B-INDEX            TO  BSELI.
00464
00465      MOVE PI-SAVED-PROGRAM-1     TO  THIS-PGM.
00466
00467      IF EL659B-TYPE (EL659B-INDEX)  IS EQUAL TO  'C'
00468          GO TO 0112-SET-COMP-KEY.
00469
00470      IF EL659B-TYPE (EL659B-INDEX)  IS EQUAL TO  'R'
00471          GO TO 0113-SET-REIN-KEY.
00472
00473  0111-SET-ACCT-KEY.
00474      MOVE EL659B-KEY (EL659B-INDEX)
00475                                  TO  ACCOUNT-KEY-LINE.
00476      MOVE AKL-CARRIER            TO  PI-CR-CARRIER.
00477      MOVE AKL-GROUPING           TO  PI-CR-GROUPING.
00478      MOVE AKL-STATE              TO  PI-CR-STATE.
00479      MOVE AKL-ACCOUNT            TO  PI-CR-ACCOUNT.
00480      MOVE 'EL650'                TO  THIS-PGM.
00481
00482      GO TO 0114-CONTINUE.
00483
00484  0112-SET-COMP-KEY.
00485      MOVE EL659B-KEY (EL659B-INDEX)
00486                                  TO  COMPENSATION-KEY-LINE.
00487      MOVE CKL-CARRIER            TO  PI-CR-CARRIER.
00488      MOVE CKL-TYPE               TO  PI-CR-TYPE.
00489
00490      IF CKL-GROUPING IS EQUAL TO SPACES
00491         MOVE LOW-VALUES          TO  PI-CR-GROUPING
00492      ELSE
00493         MOVE CKL-GROUPING        TO  PI-CR-GROUPING.
00494
00495      IF CKL-RESP  IS EQUAL TO  SPACES
00496          MOVE LOW-VALUES         TO  PI-CR-FIN-RESP
00497      ELSE
00498          MOVE CKL-RESP           TO  PI-CR-FIN-RESP.
00499
00500      IF CKL-ACCOUNT  IS EQUAL TO  SPACES
00501          MOVE LOW-VALUES         TO  PI-CR-ACCOUNT
00502      ELSE
00503          MOVE CKL-ACCOUNT        TO  PI-CR-ACCOUNT.
00504
00505      MOVE 'EL652'                TO  THIS-PGM.
00506
00507      GO TO 0114-CONTINUE.
00508
00509  0113-SET-REIN-KEY.
00510      MOVE EL659B-KEY (EL659B-INDEX)
00511                                  TO  REINSURANCE-KEY-LINE.
00512      MOVE LOW-VALUES             TO  PI-ERREIN-KEY.
00513      MOVE PI-COMPANY-CD          TO  PI-ERR-COMPANY-CD.
00514      MOVE 'A'                    TO  PI-ERR-CODE.
00515      MOVE RKL-REIN-TABLE         TO  PI-ERR-TABLE.
00516      MOVE RKL-LEVEL (1)          TO  PI-ERR-LEVEL.
00517      MOVE 'EL651'                TO  THIS-PGM.
00518
00519  0114-CONTINUE.
00520      MOVE +2                     TO  PI-1ST-TIME-SW.
00521
00522      IF THIS-PGM  IS NOT EQUAL TO  'EL6592'
00523          MOVE 'EL6592'           TO  PI-CALLING-PROGRAM
00524          MOVE PI-SAVED-PROGRAM-1 TO  PI-RETURN-TO-PROGRAM
00525          MOVE PI-SAVED-PROGRAM-2 TO  PI-SAVED-PROGRAM-1
00526          MOVE PI-SAVED-PROGRAM-3 TO  PI-SAVED-PROGRAM-2
00527          MOVE PI-SAVED-PROGRAM-4 TO  PI-SAVED-PROGRAM-3
00528          MOVE PI-SAVED-PROGRAM-5 TO  PI-SAVED-PROGRAM-4
00529          MOVE PI-SAVED-PROGRAM-6 TO  PI-SAVED-PROGRAM-5
00530          MOVE SPACES             TO  PI-SAVED-PROGRAM-6.
00531
00532      GO TO 9300-XCTL.
00533  EJECT
00534  0120-MAIN-LOGIC.
00535      IF BSELL  = ZERO AND
00536         EIBAID = DFHPF3
00537          MOVE ER-7323            TO  EMI-ERROR
00538          MOVE -1                 TO  BSELL
00539          PERFORM 8200-SEND-DATAONLY
00540          GO TO 9100-RETURN-TRAN.
00541
00542      IF EIBAID  IS EQUAL TO  (DFHENTER  OR  DFHPF1)
00543        OR ((EIBAID  IS EQUAL TO  DFHPF2)
00544        AND (PI-SCREEN-COUNT  IS GREATER THAN  +1))
00545          NEXT SENTENCE
00546      ELSE
00547          MOVE ER-0008            TO  EMI-ERROR
00548          MOVE -1                 TO  BPFKL
00549          PERFORM 8200-SEND-DATAONLY
00550          GO TO 9100-RETURN-TRAN.
00551
00552      IF PI-END-OF-FILE  IS NOT GREATER THAN  ZERO
00553          PERFORM 4000-BROWSE-CERT-FILE.
00554
00555      IF PI-END-OF-FILE  IS GREATER THAN  ZERO
00556          IF EIBAID  IS EQUAL TO  DFHPF2
00557              NEXT SENTENCE
00558          ELSE
00559              MOVE -1             TO  BSELL
00560              PERFORM 8200-SEND-DATAONLY
00561              GO TO 9100-RETURN-TRAN.
00562
00563      IF EIBAID  IS EQUAL TO  DFHPF2
00564          NEXT SENTENCE
00565      ELSE
00566          IF PI-DSID  IS NOT EQUAL TO  'ERNAME'
00567              PERFORM 9400-CLEAR.
00568
00569      IF (PI-PREV-AID  IS EQUAL TO  (DFHPF1  OR  DFHENTER)
00570        AND EIBAID  IS EQUAL TO  DFHPF2)
00571        OR (PI-PREV-AID  IS EQUAL TO  DFHPF2
00572        AND EIBAID  IS EQUAL TO  (DFHPF1  OR  DFHENTER))
00573          PERFORM 4000-BROWSE-CERT-FILE
00574      ELSE
00575          PERFORM 9400-CLEAR.
00576  EJECT
00577  4000-BROWSE-CERT-FILE  SECTION.
00578      
      * EXEC CICS HANDLE CONDITION
00579 *        NOTFND  (8700-NOT-FOUND)
00580 *    END-EXEC.
      *    MOVE '"$I                   ! # #00002038' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303032303338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00581
00582      MOVE LOW-VALUES             TO  EL659BO.
00583
00584      IF PI-BROWSE-SW  IS EQUAL TO  ZERO
00585        AND PI-START-SW  IS EQUAL TO  +1
00586          
      * EXEC CICS STARTBR
00587 *            DATASET    (PI-DSID)
00588 *            RIDFLD     (PI-NAME-LOOKUP-KEY)
00589 *            GENERIC
00590 *            EQUAL
00591 *            KEYLENGTH  (PI-KEY-LENGTH)
00592 *        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,   KG    E          &   #00002046' TO DFHEIV0
           MOVE X'262C2020204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032303436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 PI-NAME-LOOKUP-KEY, 
                 PI-KEY-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00593          GO TO 4005-NEXT-SENTENCE.
00594
00595      IF EIBAID  IS EQUAL TO  DFHPF2
00596          SUBTRACT 2              FROM  PI-SCREEN-COUNT
00597          PERFORM 7000-PF2-POSITION
00598          GO TO 4005-NEXT-SENTENCE.
00599
00600      
      * EXEC CICS STARTBR
00601 *        DATASET  (PI-DSID)
00602 *        RIDFLD   (PI-NAME-LOOKUP-KEY)
00603 *        EQUAL
00604 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         E          &   #00002060' TO DFHEIV0
           MOVE X'262C20202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032303630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 PI-NAME-LOOKUP-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00605
00606  4005-NEXT-SENTENCE.
00607      MOVE +1                     TO  PI-BROWSE-SW.
00608      MOVE ZERO                   TO  PI-LINE-COUNT.
00609      MOVE LOW-VALUES             TO  EL659BO.
00610      MOVE PI-NAME-LOOKUP-KEY     TO  WS-KEY-HOLD.
00611      SET EL659B-INDEX            TO  +1.
00612
00613  4010-READNEXT.
00614      MOVE PI-NAME-LOOKUP-KEY     TO  PI-PREV-NAME-LOOKUP-KEY.
00615
00616      
      * EXEC CICS READNEXT
00617 *        DATASET  (PI-DSID)
00618 *        RIDFLD   (PI-NAME-LOOKUP-KEY)
00619 *        SET      (ADDRESS OF NAME-LOOKUP-MASTER)
00620 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00002076' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303032303736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-NAME-LOOKUP-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF NAME-LOOKUP-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00621
00622  4015-DUPKEY.
00623      IF NL-COMPANY-CD GREATER PI-COMPANY-CD
00624          GO TO 4600-ENDFILE.
00625
00626      IF PI-ST NOT = SPACES
00627        IF PI-ST = NL-ST
00628            NEXT SENTENCE
00629          ELSE
00630            GO TO 4010-READNEXT.
00631
00632      IF PI-CITY = SPACES
00633          GO TO 4017-CONTINUE.
00634
00635      MOVE PI-CITY                TO WS-PI-CITY.
00636      MOVE NL-CITY                TO WS-NL-CITY.
00637
00638      MOVE SPACE                  TO WS-COMPARE-INDICATOR.
00639      PERFORM 4099-CHECK-CITY THRU 4099-EXIT
00640          VARYING WS-CITY-INDEX FROM PI-CITY-LENGTH BY -1
00641              UNTIL WS-CITY-INDEX = ZERO.
00642
00643      IF CITY-NOT-FOUND
00644          GO TO 4010-READNEXT.
00645
00646  4017-CONTINUE.
00647      IF OPTION-ONE-SELECTED
00648          GO TO 4018-CONTINUE.
00649
00650      IF OPTION-TWO-SELECTED
00651          IF NL-RECORD-TYPE  IS EQUAL TO  PI-SC-RECORD-TYPE
00652              GO TO 4018-CONTINUE
00653          ELSE
00654              GO TO 4010-READNEXT.
00655
00656      IF OPTION-THREE-SELECTED
00657          IF NL-RECORD-TYPE  IS EQUAL TO  PI-SC-RECORD-TYPE
00658              GO TO 4021-CONTINUE
00659          ELSE
00660              GO TO 4010-READNEXT.
00661
00662  4018-CONTINUE.
00663      MOVE PI-NAME-LOOKUP-KEY     TO  WS-KEY-INPUT.
00664
00665      SET KEY-INDEX
00666          KEY-INDEX2              TO  +1.
00667
00668  4020-COMPARE-KEY.
00669      IF WS-KH-CHAR (KEY-INDEX)
00670          IS NOT EQUAL TO  WS-KI-CHAR (KEY-INDEX2)
00671              GO TO 4700-END-OF-BROWSE.
00672
00673      IF KEY-INDEX  IS LESS THAN  PI-KEY-LENGTH
00674          SET KEY-INDEX
00675              KEY-INDEX2  UP  BY  +1
00676          GO TO 4020-COMPARE-KEY.
00677
00678 ******************************************************************
00679 *        SECURITY CHECK FOR CARRIER AND ACCOUNT NUMBER           *
00680 *                        04/04/84                                *
00681 ******************************************************************
00682
00683 ******************************************************************
00684 *    IF THE SECURITY CHECK ROUTINE IS CHANGED HERE, YOU MUST     *
00685 *        ALSO CHANGE THE SECURITY CHECK ROUTINE IN               *
00686 *        7100-READNEXT-PF2.                                      *
00687 ******************************************************************
00688
00689  4021-CONTINUE.
00690      IF NL-RECORD-TYPE  IS EQUAL TO  'R'
00691          GO TO 4090-MOVE-DATA.
00692
00693      IF PI-NO-CARRIER-SECURITY
00694        AND PI-NO-ACCOUNT-SECURITY
00695          GO TO 4090-MOVE-DATA.
00696
00697      IF PI-CARRIER-SECURITY  IS GREATER THAN  SPACES
00698          NEXT SENTENCE
00699      ELSE
00700          GO TO 4022-CONTINUE.
00701
00702      IF NL-RECORD-TYPE  IS EQUAL TO  'A'
00703          IF NL-AM-CARRIER  IS EQUAL TO  PI-CARRIER-SECURITY
00704              GO TO 4022-CONTINUE
00705          ELSE
00706              GO TO 4010-READNEXT.
00707
00708      IF NL-CO-CARRIER  IS EQUAL TO  PI-CARRIER-SECURITY
00709          NEXT SENTENCE
00710      ELSE
00711          GO TO 4010-READNEXT.
00712
00713  4022-CONTINUE.
00714      IF PI-ACCOUNT-SECURITY  IS GREATER THAN  SPACES
00715          NEXT SENTENCE
00716      ELSE
00717          GO TO 4090-MOVE-DATA.
00718
00719      IF NL-RECORD-TYPE  IS EQUAL TO  'A'
00720          IF NL-AM-ACCOUNT  IS EQUAL TO  PI-ACCOUNT-SECURITY
00721              GO TO 4090-MOVE-DATA
00722          ELSE
00723              GO TO 4010-READNEXT.
00724
00725      IF NL-CO-ACCOUNT  IS EQUAL TO  PI-ACCOUNT-SECURITY
00726          NEXT SENTENCE
00727      ELSE
00728          GO TO 4010-READNEXT.
00729
00730  4090-MOVE-DATA.
CIDMOD     MOVE LOW-VALUES             TO  ACCOUNT-KEY-LINE
CIDMOD
00731      IF LCP-ONCTR-01 =  0
00732          ADD 1 TO LCP-ONCTR-01
00733          MOVE +1                 TO  WS-NAME-SW
00734          MOVE PI-NAME-LOOKUP-KEY TO  PI-LIN1-NAME-LOOKUP-KEY.
00735
00736      ADD +1                      TO  PI-LINE-COUNT.
00737
00738      IF EL659B-INDEX  IS EQUAL TO  1
00739          MOVE NL-RECORD-KEY      TO  PI-START-NAME-LOOKUP-KEY.
00740
00741      MOVE NL-RECORD-KEY          TO  PI-END-NAME-LOOKUP-KEY.
00742      MOVE NL-RECORD-TYPE         TO  EL659B-TYPE (EL659B-INDEX).
00743      MOVE NL-NAME                TO  EL659B-NAME (EL659B-INDEX).
00744      MOVE NL-CITY                TO  EL659B-CITY (EL659B-INDEX).
00745      MOVE NL-ST                  TO  EL659B-ST   (EL659B-INDEX).
00746
00747      IF NL-RECORD-TYPE  IS EQUAL TO  'C'
00748          GO TO 4100-MOVE-COMP.
00749
00750      IF NL-RECORD-TYPE  IS EQUAL TO  'R'
00751          GO TO 4110-MOVE-REIN.
00752
00753      MOVE SPACES                 TO  ACCOUNT-KEY-LINE.
00754      MOVE 'CARR '                TO  AKL-FILL-1.
00755      MOVE NL-AM-CARRIER          TO  AKL-CARRIER.
00756      MOVE ' GROUP '              TO  AKL-FILL-2.
00757      MOVE NL-AM-GROUPING         TO  AKL-GROUPING.
00758      MOVE ' STATE '              TO  AKL-FILL-3.
00759      MOVE NL-AM-STATE            TO  AKL-STATE.
00760      MOVE ' ACCOUNT '            TO  AKL-FILL-4.
00761      MOVE NL-AM-ACCOUNT          TO  AKL-ACCOUNT.
00762      MOVE ACCOUNT-KEY-LINE       TO  EL659B-KEY (EL659B-INDEX).
00763
00764      GO TO 4200-CONTINUE.
00765
00766  4099-CHECK-CITY.
00767      IF WS-NL-CITY-CHAR (WS-CITY-INDEX) NOT =
00768         WS-PI-CITY-CHAR (WS-CITY-INDEX)
00769           MOVE 'X'               TO WS-COMPARE-INDICATOR.
00770
00771  4099-EXIT.
00772       EXIT.
00773
00774  4100-MOVE-COMP.
CIDMOD     MOVE LOW-VALUES             TO  COMPENSATION-KEY-LINE
CIDMOD
00775      MOVE 'CARR '                TO  CKL-FILL-1.
00776      MOVE NL-CO-CARRIER          TO  CKL-CARRIER.
00777      MOVE ' GROUP '              TO  CKL-FILL-2.
00778
00779      IF NL-CO-GROUPING IS EQUAL TO LOW-VALUES
00780          MOVE SPACES             TO  CKL-GROUPING
00781      ELSE
00782          MOVE NL-CO-GROUPING     TO  CKL-GROUPING.
00783
00784      MOVE ' FIN RESP '           TO  CKL-FILL-3.
00785
00786      IF NL-CO-RESP-NO  IS EQUAL TO  LOW-VALUES
00787          MOVE SPACES             TO  CKL-RESP
00788      ELSE
00789          MOVE NL-CO-RESP-NO      TO  CKL-RESP.
00790
00791      MOVE ' ACCOUNT '            TO  CKL-FILL-4.
00792
00793      IF NL-CO-ACCOUNT  IS EQUAL TO  LOW-VALUES
00794          MOVE SPACES             TO  CKL-ACCOUNT
00795      ELSE
00796          MOVE NL-CO-ACCOUNT      TO  CKL-ACCOUNT.
00797
00798      MOVE ' TYPE '               TO  CKL-FILL-5.
00799      MOVE NL-CO-TYPE             TO  CKL-TYPE.
00800      MOVE COMPENSATION-KEY-LINE  TO  EL659B-KEY (EL659B-INDEX).
00801
00802      GO TO 4200-CONTINUE.
00803
00804  4110-MOVE-REIN.
CIDMOD     MOVE LOW-VALUES             TO  REINSURANCE-KEY-LINE
CIDMOD
00805      IF NL-RE-LEVEL (12)  IS NOT EQUAL TO  SPACES
00806          MOVE '*'                TO  EL659B-AST (EL659B-INDEX).
00807
00808      MOVE 'TABLE '               TO  RKL-FILL-1.
00809      MOVE NL-RE-TABLE            TO  RKL-REIN-TABLE.
00810      MOVE ' COMP '               TO  RKL-FILL-2.
00811      MOVE NL-RE-COMP             TO  RKL-REIN-COMP.
00812      MOVE ' SUB '                TO  RKL-FILL-3.
00813      MOVE NL-RE-CO-SUB           TO  RKL-REIN-COMP-SUB.
00814      MOVE ' ON LEVEL'            TO  RKL-FILL-4.
00815
00816      IF NL-RE-LEVEL (1)  IS NUMERIC
00817          IF NL-RE-LEVEL (1)  IS GREATER THAN  ZERO
00818              MOVE ' '            TO  RKL-FILL-5 (1)
00819              MOVE NL-RE-LEVEL (1)
00820                                  TO  RKL-LEVEL (1).
00821
00822      IF NL-RE-LEVEL (2)  IS NUMERIC
00823          IF NL-RE-LEVEL (2)  IS GREATER THAN  ZERO
00824              MOVE ' '            TO  RKL-FILL-5 (2)
00825              MOVE NL-RE-LEVEL (2)
00826                                  TO  RKL-LEVEL (2).
00827
00828      IF NL-RE-LEVEL (3)  IS NUMERIC
00829          IF NL-RE-LEVEL (3)  IS GREATER THAN  ZERO
00830              MOVE ' '            TO  RKL-FILL-5 (3)
00831              MOVE NL-RE-LEVEL (3)
00832                                  TO  RKL-LEVEL (3).
00833
00834      IF NL-RE-LEVEL (4)  IS NUMERIC
00835          IF NL-RE-LEVEL (4)  IS GREATER THAN  ZERO
00836              MOVE ' '            TO  RKL-FILL-5 (4)
00837              MOVE NL-RE-LEVEL (4)
00838                                  TO  RKL-LEVEL (4).
00839
00840      IF NL-RE-LEVEL (5)  IS NUMERIC
00841          IF NL-RE-LEVEL (5)  IS GREATER THAN  ZERO
00842              MOVE ' '            TO  RKL-FILL-5 (5)
00843              MOVE NL-RE-LEVEL (5)
00844                                  TO  RKL-LEVEL (5).
00845
00846      IF NL-RE-LEVEL (6)  IS NUMERIC
00847          IF NL-RE-LEVEL (6)  IS GREATER THAN  ZERO
00848              MOVE ' '            TO  RKL-FILL-5 (6)
00849              MOVE NL-RE-LEVEL (6)
00850                                  TO  RKL-LEVEL (6).
00851
00852      IF NL-RE-LEVEL (7)  IS NUMERIC
00853          IF NL-RE-LEVEL (7)  IS GREATER THAN  ZERO
00854              MOVE ' '            TO  RKL-FILL-5 (7)
00855              MOVE NL-RE-LEVEL (7)
00856                                  TO  RKL-LEVEL (7).
00857
00858      IF NL-RE-LEVEL (8)  IS NUMERIC
00859          IF NL-RE-LEVEL (8)  IS GREATER THAN  ZERO
00860              MOVE ' '            TO  RKL-FILL-5 (8)
00861              MOVE NL-RE-LEVEL (8)
00862                                  TO  RKL-LEVEL (8).
00863
00864      IF NL-RE-LEVEL (9)  IS NUMERIC
00865          IF NL-RE-LEVEL (9)  IS GREATER THAN  ZERO
00866              MOVE ' '            TO  RKL-FILL-5 (9)
00867              MOVE NL-RE-LEVEL (9)
00868                                  TO  RKL-LEVEL (9).
00869
00870      IF NL-RE-LEVEL (10)  IS NUMERIC
00871          IF NL-RE-LEVEL (10)  IS GREATER THAN  ZERO
00872              MOVE ' '            TO  RKL-FILL-5 (10)
00873              MOVE NL-RE-LEVEL (10)
00874                                  TO  RKL-LEVEL (10).
00875
00876      IF NL-RE-LEVEL (11)  IS NUMERIC
00877          IF NL-RE-LEVEL (11)  IS GREATER THAN  ZERO
00878              MOVE ' '            TO  RKL-FILL-5 (11)
00879              MOVE NL-RE-LEVEL (11)
00880                                  TO  RKL-LEVEL (11).
00881
00882      MOVE REINSURANCE-KEY-LINE   TO  EL659B-KEY (EL659B-INDEX).
00883
00884  4200-CONTINUE.
00885      PERFORM 6000-SET-ATTRB  THRU  6000-EXIT.
00886
00887      IF EL659B-INDEX  IS LESS THAN  +8
00888          SET EL659B-INDEX  UP  BY  +1
00889          GO TO 4010-READNEXT.
00890
00891      GO TO 4900-ENDBROWSE.
00892
00893  4600-ENDFILE.
00894      MOVE ER-0130                TO  EMI-ERROR.
00895
00896  4700-END-OF-BROWSE.
00897      ADD +1                      TO  PI-END-OF-FILE.
00898
00899  4900-ENDBROWSE.
00900      ADD 1                       TO  PI-SCREEN-COUNT.
00901
00902      
      * EXEC CICS ENDBR
00903 *        DATASET  (PI-DSID)
00904 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00002368' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00905
00906  4910-SEND-MAP.
00907      IF WS-NO-NAME-FOUND
00908          MOVE +9                 TO  PI-BROWSE-SW
00909          PERFORM 9400-CLEAR.
00910
00911      MOVE -1                     TO  BSELL.
00912
00913      PERFORM 8100-SEND-INITIAL-MAP.
00914
00915      GO TO 9100-RETURN-TRAN.
00916
00917  4990-EXIT.
00918      EXIT.
00919  EJECT
00920  6000-SET-ATTRB  SECTION.
00921      MOVE AL-SABON           TO  EL659B-TYPE-ATTR (EL659B-INDEX)
00922                                  EL659B-NAME-ATTR (EL659B-INDEX)
00923                                  EL659B-CITY-ATTR (EL659B-INDEX)
00924                                  EL659B-ST-ATTR   (EL659B-INDEX)
00925                                  EL659B-AST-ATTR  (EL659B-INDEX).
00926      MOVE AL-SANON           TO  EL659B-KEY-ATTR  (EL659B-INDEX).
00927
00928      MOVE AL-UNNON               TO  BSELA.
00929
00930  6000-EXIT.
00931      EXIT.
00932  EJECT
00933  7000-PF2-POSITION  SECTION.
00934      
      * EXEC CICS IGNORE CONDITION
00935 *        DUPKEY
00936 *    END-EXEC.
      *    MOVE '"*$                   !   #00002400' TO DFHEIV0
           MOVE X'222A24202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00937
00938      
      * EXEC CICS HANDLE CONDITION
00939 *        NOTFND  (8700-NOT-FOUND)
00940 *    END-EXEC.
      *    MOVE '"$I                   ! $ #00002404' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303032343034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00941
00942      COMPUTE WS-CALC-RDNXT  =  PI-SCREEN-COUNT  *  7.
00943
00944      MOVE PI-1ST-KEY             TO  PI-NAME-LOOKUP-KEY.
00945      MOVE ZERO                   TO  PI-END-OF-FILE.
00946
00947      
      * EXEC CICS STARTBR
00948 *        DATASET    (PI-DSID)
00949 *        RIDFLD     (PI-NAME-LOOKUP-KEY)
00950 *        GENERIC
00951 *        EQUAL
00952 *        KEYLENGTH  (PI-KEY-LENGTH)
00953 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,   KG    E          &   #00002413' TO DFHEIV0
           MOVE X'262C2020204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 PI-NAME-LOOKUP-KEY, 
                 PI-KEY-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00954
00955  7100-READNEXT-PF2.
00956      IF WS-CALC-RDNXT  IS GREATER THAN  ZERO
00957          NEXT SENTENCE
00958      ELSE
00959          GO TO 7999-EXIT.
00960
00961      
      * EXEC CICS READNEXT
00962 *        DATASET  (PI-DSID)
00963 *        RIDFLD   (PI-NAME-LOOKUP-KEY)
00964 *        SET      (ADDRESS OF NAME-LOOKUP-MASTER)
00965 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00002427' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-NAME-LOOKUP-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF NAME-LOOKUP-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00966
00967 ******************************************************************
00968 *    IF THE SECURITY CHECKING ROUTINE, INITIAL COMPARE ROUTINE   *
00969 *        OR THE ACCOUNT COMPARE ROUTINE IS CHANGED HERE, YOU     *
00970 *        MUST ALSO CHANGE THE CORRESPONDING ROUTINE IN           *
00971 *        PARAGRAPH 4020-COMPARE-KEY OR 4030-CHECK-OPTION.        *
00972 ******************************************************************
00973
00974      IF NL-COMPANY-CD  IS EQUAL TO  PI-COMPANY-CD
00975          NEXT SENTENCE
00976      ELSE
00977          GO TO 7100-READNEXT-PF2.
00978
00979      IF OPTION-ONE-SELECTED
00980          GO TO 7110-CHECK-SECURITY.
00981
00982      IF OPTION-TWO-SELECTED OR
00983         OPTION-THREE-SELECTED
00984          IF NL-RECORD-TYPE  IS EQUAL TO  PI-SC-RECORD-TYPE
00985              GO TO 7110-CHECK-SECURITY
00986          ELSE
00987              GO TO 7100-READNEXT-PF2.
00988
00989  7110-CHECK-SECURITY.
00990      IF NL-RECORD-TYPE  IS EQUAL TO  'R'
00991          GO TO 7190-COMPUTE.
00992
00993      IF PI-NO-CARRIER-SECURITY
00994        AND PI-NO-ACCOUNT-SECURITY
00995          GO TO 7190-COMPUTE.
00996
00997      IF PI-CARRIER-SECURITY  IS GREATER THAN  SPACES
00998          NEXT SENTENCE
00999      ELSE
01000          GO TO 7120-CONTINUE.
01001
01002      IF NL-RECORD-TYPE  IS EQUAL TO  'A'
01003          IF NL-AM-CARRIER  IS EQUAL TO  PI-CARRIER-SECURITY
01004              GO TO 7120-CONTINUE
01005          ELSE
01006              GO TO 7100-READNEXT-PF2.
01007
01008      IF NL-CO-CARRIER  IS EQUAL TO  PI-CARRIER-SECURITY
01009          NEXT SENTENCE
01010      ELSE
01011          GO TO 7100-READNEXT-PF2.
01012
01013  7120-CONTINUE.
01014      IF PI-ACCOUNT-SECURITY  IS GREATER THAN  SPACES
01015          NEXT SENTENCE
01016      ELSE
01017          GO TO 7190-COMPUTE.
01018
01019      IF NL-RECORD-TYPE  IS EQUAL TO  'A'
01020          IF NL-AM-ACCOUNT  IS EQUAL TO  PI-ACCOUNT-SECURITY
01021              GO TO 7190-COMPUTE
01022          ELSE
01023              GO TO 7100-READNEXT-PF2.
01024
01025      IF NL-CO-ACCOUNT  IS EQUAL TO  PI-ACCOUNT-SECURITY
01026          NEXT SENTENCE
01027      ELSE
01028          GO TO 7100-READNEXT-PF2.
01029
01030  7190-COMPUTE.
01031      COMPUTE WS-CALC-RDNXT  =  WS-CALC-RDNXT  -  1.
01032
01033      GO TO 7100-READNEXT-PF2.
01034
01035  7999-EXIT.
01036      EXIT.
01037  EJECT
01038  8100-SEND-INITIAL-MAP  SECTION.
01039      MOVE SAVE-DATE              TO  BDATEO.
01040      MOVE EIBTIME                TO  TIME-IN.
01041      MOVE TIME-OUT               TO  BTIMEO.
101501     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101501     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01042
01043      IF EMI-ERROR  IS NOT EQUAL TO  ZERO
01044          PERFORM 9900-ERROR-FORMAT.
01045
01046      MOVE EMI-MESSAGE-AREA (1)   TO  BEMSG1O.
01047
01048      
      * EXEC CICS SEND
01049 *        FROM    (EL659BO)
01050 *        MAPSET  (WS-MAPSET-NAME)
01051 *        MAP     (WS-MAP-NAME)
01052 *        CURSOR
01053 *        ERASE
01054 *    END-EXEC.
           MOVE LENGTH OF
            EL659BO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00002516' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL659BO, 
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
           
01055
01056  8100-EXIT.
01057      EXIT.
01058
01059  EJECT
01060  8200-SEND-DATAONLY  SECTION.
01061      MOVE SAVE-DATE              TO  BDATEO.
01062      MOVE EIBTIME                TO  TIME-IN.
01063      MOVE TIME-OUT               TO  BTIMEO.
101501     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101501     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01064
01065      IF EMI-ERROR  IS NOT EQUAL TO  ZERO
01066          PERFORM 9900-ERROR-FORMAT.
01067
01068      MOVE EMI-MESSAGE-AREA (1)   TO  BEMSG1O.
01069
01070      
      * EXEC CICS SEND DATAONLY
01071 *        FROM    (EL659BO)
01072 *        MAPSET  (WS-MAPSET-NAME)
01073 *        MAP     (WS-MAP-NAME)
01074 *        CURSOR
01075 *    END-EXEC.
           MOVE LENGTH OF
            EL659BO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00002540' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL659BO, 
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
           
01076
01077  8200-EXIT.
01078      EXIT.
01079  EJECT
01080  8300-SEND-TEXT  SECTION.
01081      
      * EXEC CICS SEND TEXT
01082 *        FROM    (LOGOFF-TEXT)
01083 *        LENGTH  (LOGOFF-LENGTH)
01084 *        ERASE
01085 *        FREEKB
01086 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00002551' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353531' TO DFHEIV0(25:11)
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
           
01087
01088      
      * EXEC CICS RETURN
01089 *    END-EXEC.
      *    MOVE '.(                    &   #00002558' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01090
01091  8300-EXIT.
01092      EXIT.
01093
01094  8500-DATE-CONVERSION  SECTION.
01095      
      * EXEC CICS LINK
01096 *        PROGRAM   ('ELDATCV')
01097 *        COMMAREA  (DATE-CONVERSION-DATA)
01098 *        LENGTH    (DC-COMM-LENGTH)
01099 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00002565' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01100
01101  8500-EXIT.
01102      EXIT.
01103  EJECT
01104  8650-WRITE-SECURITY-TEMP-STORE  SECTION.
01105      
      * EXEC CICS HANDLE CONDITION
01106 *        QIDERR  (8651-WRITE-SECURITY)
01107 *    END-EXEC.
      *    MOVE '"$N                   ! % #00002575' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303032353735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01108
01109      MOVE EIBTRMID               TO  QID.
01110
01111  8651-WRITE-SECURITY.
01112      
      * EXEC CICS WRITEQ TS
01113 *        QUEUE   (QID)
01114 *        FROM    (SECURITY-CONTROL)
01115 *        LENGTH  (SC-COMM-LENGTH)
01116 *        ITEM    (QID-ITEM)
01117 *    END-EXEC.
      *    MOVE '*" I                  ''   #00002582' TO DFHEIV0
           MOVE X'2A2220492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032353832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 QID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 QID-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01118
01119      MOVE QID                    TO  PI-SECURITY-TEMP-STORE-ID.
01120
01121      IF PI-PROCESSOR-ID  IS EQUAL TO  'LGXX'
01122          MOVE ALL 'Y'            TO  SC-CREDIT-CODES
01123                                      SC-CLAIMS-CODES
01124                                      PI-PROCESSOR-USER-ALMIGHTY.
01125
01126  8650-EXIT.
01127      EXIT.
01128  EJECT
01129  8700-NOT-FOUND  SECTION.
01130      PERFORM 8800-INITIALIZE-MAP
01131          VARYING  EL659B-INDEX  FROM  +1  BY  +1
01132              UNTIL  EL659B-INDEX  IS GREATER THAN  +8.
01133
01134      MOVE -1                     TO  BSELL.
01135      MOVE ER-0673                TO  EMI-ERROR.
01136
01137      PERFORM 8100-SEND-INITIAL-MAP.
01138
01139      GO TO 9100-RETURN-TRAN.
01140
01141  8700-EXIT.
01142      EXIT.
01143
01144  8800-INITIALIZE-MAP  SECTION.
01145      MOVE LOW-VALUES         TO  EL659B-MAP-LINE (EL659B-INDEX).
01146
01147  8800-EXIT.
01148      EXIT.
01149  EJECT
01150  9000-RETURN-CICS  SECTION.
01151      MOVE 'EL005'                TO  THIS-PGM.
01152      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
01153
01154      GO TO 9300-XCTL.
01155
01156  9000-EXIT.
01157      EXIT.
01158
01159  9100-RETURN-TRAN  SECTION.
01160      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
01161      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.
01162      MOVE EIBAID                 TO  PI-PREV-AID.
01163
01164      
      * EXEC CICS RETURN
01165 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
01166 *        LENGTH    (PI-COMM-LENGTH)
01167 *        TRANSID   (WS-TRANS-ID)
01168 *    END-EXEC.
      *    MOVE '.(CT                  &   #00002634' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01169
01170  9100-EXIT.
01171      EXIT.
01172
01173  9300-XCTL  SECTION.
01174      MOVE DFHENTER               TO  EIBAID.
01175
01176      
      * EXEC CICS XCTL
01177 *        PROGRAM   (THIS-PGM)
01178 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
01179 *        LENGTH    (PI-COMM-LENGTH)
01180 *    END-EXEC.
      *    MOVE '.$C                   $   #00002646' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 THIS-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01181
01182  9300-EXIT.
01183      EXIT.
01184  EJECT
01185  9400-CLEAR  SECTION.
01186      MOVE PI-RETURN-TO-PROGRAM   TO  THIS-PGM.
01187
01188      GO TO 9300-XCTL.
01189
01190  9400-EXIT.
01191      EXIT.
01192
01193  9600-PGMIDERR  SECTION.
01194      
      * EXEC CICS HANDLE CONDITION
01195 *        PGMIDERR  (8300-SEND-TEXT)
01196 *    END-EXEC.
      *    MOVE '"$L                   ! & #00002664' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303032363634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01197
01198      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM
01199                                      LOGOFF-PGM.
01200      MOVE 'EL005'                TO  THIS-PGM.
01201      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
01202      MOVE SPACES                 TO  PI-ENTRY-CD-1.
01203
01204      GO TO 9300-XCTL.
01205
01206  9600-EXIT.
01207      EXIT.
01208  EJECT
01209  9900-ERROR-FORMAT  SECTION.
01210      IF EMI-ERRORS-COMPLETE
01211          GO TO 9900-EXIT.
01212
01213      
      * EXEC CICS LINK
01214 *        PROGRAM   ('EL001')
01215 *        COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
01216 *        LENGTH    (EMI-COMM-LENGTH)
01217 *    END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   ''   #00002683' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01218
01219  9900-EXIT.
01220      EXIT.
01221
01222  9990-ERROR  SECTION.
01223      MOVE DFHEIBLK               TO  EMI-LINE1.
01224
01225      
      * EXEC CICS LINK
01226 *        PROGRAM  ('EL004')
01227 *        COMMAREA (EMI-LINE1)
01228 *        LENGTH   (72)
01229 *    END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00002695' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032363935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01230
01231      MOVE -1                     TO  BSELL.
01232
01233      PERFORM 8100-SEND-INITIAL-MAP.
01234
01235      GO TO 9100-RETURN-TRAN.
01236
01237  9990-EXIT.
01238      EXIT.
01239
01240  9999-LAST-PARAGRAPH  SECTION.
01241      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6592' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.


       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6592' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMIDERR,
                     8700-NOT-FOUND,
                     4600-ENDFILE,
                     4015-DUPKEY,
                     9990-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 8700-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 8700-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 8651-WRITE-SECURITY
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6592' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
