00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL659 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/14/96 12:01:56.
00007 *                            VMOD=2.007
00008 *
00008 *AUTHOR.
00009 *               LOGIC, INC.
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
00025 *        THIS PROGRAM PROVIDES THE QUALIFICATION NECESSARY FOR
00026 *    THE ACCOUNT MASTER (ACCOUNT NAME AND/OR MAIL TO NAME),
00027 *    COMPENSATION MASTER (ACCOUNT NAME AND/OR MAIL TO NAME),
00028 *    AND REINSURANCE COMPANY NAME (WITH LEVEL NUMBERS) LOOK-UP.
00029
00030 *    SCREENS     - EL659A - ACCOUNT/COMPANY NAME REFERENCE
00031
00032 *    ENTERED BY  - EL601 - SYSTEM ADMINISTRATION MENU
00033 *                  EL650 - ACCOUNT MAINTENANCE
00034 *                  EL652 - COMPENSATION MAINTENANCE
00035 *                  EL651 - REINSURANCE TABLE MAINTENANCE
00036
00037 *    EXIT TO     - CALLING PROGRAM
00038 *                  EL6592 - ACCOUNT/COMPANY NAME LOOK-UP
00039
00040 *    INPUT FILE  - ERNAME - ACCOUNT/COMPANY NAME XREF FILE
00041
00042 *    OUTPUT FILE - NONE
00043
00044 *    COMMAREA    - PASSED.  IF AN ACCOUNT/COMPANY IS SELECTED,
00045 *                  THE CONTROL OF THAT ACCOUNT/COMPANY IS PLACED
00046 *                  IN THE APPROPRIATE FIELDS OF THE COMMAREA FOR
00047 *                  REFERENCE BY SUCCESSIVE PROGRAMS.  THE PROGRAM
00048 *                  WORK AREA OF THE COMMAREA IS USED TO PASS THE
00049 *                  RECORD KEY INFORMATION NEEDED BY EL6592.
00050
00051 *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL601.  ON
00052 *                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE
00053 *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVE
00054 *                  ENTRIES (XCTL FROM CICS VIA EX65) THE SCREEN
00055 *                  WILL BE READ AND ACTION WILL BE BASED ON THE
00056 *                  OPTION INDICATED.
00057  EJECT
081905******************************************************************
081905*                   C H A N G E   L O G
081905*
081905* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
081905*-----------------------------------------------------------------
081905*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
081905* EFFECTIVE    NUMBER
081905*-----------------------------------------------------------------
081905* 081905  IR2005041400004  PEMA  ADD DELETE OF TEMP STORAGE ID'S
081905******************************************************************
00058  ENVIRONMENT DIVISION.
00059  DATA DIVISION.
00060  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00061
00062  77  FILLER  PIC  X(32) VALUE '********************************'.
00063  77  FILLER  PIC  X(32) VALUE '*    EL659 WORKING STORAGE     *'.
00064  77  FILLER  PIC  X(32) VALUE '*********** VMOD=2.007 *********'.
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
00074  01  FILLER                  COMP-3.
00075      12  TIME-IN                     PIC S9(07)  VALUE ZERO.
00076      12  TIME-OUT  REDEFINES
00077          TIME-IN                     PIC S9(03)V9(4).
00078
00079  01  FILLER                  COMP SYNC.
00080      12  SC-ITEM                 PIC S9(04)      VALUE +0001.
00081
00082  01  FILLER.
081905     12  WS-RESPONSE             PIC S9(8)   COMP.
081905         88  RESP-NORMAL                  VALUE +00.
081905         88  RESP-NOTFND                  VALUE +13.
081905         88  RESP-NOTOPEN                 VALUE +19.
081905         88  RESP-ENDFILE                 VALUE +20.
00083      12  WS-MAPSET-NAME          PIC  X(08)      VALUE 'EL659S'.
00084      12  WS-MAP-NAME             PIC  X(08)      VALUE 'EL659A'.
00085      12  FILLER  REDEFINES  WS-MAP-NAME.
00086          16  FILLER              PIC  X(02).
00087          16  WS-MAP-NUMBER       PIC  X(04).
00088          16  FILLER              PIC  X(02).
00089      12  THIS-PGM                PIC  X(08)      VALUE 'EL659'.
00090      12  WS-ERNAME-DSID          PIC  X(08)      VALUE 'ERNAME'.
00091      12  WS-TRANS-ID             PIC  X(04)      VALUE 'EX65'.
00092      12  WS-INPUT-FIELD          PIC  X(50)      VALUE SPACES.
00093      12  WS-INPUT-CHAR  REDEFINES
00094          WS-INPUT-FIELD          PIC  X(01)
00095              OCCURS  50  TIMES       INDEXED  BY  INPUT-INDEX.
00096  EJECT
081905     12  WS-TEMP-STORAGE-KEY.
081905         16  WS-TSK-TERM-ID          PIC  X(04)  VALUE 'XXXX'.
081905         16  FILLER                  PIC  X(04)  VALUE '6592'.
081905     12  WS-TEMP-STORAGE-KEY1.
081905         16  WS-TSK-TERM-ID1         PIC  X(04)  VALUE 'XXXX'.
081905         16  FILLER                  PIC  X(04)  VALUE '659P'.
00097      12  ERROR-MESSAGES.
00098          16  ER-0004             PIC  X(04)      VALUE '0004'.
00099          16  ER-0008             PIC  X(04)      VALUE '0008'.
00100          16  ER-0029             PIC  X(04)      VALUE '0029'.
00101          16  ER-0070             PIC  X(04)      VALUE '0070'.
00102          16  ER-0647             PIC  X(04)      VALUE '0647'.
00103          16  ER-0671             PIC  X(04)      VALUE '0671'.
00104          16  ER-0673             PIC  X(04)      VALUE '0673'.
00105  EJECT
00106 *    COPY ELCINTF.
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
00107
00108 *    COPY ELC659PI.
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
00109          16  PI-EL659-TO-EL130-CNTRL.
00110              20  PI-EL659-CARRIER    PIC X.
00111              20  PI-EL659-GROUPING   PIC X(6).
00112              20  PI-EL659-STATE      PIC XX.
00113              20  PI-EL659-ACCOUNT    PIC X(10).
00114          16  FILLER                  PIC X(90).
00115  EJECT
00116 *    COPY ELCEMIB.
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
00117  EJECT
00118 *    COPY ELCDATE.
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
00119  EJECT
00120 *    COPY ELCLOGOF.
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
00121  EJECT
00122 *    COPY EL659S.
       01  EL659AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  ADATEL PIC S9(0004) COMP.
           05  ADATEF PIC  X(0001).
           05  FILLER REDEFINES ADATEF.
               10  ADATEA PIC  X(0001).
           05  ADATEI PIC  X(0008).
      *    -------------------------------
           05  ATIMEL PIC S9(0004) COMP.
           05  ATIMEF PIC  X(0001).
           05  FILLER REDEFINES ATIMEF.
               10  ATIMEA PIC  X(0001).
           05  ATIMEI PIC  X(0005).
      *    -------------------------------
           05  ANAMEL PIC S9(0004) COMP.
           05  ANAMEF PIC  X(0001).
           05  FILLER REDEFINES ANAMEF.
               10  ANAMEA PIC  X(0001).
           05  ANAMEI PIC  X(0030).
      *    -------------------------------
           05  ARTYPL PIC S9(0004) COMP.
           05  ARTYPF PIC  X(0001).
           05  FILLER REDEFINES ARTYPF.
               10  ARTYPA PIC  X(0001).
           05  ARTYPI PIC  X(0001).
      *    -------------------------------
           05  CITYL PIC S9(0004) COMP.
           05  CITYF PIC  X(0001).
           05  FILLER REDEFINES CITYF.
               10  CITYA PIC  X(0001).
           05  CITYI PIC  X(0015).
      *    -------------------------------
           05  STL PIC S9(0004) COMP.
           05  STF PIC  X(0001).
           05  FILLER REDEFINES STF.
               10  STA PIC  X(0001).
           05  STI PIC  X(0002).
      *    -------------------------------
           05  AEMSG1L PIC S9(0004) COMP.
           05  AEMSG1F PIC  X(0001).
           05  FILLER REDEFINES AEMSG1F.
               10  AEMSG1A PIC  X(0001).
           05  AEMSG1I PIC  X(0079).
      *    -------------------------------
           05  AEMSG2L PIC S9(0004) COMP.
           05  AEMSG2F PIC  X(0001).
           05  FILLER REDEFINES AEMSG2F.
               10  AEMSG2A PIC  X(0001).
           05  AEMSG2I PIC  X(0079).
      *    -------------------------------
           05  APFKL PIC S9(0004) COMP.
           05  APFKF PIC  X(0001).
           05  FILLER REDEFINES APFKF.
               10  APFKA PIC  X(0001).
           05  APFKI PIC  99.
       01  EL659AO REDEFINES EL659AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANAMEO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CITYO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APFKO PIC  99.
      *    -------------------------------
00123  EJECT
00124 *    COPY ELCATTR.
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
00125  EJECT
00126 *    COPY ELCAID.
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
00127
00128  01  FILLER  REDEFINES  DFHAID.
00129      12  FILLER                      PIC  X(08).
00130      12  PF-VALUES                   PIC  X(01)
00131              OCCURS  24  TIMES.
00132  EJECT
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
00134
00135  01  DFHCOMMAREA                     PIC  X(1024).
00136
00137 *01 PARMLIST             COMP  SYNC.
00138 *    12  FILLER                      PIC S9(09).
00139 *    12  ERNAME-POINTER              PIC S9(09).
00140  EJECT
00141 *    COPY ERCNAME.
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
00142  EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA
                                NAME-LOOKUP-MASTER.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL659' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00144
00145      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00146      MOVE '5'                    TO  DC-OPTION-CODE.
00147
00148      PERFORM 7000-DATE-CONVERSION.
00149
00150      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00151      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00152      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00153      MOVE +2                     TO  EMI-NUMBER-OF-LINES
00154                                      EMI-SWITCH2.
00155
00156 *    NOTE *******************************************************
00157 *         *                                                     *
00158 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL *
00159 *         *  FROM ANOTHER MODULE.                               *
00160 *         *                                                     *
00161 *         *******************************************************.
00162
00163      IF EIBCALEN  IS NOT GREATER THAN  ZERO
00164          MOVE UNACCESS-MSG       TO  LOGOFF-MSG
00165          GO TO 6000-SEND-TEXT.
00166
00167      
      * EXEC CICS HANDLE CONDITION
00168 *        PGMIDERR  (9400-PGMIDERR)
00169 *        NOTFND    (1020-NOTFND)
00170 *        ENDFILE   (1020-NOTFND)
00171 *        ERROR     (9600-ERROR)
00172 *    END-EXEC.
      *    MOVE '"$LI''.                ! " #00001222' TO DFHEIV0
           MOVE X'22244C49272E202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031323232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00173  EJECT
00174  0100-MAIN-LOGIC.
00175      IF PI-CALLING-PROGRAM  IS NOT EQUAL TO  THIS-PGM
00176          IF PI-RETURN-TO-PROGRAM  IS NOT EQUAL TO  THIS-PGM
00177              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6
00178              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5
00179              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4
00180              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3
00181              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2
00182              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1
00183              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM
00184              MOVE THIS-PGM              TO  PI-CALLING-PROGRAM
00185          ELSE
00186              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM
00187              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM
00188              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1
00189              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2
00190              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3
00191              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4
00192              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5
00193              MOVE SPACES                TO  PI-SAVED-PROGRAM-6
00194              PERFORM 3000-BUILD-SCREEN
00195      ELSE
00196          GO TO 0120-MAIN-LOGIC.
00197
00198  0110-MAIN-LOGIC.
00199 *    NOTE *******************************************************
00200 *         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      *
00201 *         *  INTERFACE BLOCK FOR THIS MODULE.                   *
00202 *         *******************************************************.
00203
00204      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA
00205                                      PI-CONTROL-IN-PROGRESS.
00206      MOVE ZERO                   TO  PI-1ST-TIME-SW
00207                                      PI-LINE-COUNT
00208                                      PI-BROWSE-SW
00209                                      PI-KEY-LENGTH
00210                                      PI-TS-ITEM
00211                                      PI-END-OF-FILE
00212                                      PI-START-SW.
00213
081905*    NOTE *******************************************************
081905*         *      I ADDED THE DELETES TO SEE IF THIS WILL FIX    *
081905*         *  THE IR 2005041400004. IT APPEARS THAT THE ONLY     *
081905*         *  TIME IT HAPPENS IS IN THIS PROGRAM AND EL6592      *
081905*         *  IT SEEMS LIKE EL6592 CLEANS UP AFTER ITSELF OKAY   *
081905*         *******************************************************
081905         MOVE EIBTRMID           TO  WS-TSK-TERM-ID
081905                                     WS-TSK-TERM-ID1
081905         
      * EXEC CICS DELETEQ TS
081905*            QUEUE  (WS-TEMP-STORAGE-KEY)
081905*            RESP   (WS-RESPONSE)
081905*        END-EXEC
      *    MOVE '*&                    #  N#00001277' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'204E233030303031323737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TEMP-STORAGE-KEY, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
081905         
      * EXEC CICS DELETEQ TS
081905*            QUEUE  (WS-TEMP-STORAGE-KEY1)
081905*            RESP   (WS-RESPONSE)
081905*        END-EXEC
      *    MOVE '*&                    #  N#00001281' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'204E233030303031323831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TEMP-STORAGE-KEY1, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00214 *    NOTE *******************************************************
00215 *         *      SEND THE INITIAL MAP OUT TO BEGIN PROCESSING   *
00216 *         *  FOR EL659.                                         *
00217 *         *******************************************************.
00218
00219      MOVE LOW-VALUES             TO  EL659AO.
00220
00221      PERFORM 4000-SEND-INITIAL-MAP.
00222
00223      GO TO 9100-RETURN-TRAN.
00224  EJECT
00225  0120-MAIN-LOGIC.
00226      IF PI-1ST-TIME-SW  IS NOT EQUAL TO  ZERO
00227          GO TO 0110-MAIN-LOGIC.
00228
00229 *    NOTE *******************************************************
00230 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- *
00231 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    *
00232 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         *
00233 *         *******************************************************.
00234
00235      IF EIBAID  IS EQUAL TO  DFHCLEAR
00236          GO TO 9300-CLEAR.
00237
00238      IF EIBAID  IS EQUAL TO  DFHPA1  OR  DFHPA2  OR  DFHPA3
00239          MOVE LOW-VALUES         TO  EL659AO
00240          MOVE -1                 TO  APFKL
00241          MOVE ER-0008            TO  EMI-ERROR
00242          PERFORM 5000-SEND-DATA-ONLY
00243          GO TO 9100-RETURN-TRAN.
00244
00245      
      * EXEC CICS RECEIVE
00246 *        INTO    (EL659AI)
00247 *        MAPSET  (WS-MAPSET-NAME)
00248 *        MAP     (WS-MAP-NAME)
00249 *    END-EXEC.
           MOVE LENGTH OF
            EL659AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00001316' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL659AI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00250
00251      IF APFKL  IS GREATER THAN  ZERO
00252          IF EIBAID  IS NOT EQUAL TO  DFHENTER
00253              MOVE ER-0004        TO  EMI-ERROR
00254              MOVE AL-UNBOF       TO  APFKA
00255              MOVE -1             TO  APFKL
00256              PERFORM 5000-SEND-DATA-ONLY
00257              GO TO 9100-RETURN-TRAN
00258          ELSE
00259              IF APFKO  IS NUMERIC
00260                  IF APFKO  IS GREATER THAN  0
00261                    AND APFKO  IS LESS THAN  25
00262                      MOVE PF-VALUES (APFKI)
00263                                  TO  EIBAID
00264                  ELSE
00265                      MOVE ER-0029
00266                                  TO  EMI-ERROR
00267                      MOVE AL-UNBOF
00268                                  TO  APFKA
00269                      MOVE -1     TO  APFKL
00270                      PERFORM 5000-SEND-DATA-ONLY
00271                      GO TO 9100-RETURN-TRAN.
00272
00273      IF EIBAID  IS EQUAL TO  DFHPF12
00274          MOVE 'EL010'            TO  THIS-PGM
00275          GO TO 9200-XCTL.
00276
00277      IF EIBAID  IS EQUAL TO  DFHPF23
00278          GO TO 9000-RETURN-CICS.
00279
00280      IF EIBAID  IS EQUAL TO  DFHPF24
00281          MOVE 'EL126'            TO  THIS-PGM
00282          GO TO 9200-XCTL.
00283
00284      IF EIBAID  IS EQUAL TO  DFHENTER
00285          NEXT SENTENCE
00286      ELSE
00287          MOVE ER-0008            TO  EMI-ERROR
00288          MOVE -1                 TO  APFKL
00289          PERFORM 5000-SEND-DATA-ONLY
00290          GO TO 9100-RETURN-TRAN.
00291  EJECT
00292  0130-MAIN-LOGIC.
00293      MOVE SPACES                 TO  PI-SELECTION-CRITERIA
00294                                      PI-NAME-LOOKUP-KEY
00295                                      PI-CITY
00296                                      PI-ST.
00297      MOVE PI-COMPANY-CD          TO  PI-SC-COMPANY-CD
00298                                      PI-NLK-COMPANY-CD.
00299
00300      IF PI-PROCESSOR-ID  IS EQUAL TO  'LGXX'
00301          NEXT SENTENCE
00302      ELSE
00303          
      * EXEC CICS READQ TS
00304 *            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00305 *            INTO    (SECURITY-CONTROL)
00306 *            LENGTH  (SC-COMM-LENGTH)
00307 *            ITEM    (SC-ITEM)
00308 *        END-EXEC
      *    MOVE '*$II   L              ''   #00001374' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00309          MOVE SC-CREDIT-DISPLAY (31)
00310                                  TO  PI-DISPLAY-CAP
00311          MOVE SC-CREDIT-UPDATE  (31)
00312                                  TO  PI-MODIFY-CAP
00313          IF NOT DISPLAY-CAP
00314              MOVE 'READ'         TO  SM-READ
00315              PERFORM 9700-SECURITY-VIOLATION
00316              MOVE ER-0070        TO  EMI-ERROR
00317              PERFORM 4000-SEND-INITIAL-MAP
00318              GO TO 9100-RETURN-TRAN.
00319  EJECT
00320  0200-DETERMINE-OPTION.
00321      MOVE ZERO                   TO  PI-OPTION.
00322      MOVE WS-ERNAME-DSID         TO  PI-DSID.
00323      MOVE +1                     TO  PI-KEY-LENGTH.
00324      MOVE +0                     TO  PI-CITY-LENGTH.
00325
00326      IF ANAMEL GREATER ZERO
00327          MOVE ANAMEI             TO  PI-SC-NAME
00328                                      WS-INPUT-FIELD
00329          PERFORM 0220-GET-KEY-LENGTH THRU 0229-EXIT
00330              VARYING INPUT-INDEX FROM ANAMEL BY -1
00331                  UNTIL WS-INPUT-CHAR (INPUT-INDEX)
00332                   NOT = SPACE
00333          ADD ANAMEL              TO  PI-KEY-LENGTH
00334          MOVE -1                 TO  ANAMEL
00335          MOVE '1'                TO  PI-OPTION.
00336
00337      IF ARTYPL GREATER ZERO
00338        IF ARTYPI = 'A' OR 'C' OR 'R'
00339            MOVE ARTYPI           TO  PI-SC-RECORD-TYPE
00340            IF PI-SC-NAME NOT = SPACES
00341                MOVE '2'          TO  PI-OPTION
00342              ELSE
00343                MOVE '3'          TO  PI-OPTION
00344          ELSE
00345            MOVE ER-0647          TO  EMI-ERROR
00346            MOVE ZERO             TO  ANAMEL
00347            MOVE -1               TO  ARTYPL
00348            PERFORM 5000-SEND-DATA-ONLY
00349            GO TO 9100-RETURN-TRAN.
00350
00351      IF CITYL GREATER ZERO
00352          MOVE CITYI              TO  PI-CITY
00353                                      WS-INPUT-FIELD
00354          PERFORM 0230-GET-CITY-LENGTH THRU 0239-EXIT
00355              VARYING INPUT-INDEX FROM CITYL BY -1
00356                  UNTIL WS-INPUT-CHAR (INPUT-INDEX)
00357                   NOT = SPACE
00358          ADD CITYL               TO  PI-CITY-LENGTH.
00359
00360      IF STL GREATER ZERO
00361          MOVE STI                TO  PI-ST.
00362
00363      MOVE PI-SELECTION-CRITERIA  TO  PI-NAME-LOOKUP-KEY.
00364
00365      GO TO 1000-READ-NAME-LOOKUP.
00366
00367  0220-GET-KEY-LENGTH.
00368      SUBTRACT +1                 FROM  ANAMEL.
00369
00370  0229-EXIT.
00371       EXIT.
00372
00373  0230-GET-CITY-LENGTH.
00374      SUBTRACT +1                 FROM  CITYL.
00375
00376  0239-EXIT.
00377       EXIT.
00378
00379  EJECT
00380  1000-READ-NAME-LOOKUP  SECTION.
00381      
      * EXEC CICS HANDLE CONDITION
00382 *        DUPKEY   (9200-XCTL)
00383 *        NOTFND   (1020-NOTFND)
00384 *        DSIDERR  (1010-DSIDERR)
00385 *    END-EXEC.
      *    MOVE '"$$I"                 ! # #00001452' TO DFHEIV0
           MOVE X'222424492220202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303031343532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00386
00387      MOVE +1                     TO  PI-START-SW.
00388
00389      
      * EXEC CICS READ
00390 *        DATASET    (PI-DSID)
00391 *        RIDFLD     (PI-NAME-LOOKUP-KEY)
00392 *        SET        (ADDRESS OF NAME-LOOKUP-MASTER)
00393 *        GENERIC
00394 *        EQUAL
00395 *        KEYLENGTH  (PI-KEY-LENGTH)
00396 *    END-EXEC.
      *    MOVE '&"S  KG    E          (   #00001460' TO DFHEIV0
           MOVE X'26225320204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-NAME-LOOKUP-KEY, 
                 PI-KEY-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF NAME-LOOKUP-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00397
00398      IF NL-COMPANY-CD  IS EQUAL TO  PI-COMPANY-CD
00399          NEXT SENTENCE
00400      ELSE
00401          GO TO 1020-NOTFND.
00402
00403      MOVE 'EL6592'               TO  THIS-PGM.
00404
00405      GO TO 9200-XCTL.
00406
00407  1010-DSIDERR.
00408      MOVE ER-0671                TO  EMI-ERROR.
00409
00410      PERFORM 5000-SEND-DATA-ONLY.
00411
00412      GO TO 9100-RETURN-TRAN.
00413
00414  1020-NOTFND.
00415      MOVE ER-0673                TO  EMI-ERROR.
00416
00417      PERFORM 5000-SEND-DATA-ONLY.
00418
00419      GO TO 9100-RETURN-TRAN.
00420
00421  EJECT
00422  3000-BUILD-SCREEN  SECTION.
00423 ******************************************************************
00424 *          REBUILD ORIGNAL SCREEN AND ERROR MESSAGE IF EL6592    *
00425 *          DID NOT FIND ANY CERTIFICATES DURING BROWSE OF FILE.  *
00426 ******************************************************************
00427
00428      IF EIBTRNID  IS EQUAL TO  WS-TRANS-ID
00429          NEXT SENTENCE
00430      ELSE
00431          GO TO 3099-EXIT.
00432
00433      IF PI-BROWSE-SW  IS EQUAL TO  +9
00434          NEXT SENTENCE
00435      ELSE
00436          GO TO 3099-EXIT.
00437
00438      MOVE LOW-VALUES             TO  EL659AO.
00439      MOVE ER-0673                TO  EMI-ERROR.
00440
00441      IF PI-ST GREATER SPACES
00442          MOVE PI-ST              TO  STO
00443          MOVE AL-UABON           TO  STA
00444          MOVE -1                 TO  STL.
00445
00446      IF PI-CITY GREATER SPACES
00447          MOVE PI-CITY            TO  CITYO
00448          MOVE AL-UABON           TO  CITYA
00449          MOVE -1                 TO  CITYL.
00450
00451      IF PI-SC-RECORD-TYPE GREATER SPACES
00452          MOVE PI-SC-RECORD-TYPE  TO  ARTYPO
00453          MOVE AL-UABON           TO  ARTYPA
00454          MOVE -1                 TO  ARTYPL.
00455
00456      IF PI-SC-NAME GREATER SPACES
00457          MOVE PI-SC-NAME         TO  ANAMEO
00458          MOVE AL-UABON           TO  ANAMEA
00459          MOVE -1                 TO  ANAMEL.
00460
00461  3040-INITIALIZE-WORK-AREAS.
00462      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA
00463                                      PI-CONTROL-IN-PROGRESS.
00464      MOVE ZERO                   TO  PI-1ST-TIME-SW
00465                                      PI-LINE-COUNT
00466                                      PI-BROWSE-SW
00467                                      PI-KEY-LENGTH
00468                                      PI-TS-ITEM
00469                                      PI-END-OF-FILE
00470                                      PI-START-SW.
00471
00472      PERFORM 5000-SEND-DATA-ONLY.
00473
00474      GO TO 9100-RETURN-TRAN.
00475
00476  3099-EXIT.
00477      EXIT.
00478  EJECT
00479  4000-SEND-INITIAL-MAP  SECTION.
00480      MOVE -1                     TO  ANAMEL.
00481      MOVE SAVE-DATE              TO  ADATEO.
00482      MOVE EIBTIME                TO  TIME-IN.
00483      MOVE TIME-OUT               TO  ATIMEO.
00484
00485      IF EMI-ERROR  IS NOT EQUAL TO  ZERO
00486          PERFORM 9500-ERROR-FORMAT.
00487
00488      MOVE EMI-MESSAGE-AREA (1)   TO  AEMSG1O.
00489      MOVE EMI-MESSAGE-AREA (2)   TO  AEMSG2O.
00490
00491      
      * EXEC CICS SEND
00492 *        FROM    (EL659AO)
00493 *        MAPSET  (WS-MAPSET-NAME)
00494 *        MAP     (WS-MAP-NAME)
00495 *        CURSOR
00496 *        ERASE
00497 *    END-EXEC.
           MOVE LENGTH OF
            EL659AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00001562' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL659AO, 
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
           
00498
00499  4099-EXIT.
00500      EXIT.
00501  EJECT
00502  5000-SEND-DATA-ONLY SECTION.
00503      MOVE SAVE-DATE              TO  ADATEO.
00504      MOVE EIBTIME                TO  TIME-IN.
00505      MOVE TIME-OUT               TO  ATIMEO.
00506
00507      IF EMI-ERROR  IS NOT EQUAL TO  ZERO
00508          PERFORM 9500-ERROR-FORMAT.
00509
00510      MOVE EMI-MESSAGE-AREA (1)   TO  AEMSG1O.
00511      MOVE EMI-MESSAGE-AREA (2)   TO  AEMSG2O.
00512
00513      
      * EXEC CICS SEND DATAONLY
00514 *        FROM    (EL659AO)
00515 *        MAPSET  (WS-MAPSET-NAME)
00516 *        MAP     (WS-MAP-NAME)
00517 *        CURSOR
00518 *    END-EXEC.
           MOVE LENGTH OF
            EL659AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00001584' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL659AO, 
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
           
00519
00520  5099-EXIT.
00521      EXIT.
00522  EJECT
00523  6000-SEND-TEXT  SECTION.
00524      
      * EXEC CICS SEND TEXT
00525 *        FROM    (LOGOFF-TEXT)
00526 *        LENGTH  (LOGOFF-LENGTH)
00527 *        ERASE
00528 *        FREEKB
00529 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00001595' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353935' TO DFHEIV0(25:11)
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
           
00530
00531      
      * EXEC CICS RETURN
00532 *        END-EXEC.
      *    MOVE '.(                    &   #00001602' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00533
00534  6099-EXIT.
00535      EXIT.
00536  EJECT
00537  7000-DATE-CONVERSION  SECTION.
00538      
      * EXEC CICS LINK
00539 *        PROGRAM   ('ELDATCV')
00540 *        COMMAREA  (DATE-CONVERSION-DATA)
00541 *        LENGTH    (DC-COMM-LENGTH)
00542 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00001609' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00543
00544  7099-EXIT.
00545      EXIT.
00546
00547  EJECT
00548  9000-RETURN-CICS  SECTION.
00549      MOVE 'EL005'                TO  THIS-PGM.
00550      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
00551
00552      GO TO 9200-XCTL.
00553
00554  9099-EXIT.
00555      EXIT.
00556
00557  9100-RETURN-TRAN  SECTION.
00558      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
00559      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.
00560
00561      
      * EXEC CICS RETURN
00562 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
00563 *        LENGTH    (PI-COMM-LENGTH)
00564 *        TRANSID   (WS-TRANS-ID)
00565 *    END-EXEC.
      *    MOVE '.(CT                  &   #00001632' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00566
00567  9199-EXIT.
00568      EXIT.
00569
00570  9200-XCTL  SECTION.
00571      MOVE DFHENTER               TO  EIBAID.
00572
00573      
      * EXEC CICS XCTL
00574 *        PROGRAM   (THIS-PGM)
00575 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
00576 *        LENGTH    (PI-COMM-LENGTH)
00577 *    END-EXEC.
      *    MOVE '.$C                   $   #00001644' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 THIS-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00578
00579  9299-EXIT.
00580      EXIT.
00581  EJECT
00582  9300-CLEAR  SECTION.
00583      MOVE PI-RETURN-TO-PROGRAM   TO  THIS-PGM.
00584
00585      GO TO 9200-XCTL.
00586
00587  9399-EXIT.
00588      EXIT.
00589
00590  9400-PGMIDERR  SECTION.
00591      
      * EXEC CICS HANDLE CONDITION
00592 *        PGMIDERR  (6000-SEND-TEXT)
00593 *    END-EXEC.
      *    MOVE '"$L                   ! $ #00001662' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303031363632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00594
00595      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM
00596                                      LOGOFF-PGM.
00597      MOVE 'EL005'                TO  THIS-PGM.
00598      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
00599      MOVE SPACES                 TO  PI-ENTRY-CD-1.
00600
00601      GO TO 9200-XCTL.
00602
00603  9499-EXIT.
00604      EXIT.
00605  EJECT
00606  9500-ERROR-FORMAT  SECTION.
00607      
      * EXEC CICS LINK
00608 *        PROGRAM   ('EL001')
00609 *        COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
00610 *        LENGTH    (EMI-COMM-LENGTH)
00611 *    END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   ''   #00001678' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00612
00613  9599-EXIT.
00614      EXIT.
00615
00616  9600-ERROR  SECTION.
00617      MOVE DFHEIBLK               TO  EMI-LINE1.
00618
00619      
      * EXEC CICS LINK
00620 *        PROGRAM   ('EL004')
00621 *        COMMAREA  (EMI-LINE1)
00622 *        LENGTH    (72)
00623 *    END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00001690' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00624
00625      PERFORM 5000-SEND-DATA-ONLY.
00626
00627      GO TO 9100-RETURN-TRAN.
00628
00629  9699-EXIT.
00630      EXIT.
00631
00632  9700-SECURITY-VIOLATION.
00633 *                                COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00001721' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031373231' TO DFHEIV0(25:11)
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
00634
00635  9799-EXIT.
00636      EXIT.
00637
00638  9999-LAST-PARAGRAPH  SECTION.
00639      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL659' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL659' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9400-PGMIDERR,
                     1020-NOTFND,
                     1020-NOTFND,
                     9600-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 9200-XCTL,
                     1020-NOTFND,
                     1010-DSIDERR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 6000-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL659' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
