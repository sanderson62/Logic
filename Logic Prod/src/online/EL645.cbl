00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL645 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 07:42:58.
00007 *                            VMOD=2.004
00008 *
00008 *
00009 *AUTHOR.        LOGIC, INC.
00010 *               DALLAS, TEXAS.
00011
00012 *DATE-COMPILED.
00013  SKIP1
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
00026 *        LOSS RATIOS LOOK-UP.
00027
00028 *    SCREENS     - EL645A - LOSS RATIOS SELECTION MENU
00029
00030 *    ENTERED BY  - EL626 - MAIN PROCESSING MENU
00031
00032 *    EXIT TO     - CALLING PROGRAM
00033 *                  EL6451 - LOSS RATIOS AS OF (MM/DD/YY)
00034 *                  EL659  - NAME LOOKUP (ACCT,COMP,REIN)
00035
00036 *    INPUT FILE  - ERLOSS - LOSS RATIOS
00037
00038 *    OUTPUT FILE - NONE
00039
00040 *    COMMAREA    - PASSED.  THE CONTROL INFORMATION KEYED IN THIS
00041 *                  PROGRAM IS PLACED IN THE APPROPRIATE FIELDS OF
00042 *                  THE COMMAREA FOR REFERENCE BY EL6451.  THE
00043 *                  PROGRAM WORK AREA OF THE COMMAREA IS USED TO
00044 *                  PASS THE RECORD KEY INFORMATION NEEDED BY
00045 *                  EL6451.
00046
00047 *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL626.  ON
00048 *                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE
00049 *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVE
00050 *                  ENTRIES (XCTL FROM CICS VIA EXR1) THE SCREEN
00051 *                  WILL BE READ AND ACTION WILL BE BASED ON THE
00052 *                  OPTION INDICATED.  PROGRAM FUNCTION KEY 10 WILL
00053 *                  TRANSFER CONTROL TO PROGRAM EL659 - NAME LOOKUP
00054  EJECT
00055  ENVIRONMENT DIVISION.
00056  DATA DIVISION.
00057  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00058
00059  77  FILLER  PIC  X(32) VALUE '********************************'.
00060  77  FILLER  PIC  X(32) VALUE '*    EL645 WORKING STORAGE     *'.
00061  77  FILLER  PIC  X(32) VALUE '*********** VMOD=2.004 *********'.
00062
00063 *                                COPY ELCSCTM.
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
00064
00065 *                                COPY ELCSCRTY.
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
00066
00067  01  WS-DATE-AREA.
00068      12  SAVE-DATE               PIC  X(08)      VALUE SPACES.
00069      12  SAVE-BIN-DATE           PIC  X(02)      VALUE SPACES.
00070
00071  01  FILLER                  COMP-3.
00072      12  WS-COMPLETED-SUCCESSFUL     PIC S9(01)  VALUE ZERO.
00073          88  TRANSACTION-SUCCESSFUL              VALUE +1.
00074      12  TIME-IN                     PIC S9(07)  VALUE ZERO.
00075      12  TIME-OUT  REDEFINES
00076          TIME-IN                     PIC S9(03)V9(4).
00077
00078  01  FILLER                  COMP SYNC.
00079      12  SC-ITEM                 PIC S9(04)      VALUE +0001.
00080
00081  01  FILLER.
00082      12  WS-MAPSET-NAME          PIC  X(08)      VALUE 'EL645S'.
00083      12  WS-MAP-NAME             PIC  X(08)    VALUE 'EL645A  '.
00084      12  FILLER  REDEFINES  WS-MAP-NAME.
00085          16  FILLER              PIC  X(02).
00086          16  WS-MAP-NUMBER       PIC  X(04).
00087          16  FILLER              PIC  X(02).
00088      12  THIS-PGM                PIC  X(08)      VALUE 'EL645'.
00089      12  WS-LOSS-RATIO-DSID      PIC  X(08)      VALUE 'ERLOSS'.
00090      12  WS-TRANS-ID             PIC  X(04)      VALUE 'EXR1'.
00091      12  ERROR-MESSAGES.
00092          16  ER-0002             PIC  X(04)      VALUE '0002'.
00093          16  ER-0004             PIC  X(04)      VALUE '0004'.
00094          16  ER-0005             PIC  X(04)      VALUE '0005'.
00095          16  ER-0008             PIC  X(04)      VALUE '0008'.
00096          16  ER-0029             PIC  X(04)      VALUE '0029'.
00097          16  ER-0070             PIC  X(04)      VALUE '0070'.
00098          16  ER-0671             PIC  X(04)      VALUE '0671'.
00099          16  ER-0674             PIC  X(04)      VALUE '0674'.
00100  EJECT
00101 *                                COPY ELCINTF.
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
00102
00103 *                                COPY ELC645PI.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELC645PI                            *
00004 *                            VMOD=2.003                          *
00005 *                                                                *
00006 ******************************************************************
00007
00008      12  PI-WORK-AREA  REDEFINES  PI-PROGRAM-WORK-AREA.
00009          16  FILLER                      PIC  X(12).
00010          16  PI-1ST-TIME-SW              PIC S9(01)  COMP-3.
00011          16  PI-LINE-COUNT               PIC S9(03)  COMP-3.
00012          16  PI-AIX-RECORD-COUNT         PIC S9(04)  COMP SYNC.
00013          16  PI-START-SW                 PIC S9(01)  COMP-3.
00014          16  PI-BROWSE-SW                PIC S9(01)  COMP-3.
00015          16  PI-END-OF-FILE              PIC S9(01)  COMP-3.
00016          16  PI-DSID                     PIC  X(08).
00017          16  PI-OPTION                   PIC  X(01).
00018              88  OPTION-ONE-SELECTED             VALUE '1'.
00019              88  OPTION-TWO-SELECTED             VALUE '2'.
00020              88  OPTION-THREE-SELECTED           VALUE '3'.
00021              88  OPTION-FOUR-SELECTED            VALUE '4'.
00022              88  OPTION-FIVE-SELECTED            VALUE '5'.
00023              88  OPTION-SIX-SELECTED             VALUE '6'.
00024          16  PI-READ-SWITCH              PIC  X(01).
00025              88  PI-READ-TOTAL                   VALUE 'B'.
00026              88  PI-READ-DETAIL                  VALUE 'C'.
00027          16  PI-RETURN-SWITCH            PIC  X(01).
00028              88  PI-REENTERED                    VALUE 'Y'.
00029          16  PI-SELECTION-CRITERIA.
00030              20  PI-SC-COMPANY-CD        PIC  X(01).
00031              20  PI-SC-RCD-TYPE          PIC  X(01).
00032              20  PI-SC-REIN-CO           PIC  X(03).
00033              20  PI-SC-RPT-CD-1          PIC  X(10).
00034              20  PI-SC-CARRIER           PIC  X(01).
00035              20  PI-SC-GROUPING          PIC  X(06).
00036              20  PI-SC-GA-RPT-CD-2       PIC  X(10).
00037              20  PI-SC-STATE             PIC  X(02).
00038              20  PI-SC-ACCOUNT           PIC  X(10).
00039              20  PI-SC-REIN-SUB          PIC  X(03).
00040          16  PI-LOSS-RATIO-KEY.
00041              20  PI-LRK-COMPANY-CD       PIC  X(01).
00042              20  PI-LRK-RCD-TYPE         PIC  X(01).
00043              20  PI-LRK-REIN-CO          PIC  X(03).
00044              20  PI-LRK-RPT-CD-1         PIC  X(10).
00045              20  PI-LRK-CARRIER          PIC  X(01).
00046              20  PI-LRK-GROUPING         PIC  X(06).
00047              20  PI-LRK-GA-RPT-CD-2      PIC  X(10).
00048              20  PI-LRK-STATE            PIC  X(02).
00049              20  PI-LRK-ACCOUNT          PIC  X(10).
00050              20  PI-LRK-REIN-SUB         PIC  X(03).
00051          16  PI-PREV-LOSS-RATIO-KEY.
00052              20  PI-PREV-LRK-COMPANY-CD  PIC  X(01).
00053              20  PI-PREV-LRK-RCD-TYPE    PIC  X(01).
00054              20  PI-PREV-LRK-REIN-CO     PIC  X(03).
00055              20  PI-PREV-LRK-RPT-CD-1    PIC  X(10).
00056              20  PI-PREV-LRK-CARRIER     PIC  X(01).
00057              20  PI-PREV-LRK-GROUPING    PIC  X(06).
00058              20  PI-PREV-LRK-GA-RPT-CD-2
00059                                          PIC  X(10).
00060              20  PI-PREV-LRK-STATE       PIC  X(02).
00061              20  PI-PREV-LRK-ACCOUNT     PIC  X(10).
00062              20  PI-PREV-LRK-REIN-SUB    PIC  X(03).
00063          16  PI-KEY-LENGTH               PIC S9(04)  COMP SYNC.
00064          16  PI-TS-ITEM                  PIC S9(04)  COMP SYNC.
00065          16  PI-1ST-KEY                  PIC  X(47).
00066          16  PI-LAST-KEY                 PIC  X(47).
00067          16  PI-PREV-AID                 PIC  X(01).
00068          16  PI-START-LOSS-RATIO-KEY.
00069              20  PI-START-LRK-COMPANY-CD
00070                                          PIC  X(01).
00071              20  PI-START-LRK-RCD-TYPE   PIC  X(01).
00072              20  PI-START-LRK-REIN-CO    PIC  X(03).
00073              20  PI-START-LRK-RPT-CD-1   PIC  X(10).
00074              20  PI-START-LRK-CARRIER    PIC  X(01).
00075              20  PI-START-LRK-GROUPING   PIC  X(06).
00076              20  PI-START-LRK-GA-RPT-CD-2
00077                                          PIC  X(10).
00078              20  PI-START-LRK-STATE      PIC  X(02).
00079              20  PI-START-LRK-ACCOUNT    PIC  X(10).
00080              20  PI-START-LRK-REIN-SUB   PIC  X(03).
00081          16  PI-END-LOSS-RATIO-KEY.
00082              20  PI-END-LRK-COMPANY-CD   PIC  X(01).
00083              20  PI-END-LRK-RCD-TYPE     PIC  X(01).
00084              20  PI-END-LRK-REIN-CO      PIC  X(03).
00085              20  PI-END-LRK-RPT-CD-1     PIC  X(10).
00086              20  PI-END-LRK-CARRIER      PIC  X(01).
00087              20  PI-END-LRK-GROUPING     PIC  X(06).
00088              20  PI-END-LRK-GA-RPT-CD-2  PIC  X(10).
00089              20  PI-END-LRK-STATE        PIC  X(02).
00090              20  PI-END-LRK-ACCOUNT      PIC  X(10).
00091              20  PI-END-LRK-REIN-SUB     PIC  X(03).
00092          16  PI-SAVE-LOSS-RATIO-KEY.
00093              20  PI-SAVE-LRK-COMPANY-CD  PIC  X(01).
00094              20  PI-SAVE-LRK-RCD-TYPE    PIC  X(01).
00095              20  PI-SAVE-LRK-REIN-CO     PIC  X(03).
00096              20  PI-SAVE-LRK-RPT-CD-1    PIC  X(10).
00097              20  PI-SAVE-LRK-CARRIER     PIC  X(01).
00098              20  PI-SAVE-LRK-GROUPING    PIC  X(06).
00099              20  PI-SAVE-LRK-GA-RPT-CD-2
00100                                          PIC  X(10).
00101              20  PI-SAVE-LRK-STATE       PIC  X(02).
00102              20  PI-SAVE-LRK-ACCOUNT     PIC  X(10).
00103              20  PI-SAVE-LRK-REIN-SUB    PIC  X(03).
00104          16  PI-LIN1-LOSS-RATIO-KEY      PIC  X(47).
00105          16  PI-SCREEN-COUNT             PIC S9(08)  COMP.
00106          16  PI-SUB                      PIC S9(02).
00107          16  PI-FIRST-TIME-SW            PIC  X(01).
00108          16  PI-MAPNAME                  PIC  X(08).
00109              88  PI-MAP-B                    VALUE 'EL654B  '.
00110              88  PI-MAP-C                    VALUE 'EL654C  '.
00111          16  PI-ACTIVE-ONLY              PIC  X(01).
00112              88  PI-ACCT-ACTIVE              VALUE 'Y'.
00113          16  FILLER                      PIC  X(163).
00114
00115 ******************************************************************
00104  EJECT
00105 *                                COPY ELCEMIB.
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
00106  EJECT
00107 *                                COPY ELCDATE.
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
00108  EJECT
00109 *                                COPY ELCLOGOF.
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
00110  EJECT
00111 *                                COPY EL645S.
       01  EL645AI.
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
           05  ACARRL PIC S9(0004) COMP.
           05  ACARRF PIC  X(0001).
           05  FILLER REDEFINES ACARRF.
               10  ACARRA PIC  X(0001).
           05  ACARRI PIC  X(0001).
      *    -------------------------------
           05  AGROUPL PIC S9(0004) COMP.
           05  AGROUPF PIC  X(0001).
           05  FILLER REDEFINES AGROUPF.
               10  AGROUPA PIC  X(0001).
           05  AGROUPI PIC  X(0006).
      *    -------------------------------
           05  ASTATEL PIC S9(0004) COMP.
           05  ASTATEF PIC  X(0001).
           05  FILLER REDEFINES ASTATEF.
               10  ASTATEA PIC  X(0001).
           05  ASTATEI PIC  X(0002).
      *    -------------------------------
           05  AACCTL PIC S9(0004) COMP.
           05  AACCTF PIC  X(0001).
           05  FILLER REDEFINES AACCTF.
               10  AACCTA PIC  X(0001).
           05  AACCTI PIC  X(0010).
      *    -------------------------------
           05  AGENAGTL PIC S9(0004) COMP.
           05  AGENAGTF PIC  X(0001).
           05  FILLER REDEFINES AGENAGTF.
               10  AGENAGTA PIC  X(0001).
           05  AGENAGTI PIC  X(0010).
      *    -------------------------------
           05  AREINCOL PIC S9(0004) COMP.
           05  AREINCOF PIC  X(0001).
           05  FILLER REDEFINES AREINCOF.
               10  AREINCOA PIC  X(0001).
           05  AREINCOI PIC  X(0003).
      *    -------------------------------
           05  ARPTCD1L PIC S9(0004) COMP.
           05  ARPTCD1F PIC  X(0001).
           05  FILLER REDEFINES ARPTCD1F.
               10  ARPTCD1A PIC  X(0001).
           05  ARPTCD1I PIC  X(0010).
      *    -------------------------------
           05  ARPTCD2L PIC S9(0004) COMP.
           05  ARPTCD2F PIC  X(0001).
           05  FILLER REDEFINES ARPTCD2F.
               10  ARPTCD2A PIC  X(0001).
           05  ARPTCD2I PIC  X(0010).
      *    -------------------------------
           05  ACTONLYL PIC S9(0004) COMP.
           05  ACTONLYF PIC  X(0001).
           05  FILLER REDEFINES ACTONLYF.
               10  ACTONLYA PIC  X(0001).
           05  ACTONLYI PIC  X(0001).
      *    -------------------------------
           05  ASELL PIC S9(0004) COMP.
           05  ASELF PIC  X(0001).
           05  FILLER REDEFINES ASELF.
               10  ASELA PIC  X(0001).
           05  ASELI PIC  99.
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
       01  EL645AO REDEFINES EL645AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASTATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AACCTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AGENAGTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AREINCOO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARPTCD1O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARPTCD2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTONLYO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ASELO PIC  99.
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
00112  EJECT
00113 *                                COPY ELCATTR.
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
00114  EJECT
00115 *                                COPY ELCAID.
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
00116
00117  01  FILLER  REDEFINES  DFHAID.
00118      12  FILLER                      PIC  X(08).
00119      12  PF-VALUES                   PIC  X(01)
00120              OCCURS  24  TIMES.
00121  EJECT
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
00123
00124  01  DFHCOMMAREA                     PIC  X(1024).
00125
00126 *01 PARMLIST             COMP  SYNC.
00127 *    12  FILLER                      PIC S9(09).
00128 *    12  ERLOSS-POINTER              PIC S9(09).
00129  EJECT
00130 *                                COPY ERCLOSS.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCLOSS                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.005                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION  =  LOSS RATIOS                             *
00008 *                                                                *
00009 *   **NOTE**  THIS COPYBOOK IS USED FOR BOTH THE ON-LINE         *
00010 *             AND OFF-LINE LOSS-RATIO FILES.                     *
00011 *                                                                *
00012 *                                                                *
00013 *   FILE TYPE  =  VSAM,KSDS(ON-LINE),  SEQUENTIAL(OFFLINE)       *
00014 *   RECORD SIZE = 525           RECFORM = FIXED                  *
00015 *                                                                *
00016 *   BASE CLUSTER NAME  =  ERLOSS               RKP=2,LEN=47      *
00017 *                                                                *
00018 *   LOG = NO                                                     *
00019 *   SERVREQ = BROWSE                                             *
00020 *                                                                *
00021 ******************************************************************
00022
00023  01  LOSS-RATIO-MASTER.
00024
00025      12  LR-RECORD-ID        PIC XX.
00026          88  VALID-LR-RECORD           VALUE 'LR'.
00027
00028      12  LR-CONTROL.
00029          16  LR-COMPANY-CD   PIC X.
00030          16  LR-RCD-TYPE     PIC X.
00031              88  ACCOUNT-RECORD        VALUE 'A'.
00032              88  G-A-RECORD            VALUE 'G'.
00033              88  REIN-RECORD           VALUE 'R'.
00034              88  REPORT-CD1-RECORD     VALUE 'B'.
00035              88  REPORT-CD2-RECORD     VALUE 'C'.
00036              88  STATE-RECORD          VALUE 'S'.
00037          16  LR-REIN-CO      PIC XXX.
00038          16  LR-RPT-CD-1     PIC X(10).
00039          16  LR-CARRIER      PIC X.
00040          16  LR-GROUPING     PIC X(6).
00041          16  LR-GA-RPT-CD-2  PIC X(10).
00042          16  LR-STATE        PIC XX.
00043          16  LR-ACCOUNT      PIC X(10).
00044          16  LR-REIN-SUB     PIC XXX.
00045
00046      12  LR-DATA.
00047          16  LR-RUN-DATE     PIC X(6).
00048          16  LR-RUN-DATE-N REDEFINES
00049                 LR-RUN-DATE  PIC 9(6).
00050          16  LR-ACCT-NAME    PIC X(30).
00051          16  LR-ACCT-RANGES  OCCURS  2  TIMES.
00052              20  LR-EXP-DATE PIC X(6).
00053              20  LR-REI-TAB  PIC XXX.
00054              20  LR-RETRO    PIC X.
00055              20  LR-BASIS    PIC X.
00056              20  LR-GA-DATA  OCCURS  3  TIMES.
00057                  24  LR-AGT-NO   PIC X(10).
00058                  24  LR-SNG-PCT  PIC SV9(5)        COMP-3.
00059                  24  LR-SNG-PCT-X REDEFINES
00060                      LR-SNG-PCT  PIC XXX.
00061                  24  LR-JNT-PCT  PIC SV9(5)        COMP-3.
00062                  24  LR-JNT-PCT-X REDEFINES
00063                      LR-JNT-PCT  PIC XXX.
00064                  24  LR-A-H-PCT  PIC SV9(5)        COMP-3.
00065                  24  LR-A-H-PCT-X REDEFINES
00066                      LR-A-H-PCT  PIC XXX.
00067
00068          16  LR-REIN-NAME    PIC X(30).
00069          16  LR-G-A-NAME     PIC X(30).
00070
00071          16  LR-ACCT-STATUS  PIC X.
00072              88  ACCT-ACTIVE          VALUE 'A' '0'.
00073              88  ACCT-INACTIVE        VALUE 'I' '1'.
00074              88  ACCT-TRANSFERRED     VALUE 'T' '2'.
00075
00076          16  FILLER          PIC X(15).
00077
00078      12  LR-TOTALS  OCCURS  3  TIMES.
00079          16  LR-YTD-NET      PIC S9(11)V99     COMP-3.
00080          16  LR-YTD-EARN     PIC S9(11)V99     COMP-3.
00081          16  LR-YTD-PAID     PIC S9(11)V99     COMP-3.
00082          16  LR-YTD-RESV     PIC S9(11)V99     COMP-3.
00083          16  LR-YTD-INCUR    PIC S9(11)V99     COMP-3.
00084          16  LR-YTD-RATIO    PIC S9(4)V9       COMP-3.
00085          16  LR-ITD-NET      PIC S9(11)V99     COMP-3.
00086          16  LR-ITD-EARN     PIC S9(11)V99     COMP-3.
00087          16  LR-ITD-PAID     PIC S9(11)V99     COMP-3.
00088          16  LR-ITD-RESV     PIC S9(11)V99     COMP-3.
00089          16  LR-ITD-INCUR    PIC S9(11)V99     COMP-3.
00090          16  LR-ITD-RATIO    PIC S9(4)V9       COMP-3.
00091
00092 ******************************************************************
00131  EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA LOSS-RATIO-MASTER.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL645' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00133
00134      CONTINUE.
00135
00136      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00137      MOVE '5'                    TO  DC-OPTION-CODE.
00138
00139      PERFORM 7000-DATE-CONVERSION.
00140
00141      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00142      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00143      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00144      MOVE +2                     TO  EMI-NUMBER-OF-LINES
00145                                      EMI-SWITCH2.
00146
00147 *    NOTE *******************************************************
00148 *         *                                                     *
00149 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL *
00150 *         *  FROM ANOTHER MODULE.                               *
00151 *         *                                                     *
00152 *         *******************************************************.
00153
00154      IF EIBCALEN  IS NOT GREATER THAN  ZERO
00155          MOVE UNACCESS-MSG       TO  LOGOFF-MSG
00156          GO TO 6000-SEND-TEXT.
00157
00158      
      * EXEC CICS HANDLE CONDITION
00159 *        PGMIDERR  (9400-PGMIDERR)
00160 *        NOTFND    (1020-NOTFND)
00161 *        ENDFILE   (1020-NOTFND)
00162 *        ERROR     (9600-ERROR)
00163 *    END-EXEC.
      *    MOVE '"$LI''.                ! " #00001311' TO DFHEIV0
           MOVE X'22244C49272E202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031333131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00164  EJECT
00165  0100-MAIN-LOGIC.
00166      IF PI-CALLING-PROGRAM  IS NOT EQUAL TO  THIS-PGM
00167          IF PI-RETURN-TO-PROGRAM  IS NOT EQUAL TO  THIS-PGM
00168              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6
00169              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5
00170              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4
00171              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3
00172              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2
00173              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1
00174              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM
00175              MOVE THIS-PGM              TO  PI-CALLING-PROGRAM
00176          ELSE
00177              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM
00178              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM
00179              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1
00180              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2
00181              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3
00182              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4
00183              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5
00184              MOVE SPACES                TO  PI-SAVED-PROGRAM-6
00185              PERFORM 3000-BUILD-SCREEN
00186      ELSE
00187          GO TO 0120-MAIN-LOGIC.
00188
00189  0110-MAIN-LOGIC.
00190 *    NOTE *******************************************************
00191 *         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      *
00192 *         *  INTERFACE BLOCK FOR THIS MODULE.                   *
00193 *         *******************************************************.
00194
00195      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA
00196                                      PI-RETURN-SWITCH
00197                                      PI-CONTROL-IN-PROGRESS.
00198      MOVE ZERO                   TO  PI-1ST-TIME-SW
00199                                      PI-LINE-COUNT
00200                                      PI-BROWSE-SW
00201                                      PI-KEY-LENGTH
00202                                      PI-TS-ITEM
00203                                      PI-END-OF-FILE
00204                                      PI-START-SW
00205                                      PI-AIX-RECORD-COUNT.
00206
00207 *    NOTE *******************************************************
00208 *         *      SEND THE INITIAL MAP OUT TO BEGIN PROCESSING   *
00209 *         *  FOR EL645.                                         *
00210 *         *******************************************************.
00211
00212      MOVE LOW-VALUES             TO  EL645AO.
00213
00214      PERFORM 4000-SEND-INITIAL-MAP.
00215
00216      GO TO 9100-RETURN-TRAN.
00217  EJECT
00218  0120-MAIN-LOGIC.
00219      IF PI-REENTERED
00220          GO TO 0900-RESTORE-SCREEN.
00221
00222 *    NOTE *******************************************************
00223 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- *
00224 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    *
00225 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         *
00226 *         *******************************************************.
00227
00228      IF EIBAID  IS EQUAL TO  DFHCLEAR
00229          GO TO 9300-CLEAR.
00230
00231      IF EIBAID  IS EQUAL TO  DFHPA1  OR  DFHPA2  OR  DFHPA3
00232          MOVE LOW-VALUES         TO  EL645AO
00233          MOVE -1                 TO  ASELL
00234          MOVE ER-0008            TO  EMI-ERROR
00235          PERFORM 5000-SEND-DATA-ONLY
00236          GO TO 9100-RETURN-TRAN.
00237
00238      
      * EXEC CICS RECEIVE
00239 *        INTO    (EL645AI)
00240 *        MAPSET  (WS-MAPSET-NAME)
00241 *        MAP     (WS-MAP-NAME)
00242 *    END-EXEC.
           MOVE LENGTH OF
            EL645AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00001391' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031333931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL645AI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00243
00244      IF ASELL  EQUAL  ZERO
00245          GO TO 0125-CHECK-PFKEYS.
00246
00247      IF EIBAID  IS NOT EQUAL TO  DFHENTER
00248          MOVE ER-0004            TO  EMI-ERROR
00249          MOVE AL-UNBOF           TO  ASELA
00250          MOVE -1                 TO  ASELL
00251          PERFORM 5000-SEND-DATA-ONLY
00252          GO TO 9100-RETURN-TRAN.
00253
00254      IF ASELI  IS NUMERIC  AND
00255        (ASELI  IS GREATER THAN  0  AND
00256         ASELI  IS LESS THAN  25)
00257          MOVE PF-VALUES (ASELI)  TO EIBAID
00258      ELSE
00259          MOVE ER-0029            TO  EMI-ERROR
00260          MOVE AL-UNBOF           TO  ASELA
00261          MOVE -1                 TO  ASELL
00262          PERFORM 5000-SEND-DATA-ONLY
00263          GO TO 9100-RETURN-TRAN.
00264
00265  0125-CHECK-PFKEYS.
00266      IF EIBAID  IS EQUAL TO  DFHPF1 OR DFHPF2 OR DFHPF3 OR
00267                              DFHPF4 OR DFHPF5 OR DFHPF6
00268          GO TO 0130-MAIN-LOGIC.
00269
00270      IF EIBAID  IS EQUAL TO  DFHPF10
00271          MOVE 'EL659'            TO  THIS-PGM
00272          GO TO 9200-XCTL.
00273
00274      IF EIBAID  IS EQUAL TO  DFHPF12
00275          MOVE 'EL010'            TO  THIS-PGM
00276          GO TO 9200-XCTL.
00277
00278      IF EIBAID  IS EQUAL TO  DFHPF23
00279          GO TO 9000-RETURN-CICS.
00280
00281      IF EIBAID  IS EQUAL TO  DFHPF24
00282          MOVE 'EL126'            TO  THIS-PGM
00283          GO TO 9200-XCTL.
00284
00285      MOVE ER-0029                TO  EMI-ERROR.
00286      MOVE AL-UNBOF               TO  ASELA.
00287      MOVE -1                     TO  ASELL.
00288      PERFORM 5000-SEND-DATA-ONLY.
00289      GO TO 9100-RETURN-TRAN.
00290  EJECT
00291  0130-MAIN-LOGIC.
00292      MOVE SPACES                 TO  PI-SELECTION-CRITERIA
00293                                      PI-LOSS-RATIO-KEY.
00294      MOVE PI-COMPANY-CD          TO  PI-SC-COMPANY-CD
00295                                      PI-LRK-COMPANY-CD.
00296      MOVE ACTONLYI               TO  PI-ACTIVE-ONLY.
00297
00298      IF PI-PROCESSOR-ID  IS EQUAL TO  'LGXX'
00299          NEXT SENTENCE
00300      ELSE
00301          
      * EXEC CICS READQ TS
00302 *            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00303 *            INTO    (SECURITY-CONTROL)
00304 *            LENGTH  (SC-COMM-LENGTH)
00305 *            ITEM    (SC-ITEM)
00306 *        END-EXEC
      *    MOVE '*$II   L              ''   #00001454' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031343534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00307          MOVE SC-CREDIT-DISPLAY (31)
00308                                  TO  PI-DISPLAY-CAP
00309          MOVE SC-CREDIT-UPDATE  (31)
00310                                  TO  PI-MODIFY-CAP
00311          IF NOT DISPLAY-CAP
00312              MOVE 'READ'         TO  SM-READ
00313              PERFORM 9700-SECURITY-VIOLATION
00314              MOVE ER-0070        TO  EMI-ERROR
00315              PERFORM 4000-SEND-INITIAL-MAP
00316              GO TO 9100-RETURN-TRAN.
00317  EJECT
00318  0200-OPTION-1-PROCESSING.
00319 ******************************************************************
00320 *           O P T I O N  1  P R O C E S S I N G                  *
00321 ******************************************************************
00322
00323      IF EIBAID IS EQUAL TO  DFHPF6
00324          GO TO 0700-OPTION-6-PROCESSING.
00325
00326      IF EIBAID IS EQUAL TO  DFHPF5
00327          GO TO 0600-OPTION-5-PROCESSING.
00328
00329      IF EIBAID IS EQUAL TO  DFHPF4
00330          GO TO 0500-OPTION-4-PROCESSING.
00331
00332      IF EIBAID IS EQUAL TO  DFHPF3
00333          GO TO 0400-OPTION-3-PROCESSING.
00334
00335      IF EIBAID IS EQUAL TO  DFHPF2
00336          GO TO 0300-OPTION-2-PROCESSING.
00337
00338      MOVE LOW-VALUES             TO  PI-SELECTION-CRITERIA.
00339
00340      MOVE 'EL645C'               TO  PI-MAPNAME.
00341
00342      IF PI-CERT-ACCESS-CONTROL = '1'  OR  '2'  OR  '4'
00343          IF ACARRL  IS GREATER THAN  ZERO
00344              MOVE ACARRI         TO  PI-SC-CARRIER
00345          ELSE
00346              MOVE ER-0005        TO  EMI-ERROR
00347              MOVE AL-UNBOF       TO  ACARRA
00348              MOVE -1             TO  ACARRL
00349              PERFORM 5000-SEND-DATA-ONLY
00350              GO TO 9100-RETURN-TRAN.
00351
00352      IF PI-CERT-ACCESS-CONTROL = '1'
00353          IF AGROUPL  IS GREATER THAN  ZERO
00354              MOVE AGROUPI        TO  PI-SC-GROUPING
00355          ELSE
00356              MOVE ER-0005        TO  EMI-ERROR
00357              MOVE AL-UNBOF       TO  AGROUPA
00358              MOVE -1             TO  AGROUPL
00359              PERFORM 5000-SEND-DATA-ONLY
00360              GO TO 9100-RETURN-TRAN.
00361
00362      IF PI-CERT-ACCESS-CONTROL = ' '  OR  '1'  OR  '2'
00363          IF ASTATEL  IS GREATER THAN  ZERO
00364              MOVE ASTATEI        TO  PI-SC-STATE
00365          ELSE
00366              MOVE ER-0005        TO  EMI-ERROR
00367              MOVE AL-UNBOF       TO  ASTATEA
00368              MOVE -1             TO  ASTATEL
00369              PERFORM 5000-SEND-DATA-ONLY
00370              GO TO 9100-RETURN-TRAN.
00371
00372      IF AACCTL  IS GREATER THAN  ZERO
00373          MOVE AACCTI             TO  PI-SC-ACCOUNT
00374      ELSE
00375          MOVE ER-0005            TO  EMI-ERROR
00376          MOVE AL-UNBOF           TO  AACCTA
00377          MOVE -1                 TO  AACCTL
00378          PERFORM 5000-SEND-DATA-ONLY
00379          GO TO 9100-RETURN-TRAN.
00380
00381  0210-PROCESS-OPTION-1.
00382
00383      MOVE WS-LOSS-RATIO-DSID     TO  PI-DSID.
00384      MOVE '1'                    TO  PI-OPTION.
00385      MOVE PI-COMPANY-CD          TO  PI-SC-COMPANY-CD.
00386      MOVE 'A'                    TO  PI-SC-RCD-TYPE.
00387      MOVE +47                    TO  PI-KEY-LENGTH.
00388      MOVE PI-SELECTION-CRITERIA  TO  PI-LOSS-RATIO-KEY
00389                                      PI-SAVE-LOSS-RATIO-KEY.
00390      MOVE -1                     TO  ACARRL.
00391
00392      PERFORM 1000-READ-LOSS-RATIO.
00393      GO TO 9200-XCTL.
00394  EJECT
00395  0300-OPTION-2-PROCESSING.
00396 ******************************************************************
00397 *           O P T I O N  2  P R O C E S S I N G                  *
00398 ******************************************************************
00399
00400      MOVE LOW-VALUES             TO  PI-SELECTION-CRITERIA.
00401
00402      MOVE 'EL645B'               TO  PI-MAPNAME.
00403
00404      IF PI-CAR-GROUP-ACCESS-CNTL = ' ' OR '2'
00405          IF ACARRL  IS GREATER THAN  ZERO
00406              MOVE ACARRI         TO  PI-SC-CARRIER
00407          ELSE
00408              MOVE ER-0005        TO  EMI-ERROR
00409              MOVE AL-UNBOF       TO  ACARRA
00410              MOVE -1             TO  ACARRL
00411              PERFORM 5000-SEND-DATA-ONLY
00412              GO TO 9100-RETURN-TRAN
00413      ELSE
00414          IF PI-CAR-GROUP-ACCESS-CNTL = '1' OR '3'
00415              MOVE '0'            TO  PI-SC-CARRIER.
00416
00417      IF PI-CAR-GROUP-ACCESS-CNTL = ' ' OR '1'
00418          IF AGROUPL  IS GREATER THAN  ZERO
00419              MOVE AGROUPI        TO  PI-SC-GROUPING
00420          ELSE
00421              MOVE ER-0005        TO  EMI-ERROR
00422              MOVE AL-UNBOF       TO  AGROUPA
00423              MOVE -1             TO  AGROUPL
00424              PERFORM 5000-SEND-DATA-ONLY
00425              GO TO 9100-RETURN-TRAN
00426      ELSE
00427          IF PI-CAR-GROUP-ACCESS-CNTL = '2' OR '3'
00428              MOVE '000000'       TO  PI-SC-GROUPING.
00429
00430      IF AGENAGTL  IS GREATER THAN  ZERO
00431          MOVE AGENAGTI           TO  PI-SC-GA-RPT-CD-2
00432      ELSE
00433          MOVE ER-0005            TO  EMI-ERROR
00434          MOVE AL-UNBOF           TO  AGENAGTA
00435          MOVE -1                 TO  AGENAGTL
00436          PERFORM 5000-SEND-DATA-ONLY
00437          GO TO 9100-RETURN-TRAN.
00438
00439      IF AACCTL  IS GREATER THAN  ZERO
00440          MOVE 'EL645C'           TO  PI-MAPNAME
00441          MOVE AACCTI             TO  PI-SC-ACCOUNT
00442      ELSE
00443          GO TO 0310-PROCESS-OPTION-2.
00444
00445      IF PI-CERT-ACCESS-CONTROL = ' '  OR  '1'  OR  '2'
00446          IF ASTATEL  IS GREATER THAN  ZERO
00447 *            MOVE 'EL645C'       TO  PI-MAPNAME
00448              MOVE ASTATEI        TO  PI-SC-STATE
00449          ELSE
00450              MOVE ER-0005        TO  EMI-ERROR
00451              MOVE AL-UNBOF       TO  ASTATEA
00452              MOVE -1             TO  ASTATEL
00453              PERFORM 5000-SEND-DATA-ONLY
00454              GO TO 9100-RETURN-TRAN.
00455
00456  0310-PROCESS-OPTION-2.
00457      MOVE WS-LOSS-RATIO-DSID     TO  PI-DSID.
00458      MOVE '2'                    TO  PI-OPTION.
00459      MOVE PI-COMPANY-CD          TO  PI-SC-COMPANY-CD.
00460      MOVE 'G'                    TO  PI-SC-RCD-TYPE.
00461      MOVE +47                    TO  PI-KEY-LENGTH.
00462      MOVE PI-SELECTION-CRITERIA  TO  PI-LOSS-RATIO-KEY
00463                                      PI-SAVE-LOSS-RATIO-KEY.
00464      MOVE -1                     TO  ACARRL.
00465
00466      PERFORM 1000-READ-LOSS-RATIO.
00467      GO TO 9200-XCTL.
00468  EJECT
00469  0400-OPTION-3-PROCESSING.
00470 ******************************************************************
00471 *           O P T I O N  3  P R O C E S S I N G                  *
00472 ******************************************************************
00473
00474      MOVE LOW-VALUES             TO  PI-SELECTION-CRITERIA.
00475
00476      MOVE 'EL645B'               TO  PI-MAPNAME.
00477
00478      IF AREINCOL  IS GREATER THAN  ZERO
00479          MOVE AREINCOI           TO  PI-SC-REIN-CO
00480      ELSE
00481          MOVE ER-0005            TO  EMI-ERROR
00482          MOVE AL-UNBOF           TO  AREINCOA
00483          MOVE -1                 TO  AREINCOL
00484          PERFORM 5000-SEND-DATA-ONLY
00485          GO TO 9100-RETURN-TRAN.
00486
00487      IF AACCTL  IS GREATER THAN  ZERO
00488          MOVE 'EL645C'           TO  PI-MAPNAME
00489          MOVE AACCTI             TO  PI-SC-ACCOUNT
00490      ELSE
00491          GO TO 0410-PROCESS-OPTION-3.
00492
00493      IF PI-CERT-ACCESS-CONTROL = '1'  OR  '2'  OR  '4'
00494          IF ACARRL  IS GREATER THAN  ZERO
00495              MOVE ACARRI         TO  PI-SC-CARRIER
00496          ELSE
00497              MOVE ER-0005        TO  EMI-ERROR
00498              MOVE AL-UNBOF       TO  ACARRA
00499              MOVE -1             TO  ACARRL
00500              PERFORM 5000-SEND-DATA-ONLY
00501              GO TO 9100-RETURN-TRAN.
00502
00503      IF PI-CERT-ACCESS-CONTROL = '1'
00504          IF AGROUPL  IS GREATER THAN  ZERO
00505              MOVE AGROUPI        TO  PI-SC-GROUPING
00506          ELSE
00507              MOVE ER-0005        TO  EMI-ERROR
00508              MOVE AL-UNBOF       TO  AGROUPA
00509              MOVE -1             TO  AGROUPL
00510              PERFORM 5000-SEND-DATA-ONLY
00511              GO TO 9100-RETURN-TRAN.
00512
00513      IF PI-CERT-ACCESS-CONTROL = ' '  OR  '1'  OR  '2'
00514          IF ASTATEL  IS GREATER THAN  ZERO
00515              MOVE ASTATEI        TO  PI-SC-STATE
00516          ELSE
00517              MOVE ER-0005        TO  EMI-ERROR
00518              MOVE AL-UNBOF       TO  ASTATEA
00519              MOVE -1             TO  ASTATEL
00520              PERFORM 5000-SEND-DATA-ONLY
00521              GO TO 9100-RETURN-TRAN.
00522
00523  0410-PROCESS-OPTION-3.
00524      MOVE WS-LOSS-RATIO-DSID     TO  PI-DSID.
00525      MOVE '3'                    TO  PI-OPTION.
00526      MOVE PI-COMPANY-CD          TO  PI-SC-COMPANY-CD.
00527      MOVE 'R'                    TO  PI-SC-RCD-TYPE.
00528      MOVE +47                    TO  PI-KEY-LENGTH.
00529      MOVE PI-SELECTION-CRITERIA  TO  PI-LOSS-RATIO-KEY
00530                                      PI-SAVE-LOSS-RATIO-KEY.
00531      MOVE -1                     TO  AREINCOL.
00532
00533      PERFORM 1000-READ-LOSS-RATIO.
00534      GO TO 9200-XCTL.
00535  EJECT
00536  0500-OPTION-4-PROCESSING.
00537 ******************************************************************
00538 *           O P T I O N  4  P R O C E S S I N G                  *
00539 ******************************************************************
00540
00541      MOVE LOW-VALUES             TO  PI-SELECTION-CRITERIA.
00542
00543      MOVE 'EL645B'               TO  PI-MAPNAME.
00544
00545      IF ARPTCD1L  IS GREATER THAN  ZERO
00546          MOVE ARPTCD1I           TO  PI-SC-RPT-CD-1
00547      ELSE
00548          MOVE ER-0005            TO  EMI-ERROR
00549          MOVE AL-UNBOF           TO  ARPTCD1A
00550          MOVE -1                 TO  ARPTCD1L
00551          PERFORM 5000-SEND-DATA-ONLY
00552          GO TO 9100-RETURN-TRAN.
00553
00554      IF AACCTL  IS GREATER THAN  ZERO
00555          MOVE 'EL645C'           TO  PI-MAPNAME
00556          MOVE AACCTI             TO  PI-SC-ACCOUNT
00557      ELSE
00558          GO TO 0510-PROCESS-OPTION-4.
00559
00560      IF PI-CERT-ACCESS-CONTROL = '1'  OR  '2'  OR  '4'
00561          IF ACARRL  IS GREATER THAN  ZERO
00562              MOVE ACARRI         TO  PI-SC-CARRIER
00563          ELSE
00564              MOVE ER-0005        TO  EMI-ERROR
00565              MOVE AL-UNBOF       TO  ACARRA
00566              MOVE -1             TO  ACARRL
00567              PERFORM 5000-SEND-DATA-ONLY
00568              GO TO 9100-RETURN-TRAN.
00569
00570      IF PI-CERT-ACCESS-CONTROL = '1'
00571          IF AGROUPL  IS GREATER THAN  ZERO
00572              MOVE AGROUPI        TO  PI-SC-GROUPING
00573          ELSE
00574              MOVE ER-0005        TO  EMI-ERROR
00575              MOVE AL-UNBOF       TO  AGROUPA
00576              MOVE -1             TO  AGROUPL
00577              PERFORM 5000-SEND-DATA-ONLY
00578              GO TO 9100-RETURN-TRAN.
00579
00580      IF PI-CERT-ACCESS-CONTROL = ' '  OR  '1'  OR  '2'
00581          IF ASTATEL  IS GREATER THAN  ZERO
00582              MOVE ASTATEI        TO  PI-SC-STATE
00583          ELSE
00584              MOVE ER-0005        TO  EMI-ERROR
00585              MOVE AL-UNBOF       TO  ASTATEA
00586              MOVE -1             TO  ASTATEL
00587              PERFORM 5000-SEND-DATA-ONLY
00588              GO TO 9100-RETURN-TRAN.
00589
00590  0510-PROCESS-OPTION-4.
00591      MOVE WS-LOSS-RATIO-DSID     TO  PI-DSID.
00592      MOVE '4'                    TO  PI-OPTION.
00593      MOVE PI-COMPANY-CD          TO  PI-SC-COMPANY-CD.
00594      MOVE 'B'                    TO  PI-SC-RCD-TYPE.
00595      MOVE +47                    TO  PI-KEY-LENGTH.
00596      MOVE PI-SELECTION-CRITERIA  TO  PI-LOSS-RATIO-KEY
00597                                      PI-SAVE-LOSS-RATIO-KEY.
00598      MOVE -1                     TO  ARPTCD1L.
00599
00600      PERFORM 1000-READ-LOSS-RATIO.
00601      GO TO 9200-XCTL.
00602  EJECT
00603  0600-OPTION-5-PROCESSING.
00604 ******************************************************************
00605 *           O P T I O N  5  P R O C E S S I N G                  *
00606 ******************************************************************
00607
00608      MOVE LOW-VALUES             TO  PI-SELECTION-CRITERIA.
00609
00610      MOVE 'EL645B'               TO  PI-MAPNAME.
00611
00612      IF ARPTCD2L  IS GREATER THAN  ZERO
00613          MOVE ARPTCD2I           TO  PI-SC-GA-RPT-CD-2
00614      ELSE
00615          MOVE ER-0005            TO  EMI-ERROR
00616          MOVE AL-UNBOF           TO  ARPTCD2A
00617          MOVE -1                 TO  ARPTCD2L
00618          PERFORM 5000-SEND-DATA-ONLY
00619          GO TO 9100-RETURN-TRAN.
00620
00621      IF PI-CERT-ACCESS-CONTROL = '1'  OR  '2'  OR  '4'
00622          IF ACARRL  IS GREATER THAN  ZERO
00623              MOVE ACARRI         TO  PI-SC-CARRIER
00624          ELSE
00625              MOVE ER-0005        TO  EMI-ERROR
00626              MOVE AL-UNBOF       TO  ACARRA
00627              MOVE -1             TO  ACARRL
00628              PERFORM 5000-SEND-DATA-ONLY
00629              GO TO 9100-RETURN-TRAN.
00630
00631      IF PI-CERT-ACCESS-CONTROL = '1'
00632          IF AGROUPL  IS GREATER THAN  ZERO
00633              MOVE AGROUPI        TO  PI-SC-GROUPING
00634          ELSE
00635              MOVE ER-0005        TO  EMI-ERROR
00636              MOVE AL-UNBOF       TO  AGROUPA
00637              MOVE -1             TO  AGROUPL
00638              PERFORM 5000-SEND-DATA-ONLY
00639              GO TO 9100-RETURN-TRAN.
00640
00641      IF AACCTL  IS GREATER THAN  ZERO
00642          MOVE 'EL645C'           TO  PI-MAPNAME
00643          MOVE AACCTI             TO  PI-SC-ACCOUNT
00644      ELSE
00645          GO TO 0610-PROCESS-OPTION-5.
00646
00647      IF PI-CERT-ACCESS-CONTROL = ' '  OR  '1'  OR  '2'
00648          IF ASTATEL  IS GREATER THAN  ZERO
00649              MOVE ASTATEI        TO  PI-SC-STATE
00650          ELSE
00651              MOVE ER-0005        TO  EMI-ERROR
00652              MOVE AL-UNBOF       TO  ASTATEA
00653              MOVE -1             TO  ASTATEL
00654              PERFORM 5000-SEND-DATA-ONLY
00655              GO TO 9100-RETURN-TRAN.
00656
00657  0610-PROCESS-OPTION-5.
00658      MOVE WS-LOSS-RATIO-DSID     TO  PI-DSID.
00659      MOVE '5'                    TO  PI-OPTION.
00660      MOVE PI-COMPANY-CD          TO  PI-SC-COMPANY-CD.
00661      MOVE 'C'                    TO  PI-SC-RCD-TYPE.
00662      MOVE +47                    TO  PI-KEY-LENGTH.
00663      MOVE PI-SELECTION-CRITERIA  TO  PI-LOSS-RATIO-KEY
00664                                      PI-SAVE-LOSS-RATIO-KEY.
00665      MOVE -1                     TO  ACARRL.
00666
00667      PERFORM 1000-READ-LOSS-RATIO.
00668      GO TO 9200-XCTL.
00669  EJECT
00670  0700-OPTION-6-PROCESSING.
00671 ******************************************************************
00672 *           O P T I O N  6  P R O C E S S I N G                  *
00673 ******************************************************************
00674
00675      MOVE LOW-VALUES             TO  PI-SELECTION-CRITERIA.
00676
00677      MOVE 'EL645B'               TO  PI-MAPNAME.
00678
00679      IF ASTATEL  IS GREATER THAN  ZERO
00680          MOVE ASTATEI            TO  PI-SC-STATE
00681      ELSE
00682          MOVE ER-0005            TO  EMI-ERROR
00683          MOVE AL-UNBOF           TO  ASTATEA
00684          MOVE -1                 TO  ASTATEL
00685          PERFORM 5000-SEND-DATA-ONLY
00686          GO TO 9100-RETURN-TRAN.
00687
00688  0710-PROCESS-OPTION-6.
00689      MOVE WS-LOSS-RATIO-DSID     TO  PI-DSID.
00690      MOVE '6'                    TO  PI-OPTION.
00691      MOVE PI-COMPANY-CD          TO  PI-SC-COMPANY-CD.
00692      MOVE 'S'                    TO  PI-SC-RCD-TYPE.
00693      MOVE +47                    TO  PI-KEY-LENGTH.
00694      MOVE PI-SELECTION-CRITERIA  TO  PI-LOSS-RATIO-KEY
00695                                      PI-SAVE-LOSS-RATIO-KEY.
00696      MOVE -1                     TO  ASELL.
00697
00698      PERFORM 1000-READ-LOSS-RATIO.
00699      GO TO 9200-XCTL.
00700  EJECT
00701  0900-RESTORE-SCREEN.
00702      MOVE SPACE                  TO  PI-RETURN-SWITCH.
00703      MOVE ZERO                   TO  PI-1ST-TIME-SW
00704                                      PI-LINE-COUNT
00705                                      PI-BROWSE-SW
00706                                      PI-KEY-LENGTH
00707                                      PI-TS-ITEM
00708                                      PI-END-OF-FILE
00709                                      PI-START-SW
00710                                      PI-AIX-RECORD-COUNT.
00711      MOVE LOW-VALUES             TO  EL645AO.
00712      MOVE PI-SAVE-LRK-REIN-CO    TO  AREINCOO.
00713      MOVE PI-SAVE-LRK-RPT-CD-1   TO  ARPTCD1O.
00714      MOVE PI-SAVE-LRK-CARRIER    TO  ACARRO.
00715      MOVE PI-SAVE-LRK-GROUPING   TO  AGROUPO.
00716      IF OPTION-FIVE-SELECTED
00717          MOVE PI-SAVE-LRK-GA-RPT-CD-2
00718                                  TO  ARPTCD2O
00719      ELSE
00720          MOVE PI-SAVE-LRK-GA-RPT-CD-2
00721                                  TO  AGENAGTO.
00722      MOVE PI-SAVE-LRK-STATE      TO  ASTATEO.
00723      MOVE PI-SAVE-LRK-ACCOUNT    TO  AACCTO.
00724      MOVE AL-UANON               TO  ACTONLYA.
00725      MOVE PI-ACTIVE-ONLY         TO  ACTONLYO.
00726      MOVE -1                     TO  ACARRL.
00727
00728      PERFORM 5000-SEND-DATA-ONLY.
00729
00730      GO TO 9100-RETURN-TRAN.
00731  EJECT
00732  1000-READ-LOSS-RATIO  SECTION.
00733      
      * EXEC CICS HANDLE CONDITION
00734 *        DUPKEY   (9200-XCTL)
00735 *        NOTFND   (1020-NOTFND)
00736 *        DSIDERR  (1010-DSIDERR)
00737 *    END-EXEC.
      *    MOVE '"$$I"                 ! # #00001886' TO DFHEIV0
           MOVE X'222424492220202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303031383836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00738
00739      MOVE +1                     TO  PI-START-SW.
00740
00741      
      * EXEC CICS READ
00742 *        DATASET    (PI-DSID)
00743 *        RIDFLD     (PI-LOSS-RATIO-KEY)
00744 *        SET        (ADDRESS OF LOSS-RATIO-MASTER)
00745 *    END-EXEC.
      *    MOVE '&"S        E          (   #00001894' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303031383934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-LOSS-RATIO-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LOSS-RATIO-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00746
00747      CONTINUE.
00748
00749      IF LR-COMPANY-CD  IS EQUAL TO  PI-COMPANY-CD
00750          NEXT SENTENCE
00751      ELSE
00752          GO TO 1020-NOTFND.
00753
00754      MOVE 'EL6451'               TO  THIS-PGM.
00755
00756      GO TO 1099-EXIT.
00757 *PEM GO TO 9200-XCTL.
00758
00759  1010-DSIDERR.
00760      MOVE ER-0671                TO  EMI-ERROR.
00761
00762      PERFORM 5000-SEND-DATA-ONLY.
00763
00764      GO TO 9100-RETURN-TRAN.
00765
00766  1020-NOTFND.
00767      MOVE ER-0674                TO  EMI-ERROR.
00768
00769      PERFORM 5000-SEND-DATA-ONLY.
00770
00771      GO TO 9100-RETURN-TRAN.
00772
00773  1099-EXIT.
00774      EXIT.
00775  EJECT
00776  3000-BUILD-SCREEN  SECTION.
00777 ******************************************************************
00778 *          REBUILD ORIGNAL SCREEN AND ERROR MESSAGE IF EL6451    *
00779 *          DID NOT FIND ANY CERTIFICATES DURING BROWSE OF FILE.  *
00780 ******************************************************************
00781
00782      IF EIBTRNID  IS EQUAL TO  WS-TRANS-ID
00783          NEXT SENTENCE
00784      ELSE
00785          GO TO 3099-EXIT.
00786
00787      MOVE LOW-VALUES             TO  EL645AO.
00788      MOVE ER-0674                TO  EMI-ERROR.
00789
00790      MOVE PI-SC-CARRIER          TO  ACARRO.
00791      MOVE PI-SC-GROUPING         TO  AGROUPO.
00792      MOVE PI-SC-STATE            TO  ASTATEO.
00793      MOVE PI-SC-ACCOUNT          TO  AACCTO.
00794
00795      IF OPTION-TWO-SELECTED
00796          GO TO 3020-OPTION-TWO.
00797
00798      IF OPTION-THREE-SELECTED
00799          GO TO 3030-OPTION-THREE.
00800
00801      IF OPTION-FOUR-SELECTED
00802          GO TO 3040-OPTION-FOUR.
00803
00804      IF OPTION-FIVE-SELECTED
00805          GO TO 3050-OPTION-FIVE.
00806
00807      IF OPTION-SIX-SELECTED
00808          GO TO 3060-OPTION-SIX.
00809
00810  3010-OPTION-ONE.
00811      MOVE -1                     TO  ACARRL.
00812      MOVE AL-UANON               TO  ACARRA.
00813
00814      GO TO 3070-INITIALIZE-WORK-AREAS.
00815
00816  3020-OPTION-TWO.
00817      MOVE PI-SC-GA-RPT-CD-2      TO  AGENAGTO.
00818      MOVE -1                     TO  AGENAGTL.
00819      MOVE AL-UANON               TO  AGENAGTA.
00820
00821      GO TO 3070-INITIALIZE-WORK-AREAS.
00822
00823  3030-OPTION-THREE.
00824      MOVE PI-SC-REIN-CO          TO  AREINCOO.
00825      MOVE -1                     TO  AREINCOL.
00826      MOVE AL-UANON               TO  AREINCOA.
00827
00828      GO TO 3070-INITIALIZE-WORK-AREAS.
00829
00830  3040-OPTION-FOUR.
00831      MOVE PI-SC-RPT-CD-1         TO  ARPTCD1O.
00832      MOVE -1                     TO  ARPTCD1L.
00833      MOVE AL-UANON               TO  ARPTCD1A.
00834
00835      GO TO 3070-INITIALIZE-WORK-AREAS.
00836
00837  3050-OPTION-FIVE.
00838      MOVE PI-SC-GA-RPT-CD-2      TO  ARPTCD2O.
00839      MOVE -1                     TO  ARPTCD2L.
00840      MOVE AL-UANON               TO  ARPTCD2A.
00841
00842      GO TO 3070-INITIALIZE-WORK-AREAS.
00843
00844  3060-OPTION-SIX.
00845      MOVE PI-SC-STATE            TO  ASTATEO.
00846      MOVE -1                     TO  ASTATEL.
00847      MOVE AL-UANON               TO  ASTATEA.
00848
00849  3070-INITIALIZE-WORK-AREAS.
00850      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA
00851                                      PI-CONTROL-IN-PROGRESS.
00852      MOVE ZERO                   TO  PI-1ST-TIME-SW
00853                                      PI-LINE-COUNT
00854                                      PI-BROWSE-SW
00855                                      PI-KEY-LENGTH
00856                                      PI-TS-ITEM
00857                                      PI-END-OF-FILE
00858                                      PI-START-SW
00859                                      PI-AIX-RECORD-COUNT.
00860
00861      PERFORM 5000-SEND-DATA-ONLY.
00862
00863      GO TO 9100-RETURN-TRAN.
00864
00865  3099-EXIT.
00866      EXIT.
00867  EJECT
00868  4000-SEND-INITIAL-MAP  SECTION.
00869      MOVE -1                     TO  ACARRL.
00870      MOVE SAVE-DATE              TO  ADATEO.
00871      MOVE EIBTIME                TO  TIME-IN.
00872      MOVE TIME-OUT               TO  ATIMEO.
00873      MOVE AL-UANON               TO  ACTONLYA.
00874      MOVE 'Y'                    TO  ACTONLYO.
00875
00876      IF EMI-ERROR  IS NOT EQUAL TO  ZERO
00877          PERFORM 9500-ERROR-FORMAT
00878      ELSE
00879          IF TRANSACTION-SUCCESSFUL
00880              PERFORM 9500-ERROR-FORMAT.
00881
00882      MOVE EMI-MESSAGE-AREA (1)   TO  AEMSG1O.
00883      MOVE EMI-MESSAGE-AREA (2)   TO  AEMSG2O.
00884
00885      
      * EXEC CICS SEND
00886 *        FROM    (EL645AO)
00887 *        MAPSET  (WS-MAPSET-NAME)
00888 *        MAP     (WS-MAP-NAME)
00889 *        CURSOR
00890 *        ERASE
00891 *    END-EXEC.
           MOVE LENGTH OF
            EL645AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00002038' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032303338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL645AO, 
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
           
00892
00893  4099-EXIT.
00894      EXIT.
00895  EJECT
00896  5000-SEND-DATA-ONLY SECTION.
00897      MOVE SAVE-DATE              TO  ADATEO.
00898      MOVE EIBTIME                TO  TIME-IN.
00899      MOVE TIME-OUT               TO  ATIMEO.
00900
00901      IF EMI-ERROR  IS NOT EQUAL TO  ZERO
00902          PERFORM 9500-ERROR-FORMAT.
00903
00904      MOVE EMI-MESSAGE-AREA (1)   TO  AEMSG1O.
00905      MOVE EMI-MESSAGE-AREA (2)   TO  AEMSG2O.
00906
00907      
      * EXEC CICS SEND DATAONLY
00908 *        FROM    (EL645AO)
00909 *        MAPSET  (WS-MAPSET-NAME)
00910 *        MAP     (WS-MAP-NAME)
00911 *        CURSOR
00912 *    END-EXEC.
           MOVE LENGTH OF
            EL645AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00002060' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032303630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL645AO, 
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
           
00913
00914  5099-EXIT.
00915      EXIT.
00916  EJECT
00917  6000-SEND-TEXT  SECTION.
00918      
      * EXEC CICS SEND TEXT
00919 *        FROM    (LOGOFF-TEXT)
00920 *        LENGTH  (LOGOFF-LENGTH)
00921 *        ERASE
00922 *        FREEKB
00923 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00002071' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032303731' TO DFHEIV0(25:11)
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
           
00924
00925      
      * EXEC CICS RETURN
00926 *        END-EXEC.
      *    MOVE '.(                    &   #00002078' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032303738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00927
00928  6099-EXIT.
00929      EXIT.
00930  EJECT
00931  7000-DATE-CONVERSION  SECTION.
00932      
      * EXEC CICS LINK
00933 *        PROGRAM   ('ELDATCV')
00934 *        COMMAREA  (DATE-CONVERSION-DATA)
00935 *        LENGTH    (DC-COMM-LENGTH)
00936 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00002085' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032303835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00937
00938  7099-EXIT.
00939      EXIT.
00940  EJECT
00941  9000-RETURN-CICS  SECTION.
00942      MOVE 'EL005'                TO  THIS-PGM.
00943      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
00944
00945      GO TO 9200-XCTL.
00946
00947  9099-EXIT.
00948      EXIT.
00949
00950  9100-RETURN-TRAN  SECTION.
00951      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
00952      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.
00953
00954      
      * EXEC CICS RETURN
00955 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
00956 *        LENGTH    (PI-COMM-LENGTH)
00957 *        TRANSID   (WS-TRANS-ID)
00958 *    END-EXEC.
      *    MOVE '.(CT                  &   #00002107' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00959
00960  9199-EXIT.
00961      EXIT.
00962
00963  9200-XCTL  SECTION.
00964      MOVE DFHENTER               TO  EIBAID.
00965
00966      
      * EXEC CICS XCTL
00967 *        PROGRAM   (THIS-PGM)
00968 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
00969 *        LENGTH    (PI-COMM-LENGTH)
00970 *    END-EXEC.
      *    MOVE '.$C                   $   #00002119' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 THIS-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00971
00972  9299-EXIT.
00973      EXIT.
00974  EJECT
00975  9300-CLEAR  SECTION.
00976      MOVE PI-RETURN-TO-PROGRAM   TO  THIS-PGM.
00977
00978      GO TO 9200-XCTL.
00979
00980  9399-EXIT.
00981      EXIT.
00982
00983  9400-PGMIDERR  SECTION.
00984      
      * EXEC CICS HANDLE CONDITION
00985 *        PGMIDERR  (6000-SEND-TEXT)
00986 *    END-EXEC.
      *    MOVE '"$L                   ! $ #00002137' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303032313337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00987
00988      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM
00989                                      LOGOFF-PGM.
00990      MOVE 'EL005'                TO  THIS-PGM.
00991      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
00992      MOVE SPACES                 TO  PI-ENTRY-CD-1.
00993
00994      GO TO 9200-XCTL.
00995
00996  9499-EXIT.
00997      EXIT.
00998  EJECT
00999  9500-ERROR-FORMAT  SECTION.
01000      
      * EXEC CICS LINK
01001 *        PROGRAM   ('EL001')
01002 *        COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
01003 *        LENGTH    (EMI-COMM-LENGTH)
01004 *    END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   ''   #00002153' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01005
01006  9599-EXIT.
01007      EXIT.
01008
01009  9600-ERROR  SECTION.
01010      MOVE DFHEIBLK               TO  EMI-LINE1.
01011
01012      
      * EXEC CICS LINK
01013 *        PROGRAM   ('EL004')
01014 *        COMMAREA  (EMI-LINE1)
01015 *        LENGTH    (72)
01016 *    END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00002165' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01017
01018      PERFORM 5000-SEND-DATA-ONLY.
01019
01020      GO TO 9100-RETURN-TRAN.
01021
01022  9699-EXIT.
01023      EXIT.
01024
01025  9700-SECURITY-VIOLATION.
01026 *                                COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00002196' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313936' TO DFHEIV0(25:11)
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
01027
01028  9799-EXIT.
01029      EXIT.
01030
01031  9999-LAST-PARAGRAPH  SECTION.
01032      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL645' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL645' TO DFHEIV1
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
           MOVE 'EL645' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
