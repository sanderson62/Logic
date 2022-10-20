00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL1324.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/13/96 09:48:12.
00007 *                            VMOD=2.003
00008 *
00008 *
00009 *AUTHOR.    LOGIC, INC.
00010 *           DALLAS, TEXAS.
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
00026 *    THE ALPHA LOOK-UP.
00027
00028 *    SCREENS     - EL132D  - ALPHA LOOK-UP QUALIFICATION
00029
00030 *    ENTERED BY  - EL126 - MAINTENANCE MENU
00031 *                  EL132 - CLAIM LOOK-UP
00032
00033 *    EXIT TO     - CALLING PROGRAM
00034 *                  EL1322 - CLAIM LOOK-UP FOR STATUS
00035
00036 *    INPUT FILE  - ELALPH - ALPHA FILE
00037
00038 *    OUTPUT FILE - NONE
00039
00040 *    COMMAREA    - PASSED.  IF A NAME IS SELECED, THE CONTROL OF
00041 *                  THAT NAME IS PLACED IN THE APPROPRIATE FIELDS
00042 *                  OF THE COMMAREA FOR REFERENCE BY SUCCESSIVE
00043 *                  PROGRAMS.  THE PROGRAM WORK AREA OF THE
00044 *                  COMMAREA IS USED TO PASS THE RECORD KEY
00045 *                  INFORMATION NEEDED BY EL1325 TO LOCATE THE
00046 *                  NAME.
00047
00048 *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL101.  ON
00049 *                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE
00050 *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVE
00051 *                  ENTRIES (XCTL FROM CICS VIA EX21) THE SCREEN
00052 *                  WILL BE READ AND ACTION WILL BE BASED ON THE
00053 *                  MAINTENANCE TYPE INDICATED.
101501******************************************************************
101501*                   C H A N G E   L O G
101501*
101501* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101501*-----------------------------------------------------------------
101501*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101501* EFFECTIVE    NUMBER
101501*-----------------------------------------------------------------
101501* 101501    2001100100006  SMVA  ADD USERID & CO ID TO SCREEN
101501******************************************************************
00054
00055      EJECT
00056  ENVIRONMENT DIVISION.
00057
00058  DATA DIVISION.
00059
00060  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00061
00062  77  FILLER  PIC X(32)  VALUE '********************************'.
00063  77  FILLER  PIC X(32)  VALUE '*  EL1324  WORKING STORAGE     *'.
00064  77  FILLER  PIC X(32)  VALUE '********** VMOD=2.003 **********'.
00065
00066 *                                COPY ELCSCTM.
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
00068 *                                COPY ELCSCRTY.
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
00071      12  SAVE-DATE               PIC X(8)    VALUE SPACES.
00072      12  SAVE-BIN-DATE           PIC X(2)    VALUE SPACES.
00073
00074  01  FILLER                          COMP-3.
00075
00076      12  TIME-IN                 PIC S9(7)   VALUE ZERO.
00077      12  TIME-OUT                    REDEFINES
00078          TIME-IN                 PIC S9(3)V9(4).
00079      12  WS-CALL-SW              PIC S9      VALUE ZERO.
00080
00081  01  FILLER                          COMP  SYNC.
00082
00083      12  SC-ITEM                 PIC S9(4)   VALUE +0001.
00084      12  WS-INDEX                PIC S9(4)   VALUE ZERO.
00085      EJECT
00086
00087  01  FILLER.
00088
00089      12  ALPHA-INDEX-DSID        PIC X(8)    VALUE 'ELALPH'.
00090      12  ALPHA2-INDEX-DSID       PIC X(8)    VALUE 'ELALPH2'.
00091      12  LINK-ELDATCV            PIC X(8)    VALUE 'ELDATCV'.
00092      12  LINK-001                PIC X(8)    VALUE 'EL001'.
00093      12  LINK-004                PIC X(8)    VALUE 'EL004'.
00094
00095      12  XCTL-005                PIC X(8)    VALUE 'EL005'.
00096      12  XCTL-010                PIC X(8)    VALUE 'EL010'.
00097      12  XCTL-126                PIC X(8)    VALUE 'EL126'.
00098      12  XCTL-132                PIC X(8)    VALUE 'EL132'.
00099      12  XCTL-150                PIC X(8)    VALUE 'EL150'.
00100      12  XCTL-1324               PIC X(8)    VALUE 'EL1324'.
00101      12  XCTL-1325               PIC X(8)    VALUE 'EL1325'.
00102      12  THIS-PGM                PIC X(8)    VALUE 'EL1324'.
00103
00104      12  WS-MAPSET-NAME          PIC X(8)  VALUE 'EL1324S'.
00105      12  WS-MAP-NAME             PIC X(8)  VALUE 'EL1324A'.
00106      12  FILLER                      REDEFINES
00107          WS-MAP-NAME.
00108          16  FILLER              PIC X(2).
00109          16  WS-MAP-NUMBER       PIC X(4).
00110          16  FILLER              PIC X(2).
00111
00112      12  WS-TRANS-ID             PIC X(4)   VALUE 'E033'.
00113
00114      12  WS-INPUT-FIELD          PIC X(50)  VALUE SPACES.
00115      12  WS-INPUT-CHAR  REDEFINES  WS-INPUT-FIELD  PIC X(01)
00116               OCCURS 50 TIMES   INDEXED BY  INPUT-INDEX.
00117
00118  01  ERROR-MESSAGES.
00119      12  ER-0000                 PIC X(4)  VALUE '0000'.
00120      12  ER-0004                 PIC X(4)  VALUE '0004'.
00121      12  ER-0008                 PIC X(4)  VALUE '0008'.
00122      12  ER-0029                 PIC X(4)  VALUE '0029'.
00123      12  ER-0070                 PIC X(4)  VALUE '0070'.
00124      12  ER-0194                 PIC X(4)  VALUE '0194'.
00125      12  ER-0195                 PIC X(4)  VALUE '0195'.
00126      12  ER-0196                 PIC X(4)  VALUE '0196'.
00127      12  ER-0197                 PIC X(4)  VALUE '0197'.
00128      12  ER-0203                 PIC X(4)  VALUE '0203'.
00129      12  ER-0209                 PIC X(4)  VALUE '0209'.
00130      12  ER-0216                 PIC X(4)  VALUE '0216'.
00131      12  ER-0284                 PIC X(4)  VALUE '0284'.
00132      12  ER-0348                 PIC X(4)  VALUE '0348'.
00133      12  ER-0488                 PIC X(4)  VALUE '0488'.
00134      12  ER-0777                 PIC X(4)  VALUE '0777'.
00135      12  ER-2370                 PIC X(4)  VALUE '2370'.
00136      12  ER-2374                 PIC X(4)  VALUE '2374'.
00137
00138      EJECT
00139 *                                COPY ELCINTF.
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
00140
00141 *                                COPY ELC132PI.
00001 *****************************************************************
00002 *                                                               *
00002 *                                                               *
00003 *                            ELC132PI.                          *
00004 *                            VMOD=2.004                         *
00005 *****************************************************************.
00006
00007      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.
00008          16  PI-1ST-TIME-SW              PIC S9       COMP-3.
00009          16  PI-LINE-COUNT               PIC S9(3)    COMP-3.
00010          16  PI-AIX-RECORD-COUNT         PIC S9(5)    COMP-3.
00011          16  PI-BROWSE-SW                PIC S9       COMP-3.
00012          16  PI-START-SW                 PIC S9       COMP-3.
00013          16  PI-END-OF-FILE              PIC S9       COMP-3.
00014          16  PI-DSID                     PIC X(8).
00015
00016          16  PI-KEEP-CERT-NO     PIC X(11)   OCCURS 16
00017                      INDEXED BY PI-K-INDEX.
00018
00019          16  PI-OPTION                   PIC X.
00020              88  NO-OPTION-SELECTED           VALUE ZERO.
00021              88  OPTION-ONE-SELECTED          VALUE '1'.
00022              88  OPTION-TWO-SELECTED          VALUE '2'.
00023              88  OPTION-THREE-SELECTED        VALUE '3'.
00024              88  OPTION-FOUR-SELECTED         VALUE '4'.
00025              88  OPTION-FIVE-SELECTED         VALUE '5'.
00026
00027          16  PI-SELECTION-CRITERIA.
00028              20  PI-SC-COMPANY-CD        PIC X.
00029              20  PI-SC-CARRIER           PIC X.
00030              20  PI-SC-CLAIM-NO          PIC X(7).
00031              20  PI-SC-CERT-NO.
00032                  25  PI-SC-CERT-PRIME    PIC X(10).
00033                  25  PI-SC-CERT-SFX      PIC X.
00034              20  FILLER                  PIC X(09).
00035
00036          16  FILLER REDEFINES PI-SELECTION-CRITERIA.
00037              20  FILLER                  PIC X.
00038              20  PI-SC-LAST-NAME         PIC X(15).
00039              20  PI-SC-INITIALS.
00040                  25  PI-SC-FIRST-NAME    PIC X(12).
00041                  25  PI-SC-INITIAL2      PIC X.
00042
00043          16  FILLER REDEFINES PI-SELECTION-CRITERIA.
00044              20  FILLER                  PIC X.
00045              20  PI-SC-SOC-SEC-NO        PIC X(11).
00046              20  FILLER                  PIC X(17).
00047
00048          16  FILLER REDEFINES PI-SELECTION-CRITERIA.
00049              20  FILLER                  PIC X.
00050              20  PI-SC-CERT-NO-A4.
00051                  25  PI-SC-CERT-PRIME-A4 PIC X(10).
00052                  25  PI-SC-CERT-SFX-A4   PIC X.
00053              20  FILLER                  PIC X(17).
00054
00055          16  FILLER REDEFINES PI-SELECTION-CRITERIA.
00056              20  FILLER                  PIC X.
00057              20  PI-SC-CCN-NO-A5         PIC X(20).
00058              20  FILLER                  PIC X(8).
00059
00060          16  PI-CLAIM-KEY.
00061              20  PI-CK-COMPANY-CD        PIC X.
00062              20  PI-CK-CARRIER           PIC X.
00063              20  PI-CK-CLAIM             PIC X(7).
00064              20  PI-CK-CERT-NO.
00065                  25  PI-CK-CERT-PRIME    PIC X(10).
00066                  25  PI-CK-CERT-SFX      PIC X.
00067              20  FILLER                  PIC X(09).
00068
00069          16  FILLER REDEFINES PI-CLAIM-KEY.
00070              20  FILLER                  PIC X.
00071              20  PI-CK-INSURED-LAST-NAME PIC X(15).
00072              20  PI-CK-INSURED-FRST-NAME PIC X(12).
00073              20  PI-CK-INSURED-MID-INIT  PIC X.
00074
00075          16  FILLER REDEFINES PI-CLAIM-KEY.
00076              20  FILLER                  PIC X.
00077              20  PI-CK-SOC-SEC-NO        PIC X(11).
00078              20  FILLER                  PIC X(17).
00079
00080          16  FILLER REDEFINES PI-CLAIM-KEY.
00081              20  FILLER                  PIC X.
00082              20  PI-CK-CERT-NO-A4.
00083                  25  PI-CK-CERT-PRIME-A4 PIC X(10).
00084                  25  PI-CK-CERT-SFX-A4   PIC X.
00085              20  FILLER                  PIC X(17).
00086
00087          16  FILLER REDEFINES PI-CLAIM-KEY.
00088              20  FILLER                  PIC X.
00089              20  PI-CK-CCN-NO-A5         PIC X(20).
00090              20  FILLER                  PIC X(8).
00091
00092          16  PI-LAST-EIBAID              PIC X.
00093          16  PI-SCREEN-COUNT             PIC S9(5)  COMP-3.
00094
00095          16  PI-KEY-LENGTH               PIC S9(4) COMP SYNC.
00096          16  PI-TS-ITEM                  PIC S9(4) COMP SYNC.
00097          16  PI-1ST-ALPH-KEY.
00098              20  PI-1ST-KEY            PIC X(29).
00099              20  FILLER                PIC X(15).
00100          16  PI-LAST-ALPH-KEY.
00101              20  PI-LAST-KEY           PIC X(29).
00102              20  FILLER                PIC X(15).
00103
00104          16  PI-SAVE-AREA            OCCURS 16 TIMES
00105              INDEXED BY PI-INDEX.
00106              20  PI-SA-STATE             PIC X(2).
00107              20  PI-SA-GROUP             PIC X(6).
00108              20  PI-SA-EFF-DATE          PIC X(2).
00109
00110          16  PI-LAST-NAME                PIC X(15).
00111          16  PI-ACCOUNT-NUMBER           PIC X(10).
00112          16  PI-CCN-NO                   PIC X(20).
00113          16  PI-CREDIT-CARD-INDEX        PIC X.
00114              88  CREDIT-CARD-INDEX            VALUE 'Y'.
00115
00116          16  PI-ALPH-CLAIM-KEY.
00117              20  PI-ALPH-CO-CD          PIC X(01).
00118              20  PI-ALPH-SOURCE         PIC X(01).
00119              20  PI-ALPH-NAME.
00120                  24  PI-ALPH-LAST-NAME  PIC X(15).
00121                  24  PI-ALPH-FRST-NAME.
00122                      28  PI-ALPH-F-INIT PIC X(01).
00123                      28  FILLER         PIC X(11).
00124                  24  PI-ALPH-MID-INIT   PIC X(01).
00125              20  PI-ALPH-DATE           PIC X(08).
00126              20  PI-ALPH-TIME           PIC S9(04)  COMP.
00127
00128          16  PI-ALPH-ADMIN-KEY.
00129              20  PI-ADM-COMP-CD        PIC X(01).
00130              20  PI-ADM-SOURCE         PIC X(01).
00131              20  PI-ADM-CARRIER        PIC X(01).
00132              20  PI-ADM-GROUPING.
00133                  24  PI-ADM-GRP-PREFIX PIC X(03).
00134                  24  PI-ADM-GRP-PRIME  PIC X(03).
00135              20  PI-ADM-STATE          PIC X(02).
00136              20  PI-ADM-PRODUCER.
00137                  24  PI-ADM-PROD-PRE   PIC X(04).
00138                  24  PI-ADM-PROD-PRM   PIC X(06).
00139              20  PI-ADM-CERT-EFF-DT    PIC X(02).
00140              20  PI-ADM-CERT-NUMBER.
00141                  24  PI-ADM-CERT-PRM   PIC X(10).
00142                  24  PI-ADM-CERT-SFX   PIC X(01).
00143              20  PI-ADM-DATE           PIC X(08).
00144              20  PI-ADM-TIME           PIC S9(04)  COMP.
00145
00146          16  P1-CLAIM-KEY   REDEFINES PI-ALPH-ADMIN-KEY.
00147              20  PI-CLM-COMP-CD        PIC X(01).
00148              20  PI-CLM-SOURCE         PIC X(01).
00149              20  PI-CLM-CARRIER        PIC X(01).
00150              20  PI-CLM-CLAIM-NO       PIC X(07).
00151              20  PI-CLM-CERT-NUMBER.
00152                  24  PI-CLM-CERT-PRM   PIC X(10).
00153                  24  PI-CLM-CERT-SFX   PIC X(01).
00154              20  PI-CLM-DATE           PIC X(08).
00155              20  PI-CLM-TIME           PIC S9(04)  COMP.
00156              20  FILLER                PIC X(13).
00157
00158          16  FILLER                    PIC X(02).
00142      EJECT
00143 *                                COPY ELCEMIB.
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
00144
00145      EJECT
00146 *                                COPY ELCDATE.
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
00147
00148      EJECT
00149 *                                COPY EL1324S.
       01  EL1324AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  ALDATEL PIC S9(0004) COMP.
           05  ALDATEF PIC  X(0001).
           05  FILLER REDEFINES ALDATEF.
               10  ALDATEA PIC  X(0001).
           05  ALDATEI PIC  X(0008).
      *    -------------------------------
           05  ALTIMEL PIC S9(0004) COMP.
           05  ALTIMEF PIC  X(0001).
           05  FILLER REDEFINES ALTIMEF.
               10  ALTIMEA PIC  X(0001).
           05  ALTIMEI PIC  X(0005).
      *    -------------------------------
           05  ALCOMPL PIC S9(0004) COMP.
           05  ALCOMPF PIC  X(0001).
           05  FILLER REDEFINES ALCOMPF.
               10  ALCOMPA PIC  X(0001).
           05  ALCOMPI PIC  X(0003).
      *    -------------------------------
           05  USERIDL PIC S9(0004) COMP.
           05  USERIDF PIC  X(0001).
           05  FILLER REDEFINES USERIDF.
               10  USERIDA PIC  X(0001).
           05  USERIDI PIC  X(0004).
      *    -------------------------------
           05  ALLNAMEL PIC S9(0004) COMP.
           05  ALLNAMEF PIC  X(0001).
           05  FILLER REDEFINES ALLNAMEF.
               10  ALLNAMEA PIC  X(0001).
           05  ALLNAMEI PIC  X(0015).
      *    -------------------------------
           05  ALFNAMEL PIC S9(0004) COMP.
           05  ALFNAMEF PIC  X(0001).
           05  FILLER REDEFINES ALFNAMEF.
               10  ALFNAMEA PIC  X(0001).
           05  ALFNAMEI PIC  X(0012).
      *    -------------------------------
           05  ALINTALL PIC S9(0004) COMP.
           05  ALINTALF PIC  X(0001).
           05  FILLER REDEFINES ALINTALF.
               10  ALINTALA PIC  X(0001).
           05  ALINTALI PIC  X(0001).
      *    -------------------------------
           05  ALCLMNOL PIC S9(0004) COMP.
           05  ALCLMNOF PIC  X(0001).
           05  FILLER REDEFINES ALCLMNOF.
               10  ALCLMNOA PIC  X(0001).
           05  ALCLMNOI PIC  X(0007).
      *    -------------------------------
           05  ALCARNOL PIC S9(0004) COMP.
           05  ALCARNOF PIC  X(0001).
           05  FILLER REDEFINES ALCARNOF.
               10  ALCARNOA PIC  X(0001).
           05  ALCARNOI PIC  X(0001).
      *    -------------------------------
           05  ALCERTL PIC S9(0004) COMP.
           05  ALCERTF PIC  X(0001).
           05  FILLER REDEFINES ALCERTF.
               10  ALCERTA PIC  X(0001).
           05  ALCERTI PIC  X(0010).
      *    -------------------------------
           05  ALCRTSXL PIC S9(0004) COMP.
           05  ALCRTSXF PIC  X(0001).
           05  FILLER REDEFINES ALCRTSXF.
               10  ALCRTSXA PIC  X(0001).
           05  ALCRTSXI PIC  X(0001).
      *    -------------------------------
           05  ALFCARRL PIC S9(0004) COMP.
           05  ALFCARRF PIC  X(0001).
           05  FILLER REDEFINES ALFCARRF.
               10  ALFCARRA PIC  X(0001).
           05  ALFCARRI PIC  X(0001).
      *    -------------------------------
           05  ALFGRPL PIC S9(0004) COMP.
           05  ALFGRPF PIC  X(0001).
           05  FILLER REDEFINES ALFGRPF.
               10  ALFGRPA PIC  X(0001).
           05  ALFGRPI PIC  X(0006).
      *    -------------------------------
           05  ALFSTL PIC S9(0004) COMP.
           05  ALFSTF PIC  X(0001).
           05  FILLER REDEFINES ALFSTF.
               10  ALFSTA PIC  X(0001).
           05  ALFSTI PIC  X(0002).
      *    -------------------------------
           05  ALFACCTL PIC S9(0004) COMP.
           05  ALFACCTF PIC  X(0001).
           05  FILLER REDEFINES ALFACCTF.
               10  ALFACCTA PIC  X(0001).
           05  ALFACCTI PIC  X(0010).
      *    -------------------------------
           05  ALFEFDTL PIC S9(0004) COMP.
           05  ALFEFDTF PIC  X(0001).
           05  FILLER REDEFINES ALFEFDTF.
               10  ALFEFDTA PIC  X(0001).
           05  ALFEFDTI PIC  X(0008).
      *    -------------------------------
           05  ALFCERTL PIC S9(0004) COMP.
           05  ALFCERTF PIC  X(0001).
           05  FILLER REDEFINES ALFCERTF.
               10  ALFCERTA PIC  X(0001).
           05  ALFCERTI PIC  X(0010).
      *    -------------------------------
           05  ALFCRTXL PIC S9(0004) COMP.
           05  ALFCRTXF PIC  X(0001).
           05  FILLER REDEFINES ALFCRTXF.
               10  ALFCRTXA PIC  X(0001).
           05  ALFCRTXI PIC  X(0001).
      *    -------------------------------
           05  ALFLNMEL PIC S9(0004) COMP.
           05  ALFLNMEF PIC  X(0001).
           05  FILLER REDEFINES ALFLNMEF.
               10  ALFLNMEA PIC  X(0001).
           05  ALFLNMEI PIC  X(0015).
      *    -------------------------------
           05  ALFFNMEL PIC S9(0004) COMP.
           05  ALFFNMEF PIC  X(0001).
           05  FILLER REDEFINES ALFFNMEF.
               10  ALFFNMEA PIC  X(0001).
           05  ALFFNMEI PIC  X(0012).
      *    -------------------------------
           05  ALFINTLL PIC S9(0004) COMP.
           05  ALFINTLF PIC  X(0001).
           05  FILLER REDEFINES ALFINTLF.
               10  ALFINTLA PIC  X(0001).
           05  ALFINTLI PIC  X(0001).
      *    -------------------------------
           05  ALFMSG1L PIC S9(0004) COMP.
           05  ALFMSG1F PIC  X(0001).
           05  FILLER REDEFINES ALFMSG1F.
               10  ALFMSG1A PIC  X(0001).
           05  ALFMSG1I PIC  X(0079).
      *    -------------------------------
           05  ALFMSG2L PIC S9(0004) COMP.
           05  ALFMSG2F PIC  X(0001).
           05  FILLER REDEFINES ALFMSG2F.
               10  ALFMSG2A PIC  X(0001).
           05  ALFMSG2I PIC  X(0079).
      *    -------------------------------
           05  ALFPFKL PIC S9(0004) COMP.
           05  ALFPFKF PIC  X(0001).
           05  FILLER REDEFINES ALFPFKF.
               10  ALFPFKA PIC  X(0001).
           05  ALFPFKI PIC  99.
       01  EL1324AO REDEFINES EL1324AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALCOMPO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALLNAMEO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFNAMEO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALINTALO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALCLMNOO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALCARNOO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALCERTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALCRTSXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFCARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFGRPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFSTO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFACCTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFEFDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFCERTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFCRTXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFLNMEO PIC  X(0015).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFFNMEO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFINTLO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALFPFKO PIC  X(0002).
      *    -------------------------------
00150
00151      EJECT
00152 *                                COPY ELCLOGOF.
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
00153
00154      EJECT
00155 *                                COPY ELCATTR.
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
00156
00157      EJECT
00158 *                                COPY ELCAID.
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
00159
00160  01  FILLER      REDEFINES
00161      DFHAID.
00162
00163      12  FILLER                  PIC X(8).
00164
00165      12  PF-VALUES               PIC X
00166          OCCURS 24 TIMES.
00167      EJECT
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
00169
00170  01  DFHCOMMAREA                 PIC X(1024).
00171
00172      EJECT
00173 *                                COPY ELCALPH.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCALPH.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = ALPHA CROSS REFERENCE FILE                *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 128  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELALPH                         RKP=2,LEN=42   *
00013 *       ALTERNATE PATH1 = ELALPH2 (FULL CONTROL)  RKP=44,LEN=57  *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
CIDMOD*                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCALPH                          *
00017 ******************************************************************
00018
00019  01  ALPHA-INDEX.
00020      12  AI-RECORD-ID                         PIC XX.
00021          88  VALID-AI-ID                  VALUE 'AI'.
00022      12  AI-CONTROL-PRIMARY.
00023          16  AI-COMPANY-CD                     PIC X.
00024          16  AI-SOURCE                         PIC X.
00025              88  AI-CLAIM-SYSTEM          VALUE 'C'.
00026              88  AI-ADMIN-SYSTEM          VALUE 'A'.
00027          16  AI-NAME.
00028              20  AI-LAST-NAME                  PIC X(15).
00029              20  AI-FIRST-NAME.
00030                  24  AI-FIRST-INITIAL          PIC X.
00031                  24  FILLER                    PIC X(11).
00032              20  AI-MIDDLE-INIT                PIC X.
00033          16  AI-DATE                           PIC X(8).
00034          16  AI-TIME                           PIC S9(07) COMP-3.
00035
00036      12  AI-CONTROL-BY-ADMIN-KEY.
00037          16  AI-CM-COMPANY-CD                  PIC X.
00038          16  AI-CM-SOURCE                      PIC X.
00039          16  AI-CM-CARRIER                     PIC X.
00040          16  AI-CM-GROUPING.
00041              20  AI-CM-GROUPING-PREFIX         PIC X(3).
00042              20  AI-CM-GROUPING-PRIME          PIC X(3).
00043          16  AI-CM-STATE                       PIC XX.
00044          16  AI-CM-PRODUCER.
00045              20  AI-CM-PRODUCER-PREFIX         PIC X(4).
00046              20  AI-CM-PRODUCER-PRIME          PIC X(6).
00047          16  AI-CM-CERT-EFF-DT                 PIC XX.
00048          16  AI-CM-CERTIFICATE-NUMBER.
00049              20  AI-CM-CERT-PRIME              PIC X(10).
00050              20  AI-CM-CERT-SFX                PIC X.
00051          16  AI-CM-DATE                        PIC X(8).
00052          16  AI-CM-TIME                        PIC S9(7) COMP-3.
00053          16  FILLER                            PIC X(11).
00054
00055      12  AI-CONTROL-BY-CLAIM-KEY REDEFINES
00056          AI-CONTROL-BY-ADMIN-KEY.
00057          16  AI-CL-COMPANY-CD                  PIC X.
00058          16  AI-CL-SOURCE                      PIC X.
00059          16  AI-CL-CARRIER                     PIC X.
00060          16  AI-CL-CLAIM-NUMBER                PIC X(7).
00061          16  AI-CL-CERTIFICATE-NUMBER.
00062              20  AI-CL-CERT-PRIME              PIC X(10).
00063              20  AI-CL-CERT-SFX                PIC X.
00064          16  AI-CL-DATE                        PIC X(8).
00065          16  AI-CL-INCURRED-DATE               PIC XX.
00066          16  AI-CL-CLOSE-DATE                  PIC XX.
00067          16  AI-CL-TIME                        PIC S9(7)  COMP-3.
00068          16  AI-CREDIT-CARD-NUMBER.
00069              20  AI-CCN.
00070                  24  AI-CCN-PREFIX             PIC X(4).
00071                  24  AI-CCN-PRIME              PIC X(12).
00072              20  AI-CCN-FILLER                 PIC X(4).
00073
00074      12  AI-MAINT-INFO.
00075          16  AI-LAST-MAINT-BY                  PIC X(4).
00076          16  AI-LAST-MAINT-DT                  PIC XX.
00077          16  AI-LAST-MAINT-HHMMSS              PIC S9(7)  COMP-3.
00078
00079      12  AI-CLAIM-PAID-THRU-DT                 PIC XX.
00080
00081      12  FILLER                                PIC X(15).
00082
00174
00175      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA ALPHA-INDEX.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL1324' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00177
00178      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00179      MOVE '5'                    TO DC-OPTION-CODE.
00180      PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT.
00181      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00182      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00183
00184      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00185
00186      MOVE +2                     TO  EMI-NUMBER-OF-LINES
00187                                      EMI-SWITCH2.
00188
00189 *    NOTE *******************************************************
00190 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL *
00191 *         *  FROM ANOTHER MODULE.                               *
00192 *         *******************************************************.
00193
00194      IF EIBCALEN NOT GREATER ZERO
00195          MOVE UNACCESS-MSG       TO  LOGOFF-MSG
00196          GO TO 8300-SEND-TEXT.
00197
00198      
      * EXEC CICS HANDLE CONDITION
00199 *        PGMIDERR (9600-PGMIDERR)
00200 *        NOTOPEN  (8800-NOTOPEN)
00201 *        ERROR    (9990-ERROR)
00202 *    END-EXEC.
      *    MOVE '"$LJ.                 ! " #00001476' TO DFHEIV0
           MOVE X'22244C4A2E20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303031343736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00203
00204      EJECT
00205  0010-MAIN-LOGIC.
00206 *    NOTE *******************************************************
00207 *         *      IF THE TRANSACTION CODE OF THE TASK THAT       *
00208 *         *  INVOKED THIS MODULE IS NOT EX21, THIS IS THE FIRST *
00209 *         *  TIME THROUGH THIS MODULE.                          *
00210 *         *******************************************************.
00211
00212      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00213          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00214              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6
00215              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5
00216              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4
00217              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3
00218              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2
00219              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1
00220              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM
00221              MOVE THIS-PGM             TO  PI-CALLING-PROGRAM
00222            ELSE
00223              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM
00224              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
00225              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1
00226              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
00227              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3
00228              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4
00229              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5
00230              MOVE SPACES               TO  PI-SAVED-PROGRAM-6
00231              PERFORM 7000-BUILD-SCREEN
00232              MOVE +1             TO  WS-CALL-SW
00233        ELSE
00234          GO TO 0020-MAIN-LOGIC.
00235
00236  0015-MAIN-LOGIC.
00237 *    NOTE *******************************************************
00238 *         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      *
00239 *         *  INTERFACE BLOCK FOR THIS MODULE.                   *
00240 *         *******************************************************.
00241
00242      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA.
00243
00244      MOVE ZERO                   TO  PI-1ST-TIME-SW
00245                                      PI-LINE-COUNT
00246                                      PI-AIX-RECORD-COUNT
00247                                      PI-BROWSE-SW
00248                                      PI-KEY-LENGTH
00249                                      PI-TS-ITEM
00250                                      PI-END-OF-FILE
00251                                      PI-START-SW.
00252
00253      MOVE LOW-VALUES             TO  EL1324AO.
00254
00255  0018-MAIN-LOGIC.
00256      MOVE SPACES                 TO  PI-CONTROL-IN-PROGRESS.
00257
00258      GO TO 8100-SEND-INITIAL-MAP.
00259
00260      EJECT
00261  0020-MAIN-LOGIC.
00262      IF PI-1ST-TIME-SW NOT = ZERO
00263          GO TO 0015-MAIN-LOGIC.
00264
00265 *    NOTE *******************************************************
00266 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- *
00267 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    *
00268 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         *
00269 *         *******************************************************.
00270
00271      IF EIBAID = DFHCLEAR
00272          GO TO 9400-CLEAR.
00273
00274      IF EIBAID EQUAL DFHPA1 OR DFHPA2 OR DFHPA3
00275          MOVE LOW-VALUES         TO  EL1324AI
00276          MOVE -1                 TO  ALFPFKL
00277          MOVE ER-0008            TO  EMI-ERROR
00278          GO TO 8200-SEND-DATAONLY.
00279
00280      
      * EXEC CICS RECEIVE
00281 *        INTO   (EL1324AO)
00282 *        MAPSET (WS-MAPSET-NAME)
00283 *        MAP    (WS-MAP-NAME)
00284 *    END-EXEC.
           MOVE LENGTH OF
            EL1324AO
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00001558' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031353538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL1324AO, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00285
00286      IF ALFPFKL GREATER THAN  ZERO
00287          IF EIBAID NOT = DFHENTER
00288              MOVE ER-0004        TO  EMI-ERROR
00289              MOVE AL-UNBOF       TO  ALFPFKA
00290              MOVE -1             TO  ALFPFKL
00291              GO TO  8200-SEND-DATAONLY
00292          ELSE
00293              IF ALFPFKO  GREATER ZERO AND LESS '25'
00294                  MOVE PF-VALUES (ALFPFKI) TO  EIBAID
00295              ELSE
00296                  MOVE ER-0029        TO  EMI-ERROR
00297                  MOVE AL-UNBOF       TO  ALFPFKA
00298                  MOVE -1             TO  ALFPFKL
00299                  GO TO  8200-SEND-DATAONLY.
00300
00301      IF EIBAID = DFHPF12
00302          MOVE XCTL-010           TO  THIS-PGM
00303          GO TO 9300-XCTL.
00304
00305      IF EIBAID = DFHPF23
00306          GO TO 9000-RETURN-CICS.
00307
00308      IF EIBAID = DFHPF24
00309          MOVE XCTL-126           TO  THIS-PGM
00310          GO TO 9300-XCTL.
00311
00312      IF EIBAID = DFHENTER
00313          GO TO 0025-MAIN-LOGIC.
00314
00315      IF EIBAID NOT = DFHENTER
00316          MOVE -1                 TO  ALFPFKL
00317          MOVE ER-0008            TO  EMI-ERROR
00318          GO TO 8200-SEND-DATAONLY.
00319  EJECT
00320  0025-MAIN-LOGIC.
00321
00322      MOVE  SPACES                    TO  PI-ALPH-CLAIM-KEY
00323                                          PI-ALPH-ADMIN-KEY.
00324
00325      MOVE  PI-COMPANY-CD             TO  PI-ALPH-CO-CD
00326                                          PI-ADM-COMP-CD
00327                                          PI-CLM-COMP-CD.
00328
00329      MOVE  XCTL-1325                 TO  THIS-PGM.
00330
00331      IF PI-PROCESSOR-ID = 'LGXX'
00332          NEXT SENTENCE
00333      ELSE
00334          
      * EXEC CICS READQ TS
00335 *            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00336 *            INTO    (SECURITY-CONTROL)
00337 *            LENGTH  (SC-COMM-LENGTH)
00338 *            ITEM    (SC-ITEM)
00339 *        END-EXEC
      *    MOVE '*$II   L              ''   #00001612' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303031363132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00340          MOVE SC-CLAIMS-DISPLAY (21)   TO  PI-DISPLAY-CAP
00341          MOVE SC-CLAIMS-UPDATE  (21)   TO  PI-MODIFY-CAP
00342          IF NOT DISPLAY-CAP
00343              MOVE 'READ'               TO  SM-READ
00344              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00345              MOVE ER-0070              TO  EMI-ERROR
00346              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00347              GO TO 8100-SEND-INITIAL-MAP.
00348
00349 ******************************************************************
00350 *           O P T I O N  1  P R O C E S S I N G                  *
00351 ******************************************************************
00352
00353      IF PI-SESSION-IN-PROGRESS = '1'
00354          NEXT SENTENCE
00355      ELSE
00356         IF PI-SESSION-IN-PROGRESS  =  '2'
00357            MOVE -1              TO ALFCARRL
00358            GO TO 0200-MAIN-LOGIC.
00359
00360      IF (ALLNAMEL  GREATER THAN ZERO)  OR
00361         (ALFNAMEL  GREATER THAN ZERO)  OR
00362         (ALINTALL  GREATER THAN ZERO)
00363          NEXT SENTENCE
00364      ELSE
00365          GO TO 0100-MAIN-LOGIC.
00366
00367      IF (ALFNAMEL GREATER THAN +0    OR
00368          ALINTALL GREATER THAN +0)   AND
00369          ALLNAMEL NOT GREATER  ZERO
00370           MOVE ER-0488          TO  EMI-ERROR
00371           MOVE -1               TO  ALLNAMEL
00372           GO TO 8200-SEND-DATAONLY.
00373
00374      IF ALLNAMEL GREATER THAN ZEROS
00375         IF ALLNAMEI  IS EQUAL TO  SPACES OR LOW-VALUES
00376            MOVE ER-0284          TO  EMI-ERROR
00377            MOVE -1               TO  ALLNAMEL
00378            GO TO 8200-SEND-DATAONLY.
00379
00380      MOVE ALPHA-INDEX-DSID       TO  PI-DSID.
00381      MOVE '1'                    TO  PI-OPTION.
00382      MOVE PI-COMPANY-CD          TO  PI-ALPH-CO-CD.
00383      MOVE 'C'                    TO  PI-ALPH-SOURCE.
00384
00385      MOVE +2                     TO  PI-KEY-LENGTH.
00386
00387      IF ALLNAMEL GREATER ZERO
00388         MOVE ALLNAMEI            TO  PI-ALPH-LAST-NAME
00389                                      WS-INPUT-FIELD
00390         IF (ALFNAMEL  EQUAL +0)   AND
00391            (ALINTALL  EQUAL +0)
00392               PERFORM 0030-MAIN-LOGIC THRU 0030-MAIN-LOGIC-EXIT
00393                  VARYING INPUT-INDEX FROM ALLNAMEL BY -1
00394                    UNTIL WS-INPUT-CHAR (INPUT-INDEX) NOT = SPACE
00395            ADD ALLNAMEL         TO  PI-KEY-LENGTH
00396         ELSE
00397            MOVE +17             TO  PI-KEY-LENGTH.
00398
00399      IF ALFNAMEL  GREATER ZERO
00400         MOVE ALFNAMEI           TO  PI-ALPH-FRST-NAME
00401         IF ALINTALL  GREATER ZERO
00402            ADD +13              TO  PI-KEY-LENGTH
00403            MOVE ALINTALI        TO  PI-ALPH-MID-INIT
00404         ELSE
00405            MOVE ALFNAMEI       TO  WS-INPUT-FIELD
00406               PERFORM 0050-MAIN-LOGIC THRU 0050-MAIN-LOGIC-EXIT
00407                  VARYING INPUT-INDEX FROM ALFNAMEL BY -1
00408                    UNTIL WS-INPUT-CHAR (INPUT-INDEX) NOT = SPACE
00409            ADD ALFNAMEL           TO  PI-KEY-LENGTH.
00410
00411      IF ALLNAMEL GREATER ZERO
00412          MOVE AL-UABON           TO  ALLNAMEA
00413      ELSE
00414          MOVE AL-UABOF           TO  ALLNAMEA.
00415
00416      IF ALFNAMEL GREATER ZERO
00417          MOVE AL-UABON           TO  ALFNAMEA
00418      ELSE
00419          MOVE AL-UABOF           TO  ALFNAMEA.
00420
00421      IF ALINTALL GREATER ZERO
00422          MOVE AL-UABON           TO  ALINTALA
00423      ELSE
00424          MOVE AL-UABOF           TO  ALINTALA.
00425
00426      MOVE -1                     TO  ALLNAMEL.
00427      PERFORM 4000-READ-ALPHA-FILE.
00428
00429      MOVE +1                     TO  PI-1ST-TIME-SW.
00430
00431      IF PI-RETURN-TO-PROGRAM = 'EL132   '
00432          MOVE  XCTL-150          TO  THIS-PGM
00433      ELSE
00434          MOVE  XCTL-1325         TO  THIS-PGM.
00435
00436      PERFORM 9300-XCTL.
00437
00438  0030-MAIN-LOGIC.
00439      SUBTRACT +1 FROM ALLNAMEL.
00440
00441  0030-MAIN-LOGIC-EXIT.
00442      EXIT.
00443
00444  0050-MAIN-LOGIC.
00445      SUBTRACT +1 FROM ALFNAMEL.
00446
00447  0050-MAIN-LOGIC-EXIT.
00448      EXIT.
00449
00450      EJECT
00451  0100-MAIN-LOGIC SECTION.
00452
00453 ******************************************************************
00454 *           O P T I O N  2  P R O C E S S I N G                  *
00455 ******************************************************************
00456
00457      IF (ALCLMNOL  GREATER ZERO)  OR
00458         (ALCARNOL  GREATER ZERO)  OR
00459         (ALCERTL   GREATER ZERO)  OR
00460         (ALCRTSXL  GREATER ZERO)
00461          NEXT SENTENCE
00462        ELSE
00463          GO TO 0200-MAIN-LOGIC.
00464
00465      MOVE ALPHA2-INDEX-DSID      TO  PI-DSID.
00466      MOVE '2'                    TO  PI-OPTION.
00467      MOVE PI-COMPANY-CD          TO  PI-ADM-COMP-CD
00468                                      PI-CLM-COMP-CD.
00469      MOVE 'C'                    TO  PI-CLM-SOURCE.
00470
00471      IF ALCLMNOL  GREATER THAN ZERO
00472         IF ALCLMNOI  IS EQUAL TO  SPACES OR LOW-VALUES
00473             MOVE ER-0284         TO  EMI-ERROR
00474             MOVE -1              TO  ALCLMNOL
00475             GO TO 8200-SEND-DATAONLY.
00476
00477 ******************************************************************
00478 *              SECURITY CHECK FOR CARRIER                        *
00479 *                    04/02/84                                    *
00480 ******************************************************************
00481
00482      IF  PI-NO-CARRIER-SECURITY
00483          GO TO 0128-BUILD-CARRIER-NO.
00484
00485      IF ALCARNOL GREATER ZERO
00486         IF  ALCARNOI = PI-CARRIER-SECURITY
00487             GO TO 0128-BUILD-CARRIER-NO.
00488
00489      MOVE ER-2370                TO  EMI-ERROR.
00490      MOVE -1                     TO  ALCARNOL.
00491      MOVE AL-UABON               TO  ALCARNOA.
00492      GO TO 8200-SEND-DATAONLY.
00493
00494  0128-BUILD-CARRIER-NO.
00495      IF ALCARNOL GREATER ZERO
00496          MOVE ALCARNOI           TO  PI-CLM-CARRIER
00497                                      PI-CARRIER
00498          MOVE +3                 TO  PI-KEY-LENGTH
00499      ELSE
00500          MOVE ER-0194            TO  EMI-ERROR
00501          PERFORM 9900-ERROR-FORMAT
00502          MOVE -1                 TO  ALCARNOL.
00503
00504      IF ALCLMNOL  GREATER ZERO
00505          MOVE ALCLMNOI           TO  PI-CLM-CLAIM-NO
00506                                      PI-CLAIM-NO
00507                                      WS-INPUT-FIELD
00508          PERFORM 0130-MAIN-LOGIC THRU 0130-MAIN-LOGIC-EXIT
00509              VARYING INPUT-INDEX FROM ALCLMNOL  BY -1
00510                  UNTIL WS-INPUT-CHAR (INPUT-INDEX) NOT = SPACE
00511          ADD +3  ALCLMNOL GIVING PI-KEY-LENGTH
00512        ELSE
00513          MOVE ER-0209           TO  EMI-ERROR
00514          PERFORM 9900-ERROR-FORMAT
00515          MOVE -1                TO  ALCLMNOL.
00516
00517      IF ALCERTL  GREATER ZERO
00518          MOVE ALCERTI            TO  PI-CLM-CERT-PRM
00519                                      WS-INPUT-FIELD
00520          MOVE +20                TO  PI-KEY-LENGTH.
00521
00522      IF ALCRTSXL GREATER ZERO
00523          MOVE +21                TO  PI-KEY-LENGTH
00524          MOVE ALCRTSXI           TO  PI-CLM-CERT-SFX.
00525
00526      MOVE PI-CLM-CERT-NUMBER     TO  PI-CERT-NO.
00527
00528      IF EMI-FATAL-CTR GREATER ZERO
00529          GO TO 8200-SEND-DATAONLY.
00530
00531      IF ALCLMNOL  GREATER ZERO
00532          MOVE AL-UABON           TO  ALCLMNOA
00533        ELSE
00534          MOVE AL-UABOF           TO  ALCLMNOA.
00535
00536      IF ALCARNOL GREATER ZERO
00537          MOVE AL-UABON           TO  ALCARNOA
00538        ELSE
00539          MOVE AL-UABOF           TO  ALCARNOA.
00540
00541      IF ALCERTL  GREATER ZERO
00542          MOVE AL-UABON           TO  ALCERTA
00543        ELSE
00544          MOVE AL-UABOF           TO  ALCERTA.
00545
00546      IF ALCRTSXL GREATER ZERO
00547          MOVE AL-UABON           TO  ALCRTSXA
00548        ELSE
00549          MOVE AL-UABOF           TO  ALCRTSXA.
00550
00551      MOVE -1                     TO  ALCLMNOL.
00552      PERFORM 4000-READ-ALPHA-FILE.
00553
00554  0130-MAIN-LOGIC.
00555      SUBTRACT +1 FROM ALCLMNOL.
00556
00557  0130-MAIN-LOGIC-EXIT.
00558      EXIT.
00559
00560      EJECT
00561  0200-MAIN-LOGIC SECTION.
00562 ******************************************************************
00563 *           O P T I O N  3  P R O C E S S I N G                  *
00564 ******************************************************************
00565
00566      IF (ALFCARRL  GREATER ZERO)  OR
00567         (ALFGRPL   GREATER ZERO)  OR
00568         (ALFSTL    GREATER ZERO)  OR
00569         (ALFACCTL  GREATER ZERO)  OR
00570         (ALFEFDTL  GREATER ZERO)  OR
00571         (ALFCERTL  GREATER ZERO)
00572          NEXT SENTENCE
00573        ELSE
00574          GO TO 0300-MAIN-LOGIC.
00575
00576      MOVE '3'                    TO  PI-OPTION.
00577      MOVE PI-COMPANY-CD          TO  PI-ADM-COMP-CD.
00578      MOVE 'A'                    TO  PI-ADM-SOURCE.
00579
00580 ******************************************************************
00581 *              SECURITY CHECK FOR CARRIER                        *
00582 *                    04/02/84                                    *
00583 ******************************************************************
00584
00585      IF  PI-NO-CARRIER-SECURITY
00586          GO TO 0200-BUILD-ADMIN-KEY.
00587
00588      IF ALFCARRL GREATER ZERO
00589         IF  ALFCARRI = PI-CARRIER-SECURITY
00590             GO TO 0200-BUILD-ADMIN-KEY.
00591
00592      MOVE ER-2370                TO  EMI-ERROR.
00593      MOVE -1                     TO  ALFCARRL.
00594      MOVE AL-UABON               TO  ALFCARRA.
00595      GO TO 8200-SEND-DATAONLY.
00596
00597  0200-BUILD-ADMIN-KEY.
00598      IF ALFCARRL GREATER ZERO
00599          MOVE ALFCARRI           TO  PI-ADM-CARRIER
00600                                      PI-CARRIER
00601          MOVE +1                 TO  PI-KEY-LENGTH
00602      ELSE
00603          MOVE ER-0194            TO  EMI-ERROR
00604          PERFORM  9900-ERROR-FORMAT THRU 9900-EXIT
00605          MOVE  -1                TO  ALFCARRL.
00606
00607      IF ALFGRPL  GREATER THAN  +0
00608         MOVE ALFGRPI             TO  PI-ADM-GROUPING
00609                                      PI-GROUPING
00610         ADD  +6                  TO  PI-KEY-LENGTH
00611      ELSE
00612         MOVE ER-0195             TO  EMI-ERROR
00613         PERFORM  9900-ERROR-FORMAT THRU 9900-EXIT
00614         MOVE  -1                 TO  ALFGRPL.
00615
00616      IF ALFSTL  GREATER THAN  +0
00617         MOVE ALFSTI              TO  PI-ADM-STATE
00618                                      PI-STATE
00619         ADD  +2                  TO  PI-KEY-LENGTH
00620      ELSE
00621         MOVE ER-0196             TO  EMI-ERROR
00622         PERFORM  9900-ERROR-FORMAT THRU 9900-EXIT
00623         MOVE  -1                 TO  ALFSTL.
00624
00625      IF ALFACCTL  GREATER THAN  +0
00626         MOVE ALFACCTI            TO  PI-ADM-PRODUCER
00627                                      PI-PRODUCER
00628         ADD +10                  TO  PI-KEY-LENGTH
00629      ELSE
00630         MOVE ER-0197             TO  EMI-ERROR
00631         PERFORM  9900-ERROR-FORMAT THRU 9900-EXIT
00632         MOVE  -1                 TO  ALFACCTL.
00633
00634      IF ALFEFDTL  GREATER THAN  +0
00635         MOVE  ALFEFDTI           TO  DC-GREG-DATE-1-EDIT
00636         MOVE  '2'                TO  DC-OPTION-CODE
00637         PERFORM 8500-DATE-CONVERSION  THRU 8500-EXIT
00638         IF DATE-CONVERSION-ERROR
00639            MOVE  ER-0348         TO  EMI-ERROR
00640            MOVE  -1              TO  ALFEFDTL
00641            MOVE AL-UABON         TO  ALFEFDTA
00642            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00643         ELSE
00644            MOVE  DC-BIN-DATE-1   TO  PI-ADM-CERT-EFF-DT
00645                                      PI-CERT-EFF-DT
00646            ADD +08               TO  PI-KEY-LENGTH
00647      ELSE
00648         MOVE ER-0216             TO  EMI-ERROR
00649         PERFORM  9900-ERROR-FORMAT THRU 9900-EXIT
00650         MOVE  -1                 TO  ALFEFDTL.
00651
00652      IF ALFCERTL  GREATER THAN  +0
00653          MOVE ALFCERTI           TO  PI-ADM-CERT-PRM
00654          ADD  +10                TO  PI-KEY-LENGTH
00655      ELSE
00656         MOVE ER-0203             TO  EMI-ERROR
00657         PERFORM  9900-ERROR-FORMAT THRU 9900-EXIT
00658         MOVE  -1                 TO  ALFEFDTL.
00659
00660      IF ALFCRTXL GREATER ZERO
00661          ADD  +01                TO  PI-KEY-LENGTH
00662          MOVE ALFCRTXI           TO  PI-ADM-CERT-SFX.
00663
00664      MOVE PI-CLM-CERT-NUMBER     TO  PI-CERT-NO.
00665
00666      IF ALFCARRL  GREATER THAN  +0
00667          MOVE AL-UABON           TO  ALFCARRA
00668      ELSE
00669          MOVE AL-UABOF           TO  ALFCARRA.
00670
00671      IF ALFGRPL   GREATER THAN  +0
00672          MOVE AL-UABON           TO  ALFGRPA
00673      ELSE
00674          MOVE AL-UABOF           TO  ALFGRPA.
00675
00676      IF ALFSTL    GREATER THAN  +0
00677          MOVE AL-UABON           TO  ALFSTA
00678      ELSE
00679          MOVE AL-UABOF           TO  ALFSTA.
00680
00681      IF ALFACCTL  GREATER THAN  +0
00682          MOVE AL-UABON           TO  ALFACCTA
00683      ELSE
00684          MOVE AL-UABOF           TO  ALFACCTA.
00685
00686      IF ALFEFDTL  GREATER THAN  +0
00687          MOVE AL-UABON           TO  ALFEFDTA
00688      ELSE
00689          MOVE AL-UABOF           TO  ALFEFDTA.
00690
00691      IF ALFCERTL  GREATER THAN  +0
00692          MOVE AL-UABON           TO  ALFCERTA
00693      ELSE
00694          MOVE AL-UABOF           TO  ALFCERTA.
00695
00696      IF ALFCRTXL  GREATER THAN  +0
00697          MOVE AL-UABON           TO  ALFCRTXA
00698      ELSE
00699          MOVE AL-UABOF           TO  ALFCRTXA.
00700
00701      MOVE -1                     TO  ALFCARRL.
00702      MOVE ALPHA2-INDEX-DSID      TO  PI-DSID.
00703      PERFORM 4000-READ-ALPHA-FILE.
00704
00705  0220-MAIN-LOGIC-EXIT.
00706      EXIT.
00707
00708      EJECT
00709  0300-MAIN-LOGIC SECTION.
00710 ******************************************************************
00711 *           O P T I O N  4  P R O C E S S I N G                  *
00712 ******************************************************************
00713
00714      IF (ALFLNMEL  GREATER ZERO)  OR
00715         (ALFFNMEL  GREATER ZERO)  OR
00716         (ALFINTLL  GREATER ZERO)
00717          NEXT SENTENCE
00718        ELSE
00719          GO TO 0400-MAIN-LOGIC.
00720
00721      IF (ALFFNMEL GREATER THAN ZERO  OR
00722          ALFINTLL GREATER THAN ZERO) AND
00723          ALFLNMEL NOT GREATER  ZERO
00724          MOVE ER-0488            TO  EMI-ERROR
00725          MOVE -1                 TO  ALFLNMEL
00726          GO TO 8200-SEND-DATAONLY.
00727
00728      MOVE ALPHA-INDEX-DSID       TO  PI-DSID.
00729      MOVE '4'                    TO  PI-OPTION.
00730      MOVE PI-COMPANY-CD          TO  PI-ALPH-CO-CD.
00731      MOVE 'A'                    TO  PI-ALPH-SOURCE.
00732
00733      MOVE +1                     TO  PI-KEY-LENGTH.
00734
00735      IF ALFLNMEL GREATER ZERO
00736         MOVE ALFLNMEI            TO  PI-ALPH-LAST-NAME
00737                                      WS-INPUT-FIELD
00738         IF ALFFNMEL  EQUAL +0
00739                   AND
00740            ALFINTLL  EQUAL +0
00741            PERFORM 0320-MAIN-LOGIC THRU 0320-MAIN-LOGIC-EXIT
00742              VARYING INPUT-INDEX FROM ALFLNMEL BY -1
00743              UNTIL WS-INPUT-CHAR (INPUT-INDEX) NOT = SPACE
00744            ADD ALFLNMEL  TO  PI-KEY-LENGTH
00745         ELSE
00746            MOVE +15     TO  PI-KEY-LENGTH.
00747
00748      IF ALFFNMEL  GREATER ZERO
00749         MOVE ALFFNMEI           TO  PI-ALPH-FRST-NAME
00750         IF ALFINTLL  GREATER ZERO
00751            ADD +13              TO  PI-KEY-LENGTH
00752            MOVE ALFINTLI        TO  PI-ALPH-MID-INIT
00753         ELSE
00754            MOVE ALFFNMEI       TO  WS-INPUT-FIELD
00755            PERFORM 0325-MAIN-LOGIC THRU 0325-MAIN-LOGIC-EXIT
00756              VARYING INPUT-INDEX FROM ALFFNMEL BY -1
00757              UNTIL WS-INPUT-CHAR (INPUT-INDEX) NOT = SPACE
00758            ADD ALFFNMEL           TO  PI-KEY-LENGTH.
00759
00760      IF ALFLNMEL GREATER ZERO
00761          MOVE AL-UABON           TO  ALFLNMEA
00762      ELSE
00763          MOVE AL-UABOF           TO  ALFLNMEA.
00764
00765      IF ALFFNMEL GREATER ZERO
00766          MOVE AL-UABON           TO  ALFFNMEA
00767      ELSE
00768          MOVE AL-UABOF           TO  ALFFNMEA.
00769
00770      IF ALFINTLL GREATER ZERO
00771          MOVE AL-UABON           TO  ALFINTLA
00772      ELSE
00773          MOVE AL-UABOF           TO  ALFINTLA.
00774
00775      MOVE -1                     TO  ALFLNMEL.
00776      PERFORM 4000-READ-ALPHA-FILE.
00777
00778  0320-MAIN-LOGIC.
00779      SUBTRACT +1 FROM ALFLNMEL.
00780
00781  0320-MAIN-LOGIC-EXIT.
00782      EXIT.
00783
00784  0325-MAIN-LOGIC.
00785      SUBTRACT +1 FROM ALFFNMEL.
00786
00787  0325-MAIN-LOGIC-EXIT.
00788      EXIT.
00789
00790      EJECT
00791  0400-MAIN-LOGIC.
00792
00793      MOVE ZERO                   TO  PI-OPTION.
00794      MOVE +1                     TO  PI-KEY-LENGTH.
00795      MOVE ALPHA-INDEX-DSID       TO  PI-DSID.
00796      MOVE ZERO                   TO  PI-OPTION.
00797      MOVE -1                     TO  ALLNAMEL.
00798      PERFORM 4000-READ-ALPHA-FILE.
00799
00800      EJECT
00801  4000-READ-ALPHA-FILE SECTION.
00802
00803      
      * EXEC CICS HANDLE CONDITION
00804 *        DUPKEY (9300-XCTL)
00805 *        NOTFND (4080-NOTFND)
00806 *    END-EXEC.
      *    MOVE '"$$I                  ! # #00002081' TO DFHEIV0
           MOVE X'222424492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303032303831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00807
00808      IF (PI-OPTION  =  ZERO) OR
00809         (PI-OPTION  =  '1')  OR
00810         (PI-OPTION  =  '4')
00811           NEXT SENTENCE
00812      ELSE
00813          GO TO 4010-READ-ALPHA-FILE.
00814
00815      IF (PI-DSID = ALPHA-INDEX-DSID)  AND
00816         (PI-KEY-LENGTH  LESS THAN +31)
00817                      OR
00818         (PI-DSID = ALPHA-INDEX-DSID) AND
00819         (PI-KEY-LENGTH  LESS THAN +31)
00820           MOVE +1             TO  PI-START-SW
00821           
      * EXEC CICS READ
00822 *             DATASET   (PI-DSID)
00823 *             RIDFLD    (PI-ALPH-CLAIM-KEY)
00824 *             SET       (ADDRESS OF ALPHA-INDEX)
00825 *             GENERIC   EQUAL
00826 *             KEYLENGTH (PI-KEY-LENGTH)
00827 *         END-EXEC
      *    MOVE '&"S  KG    E          (   #00002099' TO DFHEIV0
           MOVE X'26225320204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032303939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ALPH-CLAIM-KEY, 
                 PI-KEY-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ALPHA-INDEX TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00828      ELSE
00829           MOVE ZERO           TO  PI-START-SW
00830           
      * EXEC CICS READ
00831 *             DATASET   (PI-DSID)
00832 *             RIDFLD    (PI-ALPH-CLAIM-KEY)
00833 *             SET       (ADDRESS OF ALPHA-INDEX)
00834 *         END-EXEC.
      *    MOVE '&"S        E          (   #00002108' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ALPH-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ALPHA-INDEX TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00835
00836      GO TO 9300-XCTL.
00837
00838  4010-READ-ALPHA-FILE.
00839
00840      IF (PI-DSID = ALPHA2-INDEX-DSID) AND
00841         (PI-KEY-LENGTH  LESS THAN +22)
00842                     OR
00843         (PI-KEY-LENGTH  LESS THAN +41)
00844           MOVE +1             TO  PI-START-SW
00845           
      * EXEC CICS READ
00846 *             DATASET   (PI-DSID)
00847 *             RIDFLD    (PI-ALPH-ADMIN-KEY)
00848 *             SET       (ADDRESS OF ALPHA-INDEX)
00849 *             GENERIC   EQUAL
00850 *             KEYLENGTH (PI-KEY-LENGTH)
00851 *         END-EXEC
      *    MOVE '&"S  KG    E          (   #00002123' TO DFHEIV0
           MOVE X'26225320204B472020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ALPH-ADMIN-KEY, 
                 PI-KEY-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ALPHA-INDEX TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00852      ELSE
00853           MOVE ZERO           TO  PI-START-SW
00854           
      * EXEC CICS READ
00855 *             DATASET   (PI-DSID)
00856 *             RIDFLD    (PI-ALPH-ADMIN-KEY)
00857 *             SET       (ADDRESS OF ALPHA-INDEX)
00858 *         END-EXEC.
      *    MOVE '&"S        E          (   #00002132' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ALPH-ADMIN-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ALPHA-INDEX TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00859
00860      GO TO 9300-XCTL.
00861
00862  4000-EXIT.
00863      EXIT.
00864  4080-NOTFND.
00865      MOVE ER-0284                   TO  EMI-ERROR.
00866
00867      IF PI-RETURN-TO-PROGRAM = 'EL127   '
00868        AND WS-CALL-SW = ZERO
00869          GO TO 4090-EXIT.
00870
00871      GO TO 8200-SEND-DATAONLY.
00872
00873  4090-EXIT.
00874      EXIT.
00875
00876      EJECT
00877  7000-BUILD-SCREEN     SECTION.
00878 ******************************************************************
00879 *          REBUILD ORIGNAL SCREEN AND ERROR MESSAGE IF EL1322    *
00880 *          DID NOT FIND ANY CLAIM RECORDS DURING BROWSE OF FILE. *
00881 ******************************************************************
00882      IF EIBTRNID = WS-TRANS-ID
00883          NEXT SENTENCE
00884        ELSE
00885          GO TO 7099-EXIT.
00886
00887      IF  PI-BROWSE-SW = +9
00888          NEXT SENTENCE
00889         ELSE
00890          GO TO 7099-EXIT.
00891
00892      MOVE LOW-VALUES             TO  EL1324AO.
00893
00894      MOVE ER-2374                TO  EMI-ERROR.
00895
00896      IF OPTION-ONE-SELECTED
00897         GO TO 7099-EXIT.
00898
00899      IF OPTION-TWO-SELECTED
00900         NEXT SENTENCE
00901      ELSE
00902         GO TO 7030-OPTION-THREE.
00903
00904  7020-OPTION-TWO.
00905
00906      IF  PI-CLM-CLAIM-NO   GREATER SPACES
00907          MOVE PI-CLM-CLAIM-NO        TO ALCLMNOO
00908          MOVE AL-UANON               TO ALCLMNOA.
00909
00910      IF  PI-CLM-CARRIER    GREATER SPACES
00911          MOVE PI-CLM-CARRIER         TO ALCARNOO
00912          MOVE AL-UANON               TO ALCARNOA.
00913
00914      IF  PI-CLM-CERT-PRM   GREATER SPACES
00915          MOVE PI-CLM-CERT-PRM        TO ALCERTO
00916          MOVE AL-UANON               TO ALCERTA.
00917
00918      IF  PI-CLM-CERT-SFX   GREATER SPACES
00919          MOVE PI-CLM-CERT-SFX        TO ALCRTSXO
00920          MOVE AL-UANON               TO ALCRTSXA.
00921
00922      GO TO   7090-INITIALIZE-WORK-AREAS.
00923
00924  7030-OPTION-THREE.
00925
00926      IF OPTION-THREE-SELECTED
00927         NEXT SENTENCE
00928      ELSE
00929         GO TO 7040-OPTION-FOUR.
00930
00931      MOVE -1                          TO ALFCARRL.
00932
00933      IF PI-ADM-CARRIER   GREATER SPACES
00934         MOVE PI-ADM-CARRIER           TO ALFCARRO
00935         MOVE AL-UANON                 TO ALFCARRA.
00936
00937      IF PI-ADM-GROUPING  GREATER SPACES
00938         MOVE PI-ADM-GROUPING          TO ALFGRPO
00939         MOVE AL-UANON                 TO ALFGRPA.
00940
00941      IF PI-ADM-STATE     GREATER SPACES
00942         MOVE PI-ADM-STATE             TO ALFSTO
00943         MOVE AL-UANON                 TO ALFSTA.
00944
00945      IF PI-ADM-PRODUCER  GREATER SPACES
00946         MOVE PI-ADM-PRODUCER          TO ALFACCTO
00947         MOVE AL-UANON                 TO ALFACCTA.
00948
00949      IF PI-ADM-CERT-EFF-DT   GREATER SPACES
00950         MOVE PI-ADM-CERT-EFF-DT       TO ALFEFDTO
00951         MOVE AL-UANON                 TO ALFEFDTA.
00952
00953      IF PI-ADM-CERT-NUMBER   GREATER SPACES
00954         MOVE PI-ADM-CERT-PRM          TO ALFCERTO
00955         MOVE AL-UANON                 TO ALFCERTA
00956         MOVE PI-ADM-CERT-SFX          TO ALFCRTXO
00957         MOVE AL-UANON                 TO ALFCRTXA.
00958
00959      GO TO   7090-INITIALIZE-WORK-AREAS.
00960
00961  7040-OPTION-FOUR.
00962      MOVE -1                    TO ALFLNMEL.
00963
00964      IF  PI-ALPH-LAST-NAME GREATER SPACES
00965          MOVE PI-ALPH-LAST-NAME       TO ALFLNMEO
00966          MOVE AL-UANON                TO ALFLNMEA
00967          IF  PI-ALPH-F-INIT GREATER SPACES
00968              MOVE PI-ALPH-FRST-NAME   TO ALFFNMEO
00969              MOVE PI-ALPH-MID-INIT    TO ALFINTLO
00970              MOVE AL-UANON            TO ALFFNMEA
00971              MOVE AL-UANON            TO ALFINTLA.
00972
00973
00974  7090-INITIALIZE-WORK-AREAS.
00975      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA.
00976
00977      MOVE ZERO                   TO  PI-1ST-TIME-SW
00978                                      PI-LINE-COUNT
00979                                      PI-AIX-RECORD-COUNT
00980                                      PI-BROWSE-SW
00981                                      PI-KEY-LENGTH
00982                                      PI-TS-ITEM
00983                                      PI-END-OF-FILE
00984                                      PI-START-SW.
00985
00986      GO TO 8100-SEND-INITIAL-MAP.
00987
00988  7099-EXIT.
00989      EXIT.
00990
00991      EJECT
00992  8100-SEND-INITIAL-MAP SECTION.
00993
00994      MOVE EIBTIME                TO  TIME-IN.
00995      MOVE SAVE-DATE              TO  ALDATEO.
00996      MOVE TIME-OUT               TO  ALTIMEO.
101501     MOVE PI-COMPANY-ID          TO  ALCOMPO.
101501     MOVE PI-PROCESSOR-ID        TO  USERIDO.
00997
00998      IF EMI-ERROR NOT = ZERO
00999          PERFORM 9900-ERROR-FORMAT.
01000
01001      IF PI-SESSION-IN-PROGRESS  =  '1'
01002          MOVE -1                 TO  ALLNAMEL
01003      ELSE
01004          MOVE -1                 TO  ALFCARRL.
01005
01006      MOVE EMI-MESSAGE-AREA (1)   TO  ALFMSG1O.
01007      MOVE EMI-MESSAGE-AREA (2)   TO  ALFMSG2O.
01008
101501*    IF PI-ORIGINAL-COMPANY-ID NOT EQUAL SPACES
101501*        MOVE PI-COMPANY-ID          TO  ALCOMPO
101501*    ELSE
101501*        MOVE SPACES                 TO  ALCOMPO.
01013
01014      
      * EXEC CICS SEND
01015 *        FROM   (EL1324AO)
01016 *        MAPSET (WS-MAPSET-NAME)
01017 *        MAP    (WS-MAP-NAME)
01018 *        CURSOR
01019 *        ERASE
01020 *    END-EXEC.
           MOVE LENGTH OF
            EL1324AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00002294' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032323934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL1324AO, 
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
           
01021
01022      GO TO 9100-RETURN-TRAN.
01023
01024  8100-EXIT.
01025      EXIT.
01026
01027      EJECT
01028  8200-SEND-DATAONLY SECTION.
01029
01030      MOVE EIBTIME                TO  TIME-IN.
01031      MOVE SAVE-DATE              TO  ALDATEO.
01032      MOVE TIME-OUT               TO  ALTIMEO.
101501     MOVE PI-COMPANY-ID          TO  ALCOMPO.
101501     MOVE PI-PROCESSOR-ID        TO  USERIDO.
01033
01034      IF EMI-ERROR NOT = ZERO
01035          PERFORM 9900-ERROR-FORMAT.
01036
01037      MOVE EMI-MESSAGE-AREA (1)   TO  ALFMSG1O.
01038      MOVE EMI-MESSAGE-AREA (2)   TO  ALFMSG2O.
01039
101501*    IF PI-ORIGINAL-COMPANY-ID NOT = SPACES
101501*        MOVE PI-COMPANY-ID      TO  ALCOMPO
101501*    ELSE
101501*        MOVE SPACES             TO  ALCOMPO.
01044
01045      
      * EXEC CICS SEND DATAONLY
01046 *        FROM   (EL1324AO)
01047 *        MAPSET (WS-MAPSET-NAME)
01048 *        MAP    (WS-MAP-NAME)
01049 *        CURSOR
01050 *    END-EXEC.
           MOVE LENGTH OF
            EL1324AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00002327' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL1324AO, 
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
           
01051
01052      GO TO 9100-RETURN-TRAN.
01053
01054      EJECT
01055  8300-SEND-TEXT SECTION.
01056      
      * EXEC CICS SEND TEXT
01057 *        FROM   (LOGOFF-TEXT)
01058 *        LENGTH (LOGOFF-LENGTH)
01059 *        ERASE  FREEKB
01060 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00002338' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333338' TO DFHEIV0(25:11)
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
           
01061
01062      
      * EXEC CICS RETURN
01063 *    END-EXEC.
      *    MOVE '.(                    &   #00002344' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01064
01065  8300-EXIT.
01066      EXIT.
01067
01068  8500-DATE-CONVERSION SECTION.
01069      
      * EXEC CICS LINK
01070 *        PROGRAM  ('ELDATCV')
01071 *        COMMAREA (DATE-CONVERSION-DATA)
01072 *        LENGTH   (DC-COMM-LENGTH)
01073 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   ''   #00002351' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01074
01075  8500-EXIT.
01076      EXIT.
01077
01078      EJECT
01079  8800-NOTOPEN SECTION.
01080
01081      MOVE ER-0777                TO  EMI-ERROR
01082      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01083      GO TO  8200-SEND-DATAONLY.
01084
01085
01086  9000-RETURN-CICS SECTION.
01087      MOVE XCTL-005               TO  THIS-PGM.
01088      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
01089      PERFORM 9300-XCTL.
01090
01091  9000-EXIT.
01092      EXIT.
01093
01094  9100-RETURN-TRAN SECTION.
01095      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
01096      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.
01097
01098      
      * EXEC CICS RETURN
01099 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
01100 *        LENGTH   (PI-COMM-LENGTH)
01101 *        TRANSID  (WS-TRANS-ID)
01102 *    END-EXEC.
      *    MOVE '.(CT                  &   #00002380' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01103
01104  9100-EXIT.
01105      EXIT.
01106
01107  9300-XCTL SECTION.
01108      MOVE DFHENTER               TO  EIBAID.
01109
01110      
      * EXEC CICS XCTL
01111 *        PROGRAM  (THIS-PGM)
01112 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
01113 *        LENGTH   (PI-COMM-LENGTH)
01114 *    END-EXEC.
      *    MOVE '.$C                   $   #00002392' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 THIS-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01115
01116  9300-EXIT.
01117      EXIT.
01118
01119      EJECT
01120  9400-CLEAR SECTION.
01121      MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM
01122      GO TO 9300-XCTL.
01123
01124  9400-EXIT.
01125      EXIT.
01126
01127  9600-PGMIDERR SECTION.
01128      
      * EXEC CICS HANDLE CONDITION
01129 *        PGMIDERR (8300-SEND-TEXT)
01130 *    END-EXEC.
      *    MOVE '"$L                   ! $ #00002410' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303032343130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01131
01132      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM
01133                                      LOGOFF-PGM.
01134
01135      MOVE XCTL-005               TO  THIS-PGM.
01136      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
01137      MOVE SPACES                 TO  PI-ENTRY-CD-1.
01138      GO TO 9300-XCTL.
01139
01140  9600-EXIT.
01141      EXIT.
01142
01143      EJECT
01144  9900-ERROR-FORMAT SECTION.
01145
01146      MOVE LINK-001                TO  THIS-PGM.
01147
01148      
      * EXEC CICS LINK
01149 *        PROGRAM  (THIS-PGM)
01150 *        COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
01151 *        LENGTH   (EMI-COMM-LENGTH)
01152 *    END-EXEC.
      *    MOVE '."C                   ''   #00002430' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 THIS-PGM, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01153
01154      MOVE ER-0000                   TO  EMI-ERROR.
01155
01156  9900-EXIT.
01157      EXIT.
01158
01159      EJECT
01160  9990-ERROR SECTION.
01161      MOVE DFHEIBLK TO EMI-LINE1.
01162      MOVE LINK-004                TO  THIS-PGM.
01163
01164      
      * EXEC CICS LINK
01165 *        PROGRAM  (THIS-PGM)
01166 *        COMMAREA (EMI-LINE1)
01167 *        LENGTH   (72)
01168 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   ''   #00002446' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 THIS-PGM, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01169
01170      GO TO 8200-SEND-DATAONLY.
01171
01172  9990-EXIT.
01173      EXIT.
01174
01175  9995-SECURITY-VIOLATION.
01176 *           COPY ELCSCTP.
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
      *    MOVE '."C                   ''   #00002475' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343735' TO DFHEIV0(25:11)
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
01177
01178  9995-EXIT.
01179      EXIT.
01180
01181


       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1324' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMIDERR,
                     8800-NOTOPEN,
                     9990-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 9300-XCTL,
                     4080-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1324' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
